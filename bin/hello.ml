open Ocaml_common
open Types
open Asttypes

open Ast_helper

[@@@warning "-34"]

let () = Printexc.record_backtrace true

let loc = Location.none

let mkloc v =
  let open Location in
  { txt = v; loc }

let ct_of_desc desc =
  Parsetree.
    {
      ptyp_desc = desc;
      ptyp_loc = loc;
      ptyp_loc_stack = [];
      ptyp_attributes = [];
    }

let unimplemented pp value loc =
  failwith
    (Format.asprintf "(%s:%d) TODO: implement this %a\n%!" __FILE__ loc pp value)

let to_lident path = mkloc (Untypeast.lident_of_path path)

let rec ct_of_te typ = ct_of_desc (ct_desc_of_te typ)

and ct_desc_of_te te =
  match te.desc with
  | Tarrow (label, param, body, _) ->
      let param = ct_of_te param in
      let body = ct_of_te body in
      Ptyp_arrow (label, param, body)
  | Ttuple ts -> Ptyp_tuple (List.map ct_of_te ts)
  | Tconstr (path, args, _) ->
      Ptyp_constr (to_lident path, List.map ct_of_te args)
  | Tlink typ | Tsubst typ -> ct_desc_of_te typ
  | _ -> unimplemented Printtyp.type_expr te __LINE__

let to_constant constant =
  match constant with
  | Const_int n -> Parsetree.Pconst_integer (string_of_int n, None)
  | _ -> failwith "TODO: constants not implemented"

let tconstrain_pattern pattern =
  Pat.constraint_ (Untypeast.untype_pattern pattern) (ct_of_te pattern.pat_type)

let untype_expression = Untypeast.untype_expression

let rec to_expression expr =
  let open Typedtree in
  match expr.exp_desc with
  | Texp_ident (_, lident, _) -> Exp.ident lident
  | Texp_constant constant -> Exp.constant (to_constant constant)
  | Texp_function { arg_label; param = _; cases; partial = _ } ->
      let pattern, guard, body =
        match cases with
        | [ { c_lhs; c_guard; c_rhs } ] ->
            (tconstrain_pattern c_lhs, c_guard, c_rhs)
        | _ ->
            let pexpr = Untypeast.untype_expression expr in
            Format.printf "function not supported. Representation is %a\n"
              (Printast.expression 0) pexpr;
            unimplemented Pprintast.expression pexpr __LINE__
      in
      Exp.fun_ arg_label
        (Option.map to_expression guard)
        pattern (to_expression body)
  | Texp_apply (funct, args) ->
      let funct = to_expression funct in
      let args = List.map to_arg args in
      Exp.apply funct args
  | Texp_match (exp, cases, _) ->
      let match_cases =
        cases
        |> List.map (fun { c_lhs; c_guard; c_rhs } ->
               Parsetree.
                 {
                   pc_lhs = Untypeast.untype_pattern c_lhs;
                   pc_guard = Option.map untype_expression c_guard;
                   pc_rhs = untype_expression c_rhs;
                 })
      in
      Exp.match_ (untype_expression exp) match_cases
  | Texp_ifthenelse (if_, then_, else_) ->
      Exp.ifthenelse (untype_expression if_) (untype_expression then_)
        (Option.map untype_expression else_)
  | _ ->
      let pexpr = Untypeast.untype_expression expr in
      unimplemented Pprintast.expression pexpr __LINE__

and to_arg arg =
  match arg with
  | Nolabel, Some expr -> (Nolabel, to_expression expr)
  | _ -> failwith "TODO: implement this"

let loc = Location.none

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

(* Need to write a function that behaves the same as Pprintast.structure that behaves the same except
   doesn't print the module type when pritning a module
   also it needs to do other things like make function arguments explicit but that's already done
   by eduardo for individual expressions, so just need to map the structure list and apply that
   change before printing *)

(* let () = Format.printf "%a\n" Pprintast.structure code *)

let rec typed_string_of_struct indentation
    (Typedtree.{ str_desc = sid; _ } as si) =
  match sid with
  | Tstr_value (rec', [ vb ]) ->
      let pattern = vb.vb_pat in

      let expr = vb.vb_expr |> to_expression in

      let expr_type = vb.vb_expr.exp_type in

      Format.asprintf "let %s%a : %a = %a\n"
        (if rec' = Recursive then "rec " else "")
        Pprintast.pattern
        (Untypeast.untype_pattern pattern)
        Printtyp.type_expr expr_type Pprintast.expression expr
  | Tstr_value (_, _) -> failwith "let ... and not implemented "
  | Tstr_module
      {
        mb_name = { txt = Some a; _ };
        mb_expr =
          {
            mod_desc =
              ( Tmod_structure struc
              | Tmod_constraint ({ mod_desc = Tmod_structure struc; _ }, _, _, _)
                );
            _;
          };
        _;
      } ->
      Format.asprintf "module %s = struct \n%s end" a
        (stringify_structure (indentation + 1) struc.str_items)
  | _ ->
      [ Untypeast.default_mapper.structure_item Untypeast.default_mapper si ]
      |> Pprintast.string_of_structure

and stringify_structure : int -> Typedtree.structure_item list -> string =
 fun indentation struc ->
  let rec repeat n s =
    match n with
    | 0 -> ""
    | x when x > 0 -> s ^ repeat (n - 1) s
    | _ -> assert false
  in
  struc
  |> List.map (typed_string_of_struct indentation)
  |> List.map (( ^ ) (repeat (indentation * 2) " "))
  |> String.concat "\n"

let type_structure env structure =
  match
    !Typecore.type_module env
      {
        pmod_desc = Pmod_structure structure;
        pmod_loc =
          {
            loc_start = Lexing.dummy_pos;
            loc_end = Lexing.dummy_pos;
            loc_ghost = true;
          };
        pmod_attributes = [];
      }
  with
  | { mod_desc = Tmod_structure { str_items = struc; str_final_env; _ }; _ } ->
      (struc, str_final_env)
  | { mod_desc =  Tmod_constraint ({ mod_desc = Tmod_structure { str_items = struc; str_final_env; _ }; _ }, _, _, _); _ } ->
      (struc, str_final_env)
  | { mod_desc; _ } -> (match mod_desc with 
  | Tmod_ident _ -> failwith "Tmod_ident impossible"
  | Tmod_structure _ -> failwith "Tmod_structure impossible"
  | Tmod_functor _ -> failwith "Tmod_functor impossible"
  | Tmod_apply _ -> failwith "Tmod_apply impossible"
  | Tmod_constraint _ -> failwith "Tmod_constraint impossible"
  | Tmod_unpack _ -> failwith "Tmod_unpack impossible")
(* let tcode = Typecore.type_exp env code *)

(* let scode = to_expression tcode |> Format.printf "%a\n%!" Pprintast.expression *)

open Parsetree

let code =
  [%str
    let a = Tezos.level
    let a = 1
    let b = a
    let add a = a + 1
    let rec add' (a, b) = a + b

    type my_variant = VarA | VarB

    type my_record = { field1 : int; field2 : string }

    module M : sig
      val a : int
    end = struct
      let a = 1
    end]


let stdlib = [%str 

type nat = nativeint

type (_, _) big_map

type (_, _) map

type _ set

type mutez = nat

type tez = nat

type operation

type address

type _ contract

let min : nat -> nat -> nat = assert false
let abs : int -> nat = assert false

module Tezos = struct
  let level : nat = assert false

  let amount : tez = assert false

  let sender : address = assert false

  let transaction : 'parameter -> mutez -> 'parameter contract -> operation =
    assert false

  let get_contract_opt : address -> 'parameter contract option = assert false
end

module rec Big_map : sig
  val empty : ('key, 'value) big_map

  val find_opt : 'key -> ('key, 'value) big_map -> 'value option

  val add : 'key -> 'value -> ('key, 'value) big_map -> ('key, 'value) big_map

  val remove : 'key -> ('key, 'value) big_map -> ('key, 'value) big_map

  val mem : 'key -> ('key, 'value) big_map -> bool
end = struct
  let empty = assert false

  let find_opt = assert false

  let add = assert false

  let remove = assert false

  let mem = assert false
end

module rec Set : sig
  val add : 'el -> 'el set -> 'el set

  val empty : 'a set

  val cardinal : 'a set -> nat

  val fold : ('acc * 'el -> 'acc) -> 'el set -> 'acc -> 'acc
end = struct
  let add = assert false

  let empty = assert false

  let cardinal = assert false

  let fold = assert false
end
]
let env = type_structure env stdlib |> snd

let () = stringify_structure 0 (code |> type_structure env |> fst) |> print_endline
