open Ocaml_common
open Types
open Asttypes
open Parsetree
open Typedtree
open Ast_helper

[@@@warning "-34"]

let () = Printexc.record_backtrace true

let loc = Location.none
let mkloc v =
  let open Location in
  { txt = v; loc }
let p desc =
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
let rec to_coretype typ = p (to_coretype_desc typ)

and to_coretype_desc typ =
  match typ.desc with
  | Tarrow (label, param, body, _) ->
      let param = to_coretype param in
      let body = to_coretype body in
      Ptyp_arrow (label, param, body)
  | Ttuple ts -> Ptyp_tuple (List.map to_coretype ts)
  | Tconstr (path, args, _) ->
      Ptyp_constr (to_lident path, List.map to_coretype args)
  | Tlink typ | Tsubst typ -> to_coretype_desc typ
  | _ -> unimplemented Printtyp.type_expr typ __LINE__

let to_constant constant =
  match constant with
  | Const_int n -> Pconst_integer (string_of_int n, None)
  | _ -> failwith "TODO: constants not implemented"

let tconstrain_pattern pattern =
  Pat.constraint_
    (Untypeast.untype_pattern pattern)
    (to_coretype pattern.pat_type)

let untype_expression = Untypeast.untype_expression

let rec to_expression expr =
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
               {
                 pc_lhs = Untypeast.untype_pattern c_lhs;
                 pc_guard = Option.map untype_expression c_guard;
                 pc_rhs = untype_expression c_rhs;
               })
      in
      Exp.match_ (untype_expression exp) match_cases
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

let rec get_typed_struct : int -> structure_item_desc -> label list =
 fun indentation struct_item_desc ->
  match struct_item_desc with
  | Tstr_value (rec', [ vb ]) ->
      let pattern = vb.vb_pat in

      let expr = vb.vb_expr |> to_expression in

      let expr_type = vb.vb_expr.exp_type in

      [
        Format.asprintf "let %s %a : %a = %a\n"
          (if rec' = Recursive then "rec" else "")
          Pprintast.pattern
          (Untypeast.untype_pattern pattern)
          Printtyp.type_expr expr_type Pprintast.expression expr;
      ]
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
      [
        Format.asprintf "module %s = struct \n%s end" a
          (stringify_structure (indentation + 1) struc.str_items);
      ]
  | _ -> failwith "Not implemented"

and stringify_structure : int -> Typedtree.structure_item list -> string =
 fun indentation struc ->
  let rec repeat n s =
    match n with
    | 0 -> ""
    | x when x > 0 -> s ^ repeat (n - 1) s
    | _ -> assert false
  in
  struc
  |> List.map (fun si -> si.str_desc)
  |> List.map (get_typed_struct indentation)
  |> List.flatten
  |> List.map (( ^ ) (repeat (indentation * 2) " "))
  |> String.concat "\n"

let type_structure structure =
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
  | { mod_desc = Tmod_structure { str_items = module_expr; _ }; _ } ->
      module_expr
  | _ -> failwith "impossible"
(* let tcode = Typecore.type_exp env code *)

(* let scode = to_expression tcode |> Format.printf "%a\n%!" Pprintast.expression *)

let code =
  [%str
    let a = 1
    let b = a
    let add a = a + 1
    let rec add' (a, b) = a + b

    let fn l = match l with [] -> 1 | h :: t -> h + 1

    module M : sig
      val a : int
    end = struct
      let a = 1
    end]

let () = stringify_structure 0 (code |> type_structure) |> print_endline
