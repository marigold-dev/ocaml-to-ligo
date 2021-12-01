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

let rec to_expression expr =
  match expr.exp_desc with
  | Texp_ident (_, lident, _) -> Exp.ident lident
  | Texp_constant constant -> Exp.constant (to_constant constant)
  | Texp_function { arg_label; param = _; cases; partial = _ } ->
      let pattern, guard, body =
        match cases with
        | [ { c_lhs; c_guard; c_rhs } ] ->
            ( Pat.constraint_
                (Untypeast.untype_pattern c_lhs)
                (to_coretype c_lhs.pat_type),
              c_guard,
              c_rhs )
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
      (*
  none of this is actually necessary
  | Texp_letmodule (Some ident, _loc , _module_presence , module_expr , expression) ->
    let pexpr = Untypeast.untype_expression expression in
    let () = Format.printf "%a" Pprintast.expression pexpr in
    Exp.letmodule (Exp.ident ident) (failwith "need to do smth with module_expr") pexpr
   | Texp_letmodule _ ->
      let pexpr = Untypeast.untype_expression expr in
      let () = Format.printf "want to remove the letmodule: %a\n%!" Pprintast.expression in
      sounds good, doesn't work 
      pexpr*)
  | _ ->
      let pexpr = Untypeast.untype_expression expr in
      unimplemented Pprintast.expression pexpr __LINE__

and to_arg arg =
  match arg with
  | Nolabel, Some expr -> (Nolabel, to_expression expr)
  | _ -> failwith "TODO: implement this"

(* let rec to_module_expr md_expr =
     match md_expr.mod_desc with
     | Tmod_structure structure -> ()
     | Tmod_constraint (md_expr, _md_type, _md_constraint, _md_coercion) ->
         to_module_expr md_expr
     | _ -> failwith "TODO: not implemented"

   and to_structure_item stri =
     match stri.str_desc with
     | Tstr_value (rec_flag, binding) -> ()
     | _ -> failwith "TODO: not implemented" *)
let loc = Location.none

let code =
  [%str
    let a = 1
    let add a = a + 1
    let rec add' (a, b) = a + b

    module M : sig
      val a : int
    end = struct
      let a = 1
    end]

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

(* Need to write a function that behaves the same as Pprintast.structure that behaves the same except
   doesn't print the module type when pritning a module
   also it needs to do other things like make function arguments explicit but that's already done
   by eduardo for individual expressions, so just need to map the structure list and apply that
   change before printing *)

(* let () = Format.printf "%a\n" Pprintast.structure code *)

let rec get_typed_struct indentation struct_item_desc =
  match struct_item_desc with
  | Pstr_value (rec', [ vb ]) ->
      let pattern = vb.pvb_pat in

      let expr = vb.pvb_expr |> Typecore.type_exp env |> to_expression in

      let expr_type =
        Typecore.type_exp env vb.pvb_expr |> fun e -> e.exp_type
      in

      [
        Format.asprintf "let %s%a : %a = %a\n"
          (if rec' = Recursive then "rec " else "")
          Pprintast.pattern pattern Printtyp.type_expr expr_type
          Pprintast.expression expr;
      ]
  | Pstr_value (_, _) -> failwith "let ... and not implemented "
  | Pstr_module
      {
        pmb_name = { txt = Some a; _ };
        pmb_expr =
          {
            pmod_desc =
              ( Pmod_structure struc
              | Pmod_constraint ({ pmod_desc = Pmod_structure struc; _ }, _) );
            _;
          };
        _;
      } ->
      [
        Format.asprintf "module %s = struct \n%s end" a
          (stringify_structure (indentation + 1) struc);
      ]
  | _ -> failwith "Not implemented"

and stringify_structure indentation struc =
  let rec repeat n s =
    match n with
    | 0 -> ""
    | x when x > 0 -> s ^ repeat (n - 1) s
    | _ -> assert false
  in
  struc
  |> List.map (fun si -> si.pstr_desc)
  |> List.map (get_typed_struct indentation)
  |> List.flatten
  |> List.map (( ^ ) (repeat (indentation * 2) " "))
  |> String.concat "\n"
(* let tcode = Typecore.type_exp env code *)

(* let scode = to_expression tcode |> Format.printf "%a\n%!" Pprintast.expression *)

let () = stringify_structure 0 code |> print_endline
