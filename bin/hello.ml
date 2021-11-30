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
  | Texp_function
      {
        arg_label = Nolabel;
        (* TODO: what is this??? *)
        param;
        cases =
          [
            {
              c_lhs =
                {
                  pat_desc = Tpat_var (param', _);
                  pat_loc = _;
                  pat_extra = [];
                  pat_type;
                  pat_env = _;
                  pat_attributes = [];
                };
              c_guard = None;
              c_rhs = body;
            };
          ];
        partial = Total;
      } ->
      assert (param = param');
      let param =
        Pat.constraint_
          (Pat.var (mkloc (Ident.name param)))
          (to_coretype pat_type)
      in
      Exp.fun_ Nolabel None param (to_expression body)
  | Texp_function
      {
        arg_label = Nolabel;
        (* TODO: what is this??? *)
        param = _;
        cases =
          [
            {
              c_lhs =
                {
                  pat_desc = Tpat_tuple params;
                  pat_loc = p;
                  pat_extra = [];
                  pat_type = _;
                  pat_env = _;
                  pat_attributes = [];
                };
              c_guard = None;
              c_rhs = body;
            };
          ];
        partial = Total;
      } ->
      let params =
        List.map
          (function
            | Typedtree.{ pat_desc = Tpat_var (param, _); pat_type; _ } ->
                Pat.constraint_
                  (Pat.var (mkloc (Ident.name param)))
                  (to_coretype pat_type)
            | _ ->
                let pexpr = Untypeast.untype_expression expr in
                unimplemented Pprintast.expression pexpr __LINE__)
          params
      in
      Exp.fun_ Nolabel None
        {
          ppat_desc = Ppat_tuple params;
          ppat_loc = p;
          ppat_loc_stack = [];
          ppat_attributes = [];
        }
        (to_expression body)
  | Texp_function _ ->
      let pexpr = Untypeast.untype_expression expr in
      let () =
        Format.printf "function not supported. Representation is %a\n"
          (Printast.expression 0) pexpr
      in
      unimplemented Pprintast.expression pexpr __LINE__
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

let code = [%str let add (a, b) = a + b]

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

(* Need to write a function that behaves the same as Pprintast.structure that behaves the same except
   doesn't print the module type when pritning a module
   also it needs to do other things like make function arguments explicit but that's already done
   by eduardo for individual expressions, so just need to map the structure list and apply that
   change before printing *)

(* let () = Format.printf "%a\n" Pprintast.structure code *)

let print_typed_struct struct_item_desc =
  match struct_item_desc with
  | Pstr_value (_, vbl) ->
      vbl
      |> List.map (fun vb ->
             ( vb.pvb_pat,
               to_expression (Typecore.type_exp env vb.pvb_expr),
               Typecore.type_exp env vb.pvb_expr ))
      |> List.map (fun (pattern, exp, typed_exp) ->
             Format.asprintf "%a : %a = %a\n" Pprintast.pattern pattern
               Printtyp.type_expr typed_exp.exp_type Pprintast.expression exp)
      |> List.iter (Format.printf "let %s\n")
  | _ -> failwith "Not implemented"

(* let tcode = Typecore.type_exp env code *)

(* let scode = to_expression tcode |> Format.printf "%a\n%!" Pprintast.expression *)

let () =
  code |> List.map (fun si -> si.pstr_desc) |> List.iter print_typed_struct
