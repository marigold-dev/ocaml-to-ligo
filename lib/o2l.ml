open Ocaml_common
open Types
open Asttypes
open Ast_helper

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

and mapper =
  let tconstrain_pattern pattern =
    Pat.constraint_
      (Untypeast.untype_pattern pattern)
      (ct_of_te pattern.pat_type)
  in
  let untype_expression_expanded mapper expr =
    let recurse = Untypeast.(mapper.expr) mapper in
    let open Typedtree in
    match expr.exp_desc with
    | Texp_let (rec', [ vb ], e) ->
        let untyped_value_binding =
          Parsetree.
            {
              pvb_pat = tconstrain_pattern vb.vb_pat;
              pvb_expr = recurse vb.vb_expr;
              pvb_attributes = vb.vb_attributes;
              pvb_loc = vb.vb_loc;
            } in
        Exp.let_ rec' [ untyped_value_binding ] (recurse e)
    | Texp_function { arg_label; param = _; cases; partial = _ } ->
        let pattern, guard, body =
          match cases with
          | [ { c_lhs; c_guard; c_rhs } ] ->
              (tconstrain_pattern c_lhs, c_guard, c_rhs)
          | _ ->
              let pexpr = untype_expression expr in
              Format.printf "function not supported. Representation is %a\n"
                (Printast.expression 0) pexpr;
              unimplemented Pprintast.expression pexpr __LINE__
        in
        Exp.fun_ arg_label (Option.map recurse guard) pattern (recurse body)
    | _ -> Untypeast.default_mapper.expr mapper expr
  in
  { Untypeast.default_mapper with expr = untype_expression_expanded }

and untype_expression a = Untypeast.untype_expression ~mapper a

let loc = Location.none


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

      let expr = vb.vb_expr |> mapper.expr mapper in

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
  | {
   mod_desc =
     Tmod_constraint
       ( { mod_desc = Tmod_structure { str_items = struc; str_final_env; _ }; _ },
         _,
         _,
         _ );
   _;
  } ->
      (struc, str_final_env)
  | { mod_desc; _ } -> (
      match mod_desc with
      | Tmod_ident _ -> failwith "Tmod_ident impossible"
      | Tmod_structure _ -> failwith "Tmod_structure impossible"
      | Tmod_functor _ -> failwith "Tmod_functor impossible"
      | Tmod_apply _ -> failwith "Tmod_apply impossible"
      | Tmod_constraint _ -> failwith "Tmod_constraint impossible"
      | Tmod_unpack _ -> failwith "Tmod_unpack impossible")