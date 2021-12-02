open Ocaml_common
open Types
open Asttypes

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

let rec ct_of_te typ = Option.map ct_of_desc (ct_desc_of_te typ)

and ct_desc_of_te te : Parsetree.core_type_desc option =
  let ( let* ) = Option.bind in
  let rec mapM f = function
    | [] -> Some []
    | x :: y ->
        let* x = f x in
        let* y = mapM f y in
        Some (x :: y)
  in
  match te.desc with
  | Tarrow (label, param, body, _) ->
      let* param = ct_of_te param in
      let* body = ct_of_te body in
      Some (Parsetree.Ptyp_arrow (label, param, body))
  | Ttuple ts ->
      let* ts = mapM ct_of_te ts in
      Some (Parsetree.Ptyp_tuple ts)
  | Tconstr (path, args, _) ->
      let* args = mapM ct_of_te args in
      Some (Parsetree.Ptyp_constr (to_lident path, args))
  | Tlink typ | Tsubst typ -> ct_desc_of_te typ
  | Tvar _ -> None
  | _ -> unimplemented Printtyp.type_expr te __LINE__

and mapper =
  let tconstrain_pattern (loc : Warnings.loc)
      (pattern : 'a Typedtree.general_pattern) =
    match ct_of_te pattern.pat_type with
    | Some ct ->
        Ast_helper.Pat.constraint_ (Untypeast.untype_pattern pattern) ct
    | None ->
        failwith
          (Format.asprintf
             "You have a type variable at %a, you must give it an annotation \
              of a concrete type"
             Location.print_loc loc)
  in
  let untype_expression_expanded mapper expr =
    let loc = Typedtree.(expr.exp_loc) in
    let tconstrain_pattern = tconstrain_pattern loc in
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
            }
        in
        Ast_helper.Exp.let_ rec' [ untyped_value_binding ] (recurse e)
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
        Ast_helper.Exp.fun_ arg_label (Option.map recurse guard) pattern
          (recurse body)
    | _ -> Untypeast.default_mapper.expr mapper expr
  in
  let untype_structure_item_expanded (mapper : Untypeast.mapper) structure :
      Parsetree.structure_item =
    match structure with
    | Typedtree.
        {
          str_desc =
            Tstr_module
              {
                mb_name;
                mb_expr =
                  { mod_desc = Tmod_constraint ((_ as mod_expr), _, _, _); _ };
                _;
              };
          _;
        } ->
        mod_expr |> mapper.module_expr mapper |> Ast_helper.Mb.mk mb_name
        |> Ast_helper.Str.module_
    | m -> Untypeast.default_mapper.structure_item mapper m
  in
  let untype_structure_expanded mapper Typedtree.{ str_items; _ } =
    List.map (untype_structure_item_expanded mapper) str_items
  in
  {
    Untypeast.default_mapper with
    expr = untype_expression_expanded;
    structure_item = untype_structure_item_expanded;
    structure = untype_structure_expanded;
  }

and untype_expression a = Untypeast.untype_expression ~mapper a

let loc = Location.none

let rec string_of_expr expr =
  let parentheses_pattern = Str.regexp "[)(]" in
  let remove_parens = Str.global_replace parentheses_pattern "" in
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_let (rec', [ vb ], e) ->
      let pattern_string =
        vb.pvb_pat |> Format.asprintf "%a" Pprintast.pattern |> remove_parens
      in
      let expr = vb.pvb_expr in

      Format.sprintf "let %s%s = %s in %s"
        (if rec' = Recursive then "rec " else "")
        pattern_string (string_of_expr expr) (string_of_expr e)
  | Pexp_fun (arg_label, default_arg, pattern, e) ->
      (* Print the lambda but stringify the body with this function, recursively *)
      let arg_pattern =
        match (arg_label, default_arg) with
        | Nolabel, _ -> Format.asprintf "%a" Pprintast.pattern pattern
        | Labelled l, _ -> Format.asprintf "~%s:%a" l Pprintast.pattern pattern
        | Optional l, None ->
            Format.asprintf "?%s:%a" l Pprintast.pattern pattern
        | Optional l, Some default ->
            Format.asprintf "?%s:(%a = %s)" l Pprintast.pattern pattern
              (string_of_expr default)
      in
      Format.sprintf "fun %s -> %s" arg_pattern (string_of_expr e)
  | _ ->
      Printf.printf "Defaulting for %s\n" (Pprintast.string_of_expression expr);
      Pprintast.string_of_expression expr

let typed_string_of_struct (Typedtree.{ str_desc = sid; _ } as si) =
  match sid with
  | Tstr_value (rec', [ vb ]) ->
      let pattern = vb.vb_pat in

      let expr = vb.vb_expr |> mapper.expr mapper in

      let expr_type = vb.vb_expr.exp_type in

      Format.asprintf "let %s%a : %a = %s\n"
        (if rec' = Recursive then "rec " else "")
        Pprintast.pattern
        (Untypeast.untype_pattern pattern)
        Printtyp.type_expr expr_type (string_of_expr expr)
  | Tstr_value (_, _) -> failwith "let ... and not implemented "
  | _ ->
      Pprintast.string_of_structure [ mapper.structure_item mapper si ] ^ "\n"

let typed_string_of_code sil =
  sil |> List.map typed_string_of_struct |> String.concat "\n"

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
