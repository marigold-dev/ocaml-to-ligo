open Ocaml_common
open Types

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
  | Tpoly (expr, _) -> let* expr = ct_of_te expr in Some (Parsetree.Ptyp_poly ([], expr))
  | Tobject _ -> let () = Printf.printf "Tobject\n" in unimplemented Printtyp.type_expr te __LINE__
  | Tfield _ -> let () = Printf.printf "Tfield\n" in unimplemented Printtyp.type_expr te __LINE__
  | Tnil -> let () = Printf.printf "Tnil\n" in unimplemented Printtyp.type_expr te __LINE__
  | Tvariant _ -> let () = Printf.printf "Tvariant\n" in unimplemented Printtyp.type_expr te __LINE__
  | Tunivar _ -> let () = Printf.printf "Tunivar\n" in unimplemented Printtyp.type_expr te __LINE__
  | Tpackage _ -> let () = Printf.printf "Tpackage\n" in unimplemented Printtyp.type_expr te __LINE__
  
  (*
  | Tlink typ | Tsubst typ -> ct_desc_of_te typ
  | Tvar _ -> None
  | Tobject _ -> failwith "Tobject"
  | Tfield _ -> failwith "Tfield"
  | Tnil -> failwith "Tnil"
  | Tvariant _ -> failwith "Tvariant"
  | Tunivar _ -> failwith "Tunivar"
  | Tpackage _ -> failwith "Tpackage"
  *)

(* Defines a custom "mapper" that transforms any Typedtree type
   to its corresponding Parsetree type, but with explicit types *)
and mapper =
  (* Takes a Typedtree pattern and transforms it into
     a corresponding Parsetree pattern that's "constrained",
     i.e. includes an explicit type annotation *)
  let tconstrain_pattern (loc : Warnings.loc)
      (pattern : 'a Typedtree.general_pattern) =
    match ct_of_te pattern.pat_type with
    | Some ct ->
        Ast_helper.Pat.constraint_ (Untypeast.untype_pattern pattern) ct
    | None ->
        failwith
          (Format.asprintf
             "You have a type variable at %a, you must annotate it with \
              a concrete type"
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
        } ->        mod_expr |> mapper.module_expr mapper |> Ast_helper.Mb.mk mb_name
        |> Ast_helper.Str.module_
    | Typedtree.{ str_desc = Tstr_value (recflag, value_bindings); _ } ->
        let value_bindings =
          List.map
            (fun binding ->
              Parsetree.
                {
                  (mapper.value_binding mapper binding) with
                  pvb_pat = tconstrain_pattern binding.vb_loc binding.vb_pat;
                })
            value_bindings
        in
        value_bindings |> Ast_helper.Str.value recflag
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


and typed_string_of_code : Typedtree.structure -> string =
 fun struc -> struc |> mapper.structure mapper |> Format.asprintf "%a" Pprintast.structure 

let type_structure ~env structure =
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
  | { mod_desc = Tmod_structure ({ str_final_env; _ } as structure); _ } ->
      (structure, str_final_env)
  | {
   mod_desc =
     Tmod_constraint
       ( { mod_desc = Tmod_structure ({  str_final_env; _ } as structure); _ },
         _,
         _,
         _ );
   _;
  } ->
      (structure, str_final_env)
  | { mod_desc; _ } -> (
      match mod_desc with
      | Tmod_ident _ -> failwith "Tmod_ident impossible"
      | Tmod_structure _ -> failwith "Tmod_structure impossible"
      | Tmod_functor _ -> failwith "Tmod_functor impossible"
      | Tmod_apply _ -> failwith "Tmod_apply impossible"
      | Tmod_constraint _ -> failwith "Tmod_constraint impossible"
      | Tmod_unpack _ -> failwith "Tmod_unpack impossible")

