module Better_pprintast = Pprintast
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
  | Tpoly (expr, _) ->
      let* expr = ct_of_te expr in
      Some (Parsetree.Ptyp_poly ([], expr))
  | Tobject _ ->
      Printf.printf "Tobject\n";
      unimplemented Printtyp.type_expr te __LINE__
  | Tfield _ ->
      Printf.printf "Tfield\n";
      unimplemented Printtyp.type_expr te __LINE__
  | Tnil ->
      Printf.printf "Tnil\n";
      unimplemented Printtyp.type_expr te __LINE__
  | Tvariant _ ->
      Printf.printf "Tvariant\n";
      unimplemented Printtyp.type_expr te __LINE__
  | Tunivar _ ->
      Printf.printf "Tunivar\n";
      unimplemented Printtyp.type_expr te __LINE__
  | Tpackage _ ->
      Printf.printf "Tpackage\n";
      unimplemented Printtyp.type_expr te __LINE__

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
  let tconstrain_pattern
        : 'a.
          Untypeast.mapper -> 'a Typedtree.general_pattern -> Parsetree.pattern
      =
   fun _mapper pattern ->
    match Untypeast.default_mapper.pat Untypeast.default_mapper pattern with
    | { ppat_desc = Ppat_constraint _; _ } as untyped_pat -> untyped_pat
    | untyped_pat -> (
        match ct_of_te pattern.pat_type with
        | Some ct -> Ast_helper.Pat.constraint_ untyped_pat ct
        | None ->
            failwith
              (Format.asprintf
                 "You have a type variable at %a, you must annotate it with a \
                  concrete type"
                 Location.print_loc pattern.pat_loc))
  in
  let untype_expression_expanded mapper expr =
    let open Typedtree in
    let open Untypeast in
    let recurse = mapper.expr mapper in
    match expr.exp_desc with
    | Texp_let (rec', vbs, e) ->
        let vbs =
          List.map
            (function
              | Typedtree.{ vb_pat = { pat_extra; _ }; _ } as vb
                when (not
                        (List.for_all
                           (function
                             | Tpat_constraint _, _, _ -> false | _ -> true)
                           pat_extra))
                     && List.length pat_extra != 0 ->
                  mapper.value_binding mapper vb
              | vb -> (
                  let vb_untyped = mapper.value_binding mapper vb in
                  match ct_of_te vb.vb_expr.exp_type with
                  | Some ct ->
                      {
                        vb_untyped with
                        pvb_pat =
                          {
                            vb_untyped.pvb_pat with
                            ppat_desc =
                              Ppat_constraint
                                ( vb_untyped.pvb_pat,
                                  Parsetree.Ptyp_poly ([], ct)
                                  |> Ast_helper.Typ.mk );
                          };
                      }
                  | None ->
                      failwith
                        "inferred type was polymorphic, need type annotation \
                         of an explicit type & better error message"))
            vbs
          (*Parsetree.
            {
              pvb_pat = mapper.pat mapper vb.vb_pat;
              pvb_expr = recurse vb.vb_expr;
              pvb_attributes = vb.vb_attributes;
              pvb_loc = vb.vb_loc;
            }*)
        in
        Ast_helper.Exp.let_ rec' vbs (recurse e)
    | Texp_function { arg_label; param = _; cases; partial = _ } ->
        let pattern, guard, body =
          match cases with
          | [ { c_lhs; c_guard; c_rhs } ] ->
              (tconstrain_pattern mapper c_lhs, c_guard, c_rhs)
          | _ ->
              let pexpr = recurse expr in
              Format.printf "function not supported. Representation is %a\n"
                (Printast.expression 0) pexpr;
              unimplemented Better_pprintast.expression pexpr __LINE__
        in
        Ast_helper.Exp.fun_ arg_label (Option.map recurse guard) pattern
          (recurse body)
    | Texp_ident (_, _, _) ->
        Untypeast.default_mapper.expr mapper expr
        (* here, we need to rewrite the name using the path, also need to do it in patterns *)
    | _ -> Untypeast.default_mapper.expr mapper expr
  in
  let untype_structure_item_expanded (mapper : Untypeast.mapper) structure :
      Parsetree.structure_item =
    let open Typedtree in
    match structure.str_desc with
    | Tstr_module
        {
          mb_name;
          mb_expr = { mod_desc = Tmod_constraint (mod_expr, _, _, _); _ };
          _;
        } ->
        mod_expr |> mapper.module_expr mapper |> Ast_helper.Mb.mk mb_name
        |> Ast_helper.Str.module_
    | Tstr_value (recflag, value_bindings) ->
        let value_bindings =
          List.filter_map
            (fun binding ->
              match mapper.value_binding mapper binding with
              | { pvb_pat = { ppat_desc = Ppat_constraint _; _ }; _ } as
                untyped_binding ->
                  Some untyped_binding
              | untyped_binding -> (
                  if
                    List.exists
                      (fun x -> x.Parsetree.attr_name.txt = "ligo.disable")
                      untyped_binding.pvb_attributes
                  then None
                  else
                    match ct_of_te Typedtree.(binding.vb_expr.exp_type) with
                    | Some ct ->
                        Some
                          Parsetree.
                            {
                              untyped_binding with
                              pvb_pat =
                                {
                                  untyped_binding.pvb_pat with
                                  ppat_desc =
                                    Ppat_constraint
                                      ( untyped_binding.pvb_pat,
                                        Ptyp_poly ([], ct) |> Ast_helper.Typ.mk
                                      );
                                };
                            }
                    | None ->
                        failwith
                          (Format.asprintf
                             "You have a type variable at %a, you must \
                              annotate it with a concrete type"
                             Location.print_loc binding.vb_loc)))
            value_bindings
        in
        value_bindings |> Ast_helper.Str.value recflag
    | _ -> Untypeast.default_mapper.structure_item mapper structure
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

(* User facing function that maps typed_string_of_struct to a list
   of structure items and joins them with newlines. Essentially this
   processes an arbitrary snippet of OCaml code *)
let typed_string_of_code struc =
  struc |> mapper.structure mapper
  |> Format.asprintf "%a" Better_pprintast.structure

let default_environment =
  Compmisc.init_path ();
  Compmisc.initial_env ()

let module_erasure (structure : Typedtree.structure) =
  {
    structure with
    str_items =
      structure.str_items
      |> List.map (function
           | Typedtree.
               {
                 str_desc =
                   Tstr_module
                     {
                       mb_expr =
                         { mod_desc = Tmod_structure { str_items; _ }; _ };
                       _;
                     };
                 _;
               } ->
               str_items
           | Typedtree.
               {
                 str_desc =
                   Tstr_module
                     {
                       mb_expr =
                         {
                           mod_desc =
                             Tmod_constraint
                               ( {
                                   mod_desc = Tmod_structure { str_items; _ };
                                   _;
                                 },
                                 _,
                                 _,
                                 _ );
                           _;
                         };
                       _;
                     };
                 _;
               } ->
               str_items
           | x -> [ x ])
      |> List.flatten;
  }

let type_structure ?(env = default_environment) structure =
  try
    let tstr, _, _, env = Typemod.type_structure env structure in
    Warnings.check_fatal ();
    (tstr, env)
  with exn -> (
    match Location.error_of_exn exn with
    | Some (`Ok report) ->
        Format.eprintf "%a\n%!" Location.print_report report;
        exit 0
    | Some _ -> assert false
    | None -> assert false)
