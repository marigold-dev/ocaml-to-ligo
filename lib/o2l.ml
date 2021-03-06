module Better_pprintast = Pprintast
open Ocaml_common
open Types

module H = Hashtbl.Make (struct
  type t = string

  let hash = Hashtbl.hash

  let equal = String.equal
end)

type stored_module =
  | Functor of string option * Typedtree.module_expr
  | Mod of Typedtree.module_expr

let init = H.create 10

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
          mb_name = name;
          mb_expr =
            {
              mod_desc =
                Tmod_functor
                  ( Named (_, param_name, _),
                    ({ mod_desc = Tmod_structure _; _ } as x) );
              mod_env = _env;
              _;
            };
          _;
        } ->
        let name = Option.get name.txt in
        let entry = Functor (param_name.txt, x) in
        H.add init name entry;
        Untypeast.default_mapper.structure_item mapper structure
    | Tstr_module
        { mb_name = name; mb_expr = { mod_desc = Tmod_structure _; _ } as x; _ }
      ->
        let name = Option.get name.txt in
        H.add init name (Mod x);
        Untypeast.default_mapper.structure_item mapper structure
    | Tstr_module
        {
          mb_expr =
            {
              mod_desc =
                Tmod_apply
                  ( { mod_desc = Tmod_ident (path, _); _ },
                    { mod_desc = Tmod_ident (path', _); _ },
                    _ );
              mod_env = _env;
              _;
            };
          _;
        } ->
        let[@warning "-8"] (Functor (param, outer)) =
          H.find init (Path.name path)
        in
        let[@warning "-8"] (Mod inner) = H.find init (Path.name path') in
        let inner_mod =
          inner |> mapper.module_expr mapper
          |> Ast_helper.Mb.mk { txt = param; loc }
          |> Ast_helper.Str.module_
        in
        let[@warning "-8"] ({
                              Parsetree.pstr_desc =
                                Pstr_module
                                  ({
                                     pmb_expr =
                                       {
                                         pmod_desc = Pmod_structure structure;
                                         _;
                                       } as mod_expr;
                                     _;
                                   } as pstr_mod);
                              _;
                            } as desc) =
          outer |> mapper.module_expr mapper
          |> Ast_helper.Mb.mk { txt = Some (Path.name path); loc }
          |> Ast_helper.Str.module_
        in
        let mod_expr =
          { mod_expr with pmod_desc = Pmod_structure (inner_mod :: structure) }
        in
        let new_name =
          Format.asprintf "%s_%s" (Path.name path) (Option.get param)
        in
        let pstr_mod =
          Parsetree.Pstr_module
            {
              pstr_mod with
              Parsetree.pmb_expr = mod_expr;
              pmb_name = { txt = Some new_name; loc };
            }
        in
        let desc = { desc with pstr_desc = pstr_mod } in
        desc
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

let functor_erasure (structure : Typedtree.structure) =
  let matcher structure =
    match structure.Typedtree.str_desc with
    | Tstr_module
        {
          mb_name = name;
          mb_expr =
            {
              mod_desc =
                Tmod_functor
                  ( Named (_, param_name, _),
                    ({ mod_desc = Tmod_structure _; _ } as x) );
              mod_env = _env;
              _;
            };
          _;
        } ->
        let name = Option.get name.txt in
        let entry = Functor (param_name.txt, x) in
        H.add init name entry;
        None
    | Tstr_module
        { mb_name = name; mb_expr = { mod_desc = Tmod_structure _; _ } as x; _ }
      ->
        let name = Option.get name.txt in
        H.add init name (Mod x);
        Some structure
    | Tstr_module
        {
          mb_expr =
            {
              mod_desc =
                Tmod_apply
                  ( { mod_desc = Tmod_ident (path, _); _ },
                    { mod_desc = Tmod_ident (path', _); _ },
                    _ );
              mod_env = _env;
              _;
            };
          _;
        } ->
        let[@warning "-8"] (Functor (param, outer)) =
          H.find init (Path.name path)
        in
        let[@warning "-8"] (Mod inner) = H.find init (Path.name path') in
        let inner_mod =
          inner |> mapper.module_expr mapper
          |> Ast_helper.Mb.mk { txt = param; loc }
          |> Ast_helper.Str.module_
        in
        let[@warning "-8"] ({
                              Parsetree.pstr_desc =
                                Pstr_module
                                  ({
                                     pmb_expr =
                                       {
                                         pmod_desc = Pmod_structure structure;
                                         _;
                                       } as mod_expr;
                                     _;
                                   } as pstr_mod);
                              _;
                            } as desc) =
          outer |> mapper.module_expr mapper
          |> Ast_helper.Mb.mk { txt = Some (Path.name path); loc }
          |> Ast_helper.Str.module_
        in
        let mod_expr =
          { mod_expr with pmod_desc = Pmod_structure (inner_mod :: structure) }
        in
        let new_name =
          Format.asprintf "%s_%s" (Path.name path) (Option.get param)
        in
        let pstr_mod =
          Parsetree.Pstr_module
            {
              pstr_mod with
              Parsetree.pmb_expr = mod_expr;
              pmb_name = { txt = Some new_name; loc };
            }
        in
        let lst, _, _, _ =
          Typemod.type_structure _env [ { desc with pstr_desc = pstr_mod } ]
        in
        Some (List.hd lst.str_items)
    | _ ->
        let ret = structure in
        Some ret
  in
  let str_items = List.filter_map matcher structure.str_items in
  { structure with str_items }

let rename _prefix = function
  | { Typedtree.str_desc = Tstr_type (bind, types); _ } as x ->
      let open Typedtree in
      let typ =
        List.map
          (fun x ->
            let new_manifest =
              x.typ_manifest
              |> Option.map (function
                   | {
                       ctyp_type =
                         { desc = Tconstr (_, _type_expr, _ref'); _ } as _ctyp;
                       ctyp_desc =
                         Ttyp_constr (Path.Pdot (Path.Pident s, s'), _, cores);
                       _;
                     } as x
                     when String.uppercase_ascii (Ident.name s) = Ident.name s
                     ->
                       let path =
                         Path.Pident
                           (Ident.create_local
                              (Format.asprintf "%s_%s"
                                 (Ident.name s |> String.lowercase_ascii)
                                 s'))
                       in
                       {
                         x with
                         ctyp_desc = Ttyp_constr (path, to_lident path, cores);
                       }
                   | x -> x)
            in
            {
              x with
              Typedtree.typ_name =
                {
                  x.Typedtree.typ_name with
                  txt = Format.asprintf "%s_%s" _prefix x.typ_name.txt;
                };
              typ_manifest = new_manifest;
            })
          types
      in
      { x with str_desc = Tstr_type (bind, typ) }
  | { Typedtree.str_desc = Tstr_value (bind, values); _ } as x ->
      let open Typedtree in
      let values =
        List.map
          (fun x ->
            let rename_pat = function
              | { pat_desc = Tpat_var (ident, name); _ } as x ->
                  {
                    x with
                    pat_desc =
                      Tpat_var
                        ( ident,
                          {
                            name with
                            txt = Format.asprintf "%s_%s" _prefix name.txt;
                          } );
                  }
              | _ -> failwith "unimplemented"
            in
            let new_pat = rename_pat x.Typedtree.vb_pat in
            let new_expr =
              match x.vb_expr.exp_desc with
              | Texp_ident (Path.Pdot (Path.Pident s, s'), _, typ)
                when String.uppercase_ascii (Ident.name s) = Ident.name s ->
                  let path =
                    Path.Pident
                      (Ident.create_local
                         (Format.asprintf "%s_%s"
                            (Ident.name s |> String.lowercase_ascii)
                            s'))
                  in
                  Texp_ident (path, to_lident path, typ)
              | x -> x
            in
            {
              x with
              Typedtree.vb_pat = new_pat;
              vb_expr = { x.vb_expr with exp_desc = new_expr };
            })
          values
      in
      { x with str_desc = Tstr_value (bind, values) }
  | x -> x

let rec erase_mod = function
  | Typedtree.
      {
        str_desc =
          Tstr_module
            {
              mb_name;
              mb_expr = { mod_desc = Tmod_structure { str_items; _ }; _ };
              _;
            };
        _;
      } ->
      List.concat_map
        (fun x ->
          rename (mb_name.txt |> Option.get |> String.lowercase_ascii) x
          |> erase_mod)
        str_items
  | Typedtree.
      {
        str_desc =
          Tstr_module
            {
              mb_name;
              mb_expr =
                {
                  mod_desc =
                    Tmod_constraint
                      ( { mod_desc = Tmod_structure { str_items; _ }; _ },
                        _,
                        _,
                        _ );
                  _;
                };
              _;
            };
        _;
      } ->
      List.concat_map
        (fun x ->
          rename (mb_name.txt |> Option.get |> String.lowercase_ascii) x
          |> erase_mod)
        str_items
  | x -> [ x ]

let module_erasure (structure : Typedtree.structure) =
  {
    structure with
    str_items = structure.str_items |> List.concat_map erase_mod;
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
