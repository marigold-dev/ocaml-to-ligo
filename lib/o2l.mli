(* Processes a list of Typedtree.structure_items, representing an arbitrary
   snippet of code, into a string of valid CameLIGO code. *)
val typed_string_of_code : Typedtree.structure -> string

(* Type constrains an arbitrary snippet of partially or totally
   untyped code in a Parsetree.structure. Optionally injects
   a custom environment into the tree. Returns a tuple
   of the type constrained code with the new environment
   after processing the given code. *)
val type_structure :
  env:Env.t -> Parsetree.structure -> Typedtree.structure * Env.t
