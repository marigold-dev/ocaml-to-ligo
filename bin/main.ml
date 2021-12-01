open Ocaml_common
open Asttypes

[@@@warning "-34"]
let () = Printexc.record_backtrace true

let loc = Location.none

open Parsetree

let code =
  [%str
    let a = Tezos.level
    let a = 1
    let b = a
    let add a = a + 1

    let sum_list l =
      let rec aux l' acc =
        match l' with [] -> acc | h :: t -> aux t (acc + h)
      in
      aux l 0

    let rec add' (a, b) = a + b

    let l =
      let a = 1 in
      a

    type my_variant = VarA | VarB

    type my_record = { field1 : int; field2 : string }

    module M : sig
      val a : int
    end = struct
      let a = 1
    end]

let stdlib =
  [%str
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

      let transaction : 'parameter -> mutez -> 'parameter contract -> operation
          =
        assert false

      let get_contract_opt : address -> 'parameter contract option =
        assert false
    end

    module rec Big_map : sig
      val empty : ('key, 'value) big_map

      val find_opt : 'key -> ('key, 'value) big_map -> 'value option

      val add :
        'key -> 'value -> ('key, 'value) big_map -> ('key, 'value) big_map

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
    end]

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

(* Inject the defined stdlib into the environment via type_structure *)
let env = O2l.type_structure env stdlib |> snd

(* Pass the AST generated from the input to type_structure using the generated env.
   Extract the structure item list and stringify it, then print the result *)
let () =
  code |> O2l.type_structure env |> fst |> O2l.stringify_structure 0
  |> print_endline