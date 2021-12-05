open Ocaml_common
open Asttypes

[@@@warning "-34"]
let () = Printexc.record_backtrace true

let loc = Location.none

open Parsetree


let code =
  [%str
(* anyone can defend a commit *)
let current_level = Tezos.level

let sender = Tezos.sender

type level = nat
type step = nat

(* TODO: calculate worse scenarion, how much money honest needs *)
(* TODO: submit hash only *)
type submission = bytes
type state_hash = bytes
type rejection = { operation_id : int; proof : bytes }

(* TODO: put all required money to be a honest validator on the contract before starting rejections or commits *)

(* TODO: commits are allowed to also clean a level to avoid paying for increasing the storage *)

(* TODO: batch parameter to be more efficient in gas*)
type rejection_game_id = nat
type new_rejection_game = {
  level : level;
  state_hash : state_hash;
  steps : step;
}
type parameter =
  (* users *)
  | Submit of submission
  (* validators *)
  | Join
  | Exit
  | Commit of level * state_hash * step
  (* | Trust_commit of level * state_hash *)
  | Start_rejection_game of new_rejection_game
  | Define_steps_for_rejection_game of rejection_game_id * step
  | Send_middle_hash of rejection_game_id * state_hash

(* TODO: split in two state machines, rejection game and optimistic rollup *)

type commitments = {
  finalized_after : level;
  state_hashes : (state_hash, address) map;
}

let stake_amount : mutez = assert false
let commitment_amount : tez = (* 1000tz *) assert false

module Collateral_vault : sig
  type t

  val has_stake : address -> t -> bool
end = struct
  type t = (address, unit) big_map

  let has_stake (address : address) (t : t) = Big_map.mem address t
end

let a : int = let b = 1 in b
let c = let d : int = 1 in d
let e = Collateral_vault.has_stake
]

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
  
  let ( + ) : nat -> nat -> nat = assert false
  
  let ( - ) : nat -> nat -> int = assert false
  
  let ( * ) : nat -> nat -> nat = assert false
  
  let ( < ) : nat -> nat -> bool = assert false
  
  let ( >= ) : nat -> nat -> bool = assert false
  
  let min : nat -> nat -> nat = assert false
  
  let ( / ) : nat -> nat -> (nat * nat) option = assert false
  
  let abs : int -> nat = assert false
  
  module Tezos = struct
    let level : nat = assert false
  
    let amount : tez = assert false
  
    let sender : address = assert false
  
    let transaction : 'parameter -> mutez -> 'parameter contract -> operation =
      assert false
  
    let get_contract_opt : address -> 'parameter contract option = assert false
  end
  
  module rec Big_map : sig
    val empty : ('key, 'value) big_map
  
    val find_opt : 'key -> ('key, 'value) big_map -> 'value option
  
    val add : 'key -> 'value -> ('key, 'value) big_map -> ('key, 'value) big_map
  
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
  end
]

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

(* Inject the defined stdlib into the environment via type_structure *)
let env = O2l.type_structure ~env stdlib |> snd



(* Pass the AST generated from the input to type_structure using the generated env.
   Extract the structure item list and stringify it, then print the result *)
let () =
  (* Sys.argv.(1) (* .ml file to convert to .mligo *)
    |> Pparse.parse_implementation ~tool_name:"O2L" *)
  code |> O2l.type_structure ~env |> fst |> (*O2l.module_erasure |>*) O2l.typed_string_of_code
  |> print_endline