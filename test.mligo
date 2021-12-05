let current_level : nat = Tezos.level
type level = nat
type step = nat
type submission = bytes
type state_hash = bytes
type rejection = {
  operation_id: int ;
  proof: bytes }
type rejection_game_id = nat
type new_rejection_game =
  {
  level: level ;
  state_hash: state_hash ;
  steps: step }
type parameter =
  | Submit of submission 
  | Join 
  | Exit 
  | Commit of level * state_hash * step 
  | Start_rejection_game of new_rejection_game 
  | Define_steps_for_rejection_game of rejection_game_id * step 
  | Send_middle_hash of rejection_game_id * state_hash 
type commitments =
  {
  finalized_after: level ;
  state_hashes: (state_hash, address) map }
let stake_amount : mutez = assert false
let commitment_amount : tez = assert false
module Collateral_vault =
  struct
    type t = (address, unit) big_map
    let has_stake : address -> t -> bool =
      fun (address : address) -> fun (t : t) -> Big_map.mem address t
  end
let a : int = let b : int = 1 in b
let c : int = let d : int = 1 in d
let e : address -> Collateral_vault.t -> bool = Collateral_vault.has_stake