open Ocaml_common
open Asttypes
open Parsetree

let () = Printexc.record_backtrace true

let loc = Location.none

let ex =
  [%str
    let x = 3
    let[@ligo.disable] x = 4
    let f2 () = 3
    let[@ligo.disable] f2 () = 4
    module X = struct
      type t = int
      let x = 5
    end
    module Y = struct
      type t = X.t
      let x = X.x
    end
    module Make (K : sig
      type t
    end) =
    struct
      type t = K.t
    end
    module M = Make (X)]

let code =
  [%str
    type hash = bytes
    module Pool : sig
      type t

      (* O(1) *)
      val empty : unit -> t

      (* O(1) *)
      val hash : t -> hash

      (* O(1) *)
      val push : nat -> t -> t

      (* O(1) *)
      val pop : t -> (nat * t) option

      (* O(1) *)
      val is_empty : t -> bool

      (* O(1) *)
      val single_step_data : t -> t option
    end = struct
      type t = (nat * hash) list

      let empty_hash () = Crypto.blake2b (Bytes.pack ([] : nat list))
      let empty () : t = []

      let hash (t : t) =
        match t with [] -> empty_hash () | (_el, hash) :: _ -> hash

      let push (el : nat) (t : t) : t =
        let hash_of_t = hash t in
        let hash_of_el = Crypto.blake2b (Bytes.pack el) in
        let hash = Crypto.blake2b (Bytes.concat hash_of_el hash_of_t) in
        (el, hash) :: t
      let pop (t : t) =
        match t with [] -> None | (el, _) :: tl -> Some (el, tl)

      let is_empty (t : t) = match t with [] -> true | _ -> false

      let single_step_data (t : t) : t option =
        match t with [] -> None | el :: _tl -> Some [ el ]
    end

    (* TODO: attack vector, nat is unbounded, so very large state *)
    type t = { level : nat; steps : nat; counter : nat; pool : Pool.t }

    let single_step_data t =
      let { level; steps; counter; pool } = t in
      match Pool.single_step_data pool with
      | Some pool -> Some { level; steps; counter; pool }
      | None -> None

    let halted t = Pool.is_empty t.pool
    let execute_step t =
      let { level; steps; counter; pool } = t in
      match Pool.pop pool with
      | None ->
          (* can only happen if not enough data was sent *)
          assert false
      | Some (el, pool) ->
          let steps = steps + 1n in
          let counter = el + counter in
          { level; steps; counter; pool }

    let hash (t : t) =
      let steps = t.steps in
      let counter = t.counter in
      let pool = Pool.hash t.pool in
      let data = (steps, (counter, pool)) in
      Crypto.blake2b (Bytes.pack data)

    let steps t = t.steps

    let apply nats t =
      let { level; steps = _; counter; pool } = t in
      let level = level + 1n in
      let steps = 0n in

      let nats = 0n :: 0n :: nats in
      let pool =
        List.fold_left (fun pool nat -> Pool.push nat pool) pool nats
      in
      { level; steps; counter; pool }]

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

      let transaction : 'parameter -> mutez -> 'parameter contract -> operation
          =
        assert false

      let get_contract_opt : address -> 'parameter contract option =
        assert false
    end

    module Big_map : sig
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

    module Set : sig
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

    module Bytes : sig
      type t = bytes
      val pack : 'a -> t
      val concat : bytes -> bytes -> bytes
    end = struct
      type t = bytes
      let pack = assert false
      let concat = assert false
    end

    module Crypto : sig
      val blake2b : bytes -> bytes
    end = struct
      let blake2b = assert false
    end

    type state_hash = bytes
    type level = nat
    type steps = nat

    type committer = address
    type rejector = address

    let current_level = Tezos.level
    let sender = Tezos.sender

    let round_time = 10n]
