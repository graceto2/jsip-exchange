open! Core
open Jsip_types

(* [name_to_id] hashes a name to its id; [id_to_name] recovers the name by
   using the id as a direct index. [generator] mints the next id. All three
   are kept in lockstep by [intern]. *)
type t =
  { generator : Participant_id.Generator.t
  ; name_to_id : Participant_id.t Participant.Table.t
  ; id_to_name : Participant.t Queue.t
  }
[@@deriving sexp_of]

let create () =
  { generator = Participant_id.Generator.create ()
  ; name_to_id = Participant.Table.create ()
  ; id_to_name = Queue.create ()
  }
;;

let intern t name =
  match Hashtbl.find t.name_to_id name with
  | Some id -> id
  | None ->
    (* TODO(human): [name] is new. Mint a fresh id from [t.generator], record
       the [name -> id] mapping in [t.name_to_id], and append [name] to
       [t.id_to_name] so the id doubles as its slot. Return the id.

       Invariant to preserve: because the generator starts at 0 and the queue
       starts empty, the id you mint must equal [Queue.length t.id_to_name] at
       the moment you enqueue — that lockstep is exactly what lets
       [name_of_id] be a direct [Queue.get]. *)
    failwith "TODO(human): mint and record a fresh id for a new name"
;;

let name_of_id t (id : Participant_id.t) =
  Queue.get t.id_to_name (id :> int)
;;
