open! Core
open Jsip_types

(* Two structures for identity, one for presence.

   Identity: [participant_to_id] hashes a name to its id, and
   [id_to_participant] recovers the name by using the id as a direct index.
   The two keys want different structures — a name (string) hashes, a dense
   integer indexes a growable array. They are kept in lockstep by [intern],
   which is what makes [Queue.length id_to_participant] the next free id:
   there is no separate counter to drift out of sync with the queue.

   Presence: [logged_in] holds the ids that are connected *right now*, and is
   pruned by [log_out]. Identity is permanent, presence is not — that is why
   these are separate fields rather than one structure. *)
type t =
  { participant_to_id : Participant_id.t Participant.Table.t
  ; id_to_participant : Participant.t Queue.t
  ; logged_in : Participant_id.Hash_set.t
  }
[@@deriving sexp_of]

let create () =
  { participant_to_id = Participant.Table.create ()
  ; id_to_participant = Queue.create ()
  ; logged_in = Participant_id.Hash_set.create ()
  }
;;

let intern t participant =
  match Hashtbl.find t.participant_to_id participant with
  | Some id -> id
  | None ->
    (* The queue's length is the next free id, by construction: ids are dense
       from 0 and every mint enqueues exactly once, just below. *)
    let id = Participant_id.of_int (Queue.length t.id_to_participant) in
    Hashtbl.set t.participant_to_id ~key:participant ~data:id;
    Queue.enqueue t.id_to_participant participant;
    id
;;

let name_of_id_exn t (id : Participant_id.t) =
  Queue.get t.id_to_participant (id :> int)
;;

let is_logged_in t id = Hash_set.mem t.logged_in id

let log_in t participant =
  (* Interning first is safe even on the rejection path below: identity is
     permanent and idempotent, so recording the name costs nothing and a
     rejected login leaves the registry exactly as a successful one would. *)
  let id = intern t participant in
  if is_logged_in t id
  then
    Or_error.error_s
      [%message
        "participant name already in use" (participant : Participant.t)]
  else (
    Hash_set.add t.logged_in id;
    Ok id)
;;

let log_out t id = Hash_set.remove t.logged_in id
