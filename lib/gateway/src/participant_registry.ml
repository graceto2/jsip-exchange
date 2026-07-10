open! Core
open Jsip_types

(* [name_to_id] hashes a name to its id; [id_to_name] recovers the name by
   using the id as a direct index. [generator] mints the next id. All three
   are kept in lockstep by [intern]. *)
type t =
  { generator : Participant_id.Generator.t
  ; participant_to_id : Participant_id.t Participant.Table.t
  ; id_to_participant : Participant.t Queue.t
  }
[@@deriving sexp_of]

let create () =
  { generator = Participant_id.Generator.create ()
  ; participant_to_id = Participant.Table.create ()
  ; id_to_participant = Queue.create ()
  }
;;

(* add of_int to participant_id and generate in *)
let intern t participant =
  match Hashtbl.find t.participant_to_id participant with
  | Some id -> id
  | None ->
    let id = Participant_id.Generator.next t.generator in
    let int_id = Participant_id.to_int id in
    if not (int_id = Queue.length t.id_to_participant)
    then
      raise_s
        [%message
          "id and queue length not in sync"
            (int_id : int)
            ~len:(Queue.length t.id_to_participant : int)];
    (match Hashtbl.add t.participant_to_id ~key:participant ~data:id with
     | `Duplicate ->
       raise_s
         [%message "name already interned" (participant : Participant.t)]
     | `Ok ->
       Queue.enqueue t.id_to_participant participant;
       id)
;;

let name_of_id t (id : Participant_id.t) =
  Queue.get t.id_to_participant (id :> int)
;;
