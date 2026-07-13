(** The participant registry: the server's record of who exists and who is
    here.

    One registry is shared across all connections and lives for the whole
    run. It answers two different questions, and the difference matters:

    - {b Identity} (permanent): which {!Participant_id.t} does this name
      have? Additive — a name keeps the same id across reconnects and ids are
      never reused. See {!intern} and {!name_of_id_exn}.
    - {b Presence} (transient): is this participant connected {i right now}?
      Pruned on disconnect. See {!log_in}, {!log_out} and {!is_logged_in}.

    {!log_in} is where the two meet: it interns the name (identity) and marks
    it present (presence), failing if the name is already live on another
    connection. The gateway's login RPC is a thin wrapper around it. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

val create : unit -> t

(** {2 Identity} *)

(** Return [participant]'s id, minting and recording a fresh one the first
    time the name is seen. Idempotent: interning the same name again returns
    the same id. Ids are dense and handed out from 0.

    This records identity only — it says nothing about whether the
    participant is connected. Use {!log_in} on the login path. *)
val intern : t -> Participant.t -> Participant_id.t

(** The name an id was interned for. Raises if [id] did not come from this
    registry — ids are dense indices into an array of names, and
    {!Participant_id.of_int} will happily build one that points past the end.
    Every id this registry hands out resolves. *)
val name_of_id_exn : t -> Participant_id.t -> Participant.t

(** {2 Presence} *)

(** Intern [participant] and mark it connected, returning its id. Errors if
    the participant is already logged in — a name may be live on at most one
    connection at a time. *)
val log_in : t -> Participant.t -> Participant_id.t Or_error.t

(** Mark [id] disconnected. Identity is untouched: the participant keeps its
    id and can {!log_in} again later. Idempotent. *)
val log_out : t -> Participant_id.t -> unit

(** Is this participant connected right now? *)
val is_logged_in : t -> Participant_id.t -> bool
