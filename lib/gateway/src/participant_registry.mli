(** The participant registry: interns each participant name to a server-local
    {!Participant_id.t} at login, and resolves ids back to names at the edges.

    One registry is shared across all connections and lives for the whole run.
    It is {b additive} — a name keeps the same id across reconnects and ids are
    never reused — which is why it is a distinct structure from the server's
    [logged_in_participants] set: that tracks who is *currently* connected and
    is pruned on disconnect, whereas identity here is permanent.

    The two directions use different structures because the two keys differ: a
    name (string) hashes to an id (a {!Participant.Table}), while a dense
    integer id indexes a growable array (a [Queue]) back to its name. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

val create : unit -> t

(** Return [name]'s id, minting and recording a fresh one the first time the
    name is seen. Idempotent: interning the same name again returns the same
    id. *)
val intern : t -> Participant.t -> Participant_id.t

(** The name a [Participant_id.t] was interned for. This is total: an id can
    only be produced by {!intern} (ids can't be fabricated — {!Participant_id}
    is a private int), so it always resolves. *)
val name_of_id : t -> Participant_id.t -> Participant.t
