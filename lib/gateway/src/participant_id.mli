(** A server-local integer id standing in for a participant name.

    Inside the server we intern each {!Jsip_types.Participant.t} to a small
    [Participant_id.t] at login, key our own lookup tables by it, and resolve
    back to the name at every human-facing edge. The id is server-local: it
    lives in the gateway, not in {!Jsip_types}, and it has no [bin_io] — it
    must never cross the wire.

    Ids are dense and start at 0, so an id doubles as an index: the k-th
    participant {!Jsip_gateway.Participant_registry.log_in}s as id [k] and
    sits at slot [k] of the registry's id->name array. Nothing here enforces
    that — it is the registry's invariant, and the registry is the only
    module that should call {!of_int}. *)

open! Core

type t = private int [@@deriving sexp_of, compare, equal, hash]

(** Ids are cheap to compare and hash, so they make good keys: this gives us
    [Participant_id.Hash_set], [.Table], [.Set] and [.Map] for the server's
    own lookup tables. *)
include Comparable.S_plain with type t := t

include Hashable.S_plain with type t := t

(** Read an id as a plain [int] (e.g. to index a table). Equivalent to the
    coercion [(id :> int)], but easier to read. *)
val to_int : t -> int

(** Build an id from an [int]. No validation: an id is meaningful only
    relative to the registry that handed it out, so a number pulled from
    nowhere ([of_int 999]) is a well-typed id that resolves to no
    participant. Callers outside {!Jsip_gateway.Participant_registry} should
    be getting ids from the registry, not minting them. *)
val of_int : int -> t
