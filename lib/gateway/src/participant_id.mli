(** A server-local integer id standing in for a participant name.

    Inside the server we intern each {!Jsip_types.Participant.t} (a human name)
    to a small [Participant_id.t] at login, key our own lookup tables by it,
    and resolve back to the name at every human-facing edge. The id is
    deliberately server-local: it lives here in the gateway, not in
    {!Jsip_types}, and it has no [bin_io] — it must never cross the wire.

    [t] is [private int]: callers can read it as an int (e.g. [(id :> int)] to
    index a table) but cannot fabricate one from an arbitrary integer; only
    the login registry is allowed to mint ids. *)

open! Core

type t = private int [@@deriving sexp_of, compare, equal, hash]

(** {2 Generation}

    Participant ids should only be minted by the login registry. The
    [Generator] module encapsulates this — because {!t} is [private int],
    holding a [Generator.t] is the only way to produce a fresh id, so the
    capability to intern a new participant stays with whoever owns the
    generator. *)

module Generator : sig
  type participant_id := t
  type t [@@deriving sexp_of]

  val create : unit -> t
  val next : t -> participant_id
end
