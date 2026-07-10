(** A compact integer id standing in for a {!Symbol.t}.

    Unlike a server-local id (e.g. {!Jsip_gateway.Participant_id}), a
    [Symbol_id.t] is a {b public} identifier: clients submit orders and market
    data requests against the id directly, and a directory (added later) will
    publish the id-to-ticker mapping so participants know which id is which.

    This module is deliberately pure data — it carries an [int] and nothing
    more. It does not know how many symbols exist, so it {b cannot} validate
    that an id is in range; that check belongs at the server edge, where the
    symbol count is known. In particular {!of_int} wraps any integer without
    checking it, and [to_string] prints the raw int (not a ticker). *)

open! Core

type t = private int [@@deriving sexp, bin_io, compare, equal, hash, string]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** Read the id as a plain [int] (e.g. to index a per-symbol array). *)
val to_int : t -> int

(** Tag an [int] as a [Symbol_id.t]. Unchecked: does not verify the id
    corresponds to a real symbol — the server must range-check ids that come
    from a client before trusting them. *)
val of_int : int -> t
