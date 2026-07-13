(** The exchange's instrument list: which symbols trade, and which
    {!Jsip_types.Symbol_id.t} each one trades under.

    Ids are positions in the list — the k-th symbol is id [k] — which is
    exactly the invariant {!Jsip_order_book.Matching_engine} relies on when
    it indexes its dense array of books. Sizing the engine from a directory
    is what keeps the two in agreement: "id is in the directory" and "id is
    in range for the engine" become the same statement, so there is still
    only one bounds check (in {!Jsip_order_book.Matching_engine.book}), not
    two that can drift.

    The directory is a {b readability} feature, not a correctness one: the
    exchange matches, fills and streams entirely in ids and never renders a
    name. Resolution happens at the human-facing edges — name->id when
    {!Exchange_command} parses [BUY AAPL 100], id->name when a client or the
    monitor prints an event.

    The authoritative directory is built once in the server's [main] and
    handed to {!Exchange_server.start}. Clients fetch {!to_alist} over
    {!Rpc_protocol.symbol_directory_rpc} and rebuild a local mirror with
    {!of_alist}. Note the wire carries the plain pairs, not a [t] — [t] is a
    local lookup structure, so it needs no [bin_io].

    Both directions are needed (parse wants name->id, render wants id->name),
    which is why this is a module and not just a [Symbol.t array]: the array
    and the reverse table must stay in lockstep, and only the constructors
    here can guarantee that. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** Build a directory, assigning ids densely from 0 in list order. Errors on
    an empty list or a repeated ticker — a ticker must name exactly one id,
    or {!id_of_symbol} has no single answer. *)
val of_symbols : Symbol.t list -> t Or_error.t

(** Rebuild a directory from the pairs served over the wire. Errors if the
    ids are not exactly [0 .. n-1], since a client cannot mirror a directory
    whose ids are not dense. *)
val of_alist : (Symbol_id.t * Symbol.t) list -> t Or_error.t

(** The identity directory: symbol [k] is {i named} ["k"]. For callers that
    need a working exchange but do not care what the instruments are called —
    integration tests and scenarios, which speak in ids throughout. Because
    each name is its own id, rendering through this directory prints exactly
    what printing the raw id would, so the directory stays invisible in their
    output. Real deployments use {!of_symbols} with real names. *)
val numbered : num_symbols:int -> t

(** Every [(id, ticker)] pair, in id order. This is the wire form. *)
val to_alist : t -> (Symbol_id.t * Symbol.t) list

(** How many instruments trade. This is the authoritative symbol count:
    {!Exchange_server.start} sizes the matching engine from it. *)
val num_symbols : t -> int

(** The ticker an id stands for, or [None] if it is not an id we trade. Used
    at render time. *)
val symbol_of_id : t -> Symbol_id.t -> Symbol.t option

(** The id a ticker trades under, or [None] if we do not trade it. Used at
    parse time, so a human can type [AAPL] instead of [0]. *)
val id_of_symbol : t -> Symbol.t -> Symbol_id.t option
