(** Text protocol for communicating with the exchange.

    This module defines how order requests are represented as text and how
    exchange events are formatted for display. On a production exchange, this
    would be a binary protocol like FIX for performance and interoperability.
    We use a simple human-readable text format for ease of debugging and
    interactive use.

    {2 Command format}

    Each command is a single line of text:
    {v
    BUY  <client_id> <symbol> <size> <price> [<time_in_force>]
    SELL <client_id> <symbol> <size> <price> [<time_in_force>]
    v}

    Examples:
    {v
    BUY 0 AAPL 100 150.25
    SELL 1 TSLA 50 200.00 IOC
    BUY 2 AAPL 100 150.00 DAY
    v}

    Time-in-force defaults to DAY if omitted. Identity is not part of a
    command: the server attaches the authenticated participant from the
    session.

    {2 Symbols}

    Humans type a symbol name ([AAPL]); the wire carries a
    {!Jsip_types.Symbol_id.t} ([0]). Resolution happens here, at parse time,
    which is why every entry point takes a [~directory] — see
    {!Symbol_directory}. A name the directory does not know is rejected
    locally, without a round trip to the server.

    Names only: an id typed directly is {i not} accepted, because it would be
    ambiguous — {!Jsip_types.Symbol.of_string} accepts any alphanumeric
    string, so [0] is itself a legal symbol name. *)

open! Core
open! Jsip_types

type t =
  | Submit of Order.Request.t
  | Cancel of Client_order_id.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t

(** Parse a text command into a {!t}, resolving symbol names to ids through
    [directory]. Returns [Error] with a human-readable message if the input
    is malformed or names a symbol the exchange does not trade. Identity is
    not part of a command — the server attaches the authenticated participant
    from the session. *)
val parse : directory:Symbol_directory.t -> string -> t Or_error.t
