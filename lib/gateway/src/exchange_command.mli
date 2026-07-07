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
    session. *)

open! Core
open! Jsip_types

type t =
  | Submit of Order.Request.t
  | Cancel of Client_order_id.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

(** Parse a text command into a {!t}. Returns [Error] with a human-readable
    message if the input is malformed. Identity is not part of a command —
    the server attaches the authenticated participant from the session. *)
val parse : string -> t Or_error.t
