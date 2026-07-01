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

    Time-in-force defaults to DAY if omitted. Participant defaults to
    "anonymous" if omitted. *)

open! Core
open! Jsip_types

type t =
  | Submit of Order.Submit_request.t
  | Cancel of Order.Cancel_request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

(** Parse a text command into an order request. Returns [Error] with a
    human-readable message if the input is malformed. *)
(* val parse_command : string -> (Order.Submit_request.t, string) Result.t *)

(** If no participant is provided in the input, and if none is specified in
    the command text, uses [default] as the participant. Useful for clients
    that already know their identity. *)
(* val parse_command_with_default_participant : string ->
   default:Participant.t -> (Order.Submit_request.t, string) Result.t *)

val parse : ?default_participant:Participant.t -> string -> t Or_error.t
