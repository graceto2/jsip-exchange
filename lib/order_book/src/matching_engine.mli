(** The matching engine: receives order requests, manages order books, and
    produces exchange events.

    The engine is the heart of the exchange. It assigns order IDs, determines
    which orders can trade against each other, executes fills, and manages
    the lifecycle of resting orders. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** Create a matching engine trading [num_symbols] instruments, with symbol
    ids [0 .. num_symbols - 1]. Each id gets its own order book. *)
val create : int -> t

(** {2 Order submission} *)

(** Submit a new order request. Returns the list of exchange events produced:
    an acceptance or rejection, followed by any fills, and possibly a
    cancellation of unfilled remainder (for IOC orders).

    The event list is always non-empty (at minimum an acceptance or
    rejection). *)
val submit
  :  t
  -> participant:Participant.t
  -> Order.Request.t
  -> Exchange_event.t list

val cancel
  :  t
  -> participant:Participant.t
  -> client_order_id:Client_order_id.t
  -> Exchange_event.t list

(** {2 Queries} *)

(** The order book for a given symbol id, or [None] if the id is out of range
    (not a symbol this engine trades). *)
val book : t -> Symbol_id.t -> Order_book.t option

(** Close the trading day: every resting [Day] order is removed from its
    book, yielding one [Order_cancel] event per order with reason
    {!Jsip_types.Cancel_reason.End_of_day}, followed by a
    [Best_bid_offer_update] for each symbol whose quote changed as a result
    (which is every symbol that had a resting order). [Ioc] orders never
    rest, so they cannot appear here.

    The books are genuinely emptied, not just announced as empty: after this
    the engine will not match against the cancelled orders. Calling it on an
    idle exchange produces no events. *)
val end_of_day : t -> Exchange_event.t list
