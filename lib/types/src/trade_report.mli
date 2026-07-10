(** A public trade print: the payload of an {!Exchange_event.Trade_report}.

    Unlike a {!Fill.t}, a trade report carries no participant information —
    it is what the broader market sees when a trade occurs: the [symbol], the
    [price] at which it printed, and the [size].

    Consumers such as {!Jsip_pnl.Pnl} use the [price] as the reference
    (mark-to-market) price when computing unrealized P&L. *)

open! Core

type t =
  { symbol : Symbol_id.t
  ; price : Price.t
  ; size : Size.t
  }
[@@deriving sexp, bin_io]
