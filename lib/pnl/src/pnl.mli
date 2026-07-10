(** Per-participant, per-symbol profit-and-loss tracking.

    A {!type:t} is an immutable accumulator. Feed it the {!Fill.t}s a
    participant is a party to and the public trade prints ({!Trade_report.t})
    from the market, and it maintains, for every (participant, symbol) pair:

    - the current [inventory] (signed — positive is long, negative is short),
    - the running cost basis of the open position (from which the average
      entry price is derived), and
    - the [realized] cash booked whenever a position is reduced or closed.

    Together with the latest reference price from a trade print, this yields
    both realized and unrealized P&L via {!summary}.

    {2 P&L model}

    We use {e average-cost} accounting. Opening or adding to a position
    simply accumulates shares and cost. Reducing or closing a position books
    realized P&L against the average entry price:

    {[
      realized += (shares_closed * (exit_price - average_entry_price))
    ]}

    for a long, with the sign flipped for a short. Unrealized P&L marks the
    open position to the reference price:

    {[
      unrealized = inventory * (reference_price - average_entry_price)
    ]}

    All money is in integer cents, matching {!Price.to_int_cents} and
    {!Fill.notional_cents}.

    {2 Example}

    {[
      let pnl =
        Pnl.empty
        |> Fn.flip Pnl.apply_fill alice_buys_100_at_150
        |> Fn.flip Pnl.apply_trade_report aapl_prints_at_152
      in
      Pnl.summary pnl alice
      (* Alice is long 100 @ $150.00, marked at $152.00: unrealized =
         +$200.00, realized = $0.00 *)
    ]} *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** A P&L accumulator with no positions and no known reference prices. *)
val empty : t

(** Book a fill. A fill has two parties — the aggressor and the resting order
    — and [apply_fill] updates the position of {e both}, each on their own
    side (the resting participant trades on the opposite side from the
    aggressor). *)
val apply_fill : t -> Fill.t -> t

(** Record a public trade print, refreshing the reference (mark-to-market)
    price used for that symbol's unrealized P&L. Positions are left
    unchanged. *)
val apply_trade_report : t -> Trade_report.t -> t

(** {2 Summaries} *)

module Position_summary : sig
  (** P&L for a single (participant, symbol) pair at a point in time. *)
  type t =
    { symbol : Symbol_id.t
    ; inventory : int
    (** Signed share count: positive long, negative short, zero flat. *)
    ; average_entry_price : Price.t option
    (** Average price of the open position, or [None] when flat. *)
    ; reference_price : Price.t option
    (** Latest trade-print price for the symbol, if one has been seen. *)
    ; realized_cents : int
    (** Cash booked from closed positions, in cents. *)
    ; unrealized_cents : int
    (** Mark-to-market P&L of the open position against [reference_price], in
        cents. Zero when flat or when no reference price is known. *)
    }
  [@@deriving sexp_of]
end

module Summary : sig
  (** A participant's P&L broken down per symbol, plus their totals. *)
  type t =
    { per_symbol : Position_summary.t list
    (** One entry per symbol the participant has traded, sorted by symbol. *)
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    }
  [@@deriving sexp_of]
end

(** Summarize a participant's P&L across every symbol they have traded. A
    participant with no recorded fills yields an empty [per_symbol] and zero
    totals. *)
val summary : t -> Participant.t -> Summary.t
