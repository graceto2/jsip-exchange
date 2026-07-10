open! Core
open Jsip_types

module Position = struct
  (* The open position for one (participant, symbol) pair, plus the realized
     cash booked so far.

     [cost_basis] is the signed total cost, in cents, of the shares currently
     held: positive for a long, negative for a short. Keeping the sign means
     [cost_basis / inventory] is always the positive average entry price, and
     unrealized P&L reduces to the closed form
     [inventory * ref - cost_basis]. *)
  type t =
    { inventory : int
    ; cost_basis : int
    ; realized : int
    }
  [@@deriving sexp_of]

  let empty = { inventory = 0; cost_basis = 0; realized = 0 }

  (* Apply a signed trade of [qty] shares (positive buy, negative sell) at
     [price_cents], booking realized P&L when the trade reduces, closes, or
     flips the existing position. *)
  let apply_trade t ~qty ~price_cents =
    let opening_or_adding =
      t.inventory = 0 || Bool.equal (t.inventory > 0) (qty > 0)
    in
    if opening_or_adding
    then
      (* Same direction (or opening from flat): no P&L is realized, we just
         accumulate shares and their cost. *)
      { t with
        inventory = t.inventory + qty
      ; cost_basis = t.cost_basis + (qty * price_cents)
      }
    else (
      (* We close up to [closing] shares of the existing position against its
         average entry price, then — if [qty] is larger than the position —
         open a fresh position with whatever is left. Direction is 1 if long,
         -1 if short. *)
      let direction = if t.inventory > 0 then 1 else -1 in
      let closing = Int.min (Int.abs t.inventory) (Int.abs qty) in
      let average_entry = t.cost_basis / t.inventory in
      let realized_delta =
        direction * closing * (price_cents - average_entry)
      in
      let remainder = Int.abs qty - closing in
      let cost_basis =
        if remainder > 0
        then
          (* Flipped through zero: the old position is fully closed and a new
             one opens at [price_cents] on the far side. *)
          Int.neg direction * remainder * price_cents
        else
          (* Reduced or exactly closed: drop the closed shares' cost. *)
          t.cost_basis - (direction * closing * average_entry)
      in
      { inventory = t.inventory + qty
      ; cost_basis
      ; realized = t.realized + realized_delta
      })
  ;;
end

type t =
  { positions : Position.t Symbol_id.Map.t Participant.Map.t
  ; reference_prices : Price.t Symbol_id.Map.t
  }
[@@deriving sexp_of]

let empty =
  { positions = Participant.Map.empty; reference_prices = Symbol_id.Map.empty }
;;

let update_position t ~participant ~symbol ~qty ~price_cents =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol_id.Map.empty
  in
  let position =
    Map.find by_symbol symbol |> Option.value ~default:Position.empty
  in
  let position = Position.apply_trade position ~qty ~price_cents in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:position in
  { t with positions = Map.set t.positions ~key:participant ~data:by_symbol }
;;

let apply_fill t (fill : Fill.t) =
  let symbol = fill.symbol in
  let price_cents = Price.to_int_cents fill.price in
  let size = Size.to_int fill.size in
  let apply t ~participant ~side =
    update_position
      t
      ~participant
      ~symbol
      ~qty:(Side.sign side * size)
      ~price_cents
  in
  apply t ~participant:fill.aggressor_participant ~side:fill.aggressor_side
  |> fun t ->
  apply
    t
    ~participant:fill.resting_participant
    ~side:(Side.flip fill.aggressor_side)
;;

let apply_trade_report t (trade_report : Trade_report.t) =
  { t with
    reference_prices =
      Map.set
        t.reference_prices
        ~key:trade_report.symbol
        ~data:trade_report.price
  }
;;

module Position_summary = struct
  type t =
    { symbol : Symbol_id.t
    ; inventory : int
    ; average_entry_price : Price.t option
    ; reference_price : Price.t option
    ; realized_cents : int
    ; unrealized_cents : int
    }
  [@@deriving sexp_of]
end

module Summary = struct
  type t =
    { per_symbol : Position_summary.t list
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    }
  [@@deriving sexp_of]
end

let position_summary ~symbol ~reference_price (position : Position.t) =
  let average_entry_price =
    if position.inventory = 0
    then None
    else Some (Price.of_int_cents (position.cost_basis / position.inventory))
  in
  let unrealized_cents =
    match reference_price with
    | None -> 0
    | Some reference_price ->
      (position.inventory * Price.to_int_cents reference_price)
      - position.cost_basis
  in
  { Position_summary.symbol
  ; inventory = position.inventory
  ; average_entry_price
  ; reference_price
  ; realized_cents = position.realized
  ; unrealized_cents
  }
;;

let summary t participant =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol_id.Map.empty
  in
  let per_symbol =
    Map.to_alist by_symbol
    |> List.map ~f:(fun (symbol, position) ->
      let reference_price = Map.find t.reference_prices symbol in
      position_summary ~symbol ~reference_price position)
  in
  let total_realized_cents =
    List.sum (module Int) per_symbol ~f:(fun s -> s.realized_cents)
  in
  let total_unrealized_cents =
    List.sum (module Int) per_symbol ~f:(fun s -> s.unrealized_cents)
  in
  { Summary.per_symbol; total_realized_cents; total_unrealized_cents }
;;
