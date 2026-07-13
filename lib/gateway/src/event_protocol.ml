open! Core
open Jsip_types

(* The exchange runs on ids; a person reads names. This module is the one
   place that bridges them, because it is the one place that writes text a
   person will read — the client and the monitor both render through here.
   [Jsip_types] deliberately cannot do this: a [Book.t] does not know that id
   0 is AAPL, and no amount of plumbing would make it know. That fact lives
   in the directory, which lives out here with the consumers.

   Falls back to the raw id rather than raising. A directory is a mirror
   fetched once at connect, so an id it has never heard of means the mirror
   is stale, not that the event is corrupt — and printing "7" beats crashing
   the client's render loop. *)
let symbol_to_string ~directory symbol_id =
  match Symbol_directory.symbol_of_id directory symbol_id with
  | Some symbol -> Symbol.to_string symbol
  | None -> Symbol_id.to_string symbol_id
;;

(* Reimplements [Fill.to_string]'s layout rather than calling it, because
   that one prints the id. Same reason for [format_book] below. *)
let format_fill ~directory (fill : Fill.t) =
  sprintf
    "fill_id=%s aggressor_client_oid=%s resting_client_oid=%s %s %s x%d \
     aggressor=%s(%s) %s resting=%s(%s)"
    (Int.to_string fill.fill_id)
    (Client_order_id.to_string fill.aggressor_client_order_id)
    (Client_order_id.to_string fill.resting_client_order_id)
    (symbol_to_string ~directory fill.symbol)
    (Price.to_string_dollar fill.price)
    (Size.to_int fill.size)
    (Order_id.to_string fill.aggressor_order_id)
    (Participant.to_string fill.aggressor_participant)
    (Side.to_string fill.aggressor_side)
    (Order_id.to_string fill.resting_order_id)
    (Participant.to_string fill.resting_participant)
;;

let format_event ~directory event =
  let symbol_to_string = symbol_to_string ~directory in
  match event with
  | Exchange_event.Order_accept { order_id; participant = _; request } ->
    sprintf
      "ACCEPTED id=%s %s %s %d@%s %s"
      (Order_id.to_string order_id)
      (symbol_to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      (Time_in_force.to_string request.time_in_force)
  | Fill fill -> [%string "FILL %{format_fill ~directory fill}"]
  | Order_cancel
      { order_id
      ; participant = _
      ; symbol
      ; remaining_size
      ; reason
      ; client_order_id
      } ->
    sprintf
      "CANCELLED order_id=%s client_oid=%s %s remaining=%d reason=%s"
      (Order_id.to_string order_id)
      (Client_order_id.to_string client_order_id)
      (symbol_to_string symbol)
      (Size.to_int remaining_size)
      (Cancel_reason.to_string reason)
  | Order_reject { participant = _; request; reason } ->
    sprintf
      "REJECTED %s %s %d@%s reason=%s"
      (symbol_to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      reason
  | Cancel_reject { participant = _; client_order_id; reason } ->
    sprintf
      "REJECTED cancel request with client_oid=%s reason=%s"
      (Client_order_id.to_string client_order_id)
      reason
  | Best_bid_offer_update { symbol; bbo } ->
    let symbol = symbol_to_string symbol in
    let bid = Level.opt_to_string bbo.bid in
    let ask = Level.opt_to_string bbo.ask in
    [%string "BBO %{symbol} bid=%{bid} ask=%{ask}"]
  | Trade_report { symbol; price; size } ->
    let symbol = symbol_to_string symbol in
    let size = Size.to_int size in
    [%string "TRADE %{symbol} %{price#Price} x%{size#Int}"]
;;

let format_events ~directory events =
  List.map events ~f:(format_event ~directory) |> String.concat ~sep:"\n"
;;

let format_book ~directory ({ symbol; bids; asks; bbo } : Book.t) =
  let format_side label levels =
    match levels with
    | [] -> [%string "  %{label}: (empty)"]
    | _ ->
      let lines =
        List.map levels ~f:(fun level -> [%string "    %{level#Level}"])
        |> String.concat ~sep:"\n"
      in
      [%string "  %{label}:\n%{lines}"]
  in
  let symbol = symbol_to_string ~directory symbol in
  String.concat
    ~sep:"\n"
    [ [%string "=== %{symbol} ==="]
    ; format_side "BIDS" bids
    ; format_side "ASKS" asks
    ; [%string "  BBO: %{bbo#Bbo}"]
    ]
;;

(* What [participant] should be told about a fill they were part of, from
   their own side of it. [None] if the fill is not theirs — the caller is
   reading a feed that may carry other people's trades. *)
let format_fill_for_participant ~directory fill participant =
  let (fill : Fill.t) = fill in
  let my_side =
    if Participant.equal participant fill.aggressor_participant
    then Some fill.aggressor_side
    else if Participant.equal participant fill.resting_participant
    then Some (Side.flip fill.aggressor_side)
    else None
  in
  let verb : Side.t -> string = function
    | Buy -> "bought"
    | Sell -> "sold"
  in
  Option.map my_side ~f:(fun side ->
    let symbol = symbol_to_string ~directory fill.symbol in
    let size = Size.to_int fill.size in
    let price = Price.to_string_dollar fill.price in
    [%string
      "You %{verb side} size=%{size#Int} symbol=%{symbol} at %{price}"])
;;
