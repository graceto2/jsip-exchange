open! Core

type t =
  { fill_id : int
  ; symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  ; aggressor_order_id : Order_id.t
  ; aggressor_client_order_id : Client_order_id.t
  ; aggressor_participant : Participant.t
  ; aggressor_side : Side.t
  ; resting_order_id : Order_id.t
  ; resting_client_order_id : Client_order_id.t
  ; resting_participant : Participant.t
  }
[@@deriving sexp, bin_io]

let to_string
  ({ fill_id
   ; symbol
   ; price
   ; size
   ; aggressor_order_id
   ; aggressor_participant
   ; aggressor_side
   ; aggressor_client_order_id
   ; resting_order_id
   ; resting_participant
   ; resting_client_order_id
   } :
    t)
  =
  sprintf
    "fill_id=%d aggressor_client_oid=%s resting_client_oid=%s %s %s x%d \
     aggressor=%s(%s) %s resting=%s(%s)"
    fill_id
    (Client_order_id.to_string aggressor_client_order_id)
    (Client_order_id.to_string resting_client_order_id)
    (Symbol.to_string symbol)
    (Price.to_string_dollar price)
    (Size.to_int size)
    (Order_id.to_string aggressor_order_id)
    (Participant.to_string aggressor_participant)
    (Side.to_string aggressor_side)
    (Order_id.to_string resting_order_id)
    (Participant.to_string resting_participant)
;;

let to_participant_view fill p =
  let aggressor = fill.aggressor_participant in
  let resting = fill.resting_participant in
  let size = fill.size in
  let symbol = fill.symbol in
  let price = Price.to_string_dollar fill.price in
  let aggressor_side = fill.aggressor_side in
  let resting_side = Side.flip aggressor_side in
  let my_side =
    if Participant.equal p aggressor
    then Some aggressor_side
    else if Participant.equal p resting
    then Some resting_side
    else None
  in
  match my_side with
  | Some Side.Buy ->
    Some
      [%string
        "You bought size=%{size#Size} symbol=%{symbol#Symbol} at $%{price}"]
  | Some Sell ->
    Some
      [%string
        "You sold size=%{size#Size} symbol=%{symbol#Symbol} at $%{price}"]
  | None -> None
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size
