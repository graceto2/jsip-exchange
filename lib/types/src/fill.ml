open! Core

type t =
  { fill_id : int
  ; symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  ; aggressor_order_id : Order_id.t
  ; aggressor_participant : Participant.t
  ; aggressor_side : Side.t
  ; resting_order_id : Order_id.t
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
   ; resting_order_id
   ; resting_participant
   } :
    t)
  =
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s(%s) %s resting=%s(%s)"
    fill_id
    (Symbol.to_string symbol)
    (Price.to_string_dollar price)
    (Size.to_int size)
    (Order_id.to_string aggressor_order_id)
    (Participant.to_string aggressor_participant)
    (Side.to_string aggressor_side)
    (Order_id.to_string resting_order_id)
    (Participant.to_string resting_participant)
;;

(* let x : Int.t = 5 let y : String.t = "6" let output_string : string =
   [%string "I can write anything in here. x=%{x#Int}, y=%{y}"] *)

let to_participant_view fill p =
  let aggressor = fill.aggressor_participant in
  let resting = fill.resting_participant in
  let size = fill.size in
  let symbol = fill.symbol in
  let price = Price.to_float fill.price in
  let aggressor_side = fill.aggressor_side in
  let resting_side = Side.flip aggressor_side in
  if Participant.equal p aggressor
  then (
    match aggressor_side with
    | Side.Buy ->
      Some
        [%string
          "You bought size=%{size#Size} symbol=%{symbol#Symbol} at \
           $price=%{price#Float}"]
    | Sell ->
      Some
        [%string
          "You sold size=%{size#Size} symbol=%{symbol#Symbol} at \
           $price=%{price#Float}"])
  else if Participant.equal p resting
  then (
    match resting_side with
    | Side.Buy ->
      Some
        [%string
          "You bought size=%{size#Size} symbol=%{symbol#Symbol} at \
           $price=%{price#Float}"]
    | Sell ->
      Some
        [%string
          "You sold size=%{size#Size} symbol=%{symbol#Symbol} at \
           $price=%{price#Float}"])
  else None
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size
