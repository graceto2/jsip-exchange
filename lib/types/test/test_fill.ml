open! Core
open Jsip_types

let%expect_test "notional_cents: price * size" =
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol_id.of_int 0
     ; price = Price.of_int_cents 15025
     ; size = Size.of_int 100
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Buy
     ; aggressor_client_order_id = 6
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Bob"
     ; resting_client_order_id = 36
     }
     : Fill.t)
  in
  [%test_result: int] (Fill.notional_cents fill) ~expect:1502500
;;
