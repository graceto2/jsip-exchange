open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness

(* Hand-roll a fill in which [buyer] lifts [seller]'s resting offer at
   [price_cents] for [size] shares. Only the price, size, sides, and
   participants matter to P&L, so the order/fill ids are arbitrary. *)
let fill ~buyer ~seller ~price_cents ~size =
  { Fill.fill_id = 0
  ; symbol = Harness.aapl
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_order_id = Order_id.For_testing.of_int 1
  ; aggressor_participant = buyer
  ; aggressor_side = Buy
  ; aggressor_client_order_id = 0
  ; resting_order_id = Order_id.For_testing.of_int 2
  ; resting_participant = seller
  ; resting_client_order_id = 0
  }
;;

let trade_report ~price_cents =
  { Trade_report.symbol = Harness.aapl
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int 1
  }
;;

let show pnl who =
  print_s
    [%message
      ""
        ~participant:(who : Participant.t)
        ~_:(Pnl.summary pnl who : Pnl.Summary.t)]
;;

let%expect_test "open a position, mark it, then partially close it" =
  (* Alice buys 100 @ $150.00 from Bob: Alice is long 100, Bob short 100. *)
  let pnl =
    Pnl.apply_fill
      Pnl.empty
      (fill
         ~buyer:Harness.alice
         ~seller:Harness.bob
         ~price_cents:15000
         ~size:100)
  in
  (* A public trade prints at $152.00 — the mark used for unrealized P&L. *)
  let pnl = Pnl.apply_trade_report pnl (trade_report ~price_cents:15200) in
  show pnl Harness.alice;
  show pnl Harness.bob;
  [%expect {|
    ((participant Alice)
     ((per_symbol
       (((symbol AAPL) (inventory 100) (average_entry_price (15000))
         (reference_price (15200)) (realized_cents 0) (unrealized_cents 20000))))
      (total_realized_cents 0) (total_unrealized_cents 20000)))
    ((participant Bob)
     ((per_symbol
       (((symbol AAPL) (inventory -100) (average_entry_price (15000))
         (reference_price (15200)) (realized_cents 0) (unrealized_cents -20000))))
      (total_realized_cents 0) (total_unrealized_cents -20000)))
    |}];
  (* Alice sells 60 @ $153.00 back to Bob, realizing P&L on 60 shares. *)
  let pnl =
    Pnl.apply_fill
      pnl
      (fill
         ~buyer:Harness.bob
         ~seller:Harness.alice
         ~price_cents:15300
         ~size:60)
  in
  show pnl Harness.alice;
  show pnl Harness.bob;
  [%expect {|
    ((participant Alice)
     ((per_symbol
       (((symbol AAPL) (inventory 40) (average_entry_price (15000))
         (reference_price (15200)) (realized_cents 18000)
         (unrealized_cents 8000))))
      (total_realized_cents 18000) (total_unrealized_cents 8000)))
    ((participant Bob)
     ((per_symbol
       (((symbol AAPL) (inventory -40) (average_entry_price (15000))
         (reference_price (15200)) (realized_cents -18000)
         (unrealized_cents -8000))))
      (total_realized_cents -18000) (total_unrealized_cents -8000)))
    |}]
;;

let%expect_test "selling through zero flips a long into a short" =
  (* Charlie buys 40 @ $150.00, then sells 100 @ $160.00: 40 shares close his
     long (realized), and the remaining 60 open a fresh short at $160.00. *)
  let pnl =
    Pnl.apply_fill
      Pnl.empty
      (fill
         ~buyer:Harness.charlie
         ~seller:Harness.alice
         ~price_cents:15000
         ~size:40)
  in
  let pnl =
    Pnl.apply_fill
      pnl
      (fill
         ~buyer:Harness.alice
         ~seller:Harness.charlie
         ~price_cents:16000
         ~size:100)
  in
  let pnl = Pnl.apply_trade_report pnl (trade_report ~price_cents:15500) in
  show pnl Harness.charlie;
  [%expect {|
    ((participant Charlie)
     ((per_symbol
       (((symbol AAPL) (inventory -60) (average_entry_price (16000))
         (reference_price (15500)) (realized_cents 40000)
         (unrealized_cents 30000))))
      (total_realized_cents 40000) (total_unrealized_cents 30000)))
    |}]
;;
