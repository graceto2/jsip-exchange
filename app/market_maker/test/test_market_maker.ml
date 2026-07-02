(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_test_harness
open Jsip_market_maker
open E2e_helpers
open Jsip_types

let default_config : Market_maker.Config.t =
  { participant = Harness.market_maker
  ; symbol = Harness.aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  ; fill_client_oid = ref 0
  ; inventory = Map.empty (module Symbol)
  ; currently_resting_orders = Map.empty (module Client_order_id)
  }
;;

let%expect_test "seed_book: places symmetric bids and asks around fair value"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let%bind () = Market_maker.seed_book default_config (connection mm) in
    [%expect
      {|
      [for MarketMaker] ACCEPTED id=1 AAPL BUY 100@$149.90 DAY
      [for MarketMaker] ACCEPTED id=2 AAPL SELL 100@$150.10 DAY
      [for MarketMaker] ACCEPTED id=3 AAPL BUY 100@$149.89 DAY
      [for MarketMaker] ACCEPTED id=4 AAPL SELL 100@$150.11 DAY
      [for MarketMaker] ACCEPTED id=5 AAPL BUY 100@$149.88 DAY
      [for MarketMaker] ACCEPTED id=6 AAPL SELL 100@$150.12 DAY
      |}];
    Market_maker.reset_fill_client_oids default_config;
    (* how come the inventory and resting orders seem to reset, but not the
       fill order ids ? *)
    return ())
;;

let%expect_test "run: resulting inventory and outstanding orders state \
                 match what was expected"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = login_as ~port Harness.alice in
    let%bind mm = connect_as ~port Harness.market_maker in
    let%bind () = Market_maker.run default_config (connection mm) in
    let%bind () = Market_maker.seed_book default_config (connection mm) in
    printf
      "before: %d\n"
      (Map.length default_config.currently_resting_orders);
    let%bind () =
      rpc_submit
        alice
        (Harness.sell
           ~price_cents:14990
           ~participant:Harness.alice
           ~size:100
           ~symbol:Harness.aapl
           ())
    in
    printf "after:%d\n" (Map.length default_config.currently_resting_orders);
    (* Fill consumed the remaining size of order, the first order (of client
       OID = 7) should have been removed. *)
    print_string "Current inventory: ";
    printf
      !"%{sexp: (Symbol.t * int) list}\n"
      (Map.to_alist default_config.inventory);
    [%expect
      {|
      before: 6
      after:5
      Current inventory: ((AAPL 100))
      |}];
    (* Test buying into MM's orders. *)
    let%bind () =
      rpc_submit
        alice
        (Harness.buy
           ~price_cents:16000
           ~participant:Harness.alice
           ~size:75
           ~symbol:Harness.aapl
           ())
    in
    (* Should decrease by 75, since we sold to Alice. *)
    print_string "Current inventory: ";
    printf
      !"%{sexp: (Symbol.t * int) list}\n"
      (Map.to_alist default_config.inventory);
    [%expect {| Current inventory: ((AAPL 25)) |}];
    return ())
;;

(* Test cancel. *)
