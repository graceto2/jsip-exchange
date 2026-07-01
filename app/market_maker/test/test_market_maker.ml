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

(* [CR] claude for Grace: Two issues with this test.

   (2) It only ever has Alice *sell* into the MM's bids, so the ask-side
   remaining-size bug (also CR'd in market_maker.ml) is never exercised. Add
   a case where a counterparty *buys* into the MM's asks. Also prefer
   asserting on the [inventory]/[currently_resting_orders] maps via [sexp_of]
   over hand-rolled [print_string] scaffolding — it shows structure and
   survives reordering. *)
let%expect_test "run: resulting inventory and outstanding orders state \
                 match what was expected"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let%bind alice = connect_as ~port Harness.alice in
    let%bind () = Market_maker.run default_config (connection mm) in
    let%bind () = Market_maker.seed_book default_config (connection mm) in
    print_string "Currently resting orders: ";
    Map.iteri
      default_config.currently_resting_orders
      ~f:(fun ~key:oid ~data ->
        ignore data;
        print_string [%string "%{oid#Client_order_id}, "]);
    print_string "\n";
    let%bind () =
      rpc_submit
        alice
        (Harness.sell
           ~price_cents:14990
           ~participant:Harness.alice
           ~size:50
           ~symbol:Harness.aapl
           ())
    in
    print_string "Current inventory: ";
    Map.iteri default_config.inventory ~f:(fun ~key ~data ->
      print_string [%string "%{key#Symbol} (size = %{data#Int}), "]);
    print_string "\n";
    let%bind () =
      rpc_submit
        alice
        (Harness.sell
           ~price_cents:14990
           ~participant:Harness.alice
           ~size:50
           ~symbol:Harness.aapl
           ())
    in
    (* Fill consumed the remaining size of order, the first order (of client
       OID = 7) should have been removed. *)
    print_string "Current inventory: ";
    Map.iteri default_config.inventory ~f:(fun ~key ~data ->
      print_string [%string "%{key#Symbol} (size = %{data#Int}), "]);
    print_string "\n";
    print_string "Currently resting orders: ";
    Map.iteri
      default_config.currently_resting_orders
      ~f:(fun ~key:oid ~data ->
        ignore data;
        print_string [%string "%{oid#Client_order_id}, "]);
    [%expect
      {|
      Currently resting orders: 1, 2, 3, 4, 5, 6,
      [for Alice] ACCEPTED id=7 AAPL SELL 50@$149.90 DAY
      [for Alice] FILL fill_id=1 aggressor_client_oid=0 resting_client_oid=1 AAPL $149.90 x50 aggressor=7(Alice) SELL resting=1(MarketMaker)
      Current inventory: AAPL (size = 50),
      [for Alice] REJECTED AAPL SELL 50@$149.90 reason=Client order ID already in use
      Current inventory: AAPL (size = 50),
      Currently resting orders: 1, 2, 3, 4, 5, 6,
      |}];
    return ())
;;
