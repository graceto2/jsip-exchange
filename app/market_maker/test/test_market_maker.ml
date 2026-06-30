(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_test_harness
open Jsip_market_maker
open E2e_helpers
(* open Jsip_types *)

let default_config : Market_maker.Config.t =
  { participant = Harness.market_maker
  ; symbol = Harness.aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels =
      3
      (* ; inventory = Map.empty (module Symbol) ; currently_resting_orders =
         Set.empty (module Client_order_id) *)
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
    return ())
;;

(* let%expect_test "run: resulting inventory and outstanding orders state \
   match what was expected" = with_server ~symbols:[ Harness.aapl ] (fun
   ~server:_ ~port -> let%bind mm = connect_as ~port Harness.market_maker in
   let%bind alice = connect_as ~port Harness.alice in let%bind () =
   Market_maker.run default_config (connection mm) in let%bind () =
   Market_maker.seed_book default_config (connection mm) in (* check that
   resting orders were all added to set of client OIDs *) (* Set.iter
   default_config.currently_resting_orders ~f:(fun oid -> print_string
   [%string "%{oid#Client_order_id}, "]);
   [%expect {|"print client oids"|}]; *) let%bind () = rpc_submit alice
   (Harness.sell ~price_cents:14990 ~participant:Harness.alice ~size:50
   ~symbol:Harness.aapl ()) in Map.iteri default_config.inventory ~f:(fun
   ~key ~data -> print_endline [%string "%{key#Symbol}: %{data#Int}"]);
   let%bind () = rpc_submit alice (Harness.sell ~price_cents:14990
   ~participant:Harness.alice ~size:50 ~symbol:Harness.aapl ()) in (* fill
   consumed the remaining size of order, client OID should have been
   removed *) Map.iteri default_config.inventory ~f:(fun ~key ~data ->
   print_endline [%string "%{key#Symbol}: %{data#Int}"]); Set.iter
   default_config.currently_resting_orders ~f:(fun oid -> print_string
   [%string "%{oid#Client_order_id}, "]);
   [%expect {|"some client oid was removed|}]; return ()) ;; *)
