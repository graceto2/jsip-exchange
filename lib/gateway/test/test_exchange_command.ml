open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

let print_parse line =
  match Exchange_command.parse line with
  | Error err -> print_endline [%string "%{Error.to_string_hum err}"]
  | Ok (Exchange_command.Submit req) ->
    print_endline [%string "%{req#Order.Request}"]
  | Ok (Exchange_command.Book symb) | Ok (Exchange_command.Subscribe symb) ->
    print_endline [%string "%{symb#Symbol}"]
;;

let%expect_test "parse: basic subscribe" =
  print_parse "SUBSCRIBE Aapl";
  [%expect {| AAPL |}]
;;

let%expect_test "parse: basic book" =
  print_parse "BOOK AAPL";
  [%expect {| AAPL |}]
;;

(* --- Successful parsing --- *)

let%expect_test "parse: basic buy" =
  print_parse "BUY AAPL 100 150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL TSLA 50 200.00";
  [%expect {| SELL TSLA 50@$200.00 DAY as anonymous |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy AAPL 100 150.00";
  print_parse "Buy AAPL 100 150.00";
  [%expect
    {|
    BUY AAPL 100@$150.00 DAY as anonymous
    BUY AAPL 100@$150.00 DAY as anonymous
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY AAPL 100 150.00 IOC";
  [%expect {| BUY AAPL 100@$150.00 IOC as anonymous |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL AAPL 200 151.00 DAY";
  [%expect {| SELL AAPL 200@$151.00 DAY as anonymous |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY AAPL 100 150.00 as Alice";
  [%expect {| BUY AAPL 100@$150.00 DAY as Alice |}]
;;

let%expect_test "parse: with TIF and participant" =
  print_parse "SELL GOOG 75 2800.50 IOC as Bob";
  [%expect {| SELL GOOG 75@$2800.50 IOC as Bob |}]
;;

let%expect_test "parse: symbol is uppercased" =
  print_parse "BUY aapl 100 150.00";
  [%expect {| BUY AAPL 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse "  BUY   AAPL   100   150.00  ";
  [%expect {| BUY AAPL 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY AAPL 100 $150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

(* --- Parse errors --- *)

let%expect_test "parse error: empty string" =
  print_parse "";
  print_parse "   ";
  [%expect
    {|
    (Failure "empty command")
    (Failure "empty command")
    |}]
;;

let%expect_test "parse error: unknown command" =
  print_parse "HOLD AAPL 100 150.00";
  [%expect
    {| ("Exchange_command.Verb.of_string: invalid string" (value HOLD)) |}]
;;

let%expect_test "parse error: missing fields" =
  print_parse "BUY AAPL";
  print_parse "BUY";
  [%expect
    {|
    (Failure "expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]")
    (Failure "expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]")
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY AAPL abc 150.00";
  print_parse "BUY AAPL 0 150.00";
  print_parse "BUY AAPL -5 150.00";
  [%expect
    {|
    (Failure "invalid size: abc")
    (Failure "size must be positive")
    (Failure "size must be positive")
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY AAPL 100 xyz";
  [%expect
    {|
    (Failure
      "invalid price: xyz\
     \nexception: (Invalid_argument \"Float.of_string xyz\")")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY AAPL 100 150.00 QQQ";
  [%expect {| ("Time_in_force.of_string: invalid string" (value QQQ)) |}]
;;

(* --- parse_command_with_default_participant --- *)

let%expect_test "default participant: used when none specified" =
  let default = Participant.of_string "DefaultTrader" in
  let req =
    Exchange_command.parse ~default_participant:default "BUY AAPL 100 150.00"
    |> ok_exn
  in
  match req with
  | Submit order ->
    print_endline [%string "participant=%{order.participant#Participant}"];
    [%expect {| participant=DefaultTrader |}]
  | _ -> [%expect.unreachable]
;;

let%expect_test "default participant: overridden by explicit 'as'" =
  let default = Participant.of_string "DefaultTrader" in
  let req =
    Exchange_command.parse
      ~default_participant:default
      "BUY AAPL 100 150.00 as Alice"
    |> ok_exn
  in
  match req with
  | Submit order ->
    print_endline [%string "participant=%{order.participant#Participant}"];
    [%expect {| participant=Alice |}]
  | _ -> [%expect.unreachable]
;;

(* --- Round-trip: parse then format --- *)

let%expect_test "round-trip: parse a command, submit, format result" =
  let open Jsip_test_harness in
  let t = Harness.create () in
  (* Place a resting sell *)
  Harness.submit_
    t
    (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  (* Parse a buy command from text and submit it *)
  let request =
    Exchange_command.parse "BUY AAPL 100 150.00 as Alice" |> ok_exn
  in
  match request with
  | Submit order ->
    let events = Matching_engine.submit (Harness.engine t) order in
    print_endline (Event_protocol.format_events events);
    [%expect
      {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    BBO AAPL bid=- ask=$150.00 x100
    ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    TRADE AAPL $150.00 x100
    BBO AAPL bid=- ask=-
    |}]
  | _ -> [%expect.unreachable]
;;
