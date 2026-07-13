open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

(* Commands name symbols; the parsed command carries ids. The directory is
   what bridges the two, so every test needs one — [AAPL] is id 0, [TSLA] is
   1, [GOOG] is 2, matching the positions below. *)
let directory =
  [ "AAPL"; "TSLA"; "GOOG" ]
  |> List.map ~f:Symbol.of_string
  |> Symbol_directory.of_symbols
  |> ok_exn
;;

(* Prints the *id* a command resolved to, not the name that was typed — that
   is the whole point of parsing, so the tests below read as name-in /
   id-out. *)
let print_parse line =
  match Exchange_command.parse ~directory line with
  | Error err -> print_endline [%string "%{Error.to_string_hum err}"]
  | Ok (Exchange_command.Submit req) ->
    print_endline [%string "%{req#Order.Request}"]
  | Ok (Exchange_command.Book symb) | Ok (Exchange_command.Subscribe symb) ->
    print_endline [%string "%{symb#Symbol_id}"]
  | Ok (Exchange_command.Cancel client_order_id) ->
    print_endline [%string "%{client_order_id#Client_order_id}"]
;;

let%expect_test "parse: basic subscribe" =
  print_parse "SUBSCRIBE AAPL";
  [%expect {| 0 |}]
;;

let%expect_test "parse: basic book" =
  print_parse "BOOK GOOG";
  [%expect {| 2 |}]
;;

(* --- Successful parsing --- *)

let%expect_test "parse: basic buy" =
  print_parse "BUY 1 AAPL 100 150.25";
  [%expect {| BUY 1 0 100@$150.25 DAY |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL 1 TSLA 50 200.00";
  [%expect {| SELL 1 1 50@$200.00 DAY |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy 1 AAPL 100 150.00";
  print_parse "Buy 1 AAPL 100 150.00";
  [%expect {|
    BUY 1 0 100@$150.00 DAY
    BUY 1 0 100@$150.00 DAY
    |}]
;;

(* [Symbol.of_string] uppercases, so a lowercase name resolves to the same id
   a shouted one does. *)
let%expect_test "parse: symbol names are case insensitive" =
  print_parse "BUY 1 aapl 100 150.00";
  print_parse "BUY 1 Tsla 100 150.00";
  [%expect {|
    BUY 1 0 100@$150.00 DAY
    BUY 1 1 100@$150.00 DAY
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY 1 AAPL 100 150.00 IOC";
  [%expect {| BUY 1 0 100@$150.00 IOC |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL 1 AAPL 200 151.00 DAY";
  [%expect {| SELL 1 0 200@$151.00 DAY |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY 1 AAPL 100 $150.25";
  [%expect {| BUY 1 0 100@$150.25 DAY |}]
;;

(* --- Parse errors --- *)

(* A well-formed name the exchange does not trade is caught here, on the
   client, without a round trip to the server. *)
let%expect_test "parse error: unknown symbol" =
  print_parse "BUY 1 BANANA 100 150.00";
  print_parse "BOOK BANANA";
  [%expect
    {|
    (Failure "unknown symbol: BANANA")
    (Failure "unknown symbol: BANANA")
    |}]
;;

(* Ids are a wire detail; typing one is not a shortcut for the name. [0] is
   rejected as an unknown *name*, not read as symbol id 0. *)
let%expect_test "parse error: a raw id is not a symbol name" =
  print_parse "BUY 1 0 100 150.00";
  [%expect {| (Failure "unknown symbol: 0") |}]
;;

let%expect_test "parse error: symbol name is not alphanumeric" =
  print_parse "BUY 1 a-b 100 150.00";
  [%expect {| (Failure "invalid symbol: a-b") |}]
;;

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
  print_parse "HOLD 1 AAPL 100 150.00";
  [%expect
    {| ("Exchange_command.Verb.of_string: invalid string" (value HOLD)) |}]
;;

let%expect_test "parse error: missing fields" =
  print_parse "BUY 1 AAPL";
  print_parse "BUY";
  [%expect
    {|
    (Failure "expected: BUY|SELL <client_id> <symbol> <size> <price> [DAY|IOC]")
    (Failure "expected: BUY|SELL <client_id> <symbol> <size> <price> [DAY|IOC]")
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY 1 AAPL abc 150.00";
  print_parse "BUY 1 AAPL 0 150.00";
  print_parse "BUY 1 AAPL -5 150.00";
  [%expect
    {|
    (Failure "invalid size: abc")
    (Failure "size must be positive")
    (Failure "size must be positive")
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY 1 AAPL 100 xyz";
  [%expect
    {|
    (Failure
      "invalid price: xyz\
     \nexception: (Invalid_argument \"Float.of_string xyz\")")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY 1 AAPL 100 150.00 QQQ";
  [%expect {| ("Time_in_force.of_string: invalid string" (value QQQ)) |}]
;;

(* --- Round-trip: parse a command, submit, format result --- *)

let%expect_test "round-trip: parse a command, submit, format result" =
  let open Jsip_test_harness in
  let t = Harness.create () in
  (* Place a resting sell for someone to buy against. *)
  Harness.submit_
    t
    (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  (* A human types [AAPL]; everything downstream of the parse — the engine,
     the events, the formatted output — speaks id 0. *)
  let request =
    Exchange_command.parse ~directory "BUY 1 AAPL 100 150.00" |> ok_exn
  in
  (match request with
   | Submit order ->
     let events =
       Matching_engine.submit
         (Harness.engine t)
         ~participant:Harness.alice
         order
     in
     print_endline (Event_protocol.format_events ~directory events)
   | Book _ | Subscribe _ | Cancel _ -> print_endline "expected a Submit");
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    BBO AAPL bid=- ask=$150.00 x100
    ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 aggressor_client_oid=1 resting_client_oid=0 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    TRADE AAPL $150.00 x100
    BBO AAPL bid=- ask=-
    |}]
;;
