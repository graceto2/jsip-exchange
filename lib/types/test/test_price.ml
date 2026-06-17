open! Core
open Jsip_types

let%expect_test "of_int_cents and to_int_cents round-trip" =
  [%test_result: int]
    (Price.to_int_cents (Price.of_int_cents 15025))
    ~expect:15025
;;

let%expect_test "to_float: converts cents to dollars" =
  [%test_result: float]
    (Price.to_float (Price.of_int_cents 15025))
    ~expect:150.25;
  [%test_result: float] (Price.to_float (Price.of_int_cents 100)) ~expect:1.0;
  [%test_result: float] (Price.to_float (Price.of_int_cents 0)) ~expect:0.0
;;

let%expect_test "of_float_exn: converts dollar amount to cents" =
  [%test_result: int]
    (Price.to_int_cents (Price.of_float_exn 150.25))
    ~expect:15025;
  [%test_result: int]
    (Price.to_int_cents (Price.of_float_exn 1.0))
    ~expect:100;
  [%test_result: int]
    (Price.to_int_cents (Price.of_float_exn 0.01))
    ~expect:1
;;

let%expect_test "to_string_dollar: formatted display" =
  print_endline (Price.to_string_dollar (Price.of_int_cents 15025));
  print_endline (Price.to_string_dollar (Price.of_int_cents 100));
  print_endline (Price.to_string_dollar (Price.of_int_cents 5));
  print_endline (Price.to_string_dollar (Price.of_int_cents 0));
  [%expect {|
    $150.25
    $1.00
    $0.05
    $0.00
    |}]
;;

let%expect_test "of_string: parses dollar amounts with or without $" =
  [%test_result: int]
    (Price.to_int_cents (Price.of_string "150.25"))
    ~expect:15025;
  [%test_result: int]
    (Price.to_int_cents (Price.of_string "$150.25"))
    ~expect:15025;
  [%test_result: int]
    (Price.to_int_cents (Price.of_string "1.00"))
    ~expect:100
;;

let%expect_test "arithmetic: addition and subtraction" =
  let a = Price.of_int_cents 1500 in
  let b = Price.of_int_cents 250 in
  [%test_result: Price.t] Price.(a + b) ~expect:(Price.of_int_cents 1750);
  [%test_result: Price.t] Price.(a - b) ~expect:(Price.of_int_cents 1250)
;;

let%expect_test "arithmetic: multiplication by quantity" =
  let price = Price.of_int_cents 1500 in
  [%test_result: int] (Price.to_int_cents Price.(price * 100)) ~expect:150000
;;

let%expect_test "zero: is zero" =
  [%test_result: Price.t] Price.zero ~expect:(Price.of_int_cents 0)
;;

let%expect_test "negative to_string_dollar" =
  print_endline (Price.to_string_dollar (Price.of_int_cents (-1)));
  [%expect {| -$0.01 |}];
  print_endline (Price.to_string_dollar (Price.of_int_cents (-150)));
  [%expect {| -$1.50 |}]
;;

(* test is_marketable, is_more aggressive *)

let%expect_test "same price is not more aggressive" =
  [%test_result: bool]
    (Price.is_more_aggressive
       Side.Buy
       ~price:(Price.of_int_cents 100)
       ~than:(Price.of_int_cents 100))
    ~expect:false
;;

let%expect_test "higher buy price is more aggressive" =
  [%test_result: bool]
    (Price.is_more_aggressive
       Side.Buy
       ~price:(Price.of_int_cents 200)
       ~than:(Price.of_int_cents 2))
    ~expect:true
;;

let%expect_test "higher sell price is less aggressive" =
  [%test_result: bool]
    (Price.is_more_aggressive
       Side.Sell
       ~price:(Price.of_int_cents 333)
       ~than:(Price.of_int_cents 33))
    ~expect:false
;;

let%expect_test "buy price higher than resting price is marketable" =
  [%test_result: bool]
    (Price.is_marketable
       Side.Buy
       ~price:(Price.of_int_cents 234)
       ~resting_price:(Price.of_int_cents 3))
    ~expect:true
;;

let%expect_test "sell price same as resting price is marketable" =
  [%test_result: bool]
    (Price.is_marketable
       Side.Sell
       ~price:(Price.of_int_cents 234)
       ~resting_price:(Price.of_int_cents 234))
    ~expect:true
;;

let%expect_test "sell price higher than resting price is not marketable" =
  [%test_result: bool]
    (Price.is_marketable
       Side.Sell
       ~price:(Price.of_int_cents 234)
       ~resting_price:(Price.of_int_cents 3))
    ~expect:false
;;
