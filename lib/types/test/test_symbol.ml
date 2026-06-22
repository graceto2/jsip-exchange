open! Core
open Jsip_types
open Expect_test_helpers_core

(* invalid symbols *)

let%expect_test "of_string: empty string raises" =
  require_does_raise (fun () -> Symbol.of_string "");
  [%expect {| "Symbol.of_string: symbol must be non-empty" |}]
;;

let%expect_test "of_string: non alphanumeric characters in string raises" =
  require_does_raise (fun () -> Symbol.of_string "3j#ff");
  [%expect
    {| "Symbol.of_string: symbol must consist of only alphanum characters" |}]
;;

(* valid symbols *)

let%expect_test "of_string: lower case automatically capitalizes raises" =
  let symbol = Symbol.of_string "aapl" in
  print_endline (Symbol.to_string symbol);
  [%expect {| AAPL |}]
;;

let%expect_test "of_string: valid symbol" =
  let symbol = Symbol.of_string "TSLA" in
  print_endline (Symbol.to_string symbol);
  [%expect {| TSLA |}]
;;
