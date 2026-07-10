open! Core
open! Jsip_types

(* wrap in module *)
module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving string ~case_insensitive]
end

type t =
  | Submit of Order.Request.t
  | Cancel of Client_order_id.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t

(* No "as <name>" should be specified in the command, since we require participants to log in before submitting orders. *)

let parse_buy_or_sell input_tokens ~side =
  let open Result.Let_syntax in
  match input_tokens with
  | client_order_id :: symbol_str :: size_str :: price_str :: rest ->
    let%bind size =
      match Int.of_string_opt size_str with
      | Some n when n > 0 -> Ok n
      | Some _ -> Error "size must be positive"
      | None -> Error [%string "invalid size: %{size_str}"]
    in
    let%bind price =
      try Ok (Price.of_string price_str) with
      | exn ->
        let exn_str = Exn.to_string exn in
        Error [%string "invalid price: %{price_str}\nexception: %{exn_str}"]
    in
    let%bind symbol =
      (* The client now submits the numeric symbol id directly, not a ticker.
         Range-checking (is this a symbol we trade?) happens server-side; here
         we only parse the integer. *)
      match Int.of_string_opt symbol_str with
      | Some n when n >= 0 -> Ok (Symbol_id.of_int n)
      | Some _ -> Error "symbol id must be non-negative"
      | None -> Error [%string "invalid symbol id: %{symbol_str}"]
    in
    let%bind time_in_force, rest =
      match rest with
      | tif_str :: rest' ->
        (match String.uppercase tif_str with
         | _ -> Ok (Time_in_force.of_string tif_str, rest'))
      | [] -> Ok (Day, [])
    in
    let%bind () =
      match rest with
      | [] -> Ok ()
      | _ ->
        let trailing = String.concat ~sep:" " rest in
        Error [%string "unexpected trailing arguments: %{trailing}"]
    in
    Ok
      (Submit
         ({ client_order_id = Int.of_string client_order_id
          ; symbol
          ; side
          ; price
          ; size = Size.of_int size
          ; time_in_force
          }
          : Order.Request.t))
  | _ ->
    Error
      "expected: BUY|SELL <client_id> <symbol_id> <size> <price> [DAY|IOC]"
;;

let parse_buy_or_sell_exn list ~side =
  Result.ok_or_failwith (parse_buy_or_sell list ~side)
;;

let parse_exn string =
  let line = String.strip string in
  let parts =
    String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  in
  match parts with
  | [] -> failwith "empty command"
  | first_word :: rest ->
    let verb = Verb.of_string first_word in
    (match verb with
     | Buy -> parse_buy_or_sell_exn rest ~side:Side.Buy
     | Sell -> parse_buy_or_sell_exn rest ~side:Side.Sell
     | Book ->
       (match rest with
        | symbol :: [] -> Book (Symbol_id.of_int (Int.of_string symbol))
        | _ -> failwith "failed book, too many entries")
     | Subscribe ->
       (match rest with
        | symbol :: [] -> Subscribe (Symbol_id.of_int (Int.of_string symbol))
        | _ -> failwith "failed subscribe, too many entries"))
;;

let parse string = Or_error.try_with (fun () -> parse_exn string)
