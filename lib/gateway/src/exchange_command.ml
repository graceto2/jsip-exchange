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
  | Book of Symbol.t
  | Subscribe of Symbol.t

(* Default participant when no "as <name>" is specified in the command.
   [parse_command_with_default_participant] overrides this with the
   caller-supplied default. *)
let default_p = Participant.of_string "anonymous"

let parse_buy_or_sell ?default_participant list ~side =
  let open Result.Let_syntax in
  match list with
  | symbol_str :: size_str :: price_str :: rest ->
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
      try Ok (Symbol.of_string symbol_str) with
      | exn ->
        let exn_str = Exn.to_string exn in
        Error
          [%string "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
    in
    let%bind time_in_force, rest =
      match rest with
      | tif_str :: rest' ->
        (match String.uppercase tif_str with
         | "AS" -> Ok (Time_in_force.Day, rest)
         | _ -> Ok (Time_in_force.of_string tif_str, rest'))
      | [] -> Ok (Day, [])
    in
    let%bind participant =
      match rest with
      | "as" :: name :: _ | "AS" :: name :: _ ->
        Ok (Participant.of_string name)
      | [] ->
        (match default_participant with
         | Some p -> Ok p
         | None -> Ok default_p)
      | _ ->
        let trailing = String.concat ~sep:" " rest in
        Error [%string "unexpected trailing arguments: %{trailing}"]
    in
    Ok
      (Submit
         ({ symbol
          ; participant
          ; side
          ; price
          ; size = Size.of_int size
          ; time_in_force
          }
          : Order.Request.t))
  | _ ->
    Error "expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]"
;;

let parse_buy_or_sell_exn ?default_participant list ~side =
  Result.ok_or_failwith (parse_buy_or_sell ?default_participant list ~side)
;;

let parse_exn ?default_participant string =
  let line = String.strip string in
  let parts =
    String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  in
  match parts with
  | [] -> failwith "empty command"
  | first_word :: rest ->
    let verb = Verb.of_string first_word in
    (match verb with
     | Buy -> parse_buy_or_sell_exn ?default_participant rest ~side:Side.Buy
     | Sell ->
       parse_buy_or_sell_exn ?default_participant rest ~side:Side.Sell
     | Book ->
       (match rest with
        | symbol :: [] -> Book (Symbol.of_string symbol)
        | _ -> failwith "failed book, too many entries")
     | Subscribe ->
       (match rest with
        | symbol :: [] -> Subscribe (Symbol.of_string symbol)
        | _ -> failwith "failed subscribe, too many entries"))
;;

let parse ?default_participant string =
  Or_error.try_with (fun () -> parse_exn ?default_participant string)
;;
