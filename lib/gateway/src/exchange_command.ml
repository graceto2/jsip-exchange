open! Core
open! Jsip_types

type verb =
  | Buy
  | Sell
  | Book
  | Subscribe
[@@deriving string ~case_insensitive]

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

(* val parse : ?default_participant:Participant.t -> string -> t Or_error.t *)
