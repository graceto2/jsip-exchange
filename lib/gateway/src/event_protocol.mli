open! Core
open Jsip_types

(** Format an exchange event as a single line of human-readable text. *)
val format_event : Exchange_event.t -> string

(** Format a list of events, one per line. *)
val format_events : Exchange_event.t list -> string
