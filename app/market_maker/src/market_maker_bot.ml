open! Core
open! Async
open Jsip_types

(* [CR] claude for Grace: This whole module is unfinished and unwired — the
   inventory/remaining-size update in [on_event] is entirely commented out
   (so it's a no-op bot), the final match arm logs "submit failed" on
   a *Fill* event, nothing constructs a [Bot_spec] from it, and it has no
   tests. It also duplicates [Config] and the seed logic from
   market_maker.ml. Either finish + test it and port the [Market_maker] over
   to the [Bot] interface (the intended end state), or leave it out of the
   commit — a half-built second copy will rot and drift from the real one. *)

(* open Jsip_bot_runtime *)
module Context = Jsip_bot_runtime.Bot_runtime.Context

module Config = struct
  type t =
    { symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; fill_client_oid : int ref
    ; mutable inventory : int Symbol.Map.t
    ; mutable currently_resting_orders : Size.t Client_order_id.Map.t
    }
  [@@deriving sexp_of]
end

let name = "market_maker"

let on_start (config : Config.t) (context : Context.t) =
  let submit request = Context.submit context request in
  let participant = Context.participant context in
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let%bind () =
        config.fill_client_oid := !(config.fill_client_oid) + 1;
        Deferred.ignore_m
          (submit
             ({ symbol = config.symbol
              ; participant
              ; side = Buy
              ; price = Price.of_int_cents (config.fair_value_cents - offset)
              ; size = Size.of_int config.size_per_level
              ; time_in_force = Day
              ; client_order_id = !(config.fill_client_oid)
              }
              : Order.Submit_request.t))
      and () =
        config.fill_client_oid := !(config.fill_client_oid) + 1;
        (* is deferred ignore okay here? *)
        Deferred.ignore_m
          (submit
             ({ symbol = config.symbol
              ; participant
              ; side = Sell
              ; price = Price.of_int_cents (config.fair_value_cents + offset)
              ; size = Size.of_int config.size_per_level
              ; time_in_force = Day
              ; client_order_id = !(config.fill_client_oid)
              }
              : Order.Submit_request.t))
      in
      Deferred.unit)
;;

(* market maker only requotes *)
let on_tick _config _context = return ()
let on_event (_config : Config.t) (_context : Context.t) _event = ()
