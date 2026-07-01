open! Core
open! Async
open Jsip_types

(* open Jsip_bot_runtime *)
module Context = Jsip_bot_runtime.Bot_runtime.Context

let fill_client_oid = ref 0

module Config = struct
  type t =
    { symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
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
        fill_client_oid := !fill_client_oid + 1;
        Deferred.ignore_m
          (submit
             ({ symbol = config.symbol
              ; participant
              ; side = Buy
              ; price = Price.of_int_cents (config.fair_value_cents - offset)
              ; size = Size.of_int config.size_per_level
              ; time_in_force = Day
              ; client_order_id = !fill_client_oid
              }
              : Order.Submit_request.t))
      and () =
        fill_client_oid := !fill_client_oid + 1;
        (* is deferred ignore okay here? *)
        Deferred.ignore_m
          (submit
             ({ symbol = config.symbol
              ; participant
              ; side = Sell
              ; price = Price.of_int_cents (config.fair_value_cents + offset)
              ; size = Size.of_int config.size_per_level
              ; time_in_force = Day
              ; client_order_id = !fill_client_oid
              }
              : Order.Submit_request.t))
      in
      Deferred.unit)
;;

(* market maker only requotes *)
let on_tick _config _context = return ()

let on_event (config : Config.t) (context : Context.t) event =
  let participant = Context.participant context in
  match event with
  | Exchange_event.Order_accept { request; order_id = _ } ->
    let client_oid = request.client_order_id in
    config.currently_resting_orders
    <- Map.add_exn
         config.currently_resting_orders
         ~key:client_oid
         ~data:request.size
  | Order_cancel
      { client_order_id = _
      ; participant = _
      ; symbol = _
      ; remaining_size = _
      ; reason = _
      ; order_id = _
      } ->
    ()
    (* config.currently_resting_orders <- Map.remove
       config.currently_resting_orders client_order_id *)
  | Fill
      { fill_id = _
      ; symbol = _
      ; price = _
      ; size
      ; aggressor_order_id = _
      ; aggressor_participant
      ; aggressor_side
      ; aggressor_client_order_id
      ; resting_order_id = _
      ; resting_participant
      ; resting_client_order_id
      } ->
    let resting_side = Side.flip aggressor_side in
    let size = ref (Size.to_int size) in
    let client_oid = ref aggressor_client_order_id in
    (* let curr_pos = match Map.find config.inventory symbol with | None -> 0
       | Some int -> int in *)
    if Participant.equal aggressor_participant participant
    then (match aggressor_side with Buy -> () | Sell -> size := !size * -1)
    else if Participant.equal resting_participant participant
    then (
      match resting_side with
      | Buy -> ()
      | Sell ->
        size := !size * -1;
        client_oid := resting_client_order_id)
    else [%log.error "market_maker: submit failed"]
    (* made sign of size reflect whether participant bought or sold *)
    (* config.inventory <- Map.update config.inventory symbol ~f:(function |
       None -> !size | Some curr -> curr + !size);
       config.currently_resting_orders <- Map.update
       config.currently_resting_orders !client_oid ~f:(function | None ->
       failwith "couldn't find resting order" | Some remaining_size ->
       Size.of_int (Size.to_int remaining_size - !size)) *)
  | _ -> ()
;;
