open! Core
open! Async
open Jsip_types
open Jsip_gateway

let fill_client_oid = ref 0

module Config = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; mutable inventory : int Symbol.Map.t
    ; mutable currently_resting_orders : Client_order_id.Set.t
    }
  [@@deriving sexp_of]
end

let seed_book (config : Config.t) conn =
  let submit request =
    let%map result =
      Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
    in
    match result with
    | Ok () -> ()
    | Error msg ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (msg : Error.t)]
  in
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let%bind () =
        fill_client_oid := !fill_client_oid + 1;
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Buy
           ; price = Price.of_int_cents (config.fair_value_cents - offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = !fill_client_oid
           }
           : Order.Request.t)
      and () =
        fill_client_oid := !fill_client_oid + 1;
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Sell
           ; price = Price.of_int_cents (config.fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = !fill_client_oid
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;


(* let update_and_remove_if_consumed ~config ~side curr_pos *)
let run (config : Config.t) conn =
  let%bind feed =
    Rpc.Pipe_rpc.dispatch Rpc_protocol.session_feed_rpc conn ()
  in
  let reader =
    match feed with
    | Ok (Ok (reader, _id)) -> reader
    | _ -> failwith "subscribe failed"
  in
  don't_wait_for
    (Pipe.iter_without_pushback reader ~f:(fun event ->
       match event with
       | Order_accept { request; _ } ->
         let client_oid = request.client_order_id in
         (* let symbol = request.symbol in let size = Size.to_int
            request.size in let current_pos = match Map.find config.inventory
            symbol with | None -> 0 | Some i -> i in *)
         config.currently_resting_orders
         <- Set.add config.currently_resting_orders client_oid
       | Order_cancel { client_order_id; _ } ->
         config.currently_resting_orders
         (* do i need to update inventory here? *)
         <- Set.remove config.currently_resting_orders client_order_id
       | Fill
           { aggressor_participant
           ; aggressor_client_order_id
           ; resting_participant
           ; resting_client_order_id
           ; aggressor_side
           ; symbol
           ; size
           ; _ (* add rest of unused fields *)
           } ->
         let resting_side = Side.flip aggressor_side in
         let size = Size.to_int size in
         let curr_pos =
           match Map.find config.inventory symbol with
           | None -> 0
           | Some int -> int
         in
         if Participant.equal aggressor_participant config.participant
         then ((* net size = signed size (match on aggressor side) *)
           match aggressor_side with
           | Buy ->
             config.inventory (** need to refactor lots of repetitive code *)
             <- Map.update config.inventory symbol ~f:(function
                  | None -> size
                  | Some curr -> curr + size);
             if curr_pos + size = 0
             then
               config.currently_resting_orders
               <- Set.remove
                    config.currently_resting_orders
                    aggressor_client_order_id
           | Sell ->
             config.inventory
             <- Map.update config.inventory symbol ~f:(function
                  | None -> -1 * size
                  | Some curr -> curr - size);
         if curr_pos - size = 0
         then
           config.currently_resting_orders
           <- Set.remove
                config.currently_resting_orders
                aggressor_client_order_id (* wrong place? *))
         else if Participant.equal resting_participant config.participant
         then (
           match resting_side with
           | Buy ->
             config.inventory
             <- Map.update config.inventory symbol ~f:(function
                  | None -> size
                  | Some curr -> curr + size);
             if curr_pos + size = 0
             then
               config.currently_resting_orders
               <- Set.remove
                    config.currently_resting_orders
                    resting_client_order_id
           | Sell ->
             config.inventory
             <- Map.update config.inventory symbol ~f:(function
                  | None -> -1 * size
                  | Some curr -> curr - size);
             if curr_pos - size = 0
             then
               config.currently_resting_orders
               <- Set.remove
                    config.currently_resting_orders
                    resting_client_order_id)
       | _ -> ()));
  return Deferred.never
;;
