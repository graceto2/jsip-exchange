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
    (* ; mutable inventory : int Symbol.Map.t

       ; mutable currently_resting_orders : Size.t Client_order_id.Map.t *)
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
          (request : Order.Submit_request.t)
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
           : Order.Submit_request.t)
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
           : Order.Submit_request.t)
      in
      Deferred.unit)
;;

(* let run (config : Config.t) conn = Deferred.unit *)
(* let update_and_remove_if_consumed ~config ~side curr_pos *)
(* let run (config : Config.t) conn = let%bind session_feed, _metadata =
   Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn () in (*
   raises exception if subscribe fails *) don't_wait_for
   (Pipe.iter_without_pushback session_feed ~f:(fun event -> match event with
   | Order_accept [{ request; order_id = _ }] -> let client_oid =
   request.client_order_id in config.currently_resting_orders <- Map.add_exn
   config.currently_resting_orders ~key:client_oid ~data:request.size |
   Order_cancel
   [{ client_order_id ; participant = _ ; symbol = _ ; remaining_size = _ ; reason = _ ; order_id = _ }]
   -> config.currently_resting_orders <- Map.remove
   config.currently_resting_orders client_order_id | Fill
   [{ fill_id = _ ; symbol ; price = _ ; size ; aggressor_order_id = _ ; aggressor_participant ; aggressor_side ; aggressor_client_order_id ; resting_order_id = _ ; resting_participant ; resting_client_order_id }]
   -> let resting_side = Side.flip aggressor_side in let size = ref
   (Size.to_int size) in let client_oid = ref aggressor_client_order_id in (*
   let curr_pos = match Map.find config.inventory symbol with | None -> 0 |
   Some int -> int in *) if Participant.equal aggressor_participant
   config.participant then ( match aggressor_side with Buy -> () | Sell ->
   size := !size * -1) else if Participant.equal resting_participant
   config.participant then ( match resting_side with | Buy -> () | Sell ->
   size := !size * -1; client_oid := resting_client_order_id) else
   [%log.error "market_maker: submit failed"]; (* made sign of size reflect
   whether participant bought or sold *) config.inventory <- Map.update
   config.inventory symbol ~f:(function | None -> !size | Some curr -> curr +
   !size); config.currently_resting_orders <- Map.update
   config.currently_resting_orders !client_oid ~f:(function | None ->
   failwith "couldn't find resting order" | Some remaining_size ->
   Size.of_int (Size.to_int remaining_size - !size)) (* match (Map.find
   config.currently_resting_orders client_oid) with | None -> () | Some
   remaining_size -> *) (* if curr_pos + !size = 0 then
   config.currently_resting_orders <- Map.remove
   config.currently_resting_orders !client_oid *) | _ -> ())); Deferred.unit
   ;; *)
