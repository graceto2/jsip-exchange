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
        (* how much of each symbol we have successfully bought/sold *)
    ; mutable currently_resting_orders : Size.t Client_order_id.Map.t
    (* maps client_order_ids (corresponding to currently resting orders) ->
       remaining size in the order book *)
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

let remove_resting_order (config : Config.t) client_oid =
  print_endline "entered remove resting order";
  config.currently_resting_orders
  <- Map.remove config.currently_resting_orders client_oid
;;

let add_resting_order (config : Config.t) client_oid size =
  config.currently_resting_orders
  <- Map.add_exn config.currently_resting_orders ~key:client_oid ~data:size
;;

(* let update_and_remove_if_consumed ~config ~side curr_pos *)
(* the inventory is only updated when a fill goes through, resting orders
   only updated on order accept, order cancel, fill *)
(* need to fix bug where when a fill completes the order, does not get
   removed *)
let run (config : Config.t) conn =
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       match event with
       | Order_accept { request; order_id = _ } ->
         (* record id in currently_resting_orders *)
         add_resting_order config request.client_order_id request.size
       | Order_cancel
           { client_order_id
           ; participant = _
           ; symbol = _
           ; remaining_size = _
           ; reason = _
           ; order_id = _
           } ->
         remove_resting_order config client_order_id
       | Fill
           { fill_id = _
           ; symbol
           ; price = _
           ; size
           ; aggressor_order_id = _
           ; aggressor_participant = _
           ; aggressor_side
           ; aggressor_client_order_id
           ; resting_order_id = _
           ; resting_participant
           ; resting_client_order_id
           } ->
         let side = ref aggressor_side in
         let size = ref (Size.to_int size) in
         let client_oid = ref aggressor_client_order_id in
         if Participant.equal resting_participant config.participant
         then (
           side := Side.flip aggressor_side;
           client_oid := resting_client_order_id);
         (* do we need to check whether we are on some side? or do we assume
            we will be on at least one side of the fill *)
         (match !side with Sell -> size := !size * -1 | Buy -> ());
         config.inventory
         (* update inventory with added size for symbol *)
         <- Map.update config.inventory symbol ~f:(function
              | None -> !size
              | Some curr -> curr + !size);
         config.currently_resting_orders
         (* update remaining size in currently resting orders, and remove if
            completely filled *)
         <- Map.update
              config.currently_resting_orders
              !client_oid
              ~f:(function
              | None ->
                failwith "couldn't find resting order"
                (* is this the correct way to error handle? *)
              | Some remaining_size ->
                print_endline "was here ";
                let remaining_size = Size.to_int remaining_size in
                print_endline
                  [%string
                    "remaining size: %{remaining_size#Int}, size removed: \
                     %{!size#Int}"];
                if remaining_size = !size
                then
                  config.currently_resting_orders
                  <- Map.remove config.currently_resting_orders !client_oid;
                Size.of_int (remaining_size - !size))
       | _ -> ()));
  return ()
;;
