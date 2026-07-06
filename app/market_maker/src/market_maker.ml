open! Core
open! Async
open Jsip_types
open Jsip_gateway

module Config = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; fill_client_oid : int ref
        (* [CR-soon] claude for Grace: [Config.t] reads as static setup (it's
           [sexp_of]'d and passed to [seed_book]), but these two [mutable]
           fields are live per-run state. Consider splitting them into a
           separate [State.t] so the config stays immutable and the running
           state is clearly distinguished. *)
    ; mutable inventory : int Symbol.Map.t
        (* How much of each symbol we have successfully bought/sold. Should
           this be of Size.t? *)
    ; mutable currently_resting_orders : Size.t Client_order_id.Map.t
    (* Maps client_order_ids (corresponding to currently resting orders) ->
       remaining size in the order book. *)
    }
  [@@deriving sexp_of]
end

let reset_fill_client_oids (config : Config.t) = config.fill_client_oid := 0

let seed_book (config : Config.t) conn =
  let submit request =
    let%map result =
      Rpc.Rpc.dispatch_exn
        Rpc_protocol.submit_order_rpc
        conn
        (Order.Submit_wire.of_submit_request request)
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
        config.fill_client_oid := !(config.fill_client_oid) + 1;
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Buy
           ; price = Price.of_int_cents (config.fair_value_cents - offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = !(config.fill_client_oid)
           }
           : Order.Submit_request.t)
      and () =
        config.fill_client_oid := !(config.fill_client_oid) + 1;
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Sell
           ; price = Price.of_int_cents (config.fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = !(config.fill_client_oid)
           }
           : Order.Submit_request.t)
      in
      Deferred.unit)
;;

let remove_resting_order (config : Config.t) client_oid =
  config.currently_resting_orders
  <- Map.remove config.currently_resting_orders client_oid
;;

let add_resting_order (config : Config.t) client_oid size =
  config.currently_resting_orders
  <- Map.add_exn config.currently_resting_orders ~key:client_oid ~data:size
;;

let add_size_to_symbol (config : Config.t) symbol (signed_size : int) =
  config.inventory
  <- Map.update config.inventory symbol ~f:(function
       | None -> signed_size
       | Some curr -> curr + signed_size)
;;

let update_resting_order_size (config : Config.t) client_oid (size : Size.t) =
  let size = Size.to_int size in
  config.currently_resting_orders
  <- (match Map.find config.currently_resting_orders client_oid with
      | Some remaining_size ->
        let remaining_size = Size.to_int remaining_size in
        let updated_size = Size.of_int (remaining_size - size) in
        Map.change config.currently_resting_orders client_oid ~f:(function
          | Some _ -> Some updated_size
          | None -> None)
      | None ->
        [%log.error
          "Couldn't find that order in config.currently_resting_orders"];
        config.currently_resting_orders)
;;

(* let add_size_to_inventory (config:Config.t) ~symbol (signed_size:int) = *)
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
         let signed_size = ref (Size.to_int size) in
         let client_oid = ref aggressor_client_order_id in
         if Participant.equal resting_participant config.participant
         then (
           side := Side.flip aggressor_side;
           client_oid := resting_client_order_id);
         (* Assume that market maker is on one side of the fill. We don't
            check that it is on the other side if it is not on the resting
            side. *)
         (match !side with
          | Sell -> signed_size := !signed_size * -1
          | Buy -> ());
         (* Update inventory with added size for symbol. *)
         add_size_to_symbol config symbol !signed_size;
         (* Update the size of currently resting orders - removes however
            much the fill sold/bought against us. *)
         update_resting_order_size config !client_oid size;
         (* Remove the corresponding resting order if the fill consumed the
            full remaining size of the order *)
         (match Map.find config.currently_resting_orders !client_oid with
          | None -> ()
          | Some size ->
            if Size.to_int size = 0
            then
              config.currently_resting_orders
              <- Map.remove config.currently_resting_orders !client_oid)
       | Order_reject _ | Cancel_reject _ | Best_bid_offer_update _
       | Trade_report _ ->
         ()));
  return ()
;;
