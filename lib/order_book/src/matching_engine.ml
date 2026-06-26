open! Core
open Jsip_types

type t =
  { books : Order_book.t Symbol.Map.t
  ; order_id_gen : Order_id.Generator.t
  ; mutable next_fill_id : int
  ; client_order_ids : Order.t Client_order_id.Table.t
  ; server_id_to_client_id : Client_order_id.t Order_id.Table.t
  }
[@@deriving sexp_of]

let create symbols =
  let books =
    List.map symbols ~f:(fun sym -> sym, Order_book.create sym)
    |> Symbol.Map.of_alist_exn
  in
  { books
  ; order_id_gen = Order_id.Generator.create ()
  ; next_fill_id = 1
  ; client_order_ids = Client_order_id.Table.create ()
  ; server_id_to_client_id = Order_id.Table.create ()
  }
;;

let book t symbol = Map.find t.books symbol

(** Run the matching loop: repeatedly find a compatible resting order and
    fill against it. Returns the list of Fill and Trade_report events
    produced, and the next fill_id to use. *)
let rec match_loop t ~book ~order ~fill_id =
  if Size.( <= ) (Order.remaining_size order) Size.zero
  then [], fill_id
  else (
    match Order_book.find_match book order with
    | None -> [], fill_id
    | Some resting ->
      let fill_size =
        Size.min (Order.remaining_size order) (Order.remaining_size resting)
      in
      Order.fill order ~by:fill_size;
      Order.fill resting ~by:fill_size;
      if Order.is_fully_filled resting
      then Order_book.remove book (Order.order_id resting);
      let fill_event =
        Exchange_event.Fill
          { fill_id
          ; symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          ; aggressor_order_id = Order.order_id order
          ; aggressor_participant = Order.participant order
          ; aggressor_side = Order.side order
          ; aggressor_client_order_id =
              Hashtbl.find_exn
                t.server_id_to_client_id
                (Order.order_id order)
          ; resting_order_id = Order.order_id resting
          ; resting_participant = Order.participant resting
          ; resting_client_order_id =
              Hashtbl.find_exn
                t.server_id_to_client_id
                (Order.order_id resting)
          }
      in
      let trade_event =
        Exchange_event.Trade_report
          { symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          }
      in
      let remaining_events, next_fill_id =
        match_loop t ~book ~order ~fill_id:(fill_id + 1)
      in
      fill_event :: trade_event :: remaining_events, next_fill_id)
;;

let submit t (request : Order.Request.t) =
  match Map.find t.books request.symbol with
  | None ->
    [ Exchange_event.Order_reject { request; reason = "unknown symbol" } ]
  | Some book ->
    let order_id = Order_id.Generator.next t.order_id_gen in
    let order = Order.create request ~order_id in
    let client_order_id = request.client_order_id in
    let accepted = Exchange_event.Order_accept { order_id; request } in
    let rejected =
      Exchange_event.Order_reject
        { request; reason = "Client order ID already in use" }
    in
    (* check if valid client_order_id *)
    let prev_order = Hashtbl.find t.client_order_ids client_order_id in
    (match prev_order with
     | Some _ -> [ rejected ]
     | None ->
       Hashtbl.add_exn t.client_order_ids ~key:client_order_id ~data:order;
       Hashtbl.add_exn
         t.server_id_to_client_id
         ~key:order_id
         ~data:client_order_id;
       (* Snapshot BBO before matching so we can detect changes. *)
       let bbo_before = Order_book.best_bid_offer book in
       (* Match *)
       let fill_events, next_fill_id =
         match_loop t ~book ~order ~fill_id:t.next_fill_id
       in
       t.next_fill_id <- next_fill_id;
       (* Post-match: rest on book or cancel unfilled remainder. *)
       let post_events =
         if Size.( > ) (Order.remaining_size order) Size.zero
         then (
           match Order.time_in_force order with
           | Day ->
             Order_book.add book order;
             []
           | Ioc ->
             [ Exchange_event.Order_cancel
                 { order_id
                 ; participant = Order.participant order
                 ; symbol = Order.symbol order
                 ; remaining_size = Order.remaining_size order
                 ; reason = Ioc_remainder
                 ; client_order_id = request.client_order_id
                 }
             ])
         else []
       in
       (* Emit BBO update if the best bid or ask changed. *)
       let bbo_after = Order_book.best_bid_offer book in
       let bbo_events =
         if Bbo.equal bbo_before bbo_after
         then []
         else
           [ Exchange_event.Best_bid_offer_update
               { symbol = Order.symbol order; bbo = bbo_after }
           ]
       in
       List.concat [ [ accepted ]; fill_events; post_events; bbo_events ])
;;

let cancel t (cancel_request : Order.Cancel_request.t) =
  let client_order_id = cancel_request.client_order_id in
  let participant = cancel_request.participant in
  let order = Hashtbl.find t.client_order_ids client_order_id in
  (* find order in client_order_id, which contains ALL orders that have been
     successfully submitted so far, even cancelled/filled ones *)
  match order with
  | None ->
    [ Exchange_event.Cancel_reject
        { participant
        ; client_order_id
        ; reason = "Order with that ID was never placed"
        }
    ]
  | Some order ->
    (match Map.find t.books (Order.symbol order) with
     (* find order in our order book -- here, filled/cancelled orders have
        been removed *)
     | None ->
       [ Exchange_event.Cancel_reject
           { participant; client_order_id; reason = "Order not found" }
       ]
     | Some book ->
       let symbol = Order.symbol order in
       let remaining_size = Order.remaining_size order in
       let order_id = Order.order_id order in
       let bbo_before = Order_book.best_bid_offer book in
       (match Order_book.find book order_id with
        | None ->
          [ Exchange_event.Cancel_reject
              { participant; client_order_id; reason = "Order not found" }
          ]
        | Some order ->
          let cancelled =
            Exchange_event.Order_cancel
              { order_id
              ; participant
              ; symbol
              ; remaining_size
              ; reason = Cancel_reason.Participant_requested
              ; client_order_id
              }
          in
          Order_book.remove book order_id;
          (* emit BBO update if cancelled order was at best price *)
          let bbo_after = Order_book.best_bid_offer book in
          let bbo_events =
            if Bbo.equal bbo_before bbo_after
            then []
            else
              [ Exchange_event.Best_bid_offer_update
                  { symbol = Order.symbol order; bbo = bbo_after }
              ]
          in
          List.concat [ [ cancelled ]; bbo_events ]))
;;

(* pt 1 ex 5: did not finish *)
(* let remove_day_orders_on_side order_book ~side = let day_orders =
   List.filter (Order_book.orders_on_side order_book side) ~f:(fun order ->
   Time_in_force.equal (Order.time_in_force order) Time_in_force.Day) in
   List.map day_orders ~f:(fun order -> Exchange_event.Order_cancel
   [{ order_id = Order.order_id order ; participant = Order.participant order ; symbol = Order.symbol order ; remaining_size = Order.size order ; reason = Cancel_reason.End_of_day }])
   ;; *)

(* do separately using built in functions or do at once using recursion? *)
(* let remove_day_orders_on_side order_book ~side = let oid = Order.order_id
   order in match Order_book.orders_on_side order_book side with | order ::
   rest -> if order.time_in_force = Time_in_force.Day then Order_book.remove
   order_book (Order.order_id order) else () ;; *)

(* turn into pipe, use recursive approach *)
(* let end_of_day t event_list = List.iter t.books ~f:(fun book -> List.iter
   (Order_book.orders_on_side book Side.Buy) ~f:()) *)
