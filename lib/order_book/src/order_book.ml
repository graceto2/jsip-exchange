open! Core
open Jsip_types
(* open Async_log_kernel.Ppx_log_syntax *)

module OrderKey = struct
  type t = Price.t * Order_id.t [@@deriving compare, sexp]

  include functor Comparable.Make_plain
end

type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t OrderKey.Map.t
  ; mutable asks : Order.t OrderKey.Map.t
  ; mutable ids : (Side.t * Price.t) Order_id.Map.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Map.empty (module OrderKey)
  ; asks = Map.empty (module OrderKey)
  ; ids = Map.empty (module Order_id)
  }
;;

let symbol t = t.symbol

let side_list t side =
  match (side : Side.t) with
  | Buy -> Map.to_alist t.bids |> List.map ~f:snd |> List.rev
  | Sell -> Map.to_alist t.asks |> List.map ~f:snd
;;

(* let set_side_list t side orders = match (side : Side.t) with | Buy ->
   bids.add ~key: | Sell -> t.asks <- orders ;; *)

let add t order =
  let side = Order.side order in
  let price = Order.price order in
  let order_id = Order.order_id order in
  t.ids <- Map.add_exn t.ids ~key:order_id ~data:(side, price);
  match side with
  | Buy -> t.bids <- Map.add_exn t.bids ~key:(price, order_id) ~data:order
  | Sell ->
    t.asks
    <- Map.add_exn
         t.asks
         ~key:(Order.price order, Order.order_id order)
         ~data:order
;;

(* Edited order book data structure, below is old code *)

(* set_side_list t side (order :: side_list t side) *)

(* let removed = ref None in match order_side_price with | Some (side, price)
   -> (match side with | Side.Buy -> t.bids <- Map.update t.bids (price,
   order_id) ~f:(function Some order -> removed := Some order); None (*
   t.bids <- Map.remove t.bids (price, order_id) *) | Sell -> None) | None ->
   None *)

(* let remove_from t side order_id = let orders = side_list t side in match
   List.partition_tf orders ~f:(fun o -> Order_id.equal (Order.order_id o)
   order_id) with | [], _ -> None | [ found ], rest -> set_side_list t side
   rest; Some found | matches, _ ->
   [%log.info "BUG: More than one order matching order_id found when removing" (order_id : Order_id.t) (matches : Order.t list) (t.symbol : Symbol.t) (side : Side.t)];
   None in match remove_from t Buy order_id with | Some _ as result -> result
   | None -> remove_from t Sell order_id *)

let find t order_id =
  let order_side_price = Map.find t.ids order_id in
  match order_side_price with
  | Some (side, price) ->
    (match side with
     | Side.Buy -> Map.find t.bids (price, order_id)
     | Sell -> Map.find t.asks (price, order_id))
  | None -> None
;;

(* uses find then remove, so that we can return the removed value for
   testing. is there a better way? *)
let remove' t order_id =
  let order_side_price = Map.find t.ids order_id in
  let removed = find t order_id in
  (match order_side_price with
   | Some (side, price) ->
     (match side with
      | Side.Buy -> t.bids <- Map.remove t.bids (price, order_id)
      | Sell -> t.asks <- Map.remove t.asks (price, order_id))
   | None -> ());
  removed
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

(* let find_in side = List.find (side_list t side) ~f:(fun o ->
   Order_id.equal (Order.order_id o) order_id) in match find_in Buy with Some
   _ as result -> result | None -> find_in Sell *)

(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)

let find_match t incoming =
  let incoming_side = Order.side incoming in
  let opposite_side = Side.flip incoming_side in
  let resting_orders = side_list t opposite_side in
  let marketable_resting_orders =
    List.filter resting_orders ~f:(fun resting_order ->
      Price.is_marketable
        incoming_side
        ~price:(Order.price incoming)
        ~resting_price:(Order.price resting_order))
  in
  List.reduce marketable_resting_orders ~f:(fun order than ->
    if Price.is_more_aggressive
         opposite_side
         ~price:(Order.price order)
         ~than:(Order.price than)
    then order
    else if Order_id.compare (Order.order_id than) (Order.order_id order) > 0
    then order
    else than)
;;

(* match marketable_resting_orders with | [] -> None | first :: rest -> Some
   (List.fold rest ~init:first ~f:(fun order than -> if
   Price.is_more_aggressive opposite_side ~price:(Order.price order)
   ~than:(Order.price than) then order else if Order_id.compare
   (Order.order_id than) (Order.order_id order) > 0 then order else than)) *)

let orders_on_side t side = side_list t side
let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks
let count t side = List.length (side_list t side)

let best_price t side =
  match side_list t side with
  | [] -> None
  | first :: rest ->
    Some
      (List.fold rest ~init:(Order.price first) ~f:(fun best order ->
         let price = Order.price order in
         if Price.is_more_aggressive side ~price ~than:best
         then price
         else best))
;;

let best_level t side : Level.t option =
  match best_price t side with
  | None -> None
  | Some price ->
    let total_size =
      List.fold (side_list t side) ~init:Size.zero ~f:(fun acc order ->
        if Price.equal (Order.price order) price
        then Size.( + ) acc (Order.remaining_size order)
        else acc)
    in
    Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

(* sort from most aggressive to least aggressive *)
let snapshot_side t (side : Side.t) =
  let priority_order_book =
    List.sort (side_list t side) ~compare:(fun this that ->
      if Price.is_more_aggressive
           side
           ~price:(Order.price this)
           ~than:(Order.price that)
      then -1
      else if Order_id.compare (Order.order_id this) (Order.order_id that)
              < 0
      then -1
      else if Order_id.compare (Order.order_id this) (Order.order_id that)
              = 0
      then 0
      else 1)
  in
  priority_order_book |> List.map ~f:Level.of_order
;;

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  let remove = remove'
end
