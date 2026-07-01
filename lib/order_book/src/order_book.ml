open! Core
open Jsip_types
(* open Async_log_kernel.Ppx_log_syntax *)

(* [CR] claude for Grace: Naming — module names are snake_case in this
   codebase (see the conventions in CLAUDE.md), so this should be
   [Order_key], not [OrderKey]. *)
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

let side_list_ids t side =
  match (side : Side.t) with
  | Buy ->
    Map.to_alist t.bids |> List.map ~f:fst |> List.map ~f:snd |> List.rev
  | Sell -> Map.to_alist t.asks |> List.map ~f:fst |> List.map ~f:snd
;;

let add t order =
  let side = Order.side order in
  let price = Order.price order in
  let order_id = Order.order_id order in
  t.ids <- Map.add_exn t.ids ~key:order_id ~data:(side, price);
  match side with
  | Buy -> t.bids <- Map.add_exn t.bids ~key:(price, order_id) ~data:order
  | Sell -> t.asks <- Map.add_exn t.asks ~key:(price, order_id) ~data:order
;;

let find t order_id =
  let order_side_price = Map.find t.ids order_id in
  match order_side_price with
  | Some (side, price) ->
    (match side with
     | Side.Buy -> Map.find t.bids (price, order_id)
     | Sell -> Map.find t.asks (price, order_id))
  | None -> None
;;

let remove' t order_id =
  let order_side_price = Map.find t.ids order_id in
  let removed = find t order_id in
  t.ids <- Map.remove t.ids order_id;
  (match order_side_price with
   | Some (side, price) ->
     (match side with
      | Side.Buy -> t.bids <- Map.remove t.bids (price, order_id)
      | Sell -> t.asks <- Map.remove t.asks (price, order_id))
   | None -> ());
  removed
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

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
  (* [CR] claude for Grace: This reduce is correct only by accident. When
     [than] is *more aggressive* than [order] (i.e. [order] is not more
     aggressive, but the prices are not equal either), we fall through to the
     order-id tie-break instead of picking [than]'s better price. It happens
     to return the right answer today only because [side_list] hands back
     orders already sorted by (price, id) — so the function's correctness
     silently depends on a caller invariant it doesn't state. Prefer folding
     with an explicit "better price wins, tie-break on older id" rule that is
     correct regardless of input order (the same helper would fix
     [snapshot_side] below). *)
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

let orders_on_side t side = side_list t side
let order_ids_on_side t side = side_list_ids t side
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
(* [CR] claude for Grace: This comparator is not a valid total order, so
   [List.sort] can produce a wrong result. Take two Sell orders A=(price 100,
   id 2) and B=(price 99, id 9): [compare A B] returns -1 (A not more
   aggressive, but id 2 < id 9), and [compare B A] also returns -1 (B *is*
   more aggressive). Both claim to sort first, which violates antisymmetry —
   the more-aggressive order can end up behind the worse one. The price
   comparison must fully dominate; only fall back to the id when prices are
   *equal*. Reachable whenever an aggressive price arrives with a later
   (larger) id, and no current test exercises it. *)
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
