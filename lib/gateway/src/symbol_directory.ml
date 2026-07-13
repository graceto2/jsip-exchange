open! Core
open Jsip_types

(* [symbols] is indexed by id — the id *is* the array index, mirroring
   [Matching_engine]'s book array — and [ids] is the reverse lookup a parser
   needs to turn a typed ticker back into an id. The two must agree, which is
   why they are built together in [of_symbols] and nowhere else. *)
type t =
  { symbols : Symbol.t array
  ; ids : Symbol_id.t Symbol.Table.t
  }
[@@deriving sexp_of]

(* The duplicate-ticker check is not a guard we remember to write: a
   [Hashtbl] cannot hold two ids for one key, so [of_alist_or_error] detects
   the collision while building the reverse table, in the same pass — and it
   names the offending ticker, which we tag rather than discard. *)
let of_symbols (symbols : Symbol.t list) : t Or_error.t =
  if List.is_empty symbols
  then Or_error.error_string "symbol directory must not be empty"
  else (
    let symbols_array = Array.of_list symbols in
    let pairs =
      List.mapi symbols ~f:(fun i symbol -> symbol, Symbol_id.of_int i)
    in
    let ids = Hashtbl.of_alist_or_error (module Symbol) pairs in
    match ids with
    | Error error ->
      Error (Error.tag error ~tag:"symbol directory has a duplicate ticker")
    | Ok ids -> Ok { symbols = symbols_array; ids })
;;

let num_symbols t = Array.length t.symbols

let symbol_of_id t id =
  let i = Symbol_id.to_int id in
  if i >= 0 && i < Array.length t.symbols then Some t.symbols.(i) else None
;;

let id_of_symbol t symbol = Hashtbl.find t.ids symbol

let to_alist t =
  Array.to_list
    (Array.mapi t.symbols ~f:(fun i symbol -> Symbol_id.of_int i, symbol))
;;

(* A mirror is only faithful if the ids are dense from 0 — that is what lets
   the client index by id the same way the server does. Sorting by id and
   checking the positions line up is cheaper than trusting the sender. *)
let of_alist pairs =
  let sorted =
    List.sort pairs ~compare:(fun (id, _) (id', _) ->
      Symbol_id.compare id id')
  in
  let ids_are_dense =
    List.for_alli sorted ~f:(fun i (id, _) -> Symbol_id.to_int id = i)
  in
  if not ids_are_dense
  then
    Or_error.error_s
      [%message
        "symbol directory ids are not dense from 0"
          ~ids:(List.map sorted ~f:(fun (id, _) -> id) : Symbol_id.t list)]
  else of_symbols (List.map sorted ~f:snd)
;;

(* Names each symbol after its own id, so rendering through a [numbered]
   directory prints exactly what printing the raw id would. That is the
   point: callers who speak in ids (integration tests, scenarios) get a
   directory that is required by the API but invisible in the output. *)
let numbered ~num_symbols =
  List.init num_symbols ~f:(fun i -> Symbol.of_string (Int.to_string i))
  |> of_symbols
  |> ok_exn
;;
