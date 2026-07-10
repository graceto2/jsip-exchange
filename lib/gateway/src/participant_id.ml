open! Core

(* Implementation is a plain [int]; the [.mli] narrows it to [private int] so
   that only this module (and, later, the login registry built on it) can mint
   ids, while the rest of the server may still read one as an int. *)
type t = int [@@deriving sexp_of, compare, equal, hash]

(* Reading an id as a plain [int] — e.g. to index the registry's id->name
   [Queue]. The [private int] representation already permits this via a
   coercion; [to_int] just gives it a name callers can read. *)
let to_int t = t

module Generator = struct
  type t = { mutable next : int } [@@deriving sexp_of]

  (* Starts at 0 so an id doubles as its index in the registry's id->name
     [Queue] — the k-th interned participant gets id [k] and sits at slot [k].
     (Unlike [Order_id.Generator], which starts at 1.) *)
  let create () = { next = 0 }

  let next t =
    let id = t.next in
    t.next <- t.next + 1;
    id
  ;;
end
