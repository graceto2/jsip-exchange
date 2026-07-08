open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

(* A single process-wide counter behind [next], so every generated id is
   unique within a run. Tests reset it (via [Harness.create]) so expect
   output stays deterministic regardless of which tests ran first. *)
let counter = ref 0

let next () : t =
  let id = !counter in
  incr counter;
  id
;;

module For_testing = struct
  let reset () = counter := 0
end

let of_int t = t
let to_int t = t

module Generator = struct
  type t = { mutable next_id : int }

  let create () = { next_id = 0 }

  let generate t =
    let id = of_int t.next_id in
    t.next_id <- t.next_id + 1;
    id
  ;;
end
