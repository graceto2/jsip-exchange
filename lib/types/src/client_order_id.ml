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
