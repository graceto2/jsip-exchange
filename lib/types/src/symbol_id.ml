open! Core

(* Backed by a plain [int]; the [.mli] narrows it to [private int] so callers
   can read one as an int (or via [to_int]) but only [of_int] tags one. There
   is intentionally no range checking here — this layer is pure data and has
   no idea how many symbols exist. *)
module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_int t = t
let of_int t = t
