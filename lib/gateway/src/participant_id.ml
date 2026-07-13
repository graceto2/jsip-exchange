open! Core

module T = struct
  (* Implementation is a plain [int]; the [.mli] narrows it to [private int]
     so that reading an id as an int stays cheap while constructing one has
     to go through [of_int], which is a deliberate, greppable act. *)
  type t = int [@@deriving sexp_of, compare, equal, hash]
end

include T

(* [Make_plain] rather than [Make]: we derive [sexp_of] but not [t_of_sexp],
   since an id is meaningful only relative to the registry that handed it out
   and shouldn't be reconstructible from a sexp. *)
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

let to_int t = t
let of_int t = t
