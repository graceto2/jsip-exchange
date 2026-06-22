open! Core

module T = struct
  type t = string [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

(* Automatically uppercases lowercase characters in the input string, since
   they represent the same thing and to maintain consistency. *)
let of_string s =
  if String.is_empty s
  then raise_s [%message "Symbol.of_string: symbol must be non-empty"]
  else if not (String.for_all s ~f:Char.is_alphanum)
  then
    raise_s
      [%message
        "Symbol.of_string: symbol must consist of only alphanum characters"]
  else String.uppercase s
;;
