open! Core

type t =
  { symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  }
[@@deriving sexp, bin_io]
