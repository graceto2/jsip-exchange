open! Core

type t =
  { symbol : Symbol_id.t
  ; price : Price.t
  ; size : Size.t
  }
[@@deriving sexp, bin_io]
