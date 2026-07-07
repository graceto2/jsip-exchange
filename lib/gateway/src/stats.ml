open! Core
open! Async

module Stats_snapshot = struct
  type t =
    { sampled_at : Time_ns.t
    ; live_words : int
    ; submit_latencies : Time_ns.Span.t list
    ; cancel_latencies : Time_ns.Span.t list
    }
  [@@deriving sexp, bin_io]
end
