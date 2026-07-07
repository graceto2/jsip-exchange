(** Infrastructure metrics streamed by {!Rpc_protocol.stats_rpc}, one
    snapshot per second, for the monitoring dashboard.

    This lives in the gateway layer, deliberately *not* alongside
    {!Jsip_types.Exchange_event} in the domain layer: an [Exchange_event.t]
    records something that happened on the exchange (an acceptance, a fill, a
    cancel), whereas a [Stats_snapshot.t] describes the health of the process
    serving the exchange (heap size, per-RPC latency). Keeping the two apart
    is the point — see the audit-log firehose in {!Dispatcher}, which carries
    [Exchange_event.t] and nothing else. *)

open! Core
open! Async

module Stats_snapshot : sig
  (** One per-second sample of everything the dashboard's required panes
      need. The record is deliberately interpretation-free: the exchange
      ships raw measurements and the dashboard does all aggregation (rolling
      windows, percentiles, histograms). *)
  type t =
    { sampled_at : Time_ns.t
    (** When the exchange took this sample. The dashboard uses it as the
        x-axis for the memory pane's rolling window, so the timeline is
        driven by the server clock rather than by arrival jitter. *)
    ; live_words : int
    (** [Gc.stat ().live_words]: total words reachable on the OCaml heap
        right now — the memory pane's headline number. Not sure what this is. *)
    ; submit_latencies : Time_ns.Span.t list
    (** Every submit-order latency measured since the previous snapshot (i.e.
        over the last ~1s): the span from the [submit_order_rpc] handler
        receiving a request to the matching engine finishing it. Empty when
        no orders were submitted in the interval. *)
    ; cancel_latencies : Time_ns.Span.t list
    (** Same, for [cancel_order_rpc]. *)
    }
  [@@deriving sexp, bin_io]
end
