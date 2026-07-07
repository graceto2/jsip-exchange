(** Mutable accumulator for the per-second metrics feed.

    Owned by {!Exchange_server}. The matching loop calls [record_*] as it
    handles each request; once a second the sampler calls {!snapshot} to turn
    everything gathered since the last call into one
    {!Stats.Stats_snapshot.t} to publish on {!Rpc_protocol.stats_rpc}.

    This is where the raw measurements live before they hit the wire: it
    holds the latency samples between snapshots and reads process memory on
    demand. *)

open! Core
open! Async

type t

val create : unit -> t

(** Record one submit-order latency — the span the matching loop measured
    from the [submit_order_rpc] handler accepting a request to the engine
    finishing it. Accumulated until the next {!snapshot}. *)
val record_submit_latency : t -> Time_ns.Span.t -> unit

(** Record one cancel-order latency; same shape as {!record_submit_latency}. *)
val record_cancel_latency : t -> Time_ns.Span.t -> unit

(** Assemble one snapshot for the interval just ended: read current heap
    usage, and drain the accumulated submit/cancel latency samples into the
    record. Clears the buffers, so each snapshot reports only the samples
    gathered since the previous call. *)
val snapshot : t -> Stats.Stats_snapshot.t
