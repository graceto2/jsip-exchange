open! Core
open! Async

(* Two FIFO buffers of latency samples, one per instrumented RPC. The
   matching loop enqueues into them as it works; [snapshot] drains them once
   a second. A [Queue.t] is the right shape here: appends are O(1), and
   draining with [Queue.to_list] + [Queue.clear] naturally gives "everything
   since the last snapshot" and resets the window. *)
type t =
  { submit_latencies : Time_ns.Span.t Queue.t
  ; cancel_latencies : Time_ns.Span.t Queue.t
  }

let create () =
  { submit_latencies = Queue.create (); cancel_latencies = Queue.create () }
;;

let record_submit_latency t latency =
  Queue.enqueue t.submit_latencies latency
;;

let record_cancel_latency t latency =
  Queue.enqueue t.cancel_latencies latency
;;

let snapshot (t : t) : Stats.Stats_snapshot.t =
  (* Read the samples, then clear the buffers, so each sample is reported in
     exactly one snapshot and the next interval starts empty. [Gc.stat] walks
     the heap to report [live_words] (total words reachable right now), which
     is why the sampler calls this only once a second. *)
  let sampled_at = Time_ns.now () in
  let live_words = (Gc.stat ()).live_words in
  let submit_latencies = Queue.to_list t.submit_latencies in
  let cancel_latencies = Queue.to_list t.cancel_latencies in
  Queue.clear t.submit_latencies;
  Queue.clear t.cancel_latencies;
  { sampled_at; live_words; submit_latencies; cancel_latencies }
;;
