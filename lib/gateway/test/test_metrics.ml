(* (** Unit tests for {!Metrics_collector}, the per-second latency/memory
   accumulator owned by {!Exchange_server}.

   Skeletons only — the setup is here; fill in the record/assert logic.

   Note: {!Stats.Stats_snapshot.t} also carries [sampled_at] (wall clock) and
   [live_words] (heap size). Both change run-to-run, so never print them raw
   in a [%expect] block — pull out the deterministic [submit_latencies] /
   [cancel_latencies] fields instead. Build spans with
   [Time_ns.Span.of_ms 5.] and friends. *)

   open! Core open Jsip_gateway

   let%expect_test "fresh collector: snapshot reports no latencies" = let
   collector = Metrics_collector.create () in let snapshot =
   Metrics_collector.snapshot collector in print_s
   [%sexp (snapshot.submit_latencies : Time_ns.Span.t list)]; [%expect {| |}]
   ;;

   let%expect_test "snapshot drains the recorded submit/cancel latencies" =
   let collector = Metrics_collector.create () in (*
   Metrics_collector.record_cancel_latency *) (* TODO(you): record a couple
   of submit and cancel latencies with
   [Metrics_collector.record_submit_latency] / [record_cancel_latency], take
   a [snapshot], and assert the lists contain exactly what you fed in. Mind
   the order — the collector backs each feed with a FIFO queue. *) ignore
   (collector : Metrics_collector.t); [%expect {| |}] ;;

   let%expect_test "snapshot clears the buffers: the next snapshot is empty"
   = let collector = Metrics_collector.create () in (* TODO(you): record
   something, take one snapshot (which drains it), then take a second
   snapshot and assert its latency lists are empty. This is the "each sample
   is reported in exactly one snapshot" invariant the module comment
   promises. *) ignore (collector : Metrics_collector.t); [%expect {| |}] ;; *)
