open! Core

type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** [next ()] returns a fresh client order id, unique within this process.
    Ids are handed out sequentially from 0 by a single shared counter, so a
    caller that just needs a non-colliding id — a test order builder, an
    automated submitter like the market maker — can call [next ()] instead of
    inventing one and risking a duplicate.

    A sequential counter (rather than a random value) is deliberate: it keeps
    expect-test output stable and readable. See {!For_testing.reset} for how
    tests stay deterministic despite the shared state. *)
val next : unit -> t

module For_testing : sig
  (** Reset the shared counter so the next {!next} call starts from 0 again.
      {!Jsip_test_harness.Harness.create} calls this, giving each expect test
      a fresh id sequence that doesn't depend on how many ids the tests
      before it happened to draw. *)
  val reset : unit -> unit
end
