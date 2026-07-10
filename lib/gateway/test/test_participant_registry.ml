(** Unit tests for {!Participant_registry}: interning participant names to
    server-local {!Participant_id.t}s and resolving ids back to names.

    The registry is additive and idempotent — a name keeps its id for the whole
    run and ids are handed out densely from 0 — so these tests pin both the id
    assignment and the name round-trip. *)

open! Core
open Jsip_types
open Jsip_gateway
open Jsip_test_harness

(* Ids are a [private int]; print them via [to_int] so the expected output is a
   plain number rather than a wrapped sexp. *)
let show_id id = print_s [%sexp (Participant_id.to_int id : int)]

let%expect_test "fresh names get dense ids starting at 0" =
  let t = Participant_registry.create () in
  show_id (Participant_registry.intern t Harness.alice);
  [%expect {| 0 |}];
  show_id (Participant_registry.intern t Harness.bob);
  [%expect {| 1 |}];
  show_id (Participant_registry.intern t Harness.charlie);
  [%expect {| 2 |}]
;;

let%expect_test "name_of_id resolves an id back to the name it was interned for" =
  let t = Participant_registry.create () in
  let alice_id = Participant_registry.intern t Harness.alice in
  let bob_id = Participant_registry.intern t Harness.bob in
  print_s [%sexp (Participant_registry.name_of_id t alice_id : Participant.t)];
  [%expect {| Alice |}];
  print_s [%sexp (Participant_registry.name_of_id t bob_id : Participant.t)];
  [%expect {| Bob |}]
;;

let%expect_test "interning is idempotent: a name keeps its id" =
  let t = Participant_registry.create () in
  let first = Participant_registry.intern t Harness.alice in
  (* Intern another name in between, so we're sure re-interning [alice] finds
     the existing id rather than just handing back the most recent one. *)
  let (_ : Participant_id.t) = Participant_registry.intern t Harness.bob in
  let again = Participant_registry.intern t Harness.alice in
  print_s [%sexp (Participant_id.equal first again : bool)];
  [%expect {| true |}]
;;
