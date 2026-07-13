(** Unit tests for {!Participant_registry}: interning participant names to
    server-local {!Participant_id.t}s, resolving ids back to names, and
    tracking who is connected right now.

    Identity is additive and idempotent — a name keeps its id for the whole
    run and ids are handed out densely from 0 — so these tests pin both the
    id assignment and the name round-trip. Presence is the opposite: it is
    pruned on log out, so the tests below check that logging out frees the
    name for another connection {i without} changing the id it comes back
    with. *)

open! Core
open Jsip_types
open Jsip_gateway
open Jsip_test_harness

(* Ids are a [private int]; print them via [to_int] so the expected output is
   a plain number rather than a wrapped sexp. *)
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

let%expect_test "name_of_id resolves an id back to the name it was interned \
                 for"
  =
  let t = Participant_registry.create () in
  let alice_id = Participant_registry.intern t Harness.alice in
  let bob_id = Participant_registry.intern t Harness.bob in
  print_s
    [%sexp (Participant_registry.name_of_id_exn t alice_id : Participant.t)];
  [%expect {| Alice |}];
  print_s
    [%sexp (Participant_registry.name_of_id_exn t bob_id : Participant.t)];
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

let show_log_in t participant =
  print_s
    [%sexp
      (Participant_registry.log_in t participant
       |> Or_error.map ~f:Participant_id.to_int
       : int Or_error.t)]
;;

let%expect_test "a name may be logged in on only one connection at a time" =
  let t = Participant_registry.create () in
  show_log_in t Harness.alice;
  [%expect {| (Ok 0) |}];
  (* Alice is still live, so a second login under the same name is refused —
     this is what stops two clients from trading as the same participant. *)
  show_log_in t Harness.alice;
  [%expect
    {| (Error ("participant name already in use" (participant Alice))) |}];
  (* A *different* name is unaffected, and gets the next dense id. *)
  show_log_in t Harness.bob;
  [%expect {| (Ok 1) |}]
;;

let%expect_test "logging out frees the name but not the identity" =
  let t = Participant_registry.create () in
  show_log_in t Harness.alice;
  [%expect {| (Ok 0) |}];
  let alice_id = Participant_registry.intern t Harness.alice in
  Participant_registry.log_out t alice_id;
  print_s [%sexp (Participant_registry.is_logged_in t alice_id : bool)];
  [%expect {| false |}];
  (* Reconnecting works, and Alice is still id 0: log out pruned presence,
     not identity. No fresh id was minted. *)
  show_log_in t Harness.alice;
  [%expect {| (Ok 0) |}];
  print_s
    [%sexp (Participant_registry.name_of_id_exn t alice_id : Participant.t)];
  [%expect {| Alice |}]
;;
