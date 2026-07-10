(** Shared helpers for end-to-end tests that use a real server and RPC
    clients. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway

(** Start a server on an OS-assigned port, run [f], then shut down. *)
val with_server
  :  num_symbols:int
  -> (server:Exchange_server.t -> port:int -> 'a Deferred.t)
  -> 'a Deferred.t

(** A test client: an open RPC connection to the server. A future revision
    (once the session-feed RPC and login flow exist) will extend this with a
    buffered session feed so [rpc_submit] can return the events produced by
    the just-submitted request. *)
type client

(** Connect a client to [port]. Logs in with [participant] argument. *)
val connect_as : port:int -> Participant.t -> client Deferred.t

(** Same as [connect_as], but doesn't subscribe to the participants feed. *)
val login_as : port:int -> Participant.t -> client Deferred.t

(** The raw RPC connection, useful for tests that exercise unusual RPC paths
    (audit log subscriptions, second clients on the same connection, etc.). *)
val connection : client -> Rpc.Connection.t

(** Submit an order via RPC. The RPC is one-way: this returns once the server
    has enqueued the request. Participant-targeted events (acceptance, fills,
    rejection) are currently printed on the server's stdout via the
    dispatcher's session stub. *)
val rpc_submit : client -> Harness.Order_request.t -> unit Deferred.t

(** Query the book via RPC. *)
val rpc_book : client -> Symbol_id.t -> Book.t option Deferred.t

val rpc_cancel : client -> Client_order_id.t -> unit Deferred.t
