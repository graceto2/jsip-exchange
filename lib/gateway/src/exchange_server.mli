(** Exchange server for production use and testing.

    Bundles the matching engine, market data bus, and RPC implementations
    into a single server that can be started on any port. Used by the server
    binary, the market maker binary, and integration tests. *)

open! Core
open! Async

type t

(** Start a server on the given port trading the instruments in [directory]
    (symbol ids [0 .. n-1], where [n] is {!Symbol_directory.num_symbols}).
    Returns the server handle and the port it is actually listening on
    (useful when you pass port 0 to get an OS-assigned port).

    The directory is authoritative for how many instruments exist: it sizes
    the matching engine, so an id is valid exactly when the directory knows
    it. The server itself never resolves an id to a name — it only serves the
    directory to clients over {!Rpc_protocol.symbol_directory_rpc}, which do
    the resolving. *)
val start : directory:Symbol_directory.t -> port:int -> unit -> t Deferred.t

(** The port the server is listening on. *)
val port : t -> int

(** Stop the server and close all connections. *)
val close : t -> unit Deferred.t

(** Wait until the server's TCP listener is closed. *)
val close_finished : t -> unit Deferred.t
