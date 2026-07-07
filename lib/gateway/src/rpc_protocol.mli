(** RPC definitions for client-server communication.

    Defines the RPCs that clients use to interact with the exchange server.
    Each RPC has a query type (what the client sends) and a response type
    (what the server returns).

    We use Async RPCs, but on a production exchange, clients would connect
    over a binary protocol like FIX or a proprietary format. *)

open! Core
open! Async
open Jsip_types

(** Submit an order to the exchange.

    This is a one-way RPC. The server enqueues the order and returns as soon
    as possible. The matching engine processes the queued request on a
    background worker and hands the resulting [Exchange_event.t]s to the
    [Dispatcher], which routes participant-targeted events (acceptance,
    fills, rejection) to the owning participant's [Session]. The per-session
    RPC that lets a client read its session feed does not exist yet (planned
    for week 2); until it does, those events are printed on the server's
    stdout.

    The error case covers connection-level failures only — connection closed,
    server shutting down, etc. — not domain errors like unknown symbols
    (those arrive as [Order_reject] events on the session feed). *)
val submit_order_rpc : (Order.Request.t, unit Or_error.t) Rpc.Rpc.t

(** Cancel a resting order the client previously submitted. The wire carries
    only the [Client_order_id.t]; the server reattaches the authenticated
    participant from the session, so a client cannot cancel another
    participant's order. Mirrors {!submit_order_rpc}'s trust boundary. *)
val cancel_order_rpc : (Client_order_id.t, unit Or_error.t) Rpc.Rpc.t

(** Query the order book for a given symbol. Returns a structured snapshot of
    all resting orders on both sides, if a book for that symbol exists. *)
val book_query_rpc : (Symbol.t, Book.t option) Rpc.Rpc.t

(** Subscribe to market data for one or more symbols. The server pushes BBO
    updates and trade reports as they happen via a single pipe. The query is
    the list of symbols to subscribe to; using one RPC for the whole list
    avoids the overhead of opening a separate pipe per symbol when a client
    cares about several. *)
val market_data_rpc
  : (Symbol.t list, Exchange_event.t, Error.t) Rpc.Pipe_rpc.t

(** Subscribe to the full audit log: every [Exchange_event.t] the matching
    engine produces, across every symbol and participant.

    This RPC is intended for the exchange operator's monitoring and audit
    tools (e.g. the bonsai_term monitor in [app/monitor]) only. Ordinary
    participants — automated bots, human-driven clients — should use
    [market_data_rpc] for public events, and (once it exists, week 2) a
    per-participant session-feed RPC for their own order-lifecycle events. A
    production exchange would gate this RPC behind operator-level
    credentials; this simulator does not, but the same intent applies. *)
val audit_log_rpc : (unit, Exchange_event.t, Error.t) Rpc.Pipe_rpc.t

val login_rpc : (String.t, Participant.t Or_error.t) Rpc.Rpc.t

(** Delivers order accept/cancel/reject and fill events related to the
    participant. *)
val session_feed_rpc : (unit, Exchange_event.t, Error.t) Rpc.Pipe_rpc.t

(** Subscribe to the exchange's infrastructure-metrics feed: one
    {!Stats.Stats_snapshot.t} per second carrying process memory and the
    submit/cancel-order latencies measured over that second.

    This is a *separate* RPC from {!audit_log_rpc} by design. The audit log
    streams [Exchange_event.t] — things that happened on the exchange. These
    are metrics about the process serving the exchange. Folding metrics into
    [Exchange_event.t] would conflate two unrelated concerns and change the
    audit log's wire contract, so they get their own RPC and their own pipe.

    Like {!audit_log_rpc}, this is intended for the operator's monitoring and
    dashboard tools, not for ordinary participants. *)
val stats_rpc : (unit, Stats.Stats_snapshot.t, Error.t) Rpc.Pipe_rpc.t
