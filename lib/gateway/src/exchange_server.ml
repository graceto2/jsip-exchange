open! Core
open! Async
open Jsip_types
open Jsip_order_book

module Connection_state = struct
  type t = { mutable session : Session.t option }

  let participant t = Option.map t.session ~f:Session.participant
end

module Matching_engine_action = struct
  type t =
    | New_order of
        { participant : Participant.t
        ; request : Order.Request.t
        }
    | Cancel_order of
        { participant : Participant.t
        ; client_order_id : Client_order_id.t
        }
end

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : (Time_ns.t * Matching_engine_action.t) Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  ; logged_in_participants : Hash_set.M(Participant_id).t
  ; stop : unit Ivar.t
  ; registry : Participant_registry.t
  (* Filled by [close] to stop the per-second stats sampler. *)
  }

let require_login (conn_state : Connection_state.t) =
  match conn_state.session with
  | Some session -> Ok session
  | None -> Or_error.error_string "not logged in"
;;

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

(* How often the exchange publishes a metrics snapshot on [stats_rpc]. The
   memory pane wants a sample "at least once per second"; the latency panes
   report the samples gathered within each interval, so this doubles as the
   window over which each snapshot's latencies accumulate. *)
let stats_sample_interval = Time_ns.Span.of_sec 1.

(* The request pipe carries each action paired with the [Time_ns.t] at which
   its RPC handler accepted it. The timestamp travels *with* the action
   through the pipe — rather than in a side channel — so it can never be
   misattributed to a different request. The matching loop reads it back to
   measure how long the request waited in the queue plus how long the engine
   took to handle it: the submit/cancel latency the dashboard reports. *)
let handle_submit
  ~(request_writer : (Time_ns.t * Matching_engine_action.t) Pipe.Writer.t)
  ~participant
  request
  =
  let enqueued_at = Time_ns.now () in
  let%map () =
    Pipe.write_if_open
      request_writer
      (enqueued_at, New_order { participant; request })
  in
  Ok ()
;;

let handle_cancel
  ~(request_writer : (Time_ns.t * Matching_engine_action.t) Pipe.Writer.t)
  ~participant
  client_order_id
  =
  let enqueued_at = Time_ns.now () in
  let%map () =
    Pipe.write_if_open
      request_writer
      (enqueued_at, Cancel_order { participant; client_order_id })
  in
  Ok ()
;;

let start_matching_loop
  ~engine
  ~dispatcher
  ~collector
  (request_reader : (Time_ns.t * Matching_engine_action.t) Pipe.Reader.t)
  =
  don't_wait_for
    (Pipe.iter_without_pushback
       request_reader
       ~f:(fun (enqueued_at, request) ->
         let latency = Time_ns.diff (Time_ns.now ()) enqueued_at in
         let events =
           match request with
           | Cancel_order { participant; client_order_id } ->
             Metrics_collector.record_cancel_latency collector latency;
             Matching_engine.cancel engine ~participant ~client_order_id
           | New_order { participant; request } ->
             Metrics_collector.record_submit_latency collector latency;
             Matching_engine.submit engine ~participant request
         in
         Dispatcher.dispatch dispatcher events))
;;

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let collector = Metrics_collector.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher ~collector request_reader;
  let stop = Ivar.create () in
  (* Publish one metrics snapshot per second until the server is closed. This
     lives in the gateway, not the server binary, so any client that
     subscribes to [stats_rpc] gets data regardless of which server mode is
     running. *)
  Clock_ns.every ~stop:(Ivar.read stop) stats_sample_interval (fun () ->
    Dispatcher.push_stats dispatcher (Metrics_collector.snapshot collector));
  let registry = Participant_registry.create () in
  (* Who is connected *right now*, keyed by their permanent
     [Participant_id.t]. The id is already in hand from [intern] at login, and
     int hashing beats string hashing. Deliberately separate from [registry]:
     this set is pruned on disconnect (presence), whereas the registry never
     forgets a name (identity). *)
  let logged_in_participants = Hash_set.create (module Participant_id) in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement Rpc_protocol.login_rpc (fun conn_state name ->
            if String.is_empty (String.strip name)
            then
              Deferred.Or_error.error_string "login name must not be empty"
            else (
              let participant = Participant.of_string name in
              match Connection_state.participant conn_state with
              | Some existing ->
                Deferred.Or_error.error_s
                  [%message "already logged in" (existing : Participant.t)]
              | None ->
                (* Resolve the name to its permanent id: a new name mints one,
                   a reconnecting name gets the id it had before. *)
                let id =
                  Participant_registry.intern registry participant
                in
                if Hash_set.mem logged_in_participants id
                then
                  (* The id is present, so the name is already live on another
                     connection — reject this second login. *)
                  Deferred.Or_error.error_s
                    [%message
                      "participant name already in use"
                        (participant : Participant.t)]
                else (
                  let session =
                    Dispatcher.set_up_session dispatcher participant
                  in
                  conn_state.session <- Some session;
                  Hash_set.add logged_in_participants id;
                  Deferred.Or_error.return participant)))
        ; Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun conn_state request ->
               match require_login conn_state with
               | Error _ as err -> return err
               | Ok session ->
                 handle_submit
                   ~request_writer
                   ~participant:(Session.participant session)
                   request)
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun conn_state client_order_id ->
               match require_login conn_state with
               | Error _ as err -> return err
               | Ok session ->
                 handle_cancel
                   ~request_writer
                   ~participant:(Session.participant session)
                   client_order_id)
        ; Rpc.Rpc.implement'
            Rpc_protocol.book_query_rpc
            (fun _conn_state symbol ->
               Matching_engine.book engine symbol
               |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun conn_state symbols ->
               ignore conn_state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.audit_log_rpc
            (fun conn_state () ->
               ignore conn_state;
               let reader = Dispatcher.subscribe_audit dispatcher in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun conn_state () ->
               match require_login conn_state with
               | Error _ as err -> return err
               | Ok session ->
                 Deferred.Or_error.return (Session.reader session))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.stats_rpc (fun conn_state () ->
            ignore conn_state;
            let reader = Dispatcher.subscribe_stats dispatcher in
            return (Ok reader))
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr conn ->
        let (state : Connection_state.t) = { session = None } in
        don't_wait_for
          (let%map () = Rpc.Connection.close_finished conn in
           match state.session with
           | None -> ()
           | Some session ->
             Dispatcher.clean_up_session dispatcher session;
             (* The participant is already registered, so [intern] just looks
                up the id it minted at login — it never mints here. *)
             Hash_set.remove
               logged_in_participants
               (Participant_registry.intern
                  registry
                  (Session.participant session)));
        state)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  { engine
  ; dispatcher
  ; request_writer
  ; tcp_server
  ; port = actual_port
  ; logged_in_participants
  ; stop
  ; registry
  }
;;

let port t = t.port

let close t =
  Ivar.fill_if_empty t.stop ();
  (* Fills the variable stop with something, so that the metric snapshots
     stop getting recorded every second. *)
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server
