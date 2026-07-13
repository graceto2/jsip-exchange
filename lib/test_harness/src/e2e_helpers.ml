open! Core
open! Async
open Jsip_gateway
open Jsip_types

let with_server ~num_symbols f =
  (* Integration tests speak in ids from end to end, so the tickers are
     irrelevant — placeholders keep the count as the only thing a test has to
     state. *)
  let directory = Symbol_directory.numbered ~num_symbols in
  let%bind server = Exchange_server.start ~directory ~port:0 () in
  let port = Exchange_server.port server in
  Monitor.protect
    (fun () -> f ~server ~port)
    ~finally:(fun () -> Exchange_server.close server)
;;

type client = { conn : Rpc.Connection.t }

let connect_as ~port participant =
  let where =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%bind conn = Rpc.Connection.client where >>| Result.ok_exn in
  let%bind participant =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      conn
      (Participant.to_string participant)
    >>| Or_error.ok_exn
  in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  (* Fetch the instrument list the same way a real client does, rather than
     constructing one locally — so these tests exercise the directory RPC
     too, and render exactly what a user would see. *)
  let%bind directory =
    Rpc.Rpc.dispatch_exn Rpc_protocol.symbol_directory_rpc conn ()
    >>| Symbol_directory.of_alist
    >>| ok_exn
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       let e = Event_protocol.format_event ~directory event in
       print_endline [%string "[for %{participant#Participant}] %{e}"]));
  return { conn }
;;

let login_as ~port participant =
  let where =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%bind conn = Rpc.Connection.client where >>| Result.ok_exn in
  let%bind participant =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      conn
      (Participant.to_string participant)
    >>| Or_error.ok_exn
  in
  (* let%bind session_feed, _metadata = Rpc.Pipe_rpc.dispatch_exn
     Rpc_protocol.session_feed_rpc conn () in don't_wait_for
     (Pipe.iter_without_pushback session_feed ~f:(fun event -> (* match event
     with | Fill fill -> let fill = Fill.to_participant_view fill participant
     in (match fill with Some s -> print_endline s | None -> ()) | _ -> let e
     = Event_protocol.format_event event in print_endline
     [%string "[%{participant#Participant}] %{e}"])); *) let e =
     Event_protocol.format_event event in print_endline
     [%string "[for %{participant#Participant}] %{e}"])); *)
  ignore participant;
  return { conn }
;;

let connection client = client.conn

let rpc_submit client request =
  Rpc.Rpc.dispatch_exn
    Rpc_protocol.submit_order_rpc
    client.conn
    (Harness.to_request request)
  >>| ok_exn
;;

let rpc_book client symbol =
  Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc client.conn symbol
;;

let rpc_cancel client client_order_id =
  Rpc.Rpc.dispatch_exn
    Rpc_protocol.cancel_order_rpc
    client.conn
    client_order_id
  >>| ok_exn
;;
