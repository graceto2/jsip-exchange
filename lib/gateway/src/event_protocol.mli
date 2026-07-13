(** Human-readable rendering of exchange events, books and fills.

    This is the exchange's display layer: the client and the monitor both
    print through it. It lives here, and not in {!Jsip_types}, because
    rendering a symbol needs a name — and a name is not a function of the
    event. [Jsip_types] carries {!Jsip_types.Symbol_id.t}s and can only ever
    print integers; the id->name mapping lives in a {!Symbol_directory},
    which a consumer fetches from the server at connect. So every function
    here takes [~directory].

    The {!Jsip_types} [to_string] functions still exist and still print ids.
    They are for tests and debugging, where the id {i is} the interesting
    thing. Anything a person reads should come from here.

    An id the directory does not know renders as the raw id rather than
    raising: a directory is a mirror fetched once, and a stale mirror should
    not take down a client's render loop. *)

open! Core
open Jsip_types

(** The name [directory] knows this id by, or the raw id if it knows none.
    Every function below resolves symbols through this; it is exposed for
    callers rendering a symbol outside an event (a prompt, a panel header). *)
val symbol_to_string : directory:Symbol_directory.t -> Symbol_id.t -> string

(** Format an exchange event as a single line of human-readable text, with
    symbols rendered as names ([BBO AAPL bid=...], not [BBO 0 bid=...]). *)
val format_event : directory:Symbol_directory.t -> Exchange_event.t -> string

(** Format a list of events, one per line. *)
val format_events
  :  directory:Symbol_directory.t
  -> Exchange_event.t list
  -> string

(** Format a book snapshot as a multi-line block, headed by the symbol's
    name. The name-rendering counterpart of {!Jsip_types.Book.to_string}. *)
val format_book : directory:Symbol_directory.t -> Book.t -> string

(** What [participant] should be told about a fill, phrased from their own
    side of it ([You bought size=100 symbol=AAPL at $150.00]). [None] if the
    fill is not theirs, since a feed may carry other participants' trades. *)
val format_fill_for_participant
  :  directory:Symbol_directory.t
  -> Fill.t
  -> Participant.t
  -> string option
