(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** {1 Flow-related signatures}

    This module defines the flow signature for MirageOS.

    {e Release %%VERSION%% } *)

type write_error = [ `Closed ]
(** The type for generic write errors on flows. *)

val pp_write_error: write_error Fmt.t
(** [pp_write_error] is the pretty-printer for write errors. *)

type 'a or_eof = [`Data of 'a | `Eof ]
(** The type for read results on flows. *)

val pp_or_eof: 'a Fmt.t -> 'a or_eof Fmt.t
(** [pp_or_eof] is the pretty-printer for {!or_eof} values. *)

(** Abstract flow signature. *)
module type S = sig

  type error
  (** The type for flow errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type nonrec write_error = private [> write_error ]
  (** The type for write errors. *)

  val pp_write_error: write_error Fmt.t
  (** [pp_write_error] is the pretty-printer for write errors. *)

  type flow
  (** The type for flows. A flow represents the state of a single reliable
      stream that is connected to an endpoint. *)

  val read: flow -> (Cstruct.t or_eof, error) result Lwt.t
  (** [read flow] blocks until some data is available and returns a fresh buffer
      containing it.

      The returned buffer will be of a size convenient to the flow
      implementation, but will always have at least 1 byte.

      When [read] returns [`Eof] or an error, [close] (or [shutdown]) should be
      called on the [flow] by the client. Once [read] returned [`Eof] or an
      error, no subsequent [read] call will be successful. *)

  val write: flow -> Cstruct.t -> (unit, write_error) result Lwt.t
  (** [write flow buffer] writes a buffer to the flow. There is no indication
      when the buffer has actually been sent and, therefore, it must not be
      reused. The contents may be transmitted in separate packets, depending on
      the underlying transport. The result [Ok ()] indicates success,
      [Error `Closed] indicates that the connection is now closed and therefore
      the data could not be written. Other errors are possible.

      The call to [write] blocks until the buffer has been accepted by the
      implementation (if a partial write occured, write will wait until the
      remainder of the buffer has been accepted by the implementation).

      If [write] returns an error, [close] (or [shutdown]) should be called on
      the [flow] by the client. Once [write] returned an error, no subsequent
      [write] or [writev] call will be successful. *)

  val writev: flow -> Cstruct.t list -> (unit, write_error) result Lwt.t
  (** [writev flow buffers] writes a sequence of buffers to the flow. There is
      no indication when the buffers have actually been read and, therefore,
      they must not be reused. The result [Ok ()] indicates success,
      [Error `Closed] indicates that the connection is now closed and therefore
      the data could not be written. Other errors are possible.

      If [writev] returns an error, [close] (or [shutdown]) should be called on
      the [flow] by the client. Once [writev] returned an error, no subsequent
      [writev] or [write] call will be successful. *)

  val shutdown : flow -> [ `read | `write | `read_write ] -> unit Lwt.t
  (** [shutdown flow mode] shuts down the [flow] for the specific [mode]:
      A flow which is [shutdown `read] (or [`read_write]) will never be [read]
      again (subsequent calls will return [`Eof]); a flow which is
      [shutdown `write] (or [`read_write]) flushes all pending writes and
      signals the remote endpoint there won't be any future [write] or [writev]
      calls (subsequent calls will return [`Closed]). E.g. in TCP, the
      signalling is done by sending a segment with the FIN flag.

      If this [flow] is layered upon another [flow'] (e.g. TLS over TCP),
      [shutdown] on the underlying [flow'] is executed. *)

  val close: flow -> unit Lwt.t
  (** [close flow] terminates the [flow] and frees all associated data. Any
      subsequent [read] or [write] will return an error. A subsequent [close]
      will not do anything (esp. not raising an exception), but it may log an
      error.

      If this [flow] is layered upon another [flow'] (e.g. TLS over TCP),
      [close] on the underlying [flow'] is executed. *)
end
