(*
 * Copyright (c) 2011-present Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-present Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (C) 2016-present David Scott <dave.scott@docker.com>
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

open Lwt.Infix

let src = Logs.Src.create "mirage-flow-combinators"
module Log = (val Logs.src_log src : Logs.LOG)

type stats = {
  read_bytes: int64;
  read_ops: int64;
  write_bytes: int64;
  write_ops: int64;
  duration: int64;
}

let kib = 1024L
let ( ** ) = Int64.mul
let mib = kib ** 1024L
let gib = mib ** 1024L
let tib = gib ** 1024L

let suffix = [
  kib, "KiB";
  mib, "MiB";
  gib, "GiB";
  tib, "TiB";
]

let add_suffix x =
  List.fold_left (fun acc (y, label) ->
      if Int64.div x y > 0L
      then Printf.sprintf "%.1f %s" Int64.((to_float x) /. (to_float y)) label
      else acc
    ) (Printf.sprintf "%Ld bytes" x) suffix

let pp_stats ppf s =
  Fmt.pf ppf "%s bytes at %s/nanosec and %Lu IOPS/nanosec"
    (add_suffix s.read_bytes)
    (add_suffix Int64.(div s.read_bytes s.duration))
    (Int64.div s.read_ops s.duration)

module type CONCRETE =  Mirage_flow.S
  with type error = [ `Msg of string ]
   and type write_error = [ Mirage_flow.write_error | `Msg of string ]

module Concrete (S: Mirage_flow.S) = struct
  type error = [`Msg of string]
  type write_error = [ Mirage_flow.write_error | `Msg of string]
  type flow = S.flow

  let pp_error ppf = function
    | `Msg s -> Fmt.string ppf s

  let pp_write_error ppf = function
    | #error as e -> pp_error ppf e
    | `Closed     -> Mirage_flow.pp_write_error ppf `Closed

  let lift_read = function
    | Ok x    -> Ok x
    | Error e -> Error (`Msg (Fmt.str "%a" S.pp_error e))

  let lift_write = function
    | Ok ()         -> Ok ()
    | Error `Closed -> Error `Closed
    | Error e       -> Error (`Msg (Fmt.str "%a" S.pp_write_error e))

  let read t = S.read t >|= lift_read
  let write t b = S.write t b >|= lift_write
  let writev t bs = S.writev t bs >|= lift_write
  let shutdown t m = S.shutdown t m
  let close t = S.close t
end

type time = int64

type 'a stats_lwt = {
  read_bytes: int64 ref;
  read_ops: int64 ref;
  write_bytes: int64 ref;
  write_ops: int64 ref;
  finish: time option ref;
  start: time;
  time: unit -> time;
  t: (unit, 'a) result Lwt.t;
}

let stats_lwt t =
  let duration : int64 = match !(t.finish) with
    | None -> Int64.sub (t.time ()) t.start
    | Some x -> Int64.sub x t.start
  in {
    read_bytes  = !(t.read_bytes);
    read_ops    = !(t.read_ops);
    write_bytes = !(t.write_bytes);
    write_ops   = !(t.write_ops);
    duration;
  }

module Copy (A: Mirage_flow.S) (B: Mirage_flow.S) =
struct

  type error = [`A of A.error | `B of B.write_error]

  let pp_error ppf = function
    | `A e -> A.pp_error ppf e
    | `B e -> B.pp_write_error ppf e

  let start (a: A.flow) (b: B.flow) =
    let read_bytes = ref 0L in
    let read_ops = ref 0L in
    let write_bytes = ref 0L in
    let write_ops = ref 0L in
    let finish = ref None in
    let start = Mirage_mtime.elapsed_ns () in
    let rec loop () =
      A.read a >>= function
      | Error e ->
        finish := Some (Mirage_mtime.elapsed_ns ());
        Lwt.return (Error (`A e))
      | Ok `Eof ->
        finish := Some (Mirage_mtime.elapsed_ns ());
        Lwt.return (Ok ())
      | Ok (`Data buffer) ->
        read_ops := Int64.succ !read_ops;
        read_bytes := Int64.(add !read_bytes (of_int @@ Cstruct.length buffer));
        B.write b buffer
        >>= function
        | Ok () ->
          write_ops := Int64.succ !write_ops;
          write_bytes := Int64.(add !write_bytes (of_int @@ Cstruct.length buffer));
          loop ()
        | Error e ->
          finish := Some (Mirage_mtime.elapsed_ns ());
          Lwt.return (Error (`B e))
    in
    {
      read_bytes;
      read_ops;
      write_bytes;
      write_ops;
      finish;
      start;
      time = (fun () -> Mirage_mtime.elapsed_ns ());
      t = loop ();
    }

  let wait t = t.t

  let copy ~src:a ~dst:b =
    let t = start a b in
    wait t >|= function
    | Ok ()   -> Ok (stats_lwt t)
    | Error e -> Error e

end

module Proxy (A: Mirage_flow.S) (B: Mirage_flow.S) =
struct

  module A_to_B = Copy(A)(B)
  module B_to_A = Copy(B)(A)

  type error = [
    | `A of A_to_B.error
    | `B of B_to_A.error
    | `A_and_B of A_to_B.error * B_to_A.error
  ]

  let pp_error ppf = function
    | `A_and_B (e1, e2) ->
      Fmt.pf ppf "flow proxy a: %a; flow proxy b: %a"
        A_to_B.pp_error e1 B_to_A.pp_error e2
    | `A e -> Fmt.pf ppf "flow proxy a: %a" A_to_B.pp_error e
    | `B e -> Fmt.pf ppf "flow proxy b: %a" B_to_A.pp_error e

  let proxy a b =
    let a2b =
      let t = A_to_B.start a b in
      A_to_B.wait t >>= fun result ->
      A.shutdown a `read >>= fun () ->
      B.shutdown b `write >|= fun () ->
      let stats = stats_lwt t in
      match result with
      | Ok ()   -> Ok stats
      | Error e -> Error e
    in
    let b2a =
      let t = B_to_A.start b a in
      B_to_A.wait t >>= fun result ->
      B.shutdown b `read >>= fun () ->
      A.shutdown a `write >|= fun () ->
      let stats = stats_lwt t in
      match result with
      | Ok ()   -> Ok stats
      | Error e -> Error e
    in
    a2b >>= fun a_stats ->
    b2a >|= fun b_stats ->
    match a_stats, b_stats with
    | Ok a_stats, Ok b_stats -> Ok (a_stats, b_stats)
    | Error e1  , Error e2   -> Error (`A_and_B (e1, e2))
    | Error e1  ,  _         -> Error (`A e1)
    | _         , Error e2   -> Error (`B e2)

end

module F = struct

  let (>>=) = Lwt.bind

  type refill = Cstruct.t -> int -> int -> int Lwt.t

  type error
  let pp_error ppf (_:error) =
    Fmt.string ppf "Mirage_flow_combinators.F.error"
  type write_error = Mirage_flow.write_error
  let pp_write_error = Mirage_flow.pp_write_error

  let seq f1 f2 buf off len =
    f1 buf off len >>= function
    | 0 -> f2 buf off len
    | n -> Lwt.return n

  let zero _buf _off _len = Lwt.return 0

  let rec iter fn = function
    | []   -> zero
    | h::t -> seq (fn h) (iter fn t)

  type flow = {
    close: unit -> unit Lwt.t;
    input: refill;
    output: refill;
    mutable buf: Cstruct.t;
    mutable ic_closed: bool;
    mutable oc_closed: bool;
  }

  let default_buffer_size = 4096

  let make ?(close=fun () -> Lwt.return_unit) ?input ?output () =
    let buf = Cstruct.create default_buffer_size in
    let ic_closed = input = None in
    let oc_closed = output = None in
    let input = match input with None -> zero | Some x -> x in
    let output = match output with None -> zero | Some x -> x in
    { close; input; output; buf; ic_closed; oc_closed; }

  let input_fn len blit str =
    let str_off = ref 0 in
    let str_len = len str in
    fun buf off len ->
      if !str_off >= str_len then Lwt.return 0
      else (
        let len = min (str_len - !str_off) len in
        blit str !str_off buf off len;
        str_off := !str_off + len;
        Lwt.return len
      )

  let output_fn len blit str =
    let str_off = ref 0 in
    let str_len = len str in
    fun buf off len ->
      if !str_off >= str_len then Lwt.return 0
      else (
        let len = min (str_len - !str_off) len in
        blit buf off str !str_off len;
        str_off := !str_off + len;
        Lwt.return len
      )

  let mk fn_i fn_o ?input ?output () =
    let input = match input with None -> None | Some x -> Some (fn_i x) in
    let output = match output with None -> None | Some x -> Some (fn_o x) in
    make ?input ?output ()

  let input_string = input_fn String.length Cstruct.blit_from_string
  let output_bytes = output_fn Bytes.length Cstruct.blit_to_bytes
  let string = mk input_string output_bytes

  let input_cstruct = input_fn Cstruct.length Cstruct.blit
  let output_cstruct = output_fn Cstruct.length Cstruct.blit
  let cstruct = mk input_cstruct output_cstruct

  let input_strings = iter input_string
  let output_bytess = iter output_bytes
  let strings = mk input_strings output_bytess

  let input_cstructs = iter input_cstruct
  let output_cstructs = iter output_cstruct
  let cstructs = mk input_cstructs output_cstructs

  let refill ch =
    if Cstruct.length ch.buf = 0 then (
      let buf = Cstruct.create default_buffer_size in
      ch.buf <- buf
    )

  let read ch =
    if ch.ic_closed then Lwt.return @@ Ok `Eof
    else (
      refill ch;
      ch.input ch.buf 0 default_buffer_size >>= fun n ->
      if n = 0 then (
        ch.ic_closed <- true;
        Lwt.return (Ok `Eof);
      ) else (
        let ret = Cstruct.sub ch.buf 0 n in
        let buf = Cstruct.shift ch.buf n in
        ch.buf <- buf;
        Lwt.return (Ok (`Data ret))
      )
    )

  let write ch buf =
    if ch.oc_closed then Lwt.return @@ Error `Closed
    else (
      let len = Cstruct.length buf in
      let rec aux off =
        if off = len then Lwt.return (Ok ())
        else (
          ch.output buf off (len - off) >>= fun n ->
          if n = 0 then (
            ch.oc_closed <- true;
            Lwt.return @@ Error `Closed
          ) else aux (off+n)
        )
      in
      aux 0
    )

  let writev ch bufs =
    if ch.oc_closed then Lwt.return @@ Error `Closed
    else
      let rec aux = function
        | []   -> Lwt.return (Ok ())
        | h::t ->
          write ch h >>= function
          | Error e -> Lwt.return (Error e)
          | Ok ()   -> aux t
      in
      aux bufs

  let shutdown ch mode =
    (match mode with
     | `read -> ch.ic_closed <- true
     | `write -> ch.oc_closed <- true
     | `read_write ->
       ch.ic_closed <- true;
       ch.oc_closed <- true);
    Lwt.return_unit

  let close ch =
    ch.ic_closed <- true;
    ch.oc_closed <- true;
    ch.close ()

end

type error = [`Msg of string]
type write_error = [ Mirage_flow.write_error | error ]
let pp_error ppf (`Msg s) = Fmt.string ppf s

let pp_write_error ppf = function
  | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
  | #error as e                   -> pp_error ppf e

type flow =
  | Flow: string * (module CONCRETE with type flow = 'a) * 'a -> flow

type t = flow

let create (type a) (module M: Mirage_flow.S with type flow = a) t name =
  let m = (module Concrete(M): CONCRETE with type flow = a) in
  Flow (name, m , t)

let read (Flow (_, (module F), flow)) = F.read flow
let write (Flow (_, (module F), flow)) b = F.write flow b
let writev (Flow (_, (module F), flow)) b = F.writev flow b
let close (Flow (_, (module F), flow)) = F.close flow
let shutdown (Flow (_, (module F), flow)) m = F.shutdown flow m
let pp ppf (Flow (name, _, _)) = Fmt.string ppf name

let forward ?(verbose=false) ~src ~dst () =
  let rec loop () =
    read src >>= function
    | Ok `Eof ->
      Log.err (fun l -> l "forward[%a => %a] EOF" pp src pp dst);
      Lwt.return_unit
    | Error e ->
      Log.err (fun l -> l "forward[%a => %a] %a" pp src pp dst pp_error e);
      Lwt.return_unit
    | Ok (`Data buf) ->
      Log.debug (fun l ->
          let payload =
            if verbose then Fmt.str "[%S]" @@ Cstruct.to_string buf
            else Fmt.str "%d bytes" (Cstruct.length buf)
          in
          l "forward[%a => %a] %s" pp src pp dst payload);
      write dst buf >>= function
      | Ok ()   -> loop ()
      | Error e ->
        Log.err (fun l -> l "forward[%a => %a] %a"
                    pp src pp dst pp_write_error e);
        Lwt.return_unit
  in
  loop ()

let proxy ?verbose f1 f2 =
  Lwt.join [
    forward ?verbose ~src:f1 ~dst:f2 ();
    forward ?verbose ~src:f2 ~dst:f1 ();
  ]
