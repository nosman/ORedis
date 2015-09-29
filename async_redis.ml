open Core.Std
open Async.Std

module Async_io : S.IO = struct
	type 'a t = 'a Deferred.t
	let (>>=) = Deferred.(>>=)
	let (>>|) = Deferred.(>>|)
	let return = Deferred.return
	
	type fd = Fd.t
	type reader = Reader.t
	type writer = Writer.t

	type 'a read_result = [ `Ok of 'a | `Eof ]

	let really_read = Reader.really_read
	let write = Writer.write
	let flushed = Writer.flushed

	let read_char = Reader.read_char

	let connect host port =
		Tcp.connect (Tcp.to_host_and_port host port) >>|
		fun (_, reader, writer) ->
			(reader, writer)

	(*

type 'a read_result = [ `Ok of 'a | `Eof ]

	val read_char : reader -> char read_result t
	val really_read : reader -> ?len:int -> string -> [ `Eof of int | `Ok ] t

	val write : writer -> string -> unit
	val flushed : writer -> unit t

	val connect : string -> int -> fd * reader * writer

	*)
end