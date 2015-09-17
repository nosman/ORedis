open Core.Std

module type IO = sig
	type 'a t
	include Monad.Infix with type 'a t := 'a t
	include Monad.Basic with type 'a t := 'a t
	
	type socket
	type fd
	type reader
	type writer
	type 'a read_result = [ `Ok of 'a | `Eof ]

	val read_char : reader -> char read_result t
	val really_read : reader -> ?len:int -> string -> [ `Eof of int | `Ok ] t

	val write : writer -> string -> unit
	val flushed : writer -> unit t

	val connect : string -> int -> fd * reader * writer
end

module type Api = sig
	module I : IO

	type resp_response =  [ `String of string | `Nil | `Error of string | `Array of resp_response list | `Number of Int64.t ]

	val parse_resp : I.reader -> resp_response I.t

	val send_command : I.writer * I.reader -> string -> string list -> resp_response I.t
end