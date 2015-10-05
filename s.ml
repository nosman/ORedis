open Core.Std

(* Make IO connection type abstract *)

module type IO = sig
	type 'a t
	include Monad.Infix with type 'a t := 'a t
	val return : 'a -> 'a t
	
	type fd
	type reader
	type writer
	type 'a read_result = [ `Ok of 'a | `Eof ]

	val read_char : reader -> char read_result t
	val really_read :
           reader ->
           ?pos:int -> ?len:int -> string -> [ `Eof of int | `Ok ] t


	val write : ?pos:int -> ?len:int -> writer -> string -> unit
	val flushed : writer -> unit t
	val connect : string -> int -> (reader * writer) t

	val start : ?raise_unhandled_exn:bool -> unit -> never_returns

end
	
module type Api = sig
	module I : IO

	(*Make some functions return response lists. *)
	type resp_response =  [ `String of string | `Nil | `Error of string | `Array of resp_response list | `Number of Int64.t ]

	type fd = I.fd
	type reader = I.reader
	type writer = I.writer
	val connect : string -> int -> (reader * writer) I.t
	(*Option to disconnect? *)
	(*Take an Api module. Provide functions that let the user connect and start the actual client. 
	Add *)


	val parse_resp : I.reader -> resp_response I.t

	val send_command : I.writer * I.reader -> string -> string list -> resp_response I.t

	val print_command : resp_response -> unit

	val get : I.writer * I.reader -> string -> string I.t

	val del : I.writer * I.reader -> string list -> int I.t

	val dump : I.writer * I.reader -> string -> string I.t

	val exists : I.writer * I.reader -> string -> bool I.t

	val expire : I.writer * I.reader -> string -> int -> bool I.t

	val expireat : I.writer * I.reader -> string -> float -> bool I.t

	val keys : I.writer * I.reader -> string -> string list I.t

	val migrate : I.writer * I.reader -> string -> int -> string -> string -> ?copy:bool -> ?replace:bool -> int -> string I.t

	val move : I.writer * I.reader -> string -> string -> string I.t

	val object_refcount : I.writer * I.reader -> string -> int option I.t

	val object_idletime : I.writer * I.reader -> string -> int option I.t

	val object_encoding : I.writer * I.reader -> string -> string option I.t

	val persist : I.writer * I.reader -> string -> bool I.t

	val pexpire : I.writer * I.reader -> string -> int -> bool I.t

	val pexpireat : I.writer * I.reader -> string -> int -> bool I.t

	val pttl : I.writer * I.reader -> string -> int I.t

	val randomkey : I.writer * I.reader -> string option I.t

	val rename : I.writer * I.reader -> string -> string -> string I.t

	val renamenx : I.writer * I.reader -> string -> string -> bool I.t

	val restore : I.writer * I.reader -> ?replace:bool -> string -> int -> string -> string I.t

	val sort :
	  I.writer * I.reader ->
	  ?by:string -> ?limit:int * int -> ?get:'a list -> ?order:[< `Asc | `Desc ] -> ?alpha:bool -> string -> string list I.t

	val set : I.writer * I.reader -> ?ex:int -> ?px:int -> ?nx_or_xx:[< `NX | `XX ] -> string -> string -> string I.t

	val ttl : I.writer * I.reader -> string -> int I.t

	val type_ : I.writer * I.reader -> string -> string I.t

	(*val main : string -> int -> unit I.t *)

	val start : ?raise_unhandled_exn:bool -> unit -> never_returns

	(*Commands used in tutorial *)

	(*List operations*)

	val blpop : I.writer * I.reader -> string list -> int -> (string * string) option I.t

	val brpop : I.writer * I.reader -> string list -> int -> (string * string) option I.t

	val ltrim : I.writer * I.reader -> string -> int -> int -> string I.t

	val lpush :  I.writer * I.reader -> string -> string -> string list -> int I.t

	val blrpoplpush : I.writer * I.reader -> string -> string -> int -> string option I.t

	val rpoplpush : I.writer * I.reader -> string -> string -> string option I.t

	val lindex : I.writer * I.reader -> string -> int -> string option I.t

	val linsert : I.writer * I.reader -> string -> [< `Before | `After ] -> string -> string -> int option I.t

	val llen : I.writer * I.reader -> string -> int I.t


	val incr : I.writer * I.reader -> string -> int I.t

	val hset : I.writer * I.reader -> string -> (string * string) -> bool I.t

	val hmset : I.writer * I.reader -> string -> (string * string) -> (string * string) list -> string I.t

	val hget : I.writer * I.reader -> string -> string -> string option I.t

	val hgetall : I.writer * I.reader -> string -> (string * string) list I.t

	val hdel : I.writer * I.reader -> string -> string -> string list -> bool I.t


	(*can change *)
	val zadd : I.writer * I.reader -> string -> ?nx_or_xx:[< `NX | `XX ] -> ?ch:bool -> ?incr:bool -> (float * string) list ->
	int I.t

	val zrange : I.writer * I.reader -> string -> int -> int -> ?withscores:bool -> resp_response list I.t

	(*SortedSet operations *)

end