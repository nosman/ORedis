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
	type connection
	val connect : string -> int -> connection I.t
	(*Option to disconnect? *)
	(*Take an Api module. Provide functions that let the user connect and start the actual client. 
	Add *)


	val send_command : connection -> string -> string list -> resp_response I.t

	val print_command : resp_response -> unit

	val get : connection -> string -> string I.t

	val del : connection -> string list -> int I.t

	val dump : connection -> string -> string I.t

	val exists : connection -> string -> bool I.t

	val expire : connection -> string -> int -> bool I.t

	val expireat : connection -> string -> float -> bool I.t

	val keys : connection -> string -> string list I.t

	val migrate : connection -> string -> int -> string -> string -> ?copy:bool -> ?replace:bool -> int -> string I.t

	val move : connection -> string -> string -> string I.t

	val object_refcount : connection -> string -> int option I.t

	val object_idletime : connection -> string -> int option I.t

	val object_encoding : connection -> string -> string option I.t

	val persist : connection -> string -> bool I.t

	val pexpire : connection -> string -> int -> bool I.t

	val pexpireat : connection -> string -> int -> bool I.t

	val pttl : connection -> string -> int I.t

	val randomkey : connection -> string option I.t

	val rename : connection -> string -> string -> string I.t

	val renamenx : connection -> string -> string -> bool I.t

	val restore : connection -> ?replace:bool -> string -> int -> string -> string I.t

	val sort :
	  connection ->
	  ?by:string -> ?limit:int * int -> ?get:'a list -> ?order:[< `Asc | `Desc ] -> ?alpha:bool -> string -> string list I.t

	val set : connection -> ?ex:int -> ?px:int -> ?nx_or_xx:[< `NX | `XX ] -> string -> string -> string I.t

	val ttl : connection -> string -> int I.t

	val type_ : connection -> string -> string I.t

	(*val main : string -> int -> unit I.t *)

	val start : ?raise_unhandled_exn:bool -> unit -> never_returns

	(*Commands used in tutorial *)

	(*List operations*)

	val blpop : connection -> string list -> int -> (string * string) option I.t

	val brpop : connection -> string list -> int -> (string * string) option I.t

	val ltrim : connection -> string -> int -> int -> string I.t

	val blrpoplpush : connection -> string -> string -> int -> string option I.t

	val rpoplpush : connection -> string -> string -> string option I.t

	val lindex : connection -> string -> int -> string option I.t

	val linsert : connection -> string -> [< `Before | `After ] -> string -> string -> int option I.t

	val llen : connection -> string -> int I.t

	val lpop : connection -> string -> string I.t

	val lpush :  connection -> string -> string -> string list -> int I.t

	val lpushx : connection -> string -> string -> int I.t

	val lrange : connection -> string -> int -> int -> resp_response list I.t

	(* How to make this function work with other types than string values? GADTs? *)
	val lrem : connection -> string -> int -> string -> int I.t

	val lset : connection -> string -> int -> string -> string I.t

	val ltrim : connection -> string -> int -> int -> string I.t

	val rpop : connection -> string -> string option I.t

	val rpoplpush : connection -> string -> string -> string option I.t

	val rpush : connection -> string -> string list -> int I.t

	val rpushx : connection -> string -> string list -> int I.t


	val incr : connection -> string -> int I.t

	val hset : connection -> string -> (string * string) -> bool I.t

	val hmset : connection -> string -> (string * string) -> (string * string) list -> string I.t

	val hget : connection -> string -> string -> string option I.t

	val hgetall : connection -> string -> (string * string) list I.t

	val hdel : connection -> string -> string -> string list -> bool I.t



	(*Transactional commands *)

	val discard : connection -> unit I.t

	val exec : connection -> unit I.t

	val multi : connection -> unit I.t

	val unwatch : connection -> unit I.t

	val watch : connection -> string list -> string I.t



	(*can change *)
	val zadd : connection -> string -> ?nx_or_xx:[< `NX | `XX ] -> ?ch:bool -> ?incr:bool -> (float * string) list ->
	int I.t

	val zrange : connection -> string -> int -> int -> ?withscores:bool -> resp_response list I.t

	(*SortedSet operations *)

end