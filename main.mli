open Core.Std
open Async.Std

type resp_response

type connection

(* Do we need this? *)
val parse_resp : Reader.t -> resp_response Deferred.t

val send_command : Writer.t * Reader.t -> string -> string list -> resp_response Deferred.t

val del : connection -> string list -> int Deferred.t

val dump : connection -> string -> string Deferred.t

val exists : connection -> string -> bool Deferred.t

val expire : connection -> string -> int -> bool Deferred.t

val expireat : connection -> string -> float -> bool Deferred.t

val keys : connection  -> string -> string list Deferred.t

val migrate : connection -> string -> int -> string -> string -> ?copy:bool -> ?replace:bool -> int -> string Deferred.t

val move : connection -> string -> string -> string Deferred.t

val object_refcount: connection -> string -> int option Deferred.t

val object_encoding: connection -> string -> string option Deferred.t

val object_idletime: connection -> string -> int option Deferred.t

val persist: connection -> string -> bool Deferred.t

val pexpire: connection -> string -> int -> bool Deferred.t

val pexpireat : connection -> string -> int -> bool Deferred.t

val pttl : connection -> string -> int Deferred.t

val randomkey : connection -> string option Deferred.t

val rename : connection -> string -> string -> string Deferred.t

val renamenx : connection -> string -> string -> bool Deferred.t

val restore: connection -> ?replace:bool -> string -> int -> string -> string Deferred.t

(*
val sort :
    connection ->
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] -> ?alpha:bool -> string -> string list Deferred.t *)

val ttl : connection -> string -> int Deferred.t

val type_ : connection -> string -> string Deferred.t

