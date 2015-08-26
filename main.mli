open Core.Std
open Async.Std

type resp_response

type connection

(* Do we need this? *)
val parse_resp : Reader.t -> resp_response Deferred.t


val send_command : Writer.t * Reader.t -> string -> string list -> resp_response Deferred.t

val del : connection -> string list -> int Deferred.t

val exists : connection -> string -> bool Deferred.t

val expire : connection -> string -> int -> bool Deferred.t

val expireat : connection -> string -> float -> bool Deferred.t

val keys : connection  -> string -> string list Deferred.t

(*val x : ?y:'a*)
val migrate : connection -> string -> int -> string -> string -> ?copy:bool -> ?replace:bool -> int -> string Deferred.t

val move : connection -> string -> string -> string Deferred.t

val object_refcount: connection -> string -> int option Deferred.t

val object_encoding: connection -> string -> string option Deferred.t

val object_idletime: connection -> string -> int option Deferred.t

(*type db = string

val del 

val dump 

val exists

val expire key seconds

val expireat

val keys pattern

val move key db

*)