open Core.Std
open Async.Std

type resp_response

type connection

(* Do we need this? *)
val parse_resp : Reader.t -> resp_response Deferred.t


val send_command : Writer.t * Reader.t -> string -> string list -> resp_response Deferred.t

val del : Writer.t * Reader.t -> string list -> int Deferred.t

val exists : Writer.t * Reader.t -> string -> bool Deferred.t

(*type db = string

val del 

val dump 

val exists

val expire key seconds

val expireat

val keys pattern

val move key db

*)