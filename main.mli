open Core.Std
open Async.Std

type resp_response =  [ `String of string | `NilString | `Error of string | `Array of resp_response list | `NilArray | `Number of Int64.t ]

(* Do we need this? *)
val parse_resp : Reader.t -> resp_response Deferred.t


val send_command : Writer.t * Reader.t -> string -> string list -> resp_response Deferred.t