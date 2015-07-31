open Core.Std
open Async.Std
open Tcp

(*
let print_msg msg =
	return msg >>| fun msg -> print_endline msg *)

(*
let get_resp_response reader buffer 
'+'
'-'
':'
'$'
'*' *)

type resp_response =  [ `String of string | `Error of string | `Array of resp_response | `Number of Int64.t ]

let default_host = "localhost"

let default_port = 6379

let print_msg msg = Out_channel.output_string stdout msg;
Out_channel.flush stdout

let send_command writer com =
	ignore (Writer.write_line writer com)

let rec read_n_characters reader buffer num = 
	if num = 0 then
		return buffer
	else
	Reader.read_char reader >>= function
		| `Eof -> return buffer
		| `Ok(c) -> Buffer.add_char buffer c;
			read_n_characters reader buffer (num - 1)

(*
let resp_bulk_string reader =
	read_n_characters reader (Buffer.create 3) 3 >>= fun buffer ->
		if Buffer.length buffer = 3 then
			let num_bytes = Buffer.nth buffer 0 in
				(* TODO: check that the *)
		else
			failwith "Not enough characters read" *)
(* Handle number format exception *)
let read_number reader =
	Reader.read_until reader ~keep_delim:false (`Pred(fun c -> not (Char.is_digit c)))


(* Terminator string is \r\n *)
(*Make buffer an optional parameter*)
(*
let rec resp_simple_string reader =
	let rec helper reader buffer =
		Reader.read_char reader >>= fun result ->
			match result
*)

let rec read_until_terminator reader buffer =
	Reader.read_char reader >>= fun result ->
			match result with
				| `Eof -> failwith "Unexpected end of file"
				| `Ok(c) -> (* Using short-circuit && operator *)
						if Buffer.length buffer > 0 && Buffer.nth buffer ((Buffer.length buffer) - 1) = '\r' && c = '\n' then
							return (Buffer.contents buffer)
						else
							(Buffer.add_char buffer c; read_until_terminator reader buffer)

(* Optional buffer parameter *)
let resp_simple_string reader =
	read_until_terminator reader (Buffer.create 32) >>| fun result -> (`String result)

let resp_integer reader =
	read_until_terminator reader (Buffer.create 8) >>| fun result -> `Number (Int64.of_string result)

let resp_error reader =
	read_until_terminator reader (Buffer.create 32) >>| fun result -> (`Error result)

(*
let rec resp_integer reader =
	let rec helper reader buffer = *)


let terminate buffer c =
	let len = Buffer.length buffer in
	len > 0 && Buffer.nth buffer (len - 1) = '\r' && c = '\n'

let connection host port =
	connect (to_host_and_port host port) 


let rec parse_resp r buffer =
	Reader.read_char r >>= function
		| `Eof -> failwith "Unexpected end of file"
		| `Ok(c) -> begin
			match c with
				| '+' -> resp_simple_string r
				| '-' -> resp_integer r
				| ':' -> failwith "Integer"
				| '$' -> failwith "Bulk string"
				| '*' -> failwith "Array"
				| _ -> failwith "Wrong type"
			end

(* This will eventually take the type of command as well *)
let get_command reader =
	parse_resp reader (Buffer.create 32) >>| 
	function
	| `String(msg) -> print_endline msg
	| _ -> failwith "Not implemented"

let main host port =
	connection host port >>=
	fun (socket, r, w) -> 
		send_command w "PING";
		get_command r

(*
	>>|
	fun (addr, r, w) -> 
		ignore( Writer.write w "PING" *)

let print_deferred msg = return msg >>| print_msg



(* Want to get a connection to tcp socket *)

let _ =
	ignore (main default_host default_port);
	never_returns (Scheduler.go ())
(*
	ignore (print_endline "foo");
	never_returns (Scheduler.go ()) *)
