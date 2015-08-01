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

(* Do some kind of "get or fail" method to avoid unwrapping the responses *)

let read f reader =
	f reader >>= function
		| `Eof -> failwith "Unexpected"
		| `Ok(c) -> return c

let read_char =
	read Reader.read_char

type resp_response =  [ `String of string | `Error of string | `Array of resp_response list | `Number of Int64.t ]

let default_host = "localhost"

let default_port = 6379

let print_msg msg = Out_channel.output_string stdout msg;
Out_channel.flush stdout

let send_command writer com =
	ignore (Writer.write_line writer com)


let read_until_terminator reader buffer =
	let rec loop () =
	 Reader.read_char reader >>= function
	 | `Eof -> failwith "Unexpected end of file"
	 | `Ok('\r') -> Reader.read_char reader >>=  (function
	 	| `Eof -> failwith "End of file"
	 	| `Ok('\n') -> return (Buffer.contents buffer)
	 	| `Ok(c) -> failwith "Illegal character in simple string"
	 )
	 | `Ok(c) -> Buffer.add_char buffer c; loop () in
	 loop ()

let read_fixed_line reader length =
	let buffer = Bytes.create length in
		Reader.really_read ~len:length reader buffer >>=
			function 
			| `Eof(len) -> failwith "Unexpected end of file"
			| `Ok -> (Reader.read_char reader >>= function
				| `Eof -> failwith "blah"
				| `Ok(c) -> (Reader.read_char reader >>= function
					| `Eof -> failwith "blah"
					| `Ok(c') -> if c = '\r' && c' = '\n' then 
									return (Bytes.unsafe_to_string buffer)
									else
									failwith "Blah"
				)
			)

let read_length reader =
	read_until_terminator reader (Buffer.create 8) >>| fun result -> Int64.of_string result

(* Optional buffer parameter *)
let resp_simple_string reader =
	read_until_terminator reader (Buffer.create 32) >>| fun result -> (`String result)

(* Handle error parsing int *)
(* Just use the read length function *)
let resp_bulk_string reader =
	read_length reader >>= fun len -> let len = Int64.to_int len in
	match len with Some(x) -> read_fixed_line reader x >>| fun result -> `String result
			| _ -> failwith "Wrong int conversion"

let resp_integer reader =
	read_until_terminator reader (Buffer.create 8) >>| fun result -> `Number (Int64.of_string result)

let resp_error reader =
	read_until_terminator reader (Buffer.create 32) >>| fun result -> (`Error result)

let connection host port =
	connect (to_host_and_port host port) 

let rec resp_array r =
	let buffer = Buffer.create 32 in
	read_length r >>= fun len -> let len = Int64.to_int len in
	match len with Some(x) ->
		let rec helper reader lst count =
			if count = 0 then return (`Array lst) else
				parse_resp reader buffer >>= fun result ->
					helper reader (result::lst) (count - 1) in
					helper r [] x
		| _ -> failwith "Wrong int conversion"
and parse_resp r buffer =
	Reader.read_char r >>= function
		| `Eof -> failwith "Unexpected end of file"
		| `Ok(c) -> begin
			match c with
				| '+' -> resp_simple_string r
				| '-' -> resp_error r
				| ':' -> resp_integer r
				| '$' -> print_char c; resp_bulk_string r
				| '*' -> resp_array r
				| _ -> failwith "Wrong type"
			end

let print_array =
	function `Array(lst) -> let rec helper lst = (
		match lst with [] -> ()
		| h::tl -> match h with `String(s) -> print_endline s; helper tl
		| _ -> failwith "fuck"
	) in helper lst
	| _ -> failwith "Fuck"

(* This will eventually take the type of command as well *)
let get_command reader =
	parse_resp reader (Buffer.create 32) >>| 
	function
	| `String(msg) -> print_endline msg
	| `Array(lst) -> print_array (`Array lst)
	| _ -> failwith "Not implemented"

let main host port =
	connection host port >>=
	fun (socket, r, w) -> 
		send_command w "CLIENT LIST";
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
