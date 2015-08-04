open Core.Std
open Async.Std
open Tcp


(* Null handling for RESP replies *)
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

(* Thread-safe reading and writing from sockets *)

(* Connect should start the scheduler *)
(*Scheduler.is_running () tells us if scheduler has been started *)


exception UnexpectedEOF of string

let read f reader =
	f reader >>= function
		| `Eof -> raise (UnexpectedEOF "unexpected eof")
		| `Ok(c) -> return c

let read_char =
	read Reader.read_char

type resp_response =  [ `String of string | `NilString | `Error of string | `Array of resp_response list | `NilArray | `Number of Int64.t ]

let default_host = "localhost"

let default_port = 6379

(*
let send writer args =
	let num_args = List.length args in
*)

(*
let write out_ch args =
    let num_args = List.length args in
    IO.output_string out_ch (Printf.sprintf "*%d" num_args) >>= fun () ->
    IO.output_string out_ch "\r\n" >>= fun () ->
    IO.iter
      (fun arg ->
        let length = String.length arg in
        IO.output_string out_ch (Printf.sprintf "$%d" length) >>= fun () ->
        IO.output_string out_ch "\r\n" >>= fun () ->
        IO.output_string out_ch arg >>= fun () ->
        IO.output_string out_ch "\r\n"
      )
      args >>= fun () ->
    IO.flush out_ch *)

let nil_bulk_string = "$-1\r\n"

let nil_array = "*-1\r\n"


let to_bulk_string str =
	let len = String.length str in
		let buffer = Buffer.create (len + 6) in
			Buffer.add_char buffer '$';
			Buffer.add_string buffer (string_of_int len); Buffer.add_string buffer "\r\n";
			Buffer.add_string buffer str;
			Buffer.add_string buffer "\r\n";
			Buffer.contents buffer

let to_resp_fmt com args =
	let len = 1 + (List.length args) in
		let rec helper args buffer =
			match args with [] -> Buffer.contents buffer
			| h::tl -> let str = to_bulk_string h in
				(Buffer.add_string buffer str; helper tl buffer) in
		let buffer = Buffer.create 32 in
			(Buffer.add_string buffer ("*" ^ (string_of_int len) ^ "\r\n");
			Buffer.add_string buffer (to_bulk_string com);
			helper args buffer)

let write_command writer com args =
	let resp = to_resp_fmt com args in
	Writer.write writer resp;
	Writer.flushed writer

let print_msg msg = Out_channel.output_string stdout msg;
Out_channel.flush stdout

let read_until_terminator reader buffer =
	let rec loop () =
	 read_char reader >>= fun c1 ->
	 		if c1 = '\r' then read_char reader >>| fun c2 ->
	 			if c2 = '\n' then Buffer.contents buffer
	 		else failwith "Unexpected Terminator"
	 	else
	 		(Buffer.add_char buffer c1; loop ()) in
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
	match len with Some(x) -> ( if x > -1 then
	read_fixed_line reader x >>| fun result -> `String result
	else
	if x = -1 then return `NilString else failwith "Wrong length field")
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
	print_int x; print_endline "";
		let rec helper reader lst count =
			if count > 0 then
				parse_resp reader buffer >>= fun result ->
					helper reader (result::lst) (count - 1) 
			else if count = 0 then return (`Array lst) else if count = -1 then return (`NilArray) else failwith "Wrong length"
				in
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
				| '$' -> resp_bulk_string r
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

(* This will eventually take the type of command as well, as a function *)
let get_command reader = print_endline "_____________________";
	parse_resp reader (Buffer.create 32) >>| 
	function
	| `NilString -> print_endline "Nil String"
	| `String(msg) -> print_endline msg
	| `Array(lst) -> print_array (`Array lst)
	| `Number(num) -> (let num = Int64.to_int num in match num with
		Some(x) -> print_int x; print_endline ""
		| None -> failwith "Fuck"
	)
	| `Error(msg) -> print_endline msg
	| _ -> failwith "Fuck Fuck"

(* Switch arguments *)
let send_command (writer, reader) comm args =
	write_command writer comm args >>=
	fun () -> get_command reader

let main host port =
	connection host port >>=
	fun (socket, r, w) ->
		send_command (w,r) "PING" [] >>=
		fun _ -> send_command (w,r) "GET" ["Baz"]

		

let _ =
	ignore (main default_host default_port);
	never_returns (Scheduler.go ())
(*
	ignore (print_endline "foo");
	never_returns (Scheduler.go ()) *)
