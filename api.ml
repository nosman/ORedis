open Core.Std
open S

module Make(I : IO) : Api = struct
	module I = I
	open I
	type 'a t = 'a I.t

	type resp_response =  [ `String of string | `Nil | `Error of string | `Array of resp_response list | `Number of Int64.t ]

	exception UnexpectedEOF of string

	let read f reader =
	f reader >>= function
		| `Eof -> raise (UnexpectedEOF "unexpected eof")
		| `Ok(c) -> return c

let read_char =
	read read_char

let default_host = "localhost"

let default_port = 6379

type connection = writer * reader

let nil_bulk_string = "$-1\r\n"

let nil_array = "*-1\r\n"

let print_string str =
	return (print_endline str)


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
	write writer resp;
	flushed writer

let read_until_terminator reader buffer =
	let rec loop () =
	 read_char reader >>= fun c1 ->
	 		if c1 = '\r' then read_char reader >>| fun c2 ->
	 			if c2 = '\n' then Buffer.contents buffer
	 		else failwith "Unexpected Terminator"
	 	else
	 		(Buffer.add_char buffer c1; loop ()) in
	 		loop ()

(*This breaks with -safe-string on, because of Reader.really_read. how to modify *)
let read_fixed_line reader length =
	let buffer = Bytes.create length in
		really_read ~len:length reader buffer >>=
		function
		| `Eof(len) -> failwith "Unexpected end of file"
		| `Ok -> read_char reader >>= fun c -> read_char reader >>= fun c' -> if c = '\r' && c' = '\n' then 
									return buffer
									else
									failwith "Blah"

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
	if x = -1 then return `Nil else failwith "Wrong length field")
			| _ -> failwith "Wrong int conversion"

let resp_integer reader =
	read_until_terminator reader (Buffer.create 8) >>| fun result -> `Number (Int64.of_string result)

let resp_error reader =
	read_until_terminator reader (Buffer.create 32) >>| fun result -> (`Error result)

let rec resp_array r =
	read_length r >>= fun len -> let len = Int64.to_int len in
	match len with Some(x) ->
	print_int x; print_endline "";
		let rec helper reader lst count =
			if count > 0 then
				parse_resp reader >>= fun result ->
					helper reader (result::lst) (count - 1) 
			else if count = 0 then return (`Array lst) else if count = -1 then return (`Nil) else failwith "Wrong length"
				in
					helper r [] x
		| _ -> failwith "Wrong int conversion"
and parse_resp r = 
	read_char r >>= function
				| '+' -> resp_simple_string r
				| '-' -> resp_error r
				| ':' -> resp_integer r
				| '$' -> resp_bulk_string r
				| '*' -> resp_array r
				| _ -> failwith "Wrong type"

let get_command reader =
	parse_resp reader 

let send_command (writer, reader) comm args =
	write_command writer comm args >>=
	fun () -> get_command reader

end