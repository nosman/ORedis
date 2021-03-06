open Core.Std
open Async.Std
open Tcp

(* Thread-safe reading and writing from sockets *)

(* Connect should start the scheduler *)

(* Replace failwiths with async error handling *)

(*Make one helper further? *)


exception UnexpectedEOF of string

let read f reader =
	f reader >>= function
		| `Eof -> raise (UnexpectedEOF "unexpected eof")
		| `Ok(c) -> return c

let read_char =
	read Reader.read_char

let default_host = "localhost"

let default_port = 6379

type resp_response =  [ `String of string | `Nil | `Error of string | `Array of resp_response list | `Number of Int64.t ]

type connection = Writer.t * Reader.t

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
	Writer.write writer resp;
	Writer.flushed writer

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
		Reader.really_read ~len:length reader buffer >>=
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

let connection host port =
	connect (to_host_and_port host port)

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

let get_command reader =
	parse_resp reader 

let rec print_command command =
	match command with
		| `Nil -> print_endline "Nil"
		| `String(msg) -> print_endline msg
		| `Array(lst) -> print_array (`Array lst)
		| `Number(num) -> (let num = Int64.to_int num in match num with
			Some(x) -> print_int x; print_endline ""
		| None -> failwith "Fuck"
	)
	| `Error(msg) -> print_endline msg
and
print_array = function `Array lst ->
	let rec helper lst =
		match lst with
		| [] -> ()
		| h::tl -> print_command h; helper lst
	in
	helper lst

let string_list_of_array arr =
	arr >>| function `Array a -> 
		List.map a (fun x -> match x with
			| `String str -> str
			| _ -> failwith "tried to convert wrong resp type into string")
		| _ -> failwith "tried to convert wrong resp type into list"


(* Switch arguments *)
let send_command (writer, reader) comm args =
	write_command writer comm args >>=
	fun () -> get_command reader

let apply_to_resp_reply connection key args f =
		f (send_command connection key args)

let bool_of_resp_num num = num >>| function
	`Number boolean -> (if boolean = Int64.one then true
		else if boolean = Int64.zero then false
	else failwith "Unexpected number")
	| _ -> failwith "Wrong resp value passed in"

let int_of_resp_num num =
	num >>| function
	`Number num -> (match Int64.to_int num with
		| Some(x) -> x
		| None -> failwith "int64 to int conversion failed")
	| _ -> failwith "Wrong resp value passed in"

let string_of_resp_string str =
	str >>| function
	`String s -> s
	| _ -> failwith "Conversion to string failed"

let value_of_nil_or_resp conversion v = v >>= function
	| `Nil -> return None
	| `String _ | `Number _ | `Error _ | `Array _ -> conversion v >>| fun res -> Some res


let del connection keys =
	apply_to_resp_reply connection "DEL" keys int_of_resp_num

let dump connection key =
	apply_to_resp_reply connection "DUMP" [key] string_of_resp_string

let exists connection key =
	apply_to_resp_reply connection "EXISTS" [key] bool_of_resp_num

let expire connection key seconds =
	apply_to_resp_reply connection "EXPIRE" [key; (string_of_int seconds)] bool_of_resp_num

let expireat connection key timestamp =
	apply_to_resp_reply connection "EXPIREAT" [key; (Float.to_string timestamp)] bool_of_resp_num

let keys connection pattern =
	apply_to_resp_reply connection "KEYS" [pattern] string_list_of_array

let migrate connection host port key destination ?(copy = false) ?(replace = false) timeout =
	let optionals = if copy then ["COPY"] else [] in
		let optionals = if replace then "REPLACE"::optionals else optionals in
	apply_to_resp_reply connection "MIGRATE" ([ host; (string_of_int port); key; destination; (string_of_int timeout)]@optionals) string_of_resp_string

let move connection key db =
	apply_to_resp_reply connection "MIGRATE" [key;db] string_of_resp_string

let object_refcount connection key =
	apply_to_resp_reply connection "OBJECT" ["REFCOUNT";key] (value_of_nil_or_resp int_of_resp_num)

let object_idletime connection key =
	apply_to_resp_reply connection "OBJECT" ["IDLETIME";key] (value_of_nil_or_resp int_of_resp_num)

let object_encoding connection key =
	apply_to_resp_reply connection "OBJECT" ["ENCODING";key] (value_of_nil_or_resp string_of_resp_string)

let persist connection key =
	apply_to_resp_reply connection "PERSIST" [key] bool_of_resp_num

let pexpire connection key millis =
	apply_to_resp_reply connection "PEXPIRE" [key; (string_of_int millis)] bool_of_resp_num

let pexpireat connection key millis =
	apply_to_resp_reply connection "PEXPIREAT" [key; (string_of_int millis)] bool_of_resp_num

let pttl connection key =
	apply_to_resp_reply connection "PTTL" [key] int_of_resp_num

let randomkey connection =
	apply_to_resp_reply connection "RANDOMKEY" [] (value_of_nil_or_resp string_of_resp_string)

let rename connection orig_key dest_key =
	apply_to_resp_reply connection "RENAME" [orig_key; dest_key] string_of_resp_string

let renamenx connection orig_key dest_key =
	apply_to_resp_reply connection "RENAMEX" [orig_key; dest_key] bool_of_resp_num

let restore connection ?(replace = false) key ttl serialized_value =
	let options = if replace then
		["REPLACE"] else [] in
		apply_to_resp_reply connection "RESTORE" ([key; (string_of_int ttl); serialized_value]@options) string_of_resp_string

let sort connection ?by ?limit ?(get = []) ?order ?(alpha = false) key =
	let args = match by with
	| Some by -> ["BY"; by]
	| None -> [] in
	let args = match limit with
	| Some (start, limit) -> (["LIMIT"; (string_of_int start); (string_of_int limit)]@args)
	| None -> args in
	let args = match order with
	| Some `Asc -> "ASC"::args
	| Some `Desc -> "DESC"::args
	| None -> args in
	let args = if alpha then
	"ALPHA"::args else args in
	apply_to_resp_reply connection "SORT" args string_list_of_array

let ttl connection key =
	apply_to_resp_reply connection "TTL" [key] int_of_resp_num

let type_ connection key =
	apply_to_resp_reply connection "TYPE" [key] string_of_resp_string


let main host port =
	connection host port >>=
	fun (socket, r, w) ->
		send_command (w,r) "PING" [] >>=
		fun x -> return (print_command x) >>=
		fun _ -> send_command (w,r) "GET" ["Baz"]
		>>= fun x -> return (print_command x)

(*Make this into an init method*)
let _ =
	ignore (main default_host default_port);
	never_returns (Scheduler.go ())
