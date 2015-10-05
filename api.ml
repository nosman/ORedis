open Core.Std
open S

module Make(I : IO) : Api = struct
	module I = I
	open I
	type 'a t = 'a I.t

	type fd = I.fd
	type reader = I.reader
	type writer = I.writer
	

	type resp_response =  [ `String of string | `Nil | `Error of string | `Array of resp_response list | `Number of Int64.t ]

	exception UnexpectedEOF of string

	let connect host port = I.connect host port

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

let start = I.start


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
		| `Eof(len) -> failwith ("Unexpected end of file, only read " ^ (string_of_int len) ^ " bytes")
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
		| h::tl -> print_command h; helper tl
	in
	helper lst

let string_of_resp_str =
	function `String str -> str
	| _ -> failwith "not a resp string"

let string_list_of_array arr =
	arr >>| function `Array a -> 
		List.map a (fun x -> match x with
			| `String str -> str
			| _ -> failwith "tried to convert wrong resp type into string")
		| _ -> failwith "tried to convert wrong resp type into list"

(* connection string string list (resp_reply -> something) *)
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
	str >>| fun res -> match res with
	`String s -> s
	| _ -> failwith "Conversion to string failed"

let option_of_resp_string str =
	str >>| fun res -> match res with
	`String s -> Some s
	| `Nil -> None
	| _ -> failwith "Conversion to string failed"

let tuple_of_resp_array arr =
	arr >>| fun res ->
	match res with
	| `Array (x::x'::[]) -> Some (string_of_resp_str x, string_of_resp_str x')
	| _ -> None

let tuple_list_of_resp_array arr =
	arr >>| fun res ->
	match res with
	`Array a -> let (opt, result) = List.fold_left a ~init:(None, []) ~f:(fun (prev, lst) elem -> 
		match prev with Some s -> (None, (s, string_of_resp_str elem)::lst)
		| None -> (Some(string_of_resp_str elem), lst)
	) in (
		match opt with None -> result
		| Some _ -> failwith "List had odd number of elements, could not convert to tuple"
	)
	| _ -> failwith "Conversion to array failed"

let list_of_resp_array arr = arr >>| fun res -> match res with
| `Array arr -> arr
| _ -> failwith "Conversion to array failed"

let value_of_nil_or_resp conversion v = v >>= function
	| `Nil -> return None
	| `String _ | `Number _ | `Error _ | `Array _ -> conversion v >>| fun res -> Some res

let get connection key =
	apply_to_resp_reply connection "GET" [key] string_of_resp_string

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

let set connection ?ex ?px ?nx_or_xx key value =
	(* Unwrap optional args *)
	let args = match ex with
	| Some(expiration_secs) -> ["EX"; string_of_int expiration_secs]
	| None -> [] in
	let args = match px with
	| Some(expiration_msecs) -> ("PX"::(string_of_int expiration_msecs)::args)
	| None -> args in
	let args = match nx_or_xx with
	| Some(nx_or_xx) -> begin
		match nx_or_xx with 
		| `NX -> ("NX"::args)
		| `XX -> ("XX"::args)
	end
	| None -> args
in
	apply_to_resp_reply connection "SET" (key::value::(List.rev args)) string_of_resp_string

let sort connection ?by ?limit ?(get = []) ?order ?(alpha = false) key =
	(*Make a function to unwrap all the optional args *)
	let args = match by with
	| Some by -> ["BY"; by]
	| None -> [] in
	let args = match limit with
	| Some (start, limit) -> ("LIMIT"::(string_of_int start)::(string_of_int limit)::args)
	| None -> args in
	let args = match order with
	| Some `Asc -> "ASC"::args
	| Some `Desc -> "DESC"::args
	| None -> args in
	let args = if alpha then
	"ALPHA"::args else args in
	apply_to_resp_reply connection "SORT" (key::args) string_list_of_array

let ttl connection key =
	apply_to_resp_reply connection "TTL" [key] int_of_resp_num

let type_ connection key =
	apply_to_resp_reply connection "TYPE" [key] string_of_resp_string

let blpop connection lsts timeout =
	apply_to_resp_reply connection "BLPOP" (lsts@[string_of_int timeout]) tuple_of_resp_array

let brpop connection lsts timeout =
	apply_to_resp_reply connection "BRPOP" (lsts@[string_of_int timeout]) tuple_of_resp_array

let incr connection key =
	apply_to_resp_reply connection "INCR" [key] int_of_resp_num

let hset connection key (field, value) =
	apply_to_resp_reply connection "HSET" [key;field;value] bool_of_resp_num

let hmset connection key (field, value) field_value_list =
	let args = List.fold_left field_value_list ~init:[value;field;key] ~f:(fun acc (key, value) -> key::value::acc) in
	apply_to_resp_reply connection "HMSET" (List.rev args) string_of_resp_string

let hget connection key field =
	apply_to_resp_reply connection "HGET" [key;field] option_of_resp_string

let hdel connection key field field_lst =
	apply_to_resp_reply connection "HDEL" (key::field::field_lst) bool_of_resp_num

let zadd connection key ?nx_or_xx ?(ch = false) ?(incr = false) score_member_lst =
	let args = List.rev (List.fold_left score_member_lst ~init:[] ~f:(fun acc (score, member) -> (Float.to_string score)::member::acc)) in
	let args = if incr then
	"INCR"::args else args in
	let args = if ch then
	"CH"::args else
	args in
	let args = match nx_or_xx with
	| Some arg -> ( match arg with
		|`XX -> ["XX"]
		|`NX -> ["NX"]
	)
	| None -> args in
	apply_to_resp_reply connection "ZADD" (key::args) int_of_resp_num

let lpush connection key value value_lst =
	apply_to_resp_reply connection "LPUSH" (key::value::value_lst) int_of_resp_num

let ltrim connection key start stop =
	apply_to_resp_reply connection "LTRIM" [key;string_of_int start; string_of_int stop] string_of_resp_string

let hgetall connection key =
	apply_to_resp_reply connection "HGETALL" [key] tuple_list_of_resp_array

let zrange connection key start stop ?(withscores = false) =
	let args = if withscores then [key;string_of_int start; string_of_int stop; "WITHSCORES"] else
	[key;string_of_int start; string_of_int stop] in
	apply_to_resp_reply connection "ZRANGE" args list_of_resp_array

end