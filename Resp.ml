type respType = SimpleString of string | Error of string | Integer of int | BulkString of bytes | Array of respType list

exception MalformedInput

let terminator = "\r\n"

let terminatorLen = String.length terminator

let parseSimpleString str = 
	let len = String.length str in
	if len < terminatorLen + 1 then
	failwith "Malformed string" else
	SimpleString((String.sub str 1 (len - terminatorLen)))

let parseError str = 
	let len = String.length str in
	if len < terminatorLen + 1 then
	failwith "Malformed string" else
	Error((String.sub str 1 (len - terminatorLen)))

(*Check if string is too long? *)
let parseInteger str = 
	let len = String.length str in
	Integer(int_of_string (String.sub str 1 ((String.length str) - terminatorLen)))

(*Length field after $ *)
let parseBulkString str = 
	let numBytes = int_of_string (String.sub str 1 2) in 
	let bytes = Bytes.of_string (str) in
	failwith "Let me figure this out"

(*Add proper error handling for malformed input *)
let rec parse str =
	let switch = String.get str 0 in
		match switch with
			| '+' -> parseSimpleString str
			| '-' -> parseError str
			| ':' -> parseInteger str
			| '$' -> parseBulkString str
			| '*' -> parseArray str
			| _   -> failwith "Not a correct protocol string."
and parseArray str = failwith "Fuck you"

(* Uses tcp to connect and send strings to redis *)