module type IO : sig
	type 'a t
	val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
	val (>>|) : 'a t -> ('a -> 'b) -> 'b t
	val return : 'a -> 'a t
end

module Make(IO : IO) = struct

end