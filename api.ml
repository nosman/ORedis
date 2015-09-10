
module type IO : sig
	type t
	val (>>=) : 'a t -> ('a -> 'b t ) -> 'b t
end



module Make(IO : IO) = struct

end