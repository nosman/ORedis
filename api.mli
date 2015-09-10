

module type IO = sig
	type 'a t
	include Monad.Infix with type 'a t := 'a t
	include Monad.Basic with type 'a t := 'a t
	
	type fd
	type reader
	type writer
end

module Make(IO : IO) : Api with module IO = IO
