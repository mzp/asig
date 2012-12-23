type ('a,'b) t = ('a, 'b) Either.t Lwt.t

val fmap : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
val lift : ('a -> ('b, 'c) Either.t) -> ('a, 'c) t -> ('b, 'c) t
