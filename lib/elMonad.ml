type ('a,'b) t = ('a, 'b) Either.t Lwt.t

let fmap f x =
  Lwt.map (Either.fmap f) x

let lift f x =
  Lwt.map (fun y -> Either.bind y f) x
