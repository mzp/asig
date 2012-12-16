type ('a,'b) t =
    [ `Ok of 'a
    | `Error of 'b ]

let bind t f = match t with
  | `Ok v -> f v
  | `Error err -> `Error err

let (>>=) = bind

let fmap f t = match t with
  | `Ok v -> `Ok (f v)
  | `Error err -> `Error err

let return x = `Ok x
