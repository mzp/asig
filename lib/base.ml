external id : 'a -> 'a = "%identity"
let (@@) f g = f g
let ($)  f g x = f (g x)
let (+>) g f = f g
let const x _ = x
let tee f x = ignore (f x); x
