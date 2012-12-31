external id : 'a -> 'a = "%identity"
let (@@) f g = f g
let ($)  f g x = f (g x)
let (+>) g f = f g
let const x _ = x
let tee f x = ignore (f x); x
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
