let ( tringolo) : 'a -> ('a -> 'b) -> 'b =
fun x f -> f x

let double x = x * 2
let square x = x * x;;

assert (3 triangolo double = 6);;
assert (3 triangolo square = 9);;
assert 3 triangolo square double