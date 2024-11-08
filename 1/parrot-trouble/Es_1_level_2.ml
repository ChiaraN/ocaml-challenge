let comp f g = fun x -> f(g x);;

fun f : 'b -> 'c;;
fun g : 'a -> 'b;;

comp : ('b -> c) -> ('a -> 'b) -> ('a -> 'c);;

