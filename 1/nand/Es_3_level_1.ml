let mux2 (s0 :bool) (a : bool) (b :bool) : bool =
  s0 && a || (not s0) && b


let mux2 (s0 : bool) (a : bool) (b : bool) :
bool = if s0 then a else b
;;

let mux2 (s0 : bool) (a : bool) (b : bool) : bool = 
  match s0 with
  |true -> a
  | false -> b
;;



assert (mux2 true true true = true);;
assert (mux2 true true false = true);;
assert (mux2 true false true= false);;
assert (mux2 true false false = false);;
assert (mux2 false true true = true);;
assert (mux2 false true false = false);;
assert (mux2 false false true = true);;
assert (mux2 false false false = false);;


let mux4 (s0: bool) (s1 :bool) (a: bool) (b: bool) (c: bool) (d: bool) =
  mux2 s0 (mux2 s1 d c) (mux2 s0 a b) = mux2 s0 (mux2 s1 d c) (mux2 s1 b a)
;;

assert(mux4 false false false true false true = false);;
assert(mux4 false true false true false true = true);;
assert(mux4 true false false true false true = false);;
assert(mux4 true true false true false true = true);;