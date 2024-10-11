let mux2 (s0 :bool) (a : bool) (b :bool) : bool =
  s0 && a || (not s0) && b


let mux2 (s0 : bool) (a : bool) (b : bool) :
bool = if s0 then a else b
;;

let mux2 (s0 : bool) (a : bool) (b : bool) : bool = 
  match s0 with
  |true -> a
  | false -> b

assert (mux2_1 true true true = true);;
assert (mux2_1 true true false = true);;
assert (mux2_1 true false true= false);;
assert (mux2_1 true false false = false);;
assert (mux2_1 false true true = true);;
assert (mux2_1 false true false = false);;
assert (mux2_1 false false true = true);;
assert (mux2_1 false false false = false);;