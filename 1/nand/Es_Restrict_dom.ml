let restrict ( f : 'a -> 'b)
(p : 'a -> bool) : 'a -> 'b option = 
if p x then 
  Some 
else 
  None

  let f1 = restrict succ (fun x -> x > 0);;
  assert (f1 1 = Some 2);;
  assert (f1 0 = None);;

  let f2 = restrict (fun x,y ) ->