let apply_if (p : 'a -> bool) (f : 'a -> 'a) : 'a -> 'a = 
  fun x ->
    if p x then 
      f x
  else 
    x

    let is_even = fun x -> x  mod 2 = 0
    let is_odd = fun x -> not (is_even x)
    let double x = x * 2;;
    let square x = x * x;;