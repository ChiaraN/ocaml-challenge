let f1 p1 p2 = not (p1 && p2);;

let f2 b1 b2 = if b1 than b2 else b1 * b2 ;;

let f3 c1 c2 = match (c1, c2) with 
    | (false, false) -> true 
    | _ -> false
    ;;

