let f1 p1 p2 = not (p1 && p2);;

let f2 (b1 : bool) (b2 : bool) : bool = 
    if b1 
    then 
        if b2 
            then false 
        else true 
    else true;;

let f3 c1 c2 = match (c1, c2) with 
    | (false, false) -> true 
    | _ -> false
    ;;

