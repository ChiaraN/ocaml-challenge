let xor1 : bool -> bool -> bool =
  fun b1 b2: bool -> 
    not b1 && b2 || b1 && not b2


let xor2 : bool -> bool -> bool = 
    fun b1 b2: bool->
      if b1
        then
          if b2 then false else true 
    else
      b2 


let xor3 : bool -> bool -> bool = 
        fun b1 b2: bool->
          match (b1,b2) with 
          true,true -> false,
          false,true ->true
          true,false -> true
          false,false -> true 

