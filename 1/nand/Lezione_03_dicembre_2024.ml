type state = Q0 | Q1 | Qs;;
type label = L0 | L1;;

let step (a : label) (q : state) : state = match q with
  Q0 -> if a=L0 then Q0 else Q1
| Q1 -> Qs
| Qs -> Qs;;

let rec trans (w : label list) (q : state) : state = match w with
  [] -> q
| a::w' -> step a q |> trans w';;

let is_final = function
  Q1 -> true
| _ -> false;;
let accept (w : label list) : bool = 
  trans w Q0 |> is_final;;

accept [L0;L0;L1];;
accept [L0;L0;L1;L0;L1];;
