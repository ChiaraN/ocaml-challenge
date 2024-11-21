(* for_all p l checks p on all the elements of a list *)
(*for_all ('a -> bool) -> 'a list -> bool *)

let rec for_all p = function
  [] -> true
| h::l -> p h && for_all p l;;

assert(for_all (fun x -> x > 0) [1;2;3]);;
assert(for_all (fun x-> x > 0) [1; -2;3] = false);;

let rec exists p = function
  [] -> false
 | h:: l -> p h || exists p l;;
 assert (exists (fun x -> x mod 2 = 0) [1;2;3]);;
 assert (exists (fun x -> x mod 2 = 0)[1;3;5] = false);;
 
 (* map f l applica la funzione f a tutti gli elementi di l *)
 (* map : ('a -> 'b) -> 'a list' -> 'b list'*)

 let rec map f = function
   [] -> []
  | h::l -> f h :: map f l;;
  
  assert(map(fun x -> x*2) [1;2;3] = [2;4;6]);;
  assert(map (fun x -> x mod 2 = 0) [1;2;3] = [false;true;false]);;

  [1;2;3] |> map ( fun x -> x + 1) |> map (fun x -> x*2);;    (*L'operatore |> prende in input una funzione e restituisce un valore*)

  (*filter p l filtra tutti gli elementi di l che soddisfano p*)
  (*filter : ... -> 'a list -> 'a list -> 'a list *)

  let rec filter p = function
    [] -> []
   | h::l -> if p h then h :: (filter p l) else filter p l;;   

   (*oppure*)
   let rec filter2 p = function
   [] -> []
  | h::l when p h -> h :: (filter2 p l)
  | _::l -> filter2 p l;; 
 
  assert(filter (fun x -> x mod 2 = 0) [1;2;3;4;5] = [2;4]);;

  [1;2;3;4;5]
  |> filter (fun x -> x mod 2 = 0)
  |> map (fun x -> x / 2);;
  
  let rec sum = function
   [] -> 0
  | h::l -> h + sum l;;
  
  sum [1;2;3];;

  List.fold_left (+) 0 [1;2;3];;
  List.fold_left (fun x y -> x*x + y*y) 0 [1;2;3];;
  List.fold_left (fun b n -> b && n mod 2 = 0) true [];;
  List.fold_left (fun b n -> b && n mod 2 = 0) true [1];;
  List.fold_left (fun b n -> b && n mod 2 = 0) true [1;2];;
  List.fold_left (fun b n -> b && n mod 2 = 0) true [4];;
  List.fold_left (fun b n -> b && n mod 2 = 0) true [4;6];;

  let rec fold_left f z = function
    [] -> z
   | h::l -> f(fold_left f z l) h;;

  fold_left (+) 0 [1;2;3];;

(* minlist : calcola il minimo elemento di una lista (di naturali) *)

let rec minlist = function
 [] -> 0
 | [h] -> h
| h::l -> let m = minlist l in if m<h then m else h;;


let minlist2 l = fold_left (fun acc x -> if x<acc then x else acc) 0 l;;
minlist [3;2;5;1;7;8];;

type ' a option = None |Some of 'a;;

let rec minlist_opt = function
  [] -> None
 | h::l -> (match minlist_opt l with
    None -> Some h 
  | Some m -> if m<h then Some m else Some h);;
 
minlist_opt [];;
minlist_opt [5;2;3;7;1;8];;

let minlist2_opt l = fold_left
(fun acc x -> match acc with
None -> Some x
| Some m -> if m<x then Some m else Some x)
None
l;;

let exists2 p l = fold_left
(fun acc x -> if acc then acc else p x)
false
;;

exists2 (fun x -> x mod 2 = 0) [1;3;3;7;5];;

minlist_opt [3;2;5;1;7;8];;

(*somma dei quadrati degli elementi pari alla lista*)
[1;2;3;4;5]
|> filter (fun x -> x mod 2 = 0)
|> map (fun x -> x*x)
|> fold_left (+) 0;;

(*split : da lista di coppie a coppia di liste*)
(*split [(1,2);(2,3);(3,4)] = [1;2;3],[2;3;4]*)
List.split;;

let rec split


(*fold_left('a -> 'b -> ...) *)
(*fold_right*)
