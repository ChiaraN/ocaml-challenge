(*Ricordiamo come sono rappresentate le liste in OCaml*)
[1;2;3];;
(* - : int list = [1; 2; 3] *)
(*Questa è la rappresentazione human-readable, ma sono rappresentate internamente utilizzando l'operatore di concatenazione ::*)
1::2::3::[];;
(* - : int list = [1; 2; 3] *)
(*Queste due rappresentazioni sono equivalenti*)
(*Rivediamo la funzione che cerca un elemento in una lista*)
(*Ricordiamo che scrivendo in questo modo non è nexessario battezzare la variabile su cui fare il pattern matching*)
let rec mem a = function
| [] -> false
| h::l -> if h = a then true 
          else mem a l
(*Se la lista è vuota false, altrimenti la lista ha una testa h e una coda lista l, se h=a abbiamo trovato l'elemento cercato, altrimenti continuiamo a cercare nella coda della lista*)
;;
(* val mem : 'a -> 'a list -> bool = <fun> *)
(***LIST COMBINATORS***)
(*Sono funzioni su lista generalmente higher-order, polimorfe e ricorsive, sono detti combinatori perchè come operatori molto generali sulle liste, possono essere combinati per effettuare operazioni molto complesse*)
;;

(*Vogliamo provare a scrivere una funzione
  for_all p l controlla il predicato p su tutti gli elementi di una lista
  Proviamo a vedere il tipo della funzione
  for_all ('a -> bool) -> 'a list -> bool*)
let rec for_all p = function
(*Ogni studente che ha tre occhi ha sette braccia è true, se l'insieme è vuoto allora è true*)
| [] -> true
(*Altrimenti prendiamo la testa e valutiamo il predicato, lo mettiamo in hand con la funz sul resto della lista*)
| h::l -> p h && for_all p l
;;

assert(for_all (fun x -> x > 0)[1;2;3]);;
assert(for_all (fun x -> x > 0)[1;-2;3] = false);;

(*exists: quando almeno uno degli elementi della lista soddisfa il predicato*)
let rec exists p = function
| [] -> false
| h::l -> p h || exists p l
;;

assert(exists(fun x -> x mod 2 = 0)[1;2;3]);;
assert(exists(fun x -> x mod 2 = 0)[1;3;5] = false);;

(* map f l: prende una funzione e una lista, applica la funzione f a tutti gli elementi della lista l*)

(*Proviamo a capire il tipo
  map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f = function
| [] -> []
| h::l -> f h :: map f l
;;

assert(map (fun x -> x*2) [1;2;3] = [2;4;6]);;
assert(map (fun x -> x mod 2 = 0)[1;2;3] = [false;true;false]);;

map (fun x -> [x]) [1;2;3];; (*Questo esempio ci mostra che le liste di liste sono una cosa molto tranquilla*)

[1;2;3] |> map (fun x -> x+1);;

[1;2;3] |> map (fun x -> x+1) |> map (fun x -> x*2);;
(*La pipe ci facilita l'utilizzo di più map successive*)

(* filter p l: restituisce una lista composta dagli elementi della lista l che soddisfano il predicato p *)
(* Valutiamo il tipo
  filter: ('a -> bool) -> 'a list -> 'a list*)

let rec filter p = function
| [] -> []
| h::l -> if p h then h::filter p l else filter p l
;;

let rec filter2 p = function
| [] -> []
| h::l when p h -> h::(filter2 p l)
| _::l -> filter2 p l
;;

assert(filter2 (fun x -> x mod 2 = 0) [1;2;3;4;5] = [2;4]);;

[1;2;3;4;5]
|> filter2 (fun x -> x mod 2 = 0)
|> map (fun x -> x/2);;


let rec sum = function
| [] -> 0
| h::l -> h + sum l
;;

sum[1;2;3];;


(* fold_left (aka reduce su python) fold_left : ('a -> 'b -> ...)*)
List.fold_left (+) 0 [1;2;3];;
List.fold_left (fun x y -> x*x + y*y) 0 [1;2;3];;
List.fold_left (fun b n -> b && n mod 2 = 0) true [1;2;3];;
(* Il tipo di fold è ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a:
  prende una funzione da 'a e 'b a 'a, preso il caso base 'a e la lista di 'b restituisce 'a*)

let rec fold_left f z = function
| [] -> z
| h::l -> f (fold_left f z l) h
;;

fold_left (+) 0 [1;2;3];;

(*Proviamo ad utilizzare fold_left per trovare il minimo elemento di una lista di naturali*)
let rec minlist = function
| [] -> 0
| [h] -> h
| h::l -> let m = minlist l in if m<h then m else h
;;
minlist[3;2;5;1;7;8];;

type 'a option = None | Some of 'a;;
let rec minlist_opt = function
| [] -> None
| h::l -> (match minlist_opt l with 
  | None -> Some h
  | Some m -> if m<h then Some m else Some h)
;;

minlist_opt[];;
minlist_opt[3;2;5;1;7;8];;


let minlist2_opt l = fold_left
  (fun acc x -> match acc with
    | None -> Some x
    | Some m -> if m < x then Some m else Some x)
  None
  l
;;

minlist2_opt[3;2;5;1;7;8];;

(* Usiamo la fold per ridefinire exists visto prima*)
let exists2 p l = fold_left
  (fun acc x -> if acc then acc else p x) (* o acc || p x*)
  false
  l
;;
exists2 (fun x -> x mod 2 = 0) [1;3;3;7;5];;
let exists3 p = fold_left
  (fun acc x -> if acc then acc else p x) (* o acc || p x*)
  false
;;
exists2 (fun x -> x mod 2 = 0) [1;3;3;7;5];;

(*Somma dei quadrati degli elementi pari della lista*)
[1;2;3;4;5]
|> filter2 (fun x -> x mod 2 = 0)
|> map (fun x -> x*x)
|> fold_left (+) 0
;;

(*Esercizi*)
(* split : da lista di coppie a coppia di liste *)
List.split;;
(*('a * 'b) list -> 'a list * 'b list*)

let rec split = function
| [] -> ([],[])
| (a,b)::l -> let (l1, l2) = split l in (a::l1, b::l2)
;;

assert(split [(1,2);(2,3);(3,4)] = ([1;2;3],[2;3;4]));;

(* Partiamo da una coppia di liste, vogliamo una lista di coppie: *)
List.combine;;
(*'a list -> 'b list -> ('a * 'b) list*)

let rec combine l1 l2 = match (l1,l2) with
  ([],[]) -> []
| (a::l1', b::l2') -> (a,b) :: combine l1' l2'
| _ -> failwith "The lists must be of equal length"
;;

combine [1;2;3] ["a"; "b"; "c"];;