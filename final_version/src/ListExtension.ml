(* val isplit : int -> 'a list -> 'a list * 'a list *)
let rec isplit = fun i l ->
	match l with 
	| h::t -> if i <= 0 then ([],l) 
			  else let p = isplit (i-1) t in (h::fst p,snd p)
	| [] -> ([],[]);;

(* val tail_fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let tail_fold_left = fun op neutral l ->
	let rec aux = fun l res ->
		match l with
		|[] -> res
		|h::t -> aux t (op res h)
	in aux l neutral;;

(* val index_of : ('a -> bool) -> 'a list -> int *)
let index_of = fun p l ->
	let rec aux = fun l i ->
		match l with
		| h::t -> if p h then i else aux t (i+1)
		| [] -> -1
	in aux l 0;;

(* val all_index_of : ('a -> bool) -> 'a list -> int list *)
let all_index_of = fun p l ->
	let rec aux = fun l i res ->
		match l with
		| h::t -> if p h then aux t (i+1) (i::res) else aux t (i+1) res
		| [] -> res 
	in aux l 0 [];;

(* val apply_to_nth : ('a -> 'a) -> int -> 'a list -> 'a list *)
let apply_to_nth = fun op n l ->
	let rec aux = fun n l ->
		match l with
		| h::t -> if n = 0 then op h :: t
			  else h :: (aux (n-1) t)
		| [] -> failwith "out of bounds"
	in 
	if n < 0 then failwith "out of bounds"
	else aux n l;;
