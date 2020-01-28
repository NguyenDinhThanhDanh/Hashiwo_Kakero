open Coordinates;;
        
(* val get_elt : Coordinates.coord -> 'a list list -> 'a *)
let get_elt: coord -> 'a list list -> 'a =
	fun (x,y) mat  -> List.nth (List.nth mat (x-1)) (y-1);;

(* val size : 'a list list -> int * int *)
let size = fun mat -> (List.length mat,List.length (List.hd mat));;

(* val for_all : ('a -> bool) -> 'a list list -> bool *)
let for_all = fun op mat -> List.for_all (List.for_all op) mat;;

(* val tail_fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list list -> 'a *)
let tail_fold_left = fun op neutral mat -> 
	ListExtension.tail_fold_left (fun res col -> ListExtension.tail_fold_left op res col) neutral mat;;
        
(* val coords_of : ('a -> bool) -> 'a list list -> Coordinates.coord option *)
let coords_of: ('a -> bool) -> 'a list list -> coord option =
	fun p mat ->
		let rec aux = fun mat x ->
			match mat with
			|h::t -> let y = ListExtension.index_of p h in 
					 if y <> -1 then Some (x+1,y+1)
					 else aux t (x+1)
			| [] -> None
		in aux mat 0;;
        
(* val all_coords_of : ('a -> bool) -> 'a list list -> Coordinates.coord list *)
let all_coords_of: ('a -> bool) -> 'a list list -> coord list =
	fun p mat ->
		let rec aux = fun mat x res ->
			match mat with
			|h::t -> aux t (x+1) (res @ (List.map (fun y -> (x+1,y+1)) (ListExtension.all_index_of p h)))
			| [] -> res
		in aux mat 0 [];;

(* val replace : ('a -> 'a) -> Coordinates.coord -> 'a list list -> 'a list list *)
let apply_to_coord: ('a -> 'a) -> coord -> 'a list list -> 'a list list =
	fun op (x,y) mat -> 
		match ListExtension.isplit (x-1) mat with
		| (b,h::e) -> b @ (ListExtension.apply_to_nth op (y-1) h :: e)
		| _ -> failwith "out of bounds";;
            
(* val matrix_of_coord_list : 'a -> (Coordinates.coord * 'b) list -> 'a list list *)
let matrix_of_coord_list: 'a -> (coord * 'b) list -> 'a list list =
	fun neutral coord_list ->
		let coord_list = List.sort (fun (c1,_) (c2,_) -> coord_comp c1 c2) coord_list
		in
		let (mx, my) = ListExtension.tail_fold_left (fun (x,y) ((u,v), _) -> (max x u, max y v)) (1,1) coord_list  
		in
		let rec aux = fun (x,y) cl col res ->
			let (c,rem) = 
				match cl with
				|((u,v), elt)::t -> 
					if u = x && v = y then (elt, t)
					else if u < 1 || v < 1 then failwith "coordinates lower than 1 in coord_list"
					else (neutral,cl)
				|[] -> (neutral,[])
			in
			if x = mx && y = my then res@[col@[c]]
			else if y = my then aux (x+1,1) rem [] (res@[col@[c]])
			else aux (x,y+1) rem (col@[c]) res

		in aux (1,1) coord_list [] [];;
        
(* val string_of_matrix : ('a -> string) -> 'a list list -> string = <fun> *)
let string_of_matrix = fun op sol ->
	let rec aux = fun sol_begin sol_end res ->
		match (sol_begin ,sol_end) with
		| (b,(yh::yt)::xt) -> aux (b@[yt]) xt (res ^ op yh)
		| (b,[]) -> aux [] b (res^"\n")
		| (_, []::_) -> res
	in aux [] sol "";;

