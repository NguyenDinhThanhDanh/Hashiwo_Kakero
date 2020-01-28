open Coordinates;;
open List;;
open ListExtension;;

exception Unsolvable_puzzle;;
exception Insufficient_island_importance;;

type imp = int;;
type puzzle = (coord * imp) list;;
        
type bridge = {isVertical : bool; isDoubled : bool};;
type cell = Nothing | Island of imp | Bridge of bridge;;
type solution = cell list list;;
        
type solver_cell = SNothing | SIsland of imp * imp | SBridge of bridge;;
type solver_puzzle = solver_cell list list ;;



(* val solver_puzzle_of_puzzle : puzzle -> solver_puzzle *)
let solver_puzzle_of_puzzle: puzzle -> solver_puzzle = fun puz ->
	Matrix.matrix_of_coord_list (SNothing) (map (fun (c,imp) -> (c,SIsland (imp,imp))) puz);;

(* val solution_of_solver_puzzle : solver_puzzle -> solution *)
let solution_of_solver_puzzle : solver_puzzle -> solution =
	let aux = fun s_cell ->
		match s_cell with
		| SIsland (init_imp,_) -> Island init_imp
		| SBridge b -> Bridge b
		| SNothing -> Nothing
	in map (map aux);;

(* val string_of_cell : cell -> string *)
let string_of_cell = fun c ->
	match c with
	|Nothing -> " "
	|Island i -> string_of_int i
	|Bridge {isVertical=true; isDoubled=true} -> "H"
	|Bridge {isVertical=true; isDoubled=false} -> "|"
 	|Bridge {isVertical=false; isDoubled=true} -> "="
	|Bridge {isVertical=false; isDoubled=false} -> "-";;




(* val string_of_solution : solution -> string *)
let string_of_solution:solution -> string = Matrix.string_of_matrix string_of_cell;;



(* val get_neighbors :
	solver_puzzle -> Coordinates.coord -> (Coordinates.coord * imp) list *)
let get_neighbors: solver_puzzle -> coord -> (coord * imp) list = fun s_puz (x,y) ->
	let rec west_island = fun s_puz u res ->
		match s_puz with
		|h::t ->
			if u < x then
				match (try(nth h (y-1)) with _ -> failwith "coordinates out of bounds") with
				| SNothing -> west_island t (u+1) res
				| SIsland (_,imp) when imp <> 0 -> west_island t (u+1) [((u,y),imp)]
				| _ -> west_island t (u+1) []
			else (res,h::t)
		|[] -> failwith "coordinates out of bounds"

		in
		let rec north_island = fun col v res ->
			match col with
			| h::t -> 
				if v < y then
					match h with
					| SNothing -> north_island t (v+1) res
					| SIsland (init_imp,imp) when imp <> 0 -> north_island t (v+1) [((x,v),imp)]
					| _ -> north_island t (v+1) []
				else (res, h::t)
			| [] -> failwith "coordinates out of bounds"

		in
		let rec south_island = fun col v ->
			match col with
			| h::t -> 
				begin
					match h with
					| SNothing -> south_island t (v+1) 
					| SIsland (init_imp,imp) when imp <> 0 -> [((x,v),imp)]
					| _ -> []
				end
			| [] -> []

		in 
		let rec east_island = fun s_puz u ->
			match s_puz with
			| h::t -> 
				begin
					match (try(nth h (y-1)) with _ -> failwith "coordinates out of bounds") with
					| SNothing -> east_island t (u+1)
					| SIsland (init_imp,imp) when imp <> 0 -> [((u,y),imp)]
					| _ -> []
				end
			| [] -> []

		in
		if x < 1 || y < 1 then failwith "coordinates out of bounds"
		else
			let (west, rem) = west_island s_puz 1 [] in
			let (north, col_rem) = north_island (List.hd rem) 1 [] in

			match List.hd col_rem with
			| SIsland _ -> 
				let south = south_island (List.tl col_rem) (y+1) in
				let east = east_island (List.tl rem) (x+1) in

				west @ north @ south @ east

			| _ -> failwith ("There is no island at (" ^ string_of_int x ^ "," ^ string_of_int y ^ ")");;



(*val build_bridge :
	solver_puzzle ->
	Coordinates.coord -> Coordinates.coord -> bool -> solver_puzzle*)
let build_bridge: solver_puzzle -> coord -> coord -> bool -> solver_puzzle = fun s_puz (x,y) (u,v) is_doubled ->
	let bridge = if x = u then {isVertical=true; isDoubled=is_doubled}
				 else if y = v then {isVertical=false; isDoubled=is_doubled}
				 else failwith "invalid coordinates"

	in
	let update_island_imp = fun island (w,z) -> (*sert a verifier si l'importance résiduelle de l'ile est suffisante*)
			match island with 
			| SIsland (init_imp,imp) ->
				let new_imp = (imp - if is_doubled then 2 else 1) in
				if new_imp < 0 then raise Insufficient_island_importance
				else SIsland (init_imp,new_imp)
			| _ -> failwith ("There is no island at (" ^ string_of_int w ^ "," ^ string_of_int z ^ ")")

	in
	let build = fun c -> (*sert a verifier si il n'y a rien a l'endroit ou l'on veut construire le pont*)
					match c with 
					| SNothing -> SBridge bridge
					| _ -> failwith "an island or a bridge already exists on bridge path"
	in
	let build_horizontal_bridge = fun x_begin x_end ->
		let update_island_imp = fun x -> try(apply_to_nth (fun island -> update_island_imp island (x,y)) (y-1))
										 with _ -> failwith "start or end y coordinates out of bounds"
		in
		let rec aux = fun s i res ->
			match s with
			| h::t ->
				if i < x_begin then aux t (i+1) (res@[h])
				else if i = x_begin then aux t (i+1) (res@[update_island_imp x_begin h])
				else if i < x_end then 
					aux t (i+1) (res@ try([apply_to_nth build (y-1) h])
									  with _ -> failwith "start or end y coordinates out of bounds")  
				else res@ update_island_imp x_end h :: t

			| [] -> failwith "start or end x coordinates out of bounds"
		in
		if x_begin < 1 then failwith "start or end x coordinates out of bounds"
		else aux s_puz 1 []

	in
	let build_vertical_bridge = fun y_begin y_end ->
		let rec aux = fun col j res ->
			match col with
			| h::t -> 
				if j < y_begin then aux t (j+1) (res@[h])
				else if j = y_begin then aux t (j+1) (res@[update_island_imp h (x,y_begin)])
				else if j < y_end then aux t (j+1) (res@[build h])
				else res@update_island_imp h (x,y_end)::t
			| [] -> failwith "start or end y coordinates out of bounds"

		in
		if y_begin < 1 then failwith "start or end y coordinates out of bounds"
		else
			match isplit (x-1) s_puz with
			| (b,col::e) -> b@ aux col 1 [] :: e
			| _ -> failwith "start or end x coordinates out of bounds"

	in
	if (x,y) = (u,v) then failwith "Can't build a bridge to the same island"
	else if bridge.isVertical then build_vertical_bridge (min y v) (max y v)
	else build_horizontal_bridge (min x u) (max x u);;



(* val nb_of_island : solver_puzzle -> int *)
let nb_of_island: solver_puzzle -> int =
	Matrix.tail_fold_left (fun r i -> match i with SIsland _ -> r+1 | _ -> r) 0;;



(* val is_connected : solver_puzzle -> bool *)
let is_connected: solver_puzzle -> bool = fun s_puz ->
	let destroy_island = fun coord increment isXAxis s_puz coord_list ->
		let next_coord = fun (u,v) -> if isXAxis then (u+increment,v) else (u,v+increment) in
		let rec aux = fun c ->
			match Matrix.get_elt c s_puz with
			| SBridge _ -> aux (next_coord c)
			| SIsland _ -> (Matrix.apply_to_coord (fun _ -> SNothing) c s_puz,c::coord_list)
			| SNothing -> (s_puz, coord_list)
		in
		let nc = next_coord coord in
		try(
			match Matrix.get_elt nc s_puz with
			| SBridge b when (b.isVertical && not isXAxis) || (not b.isVertical && isXAxis) ->
				aux (next_coord nc)
			| _ -> (s_puz, coord_list)
		) with _ -> (s_puz, coord_list)

	in
	let rec aux = fun s_puz coord_list ->
		match coord_list with
		| c::t ->
			let (s,l) = destroy_island c (-1) true s_puz t in (* on détruit le voisin connecté par un pont a l'ouest ainsi que les voisins connectés de ce voisin, etc *)
			let (s,l) = destroy_island c 1 true s l in (* même chose pour le voisin a l'est *)
			let (s,l) = destroy_island c (-1) false s l in (* même chose pour le voisin au nord *)
			let (s,l) = destroy_island c 1 false s l in (* même chose pour le voisin au sud *)
			aux s l
		| [] -> s_puz

	in
	match Matrix.coords_of (fun x -> match x with SIsland _ -> true | _ -> false) s_puz with
	| Some coord -> nb_of_island (aux s_puz [coord]) = 0 (*si il ne reste plus aucunne ile, le réseau est connexe*)
	| None -> true;;



(* val is_solved : solver_puzzle -> bool *)
let is_solved: solver_puzzle -> bool =
	Matrix.for_all (fun c -> 
					match c with
					| SIsland (_,imp) when imp > 0 -> false
					| _ -> true);;




(* val apply_tip_one : solver_puzzle -> solver_puzzle *)
let apply_tip_one: solver_puzzle -> solver_puzzle = fun s_puz ->
	let (mx, my) = Matrix.size s_puz

	in
	let max_briges = fun island_imp nei_list -> (* permet de connaitre le nombre max de ponts constructibles entre une ile et ses voisins *)
                tail_fold_left (fun res (_,i) -> (min (min 2 i) island_imp) + res) 0 nei_list

	in
	let connect_with_neighbors = fun s_puz coord imp nei_list ->
		tail_fold_left (fun p (c,i) -> build_bridge p coord c (min (min 2 i) imp = 2)) s_puz nei_list

	in
	let rec apply_once = fun s_puz (x,y) isStuck ->
		if (x,y) = (mx+1,1) then (s_puz,isStuck)
		else 
			let next_coord = if (y = my) then (x+1,1) else (x,y+1) in

			match Matrix.get_elt (x,y) s_puz  with
			| SIsland (_,imp) when imp <> 0 ->
				let nei = get_neighbors s_puz (x,y) in
				let mb = max_briges imp nei in

				if mb < imp then raise Unsolvable_puzzle
				else if mb = imp then apply_once (connect_with_neighbors s_puz (x,y) imp nei) next_coord false
				else apply_once s_puz next_coord isStuck
			| _ -> apply_once s_puz next_coord isStuck

	in
	let rec aux = fun s_puz ->
		let (p,s) = apply_once s_puz (1,1) true in
		if s then p 
		else aux p

	in aux s_puz;;




(* val apply_tip_two : solver_puzzle -> solver_puzzle *)
let apply_tip_two: solver_puzzle -> solver_puzzle = fun s_puz ->
	let rec test_sol = fun s_puz coord neighbors is_doubled_bridge ->
		match neighbors with
		| h::t -> 
			begin
				(* on essaye un solution en constuisant un pont vers un des voisins de l'ile choisie *)
				try(
					let new_sol = build_bridge s_puz coord (fst (hd neighbors)) is_doubled_bridge in
				
					let p = apply_tip_one new_sol in (* on verifie que l'on a la bonne solution en poursuivant la résolution *)
					if is_solved p then 
						if is_connected p then p
						else raise Unsolvable_puzzle
					else apply_once p 1 false (* si l'application de l'astuce 1 bloque, on réaplique l'astuce 2*)
				(* si la solution ne marche pas avec le voisin choisit, on essaye avec un autre voisin *)
				) with Unsolvable_puzzle | Insufficient_island_importance -> test_sol s_puz coord t is_doubled_bridge
			end
		| [] -> raise Unsolvable_puzzle

	and apply_once = fun s_puz tested_imp use_double_brige ->
		let rec aux = fun coords_list -> (*choisit l'ile a partir de laquelle on va construire un pont*)
			match coords_list with
			| coord::t ->
				let nei = get_neighbors s_puz coord in
				if length nei = 2 || t = [] then test_sol s_puz coord nei use_double_brige
				else aux t
			| [] -> if use_double_brige || tested_imp = 1 then apply_once s_puz (tested_imp+1) false 
					else apply_once s_puz tested_imp true

		in aux (Matrix.all_coords_of 
			(fun i -> match i with SIsland (_,imp) when imp = tested_imp -> true | _ -> false) s_puz)

	in apply_once s_puz 1 false;;




(* val solve : puzzle -> solution *)
let solve: puzzle -> solution = fun puz ->
	let p = apply_tip_one (solver_puzzle_of_puzzle puz) in
	let p = if is_solved p then 
				if is_connected p then p
				else raise Unsolvable_puzzle
			else apply_tip_two p
	in solution_of_solver_puzzle p;;
