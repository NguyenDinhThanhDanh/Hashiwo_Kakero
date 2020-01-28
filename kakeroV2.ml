
module ListExtension =
    struct
        (* replace la n ième valeur de l par new_val et retourne un couple
        composé de la nouvelle liste ainsi formées et de l'ancienne valeur
        se trouvant a la place de new_val*)
        let replace = fun l n new_val ->
            let rec aux = fun i l_begin l_end ->
                match l_end with
                | h::t -> 
                        if i = n then (l_begin@new_val::t)
                        else aux (i+1) (l_begin@[h]) t
                | [] -> failwith "out of bounds"
            in aux 0 [] l;;
            
        (* divise l en deux parties et renvoie le resultat sous forme de couple
        le 1er élément est la sous liste de l composé de tout les élements dont
        l'indice est inférieur a i exclu, le 2ème element est le reste de la 
        liste*)
        let rec isplit = fun l i ->
            match l with 
            | h::t -> if i <= 0 then ([],l) 
                    else let p = isplit t (i-1) in (h::fst p,snd p)
            | [] -> ([],[])

        (* fold left en recursif terminal *)
        let tail_fold_left = fun op neutral l ->
            let rec aux = fun l res ->
                match l with
                |[] -> res
                |h::t -> aux t (op res h)
            in aux l neutral

        (* donne l'indice de la 1ere occurence satisfaisant p *)
        let index_of = fun p l ->
            let rec aux = fun l i ->
                match l with
                | h::t -> if p h then i else aux t (i+1)
                | [] -> -1
            in aux l 0
    end;;

module Matrix = 
    struct
        type 'a matrix = 'a list list

        let get_elt = fun mat (x,y) -> List.nth (List.nth mat x) y

        let size = fun mat -> (List.length mat,List.length (List.hd mat))
    
        let for_all = fun op mat -> List.for_all (List.for_all op) mat

        let tail_fold_left = fun op neutral mat -> 
            ListExtension.tail_fold_left (fun res col -> ListExtension.tail_fold_left op res col) neutral mat

        let coords_of = fun p mat ->
            let rec aux = fun mat x ->
                match mat with
                |h::t -> let y = ListExtension.index_of p h in 
                        if y <> -1 then Some (x,y)
                        else aux t (x+1)
                | [] -> None
            in aux mat 0
         
    end;;

module Coordinates =
    struct
        type coord = int * int;;

        let coord_comp: coord -> coord -> int = fun (x,y) (u,v) ->  
            if x > u || (x = u && y > v) then 1 
            else if x=u && y = v then 0 
            else -1
    end;;
    
open Coordinates;;

module Puzzle = 
    struct
        open Coordinates

        type imp = int
        type puzzle = (coord * imp) list

        let puzzle_comp: coord * 'a -> coord * 'a -> int = fun (c1,_) (c2,_) ->  coord_comp c1 c2 

        let sort_puzzle = List.sort puzzle_comp

    end;;
    
open Puzzle;;

module Solver =
    struct
        open Puzzle
        open List
        open ListExtension
        
        exception Unsolvable_puzzle
        
        type bridge = {isVertical : bool; isDoubled : bool}
        type solver_cell = SNothing | SIsland of imp * imp | SBridge of bridge
        type solver_puzzle = solver_cell list list 
        
        type cell = Nothing | Island of imp | Bridge of bridge
        type solution = cell list list
        
        
        (* conversions de type puzzle en type solver_puzzle *)
        let solver_puzzle_of_puzzle: puzzle -> solver_puzzle = fun puz ->
            let puz = sort_puzzle puz
            in
            let (mx, my) = tail_fold_left (fun (x,y) ((u,v), _) -> (max x u, max y v)) (1,1) puz  
            in
            let rec aux = fun (x,y) p col res ->
                let (c,rem) = 
                    match p with
                    |((u,v), imp)::t -> 
                        if u = x && v = y then (SIsland (imp,imp), t)
                        else if u < 1 || v < 1 then failwith "coordinates lower than 1 in puz"
                        else (SNothing,p)
                    |[] -> (SNothing,[])
                in
                if x = mx && y = my then res@[col@[c]]
                else if y = my then aux (x+1,1) rem [] (res@[col@[c]])
                else aux (x,y+1) rem (col@[c]) res

            in aux (1,1) puz [] []
        
        (* conversion de type solver_puzzle en type solution *)
        let solution_of_solver_puzzle : solver_puzzle -> solution =
            let aux = fun s_cell ->
                match s_cell with
                | SIsland (init_imp,_) -> Island init_imp
                | SBridge b -> Bridge b
                | SNothing -> Nothing
            in
           map (map aux)
        
        (* récupération des voisins *)
        let get_neighbors: solver_puzzle -> coord -> (coord * (imp * imp)) list = fun sol (x,y) ->
            let rec west_island = fun sol u res ->
                match sol with
                |h::t ->
                    if u < x then
                        match (try(nth h (y-1)) with _ -> failwith "coordinates out of bounds") with
                        | SNothing -> west_island t (u+1) res
                        | SIsland (init_imp,imp) when imp <> 0 -> west_island t (u+1) [((u,y), (init_imp,imp))]
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
                        | SIsland (init_imp,imp) when imp <> 0 -> north_island t (v+1) [((x,v), (init_imp,imp))]
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
                        | SIsland (init_imp,imp) when imp <> 0 -> [((x,v), (init_imp,imp))]
                        | _ -> []
                    end
                | [] -> []

            in 
            let rec east_island = fun sol u ->
                match sol with
                | h::t -> 
                    begin
                        match (try(nth h (y-1)) with _ -> failwith "coordinates out of bounds") with
                            | SNothing -> east_island t (u+1)
                            | SIsland (init_imp,imp) when imp <> 0 -> [((u,y), (init_imp,imp))]
                            | _ -> []
                    end
                | [] -> []
            in

            if x < 1 || y < 1 then failwith "coordinates out of bounds"
            else
                let (west, rem) = west_island sol 1 [] in
                let (north, col_rem) = north_island (List.hd rem) 1 [] in

                match List.hd col_rem with
                | SIsland _ -> 
                    let south = south_island (List.tl col_rem) (y+1) in
                    let east = east_island (List.tl rem) (x+1) in

                    west @ north @ south @ east

                | _ -> failwith ("There is no island at (" ^ string_of_int x ^ "," ^ string_of_int y ^ ")")
        
         
        
        (*simplification possible avec isplit des conditions avec x_begin et x_end*)
        (* construction de ponts entre deux iles*)
        let build_bridge: solver_puzzle -> coord -> coord -> bool -> solver_puzzle = fun sol (x,y) (u,v) doubled ->
            let bridge = if x = u then {isVertical=true; isDoubled=doubled}
                         else if y = v then {isVertical=false; isDoubled=doubled}
                         else failwith "invalid coordinates"

            in
            let build_horizontal_bridge = fun x_begin x_end ->
                let update_island_imp = fun column (w,z) ->
                    match isplit column (y-1) with 
                    | (b,SIsland (init_imp,imp)::e) -> 
                        let new_imp = (imp - if doubled then 2 else 1) in
                        if new_imp < 0 then failwith ("insufficient island importance at (" ^ string_of_int w ^ "," ^ string_of_int z ^ ")")
                        else b @ SIsland (init_imp,new_imp) ::e
                    | _ -> failwith ("There is no island at (" ^ string_of_int w ^ "," ^ string_of_int z ^ ")")
                in
                let rec aux = fun s i res ->
                    match s with
                    | h::t -> 
                        if i < x_begin then aux t (i+1) (res@[h])
                        else if i = x_begin then aux t (i+1) (res@[update_island_imp h (x_begin,y)])
                        else if i < x_end then 
                            aux t (i+1) (res@ try([replace h (y-1) (SBridge bridge)])
                                with _ -> failwith "start or end y coordinates out of bounds")   
                        else res@ update_island_imp h (x_end,y) :: t

                    | [] -> failwith "start or end x coordinates out of bounds"
                in
                if x_begin < 1 then failwith "start or end x coordinates out of bounds"
                else aux sol 1 []

            in
            let build_vertical_bridge = fun y_begin y_end ->
                let update_island_imp = fun island (w,z) ->
                    match island with 
                    | SIsland (init_imp,imp) ->
                        let new_imp = (imp - if doubled then 2 else 1) in
                        if new_imp < 0 then failwith ("insufficient island importance at (" ^ string_of_int w ^ "," ^ string_of_int z ^ ")")
                        else SIsland (init_imp,new_imp)
                    | _ -> failwith ("There is no island at (" ^ string_of_int w ^ "," ^ string_of_int z ^ ")")

                in
                let rec aux = fun col j res ->
                    match col with
                    | h::t -> 
                        if j < y_begin then aux t (j+1) (res@[h])
                        else if j = y_begin then aux t (j+1) (res@[update_island_imp h (x,y_begin)])
                        else if j < y_end then aux t (j+1) (res@[SBridge bridge])
                        else res@update_island_imp h (x,y_end)::t
                    | [] -> failwith "start or end y coordinates out of bounds"

                in
                if y_begin < 1 then failwith "start or end y coordinates out of bounds"
                else
                    match isplit sol (x-1) with
                    | (b,col::e) -> b@ aux col 1 [] :: e
                    | _ -> failwith "start or end x coordinates out of bounds"


            in
            if (x,y) = (u,v) then failwith "Can't build a bridge to the same island"
            else if bridge.isVertical then build_vertical_bridge (min y v) (max y v)
            else build_horizontal_bridge (min x u) (max x u)
        
        (* teste la connexité du réseau de ponts *)
        let is_connected = fun sol -> true



        (* permet de verifier si le puzzle est résolu *)
        let is_solved: solver_puzzle -> bool = fun sol ->
            let verify_cell = fun c -> 
                match c with
                | SIsland (_,imp) when imp > 0 -> false
                | _ -> true
            in
            Matrix.for_all verify_cell sol

        

        
        (* ASTUCE 1 *)
        let apply_tip_one: solver_puzzle -> solver_puzzle * bool = fun puz ->
            let (mx, my) = Matrix.size puz
            
            in
            let max_briges = fun island_imp nei_list ->
                tail_fold_left (fun res (_, (_,i)) -> (min (min 2 i) island_imp) + res) 0 nei_list

            in
            let connect_with_neighbors = fun puz coord imp nei_list ->
                tail_fold_left (fun p (c, (_,i)) -> build_bridge p coord c (min (min 2 i) imp = 2)) puz nei_list

            in
            let rec apply_once = fun puz (x,y) isStuck ->
                if (x,y) = (mx+1,1) then (puz,isStuck)
                else 
                    let next_coord = if (y = my) then (x+1,1) else (x,y+1) in

                    match Matrix.get_elt puz (x-1,y-1) with
                      | SIsland (_,imp) when imp <> 0 ->
                        let nei = get_neighbors puz (x,y) in
                        let mb = max_briges imp nei in

                        if mb < imp then raise Unsolvable_puzzle
                        else if mb = imp then apply_once (connect_with_neighbors puz (x,y) imp nei) next_coord false
                        else apply_once puz next_coord isStuck
                      | _ -> apply_once puz next_coord isStuck

            in
            let rec aux = fun puz ->
                let (p,s) = apply_once puz (1,1) true in
                if s then p 
                else aux p

            in
            let p = aux puz in
            if is_solved p then 
                if is_connected p then (p,true)
                else raise Unsolvable_puzzle
            else (p, false)
         
        
         
        
        (* ASTUCE 2 *)
        let apply_tip_two: solver_puzzle -> solver_puzzle * bool = fun sol ->
            let rec test_sol = fun (x,y) neighbors ->
                match neighbors with
                | h::t -> 
                    begin
                        let new_sol = build_bridge sol (x,y) (fst (hd neighbors)) false
                        in
                        try(apply_tip_one new_sol)
                        with Unsolvable_puzzle -> test_sol (x,y) t
                    end
                | [] -> raise Unsolvable_puzzle
            in
            let rec search_in_column = fun col y ->
                match col with
                | SIsland (_,imp)::t when imp = 1 -> Some y
                | h::t -> search_in_column t (y+1)
                | [] -> None
            in
            let rec aux = fun s x->
                match s with
                | h::t ->
                    begin
                        match search_in_column h 1 with
                        | Some y -> 
                            let nei = get_neighbors sol (x,y) in
                            if length nei = 2 then 
                                try(test_sol (x,y) nei) 
                                with Unsolvable_puzzle -> aux t (x+1)
                            else aux t (x+1)
                        | None -> aux t (x+1)
                    end
                | [] -> failwith "tip 2 can't be applied"

            in aux sol 1




        (* Resolution de puzzle *)
        let solve: puzzle -> solution = fun puz ->
            let rec aux = fun p solved ->
                if solved then p
                else let (p,solved) = apply_tip_two p in aux p solved
            in
            let (p,solved) = apply_tip_one (solver_puzzle_of_puzzle puz) in
            solution_of_solver_puzzle (aux p solved)





        (**)
        let string_of_cell = fun c ->
            match c with
            |Nothing -> " "
            |Island i -> string_of_int i
            |Bridge {isVertical=true; isDoubled=true} -> "H"
            |Bridge {isVertical=true; isDoubled=false} -> "|"
            |Bridge {isVertical=false; isDoubled=true} -> "="
            |Bridge {isVertical=false; isDoubled=false} -> "-"




        (**)
        let string_of_solution:solution -> string = fun sol ->
            let rec aux = fun sol_begin sol_end res ->
                match (sol_begin ,sol_end) with
                | (b,(yh::yt)::xt) -> aux (b@[yt]) xt (res ^ string_of_cell yh)
                | (b,[]) -> aux [] b (res^"\n")
                | (_, []::_) -> res
            in aux [] sol ""
  
    end;;

let puzzle_one:puzzle = [((3,1), 2); ((1,3), 3);((3,3), 8);((5,3), 4);
    ((1,5), 3);((3,5), 5);((5,5), 3)];;
            
let puzzle_two:puzzle = [((1,1), 4);((4,1), 4);((7,1), 3);((2,3), 1);
    ((4,3), 4);((6,3), 2);((1,4), 4);((7,4), 5);((1,6), 2);((6,6), 1);
    ((3,7), 1);((5,7), 3);((7,7), 4)];;
            
let puzzle_six:puzzle = [((1,1), 4);((3,1), 4);((6,1), 2);((9,1), 3);
    ((1,3), 6);((3,3), 8);((5,3), 4);((8,3), 1);((7,4), 1);((9,4), 3);
    ((3,5), 2);((5,5), 2);((8,5), 1);((1,6), 4);((4,6), 3);((6,6), 2);
    ((7,7), 2);((9,7), 3);((2,8), 1);((4,8), 5);((6,8), 4);((1,9), 3);
    ((3,9), 3);((5,9), 2);((7,9), 3);((9,9), 2)];;  

open Solver;;
solve puzzle_one;;
solve puzzle_two;;
solve puzzle_six;;





