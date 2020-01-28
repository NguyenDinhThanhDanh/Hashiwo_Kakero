
module type KAKERO = 
    sig
        type coord
        type imp
        type puzzle

        type bridge
        type cell
        type solution

        type cell_puzzle

        val cell_puzzle_of_puzzle : puzzle -> cell_puzzle
    
        (* convertis un type puzzle en type solution*)
        val solution_of_cell_puzzle : puzzle -> solution
        
        (* retourne ce qui se trouve aux coordonées données en paramètre
        dans le puzzle donné en paramètre*)
        val get : puzzle -> coord -> cell 
        
        (*récupération de l'importance voisins d'un île, Nothing si pas de voisin*)
        val north_island :  puzzle -> coord -> int
        val south_island :  puzzle -> coord -> int
        val east_island :  puzzle -> coord -> int
        val west_island :  puzzle -> coord -> int
        
        (*création d'un pont entre deux îles*)
        val build_bridge :  puzzle -> coord -> coord -> bool -> puzzle
        
        (* applique l'astuce 1 pour résoudre le puzzle *)
        val apply_tip_one : puzzle -> solution
        
        (* applique l'astuce  pour résoudre le puzzle*)
        val apply_tip_two : puzzle -> solution
    end;;

type coord = int * int;;
type imp = int;;
type puzzle = (coord * imp) list;;

type bridge = {isVertical : bool; isDoubled : bool};;
type cell = Nothing | Island of imp | Bridge of bridge;;
type solution = cell list list;;

type cell_puzzle = (coord * cell) list;;

let reduc = fun op neutral list ->
    let rec aux = fun l res ->
        match l with
        |[] -> res
        |h::t -> aux t (op res h)
    in aux list neutral;;

let coord_comp = fun ((x,y),_) ((u,v),_) ->  
    if y > v || (y = v && x > u) then 1 
    else if x=u && y = v then 0 
    else -1;;

let sort_puzzle = List.sort coord_comp;;

let (cell_puzzle_of_puzzle: puzzle -> cell_puzzle) = fun puz -> 
    let rec aux = fun p res ->
        match p with
        |[] -> res
        |(coord, i)::t -> aux t (res@[(coord, Island i)])
    in aux puz [];;

let north_island:cell_puzzle -> coord -> (coord * cell) option = fun list (x,y) ->
        let rec aux = fun list res ->
            match list with 
            | ((_,v),_)::_ when v >= y -> res
            |((u,v),Island i)::t when x=u ->  aux t (Some ((u,v),Island i))
            |((u,v),Bridge _)::t when x=u -> aux t None  
            |_::t -> aux t res
            | [] -> None
        in aux list None;;

let south_island:cell_puzzle -> coord -> (coord * cell) option = fun list (x,y)->
        let rec aux = fun list ->
            match list with
            |((u,v),Island i)::t when x=u && v>y  -> (Some ((u,v),Island i))
            |((u,v),Bridge _)::t when x=u && v>y  ->  None
            |_::t -> aux t
            |[]-> None
        in aux list;;

    let west_island:cell_puzzle -> coord -> (coord * cell) option = fun list(x,y)->
        let rec aux = fun list res ->
            match list with 
            |((_,v),_)::t when v <> y -> aux t res 
            |((u,_),_)::_ when u >= x -> res
            |((u,v),Island i)::t -> aux t (Some ((u,v),Island i))
            |((u,v),Bridge _)::t -> aux t None 
            |_::t -> aux t res
            |[] -> None
        in aux list None;;

  let east_island:cell_puzzle -> coord -> (coord * cell) option = fun list (x,y)->
        let rec aux = fun list ->
            match list with 
            |((_,v),_)::t when v <> y -> aux t
            |((u,_),_)::t when u <= x -> aux t
            |((u,v),Island i)::t -> (Some ((u,v), Island i))
            |((u,v),Bridge _)::t -> None
            |_::t -> aux t
            |[] -> None
        in aux list;;

let build_bridge:cell_puzzle -> coord -> coord -> bool -> cell_puzzle = fun c_puzzle depature arrival doubled ->
    let bridge = if fst depature = fst arrival then {isVertical=true; isDoubled=doubled}
                else if snd depature = snd arrival then {isVertical=false; isDoubled=doubled}
                else failwith "coordonnées incorrectes"
    in
    let increment = 
        let diff = if bridge.isVertical then snd depature - snd arrival
                else fst depature - fst arrival 
        in 
        if (diff > 1) then -1
        else if diff < -1 then 1
        else failwith "coordonnées invalides"
                    
    in
    let rec aux = fun curr_coord bridge_list->
            let new_coord = if bridge.isVertical then (fst curr_coord, (snd curr_coord)+increment)
                            else((fst curr_coord)+increment, snd curr_coord)
            in
            if curr_coord = arrival then bridge_list
            else if increment < 0 then aux new_coord ((curr_coord,Bridge bridge)::bridge_list)
            else aux new_coord (bridge_list@[(curr_coord,Bridge bridge)])
    in
    let nb_of_bridge = if doubled then 2 else 1
    in
    let imp_depature = match List.assoc depature c_puzzle with Island imp -> imp
    and
    imp_arrival = match List.assoc arrival c_puzzle with Island imp -> imp
    in
    let c_puzzle = List.remove_assoc arrival (List.remove_assoc depature c_puzzle)
    
    in
    let bridge_list = (if bridge.isVertical then aux (fst depature, (snd depature)+increment) []
        else aux ((fst depature)+increment, snd depature) [])
        
    in
    List.merge coord_comp c_puzzle 
        (if increment < 0 then ((arrival,Island (imp_arrival-nb_of_bridge))::bridge_list@[(depature,Island (imp_depature-nb_of_bridge))])
        else ((depature,Island (imp_depature-nb_of_bridge))::bridge_list@[(arrival,Island (imp_arrival-nb_of_bridge))]))
        ;;

exception Invalid_solution;;

let apply_tip_one: cell_puzzle -> cell_puzzle = fun cell_p ->
    let pop_last = fun l ->
        let rec aux = fun l res ->
            match l with
            |h::t -> if t = [] then res else aux t (res@[h])
            |[] -> res
        in aux l []
    in
    let max_brige = fun island_imp neighbor ->
        match neighbor with
        | None -> 0
        | Some (_,Island i) -> min (min 2 i) island_imp
        | _ -> failwith "le voisin n'est pas une ile"
    in
    let build_aux = fun puz coord island_imp arrival_island ->
        match arrival_island with
        | None -> puz
        | Some a -> build_bridge puz coord (fst a) (max_brige island_imp (Some a) = 2)
    
    in
    let rec aux = fun p_begin p_end ->
        match p_end with
        | [] -> p_begin
        | (coord,Island imp)::t ->
            begin
                let p_begin = p_begin@[(coord,Island imp)] in
                if imp = 0 then aux p_begin t
                else
                    let north = north_island p_begin coord
                    and south = south_island p_end coord
                    and east = east_island p_end coord
                    and west = west_island p_begin coord

                    in
                    let mb = max_brige imp north
                        + max_brige imp south 
                        + max_brige imp east 
                        + max_brige imp west
                        
                    in
                    if mb < imp then raise Invalid_solution
                    else if mb = imp then
                        let p_begin = (build_aux (build_aux p_begin coord imp north) coord imp west)
                        and p_end = (build_aux (build_aux p_end coord imp south) coord imp east)
                        in
                        aux ((pop_last p_begin)@[((coord),Island (imp-mb))]) (List.tl p_end)
                    else aux p_begin t
            end
        | h::t -> aux (p_begin@[h]) t
    
    in aux [] cell_p;;

let apply_tip_two: cell_puzzle -> cell_puzzle = fun cell_p ->
    let test_solutions = fun island_coord ->
        let rec aux = fun neighbor ->
            match neighbor with
            |(coord, _)::t -> 
                begin
                    let new_p = build_bridge cell_p island_coord coord false in
                    try (apply_tip_one new_p)
                    with Invalid_solution -> aux t
                end
            | [] -> raise Invalid_solution
        in
        let nei = [north_island cell_p island_coord;
        south_island cell_p island_coord;
        east_island cell_p island_coord;
        west_island cell_p island_coord]
        in
        aux (List.map (fun elt -> match elt with | Some e -> e) 
                (List.filter (fun n -> match n with | Some (_,Island _) -> true | _ -> false) nei))
         
    in
    let rec aux = fun p ->
        match p with
        |((x,y), Island imp)::t -> 
            if imp = 1 then test_solutions (x,y)
            else aux t
        | _::t -> aux t
        |[] -> failwith "l'astuce 2 n'est pas appliquable"
    in aux cell_p;;

let (cell_puzzle_to_sol: cell_puzzle -> solution) = fun p ->
    
    let (mx, my) = reduc (fun (x,y) ((u,v), _) -> (max x u, max y v)) (1,1) p
    
    in
    let rec aux = fun (x,y) p col res ->
        let (w,rem) = 
            match p with
            |[] -> (Nothing,[])
            |((u,v), i)::t ->  (i,t)
        in
        if x = mx && y = my then res@[col@[w]]
        else if y = my then aux (1,y+1) rem [] (res@[col@[w]])
        else aux (x+1,y) rem (col@[w]) res
    
    in aux (1,1) p [] [];;

let string_of_cell = fun c ->
        match c with
        |Nothing -> "  "
        |Island i -> string_of_int i ^ "  "
        |Bridge {isVertical=true; isDoubled=true} -> "|| "
        |Bridge {isVertical=true; isDoubled=false} -> "|  "
        |Bridge {isVertical=false; isDoubled=true} -> "==="
        |Bridge {isVertical=false; isDoubled=false} -> "---";;

(*A coriger*)
let string_of_sol = fun p ->
    print_string 
    (reduc 
        (fun r l -> 
            r ^ (reduc (fun r e -> r ^ string_of_cell e) "" l) ^ "\n")
        "" p
    );;

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

























