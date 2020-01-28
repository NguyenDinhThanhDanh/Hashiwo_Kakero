type coord = int * int;;

(* val coord_comp : coord -> coord -> int *)
let coord_comp: coord -> coord -> int = fun (x,y) (u,v) ->  
	if x > u || (x = u && y > v) then 1 
	else if x=u && y = v then 0 
	else -1;;
