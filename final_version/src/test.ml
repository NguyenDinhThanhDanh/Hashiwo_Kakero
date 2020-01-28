open HKakero;;

let p_one:puzzle = [((3,1), 2); ((1,3), 3);((3,3), 8);((5,3), 4);
    ((1,5), 3);((3,5), 5);((5,5), 3)];;
            
let p_two:puzzle = [((1,1), 4);((4,1), 4);((7,1), 3);((2,3), 1);
    ((4,3), 4);((6,3), 2);((1,4), 4);((7,4), 5);((1,6), 2);((6,6), 1);
    ((3,7), 1);((5,7), 3);((7,7), 4)];;

let p_three:puzzle = [((3,1), 1);((5,1), 3);((7,1), 1);((1,2),2);
    ((6,2), 1);((3,3), 4);((5,3), 5);((1,4), 4);((5,6), 1);((1,7), 3);
    ((3,7), 3);((6,7), 2)]

let p_four:puzzle = [((1,1), 2);((3,1), 3);((5,1), 1);((7,1), 1);
	((2,2), 2);((4,2), 1);((3,4), 1);((2,5), 3);((4,5), 5);((7,5), 2);
    ((1,7), 2);((4,7), 4);((6,7), 1)]

let p_five:puzzle = [((2,1), 2);((4,1), 6);((7,1), 3);((1,3), 1);
	((4,3), 6);((6,3), 2);((5,4), 1);((7,4), 3);((1,5), 1);((5,6), 1);
    ((7,6), 2);((1,7), 3);((4,7), 5);((6,7), 2)]
            
let p_six:puzzle = [((1,1), 4);((3,1), 4);((6,1), 2);((9,1), 3);
    ((1,3), 6);((3,3), 8);((5,3), 4);((8,3), 1);((7,4), 1);((9,4), 3);
    ((3,5), 2);((5,5), 2);((8,5), 1);((1,6), 4);((4,6), 3);((6,6), 2);
    ((7,7), 2);((9,7), 3);((2,8), 1);((4,8), 5);((6,8), 4);((1,9), 3);
    ((3,9), 3);((5,9), 2);((7,9), 3);((9,9), 2)];;

let sol_one = solve p_one;;
let sol_two = solve p_two;;
let sol_three = solve p_three;;
let sol_four = solve p_four;;
let sol_five = solve p_five;;
let sol_six = solve p_six;;

print_string "########### Puzzle 1 ###########\n";;
print_string (string_of_solution sol_one ^ "\n\n");;

print_string "########### Puzzle 2 ###########\n";;
print_string (string_of_solution sol_two ^ "\n\n");;

print_string "########### Puzzle 3 ###########\n";;
print_string (string_of_solution sol_three ^ "\n\n");;

print_string "########### Puzzle 4 ###########\n";;
print_string (string_of_solution sol_four ^ "\n\n");;

print_string "########### Puzzle 5 ###########\n";;
print_string (string_of_solution sol_five ^ "\n\n");;

print_string "########### Puzzle 6 ###########\n";;
print_string (string_of_solution sol_six ^ "\n\n");;
