(* 11.1 *)
let rec sum_of_square (i: int) : int = 
    if i = 0 then 0 else (i * i) + sum_of_square (i - 1)

(* 11.1 tests *)
let test_11_1_1 = sum_of_square 0 = 0
let test_11_1_2 = sum_of_square 1 = 1
let test_11_1_3 = sum_of_square 4 = 30

(* 11.2 *)
let rec a (n: int) : int = 
    if n = 0 then 3 else 2 * a (n - 1) - 1

(* 11.2 tests *)
let test_11_2_1 = a 0 = 3
let test_11_2_2 = a 1 = 5
let test_11_2_3 = a 2 = 9
let test_11_2_4 = a 3 = 17
let test_11_2_5 = a 4 = 33
let test_11_2_6 = a 5 = 65
let test_11_2_7 = a 6 = 129
