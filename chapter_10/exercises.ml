(* 10.1 *)
(* insert : int list -> int -> int list *)
let rec insert (l: int list) (n: int) : int list = match l with
    [] -> n :: []
    | x::xs -> if x <= n
                  then x :: insert xs n
                  else n :: x :: xs

(* tests *)
let test_10_1_1 = insert [] 0 = [0]
let test_10_1_2 = insert [1; 3; 4 ; 7; 8] 5 = [1; 3; 4; 5; 7; 8]
let test_10_1_3 = insert [1; 3; 6] 7 = [1; 3; 6; 7]
let test_10_1_4 = insert [2; 4; 5] 0 = [0; 2; 4; 5]
let test_10_1_5 = insert [1; 2; 3; 4] 3 = [1; 2; 3; 3; 4]

(* 10.2 *)
(* ins_sort : int list -> int list *)
let rec ins_sort l = match l with
    [] -> []
    | x::xs -> insert (ins_sort xs) x

(* tests *)
let test_10_2_1 = ins_sort [] = []
let test_10_2_2 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8]
let test_10_2_3 = ins_sort [5; 4; 3; 2; 1] = [1; 2; 3; 4; 5]
let test_10_2_4 = ins_sort [1; 2; 3; 4; 5] = [1; 2; 3; 4; 5]
let test_10_2_5 = ins_sort [4; 7; 3; 1; 5; 3; 2; 4] = [1; 2; 3; 3; 4; 4; 5; 7]