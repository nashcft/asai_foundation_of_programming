(* 16.1 *)
let sum_list l =
    let rec inner il a = match il with
        [] -> []
        | x::xs -> x + a :: inner xs (x + a) in
    inner l 0

let test_16_1 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]

(* 16.2 *)
let rec fold_left f init l = match l with
    [] -> init
    | x::xs -> fold_left f (f init x) xs

let test_16_2_1 = fold_left (+) 0 [3; 2; 1; 4] = 10
let test_16_2_2 = fold_left (+) 0 [3; 2; 1; 4] = List.fold_left (+) 0 [3; 2; 1; 4]
let test_16_2_2 = fold_left (+) 0 [3; 2; 1; 4] = List.fold_right (+) [3; 2; 1; 4] 0
