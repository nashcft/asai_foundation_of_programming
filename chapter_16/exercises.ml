(* 16.1 *)
let sum_list l =
    let rec inner il a = match il with
        [] -> []
        | x::xs -> x + a :: inner xs (x + a) in
    inner l 0

let test_16_1 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]
