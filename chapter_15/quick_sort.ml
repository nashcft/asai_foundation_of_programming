let take th l p = List.filter (fun i -> p i th) l

let rec quick_sort l = match l with
    [] -> []
    | x::xs -> quick_sort (take x xs (<))
                @ [x]
                @ quick_sort (take x xs (>))

(* tests *)
let test1 = quick_sort [] = []
let test2 = quick_sort [1] = [1]
let test3 = quick_sort [1; 2] = [1; 2]
let test4 = quick_sort [2; 1] = [1; 2]
let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]

(* 15.1 *)
(* Initial implementation of quick_sort removes duplication *)
let test6 = quick_sort [5; 4; 0; 2; 4; 1; 8] = [0; 1; 2; 4; 4; 5; 8]

let rec quick_sort_improved l = match l with
    [] -> []
    | x::xs -> quick_sort_improved (take x xs (<=))
                @ [x]
                @ quick_sort_improved (take x xs (>))

let test1 = quick_sort_improved [] = []
let test2 = quick_sort_improved [1] = [1]
let test3 = quick_sort_improved [1; 2] = [1; 2]
let test4 = quick_sort_improved [2; 1] = [1; 2]
let test5 = quick_sort_improved [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]
let test6 = quick_sort_improved [5; 4; 0; 2; 4; 1; 8] = [0; 1; 2; 4; 4; 5; 8]
