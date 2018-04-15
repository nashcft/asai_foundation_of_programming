let fib_array arr =
    let rec update_array n = 
        if n <= 1 then (arr.(n) <- n; n) 
        else let fib = update_array (n - 1) + update_array (n - 2) in (arr.(n) <- fib; fib) in
    let len = Array.length arr in
    if len = 0 then [||] else (update_array (len - 1); arr) 

let test_22_2_1 = fib_array [|0|] = [|0|]
let test_22_2_2 = fib_array [|0; 0; 0; 0; 0; 0; 0; 0|] = [|0; 1; 1; 2; 3; 5; 8; 13|]
let test_22_2_3 = fib_array [||] = [||]
