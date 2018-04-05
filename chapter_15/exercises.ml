(* 15.2 *)
(* m >= n >= 0 
 * 停止性について: (m mod n) > n >= 0
 *)
let rec gdc m n = if n = 0 then m else gdc n (m mod n)

(* 15.2 tests *)
let test_15_2_1 = gdc 0 0 = 0
let test_15_2_1 = gdc 28764 26208 = 36

(* 15.3 *)
(* 停止性について: l = x::xs に対して filter をかけており、引数 l は次第に短くなり最終的に [] となる *)
let rec sieve (l: int list) : int list = match l with
    [] -> []
    | x::xs -> x :: sieve (List.filter (fun n -> n mod x <> 0) xs)

let rec range i t = if i <= t then i :: range (i + 1) t else []

let rec prime (n: int) : int list = sieve (range 2 n)

(* 15.3 tests *)
let test_15_3_1 = prime 0 = []
let test_15_3_2 = prime 2 = [2]
let test_15_3_3 = prime 10 = [2; 3; 5; 7]