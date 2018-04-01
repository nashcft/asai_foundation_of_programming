(* 14.1 *)
let is_even n = n mod 2 = 0
let even l = List.filter is_even l

(* 14.1 tests (from 9.5) *)
let test_9_5_1 = even [] = []
let test_9_5_2 = even [1; 2; 3] = [2]
let test_9_5_3 = even [1; 3; 5] = []
let test_9_5_4 = even [2; 4; 6; 8] = [2; 4; 6; 8]

(* 14.2 *)
type gakusei_t = {
    name: string;
    score: int;
    grade: string;
}

let grade_A s = match s with { name = n; score = s; grade =g; } -> g = "A"
let count_A l = List.length (List.filter grade_A l)

(* 14.2 tests (from textbook) *)
let test_14_2_1 = count_A [] = 0
let test_14_2_2 = count_A [{ name = ""; score = 1; grade = "B" }] = 0
let test_14_2_3 = count_A [{ name = ""; score = 1; grade = "B" }; { name = ""; score = 1; grade = "A" }] = 1
let test_14_2_4 = count_A [
    { name = ""; score = 1; grade = "B" };
    { name = ""; score = 1; grade = "A" };
    { name = ""; score = 1; grade = "A" }
] = 2
