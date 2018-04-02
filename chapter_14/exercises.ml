(* 14.1 *)
let even l = List.filter (fun n -> n mod 2 = 0) l

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

let count_A l =
    List.length (List.filter (fun s -> match s with { name = n; score = s; grade =g; } -> g = "A") l)

(* 14.2 tests (from textbook) *)
let slist = [
    { name = ""; score = 1; grade = "B" };
    { name = ""; score = 1; grade = "A" };
    { name = ""; score = 1; grade = "A" }
]

let test_14_2_1 = count_A [] = 0
let test_14_2_2 = count_A [{ name = ""; score = 1; grade = "B" }] = 0
let test_14_2_3 = count_A [{ name = ""; score = 1; grade = "B" }; { name = ""; score = 1; grade = "A" }] = 1
let test_14_2_4 = count_A slist = 2

(* 14.3 *)
let concat (l: string list) : string = List.fold_right (fun s1 s2 -> s1 ^ s2) l ""

(* 14.3 tests *)
let test_14_3_1 = concat [] = ""
let test_14_3_2 = concat ["a"; "b"; "c"] = "abc"

(* 14.4 *)
let gakusei_sum (l: gakusei_t list) : int = 
    List.fold_right (fun (s: gakusei_t) (n: int) -> match s with
        { name = nm; score = sc; grade = gr } -> sc + n) l 0

(* 14.4 tests *)
let test_14_4_1 = gakusei_sum [] = 0
let test_14_4_2 = gakusei_sum [
    { name  = ""; score = 10; grade = "E" };
    { name  = ""; score = 46; grade = "E" };
    { name  = ""; score = 90; grade = "E" };
] = 146

(* 14.6 *)
let count (l: gakusei_t list) (target: string) : int =
    List.length (List.filter (fun s -> match s with
        { name = nm; score = sc; grade = gr } -> gr = target) l)

(* 14.6 tests *)
let test_14_6_1 = count slist "A" = 2
let test_14_6_2 = count slist "B" = 1
let test_14_6_3 = count slist "C" = 0

(* 14.8 *)
let f14_8 =fun n -> n * n - 1

(* 14.9 *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birth_month : int;
    birth_day : int;
    blood_type : string;
}
let f14_9 = fun p -> match p with 
    { name = n;
    height = h;
    weight = w;
    birth_month = m;
    birth_day = d;
    blood_type = b } -> n
