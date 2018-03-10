(* 9.1 *)
let seasons = "Spring" :: "Summer" :: "Autumn" :: "Winter" :: []

(* 9.2 *)
(* from 8.3. *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birth_month : int;
    birth_day : int;
    blood_type : string;
}

let people = {name = "Adam"; height = 178.0; weight = 70.1; birth_month = 8; birth_day = 3; blood_type = "A"} ::
             {name = "Bob"; height = 190.2; weight = 80.3; birth_month = 2; birth_day = 15; blood_type = "AB"} ::
             {name = "Carol"; height = 167.9; weight = 54.9; birth_month = 5; birth_day = 4; blood_type = "O"} :: []

(* 9.3 *)
let seasons_2 = ["Spring"; "Summer"; "Autumn"; "Winter"]
let people_2 = [
    {name = "Adam"; height = 178.0; weight = 70.1; birth_month = 8; birth_day = 3; blood_type = "A"};
    {name = "Bob"; height = 190.2; weight = 80.3; birth_month = 2; birth_day = 15; blood_type = "AB"};
    {name = "Carol"; height = 167.9; weight = 54.9; birth_month = 5; birth_day = 4; blood_type = "O"}
]

(* 9.4 *)
(* length : int list -> int *)
let rec length lst = match lst with
    [] -> 0
    | x::xs -> 1 + length xs

let test_9_4_1 = length [] = 0
let test_9_4_2 = length (0 :: []) = 1
let test_9_4_3 = length [1; 2; 3] = 3
let test_9_4_4 = length [1; 2; 3; 4; 5] = List.length [1; 2; 3; 4; 5]
let test_9_4_5 = length [] = List.length []

(* 9.5 *)
(* even : int list -> int list *)
let rec even l = match l with
    [] -> []
    | x::xs -> if x mod 2 = 0
                 then x :: even xs
                 else even xs

let test_9_5_1 = even [] = []
let test_9_5_2 = even [1; 2; 3] = (2 :: [])
let test_9_5_3 = even [1; 3; 5] = []
let test_9_5_4 = even [2; 4; 6; 8] = [2; 4; 6; 8]

(* 9.6 *)
(* concat : string list -> string *)
let rec concat l = match l with
    [] -> ""
    | x::xs -> x ^ concat xs

let test_9_6_1 = concat [] = ""
let test_9_6_2 = concat seasons = "SpringSummerAutumnWinter"
