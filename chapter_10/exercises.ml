(* 10.1 *)
(* insert : int list -> int -> int list *)
let rec insert (l: int list) (n: int) : int list = match l with
    [] -> n :: []
    | x::xs -> if x <= n
                  then x :: insert xs n
                  else n :: l

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

(* 10.3 *)
type gakusei_t = {
    name: string;
    score: int;
    grade: string;
}

let rec insert (l: gakusei_t list) (g: gakusei_t) : gakusei_t list = match l with
    [] -> g::[]
    | ({ name = lnm; score = lsc; grade = lgr; } as x)::xs -> 
        (match g with { name = nm; score = sc; grade = gr; } -> 
            if lsc >= sc then x :: insert xs g else g :: l)

let rec gakusei_sort (l: gakusei_t list) : gakusei_t list = match l with
    [] -> []
    | x::xs -> insert (gakusei_sort xs) x

(* test data *)
let students1: gakusei_t list = [
    { name = ""; score = 70; grade = "B" };
    { name = ""; score = 75; grade = "B" };
    { name = ""; score = 68; grade = "C" };
]
let students1_sorted: gakusei_t list = [
    { name = ""; score = 75; grade = "B" };
    { name = ""; score = 70; grade = "B" };
    { name = ""; score = 68; grade = "C" };
]

(* tests *)
let test_10_3_1 = gakusei_sort [] = []
let test_10_3_2 = gakusei_sort students1 = students1_sorted
let test_10_3_3 = gakusei_sort students1_sorted = students1_sorted

(* 10.4 *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birth_month : int;
    birth_day : int;
    blood_type : string;
}

let rec insert (l: person_t list) (p: person_t) : person_t list = match l with
    [] -> p :: []
    | ({ name = ln; height = lh; weight = lw; birth_month = lm; birth_day = ld; blood_type = lb } as x)::xs -> 
        (match p with { name = n; height = h; weight = w; birth_month = m; birth_day = d; blood_type = b } -> 
            if ln <= n then x :: insert xs p else p :: l)

let rec person_sort (l: person_t list) : person_t list = match l with 
    [] -> []
    | x::xs -> insert (person_sort xs) x

(* test data *)
let people = [
    { name = "Alice"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Bob"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Adam"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Carol"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Ben"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" }
]
let people_sorted = [
    { name = "Adam"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Alice"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Ben"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Bob"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Carol"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" }
]

(* tests *)
let test_10_4_1 = person_sort [] = []
let test_10_4_2 = person_sort people = people_sorted
let test_10_4_3 = person_sort people_sorted = people_sorted

exception Empty

(* 10.5, 10.6 *)
let get_score g = match g with { name = n; score =s; grade = g } -> s

let n_a_student = { name = "N/A"; score = min_int; grade = "N/A" }

let rec gakusei_max (l: gakusei_t list) : gakusei_t = match l with
    [] -> n_a_student
    | ({ name = n; score = s; grade = g} as x)::xs -> 
        let max_xs = gakusei_max xs in
            if s >= get_score max_xs then x else max_xs

(* test data *)
let alice = { name = "Alice"; score = 70; grade = "B" }
let bob = { name = "Bob"; score = 75; grade = "B" }
let carol = { name = "Carol"; score = 65; grade = "C" }

let students = [alice; bob; carol]

(* tests *)
let test_10_5_1 = gakusei_max [] = n_a_student
let test_10_5_2 = gakusei_max students = bob

(* 10.7 *)
let rec ketsueki_shukei (l: person_t list) : int*int*int*int = match l with
    [] -> (0, 0, 0, 0)
    | { name = n; height = h; weight = w; birth_month = m; birth_day = d; blood_type = bt }::xs ->
        let (a, b, o, ab) = ketsueki_shukei xs in 
            if bt = "A" then (a + 1, b, o, ab)
            else if bt = "B" then (a, b + 1, o, ab)
            else if bt = "O" then (a, b, o + 1, ab)
            else (a, b, o, ab + 1)

(* test data *)
let people_2 = [
    { name = "Alice"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Bob"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "B" };
    { name = "Adam"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "O" };
    { name = "Carol"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "AB" };
    { name = "Ben"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "O" }
]

(* test *)
let test_10_7_1 = ketsueki_shukei [] = (0, 0, 0, 0)
let test_10_7_2 = ketsueki_shukei people_2 = (1, 1, 2, 1)
let test_10_7_3 = ketsueki_shukei people = (5, 0, 0, 0)

(* 10.8 *)
let rec saita_ketdueki (l: person_t list) : string = match l with
    [] -> "No entry"
    | x::xs -> 
        let (a, b, o, ab) = ketsueki_shukei l in
        let max_bt = max (max a b) (max o ab) in
        if max_bt = a then "A"
        else if max_bt = b then "B"
        else if max_bt = o then "O"
        else "AB"

(* tests *)
let test_10_8_1 = saita_ketdueki [] = "No entry"
let test_10_8_2 = saita_ketdueki people = "A"
let test_10_8_3 = saita_ketdueki people_2 = "O"
let test_10_8_4 = saita_ketdueki [
    { name = "Alice"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "A" };
    { name = "Bob"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "B" };
    { name = "Adam"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "O" };
    { name = "Carol"; height = 162.1; weight = 49.7; birth_month = 8; birth_day = 18; blood_type = "AB" }
] = "A"
