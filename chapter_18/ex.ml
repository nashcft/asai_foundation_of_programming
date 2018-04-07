(* 18.1 *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birth_month : int;
    birth_day : int;
    blood_type : string;
}

let rec first_A (l: person_t list) = match l with
    [] -> None
    | ({ name = n; height = h; weight = w; birth_month = m; birth_day = d; blood_type =bd } as x)::xs -> 
        if bd = "A" then Some x else first_A xs 
(* 18.1 tests *)
let p1 = { name = "1"; height = 1.; weight = 1.; birth_month = 1; birth_day = 1; blood_type = "A" }
let p2 = { name = "2"; height = 1.; weight = 1.; birth_month = 1; birth_day = 1; blood_type = "B" }
let p3 = { name = "3"; height = 1.; weight = 1.; birth_month = 1; birth_day = 1; blood_type = "O" }
let p4 = { name = "4"; height = 1.; weight = 1.; birth_month = 1; birth_day = 1; blood_type = "AB" }
let p5 = { name = "5"; height = 1.; weight = 1.; birth_month = 1; birth_day = 1; blood_type = "A" }
let p6 = { name = "6"; height = 1.; weight = 1.; birth_month = 1; birth_day = 1; blood_type = "AB" }

let test_18_1_1 = first_A [p2; p5; p6] = Some p5
let test_18_1_2 = first_A [p2; p1; p6; p5] = Some p1
let test_18_1_1 = first_A [p2; p4; p6] = None

(* 18.2 *)
let rec price item l = match l with
    [] -> None
    | (v, p)::xs -> if item = v then Some (p) else price item xs
let rec count_urikire_yasai (l: string list) (l2: (string*int) list) : int = match l with
    [] -> 0
    | x::xs -> match price x l2 with
        None -> 1 + count_urikire_yasai xs l2
        | Some (p) -> count_urikire_yasai xs l2
(* 18.2 tests *)
let test_18_2_1 = count_urikire_yasai ["a"; "b"] [("a", 1); ("b", 1)] = 0
let test_18_2_1 = count_urikire_yasai ["a"; "b"] [("a", 1); ("c", 1)] = 1
let test_18_2_1 = count_urikire_yasai ["a"; "b"] [("c", 1); ("d", 1)] = 2
