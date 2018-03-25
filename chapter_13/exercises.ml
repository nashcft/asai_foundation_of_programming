(* From 8.3 *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birth_month : int;
    birth_day : int;
    blood_type : string;
}

(* 13.2 *)
let get_name (p: person_t) : string = match p with
    { name = n; height = h; weight = w; birth_month = m; birth_day = d; blood_type = bt } -> n

let person_namae (l: person_t list) : string list = List.map get_name l

(* 13.2 tests *)
let test_13_1_1 = person_namae [] = []
let test_13_1_2 = person_namae [
    { name = "Alice"; height = 150.; weight = 50.; birth_month = 7; birth_day = 12; blood_type = "B" };
    { name = "Bob"; height = 180.; weight = 80.; birth_month = 11; birth_day = 1; blood_type = "O" };
    { name = "Carol"; height = 160.; weight = 60.; birth_month = 9; birth_day = 14; blood_type = "AB" };
    { name = "Dave"; height = 170.; weight = 70.; birth_month = 5; birth_day = 21; blood_type = "A" };
] = [ "Alice"; "Bob"; "Carol"; "Dave"]
