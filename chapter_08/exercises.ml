(* 8.3. *)
type person_t = {
    name : string;
    height : float;
    weight : float;
    birth_month : int;
    birth_day : int;
    blood_type : string;
}

(* examples *)
let persion1 = { name = "Adam"; height = 1.5; weight = 60.0; birth_month = 2; birth_day = 23; blood_type = "A" }
let persion2 = { name = "Ben"; height = 1.9; weight = 86.0; birth_month = 6; birth_day = 3; blood_type = "AB" }
let persion3 = { name = "Chris"; height = 1.78; weight = 76.0; birth_month = 9; birth_day = 1; blood_type = "O" }

(* 8.4. *)
let ketsueki_hyoji person = match person with
  { name = n; height = h; weight = w; birth_month = mon; birth_day = day; blood_type = t } ->
    n ^ "さんの血液型は" ^ t ^ "です"

(* tests *)
let test_ketsueki_hyoji1 = ketsueki_hyoji persion1 = "Adamさんの血液型はAです"
let test_ketsueki_hyoji2 = ketsueki_hyoji persion2 = "Benさんの血液型はABです"
let test_ketsueki_hyoji3 = ketsueki_hyoji persion3 = "Chrisさんの血液型はOです"
