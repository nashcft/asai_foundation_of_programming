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
