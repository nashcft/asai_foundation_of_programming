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

(* 13.3 *)
let f1 a = a
let f2 a b = a
let f3 a b = b
let f4 a f = f a
let f5 f g a = g (f a)

(* 13.4 *)
let compose f g = let h x = f (g x) in h
(* compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'a *)

(* 13.5:
 * (twice twice) : ('_weak1 -> '_weak1) -> '_weak1 -> '_weak1
 * (twice twice) is a function that applies the given function 2 ^ 2 times to its argument
 *)
