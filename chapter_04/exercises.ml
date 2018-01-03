(* 4.1. *)
let baito_kyuyo hour year = hour * (850 + 100 * (year - 1))

(* 4.1. test *)
let test_4_1_1 = (baito_kyuyo 20 1) = 850 * 20
let test_4_1_2 = (baito_kyuyo 20 3) = 1050 * 20

(* 4.2. *)
let jikoshokai name = "Hello, I'm " ^ name ^ "."

(* 4.2. example *)
let test_4_2_1 = jikoshokai "Adam"
let test_4_2_2 = jikoshokai "太郎"

(* 4.3. *)
let hyojun_taiju height = height ** 2.0 *. 22.0

(* 4.3. example *)
let test_4_3_1 = hyojun_taiju 1.7

(* 4.4. *)
let bmi height weight = weight /. (height ** 2.0)

(* 4.4. example *)
let test_4_4_1 = bmi 1.7 65.5