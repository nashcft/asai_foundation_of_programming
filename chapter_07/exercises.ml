(* Summarize five float values *)
let total5 a b c d e = a +. b +. c +. d +. e

(* 7.1. 5科目の点数を受け取り合計点と平均点を返す *)
(* goukei_to_heikin : float -> float -> float -> float -> float -> float * float *)
let goukei_to_heikin l m e ns ss = (total5 l m e ns ss, (total5 l m e ns ss) /. 5.)

(* example *)
let result1 = goukei_to_heikin 70. 75. 80. 85. 85.
let result2 = goukei_to_heikin 27. 83. 54. 97. 87.

(* 7.2. 名前と成績の組から "<name>さんの評価は<grade>です"という文字列を生成する *)
(* seiseki : string * string -> string *)
let seiseki p = match p with (name, grade) -> name ^ "さんの評価は" ^ grade ^ "です"

(* tests *)
let test_seiseki1 = seiseki ("鈴木", "A") = "鈴木さんの評価はAです"
let test_seiseki2 = seiseki ("佐藤", "B") = "佐藤さんの評価はBです"

(* 7.4. 平面座標 (x, y) を2つ受け取ってそれらの中点を算出する *)
(* chuten : float * float -> float * float -> float * float *)
let chuten coord1 coord2 =
  match coord1 with (x1, y1) -> match coord2 with (x2, y2) -> ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0)

(* tests *)
let test_chuten1 = chuten (10.0, 4.0) (3.0, (-3.0)) = (6.5, 0.5)
let test_chuten2 = chuten (1.0, 1.0) (1.0, 1.0) = (1.0, 1.0)
let test_chuten2 = chuten (0.0, 0.0) (0.0, 0.0) = (0.0, 0.0)
