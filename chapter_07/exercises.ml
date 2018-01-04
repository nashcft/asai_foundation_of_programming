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