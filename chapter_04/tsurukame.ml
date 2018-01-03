(* 4.6. 目的: 鶴の数 x に応じて鶴の足の本数を返す *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi x = x * 2

(* tests *)
let test_tsuru_no_ashi_1 = tsuru_no_ashi 1 = 2
let test_tsuru_no_ashi_2 = tsuru_no_ashi 5 = 10
let test_tsuru_no_ashi_3 = tsuru_no_ashi 0 = 0

(* 4.7. 目的: 鶴の数 x と亀の数 y に応じてそれぞれの足の数の合計を返す *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi x y = (tsuru_no_ashi x) + y * 4

(* tests *)
let test_tsurukame_no_ashi_1 = tsurukame_no_ashi 0 1 = 4
let test_tsurukame_no_ashi_2 = tsurukame_no_ashi 1 0 = 2
let test_tsurukame_no_ashi_3 = tsurukame_no_ashi 2 2 = 12

(* 4.8 目的: 鶴亀算を解く; 鶴と亀の合計数とそれぞれの足の数の合計を受け取り、鶴の数を返す *)
(* tsurukame : int -> int -> int *)
let tsurukame x y = 2 * x - y / 2

(* tests *)
let test_tsurukame_1 = tsurukame 10 28 = 6
let test_tsurukame_2 = tsurukame 8 26 = 3
let test_tsurukame_3 = tsurukame 0 0 = 0