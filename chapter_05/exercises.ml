(* 5.2. 目的: 時間 x (24h) を受け取って午前か午後か返す *)
(* jikan : int -> string *)
let jikan x = if x < 12 then "a.m." else "p.m."

(* tests *)
let test_jikan_1 = jikan 0 = "a.m."
let test_jikan_2 = jikan 5 = "a.m."
let test_jikan_3 = jikan 12 = "p.m."
let test_jikan_4 = jikan 20 = "p.m."
let test_jikan_5 = jikan 23 = "p.m."

(* 5.3. 月 x と日 y を受け取ったら星座を返す *)
(* seiza : int -> int -> string *)
let seiza x y = 
  if y < 1 then "Day should be >= 1"
  else if (x = 3 && y >= 21 && y <= 31) || (x = 4 && y <= 19) then "Aries"
  else if (x = 4 && y <= 30) || (x = 5 && y <= 20) then "Taurus"
  else if (x = 5 && y <= 31) || (x = 6 && y <= 21) then "Gemini"
  else if (x = 6 && y <= 30) || (x = 7 && y <= 22) then "Cancer"
  else if (x = 7 && y <= 31) || (x = 8 && y <= 22) then "Leo"
  else if (x = 8 && y <= 31) || (x = 9 && y <= 22) then "Virgo"
  else if (x = 9 && y <= 30) || (x = 10 && y <= 23) then "Libra"
  else if (x = 10 && y <= 31) || (x = 11 && y <= 22) then "Scorpio"
  else if (x = 11 && y <= 30) || (x = 12 && y <= 21) then "Sagittarius"
  else if (x = 12 && y <= 31) || (x = 1 && y <= 19) then "Capricorn"
  else if (x = 1 && y <= 31) || (x = 2 && y <= 18) then "Aquarius"
  else if (x = 2 && y <= 29) || (x = 3 && y <= 20) then "Pisces"
  else "Invalid date"

(* tests *)
let test_seiza_aries1 = seiza 3 21 = "Aries"
let test_seiza_aries2 = seiza 3 31 = "Aries"
let test_seiza_aries3 = seiza 4 1 = "Aries"
let test_seiza_aries4 = seiza 4 19 = "Aries"
let test_seiza_taurus1 = seiza 4 20 = "Taurus"
let test_seiza_taurus2 = seiza 4 30 = "Taurus"
let test_seiza_taurus3 = seiza 5 1 = "Taurus"
let test_seiza_taurus4 = seiza 5 20 = "Taurus"
let test_seiza_gemini1 = seiza 5 21 = "Gemini"
let test_seiza_gemini2 = seiza 5 31 = "Gemini"
let test_seiza_gemini3 = seiza 6 1 = "Gemini"
let test_seiza_gemini4 = seiza 6 21 = "Gemini"
let test_seiza_cancer1 = seiza 6 22 = "Cancer"
let test_seiza_cancer2 = seiza 6 30 = "Cancer"
let test_seiza_cancer3 = seiza 7 1 = "Cancer"
let test_seiza_cancer4 = seiza 7 22 = "Cancer"
let test_seiza_leo1 = seiza 7 23 = "Leo"
let test_seiza_leo2 = seiza 7 31 = "Leo"
let test_seiza_leo3 = seiza 8 1 = "Leo"
let test_seiza_leo4 = seiza 8 22 = "Leo"
let test_seiza_virgo1 = seiza 8 23 = "Virgo"
let test_seiza_virgo2 = seiza 8 31 = "Virgo"
let test_seiza_virgo3 = seiza 9 1 = "Virgo"
let test_seiza_virgo4 = seiza 9 22 = "Virgo"
let test_seiza_libra1 = seiza 9 23 = "Libra"
let test_seiza_libra2 = seiza 9 30 = "Libra"
let test_seiza_libra3 = seiza 10 1 = "Libra"
let test_seiza_libra4 = seiza 10 23 = "Libra"
let test_seiza_scorpio1 = seiza 10 24 = "Scorpio"
let test_seiza_scorpio2 = seiza 10 30 = "Scorpio"
let test_seiza_scorpio3 = seiza 11 1 = "Scorpio"
let test_seiza_scorpio4 = seiza 11 22 = "Scorpio"
let test_seiza_sagittarius1 = seiza 11 23 = "Sagittarius"
let test_seiza_sagittarius2 = seiza 11 30 = "Sagittarius"
let test_seiza_sagittarius3 = seiza 12 1 = "Sagittarius"
let test_seiza_sagittarius4 = seiza 12 21 = "Sagittarius"
let test_seiza_capricorn1 = seiza 12 22 = "Capricorn"
let test_seiza_capricorn2 = seiza 12 31 = "Capricorn"
let test_seiza_capricorn3 = seiza 1 1 = "Capricorn"
let test_seiza_capricorn4 = seiza 1 19 = "Capricorn"
let test_seiza_aquarius1 = seiza 1 20 = "Aquarius"
let test_seiza_aquarius2 = seiza 1 31 = "Aquarius"
let test_seiza_aquarius3 = seiza 2 1 = "Aquarius"
let test_seiza_aquarius4 = seiza 2 18 = "Aquarius"
let test_seiza_pisces1 = seiza 2 19 = "Pisces"
let test_seiza_pisces2 = seiza 2 29 = "Pisces"
let test_seiza_pisces3 = seiza 3 1 = "Pisces"
let test_seiza_pisces4 = seiza 3 20 = "Pisces"

(* 5.4. 二次方程式 ax^2 + bx + c = 0 の係数 a, b, c (いずれも実数、 a<>0) より判別式の値を返す *)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = b ** 2.0 -. 4.0 *. a *. c

(* 5.5. 二次方程式の係数 a, b, c (いずれも実数、 a<>0) より解の個数を返す *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0.0 then 2
  else if hanbetsushiki a b c = 0.0 then 1
  else 0

(* tests *)
let test_kai_no_kosuu1 = kai_no_kosuu 2.0 3.0 14.0 = 0
let test_kai_no_kosuu2 = kai_no_kosuu 1.0 4.0 4.0 = 1
let test_kai_no_kosuu3 = kai_no_kosuu 4.0 6.0 2.0 = 2

(* 5.6. 二次方程式の係数より虚数解を持つかどうか判定する *)
(* kyosuukai : float -> float -> float -> bool *)
let kyosuukai a b c = hanbetsushiki a b c < 0.0

(* tests *)
let test_kyosuukai1 = kyosuukai 2.0 3.0 14.0 = true
let test_kyosuukai2 = kyosuukai 1.0 4.0 4.0 = false
let test_kyosuukai3 = kyosuukai 4.0 6.0 2.0 = false
let test_kyosuukai3 = kyosuukai (-.4.0) 1.0 (-.2.0) = true

(* bmi from chapter 4 *)
let bmi height weight = weight /. (height ** 2.0)

(* 5.7. 身長 (m) と体重 (kg) からBMIを計算し、体型について評価する *)
(* taikei : float -> float -> string *)
let taikei h w =
  if bmi h w < 18.5 then "痩せ"
  else if bmi h w >= 30.0 then "高度肥満"
  else if bmi h w < 25.0 then "普通"
  else "軽度肥満"

(* tests *)
let test_taikei1 = taikei 1.7 65.8 = "普通"
let test_taikei2 = taikei 1.9 60.0 = "痩せ"
let test_taikei3 = taikei 1.8 83.8 = "軽度肥満"
let test_taikei4 = taikei 1.7 120.3 = "高度肥満"