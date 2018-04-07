type nengou_t = Meiji of int
                | Taisho of int
                | Showa of int
                | Heisei of int

let to_seireki (n: nengou_t) = match n with
    Meiji (y) -> y + 1867
    | Taisho (y) -> y + 1911
    | Showa (y) -> y + 1925
    | Heisei (y) -> y + 1988

(* 17.1 *)
let nenrei (birth: nengou_t) (current: nengou_t) : int =
    (to_seireki current) - (to_seireki birth)

(* 17.1 tests *)
let test_17_1_1 = nenrei (Showa (50)) (Heisei (30)) = 43
let test_17_1_2 = nenrei (Heisei (10)) (Heisei (30)) = 20
let test_17_1_3 = nenrei (Meiji (17)) (Heisei (14)) = 118

(* 17.2 *)
type year_t = January of int | February of int | March of int | April of int
                | May of int | June of int | July of int | August of int
                | September of int | October of int | November of int | December of int

(* 17.3 *)
type seiza_t = Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra
                | Scorpio | Sagittarius | Capricorn | Aquarius | Pisces

(* 17.4 *)
let seiza (y: year_t) : seiza_t = match y with
    January (d) -> if d <= 19 then Capricorn else Aquarius
    | February (d) -> if d <= 18 then Aquarius else Pisces
    | March (d) -> if d <= 20 then Pisces else Aries
    | April (d) -> if d <= 19 then Aries else Taurus
    | May (d) -> if d <= 20 then Taurus else Gemini
    | June (d) -> if d <= 21 then Gemini else Cancer
    | July (d) -> if d <= 22 then Cancer else Leo
    | August (d) -> if d <= 22 then Leo else Virgo
    | September (d) -> if d <= 22 then Virgo else Libra
    | October (d) -> if d <= 23 then Libra else Scorpio
    | November (d) -> if d <= 22 then Scorpio else Sagittarius
    | December (d) -> if d <= 21 then Sagittarius else Capricorn

type tree_t = Empty
                | Leaf of int
                | Node of int * tree_t * tree_t

let eg_17_1 =
    Node (17,
        Node (7,
            Empty,
            Leaf (3)),
        Leaf (24))
