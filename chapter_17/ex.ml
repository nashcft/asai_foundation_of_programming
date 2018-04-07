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

let test_tree =
    Node (2,
        Node (0,
            Empty,
            Leaf (5)),
        Node (3,
            Leaf (0),
            Node (1,
                Leaf (1),
                Leaf(1))))

(* 17.5 *)
let rec tree_double (t: tree_t) : tree_t = match t with
    Empty -> Empty
    | Leaf (n) -> Leaf (n * 2)
    | Node (n, l, r) -> Node(n * 2, tree_double l, tree_double r)
(* 17.5 tests *)
let test_17_5_1 = tree_double (Leaf (1)) = Leaf (2)
let test_17_5_2 = tree_double test_tree =
    Node(4, (Node (0, Empty, Leaf (10))), Node(6, Leaf(0), Node(2, Leaf (2), Leaf(2))))

(* 17.6 *)
let rec tree_map (f: int -> int) (t: tree_t) : tree_t = match t with
    Empty -> Empty
    | Leaf (n) -> Leaf (f n)
    | Node (n, l, r) -> Node (f n, tree_map f l, tree_map f r)
(* 17.6 tests *)
let test_17_6_1 = tree_map (fun x -> x + 3) test_tree =
    Node(5, (Node (3, Empty, Leaf (8))), Node(6, Leaf(3), Node(4, Leaf (4), Leaf(4))))

(* 17.7 *)
let rec tree_length (t: tree_t) : int = match t with
    Empty -> 0
    | Leaf (_) -> 1
    | Node (_, l, r) -> 1 + tree_length l + tree_length r
(* 17.7 tests *)
let test_17_7_1 = tree_length test_tree = 8

(* 17.8 *)
let rec tree_depth (t: tree_t) : int = match t with
    Empty -> 0
    | Leaf (_) -> 0
    | Node (_, l, r) -> 1 + max (tree_depth l) (tree_depth r)
(* 17.8 tests *)
let test_17_8_1 = tree_depth test_tree = 3
