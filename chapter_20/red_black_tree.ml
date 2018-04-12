type color_t = Red | Black
type ('a, 'b) rb_tree_t =
    Empty
    | Node of 'a * 'b * color_t * ('a, 'b) rb_tree_t * ('a, 'b) rb_tree_t

let empty = Empty

let balance tree = match tree with
    Node (zk, zv, Black, Node (yk, yv, Red, Node (xk, xv, Red, a, b), c), d)
    | Node (zk, zv, Black, Node (xk, xv, Red, a, Node (yk, yv, Red, b, c)), d)
    | Node (xk, xv, Black, a, Node (zk, zv, Red, Node (yk, yv, Red, b, c), d))
    | Node (xk, xv, Black, a, Node (yk, yv, Red, b, Node (zk, zv, Red, c, d)))
        -> Node (yk, yv, Red, Node (xk, xv, Black, a, b), Node (zk, zv, Black, c, d))
    | _ -> tree

(*
let t1 = Node ("z", 1, Black, Node ("y", 1, Red, Node ("x", 1, Red, Empty, Empty), Empty), Empty)
let t2 = Node ("z", 1, Black, Node ("x", 1, Red, Empty, Node ("y", 1, Red, Empty, Empty)), Empty)
let t3 = Node ("x", 1, Black, Empty, Node ("z", 1, Red, Node ("y", 1, Red, Empty, Empty), Empty))
let t4 = Node ("x", 1, Black, Empty, Node ("y", 1, Red, Empty, Node ("z", 1, Red, Empty, Empty)))
let t5 = Node ("y", 1, Red, Node ("x", 1, Black, Empty, Empty), Node ("z", 1, Black, Empty, Empty))

let test_20_2_1 = balance Empty = Empty
let test_20_2_2 = balance t1 = t5
let test_20_2_3 = balance t2 = t5
let test_20_2_3 = balance t3 = t5
let test_20_2_3 = balance t4 = t5
*)

let insert tree key value =
    let rec inner tree = match tree with
        Empty -> Node (key, value, Red, Empty, Empty)
        | Node (k, v, c, l, r) ->
            if k = key then Node (k, value, c, l, r)
            else if k > key then balance (Node (k, v, c, inner l, r))
            else balance (Node (k, v, c, l, inner r)) in
    match inner tree with
        Empty -> assert false
        | Node (k, v, c, l, r) -> Node(k, v, Black, l, r)
(*
let test_20_3_1 = insert Empty "d" 1 = Node ("d", 1, Black, Empty, Empty)
let test_20_3_2 = insert (Node ("d", 1, Black, Empty, Empty)) "c" 1 =
    Node ("d", 1, Black, Node("c", 1, Red, Empty, Empty), Empty)
let test_20_3_3 = insert (Node ("d", 1, Black, Node("c", 1, Red, Empty, Empty), Empty)) "f" 1 =
    Node ("d", 1, Black, Node("c", 1, Red, Empty, Empty), Node ("f", 1, Red, Empty, Empty))
let test_20_3_4 = insert (Node ("d", 1, Black, Node("c", 1, Red, Empty, Empty), Empty)) "a" 1 =
    Node ("c", 1, Black, Node("a", 1, Black, Empty, Empty), Node("d", 1, Black, Empty, Empty))
*)

let rec search tree key = match tree with
    Empty -> raise Not_found
    | Node (k, v, c, l, r) ->
        if k = key then v
        else if k > key then search l key
        else search r key

(*
let target = 
    Node ("f", 6, Black,
        Node ("c", 3, Black,
            Node ("a", 1, Red, Empty, Empty),
            Node ("e", 5, Red, Empty, Empty)),
        Node ("h", 8, Black, 
            Empty,
            Node("r", 18, Red, Empty, Empty)))
let test_20_4_1 = search target "c" = 3
let test_20_4_2 = search target "r" = 18
let test_20_4_3 = (try search target "j" with Not_found -> 0) = 0
*)
