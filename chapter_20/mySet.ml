module type MySet = sig
    type 'a t

    val empty : 'a t

    val singleton : 'a -> 'a t

    val to_set : 'a list -> 'a t

    val to_list : 'a t -> 'a list

    val union : 'a t -> 'a t -> 'a t

    val inter : 'a t -> 'a t -> 'a t

    val diff : 'a t -> 'a t -> 'a t

    val mem : 'a -> 'a t -> bool
end

module ListSet : MySet = struct
    type 'a t = Set of 'a list

    let empty = Set ([])

    let singleton e = Set ([e])

    let to_set l = Set (List.fold_right (fun x a -> if List.mem x a then a else x :: a) l [])

    let to_list s = match s with Set (l) -> l

    let inter_list l1 l2 = List.filter (fun x -> List.mem x l2) l1

    let inter s1 s2 = match (s1, s2) with (Set (l1), Set (l2)) -> Set (inter_list l1 l2)

    let diff_list l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

    let diff s1 s2 = match (s1, s2) with (Set (l1), Set (l2)) -> Set (diff_list l1 l2)

    let union s1 s2 = match (s1, s2) with (Set (l1), Set (l2)) -> Set (l1 @ diff_list l2 l1)

    let mem e s = match s with Set (l) -> List.mem e l
end

module TreeSet : MySet = struct
    type color_t = R | B
    type 'a t = Empty | Node of 'a * color_t * 'a t * 'a t

    let balance tree = match tree with
        Node (zv, B, Node (yv, R, Node (xv, R, a, b), c), d)
        | Node (zv, B, Node (xv, R, a, Node (yv, R, b, c)), d)
        | Node (xv, B, a, Node (zv, R, Node (yv, R, b, c), d))
        | Node (xv, B, a, Node (yv, R, b, Node (zv, R, c, d)))
            -> Node (yv, R, Node (xv, B, a, b), Node (zv, B, c, d))
        | _ -> tree

    let insert e tree =
        let rec inner in_t = match in_t with
            Empty -> Node (e, R, Empty, Empty)
            | Node (v, c, l, r) ->
                if e = v then in_t
                else if e < v then balance (Node (v, c, inner l, r))
                else  balance (Node (v, c, l, inner r)) in
        match inner tree with
            Empty -> assert false
            | Node (v, c, l, r) -> Node (v, B, l, r)

    let empty = Empty

    let singleton e = Node (e, B, Empty, Empty)

    let to_set l = List.fold_right insert l Empty

    let rec to_list s = match s with
        Empty -> []
        | Node (v, c, l, r) -> to_list l @ [v] @ to_list r

    let rec mem e s = match s with
        Empty -> false
        | Node (v, c, l, r) ->
            if e = v then true
            else if e < v then mem e l
            else mem e r

    let union s1 s2 = List.fold_right insert (to_list s2) s1

    let inter s1 s2 =
        List.fold_right (fun e s -> if mem e s1 then insert e s else s) (to_list s2) Empty

    let diff s1 s2 =
        List.fold_right (fun e s -> if mem e s2 then s else insert e s) (to_list s1) Empty
end
