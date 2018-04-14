module MySet : sig
    type 'a t

    val empty : 'a t

    val singleton : 'a -> 'a t

    val union : 'a t -> 'a t -> 'a t

    val inter : 'a t -> 'a t -> 'a t

    val diff : 'a t -> 'a t -> 'a t

    val mem : 'a -> 'a t -> bool
end = struct
    type 'a t = 'a list

    let empty = []

    let singleton e = [e]

    let rec mem e s1 = match s1 with
        [] -> false
        | x::xs -> if e = x then true else mem e xs

    let inter s1 s2 = List.filter (fun x -> mem x s2) s1

    let diff s1 s2 = List.filter (fun x -> not (mem x s2)) s1

    let union s1 s2 = (diff s1 s2) @ s2
end
