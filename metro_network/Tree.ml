module Tree = struct
    type ('a, 'b) t = Empty
                        | Node of 'a * 'b * ('a, 'b) t * ('a, 'b) t
    let empty = Empty

    let rec insert (tree: ('a, 'b) t) (k: 'a) (v: 'b) : ('a, 'b) t = match tree with
        Empty -> Node (k, v, Empty, Empty)
        | Node (key, value, l, r) ->
            if k = key then Node (key, v, l, r)
            else if k < key then Node (key, value, insert l k v, r)
            else Node (key, value, l, insert r k v)
    
    let rec search (tree: ('a, 'b) t) (k: 'a) : 'b = match tree with
        Empty -> raise Not_found
        | Node (key, value, l, r) ->
            if k = key then value
            else if k < key then search l k
            else search r k
end