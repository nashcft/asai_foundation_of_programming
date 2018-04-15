(* wip *)
module Heap : sig
    type ('a, 'b) t
    type index_t

    (* create <size> <key> <value> => <heap> *)
    val create : int -> 'a -> 'b -> ('a, 'b) t

    (* insert <heap> <key> <value> => <updated_heap> *)
    (* destructive *)
    val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t

    val get : ('a, 'b) t -> index_t -> 'a * 'b

    (* destructive *)
    val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t

    (* destructive *)
    val split_top : ('a , 'b) t -> ('a * 'b) * ('a, 'b) t
end = struct
    type index_t = int ref
    type ('a, 'b) t = int ref * (index_t * 'a * 'b) array
    exception Empty
    exception Full

    let init_ref = ref (-1)

    let create size key value =
        (ref 0, Array.make size (init_ref, key, value))

    let insert (s_ref, ar) key value =
        if !s_ref >= Array.length ar then raise Full
        else let i = ref !s_ref in
            ar.(!i) <- (i, key, value);
            s_ref := !s_ref + 1;
            (i, (s_ref, ar))

    let get (s_ref, ar) i_ref =
        if 0 <= !i_ref && !i_ref < !s_ref
            then let (_, a, b) = ar.(!i_ref) in (a, b)
            else raise Not_found

    let set (s_ref, ar) i_ref key value = 
        let (_, k, _) = ar.(!i_ref) in
            ar.(!i_ref) <- (i_ref, key, value);
            (s_ref, ar)

    let split_top (s_ref, ar) = 
        let (_, a, b) = ar.(0) in
        ((a, b), (s_ref, ar))
end
