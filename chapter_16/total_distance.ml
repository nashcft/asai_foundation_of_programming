type distance_t = {
    distance: float;
    total: float;
}
let total_distance l = match l with
    [] -> []
    | { distance = d; total = t }::xs ->
        let rec inner_total_dist il pt = match il with
            [] -> []
            | { distance = id; total = it }::ixs ->
                { distance = id; total = id +. pt } :: inner_total_dist ixs (id +. pt) in
        { distance = d; total = d +. t } :: inner_total_dist xs (d +. t)

let test = total_distance [
    { distance = 0.3; total = 0. };
    { distance = 0.9; total = 0. };
    { distance = 1.4; total = 0. };
    { distance = 0.8; total = 0. }
]
