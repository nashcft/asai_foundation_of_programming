type distance_t = {
    distance: float;
    total: float;
}
let total_distance l =
    let rec inner_total_dist il pt = match il with
        [] -> []
        | { distance = d; total = t }::xs ->
            { distance = d; total = d +. pt } :: inner_total_dist xs (d +. pt) in
    inner_total_dist l 0.

let test = total_distance [
    { distance = 0.3; total = 0. };
    { distance = 0.9; total = 0. };
    { distance = 1.4; total = 0. };
    { distance = 0.8; total = 0. }
]
