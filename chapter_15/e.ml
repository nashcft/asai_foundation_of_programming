let rec nth_term (n: int) : float =
    if n = 0
        then 1.0
        else nth_term (n - 1) /. float_of_int n

let rec e (n: int) : float =
    let t = nth_term n in
    if t < 0.00001
        then t
        else t +. e (n + 1)
