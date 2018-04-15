let c = ref 0

let gensym (s: string) : string =
    let ret = s ^ string_of_int !c in (c := !c + 1; ret)

let test_22_1_1 = gensym "a" = "a0"
let test_22_1_2 = gensym "abc" = "abc1"
let test_22_1_3 = gensym "" = "2"
