(* 8.5. *)
type ekimei_t = {
    kanji : string;
    kana : string;
    romaji : string;
    shozoku : string;
}

(* 8.6 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei = match ekimei with {
    kanji = kanji;
    kana = kana;
    romaji = rom;
    shozoku = sh;
} -> sh ^ ", " ^ kanji ^ " (" ^ kana ^ ")"

let test_8_6_1 = hyoji {kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線"} = "丸ノ内線, 池袋 (いけぶくろ)"
let test_8_6_2 = hyoji {kanji = "市ヶ谷"; kana = "いちがや"; romaji = "ichigaya"; shozoku = "南北線"} = "南北線, 市ヶ谷 (いちがや)"
let test_8_6_3 = hyoji {kanji = "和光市"; kana = "わこうし"; romaji = "wakoshi"; shozoku = "有楽町線"} = "有楽町線, 和光市 (わこうし)"

(* 8.7 *)
type ekikan_t = {
    kiten : string;
    shuten : string;
    keiyu : string;
    kyori : float;
    jikan : int;
}
