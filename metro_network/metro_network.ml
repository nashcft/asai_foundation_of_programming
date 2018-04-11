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

(* data *) 
let global_ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
{kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"}; 
{kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; shozoku="千代田線"}; 
{kanji="赤坂"; kana="あかさか"; romaji="akasaka"; shozoku="千代田線"}; 
{kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidoumae"; shozoku="千代田線"}; 
{kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="千代田線"}; 
{kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="千代田線"}; 
{kanji="二重橋前"; kana="にじゅうばしまえ"; romaji="nijuubasimae"; shozoku="千代田線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="千代田線"}; 
{kanji="新御茶ノ水"; kana="しんおちゃのみず"; romaji="shin-ochanomizu"; shozoku="千代田線"}; 
{kanji="湯島"; kana="ゆしま"; romaji="yushima"; shozoku="千代田線"}; 
{kanji="根津"; kana="ねづ"; romaji="nedu"; shozoku="千代田線"}; 
{kanji="千駄木"; kana="せんだぎ"; romaji="sendagi"; shozoku="千代田線"}; 
{kanji="西日暮里"; kana="にしにっぽり"; romaji="nishinippori"; shozoku="千代田線"}; 
{kanji="町屋"; kana="まちや"; romaji="machiya"; shozoku="千代田線"}; 
{kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenjyu"; shozoku="千代田線"}; 
{kanji="綾瀬"; kana="あやせ"; romaji="ayase"; shozoku="千代田線"}; 
{kanji="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; shozoku="千代田線"}; 
{kanji="浅草"; kana="あさくさ"; romaji="asakusa"; shozoku="銀座線"}; 
{kanji="田原町"; kana="たわらまち"; romaji="tawaramachi"; shozoku="銀座線"}; 
{kanji="稲荷町"; kana="いなりちょう"; romaji="inaricho"; shozoku="銀座線"}; 
{kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="銀座線"}; 
{kanji="上野広小路"; kana="うえのひろこうじ"; romaji="uenohirokoji"; shozoku="銀座線"}; 
{kanji="末広町"; kana="すえひろちょう"; romaji="suehirocho"; shozoku="銀座線"}; 
{kanji="神田"; kana="かんだ"; romaji="kanda"; shozoku="銀座線"}; 
{kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="銀座線"}; 
{kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="銀座線"}; 
{kanji="京橋"; kana="きょうばし"; romaji="kyobashi"; shozoku="銀座線"}; 
{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="銀座線"}; 
{kanji="新橋"; kana="しんばし"; romaji="shinbashi"; shozoku="銀座線"}; 
{kanji="虎ノ門"; kana="とらのもん"; romaji="toranomon"; shozoku="銀座線"}; 
{kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesannou"; shozoku="銀座線"}; 
{kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="銀座線"}; 
{kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyamaicchome"; shozoku="銀座線"}; 
{kanji="外苑前"; kana="がいえんまえ"; romaji="gaienmae"; shozoku="銀座線"}; 
{kanji="表参道"; kana="おもてさんどう"; romaji="omotesando"; shozoku="銀座線"}; 
{kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"}; 
{kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"}; 
{kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="半蔵門線"}; 
{kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyama-itchome"; shozoku="半蔵門線"}; 
{kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="半蔵門線"}; 
{kanji="半蔵門"; kana="はんぞうもん"; romaji="hanzomon"; shozoku="半蔵門線"}; 
{kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="半蔵門線"}; 
{kanji="神保町"; kana="じんぼうちょう"; romaji="jinbocho"; shozoku="半蔵門線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="半蔵門線"}; 
{kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="半蔵門線"}; 
{kanji="水天宮前"; kana="すいてんぐうまえ"; romaji="suitengumae"; shozoku="半蔵門線"}; 
{kanji="清澄白河"; kana="きよすみしらかわ"; romaji="kiyosumi-shirakawa"; shozoku="半蔵門線"}; 
{kanji="住吉"; kana="すみよし"; romaji="sumiyoshi"; shozoku="半蔵門線"}; 
{kanji="錦糸町"; kana="きんしちょう"; romaji="kinshicho"; shozoku="半蔵門線"}; 
{kanji="押上"; kana="おしあげ"; romaji="oshiage"; shozoku="半蔵門線"}; 
{kanji="中目黒"; kana="なかめぐろ"; romaji="nakameguro"; shozoku="日比谷線"}; 
{kanji="恵比寿"; kana="えびす"; romaji="ebisu"; shozoku="日比谷線"}; 
{kanji="広尾"; kana="ひろお"; romaji="hiro"; shozoku="日比谷線"}; 
{kanji="六本木"; kana="ろっぽんぎ"; romaji="roppongi"; shozoku="日比谷線"}; 
{kanji="神谷町"; kana="かみやちょう"; romaji="kamiyacho"; shozoku="日比谷線"}; 
{kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="日比谷線"}; 
{kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="日比谷線"}; 
{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="日比谷線"}; 
{kanji="東銀座"; kana="ひがしぎんざ"; romaji="higashiginza"; shozoku="日比谷線"}; 
{kanji="築地"; kana="つきじ"; romaji="tsukiji"; shozoku="日比谷線"}; 
{kanji="八丁堀"; kana="はっちょうぼり"; romaji="hacchobori"; shozoku="日比谷線"}; 
{kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="日比谷線"}; 
{kanji="人形町"; kana="にんぎょうちょう"; romaji="ningyomachi"; shozoku="日比谷線"}; 
{kanji="小伝馬町"; kana="こでんまちょう"; romaji="kodemmacho"; shozoku="日比谷線"}; 
{kanji="秋葉原"; kana="あきはばら"; romaji="akihabara"; shozoku="日比谷線"}; 
{kanji="仲御徒町"; kana="なかおかちまち"; romaji="nakaokachimachi"; shozoku="日比谷線"}; 
{kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="日比谷線"}; 
{kanji="入谷"; kana="いりや"; romaji="iriya"; shozoku="日比谷線"}; 
{kanji="三ノ輪"; kana="みのわ"; romaji="minowa"; shozoku="日比谷線"}; 
{kanji="南千住"; kana="みなみせんじゅ"; romaji="minamisenju"; shozoku="日比谷線"}; 
{kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenju"; shozoku="日比谷線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
{kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"}; 
{kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"}; 
{kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"}; 
{kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"}; 
{kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"}; 
{kanji="淡路町"; kana="あわじちょう"; romaji="awajicho"; shozoku="丸ノ内線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="丸ノ内線"}; 
{kanji="東京"; kana="とうきょう"; romaji="tokyo"; shozoku="丸ノ内線"}; 
{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="丸ノ内線"}; 
{kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="丸ノ内線"}; 
{kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidomae"; shozoku="丸ノ内線"}; 
{kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="丸ノ内線"}; 
{kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="丸ノ内線"}; 
{kanji="四谷三丁目"; kana="よつやさんちょうめ"; romaji="yotsuyasanchome"; shozoku="丸ノ内線"}; 
{kanji="新宿御苑前"; kana="しんじゅくぎょえんまえ"; romaji="shinjuku-gyoemmae"; shozoku="丸ノ内線"}; 
{kanji="新宿三丁目"; kana="しんじゅくさんちょうめ"; romaji="shinjuku-sanchome"; shozoku="丸ノ内線"}; 
{kanji="新宿"; kana="しんじゅく"; romaji="shinjuku"; shozoku="丸ノ内線"}; 
{kanji="西新宿"; kana="にししんじゅく"; romaji="nishi-shinjuku"; shozoku="丸ノ内線"}; 
{kanji="中野坂上"; kana="なかのさかうえ"; romaji="nakano-sakaue"; shozoku="丸ノ内線"}; 
{kanji="新中野"; kana="しんなかの"; romaji="shin-nakano"; shozoku="丸ノ内線"}; 
{kanji="東高円寺"; kana="ひがしこうえんじ"; romaji="higashi-koenji"; shozoku="丸ノ内線"}; 
{kanji="新高円寺"; kana="しんこうえんじ"; romaji="shin-koenji"; shozoku="丸ノ内線"}; 
{kanji="南阿佐ヶ谷"; kana="みなみあさがや"; romaji="minami-asagaya"; shozoku="丸ノ内線"}; 
{kanji="荻窪"; kana="おぎくぼ"; romaji="ogikubo"; shozoku="丸ノ内線"}; 
{kanji="中野新橋"; kana="なかのしんばし"; romaji="nakano-shimbashi"; shozoku="丸ノ内線"}; 
{kanji="中野富士見町"; kana="なかのふじみちょう"; romaji="nakano-fujimicho"; shozoku="丸ノ内線"}; 
{kanji="方南町"; kana="ほうなんちょう"; romaji="honancho"; shozoku="丸ノ内線"}; 
{kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="南北線"}; 
{kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="南北線"}; 
{kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesanno"; shozoku="南北線"}; 
{kanji="六本木一丁目"; kana="ろっぽんぎいっちょうめ"; romaji="roppongiitchome"; shozoku="南北線"}; 
{kanji="麻布十番"; kana="あざぶじゅうばん"; romaji="azabujuban"; shozoku="南北線"}; 
{kanji="白金高輪"; kana="しろかねたかなわ"; romaji="shirokanetakanawa"; shozoku="南北線"}; 
{kanji="白金台"; kana="しろかねだい"; romaji="shirokanedai"; shozoku="南北線"}; 
{kanji="目黒"; kana="めぐろ"; romaji="meguro"; shozoku="南北線"}; 
{kanji="市ヶ谷"; kana="いちがや"; romaji="ichigaya"; shozoku="南北線"}; 
{kanji="飯田橋"; kana="いいだばし"; romaji="idabashi"; shozoku="南北線"}; 
{kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="南北線"}; 
{kanji="東大前"; kana="とうだいまえ"; romaji="todaimae"; shozoku="南北線"}; 
{kanji="本駒込"; kana="ほんこまごめ"; romaji="honkomagome"; shozoku="南北線"}; 
{kanji="駒込"; kana="こまごめ"; romaji="komagome"; shozoku="南北線"}; 
{kanji="西ヶ原"; kana="にしがはら"; romaji="nishigahara"; shozoku="南北線"}; 
{kanji="王子"; kana="おうじ"; romaji="oji"; shozoku="南北線"}; 
{kanji="王子神谷"; kana="おうじかみや"; romaji="ojikamiya"; shozoku="南北線"}; 
{kanji="志茂"; kana="しも"; romaji="shimo"; shozoku="南北線"}; 
{kanji="赤羽岩淵"; kana="あかばねいわぶち"; romaji="akabaneiwabuchi"; shozoku="南北線"}; 
{kanji="西船橋"; kana="にしふなばし"; romaji="nishi-funabashi"; shozoku="東西線"}; 
{kanji="原木中山"; kana="ばらきなかやま"; romaji="baraki-nakayama"; shozoku="東西線"}; 
{kanji="妙典"; kana="みょうでん"; romaji="myoden"; shozoku="東西線"}; 
{kanji="行徳"; kana="ぎょうとく"; romaji="gyotoku"; shozoku="東西線"}; 
{kanji="南行徳"; kana="みなみぎょうとく"; romaji="minami-gyotoku"; shozoku="東西線"}; 
{kanji="浦安"; kana="うらやす"; romaji="urayasu"; shozoku="東西線"}; 
{kanji="葛西"; kana="かさい"; romaji="kasai"; shozoku="東西線"}; 
{kanji="西葛西"; kana="にしかさい"; romaji="nishi-kasai"; shozoku="東西線"}; 
{kanji="南砂町"; kana="みなみすなまち"; romaji="minami-sunamachi"; shozoku="東西線"}; 
{kanji="東陽町"; kana="とうようちょう"; romaji="touyoucho"; shozoku="東西線"}; 
{kanji="木場"; kana="きば"; romaji="kiba"; shozoku="東西線"}; 
{kanji="門前仲町"; kana="もんぜんなかちょう"; romaji="monzen-nakacho"; shozoku="東西線"}; 
{kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="東西線"}; 
{kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="東西線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="東西線"}; 
{kanji="竹橋"; kana="たけばし"; romaji="takebashi"; shozoku="東西線"}; 
{kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="東西線"}; 
{kanji="飯田橋"; kana="いいだばし"; romaji="iidabashi"; shozoku="東西線"}; 
{kanji="神楽坂"; kana="かぐらざか"; romaji="kagurazaka"; shozoku="東西線"}; 
{kanji="早稲田"; kana="わせだ"; romaji="waseda"; shozoku="東西線"}; 
{kanji="高田馬場"; kana="たかだのばば"; romaji="takadanobaba"; shozoku="東西線"}; 
{kanji="落合"; kana="おちあい"; romaji="ochiai"; shozoku="東西線"}; 
{kanji="中野"; kana="なかの"; romaji="nakano"; shozoku="東西線"}; 
{romaji="shinkiba"; kana="しんきば"; kanji="新木場"; shozoku="有楽町線"}; 
{romaji="tatsumi"; kana="たつみ"; kanji="辰巳"; shozoku="有楽町線"}; 
{romaji="toyosu"; kana="とよす"; kanji="豊洲"; shozoku="有楽町線"}; 
{romaji="tsukishima"; kana="つきしま"; kanji="月島"; shozoku="有楽町線"}; 
{romaji="shintomityou"; kana="しんとみちょう"; kanji="新富町"; shozoku="有楽町線"}; 
{romaji="ginzaittyoume"; kana="ぎんざいっちょうめ"; kanji="銀座一丁目"; shozoku="有楽町線"}; 
{romaji="yuurakutyou"; kana="ゆうらくちょう"; kanji="有楽町"; shozoku="有楽町線"}; 
{romaji="sakuradamon"; kana="さくらだもん"; kanji="桜田門"; shozoku="有楽町線"}; 
{romaji="nagatacho"; kana="ながたちょう"; kanji="永田町"; shozoku="有楽町線"}; 
{romaji="koujimachi"; kana="こうじまち"; kanji="麹町"; shozoku="有楽町線"}; 
{romaji="ichigaya"; kana="いちがや"; kanji="市ヶ谷"; shozoku="有楽町線"}; 
{romaji="iidabashi"; kana="いいだばし"; kanji="飯田橋"; shozoku="有楽町線"}; 
{kanji="江戸川橋"; kana="えどがわばし"; romaji="edogawabasi"; shozoku="有楽町線"}; 
{kanji="護国寺"; kana="ごこくじ"; romaji="gokokuji"; shozoku="有楽町線"}; 
{kanji="東池袋"; kana="ひがしいけぶくろ"; romaji="higasiikebukuro"; shozoku="有楽町線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 
{kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"}; 
{kanji="千川"; kana="せんかわ"; romaji="senkawa"; shozoku="有楽町線"}; 
{kanji="小竹向原"; kana="こたけむかいはら"; romaji="kotakemukaihara"; shozoku="有楽町線"}; 
{kanji="氷川台"; kana="ひかわだい"; romaji="hikawadai"; shozoku="有楽町線"}; 
{kanji="平和台"; kana="へいわだい"; romaji="heiwadai"; shozoku="有楽町線"}; 
{kanji="営団赤塚"; kana="えいだんあかつか"; romaji="eidanakakuka"; shozoku="有楽町線"}; 
{kanji="営団成増"; kana="えいだんなります"; romaji="eidannarimasu"; shozoku="有楽町線"}; 
{kanji="和光市"; kana="わこうし"; romaji="wakousi"; shozoku="有楽町線"}; 
] 
let global_ekikan_list = [ 
{kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2}; 
{kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="明治神宮前"; shuten="表参道"; keiyu="千代田線"; kyori=0.9; jikan=2}; 
{kiten="表参道"; shuten="乃木坂"; keiyu="千代田線"; kyori=1.4; jikan=3}; 
{kiten="乃木坂"; shuten="赤坂"; keiyu="千代田線"; kyori=1.1; jikan=2}; 
{kiten="赤坂"; shuten="国会議事堂前"; keiyu="千代田線"; kyori=0.8; jikan=1}; 
{kiten="国会議事堂前"; shuten="霞ヶ関"; keiyu="千代田線"; kyori=0.7; jikan=1}; 
{kiten="霞ヶ関"; shuten="日比谷"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="日比谷"; shuten="二重橋前"; keiyu="千代田線"; kyori=0.7; jikan=1}; 
{kiten="二重橋前"; shuten="大手町"; keiyu="千代田線"; kyori=0.7; jikan=1}; 
{kiten="大手町"; shuten="新御茶ノ水"; keiyu="千代田線"; kyori=1.3; jikan=2}; 
{kiten="新御茶ノ水"; shuten="湯島"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="湯島"; shuten="根津"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="根津"; shuten="千駄木"; keiyu="千代田線"; kyori=1.0; jikan=2}; 
{kiten="千駄木"; shuten="西日暮里"; keiyu="千代田線"; kyori=0.9; jikan=1}; 
{kiten="西日暮里"; shuten="町屋"; keiyu="千代田線"; kyori=1.7; jikan=2}; 
{kiten="町屋"; shuten="北千住"; keiyu="千代田線"; kyori=2.6; jikan=3}; 
{kiten="北千住"; shuten="綾瀬"; keiyu="千代田線"; kyori=2.5; jikan=3}; 
{kiten="綾瀬"; shuten="北綾瀬"; keiyu="千代田線"; kyori=2.1; jikan=4}; 
{kiten="浅草"; shuten="田原町"; keiyu="銀座線"; kyori=0.8; jikan=2}; 
{kiten="田原町"; shuten="稲荷町"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="稲荷町"; shuten="上野"; keiyu="銀座線"; kyori=0.7; jikan=2}; 
{kiten="上野"; shuten="上野広小路"; keiyu="銀座線"; kyori=0.5; jikan=2}; 
{kiten="上野広小路"; shuten="末広町"; keiyu="銀座線"; kyori=0.6; jikan=1}; 
{kiten="末広町"; shuten="神田"; keiyu="銀座線"; kyori=1.1; jikan=2}; 
{kiten="神田"; shuten="三越前"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="三越前"; shuten="日本橋"; keiyu="銀座線"; kyori=0.6; jikan=2}; 
{kiten="日本橋"; shuten="京橋"; keiyu="銀座線"; kyori=0.7; jikan=2}; 
{kiten="京橋"; shuten="銀座"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="銀座"; shuten="新橋"; keiyu="銀座線"; kyori=0.9; jikan=2}; 
{kiten="新橋"; shuten="虎ノ門"; keiyu="銀座線"; kyori=0.8; jikan=2}; 
{kiten="虎ノ門"; shuten="溜池山王"; keiyu="銀座線"; kyori=0.6; jikan=2}; 
{kiten="溜池山王"; shuten="赤坂見附"; keiyu="銀座線"; kyori=0.9; jikan=2}; 
{kiten="赤坂見附"; shuten="青山一丁目"; keiyu="銀座線"; kyori=1.3; jikan=2}; 
{kiten="青山一丁目"; shuten="外苑前"; keiyu="銀座線"; kyori=0.7; jikan=2}; 
{kiten="外苑前"; shuten="表参道"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="表参道"; shuten="渋谷"; keiyu="銀座線"; kyori=1.3; jikan=1}; 
{kiten="渋谷"; shuten="表参道"; keiyu="半蔵門線"; kyori=1.3; jikan=2}; 
{kiten="表参道"; shuten="青山一丁目"; keiyu="半蔵門線"; kyori=1.4; jikan=2}; 
{kiten="青山一丁目"; shuten="永田町"; keiyu="半蔵門線"; kyori=1.3; jikan=2}; 
{kiten="永田町"; shuten="半蔵門"; keiyu="半蔵門線"; kyori=1.0; jikan=2}; 
{kiten="半蔵門"; shuten="九段下"; keiyu="半蔵門線"; kyori=1.6; jikan=2}; 
{kiten="九段下"; shuten="神保町"; keiyu="半蔵門線"; kyori=0.4; jikan=1}; 
{kiten="神保町"; shuten="大手町"; keiyu="半蔵門線"; kyori=1.7; jikan=3}; 
{kiten="大手町"; shuten="三越前"; keiyu="半蔵門線"; kyori=0.7; jikan=1}; 
{kiten="三越前"; shuten="水天宮前"; keiyu="半蔵門線"; kyori=1.3; jikan=2}; 
{kiten="水天宮前"; shuten="清澄白河"; keiyu="半蔵門線"; kyori=1.7; jikan=3}; 
{kiten="清澄白河"; shuten="住吉"; keiyu="半蔵門線"; kyori=1.9; jikan=3}; 
{kiten="住吉"; shuten="錦糸町"; keiyu="半蔵門線"; kyori=1.; jikan=2}; 
{kiten="錦糸町"; shuten="押上"; keiyu="半蔵門線"; kyori=1.4; jikan=2}; 
{kiten="中目黒"; shuten="恵比寿"; keiyu="日比谷線"; kyori=1.; jikan=2}; 
{kiten="恵比寿"; shuten="広尾"; keiyu="日比谷線"; kyori=1.5; jikan=3}; 
{kiten="広尾"; shuten="六本木"; keiyu="日比谷線"; kyori=1.7; jikan=3}; 
{kiten="六本木"; shuten="神谷町"; keiyu="日比谷線"; kyori=1.5; jikan=3}; 
{kiten="神谷町"; shuten="霞ヶ関"; keiyu="日比谷線"; kyori=1.3; jikan=2}; 
{kiten="霞ヶ関"; shuten="日比谷"; keiyu="日比谷線"; kyori=1.2; jikan=2}; 
{kiten="日比谷"; shuten="銀座"; keiyu="日比谷線"; kyori=0.4; jikan=1}; 
{kiten="銀座"; shuten="東銀座"; keiyu="日比谷線"; kyori=0.4; jikan=1}; 
{kiten="東銀座"; shuten="築地"; keiyu="日比谷線"; kyori=0.6; jikan=2}; 
{kiten="築地"; shuten="八丁堀"; keiyu="日比谷線"; kyori=1.; jikan=2}; 
{kiten="八丁堀"; shuten="茅場町"; keiyu="日比谷線"; kyori=0.5; jikan=1}; 
{kiten="茅場町"; shuten="人形町"; keiyu="日比谷線"; kyori=0.9; jikan=2}; 
{kiten="人形町"; shuten="小伝馬町"; keiyu="日比谷線"; kyori=0.6; jikan=1}; 
{kiten="小伝馬町"; shuten="秋葉原"; keiyu="日比谷線"; kyori=0.9; jikan=2}; 
{kiten="秋葉原"; shuten="仲御徒町"; keiyu="日比谷線"; kyori=1.; jikan=1}; 
{kiten="仲御徒町"; shuten="上野"; keiyu="日比谷線"; kyori=0.5; jikan=1}; 
{kiten="上野"; shuten="入谷"; keiyu="日比谷線"; kyori=1.2; jikan=2}; 
{kiten="入谷"; shuten="三ノ輪"; keiyu="日比谷線"; kyori=1.2; jikan=2}; 
{kiten="三ノ輪"; shuten="南千住"; keiyu="日比谷線"; kyori=0.8; jikan=2}; 
{kiten="南千住"; shuten="北千住"; keiyu="日比谷線"; kyori=1.8; jikan=3}; 
{kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3}; 
{kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}; 
{kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2}; 
{kiten="後楽園"; shuten="本郷三丁目"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="本郷三丁目"; shuten="御茶ノ水"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="御茶ノ水"; shuten="淡路町"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="淡路町"; shuten="大手町"; keiyu="丸ノ内線"; kyori=0.9; jikan=2}; 
{kiten="大手町"; shuten="東京"; keiyu="丸ノ内線"; kyori=0.6; jikan=1}; 
{kiten="東京"; shuten="銀座"; keiyu="丸ノ内線"; kyori=1.1; jikan=2}; 
{kiten="銀座"; shuten="霞ヶ関"; keiyu="丸ノ内線"; kyori=1.0; jikan=2}; 
{kiten="霞ヶ関"; shuten="国会議事堂前"; keiyu="丸ノ内線"; kyori=0.7; jikan=1}; 
{kiten="国会議事堂前"; shuten="赤坂見附"; keiyu="丸ノ内線"; kyori=0.9; jikan=2}; 
{kiten="赤坂見附"; shuten="四ツ谷"; keiyu="丸ノ内線"; kyori=1.3; jikan=2}; 
{kiten="四ツ谷"; shuten="四谷三丁目"; keiyu="丸ノ内線"; kyori=1.0; jikan=2}; 
{kiten="四谷三丁目"; shuten="新宿御苑前"; keiyu="丸ノ内線"; kyori=0.9; jikan=1}; 
{kiten="新宿御苑前"; shuten="新宿三丁目"; keiyu="丸ノ内線"; kyori=0.7; jikan=1}; 
{kiten="新宿三丁目"; shuten="新宿"; keiyu="丸ノ内線"; kyori=0.3; jikan=1}; 
{kiten="新宿"; shuten="西新宿"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="西新宿"; shuten="中野坂上"; keiyu="丸ノ内線"; kyori=1.1; jikan=2}; 
{kiten="中野坂上"; shuten="新中野"; keiyu="丸ノ内線"; kyori=1.1; jikan=2}; 
{kiten="新中野"; shuten="東高円寺"; keiyu="丸ノ内線"; kyori=1.0; jikan=1}; 
{kiten="東高円寺"; shuten="新高円寺"; keiyu="丸ノ内線"; kyori=0.9; jikan=1}; 
{kiten="新高円寺"; shuten="南阿佐ヶ谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}; 
{kiten="南阿佐ヶ谷"; shuten="荻窪"; keiyu="丸ノ内線"; kyori=1.5; jikan=2}; 
{kiten="中野坂上"; shuten="中野新橋"; keiyu="丸ノ内線"; kyori=1.3; jikan=2}; 
{kiten="中野新橋"; shuten="中野富士見町"; keiyu="丸ノ内線"; kyori=0.6; jikan=1}; 
{kiten="中野富士見町"; shuten="方南町"; keiyu="丸ノ内線"; kyori=1.3; jikan=2}; 
{kiten="市ヶ谷"; shuten="四ツ谷"; keiyu="南北線"; kyori=1.0; jikan=2}; 
{kiten="四ツ谷"; shuten="永田町"; keiyu="南北線"; kyori=1.3; jikan=3}; 
{kiten="永田町"; shuten="溜池山王"; keiyu="南北線"; kyori=0.9; jikan=1}; 
{kiten="溜池山王"; shuten="六本木一丁目"; keiyu="南北線"; kyori=0.9; jikan=2}; 
{kiten="六本木一丁目"; shuten="麻布十番"; keiyu="南北線"; kyori=1.2; jikan=2}; 
{kiten="麻布十番"; shuten="白金高輪"; keiyu="南北線"; kyori=1.3; jikan=2}; 
{kiten="白金高輪"; shuten="白金台"; keiyu="南北線"; kyori=1.0; jikan=2}; 
{kiten="白金台"; shuten="目黒"; keiyu="南北線"; kyori=1.3; jikan=2}; 
{kiten="市ヶ谷"; shuten="飯田橋"; keiyu="南北線"; kyori=1.1 ; jikan=2}; 
{kiten="飯田橋"; shuten="後楽園"; keiyu="南北線"; kyori=1.4 ; jikan=2}; 
{kiten="後楽園"; shuten="東大前"; keiyu="南北線"; kyori=1.3 ; jikan=3}; 
{kiten="東大前"; shuten="本駒込"; keiyu="南北線"; kyori=0.9 ; jikan=2}; 
{kiten="本駒込"; shuten="駒込"; keiyu="南北線"; kyori=1.4; jikan=2}; 
{kiten="駒込"; shuten="西ヶ原"; keiyu="南北線"; kyori=1.4; jikan=2}; 
{kiten="西ヶ原"; shuten="王子"; keiyu="南北線"; kyori=1.0; jikan=2}; 
{kiten="王子"; shuten="王子神谷"; keiyu="南北線"; kyori=1.2; jikan=2}; 
{kiten="王子神谷"; shuten="志茂"; keiyu="南北線"; kyori=1.6; jikan=3}; 
{kiten="志茂"; shuten="赤羽岩淵"; keiyu="南北線"; kyori=1.1; jikan=2}; 
{kiten="西船橋" ; shuten="原木中山"; keiyu="東西線"; kyori=1.9; jikan=3}; 
{kiten="原木中山"; shuten="妙典"; keiyu="東西線"; kyori=2.1 ; jikan=2}; 
{kiten="妙典"; shuten="行徳"; keiyu="東西線"; kyori=1.3 ; jikan=2}; 
{kiten="行徳"; shuten="南行徳"; keiyu="東西線"; kyori=1.5 ; jikan=2}; 
{kiten="南行徳"; shuten="浦安" ; keiyu="東西線"; kyori=1.2 ; jikan=2}; 
{kiten="浦安" ; shuten="葛西"; keiyu="東西線"; kyori=1.9 ; jikan=2}; 
{kiten="葛西"; shuten="西葛西"; keiyu="東西線"; kyori=1.2 ; jikan=2}; 
{kiten="西葛西"; shuten="南砂町"; keiyu="東西線"; kyori=2.7 ; jikan=2}; 
{kiten="南砂町"; shuten="東陽町"; keiyu="東西線"; kyori=1.2 ; jikan=2}; 
{kiten="東陽町"; shuten="木場" ; keiyu="東西線"; kyori=0.9 ; jikan=1}; 
{kiten="木場"; shuten="門前仲町"; keiyu="東西線"; kyori=1.1 ; jikan=1}; 
{kiten="門前仲町"; shuten="茅場町"; keiyu="東西線"; kyori=1.8 ; jikan=2}; 
{kiten="茅場町"; shuten="日本橋"; keiyu="東西線"; kyori=0.5 ; jikan=1}; 
{kiten="日本橋"; shuten="大手町"; keiyu="東西線"; kyori=0.8 ; jikan=1}; 
{kiten="大手町"; shuten="竹橋"; keiyu="東西線"; kyori=1.0; jikan=2}; 
{kiten="竹橋"; shuten="九段下"; keiyu="東西線"; kyori=1.0; jikan=1}; 
{kiten="九段下"; shuten="飯田橋"; keiyu="東西線"; kyori=0.7; jikan=1}; 
{kiten="飯田橋"; shuten="神楽坂"; keiyu="東西線"; kyori=1.2; jikan=2}; 
{kiten="神楽坂"; shuten="早稲田"; keiyu="東西線"; kyori=1.2; jikan=2}; 
{kiten="早稲田"; shuten="高田馬場"; keiyu="東西線"; kyori=1.7; jikan=3}; 
{kiten="高田馬場"; shuten="落合"; keiyu="東西線"; kyori=1.9; jikan=3}; 
{kiten="落合"; shuten="中野"; keiyu="東西線"; kyori=2.0; jikan=3}; 
{kiten="新木場"; shuten="辰巳"; keiyu="有楽町線"; kyori=1.5; jikan=2}; 
{kiten="辰巳"; shuten="豊洲"; keiyu="有楽町線"; kyori=1.7; jikan=2}; 
{kiten="豊洲"; shuten="月島"; keiyu="有楽町線"; kyori=1.4; jikan=2}; 
{kiten="月島"; shuten="新富町"; keiyu="有楽町線"; kyori=1.3; jikan=2}; 
{kiten="新富町"; shuten="銀座一丁目"; keiyu="有楽町線"; kyori=0.7; jikan=1}; 
{kiten="銀座一丁目"; shuten="有楽町"; keiyu="有楽町線"; kyori=0.5; jikan=1}; 
{kiten="有楽町"; shuten="桜田門"; keiyu="有楽町線"; kyori=1.0; jikan=1}; 
{kiten="桜田門"; shuten="永田町"; keiyu="有楽町線"; kyori=0.9; jikan=2}; 
{kiten="永田町"; shuten="麹町"; keiyu="有楽町線"; kyori=0.9; jikan=1}; 
{kiten="麹町"; shuten="市ヶ谷"; keiyu="有楽町線"; kyori=0.9; jikan=1}; 
{kiten="市ヶ谷"; shuten="飯田橋"; keiyu="有楽町線"; kyori=1.1; jikan=2}; 
{kiten="飯田橋"; shuten="江戸川橋"; keiyu="有楽町線"; kyori=1.6; jikan=3}; 
{kiten="江戸川橋"; shuten="護国寺"; keiyu="有楽町線"; kyori=1.3; jikan=2}; 
{kiten="護国寺"; shuten="東池袋"; keiyu="有楽町線"; kyori=1.1; jikan=2}; 
{kiten="東池袋"; shuten="池袋"; keiyu="有楽町線"; kyori=2.0; jikan=2}; 
{kiten="池袋"; shuten="要町"; keiyu="有楽町線"; kyori=1.2; jikan=2}; 
{kiten="要町"; shuten="千川"; keiyu="有楽町線"; kyori=1.0; jikan=1}; 
{kiten="千川"; shuten="小竹向原"; keiyu="有楽町線"; kyori=1.0; jikan=2}; 
{kiten="小竹向原"; shuten="氷川台"; keiyu="有楽町線"; kyori=1.5; jikan=2}; 
{kiten="氷川台"; shuten="平和台"; keiyu="有楽町線"; kyori=1.4; jikan=2}; 
{kiten="平和台"; shuten="営団赤塚"; keiyu="有楽町線"; kyori=1.8; jikan=2}; 
{kiten="営団赤塚"; shuten="営団成増"; keiyu="有楽町線"; kyori=1.5; jikan=2}; 
{kiten="営団成増"; shuten="和光市"; keiyu="有楽町線"; kyori=2.1; jikan=3}; 
] 

(* 18.6 *)
exception No_such_station of string

(* 10.10, 18.7 *)
let rec romaji_to_kanji (name: string) (l: ekimei_t list) : string = match l with
    [] -> raise (No_such_station (name))
    | {kanji = kj; kana = kn; romaji = r; shozoku = s}::xs ->
        if name = r then kj else romaji_to_kanji name xs

(* 10.10 tests *)
let test_10_10_1 = (romaji_to_kanji "myogadani" global_ekimei_list) = "茗荷谷"
let test_10_10_2 = romaji_to_kanji "shinjuku" global_ekimei_list = "新宿"

(* 10.11, 18.4 *)
let rec get_ekikan_kyori (s1: string) (s2: string) (l: ekikan_t list) : float = match l with
    [] -> raise Not_found
    | {kiten = h; shuten = t; keiyu = e; kyori = dist; jikan = dur}::xs ->
        if (h = s1 && t = s2) || (h = s2 && t = s1) then dist else get_ekikan_kyori s1 s2 xs

(* 10.11 tests *)
let test_10_11_1 = get_ekikan_kyori "飯田橋" "神楽坂" global_ekikan_list = 1.2
let test_10_11_2 = get_ekikan_kyori "中野" "落合" global_ekikan_list = 2.0
(* let test_10_11_3 = get_ekikan_kyori "新橋" "中野" global_ekikan_list = infinity *)

(* 10.12 *)
let rec kyori_wo_hyoji (s1: string) (s2: string) : string = 
    try
        let station_1 = romaji_to_kanji s1 global_ekimei_list in
        let station_2 = romaji_to_kanji s2 global_ekimei_list in
        let dist = get_ekikan_kyori station_1 station_2 global_ekikan_list in
            station_1 ^ "から" ^ station_2 ^ "までは" ^ (string_of_float dist) ^ "kmです"
    with 
        Not_found -> romaji_to_kanji s1 global_ekimei_list ^ "と" ^ romaji_to_kanji s2 global_ekimei_list ^ "はつながっていません"
        | No_such_station (n) -> n ^ "という駅名は存在しません"

(* 10.12 tests *)
let test_10_12_1 = kyori_wo_hyoji "iidabashi" "kagurazaka" = "飯田橋から神楽坂までは1.2kmです"
let test_10_12_2 = kyori_wo_hyoji "nakano" "ochiai" = "中野から落合までは2.kmです"
let test_10_12_3 = kyori_wo_hyoji "shinbashi" "nakano" = "新橋と中野はつながっていません"
let test_10_12_4 = kyori_wo_hyoji "ryogoku" "ueno" = "ryogokuという駅名は存在しません"
let test_10_12_5 = kyori_wo_hyoji "ueno" "yoyogi" = "yoyogiという駅名は存在しません" 

(* 12.1 *)
type eki_t = {
    namae: string;
    saitan_kyori: float;
    temae_list: string list;
}

(* 12.2 *)
let make_eki_list (l: ekimei_t list) : eki_t list = 
    List.map (fun ekimei -> match ekimei with 
        { kanji = kj; kana = kn; romaji = r; shozoku = s} -> 
            { namae = kj; saitan_kyori = infinity; temae_list = []}) l

(* 12.2 test *)
let test_12_2_1 = make_eki_list [] = []
let test_12_2_2 = make_eki_list [{ kanji = "荻窪"; kana = "おぎくぼ"; romaji = "ogikubo"; shozoku = "丸ノ内線" }] =
                                    [{ namae = "荻窪"; saitan_kyori = infinity; temae_list = []}]
let test_12_2_3 = make_eki_list [
    { kanji = "荻窪"; kana = "おぎくぼ"; romaji = "ogikubo"; shozoku = "丸ノ内線" };
    { kanji = "渋谷"; kana = "しぶや"; romaji = "shibuya"; shozoku = "銀座線" };
    { kanji = "王子"; kana = "おうじ"; romaji = "oji"; shozoku = "南北線" }
] = [
    { namae = "荻窪"; saitan_kyori = infinity; temae_list = [] };
    { namae = "渋谷"; saitan_kyori = infinity; temae_list = [] };
    { namae = "王子"; saitan_kyori = infinity; temae_list = [] }
]

let eki_list = make_eki_list global_ekimei_list

(* 12.3 *)
let shokika (l: eki_t list) (target: string) : eki_t list = 
    List.map (fun sta -> match sta with
        { namae = n; saitan_kyori = d; temae_list = l} -> 
            if n = target then { namae = n; saitan_kyori = 0.; temae_list = n :: l } else sta) l


(* 12.3 tests *)
let rec validate_12_3 (l: eki_t list) (target: string) : bool = match l with
    [] -> false
    | { namae = n; saitan_kyori = d; temae_list = l }::xs -> 
        if n = target then (d = 0. && l = [target]) else validate_12_3 xs target

let test_12_3_1 = validate_12_3 (shokika [] "") "" = false
let test_12_3_2 = validate_12_3 (shokika eki_list "荻窪") "荻窪" = true
let test_12_3_4 = validate_12_3 (shokika eki_list "新宿") "新宿" = true
let test_12_3_4 = validate_12_3 (shokika eki_list "荻窪") "王子" = false

(* 12.4 *)
let rec insert (l: ekimei_t list) (s: ekimei_t) : ekimei_t list = match l with
    [] -> [s]
    | ({ kanji = kj; kana = kn;  romaji = ro; shozoku = sh } as x)::xs ->
        match s with { kanji = skj; kana = skn; romaji = sro; shozoku = ssh } ->
            if kn < skn then x :: insert xs s
                else if kn = skn then insert xs s
                else s :: l

let rec seiretsu (l: ekimei_t list) : ekimei_t list = match l with
    [] -> []
    | x::xs -> insert (seiretsu xs) x

(* 12.4 tests *)
let test_12_4_1 = seiretsu [
    {kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenjyu"; shozoku="千代田線"};
    {kanji="綾瀬"; kana="あやせ"; romaji="ayase"; shozoku="千代田線"};
    {kanji="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; shozoku="千代田線"}
] = [
    {kanji="綾瀬"; kana="あやせ"; romaji="ayase"; shozoku="千代田線"};
    {kanji="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; shozoku="千代田線"};
    {kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenjyu"; shozoku="千代田線"}
]
let test_12_4_2 = seiretsu [
    {kanji="外苑前"; kana="がいえんまえ"; romaji="gaienmae"; shozoku="銀座線"};
    {kanji="表参道"; kana="おもてさんどう"; romaji="omotesando"; shozoku="銀座線"};
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"};
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"};
    {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="半蔵門線"};
    {kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyama-itchome"; shozoku="半蔵門線"};
    {kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="半蔵門線"}
] = [
    {kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyama-itchome"; shozoku="半蔵門線"};
    {kanji="表参道"; kana="おもてさんどう"; romaji="omotesando"; shozoku="銀座線"};
    {kanji="外苑前"; kana="がいえんまえ"; romaji="gaienmae"; shozoku="銀座線"};
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"};
    {kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="半蔵門線"}
]
let test_12_4_3 = List.length (seiretsu global_ekimei_list) < List.length global_ekimei_list

(* 13.6, 13.7, 14.7, 18.5 *)
let koushin (p: eki_t) (v: eki_t list) (g: ekikan_t list) : eki_t list = 
    List.map (
        (fun (p: eki_t) (q: eki_t) ->
        match p with { namae = pn; saitan_kyori = pd; temae_list = pl} ->
        match q with { namae = qn; saitan_kyori = qd; temae_list = ql} ->
        try
            let dist = get_ekikan_kyori pn qn g in
                if dist +. pd < qd
                    then { namae = qn; saitan_kyori = dist +. pd; temae_list = qn::pl }
                    else q
        with Not_found -> q)
        p
    ) v

(* 13.6 tests
let test_13_6_1 = koushin1 { namae = "渋谷"; saitan_kyori = 0.; temae_list = ["渋谷"] } 
                           { namae = "表参道"; saitan_kyori = infinity; temae_list = [] }
                           = { namae = "表参道"; saitan_kyori = 1.3; temae_list = ["表参道"; "渋谷"] }
let test_13_6_2 = koushin1 { namae = "渋谷"; saitan_kyori = 0.; temae_list = ["渋谷"] } 
                           { namae = "新宿"; saitan_kyori = infinity; temae_list = [] }
                           = { namae = "新宿"; saitan_kyori = infinity; temae_list = [] }
let test_13_6_3 = koushin1 { namae = "池袋"; saitan_kyori = 14.; temae_list = ["池袋"; "要町"; "千川"] } 
                           { namae = "新大塚"; saitan_kyori = 20.; temae_list = ["foo"; "bar"] }
                           = { namae = "新大塚"; saitan_kyori = 15.8; temae_list = ["新大塚"; "池袋"; "要町"; "千川"] }
let test_13_6_3 = koushin1 { namae = "池袋"; saitan_kyori = 14.; temae_list = ["池袋"; "要町"; "千川"] } 
                           { namae = "新大塚"; saitan_kyori = 15.5; temae_list = ["foo"; "bar"] }
                           = { namae = "新大塚"; saitan_kyori = 15.5; temae_list = ["foo"; "bar"] }
*)
(* test data *)
let s1 = { namae = "護国寺"; saitan_kyori = infinity; temae_list = [] }
let s2 = { namae = "江戸川橋"; saitan_kyori = 12.; temae_list = ["江戸川橋"; "飯田橋"] }
let s3 = { namae = "飯田橋"; saitan_kyori = 0.; temae_list = ["c"] }
let s4 = { namae = "市ヶ谷"; saitan_kyori = infinity; temae_list = [] }

(* 13.7 tests *)
let test_13_7_1 = koushin s1 [] global_ekikan_list = []
let test_13_7_2 = koushin s2 [s1; s2; s3; s4] global_ekikan_list
                = [{ namae = "護国寺"; saitan_kyori = 13.3; temae_list = ["護国寺"; "江戸川橋"; "飯田橋"] }; s2; s3; s4]

(* 14.12 *)
let make_initial_eki_list (l: ekimei_t list) (target: string) : eki_t list =
    List.map (fun ekimei -> match ekimei with
        { kanji = kj; kana = kn; romaji = r; shozoku = s} ->
            if kj = target
                then { namae = kj; saitan_kyori = 0.; temae_list = [kj] }
                else { namae = kj; saitan_kyori = infinity; temae_list = []}) l

(* 14.12 tests *)
let test_14_12_1 = validate_12_3 (make_initial_eki_list [] "") "" = false
let test_14_12_2 = validate_12_3 (make_initial_eki_list global_ekimei_list "荻窪") "荻窪" = true
let test_14_12_4 = validate_12_3 (make_initial_eki_list global_ekimei_list "新宿") "新宿" = true
let test_14_12_4 = validate_12_3 (make_initial_eki_list global_ekimei_list "荻窪") "王子" = false

(* 15.4, 15.5 *)
let saitan_wo_bunri (e: eki_t) (l: eki_t list) : eki_t*(eki_t list) =
    List.fold_right (fun s (p, v) -> 
        match s with { namae = sn; saitan_kyori = sd; temae_list = st} -> 
        match p with { namae = pn; saitan_kyori = pd; temae_list = pt} -> 
            if sd < pd then (s, p::v) else (p, s::v)) l (e, [])

(* 15.4 tests *)
let s15_1 = { namae = "a"; saitan_kyori = 12.3; temae_list = ["z"; "x"]}
let s15_2 = { namae = "b"; saitan_kyori = 2.3; temae_list = ["n"]}
let s15_3 = { namae = "c"; saitan_kyori = 36.4; temae_list = ["v"]}
let s15_4 = { namae = "d"; saitan_kyori = 5.6; temae_list = ["c"; "v"]}
let s15_5 = { namae = "e"; saitan_kyori = 18.4; temae_list = ["r"; "t"; "y"]}

let rec validate_15_4 (p, v) = 
    match p with { namae = pn; saitan_kyori = pd; temae_list = pt } ->
    match v with
        [] -> true
        | { namae = xn; saitan_kyori = xd; temae_list = xt }::xs -> 
            if pd <= xd then validate_15_4 (p, xs) else false

(*let test_15_4_1 = saitan_wo_bunri [] = ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])*)
let test_15_4_2 = validate_15_4 (saitan_wo_bunri s15_1 [s15_2; s15_3; s15_4; s15_5])
let test_15_4_3 = validate_15_4 (saitan_wo_bunri s15_1 [s15_3; s15_1])

(* 16.4 *)
(* 停止性: saitan_wo_bunri の出力の new_v は元の v より短くなり最終的に [] となる *)
let rec dijkstra_main (v: eki_t list) (g: ekikan_t list) : eki_t list = match v with
        [] -> []
        | x::xs -> let (new_p, new_v) = saitan_wo_bunri x xs in
                new_p :: dijkstra_main (koushin new_p new_v g) g

(* 16.5 *)
let dijkstra (start: string) (destination: string) : eki_t = try
    let network = seiretsu global_ekimei_list in
    let dest_kj = romaji_to_kanji destination network in
    let rec find l = match l with
        [] -> { namae = ""; saitan_kyori = infinity; temae_list = []}
        | ({ namae = n; saitan_kyori = d; temae_list = l} as x)::xs -> 
            if n = dest_kj then x  else find xs in
    find (
        dijkstra_main
            (make_initial_eki_list network (romaji_to_kanji start network))
            global_ekikan_list)
    with No_such_station (n) -> { namae = n; saitan_kyori = infinity; temae_list = [] }

(* tests *)
let test_dijkstra_1 = dijkstra "shinjuku" "meguro"
let test_dijkstra_2 = dijkstra "asakusa" "ikebukuro"
let test_dijkstra_3 = dijkstra "kitaayase" "wakousi"
let test_dijkstra_4 = dijkstra "foo" "bar"
let test_dijkstra_5 = dijkstra "shibuya" ""
let test_dijkstra_6 = dijkstra "" "nedu"

(* from support page: http://pllab.is.ocha.ac.jp/~asai/book-data/ex16_5.ml *)
let test1 = dijkstra "shibuya" "gokokuji"
let test2 = dijkstra "myogadani" "meguro"

(* 17.10 *)
type ekikan_tree_t = Empty
    | Node of string * (string * float) list * ekikan_tree_t * ekikan_tree_t

(* 17.11, 18.3 *)
let rec assoc target (l: ('a * 'b) list) = match l with
    [] -> raise Not_found
    | (n, d)::xs -> if n = target then d else assoc target xs
(* 17.11 tests *)
let test_17_11_1 = assoc "b" [("a", 1.); ("b", 2.); ("c", 3.)] = 2.
(* let test_17_11_2 = assoc "z" [("a", 1.); ("b", 2.); ("c", 3.)] = infinity *)

(* 17.12 *)
let insert_ekikan (e: ekikan_t) (tree: ekikan_tree_t) : ekikan_tree_t = match e with
    { kiten = h; shuten = t; keiyu = line; kyori = dist; jikan = dur} -> 
    let rec upsert_node (n: string) (pair: string * float) (target: ekikan_tree_t) : ekikan_tree_t = match target with
        Empty -> Node (n, [pair], Empty, Empty)
        | Node (tn, lst, l, r) -> if n = tn then Node(tn, pair::lst, l, r)
                                    else if n < tn then Node (tn, lst, upsert_node n pair l, r)
                                    else Node (tn, lst, l, upsert_node n pair r) in
    let k_inserted = upsert_node h (t, dist) tree in
        upsert_node t (h, dist) k_inserted
(* 17.12 tests *)
let test_tree1 = 
    Node ("根津", [("千駄木", 1.); ("湯島", 1.2)],
        Empty,
        Empty)
let test_tree2 = 
    Node ("根津", [("千駄木", 1.); ("湯島", 1.2)],
        Node ("新大塚", [("池袋", 1.8)],
            Empty,
            Empty),
        Empty)
let test_edge = { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = ""; kyori = 1.2; jikan = 2 }
let test_17_12_1 = insert_ekikan test_edge Empty
let test_17_12_2 = insert_ekikan test_edge test_tree1
let test_17_12_3 = insert_ekikan test_edge test_tree2

(* 17.13 *)
let inserts_ekikan (t: ekikan_tree_t) (l: ekikan_t list) : ekikan_tree_t = List.fold_right insert_ekikan l t

(* 17.14, 18.4 *)
let rec get_ekikan_kyori_improved (s1: string) (s2: string) (t: ekikan_tree_t) : float = 
    match t with
        Empty -> raise Not_found
        | Node (n, lst, l, r) -> if n = s1 then List.assoc s2 lst
                                    else if n > s1 then get_ekikan_kyori_improved s1 s2 l
                                    else get_ekikan_kyori_improved s1 s2 r
(* 17.14 tests *)
let test_17_14_1 = get_ekikan_kyori_improved "飯田橋" "神楽坂" (inserts_ekikan Empty global_ekikan_list) = 1.2
let test_17_14_2 = get_ekikan_kyori_improved "中野" "落合" (inserts_ekikan Empty global_ekikan_list) = 2.0
(* let test_17_14_3 = get_ekikan_kyori_improved "新橋" "中野" (inserts_ekikan Empty global_ekikan_list) = infinity *)

(* 17.15, 18.5 *)
let koushin_improved (p: eki_t) (v: eki_t list) (t: ekikan_tree_t) : eki_t list = 
    List.map (
        (fun (p: eki_t) (q: eki_t) ->
        match p with { namae = pn; saitan_kyori = pd; temae_list = pl} ->
        match q with { namae = qn; saitan_kyori = qd; temae_list = ql} ->
        try
            let dist = get_ekikan_kyori_improved pn qn t in
            if dist +. pd < qd
                then { namae = qn; saitan_kyori = dist +. pd; temae_list = qn::pl }
                else q
        with Not_found -> q)
        p
    ) v

let rec dijkstra_main_improved (v: eki_t list) (g: ekikan_tree_t) : eki_t list = match v with
    [] -> []
    | x::xs -> let (new_p, new_v) = saitan_wo_bunri x xs in
                new_p :: dijkstra_main_improved (koushin_improved new_p new_v g) g

let dijkstra_improved (start: string) (destination: string) : eki_t = try
    let network = seiretsu global_ekimei_list in
    let dest_kj = romaji_to_kanji destination network in
    let rec find l = match l with
        [] -> { namae = ""; saitan_kyori = infinity; temae_list = []}
        | ({ namae = n; saitan_kyori = d; temae_list = l} as x)::xs -> 
            if n = dest_kj then x  else find xs in
    find (
        dijkstra_main_improved
            (make_initial_eki_list network (romaji_to_kanji start network))
            (inserts_ekikan Empty global_ekikan_list))
    with No_such_station (n) -> { namae = n; saitan_kyori = infinity; temae_list = []}

let time f x y = 
    let t = Sys.time() in
    let r = f x y in
    Sys.time() -. t

(* tests *)
let test_dijkstra_1 = time dijkstra "shinjuku" "meguro"
let test_dijkstra_2 = time dijkstra "asakusa" "ikebukuro"
let test_dijkstra_3 = time dijkstra "kitaayase" "wakousi"
(* tests *)
let test_dijkstra_imp_1 = time dijkstra_improved "shinjuku" "meguro"
let test_dijkstra_imp_2 = time dijkstra_improved "asakusa" "ikebukuro"
let test_dijkstra_imp_3 = time dijkstra_improved "kitaayase" "wakousi"

(*
val test_dijkstra_1 : float = 0.0551639999999995467
val test_dijkstra_2 : float = 0.0537260000000001625
val test_dijkstra_3 : float = 0.0546279999999992327
val test_dijkstra_imp_1 : float = 0.00744399999999956208
val test_dijkstra_imp_2 : float = 0.00607999999999986329
val test_dijkstra_imp_3 : float = 0.00683200000000017127
*)
