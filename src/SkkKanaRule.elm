module SkkKanaRule exposing (KanaRuleSearchResult(..), SkkKanaRule, SkkKanaRuleTree(..), SkkKanaRules, getDefaultRules, search)

import Dict exposing (Dict)
import Dict.Extra exposing (filterGroupBy)



-- types


type alias SkkKanaRules =
    { tree : SkkKanaRuleTree
    }


type SkkKanaRuleTree
    = SkkKanaRuleLeafNode SkkKanaRule
    | SkkKanaRuleInternalNode (Dict Char SkkKanaRuleTree)


type alias SkkKanaRule =
    { key : String
    , hiragana : String
    , katakana : String
    , next : Maybe String
    }


{-| 入力文字,ひらがな,カタカナ[,次状態]
#から始まる場合はコメント行として扱う
<https://github.com/codefirst/aquaskk/blob/master/data/config/kana-rule.conf> を参考にしている
-}
kanaRuleString : String
kanaRuleString =
    """
a あ ア

bb っ ッ b
ba ば バ
bi び ビ
bu ぶ ブ
be べ ベ
bo ぼ ボ
bya びゃ ビャ
byi びぃ ビィ
byu びゅ ビュ
bye びぇ ビェ
byo びょ ビョ

cc っ ッ c
cha ちゃ チャ
chi ち チ
chu ちゅ チュ
che ちぇ チェ
cho ちょ チョ
cya ちゃ チャ
cyi ちぃ チィ
cyu ちゅ チュ
cye ちぇ チェ
cyo ちょ チョ

dd っ ッ d
da だ ダ
di ぢ ヂ
du づ ヅ
de で デ
do ど ド
dha でゃ デャ
dhi でぃ ディ
dhu でゅ デュ
dhe でぇ デェ
dho でょ デョ
dya ぢゃ ヂャ
dyi ぢぃ ヂィ
dyu ぢゅ ヂュ
dye ぢぇ ヂェ
dyo ぢょ ヂョ ﾁﾞｮ

e え ｴ

ff っ ｯ f
fa ふぁ ファ
fi ふぃ フィ
fu ふ フ
fe ふぇ フェ
fo ふぉ フォ
fya ふゃ フャ
fyi ふぃ フィ
fyu ふゅ フュ
fye ふぇ フェ
fyo ふょ フョ

gg っ ｯ g
ga が ガ
gi ぎ ギ
gu ぐ グ
ge げ ゲ
go ご ゴ
gya ぎゃ ギャ
gyi ぎぃ ギィ
gyu ぎゅ ギュ
gye ぎぇ ギェ
gyo ぎょ ギョ

hh っ ッ h
ha は ハ
hi ひ ヒ
hu ふ フ
he へ ヘ
ho ほ ホ
hya ひゃ ヒャ
hyi ひぃ ヒィ
hyu ひゅ ヒュ
hye ひぇ ヒェ
hyo ひょ ヒョ

i い イ

jj っ ッ j
ja じゃ ジャ
ji じ ジ
ju じゅ ジュ
je じぇ ジェ
jo じょ ジョ
jya じゃ ジャ
jyi じぃ ジィ
jyu じゅ ジュ
jye じぇ ジェ
jyo じょ ジョ

kk っ ッ k
ka か カ
ki き キ
ku く ク
ke け ケ
ko こ コ
kya きゃ キャ
kyi きぃ キィ
kyu きゅ キュ
kye きぇ キェ
kyo きょ キョ

mm っ ッ m
ma ま マ
mi み ミ
mu む ム
me め メ
mo も モ
mya みゃ ミャ
myi みぃ ミィ
myu みゅ ミュ
mye みぇ ミェ
myo みょ ミョ

nn ん ン
na な ナ
ni に ニ
nu ぬ ヌ
ne ね ネ
no の ノ
nya にゃ ニャ
nyi にぃ ニィ
nyu にゅ ニュ
nye にぇ ニェ
nyo にょ ニョ

o お オ

pp っ ッ p
pa ぱ パ
pi ぴ ピ
pu ぷ プ
pe ぺ ペ
po ぽ ポ
pya ぴゃ ピャ
pyi ぴぃ ピィ
pyu ぴゅ ピュ
pye ぴぇ ピェ
pyo ぴょ ピョ

rr っ ッ r
ra ら ラ
ri り リ
ru る ル
re れ レ
ro ろ ロ
rya りゃ リャ
ryi りぃ リィ
ryu りゅ リュ
rye りぇ リェ
ryo りょ リョ

ss っ ッ s
sa さ サ
si し シ
su す ス
se せ セ
so そ ソ
sha しゃ シャ
shi し シ
shu しゅ シュ
she しぇ シェ
sho しょ ショ
sya しゃ シャ
syi しぃ シィ
syu しゅ シュ
sye しぇ シェ
syo しょ ショ

tt っ ッ t
ta た タ
ti ち チ
tu つ ツ
te て テ
to と ト
tha てぁ テァ
thi てぃ ティ
thu てゅ テュ
the てぇ テェ
tho てょ テョ
tsu つ ツ
tya ちゃ チャ
tyi ちぃ チィ
tyu ちゅ チュ
tye ちぇ チェ
tyo ちょ チョ

u う ウ

vv っ ッ v
va う゛ぁ ヴァ
vi う゛ぃ ヴィ
vu う゛ ヴ
ve う゛ぇ ヴェ
vo う゛ぉ ヴォ

ww っ ッ w
wa わ ワ
wi うぃ ウィ
wu う ウ
we うぇ ウェ
wo を ヲ

xx っ ッ x
xa ぁ ァ
xi ぃ ィ
xu ぅ ゥ
xe ぇ ェ
xo ぉ ォ
xka ヵ ヵ
xke ヶ ヶ
xtsu っ ッ
xtu っ ッ
xu ぅ ゥ
xwa ゎ ヮ
xwe ゑ ヱ
xwi ゐ ヰ
xya ゃ ャ
xyo ょ ョ
xyu ゅ ュ

yy っ ッ y
ya や ヤ
yi い イ
yu ゆ ユ
ye いぇ イェ
yo よ ヨ

zz っ ッ z
za ざ ザ
zi じ ジ
zu ず ズ
ze ぜ ゼ
zo ぞ ゾ
zya じゃ ジャ
zyi じぃ ジィ
zyu じゅ ジュ
zye じぇ ジェ
zyo じょ ジョ

z, ‥ ‥
z- ～ ～
z. … …
z/ ・ ･
z[ 『 『
z] 』 』
zh ← ←
zj ↓ ↓
zk ↑ ↑
zl → →
z<SPACE> \u{3000} \u{3000}

- ー ー
: ： ：
; ； ；
[ 「 「
] 」 」

. 。 。
, 、 、
"""



-- create


getDefaultRules : SkkKanaRules
getDefaultRules =
    let
        toLines : String -> List String
        toLines =
            String.lines

        filterLines : List String -> List String
        filterLines =
            List.filter (\l -> not (String.isEmpty l) && not (String.startsWith "#" l))

        normalizeRuleStr : String -> String
        normalizeRuleStr =
            String.replace "<SPACE>" " "

        toRule : String -> Maybe SkkKanaRule
        toRule l =
            case String.split " " l of
                key :: hiragana :: katakana :: [] ->
                    Just
                        { key = normalizeRuleStr key
                        , hiragana = normalizeRuleStr hiragana
                        , katakana = normalizeRuleStr katakana
                        , next = Nothing
                        }

                key :: hiragana :: katakana :: next :: [] ->
                    Just
                        { key = normalizeRuleStr key
                        , hiragana = normalizeRuleStr hiragana
                        , katakana = normalizeRuleStr katakana
                        , next = Just (normalizeRuleStr next)
                        }

                _ ->
                    Nothing

        toTree : Int -> List SkkKanaRule -> SkkKanaRuleTree
        toTree pos rules =
            let
                leaf : Maybe SkkKanaRule
                leaf =
                    List.filter (\r -> edgeChar pos r.key == Nothing) rules |> List.head

                nodeGroup : Dict Char (List SkkKanaRule)
                nodeGroup =
                    filterGroupBy (.key >> edgeChar pos) rules
            in
            case leaf of
                Just l ->
                    SkkKanaRuleLeafNode l

                Nothing ->
                    SkkKanaRuleInternalNode (nodeGroup |> Dict.map (\_ g -> toTree (pos + 1) g))

        buildRules : List String -> SkkKanaRules
        buildRules lines =
            { tree = List.filterMap toRule lines |> toTree 0
            }

        edgeChar : Int -> String -> Maybe Char
        edgeChar pos key =
            String.toList key |> List.drop pos |> List.head
    in
    toLines kanaRuleString |> filterLines |> buildRules



-- search


type KanaRuleSearchResult
    = PerfectMatch SkkKanaRule
    | PartialMatch
    | NoMatch


search : String -> SkkKanaRules -> KanaRuleSearchResult
search key rules =
    let
        s : List Char -> SkkKanaRuleTree -> Maybe SkkKanaRuleTree
        s chars tree =
            case List.head chars of
                Just ch ->
                    case tree of
                        SkkKanaRuleInternalNode children ->
                            Dict.get ch children |> Maybe.andThen (s (List.drop 1 chars))

                        _ ->
                            Nothing

                Nothing ->
                    Just tree
    in
    case s (String.toList key) rules.tree of
        Just (SkkKanaRuleLeafNode rule) ->
            PerfectMatch rule

        Just (SkkKanaRuleInternalNode _) ->
            PartialMatch

        _ ->
            NoMatch
