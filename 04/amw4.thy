theory Day4

imports Main

begin

definition "draws \<equiv> [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]"
definition "boards \<equiv>[
[[22,13,17,11,0],
[8,2,23,4,24],
[21,9,14,16,7],
[6,10,3,18,5],
[1,12,20,15,19]],

[[3,15,0,2,22],
[9,18,13,17,5],
[19,8,7,25,23],
[20,11,10,24,4],
[14,21,16,12,6]],

[[14,21,17,24,4],
[10,16,15,9,19],
[18,8,23,26,20],
[22,11,13,6,5],
[2,0,12,3,7]]
]"

record board_number =
  number :: nat
  marked :: bool

type_synonym board_row = "board_number list"
type_synonym board = "board_row list"

(* Parsing *)
definition parse_number :: "nat \<Rightarrow> board_number" where
"parse_number n = \<lparr> number = n, marked = False \<rparr>"

definition parse_row :: "nat list \<Rightarrow> board_row" where
"parse_row br = map parse_number br"

definition parse_board :: "nat list list \<Rightarrow> board" where
"parse_board b = map parse_row b"

definition parse_board_list :: "nat list list list \<Rightarrow> board list" where
"parse_board_list bs = map parse_board bs"

definition "parsed_boards \<equiv> parse_board_list boards"

(* Marking board *)
definition mark_number :: "nat \<Rightarrow> board_number \<Rightarrow> board_number" where
"mark_number n bn = (if number bn = n then bn\<lparr> marked := True \<rparr> else bn)"

definition mark_row :: "nat \<Rightarrow> board_row \<Rightarrow> board_row" where
"mark_row n br = map (mark_number n) br"

definition mark_board :: "nat \<Rightarrow> board \<Rightarrow> board" where
"mark_board n b = map (mark_row n) b"

definition draw_num :: "nat => board list \<Rightarrow> board list" where
"draw_num n bs = map (mark_board n) bs"

fun winning_row :: "board_number list \<Rightarrow> bool" where
"winning_row [] = True" |
"winning_row (n # ns) = (marked n = True \<and> winning_row ns)"

fun is_winner :: "board \<Rightarrow> bool" where
"is_winner [] = False" |
"is_winner (r # rs) = (if winning_row r then True else is_winner rs)"

definition find_winners :: "board list \<Rightarrow> board list" where
"find_winners bs = filter is_winner bs @ filter is_winner (map transpose bs)"

fun find_winners_idx_r :: "board list \<Rightarrow> nat \<Rightarrow> nat list" where
"find_winners_idx_r [] n = []" |
"find_winners_idx_r (b # bs) n = 
  (let transposed = transpose b in
  (if (is_winner b \<or> is_winner transposed) then n # find_winners_idx_r bs (n + 1) else find_winners_idx_r bs (n + 1)))"

definition find_winners_idx :: "board list \<Rightarrow> nat list" where
"find_winners_idx bs = find_winners_idx_r bs 0"

value "find_winners_idx [
  [
    [\<lparr> number = 1, marked = True \<rparr>, \<lparr> number = 2, marked = False \<rparr>], 
    [\<lparr> number = 1, marked = True \<rparr>, \<lparr> number = 2, marked = False \<rparr>]
  ], 
  [
    [ \<lparr> number = 2, marked = False \<rparr>]
  ],
  [
    [ \<lparr> number = 2, marked = True \<rparr>]
  ]
]"

fun draw_nums :: "nat list \<Rightarrow> board list \<Rightarrow> (board list * nat)" where
"draw_nums [] bs = (bs, 0)" |
"draw_nums (n # ns) bs = 
  (let winners = find_winners bs in
  (if winners = [] then draw_nums ns (draw_num n bs) else (winners, n)))"

value "sorted_list_of_set {1, 2::nat}"

fun draw_nums_last :: "nat list \<Rightarrow> board list \<Rightarrow> nat list \<Rightarrow> (board * nat list * nat) list" where
"draw_nums_last [] bs w = []" |
"draw_nums_last (n # ns) bs w =
  (let next = draw_num n bs in
  (let winners = find_winners_idx next in
  (let new_winners :: nat set = (set winners) - (set w) in
  (draw_nums_last ns next winners @ [(next!9, sorted_list_of_set new_winners, n)]))))"

 (* (if (is_winner next \<or> is_winner (transpose next)) then (next, n) # (draw_nums_last ns next) else draw_nums_last ns next))" *)

primrec flatten :: "'a list list \<Rightarrow> 'a list" where
"flatten [] = []"
| "flatten (xs#xss) = xs @ flatten xss"

definition score :: "board \<Rightarrow> nat" where
"score b = foldl (\<lambda> a n. (if (marked n) then a else a + (number n))) 0 (flatten b)"

definition "draws_data \<equiv> [12,28,0,63,26,38,64,17,74,67,51,44,77,32,6,10,52,47,61,46,50,29,15,1,39,37,13,66,45,8,68,96,53,40,76,72,21,93,16,83,62,48,11,9,20,36,91,19,5,42,99,84,4,95,92,89,7,71,34,35,55,22,59,18,49,14,54,85,82,58,24,73,31,97,69,43,65,27,81,56,87,70,33,88,60,2,75,90,57,94,23,30,78,80,41,3,98,25,79,86]"

definition "boards_data \<equiv> [  

[[50 ,79 ,88 ,34  ,0],
[56 ,46  ,5 ,17 ,31],
[29  ,6 ,38 ,78 ,68],
[75 ,57 ,15 ,44 ,83],
[89 ,45 ,43 ,85 ,72]],

[[29  ,8 ,56 ,15 ,33],
 [7 ,14 ,51 ,88 ,67],
[91 ,32 ,62 ,18 ,73],
[53 ,63 ,49 ,34 ,46],
[70 ,25 ,77 ,87 ,31]]
]"


value "parse_board_list boards"

value "(draw_nums draws_data (parse_board_list boards_data))"

value "length (draws_data::nat list)"

definition "all_draws \<equiv> draw_nums_last draws_data (parse_board_list boards_data) []"

(* value "score (fst (all_draws!12))" *)

end