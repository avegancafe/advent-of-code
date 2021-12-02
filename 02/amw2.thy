theory Day2

imports Main "HOL-Library.Code_Target_Numeral"

begin

datatype move = forward nat | up nat | down nat

record position =
  horizontal :: nat
  vertical :: nat

definition apply_move :: "position \<Rightarrow> move \<Rightarrow> position" where
"apply_move pos m = 
  (case m of 
    forward n \<Rightarrow> pos\<lparr> horizontal := horizontal pos + n \<rparr> |
    up n \<Rightarrow> pos\<lparr> vertical := vertical pos - n \<rparr> |
    down n \<Rightarrow> pos\<lparr> vertical := vertical pos + n \<rparr>)"

definition "initial_position \<equiv> \<lparr> horizontal = 0, vertical = 0 \<rparr>"

definition position_after_course :: "move list \<Rightarrow> position" where
"position_after_course c = foldl apply_move initial_position c"

record aimed_position =
  horizontal :: nat
  vertical :: nat
  aim :: nat

definition apply_move_aim :: "aimed_position \<Rightarrow> move \<Rightarrow> aimed_position" where
"apply_move_aim pos m = 
  (case m of 
    forward n \<Rightarrow> pos\<lparr> horizontal := horizontal pos + n, vertical := vertical pos + aim pos * n \<rparr> |
    up n \<Rightarrow> pos\<lparr> aim := aim pos - n \<rparr> |
    down n \<Rightarrow> pos\<lparr> aim := aim pos + n \<rparr>)"

definition "initial_aimed_position \<equiv> \<lparr> horizontal = 0, vertical = 0, aim = 0 \<rparr>"

definition position_after_course_aim :: "move list \<Rightarrow> aimed_position" where
"position_after_course_aim c = foldl apply_move_aim initial_aimed_position c"

end