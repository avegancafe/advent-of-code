theory Day1

imports Main

begin

fun count_increases :: "nat list \<Rightarrow> nat" where 
"count_increases [] = 0" |
"count_increases [n] = 0" |
"count_increases (n1 # n2 # ls) =
  (let to_add = (if n2 > n1 then 1 else 0) in
    to_add + count_increases (n2 # ls))"

fun count_increasing_window :: "nat list \<Rightarrow> nat" where
"count_increasing_window [] = 0" |
"count_increasing_window (n1 # n2 # n3 # n4 # ls) =
  (let to_add = (if n2 + n3 + n4 > n1 + n2 + n3  then 1 else 0) in
    to_add + count_increasing_window (n2 # n3 # n4 # ls))" |
"count_increasing_window (n1 # ls) = 0"

theorem my_sorted: "strict_sorted ns \<longrightarrow> count_increases ns = length ns - 1"
  apply(induction rule: count_increases.induct)
  apply(auto)
  done

end