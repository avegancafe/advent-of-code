theory Day3

imports Main

begin

record bit_count =
  zero :: nat
  one :: nat

type_synonym bit_digits = "nat list"

fun count_bits :: "bit_digits \<Rightarrow> bit_count" where
"count_bits [] = \<lparr> zero = 0, one = 0 \<rparr>" |
"count_bits (b # bs) = 
  (if b = 0 then (let count = count_bits bs in count\<lparr> zero := zero count + 1\<rparr>)
    else (let count = count_bits bs in count\<lparr> one := one count + 1\<rparr>))"

definition max_bit where
"max_bit bc = (if max (zero bc) (one bc) = (zero bc) then 0 else 1)"

definition min_bit where
"min_bit bc = (if min (zero bc) (one bc) = (zero bc) then 0 else 1)"

definition "utf8_0 \<equiv> 48"

definition to_digits :: "string \<Rightarrow> nat list" where
"to_digits bs = map (\<lambda> b. of_char b - utf8_0) bs"

definition to_nat :: "bit_digits \<Rightarrow> nat" where
"to_nat bs = 
  (let reversed = rev bs in
  fst (foldl (\<lambda> (d, i) b. (d + b * 2 ^ i, i + 1)) (0, 0) reversed))"

definition gamma_rate :: "string list => nat" where
"gamma_rate bs = 
  (let digits = map to_digits bs in
  (let transposed = transpose digits in
  (let counted_bits = map count_bits transposed in
  to_nat (map max_bit counted_bits))))"

definition epsilon_rate :: "string list => nat" where
"epsilon_rate bs = 
  (let digits = map to_digits bs in
  (let transposed = transpose digits in
  (let counted_bits = map count_bits transposed in
  to_nat (map min_bit counted_bits))))"

definition bit_crit_ox :: "bit_count \<Rightarrow> nat" where
"bit_crit_ox bc = (if (zero bc) = (one bc) then 1 else 
  (if max (zero bc) (one bc) = (zero bc) then 0 else 1))"

fun oxygen_generator_rating_r :: "bit_digits list \<Rightarrow> nat \<Rightarrow> bit_digits list" where
"oxygen_generator_rating_r bits 0 = bits" |
"oxygen_generator_rating_r [bits] n = [bits]" |
"oxygen_generator_rating_r bits n =
  (let index = length (hd bits) - n in
  (let transposed = transpose bits in
  (let counted_bits :: nat list = map (bit_crit_ox \<circ> count_bits) transposed in
  oxygen_generator_rating_r (filter (\<lambda> bs. bs!index = counted_bits!index) bits) (n - 1))))"

definition bit_crit_co2 :: "bit_count \<Rightarrow> nat" where
"bit_crit_co2 bc = (if (zero bc) = (one bc) then 0 else 
  (if min (zero bc) (one bc) = (zero bc) then 0 else 1))"

fun co2_scrubber_rating_r :: "bit_digits list \<Rightarrow> nat \<Rightarrow> bit_digits list" where
"co2_scrubber_rating_r bits 0 = bits" |
"co2_scrubber_rating_r [bits] n = [bits]" |
"co2_scrubber_rating_r bits n =
  (let index = length (hd bits) - n in
  (let transposed = transpose bits in
  (let counted_bits :: nat list = map (bit_crit_co2 \<circ> count_bits) transposed in
  co2_scrubber_rating_r (filter (\<lambda> bs. bs!index = counted_bits!index) bits) (n - 1))))"

end