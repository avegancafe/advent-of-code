let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines
;;

let rec explode = function
    "" -> []
  | x -> ((String.sub x 0 1) :: (explode (String.sub x 1 (String.length x - 1))))
;;



let bin_to_float bin =
  let reversed_bin = List.rev (explode bin) in
  let rec helper prev rest i =
    match rest with
      [] -> prev
    | x::xs ->
      let x = float_of_string x in
      helper (prev +. (2. ** i *. x)) xs (i +. 1.)
  in
  helper 0.0 reversed_bin 0.0
;;


let calculate_gamma inputs = 
  let add_sums a b =
    let rec helper a b agg =
      match a with
        [] -> agg
      | x::xs -> helper xs (List.tl b) (agg @ [(x + (List.hd b))])
    in
    helper a b []
  in

  let int_list_of_bin bin = List.map (fun x -> int_of_string x) (explode bin) in

  let rec sums xs agg =
    match xs with
      [] -> agg
    | x::rest -> 
      let parsed_x = int_list_of_bin x in
      sums rest (add_sums agg parsed_x)
  in

  let len_of_inputs = List.length inputs in

  let bin_of_sums bin =
    let rec helper xs agg =
      match xs with
        [] -> agg
      | x::xs -> helper xs (agg ^ (if x > (len_of_inputs / 2) then "1" else "0"))
    in
    helper bin ""
  in

  bin_of_sums (sums (List.tl inputs) (int_list_of_bin (List.hd inputs)))
;;

let invert_binary bin = 
  let rec helper agg bin =
    match bin with
    "" -> agg
    | x ->
      let head = String.sub x 0 1 in
      let tail = String.sub x 1 (String.length x - 1) in
      match head with
      "1" -> helper (agg ^ "0") tail
      | "0" -> helper (agg ^ "1") tail
      | _ -> failwith "unknown value in binary string"
  in
  helper "" bin
;;

let process_inputs inputs =
  let gamma = calculate_gamma inputs in
  let epsilon = invert_binary gamma in

  (bin_to_float gamma) *. (bin_to_float epsilon)
;;


let () = print_endline ("Product is: " ^ (string_of_float (process_inputs (read_file "./input.txt"))))
