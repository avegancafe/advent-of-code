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

let rec first_last = function
    | [] -> failwith "unrecognized command format"
    | [_] -> failwith "unrecognized command format"
    | [e1;e2] -> (e1,e2) 
    | e1 :: _ :: r -> first_last (e1::r)

let extract_number_from_command command dx dy =
  let arr = String.split_on_char ' ' command in
  let (direction, value) = first_last arr in

  match direction with
  "forward" -> (dx + (int_of_string value), dy)
  | "down" -> (dx, dy + (int_of_string value))
  | "up" -> (dx, dy - (int_of_string value))
  | _ -> failwith "unrecognized direction"
;;

let rec process_inputs inputs dx dy =
  match inputs with
    [] -> dx * dy
  | command::rest ->
    let (new_dx, new_dy) = extract_number_from_command command dx dy in
    (process_inputs rest new_dx new_dy)
;;


let () = print_endline ("Product is: " ^ (string_of_int (process_inputs (read_file "./input.txt") 0 0)))
