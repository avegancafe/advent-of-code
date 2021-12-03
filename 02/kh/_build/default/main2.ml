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

let extract_number_from_command command dx dy aim =
  let arr = String.split_on_char ' ' command in
  let (direction, value) = first_last arr in

  match direction with
  "forward" -> (dx + (int_of_string value), dy + (aim * (int_of_string value)), aim)
  |"down" -> (dx, dy, aim + (int_of_string value))
  |"up" -> (dx, dy, aim - (int_of_string value))
  | _ -> failwith "unrecognized direction"
;;

let rec process_inputs inputs dx dy aim =
  match inputs with
    [] -> dx * dy
  | command::rest ->
    let (new_dx, new_dy, new_aim) = extract_number_from_command command dx dy aim in
    (process_inputs rest new_dx new_dy new_aim)
;;


let () = print_endline ("Aim is: " ^ (string_of_int (process_inputs (read_file "./input.txt") 0 0 0)))
