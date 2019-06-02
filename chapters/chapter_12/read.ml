let sprint_dict_entry (k, v) = Printf.sprintf "(%i, \"%s\")" k v
let print_dict_entry p = sprint_dict_entry p |> print_string
let sprint_dict d =
  List.map (sprint_dict_entry) d
  |> String.concat "; "
  |> Printf.sprintf "[%s]"
let print_dict d = sprint_dict d |> print_string

(* [...] write a function to read a dictionary as an (int x string) list *)
let rec read_dict () =
  try 
    match read_int () with
    | 0 -> []
    | x -> 
      let name = read_line () in
      (x, name) :: read_dict ()
  with Failure "int_of_string" ->
    print_string "This is not a valid integer. Please try again.\n\n";
    read_dict ()


let () =
  ignore (read_dict () |> print_dict)