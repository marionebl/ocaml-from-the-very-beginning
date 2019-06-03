(* Chapter 12. In and Out - p. 89 - 96 *)

(* [...] write a function to write to the screen an int x string pair [...] *)
let sprint_dict_entry (k, v) = Printf.sprintf "(%i, \"%s\")" k v
let print_dict_entry p = sprint_dict_entry p |> print_string

(* [...] print a whole dictionary (represented as a list of entries) [...] write
   our own function to iterate over all the entries *)
let sprint_dict d =
  List.map (sprint_dict_entry) d
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let print_dict d = sprint_dict d |> print_string
