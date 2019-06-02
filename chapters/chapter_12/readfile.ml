let rec iter f = function
  | [] -> ()
  | h::t -> f h; iter f t

let sprint_dict_entry (k, v) = Printf.sprintf "(%i, \"%s\")" k v
let print_dict_entry p = sprint_dict_entry p |> print_string
let sprint_dict d =
  List.map (sprint_dict_entry) d
  |> String.concat "; "
  |> Printf.sprintf "[%s]"
let print_dict d = sprint_dict d |> print_string

let entry_of_channel ch =
  let line = (input_line ch) in
  let open Core_kernel in
  let r = Str.regexp "^(\\([0-9]+\\), \"\\(.*\\)\")$" in
  try 
    ignore (Str.string_match r line 0);
    let k = Str.matched_group 1 line in
    let v = Str.matched_group 2 line in
    int_of_string k, v
  with Failure m -> 
    failwith (Printf.sprintf "%s could not be parsed as entry: %s" line m)

let rec dict_of_channel ch =
  try
    let e = entry_of_channel ch in
    e :: dict_of_channel ch
  with End_of_file -> []

let deserialize_dict filename =
  let ch = open_in filename in
  let dict = dict_of_channel ch in
  close_in ch;
  dict

let entry_to_channel ch (k, v) =
  Printf.fprintf ch "(%i, \"%s\")\n" k v

let dict_to_channel ch = 
  ch |> entry_to_channel |> iter

let serialize_dict filename d =
  let ch = open_out filename in
  dict_to_channel ch d;
  close_out ch

let () =
  serialize_dict "dict.md" [(1, "one"); (2, "two"); (3, "three")];
  deserialize_dict "dict.md" |> print_dict;