(* Chapter 12. In and Out - p. 89 - 96 *)

(* 1. Write a function to print a list of numbers to the screen in the same format OCaml uses - i.e. with 
   square brackets and semicolons *)
let (>>) a b x = x |> a |> b

let rec map f = function
  | [] -> []
  | h::t -> f h :: map f t

let rec fold f init = function
  | [] -> init
  | h::t -> fold f (f init h) t

let len = fold (fun acc _ -> acc + 1) 0 

let concat d l =
  let rec concat' s = function
    | [] -> s
    | h::t -> concat' (s ^ h ^ (if len t > 0 then d else "")) t
  in
  concat' "" l

let format_numbers = map (string_of_int) >> concat "; " >> Printf.sprintf "[%s]"

let print_numbers = format_numbers >> print_string

(* 2. Write a function to read three integers from the user and return them as a tuple. What exceptions
   could be raised in the process? Handle them appropriately. *)
let rec tuple () = 
  try (read_int (), read_int (), read_int ()) |> (fun (c, b, a) -> (a, b, c))
  with Failure m -> 
    Printf.printf "This is not a valid integer: %s Please try again.\n\n" m;
    tuple ()

(* 3. In our read_dict function we waited for the user to type 0 to indicate no more data. This is clumsy.
   Implement a new read_dict function with a nicer system. Be careful to deal with possible exceptions
   which may be reaised *)
let sprint_dict_entry (k, v) = Printf.sprintf "(%i, \"%s\")" k v
let print_dict_entry p = sprint_dict_entry p |> print_string
let sprint_dict d =
  List.map (sprint_dict_entry) d
  |> String.concat "; "
  |> Printf.sprintf "[%s]\n"
let print_dict d = sprint_dict d |> print_string

let rec read_dict () =
  try 
    match read_line () with
    | "" -> []
    | s ->
      let x = int_of_string s in
      let name = read_line () in
      (x, name) :: read_dict ()
  with Failure m ->
    Printf.printf "This is not a valid integer: %s Please try again.\n\n" m;
    read_dict ()

(* 4. Write a function which, given a number x, prints the x-times table to a give file name *)
let table n =
  List.init n (fun x -> List.init n (fun y -> (x + 1) * (y + 1) |> string_of_int))
  |> List.map (String.concat "\t")
  |> String.concat "\n"

let write_table filename n =
  let ch = open_out filename in
  table n |> Printf.fprintf ch "%s";
  close_out ch

let count_lines filename =
  let ch = open_in filename in
  let rec count_lines' n =
    try ignore (input_line ch); count_lines' (n + 1)
    with End_of_file -> n 
  in
  let count = count_lines' 0 in
  close_in ch;
  count

let rec copy_file_ch from_ch to_ch = try
    output_string to_ch (input_line from_ch);
    output_string to_ch "\n";
    copy_file_ch from_ch to_ch
  with
    End_of_file -> ()

exception CopyFailed

let copy_file from_name to_name = try
    let from_ch = open_in from_name in let to_ch = open_out to_name in
    copy_file_ch from_ch to_ch;
    close_in from_ch;
    close_out to_ch
  with
    _ -> raise CopyFailed

let () =
  match Array.to_list Sys.argv with
  | _::"tuple"::_ -> 
    Printf.printf "tuple:\n";
    tuple () |> (fun (a, b, c) -> Printf.printf "(%i, %i, %i)\n" a b c)
  | _::"dict"::_ -> 
    Printf.printf "dict:\n";
    read_dict () |> print_dict
  | _::"table"::filename::n::_ ->
    Printf.printf "table: %s %s\n" filename n;
    write_table filename (int_of_string n)
  | _::"count_lines"::filename::_ ->
    Printf.printf "count_lines: %s\n" filename;
    count_lines filename |> Printf.printf "%i\n"
  | _::"copy"::from_file::to_file::_ ->
    Printf.printf "copy: %s -> %s\n" from_file to_file;
    copy_file from_file to_file
  | _ -> Printf.printf "Missing command\n"