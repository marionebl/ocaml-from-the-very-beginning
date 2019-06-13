let with_int fn =
  try 
    begin match Sys.argv with
      | [|_; arg|] -> int_of_string arg |> fn
      | _ -> Printf.printf "Usage: fibonacci <n>\n"
    end
  with e -> 
    Printf.printf "An error occurred: %s\n" (Printexc.to_string e);
    exit 1

let rec fibonacci = function
    | 0 | 1 | 2 -> 1
    | x -> fibonacci (x - 1) + fibonacci (x - 2) 

let () =
    with_int (fun n -> for n = 1 to n do fibonacci n |> Printf.printf "%i\n" done)