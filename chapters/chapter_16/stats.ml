open Textstat

let with_filename fn =
  try 
    begin match Sys.argv with
      | [|_; filename|] -> fn filename
      | _ -> Printf.printf "Usage: stats <filname>\n"
    end
  with e -> 
    Printf.printf "An error occurred: %s\n" (Printexc.to_string e);
    exit 1

let (>>) fa fb i = fa i |> fb

let () =
  Textstat.stats_from_file 
  >> Textstat.format_stats 
  >> Printf.printf "%s\n"
  |> with_filename
