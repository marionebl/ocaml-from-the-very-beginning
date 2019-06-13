open Str

let with_stream fn =
  try 
    begin match Sys.argv with
      | [|_; filename; search|] -> fn filename search
      | _ -> Printf.printf "Usage: match <filname> <search>\n"
    end
  with e -> 
    Printf.printf "An error occurred: %s\n" (Printexc.to_string e);
    exit 1

let stream_from_file filename =
    let in_channel = open_in filename in
    Stream.from (fun _ -> try Some (input_line in_channel) with _ -> close_in in_channel; None)

let contains l s = 
    let result = ref false in
    let len = String.length s in
    let total = String.length l in
    String.iteri (fun i c -> 
        if !result == false && c == String.get s 0 && i + len <= total && String.equal (String.sub l i len) s then
        begin
          result := true
        end
    ) l;
    !result

let () =
   with_stream (fun file search -> 
    stream_from_file file
    |> Stream.iter (fun l -> if contains l search then begin Printf.printf "%s\n" l end))