let with_filename fn =
  try 
    begin match Sys.argv with
      | [|_; a; b|] -> fn a b
      | _ -> Printf.printf "Usage: reverse <in> <out>\n"
    end
  with e -> 
    Printf.printf "An error occurred: %s\n" (Printexc.to_string e);
    exit 1

let (>>) fa fb i = fa i |> fb

let process channel pos _ =
  if !pos > 0 then
    let char = ref ' ' in
    let chars = Buffer.create 100 in
    while !pos >= 0 && !char <> '\n' do
        seek_in channel !pos;
        let c = input_char channel in
        char := c;
        pos := !pos - 1;
        Buffer.add_char chars c
    done;
    Buffer.to_seq chars 
    |> List.of_seq 
    |> List.rev 
    |> List.to_seq
    |> String.of_seq
    |> (fun s -> Some s)
  else 
    None


let stream_from filename = 
  let in_channel = open_in filename in
  let pos = ref (in_channel_length in_channel - 1) in
  Stream.from (process in_channel pos)

let stream_to filename stream =
  let out_channel = open_out filename in
  Stream.iter (fun c -> output_string out_channel c) stream;
  close_out out_channel

let () =
  with_filename (fun a b -> stream_from a |> stream_to b |> ignore)
