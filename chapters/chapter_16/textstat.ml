type stats = { 
  lines: int; 
  chars: int; 
  words: int; 
  sents: int;
  histogram: int array;
}

let lines s = s.lines

let characters s = s.chars

let words s = s.words

let sentences s = s.sents

let inc r = r := !r + 1 

let log histogram c = histogram.(int_of_char c) <- histogram.(int_of_char c) + 1

let stats_from_stream stream = 
  let lines = ref 0 in
  let chars = ref 0 in
  let words = ref 0 in
  let sents = ref 0 in
  let histogram = Array.make 256 0 in
  let process = function
    | '.' | '?' | '!' -> inc chars; inc sents
    | ' ' -> inc chars; inc words
    | '\n' -> inc lines
    | c -> inc chars; log histogram c 
  in
  Stream.iter (process) stream;
  { lines=(!lines); chars=(!chars); words=(!words); sents=(!sents); histogram=histogram }


let (>>) fa fb i = fa i |> fb
let stream_from_file = open_in >> Stream.of_channel
let stats_from_file = stream_from_file >> stats_from_stream

let format_histogram h =
  Array.to_list h
  |> List.filter (fun c -> c > 0)
  |> List.mapi (fun i c -> Printf.sprintf "\"%s\": %i" (char_of_int i |> Char.escaped) c)
  |> String.concat "\n"

let format_stats s = 
  Printf.sprintf "Words: %i\t Characters: %i\t Sentences: %i\t Lines: %i\n%s" s.words s.chars s.sents s.lines (format_histogram s.histogram)