(* Chapter 13. Putting Things in Boxes, p. 99 - 108 *)

(** Study **)
(* We are going to write a program to count the number of words, sentences and lines in a text file. 
   We shall consider the opening paragraph of Kafka’s “Metamorphosis”. *)
let metamorphosis = {|One morning, when Gregor Samsa woke from troubled dreams, he found
himself transformed in his bed into a horrible vermin.  He lay on
his armour-like back, and if he lifted his head a little he could
see his brown belly, slightly domed and divided by arches into stiff
sections.  The bedding was hardly able to cover it and seemed ready
to slide off any moment.  His many legs, pitifully thin compared
with the size of the rest of him, waved about helplessly as he
looked.|}

let inc r = r := !r + 1

let count hist c =
  let i = int_of_char c in
  hist.(i) <- hist.(i) + 1

type statistics = { 
  lines: int; 
  characters: int; 
  words: int; 
  sentences: int;
  histogram: int array;
}

let stream_statistics stream =
  let lines = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let characters = ref 0 in
  let histogram = (Array.make 256 0) in
  let hist_count = count histogram in 
  Stream.iter (fun c ->
    hist_count c;
    match c with
      | '.' | '?' | '!' -> inc sentences
      | ' ' -> inc words
      | '\n' -> inc lines
      | _ -> inc characters) stream;
  { 
    lines=(!lines); 
    characters=(!characters); 
    words=(!words); 
    sentences=(!sentences);
    histogram=histogram
  }

let print_statistics (stats: statistics) = 
  Printf.printf "There were %i lines, making up %i characters with %i words in %i sentences.\n" stats.lines stats.characters stats.words stats.sentences

let print_histogram (stats: statistics) =
  Printf.printf "Character frequencies:\n";
  for x = 0 to 255 do
    let count = stats.histogram.(x) in
    if count > 0 then
      begin
        match char_of_int x with
        | '\n' -> Printf.printf "\"\\n\": %i\n" count
        | c -> Printf.printf "\"%c\": %i\n" c count
      end
  done

let file_statistics in_file =   
  let channel = open_in in_file in
  try 
    let stats = Stream.of_channel channel |> stream_statistics in
    stats |> print_statistics;
    stats |> print_histogram
  with _ -> close_in channel

let dump_file out_file contents =
  let channel = open_out out_file in
  try 
    Printf.fprintf channel "%s" contents;
    close_out channel
  with _ -> close_out_noerr channel

(* 1. Consider the expression let x = ref 1 in let y = ref 2 in x := !x + !x; y := !x + !y; !x + !y
   What references have been created? What are their initial and final values after this expression has been evaluated? What is the type of this expression? *)
(* Created references: x, y. Values of x: 1 - 2; y: 2 - 4; expression evaluates to 6 *)
let r =
  let x = ref 1 in 
  let y = ref 2 in 
  x := !x + !x; y := !x + !y; !x + !y

(* 2. What is the difference between [ref 5; ref 5] and let x = ref 5 in [x; x]? *)
let one = [ref 5; ref 5] (* List with two distinct references *)

let two = 
  let x = ref 5 in 
  [x; x] (* List with one shared reference *)

(* 3. Imagine that the for ... to ... do ... done construct did not exist. How might we create the
   same behaviour? *)
(* Same behaviour may be implemented with Lists / Sequences *)

(* 4. What are the types of these expressions *)
let four =
  ignore [|1; 2; 3|]; (* int array *)
  ignore [|true; false; true|]; (* bool array *)
  ignore [|[|1|]|]; (* int array array *)
  ignore [|[|1; 2; 3|]; [|4; 5; 6|]|]; (* int array array *)
  ignore [|1; 2; 3|].(2); (* int *)
  [|1; 2; 3|].(2) <- 4 (* unit *)

(* 5. Write a function to compute the sum of elements in an integer array *)
let sum a =
  let s = ref 0 in
  for x = 0 to Array.length a - 1 
    do 
      s := !s + a.(x) 
    done;
  !s

(* 6. Write a function to reverse the elements of an array in place *)
let rev a =
  let len = Array.length a in
  if len > 0 then
    for x = 0 to (len - 1) / 2
      do
        let y = len - x in
        let t = a.(x) in
        a.(x) <- a.(y);
        a.(x) <- t
      done
      
(* 7. Write a function table which, given an integer, builds the int array array representing the
   multiplication table up to that number. For example, table 5 should yield: *)
let row n y = 
  Array.init n (fun x -> (x + 1) * (y + 1))

let table n = Array.init n (row n)

(* 8. The ASCII codes for the lower case letters 'a'...'z' are 97...122, and for the upper case letters
   'A'...'Z' thery are 65...90. Use the built-in functions int_of_char and char_of_int to write functions
   to uppercase and lowercase a character. Non-alphabetic characters should remain unaltered. *)
let bound (l, u) n = n >= l && n <= u

let transpose b d c = 
  let p = int_of_char c in
  if bound b p then
    char_of_int (d p 32)
  else
    c

let lowercase = transpose (65, 90) (+)
let uppercase = transpose (97, 122) (-)

let () =
  dump_file "die-verwandlung.md" metamorphosis;
  file_statistics "die-verwandlung.md";
  Printf.printf "%i\n" r;
  Printf.printf "%i\n" (sum [||]);
  Printf.printf "%i\n" (sum [|2; 4; 8; 16|]);