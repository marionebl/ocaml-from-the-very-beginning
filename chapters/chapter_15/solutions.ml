(* Chapter 15 - The OCaml Standard Library, p. 117 - 119 *)
module Solutions = struct
  (* 1. Write your own version of the function List.concat. 
     The implementation OCaml provides is not tail-recursive. 
     Can you write one which is? *)
  let concat a b =
    let rec concat' a' = function
      | [] -> a'
      | h::t -> concat' (h :: a') t
    in
    concat' (List.rev a) b |> List.rev

  let%test_unit "Solutions.concat" =
    let open OUnit2 in
    let printer l = List.map string_of_int l |> String.concat "; " |> Printf.sprintf "[%s]" in
    assert_equal ([]) (concat [] []) ~printer;
    assert_equal ([1; 2; 3; 3; 2; 1]) (concat [1; 2; 3] [3; 2; 1]) ~printer;
    assert_equal ([1; 100; 4; 50; -1]) (concat [] [1; 100; 4; 50; -1]) ~printer

  (* 2. Use List.mem to write a function which returns true
     only if every list in a bool list list contains true somewhere in it. *)
  let trueish = List.for_all (List.memq true)

  let%test_unit "Solutions.trueish" =
    let open OUnit2 in
    assert_equal false (trueish [[]]);
    assert_equal true (trueish [[true]; [true]]);
    assert_equal false (trueish [[true]; [false]])

  (* 3. Write a function to count the number of exclamation marks in
     a string, using one or more functions from the String module. *)
  let exclamations s =
    let inc = function | '!' -> 1 | _ -> 0 in
    String.to_seq s |> Seq.fold_left (fun c ch -> c + inc ch) 0

  let%test_unit "Solutions.exclamations" =
    let open OUnit2 in
    assert_equal 0 (exclamations "");
    assert_equal 3 (exclamations "!!!");
    assert_equal 4 (exclamations "1elf111!!elf1! !")

  (* 4. Use the String.map function to write a function to return a new copy 
     of a string with all exclamation marks replaced with periods (full stops). *)
  let calm_down s = 
    let replace = function | '!' -> '.' | c -> c in
    String.map replace s 

  let%test_unit "Solutions.calm_down" =
    let open OUnit2 in
    assert_equal "" (calm_down "");
    assert_equal "..." (calm_down "!!!");
    assert_equal "1elf111..elf1. ." (calm_down "1elf111!!elf1! !")

  (* 5. Use the String module to write a function which concatenates a list of strings together. *)
  let join = String.concat ""

  let%test_unit "Solutions.join" =
    let open OUnit2 in
    assert_equal "" (join []);
    assert_equal "" (join [""]);
    assert_equal "12" (join ["1"; "2"])

  (* 5. Do the same with the Buffer module. This will be faster. *)
  let join_buff l =
    let result = Buffer.create 16 in
    let add = Buffer.add_string result in
    List.iter add l;
    Buffer.contents result

  let%test_unit "Solutions.join" =
    let open OUnit2 in
    assert_equal "" (join_buff []);
    assert_equal "" (join_buff [""]);
    assert_equal "12" (join_buff ["1"; "2"])

  (* 6. Use the String module to count the number of occurrences 
     of the string "OCaml" within a given string. *)
  let occurrences s = 
    let result = ref 0 in
    let inc n = result := !result + n in
    let starts_from i s' = 
      let s'len = String.length s' in
      String.length s >= s'len && String.equal (String.sub s i s'len) s' in
    String.iteri (fun i c -> if c == 'O' && starts_from i "OCaml" then inc 1) s;
    !result

  let%test_unit "Solutions.occurrences" =
    let open OUnit2 in
    let printer = string_of_int in
    assert_equal 0 (occurrences "") ~printer;
    assert_equal 0 (occurrences "O") ~printer;
    assert_equal 1 (occurrences "OCaml") ~printer;
    assert_equal 2 (occurrences "OCamlOCaml") ~printer;
    assert_equal 3 (occurrences "OCaml other OCaml something OCaml") ~printer
end 