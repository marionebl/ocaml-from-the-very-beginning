open OUnit2
open Solutions

let print_tuple (a, b) ~item = Printf.sprintf "( %s, %s )" (item a) (item b)
let print_list a ~item = Printf.sprintf "[ %s ]" ((List.map item a) |> String.concat "; ")

let tests =
  [
    "first (1, 2)">::
    (fun _ -> assert_equal 1 (first (1, 2)));
    "first (2, 1)">::
    (fun _ -> assert_equal 2 (first (2, 1)));
    "second (1, 2)">::
    (fun _ -> assert_equal 2 (second (1, 2)));
    "second (2, 1)">::
    (fun _ -> assert_equal 1 (second (2, 1)));
    "lookup 1 [(1, 1)]">::
    (fun _ -> assert_equal (Some 1) (lookup 1 [(1, 1)]));
    "lookup 2 [(1, 1), (2, 1)]">::
    (fun _ -> assert_equal (Some 1) (lookup 2 [(1, 1); (2, 1)]));
    "lookup 3 [(1, 1), (2, 1)]">::
    (fun _ -> assert_equal None (lookup 3 [(1, 1); (2, 1)]));
    "lookup None []">::
    (fun _ -> assert_equal None (lookup None []));
    "add 1 2 []">::
    (fun _ -> assert_equal [(1, 2)] (add 1 2 []));
    "add 2 3 [(1, 2)]">::
    (fun _ -> assert_equal [(1, 2); (2, 3)] (add 2 3 [(1, 2)]));
    "add 2 4 [(1, 2), (2, 3)]">::
    (fun _ -> assert_equal [(1, 2); (2, 4)] (add 2 4 [(1, 2); (2, 3)]));
    "remove None []">::
    (fun _ -> assert_equal [] (remove None []));
    "remove 1 [(2, 1)]">::
    (fun _ -> assert_equal [(2, 1)] (remove 1 [(2, 1)]));
    "remove 2 [(2, 1)]">::
    (fun _ -> assert_equal [] (remove 2 [(2, 1)]));
    "remove 1 [(2, 1); (1, 1); (3, 1)]">::
    (fun _ -> assert_equal [(2, 1); (3, 1)] (remove 1 [(2, 1); (1, 1); (3, 1)]));
    "exists 1 [(1, 1)]">::
    (fun _ -> assert_equal true (exists 1 [(1, 1)]));
    "exists 2 [(1, 1), (2, 1)]">::
    (fun _ -> assert_equal true (exists 2 [(1, 1); (2, 1)]));
    "exists 3 [(1, 1), (2, 1)]">::
    (fun _ -> assert_equal false (exists 3 [(1, 1); (2, 1)]));
    "exists None []">::
    (fun _ -> assert_equal false (exists None []));
    "distinct []">::
    (fun _ -> assert_equal 0 (distinct []));
    "distinct [(1, 1)]">::
    (fun _ -> assert_equal 1 (distinct [(1, 1)]));
    "distinct [(1, 1); (2, 1)]">::
    (fun _ -> assert_equal 2 (distinct [(1, 1); (2, 1)]));
    "distinct [(1, 1); (3, 1); (2, 1); (3, 1)]">::
    (fun _ -> assert_equal 3 (distinct [(1, 1); (3, 1); (2, 1); (3, 1)]));
    "replace 1 2 []">::
    (fun _ -> assert_equal (Error "Not found") (try Ok (replace 1 2 []) with Not_found -> Error "Not found"));
    "replace 2 3 [(1, 2)]">::
    (fun _ -> assert_equal (Error "Not found") (try Ok (replace 2 3 []) with Not_found -> Error "Not found"));
    "replace 2 4 [(1, 2); (2, 3)]">::
    (fun _ -> assert_equal (Ok [(1, 2); (2, 4)]) (try Ok (replace 2 4 [(1, 2); (2, 3)]) with Not_found -> Error "Not found"));
    "partition []">::
    (fun _ -> assert_equal ([], []) (partition []));
    "partition [(1, 2)]">::
    (fun _ -> assert_equal ([1], [2]) (partition [(1, 2)]));
    "partition [(1, 2); (3, 4)]">::
    (fun _ -> assert_equal ([1; 3], [2; 4]) (partition [(1, 2); (3, 4)]) ~printer:(print_tuple ~item:(print_list ~item:string_of_int)));
    "dict []">::
    (fun _ -> assert_equal [] (dict []));
    "dict [(1, 2)]">::
    (fun _ -> assert_equal [(1, 2)] (dict [(1, 2)]));
    "dict [(1, 2); (3, 4); (1, 5)]">::
    (fun _ -> assert_equal [(1, 2); (3, 4)] (dict [(1, 2); (3, 4); (1, 5)]) ~printer:(print_list ~item:(print_tuple ~item:string_of_int)));
    "union [] []">::
    (fun _ -> assert_equal [] (union [] []));
    "union [(1, 2)] [(3, 4)]">::
    (fun _ -> assert_equal [(1, 2); (3, 4)] (union [(1, 2)] [(3, 4)]) ~printer:(print_list ~item:(print_tuple ~item:string_of_int)));
    "union [(1, 2)] [(3, 4); (1, 5)]">::
    (fun _ -> assert_equal [(1, 2); (3, 4)] (union [(1, 2)] [(3, 4); (1, 5)]) ~printer:(print_list ~item:(print_tuple ~item:string_of_int)));
  ]

let () =
  run_test_tt_main ("tests" >::: tests)
