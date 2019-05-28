open OUnit2
open Solutions

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
  ]

let () =
  run_test_tt_main ("tests" >::: tests)
