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
  ]

let () =
  run_test_tt_main ("tests" >::: tests)
