open OUnit2
open Solutions

let tests =
  [
    "member_all None []">:: 
    (fun _ -> assert_equal false (member_all None []));
    "member_all 1 [[1]]">:: 
    (fun _ -> assert_equal true (member_all 1 [[1]]));
    "member_all 1 [[1]; [2]]">:: 
    (fun _ -> assert_equal false (member_all 1 [[1]; [2]]));
    "half [2]">::
    (fun _ -> assert_equal [10] (half [20]));
    "trunc 0 []">::
    (fun _ -> assert_equal [] (trunc 0 []));
    "trunc 0 [1]">::
    (fun _ -> assert_equal [] (trunc 0 [1]));
    "trunc 2 [1; 2; 3]">::
    (fun _ -> assert_equal [1; 2] (trunc 2 [1; 2; 3]));
    "truncate 0 [[]]">::
    (fun _ -> assert_equal [[]] (truncate 0 [[]]));
    "truncate 0 [[1]; [2]]">::
    (fun _ -> assert_equal [[]; []] (truncate 0 [[1]; [2]]));
    "truncate 1 [[1; 2; 3]; [4; 5]; [6]; []]">::
    (fun _ -> assert_equal [[1]; [4]; [6]; []] (truncate 1 [[1; 2; 3]; [4; 5]; [6]; []]));
    "compose 1 []">::
    (fun _ -> assert_equal [] (compose 1 []));
    "compose 1 [[]; []; [2]; []; [5; 4; 3]]">::
    (fun _ -> assert_equal [1; 1; 2; 1; 5] (compose 1 [[]; []; [2]; []; [5; 4; 3]]));
  ]

let () =
  run_test_tt_main ("tests" >::: tests)
