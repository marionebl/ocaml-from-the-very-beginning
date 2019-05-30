open OUnit2
open Solutions

let ppoint (x, y) = Printf.sprintf "(%i, %i)" x y
let psquare p l = Printf.sprintf "Square(%s, %i)" (ppoint p) l
let prectangle a b = Printf.sprintf "Rect(%s, %s)" (ppoint a) (ppoint b)
let prect r = match r with | Square(p, l) -> psquare p l | Rectangle(a, b) -> prectangle a b
let plist i l = l |> List.map i |> String.concat "; " |> Printf.sprintf "[%s]"
let pseq i s = Seq.to_list s |> plist i

let ai exp act __ctx = assert_equal exp act ~printer:string_of_int
let ar exp act __ctx = assert_equal exp act ~printer:prect
let alr exp act __ctx = assert_equal exp act ~printer:(plist prect)
let aseq exp act __ctx = assert_equal exp act ~printer:(pseq string_of_int)

let tests =
  [
    "area Square ((0, 0), 2) -> 4">:: 
    (ai 4 (area (Square ((0, 0), 2))));
    "area Square ((0, 0), 1) -> 1">:: 
    (ai 1 (area (Square ((0, 0), 1))));
    "area Rectangle ((0, 0), (4, 5)) -> 20">:: 
    (ai 20 (area (Rectangle ((0, 0), (4, 5)))));
    "area Rectangle ((0, 0), (-4, -5)) -> 20">:: 
    (ai 20 (area (Rectangle ((0, 0), (-4, -5)))));
    "area Rectangle ((0, 0), (4, -5)) -> 20">:: 
    (ai 20 (area (Rectangle ((0, 0), (4, -5)))));
    "area Rectangle ((3, 3), (0, 0)) -> 9">:: 
    (ai 9 (area (Rectangle ((3, 3), (0, 0)))));
    "area Rectangle ((-3, -3), (0, 0)) -> 9">:: 
    (ai 9 (area (Rectangle ((-3, -3), (0, 0)))));
    "erect Square ((0, 0), 1)">::
    (ar (Square ((0, 0), 2)) (erect (Square ((0, 0), 2))));
    "erect Rectangle ((0, 0), (2, 2))">::
    (ar (Rectangle ((0, 0), (2, 2))) (erect (Rectangle ((0, 0), (2, 2)))));
    "erect Rectangle ((0, 0), (2, 3))">::
    (ar (Rectangle ((0, 0), (2, 3))) (erect (Rectangle ((0, 0), (2, 3)))));
    "erect Rectangle ((0, 0), (3, 2))">::
    (ar (Rectangle ((0, 0), (-2, -3))) (erect (Rectangle ((0, 0), (3, 2)))));
    "stack [Rectangle ((0, 0), (5, 3)); Square ((0, 0), 2); Rectangle ((0, 0), (1, 2))]">::
    (alr [Rectangle ((0, 0), (1, 2)); Square ((0, 0), 2); Rectangle ((0, 0), (-3, -5))] (stack [Rectangle ((0, 0), (5, 3)); Square ((0, 0), 2); Rectangle ((0, 0), (1, 2))]));
    "take 1 Cons (1, (Cons (2, Nil))) -> Cons (1, Nil)">::
    (aseq (Seq.of_list [1]) (Seq.take 1 (Seq.of_list [1; 2])));
    "take 2 Cons (1, (Cons (2, Nil))) -> Cons (1, (Cons (2, Nil)))">::
    (aseq (Seq.of_list [1; 2]) (Seq.take 2 (Seq.of_list [1; 2])));
    "take 1 Nil -> fails">::
    (fun _ -> assert_equal (Error "end of list") (try Ok (Seq.take 1 Nil) with _ -> Error "end of list")); 
    "drop 1 Cons (1, (Cons (2, Nil))) -> Cons (2, Nil)">::
    (aseq (Seq.of_list [2]) (Seq.drop 1 (Seq.of_list [1; 2])));
    "drop 2 Cons (1, (Cons (2, Nil))) -> Nil">::
    (aseq Nil (Seq.drop 2 (Seq.of_list [1; 2])));
    "drop 1 Nil -> fails">::
    (fun _ -> assert_equal (Error "end of list") (try Ok (Seq.drop 1 Nil) with _ -> Error "end of list")); 
    "map ((+) 1) Cons(1, Nil)">::
    (aseq (Seq.of_list [2]) (Seq.map ((+) 1) (Seq.of_list [1])));
  ]

let () =
  run_test_tt_main ("tests" >::: tests)
