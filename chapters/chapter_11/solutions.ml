(* Chapter 11. Growing Trees - p. 81 - 86 *)
module Tree = struct
  type 'a tree =
    | Br of 'a * 'a tree * 'a tree
    | Lf

  (* To calculate the number of elements in the tree, 
     we just count one for each branch, and zero for each leaf *)
  let rec size = function
    | Br (_, l, r) -> 1 + size l + size r
    | Lf -> 0

  let%test _ = 0 == (size Lf)
  let%test _ = 1 == (size (Br (0, Lf, Lf)))
  let%test _ = 3 == (size (Br (0, (Br (0, Lf, Lf)), (Br (0, Lf, Lf)))))

  (* A similiar function can be used to add up all the integers in an int
    tree *)
  let rec total = function
    | Br (x, l, r) -> x + total l + total r
    | Lf -> 0

  let%test _ = 0 == (total Lf)
  let%test _ = 1 == (total (Br (1, Lf, Lf)))
  let%test_unit _ = 
    let open OUnit2 in
    let open Printers in
    assert_equal 6 (total (Br (1, (Br (2, Lf, Lf)), (Br (3, Lf, Lf))))) ~printer:pint

  (* The depth is longest path from the root of the tree to a leaf *)
  let rec maxdepth = function
    | Br (_, l, r) -> 
      let max x y = if x > y then x else y in
      1 + max (maxdepth l) (maxdepth r)
    | Lf -> 0

  let%test _ = 0 == (maxdepth Lf)
  let%test _ = 1 == (maxdepth (Br (1, Lf, Lf)))
  let%test_unit _ = 
    let open OUnit2 in
    let open Printers in
    assert_equal 3 (maxdepth (Br (1, (Br (2, Lf, Lf)), (Br (3, Lf, (Br (0, Lf, Lf))))))) ~printer:pint

  (* Now consider extracting all of the elements from a atree into a list.
     [...] put all elements on the left branch before the current element,
     and all the elements in the right branch after. This is arbitrary [...] *)
  let rec to_list = function
    | Br (x, l, r) -> to_list l @ [x] @ to_list r
    | Lf -> []

  let%test _ = [] == (to_list Lf)
  let%test_unit _ =
    let open OUnit2 in
    assert_equal [1] (to_list (Br (1, Lf, Lf)))
  let%test_unit _ = 
    let open OUnit2 in
    let open Printers in
    assert_equal [2; 1; 3; 0] (to_list (Br (1, (Br (2, Lf, Lf)), (Br (3, Lf, (Br (0, Lf, Lf))))))) ~printer:pintlist

  (* Here is how we map over trees *)
  let rec map f = function
    | Br (x, l, r) -> Br (f x, map f l, map f r)
    | Lf -> Lf

  let%test_unit _ = 
    let open OUnit2 in
    assert_equal (Br (2, Lf, Lf)) (map ((+) 1) (Br (1, Lf, Lf)))
end