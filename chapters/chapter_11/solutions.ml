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

  (* 1 - Write a function of type a' -> a' tree -> bool to determine if a given element is in a tree *)
  let rec contains x = function
    | Lf -> false
    | Br (x', _, _) when x' = x -> true
    | Br (_, l, r) -> contains x l || contains x r

  let%test _ = false == (contains None Lf)
  let%test_unit _ =
    let open OUnit2 in
    assert_equal true (contains 1 (Br (1, Lf, Lf)))
  let%test_unit _ =
    let open OUnit2 in
    assert_equal false (contains 0 (Br (1, Lf, Lf)))
  let%test_unit _ = 
    let open OUnit2 in
    assert_equal true (contains 0 (Br (1, (Br (2, Lf, Lf)), (Br (3, Lf, (Br (0, Lf, Lf)))))))

  (* 2 - Write a function which flips a tree left to right such that, if it were drawn on paper,
     it would appear to be a mirror image *)
  let rec flip = function
    | Lf -> Lf
    | Br(x, l, r) -> Br(x, flip r, flip l)
  
  let%test_unit _ = 
    let open OUnit2 in
    assert_equal (Br (1, Lf, Lf)) (flip (Br (1, Lf, Lf))) 
  let%test_unit _ = 
    let open OUnit2 in
    assert_equal (Br (1, Lf, Br (2, Lf, Lf))) (flip (Br (1, Br (2, Lf, Lf), Lf))) 
  let%test_unit _ = 
    let open OUnit2 in
    assert_equal (Br (1, (Br (3, (Br (0, Lf, Lf)), Lf)), (Br (2, Lf, Lf)))) (flip (Br (1, (Br (2, Lf, Lf)), (Br (3, Lf, (Br (0, Lf, Lf)))))))

  (* 3. Write a function to determine if two trees have the same shepe, irrespective of the actual values of
     the elements *)
  let rec equal_shape a b = match (a, b) with
    | (Lf, Lf) -> true
    | (Br(_, al, ar), Br(_, bl, br)) -> equal_shape al bl && equal_shape ar br
    | _ -> false

  let%test_unit _ = 
    let open OUnit2 in
    assert_equal true (equal_shape Lf Lf)

  let%test_unit _ = 
    let open OUnit2 in
    assert_equal false (equal_shape Lf (Br (0, Lf, Lf)))

  let%test_unit _ = 
    let open OUnit2 in
    assert_equal true (equal_shape (Br (1, Lf, Lf)) (Br (0, Lf, Lf)))

  let%test_unit _ = 
    let open OUnit2 in
    assert_equal false (equal_shape (Br (1, Br (0, Lf, Lf), Lf)) (Br (0, Lf, Br (0, Lf, Lf))))

  let%test_unit _ = 
    let open OUnit2 in
    assert_equal true (equal_shape (Br (1, Br (0, Lf, Lf), Lf)) (Br (0, Br (0, Lf, Lf), Lf)))
end

module BinarySearchTree = struct
  type 'a tree =
    | Br of (int * 'a) * 'a tree * 'a tree
    | Lf

  let rec lookup k = function
    | Lf -> None
    | Br ((k', v), _, _) when k' = k -> Some v
    | Br ((k', _), l, _) when k' > k -> lookup k l
    | Br ((k', _), _, r) when k' < k -> lookup k r
    | _ -> failwith "unreachable"

  let rec insert (k, v) = function
    | Lf -> Br ((k, v), Lf, Lf)
    | Br ((k', _), l, r) when k = k' -> Br ((k, v), l, r)
    | Br ((k', v'), l, r) when k < k' -> Br ((k', v'), insert (k, v) l, r)
    | Br ((k', v'), l, r) when k > k' -> Br ((k', v'), l, insert (k, v) r)
    | _ -> failwith "unreachable"

  let rec to_list = function
    | Br (x, l, r) -> to_list l @ [x] @ to_list r
    | Lf -> []

  (* 4. Write a function of_list which builds a tree representation of a dictionary from a list
     representation of a dictionary *)
  let rec of_list = function
    | [] -> Lf
    | (k, v)::t -> insert (k, v) (of_list t)

  let%test_unit _ = 
    let open OUnit2 in
    assert_equal Lf (of_list [])

  let%test_unit _ = 
    let open OUnit2 in
    let rec ptree f = function
      | Lf -> "Lf"
      | Br (v, l, r) -> Printf.sprintf "Br (%s, %s %s)" (f v) (ptree f l) (ptree f r) in
    assert_equal (Br ((2, ""), Br ((1, ""), Lf, Lf), Br ((3, ""), Lf, Lf))) (of_list [(3, ""); (1, ""); (2, "")]) ~printer:(ptree (fun (k, v) -> Printf.sprintf "(%i, %s)" k v))

  (* 5. Write a function to combine two dictionaries represented as trees into one. In case of clashing
     keys, prefer the value from the first dictionary *)
  let combine a b = to_list a @ (to_list b) |> of_list
end
