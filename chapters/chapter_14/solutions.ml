(* Chapter 14. The Other Numbers, p. 111 - 114 *)
module Vector = struct
  type t = float * float

  let make (x0, y0) (x1, y1): t = (x1 -. x0, y1 -. y0)

  let length (x, y) = sqrt (x *. x +. y *. y)

  let offset (x, y) (px, py): t = (x +. px, y +. py)

  let scale l (x, y): t =
    if length (x, y) == 0. then
      (x, y)
    else 
      let f = l /. length (x, y) in
      (x *. f, y *. f) 
end

module Solutions = struct
  (* 1. Give a function which rounds a positive floating-point number to the nearest whole number,
   returning another floating point number *)
  let round n = 
    let c = ceil n in
    let f = floor n in
    if c -. n <= n -. f then c else f

  (* 2. Write a function to find the point equidistant from two given points in two dimensions *)
  let between (x, y) (x', y') =
    ((x +. x') /. 2., (y +. y') /. 2.)

  (* 3. Write a function to separate a floating-point number into its whiole and fractioned parts.
     Return them as a tuple of type float * float *)
  let sep x = (floor x, x -. floor x)

  (* 4. Write a function star of type float → unit which, given a floating-point number between 
     zero and one, draws an asterisk to indicate the position. 
     An argument of zero will result in an asterisk in column one, 
     and an argument of one an asterisk in column fifty. *)
  let star n = 
    let n' = (int_of_float (n *. 49.)) in
    String.init 50 (fun i -> if i == n' then '*' else ' ')

  (* 5. Now write a function plot which, given a function of type float → float, a range,
     and a step size, uses star to draw a graph *)
  let plot (fn: float -> float) ((x, y): float * float) (step: float) =
    let s = ref x in
    let r = ref "" in
    while !s < y do
      r := !r ^ (star (fn !s)) ^ "\n";
      s := !s +. step
    done;
    !r
end

let%test_unit "Solutions.round" =
  let open OUnit2 in
  let printer = string_of_float in
  assert_equal 0. (Solutions.round 0.) ~printer ~cmp:cmp_float;
  assert_equal 1. (Solutions.round 1.49) ~printer ~cmp:cmp_float;
  assert_equal 3. (Solutions.round 2.5) ~printer ~cmp:cmp_float

let%test_unit "Solutions.between" =
  let open OUnit2 in
  let printer = fun (x, y) -> Printf.sprintf "(%f, %f)" x y in
  let cmp = fun (x, y) (x', y') -> cmp_float x x' && cmp_float y y' in
  assert_equal (0., 2.) (Solutions.between (-1., 1.) (1., 3.)) ~printer ~cmp

let%test_unit "Solutions.sep" =
  let open OUnit2 in
  let printer = fun (x, y) -> Printf.sprintf "(%f, %f)" x y in
  let cmp = fun (x, y) (x', y') -> cmp_float x x' && cmp_float y y' in
  assert_equal (0., 0.) (Solutions.sep 0.) ~printer ~cmp;
  assert_equal (1., 0.) (Solutions.sep 1.) ~printer ~cmp;
  assert_equal (13., 0.337) (Solutions.sep 13.337) ~printer ~cmp

let%test_unit "Solutions.star" =
  let open OUnit2 in
  let fill n = String.make n ' ' in
  assert_equal ("*" ^ fill 49) (Solutions.star 0.)