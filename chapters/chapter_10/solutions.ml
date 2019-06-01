(* 1. Design a new type rect for representing rectangles. Treat squares as a special case. *)
type point = int * int

type rect =
    | Rectangle of point * point
    | Square of point * int

(* 2. Now write a function of type rect -> int to calculate the area of a given rect *)
let area = function
  | Square(_, l) -> l * l
  | Rectangle((ax, ay), (bx, by)) -> abs (bx - ax) * abs (by - ay)

(* 3. Write a function which rotates a rect such that is is at least as tall as it is wide *)
let x = function
  | Square(_, l) -> l
  | Rectangle((ax, _), (bx, _)) -> abs (bx - ax)

let y = function
  | Square(_, l) -> l
  | Rectangle((_, ay), (_, by)) -> abs (by - ay)

let erect r = match r with
  | Square _ -> r
  | Rectangle _ when y r >= x r -> r
  | Rectangle ((ax, ay), (bx, by)) -> Rectangle ((ax, ay), (ay - by, ax - bx))

(* 4. Use this function to write one which, given a rect list, returns another such list which has the
   smallest total width and whose members are sorted narrowest first *)
let stack rs = rs 
  |> List.map erect
  |> List.sort (fun a b -> x a - x b)

(* 5. Write take, drop and fuctions for the sequence type *)
module Seq = struct
  type 'a t = Nil | Cons of 'a * 'a t

  let rec to_list = function
    | Nil -> []
    | Cons(h, t) -> h :: (to_list t)

  let rec of_list = function
    | [] -> Nil
    | h::t -> Cons(h, of_list t)

  let rec take n = function
    | Nil when n > 0 -> failwith "end of list"
    | Cons(_) when n = 0 -> Nil
    | Nil -> Nil
    | Cons(h, t) -> Cons (h, take (n - 1) t)

  let rec drop n = function
    | Nil when n > 0 -> failwith "end of list"
    | Nil -> Nil
    | Cons(h, t) when n = 0 -> Cons(h, t)
    | Cons(_, t) -> drop (n - 1) t

  let rec map f = function
    | Nil -> Nil
    | Cons(h, t) -> Cons(f h, map f t)
end

(* 6. Extend the expr type and the evaluate function to allow raising a number to a power *)
type expr =
  | Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multliply of expr * expr
  | Divide of expr * expr
  | Raise of expr * expr

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let rec ev = function
  | Num _ -> fun x _ -> x
  | Add _ -> (+)
  | Subtract _ -> (-)
  | Multliply _ -> fun x y -> x * y
  | Divide _ -> (/)
  | Raise _ -> pow

let rec evaluate exp = 
  match exp with
  | Num n -> (ev exp) n 0
  | Add(x, y) 
  | Subtract(x, y) 
  | Multliply(x, y) 
  | Divide(x, y) 
  | Raise(x, y) -> (ev exp) (evaluate x) (evaluate y)
  
(* 7. Use the option type to deal with the problem that Division_by_zero may be raised from the evaluate function *)
let rec evaluate_opt exp =
  try Ok (evaluate exp) with
  | Division_by_zero -> Error "Division By Zero"
  | exn -> raise exn