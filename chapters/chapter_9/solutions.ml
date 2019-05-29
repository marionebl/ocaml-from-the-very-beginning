(* 1. Rewrite the summary paragraph at the end of this chapter for the three argument function `g a b c` *)
(* Solution: The function g a b c hast type a' -> b' -> c' -> d'. Given a' it returns b' -> c' -> 'd, which
   given 'b returns 'c -> 'd, which given 'c returns 'd *)

(* 2. Recall the runction `member x l` which determines of an element x is contained in a list l. What is its
   type? What is the type of member x? Use partial application to write a function member_all x ls
   which determines if an element is a member of all the lists in the list of lists ls. *)
(* Solution: member x l is of type 'a -> 'a list -> bool. Accordingly, member x is of type 'a list -> bool. *)
let rec member x = function
  | [] -> false 
  | h::t -> h == x || member x t

let rec map f = function
  | [] -> []
  | h::t -> f h :: map f t

let member_all x l =
  let rec member_all' x p = function
    | [] -> p
    | h::t -> member x h && member_all' x true t
  in
  member_all' x false l

(* 3. Why can we not write a function to halve all the elements of a list like this: map ((/) 2) [10; 20; 30]?
   Write a suitable division function which can be partially applied in the manner we require. *)
(* Solution: (/) x y divides x by y, but we need y by x. A generalized solution would be a flip function
   that inverts argument order *)
let flip fn b a = fn a b 
let half = map (flip (/) 2) 

(* 4. Write a function mapll which maps a function over lists of lists of lists. You must not use the
   let rec construct. Is it possible to write a function which works like map, mapl or mapll depending
   upon the list given to it? *)
let mapll f l = map f |> map |> flip map l

(* 5. Write a function truncate which takes an integer and a list of lists, and returns a list of lists
   each of which have been truncated to a given length. If a list is shorter that the given length it is
   unchanged. Make use of partial application *)
let rec trunc x l =
  match (x, l) with
  | (0, _) | (_, []) -> []
  | (_, h::t) -> h :: trunc (x - 1) t

let truncate x = trunc x |> map

(* 6. Write a function which takes a list of lists of integers and returns the list composed of all the first
   elements of the lists. If a list is empty, a given number should be used in place of its first element. *)
let rec compose (d: int) (l: int list list): int list =
  let hd l = try List.hd l with _ -> d in
  match l with
  | [] -> []
  | h::t -> hd h :: compose d t