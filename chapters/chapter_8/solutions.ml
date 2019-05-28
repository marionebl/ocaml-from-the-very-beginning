(* Chapter 8 - Looking Things up, p. 61 - 64 *)
let first (a, _) = a
let second (_, a) = a

let census = [('1', 4); ('2', 2); ('3', 2); ('4', 3); ('5', 1); ('6', 2)]

let rec lookup key = function
  | [] -> None
  | (k, v)::t -> if k == key then Some v else lookup key t

let rec add k v = function
  | [] -> [(k, v)]
  | (k', _)::t when k' == k -> (k, v) :: t
  | (k', v')::t when k' <> k -> (k', v') :: add k v t
  | _ -> failwith "unreachable"

let rec remove k = function
  | [] -> []
  | (k', _)::t when k' == k -> t
  | (k', v')::t when k' <> k -> (k', v') :: remove k t
  |  _ -> failwith "unreachable"

let exists k d =
  match lookup k d with
  | None -> false
  | Some(_) -> true

(* 1. Write a function to determine the number of different keys in a dictionary *)
let distinct d =
  let rec distinct' s = function
  | [] -> s
  | (k, _)::t ->
    let s' = if List.exists (fun k' -> k' == k) s then s else k :: s in
    distinct' s' t
  in
  distinct' [] d |> List.length

(* 2. Write a function replace which is like add, but raises Not_found if the key is not already there *)
let rec replace k v = function
  | [] -> raise Not_found
  | (k', _)::t when k == k' -> (k, v) :: t
  | (k', v')::t when k <> k' -> (k', v') :: replace k v t
  | _ -> failwith "unreachable"

(* 3. Write a function to build a dictionary from two equal length lists,
  one containing keys and another containing values. Raise the exception
  Invalid_argument if the lists are not of equal length *)
let rec build ks vs =
  if List.length ks <> List.length vs then
    raise (Invalid_argument "list lengths are not equal")
  else
    match (ks, vs) with
    | ([], []) -> []
    | (k::kt, v::vt) -> (k, v) :: build kt vt
    | _ -> failwith "unreachable"

(* 4. Now write the inverse function: given a dictionary, return the pair of two lists -
   the first containing all the keys, the second containing all the values. *)
let partition d = 
  let rec partition' ks vs = function
  | [] -> (ks, vs)
  | (k, v)::t -> partition' (k :: ks) (v :: vs) t
  in
  partition' [] [] (List.rev d)

(* 5. Define a function to turn any list of pairs into a dictionary. If duplicate keys are found,
   the value associated with the first occurrence of the key should be kept *)
let dict ps =
  let rec dict' ps d =
    match ps with
    | [] -> d
    | (k, _)::t when exists k d -> dict' t d
    | (k, v)::t -> dict' t ((k, v) :: d)
  in
  dict' ps [] |> List.rev

(* 6. Write the function union a b which forms the union of two dictionaries. The union of two dictionaries
   is the dictionary containing all the entries in one or the other or both. In the case that a key is contained
   in both dictionaries, the value in the first should be preferred *)
let union a b =
  let rec union' a b =
    match b with
    | [] -> a
    | (k, _)::t when exists k a -> union' a t
    | (k, v)::t -> union' ((k, v) :: a) t
  in
  union' a b |> List.rev