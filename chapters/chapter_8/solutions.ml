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