let first (a, _) = a
let second (_, a) = a

let census = [('1', 4); ('2', 2); ('3', 2); ('4', 3); ('5', 1); ('6', 2)]

let rec lookup key = function
  | [] -> None
  | (k, v)::t -> if k == key then Some v else lookup key t

let rec add k v = function
  | [] -> [(k, v)]
  | (k', v')::t when k' == k -> (k, v) :: t
  | (k', v')::t when k' <> k -> (k', v') :: add k v t
