type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let empty = Leaf

let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r

let rec insert value = function
  | Leaf -> Node (value, Leaf, Leaf)
  | Node (curr, l, r) when value < curr -> Node (curr, insert value l, r)
  | Node (curr, l, r) when value > curr -> Node (curr, l, insert value r)
  | Node (curr, l, r) -> Node (curr, l, r)

let rec contains value = function
  | Leaf -> false
  | Node (curr, l, _) when value < curr -> contains value l
  | Node (curr, _, r) when value > curr -> contains value r
  | Node (_, _, _) -> true

let rec min = function
  | Leaf -> failwith "Called on empty tree"
  | Node (curr, Leaf, _) -> curr
  | Node (_, l, _) -> min l
