type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let empty = Leaf

let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r

let rec insert value = function
  | Leaf -> Node (value, Leaf, Leaf)
  | Node (curr, l, r) when value < curr -> Node (curr, insert value l, r)
  | Node (curr, l, r) when value = curr -> Node (curr, l, r)
  | Node (curr, l, r) -> Node (curr, l, insert value r)

let rec contains value = function
  | Leaf -> false
  | Node (curr, l, _) when value < curr -> contains value l
  | Node (curr, _, _) when value = curr -> true
  | Node (_, _, r) -> contains value r

let rec min = function
  | Leaf -> failwith "Called on empty tree"
  | Node (curr, Leaf, _) -> curr
  | Node (_, l, _) -> min l

let rec mergeAfterTopErase l r =
  match (l, r) with
  | Leaf, Leaf -> Leaf
  | l, Leaf -> l
  | Leaf, r -> r
  | l, r ->
      let minValue = min r in
      Node (minValue, l, erase minValue r)

and erase value = function
  | Leaf -> Leaf
  | Node (curr, l, _) when value < curr -> erase value l
  | Node (curr, l, r) when value = curr -> mergeAfterTopErase l r
  | Node (_, _, r) -> erase value r

let rec filter f = function
  | Leaf -> Leaf
  | Node (curr, l, r) when f curr -> Node (curr, filter f l, filter f r)
  | Node (_, l, r) -> mergeAfterTopErase (filter f l) (filter f r)

let rec foldr f acc = function
  | Leaf -> acc
  | Node (curr, l, r) ->
      let newAcc = foldr f (f acc curr) r in
      foldl f newAcc l

and foldl f acc = function
  | Leaf -> acc
  | Node (curr, l, r) ->
      let newAcc = foldl f (f acc curr) l in
      foldr f newAcc r

let map f tree = foldl (fun tree x -> insert (f x) tree) Leaf tree

let fromList list = List.fold_left (fun tree x -> insert x tree) Leaf list

let merge t1 t2 = foldl (fun tree x -> insert x tree) t1 t2

let rec isSame t1 t2 =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Leaf, _ -> false
  | _, Leaf -> false
  | Node (curr1, l1, r1), Node (curr2, l2, r2) ->
      curr1 == curr2 && isSame l1 l2 && isSame r1 r2

let isEqualsAsSet t1 t2 =
  size t1 == size t2
  && foldl (fun result x -> result && contains x t2) true t1
