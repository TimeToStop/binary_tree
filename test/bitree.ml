open! OUnit2
open! QCheck
open! Bitree


(* association of merge *)

let rec isBitreeValid = function
  | Leaf -> true
  | Node(curr, l, r) ->
    let leftIsSmaller = match l with Leaf -> true | Node(v, _, _) -> v < curr in
    let rightIsBigger = match r with Leaf -> true | Node(v, _, _) -> v > curr in
    isBitreeValid l && isBitreeValid r && leftIsSmaller && rightIsBigger

let association = 
  QCheck.Test.make ~count:1000 ~name:"association of merge "
    QCheck.(triple (list small_nat) (list small_nat) (list small_nat))
    (fun (a_list, b_list, c_list) -> 
      let a = Bitree.fromList a_list in
      let b = Bitree.fromList b_list in
      let c = Bitree.fromList c_list in
      Bitree.isEqualsAsSet (Bitree.merge (Bitree.merge a b) c) (Bitree.merge a (Bitree.merge b c)))

let insertToBitree = 
  QCheck.Test.make ~count:1000 ~name:"insert bitree "
    QCheck.(pair small_nat (list small_nat))
    (fun (to_insert, v_list) -> 
      let tree = Bitree.fromList v_list in
      let tree_insert = Bitree.insert to_insert tree in
      isBitreeValid tree && isBitreeValid tree_insert)

let eraseToBitree = 
  QCheck.Test.make ~count:1000 ~name:"insert bitree "
    QCheck.(list small_nat)
    (fun v_list -> 
      let tree = Bitree.fromList v_list in
      let tree_erase = match tree with Leaf -> Leaf | Node(curr, _, _) -> Bitree.erase curr tree  in
      isBitreeValid tree && isBitreeValid tree_erase)

let mergeEmpty = 
  QCheck.Test.make ~count:1000 ~name:"merge empty"
  QCheck.(list small_nat)
  (fun x_list -> 
    let tree = Bitree.fromList x_list in
    let left = Bitree.merge tree Bitree.empty in
    let right = Bitree.merge Bitree.empty tree in
    (Bitree.isSame tree left) && (Bitree.isSame tree right))

let () = QCheck_runner.run_tests_main [association; mergeEmpty; insertToBitree; eraseToBitree] ;;
