open! OUnit2
open! QCheck
open! Bitree

(* validation of binary tree *)
let rec isBitreeValid = function
  | Leaf -> true
  | Node (curr, l, r) ->
      let leftIsSmaller =
        match l with Leaf -> true | Node (v, _, _) -> v < curr
      in
      let rightIsBigger =
        match r with Leaf -> true | Node (v, _, _) -> v > curr
      in
      isBitreeValid l && isBitreeValid r && leftIsSmaller && rightIsBigger

(* association of merge *)
let association =
  QCheck.Test.make ~count:1000 ~name:"association of merge "
    QCheck.(triple (list small_nat) (list small_nat) (list small_nat))
    (fun (a_list, b_list, c_list) ->
      let a = Bitree.fromList a_list in
      let b = Bitree.fromList b_list in
      let c = Bitree.fromList c_list in
      Bitree.isEqualsAsSet
        (Bitree.merge (Bitree.merge a b) c)
        (Bitree.merge a (Bitree.merge b c)) )

(* insert element retains binary tree*)
let insertToBitree =
  QCheck.Test.make ~count:1000 ~name:"insert bitree "
    QCheck.(pair small_nat (list small_nat))
    (fun (to_insert, v_list) ->
      let tree = Bitree.fromList v_list in
      let tree_insert = Bitree.insert to_insert tree in
      isBitreeValid tree && isBitreeValid tree_insert )

(* erase element retains binary tree*)
let eraseToBitree =
  QCheck.Test.make ~count:1000 ~name:"erase bitree "
    QCheck.(list small_nat)
    (fun v_list ->
      let tree = Bitree.fromList v_list in
      let tree_erase =
        match tree with
        | Leaf -> Leaf
        | Node (curr, _, _) -> Bitree.erase curr tree
      in
      isBitreeValid tree && isBitreeValid tree_erase )

(* merge with empty retains tree unchanged *)
let mergeEmpty =
  QCheck.Test.make ~count:1000 ~name:"merge empty"
    QCheck.(list small_nat)
    (fun x_list ->
      let tree = Bitree.fromList x_list in
      let left = Bitree.merge tree Bitree.empty in
      let right = Bitree.merge Bitree.empty tree in
      Bitree.isSame tree left && Bitree.isSame tree right )

(* string binary tree *)
let stringTest =
  QCheck.Test.make ~count:1000 ~name:"string test"
    QCheck.(list small_string)
    (fun x_list ->
      let tree = Bitree.fromList x_list in
      let rec contains = function
        | head :: tail -> Bitree.contains head tree && contains tail
        | [] -> true
      in
      contains x_list )

(* float binary string *)
let floatTest =
  QCheck.Test.make ~count:1000 ~name:"float test"
    QCheck.(list float)
    (fun x_list ->
      let tree = Bitree.fromList x_list in
      let rec contains = function
        | head :: tail -> Bitree.contains head tree && contains tail
        | [] -> true
      in
      contains x_list )

let _ =
  let open OUnit in
  run_test_tt_main
    ( "tests"
    >::: List.map QCheck_ounit.to_ounit_test
           [ association
           ; mergeEmpty
           ; insertToBitree
           ; eraseToBitree
           ; stringTest
           ; floatTest ] )
