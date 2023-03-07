open! OUnit2
open Bitree

(* sum of list elements *)
let rec sum = function [] -> 0 | head :: tail -> head + sum tail

(* empty tree has size 0 *)
let sizeOfEmpty _ = OUnit2.assert_equal 0 (Bitree.size Bitree.empty)

(* insert into empty tree creates a new node *)
let insertIntoEmpty _ =
  OUnit2.assert_equal
    (Bitree.Node (5, Bitree.Leaf, Bitree.Leaf))
    (Bitree.insert 5 Bitree.empty)

(* contains finds an existing element *)
let containsOfExisting _ =
  OUnit2.assert_equal true (Bitree.contains 5 (Bitree.insert 5 Bitree.empty))

(* contains does not find missing values *)
let containsOfMissing _ =
  OUnit2.assert_equal false
    (Bitree.contains 5 (Bitree.insert 7 Bitree.empty))

(* min raises exception *)
let minRaisesException _ =
  OUnit2.assert_raises (Failure "Called on empty tree") (fun () ->
      Bitree.min Bitree.empty )

(* erase removes a value from a tree *)
let eraseRemoveElement _ =
  OUnit2.assert_equal true
    (Bitree.isEqualsAsSet
       (Bitree.insert 2 Bitree.empty)
       (Bitree.erase 1 (Bitree.insert 2 (Bitree.insert 1 Bitree.empty))) )

(* left fold *)
let foldl _ =
  let v_list = [1; 2; 3; 4; 5; 6] in
  let expected = v_list |> sum in
  let tree = Bitree.fromList v_list in
  let left_fold = Bitree.foldl (fun acc curr -> acc + curr) 0 tree in
  OUnit2.assert_equal expected left_fold

(* right fold *)
let foldr _ =
  let v_list = [1; 2; 3; 4; 5; 6] in
  let expected = v_list |> sum in
  let tree = Bitree.fromList v_list in
  let right_fold = Bitree.foldr (fun acc curr -> acc + curr) 0 tree in
  OUnit2.assert_equal expected right_fold

let tests =
  "tests"
  >::: [ "sizeOfEmpty" >:: sizeOfEmpty
       ; "insertIntoEmpty" >:: insertIntoEmpty
       ; "containsOfExisting" >:: containsOfExisting
       ; "containsOfMissing" >:: containsOfMissing
       ; "minRaisesException" >:: minRaisesException
       ; "eraseRemoveElement" >:: eraseRemoveElement
       ; "foldl" >:: foldl
       ; "foldr" >:: foldr ]

let () = run_test_tt_main tests
