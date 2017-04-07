module TestHeap where

import Test.HUnit
import Prelude
import OurHeap


test_heap = build_heap example1 @?= expected1

test_parent_position = map heap_parent_position [1..10] @?= [0,1,1,2,2,3,3,4,4,5]
test_left_position = map heap_left_position [1..10] @?= [2,4,6,8,10,12,14,16,18,20]
test_right_position = map heap_right_position [1..10] @?= [3,5,7,9,11,13,15,17,19,21]

test_sum_position = foldr (++) [] children @?= [2..21]
--the list of left and right child from 1..10 should be from 2 to 21
    where
        children = zipWith merge left right -- merge togheter the lists
        left     = map heap_left_position [1..10]   --compute list for left
        right    = map heap_right_position [1..10]  --compute for right
        merge    = \x y -> x:y:[]  -- merge together each element in position i-th of the two list


-- help functions


-- Test Data
example1 = [   15,
            6,     4,
          8,  5,  3,1,
         2,7
         ]
expected1 = [   15,
             8,     4,
           7, 5,  3,  1,
          2,6           ]