module TestHeap where

import Test.HUnit
import Prelude
import OurHeap


test_1 = build_heap example1 @?= expected1
test_2_always_ok = build_heap example1 @?= example1


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