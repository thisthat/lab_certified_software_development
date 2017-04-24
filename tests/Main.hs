module Main (main) where

import TestHeap
import Prelude


main = do
    print "Test Parent Position"
    test_parent_position
    print "Test Left Position"
    test_left_position
    print "Test Right Postion"
    test_right_position
    print "Test build heap"
    test_heap

