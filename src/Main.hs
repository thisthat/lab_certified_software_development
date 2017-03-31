module Main where

import Prelude
import Data.Maybe -- Should we use the maybe or not?
-- we can use the Nothing to express no children/parent and the Just for the value.


-- basic example of Haskell main
main = do
  putStrLn ""
  putStrLn $ show $ takeN 3 $ numFrom 200
  putStrLn ""


numFrom n = n : (numFrom $ n + 1)

takeN 0 _        = []
takeN _ []       = []
takeN n (x:xs)   = x : take (n-1) xs


-- Heapify Functions
find_max :: Num a => [a] -> a
find_max xs = value
    where
        list = build_heap xs
        value = nth 1 list


insert :: Num a => a -> [a] -> [a]
insert x xs = result
    where
        result = build_heap $ xs ++ [x]

height :: Integral a => [b] -> a
height xs = floor $ logBase 2 l
    where
    l = len xs

build_heap :: Num a => [a] -> [a]
build_heap xs = xs
-- Build Heap will scan from floor(n/2) to 1 (r2l) and heapify it.
-- The reason that it works is because floor(n/2) are leaves in the tree and therefore we can skip them.
-- We can skip because a tree of one element is always a max-heap.
-- Scanning r2l guarantees that each children is a correct max-heap
-- build_heap will assure that each element x in xs >= of both children of x

-- Heapify
-- What are the base cases of the induction? the Nothing if we use Maybe?
-- Given the node @ position i, heapify assure to create an max heap iff (l,r) children of i are max-heap.


-- General Function
nth :: Int -> [a] -> a
nth 1 (x : _)  = x
nth n (_ : xs) = nth (n - 1) xs

len [] = 0
len (x:xs) = 1 + len xs


-- Position
heap_parent_position :: (Integral b, RealFrac a) => a -> b
heap_parent_position i  = floor (i / 2)

heap_left_position :: Int -> Int
heap_left_position   i  = 2 * i

heap_right_position :: Int -> Int
heap_right_position  i  = 1 + (heap_left_position i)

get_children_position :: Int -> (Int,Int)
get_children_position i = (l,r)
    where
        l = heap_left_position i
        r = heap_right_position i


-- Element
heap_parent :: RealFrac a1 => a1 -> [a] -> a
heap_parent i list@(x:xs) = nth (heap_parent_position i) list

heap_left :: Num a => Int -> [a] -> a
heap_left i list@(x:xs) = nth (heap_left_position i) list

heap_right :: Num a => Int -> [a] -> a
heap_right i list@(x:xs) = nth (heap_right_position i) list

heap_children :: Num a => Int -> [a] -> (a, a)
heap_children i list@(x:xs) = (l_val, r_val)
    where
    (l,r) = get_children_position i
    l_val = nth l list
    r_val = nth r list

-- Test Data
example1 = [15,6,4,8,5,3,1,2,7]
expected1 = [15,8,4,7,5,3,1,2,6]