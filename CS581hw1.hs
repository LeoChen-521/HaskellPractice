-- Name: Xiaoru Chen (ID:934339052), chenxia2@oregonstate.edu
-- CS581
-- Instructor: Martin Erwig
-- 2022.1.19


module CS581HW1 where

type Bag a = [(a, Int)]


-- (a) Define the function ins that inserts an element into a multiset.
ins :: Eq a => a -> Bag a -> Bag a 
ins a [] = [(a, 1)]
ins a ((head, counter):tail) | a == head = ((head, counter+1) : tail)
                             | otherwise = ((head, counter) : ins a tail)

-- (b) Define the function del that removes a single element from a multiset.
del :: Eq a => a -> Bag a  -> Bag a 
del a [] = []
del a ((head, counter):tail) = if a == head && counter == 1 then tail else
                               if a == head && counter >= 2 then (head, counter-1) : tail else
                               (head, counter) : del a tail 

-- (c) Define a function bag that take a list of values and produces a multiset representation
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (h:t) = ins h (bag t)

-- (d) Define a function subbag that determines whether or not 
--     its first argument bag is contained in the second.
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag ((h, counter):t) [] = False
subbag ((h, counter):t) ((h', counter'):t') | h == h' && counter <= counter' && subbag t t' == True = True 
                                            | h /= h' && subbag ((h, counter):t) t' == True = True 
                                            | otherwise = False

-- (e) Define a function isSet that tests whether a bag is actually a set, 
--     which is the case when each element occours only once

isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((head, counter):tail) = if (counter <= 1) && (isSet tail == True) then True else False


-- (f) Define a function size that computes the number of elements contained in a bag

size :: Bag a -> Int
size [] = 0
size ((head, counter):t) = counter + size t

-- Exercise 2. Higher Order Functions

-- (a) Define a higher-order function applyAll that takes a list of functions fs and a value x, 
--     applies all functions in the list fs to x, and returns the list of all results. 

applyAll :: [a->b] -> a -> [b]
applyAll [] _ = []
applyAll (f:fs) x = (f x) : (applyAll fs x)
-- map?

-- (b) Using the function applyAll from part (a) and the predefined function 
--     and :: [Bool] -> Bool that checks whether all values in a list of booleans are True, 
--     define a function satisfies that checks whether a value satisfies a composite property.

type Property a = [a -> Bool]
-- a is a value of Bool

digit :: Property Int
digit = [(>=0),(<10)]

--    y必须满足x里面的所有判定。x一串functions. 
--    English: y must fit all the functions in x, x is a list of functions.
--    apply all the functions to y, it returns a list of bool.
--    Property a is a collection of functions a->Bool, such as even, digit, etc.
satisfies :: Property a -> a -> Bool
satisfies x y = if and(applyAll x y) then True else False


-- (c) Define a function power that applies a function repeatedly (that is, exactly n times) to a value. 
--     In other words, for a function f and a value x, power computes f n (x).

power :: (a->a) -> Int -> a -> a
power f n v = if n == 0 then v else 
              if n == 1 then f v else
              f (power f (n-1) v) 

-- (d) Define the following two functions for adding and multiplying integers 
--     using the function power from part (c). 
--     (Note: For the definition of plus you should use succ as the argument function for power.)
-- add 1 y times
-- e.g 5+6就是对5进行6次+1
-- English: 5+6 means add 6 times 1 to 5
plus :: Int -> Int -> Int
plus x y = power succ y x

-- 5 x 6 等于把什么事情做 N次 呢？
-- 5 + 5 + 5 + 5 + 5 + 5
times :: Int -> Int -> Int
times x y = power (+x) y 0





