module Doctest where

type Bag a = [(a, Int)]


-- (a) Define the function ins that inserts an element into a multiset.
ins :: Eq a => a -> Bag a -> Bag a 
ins a [] = [(a, 1)]
ins a ((head, counter):tail) | a == head = ((head, counter+1) : tail)
                             | otherwise = ((head, counter) : ins a tail)

-- | ins
--   >>> ins 5 [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   [(5,2),(7,3),(2,1),(3,2),(8,1)]
--
--   >>> ins 7 [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   [(5,1),(7,4),(2,1),(3,2),(8,1)]
--
--   >>> ins 2 [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   [(5,1),(7,3),(2,2),(3,2),(8,1)]
--

-- (b) Define the function del that removes a single element from a multiset.
del :: Eq a => a -> Bag a  -> Bag a 
del a [] = []
del a ((head, counter):tail) = if a == head && counter == 1 then tail else
                               if a == head && counter >= 2 then (head, counter-1) : tail else
                               (head, counter) : del a tail 

-- | del
--   >>> del 5 [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   [(7,3),(2,1),(3,2),(8,1)]
--
--   >>> del 7 [(5,1),(7,2),(2,1),(3,2),(8,1)]
--   [(5,1),(7,1),(2,1),(3,2),(8,1)]
--
--   >>> del 8 [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   [(5,1),(7,3),(2,1),(3,2)]
--

-- (c) Define a function bag that take a list of values and produces a multiset representation
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (h:t) = ins h (bag t)

-- | bag
--   >>> bag [2, 3, 3, 5, 7, 7, 7, 8]
--   [(8,1),(7,3),(5,1),(3,2),(2,1)]
--
--   >>> bag [1, 3, 3, 8, 5, 3, 3, 5, 7, 7, 7, 8]
--   [(8,2),(7,3),(5,2),(3,4),(1,1)]
--


-- (d) Define a function subbag that determines whether or not 
--     its first argument bag is contained in the second.
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag ((h, counter):t) [] = False
subbag ((h, counter):t) ((h', counter'):t') | h == h' && counter <= counter' && subbag t t' == True = True 
                                            | h /= h' && subbag ((h, counter):t) t' == True = True 
                                            | otherwise = False


-- | subbag
--
--   >>> subbag [(6,1),(7,3),(2,1),(3,2),(8,1)] [(6,2),(7,3),(2,5),(3,2),(8,2)]
--   True

--   >>> subbag [(5,1),(7,3),(2,1),(3,2),(8,1)] [(5,2),(7,3),(2,1),(3,2),(8,1)]
--   True
--
--   >>> subbag [(7,3),(2,1),(3,2),(8,1)] [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   True
--
--   >>> subbag [(7,2),(2,1),(3,2),(8,1)] [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   True
--
--   >>> subbag [(5,1),(7,3),(2,1),(3,2),(8,1)] [(7,3),(2,1),(3,2),(8,1)]
--   False
--
--   >>> subbag [(5,1),(7,3),(2,1),(3,2),(8,2)] [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   False
--
--   >>> subbag [(5,2),(7,3),(2,1),(3,2),(8,1)] [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   False
--
--   >>> subbag [(5,1),(7,3),(2,1),(8,1)] [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   True
--
--   >>> subbag [] []
--   True
--
--   >>> subbag [] [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   True
--

-- (e) Define a function isSet that tests whether a bag is actually a set, 
--     which is the case when each element occours only once

isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((head, counter):tail) = if (counter <= 1) && (isSet tail == True) then True else False

-- | isSet
--   >>> isSet [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   False
--
--   >>> isSet [(5,1),(7,1),(2,1),(3,1),(8,1)]
--   True
--
--   >>> isSet [(5,1),(7,1),(2,1),(3,2),(8,1)]
--   False
--

-- (f) Define a function size that computes the number of elements contained in a bag

size :: Bag a -> Int
size [] = 0
size ((head, counter):t) = counter + size t

-- | Size
--   >>> size [(5,1),(7,3),(2,1),(3,2),(8,1)]
--   8
--
--   >>> size [(5,4),(7,1),(2,1),(3,2),(8,1)]
--   9
--
--   >>> size []
--   0
--
--   >>> size [(5,1)]
--   1
--

applyAll :: [a->b] -> a -> [b]
applyAll [] _ = []
applyAll (f:fs) x = (f x) : (applyAll fs x)

-- | applyAll
--   >>> applyAll [even,(<9)] 7
--   [False,True]
--
--   >>> applyAll [(^2),succ] 7
--   [49,8]
--

type Property a = [a -> Bool]

digit :: Property Int
digit = [(>=0),(<10)]

satisfies :: Property a -> a -> Bool
satisfies x y = if and(applyAll x y) then True else False

-- | satisfies
--   >>> satisfies [even] 7
--   False
--
--   >>> satisfies digit 7
--   True
--

power :: (a->a) -> Int -> a -> a
power f n v = if n == 0 then v else 
              if n == 1 then f v else
              f (power f (n-1) v) 

-- | power
--   >>> power tail 3 [1..10]
--   [4,5,6,7,8,9,10]
--
--   >>> power ("Ho"++) 5 []
--   "HoHoHoHoHo"
--


plus :: Int -> Int -> Int
plus x y = power succ y x

-- | plus
--   >>> plus 9 4
--   13
--
--   >>> plus 0 0
--   0
--

times :: Int -> Int -> Int
times x y = power (+x) y 0

-- | times
--   >>> times 9 4
--   36
--
--   >>> times 0 0
--   0
--   >>> times 1 0
--   0
--





