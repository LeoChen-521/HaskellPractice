







module CS581MT1 where

compute :: Int

compute = if 5 > 2 * 3 then 5 else 2*3


compute1 :: Bool

compute1 = if 2*3 == 3+3 then True else False

compute2 :: Bool
compute2 | 2*3 == 3+3 = True
         | otherwise = False

length0 :: [Int] -> Int
length0 [] = 0
length0 (x:xs) = length0 xs + 1

member :: Int -> [Int] -> Bool
member i [] = False
member i (x:xs) = if i == x then True else member i xs 

delete :: Int -> [Int] -> [Int]
delete i [] = []
delete i (x:xs) = if i == x then delete i xs else x:delete i xs

insert :: Int -> [Int] -> [Int]
insert i [] = [i]
insert i (x:xs) = if i > x then x : insert i xs else  i : x : xs







(.) :: (b->c) -> (a->b)->a->c
(f . g) x = f (g x)


map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs 






map f . map g




(map f) . (map g)



























