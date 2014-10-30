-- Name: Gary Fixler

module HW04 where

-- Exercise 1
-- There is no source for bs; we must return the one we get.
-- This is basically the identity function with an extra, useless argument.
ex1 :: a -> b -> b
ex1 _ x = x


-- Exercise 2
-- There are 2 possible outputs here - either input arg.
-- Possible names for these functions include: right and left.
ex2 :: a -> a -> a
ex2 x _ = x


-- Exercise 3
-- This is another identity function with an extra appendage.
ex3 :: Int -> a -> a
ex3 _ x = x


-- Exercise 4
-- We can make a decision based on the Bool, but we have no
-- idea what our as will be, so all we can do is return either.
-- Thus, there are 2 functions possible with this type.
ex4 :: Bool -> a -> a -> a
ex4 b x y = if b then x else y


-- Exercise 5
-- There are 4 valid implementations: not, id, True, False
ex5 :: Bool -> Bool
ex5 = not


-- Exercise 6
-- This function takes the id function, but then returns a
-- concrete instance of the type passed to the id function,
-- sans actual instance of said type. This is impossible.
ex6 :: (a -> a) -> a
ex6 = undefined


-- Exercise 7
-- This take a simple function on any type, and something of
-- that type. As we have no idea what that function could do,
-- the best we can do is return the instance, i.e. second arg.
ex7 :: (a -> a) -> a -> a
ex7 _ x = x


-- Exercise 8
-- The only complete implementations here are reverse and id.
-- There are arbitrarily many incomplete functions, such as
-- tail/init, and const counts/indices with take/drop/sublist.
ex8 :: [a] -> [a]
ex8 = reverse


-- Exercise 9
-- The only possible implementation is map, which is complete.
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map


-- Exercise 10
-- The only source we have for an a is in the Maybe, so we can
-- only return it. However, we can't return a Nothing that we
-- might get, so this function is incomplete.
ex10 :: Maybe a -> a
ex10 (Just a) = a


-- Exercise 11
-- This truly can take any type, so it's complete, and we don't
-- even have to handle any Nothing cases; Nothing also works.
ex11 :: a -> Maybe a
ex11 = Just


-- Exercise 12
-- This is the identity function lifted up into the Maybe type.
-- It's complete, but all we can do is return what we get.
ex12 :: Maybe a -> Maybe a
ex12 = id

