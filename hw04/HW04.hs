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

