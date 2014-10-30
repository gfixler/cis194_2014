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

