{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy "" _ = True
formableBy (x:xs) h = if x `elem` h then formableBy xs (delete x h)
                      else False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate "" _ "" = True
wordFitsTemplate (t:ts) h (c:cs)
    | t == '?'  = if c `elem` h then wordFitsTemplate ts (delete c h) cs
                  else False
    | otherwise = if t == c then wordFitsTemplate ts h cs
                  else False
wordFitsTemplate _ _ _ = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = filter (wordFitsTemplate t h) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

wordFight :: Int -> [String] -> [String] -> [String]
wordFight _ bests [] = bests
wordFight n bests (w:ws)
    | n > v = wordFight n bests ws
    | n < v = wordFight v [w] ws
    | otherwise = wordFight n (w:bests) ws
    where v = scrabbleValueWord w

bestWords :: [String] -> [String]
bestWords = wordFight 0 []

scrabbleValueTemplate' :: Int -> Int -> STemplate -> String -> Int
scrabbleValueTemplate' m n [] [] = m * n
scrabbleValueTemplate' m n (t:ts) (c:cs)
    | t == '?'  = scrabbleValueTemplate' m (n + v) ts cs
    | t == 'D'  = scrabbleValueTemplate' m (n + v * 2) ts cs
    | t == 'T'  = scrabbleValueTemplate' m (n + v * 3) ts cs
    | t == '2'  = scrabbleValueTemplate' (m * 2) (n + v) ts cs
    | t == '3'  = scrabbleValueTemplate' (m * 3) (n + v) ts cs
    | otherwise = scrabbleValueTemplate' m (n + v) ts cs
    where v = scrabbleValue c

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate = scrabbleValueTemplate' 1 0

