{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.List

handleError :: String -> MaybeInt -> MaybeInt -> [String] -> MaybeLogMessage
handleError _ (ValidInt l) (ValidInt t) xs =
    ValidLM (LogMessage (Error l) t (unwords xs))
handleError s _ _ _ = InvalidLM s

handleInfo :: String -> MaybeInt -> [String] -> MaybeLogMessage
handleInfo _ (ValidInt t) xs = ValidLM (LogMessage Info t (unwords xs))
handleInfo s _ _             = InvalidLM s

handleWarning :: String -> MaybeInt -> [String] -> MaybeLogMessage
handleWarning _ (ValidInt t) xs = ValidLM (LogMessage Warning t (unwords xs))
handleWarning s _ _             = InvalidLM s

parseMessage' :: [String] -> MaybeLogMessage
parseMessage' s@("E":l:t:xs) = handleError (unwords s) (readInt l) (readInt t) xs
parseMessage' s@("I":t:xs)   = handleInfo (unwords s) (readInt t) xs
parseMessage' s@("W":t:xs)   = handleWarning (unwords s) (readInt t) xs
parseMessage' s              = InvalidLM (unwords s)

parseMessage :: String -> MaybeLogMessage
parseMessage = parseMessage' . words


validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly []                 = []
validMessagesOnly ((ValidLM x):xs)   = x : validMessagesOnly xs
validMessagesOnly ((InvalidLM _):xs) = validMessagesOnly xs


parse :: String -> [LogMessage]
parse = validMessagesOnly . map parseMessage . lines


compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ t _) (LogMessage _ t' _) = compare t t'


sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs


whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' [] = []
whatWentWrong' ((LogMessage (Error n) _ s):xs) =
    if n >= 50
    then s : whatWentWrong' xs
    else whatWentWrong' xs
whatWentWrong' (_:xs) = whatWentWrong' xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = whatWentWrong'. sortMessages

