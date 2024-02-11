{-# OPTIONS_GHC -Wall -Werror  #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ('I':ss) = LogMessage Info
                            (read (head (words ss)))
                            (unwords (tail (words ss)))
parseMessage ('W':ss) = LogMessage Warning
                            (read (head (words ss)))
                            (unwords (tail (words ss)))
parseMessage ('E':ss) = LogMessage (Error (read (head (words (ss))))) 
                            (read (last (take 2 (words ss))))
                            (unwords (drop 2 (words ss)))
parseMessage _ = Unknown "Unknown message type"

parse :: String -> [LogMessage]
parse f = [parseMessage x | x <- lines f]

main :: IO [LogMessage]
main =
    testParse parse 10 "data/error.log"
