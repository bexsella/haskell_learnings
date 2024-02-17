{-# OPTIONS_GHC -Wall -Werror #-}

module LogAnalysis where

import Log

messageTS :: LogMessage -> TimeStamp
messageTS (LogMessage _ ts _) = ts
messageTS (Unknown _) = 0

messageStr :: LogMessage -> String
messageStr (LogMessage (Error _) _ str) = str
messageStr (LogMessage _ _ str) = str
messageStr (Unknown str) = str

treeMsg :: MessageTree -> LogMessage
treeMsg (Node _ msg _) = msg
treeMsg Leaf = Unknown "No msg"

parseMessage :: String -> LogMessage
parseMessage ('I':ss)
    = LogMessage Info
      (read (head (words ss)))
      (unwords (tail (words ss)))

parseMessage ('W':ss)
    = LogMessage Warning
      (read (head (words ss)))
      (unwords (tail (words ss)))

parseMessage ('E':ss)
    = LogMessage (Error (read (head (words ss)))) 
      (read (last (take 2 (words ss))))
      (unwords (drop 2 (words ss)))

parseMessage _
    = Unknown "Unknown message type."

parse :: String -> [LogMessage]
parse f = [parseMessage x | x <- lines f]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert newMsg (Node l msg r) | messageTS newMsg < messageTS msg =
                                Node l msg (insert newMsg r)
                             | otherwise =
                                Node (insert newMsg l) msg r

build :: [LogMessage] -> MessageTree
build [] = Leaf
build msgs = insert(head msgs) (build (tail msgs))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = [messageStr x | 
    x@(LogMessage(Error s) _ _) <- inOrder (build msgs), s >= 50]

main :: IO [String]
main =
    testWhatWentWrong parse whatWentWrong "data/error.log"
