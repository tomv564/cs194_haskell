import Log

main :: IO ()
main = putStrLn "Hello World"

parseMessage :: String -> LogMessage
parseMessage line = parseLine (words line)

parseLine :: [String] -> LogMessage
parseLine ("E" : sev : stamp : rest) = LogMessage (Error $ read sev) (read stamp) (unwords rest)
parseLine ("W" : stamp : rest) = LogMessage Warning (read stamp) (unwords rest)
parseLine ("I" : stamp : rest) = LogMessage Info (read stamp) (unwords rest)
parseLine gibber = Unknown $ unwords gibber

parse :: String -> [LogMessage]
parse loglines = map parseMessage (lines loglines)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ stamp _) tree@(Node left curr@(LogMessage _ ns _) right) | stamp < ns = Node (insert message left) curr right
                                                                                 | stamp >= ns = Node left curr (insert message right)

build :: [LogMessage] -> MessageTree
build messages = foldr (insert) Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
        map text $ filter isRelevant sorted
    where
        text (LogMessage _ _ message) = message
        isRelevant (LogMessage (Error sev) _ _ ) | sev >= 50 = True
        isRelevant _ = False
        sorted = inOrder (build messages)