data Node = Node { value :: Int
                 , left  :: Maybe Node
                 , right :: Maybe Node
                 } deriving (Show)


insert :: Maybe Node -> Int -> Node
insert Nothing val = Node { value = val, left = Nothing, right = Nothing }
insert (Just node) val = if val <= (value node) then 
                            Node { value = (value node)
                                 , left = Just (insert (left node) val)
                                 , right = (right node)
                                 }
                         else 
                            Node { value = (value node)
                                 , left = (left node)
                                 , right = Just (insert (right node) val) 
                                 }

search :: Maybe Node -> Int -> Maybe Node
search Nothing _ = Nothing
search (Just node) val = if val == (value node) then Just node
                         else if val < (value node) then search (left node) val
                         else search (right node) val

main :: IO ()
main = putStrLn ( show ( search (Just (insert (Just (insert Nothing 10)) 2)) 10 ) )
