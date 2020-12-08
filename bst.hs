data Node = Null | Node { value :: Int, left  :: Node, right :: Node } deriving (Eq, Show)


insert :: Node -> Int -> Node
insert Null val = Node { value = val, left = Null, right = Null}
insert node val = if val == (value node) then node
                  else if val < (value node) then
                     Node { value = value node
                          , left = insert (left node) val
                          , right = right node
                          }
                  else
                     Node { value = value node
                          , left = left node
                          , right = insert (right node) val
                          }


search :: Node -> Int -> Node
search Null _ = Null
search node val = if val == (value node) then node
                 else if val < (value node) then search (left node) val
                 else search (right node) val


delete :: Node -> Int -> Node
delete Null _ = Null
delete node val = if val == (value node) then
                     replace node
                  else if val < (value node) then
                     Node { value = (value node)
                          , left = delete (left node) val
                          , right = right node
                          }
                  else
                     Node { value = value node
                          , left = left node
                          , right = delete (right node) val
                          }


replace :: Node -> Node
replace Node { left = Null, right = Null } = Null
replace Node { left = Null, right = rightChild } = rightChild
replace Node { left = leftChild, right = Null } = leftChild
replace node = Node { value = findSuccessor (right node)
                    , left = left node
                    , right = delete (right node) (findSuccessor (right node))
                    }


findSuccessor :: Node -> Int
findSuccessor Node { value = val, left = Null } = val
findSuccessor Node { left = leftChild } = findSuccessor leftChild


main :: IO ()
main = putStrLn (show  (search (insert (insert Null 10) 2) 10))
