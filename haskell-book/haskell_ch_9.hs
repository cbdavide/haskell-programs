myWords :: String -> [String]
myWords "" = []
myWords words = word : myWords otherWords
    where word = takeWhile (/= ' ') words
          otherWords = (dropWhile (== ' ')) . (dropWhile (/= ' ')) $ words
