import Data.List
import Data.Bool

vowels = "aeiou"
consonants = filter (`notElem` vowels) ['a'..'z']

isVowel :: Char -> Bool
isVowel = (`elem` vowels)

isConsonant :: Char -> Bool
isConsonant = (`elem` consonants)

notThe :: String -> Maybe String
notThe x
    | x == "the" = Nothing
    | otherwise  = Just x


replaceThe :: String -> String
replaceThe = inner . words
    where inner [] = ""
          inner (x:xs) =
            case notThe x of
              Nothing   -> 'a' : sep xs ++ inner xs
              Just x    -> x ++ sep xs ++ inner xs
          sep xs = if null xs then "" else " "


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = inner . words
    where inner [] = 0
          inner [xs] = 0
          inner (x:xs) =
              case notThe x of
                Nothing -> increment xs + inner xs
                Just _  -> inner xs
          increment xs = if isVowel . head . head $ xs then 1 else 0

countVowels :: String -> Int
countVowels = length . filter isVowel

countConsonants :: String -> Int
countConsonants = length . filter isConsonant


newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs =
    case compare (countVowels xs) (countConsonants xs) of
      GT -> Nothing
      _  -> Just $ Word' xs


data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | otherwise = Just (inner x)
    where inner 0 = Zero
          inner y = Succ $ inner (y - 1)


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee a _ Nothing  = a
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe  a -> [a]
maybeToList = mayybee [] (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = map (fromMaybe undefined) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
    | any isNothing xs = Nothing
    | otherwise        = Just $ catMaybes xs

lefts' :: [Either a b] -> [a]
lefts' = foldr concatLeft []
    where concatLeft (Left a) b = a : b
          concatLeft _ b        = b

rights' :: [Either a b] -> [b]
rights' = foldr concatRight []
    where concatRight (Right a) b = a : b
          concatRight _ b         = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) ->  Either a b -> Maybe c
eitherMaybe' _ (Left a)  = Nothing
eitherMaybe' f (Right a) = Just $ f a

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right a) = g a

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (const Nothing) $ Just . g


myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = 
    case f b of
      Nothing -> []
      Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just(x, f x))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold' :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold' f a =
    case f a of
      Nothing -> Leaf
      Just (a, b, c) -> Node (unfold' f a) b (unfold' f c)


treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold' f 0
    where f a
            | x == a = Nothing
            | otherwise = Just (a + 1, a, a + 1)
