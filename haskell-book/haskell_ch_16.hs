{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr
import Test.QuickCheck


replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted


checkLifted :: IO ()
checkLifted = do
    putStr "replaceWithP' lms: "
    print (replaceWithP' lms)

    putStr "liftedReplace lms: "
    print (liftedReplace lms)

    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)

    putStr "twiceLifted lms: "
    print (twiceLifted lms)

    putStr "twiceLifted' lms: "
    print (twiceLifted' lms)

    putStr "thriceLifted lms: "
    print (thriceLifted lms)

    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)


a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123" ++) (fmap show ioi))
    in fmap (*3) changed 


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose  f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a


instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

checkIdentity :: IO ()
checkIdentity = do
    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck ((functorCompose (+1) (*2)) :: Identity Int -> Bool)


data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

checkPair :: IO ()
checkPair = do
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck ((functorCompose (+1) (*2)) :: Pair Int -> Bool)


data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

checkTwo :: IO ()
checkTwo = do
    quickCheck (functorIdentity :: Two Int Int -> Bool)
    quickCheck ((functorCompose (+1) (*2)) :: Two Int Int -> Bool)


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

checkThree :: IO ()
checkThree = do
    quickCheck (functorIdentity :: Three Int Int Int -> Bool)
    quickCheck ((functorCompose (+1) (*2)) :: Three Int Int Int -> Bool)


data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

checkThree' :: IO ()
checkThree' = do
    quickCheck (functorIdentity :: Three' Int Int -> Bool)
    quickCheck ((functorCompose (+1) (*2)) :: Three' Int Int -> Bool)


data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d


checkFour :: IO ()
checkFour = do
    quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
    quickCheck ((functorCompose (+1) (*2)) :: Four Int Int Int Int -> Bool)


data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a ) where
    fmap f (Four' a b c d) = Four' a b c (f d)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d

checkFour' :: IO ()
checkFour' = do
    quickCheck (functorIdentity :: Four' Int Int -> Bool)
    quickCheck ((functorCompose (+1) (*2)) :: Four' Int Int -> Bool)


data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers $ f a


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

data Sum' b a = First' a | Second' b deriving (Show)

instance Functor (Sum' e) where
    fmap f (First' a) = First' (f a)
    fmap f (Second' b) = Second' b


data Company a c b = DeepBlue a c | Something b deriving (Show)

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b deriving (Show)

instance Functor (Quant a) where
    fmap  f (Bloor b) = Bloor $ f b
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a

data K a b = K a deriving (Show)

instance Functor (K a) where
    fmap f (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip $ K (f b)

data EvilGoateeConst a b = GoatyConst b deriving (Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b


data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut (fmap g fa)


data Parappa f g a = DaWrappa (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga) 


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil | Const a (List a) deriving (Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Const a xs) = Const (f a) (fmap f xs)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s $ f a
    fmap f (Read g) = Read $ f . g



