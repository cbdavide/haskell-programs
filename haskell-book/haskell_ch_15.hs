import Data.Monoid
import Test.QuickCheck


data Optional a = Nada | Only a deriving (Eq, Show)


instance Monoid a => Monoid (Optional a) where
    mempty = Nada


instance Semigroup a => Semigroup (Optional a) where

    x <> Nada = x
    Nada <> x = x
    Nada <> Nada = Nada
    (Only a) <> (Only b) = Only $ a <> b


checkOptional :: IO ()
checkOptional = do
    print $ (Only $ Sum 1) `mappend` (Only $ Sum 1)
    print $ (Only $ Product 4) `mappend` (Only $ Product 2)
    print $ (Only $ Sum 1) `mappend` Nada
    print $ Nada `mappend` Only (Sum 1)



type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = 
    mconcat 
    [
      e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a 

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a 

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
    arbitrary = do
        value <- arbitrary
        frequency [ (1, return $ First' Nada), (1, return $ First' (Only value) )]


instance Monoid (First' a) where
    mempty = First' Nada

instance Semigroup (First' a) where
    (First' Nada) <> all@(First' (Only b)) = all
    all@(First' (Only b)) <> (First' Nada) = all
    fst@(First' (Only b)) <> (First' (Only c)) = fst
    _ <> _ = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FirstId = First' String -> Bool


checkFirstMonoid :: IO ()
checkFirstMonoid = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity:: FirstId)
    quickCheck (monoidRightIdentity:: FirstId)


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _  = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity $ x <> y

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        val <- arbitrary
        return $ Identity val

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

checkIdentity :: IO ()
checkIdentity = do
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)


data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        valA <- arbitrary
        valB <- arbitrary
        return $ Two valA valB

type TSS = Two String String
type TowAssoc = TSS -> TSS -> TSS -> Bool

checkTwo :: IO ()
checkTwo = do
    quickCheck (semigroupAssoc :: TowAssoc)
    quickCheck (monoidLeftIdentity :: TSS -> Bool)
    quickCheck (monoidRightIdentity :: TSS -> Bool)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return $ BoolConj b

type BoolConjAss = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = do
    quickCheck (semigroupAssoc :: BoolConjAss)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> _ = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return $ BoolDisj b

type BoolDisjAss = BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkBoolDisj :: IO ()
checkBoolDisj = do
    quickCheck (semigroupAssoc :: BoolDisjAss)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd b) <> _ = (Snd b)
    _ <> b       = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [ (1, return $ Fst a), (1, return $ Snd b) ]

type OSS = Or String String
type OrAsso = OSS -> OSS -> OSS -> Bool

checkOr :: IO ()
checkOr = quickCheck (semigroupAssoc :: OrAsso)


newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ (f <> g)

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ \n -> mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return $ Combine f

instance Show (Combine a b) where
    show f = "aaaaaaa"

type SI = Sum Int
type CombineSII = Combine Int SI

semigroupAssocF :: CombineSII -> CombineSII -> CombineSII -> Int -> Bool
semigroupAssocF a b c d = (unCombine (a <> (b <> c)) d) == ( unCombine ((a <> b) <> c) d)

monoidLeftIdentityF :: CombineSII -> Int -> Bool
monoidLeftIdentityF a b = (unCombine (mempty <> a) b) == unCombine a b

monoidRightIdentityF :: CombineSII -> Int -> Bool
monoidRightIdentityF a b = (unCombine (a <> mempty) b) == unCombine a b

checkCombine :: IO ()
checkCombine = do 
    quickCheck semigroupAssocF
    quickCheck monoidLeftIdentityF
    quickCheck monoidRightIdentityF

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
    (Comp a) <> (Comp b) = Comp $ a . b

instance Monoid a => Monoid (Comp a) where
    mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return $ Comp f

instance Show (Comp a) where
    show f = "aaaaaaa"

type CS = Comp String
type CompAssoc = CS -> CS -> CS -> String -> Bool

semigroupAssocG :: CS -> CS -> CS -> String -> Bool
semigroupAssocG a b c d = (unComp (a <> (b <> c)) d) == ( unComp ((a <> b) <> c) d)

monoidLeftIdentityG :: CS -> String -> Bool
monoidLeftIdentityG a b = (unComp (mempty <> a) b) == unComp a b

monoidRightIdentityG :: CS -> String -> Bool
monoidRightIdentityG a b = (unComp (mempty <> a) b) == unComp a b

checkComp :: IO ()
checkComp = do
    quickCheck semigroupAssocG
    quickCheck monoidLeftIdentityG
    quickCheck monoidRightIdentityG

data Validation a b = Failures a | Successs b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failures a) <> (Failures b) = Failures $ a <> b
    (Successs a) <> _            = Successs a
    _ <> (Successs a)            = Successs a

checkValidation :: IO ()
checkValidation = do
    let failure :: String -> Validation String Int
        failure = Failures
        success :: Int -> Validation String Int
        success = Successs
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem $ \s -> (fst (f s)  <> fst (g s), snd (f (snd (g s))))


instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)

checkMem = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
