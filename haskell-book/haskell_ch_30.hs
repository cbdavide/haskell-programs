{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import Data.Typeable
import System.Environment (getArgs)
import System.Random (randomRIO)

data MyException = forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
    showsPrec p (MyException e) = showsPrec p e


multiError :: Int -> Either MyException Int
multiError n =
    case n of
      0 -> Left (MyException DivideByZero)
      1 -> Left (MyException StackOverflow)
      _ -> Right n


data SomeError = 
      Arith ArithException
    | Async AsyncException
    | SomethingElse
    deriving (Show)


discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
    case cast e of
      (Just arith) -> Arith arith
      Nothing -> 
          case cast e of
            (Just async) -> Async async
            Nothing -> SomethingElse

runDisc n = either discriminateError (const SomethingElse) (multiError n)

handler :: SomeException -> IO ()
handler (SomeException e) = do
    putStrLn ("Running main caused the error: " ++ show e)
    writeFile "bbb" "hi"


main' :: IO ()
main' = writeFile "zzz" "hi" `catch` handler

-- try :: Exception e => IO a -> Either e a
willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
    result <- action
    case result of
      Left e -> putStrLn $ "Error -- " ++ show e
      Right _ -> return ()

willFail :: Integer -> IO ()
willFail denom = onlyReportError $ willIFail denom

willIFail' :: Integer -> IO ()
willIFail' denom =
    print (div 5 denom) `catch` handler
        where handler :: ArithException -> IO ()
              handler e = putStrLn $ "Error -- " ++ show e


-- getArgs :: IO [String]
-- mapM_ :: (Foldable t, Monad m) -> (a -> m b) -> t a -> m ()

testDiv :: String -> IO ()
testDiv d = onlyReportError $ willIFail (read d)

main :: IO ()
main = do
    args <- getArgs -- args :: [String]
    mapM_ testDiv args


canICatch :: Exception e => e -> IO (Either SomeException ())
canICatch e = try $ throwIO e


randomException :: IO ()
randomException = do
    i <- randomRIO (1, 10 :: Int)

    if i `elem` [1..9]
       then throwIO DivideByZero
       else throwIO StackOverflow


liveForever :: IO ()
liveForever = forever $ do
    let tryS :: IO () -> IO (Either ArithException ())
        tryS = try

    _ <- tryS randomException
    putStrLn "long live the loop"
    threadDelay (1 * 1000000)


data InvalidNum = NotEven Int | NotDivThree Int deriving (Eq, Show)

instance Exception InvalidNum

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO $ NotDivThree i
  | odd i = throwIO $ NotEven i
  | otherwise = return i
