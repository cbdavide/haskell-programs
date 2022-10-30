import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

rShow :: Show a => Reader a String
rShow = reader $ show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    liftIO $ putStrLn ("Hi: " ++ show a)
    return $ a + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
    liftIO $ putStrLn ("Hi: " ++ show a)
    return (show a, a + 1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite

    case excite of
      Nothing -> putStrLn "MOAR EXCITE"
      Just e -> putStrLn ("Good, was very excite: " ++ e)
