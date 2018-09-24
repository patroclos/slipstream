import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Streamly
import qualified Streamly.Prelude as S

main :: IO ()
main = runStream $ mainStream

--mainStream :: IsStream t => t IO Int
mainStream =
  getSome 10 >>= (\x -> asyncly $ do
    liftIO $ putStrLn $ "Starting nested stream fro " ++ show x
    y <- getSome x
    liftIO $ threadDelay (200000 * (if x == 5 then 10 else 2))
    return (x, y)
    )
    |& S.mapM (\(x,y) -> do
      tid <- liftIO $ myThreadId
      liftIO $ putStrLn $ show tid ++ ": " ++ show x ++ " " ++ show y)
  {-
  |& S.mapM (\x -> do
    liftIO $ putStrLn $ "Starting nested stream for " ++ show x
    --y <- getSome x
    return x)
    -}

getSome :: IsStream t => Int -> t IO Int
getSome n = S.fromList [1..n]

-- streamly testing for ahead/async computation of actions and rate limiting
-- maxThreads and maxBuffer doesn't seem to latch on when not given a rate
{-
+------------+--------------+--------------+--------------+
| Type       | Execution    | Consumption  | Concurrency  |
+============+==============+==============+==============+
| Serial     | Serial       | Serial       | None         |
+------------+--------------+--------------+--------------+
| Ahead      | Asynchronous | Serial       | bounded      |
+------------+--------------+--------------+--------------+
| Async      | Asynchronous | Asynchronous | bounded      |
+------------+--------------+--------------+--------------+
| Parallel   | Asynchronous | Asynchronous | unbounded    |
+------------+--------------+--------------+--------------+
-}
main' = runStream . asyncly $ avgRate 200 $ maxThreads 50 $ (do
  n <- S.repeatM (return 1)--foldr (<>) mempty (return <$> [1..1000000])--return 3 <> return 2 <> return 1
  S.yieldM $ do
    threadDelay (n `mod` 3 * 1000000)
    myThreadId >>= \tid -> putStrLn (show tid ++ ": Delay " ++ show n)
    return n
  )
  |& S.mapM (\x -> putStrLn $ "Result: " ++ show x)