module Main where

import Metainfo
import Bep
import Peer
import Common (MaybeIO)

import Text.Parsec (ParseError)
import Text.Printf (printf)
import Data.Maybe (catMaybes)
import Data.Either.Combinators (rightToMaybe)
import Data.List (nub)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)
import Control.Monad (void, mapM, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Applicative (empty)

import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef, writeIORef)
import Control.Concurrent.Chan (newChan, readChan, Chan)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified URI.ByteString as URI

import Control.Concurrent.ParallelIO.Global (parallel)

main :: IO ()
-- main = spawnWorkers
main = do
  [file] <- getArgs
  parseMetainfoFromFile file >>= handleInfo

handleInfo :: Either ParseError [(String, BEnc)] -> IO ()
handleInfo (Left e) = error $ show e
handleInfo (Right meta) = do
  let announcers' = announcers meta
  void $ runMaybeT $ do
    BDict info <- dictLookupM "info" meta
    BString torrentName <- dictLookupM "name" info
    BInt pieceLength <- dictLookupM "piece length" info
    BString piecesStr <- dictLookupM "pieces" info
    let pieceHashes = BS.pack <$> chunksOf 20 piecesStr :: [B.ByteString]
    pieceQueue <- liftIO.newIORef $ zip [0..] pieceHashes
    pieceChan <- liftIO newChan

    peerEndpoints <- liftIO $ take 1000.nub <$> bepAnnounce announcers' (infoHash info)
    peers <- liftIO $ catMaybes <$> (parallel $ (\(h,p) -> runMaybeT $ createPeer h p (BS.unpack $ infoHash info) (totalSize info) pieceLength pieceQueue pieceChan) <$> peerEndpoints)
    liftIO $ putStrLn $ "Number of peers: " ++ (show $ length peers)
    liftIO $ parallel $ runPeer <$> peers

    resultBuffer <- liftIO $ newIORef []
    nextPieceIdx <- liftIO $ newIORef 0

    liftIO $ replicateM (length pieceHashes) $ do
      ((idx, hash), chunk) <- readChan pieceChan
      atomicModifyIORef resultBuffer $ \buf -> ((idx, chunk):buf, Nothing)
      putStrLn $ "Enqueued result for " ++ (show idx)
      buf <- readIORef resultBuffer
      commit torrentName buf nextPieceIdx
      where
        commit filename buf nextPieceIdx = do
          nextIdx <- readIORef nextPieceIdx
          case dictLookup nextIdx buf of
            Just next -> do
              B.appendFile filename next
              putStrLn $ "Committing piece " ++ (show nextIdx)
              writeIORef nextPieceIdx (nextIdx + 1)
              commit filename buf nextPieceIdx
            Nothing -> return ()

infoHash :: [(String, BEnc)] -> B.ByteString
infoHash dict = SHA1.finalize . SHA1.update SHA1.init $ bencode $ BDict dict

humanReadableBytes :: Int -> String
humanReadableBytes size
  | abs size < 1024 = printf "%dB" size
  | otherwise       = printf "%.4f%sB" n unit
  where
    (n, unit) = last $ takeWhile ((>=0.5).abs.fst) pairs
    pairs = zip (iterate (/1024) size') units
    size' = fromIntegral size :: Double
    units = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"]