module Main where

import Files
import Metainfo
import Bep
import Peer
import Common (MaybeIO)

import Text.Parsec (ParseError)
import Text.Printf (printf)
import Data.Maybe (catMaybes, isJust)
import Data.Either.Combinators (rightToMaybe)
import Data.List (nub)
import Data.List.Split (chunksOf)
import System.Environment (getArgs, getProgName)
import Control.Monad (void, mapM, replicateM, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Applicative (empty)
import System.FilePath.Posix ((</>))

import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef, writeIORef)
import Control.Concurrent.Chan (newChan, readChan, Chan)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified URI.ByteString as URI

import Control.Concurrent.ParallelIO.Global (parallel)

import Network.Socket (addrAddress)
import System.IO (IOMode(WriteMode), openBinaryFile, hSetFileSize, hSeek, hFlush, hClose)

import Streamly hiding (parallel)
import qualified Streamly.Prelude as S
import Control.Concurrent

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> parseMetainfoFromFile file >>= handleInfo
    _ -> getProgName >>= (\n -> putStrLn $ helpString n)

helpString :: String -> String
helpString progName = foldl (\a b -> a ++ '\n':b) "" $
  "slipstream":
  ("Usage: " ++ progName ++ " [torrent file]"):
  []

handleInfo :: Either ParseError [(String, BEnc)] -> IO ()
handleInfo (Left e) = error $ show e
handleInfo (Right meta) = void.runMaybeT $ do
  BDict info <- dictLookupM "info" meta
  BString torrentName <- dictLookupM "name" info
  BInt pieceLength <- dictLookupM "piece length" info
  BString piecesStr <- dictLookupM "pieces" info
  let pieceHashes = BS.pack <$> chunksOf 20 piecesStr :: [B.ByteString]
  let files = getFileInfo info
  let downloadFolder = "download" </> torrentName

  liftIO $ touchFiles files downloadFolder
  pieceQueue <- liftIO.newIORef $ zip [0..] pieceHashes

  liftIO $ runStream $
    asyncly $
    (foldr (<>) mempty $ (`bepAnnounce` (infoHash info)) <$> (announcers meta))
    |& S.mapMaybeM (\(h,p)->runMaybeT $ createPeer h p (BS.unpack $ infoHash info) (totalSize info) pieceLength pieceQueue)
    >>= (\peer@Peer{peerSocket=sock} -> asyncly $ do
      liftIO $ sendUnchokeInterested sock
      x@((pieceIndex, hash), buffer) <- peerStreamHandler peer
      return x
    )
    >>= (\((idx, hash), buffer) -> serially $ do
      let segments = getTorrentFileSegments files (fromIntegral idx * pieceLength) buffer
      liftIO $ segments `forM` writeFileSegment downloadFolder
      )

infoHash :: [(String, BEnc)] -> B.ByteString
infoHash dict = SHA1.finalize . SHA1.update SHA1.init $ bencode $ BDict dict

showByteSize :: Int -> String
showByteSize size
  | abs size < 1024 = printf "%dB" size
  | otherwise       = printf "%.4f%sB" n unit
  where
    (n, unit) = last $ takeWhile ((>=0.5).abs.fst) pairs
    pairs = zip (iterate (/1024) size') units
    size' = fromIntegral size :: Double
    units = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"] :: [[Char]]