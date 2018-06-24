module Main where

import Metainfo
import Bep
import Peer

import Text.Parsec
import Text.Printf
import Data.Either.Combinators (rightToMaybe)
import Data.List (nub)
import System.Environment (getArgs)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified URI.ByteString as URI

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> parseMetainfoFromFile file >>= handleInfo
    _ -> error "usage: slipstream [torrent file]"

unique :: Eq a => [a] -> [a]
unique = reverse . nub . reverse

handleInfo :: Either ParseError [(String, BEnc)] -> IO ()
handleInfo (Left e) = error $ show e
handleInfo (Right dict) = do
  let announcers' = announcers dict
  case info dict of
    Nothing -> return ()
    (Just (BDict idict)) -> do
      putStrLn $ "Meta Keys: " ++ show (fst <$> dict)
      putStrLn $ "File(s) size: " ++ humanReadableBytes (totalSize idict)
      putStrLn $ "Info Keys: " ++ show (fst <$> idict)
      putStrLn $ "Info Hash: " ++ show (infoHash idict)
      sequence_ (putStrLn.(++) "\tAnnouncer: ".show<$> announcers')
      ips <- unique <$> bepAnnounce (announcers dict) (infoHash idict)
      sequence_ $ print <$> ips
      putStrLn $ "number of addresses: " ++ show (length ips)
      void $ testItWith (show $ infoHash idict) ips

infoHash :: [(String, BEnc)] -> B.ByteString
infoHash dict = SHA1.finalize . SHA1.update SHA1.init $ bencode $ BDict dict

parseAnnouncer uri = (rightToMaybe $ URI.parseURI URI.strictURIParserOptions uri) >>= \uri -> Just $ (URI.uriScheme uri, URI.authorityHost <$> URI.uriAuthority uri)

humanReadableBytes :: Int -> String
humanReadableBytes size
  | abs size < 1024 = printf "%dB" size
  | otherwise       = printf "%.1f%sB" n unit
  where
    (n, unit) = last $ takeWhile ((>=0.5).abs.fst) pairs
    pairs = zip (iterate (/1024) size') units
    size' = fromIntegral size :: Double
    units = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"]