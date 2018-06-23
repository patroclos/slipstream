module Main where

import Metainfo
import Bep
import Peer

import Text.Parsec
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
      putStrLn $ "File(s) size: " ++ show (totalSize idict)
      putStrLn $ "Info Keys: " ++ show (fst <$> idict)
      putStrLn $ "Info Hash: " ++ show (infoHash idict)
      sequence_ (putStrLn.(++) "\tAnnouncer: ".show<$> announcers')
      ips <- unique <$> bepAnnounce (announcers dict) (infoHash idict)
      sequence_ $ print <$> ips
      putStrLn $ "number of addresses: " ++ show (length ips)

infoHash :: [(String, BEnc)] -> B.ByteString
infoHash dict = SHA1.finalize . SHA1.update SHA1.init $ bencode $ BDict dict

parseAnnouncer uri = (rightToMaybe $ URI.parseURI URI.strictURIParserOptions uri) >>= \uri -> Just $ (URI.uriScheme uri, URI.authorityHost <$> URI.uriAuthority uri)