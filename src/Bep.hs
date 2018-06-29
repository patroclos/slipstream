{-# LANGUAGE OverloadedStrings #-}
module Bep where

import Peer (slipstreamPeerId)

import System.Timeout
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Maybe
import Data.Functor
import Data.Bifunctor
import Data.Monoid ((<>))
import Data.List.Split (chunksOf)
import Data.Int (Int64, Int32, Int16)
import Data.Word (Word8, Word16, Word32, Word64, byteSwap32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.Binary.Builder
import Data.Binary.Get
import Data.ByteString.Lazy (pack, unpack, fromStrict, toStrict)
import Network.Socket hiding (recv, sendTo)
import Network.Socket.ByteString (recv, sendAll, send, sendTo)
import qualified URI.ByteString as URI
import Control.Concurrent.ParallelIO.Global (parallel)

bepAnnounce :: [String] -> B.ByteString -> IO [(String, Word16)]
bepAnnounce trackers infoHash = do
  trackerAddresses <- catMaybes <$> sequence (trackerAddr <$> trackers)
  fmap concat (parallel $ (`queryTracker` infoHash) <$> trackerAddresses)

trackerAddr :: String -> IO (Maybe AddrInfo)
trackerAddr uri =
  case URI.parseURI URI.strictURIParserOptions $ BS.pack uri of
    Left err -> return Nothing
    Right uri -> do
      let auth = URI.uriAuthority uri
      let host = BS.unpack . URI.hostBS . URI.authorityHost <$> auth
      let port = Just $ maybe "80" (show . URI.portNumber) (auth >>= URI.authorityPort)
      if (BS.unpack . URI.schemeBS) (URI.uriScheme uri) /= "udp" then
        return Nothing
      else
        Just . head <$> getAddrInfo Nothing host port

queryTimeout = 1000000 * 5

noBytesLeft = 0
noPort = 0

-- trackerAddr -> infoHash -> peers
queryTracker :: AddrInfo -> B.ByteString -> IO [(String, Word16)]
queryTracker tracker infoHash = do
  bindaddr <- head <$> getAddrInfo Nothing (Just "0.0.0.0") Nothing
  let log msg = putStrLn $ "[*] (" ++ show (addrAddress tracker) ++ ") " ++ msg
  sock <- socket (addrFamily bindaddr) Datagram defaultProtocol
  bind sock (addrAddress bindaddr)
  log "Sending connect_request"
  void $ sendTo sock (B.pack $ connectRequestData 0x414141) (addrAddress tracker)
  concat.maybeToList <$> timeout queryTimeout (recv sock 4096 >>= \ack -> do
    let (_, transactionId, connectionId) = runGet getConnectResponse (fromStrict ack)
    log $ "Established connection: connectionId " ++ show connectionId
    let query = B.pack $ announceRequestData connectionId 0xff infoHash (BS.pack slipstreamPeerId) 0 noBytesLeft 0 2 0 noPort
    sendTo sock query (addrAddress tracker)
    recv sock 4096 >>= \answer -> do
      let (action, transactionId, interval, leechers, seeders, endpoints) = runGet getAnnounceResponse (fromStrict answer)
      log $ "Received announce with " ++ (show seeders) ++ " peers"
      close sock
      sequence $ (\(ip,port)-> do
        ipString <- inet_ntoa $ byteSwap32 ip
        return (ipString, port)) <$> endpoints
        )

protocolId :: Word64
protocolId = 0x41727101980

connectRequestData :: Word32 -> [Word8]
connectRequestData transactionId =
  (unpack . toLazyByteString) $
  putWord64be protocolId <>
  putWord32be 0 <>
  putWord32be transactionId

-- Get (action, transaction_id, connection_id)
getConnectResponse :: Get (Word32, Word32, Word64)
getConnectResponse = liftM3 (\a b c -> (a, b, c)) getWord32be getWord32be getWord64be

-- connectionId transactionId infoHash peerId downloaded left uploaded event key port
announceRequestData :: Word64 -> Word32 -> B.ByteString -> B.ByteString -> Word64 -> Word64 -> Word64 -> Word32 -> Word32 -> Word16 -> [Word8]
announceRequestData
  connectionId
  transactionId
  infoHash
  peerId
  downloaded
  left
  uploaded
  event
  key
  port = (unpack . toLazyByteString) $
  putWord64be connectionId <>
  putWord32be 1 <> -- action
  putWord32be transactionId <>
  fromByteString infoHash <>
  fromByteString peerId <>
  putWord64be downloaded <>
  putWord64be left <>
  putWord64be uploaded <>
  putWord32be event <>
  putWord32be 0 <> -- ip
  putWord32be key <>
  putInt32be (-1) <>
  putWord16be port

-- Get (action, transaction_id, interval, leechers, seeders, ip_port_pairs)
-- TODO use HostAddress and PortNumber instead of WordX
getAnnounceResponse :: Get (Word32, Word32, Word32, Word32, Word32, [(HostAddress, Word16)])
getAnnounceResponse = return (,,,,,) `ap`
  getWord32be `ap`
  getWord32be `ap`
  getWord32be `ap`
  getWord32be `ap`
  getWord32be `ap`
  (runGet getEndpoints <$> getRemainingLazyByteString)

getEndpoints = do
  empty <- isEmpty
  if empty
    then return []
    else do
      endpoint <- liftM2 (,) getWord32be getWord16be
      endpoints <- getEndpoints
      return (endpoint : endpoints)