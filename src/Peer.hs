{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Peer where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Control.Monad.Writer
import Control.Monad
import Data.Bits (testBit)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import System.Timeout
import Control.Exception
import Control.Concurrent (Chan, writeChan, forkIO, ThreadId, threadDelay)
import Control.Concurrent.ParallelIO.Global (parallel)
import Text.Printf (printf)

import Network.Socket hiding (recv, send, KeepAlive)
import Network.Socket.ByteString (recv, send)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Applicative (empty)
import qualified Crypto.Hash.SHA1 as SHA1

import Streamly hiding ((<>))
import qualified Streamly.Prelude as S

import Common (MaybeIO, liftIODiscardExceptions)

type PieceIdx = Word32
type Begin = Word32
type Length = Word32
type SHA1Hash = B.ByteString

type PieceQueue = IORef [(PieceIdx, SHA1Hash)]

data PeerMsg
  = KeepAlive
  | Choke --
  | Unchoke --
  | Interested --
  | NotInterested --
  | Have PieceIdx
  | Bitfield B.ByteString
  | Request PieceIdx Begin Length
  | Piece PieceIdx Begin B.ByteString
  | Cancel PieceIdx Begin Length
  deriving (Eq, Show)

data Peer = Peer
  { peerAddr          :: AddrInfo
  , peerSocket        :: Socket
  , peerTorrentLength :: Int
  , peerPieceLength   :: Int
  , peerPieceQueue    :: PieceQueue
  , peerProvides      :: IORef [PieceIdx]
  }

createPeer :: String -> Word16 -> String -> Int -> Int -> PieceQueue -> MaybeIO Peer
createPeer host port infoHash torrentLength pieceLength queue = do
  addr <- liftIO $ head <$> getAddrInfo Nothing (Just host) (Just $ show port)
  sock <- connectPeer addr >>= doHandshake infoHash
  msg <- liftIODiscardExceptions Nothing $ timeout 1000000 $ readPeerMessage sock
  case msg of
    Nothing -> empty
    Just (Bitfield bf) -> do
      --liftIO $ print $ filter (`notElem` (piecesInBitfield $ B.unpack bf)) [0..610] -- print missing piece indices
      providedPieces <- liftIO $ newIORef $ piecesInBitfield $ B.unpack bf
      return Peer
        { peerAddr = addr
        , peerSocket = sock
        , peerTorrentLength = torrentLength
        , peerPieceLength = pieceLength
        , peerPieceQueue = queue
        , peerProvides = providedPieces }
    _ -> empty


--peerStreamHandler :: Peer -> S.SerialT IO ((PieceIdx, SHA1Hash), B.ByteString)
peerStreamHandler
  peer@Peer
  { peerSocket = sock
  , peerPieceLength = pieceLength
  , peerProvides = providesRef
  , peerPieceQueue = queue
  } = loop
  where
    log msg = putStrLn $ "[peer] (" ++ (show $ addrAddress $ peerAddr peer) ++ ") " ++ msg
    loop = do
      provided <- liftIO $ readIORef providesRef
      work <- liftIO $ atomicModifyIORef queue $ \remainingPieces ->
        case remainingPieces of
          [] -> ([], Nothing)
          r ->
            case filter ((`elem` provided).fst) r of
              [] -> (r, Nothing)
              x:xs -> (filter (/=x) r, Just x)
      case work of
        Nothing -> (liftIO $ threadDelay 500000) >> loop
        Just work@(idx, hash) -> do
          let realLength = realPieceSize (peerTorrentLength peer) pieceLength (fromIntegral idx)
          liftIO $ log $ "Beginning download of " ++ show idx
          result <- liftIO $ (`catch` (errorFallback Nothing)) $ timeout (20 * 1000000) $ pullChunkedPiece sock readPiece idx realLength (2^14)
          case result of
            Nothing -> (liftIO $ enqueue work (error "Timeout")) >> S.nil
            Just piece -> do
              let rcvHash = SHA1.finalize . SHA1.update SHA1.init $ piece
              if rcvHash /= hash then do
                liftIO $ log $ "Error validating hash " ++ (show rcvHash) ++ " with defined " ++ (show hash)
                liftIO $ enqueue work (error "Hash Mismatch")
                loop
              else do
                liftIO $ log $ "Downloaded piece " ++ (show idx)
                S.cons (work, piece) loop

    errorFallback :: Monad m => a -> SomeException -> m a
    errorFallback a _ = return a

    enqueue :: (PieceIdx, B.ByteString) -> SomeException -> IO ()
    enqueue item err = atomicModifyIORef (peerPieceQueue peer) $ \remaining -> (item:remaining, ())

    readPiece = do
      msg <- readPeerMessage $ peerSocket peer
      case msg of
        Piece _ _ buf -> return buf
        Have idx -> do
          atomicModifyIORef (peerProvides peer) $ \lst -> (idx:lst, Nothing)
          readPiece
        _ -> readPiece

connectPeer :: AddrInfo -> MaybeIO Socket
connectPeer addrInfo = do
  sock <- liftIO $ socket (addrFamily addrInfo) Stream defaultProtocol
  cr <- liftIODiscardExceptions Nothing $ timeout 1000000 (connect sock (addrAddress addrInfo))
  maybe empty (const $ return sock) cr


piecesInBitfield :: [Word8] -> [PieceIdx]
piecesInBitfield bf =
  fromIntegral <$> filter (bitfieldContainsPiece bf) [0..(8 * length bf - 1)]

bitfieldContainsPiece :: [Word8] -> Int -> Bool
bitfieldContainsPiece bf i =
  testBit relevantByte (7 - (i `mod` 8))
  where
    relevantByte = bf !! (i `div` 8)

bitfieldContainsPieces :: Foldable f => [Word8] -> f Int -> Bool
bitfieldContainsPieces bf = all (bitfieldContainsPiece bf)


getMessage :: Get PeerMsg
getMessage = getWord8 >>= readPayloadOf
  where
    readPayload p | p == B.empty = return KeepAlive
    readPayload p = getWord8 >>= readPayloadOf
    readPayloadOf 0 = return Choke
    readPayloadOf 1 = return Unchoke
    readPayloadOf 2 = return Interested
    readPayloadOf 3 = return NotInterested
    readPayloadOf 4 = Have <$> getWord32be
    readPayloadOf 5 = Bitfield . toStrict <$> getRemainingLazyByteString
    readPayloadOf 6 = liftM3 Request getWord32be getWord32be getWord32be
    readPayloadOf 7 = liftM3 Piece getWord32be getWord32be (toStrict<$>getRemainingLazyByteString)
    readPayloadOf 8 = liftM3 Cancel getWord32be getWord32be getWord32be


handshakePrefix = "\19BitTorrent protocol"
handshakePadding = replicate 8 '\0'
handshake infoHash peerId = handshakePrefix ++ handshakePadding ++ infoHash ++ peerId

slipstreamPeerId = take 20 ("-SLPSTR-" ++ ['A'..'Z'])

connectToPeer :: String -> Word16 -> MaybeIO Socket
connectToPeer host port = do
  addrInfo <- liftIO $ head <$> getAddrInfo Nothing (Just host) (Just $ show port)
  connectPeer addrInfo

doHandshake :: String -> Socket -> MaybeIO Socket
doHandshake infoHash socket = do
  Just sent <- liftIODiscardExceptions Nothing $ timeout 2000000 $ send socket (BS.pack $ handshake infoHash slipstreamPeerId)
  Just msgHandshake <- liftIODiscardExceptions Nothing $ timeout 2000000 $ recvExact socket (8 + 20 + 40)

  let msgStatic = (BS.unpack.B.take (length handshakePrefix)) msgHandshake
  if msgStatic /= handshakePrefix then do
    liftIO $ close socket
    empty
  else do
    let (remoteInfoHash, remotePeerId) = (B.splitAt 20 . B.drop 28) msgHandshake
    let infoHashesMatch = BS.unpack remoteInfoHash == infoHash
    if infoHashesMatch then
      return socket
    else
      empty

sendUnchokeInterested :: Socket -> IO ()
sendUnchokeInterested sock = do
  let msg = toStrict.runPut $ putWord32be 1 <> putWord8 1 <> putWord32be 1 <> putWord8 2
  liftIO.void $ send sock msg

sendRequest :: PieceIdx -> Begin -> Length -> Socket -> IO ()
sendRequest idx begin len sock = do
  let msg = toStrict.runPut $ buildMsg idx begin len
  void $ send sock msg
  where
    buildMsg idx begin len = putWord32be 13 <> putWord8 6 <> putWord32be idx <> putWord32be begin <> putWord32be len

realPieceSize :: Int -> Int -> Int -> Int
realPieceSize torrentSize pieceSize pieceIndex =
  min pieceSize (torrentSize - pieceOffset)
  where
    pieceOffset = pieceIndex * pieceSize

pullChunkedPiece :: Socket -> IO B.ByteString -> PieceIdx -> Int -> Int -> IO B.ByteString
pullChunkedPiece socket readPiece piece pieceSize chunkSize = getNext 0 B.empty
    where
      getNext :: Int -> B.ByteString -> IO B.ByteString
      getNext chunkIdx acc = do
        let chunkOffset = chunkIdx * chunkSize
        let realChunkSize = fromIntegral $ realPieceSize pieceSize chunkSize chunkIdx
        sendRequest piece (fromIntegral chunkOffset) realChunkSize socket
        buf <- B.append acc <$> readPiece
        if chunkOffset + chunkSize < pieceSize then
          getNext (chunkIdx + 1) buf
        else
          return buf

readPeerMessage :: Socket -> IO PeerMsg
readPeerMessage sock = do
  len <- fromIntegral.(runGet getWord32be).fromStrict <$> recvExact sock 4
  payload <- fromStrict <$> recvExact sock len
  return $ runGet getMessage payload

--  With sock n, receive exactly n bytes from sock
recvExact :: Socket -> Int -> IO B.ByteString
recvExact sock n = recvExactAcc sock n B.empty
  where
    recvExactAcc :: Socket -> Int -> B.ByteString -> IO B.ByteString
    recvExactAcc sock n accum = do
      bytes <- B.append accum <$> recv sock (n - B.length accum)
      if B.length bytes < n then recvExactAcc sock n bytes else return bytes