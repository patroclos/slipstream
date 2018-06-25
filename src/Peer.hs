module Peer where
  import Data.Binary
  import Data.Binary.Get
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Char8 as BS
  import Data.ByteString.Lazy (fromStrict, toStrict)
  import Control.Monad.Writer

  import Control.Exception
  import System.Timeout
  import Control.Concurrent.ParallelIO.Global (parallel)

  import Network.Socket hiding (recv, send, KeepAlive)
  import Network.Socket.ByteString (recv, send)

  import Control.Monad.Trans.Maybe
  import Control.Applicative (empty)

  type MaybeIO = MaybeT IO

  type PartIdx = Word32
  type Begin = Word32
  type Length = Word32

  data PeerMsg
    = KeepAlive
    | Choke --
    | Unchoke --
    | Interested --
    | NotInterested --
    | Have --PartIdx
    | Bitfield --Word32
    | Request --PartIdx Begin Length
    | Piece --PartIdx Begin B.ByteString
    | Cancel --PartIdx Begin Length
    deriving (Eq, Show)

  handshakePrefix = '\19':"BitTorrent protocol"
  handshakePadding = replicate 8 '\0'
  handshake infoHash peerId = handshakePrefix ++ handshakePadding ++ infoHash ++ peerId

  abcPeerId = take 20 ['A'..'Z']

  connectToPeer :: String -> Word16 -> MaybeIO Socket
  connectToPeer host port = do
    addrInfo <- liftIO $ head <$> getAddrInfo Nothing (Just host) (Just $ show port)
    sock <- liftIO $ socket (addrFamily addrInfo) Stream defaultProtocol
    cr <- liftIO $ timeout 1000000 (connect sock (addrAddress addrInfo))
    case cr of
      Just _ -> do
        liftIO $ putStrLn$"[=>] connected to "++(show.addrAddress) addrInfo
        return sock
      Nothing -> empty

  doHandshake :: String -> Socket -> MaybeIO Socket
  doHandshake infoHash socket = do
    sent <- liftIO $ send socket (BS.pack $ handshake infoHash abcPeerId)
    msgHandshake <- liftIO $ recv socket (8 + 20 + 40)

    let msgStatic = (BS.unpack.B.take (length handshakePrefix)) msgHandshake
    if msgStatic /= handshakePrefix then do
      liftIO $ close socket
      empty
    else do
      let (remoteInfoHash, remotePeerId) = (B.splitAt 20.(B.drop 28)) msgHandshake
      let infoHashMatches = BS.unpack remoteInfoHash == infoHash
      liftIO $ putStrLn $"[<=] remote peerId: " ++ show remotePeerId
      liftIO $ putStrLn $"[<=] remote advertised correct infoHash? " ++ show infoHashMatches
      if infoHashMatches then return socket else empty

  testItWith :: String -> [(String, Word16)] -> IO ()
  testItWith infoHash peers = void $ parallel $
    (`catch` discardE).void.runMaybeT.runSession <$> peers
      where
        runSession (ip, port) = connectToPeer ip port >>= doHandshake infoHash
  discardE :: SomeException -> IO ()
  discardE = print