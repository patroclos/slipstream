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

  connectToPeer :: String -> Word16 -> IO (Maybe Socket)
  connectToPeer host port = do
    addrInfo <- head <$> getAddrInfo Nothing (Just host) (Just $ show port)
    sock <- socket (addrFamily addrInfo) Stream defaultProtocol
    cr <- try $ timeout 1000000 (connect sock (addrAddress addrInfo))
    case cr of
      (Right (Just _)) -> do
        putStrLn$"[=>] connected to "++(show.addrAddress) addrInfo
        return $ Just sock
      (Left (SomeException e)) -> return Nothing
      _ -> return Nothing

  doHandshake :: String -> Maybe Socket -> IO (Maybe Socket)
  doHandshake _ Nothing = return Nothing
  doHandshake infoHash (Just socket) = do
    sent <- liftIO $ send socket (BS.pack $ handshake infoHash abcPeerId)
    liftIO $ putStrLn $"[=>] send handshake to: " ++ show socket
    msgHandshake <- liftIO $ recv socket (8 + 20 + 40)

    let msgStatic = (BS.unpack.B.take (length handshakePrefix)) msgHandshake
    if msgStatic /= handshakePrefix then
      liftIO $ do
        putStrLn $"[!] Received invalid handshake, abandoning peer. " ++ show (BS.pack msgStatic)
        close socket
        return Nothing
    else do
      let (remoteInfoHash, remotePeerId) = (B.splitAt 20.(B.drop 28)) msgHandshake
      let infoHashMatches = BS.unpack remoteInfoHash == infoHash
      liftIO $ putStrLn $"[<=] remote info: " ++ show remoteInfoHash
      liftIO $ putStrLn $"[<=] remote peer: " ++ show remotePeerId
      liftIO $ putStrLn $"[<=] infoHash is OK? " ++ show infoHashMatches
      if infoHashMatches then return $ Just socket else return Nothing

  testItWith :: String -> [(String, Word16)] -> IO ()
  testItWith infoHash peers = void $ parallel $
    runSession <$> peers
      where
        runSession (ip, port) = connectToPeer ip port >>= (doHandshake infoHash)