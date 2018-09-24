{-# LANGUAGE OverloadedStrings #-}
module Metainfo where

import Files

import Text.Parsec
import qualified Network.URI.Encode as URI
import Control.Monad (liftM, liftM2, Monad)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Applicative (empty)
import Data.Monoid
import Data.Foldable (fold)
import Data.Bifunctor (bimap, Bifunctor)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

parseMetainfoFromFile :: String -> IO (Either ParseError [(String, BEnc)])
parseMetainfoFromFile filepath =
  parse metainfoParser filepath <$> B.readFile filepath

data BEnc
  = BString String
  | BInt Int
  | BList [BEnc]
  | BDict [(String, BEnc)] -- using [] instead of Set, so we retain order for hashing
  deriving Show

dictLookup :: Eq k => k -> [(k, v)] -> Maybe v
dictLookup key dict = listToMaybe $ snd <$> filter ((== key).fst) dict

dictLookupM :: (Eq k , Monad m) => k -> [(k, v)] -> MaybeT m v
dictLookupM key dict = listToMaybeT $ snd <$> filter ((== key).fst) dict
  where
    listToMaybeT :: Monad m => [a] -> MaybeT m a
    listToMaybeT [] = empty
    listToMaybeT lst = return $ head lst

announcers :: [(String, BEnc)] -> [String]
announcers dict = fromMaybe [] $ dictLookup "announce-list" dict
  >>= (\(BList items) -> Just (concat [l | x@(BList l) <- items]))
  >>= (\items -> Just [s | x@(BString s)<- items])

maybeAnnouncers :: [(String, BEnc)] -> Maybe [String]
maybeAnnouncers = (flattenStr <$>) . dictLookup "announce-list"
  where
    flattenStr (BList items) =
      [s | y@(BString s) <- concat [l | x@(BList l) <- items]]

info = dictLookup "info"

numPieces :: [(String, BEnc)] -> Maybe Int
numPieces dict = (\(BString s) -> ((`div` 20).length) s) <$> dictLookup "pieces" dict

-- getFileInfo [("name", BString "myDir"), ("files", BList [BDict [("path": BList [BString "a", BString "b"]), ("length", BInt 5)]])]
-- getFileInfo [("name", BString "myFile.txt"), ("length", BInt 5)]
getFileInfo :: [(String, BEnc)] -> [TorrentFile]
getFileInfo dict = do
  let len = dictLookup "length" dict
  let name = dictLookup "name" dict
  let files = dictLookup "files" dict
  case (name, files, len) of
    (Just (BString n), _, Just (BInt l)) ->
      [TorrentFile {torrentFilePath = [n], torrentFileSize=l}]
    (_, Just (BList files), _) -> fileInfo <$> files
      where
        fileInfo (BDict f) =
          case (dictLookup "path" f, dictLookup "length" f) of
            (Just (BList segments), Just (BInt len)) ->
              TorrentFile
              {torrentFilePath = [s | x@(BString s) <- segments], torrentFileSize = len}
            (_, _) -> error "File entries must have path and length properties"
    (_, _, _) -> error "There has to be either a name and length or a files property in the info dictionary"

totalSize :: [(String, BEnc)] -> Int
totalSize meta =
  case getFileInfo meta of
    files -> sum (torrentFileSize <$> files)

pieceChunks :: Int -> Int -> [(Int, Int)]
pieceChunks pieceLength step = [0, step, pieceLength-step] `zip` repeat step

metainfoParser = bdict

int10 :: ParsecT B.ByteString u Identity Int
int10 = read <$> liftM2 (\a b -> maybe b (\_ -> '-':b) a) (optionMaybe $ char '-') (many1 digit)

belement :: ParsecT B.ByteString u Identity BEnc
belement = (BString <$> bstring) <|>
            (BInt <$> bint) <|>
            (BList <$> blist) <|>
            (BDict <$> bdict)
            <?> "element"

bstring = int10 <* char ':' >>= (`count` anyChar) <?> "string"

bint = char 'i' *> int10 <* char 'e' <?> "list"

blist :: ParsecT B.ByteString u Identity [BEnc]
blist = between (char 'l') (char 'e') (many belement) <?> "list"

bdict :: ParsecT B.ByteString u Identity [(String, BEnc)]
bdict = char 'd' *> many (liftM2 (,) bstring belement) <* char 'e' <?> "dict"

bencode :: BEnc -> B.ByteString
bencode (BString str) = BC.pack $ show (length str) ++ (':' : str)
bencode (BInt value) = BC.pack $ 'i' : show value ++ "e"
bencode (BList list) = B.concat ["l", fold (bencode <$> list), "e"]
bencode (BDict dict) = B.concat ["d", bencodeDict dict, "e"]

bencodeDict :: (Functor f, Foldable f,
                Bifoldable m, Monoid (m BC.ByteString BC.ByteString)
              ) => f (m String BEnc) -> BC.ByteString
bencodeDict = fold.fmap (bifoldMap (bencode.BString) bencode)