{-# LANGUAGE OverloadedStrings #-}
module Metainfo where

  import Text.Parsec
  import qualified Network.URI.Encode as URI
  import Control.Monad (liftM2)
  import Data.Monoid
  import Data.Foldable (fold)
  import Data.Bifunctor (bimap, Bifunctor)
  import Data.Functor.Identity (Identity)
  import Data.Maybe
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

  dictLookup key dict = listToMaybe $ snd <$> filter (\(k,v) -> k == key) dict

  announcers :: [(String, BEnc)] -> [String]

  announcers dict = Data.Maybe.fromMaybe [] $ dictLookup "announce-list" dict
    >>= (\(BList items) -> Just (concat [l | x@(BList l) <- items]))
    >>= (\items -> Just [s | x@(BString s)<- items])

  info = dictLookup "info"

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

  bencodeDict :: (Bifunctor p,
                  Functor t,
                  Monoid (p BC.ByteString BC.ByteString),
                  Foldable (p BC.ByteString),
                  Foldable t)
                  => t (p String BEnc) -> BC.ByteString
  bencodeDict dict = (fold.fold) $ bimap (bencode.BString) bencode <$> dict