{-# LANGUAGE OverloadedStrings #-}
module Files where

import qualified Data.ByteString as B
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath.Posix ((</>), takeDirectory)
import System.IO (IOMode(WriteMode, ReadWriteMode), openBinaryFile, hSetFileSize, hFlush, hClose, hSeek, SeekMode(AbsoluteSeek))

data TorrentFile = TorrentFile
  { torrentFilePath :: [String]
  , torrentFileSize :: Int
  } deriving (Eq, Show, Read)

data FileSegment = FileSegment TorrentFile Int B.ByteString deriving (Eq, Show, Read)

touchFiles :: [TorrentFile] -> FilePath -> IO ()
touchFiles files saveToDirectory =
  sequence_ $ (\TorrentFile{torrentFilePath=parts, torrentFileSize=size}-> do 
    let path = foldl (</>) saveToDirectory parts
    createDirectoryIfMissing True $ takeDirectory path
    h <- openBinaryFile path WriteMode
    hSetFileSize h (fromIntegral size)
    hFlush h
    hClose h
    ) <$> files

getTorrentFileSegments :: [TorrentFile] -> Int -> B.ByteString -> [FileSegment]--[([String], Int, B.ByteString)]
getTorrentFileSegments files = accum
    where
      accum _ "" = []
      accum offset buffer =  do
        let segment@(FileSegment _ _ segmentBuffer) = fileSegmentAtOffset files offset buffer
        let len = B.length segmentBuffer
        segment : accum (offset + len) (B.drop len buffer)

fileSegmentAtOffset :: [TorrentFile] -> Int -> B.ByteString -> FileSegment
fileSegmentAtOffset files offset buffer = head $ fst $ foldl f ([], 0) files
  where
    f (results, globalOffset) file@TorrentFile{torrentFilePath=path, torrentFileSize=size} =
      if offset >= globalOffset && offset < (globalOffset + size) then do
        let fileOffset = offset-globalOffset
        let len = min (B.length buffer) (size-fileOffset)
        (results ++ [FileSegment file fileOffset (B.take len buffer)], globalOffset + len)
      else (results, globalOffset + size)
fileAtOffset :: [TorrentFile] -> Int -> ([String], Int, Int)
fileAtOffset files offset = head $ fst $ foldl f ([], 0) files
  where
    f (results, globalOffset) TorrentFile{torrentFilePath=path, torrentFileSize=size} =
      if offset >= globalOffset && offset < (globalOffset + size) then
        (results ++ [(path, size, offset - globalOffset)], globalOffset + size)
      else (results, globalOffset + size)

writeFileSegment :: FilePath -> FileSegment -> IO ()
writeFileSegment downloadFolder (FileSegment TorrentFile{torrentFilePath=path} offset buffer) = do
  h <- openBinaryFile (foldl (</>) downloadFolder path) ReadWriteMode
  hSeek h AbsoluteSeek $ fromIntegral offset
  B.hPutStr h buffer
