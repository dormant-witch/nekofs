module NekoFS
  ( extractNeko
  , createNeko
  , generateMeta
  ) where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.UTF8 (fromString, toString)
import           Data.Digest.Adler32 as S
import           Data.Digest.CRC32 as S
import           Data.Maybe (fromJust)

import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Find as F
import System.IO

import Nekodata.DataBlock
import Nekodata.FilesMeta
import Nekodata.Serialization
import Nekodata.Types as T

-- | Extract a nekodata file
extractNeko :: FilePath -- ^ path to the nekofile
            -> FilePath -- ^ output directory
            -> Bool     -- ^ verify the output?
            -> IO ()
extractNeko nekofile outputDir verify = openBinaryFile nekofile ReadMode
                                    >>= extractAll
  where
    extractAll :: Handle -> IO ()
    extractAll hNeko = do
      meta <- readMetadata hNeko
      case meta of
        Left e         -> error e
        Right metalist -> do
          let outputDir' = outputDir </> takeBaseName nekofile
          createDirectoryIfMissing True outputDir'
          withCurrentDirectory outputDir' $
                            mapM_ (extractFile hNeko) metalist
          when verify $ putStrLn ("Verifying " ++ show nekofile)
                        >>  verifyIntegrity outputDir'
                        >>= \b -> if b then putStrLn "success."
                                       else putStrLn "integrity check failed."
                                            >> exitWith (ExitFailure 1)

    extractFile :: Handle -> Metadata -> IO ()
    extractFile hNeko meta = do
      let blocklist = getBlockSize meta
          startOffset = fromIntegral $ offset' meta
          filename = toString $ T.fileName meta
      createDirectoryIfMissing True (takeDirectory filename)
      outFile <- openBinaryFile filename WriteMode
      hSeek hNeko AbsoluteSeek startOffset
      extract' outFile blocklist
        where
          extract' hOut []     = hClose hOut
          extract' hOut (b:bs) = do
              let defsize = deflateSize b
                  infsize = inflateSize b
              block <- B.hGet hNeko defsize
              let block' = fromJust $ decompressBlock infsize defsize block
              B.hPut hOut block'
              extract' hOut bs

normalizePathSep :: FilePath -> FilePath
normalizePathSep = map (\c -> if c == '\\' then '/' else c)   -- issue #2

-- | Create a nekodata file from directory
createNeko :: FilePath -- ^ the directory to be packed into a nekofile
           -> FilePath -- ^ output filename
           -> IO ()
createNeko sourceDir outputFile = do
  exists <- doesDirectoryExist sourceDir
  unless exists $ error
            ("ERR: source directory " ++ show sourceDir ++ " doesn't exist")
  hOut <- prepareOutFile
  filelist <- find always filterFile sourceDir
  qmetaListRev <- foldM (compressFile hOut) [] filelist
  let filesMeta = makeFilesMeta qmetaListRev
  B.writeFile metaPath filesMeta
  qmetaList <- compressFile hOut qmetaListRev metaPath
  B.hPut hOut $ makeMetadata qmetaList
  hClose hOut
    where
      filterFile = fileType ==? RegularFile &&? F.fileName /=? "files.meta"
      metaPath = sourceDir </> "files.meta"

      prepareOutFile = do
        let outputFile' = if outputFile /= "SRCDIR.nekodata" then outputFile
                          else last (splitDirectories sourceDir) `addExtension` ".nekodata"
        createDirectoryIfMissing True (takeDirectory outputFile')
        h <- openBinaryFile outputFile' WriteMode
        B.hPut h nekofsHeader
        return h
        where
          nekofsHeader = fromString "pixelneko filesystem\0\0\0\0\1"

      compressFile :: Handle -> [QuasiMeta] -> FilePath -> IO [QuasiMeta]
      compressFile hOut acc file = do
        (sz, csz, crc, crcOrig, adler, bcnt, bszL) <-
          openBinaryFile file ReadMode >>= compressChunksAccu (0, 0,
              S.crc32 (mempty :: ByteString),
              S.crc32 (mempty :: ByteString),
              S.adler32 (mempty :: ByteString), 0, [])
        let qOffset = if null acc then 25 -- ^ size of nekofsHeader
                                  else getCurrentOffset (head acc)
            file' = normalizePathSep $ makeRelative sourceDir file -- to InternalPath
        return (Q file' sz csz crc crcOrig adler qOffset bcnt bszL : acc)
          where
            getCurrentOffset = (+) <$> compressedSizeQ <*> offsetQ

            compressChunksAccu :: ChunksInfo -> Handle -> IO ChunksInfo
            compressChunksAccu (sz, csz, crc, crcOrig, adler, bcnt, bszL) hInput = do
              buf <- B.hGet hInput 32768
              let len = B.length buf
              if len == 0
                then hClose hInput >> return (sz, csz, crc, crcOrig, adler, bcnt, reverse bszL)
                else do
                  let blk = fromJust $ compressBlock buf
                      blksz = B.length blk
                      bsz = BlockSize blksz len
                      crc' = S.crc32Update crc blk  -- crc32 of compressed blocks
                      crcOrig' = S.crc32Update crcOrig buf  -- crc32 of original buffer
                      adler' = S.adler32Update adler buf  -- adler32 of original buffer
                  B.hPut hOut blk
                  compressChunksAccu (sz+len, csz+blksz, crc', crcOrig', adler', bcnt+1, bsz:bszL)
                                      hInput

-- | Generate files.meta for a file or directory
generateMeta :: FilePath -- ^ the file or directory to calculate files.meta
             -> IO ()
generateMeta path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir then genForDir path
           else if isFile then genForFile path
                          else error "Not a file or directory"
  where
    genForFile f = do
      let outputDir = takeDirectory f
          metafile = outputDir </> "files.meta"
      meta <- withCurrentDirectory outputDir $
                      makeFilesMeta' [takeFileName f]
      B.writeFile metafile meta

    genForDir dir = do
      let metafile = dir </> "files.meta"
      meta <- withCurrentDirectory dir $
                        find always (fileType ==? RegularFile) "."
                        >>= makeFilesMeta' . map (normalizePathSep . makeRelative ".")
      B.writeFile metafile meta


