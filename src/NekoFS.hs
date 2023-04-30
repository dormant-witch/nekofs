module NekoFS
  ( extractNeko
  , createNeko
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.UTF8 (toString, fromString)
import           Data.Maybe (fromJust)

import           Control.Monad (forM_, forever)
import           Pipes
import qualified Pipes.Prelude as P
import           System.Directory
import           System.FilePath
import           System.IO

import Nekodata.DataBlock
import Nekodata.Serialization
import Nekodata.Types

-- | Extract a nekodata file
extractNeko :: FilePath -- ^ path to the nekofile
            -> FilePath -- ^ output directory
            -> IO ()
extractNeko nekofile outputDir = openBinaryFile nekofile ReadMode
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

    extractFile :: Handle -> Metadata -> IO ()
    extractFile hNeko meta = do
      let blocklist = getBlockSize meta
          startOffset = fromIntegral $ offset' meta
          filename = toString $ fileName meta
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


-- | Create a nekodata file from directory
createNeko :: FilePath -- ^ the directory to be packed into a nekofile
           -> FilePath -- ^ output directory
           -> IO ()
createNeko sourceDir outputDir = runEffect $ listRecursive sourceDir
                                         >-> P.map (makeRelative sourceDir)
                                         >-> compressFiles
                                         >-> collect
  where
    listRecursive :: FilePath -> Producer FilePath IO ()
    listRecursive topPath = do
      entries <- lift $ listDirectory topPath
      forM_ entries $ \name -> do
        let path = topPath </> name
        isDirectory <- lift $ doesDirectoryExist path
        if isDirectory then listRecursive path
                       else yield path


    compressFiles :: Pipe FilePath (Metadata, ByteString) IO ()
    compressFiles = forever $ do
      file <- await
      lift $ withBinaryFile file ReadMode $ \h ->
              undefined

    collect :: Consumer (Metadata, ByteString) IO ()
    collect = do
      lift $ createDirectoryIfMissing True outputDir
      hOut <- lift $ openBinaryFile outFile WriteMode
      lift $ B.hPut hOut nekofsHeader
        where
          nekofsHeader = fromString "pixelneko filesystem\0\0\0\0\1"
          outFile = outputDir </> takeBaseName sourceDir `addExtension` ".nekodata"

