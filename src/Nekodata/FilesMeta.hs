{-# LANGUAGE DeriveGeneric #-}

module Nekodata.FilesMeta
  ( makeFilesMeta
  , makeFilesMeta'
  , verifyIntegrity
  ) where

import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Word

import Control.Monad (unless)
import GHC.Generics
import System.Directory
import System.PosixCompat.Files (fileSize, getFileStatus)
import Text.Printf

import Nekodata.Checksum
import Nekodata.Types (QuasiMeta (..))

-- | files.meta format
type InternalPath = FilePath

data Checksums = Checksums
  { adler32 :: Word32
  , crc32   :: Word32
  , size    :: Int
  } deriving (Show, Generic)

data FilesMeta = FilesMeta
  { files   :: M.Map InternalPath Checksums
  , deletes :: [InternalPath]
  } deriving (Show, Generic)

instance FromJSON Checksums
instance FromJSON FilesMeta

instance ToJSON Checksums
instance ToJSON FilesMeta

makeFilesMeta :: [QuasiMeta] -> ByteString
makeFilesMeta qlist = B.toStrict $ encode $ FilesMeta filesmap []
  where
    filesmap = M.fromList $ map toFilePair qlist
    toFilePair q = (fileNameQ q, chksum)
      where
        chksum = Checksums
                  { adler32 = adler32Q q
                  , crc32   = crc32OrigQ q
                  , size    = sizeQ q
                  }

makeFilesMeta' :: [FilePath] -> IO ByteString
makeFilesMeta' filelist = do
  filespair <- mapM toFilePair filelist
  return (B.toStrict $ encode $ FilesMeta (M.fromList filespair) [])
    where
      toFilePair :: FilePath -> IO (FilePath, Checksums)
      toFilePair f = do
        adler <- adler32ofFile f
        crc   <- crc32ofFile f
        sz    <- fromIntegral . fileSize <$> getFileStatus f
        return (f, Checksums adler crc sz)


verifyIntegrity :: FilePath  -- ^ output directory
                -> IO Bool
verifyIntegrity outDir = withCurrentDirectory outDir $ do
  filelist <- files . fromJust . decode <$> BL.readFile "files.meta"
  results <- M.traverseWithKey verifyFile filelist
  return $ M.foldr' (&&) True results
  where
    verifyFile :: InternalPath -> Checksums -> IO Bool
    verifyFile file (Checksums adler32f _ sizef) = do
      adler32f' <- adler32ofFile file
      let match = adler32f == adler32f'
      unless match $
        printf " > mismatch on file %s (size: %d) with adler32 of %d, expecting %d"
               (show file) sizef adler32f' adler32f
      return match

