module Nekodata.Checksum
  ( adler32ofFile
  , crc32ofFile
  ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Digest.Adler32
import           Data.Digest.CRC32
import           Data.Word

adler32ofFile :: FilePath -> IO Word32
adler32ofFile file = adler32 <$> BL.readFile file

crc32ofFile :: FilePath -> IO Word32
crc32ofFile file = crc32 <$> BL.readFile file

