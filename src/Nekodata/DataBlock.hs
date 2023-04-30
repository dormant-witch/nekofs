module Nekodata.DataBlock
  ( compressBlock
  , decompressBlock
  ) where

import           Codec.Compression.LZ4
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Serializer (toByteString)
import           Data.Word

-- | Compress the data into raw LZ4 data block
compressBlock :: ByteString -> Maybe ByteString
compressBlock bstr = B.drop 8 <$> compressHC bstr

-- | Decompress a raw LZ4 data chunk
decompressBlock :: Int -- ^ original size
                -> Int -- ^ compressed size
                -> ByteString -- ^ the raw LZ4 data block
                -> Maybe ByteString
decompressBlock infSize defSize bstr = decompress (header <> bstr)
  where
    header = infSize' <> defSize'
    infSize' = toByteString (fromIntegral infSize :: Word32)
    defSize' = toByteString (fromIntegral defSize :: Word32)

