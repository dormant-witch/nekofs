module Nekodata.Types
  ( IntVLQ (..)
  , ShiftedVLQ (..)
  , Metadata (..)
  , BlockPos (..)
  , BlockSize (..)
  , deflateOffset'
  , inflateOffset'
  , offset'
  ) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Word (Word8)

newtype IntVLQ = IntVLQ Int
newtype ShiftedVLQ = ShiftedVLQ Int

data Metadata = Metadata
  { fileName       :: ByteString
  , tag            :: Word8
  , size           :: ShiftedVLQ
  , compressedSize :: ShiftedVLQ
  , crc32          :: IntVLQ
  , offset         :: IntVLQ
  , totalBlocks    :: ShiftedVLQ
  , blockList      :: [BlockPos]
  }

data BlockPos = BlockPos
  { deflateOffset :: ShiftedVLQ
  , inflateOffset :: ShiftedVLQ
  }

data BlockSize = BlockSize
  { deflateSize :: Int
  , inflateSize :: Int
  }

deflateOffset' :: BlockPos -> Int
deflateOffset' = coerce . deflateOffset

inflateOffset' :: BlockPos -> Int
inflateOffset' = coerce . inflateOffset

offset' :: Metadata -> Int
offset' = coerce . offset

