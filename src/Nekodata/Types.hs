{-# LANGUAGE FlexibleInstances #-}

module Nekodata.Types
  ( IntVLQ (..)
  , ShiftedVLQ (..)
  , Metadata (..)
  , QuasiMeta (..)
  , ChunksInfo
  , BlockPos (..)
  , BlockSize (..)
  , deflateOffset'
  , inflateOffset'
  , offset'
  , fromQuasiMeta
  , toBlockPos
  ) where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.Coerce (coerce)
import           Data.Word (Word32, Word8)

import Data.Serializer hiding (size)

newtype IntVLQ = IntVLQ Int
newtype ShiftedVLQ = ShiftedVLQ Int

instance Serializable IntVLQ where
  put v = byteString $ makeVLQ v

instance Serializable ShiftedVLQ where
  put v = byteString $ makeShiftedVLQ v

-- | Entry of the metadata section at the end of nekofs file
data Metadata = Metadata
  { fileName       :: ByteString
  , tag            :: Word8 -- ^ 2 for all of the compressed nekodata
  , size           :: ShiftedVLQ
  , compressedSize :: ShiftedVLQ
  , crc32          :: IntVLQ
  , offset         :: IntVLQ
  , totalBlocks    :: ShiftedVLQ
  , blockList      :: [BlockPos]
  }

instance Serializable Metadata where
  put meta = byteString (makeShiftedVLQ len) -- ^ length of filename
          <> byteString (fileName meta)
          <> word8 (tag meta)
          <> put (size meta)
          <> put (compressedSize meta)
          <> put (crc32 meta)
          <> put (offset meta)
          <> put (totalBlocks meta)
          <> put (blockList meta)
        where len = coerce (B.length $ fileName meta)

instance Serializable [Metadata] where
  put metaL = byteString (makeShiftedVLQ len) -- ^ numbers of Metadata
           <> go metaL
    where len = coerce (length metaL)
          go []     = mempty
          go (m:ms) = put m <> go ms

data QuasiMeta = Q
  { fileNameQ       :: String
  , sizeQ           :: Int
  , compressedSizeQ :: Int
  , crc32Q          :: Word32
  , crc32OrigQ      :: Word32
  , adler32Q        :: Word32
  , offsetQ         :: Int
  , totalBlocksQ    :: Int
  , blockListQ      :: [BlockSize]
  }

type ChunksInfo =
  ( Int         -- size
  , Int         -- compressedSize
  , Word32      -- crc32
  , Word32      -- crc32Orig
  , Word32      -- adler32
  , Int         -- totalBlocks
  , [BlockSize]
  )

data BlockPos = BlockPos
  { deflateOffset :: ShiftedVLQ
  , inflateOffset :: ShiftedVLQ
  }

instance Serializable [BlockPos] where
  put []     = mempty
  put (p:ps) = put (deflateOffset p)
            <> put (inflateOffset p)
            <> put ps

data BlockSize = BlockSize
  { deflateSize :: Int
  , inflateSize :: Int
  }

makeVLQ :: IntVLQ -> ByteString
makeVLQ (IntVLQ v) = B.pack $ go [] v
  where
    go acc val =
      if val < 0x80
        then reverse (fromIntegral val : acc)
        else let lsb = (val .&. 0x7f) .|. 0x80
             in go (fromIntegral lsb : acc) (val `shiftR` 7)

makeShiftedVLQ :: ShiftedVLQ -> ByteString
makeShiftedVLQ (ShiftedVLQ v) = makeVLQ $ coerce (v `rotateL` 1)

-- ---------------------------------------------------------------------
-- newtype unwrappers

deflateOffset' :: BlockPos -> Int
deflateOffset' = coerce . deflateOffset

inflateOffset' :: BlockPos -> Int
inflateOffset' = coerce . inflateOffset

offset' :: Metadata -> Int
offset' = coerce . offset

-- | Convert QuasiMeta to Metadata
fromQuasiMeta :: QuasiMeta -> Metadata
fromQuasiMeta (Q fn sz csz crc _ _ ofst bcnt bszL) = Metadata
      { fileName        = C8.pack fn
      , tag             = 2     -- ^ 2 for any compressed nekodata file
      , size            = ShiftedVLQ sz
      , compressedSize  = ShiftedVLQ csz
      , crc32           = IntVLQ $ fromIntegral crc
      , offset          = IntVLQ ofst
      , totalBlocks     = ShiftedVLQ bcnt
      , blockList       = toBlockPos bszL
      }

-- | Convert [BlockSize] to [BlockPos]
toBlockPos :: [BlockSize] -> [BlockPos]
toBlockPos = go [BlockPos (ShiftedVLQ 0) (ShiftedVLQ 0)]
  where
    go posL [_] = reverse posL
    go posL@(p:_) (s:ss) =
      let pos = BlockPos (ShiftedVLQ $ deflateOffset' p + deflateSize s)
                         (ShiftedVLQ $ inflateOffset' p + inflateSize s)
      in go (pos:posL) ss

    go _ _ = undefined -- impossible

