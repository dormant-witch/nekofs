module Nekodata.Serialization
  ( readMetadata
  , makeMetadata
  , getBlockSize
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (when)
import           Data.Attoparsec.ByteString as P
import           Data.Bits
import           Data.ByteString.Builder (toLazyByteString, byteStringHex)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Functor
import           Data.Maybe (fromJust)
import           Data.Either (isLeft)
import           Data.Serializer hiding (size, toLazyByteString)

import Nekodata.Crypto
import Nekodata.Types

import System.IO

parseMetadata :: ByteString -> Either String [Metadata]
parseMetadata = parseOnly (metadataParser <* endOfInput)

metadataParser :: Parser [Metadata]
metadataParser = shiftedVLQParser' >>= flip count (entryParser <|> entry0Parser)

intVLQParser :: Parser IntVLQ
intVLQParser = IntVLQ <$> intVLQParser'

intVLQParser' :: Parser Int
intVLQParser' = do
  lsbs <- P.takeTill (<0x80)
  msb  <- anyWord8
  let vlq = B.snoc lsbs msb
  return $ disassembleVLQ vlq

shiftedVLQParser :: Parser ShiftedVLQ
shiftedVLQParser = ShiftedVLQ <$> shiftedVLQParser'

shiftedVLQParser' :: Parser Int
shiftedVLQParser' = (`rotateR` 1) <$> intVLQParser'

-- the same but doesn't consume the input
peekShiftedVLQ :: Parser ShiftedVLQ
peekShiftedVLQ = do
  residue <- getChunk
  let v = residue >>= decodeShiftedVLQ <&> fst
  return . ShiftedVLQ $ fromJust v  -- fire the missile if failed


entryParser :: Parser Metadata
entryParser = Metadata <$> (shiftedVLQParser' >>= P.take)   -- fileName
                       <*> P.word8 0x02                     -- tag
                       <*> shiftedVLQParser                 -- size
                       <*> shiftedVLQParser                 -- compressedSize
                       <*> intVLQParser                     -- crc32
                       <*> intVLQParser                     -- offset
                       <*> peekShiftedVLQ                   -- totalBlocks
                       <*> blockListParser                  -- blockList

entry0Parser :: Parser Metadata
entry0Parser = Metadata <$> (shiftedVLQParser' >>= P.take)   -- fileName
                        <*> P.word8 0x00                     -- tag
                        <*> shiftedVLQParser                 -- size
                        <*> return (ShiftedVLQ 0)            -- [x] compressedSize
                        <*> intVLQParser                     -- crc32
                        <*> intVLQParser                     -- offset
                        <*> return (ShiftedVLQ 0)            -- [x] totalBlocks
                        <*> return []                        -- [x] blockList
               <* P.word8 0


blockListParser :: Parser [BlockPos]
blockListParser = shiftedVLQParser' >>= flip count blockPosParser
  where
    blockPosParser = BlockPos <$> shiftedVLQParser -- deflateOffset
                              <*> shiftedVLQParser -- inflateOffset


decodeVLQ :: ByteString
          -> Maybe (Int, Int) -- ^ (value, length of the VLQ)
decodeVLQ bstr = retrieveVLQ bstr
             >>= \bs -> Just (disassembleVLQ bs, B.length bs)
  where
    retrieveVLQ :: ByteString -> Maybe ByteString
    retrieveVLQ bs = B.findIndex (<0x80) bs
                 >>= Just . flip B.take bs . (+1)


decodeShiftedVLQ :: ByteString -> Maybe (Int, Int)
decodeShiftedVLQ = fmap (\(w,l) -> (w `rotateR` 1, l)) . decodeVLQ


disassembleVLQ :: ByteString -> Int
disassembleVLQ = foldl (.|.) 0
               . zipWith (flip shiftL) [0,7..]
               . map (fromIntegral . (.&. 0x7f))
               . B.unpack


-- ---------------------------------------------------------------------
-- Utilities regarding the metadata of nekofs

-- | Calculate the size of each block based on the offset
getBlockSize :: Metadata -> [BlockSize]
getBlockSize meta = toBlockSize (compressedSize meta)
                                (size meta)
                                (blockList meta)
  where
    toBlockSize raw orig poslist =
      let xs = poslist ++ [BlockPos raw orig]
      in zipWith f xs (tail xs)
        where
          f :: BlockPos -> BlockPos -> BlockSize
          f b1 b2 = BlockSize (deflateOffset' b2 - deflateOffset' b1)
                              (inflateOffset' b2 - inflateOffset' b1)


-- | Retrieve the metadata from the nekodata file
readMetadata :: Handle -> IO (Either String [Metadata])
readMetadata h = do
  hSeek h SeekFromEnd (-5)
  buf <- B.hGet h 5
  case decodeShiftedVLQ $ B.reverse buf of
    Nothing -> return $ Left "Invalid offset at EOF"
    Just (metaLen, vlen) -> do
      metadataRaw <- hSeek h SeekFromEnd (- fromIntegral metaLen - fromIntegral vlen)
                  >> B.hGet h metaLen
      let metadataB = decrypt metadataRaw
      let meta = parseMetadata metadataB
      when (isLeft meta) $
                putStrLn ("Failed at parsing metadata: " ++ show (toLazyByteString $ byteStringHex metadataB))
      return meta


-- | Build the metadata bytestring
makeMetadata :: [QuasiMeta] -> ByteString
makeMetadata = attachLen . encrypt . toByteString . map fromQuasiMeta
  where
    attachLen s = s <> B.reverse (toByteString (ShiftedVLQ $ B.length s))

