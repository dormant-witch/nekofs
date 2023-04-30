module Nekodata.Crypto
    ( encrypt
    , decrypt
    ) where

import Crypto.Cipher.ChaCha

import           Data.ByteArray (ScrubbedBytes)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C8
import           Data.Serializer (toByteString)
import           Data.String (fromString)
import           Data.Word

import Unsafe.Coerce (unsafeCoerce)

theKey :: ByteString
theKey = fromHexString "a5057f03aa62829ac7dc4c64f9f7f428b414e71516e2c3a8de9c77f9880f2ed4"

theNonce :: ByteString
theNonce = fromHexString "0200010901000100"

fromHexString :: String -> ByteString
fromHexString = Base16.decodeLenient . C8.pack

-- | Encrypt using the preset key
encrypt :: ByteString -> ByteString
encrypt = encrypt' theKey theNonce

-- | Decrypt using the preset key
decrypt :: ByteString -> ByteString
decrypt = encrypt

encrypt' :: ByteString -> ByteString -> ByteString -> ByteString
encrypt' key nonce cipher = fst $ combine ctx cipher
  where ctx = initialize' 20 key nonce (B.length cipher)

initialize' :: Int -> ByteString -> ByteString -> Int -> State
initialize' nRounds key nonce counter = unsafeCoerce sbytes -- legit since State is a newtype of ScrubbedBytes
  where sbytes = fromString $ C8.unpack st :: ScrubbedBytes
        st = constant <> key <> cnt <> zeroBytes 4 <> nonce <> prevBuf <> nbRounds <> zeroBytes 1
        constant = fromString "expand 32-byte k"
        cnt = toByteString (fromIntegral counter :: Word32)
        prevBuf = zeroBytes (64+2)
        nbRounds = toByteString (fromIntegral nRounds :: Word8)
        zeroBytes n = B.replicate n 0

