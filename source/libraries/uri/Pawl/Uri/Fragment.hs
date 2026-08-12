module Pawl.Uri.Fragment where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import qualified Data.Word as Word
import qualified Pawl.Extra.Word8 as Word8

-- | Percent-encodes every octet RFC 3986 does not allow in a fragment.
encode :: Builder.Builder -> Builder.Builder
encode =
  foldMap encodeOctet
    . LazyByteString.unpack
    . Builder.toLazyByteString

encodeOctet :: Word.Word8 -> Builder.Builder
encodeOctet w =
  if isValid w
    then Builder.word8 w
    else
      let (hi, lo) = divMod w 16
       in Builder.charUtf8 '%' <> Builder.word8Hex hi <> Builder.word8Hex lo

isValid :: Word.Word8 -> Bool
isValid w =
  let c = Char.chr $ Word8.toInt w
   in Char.isAsciiUpper c
        || Char.isAsciiLower c
        || Char.isDigit c
        || elem c "-._~!$&'()*+,;=:@/?"
