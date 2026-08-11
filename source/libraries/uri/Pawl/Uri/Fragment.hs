-- | A URI's fragment component, per RFC 3986 section 3.5.
module Pawl.Uri.Fragment where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Char as Char
import qualified Data.Word as Word
import qualified Pawl.Extra.Word8 as Word8

-- | Percent-encodes every octet RFC 3986 does not allow in a fragment. It takes
-- octets rather than text, so escaping a caller has already done survives:
-- nothing here rewrites an octet the production allows.
--
-- The @#@ that separates a fragment from the rest of a URI-reference is not
-- part of the fragment, so it is not written here.
encode :: Builder.Builder -> Builder.Builder
encode =
  foldMap encodeOctet
    . ByteString.unpack
    . Builder.toLazyByteString

encodeOctet :: Word.Word8 -> Builder.Builder
encodeOctet w =
  if isFragmentOctet w
    then Builder.word8 w
    else
      let (hi, lo) = divMod w 16
       in Builder.charUtf8 '%' <> Builder.charUtf8 (hexDigit hi) <> Builder.charUtf8 (hexDigit lo)

-- | RFC 3986's @fragment@ production: @unreserved@, @sub-delims@, @:@, @\@@,
-- @\/@ and @?@. Every octet above 127 falls outside it, which is what
-- percent-encodes a UTF-8 sequence byte by byte.
isFragmentOctet :: Word.Word8 -> Bool
isFragmentOctet w =
  let c = Char.chr (Word8.toInt w)
   in Char.isAsciiUpper c
        || Char.isAsciiLower c
        || Char.isDigit c
        || elem c "-._~!$&'()*+,;=:@/?"

-- | Uppercase, which RFC 3986 section 2.1 prefers.
hexDigit :: Word.Word8 -> Char
hexDigit w =
  Char.chr $
    if w < 10
      then Char.ord '0' + Word8.toInt w
      else Char.ord 'A' + Word8.toInt w - 10
