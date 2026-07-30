module Pawl.Extra.Builder where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.Encoding.Error as Error
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Encoding

-- | Converts a builder into a string, assuming a UTF-8 encoding.
toString :: Builder.Builder -> String
toString =
  Text.unpack
    . Encoding.decodeUtf8With Error.lenientDecode
    . Builder.toLazyByteString
