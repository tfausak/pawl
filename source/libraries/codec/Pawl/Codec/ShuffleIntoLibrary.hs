{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ShuffleIntoLibrary where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary

-- | A bare object keyed by the record's field names, with the library-naming ref
-- elided when absent.
--
-- The positional payload this replaces was a lone ObjectRef or a pair, told
-- apart on decode by JSON TYPE -- which only worked once #1304 stopped an
-- ObjectRef from ever being an array itself. A named optional key needs no such
-- argument.
codec :: Codec.Codec ShuffleIntoLibrary.ShuffleIntoLibrary
codec = Fields.object $ do
  library <- Fields.defaulted "library" Nothing (Common.maybe PlayerRef.codec) ShuffleIntoLibrary.library
  ref <- Fields.required "ref" ObjectRef.codec ShuffleIntoLibrary.ref
  pure
    ShuffleIntoLibrary.MkShuffleIntoLibrary
      { ShuffleIntoLibrary.library = library,
        ShuffleIntoLibrary.ref = ref
      }
