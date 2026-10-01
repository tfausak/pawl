{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.NamesAre where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.NamesAre as NamesAre

codec :: Codec.Codec NamesAre.NamesAre
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec NamesAre.object
  names <- Fields.required "names" (Common.set CardName.codec) NamesAre.names
  pure NamesAre.MkNamesAre {NamesAre.object = object, NamesAre.names = names}
