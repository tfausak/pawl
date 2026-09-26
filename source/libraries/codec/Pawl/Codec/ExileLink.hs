{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExileLink where

import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExileLink as ExileLink

-- | An object keyed by the record's field names; "ability" defaults to Nothing,
-- the unnamed exiling ability.
codec :: Codec.Codec ExileLink.ExileLink
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ExileLink.source
  ability <- Fields.defaulted "ability" Nothing (Common.maybe AbilityName.codec) ExileLink.ability
  pure ExileLink.MkExileLink {ExileLink.source = source, ExileLink.ability = ability}
