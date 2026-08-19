{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpellCast where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SpellCast as SpellCast

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. The first two keys are required: an unscoped
-- trigger is a different card, not a defaulted one. The zone and the ordinal are
-- elided instead, because a trigger that names neither is what almost every
-- printing writes.
codec :: Codec.Codec SpellCast.SpellCast
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) SpellCast.filter
  scope <- Fields.required "scope" TurnScope.codec SpellCast.scope
  zone <- Fields.defaulted "zone" Nothing (Common.maybe Zone.codec) SpellCast.zone
  ordinal <- Fields.defaulted "ordinal" Nothing (Common.maybe Common.natural) SpellCast.ordinal
  pure SpellCast.MkSpellCast {SpellCast.filter = filter_, SpellCast.scope = scope, SpellCast.zone = zone, SpellCast.ordinal = ordinal}
