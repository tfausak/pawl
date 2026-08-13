{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpellCast where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SpellCast as SpellCast

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Both keys are required: an unscoped trigger is
-- a different card, not a defaulted one.
codec :: Codec.Codec SpellCast.SpellCast
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) SpellCast.filter
  scope <- Fields.required "scope" TurnScope.codec SpellCast.scope
  pure SpellCast.MkSpellCast {SpellCast.filter = filter_, SpellCast.scope = scope}
