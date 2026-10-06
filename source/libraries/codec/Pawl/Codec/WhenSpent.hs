{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.WhenSpent where

import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.WhenSpent as WhenSpent

-- | Both keys required: every printing names the spells it watches, and the
-- ability is the whole point.
codec :: Codec.Codec WhenSpent.WhenSpent
codec = Fields.object $ do
  casts <- Fields.required "casts" (Filter.codec Keyword.codec) WhenSpent.casts
  ability <- Fields.required "ability" AbilityName.codec WhenSpent.ability
  pure WhenSpent.MkWhenSpent {WhenSpent.casts = casts, WhenSpent.ability = ability}
