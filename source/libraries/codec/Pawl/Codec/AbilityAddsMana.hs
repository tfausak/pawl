{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AbilityAddsMana where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaSpecification as ManaSpecification
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AbilityAddsMana as AbilityAddsMana

-- | A bare object keyed by the record's field names, every key required for
-- Pawl.Codec.PermanentTappedForMana's reason.
codec :: Codec.Codec AbilityAddsMana.AbilityAddsMana
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec AbilityAddsMana.player
  source <- Fields.required "source" (Filter.codec Keyword.codec) AbilityAddsMana.source
  mana <- Fields.required "mana" ManaSpecification.codec AbilityAddsMana.mana
  pure AbilityAddsMana.MkAbilityAddsMana {AbilityAddsMana.player = player, AbilityAddsMana.source = source, AbilityAddsMana.mana = mana}
