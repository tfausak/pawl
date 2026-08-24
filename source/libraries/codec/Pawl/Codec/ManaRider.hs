{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaRider where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaRiderEffect as ManaRiderEffect
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaRider as ManaRider

-- | CR 106.6's additional effect, as an object of two keys.
--
-- @condition@ is DEFAULTED to Pawl.Types.Filter's trivial @And []@, and the
-- default is the printed wording rather than a convenience: Delighted
-- Halfling's "and that spell can't be countered" narrows nothing, so a card
-- that writes no condition means every object the mana could have paid for.
-- Boseiju, Who Shelters All's "if that mana is spent on an instant or sorcery
-- spell" is the printing that writes the key.
--
-- @effect@ is REQUIRED: a rider with no payload is not a rider.
codec :: Codec.Codec ManaRider.ManaRider
codec = Fields.object $ do
  condition <- Fields.defaulted "condition" (Filter.And []) (Filter.codec Keyword.codec) ManaRider.condition
  effect <- Fields.required "effect" ManaRiderEffect.codec ManaRider.effect
  pure
    ManaRider.MkManaRider
      { ManaRider.condition = condition,
        ManaRider.effect = effect
      }
