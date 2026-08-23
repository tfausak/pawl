{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerStaticAbility where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- The CR 604.2 "as long as" gate is OPTIONAL, and absent means unconditional --
-- Pawl.Codec.StaticAbility's posture for the same field on the object-facing
-- carrier, so a card that states no clause writes no key.
codec :: Codec.Codec PlayerStaticAbility.PlayerStaticAbility
codec = Fields.object $ do
  scope <- Fields.required "scope" PlayerScope.codec PlayerStaticAbility.scope
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) PlayerStaticAbility.condition
  effect <- Fields.required "effect" PlayerEffect.codec PlayerStaticAbility.effect
  pure
    PlayerStaticAbility.MkPlayerStaticAbility
      { PlayerStaticAbility.scope = scope,
        PlayerStaticAbility.condition = condition,
        PlayerStaticAbility.effect = effect
      }
