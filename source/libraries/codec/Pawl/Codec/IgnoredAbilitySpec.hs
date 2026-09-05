module Pawl.Codec.IgnoredAbilitySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.IgnoredAbility as IgnoredAbility
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.IgnoredAbility" $ do
  -- CR 116.2d: who ignores, whose ability, by what name, and for how long. The
  -- seat, the permanent and the name are all different types, so only the expiry
  -- could be confused with anything -- and CR 514.2's AtCleanup is what every
  -- printed producer arms.
  Spec.it s "a player ignoring one of a permanent's abilities until end of turn" $
    Common.assertCodec
      s
      IgnoredAbility.codec
      IgnoredAbility.MkIgnoredAbility
        { IgnoredAbility.player = PlayerId.MkPlayerId 1,
          IgnoredAbility.source = ObjectId.MkObjectId 7,
          IgnoredAbility.ability = AbilityName.MkAbilityName (Text.pack "search ban"),
          IgnoredAbility.expiry = Expiry.AtCleanup
        }
      " {\"player\":1,\"source\":7,\"ability\":\"search ban\",\"expiry\":{\"type\":\"AtCleanup\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s IgnoredAbility.codec
