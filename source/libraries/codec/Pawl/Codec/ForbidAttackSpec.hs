module Pawl.Codec.ForbidAttackSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ForbidAttack as ForbidAttack
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AimedAt as AimedAt
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForbidAttack" $ do
  -- CR 508.1c. Netter en-Dal's own payload, aimedAt elided.
  Spec.it s "MkForbidAttack, aimedAt elided" $
    Common.assertCodec
      s
      ForbidAttack.codec
      ( ForbidAttack.MkForbidAttack
          { ForbidAttack.duration = Duration.UntilEndOfTurn,
            ForbidAttack.affected = RestrictedCreatures.Named (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
            ForbidAttack.aimedAt = Nothing
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"affected\":{\"type\":\"Named\",\"value\":{\"type\":\"InSlot\",\"value\":\"target\"}}} "
  -- CR 611.2c / 802.3a. Chronomantic Escape's own payload.
  Spec.it s "MkForbidAttack, a class aimed at a player" $
    Common.assertCodec
      s
      ForbidAttack.codec
      ( ForbidAttack.MkForbidAttack
          { ForbidAttack.duration = Duration.UntilYourNextTurn,
            ForbidAttack.affected = RestrictedCreatures.Matching (Filter.HasCardType CardType.Creature),
            ForbidAttack.aimedAt = Just (AimedAt.MkAimedAt PlayerScope.You (Set.singleton AttackTargetKind.OfPlayer))
          }
      )
      " {\"duration\":{\"type\":\"UntilYourNextTurn\"},\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"aimedAt\":{\"defenders\":{\"type\":\"You\"},\"kinds\":[{\"type\":\"OfPlayer\"}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ForbidAttack.codec
