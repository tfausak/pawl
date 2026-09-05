module Pawl.Codec.ActivationProhibitionSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ActivationProhibition as ActivationProhibition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivationProhibition as ActivationProhibition
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivationProhibition" $ do
  -- Arrest's second clause (CR 602.2 / CR 101.2), which names no kind, so the
  -- key is absent -- the wire form data/cards/arrest.json writes.
  Spec.it s "MkActivationProhibition, no kind named" $
    Common.assertCodec
      s
      ActivationProhibition.codec
      ( ActivationProhibition.MkActivationProhibition
          (Affected.Matching (Filter.HasCardType CardType.Creature))
          Nothing
          Nothing
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}} "
  -- Realmbreaker's Grasp's "unless they're mana abilities", which names one.
  Spec.it s "MkActivationProhibition, a named kind" $
    Common.assertCodec
      s
      ActivationProhibition.codec
      ( ActivationProhibition.MkActivationProhibition
          Affected.Attached
          (Just AbilityKind.NonManaAbility)
          Nothing
      )
      " {\"affected\":{\"type\":\"Attached\"},\"kind\":{\"type\":\"NonManaAbility\"}} "
  -- CR 116.2d: Volrath's Curse names the ability its own ignore refers to, and
  -- names it on all three of the rows that one sentence declares.
  Spec.it s "MkActivationProhibition, a named ability" $
    Common.assertCodec
      s
      ActivationProhibition.codec
      ( ActivationProhibition.MkActivationProhibition
          Affected.Attached
          Nothing
          (Just (AbilityName.MkAbilityName (Text.pack "this effect")))
      )
      " {\"affected\":{\"type\":\"Attached\"},\"name\":\"this effect\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ActivationProhibition.codec
