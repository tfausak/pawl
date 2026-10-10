module Pawl.Codec.CostModifierSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.CostModifier as CostModifier
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.ActivationCriteria as ActivationCriteria
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CostChange as CostChange
import qualified Pawl.Types.CostModifier as CostModifier.Type
import qualified Pawl.Types.CostSubject as CostSubject
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostModifier" $ do
  -- Every Maybe criterion absent: Thalia's shape, and no key written for any.
  Spec.it s "a spell increase" $
    Common.assertCodec
      s
      CostModifier.codec
      (CostModifier.Type.MkCostModifier CostSubject.Spells (Filter.Not (Filter.HasCardType CardType.Creature)) Nothing Nothing Nothing (CostChange.Increase 1))
      " {\"subject\":{\"type\":\"Spells\"},\"matching\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"change\":{\"type\":\"Increase\",\"value\":1}} "
  -- Every criterion present on the activation subject: Professor Hojo's first,
  -- Zirda's kind, Dwarven Mauler's target, Heartstone's floor.
  Spec.it s "an activation reduction with every criterion" $
    Common.assertCodec
      s
      CostModifier.codec
      ( CostModifier.Type.MkCostModifier
          (CostSubject.Activations (ActivationCriteria.MkActivationCriteria Nothing (Just AbilityKind.NonManaAbility) Nothing))
          (Filter.HasCardType CardType.Creature)
          (Just Filter.IsSource)
          (Just (Filter.HasCardType CardType.Creature))
          (Just TurnScope.ControllersTurn)
          (CostChange.Reduce (AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost [ManaSymbol.Generic 2]) 1 False))
      )
      " {\"subject\":{\"type\":\"Activations\",\"value\":{\"whichKind\":{\"type\":\"NonManaAbility\"}}},\"matching\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"whichTargets\":{\"type\":\"IsSource\"},\"perTarget\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"onlyFirst\":{\"type\":\"ControllersTurn\"},\"change\":{\"type\":\"Reduce\",\"value\":{\"amount\":[{\"type\":\"Generic\",\"value\":2}],\"atLeast\":1}}} "
  Spec.it s "refuses a spell's onlyFirst (#4924)" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CostModifier.codec =<< Common.parse (Text.pack "{\"subject\":{\"type\":\"Spells\"},\"matching\":{\"type\":\"IsSource\"},\"onlyFirst\":{\"type\":\"ControllersTurn\"},\"change\":{\"type\":\"Increase\",\"value\":1}}")))
      "expected a decode failure"
  Spec.it s "refuses onlyFirst beside whichLoyalty" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CostModifier.codec =<< Common.parse (Text.pack "{\"subject\":{\"type\":\"Activations\",\"value\":{\"whichLoyalty\":{\"type\":\"LoyaltyAbility\"}}},\"matching\":{\"type\":\"IsSource\"},\"onlyFirst\":{\"type\":\"ControllersTurn\"},\"change\":{\"type\":\"Increase\",\"value\":1}}")))
      "expected a decode failure"
  Spec.it s "refuses a target criterion on an addition to an activation" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode CostModifier.codec =<< Common.parse (Text.pack "{\"subject\":{\"type\":\"Activations\",\"value\":{}},\"matching\":{\"type\":\"IsSource\"},\"whichTargets\":{\"type\":\"IsSource\"},\"change\":{\"type\":\"Add\",\"value\":{\"components\":[]}}}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s CostModifier.codec
