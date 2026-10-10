module Pawl.Codec.CostSubjectSpec where

import qualified Pawl.Codec.CostSubject as CostSubject
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.ActivationCriteria as ActivationCriteria
import qualified Pawl.Types.CostSubject as CostSubject.Type
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.LoyaltyKind as LoyaltyKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostSubject" $ do
  Spec.it s "Spells" $
    Common.assertCodec
      s
      CostSubject.codec
      CostSubject.Type.Spells
      " {\"type\":\"Spells\"} "
  -- Every criterion absent writes an empty object: Heartstone's shape.
  Spec.it s "Activations, every ability" $
    Common.assertCodec
      s
      CostSubject.codec
      (CostSubject.Type.Activations (ActivationCriteria.MkActivationCriteria Nothing Nothing Nothing))
      " {\"type\":\"Activations\",\"value\":{}} "
  Spec.it s "Activations, every criterion" $
    Common.assertCodec
      s
      CostSubject.codec
      (CostSubject.Type.Activations (ActivationCriteria.MkActivationCriteria (Just (KeywordDesignator.OfFamily KeywordFamily.Equip)) (Just AbilityKind.NonManaAbility) (Just LoyaltyKind.LoyaltyAbility)))
      " {\"type\":\"Activations\",\"value\":{\"grantedBy\":{\"type\":\"OfFamily\",\"value\":{\"type\":\"Equip\"}},\"whichKind\":{\"type\":\"NonManaAbility\"},\"whichLoyalty\":{\"type\":\"LoyaltyAbility\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CostSubject.codec
