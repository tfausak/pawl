module Pawl.Codec.ManaRestrictionSpec where

import qualified Pawl.Codec.ManaRestriction as ManaRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaRestriction as ManaRestriction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaRestriction" $ do
  -- Mishra's Workshop: one key, and the absent one is a refusal.
  Spec.it s "only casts" $
    Common.assertCodec
      s
      ManaRestriction.codec
      (ManaRestriction.onlyCasts (Filter.HasCardType CardType.Artifact))
      " {\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  -- Omen Hawker: the other key, with CR 106.6 saying nothing about which
  -- abilities -- @And []@ being Pawl.Types.Filter's trivial predicate.
  Spec.it s "only activations" $
    Common.assertCodec
      s
      ManaRestriction.codec
      ManaRestriction.MkManaRestriction
        { ManaRestriction.casts = Nothing,
          ManaRestriction.activations = Just (Filter.And [])
        }
      " {\"activations\":{\"type\":\"And\",\"value\":[]}} "
  -- Dalakos, Crafter of Wonders' "spend this mana only to cast artifact spells
  -- or activate abilities of artifacts": both keys, with a predicate each.
  Spec.it s "both, with a filter each" $
    Common.assertCodec
      s
      ManaRestriction.codec
      ManaRestriction.MkManaRestriction
        { ManaRestriction.casts = Just (Filter.HasCardType CardType.Artifact),
          ManaRestriction.activations = Just (Filter.HasCardType CardType.Artifact)
        }
      " {\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}},\"activations\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaRestriction.codec
