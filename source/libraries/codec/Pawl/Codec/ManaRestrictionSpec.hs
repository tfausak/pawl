module Pawl.Codec.ManaRestrictionSpec where

import qualified Pawl.Codec.ManaRestriction as ManaRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.KeywordFamily as KeywordFamily
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
      ManaRestriction.none {ManaRestriction.activations = Just (Filter.And [])}
      " {\"activations\":{\"type\":\"And\",\"value\":[]}} "
  -- Dalakos, Crafter of Wonders' "spend this mana only to cast artifact spells
  -- or activate abilities of artifacts": both keys, with a predicate each.
  Spec.it s "both, with a filter each" $
    Common.assertCodec
      s
      ManaRestriction.codec
      ManaRestriction.none
        { ManaRestriction.casts = Just (Filter.HasCardType CardType.Artifact),
          ManaRestriction.activations = Just (Filter.HasCardType CardType.Artifact)
        }
      " {\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}},\"activations\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  -- Sorcerer Class's "only to cast an instant or sorcery spell or to gain a
  -- Class level": a cast key beside CR 716.2c's ability key.
  Spec.it s "casts and keyword activations" $
    Common.assertCodec
      s
      ManaRestriction.codec
      ManaRestriction.none
        { ManaRestriction.casts = Just (Filter.HasCardType CardType.Instant),
          ManaRestriction.keywordActivations = Just (KeywordDesignator.OfFamily KeywordFamily.ClassLevel)
        }
      " {\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Instant\"}},\"keywordActivations\":{\"type\":\"OfFamily\",\"value\":{\"type\":\"ClassLevel\"}}} "
  -- Overgrown Zealot's "spend this mana only to turn permanents face up": the
  -- special-action key alone, with CR 116.2b saying nothing about WHICH
  -- permanents.
  Spec.it s "only turning permanents face up" $
    Common.assertCodec
      s
      ManaRestriction.codec
      ManaRestriction.none {ManaRestriction.turnsFaceUp = Just (Filter.And [])}
      " {\"turnsFaceUp\":{\"type\":\"And\",\"value\":[]}} "
  -- Creeping Peeper's "spend this mana only to cast an enchantment spell, unlock
  -- a door, or turn a permanent face up": three kinds in one clause, which is
  -- what says unlocking and turning face up are two keys and not one.
  Spec.it s "a cast, an unlock and a turn-up at once" $
    Common.assertCodec
      s
      ManaRestriction.codec
      ManaRestriction.none
        { ManaRestriction.casts = Just (Filter.HasCardType CardType.Enchantment),
          ManaRestriction.unlocks = Just (Filter.And []),
          ManaRestriction.turnsFaceUp = Just (Filter.And [])
        }
      " {\"casts\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Enchantment\"}},\"unlocks\":{\"type\":\"And\",\"value\":[]},\"turnsFaceUp\":{\"type\":\"And\",\"value\":[]}} "
  -- Hydraulic Helper's "can't be spent to cast a nonartifact spell": the cast
  -- key under a prohibition, leaving every other payment open.
  Spec.it s "a prohibition" $
    Common.assertCodec
      s
      ManaRestriction.codec
      ManaRestriction.none
        { ManaRestriction.casts = Just (Filter.Not (Filter.HasCardType CardType.Artifact)),
          ManaRestriction.prohibits = True
        }
      " {\"casts\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}},\"prohibits\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaRestriction.codec
