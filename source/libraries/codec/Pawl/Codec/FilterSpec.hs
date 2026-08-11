{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.FilterSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | The `keyword` parameter is instantiated at 'Keyword.Keyword', the only
-- concrete instantiation anywhere in the pool, so HasKeyword's case pins a real
-- keyword's wire shape rather than a stand-in's.
toJson :: Filter.Filter Keyword.Keyword -> Value.Value
toJson = Filter.toJson Keyword.toJson

fromJson :: Value.Value -> Either Text.Text (Filter.Filter Keyword.Keyword)
fromJson = Filter.fromJson Keyword.fromJson

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Filter" $ do
  Spec.it s "HasCardType" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasCardType CardType.Creature)
      """ {"type":"HasCardType","value":{"type":"Creature"}} """
  Spec.it s "HasSupertype" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasSupertype Supertype.Basic)
      """ {"type":"HasSupertype","value":{"type":"Basic"}} """
  Spec.it s "HasColor" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasColor Color.Black)
      """ {"type":"HasColor","value":{"type":"Black"}} """
  Spec.it s "HasSubtype" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasSubtype Subtype.Mountain)
      """ {"type":"HasSubtype","value":{"type":"Mountain"}} """
  Spec.it s "HasKeyword" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasKeyword Keyword.Flying)
      """ {"type":"HasKeyword","value":{"type":"Flying"}} """
  Spec.it s "HasKeywordFamily" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasKeywordFamily KeywordFamily.Toxic)
      """ {"type":"HasKeywordFamily","value":{"type":"Toxic"}} """
    -- The two atoms must not share a wire shape: a card asking for the family
    -- and one asking for toxic 2 have to round-trip back to different filters.
    Spec.assertBool
      s
      (toJson (Filter.HasKeywordFamily KeywordFamily.Toxic) /= toJson (Filter.HasKeyword (Keyword.Toxic 2)))
      "the toxic family filter is not the toxic 2 filter"
  Spec.it s "PowerAtLeast" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.PowerAtLeast 4)
      """ {"type":"PowerAtLeast","value":4} """
  Spec.it s "PowerAtMost" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.PowerAtMost 2)
      """ {"type":"PowerAtMost","value":2} """
  Spec.it s "PowerLessThanSource" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.PowerLessThanSource
      """ {"type":"PowerLessThanSource"} """
  Spec.it s "PowerGreaterThanSource" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.PowerGreaterThanSource
      """ {"type":"PowerGreaterThanSource"} """
  Spec.it s "ControlledByDefendingPlayer" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.ControlledByDefendingPlayer
      """ {"type":"ControlledByDefendingPlayer"} """
  Spec.it s "ManaValueAtMost" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.ManaValueAtMost 2)
      """ {"type":"ManaValueAtMost","value":2} """
  Spec.it s "ControlledBy" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.ControlledBy PlayerRelation.Opponent)
      """ {"type":"ControlledBy","value":{"type":"Opponent"}} """
  -- CR 108.3, and the ONE arm this file must carry for: the decoder dispatches on
  -- a Text tag, so a missing arm there compiles and fails only when a card file
  -- is loaded.
  Spec.it s "OwnedBy" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.OwnedBy PlayerRelation.You)
      """ {"type":"OwnedBy","value":{"type":"You"}} """
  Spec.it s "IsPlayer" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.IsPlayer PlayerRelation.Opponent)
      """ {"type":"IsPlayer","value":{"type":"Opponent"}} """
  Spec.it s "IsSource" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsSource
      """ {"type":"IsSource"} """
  Spec.it s "IsAttacking" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsAttacking
      """ {"type":"IsAttacking"} """
  Spec.it s "IsBlocking" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsBlocking
      """ {"type":"IsBlocking"} """
  Spec.it s "AttackedThisTurn" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.AttackedThisTurn
      """ {"type":"AttackedThisTurn"} """
  Spec.it s "IsAttachedToCreature" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsAttachedToCreature
      """ {"type":"IsAttachedToCreature"} """
  Spec.it s "IsAttachedToPermanent" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsAttachedToPermanent
      """ {"type":"IsAttachedToPermanent"} """
  Spec.it s "IsAttachedToSource" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsAttachedToSource
      """ {"type":"IsAttachedToSource"} """
  Spec.it s "CanHostSubject" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.CanHostSubject
      """ {"type":"CanHostSubject"} """
  Spec.it s "IsToken" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsToken
      """ {"type":"IsToken"} """
  Spec.it s "IsTapped" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsTapped
      """ {"type":"IsTapped"} """
  Spec.it s "IsRingBearer" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.IsRingBearer
      """ {"type":"IsRingBearer"} """
  Spec.it s "HasNonManaActivatedAbility" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      Filter.HasNonManaActivatedAbility
      """ {"type":"HasNonManaActivatedAbility"} """
  Spec.it s "HasDesignation Renowned" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasDesignation Designation.Renowned)
      """ {"type":"HasDesignation","value":{"type":"Renowned"}} """
  Spec.it s "HasDesignation Monstrous" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasDesignation Designation.Monstrous)
      """ {"type":"HasDesignation","value":{"type":"Monstrous"}} """
  Spec.it s "HasDesignation Suspected" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasDesignation Designation.Suspected)
      """ {"type":"HasDesignation","value":{"type":"Suspected"}} """
  -- CR 122.1's presence read, the one Filter atom with a CounterKind payload.
  Spec.it s "HasCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.HasCounters CounterKind.PlusOnePlusOne)
      """ {"type":"HasCounters","value":{"type":"PlusOnePlusOne"}} """
  Spec.it s "And" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.And [Filter.HasCardType CardType.Land, Filter.HasSupertype Supertype.Basic])
      """ {"type":"And","value":[{"type":"HasCardType","value":{"type":"Land"}},{"type":"HasSupertype","value":{"type":"Basic"}}]} """
  Spec.it s "Or" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Enchantment])
      """ {"type":"Or","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"HasCardType","value":{"type":"Enchantment"}}]} """
  Spec.it s "Not" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Filter.Not (Filter.HasColor Color.Black))
      """ {"type":"Not","value":{"type":"HasColor","value":{"type":"Black"}}} """
  -- Nested And/Or/Not, exercising the recursion the per-constructor cases above
  -- do not.
  Spec.it s "nested And/Or/Not round-trips (P9)" $
    let doomBlade = Filter.Not (Filter.HasColor Color.Black)
        terror = Filter.And [Filter.Not (Filter.HasColor Color.Black), Filter.Not (Filter.HasCardType CardType.Artifact)]
        reprisal = Filter.PowerAtLeast 4
        basicLand = Filter.And [Filter.HasCardType CardType.Land, Filter.HasSupertype Supertype.Basic]
        angelicEdict = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Enchantment]
        controlled = Filter.ControlledBy PlayerRelation.Opponent
        bySubtype = Filter.HasSubtype Subtype.Wall
        isSource = Filter.IsSource
        ravenousRats = Filter.IsPlayer PlayerRelation.Opponent
        killShot = Filter.IsAttacking
        relentlessAssault = Filter.AttackedThisTurn
        crownOfTheAges = Filter.And [Filter.HasSubtype Subtype.Aura, Filter.IsAttachedToCreature]
        labyrinthOfSkophos = Filter.Or [Filter.IsAttacking, Filter.IsBlocking]
        auraGraftTarget = Filter.And [Filter.HasSubtype Subtype.Aura, Filter.IsAttachedToPermanent]
        auraGraftDestination = Filter.CanHostSubject
        roundTrip f v = Spec.assertEqWith s "preserved" (fromJson (toJson f)) (Right v)
     in mapM_
          (\f -> roundTrip f f)
          [ doomBlade,
            terror,
            reprisal,
            basicLand,
            angelicEdict,
            controlled,
            bySubtype,
            isSource,
            ravenousRats,
            killShot,
            relentlessAssault,
            crownOfTheAges,
            labyrinthOfSkophos,
            auraGraftTarget,
            auraGraftDestination
          ]
  Spec.describe s "optional (P9)" $ do
    Spec.it s "CR 702.29e's typecycling filter, present" $
      Common.assertFromJson s (Filter.optional Keyword.fromJson) "{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}" (Just (Filter.HasCardType CardType.Creature))
    Spec.it s "absent (JSON null)" $
      Common.assertFromJson s (Filter.optional Keyword.fromJson) "null" Nothing
