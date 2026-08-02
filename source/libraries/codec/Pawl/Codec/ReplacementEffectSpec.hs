module Pawl.Codec.ReplacementEffectSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.SourceRelation as SourceRelation
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReplacementEffect" $ do
  -- Rest in Peace's shape.
  Spec.it s "ZoneChangeR (Rest in Peace, Anyones)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      ( ReplacementEffect.ZoneChangeR
          ZoneChangePattern.MkZoneChangePattern
            { ZoneChangePattern.whenDestination = Zone.Graveyard,
              ZoneChangePattern.whichObject = ZoneChangeSubject.AnyObject,
              ZoneChangePattern.whoseObject = ControllerRelation.Anyones
            }
          Zone.Exile
      )
      "{\"type\":\"ZoneChangeR\",\"value\":[{\"whenDestination\":{\"type\":\"Graveyard\"},\"whichObject\":{\"type\":\"AnyObject\"},\"whoseObject\":{\"type\":\"Anyones\"}},{\"type\":\"Exile\"}]}"
  -- Leyline of the Void's shape: the relation that distinguishes it from
  -- Rest in Peace has to survive the wire too.
  Spec.it s "ZoneChangeR (Leyline of the Void, Opponents)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      ( ReplacementEffect.ZoneChangeR
          ZoneChangePattern.MkZoneChangePattern
            { ZoneChangePattern.whenDestination = Zone.Graveyard,
              ZoneChangePattern.whichObject = ZoneChangeSubject.AnyObject,
              ZoneChangePattern.whoseObject = ControllerRelation.Opponents
            }
          Zone.Exile
      )
      "{\"type\":\"ZoneChangeR\",\"value\":[{\"whenDestination\":{\"type\":\"Graveyard\"},\"whichObject\":{\"type\":\"AnyObject\"},\"whoseObject\":{\"type\":\"Opponents\"}},{\"type\":\"Exile\"}]}"
  -- CR 614.1c: EntryR's pattern is a bare Filter, and "as [THIS PERMANENT]
  -- enters" is Filter.IsSource. AsCopy pins the payload-free rewrite beside it.
  Spec.it s "EntryR (Clone, IsSource + AsCopy)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.EntryR Filter.IsSource EntryRewrite.AsCopy)
      "{\"type\":\"EntryR\",\"value\":[{\"type\":\"IsSource\"},{\"type\":\"AsCopy\"}]}"
  -- CR 208.2b: Primal Plasma's ChoiceOf, carrying P/T and keywords.
  Spec.it s "EntryR (Primal Plasma, ChoiceOf)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      ( ReplacementEffect.EntryR
          Filter.IsSource
          ( EntryRewrite.ChoiceOf
              [ EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty},
                EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender}
              ]
          )
      )
      "{\"type\":\"EntryR\",\"value\":[{\"type\":\"IsSource\"},{\"type\":\"ChoiceOf\",\"value\":[{\"power\":3,\"toughness\":3,\"keywords\":[]},{\"power\":1,\"toughness\":6,\"keywords\":[{\"type\":\"Defender\"}]}]}]}"
  -- CR 614.1d / 616.1b: Gather Specimens -- the other-objects form, whose Filter
  -- is a real characteristic predicate rather than an identity test.
  Spec.it s "EntryR (Gather Specimens, an opponent's creature + UnderSourceControl)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      ( ReplacementEffect.EntryR
          (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.Opponent])
          EntryRewrite.UnderSourceControl
      )
      "{\"type\":\"EntryR\",\"value\":[{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"Opponent\"}}]},{\"type\":\"UnderSourceControl\"}]}"
  -- Pattern and rewrite are both DATA, so both have to survive the trip.
  Spec.it s "DamageR (combat damage, prevented)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.DamageR DamagePattern.MkDamagePattern {DamagePattern.whichKind = Just DamageKind.Combat, DamagePattern.whichSource = SourceRelation.AnySource} DamageRewrite.PreventAll)
      "{\"type\":\"DamageR\",\"value\":[{\"whichKind\":{\"type\":\"Combat\"},\"whichSource\":{\"type\":\"AnySource\"}},{\"type\":\"PreventAll\"}]}"
  -- CR 614.15 / 614.1a: Galvanic Blast's metalcraft clause -- source-scoped, any
  -- kind, and a flat instead-amount rather than a prevention.
  Spec.it s "DamageR (this source's damage, set to a flat amount)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.DamageR DamagePattern.MkDamagePattern {DamagePattern.whichKind = Nothing, DamagePattern.whichSource = SourceRelation.TheSource} (DamageRewrite.SetAmount 4))
      "{\"type\":\"DamageR\",\"value\":[{\"whichKind\":null,\"whichSource\":{\"type\":\"TheSource\"}},{\"type\":\"SetAmount\",\"value\":4}]}"
  -- Furnace of Rath's "it deals double that damage ... instead".
  Spec.it s "DamageR (any source's damage, doubled)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.DamageR DamagePattern.MkDamagePattern {DamagePattern.whichKind = Nothing, DamagePattern.whichSource = SourceRelation.AnySource} (DamageRewrite.Scale (Scaling.Multiply 2)))
      "{\"type\":\"DamageR\",\"value\":[{\"whichKind\":null,\"whichSource\":{\"type\":\"AnySource\"}},{\"type\":\"Scale\",\"value\":{\"type\":\"Multiply\",\"value\":2}}]}"
  -- CR 614.8: regeneration, DestructionR's sole producer today.
  Spec.it s "DestructionR (regenerate)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.DestructionR DestructionRewrite.Regenerate)
      "{\"type\":\"DestructionR\",\"value\":{\"type\":\"Regenerate\"}}"
  -- Hardened Scales: a fixed kind, a real filter, and CR 614.16's AddMore.
  Spec.it s "CounterR (Hardened Scales)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      ( ReplacementEffect.CounterR
          CounterPattern.MkCounterPattern
            { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
              CounterPattern.whose = ControllerRelation.Yours,
              CounterPattern.onWhat = Filter.HasCardType CardType.Creature
            }
          (Scaling.AddMore 1)
      )
      "{\"type\":\"CounterR\",\"value\":[{\"whichKind\":{\"type\":\"PlusOnePlusOne\"},\"whose\":{\"type\":\"Yours\"},\"onWhat\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},{\"type\":\"AddMore\",\"value\":1}]}"
  -- Doubling Season: whichKind = Nothing is an explicit JSON null (any kind),
  -- never "no kind", and the trivial filter matches every permanent.
  Spec.it s "CounterR (Doubling Season, explicit JSON null)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      ( ReplacementEffect.CounterR
          CounterPattern.MkCounterPattern
            { CounterPattern.whichKind = Nothing,
              CounterPattern.whose = ControllerRelation.Yours,
              CounterPattern.onWhat = Filter.And []
            }
          (Scaling.Multiply 2)
      )
      "{\"type\":\"CounterR\",\"value\":[{\"whichKind\":null,\"whose\":{\"type\":\"Yours\"},\"onWhat\":{\"type\":\"And\",\"value\":[]}},{\"type\":\"Multiply\",\"value\":2}]}"
  -- Pattern and scaling are both DATA, so both have to survive the trip.
  Spec.it s "TokenR (Doubling Season, tokens)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.TokenR TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours} (Scaling.Multiply 2))
      "{\"type\":\"TokenR\",\"value\":[{\"whose\":{\"type\":\"Yours\"}},{\"type\":\"Multiply\",\"value\":2}]}"
  -- CR 614.1b: a skip carries a pattern and no rewrite, so the payload is the
  -- pattern itself rather than the usual two-element array.
  --
  -- Eon Hub's symmetric whosePhase = Nothing, the shape a card actually
  -- writes.
  Spec.it s "PhaseR (Eon Hub, symmetric skip)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep), PhasePattern.whosePhase = Nothing})
      "{\"type\":\"PhaseR\",\"value\":{\"whichPhase\":{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}}},\"whosePhase\":null}}"
  -- Fatigue's baked player-scoped whosePhase = Just, the shape only Resolve's
  -- SkipNextPhase arm produces (never authored on a card, #437) -- covered
  -- here for the same reason SetController's PlayerId is: the codec has to
  -- carry it either way.
  Spec.it s "PhaseR (Fatigue, player-scoped skip)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep), PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)})
      "{\"type\":\"PhaseR\",\"value\":{\"whichPhase\":{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"DrawStep\"}}},\"whosePhase\":1}}"
  -- Stonehorn Dignitary's whole-phase selector, once Resolve has baked the
  -- player its resolution named -- the shape a bare Phase cannot spell
  -- (CR 500.1 defines "phase" as the five-part division of a turn, distinct
  -- from a phase's steps).
  Spec.it s "PhaseR (Stonehorn Dignitary, whole-phase skip)" $
    Common.assertJsonCodec
      s
      ReplacementEffect.toJson
      ReplacementEffect.fromJson
      (ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.CombatPhase, PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)})
      "{\"type\":\"PhaseR\",\"value\":{\"whichPhase\":{\"type\":\"CombatPhase\"},\"whosePhase\":1}}"
