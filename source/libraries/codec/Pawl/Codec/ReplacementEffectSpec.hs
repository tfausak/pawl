module Pawl.Codec.ReplacementEffectSpec where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
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
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReplacementEffect" $ do
  Spec.it s "ZoneChangeR (Rest in Peace, Anyones)" $
    Common.assertCodec
      s
      codec
      ( ReplacementEffect.ZoneChangeR
          ( ZoneChangeR.MkZoneChangeR
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Anyones
                }
              Zone.Exile
          )
      )
      " {\"type\":\"ZoneChangeR\",\"value\":{\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"}},\"destination\":{\"type\":\"Exile\"}}} "
  -- The relation that distinguishes this from the shape above has to survive
  -- the wire too.
  Spec.it s "ZoneChangeR (Leyline of the Void, Opponents)" $
    Common.assertCodec
      s
      codec
      ( ReplacementEffect.ZoneChangeR
          ( ZoneChangeR.MkZoneChangeR
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Opponents
                }
              Zone.Exile
          )
      )
      " {\"type\":\"ZoneChangeR\",\"value\":{\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"},\"whoseObject\":{\"type\":\"Opponents\"}},\"destination\":{\"type\":\"Exile\"}}} "
  -- CR 614.1c: EntryR's pattern is a bare Filter, and "as this permanent
  -- enters" is Filter.IsSource. AsCopy pins the exception-free rewrite beside it.
  Spec.it s "EntryR (Clone, IsSource + AsCopy)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.IsSource (EntryRewrite.AsCopy (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) []))))
      " {\"type\":\"EntryR\",\"value\":{\"matching\":{\"type\":\"IsSource\"},\"rewrite\":{\"type\":\"AsCopy\",\"value\":{\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}}}} "
  -- CR 208.2b: Primal Plasma's ChoiceOf, carrying P/T and keywords.
  Spec.it s "EntryR (Primal Plasma, ChoiceOf)" $
    Common.assertCodec
      s
      codec
      ( ReplacementEffect.EntryR
          ( EntryR.MkEntryR
              Filter.IsSource
              ( EntryRewrite.ChoiceOf
                  [ EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty},
                    EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender}
                  ]
              )
          )
      )
      " {\"type\":\"EntryR\",\"value\":{\"matching\":{\"type\":\"IsSource\"},\"rewrite\":{\"type\":\"ChoiceOf\",\"value\":[{\"power\":3,\"toughness\":3},{\"power\":1,\"toughness\":6,\"keywords\":[{\"type\":\"Defender\"}]}]}}} "
  -- CR 614.1d / 616.1b: the other-objects form, whose Filter is a real
  -- characteristic predicate rather than an identity test.
  Spec.it s "EntryR (Gather Specimens, an opponent's creature + UnderSourceControl)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.EntryR (EntryR.MkEntryR (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.Opponent]) EntryRewrite.UnderSourceControl))
      " {\"type\":\"EntryR\",\"value\":{\"matching\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"Opponent\"}}]},\"rewrite\":{\"type\":\"UnderSourceControl\"}}} "
  -- Pattern and rewrite are both DATA, so both have to survive the trip.
  Spec.it s "DamageR (combat damage, prevented)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.DamageR (DamageR.MkDamageR DamagePattern.MkDamagePattern {DamagePattern.whichKind = Just DamageKind.Combat, DamagePattern.whatSource = Filter.And [], DamagePattern.whatRecipient = Nothing, DamagePattern.whichRecipient = Nothing} DamageRewrite.PreventAll Seq.empty))
      " {\"type\":\"DamageR\",\"value\":{\"matching\":{\"whichKind\":{\"type\":\"Combat\"}},\"rewrite\":{\"type\":\"PreventAll\"}}} "
  -- CR 614.15 / 614.1a: source-scoped, any kind, and a flat instead-amount
  -- rather than a prevention.
  Spec.it s "DamageR (this source's damage, set to a flat amount)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.DamageR (DamageR.MkDamageR DamagePattern.MkDamagePattern {DamagePattern.whichKind = Nothing, DamagePattern.whatSource = Filter.IsSource, DamagePattern.whatRecipient = Nothing, DamagePattern.whichRecipient = Nothing} (DamageRewrite.SetAmount 4) Seq.empty))
      " {\"type\":\"DamageR\",\"value\":{\"matching\":{\"whatSource\":{\"type\":\"IsSource\"}},\"rewrite\":{\"type\":\"SetAmount\",\"value\":4}}} "
  Spec.it s "DamageR (any source's damage, doubled)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.DamageR (DamageR.MkDamageR DamagePattern.MkDamagePattern {DamagePattern.whichKind = Nothing, DamagePattern.whatSource = Filter.And [], DamagePattern.whatRecipient = Nothing, DamagePattern.whichRecipient = Nothing} (DamageRewrite.Scale (Scaling.Multiply 2)) Seq.empty))
      " {\"type\":\"DamageR\",\"value\":{\"matching\":{},\"rewrite\":{\"type\":\"Scale\",\"value\":{\"type\":\"Multiply\",\"value\":2}}}} "
  -- CR 614.8: regeneration, DestructionR's sole producer today.
  Spec.it s "DestructionR (regenerate)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.DestructionR DestructionRewrite.Regenerate)
      " {\"type\":\"DestructionR\",\"value\":{\"type\":\"Regenerate\"}} "
  -- A fixed kind, a real filter, and CR 614.16's AddMore.
  Spec.it s "CounterR (Hardened Scales)" $
    Common.assertCodec
      s
      codec
      ( ReplacementEffect.CounterR
          ( CounterR.MkCounterR
              CounterPattern.MkCounterPattern
                { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
                  CounterPattern.byWhom = Nothing,
                  CounterPattern.whose = ControllerRelation.Yours,
                  CounterPattern.onWhat = Filter.HasCardType CardType.Creature,
                  CounterPattern.onWho = Nothing
                }
              (Scaling.AddMore 1)
          )
      )
      " {\"type\":\"CounterR\",\"value\":{\"matching\":{\"whichKind\":{\"type\":\"PlusOnePlusOne\"},\"whose\":{\"type\":\"Yours\"},\"onWhat\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"scaling\":{\"type\":\"AddMore\",\"value\":1}}} "
  -- whichKind = Nothing means any kind, never "no kind", and the trivial filter
  -- matches every permanent. An absent whichKind key is what that Nothing
  -- means.
  Spec.it s "CounterR (Doubling Season, whichKind omitted)" $
    Common.assertCodec
      s
      codec
      ( ReplacementEffect.CounterR
          ( CounterR.MkCounterR
              CounterPattern.MkCounterPattern
                { CounterPattern.whichKind = Nothing,
                  CounterPattern.byWhom = Nothing,
                  CounterPattern.whose = ControllerRelation.Yours,
                  CounterPattern.onWhat = Filter.And [],
                  CounterPattern.onWho = Nothing
                }
              (Scaling.Multiply 2)
          )
      )
      " {\"type\":\"CounterR\",\"value\":{\"matching\":{\"whose\":{\"type\":\"Yours\"},\"onWhat\":{\"type\":\"And\",\"value\":[]}},\"scaling\":{\"type\":\"Multiply\",\"value\":2}}} "
  -- Pattern and scaling are both DATA, so both have to survive the trip.
  Spec.it s "TokenR (Doubling Season, tokens)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.TokenR (TokenR.MkTokenR TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours} (Scaling.Multiply 2)))
      " {\"type\":\"TokenR\",\"value\":{\"matching\":{\"whose\":{\"type\":\"Yours\"}},\"scaling\":{\"type\":\"Multiply\",\"value\":2}}} "
  -- CR 614.1b: a skip carries a pattern and no rewrite, so the payload is the
  -- pattern itself rather than the usual two-element array. whosePhase =
  -- Nothing is the shape a card actually writes.
  Spec.it s "PhaseR (Eon Hub, symmetric skip)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep), PhasePattern.whosePhase = Nothing})
      " {\"type\":\"PhaseR\",\"value\":{\"whichPhase\":{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}}}}} "
  -- The baked player-scoped whosePhase = Just, which only Resolve's
  -- SkipNextPhase arm produces and no card authors (#437). The codec has to
  -- carry it either way.
  Spec.it s "PhaseR (Fatigue, player-scoped skip)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep), PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)})
      " {\"type\":\"PhaseR\",\"value\":{\"whichPhase\":{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"DrawStep\"}}},\"whosePhase\":1}} "
  -- The whole-phase selector, once Resolve has baked the player its resolution
  -- named -- the shape a bare Phase cannot spell (CR 500.1).
  Spec.it s "PhaseR (Stonehorn Dignitary, whole-phase skip)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.CombatPhase, PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)})
      " {\"type\":\"PhaseR\",\"value\":{\"whichPhase\":{\"type\":\"CombatPhase\"},\"whosePhase\":1}} "
  -- CR 614.1e / 702.37b: megamorph's "as this permanent is turned face up, put a
  -- +1/+1 counter on it". Minted by Pawl.Engine.Keyword rather than written by a
  -- card, and it round-trips anyway -- every arm of this type does.
  Spec.it s "TurnUpR (megamorph, CR 702.37b)" $
    Common.assertCodec
      s
      codec
      (ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR Filter.IsSource (TurnUpRewrite.WithCounters (WithCounters.MkWithCounters CounterKind.PlusOnePlusOne 1))))
      " {\"type\":\"TurnUpR\",\"value\":{\"matching\":{\"type\":\"IsSource\"},\"rewrite\":{\"type\":\"WithCounters\",\"value\":{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"amount\":1}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
  where
    -- CR 615.5: the DamageR arm carries riders, so the codec takes the effect
    -- codec the card boundary would pass it.
    codec = ReplacementEffect.codec (Effect.codec Card.codec)
