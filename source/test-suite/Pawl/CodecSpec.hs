-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import Pawl.Codec.Binding (bindingToJson, jsonToBinding)
import Pawl.Codec.Card (cardToJson, jsonToCard)
import qualified Pawl.Codec.Condition as Condition
import Pawl.Codec.DelayedTrigger (delayedTriggerToJson, jsonToDelayedTrigger)
import Pawl.Codec.Effect (effectToJson, jsonToEffect)
import qualified Pawl.Codec.EntryRiders as EntryRiders
import Pawl.Codec.GameEvent (gameEventToJson, jsonToGameEvent)
import qualified Pawl.Codec.Json as J
import Pawl.Codec.Modal (jsonToModal, modalToJson)
import Pawl.Codec.Mode (jsonToMode, modeToJson)
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.Codec.Optionality as Optionality
import Pawl.Codec.Printing (jsonToPrinting, printingToJson)
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import Pawl.Codec.TriggeredAbility (jsonToTriggeredAbility, triggeredAbilityToJson)
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
-- Aliased Filter.Type, not Filter, for consistency with FilterSpec: the
-- evaluator module Pawl.Engine.Filter is not imported here today, but the alias
-- convention is fixed project-wide so a later import never collides.

import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

roundTrip :: (Applicative m, Eq a, Show a) => Spec.Spec m n -> String -> (a -> Value.Value) -> (Value.Value -> Either Text a) -> a -> m ()
roundTrip s label enc dec x = Spec.assertEqWith s label (dec (enc x)) (Right x)

-- How many elements a tagged effect's array payload holds -- what an ELIDED
-- optional trailing element is asserted by. -1 for a payload that is not an
-- array, so a wrong shape fails loudly rather than matching a real length.
payloadLength :: Value.Value -> Int
payloadLength value = case J.tag value of
  Right (_, Just (Value.Array (Array.MkArray xs))) -> length xs
  _ -> -1

-- The `optionality` key of an encoded Mode, or Nothing when it was omitted (CR
-- 603.5's Mandatory default).
optionalityKey :: Value.Value -> Maybe Value.Value
optionalityKey value = case J.asObject value of
  Right ps -> J.optField (Text.pack "optionality") ps
  Left _ -> Nothing

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Codec" $ do
  -- Keyword's own per-constructor coverage, including the payload-bearing
  -- Landwalk/Cycling/Flashback/Entwine/Poisonous/Toxic arms, lives in
  -- Pawl.Codec.KeywordSpec now. CounterKind's own per-constructor coverage,
  -- including the keyword-carrying arm, lives in Pawl.Codec.CounterKindSpec.
  -- Quantity's own per-constructor coverage (including the tagged-object shape
  -- and the InSlot-nested-under-Plus payload check) lives in
  -- Pawl.Codec.QuantitySpec now; ManaCost's lives in Pawl.Codec.ManaCostSpec;
  -- Power's (and Toughness's) delegating codec lives in Pawl.Codec.PowerSpec
  -- and Pawl.Codec.ToughnessSpec. Modification's own per-constructor coverage
  -- lives in Pawl.Codec.ModificationSpec now.
  Spec.describe s "effect" $ do
    Spec.it s "DealDamage" $
      roundTrip s "e1" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 3))
    -- ModifyTarget takes the same untagged ObjectRef Destroy and Untap do,
    -- so both arms have to survive the trip: Giant Growth's slot and
    -- Trumpet Blast's filter-selected set.
    Spec.it s "ModifyTarget round-trips both ObjectRef arms" $ do
      roundTrip s "e2" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))))
      roundTrip s "e2b" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.EachMatching Filter.Type.IsAttacking))
    Spec.it s "AddMana" $
      roundTrip s "e3" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green)))
    Spec.it s "AddMana of any color" $
      roundTrip s "e3b" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddMana ManaProduction.AnyColor)
    Spec.it s "ExileAllGraveyards" $
      roundTrip s "e4" (effectToJson cardToJson) (jsonToEffect jsonToCard) Effect.ExileAllGraveyards
    Spec.it s "Proliferate" $
      roundTrip s "e4b" (effectToJson cardToJson) (jsonToEffect jsonToCard) Effect.Proliferate
    -- Both shapes in the pool: Aggravated Assault's pair and Full
    -- Throttle's two combat phases with no main phase between them.
    Spec.it s "AddPhases round-trips the pair" $
      roundTrip s "e4c" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain])
    Spec.it s "AddPhases round-trips a repeated phase" $
      roundTrip s "e4c1" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat])
    -- Untap takes the same untagged ObjectRef Destroy does, so both arms
    -- have to survive the trip: Act of Treason's slot and Aggravated
    -- Assault's swept set.
    Spec.it s "Untap round-trips both ObjectRef arms" $ do
      roundTrip s "e4d" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      roundTrip s "e4e" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Untap (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)))
    -- GainControl's own two arms: Act of Treason's slot and Aura Thief's
    -- "all enchantments". Its Duration is what tells the two cards apart on
    -- the wire, so both durations ride along.
    Spec.it s "GainControl round-trips both ObjectRef arms" $ do
      roundTrip s "e4f" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainControl Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      roundTrip s "e4g" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainControl Duration.Indefinite (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Enchantment)))
    -- CR 701.26a's Tap is Untap's mirror and shares its wire shape, so the
    -- two must not collapse into one tag: Dream's Grip prints both modes
    -- on one card and a decoder that confused them would silently swap
    -- them.
    Spec.it s "Tap round-trips both ObjectRef arms, and is not Untap" $ do
      roundTrip s "e4f" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      roundTrip s "e4g" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Tap (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)))
      Spec.assertBool
        s
        ( effectToJson cardToJson (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
            /= effectToJson cardToJson (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
        )
        "Tap and Untap of the same slot encode differently"
    -- ObjectRef's own untagged shape (a slot is a bare string, a swept set is
    -- an object) is Pawl.Codec.ObjectRefSpec's business now; the third case
    -- here keeps Destroy's own coverage of the EachMatching arm.
    Spec.it s "Destroy carries its CR 701.19c rider both ways" $ do
      roundTrip s "e5a" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.Regenerable Nothing)
      roundTrip s "e5b" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.CantBeRegenerated Nothing)
      roundTrip s "e5c" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing)
    -- Bane of Progress' "destroyed this way": the third element is the slot
    -- the sweep binds its count into, and it is ELIDED when absent -- so
    -- every Destroy already on disk keeps its two-element payload.
    Spec.it s "Destroy's bound-count slot round-trips and is elided when absent" $ do
      let counting = Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed")))
          plain = Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Artifact)) Regenerability.Regenerable Nothing
      roundTrip s "e5d" (effectToJson cardToJson) (jsonToEffect jsonToCard) counting
      Spec.assertEqWith
        s
        "a Destroy that binds nothing writes two elements"
        (payloadLength (effectToJson cardToJson plain))
        2
      Spec.assertEqWith
        s
        "and one that binds a count writes three"
        (payloadLength (effectToJson cardToJson counting))
        3
    Spec.it s "ExileHandThenDraw" $
      roundTrip s "e-powder" (effectToJson cardToJson) (jsonToEffect jsonToCard) Effect.ExileHandThenDraw
    Spec.it s "PlayerSacrifices" $
      roundTrip s "e6" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.PlayerSacrifices (SlotName.MkSlotName (Text.pack "t")) (Filter.Type.HasCardType CardType.Creature) (Quantity.Literal 1))
    -- CR 701.3: the destination filter travels in the payload, which is what
    -- distinguishes this arm's wire format from Attach's bare slot.
    Spec.it s "AttachTarget" $
      roundTrip s "e-crown" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.AttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.Type.HasCardType CardType.Creature))
    Spec.it s "Sacrifice round-trips" $
      roundTrip s "e5" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self")))
    Spec.it s "PutCounters effect round-trips through the codec" $
      let effect = Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "creature"))
       in Spec.assertEqWith s "round-trip" (jsonToEffect jsonToCard (effectToJson cardToJson effect)) (Right effect)
    Spec.it s "AffectPlayers round-trips" $
      roundTrip
        s
        "e6"
        (effectToJson cardToJson)
        (jsonToEffect jsonToCard)
        (Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells)
    -- CR 614.10a: Fatigue's slot read, plus the self-scoped arm Avizoa's
    -- "you skip your next untap step" would write -- and Stonehorn
    -- Dignitary's whole-phase selector, the arm a Phase alone cannot spell
    -- (CR 500.1).
    Spec.it s "SkipNextPhase" $ do
      roundTrip s "skip slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep)))
      roundTrip s "skip you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.SkipNextPhase (PlayerRef.Relative PlayerRelation.You) (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)))
      roundTrip s "skip a whole phase" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PhaseSelector.CombatPhase)
    -- CR 500.7: Time Warp's slot read, whose skip set is empty, plus Savor
    -- the Moment's self-scoped arm carrying CR 500.11's skip of one step of
    -- the turn it creates. The many-selector case has no producer -- no
    -- printed card skips two windows of the turn it makes -- but the field
    -- is a Set, so the wire format has to survive more than one.
    Spec.it s "TakeExtraTurn" $ do
      roundTrip s "extra turn slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.TakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Set.empty)
      roundTrip s "extra turn you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.TakeExtraTurn (PlayerRef.Relative PlayerRelation.You) Set.empty)
      roundTrip
        s
        "extra turn skipping its own untap step"
        (effectToJson cardToJson)
        (jsonToEffect jsonToCard)
        (Effect.TakeExtraTurn (PlayerRef.Relative PlayerRelation.You) (Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap))))
      roundTrip
        s
        "extra turn skipping a step and a whole phase"
        (effectToJson cardToJson)
        (jsonToEffect jsonToCard)
        (Effect.TakeExtraTurn PlayerRef.EachPlayer (Set.fromList [PhaseSelector.Step (Phase.Beginning BeginningStep.Untap), PhaseSelector.CombatPhase]))
    -- Every PlayerRef shape the opcode accepts: the self-scoped one every
    -- card in the pool uses, and the slot read CR 702.70a's "that player"
    -- needs.
    Spec.it s "GainPlayerCounters" $ do
      roundTrip s "gpc" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2))
      roundTrip s "gpc slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainPlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) PlayerCounterKind.Poison (Quantity.Literal 3))
    -- Both of Draw's proven PlayerRef shapes: Divination's controller draw
    -- and Ancestral Recall's targeted one (#272).
    Spec.it s "Draw" $ do
      roundTrip s "draw" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
      roundTrip s "draw slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Draw (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3))
    -- Sign in Blood's targeted loss, and the `Relative You` arm that no
    -- card in the pool uses yet -- the codec accepts every PlayerRef either way.
    Spec.it s "LoseLife" $ do
      roundTrip s "lose slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.LoseLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2))
      roundTrip s "lose you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.LoseLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
    -- Soul Warden's "you gain 1 life", plus the slot arm no card uses
    -- yet -- the same coverage LoseLife above gets, on the sibling opcode.
    Spec.it s "GainLife" $ do
      roundTrip s "gain you" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
      roundTrip s "gain slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.GainLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2))
    Spec.it s "CreateEmblem" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      roundTrip s "emblem" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.CreateEmblem (Printing.card piker))
    Spec.it s "BecomeMonarch" $
      roundTrip s "e" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.BecomeMonarch MonarchTarget.TheController)
    Spec.it s "ExileUntilMonarch" $
      roundTrip s "eum" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ExileUntilMonarch (SlotName.MkSlotName (Text.pack "target")))
    Spec.it s "PlaySubgame round-trips" $
      let e = Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser"))
       in Spec.assertEqWith s "PlaySubgame round-trips" (jsonToEffect jsonToCard (effectToJson cardToJson e)) (Right e)
  -- Duration's own per-constructor coverage lives in Pawl.Codec.DurationSpec
  -- now; Condition's own per-constructor coverage lives in
  -- Pawl.Codec.ConditionSpec.
  -- PlayerEffect's own per-constructor coverage, including the Edgewalker
  -- typed-mana ReduceSpellCost, lives in Pawl.Codec.PlayerEffectSpec now.
  Spec.describe s "player effects (P7)" $ do
    -- StaticAbility's own per-constructor coverage, including the CR 613.6
    -- empty-modifications-array rejection, lives in Pawl.Codec.StaticAbilitySpec
    -- now. PlayerStaticAbility's own per-constructor coverage lives in
    -- Pawl.Codec.PlayerStaticAbilitySpec now.
    Spec.it s "a Card carrying player abilities round-trips" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
          c = base {CardT.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]}
      roundTrip s "card" cardToJson jsonToCard c
    -- Byte-stability: an empty list must not appear in the rendered JSON,
    -- or every committed card file changes. The same posture
    -- colorIndicator and delayedAbilities already take.
    Spec.it s "an empty playerAbilities list is omitted from the JSON" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
      Spec.assertEqWith s "the fixture really has none" (CardT.playerAbilities base) []
      case J.asObject (cardToJson base) of
        Left err -> Spec.assertFailure s (Text.unpack err)
        Right pairs -> Spec.assertBool s (notElem (Text.pack "playerAbilities") (fmap fst pairs)) "key absent"
    Spec.it s "a Card carrying a CR 103.5b mulligan action round-trips" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
          c = base {CardT.mulliganAction = [Effect.ExileHandThenDraw]}
      roundTrip s "card" cardToJson jsonToCard c
    -- Byte-stability: an empty list must not appear in the rendered JSON,
    -- or every committed card file changes. The same posture
    -- playerAbilities and additionalCosts already take.
    Spec.it s "an empty mulliganAction list is omitted from the JSON" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
      Spec.assertEqWith s "the fixture really has none" (CardT.mulliganAction base) []
      case J.asObject (cardToJson base) of
        Left err -> Spec.assertFailure s (Text.unpack err)
        Right pairs -> Spec.assertBool s (notElem (Text.pack "mulliganAction") (fmap fst pairs)) "key absent"
    Spec.it s "a Card carrying a CR 103.6 opening-hand action round-trips" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
          c = base {CardT.openingHandAction = [Effect.MoveToZone Binding.triggerSource Zone.Battlefield EntryRiders.defaultValue Nothing]}
      roundTrip s "card" cardToJson jsonToCard c
    Spec.it s "an empty openingHandAction list is omitted from the JSON" $ do
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = Printing.card bloodMoon
      Spec.assertEqWith s "the fixture really has none" (CardT.openingHandAction base) []
      case J.asObject (cardToJson base) of
        Left err -> Spec.assertFailure s (Text.unpack err)
        Right pairs -> Spec.assertBool s (notElem (Text.pack "openingHandAction") (fmap fst pairs)) "key absent"
  -- TargetSpec's own per-constructor coverage (a bare pool, a filtered pool,
  -- and the Not IsSource conjunct that carries CR 601.2c's "another", #163)
  -- lives in Pawl.Codec.TargetSpecSpec now; Filter's own per-constructor and
  -- nested-And/Or/Not coverage lives in Pawl.Codec.FilterSpec.
  Spec.describe s "records" $ do
    -- EntryRewrite's own per-constructor coverage lives in
    -- Pawl.Codec.EntryRewriteSpec now.
    -- CR 606.3's record.
    Spec.it s "GameEvent (loyalty ability activated)" $
      roundTrip s "loyalty-activated" gameEventToJson jsonToGameEvent (GameEvent.LoyaltyAbilityActivated (ObjectId.MkObjectId 7))
    -- Cost's own per-constructor coverage, including the CR 118.6 absent-mana
    -- and CR 118.5a {0} behavior, lives in Pawl.Codec.CostSpec now.
    Spec.describe s "cost (P8)" $ do
      Spec.it s "a Card carrying an additional cost round-trips" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
            c = base {CardT.additionalCosts = [CostComponent.Sacrifice 1 (Filter.Type.HasCardType CardType.Creature)]}
        roundTrip s "card" cardToJson jsonToCard c
      -- Byte-stability: an empty list must not appear in the rendered JSON,
      -- or every committed card file changes. The posture colorIndicator,
      -- delayedAbilities and playerAbilities already take.
      Spec.it s "an empty additionalCosts list is omitted from the JSON" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
        Spec.assertEqWith s "the fixture really has none" (CardT.additionalCosts base) []
        case J.asObject (cardToJson base) of
          Left err -> Spec.assertFailure s (Text.unpack err)
          Right pairs -> Spec.assertBool s (notElem (Text.pack "additionalCosts") (fmap fst pairs)) "key absent"
      Spec.it s "a Card carrying an alternative cost round-trips" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
            alt =
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                  Cost.Type.components = [CostComponent.Sacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain)]
                }
            c = base {CardT.alternativeCosts = [alt]}
        roundTrip s "card" cardToJson jsonToCard c
      Spec.it s "an empty alternativeCosts list is omitted from the JSON" $ do
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        let base = Printing.card lightningBolt
        Spec.assertEqWith s "the fixture really has none" (CardT.alternativeCosts base) []
        case J.asObject (cardToJson base) of
          Left err -> Spec.assertFailure s (Text.unpack err)
          Right pairs -> Spec.assertBool s (notElem (Text.pack "alternativeCosts") (fmap fst pairs)) "key absent"
  -- ReplacementEffect's own per-constructor coverage -- including the
  -- ZoneChangeR Opponents/Anyones distinction, the CounterR/DamageR/TokenR
  -- "pattern and scaling/rewrite are data" shapes, the CounterR explicit
  -- JSON null, and the PhaseR whosePhase axis -- lives in
  -- Pawl.Codec.ReplacementEffectSpec now.
  Spec.describe s "modal" $ do
    Spec.it s "Modal round-trips" $
      roundTrip
        s
        "modal"
        (modalToJson cardToJson)
        (jsonToModal jsonToCard)
        ( Modal.MkModal
            ( Seq.fromList
                [ Mode.MkMode
                    (Seq.fromList [Effect.DealDamage (SlotName.MkSlotName (Text.pack "creature")) (Quantity.Literal 1)])
                    (Map.singleton (SlotName.MkSlotName (Text.pack "creature")) (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
                    Optionality.Mandatory
                ]
            )
            (ModeSelection.ChooseExactly 1)
        )
    -- CR 603.5: an Optional mode is what a printed "may" encodes to, and
    -- the key is emitted only for that value.
    Spec.it s "an Optional mode round-trips, and says so in the JSON" $ do
      let m = Mode.MkMode Seq.empty Map.empty Optionality.Optional
      roundTrip s "optional mode" (modeToJson cardToJson) (jsonToMode jsonToCard) m
      Spec.assertEqWith
        s
        "the optionality key is present"
        (optionalityKey (modeToJson cardToJson m))
        (Just (Optionality.toJson Optionality.Optional))
    -- The byte-identity guarantee for every card file that prints no
    -- "may": a Mandatory mode emits no key, and a mode with no key decodes
    -- back to Mandatory. The counterability precedent.
    Spec.it s "a Mandatory mode omits the key, and an omitted key decodes to Mandatory" $ do
      let m = Mode.MkMode Seq.empty Map.empty Optionality.Mandatory
      Spec.assertEqWith s "no optionality key" (optionalityKey (modeToJson cardToJson m)) Nothing
      Spec.assertEqWith s "decodes to Mandatory" (jsonToMode jsonToCard (modeToJson cardToJson m)) (Right m)
    Spec.it s "empty modal is a decode error" $
      Spec.assertBool
        s
        ( either
            (const True)
            (const False)
            (jsonToModal jsonToCard (J.jObject [(Text.pack "modes", J.jArray []), (Text.pack "selection", ModeSelection.toJson (ModeSelection.ChooseExactly 1))]))
        )
        "left"
  Spec.describe s "honesty round-trip over allPrintings" $ do
    Spec.it s "P1: jsonToPrinting . printingToJson == Right" $ do
      ps <- S.allPrintings s
      mapM_ (\p -> Spec.assertEqWith s (show (CardT.name (Printing.card p))) (jsonToPrinting (printingToJson p)) (Right p)) ps
    Spec.it s "P2: through text" $ do
      ps <- S.allPrintings s
      mapM_
        (\p -> Spec.assertEqWith s (show (CardT.name (Printing.card p))) (J.parse (J.render (printingToJson p)) >>= jsonToPrinting) (Right p))
        ps
    Spec.it s "M4e Cancel loads as a single Counter effect targeting a spell" $ do
      cancel <- S.printingOf s registry "Cancel"
      let card = Printing.card cancel
      Spec.assertEqWith
        s
        "effects"
        (Card.allEffects card)
        [Effect.Counter (SlotName.MkSlotName (Text.pack "spell"))]
      Spec.assertEqWith
        s
        "target spec"
        (Card.allTargetSpecs card)
        (Map.singleton (SlotName.MkSlotName (Text.pack "spell")) (TargetSpec.MkTargetSpec Pool.Spells Nothing))
    -- The key is omitted when Counterable, so this pins BOTH directions of
    -- that default: the one card that prints the clause decodes as
    -- CantBeCountered, and a card that says nothing decodes as Counterable
    -- rather than as whatever a missing key might otherwise become.
    Spec.it s "CR 113.6g counterability decodes from the card, and defaults when the key is absent" $ do
      rendingVolley <- S.printingOf s registry "Rending Volley"
      cancel <- S.printingOf s registry "Cancel"
      Spec.assertEqWith
        s
        "Rending Volley says it"
        (CardT.counterability (Printing.card rendingVolley))
        Counterability.CantBeCountered
      Spec.assertEqWith
        s
        "Cancel does not, and its file has no counterability key"
        (CardT.counterability (Printing.card cancel))
        Counterability.Counterable
  Spec.describe s "P4 runtime types" $ do
    -- A real permanent, not a projection of a nonexistent object: Typhoid
    -- Rats (1/1 deathtouch) populates keywords, colors, power, toughness,
    -- cardTypes and subtypes all at once, so a swapped field or a wrong
    -- JSON key would fail this round-trip instead of surviving it on an
    -- all-Nothing/all-empty value.
    Spec.it s "GameEvent.Moved round-trips with its snapshot" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      let (ratId, gs) = S.addCreature typhoidRats S.alice (Setup.emptyGame S.bothPlayers)
          zc = ZoneChange.MkZoneChange ratId ratId Zone.Battlefield Zone.Graveyard
          snapshot = Projection.project ratId gs
      roundTrip s "moved" gameEventToJson jsonToGameEvent (GameEvent.Moved zc snapshot)
    -- The snapshot's keywords are counted per keyword (CR 702.164b), so a
    -- COUNT has to survive the wire and not just a membership: the
    -- array-with-repeats encoding is what carries it. A Set-shaped encoder
    -- would pass every OTHER round-trip test in this group and still halve
    -- the Stalker's total toxic value on replay, which is why the count is
    -- asserted here before the round-trip rather than left to Eq alone.
    Spec.it s "a doubled keyword survives the Moved snapshot round-trip" $ do
      stalker <- S.printingOf s registry "Branchblight Stalker"
      let (oid, gs0) = S.addCreature stalker S.alice (Setup.emptyGame S.bothPlayers)
          grant ts = S.withEffectAt oid (Timestamp.MkTimestamp ts) (Modification.GainKeyword (Keyword.Toxic 1))
          snapshot = Projection.project oid (grant 101 (grant 100 gs0))
          zc = ZoneChange.MkZoneChange oid oid Zone.Battlefield Zone.Graveyard
      Spec.assertEqWith s "the fixture really does carry toxic 1 twice" (Map.lookup (Keyword.Toxic 1) (PC.keywords snapshot)) (Just 2)
      roundTrip s "moved" gameEventToJson jsonToGameEvent (GameEvent.Moved zc snapshot)
    Spec.it s "GameEvent.DamageDealt round-trips" $
      roundTrip
        s
        "damage"
        gameEventToJson
        jsonToGameEvent
        -- A NONZERO toxic value, so the CR 702.164b rider is round-tripped
        -- rather than defaulted past.
        (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlayer S.bob) 2 True False 3 DamageKind.Combat))
    -- CR 120.3c's recipient tag is a different arm of Recipient from the one
    -- above, and a CR 608.2i record the codec cannot write is one no replay can
    -- read back.
    Spec.it s "GameEvent.DamageDealt to a planeswalker round-trips" $
      roundTrip
        s
        "damage"
        gameEventToJson
        jsonToGameEvent
        (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlaneswalker (ObjectId.MkObjectId 2)) 3 False False 0 DamageKind.Noncombat))
    Spec.it s "GameEvent.StepBegan round-trips" $
      roundTrip s "step" gameEventToJson jsonToGameEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
    Spec.it s "GameEvent.SpellCast round-trips" $
      roundTrip s "ev" gameEventToJson jsonToGameEvent (GameEvent.SpellCast S.alice)
    Spec.it s "GameEvent.BecameMonarch" $
      roundTrip s "bm" gameEventToJson jsonToGameEvent (GameEvent.BecameMonarch S.alice)
    -- Keyword.Cycling's own round trip, with and without a typecycling
    -- filter, lives in Pawl.Codec.KeywordSpec now.
    -- CR 701.9a's event, carrying the incarnation the discarded card
    -- became. Both causes, because the cause is what tells a cycle from an
    -- ordinary discard (CR 702.29c) and a trip that flattened it would
    -- leave the two indistinguishable.
    Spec.it s "GameEvent.Discarded round-trips with either cause" $ do
      roundTrip s "disc" gameEventToJson jsonToGameEvent (GameEvent.Discarded S.alice (ObjectId.MkObjectId 7) DiscardCause.Ordinary)
      roundTrip s "cyc" gameEventToJson jsonToGameEvent (GameEvent.Discarded S.bob (ObjectId.MkObjectId 7) DiscardCause.ToPayCyclingCost)
    -- CR 701.20a: the reveal's whole payload IS the snapshot, so it is the
    -- one GameEvent whose round-trip failing would silently erase what the
    -- players were shown rather than merely mislabel it. Typhoid Rats for
    -- the reason the Moved case gives -- every snapshot field populated.
    Spec.it s "GameEvent.Revealed round-trips with its snapshot" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      let (ratId, gs) = S.addLibraryCard typhoidRats S.alice (Setup.emptyGame S.bothPlayers)
      roundTrip s "revealed" gameEventToJson jsonToGameEvent (GameEvent.Revealed S.alice (Projection.project ratId gs))
    -- TriggerCondition's own per-constructor coverage lives in
    -- Pawl.Codec.TriggerConditionSpec now.
    Spec.it s "GameEvent.AttackerDeclared round-trips" $
      roundTrip s "ad" gameEventToJson jsonToGameEvent (GameEvent.AttackerDeclared (ObjectId.MkObjectId 3))
    -- CR 701.6a's event. Three DISTINCT payload values, two of them
    -- ObjectIds: a trip that swapped the countered spell for the countering
    -- source would survive equal ids and fail here.
    Spec.it s "GameEvent.SpellCountered round-trips" $
      roundTrip
        s
        "countered"
        gameEventToJson
        jsonToGameEvent
        (GameEvent.SpellCountered (Countering.MkCountering (ObjectId.MkObjectId 4) (ObjectId.MkObjectId 5) S.bob))
    -- Create's EntryRiders is ELIDED when it is the CR 110.5b default, so
    -- the round trip has to hold for all four shapes the encoder emits --
    -- and the two three-element ones (a slot, or an entry) are told apart
    -- by JSON type alone, which is the part that could silently confuse
    -- them.
    Spec.it s "Effect.Create round-trips with and without a EntryRiders and a slot" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let card = Printing.card piker
          attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True}
          plain = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False}
          slot = SlotName.MkSlotName (Text.pack "token")
      roundTrip s "plain" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 2) card plain Nothing)
      roundTrip s "plain+slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 1) card plain (Just slot))
      roundTrip s "entry" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 2) card attacking Nothing)
      roundTrip s "entry+slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.Create (Quantity.Literal 1) card attacking (Just slot))
      -- The elision itself: a default entry adds nothing to the payload,
      -- which is what keeps every token-making card file written before
      -- this one byte-identical.
      Spec.assertEqWith
        s
        "a default EntryRiders is not written"
        (J.tagged (Text.pack "Create") (Just (J.jArray [Quantity.toJson (Quantity.Literal 2), cardToJson card])))
        (effectToJson cardToJson (Effect.Create (Quantity.Literal 2) card plain Nothing))
    Spec.it s "Barbarian Outcast / Sarcomancy shaped Conditions round-trip" $
      mapM_
        (roundTrip s "condition" Condition.toJson Condition.fromJson)
        [S.youControlNoSwamps, noZombiesOnBattlefield]
    Spec.it s "AbilityName round-trips" $
      roundTrip s "name" AbilityName.toJson AbilityName.fromJson (AbilityName.MkAbilityName (Text.pack "sacrifice it"))
    Spec.it s "ArmDelayedTrigger round-trips" $
      roundTrip s "arm" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ArmDelayedTrigger (AbilityName.MkAbilityName (Text.pack "sacrifice it")) Onset.Immediately Nothing)
    -- CR 603.7b's stated duration takes the two-element form; the absent
    -- one above must keep the bare shape, so both have to survive.
    Spec.it s "ArmDelayedTrigger round-trips a stated duration" $
      roundTrip s "arm1" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ArmDelayedTrigger (AbilityName.MkAbilityName (Text.pack "each combat")) Onset.Immediately (Just Duration.UntilEndOfTurn))
    -- CR 603.7a's onset takes the THREE-element form, whose last element is a
    -- duration or null -- the two forms an onset can pair with. Length, not
    -- JSON type, is what tells this apart from the two-element form, since an
    -- Onset and a Duration are both tagged objects.
    Spec.it s "ArmDelayedTrigger round-trips a stated onset, with and without a duration" $ do
      let named = AbilityName.MkAbilityName (Text.pack "return it")
      roundTrip s "arm2" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ArmDelayedTrigger named Onset.FromYourNextTurn Nothing)
      roundTrip s "arm3" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.ArmDelayedTrigger named Onset.FromYourNextTurn (Just Duration.UntilEndOfTurn))
      -- The elision: a default onset adds nothing, which is what keeps every
      -- card file written before onsets existed byte-identical.
      Spec.assertEqWith
        s
        "a default Onset is not written"
        (J.tagged (Text.pack "ArmDelayedTrigger") (Just (AbilityName.toJson named)))
        (effectToJson cardToJson (Effect.ArmDelayedTrigger named Onset.Immediately Nothing))
    -- MoveToZone's two riders elide exactly as Create's do, and its
    -- three-element form is the same pair of shapes told apart by JSON type.
    Spec.it s "Effect.MoveToZone round-trips with and without EntryRiders and a bound slot" $ do
      let slot = SlotName.MkSlotName (Text.pack "target")
          bound = SlotName.MkSlotName (Text.pack "exiled")
          attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True}
      roundTrip s "move" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.MoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing)
      roundTrip s "move+slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.MoveToZone slot Zone.Exile EntryRiders.defaultValue (Just bound))
      roundTrip s "move+entry" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.MoveToZone bound Zone.Battlefield attacking Nothing)
      roundTrip s "move+entry+slot" (effectToJson cardToJson) (jsonToEffect jsonToCard) (Effect.MoveToZone bound Zone.Battlefield attacking (Just bound))
      Spec.assertEqWith
        s
        "default riders and no bound slot are not written"
        (J.tagged (Text.pack "MoveToZone") (Just (J.jArray [SlotName.toJson slot, Zone.toJson Zone.Hand])))
        (effectToJson cardToJson (Effect.MoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing))
    -- M-5 (fix pass 1): the "DelayedTrigger round-trips" test below exercises
    -- only a Binding's `target` field via Binding.toObject. The codec is
    -- meant to be total over every Binding field -- subtypes, amount, modes,
    -- and copy too -- so round-trip a Binding with all five populated at
    -- once, exercising Subtype.fromJsonPair along the way. No real slot ever
    -- carries all five together (copy lives only under the dedicated
    -- copySource slot in practice); this is a codec totality check, not a
    -- claim about a reachable game state.
    Spec.it s "a Binding with every field populated round-trips" $
      let binding =
            Binding.Type.MkBinding
              { Binding.Type.target = Just (Recipient.ToPlayer S.alice),
                Binding.Type.subtypes = Just (Subtype.Mountain, Subtype.Island),
                Binding.Type.amount = Just 3,
                Binding.Type.modes = Just (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]),
                Binding.Type.copy = Just S.emptyCharacteristics
              }
       in roundTrip s "binding" bindingToJson jsonToBinding binding
    Spec.it s "DelayedTrigger round-trips with its captured bindings" $
      let ability =
            TriggeredAbility.MkTriggeredAbility
              { TriggeredAbility.condition = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn,
                TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1),
                TriggeredAbility.intervening = Nothing
              }
          entry =
            DelayedTrigger.MkDelayedTrigger
              { DelayedTrigger.ability = ability,
                DelayedTrigger.source = ObjectId.MkObjectId 4,
                DelayedTrigger.controller = S.alice,
                DelayedTrigger.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "token")) (Binding.toObject (ObjectId.MkObjectId 9)),
                DelayedTrigger.notBefore = Nothing,
                DelayedTrigger.expiry = Nothing
              }
       in do
            -- CR 603.7b's default and its stated-duration exception both
            -- have to survive: the absent expiry is elided to null, and a
            -- present one must come back as itself. CR 603.7a's arming gate is
            -- the same pair of cases on the other end of the envelope.
            roundTrip s "delayed" delayedTriggerToJson jsonToDelayedTrigger entry
            roundTrip s "delayed1" delayedTriggerToJson jsonToDelayedTrigger entry {DelayedTrigger.expiry = Just Expiry.AtCleanup}
            roundTrip s "delayed2" delayedTriggerToJson jsonToDelayedTrigger entry {DelayedTrigger.notBefore = Just 7}
    Spec.it s "a TriggeredAbility with an intervening if round-trips" $
      let ability =
            TriggeredAbility.MkTriggeredAbility
              { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1),
                TriggeredAbility.intervening = Just noZombiesOnBattlefield
              }
       in roundTrip s "ta" (triggeredAbilityToJson cardToJson) (jsonToTriggeredAbility jsonToCard) ability
  -- Count's own per-constructor coverage (in a zone, over the event history,
  -- and scoped to a slot) lives in Pawl.Codec.CountSpec now; Quantity's Count
  -- arm and its nested-Greatest recursion live in Pawl.Codec.QuantitySpec;
  -- Condition's own every-comparison coverage (including both sides
  -- non-Count, which the Count-on-the-left shape this type replaced could not
  -- say at all -- Deathknell Berserker's "if its power was 3 or greater", CR
  -- 603.4) lives in Pawl.Codec.ConditionSpec.
  -- Pawl.Types.Effect is parametric in `card` so that Pawl.Types stays an
  -- acyclic module graph, and the codec mirrors that: the encoder reaches
  -- its card payload ONLY through the codec it is handed. Proving it at two
  -- different `card` types is what lets Pawl.Codec.Effect sit below
  -- Pawl.Codec.Card rather than in a cycle with it (#481).
  Spec.describe s "parametricity" $ do
    Spec.it s "effectToJson reaches card only through the supplied codec" $
      Spec.assertEqWith
        s
        "the emblem payload comes from the argument, not the card"
        (effectToJson (const sentinel) (Effect.CreateEmblem "a wholly different card type"))
        (effectToJson (const sentinel) (Effect.CreateEmblem ()))

-- The stand-in a parametricity test hands over in place of a real card codec:
-- any Value at all, so long as both instantiations are given the same one.
sentinel :: Value.Value
sentinel = J.jText (Text.pack "SENTINEL")

-- Sarcomancy's migrated intervening "if" (retired
-- StateCondition.NoPermanentsOfSubtype Zombie -- CR 603.4): ANY player's
-- Zombies, unlike S.youControlNoSwamps's ControlledBy conjunct.
noZombiesOnBattlefield :: Condition.Type.Condition
noZombiesOnBattlefield =
  Condition.Type.MkCondition
    ( Quantity.Count
        ( Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.HasSubtype Subtype.Zombie)
            Aggregation.Objects
        )
    )
    Comparison.Exactly
    (Quantity.Literal 0)
