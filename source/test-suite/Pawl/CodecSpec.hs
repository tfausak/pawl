-- Covers Pawl.Codec.
module Pawl.CodecSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import Pawl.Codec.Binding (bindingToJson, jsonToBinding)
import qualified Pawl.Codec.Condition as Condition
import Pawl.Codec.DelayedTrigger (delayedTriggerToJson, jsonToDelayedTrigger)
import Pawl.Codec.GameEvent (gameEventToJson, jsonToGameEvent)
import qualified Pawl.Codec.Json as J
import Pawl.Codec.Printing (jsonToPrinting, printingToJson)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
-- Aliased Filter.Type, not Filter, for consistency with FilterSpec: the
-- evaluator module Pawl.Engine.Filter is not imported here today, but the alias
-- convention is fixed project-wide so a later import never collides.

import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Json.Value as Value
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

roundTrip :: (Applicative m, Eq a, Show a) => Spec.Spec m n -> String -> (a -> Value.Value) -> (Value.Value -> Either Text a) -> a -> m ()
roundTrip s label enc dec x = Spec.assertEqWith s label (dec (enc x)) (Right x)

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
  -- Effect's own per-constructor coverage, including the length-discriminated
  -- ArmDelayedTrigger/Create/MoveToZone shapes and their elision assertions,
  -- lives in Pawl.Codec.EffectSpec now.
  -- Duration's own per-constructor coverage lives in Pawl.Codec.DurationSpec
  -- now; Condition's own per-constructor coverage lives in
  -- Pawl.Codec.ConditionSpec.
  -- PlayerEffect's own per-constructor coverage, including the Edgewalker
  -- typed-mana ReduceSpellCost, lives in Pawl.Codec.PlayerEffectSpec now.
  -- StaticAbility's own per-constructor coverage, including the CR 613.6
  -- empty-modifications-array rejection, lives in Pawl.Codec.StaticAbilitySpec
  -- now. PlayerStaticAbility's own per-constructor coverage lives in
  -- Pawl.Codec.PlayerStaticAbilitySpec now. The playerAbilities/mulliganAction/
  -- openingHandAction round-trip-plus-byte-stability pairs formerly here needed
  -- no registry fixture -- a synthetic Card proves them just as well -- so they
  -- moved to Pawl.Codec.CardSpec with the rest of Card's own coverage.
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
  -- and CR 118.5a {0} behavior, lives in Pawl.Codec.CostSpec now. The
  -- additionalCosts/alternativeCosts round-trip-plus-byte-stability pairs
  -- formerly here (as "cost (P8)") needed no registry fixture -- a synthetic
  -- Card proves them just as well -- so they moved to Pawl.Codec.CardSpec
  -- with the rest of Card's own coverage.
  -- ReplacementEffect's own per-constructor coverage -- including the
  -- ZoneChangeR Opponents/Anyones distinction, the CounterR/DamageR/TokenR
  -- "pattern and scaling/rewrite are data" shapes, the CounterR explicit
  -- JSON null, and the PhaseR whosePhase axis -- lives in
  -- Pawl.Codec.ReplacementEffectSpec now.
  -- Mode's own per-constructor coverage, including the CR 603.5 optionality
  -- elision in both directions, lives in Pawl.Codec.ModeSpec now; Modal's own
  -- coverage, including the empty-modes decode failure, lives in
  -- Pawl.Codec.ModalSpec.
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
    Spec.it s "Barbarian Outcast / Sarcomancy shaped Conditions round-trip" $
      mapM_
        (roundTrip s "condition" Condition.toJson Condition.fromJson)
        [S.youControlNoSwamps, noZombiesOnBattlefield]
    Spec.it s "AbilityName round-trips" $
      roundTrip s "name" AbilityName.toJson AbilityName.fromJson (AbilityName.MkAbilityName (Text.pack "sacrifice it"))
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

-- TriggeredAbility's own per-constructor coverage, including both states of
-- the CR 603.4 intervening "if" and the CR 603.7 toJsonDelayed/fromJsonDelayed
-- sort order, lives in Pawl.Codec.TriggeredAbilitySpec now.

-- Count's own per-constructor coverage (in a zone, over the event history,
-- and scoped to a slot) lives in Pawl.Codec.CountSpec now; Quantity's Count
-- arm and its nested-Greatest recursion live in Pawl.Codec.QuantitySpec;
-- Condition's own every-comparison coverage (including both sides
-- non-Count, which the Count-on-the-left shape this type replaced could not
-- say at all -- Deathknell Berserker's "if its power was 3 or greater", CR
-- 603.4) lives in Pawl.Codec.ConditionSpec.

-- Pawl.Types.Effect is parametric in `card` so that Pawl.Types stays an
-- acyclic module graph, and the codec mirrors that: the encoder reaches its
-- card payload ONLY through the codec it is handed, which lets
-- Pawl.Codec.Effect sit below Pawl.Codec.Card rather than in a cycle with it
-- (#481). That parametricity's own proof -- two different `card` types
-- through the same constant codec -- lives in Pawl.Codec.EffectSpec now.

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
