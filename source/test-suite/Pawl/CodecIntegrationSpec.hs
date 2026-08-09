-- Integration coverage for the codec sublibrary: the 104 per-type
-- Pawl.Codec.XSpec modules are the codec's own spec, each proving its shape
-- against literals with no registry. What remains here needs one -- these
-- cases load real cards through S.printingOf (Cancel, Rending Volley,
-- Typhoid Rats, Branchblight Stalker) to prove codec shapes against actual
-- card data, which a synthetic fixture could not stand in for.
module Pawl.CodecIntegrationSpec where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.GameEvent as GameEvent.Codec
import qualified Pawl.Codec.Printing as Printing.Codec
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
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

roundTrip :: (Applicative m, Eq a, Show a) => Spec.Spec m n -> String -> (a -> Value.Value) -> (Value.Value -> Either Text a) -> a -> m ()
roundTrip s label enc dec x = Spec.assertEqWith s label (dec (enc x)) (Right x)

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Codec (integration)" $ do
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
  -- Pawl.Codec.PlayerStaticAbilitySpec now. The playerAbilities/mulliganActions/
  -- openingHandActions round-trip-plus-byte-stability pairs formerly here needed
  -- no registry fixture -- a synthetic Card proves them just as well -- so they
  -- moved to Pawl.Codec.FaceSpec with the rest of a face's own coverage.
  -- TargetSpec's own per-constructor coverage (a bare pool, a filtered pool,
  -- and the Not IsSource conjunct that carries CR 601.2c's "another", #163)
  -- lives in Pawl.Codec.TargetSpecSpec now; Filter's own per-constructor and
  -- nested-And/Or/Not coverage lives in Pawl.Codec.FilterSpec.
  -- EntryRewrite's own per-constructor coverage lives in
  -- Pawl.Codec.EntryRewriteSpec now. GameEvent's own per-constructor
  -- coverage -- including LoyaltyAbilityActivated's CR 606.3 record --
  -- lives in Pawl.Codec.GameEventSpec now.
  -- Cost's own per-constructor coverage, including the CR 118.6 absent-mana
  -- and CR 118.5a {0} behavior, lives in Pawl.Codec.CostSpec now. The
  -- additionalCosts/alternativeCosts round-trip-plus-byte-stability pairs
  -- formerly here (as "cost (P8)") needed no registry fixture -- a synthetic
  -- Card proves them just as well -- so they moved to Pawl.Codec.FaceSpec
  -- with the rest of a face's own coverage.
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
    Spec.it s "P1: Printing.Codec.fromJson . Printing.Codec.toJson == Right" $ do
      ps <- S.allPrintings s
      mapM_ (\p -> Spec.assertEqWith s (show (Face.name (S.combinedFace p))) (Printing.Codec.fromJson (Printing.Codec.toJson p)) (Right p)) ps
    Spec.it s "P2: through text" $ do
      ps <- S.allPrintings s
      mapM_
        (\p -> Spec.assertEqWith s (show (Face.name (S.combinedFace p))) (Common.parse (Common.render (Printing.Codec.toJson p)) >>= Printing.Codec.fromJson) (Right p))
        ps
    Spec.it s "M4e Cancel loads as a single Counter effect targeting a spell" $ do
      cancel <- S.printingOf s registry "Cancel"
      let card = S.combinedFace cancel
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
    -- Stifle beside it, and the pair is the point: ONE Counter opcode, TWO
    -- pools. CR 113.9 -- "activated and triggered abilities on the stack aren't
    -- spells, and therefore can't be countered by anything that counters only
    -- spells" -- is carried entirely by which Pool the slot names, so a card
    -- file that reached for Pool.Spells here would let Stifle counter a spell.
    --
    -- Stifle's parenthetical is reminder text and decodes to nothing: CR 605.3b
    -- and CR 605.4a keep a mana ability off the stack, so there is no filter for
    -- it to become (hence the Nothing beside Pool.Abilities).
    Spec.it s "CR 113.9 Stifle loads as the same Counter effect over Pool.Abilities" $ do
      stifle <- S.printingOf s registry "Stifle"
      let card = S.combinedFace stifle
      Spec.assertEqWith
        s
        "effects"
        (Card.allEffects card)
        [Effect.Counter (SlotName.MkSlotName (Text.pack "ability"))]
      Spec.assertEqWith
        s
        "target spec"
        (Card.allTargetSpecs card)
        (Map.singleton (SlotName.MkSlotName (Text.pack "ability")) (TargetSpec.MkTargetSpec Pool.Abilities Nothing))
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
        (Face.counterability (S.combinedFace rendingVolley))
        Counterability.CantBeCountered
      Spec.assertEqWith
        s
        "Cancel does not, and its file has no counterability key"
        (Face.counterability (S.combinedFace cancel))
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
      roundTrip s "moved" GameEvent.Codec.toJson GameEvent.Codec.fromJson (GameEvent.Moved zc snapshot)
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
      roundTrip s "moved" GameEvent.Codec.toJson GameEvent.Codec.fromJson (GameEvent.Moved zc snapshot)
    -- GameEvent's own per-constructor coverage (DamageDealt, both a player
    -- and a CR 120.3c planeswalker Recipient; StepBegan; SpellCast;
    -- BecameMonarch; Discarded, both causes; AttackerDeclared;
    -- BlockerDeclared; AttackerBlocked;
    -- SpellCountered; LoyaltyAbilityActivated) needed no registry fixture --
    -- a synthetic stand-in snapshot proves Moved/Revealed/SpellCast just as
    -- well as a real one proves the shape -- so it moved to
    -- Pawl.Codec.GameEventSpec.
    -- CR 701.20a: the reveal's whole payload IS the snapshot, so it is the
    -- one GameEvent whose round-trip failing would silently erase what the
    -- players were shown rather than merely mislabel it. Typhoid Rats for
    -- the reason the Moved case gives -- every snapshot field populated.
    Spec.it s "GameEvent.Revealed round-trips with its snapshot" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      let (ratId, gs) = S.addLibraryCard typhoidRats S.alice (Setup.emptyGame S.bothPlayers)
      roundTrip s "revealed" GameEvent.Codec.toJson GameEvent.Codec.fromJson (GameEvent.Revealed S.alice (Projection.project ratId gs))
    -- TriggerCondition's own per-constructor coverage lives in
    -- Pawl.Codec.TriggerConditionSpec now.
    Spec.it s "Barbarian Outcast / Sarcomancy shaped Conditions round-trip" $
      mapM_
        (roundTrip s "condition" Condition.toJson Condition.fromJson)
        [S.youControlNoSwamps, noZombiesOnBattlefield]

-- AbilityName's own per-constructor coverage used no registry -- a literal
-- string proves the codec just as well as one loaded from a card -- so the
-- "AbilityName round-trips" case that lived here moved to
-- Pawl.Codec.AbilityNameSpec. Pawl.Codec.AbilityNameSpec's "fromJson"/"toJson"
-- cases already pin both directions against a literal, which is strictly
-- stronger than this round-trip was (it also fixes the wire shape, not only
-- the composition), so nothing was ported: dropped as a subsumed duplicate.

-- TriggeredAbility's own per-constructor coverage, including both states of
-- the CR 603.4 intervening "if" and the CR 603.7 toJsonDelayed/fromJsonDelayed
-- sort order, lives in Pawl.Codec.TriggeredAbilitySpec now.

-- Binding's own per-constructor coverage (the empty binding, and every field
-- populated at once, exercising Subtype.fromJsonPair) and its toJsonMap/
-- fromJsonMap sort-by-slot-name proof live in Pawl.Codec.BindingSpec now.
-- DelayedTrigger's own per-constructor coverage (CR 603.7a/603.7b's default,
-- and each of a restricted window and a stated expiry) lives in
-- Pawl.Codec.DelayedTriggerSpec now. Neither needed a registry fixture -- a synthetic Binding/Card stand-in
-- proves the shape just as well.

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
