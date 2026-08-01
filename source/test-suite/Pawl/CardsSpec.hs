-- Covers data/cards/*.json and Pawl.Slug.slugify.
module Pawl.CardsSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Printing (printingToJson)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.TokenEntry as TokenEntry
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

slugOf :: Printing.Printing -> Slug.Slug
slugOf = Slug.fromText . CardT.name . Printing.card

-- Each mode of a payload as (is it optional, what does it do) -- the shape the
-- CR 603.5 assertions below compare against.
modeShapes :: Modal.Modal CardT.Card -> [(Optionality.Optionality, [Effect.Effect CardT.Card])]
modeShapes m =
  fmap
    (\mode -> (Mode.optionality mode, Foldable.toList (Mode.effects mode)))
    (Foldable.toList (Modal.modes m))

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Cards" $ do
  Spec.it s "each committed file re-parses to its compiled card (P3)" $ do
    root <- Registry.defaultRoot
    ps <- S.allPrintings s
    mapM_ (checkFile s root) ps
  Spec.it s "clone.json loads as a 0/0 Shapeshifter with an EntryR AsCopy" $ do
    c <- S.cardOf s registry "Clone"
    Spec.assertEqWith s "entry replacement" (CardT.replacementEffects c) [ReplacementEffect.EntryR EntryRewrite.AsCopy]
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Clone")
    Spec.assertEqWith s "power" (CardT.power c) (Just (Power.MkPower (Quantity.Literal 0)))
  Spec.it s "serum-powder.json loads as a {3} artifact with a CR 103.5b mulligan action" $ do
    c <- S.cardOf s registry "Serum Powder"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Serum Powder")
    Spec.assertEqWith s "the CR 103.5b action" (CardT.mulliganAction c) [Effect.ExileHandThenDraw]
    Spec.assertEqWith s "one activated ability, the {T}: Add {C} mana ability" (length (CardT.activatedAbilities c)) 1
  -- The first card file with landwalk (CR 702.14), and the first whose
  -- keyword payload is a SUBTYPE. What it pins is that the land type is on
  -- the KEYWORD and not on the type line: Bog Wraith is a Wraith and prints
  -- no Swamp anywhere, so a reader that took the land type from
  -- TypeLine.subtypes would find nothing to walk on.
  Spec.it s "bog-wraith.json loads as a {3}{B} 3/3 Wraith whose only keyword is swampwalk" $ do
    c <- S.cardOf s registry "Bog Wraith"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Bog Wraith")
    Spec.assertEqWith
      s
      "{3}{B}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, ManaSymbol.OfType (ManaType.Colored Color.Black)]))
    Spec.assertEqWith s "power" (CardT.power c) (Just (Power.MkPower (Quantity.Literal 3)))
    Spec.assertEqWith s "toughness" (CardT.toughness c) (Just (Toughness.MkToughness (Quantity.Literal 3)))
    Spec.assertEqWith s "Creature -- Wraith" (TypeLine.subtypes (CardT.typeLine c)) (Set.singleton Subtype.Wraith)
    Spec.assertEqWith s "one keyword: swampwalk" (CardT.keywords c) (Set.singleton (Keyword.Landwalk Subtype.Swamp))
    Spec.assertEqWith s "no other text" (CardT.staticAbilities c) []
    Spec.assertEqWith s "and no triggered ability either" (CardT.triggeredAbilities c) []
  -- The first card file to carry BOTH a keyword and a counterability, and the
  -- first whose CR 113.6g clause sits on a creature rather than an instant
  -- (Rending Volley's). The two clauses are separate fields because they are
  -- separate rules: CR 113.6g's "can't be countered" functions on the stack and
  -- grants no targeting immunity, while CR 702.18a's shroud functions on the
  -- battlefield and grants nothing else.
  Spec.it s "blurred-mongoose.json loads as a {1}{G} 2/1 Mongoose that is uncounterable and has shroud" $ do
    c <- S.cardOf s registry "Blurred Mongoose"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Blurred Mongoose")
    Spec.assertEqWith
      s
      "{1}{G}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Green)]))
    Spec.assertEqWith s "power" (CardT.power c) (Just (Power.MkPower (Quantity.Literal 2)))
    Spec.assertEqWith s "toughness" (CardT.toughness c) (Just (Toughness.MkToughness (Quantity.Literal 1)))
    Spec.assertEqWith s "Creature -- Mongoose" (TypeLine.subtypes (CardT.typeLine c)) (Set.singleton Subtype.Mongoose)
    Spec.assertEqWith s "one keyword: shroud" (CardT.keywords c) (Set.singleton Keyword.Shroud)
    Spec.assertEqWith s "CR 113.6g: this spell can't be countered" (CardT.counterability c) Counterability.CantBeCountered
    Spec.assertEqWith s "no other text" (CardT.staticAbilities c) []
    Spec.assertEqWith s "and no triggered ability either" (CardT.triggeredAbilities c) []
  -- The first card file whose keyword carries a payload that is not a
  -- number: rule 702.34a's flashback COST, which is where the whole ability
  -- lives -- Firebolt prints no alternativeCosts and no castingPermissions of
  -- its own, and Pawl.Engine.Keyword derives all three of rule 702.34a's
  -- consequences from this one value.
  Spec.it s "firebolt.json loads as a {R} Sorcery whose only keyword is flashback {4}{R}" $ do
    c <- S.cardOf s registry "Firebolt"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Firebolt")
    Spec.assertEqWith
      s
      "printed cost is {R}, unchanged by the flashback ability"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Red)]))
    Spec.assertEqWith
      s
      "one keyword: flashback {4}{R}"
      (CardT.keywords c)
      ( Set.singleton
          ( Keyword.Flashback
              Cost.MkCost
                { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, ManaSymbol.OfType (ManaType.Colored Color.Red)]),
                  Cost.components = []
                }
          )
      )
    Spec.assertEqWith s "no printed alternative cost" (CardT.alternativeCosts c) []
    Spec.assertEqWith s "no printed casting permission" (CardT.castingPermissions c) []
  -- The first card file with entwine (CR 702.42), and so the first whose
  -- keyword payload changes how many MODES the spell has rather than what it
  -- costs or where it can be cast from. Two things this file pins:
  --
  --   * the printed selection is still ChooseExactly 1. Rule 702.42a widens
  --     it "instead of just the number specified" at CAST time (Pawl.Engine.Cast),
  --     so a card that printed 2 here would be a different card.
  --   * the two modes name DIFFERENT slots. Card.modesTargetSpecs unions the
  --     chosen modes' specs by slot name, and an entwined cast chooses both
  --     -- so a shared name would fuse the two targets into one and make
  --     "tap one permanent and untap another" impossible to cast.
  Spec.it s "dreams-grip.json loads as a {U} Instant with entwine {1} over a tap mode and an untap mode" $ do
    c <- S.cardOf s registry "Dream's Grip"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Dream's Grip")
    Spec.assertEqWith s "{U}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
    Spec.assertEqWith
      s
      "Instant"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Instant) Set.empty)
    Spec.assertEqWith
      s
      "one keyword: entwine {1}"
      (CardT.keywords c)
      ( Set.singleton
          ( Keyword.Entwine
              Cost.MkCost
                { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
                  Cost.components = []
                }
          )
      )
    Spec.assertEqWith s "no printed additional cost: the entwine cost is the keyword's, and optional" (CardT.additionalCosts c) []
    Spec.assertEqWith s "the printed selection is still choose one" (Modal.selection (CardT.spell c)) (ModeSelection.ChooseExactly 1)
    Spec.assertEqWith
      s
      "tap first, then untap -- the printed order CR 702.42b resolves in"
      (modeShapes (CardT.spell c))
      [ (Optionality.Mandatory, [Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "tapped")))]),
        (Optionality.Mandatory, [Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "untapped")))])
      ]
    Spec.assertEqWith
      s
      "each mode targets any permanent, through a slot of its own"
      (fmap Mode.targetSpecs (Foldable.toList (Modal.modes (CardT.spell c))))
      [ Map.singleton (SlotName.MkSlotName (Text.pack "tapped")) (TargetSpec.MkTargetSpec Pool.Permanents Nothing),
        Map.singleton (SlotName.MkSlotName (Text.pack "untapped")) (TargetSpec.MkTargetSpec Pool.Permanents Nothing)
      ]
  -- The first card file whose mode prints a "may" (CR 603.5), and so the
  -- first to carry an `optionality` key at all. Its SPELL half is mandatory
  -- in the same file, which is what proves the key is per-mode rather than
  -- per-card.
  Spec.it s "renewed-faith.json loads with an Optional cycling trigger and a Mandatory spell" $ do
    c <- S.cardOf s registry "Renewed Faith"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Renewed Faith")
    Spec.assertEqWith
      s
      "the spell gains 6 and is mandatory"
      (modeShapes (CardT.spell c))
      [(Optionality.Mandatory, [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 6)])]
    Spec.assertEqWith
      s
      "the cycling trigger gains 2 and is optional"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[(Optionality.Optional, [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)])]]
  -- The first card file whose triggered ability functions somewhere other
  -- than the battlefield (CR 113.6k). Its "may" reuses renewed-faith.json's
  -- per-mode optionality key rather than adding a second spelling.
  Spec.it s "narcomoeba.json loads as a {1}{U} flying Illusion with an Optional graveyard trigger" $ do
    c <- S.cardOf s registry "Narcomoeba"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Narcomoeba")
    Spec.assertEqWith s "{1}{U}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
    Spec.assertEqWith
      s
      "Creature -- Illusion"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Illusion))
    Spec.assertEqWith s "1/1" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1)))
    Spec.assertEqWith s "flying, and nothing else" (CardT.keywords c) (Set.singleton Keyword.Flying)
    Spec.assertEqWith
      s
      "the trigger watches library -> graveyard"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfPutIntoGraveyardFromLibrary]
    Spec.assertEqWith
      s
      "and may put the card itself onto the battlefield"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[(Optionality.Optional, [Effect.MoveToZone Binding.triggerSource Zone.Battlefield])]]
  Spec.it s "hanweir-garrison.json loads as a {2}{R} 2/3 whose attack trigger makes two tapped attacking Humans" $ do
    c <- S.cardOf s registry "Hanweir Garrison"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Hanweir Garrison")
    Spec.assertEqWith s "{2}{R}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
    Spec.assertEqWith
      s
      "Creature -- Human Soldier"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Human, Subtype.Soldier]))
    Spec.assertEqWith s "2/3" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 3)))
    -- CR 508.3a: "whenever this creature attacks" is the DECLARATION, which
    -- is the whole of this condition.
    Spec.assertEqWith
      s
      "one trigger, on being declared as an attacker"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
    Spec.assertEqWith
      s
      "two tokens, tapped and attacking"
      [q | ab <- CardT.triggeredAbilities c, Effect.Create q _ _ _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))]
      [Quantity.Literal 2]
    Spec.assertEqWith
      s
      "the entry riders are the effect's, not the token's"
      [te | ab <- CardT.triggeredAbilities c, Effect.Create _ _ te _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))]
      [TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Tapped, TokenEntry.attacking = True}]
    -- Meld (CR 702.157) is not modelled: the printed reminder text says it
    -- melds with Hanweir Battlements, and neither the partner nor the melded
    -- permanent is in the pool (#369).
    Spec.assertEqWith s "no keywords" (CardT.keywords c) Set.empty
  -- The first card file whose triggered ability watches the battlefield ->
  -- graveyard pair (CR 603.6c through CR 700.4's "dies"), and the mirror of
  -- narcomoeba.json's library -> graveyard one above.
  Spec.it s "doomed-traveler.json loads as a {W} 1/1 whose dies trigger makes a flying white Spirit" $ do
    c <- S.cardOf s registry "Doomed Traveler"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Doomed Traveler")
    Spec.assertEqWith s "{W}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White)]))
    Spec.assertEqWith
      s
      "Creature -- Human Soldier"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Human, Subtype.Soldier]))
    Spec.assertEqWith s "1/1" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1)))
    Spec.assertEqWith s "no keywords of its own" (CardT.keywords c) Set.empty
    Spec.assertEqWith
      s
      "one trigger, on dying"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfDies]
    case [(q, tc) | ab <- CardT.triggeredAbilities c, Effect.Create q tc _ _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))] of
      [(quantity, token)] -> do
        Spec.assertEqWith s "one token" quantity (Quantity.Literal 1)
        -- CR 111.4: "If the spell or ability doesn't specify the name of
        -- the token, its name is the same as its subtype(s) plus the word
        -- 'Token.'" Doomed Traveler specifies no name.
        Spec.assertEqWith s "named Spirit Token" (CardT.name token) (Text.pack "Spirit Token")
        Spec.assertEqWith
          s
          "Creature -- Spirit"
          (CardT.typeLine token)
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Spirit))
        Spec.assertEqWith s "1/1" (CardT.power token, CardT.toughness token) (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1)))
        Spec.assertEqWith s "with flying" (CardT.keywords token) (Set.singleton Keyword.Flying)
        -- CR 202.2e: "An object may have a color indicator printed to the
        -- left of the type line. That object is each color denoted by that
        -- color indicator." A token has no mana cost, so CR 202.2's
        -- mana-symbol rule would leave it colorless (CR 202.2b); the
        -- indicator is what makes this one white.
        Spec.assertEqWith s "and white by colour indicator" (CardT.colorIndicator token) (Set.singleton Color.White)
      other -> Spec.assertFailure s ("expected exactly one Create, got " <> show (length other))
  -- The pool's first card whose dies trigger acts on ITSELF, and so the
  -- first that has to tell CR 113.7a's source apart from CR 400.7e's "new
  -- object that it became". The effect is narcomoeba.json's opcode with a
  -- different slot, which is precisely the distinction: that card's "self"
  -- IS the arriving graveyard card, and this one's is not.
  Spec.it s "endless-cockroaches.json loads as a {1}{B}{B} 1/1 whose dies trigger returns the card it became to hand" $ do
    c <- S.cardOf s registry "Endless Cockroaches"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Endless Cockroaches")
    Spec.assertEqWith
      s
      "{1}{B}{B}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black), ManaSymbol.OfType (ManaType.Colored Color.Black)]))
    Spec.assertEqWith
      s
      "Creature -- Insect"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Insect))
    Spec.assertEqWith s "1/1" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1)))
    Spec.assertEqWith s "no keywords" (CardT.keywords c) Set.empty
    Spec.assertEqWith
      s
      "one trigger, on dying"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfDies]
    Spec.assertEqWith s "and no intervening if" (fmap TriggeredAbility.intervening (CardT.triggeredAbilities c)) [Nothing]
    Spec.assertEqWith
      s
      "returning the became slot to its owner's hand"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[(Optionality.Mandatory, [Effect.MoveToZone Binding.became Zone.Hand])]]
  -- The pool's first INTERVENING "if" on a look-back trigger (CR 603.4 read
  -- against CR 608.2h last known information), and the first condition whose
  -- measured side is not a Count at all.
  Spec.it s "deathknell-berserker.json loads as a {1}{B} 2/2 whose dies trigger is gated on its own power" $ do
    c <- S.cardOf s registry "Deathknell Berserker"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Deathknell Berserker")
    Spec.assertEqWith s "{1}{B}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black)]))
    Spec.assertEqWith
      s
      "Creature -- Elf Berserker"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Elf, Subtype.Berserker]))
    Spec.assertEqWith s "2/2" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 2)))
    Spec.assertEqWith
      s
      "one trigger, on dying"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfDies]
    Spec.assertEqWith
      s
      "gated on its own power being 3 or greater"
      (fmap TriggeredAbility.intervening (CardT.triggeredAbilities c))
      [Just (Condition.MkCondition Quantity.Power Comparison.AtLeast (Quantity.Literal 3))]
    case [(q, tc) | ab <- CardT.triggeredAbilities c, Effect.Create q tc _ _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))] of
      [(quantity, token)] -> do
        Spec.assertEqWith s "one token" quantity (Quantity.Literal 1)
        -- CR 111.4, the multi-subtype case the rule's own Dwarven
        -- Reinforcements example spells out: both subtypes, then "Token".
        Spec.assertEqWith s "named Zombie Berserker Token" (CardT.name token) (Text.pack "Zombie Berserker Token")
        Spec.assertEqWith
          s
          "Creature -- Zombie Berserker"
          (CardT.typeLine token)
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Zombie, Subtype.Berserker]))
        Spec.assertEqWith s "2/2" (CardT.power token, CardT.toughness token) (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 2)))
        Spec.assertEqWith s "no keywords" (CardT.keywords token) Set.empty
        -- CR 202.2b/202.2e, exactly as doomed-traveler.json's Spirit: a
        -- token has no mana cost, so only the colour indicator makes it
        -- black.
        Spec.assertEqWith s "and black by colour indicator" (CardT.colorIndicator token) (Set.singleton Color.Black)
      other -> Spec.assertFailure s ("expected exactly one Create, got " <> show (length other))
  -- CR 702.19 trample plus the CR 510.1b combat-damage-to-a-player trigger
  -- condition on one card, which is what makes the trigger's event and the
  -- bearer's death land in a single CR 117.5 batch. The 1 toughness is
  -- load-bearing and pinned here so a future edit cannot quietly make the
  -- Skelemental survive its blocker: TriggerSpec's bystander group would
  -- then prove nothing.
  Spec.it s "lightning-skelemental.json loads as a {B}{R}{R} 6/1 trampler that makes the damaged player discard two" $ do
    c <- S.cardOf s registry "Lightning Skelemental"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Lightning Skelemental")
    Spec.assertEqWith
      s
      "{B}{R}{R}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Black), ManaSymbol.OfType (ManaType.Colored Color.Red), ManaSymbol.OfType (ManaType.Colored Color.Red)]))
    Spec.assertEqWith s "6/1" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 6)), Just (Toughness.MkToughness (Quantity.Literal 1)))
    Spec.assertEqWith s "trample and haste" (CardT.keywords c) (Set.fromList [Keyword.Trample, Keyword.Haste])
    Spec.assertEqWith
      s
      "Creature -- Elemental Skeleton"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Elemental, Subtype.Skeleton]))
    Spec.assertEqWith
      s
      "two triggers: the combat-damage one and the end-step sacrifice"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [ TriggerCondition.SelfDealsCombatDamageToPlayer,
        TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn
      ]
    Spec.assertEqWith
      s
      -- The reserved "that player" slot, read by a card for the first time
      -- rather than by CR 702.70a's poisonous: the discard names the slot
      -- Event.eventBindings stamps for the CR 510.1b combat-damage-to-a-
      -- player condition, not a target and not the controller.
      "the damaged player discards two, then the Skelemental sacrifices itself"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [ [(Optionality.Mandatory, [Effect.Discard Binding.triggerPlayer (Quantity.Literal 2)])],
        [(Optionality.Mandatory, [Effect.Sacrifice Binding.triggerSource])]
      ]
  Spec.it s "leyline-of-the-void.json loads with a CR 103.6a action and an Opponents redirect" $ do
    c <- S.cardOf s registry "Leyline of the Void"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Leyline of the Void")
    Spec.assertEqWith
      s
      "the CR 103.6a action puts itself onto the battlefield"
      (CardT.openingHandAction c)
      [Effect.MoveToZone Binding.triggerSource Zone.Battlefield]
    Spec.assertEqWith
      s
      "and the redirect is scoped to an opponent's graveyard"
      (CardT.replacementEffects c)
      [ ReplacementEffect.ZoneChangeR
          (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents ZoneChangeSubject.AnyObject)
          Zone.Exile
      ]
  -- CR 614.1b: the first card in the pool whose replacement effect is a
  -- SKIP. Nothing about Eon Hub is a static ability -- the whole card is one
  -- replacement -- which is the correction this file's presence records.
  Spec.it s "eon-hub.json loads as a {5} artifact whose only ability is a PhaseR skip" $ do
    c <- S.cardOf s registry "Eon Hub"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Eon Hub")
    Spec.assertEqWith s "{5}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 5]))
    Spec.assertEqWith
      s
      -- whosePhase = Nothing is the SYMMETRY: "PLAYERS skip their upkeep
      -- steps" names nobody, so the pattern reads no PlayerId and the skip
      -- takes every player's upkeep. Fatigue is the Just.
      "players skip their upkeep steps"
      (CardT.replacementEffects c)
      [ ReplacementEffect.PhaseR
          PhasePattern.MkPhasePattern
            { PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep),
              PhasePattern.whosePhase = Nothing
            }
      ]
    Spec.assertEqWith s "and it is not a continuous effect" (CardT.staticAbilities c) []
  -- CR 614.10a: the first card whose skip is created by an EFFECT rather
  -- than printed on a permanent. Nothing is in `replacementEffects` -- a
  -- sorcery has no ability that exists on the battlefield -- which is the
  -- structural contrast with Eon Hub just above.
  Spec.it s "fatigue.json loads as a {1}{U} sorcery whose only effect is a SkipNextPhase" $ do
    c <- S.cardOf s registry "Fatigue"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Fatigue")
    Spec.assertEqWith s "{1}{U}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
    Spec.assertEqWith
      s
      "target player skips their next draw step"
      (modeShapes (CardT.spell c))
      [ ( Optionality.Mandatory,
          [Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep))]
        )
      ]
    Spec.assertEqWith s "nothing of it survives on the battlefield" (CardT.replacementEffects c) []
  -- CR 500.7 / 500.11: the pool's one turn-SCOPED skip. "Take an extra turn
  -- after this one. Skip the untap step of that turn." Two printed sentences,
  -- ONE opcode: "that turn" is the turn this same resolution creates, so the
  -- skip rides on the created turn rather than being a second effect holding
  -- a reference to it (see Pawl.Types.ExtraTurn). Contrast fatigue.json just
  -- above, whose SkipNextPhase names a player's NEXT occurrence instead.
  Spec.it s "savor-the-moment.json loads as a {1}{U}{U} sorcery whose TakeExtraTurn carries the untap skip" $ do
    c <- S.cardOf s registry "Savor the Moment"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Savor the Moment")
    Spec.assertEqWith
      s
      "{1}{U}{U}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Blue), ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
    Spec.assertEqWith
      s
      "take an extra turn after this one, and skip the untap step of that turn"
      (modeShapes (CardT.spell c))
      [ ( Optionality.Mandatory,
          [Effect.TakeExtraTurn (PlayerRef.Relative PlayerRelation.You) (Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap)))]
        )
      ]
    -- Targetless: "take an extra turn" names its own caster, unlike Time
    -- Warp's "target player" below.
    Spec.assertEqWith s "no target slots" (fmap Mode.targetSpecs (Foldable.toList (Modal.modes (CardT.spell c)))) [Map.empty]
    Spec.assertEqWith s "nothing of it survives on the battlefield" (CardT.replacementEffects c) []
  -- CR 500.7: the pool's one creator of an extra turn for ANOTHER player.
  -- "Target player takes an extra turn after this one" -- so the recipient is
  -- a TARGET, which is what makes an opponent's extra turn expressible at
  -- all, and its skip set is empty.
  Spec.it s "time-warp.json loads as a {3}{U}{U} sorcery whose only effect is a TakeExtraTurn" $ do
    c <- S.cardOf s registry "Time Warp"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Time Warp")
    Spec.assertEqWith
      s
      "{3}{U}{U}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, ManaSymbol.OfType (ManaType.Colored Color.Blue), ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
    Spec.assertEqWith
      s
      "target player takes an extra turn after this one"
      (modeShapes (CardT.spell c))
      [(Optionality.Mandatory, [Effect.TakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Set.empty])]
    Spec.assertEqWith s "nothing of it survives on the battlefield" (CardT.replacementEffects c) []
  -- CR 307.5: the first card whose ACTIVATED ability prints a timing rider
  -- naming a phase, so the first file to carry a `timing` key that is not
  -- SorcerySpeed. "{T}: Add {C}. / {T}: This land deals 1 damage to target
  -- attacking creature. Activate only during the end of combat step."
  --
  -- Also the first NONBASIC land type in the pool (CR 205.3i), which is what
  -- separates Pawl.Engine.Subtype.isLandType from Pawl.Engine.Mana.subtypeMana: Desert is
  -- a land type that grants no intrinsic mana ability, so the "{T}: Add {C}"
  -- asserted here has to be PRINTED, and it is.
  Spec.it s "desert.json loads as a Land -- Desert whose ping is gated to the end of combat step" $ do
    c <- S.cardOf s registry "Desert"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Desert")
    Spec.assertEqWith s "no mana cost" (CardT.manaCost c) Nothing
    Spec.assertEqWith
      s
      "Land -- Desert"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Land) (Set.singleton Subtype.Desert))
    Spec.assertEqWith
      s
      "the mana ability is unrestricted and the ping is not"
      (fmap ActivatedAbility.timing (CardT.activatedAbilities c))
      [ActivationTiming.AnyTime, ActivationTiming.DuringPhase (Phase.Combat CombatStep.EndOfCombat)]
    -- CR 605.1a: "An activated ability is a mana ability if it meets all of
    -- the following criteria: it doesn't require a target ..., it could add
    -- mana to a player's mana pool when it resolves, and it's not a loyalty
    -- ability." The ping targets, so the rider rides on the ability that is
    -- NOT a mana ability -- which is why Pawl.Engine.Activate ever sees it at all.
    Spec.assertEqWith
      s
      "one mana ability, one not"
      (fmap Mana.isManaAbility (CardT.activatedAbilities c))
      [True, False]
    Spec.assertEqWith
      s
      "one adds {C}, the other deals 1 to its target"
      (fmap (modeShapes . ActivatedAbility.modal) (CardT.activatedAbilities c))
      [ [(Optionality.Mandatory, [Effect.AddMana (ManaProduction.OfType ManaType.Colorless)])],
        [(Optionality.Mandatory, [Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 1)])]
      ]
    -- CR 601.2c reaches an activation through CR 602.2b, and this is the
    -- pool it announces from: creatures, narrowed to the attacking ones.
    Spec.assertEqWith
      s
      "and can only pick an attacking creature"
      [Map.toList (Mode.targetSpecs m) | ab <- CardT.activatedAbilities c, m <- Foldable.toList (Modal.modes (ActivatedAbility.modal ab))]
      [ [],
        [(SlotName.MkSlotName (Text.pack "target"), TargetSpec.MkTargetSpec Pool.Creatures (Just Filter.IsAttacking))]
      ]
  -- The pool's first card whose ENTERS trigger acts on the permanent that
  -- entered. Soul Warden shares the condition and names nothing about the
  -- entrant; endless-cockroaches.json shares the slot but reads it from a
  -- look-back dies trigger, where the entrant is another incarnation of the
  -- bearer itself. Aether Flash is where CR 400.7e's "the new object that it
  -- became" is a wholly different card from the ability's source.
  --
  -- The Filter is a bare HasCardType Creature, with no Not IsSource: the
  -- printed text says "a creature", not "another creature", and an
  -- enchantment could not match a creature filter anyway.
  Spec.it s "aether-flash.json loads as a {2}{R}{R} enchantment dealing 2 to the creature that entered" $ do
    c <- S.cardOf s registry "Aether Flash"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Aether Flash")
    Spec.assertEqWith
      s
      "{2}{R}{R}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Red), ManaSymbol.OfType (ManaType.Colored Color.Red)]))
    Spec.assertEqWith
      s
      "Enchantment"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Enchantment) Set.empty)
    Spec.assertEqWith s "no power or toughness" (CardT.power c, CardT.toughness c) (Nothing, Nothing)
    Spec.assertEqWith
      s
      "one trigger, on any creature entering"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.PermanentEnters (Filter.HasCardType CardType.Creature)]
    Spec.assertEqWith
      s
      "dealing 2 damage to the became slot"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[(Optionality.Mandatory, [Effect.DealDamage Binding.became (Quantity.Literal 2)])]]
    Spec.assertEqWith
      s
      "and it targets nothing"
      (fmap (fmap Mode.targetSpecs . Foldable.toList . Modal.modes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[Map.empty]]
  -- The pool's first printing carrying CR 205.4g's supertype: "any permanent
  -- with the supertype 'snow' is a snow permanent." The whole of the rule is
  -- the type line -- no state-based action, no casting restriction -- so this
  -- file differs from mountain.json in exactly one entry.
  --
  -- The printed "({T}: Add {R}.)" is REMINDER text, not an ability: CR 305.6
  -- grants "{T}: Add {R}" intrinsically to any object with the Land card type
  -- and the Mountain subtype, "even if the text box doesn't actually contain
  -- that text." Printing it here would give the card two mana abilities.
  Spec.it s "snow-covered-mountain.json loads as a Basic Snow Land - Mountain with no printed ability" $ do
    c <- S.cardOf s registry "Snow-Covered Mountain"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Snow-Covered Mountain")
    Spec.assertEqWith
      s
      "CR 205.4a: basic and snow, over the Mountain subtype"
      (CardT.typeLine c)
      ( TypeLine.MkTypeLine
          (Set.fromList [Supertype.Basic, Supertype.Snow])
          (Set.singleton CardType.Land)
          (Set.singleton Subtype.Mountain)
      )
    Spec.assertEqWith s "CR 305.6: the mana ability is intrinsic, so the file prints none" (CardT.activatedAbilities c) []
    Spec.assertEqWith s "a land has no mana cost" (CardT.manaCost c) Nothing
  -- The card that READS the supertype, which is what makes CR 205.4g worth
  -- modelling: a snow permanent nothing counts is unobservable. "Snow
  -- permanents you control" is a count over the battlefield (CR 110.1: "a
  -- permanent is a card or token on the battlefield"), narrowed by the
  -- supertype and by CR 109.5's controller -- the same shape nightmare.json
  -- uses for Swamps, with HasSupertype where that has HasSubtype.
  Spec.it s "skred.json loads as a {R} Instant dealing damage equal to the snow permanents you control" $ do
    c <- S.cardOf s registry "Skred"
    let target = SlotName.MkSlotName (Text.pack "target")
        snowPermanentsYouControl =
          Count.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.And [Filter.HasSupertype Supertype.Snow, Filter.ControlledBy PlayerRelation.You])
            Aggregation.Objects
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Skred")
    Spec.assertEqWith s "{R}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Red)]))
    Spec.assertEqWith s "Instant" (CardT.typeLine c) (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Instant) Set.empty)
    Spec.assertEqWith
      s
      "one unnarrowed target creature"
      (fmap Mode.targetSpecs (Foldable.toList (Modal.modes (CardT.spell c))))
      [Map.singleton target (TargetSpec.MkTargetSpec Pool.Creatures Nothing)]
    Spec.assertEqWith
      s
      "CR 205.4g: damage equal to the snow permanents you control"
      (modeShapes (CardT.spell c))
      [(Optionality.Mandatory, [Effect.DealDamage target (Quantity.Count snowPermanentsYouControl)])]
  -- The pool's first KINDRED card (CR 308). CR 308.1 -- "each kindred card
  -- has another card type" -- is why the type line carries Enchantment
  -- alongside Kindred, and CR 110.4 keeps Kindred off the list of six
  -- permanent types, so it is the Enchantment that makes this a permanent.
  -- CR 308.2 -- "the set of kindred subtypes is the same as the set of
  -- creature subtypes" -- is why a NONCREATURE card carries the creature
  -- type Faerie; Pawl.TriggerSpec's Kindred group is where that is proved
  -- observable. CR 308.3 needs nothing here: cards printed with the
  -- "tribal" type were errata'd, so the Oracle text this file transcribes
  -- is already kindred.
  Spec.it s "bitterblossom.json loads as a {1}{B} Kindred Enchantment - Faerie whose upkeep trigger costs 1 life and makes a Faerie Rogue" $ do
    c <- S.cardOf s registry "Bitterblossom"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Bitterblossom")
    Spec.assertEqWith s "{1}{B}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black)]))
    Spec.assertEqWith
      s
      "Kindred Enchantment - Faerie"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.fromList [CardType.Kindred, CardType.Enchantment]) (Set.singleton Subtype.Faerie))
    Spec.assertEqWith s "no power or toughness" (CardT.power c, CardT.toughness c) (Nothing, Nothing)
    Spec.assertEqWith s "no keywords" (CardT.keywords c) Set.empty
    -- CR 603.3a / 109.5: "your upkeep" is the ability CONTROLLER's, which is
    -- what TurnScope.ControllersTurn spells.
    Spec.assertEqWith
      s
      "one trigger, at the beginning of its controller's upkeep"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn]
    case concatMap (concatMap snd . modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c) of
      [Effect.LoseLife who amount, Effect.Create quantity token entry slot] -> do
        -- Printed order, and it is the order the effects are authored in:
        -- "you lose 1 life AND create".
        Spec.assertEqWith s "its controller loses the life" who (PlayerRef.Relative PlayerRelation.You)
        Spec.assertEqWith s "1 life" amount (Quantity.Literal 1)
        Spec.assertEqWith s "one token" quantity (Quantity.Literal 1)
        Spec.assertEqWith s "with no entry riders" entry TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Untapped, TokenEntry.attacking = False}
        Spec.assertEqWith s "and no slot bound to it" slot Nothing
        -- CR 111.4: Bitterblossom names no token, so the name is the
        -- subtypes plus the word "Token" -- the rule's own example is
        -- "Dwarf Berserker Token".
        Spec.assertEqWith s "named Faerie Rogue Token" (CardT.name token) (Text.pack "Faerie Rogue Token")
        Spec.assertEqWith
          s
          "Creature - Faerie Rogue"
          (CardT.typeLine token)
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Rogue, Subtype.Faerie]))
        Spec.assertEqWith s "1/1" (CardT.power token, CardT.toughness token) (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1)))
        Spec.assertEqWith s "with flying" (CardT.keywords token) (Set.singleton Keyword.Flying)
        -- CR 202.2b/202.2e, exactly as doomed-traveler.json's Spirit: a
        -- token has no mana cost, so only the colour indicator makes it
        -- black.
        Spec.assertEqWith s "and black by colour indicator" (CardT.colorIndicator token) (Set.singleton Color.Black)
      other -> Spec.assertFailure s ("expected exactly [LoseLife, Create], got " <> show (length other) <> " effects")
  -- The first planeswalker printing (CR 306), and the first card file with
  -- three activated abilities. Its type line is where CR 306.4 is settled:
  -- the planeswalker uniqueness rule "has been removed and planeswalker cards
  -- printed before this change have received errata in the Oracle card
  -- reference to have the legendary supertype", so the file transcribes
  -- Legendary and Pawl.Engine.Sba's CR 704.5j legend rule covers it with no
  -- planeswalker-specific arm.
  --
  -- The abilities carry NO ActivationTiming rider, and that is the point: CR
  -- 606.2 makes an ability a loyalty ability by what its COST contains, and CR
  -- 606.3's sorcery-speed window follows from that in the rules core
  -- (Pawl.Engine.Activate.loyaltyOk). A file claiming SorcerySpeed here would
  -- be card data teaching the engine a rule it already has.
  Spec.it s "jace-beleren.json loads as a {1}{U}{U} Legendary Planeswalker - Jace with loyalty 3 and three loyalty abilities" $ do
    c <- S.cardOf s registry "Jace Beleren"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Jace Beleren")
    Spec.assertEqWith
      s
      "{1}{U}{U}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Blue), ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
    Spec.assertEqWith
      s
      "Legendary Planeswalker - Jace"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine (Set.singleton Supertype.Legendary) (Set.singleton CardType.Planeswalker) (Set.singleton Subtype.Jace))
    Spec.assertEqWith s "no power or toughness" (CardT.power c, CardT.toughness c) (Nothing, Nothing)
    -- CR 306.5a: "the number printed in its lower right corner".
    Spec.assertEqWith s "loyalty 3" (CardT.loyalty c) (Just (Loyalty.MkLoyalty 3))
    -- CR 606.4's loyalty symbols, in printed order, each with a {0} mana part
    -- rather than none: CR 118.6's unpayable cost is Nothing, and a loyalty
    -- ability's cost is real and free of mana.
    Spec.assertEqWith
      s
      "+2, -1, -10 and no mana"
      (fmap ((\cost -> (Cost.mana cost, Cost.components cost)) . ActivatedAbility.cost) (CardT.activatedAbilities c))
      [ (Just (ManaCost.MkManaCost []), [CostComponent.AddLoyaltyToThis 2]),
        (Just (ManaCost.MkManaCost []), [CostComponent.RemoveLoyaltyFromThis 1]),
        (Just (ManaCost.MkManaCost []), [CostComponent.RemoveLoyaltyFromThis 10])
      ]
    Spec.assertEqWith
      s
      "no timing rider on any of them"
      (fmap ActivatedAbility.timing (CardT.activatedAbilities c))
      [ActivationTiming.AnyTime, ActivationTiming.AnyTime, ActivationTiming.AnyTime]
    Spec.assertEqWith
      s
      "each player draws, target player draws, target player mills twenty"
      (fmap (modeShapes . ActivatedAbility.modal) (CardT.activatedAbilities c))
      [ [(Optionality.Mandatory, [Effect.Draw PlayerRef.EachPlayer (Quantity.Literal 1)])],
        [(Optionality.Mandatory, [Effect.Draw (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 1)])],
        [(Optionality.Mandatory, [Effect.Mill (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 20)])]
      ]
  -- The first card file to spell a PlayerDiscards condition (CR 701.9a), and
  -- the first trigger condition at all whose payload is a PlayerRelation.
  -- Its "an opponent" is that relation and nothing else -- no Filter, no
  -- second exclusion mechanism -- and the effect reads CR 702.70a's existing
  -- "that player" slot rather than adding a spelling of its own.
  Spec.it s "megrim.json loads as a {2}{B} enchantment triggering on an opponent's discard" $ do
    c <- S.cardOf s registry "Megrim"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Megrim")
    Spec.assertEqWith
      s
      "{2}{B}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Black)]))
    Spec.assertEqWith
      s
      "Enchantment"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Enchantment) Set.empty)
    Spec.assertEqWith s "no power or toughness" (CardT.power c, CardT.toughness c) (Nothing, Nothing)
    Spec.assertEqWith
      s
      "one trigger, on an opponent discarding"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.PlayerDiscards PlayerRelation.Opponent]
    Spec.assertEqWith
      s
      "dealing 2 damage to the that-player slot"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[(Optionality.Mandatory, [Effect.DealDamage Binding.triggerPlayer (Quantity.Literal 2)])]]
    Spec.assertEqWith
      s
      "and it targets nothing"
      (fmap (fmap Mode.targetSpecs . Foldable.toList . Modal.modes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[Map.empty]]
  -- The pool's first CONTINUOUS effect over a filter-selected set (CR
  -- 611.2c). Day of Judgment's EachMatching feeds a one-shot; this one feeds
  -- an effect that is stored and keeps applying, so the sweep's RESULT has to
  -- be frozen at resolution -- see Pawl.ResolveSpec's TrumpetBlast group.
  --
  -- The filter spells "attacking creatures" as And [HasCardType Creature,
  -- IsAttacking] rather than IsAttacking alone: an EachMatching has no Pool
  -- to narrow it (CR 109.2 gives it the whole battlefield), so the card type
  -- the printed text names has to be in the filter. Kill Shot writes the same
  -- two halves as Pool.Creatures plus a filter, because a TargetSpec has a
  -- pool.
  Spec.it s "trumpet-blast.json loads as a {2}{R} instant pumping every attacking creature" $ do
    c <- S.cardOf s registry "Trumpet Blast"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Trumpet Blast")
    Spec.assertEqWith s "{2}{R}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
    Spec.assertEqWith
      s
      "Instant"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Instant) Set.empty)
    Spec.assertEqWith
      s
      "attacking creatures get +2/+0 until end of turn"
      (modeShapes (CardT.spell c))
      [ ( Optionality.Mandatory,
          [ Effect.ModifyTarget
              Duration.UntilEndOfTurn
              (Modification.ModifyPowerToughness (Quantity.Literal 2) (Quantity.Literal 0))
              (ObjectRef.EachMatching (Filter.And [Filter.HasCardType CardType.Creature, Filter.IsAttacking]))
          ]
        )
      ]
    -- CR 115.10a: no "target" anywhere on the card, so no target spec and
    -- nothing for CR 608.2b to fizzle.
    Spec.assertEqWith s "and it targets nothing" (fmap Mode.targetSpecs (Foldable.toList (Modal.modes (CardT.spell c)))) [Map.empty]
  -- The control-side twin of trumpet-blast.json, and the other half of what
  -- CR 611.2c names: a resolution effect that CHANGES THE CONTROLLER of a
  -- filter-selected set. Its duration is Indefinite because the card states
  -- none -- CR 611.2a: "If no duration is stated, it lasts until the end of
  -- the game" -- which is the one place this card differs from Act of
  -- Treason's UntilEndOfTurn.
  --
  -- The filter is a bare HasCardType Enchantment: the card says "all
  -- enchantments", with no "you don't control" and no "other", and the Thief
  -- itself is in a graveyard by the time the trigger resolves.
  Spec.it s "aura-thief.json loads as a {3}{U} 2/2 flying Illusion whose dies trigger takes every enchantment" $ do
    c <- S.cardOf s registry "Aura Thief"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Aura Thief")
    Spec.assertEqWith s "{3}{U}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
    Spec.assertEqWith
      s
      "Creature -- Illusion"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Illusion))
    Spec.assertEqWith s "2/2" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 2)))
    Spec.assertEqWith s "flying, and nothing else" (CardT.keywords c) (Set.singleton Keyword.Flying)
    Spec.assertEqWith
      s
      "one trigger, on dying"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfDies]
    Spec.assertEqWith
      s
      "gaining control of every enchantment, for good"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [ [ ( Optionality.Mandatory,
            [Effect.GainControl Duration.Indefinite (ObjectRef.EachMatching (Filter.HasCardType CardType.Enchantment))]
          )
        ]
      ]
  -- CR 500.1 / 500.11: the pool's first card to skip a phase that HAS steps.
  -- The whole point of the card is the second element of the SkipNextPhase
  -- payload: PhaseSelector.CombatPhase, which no Pawl.Types.Phase value can
  -- spell, next to Fatigue's PhaseSelector.Step just above.
  Spec.it s "stonehorn-dignitary.json loads as a {3}{W} 1/4 whose enters trigger skips a whole combat phase" $ do
    c <- S.cardOf s registry "Stonehorn Dignitary"
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Stonehorn Dignitary")
    Spec.assertEqWith s "{3}{W}" (CardT.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, ManaSymbol.OfType (ManaType.Colored Color.White)]))
    Spec.assertEqWith
      s
      "Creature -- Rhino Soldier"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Rhino, Subtype.Soldier]))
    Spec.assertEqWith s "1/4" (CardT.power c, CardT.toughness c) (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 4)))
    Spec.assertEqWith
      s
      "one trigger, on this creature entering"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfEnters]
    Spec.assertEqWith
      s
      "target opponent skips their next combat phase"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [ [ ( Optionality.Mandatory,
            [Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PhaseSelector.CombatPhase]
          )
        ]
      ]
    Spec.assertEqWith
      s
      "aimed at an OPPONENT, which is what makes the skip theirs and not yours"
      (fmap (fmap Mode.targetSpecs . Foldable.toList . Modal.modes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players (Just (Filter.IsPlayer PlayerRelation.Opponent)))]]
  -- The pool's first card whose mass effect has a RIDER reading the sweep
  -- back: "destroy all artifacts and enchantments. Put a +1/+1 counter on
  -- this creature for each permanent destroyed this way." The two halves are
  -- two ordinary opcodes joined by a binding slot -- the Destroy names
  -- "destroyed" and the PutCounters reads it as Quantity.InSlot -- so nothing
  -- about this card is a fused opcode.
  --
  -- The slot is a DEFINITION rather than a target spec, which is why the mode
  -- declares none: CR 115.10a, the word "target" is nowhere on the card.
  Spec.it s "bane-of-progress.json loads as a {4}{G}{G} Elemental whose sweep binds a count its rider reads" $ do
    c <- S.cardOf s registry "Bane of Progress"
    let destroyed = SlotName.MkSlotName (Text.pack "destroyed")
    Spec.assertEqWith s "name" (CardT.name c) (Text.pack "Bane of Progress")
    Spec.assertEqWith
      s
      "{4}{G}{G}"
      (CardT.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, ManaSymbol.OfType (ManaType.Colored Color.Green), ManaSymbol.OfType (ManaType.Colored Color.Green)]))
    Spec.assertEqWith
      s
      "Creature -- Elemental"
      (CardT.typeLine c)
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Elemental))
    Spec.assertEqWith
      s
      "2/2"
      (CardT.power c, CardT.toughness c)
      (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 2)))
    Spec.assertEqWith
      s
      "one trigger, on this creature entering"
      (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
      [TriggerCondition.SelfEnters]
    Spec.assertEqWith
      s
      "the sweep binds its count, and the rider reads that slot onto the source"
      (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [ [ ( Optionality.Mandatory,
            [ Effect.Destroy
                (ObjectRef.EachMatching (Filter.Or [Filter.HasCardType CardType.Artifact, Filter.HasCardType CardType.Enchantment]))
                Regenerability.Regenerable
                (Just destroyed),
              Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.InSlot destroyed) Binding.triggerSource
            ]
          )
        ]
      ]
    Spec.assertEqWith
      s
      "and it targets nothing: CR 115.10a, the card never says 'target'"
      (fmap (fmap Mode.targetSpecs . Foldable.toList . Modal.modes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
      [[Map.empty]]
    Spec.assertEqWith s "nothing of it is a static or a replacement" (CardT.staticAbilities c, CardT.replacementEffects c) ([], [])

checkFile :: Spec.Spec IO n -> FilePath -> Printing.Printing -> IO ()
checkFile s root p = do
  let slug = slugOf p
  let path = root <> "/" <> Text.unpack (Slug.unwrap slug) <> ".json"
  -- Read as bytes and decoded as UTF-8 explicitly, matching Pawl.Corpus.loadOne:
  -- Data.Text.IO.readFile decodes using the locale encoding, which is ASCII
  -- under LC_ALL=C, so this would otherwise die on khabal-ghoul.json's "á".
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> Spec.assertFailure s (path <> ": not valid UTF-8: " <> show err)
    Right contents ->
      case Json.parse contents of
        -- Unreachable: S.allPrintings would have failed in IO first.
        Left err -> Spec.assertFailure s (path <> ": " <> Text.unpack err)
        Right value ->
          -- The loader reads everything the file says and invents nothing:
          -- re-encoding the loaded printing reproduces the file's meaning. Compared
          -- up to key order and whitespace, because JSON objects are unordered and
          -- formatting is not part of the contract. The corpus is committed
          -- pretty-printed (`jq -S .`) while Json.render emits compact output, so
          -- this can never quietly regress into a byte comparison: every file would
          -- fail at once.
          Spec.assertEqWith s path (Json.sortKeys (printingToJson p)) (Json.sortKeys value)
