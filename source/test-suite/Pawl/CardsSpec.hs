-- Covers data/cards/*.json and Pawl.Slug.slugify.
module Pawl.CardsSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Binding as Binding
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
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
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

slugOf :: Printing.Printing -> Slug.Slug
slugOf = Slug.fromText . CardT.name . Printing.card

-- Each mode of a payload as (is it optional, what does it do) -- the shape the
-- CR 603.5 assertions below compare against.
modeShapes :: Modal.Modal CardT.Card -> [(Optionality.Optionality, [Effect.Effect CardT.Card])]
modeShapes m =
  fmap
    (\mode -> (Mode.optionality mode, Foldable.toList (Mode.effects mode)))
    (Foldable.toList (Modal.modes m))

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Pawl.CardsSpec"
    [ HU.testCase "each committed file re-parses to its compiled card (P3)" $ do
        ps <- S.allPrintings registry
        mapM_ (checkFile registry) ps,
      HU.testCase "clone.json loads as a 0/0 Shapeshifter with an EntryR AsCopy" $ do
        c <- Registry.card registry "Clone"
        HU.assertEqual "entry replacement" [ReplacementEffect.EntryR EntryRewrite.AsCopy] (CardT.replacementEffects c)
        HU.assertEqual "name" (Text.pack "Clone") (CardT.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Literal 0))) (CardT.power c),
      HU.testCase "serum-powder.json loads as a {3} artifact with a CR 103.5b mulligan action" $ do
        c <- Registry.card registry "Serum Powder"
        HU.assertEqual "name" (Text.pack "Serum Powder") (CardT.name c)
        HU.assertEqual "the CR 103.5b action" [Effect.ExileHandThenDraw] (CardT.mulliganAction c)
        HU.assertEqual "one activated ability, the {T}: Add {C} mana ability" 1 (length (CardT.activatedAbilities c)),
      -- The first card file whose keyword carries a payload that is not a
      -- number: rule 702.34a's flashback COST, which is where the whole ability
      -- lives -- Firebolt prints no alternativeCosts and no castingPermissions of
      -- its own, and Pawl.Keyword derives all three of rule 702.34a's
      -- consequences from this one value.
      HU.testCase "firebolt.json loads as a {R} Sorcery whose only keyword is flashback {4}{R}" $ do
        c <- Registry.card registry "Firebolt"
        HU.assertEqual "name" (Text.pack "Firebolt") (CardT.name c)
        HU.assertEqual
          "printed cost is {R}, unchanged by the flashback ability"
          (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (CardT.manaCost c)
        HU.assertEqual
          "one keyword: flashback {4}{R}"
          ( Set.singleton
              ( Keyword.Flashback
                  Cost.MkCost
                    { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, ManaSymbol.OfType (ManaType.Colored Color.Red)]),
                      Cost.components = []
                    }
              )
          )
          (CardT.keywords c)
        HU.assertEqual "no printed alternative cost" [] (CardT.alternativeCosts c)
        HU.assertEqual "no printed casting permission" [] (CardT.castingPermissions c),
      -- The first card file whose mode prints a "may" (CR 603.5), and so the
      -- first to carry an `optionality` key at all. Its SPELL half is mandatory
      -- in the same file, which is what proves the key is per-mode rather than
      -- per-card.
      HU.testCase "renewed-faith.json loads with an Optional cycling trigger and a Mandatory spell" $ do
        c <- Registry.card registry "Renewed Faith"
        HU.assertEqual "name" (Text.pack "Renewed Faith") (CardT.name c)
        HU.assertEqual
          "the spell gains 6 and is mandatory"
          [(Optionality.Mandatory, [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 6)])]
          (modeShapes (CardT.spell c))
        HU.assertEqual
          "the cycling trigger gains 2 and is optional"
          [[(Optionality.Optional, [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2)])]]
          (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c)),
      -- The first card file whose triggered ability functions somewhere other
      -- than the battlefield (CR 113.6k). Its "may" reuses renewed-faith.json's
      -- per-mode optionality key rather than adding a second spelling.
      HU.testCase "narcomoeba.json loads as a {1}{U} flying Illusion with an Optional graveyard trigger" $ do
        c <- Registry.card registry "Narcomoeba"
        HU.assertEqual "name" (Text.pack "Narcomoeba") (CardT.name c)
        HU.assertEqual "{1}{U}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Blue)])) (CardT.manaCost c)
        HU.assertEqual
          "Creature -- Illusion"
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Illusion))
          (CardT.typeLine c)
        HU.assertEqual "1/1" (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1))) (CardT.power c, CardT.toughness c)
        HU.assertEqual "flying, and nothing else" (Set.singleton Keyword.Flying) (CardT.keywords c)
        HU.assertEqual
          "the trigger watches library -> graveyard"
          [TriggerCondition.SelfPutIntoGraveyardFromLibrary]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        HU.assertEqual
          "and may put the card itself onto the battlefield"
          [[(Optionality.Optional, [Effect.MoveToZone Binding.triggerSource Zone.Battlefield])]]
          (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c)),
      HU.testCase "hanweir-garrison.json loads as a {2}{R} 2/3 whose attack trigger makes two tapped attacking Humans" $ do
        c <- Registry.card registry "Hanweir Garrison"
        HU.assertEqual "name" (Text.pack "Hanweir Garrison") (CardT.name c)
        HU.assertEqual "{2}{R}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Red)])) (CardT.manaCost c)
        HU.assertEqual
          "Creature -- Human Soldier"
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Human, Subtype.Soldier]))
          (CardT.typeLine c)
        HU.assertEqual "2/3" (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 3))) (CardT.power c, CardT.toughness c)
        -- CR 508.3a: "whenever this creature attacks" is the DECLARATION, which
        -- is the whole of this condition.
        HU.assertEqual
          "one trigger, on being declared as an attacker"
          [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        HU.assertEqual
          "two tokens, tapped and attacking"
          [Quantity.Literal 2]
          [q | ab <- CardT.triggeredAbilities c, Effect.Create q _ _ _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))]
        HU.assertEqual
          "the entry riders are the effect's, not the token's"
          [TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Tapped, TokenEntry.attacking = True}]
          [te | ab <- CardT.triggeredAbilities c, Effect.Create _ _ te _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))]
        -- Meld (CR 702.157) is not modelled: the printed reminder text says it
        -- melds with Hanweir Battlements, and neither the partner nor the melded
        -- permanent is in the pool (#369).
        HU.assertEqual "no keywords" Set.empty (CardT.keywords c),
      -- The first card file whose triggered ability watches the battlefield ->
      -- graveyard pair (CR 603.6c through CR 700.4's "dies"), and the mirror of
      -- narcomoeba.json's library -> graveyard one above.
      HU.testCase "doomed-traveler.json loads as a {W} 1/1 whose dies trigger makes a flying white Spirit" $ do
        c <- Registry.card registry "Doomed Traveler"
        HU.assertEqual "name" (Text.pack "Doomed Traveler") (CardT.name c)
        HU.assertEqual "{W}" (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White)])) (CardT.manaCost c)
        HU.assertEqual
          "Creature -- Human Soldier"
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Human, Subtype.Soldier]))
          (CardT.typeLine c)
        HU.assertEqual "1/1" (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1))) (CardT.power c, CardT.toughness c)
        HU.assertEqual "no keywords of its own" Set.empty (CardT.keywords c)
        HU.assertEqual
          "one trigger, on dying"
          [TriggerCondition.SelfDies]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        case [(q, tc) | ab <- CardT.triggeredAbilities c, Effect.Create q tc _ _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))] of
          [(quantity, token)] -> do
            HU.assertEqual "one token" (Quantity.Literal 1) quantity
            HU.assertEqual "named Spirit" (Text.pack "Spirit") (CardT.name token)
            HU.assertEqual
              "Creature -- Spirit"
              (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Spirit))
              (CardT.typeLine token)
            HU.assertEqual "1/1" (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1))) (CardT.power token, CardT.toughness token)
            HU.assertEqual "with flying" (Set.singleton Keyword.Flying) (CardT.keywords token)
            -- CR 202.2e: "An object may have a color indicator printed to the
            -- left of the type line. That object is each color denoted by that
            -- color indicator." A token has no mana cost, so CR 202.2's
            -- mana-symbol rule would leave it colorless (CR 202.2b); the
            -- indicator is what makes this one white.
            HU.assertEqual "and white by colour indicator" (Set.singleton Color.White) (CardT.colorIndicator token)
          other -> HU.assertFailure ("expected exactly one Create, got " <> show (length other)),
      -- The pool's first card whose dies trigger acts on ITSELF, and so the
      -- first that has to tell CR 113.7a's source apart from CR 400.7e's "new
      -- object that it became". The effect is narcomoeba.json's opcode with a
      -- different slot, which is precisely the distinction: that card's "self"
      -- IS the arriving graveyard card, and this one's is not.
      HU.testCase "endless-cockroaches.json loads as a {1}{B}{B} 1/1 whose dies trigger returns the card it became to hand" $ do
        c <- Registry.card registry "Endless Cockroaches"
        HU.assertEqual "name" (Text.pack "Endless Cockroaches") (CardT.name c)
        HU.assertEqual
          "{1}{B}{B}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black), ManaSymbol.OfType (ManaType.Colored Color.Black)]))
          (CardT.manaCost c)
        HU.assertEqual
          "Creature -- Insect"
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.singleton Subtype.Insect))
          (CardT.typeLine c)
        HU.assertEqual "1/1" (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1))) (CardT.power c, CardT.toughness c)
        HU.assertEqual "no keywords" Set.empty (CardT.keywords c)
        HU.assertEqual
          "one trigger, on dying"
          [TriggerCondition.SelfDies]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        HU.assertEqual "and no intervening if" [Nothing] (fmap TriggeredAbility.intervening (CardT.triggeredAbilities c))
        HU.assertEqual
          "returning the became slot to its owner's hand"
          [[(Optionality.Mandatory, [Effect.MoveToZone Binding.became Zone.Hand])]]
          (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c)),
      -- The pool's first INTERVENING "if" on a look-back trigger (CR 603.4 read
      -- against CR 608.2h last known information), and the first condition whose
      -- measured side is not a Count at all.
      HU.testCase "deathknell-berserker.json loads as a {1}{B} 2/2 whose dies trigger is gated on its own power" $ do
        c <- Registry.card registry "Deathknell Berserker"
        HU.assertEqual "name" (Text.pack "Deathknell Berserker") (CardT.name c)
        HU.assertEqual "{1}{B}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black)])) (CardT.manaCost c)
        HU.assertEqual
          "Creature -- Elf Berserker"
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Elf, Subtype.Berserker]))
          (CardT.typeLine c)
        HU.assertEqual "2/2" (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 2))) (CardT.power c, CardT.toughness c)
        HU.assertEqual
          "one trigger, on dying"
          [TriggerCondition.SelfDies]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        HU.assertEqual
          "gated on its own power being 3 or greater"
          [Just (Condition.MkCondition Quantity.Power Comparison.AtLeast (Quantity.Literal 3))]
          (fmap TriggeredAbility.intervening (CardT.triggeredAbilities c))
        case [(q, tc) | ab <- CardT.triggeredAbilities c, Effect.Create q tc _ _ <- concatMap snd (modeShapes (TriggeredAbility.modal ab))] of
          [(quantity, token)] -> do
            HU.assertEqual "one token" (Quantity.Literal 1) quantity
            HU.assertEqual "named Zombie Berserker" (Text.pack "Zombie Berserker") (CardT.name token)
            HU.assertEqual
              "Creature -- Zombie Berserker"
              (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Zombie, Subtype.Berserker]))
              (CardT.typeLine token)
            HU.assertEqual "2/2" (Just (Power.MkPower (Quantity.Literal 2)), Just (Toughness.MkToughness (Quantity.Literal 2))) (CardT.power token, CardT.toughness token)
            HU.assertEqual "no keywords" Set.empty (CardT.keywords token)
            -- CR 202.2b/202.2e, exactly as doomed-traveler.json's Spirit: a
            -- token has no mana cost, so only the colour indicator makes it
            -- black.
            HU.assertEqual "and black by colour indicator" (Set.singleton Color.Black) (CardT.colorIndicator token)
          other -> HU.assertFailure ("expected exactly one Create, got " <> show (length other)),
      -- CR 702.19 trample plus the CR 510.1b combat-damage-to-a-player trigger
      -- condition on one card, which is what makes the trigger's event and the
      -- bearer's death land in a single CR 117.5 batch. The 1 toughness is
      -- load-bearing and pinned here so a future edit cannot quietly make the
      -- Skelemental survive its blocker: TriggerSpec's bystander group would
      -- then prove nothing.
      HU.testCase "lightning-skelemental.json loads as a {B}{R}{R} 6/1 trampler that makes the damaged player discard two" $ do
        c <- Registry.card registry "Lightning Skelemental"
        HU.assertEqual "name" (Text.pack "Lightning Skelemental") (CardT.name c)
        HU.assertEqual
          "{B}{R}{R}"
          (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Black), ManaSymbol.OfType (ManaType.Colored Color.Red), ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (CardT.manaCost c)
        HU.assertEqual "6/1" (Just (Power.MkPower (Quantity.Literal 6)), Just (Toughness.MkToughness (Quantity.Literal 1))) (CardT.power c, CardT.toughness c)
        HU.assertEqual "trample and haste" (Set.fromList [Keyword.Trample, Keyword.Haste]) (CardT.keywords c)
        HU.assertEqual
          "Creature -- Elemental Skeleton"
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Elemental, Subtype.Skeleton]))
          (CardT.typeLine c)
        HU.assertEqual
          "two triggers: the combat-damage one and the end-step sacrifice"
          [ TriggerCondition.SelfDealsCombatDamageToPlayer,
            TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn
          ]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        HU.assertEqual
          -- The reserved "that player" slot, read by a card for the first time
          -- rather than by CR 702.70a's poisonous: the discard names the slot
          -- Event.eventBindings stamps for the CR 510.1b combat-damage-to-a-
          -- player condition, not a target and not the controller.
          "the damaged player discards two, then the Skelemental sacrifices itself"
          [ [(Optionality.Mandatory, [Effect.Discard Binding.triggerPlayer (Quantity.Literal 2)])],
            [(Optionality.Mandatory, [Effect.Sacrifice Binding.triggerSource])]
          ]
          (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c)),
      HU.testCase "leyline-of-the-void.json loads with a CR 103.6a action and an Opponents redirect" $ do
        c <- Registry.card registry "Leyline of the Void"
        HU.assertEqual "name" (Text.pack "Leyline of the Void") (CardT.name c)
        HU.assertEqual
          "the CR 103.6a action puts itself onto the battlefield"
          [Effect.MoveToZone Binding.triggerSource Zone.Battlefield]
          (CardT.openingHandAction c)
        HU.assertEqual
          "and the redirect is scoped to an opponent's graveyard"
          [ ReplacementEffect.ZoneChangeR
              (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents ZoneChangeSubject.AnyObject)
              Zone.Exile
          ]
          (CardT.replacementEffects c),
      -- CR 614.1b: the first card in the pool whose replacement effect is a
      -- SKIP. Nothing about Eon Hub is a static ability -- the whole card is one
      -- replacement -- which is the correction this file's presence records.
      HU.testCase "eon-hub.json loads as a {5} artifact whose only ability is a PhaseR skip" $ do
        c <- Registry.card registry "Eon Hub"
        HU.assertEqual "name" (Text.pack "Eon Hub") (CardT.name c)
        HU.assertEqual "{5}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 5])) (CardT.manaCost c)
        HU.assertEqual
          -- whosePhase = Nothing is the SYMMETRY: "PLAYERS skip their upkeep
          -- steps" names nobody, so the pattern reads no PlayerId and the skip
          -- takes every player's upkeep. Fatigue is the Just.
          "players skip their upkeep steps"
          [ ReplacementEffect.PhaseR
              PhasePattern.MkPhasePattern
                { PhasePattern.whichPhase = Phase.Beginning BeginningStep.Upkeep,
                  PhasePattern.whosePhase = Nothing
                }
          ]
          (CardT.replacementEffects c)
        HU.assertEqual "and it is not a continuous effect" [] (CardT.staticAbilities c),
      -- CR 614.10a: the first card whose skip is created by an EFFECT rather
      -- than printed on a permanent. Nothing is in `replacementEffects` -- a
      -- sorcery has no ability that exists on the battlefield -- which is the
      -- structural contrast with Eon Hub just above.
      HU.testCase "fatigue.json loads as a {1}{U} sorcery whose only effect is a SkipNextPhase" $ do
        c <- Registry.card registry "Fatigue"
        HU.assertEqual "name" (Text.pack "Fatigue") (CardT.name c)
        HU.assertEqual "{1}{U}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Blue)])) (CardT.manaCost c)
        HU.assertEqual
          "target player skips their next draw step"
          [ ( Optionality.Mandatory,
              [Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Phase.Beginning BeginningStep.DrawStep)]
            )
          ]
          (modeShapes (CardT.spell c))
        HU.assertEqual "nothing of it survives on the battlefield" [] (CardT.replacementEffects c),
      -- CR 500.7: the pool's one creator of an extra turn. "Target player takes
      -- an extra turn after this one" -- so the recipient is a TARGET, which is
      -- what makes an opponent's extra turn expressible at all.
      HU.testCase "time-warp.json loads as a {3}{U}{U} sorcery whose only effect is a TakeExtraTurn" $ do
        c <- Registry.card registry "Time Warp"
        HU.assertEqual "name" (Text.pack "Time Warp") (CardT.name c)
        HU.assertEqual
          "{3}{U}{U}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, ManaSymbol.OfType (ManaType.Colored Color.Blue), ManaSymbol.OfType (ManaType.Colored Color.Blue)]))
          (CardT.manaCost c)
        HU.assertEqual
          "target player takes an extra turn after this one"
          [(Optionality.Mandatory, [Effect.TakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target")))])]
          (modeShapes (CardT.spell c))
        HU.assertEqual "nothing of it survives on the battlefield" [] (CardT.replacementEffects c),
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
      HU.testCase "aether-flash.json loads as a {2}{R}{R} enchantment dealing 2 to the creature that entered" $ do
        c <- Registry.card registry "Aether Flash"
        HU.assertEqual "name" (Text.pack "Aether Flash") (CardT.name c)
        HU.assertEqual
          "{2}{R}{R}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Red), ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (CardT.manaCost c)
        HU.assertEqual
          "Enchantment"
          (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Enchantment) Set.empty)
          (CardT.typeLine c)
        HU.assertEqual "no power or toughness" (Nothing, Nothing) (CardT.power c, CardT.toughness c)
        HU.assertEqual
          "one trigger, on any creature entering"
          [TriggerCondition.PermanentEnters (Filter.HasCardType CardType.Creature)]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        HU.assertEqual
          "dealing 2 damage to the became slot"
          [[(Optionality.Mandatory, [Effect.DealDamage Binding.became (Quantity.Literal 2)])]]
          (fmap (modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c))
        HU.assertEqual
          "and it targets nothing"
          [[Map.empty]]
          (fmap (fmap Mode.targetSpecs . Foldable.toList . Modal.modes . TriggeredAbility.modal) (CardT.triggeredAbilities c)),
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
      HU.testCase "bitterblossom.json loads as a {1}{B} Kindred Enchantment - Faerie whose upkeep trigger costs 1 life and makes a Faerie Rogue" $ do
        c <- Registry.card registry "Bitterblossom"
        HU.assertEqual "name" (Text.pack "Bitterblossom") (CardT.name c)
        HU.assertEqual "{1}{B}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black)])) (CardT.manaCost c)
        HU.assertEqual
          "Kindred Enchantment - Faerie"
          (TypeLine.MkTypeLine Set.empty (Set.fromList [CardType.Kindred, CardType.Enchantment]) (Set.singleton Subtype.Faerie))
          (CardT.typeLine c)
        HU.assertEqual "no power or toughness" (Nothing, Nothing) (CardT.power c, CardT.toughness c)
        HU.assertEqual "no keywords" Set.empty (CardT.keywords c)
        -- CR 603.3a / 109.5: "your upkeep" is the ability CONTROLLER's, which is
        -- what TurnScope.ControllersTurn spells.
        HU.assertEqual
          "one trigger, at the beginning of its controller's upkeep"
          [TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn]
          (fmap TriggeredAbility.condition (CardT.triggeredAbilities c))
        case concatMap (concatMap snd . modeShapes . TriggeredAbility.modal) (CardT.triggeredAbilities c) of
          [Effect.LoseLife who amount, Effect.Create quantity token entry slot] -> do
            -- Printed order, and it is the order the effects are authored in:
            -- "you lose 1 life AND create".
            HU.assertEqual "its controller loses the life" (PlayerRef.Relative PlayerRelation.You) who
            HU.assertEqual "1 life" (Quantity.Literal 1) amount
            HU.assertEqual "one token" (Quantity.Literal 1) quantity
            HU.assertEqual "with no entry riders" TokenEntry.MkTokenEntry {TokenEntry.tapped = TapState.Untapped, TokenEntry.attacking = False} entry
            HU.assertEqual "and no slot bound to it" Nothing slot
            -- CR 111.4: Bitterblossom names no token, so the name is the
            -- subtypes plus the word "Token" -- the rule's own example is
            -- "Dwarf Berserker Token".
            HU.assertEqual "named Faerie Rogue Token" (Text.pack "Faerie Rogue Token") (CardT.name token)
            HU.assertEqual
              "Creature - Faerie Rogue"
              (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) (Set.fromList [Subtype.Rogue, Subtype.Faerie]))
              (CardT.typeLine token)
            HU.assertEqual "1/1" (Just (Power.MkPower (Quantity.Literal 1)), Just (Toughness.MkToughness (Quantity.Literal 1))) (CardT.power token, CardT.toughness token)
            HU.assertEqual "with flying" (Set.singleton Keyword.Flying) (CardT.keywords token)
            -- CR 202.2b/202.2e, exactly as doomed-traveler.json's Spirit: a
            -- token has no mana cost, so only the colour indicator makes it
            -- black.
            HU.assertEqual "and black by colour indicator" (Set.singleton Color.Black) (CardT.colorIndicator token)
          other -> HU.assertFailure ("expected exactly [LoseLife, Create], got " <> show (length other) <> " effects")
    ]

checkFile :: Registry.Type.Registry -> Printing.Printing -> HU.Assertion
checkFile registry p = do
  let slug = slugOf p
  let path = Registry.Type.root registry <> "/" <> Text.unpack (Slug.unwrap slug) <> ".json"
  -- Read as bytes and decoded as UTF-8 explicitly, matching Pawl.Registry.load:
  -- Data.Text.IO.readFile decodes using the locale encoding, which is ASCII
  -- under LC_ALL=C, so this would otherwise die on khabal-ghoul.json's "á".
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> HU.assertFailure (path <> ": not valid UTF-8: " <> show err)
    Right contents ->
      case Json.parse contents of
        -- Unreachable: S.allPrintings would have failed in IO first.
        Left err -> HU.assertFailure (path <> ": " <> Text.unpack err)
        Right value ->
          -- The loader reads everything the file says and invents nothing:
          -- re-encoding the loaded printing reproduces the file's meaning. Compared
          -- up to key order and whitespace, because JSON objects are unordered and
          -- formatting is not part of the contract. The corpus is committed
          -- pretty-printed (`jq -S .`) while Json.render emits compact output, so
          -- this can never quietly regress into a byte comparison: every file would
          -- fail at once.
          HU.assertEqual path (Json.sortKeys value) (Json.sortKeys (Codec.printingToJson p))
