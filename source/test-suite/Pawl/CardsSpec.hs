-- Covers data/cards/*.json and Pawl.Slug.slugify.
module Pawl.CardsSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Binding as Binding
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Support as S
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.Cost as Cost
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EntryRewrite as EntryRewrite
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.Optionality as Optionality
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PhasePattern as PhasePattern
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Slug as Slug.Type
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TokenEntry as TokenEntry
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggerFrequency as TriggerFrequency
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Type.ZoneChangeSubject as ZoneChangeSubject
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

slugOf :: Printing.Printing -> Maybe Slug.Type.Slug
slugOf = Slug.slugify . CardT.name . Printing.card

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
          "players skip their upkeep steps"
          [ReplacementEffect.PhaseR PhasePattern.MkPhasePattern {PhasePattern.whichPhase = Phase.Beginning BeginningStep.Upkeep}]
          (CardT.replacementEffects c)
        HU.assertEqual "and it is not a continuous effect" [] (CardT.staticAbilities c)
    ]

checkFile :: Registry.Type.Registry -> Printing.Printing -> HU.Assertion
checkFile registry p =
  case slugOf p of
    -- Unreachable: every committed card's name slugifies, since Registry.card
    -- already had to slugify it (via Pawl.Slug.slugify) to fetch this printing.
    Nothing -> HU.assertFailure (Text.unpack (CardT.name (Printing.card p)) <> ": does not slugify")
    Just slug -> do
      let path = Registry.Type.root registry <> "/" <> Text.unpack (Slug.Type.toText slug) <> ".json"
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
