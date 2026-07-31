-- Covers Pawl.Card: card data, type-line rules, every printing, and the D4
-- dataflow lint.
module Pawl.CardSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Event as Event
-- The logic module, alongside Pawl.Types.Modal below: unambiguous under one
-- alias because the two modules export disjoint names (TriggerSpec's
-- precedent), and Modal.allEffects is how this lint reaches an activated or
-- triggered ability's effects (Card.allEffects only reaches the spell).
import qualified Pawl.Mana as Mana
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Slug as Slug
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
-- Aliased Condition.Type, matching Pawl.Types.Count below and the project-wide
-- convention (FilterSpec/CardSpec's Filter.Type note): Pawl.Condition may
-- later be imported and must not collide.
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layer as Layer
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
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone
import qualified System.Directory as Directory
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Not red-specific despite its first callers: just the Maybe wrapper every
-- printed mana cost needs (CR 202.1).
costOf :: [ManaSymbol.ManaSymbol] -> Maybe ManaCost.ManaCost
costOf symbols = Just (ManaCost.MkManaCost symbols)

m2aCardTests :: Registry.Type.Registry -> Tasty.TestTree
m2aCardTests registry =
  let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      green = ManaSymbol.OfType (ManaType.Colored Color.Green)
      black = ManaSymbol.OfType (ManaType.Colored Color.Black)
   in Tasty.testGroup
        "M2aCards"
        [ HU.testCase "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
            birdMaiden <- Registry.printing registry "Bird Maiden"
            let c = Printing.card birdMaiden
            HU.assertEqual "name" (Text.pack "Bird Maiden") (Card.Type.name c)
            HU.assertEqual "cost" (costOf [ManaSymbol.Generic 2, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c)
            HU.assertEqual
              "subtypes"
              (Set.fromList [Subtype.Human, Subtype.Bird])
              (TypeLine.subtypes (Card.Type.typeLine c)),
          HU.testCase "Nimble Birdsticker is a {2}{R} 2/3 Goblin with reach" $ do
            nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
            let c = Printing.card nimbleBirdsticker
            HU.assertEqual "name" (Text.pack "Nimble Birdsticker") (Card.Type.name c)
            HU.assertEqual "cost" (costOf [ManaSymbol.Generic 2, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c),
          HU.testCase "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
            ogreSentry <- Registry.printing registry "Ogre Sentry"
            let c = Printing.card ogreSentry
            HU.assertEqual "name" (Text.pack "Ogre Sentry") (Card.Type.name c)
            HU.assertEqual "cost" (costOf [ManaSymbol.Generic 1, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c),
          HU.testCase "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
            windseekerCentaur <- Registry.printing registry "Windseeker Centaur"
            let c = Printing.card windseekerCentaur
            HU.assertEqual "name" (Text.pack "Windseeker Centaur") (Card.Type.name c)
            HU.assertEqual "cost" (costOf [ManaSymbol.Generic 1, red, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c),
          HU.testCase "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
            goblinChariot <- Registry.printing registry "Goblin Chariot"
            let c = Printing.card goblinChariot
            HU.assertEqual "name" (Text.pack "Goblin Chariot") (Card.Type.name c)
            HU.assertEqual "cost" (costOf [ManaSymbol.Generic 2, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c),
          HU.testCase "Glistener Elf is a {G} 1/1 Phyrexian Elf Warrior with infect" $ do
            glistenerElf <- Registry.printing registry "Glistener Elf"
            let c = Printing.card glistenerElf
            HU.assertEqual "name" (Text.pack "Glistener Elf") (Card.Type.name c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
            HU.assertBool "has infect" (elem Keyword.Infect (Card.Type.keywords c))
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Phyrexian, Subtype.Elf, Subtype.Warrior]) (TypeLine.subtypes (Card.Type.typeLine c)),
          HU.testCase "Branchblight Stalker is a {1}{G} 3/1 Phyrexian Elf Scout with toxic 2" $ do
            stalker <- Registry.printing registry "Branchblight Stalker"
            let c = Printing.card stalker
            HU.assertEqual "name" (Text.pack "Branchblight Stalker") (Card.Type.name c)
            HU.assertEqual "cost" (costOf [ManaSymbol.Generic 1, green]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
            HU.assertEqual "toxic 2, and nothing else" (Set.singleton (Keyword.Toxic 2)) (Card.Type.keywords c)
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Phyrexian, Subtype.Elf, Subtype.Scout]) (TypeLine.subtypes (Card.Type.typeLine c)),
          -- "Enchanted creature has poisonous 3", so the keyword is on the
          -- Aura's layer-6 GRANT and not in its own printed keyword set -- the
          -- distinction the assertions below draw.
          HU.testCase "Snake Cult Initiation is a {3}{B} Aura granting poisonous 3" $ do
            initiation <- Registry.printing registry "Snake Cult Initiation"
            let c = Printing.card initiation
            HU.assertEqual "name" (Text.pack "Snake Cult Initiation") (Card.Type.name c)
            HU.assertEqual "cost" (costOf [ManaSymbol.Generic 3, black]) (Card.Type.manaCost c)
            HU.assertBool "is an Aura" (Card.isAura c)
            HU.assertEqual "no printed keywords of its own" Set.empty (Card.Type.keywords c)
            HU.assertEqual
              "one static ability: the enchanted creature gains poisonous 3"
              [StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton (Modification.GainKeyword (Keyword.Poisonous 3)))]
              (Card.Type.staticAbilities c),
          HU.testCase "every M2a printing carries exactly its keyword" $
            mapM_
              ( \(name, keyword) -> do
                  p <- Registry.printing registry name
                  let c = Printing.card p
                  HU.assertBool "creature" (Card.isCreature c)
                  HU.assertBool "not land" (not (Card.isLand c))
                  HU.assertEqual "exactly this keyword" (Set.singleton keyword) (Card.Type.keywords c)
              )
              S.m2aKeywords
        ]

cardTests :: Registry.Type.Registry -> Tasty.TestTree
cardTests registry =
  Tasty.testGroup
    "Card"
    [ HU.testCase "Mountain printing is named Mountain" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertEqual "name" (Text.pack "Mountain") (Card.Type.name (Printing.card mountain)),
      HU.testCase "Mountain is a Land" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertBool "isLand" (Card.isLand (Printing.card mountain)),
      HU.testCase "Mountain has the Mountain subtype" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertBool "subtype" (Set.member Subtype.Mountain (TypeLine.subtypes (Card.Type.typeLine (Printing.card mountain)))),
      HU.testCase "Mountain type line contains Land" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertBool "cardtype" (Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card mountain)))),
      -- CR 202.1: a land has no mana cost. Not a zero cost -- no cost at all.
      HU.testCase "Mountain has no mana cost" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertEqual "no cost" Nothing (Card.Type.manaCost (Printing.card mountain)),
      HU.testCase "Mountain has no power or toughness" $ do
        mountain <- Registry.printing registry "Mountain"
        HU.assertEqual "power" Nothing (Card.Type.power (Printing.card mountain))
        HU.assertEqual "toughness" Nothing (Card.Type.toughness (Printing.card mountain)),
      HU.testCase "Piker printing is named Goblin Piker" $ do
        piker <- Registry.printing registry "Goblin Piker"
        HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.Type.name (Printing.card piker)),
      HU.testCase "Piker costs {1}{R}" $ do
        piker <- Registry.printing registry "Goblin Piker"
        HU.assertEqual
          "cost"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (Card.Type.manaCost (Printing.card piker)),
      HU.testCase "Piker is a 2/1" $ do
        piker <- Registry.printing registry "Goblin Piker"
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (Printing.card piker))
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (Printing.card piker)),
      HU.testCase "Piker is a Goblin Warrior" $ do
        piker <- Registry.printing registry "Goblin Piker"
        HU.assertEqual
          "subtypes"
          (Set.fromList [Subtype.Goblin, Subtype.Warrior])
          (TypeLine.subtypes (Card.Type.typeLine (Printing.card piker))),
      HU.testCase "Piker is a creature and not a land" $ do
        piker <- Registry.printing registry "Goblin Piker"
        HU.assertBool "creature" (Card.isCreature (Printing.card piker))
        HU.assertBool "not land" (not (Card.isLand (Printing.card piker))),
      -- CR 110.1: the classification resolution turns on. Never card identity.
      HU.testCase "CR 110.1 both a Piker and a Mountain are permanents" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mountain <- Registry.printing registry "Mountain"
        HU.assertBool "piker" (Card.isPermanent (Printing.card piker))
        HU.assertBool "mountain" (Card.isPermanent (Printing.card mountain)),
      HU.testCase "CR 110.1 an instant is not a permanent type" $
        let instantLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Instant,
                  TypeLine.subtypes = Set.empty
                }
            card =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "Some Instant",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine = instantLine,
                  Card.Type.power = Nothing,
                  Card.Type.toughness = Nothing,
                  Card.Type.keywords = Set.empty,
                  Card.Type.colorIndicator = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.spell = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.blockRequirements = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing,
                  Card.Type.counterability = Counterability.Counterable
                }
         in do
              HU.assertBool "not a permanent" (not (Card.isPermanent card))
              HU.assertBool "an instant" (Card.isInstant card),
      HU.testCase "a Piker is not an instant" $ do
        piker <- Registry.printing registry "Goblin Piker"
        HU.assertBool "creature" (not (Card.isInstant (Printing.card piker)))
    ]

-- Every Count reachable from a Quantity: a leaf Count directly, or one nested
-- through Plus's two children (CR 208.2 composition -- a printed 1+*).
quantityCounts :: Quantity.Type.Quantity -> [Count.Type.Count Quantity.Type.Quantity]
quantityCounts quantity = case quantity of
  Quantity.Type.Literal _ -> []
  Quantity.Type.ManaValue -> []
  Quantity.Type.Power -> []
  Quantity.Type.X -> []
  Quantity.Type.Star -> []
  Quantity.Type.Plus a b -> quantityCounts a <> quantityCounts b
  Quantity.Type.Count count -> count : countCounts count

-- Every Count nested inside another Count's AGGREGATION: only Greatest carries
-- a per-member Quantity, and that Quantity may itself be a Count. Without this
-- descent the shared-zone lint below would sweep past a misauthored inner
-- scope.
countCounts :: Count.Type.Count Quantity.Type.Quantity -> [Count.Type.Count Quantity.Type.Quantity]
countCounts (Count.Type.MkCount _ _ aggregation) = case aggregation of
  Aggregation.Objects -> []
  Aggregation.DistinctCardTypes -> []
  Aggregation.Greatest quantity -> quantityCounts quantity

-- Every Count reachable from a Condition: both sides are Quantities, and either
-- may embed one (Pawl.Types.Condition).
conditionCounts :: Condition.Type.Condition -> [Count.Type.Count Quantity.Type.Quantity]
conditionCounts (Condition.Type.MkCondition measured _ threshold) =
  quantityCounts measured <> quantityCounts threshold

-- Every Count reachable from a Duration: only ForAsLongAs (CR 611.2b) carries
-- a Condition.
durationCounts :: Duration.Duration -> [Count.Type.Count Quantity.Type.Quantity]
durationCounts duration = case duration of
  Duration.UntilEndOfTurn -> []
  Duration.Indefinite -> []
  Duration.UntilYourNextTurn -> []
  Duration.ForAsLongAs condition -> conditionCounts condition

-- Every Count reachable from a Modification: only its P/T quantities
-- (layers 7b/7c) carry one.
modificationCounts :: Modification.Modification -> [Count.Type.Count Quantity.Type.Quantity]
modificationCounts modification = case modification of
  Modification.GainKeyword _ -> []
  Modification.LoseAllAbilities -> []
  Modification.SetBasePowerToughness p t -> quantityCounts p <> quantityCounts t
  Modification.ModifyPowerToughness p t -> quantityCounts p <> quantityCounts t
  Modification.SetLandSubtype _ -> []
  Modification.AddLandSubtype _ -> []
  Modification.AddCardType _ -> []
  Modification.ChangeSubtypeWord _ _ -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.SwitchPowerToughness -> []

-- Every Count reachable from a TriggerCondition: only StateIs (CR 603.8, a
-- trigger's own condition) carries one.
triggerConditionCounts :: TriggerCondition.TriggerCondition -> [Count.Type.Count Quantity.Type.Quantity]
triggerConditionCounts triggerCondition = case triggerCondition of
  TriggerCondition.SelfEnters -> []
  -- CR 603.6a's Filter is a predicate over the entering permanent, and a
  -- Filter holds no Count (Pawl.Types.Filter's atoms are all characteristics).
  TriggerCondition.PermanentEnters _ -> []
  TriggerCondition.StepBegins _ _ -> []
  TriggerCondition.StateIs condition -> conditionCounts condition
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []
  TriggerCondition.SelfAttacks _ -> []
  TriggerCondition.SelfCycled -> []
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> []
  TriggerCondition.SelfDies -> []

-- Every Count reachable from one effect: its own Quantity/Duration fields,
-- and -- for Create/CreateEmblem -- every Count in the embedded token/emblem
-- card (the same nesting Pawl.Codec's round trip walks).
effectCounts :: Effect.Effect Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
effectCounts effect = case effect of
  Effect.DealDamage _ quantity -> quantityCounts quantity
  Effect.ModifyTarget duration modification _ -> durationCounts duration <> modificationCounts modification
  Effect.ChangeText _ -> []
  Effect.AddMana _ -> []
  Effect.Search _ _ -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices _ _ quantity -> quantityCounts quantity
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone _ _ -> []
  Effect.Draw _ quantity -> quantityCounts quantity
  Effect.Mill _ quantity -> quantityCounts quantity
  Effect.Discard _ quantity -> quantityCounts quantity
  Effect.LoseLife _ quantity -> quantityCounts quantity
  Effect.GainLife _ quantity -> quantityCounts quantity
  Effect.Create quantity card _ _ -> quantityCounts quantity <> cardCounts card
  Effect.Replace duration _ _ -> durationCounts duration
  -- CR 614.10a's "next" is a use count, not a Duration and not a Quantity.
  Effect.SkipNextPhase _ _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.Counter _ -> []
  Effect.PutCounters _ quantity _ -> quantityCounts quantity
  Effect.GainPlayerCounters _ _ quantity -> quantityCounts quantity
  Effect.Untap _ -> []
  Effect.AddPhases _ -> []
  Effect.GainControl duration _ -> durationCounts duration
  Effect.ArmDelayedTrigger _ _ -> []
  Effect.AffectPlayers duration _ _ -> durationCounts duration
  Effect.CreateEmblem card -> cardCounts card
  Effect.BecomeMonarch _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.PlaySubgame _ -> []

-- Every Count reachable from one triggered ability (a card's own, or a
-- delayed one -- both TriggeredAbility Card): its TriggerCondition, its
-- intervening "if" clause, and its modes' effects.
triggeredAbilityCounts :: TriggeredAbility.TriggeredAbility Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
triggeredAbilityCounts ability =
  triggerConditionCounts (TriggeredAbility.condition ability)
    <> foldMap conditionCounts (TriggeredAbility.intervening ability)
    <> concatMap effectCounts (Modal.allEffects (TriggeredAbility.modal ability))

-- Every Count reachable from a card: every site a Pawl.Types.Count can be
-- authored -- Quantity (characteristic-defining P/T, printed P/T, and every
-- effect/modification quantity), Condition (a trigger's own condition, a
-- triggered ability's intervening clause, and a ForAsLongAs duration), and
-- every effect (spell, activated, triggered, delayed), recursing into a
-- minted token or emblem.
--
-- This traversal is hand-maintained, not derived, so it is NOT enforced
-- exhaustive by -Werror the way the Zone/Effect/Modification cases inside it
-- are: a NEW Card field, or a new CostComponent/PlayerEffect arm, that can carry
-- a Quantity or Count would bypass this lint silently. When you add a field that
-- can hold either, add it here.
-- Every effect a card can RESOLVE: its spell's modes, its activated and
-- triggered abilities' modes, and its delayed abilities' modes. Deliberately
-- wider than Card.allEffects (spell modes only) -- a stored continuous effect
-- can be created from any of these, which is what the control lint below is
-- about. Static abilities are absent on purpose: a static ability's
-- modification is never stored.
--
-- Hand-maintained, with cardCounts' caveat: a NEW Card field holding effects
-- must be added here too.
cardResolutionEffects :: Card.Type.Card -> [Effect.Effect Card.Type.Card]
cardResolutionEffects card =
  Card.allEffects card
    <> concatMap (Modal.allEffects . ActivatedAbility.modal) (Card.Type.activatedAbilities card)
    <> concatMap (Modal.allEffects . TriggeredAbility.modal) (Card.Type.triggeredAbilities card)
    <> concatMap (Modal.allEffects . TriggeredAbility.modal) (Map.elems (Card.Type.delayedAbilities card))

cardCounts :: Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
cardCounts card =
  concatMap quantityCounts (Maybe.maybeToList (Card.Type.characteristicPT card))
    <> concatMap (\(Power.MkPower quantity) -> quantityCounts quantity) (Maybe.maybeToList (Card.Type.power card))
    <> concatMap (\(Toughness.MkToughness quantity) -> quantityCounts quantity) (Maybe.maybeToList (Card.Type.toughness card))
    <> concatMap (concatMap modificationCounts . StaticAbility.modifications) (Card.Type.staticAbilities card)
    <> concatMap effectCounts (Card.allEffects card)
    <> concatMap (concatMap effectCounts . Modal.allEffects . ActivatedAbility.modal) (Card.Type.activatedAbilities card)
    <> concatMap triggeredAbilityCounts (Card.Type.triggeredAbilities card)
    <> concatMap triggeredAbilityCounts (Map.elems (Card.Type.delayedAbilities card))

-- CR 400.1: "each player has their own library, hand, and graveyard. The
-- other zones are shared by all players." Battlefield/Stack/Exile/Command are
-- shared; Library/Hand/Graveyard are per-player.
isSharedZone :: Zone.Zone -> Bool
isSharedZone zone = case zone of
  Zone.Library -> False
  Zone.Hand -> False
  Zone.Graveyard -> False
  Zone.Battlefield -> True
  Zone.Stack -> True
  Zone.Exile -> True
  Zone.Command -> True

-- A Count over a shared zone paired with anything but EachPlayer names a
-- per-player fold over a zone no player individually owns -- permitted by the
-- type, not by the rules (#161).
scopeOffends :: Scope.Scope -> Bool
scopeOffends scope = case scope of
  Scope.InZone zone ref -> isSharedZone zone && ref /= PlayerRef.EachPlayer
  Scope.InHistory _ -> False

cardOffendsSharedZoneScope :: Card.Type.Card -> Bool
cardOffendsSharedZoneScope card =
  any (\(Count.Type.MkCount scope _ _) -> scopeOffends scope) (cardCounts card)

-- The TRIGGERED-ability half of the D4 dataflow lint: every slot one of a
-- triggered ability's effects READS must be a slot something binds for that
-- ability. Without it, an effect naming CR 400.7e's `became` under a condition
-- that never binds it loads, places its trigger, misses the lookup and silently
-- no-ops (Resolve's MoveToZone arm falls through to `pure ()`).
--
-- A SUBSET check, never the spell lint's equality, and that is forced rather
-- than chosen: Pawl.Binding.triggerSource's comment spells out that an
-- equality-style lint widened over an ability's modes is mutually unsatisfiable
-- with the "a reserved slot is never a declared target slot" rule unless it
-- first subtracts every reserved name from the read side. The delayed-ability
-- lint below took the subset shape for the same reason, and this follows it.
--
-- The available side, and why each part of it is available:
--
--   * Binding.triggerSource (CR 113.7, the object whose ability triggered) and
--     Binding.you (CR 109.5, the ability's controller) are stamped for EVERY
--     triggered ability as it is placed (Engine.placeBorne, Binding.setYou), so
--     they need no agreement with the condition.
--   * Event.eventBindingSlots is the condition-SPECIFIC half -- CR 400.7e's
--     `became`, CR 702.70a's `thatPlayer` -- and is the whole point of this
--     lint.
--   * Resolve.definedSlots covers a slot the ability's own effects MINT rather
--     than read: a Create's token (CR 603.7c's "it"), a PlaySubgame's loser.
--     The same exemption both existing lints take.
--   * the ability's own declared target specs (CR 601.2c / 700.2c) are the
--     ordinary chosen targets, unioned across its modes exactly as the
--     delayed-ability lint unions across a card.
triggeredAbilityOffends :: TriggeredAbility.TriggeredAbility Card.Type.Card -> Bool
triggeredAbilityOffends ability =
  let effects = Modal.allEffects (TriggeredAbility.modal ability)
      available =
        Set.unions
          [ Set.fromList [Binding.triggerSource, Binding.you],
            Event.eventBindingSlots (TriggeredAbility.condition ability),
            Resolve.definedSlots effects,
            Map.keysSet (Modal.allTargetSpecs (TriggeredAbility.modal ability))
          ]
      wanted = Set.unions (fmap Resolve.slotsOf effects)
   in not (Set.isSubsetOf wanted available)

-- A one-mode, targetless triggered ability running one effect under one
-- condition -- the fixture the lint's own self-test misauthors on purpose. Kept
-- here rather than in data/cards, because a committed card that offends the lint
-- would fail the corpus sweep: the offender has to live where only the self-test
-- sees it.
oneEffectTrigger ::
  TriggerCondition.TriggerCondition ->
  Effect.Effect Card.Type.Card ->
  TriggeredAbility.TriggeredAbility Card.Type.Card
oneEffectTrigger condition effect =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }

-- The D4 dataflow lint: every slot an effect reads is declared, and every
-- declared slot is read. Equality, not subset: a spec no effect reads is a
-- card announcing a target it ignores -- representable in Magic, not in this
-- pool. Loosen to superset if such a card ever lands.
lintTests :: Registry.Type.Registry -> Tasty.TestTree
lintTests registry =
  Tasty.testGroup
    "Lint"
    [ HU.testCase "every mode's slot reads equal its declared slots" $ do
        ps <- S.allPrintings registry
        let modeOffends m =
              let defined = Resolve.definedSlots (Foldable.toList (Mode.effects m))
                  reads_ = Set.unions (fmap Resolve.slotsOf (Foldable.toList (Mode.effects m)))
               in -- A slot DEFINED in this mode (a Create's minted token, or a
                  -- PlaySubgame's bound subgame outcome) and then read by a later
                  -- effect is legitimate dataflow, not an undeclared target -- the
                  -- same definedSlots exemption the delayed-ability lint below uses.
                  Set.difference reads_ defined /= Map.keysSet (Mode.targetSpecs m)
            cardOffends card =
              any modeOffends (Modal.modes (Card.Type.spell card))
            offenders =
              filter
                (cardOffends . Printing.card)
                ps
        HU.assertEqual "no dangling or unused slots" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "every file in data/cards loads, and its card is named by its file name" $ do
        -- The registry checks name-against-file-name on each load (Pawl.Registry.load),
        -- so sweeping the listing is the whole assertion: a stray file, a file whose
        -- card was renamed, and a file that no test happens to name all fail here.
        -- A hand-kept list is exactly what forgets the file nobody loads.
        slugs <- S.corpusSlugs registry
        HU.assertBool "the corpus is not empty" (not (null slugs))
        mapM_ (Registry.card registry) slugs,
      -- The other direction: Registry.card slugifies the NAME it is asked for,
      -- then builds a path from that slug -- so a file whose stem is not itself a
      -- slugify fixed point is never opened by that path; a lookup would quietly
      -- open some OTHER file (or none) instead of raising the mismatch above.
      -- Every committed file name must therefore already be its own slug.
      -- Slug.fromText normalizes rather than validates, so the assertion is that
      -- it is the identity on every stem -- read the listing directly, because
      -- Registry.slugs has already normalized the evidence away.
      HU.testCase "every file name in data/cards is already a slug" $ do
        entries <- Directory.listDirectory (Registry.Type.root registry)
        let stems = fmap (reverse . drop (length ".json") . reverse) (filter (List.isSuffixOf ".json") entries)
        HU.assertBool "the corpus is not empty" (not (null stems))
        mapM_
          ( \stem ->
              HU.assertEqual
                ("file name is its own slug: " <> stem)
                (Text.pack stem)
                (Slug.unwrap (Slug.fromText (Text.pack stem)))
          )
          stems,
      HU.testCase "Blaze is a {X}{R} Sorcery dealing X to any target" $ do
        blaze <- Registry.printing registry "Blaze"
        let card = Printing.card blaze
            red = ManaSymbol.OfType (ManaType.Colored Color.Red)
        HU.assertEqual "name" (Text.pack "Blaze") (Card.Type.name card)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Variable, red])) (Card.Type.manaCost card)
        HU.assertBool "sorcery, not instant" (not (Card.isInstant card))
        HU.assertEqual "one AnyTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)) (Card.allTargetSpecs card)
        HU.assertEqual "effect deals X" [Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) Quantity.Type.X] (Card.allEffects card),
      HU.testCase "the lint itself catches a dangling reference" $
        let bad = Set.unions [Resolve.slotsOf (Effect.DealDamage (SlotName.MkSlotName (Text.pack "ghost")) (Quantity.Type.Literal 3))]
         in HU.assertBool "misauthored card detected" (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSpec.TargetSpec)),
      HU.testCase "every printing that reads X declares {X}, and vice versa" $ do
        ps <- S.allPrintings registry
        let readsX c = Resolve.readsX (Card.allEffects c)
            hasVariable c = case Card.Type.manaCost c of
              Nothing -> False
              Just (ManaCost.MkManaCost syms) -> elem ManaSymbol.Variable syms
            offenders =
              filter
                (\p -> readsX (Printing.card p) /= hasVariable (Printing.card p))
                ps
        HU.assertEqual "X read iff {X} declared" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved X slot is never a declared target slot" $ do
        ps <- S.allPrintings registry
        let offenders =
              filter
                (Map.member Binding.variableX . Card.allTargetSpecs . Printing.card)
                ps
        HU.assertEqual "no card names the X slot" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved modes slot is never a declared target slot" $ do
        ps <- S.allPrintings registry
        let offenders =
              filter
                (Map.member Binding.chosenModes . Card.allTargetSpecs . Printing.card)
                ps
        HU.assertEqual "no card names the modes slot" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved trigger-source slot is never a declared target slot" $ do
        ps <- S.allPrintings registry
        let offenders =
              filter
                (Map.member Binding.triggerSource . Card.allTargetSpecs . Printing.card)
                ps
        HU.assertEqual "no card names the self slot" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved you slot is never a declared target slot" $ do
        ps <- S.allPrintings registry
        let offenders =
              filter
                (Map.member Binding.you . Card.allTargetSpecs . Printing.card)
                ps
        HU.assertEqual "no card names the you slot" [] (fmap (Card.Type.name . Printing.card) offenders),
      -- CR 400.7e's arriving incarnation is stamped by Event.eventBindings, not
      -- chosen, so a card declaring it as a target spec would be prompted for a
      -- target and then have the answer clobbered. Same SCOPE limit as the three
      -- above -- Card.allTargetSpecs walks a card's SPELL modes only, so a
      -- triggered ability declaring the slot still slips through, which is the
      -- gap Pawl.Binding's `you` comment documents for the whole family (#428).
      -- The READ direction over a triggered ability's modes is a separate lint,
      -- below.
      HU.testCase "the reserved became slot is never a declared target slot" $ do
        ps <- S.allPrintings registry
        let offenders =
              filter
                (Map.member Binding.became . Card.allTargetSpecs . Printing.card)
                ps
        HU.assertEqual "no card names the became slot" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "Lightning Bolt is in the red pool with one AnyTarget slot" $ do
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let card = Printing.card lightningBolt
        HU.assertBool "an instant" (Card.isInstant card)
        HU.assertEqual "one slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)) (Card.allTargetSpecs card),
      -- The AbilityName half of the D4 dataflow lint (CR 603.7): an
      -- ArmDelayedTrigger naming an ability the card does not declare is a FAILING
      -- TEST, never a trigger that silently never fires. Equality, not subset: a
      -- declared ability nothing arms is dead card text.
      --
      -- SCOPE, same posture as Pawl.Binding's D4-lint-scope comment: this and the
      -- multi-token-binding lint below both walk `Card.allEffects`, which is
      -- `Modal.allEffects (Card.spell card)` -- a card's SPELL modes ONLY, never
      -- an activated or triggered ability's effects. An ArmDelayedTrigger placed
      -- inside an activated/triggered ability is therefore invisible to THIS
      -- lint's "every armed name is declared" half; if the card also declares no
      -- matching entry, the dangling arm passes silently. The reverse direction --
      -- a declared entry nothing arms, because the arm lives in an ability the
      -- lint can't see -- still fails loudly, which is the safe way round. No card
      -- in this pool arms from inside an ability today (only Tidal Wave arms
      -- anything, and it arms from its spell mode); widening the lint to
      -- non-spell modes is a separate, deliberately out-of-scope change.
      HU.testCase "every armed delayed ability is declared, and every declared one is armed" $ do
        ps <- S.allPrintings registry
        let cardOffends card =
              Resolve.armedAbilities (Card.allEffects card) /= Map.keysSet (Card.Type.delayedAbilities card)
            offenders = filter (cardOffends . Printing.card) ps
        HU.assertEqual "no dangling or unused delayed abilities" [] (fmap (Card.Type.name . Printing.card) offenders),
      -- Every slot a delayed ability READS must be one the arming card DEFINES:
      -- the reserved trigger-source slot, or a token bound by a Create.
      HU.testCase "every slot a delayed ability reads is bound by its card" $ do
        ps <- S.allPrintings registry
        let cardOffends card =
              let available = Set.insert Binding.triggerSource (Resolve.definedSlots (Card.allEffects card))
                  wanted = Set.unions (fmap Resolve.slotsOf (Card.delayedEffects card))
               in not (Set.isSubsetOf wanted available)
            offenders = filter (cardOffends . Printing.card) ps
        HU.assertEqual "no dangling delayed-ability slot" [] (fmap (Card.Type.name . Printing.card) offenders),
      -- The same subset shape over a card's TRIGGERED abilities, which is where
      -- the condition-specific reserved slots live -- CR 400.7e's `became` and
      -- CR 702.70a's `thatPlayer`. See triggeredAbilityOffends for the available
      -- side and for why this cannot be an equality check.
      HU.testCase "every slot a triggered ability reads is bound for its condition" $ do
        ps <- S.allPrintings registry
        let cardOffends = any triggeredAbilityOffends . Card.Type.triggeredAbilities
            offenders = filter (cardOffends . Printing.card) ps
        HU.assertEqual "no dangling triggered-ability slot" [] (fmap (Card.Type.name . Printing.card) offenders),
      -- The sweep above passes VACUOUSLY: no committed card misauthors the
      -- pairing, so the sweep proves nothing about the lint. Both directions are
      -- proven here instead, against a hand-built offender (never a card file --
      -- a misauthored card must not be loadable) and against the real pairing.
      --
      -- Both reserved event slots, because a classification that answered "every
      -- slot, always" would pass the offending half of either one alone.
      HU.testCase "the lint itself catches a reserved event slot the condition never binds" $ do
        roaches <- Registry.printing registry "Endless Cockroaches"
        let -- Endless Cockroaches' own payload: "return it to its owner's hand".
            returnIt = Effect.MoveToZone Binding.became Zone.Hand
            -- Rule 702.70a's shape, as a targetless read of "that player".
            thatPlayerDraws = Effect.Draw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1)
        HU.assertBool
          "CR 400.7e became under an enters trigger is rejected"
          (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfEnters returnIt))
        HU.assertBool
          "and under a dies trigger it is accepted"
          (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies returnIt)))
        HU.assertBool
          "CR 702.70a thatPlayer under a dies trigger is rejected"
          (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies thatPlayerDraws))
        HU.assertBool
          "and under a combat-damage trigger it is accepted"
          (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDealsCombatDamageToPlayer thatPlayerDraws)))
        HU.assertBool
          "the real card's dies trigger is accepted"
          (not (any triggeredAbilityOffends (Card.Type.triggeredAbilities (Printing.card roaches)))),
      -- CR 603.7c: binding a slot to a MULTI-token Create would silently name one
      -- of them. Rejected rather than guessed (#53).
      HU.testCase "no Create binds a slot while making more than one token" $ do
        ps <- S.allPrintings registry
        let offenders =
              filter
                (Resolve.bindsSeveralTokens . Card.allEffects . Printing.card)
                ps
        HU.assertEqual "no multi-token binding" [] (fmap (Card.Type.name . Printing.card) offenders),
      -- CR 400.1: every InZone Count over a shared zone (battlefield, stack,
      -- exile, command) must pair with PlayerRef.EachPlayer -- the type
      -- permits any PlayerRef there, but only EachPlayer is meaningful for a
      -- zone no player owns individually (#161).
      HU.testCase "every InZone Count over a shared zone pairs with EachPlayer" $ do
        ps <- S.allPrintings registry
        let offenders =
              filter
                (cardOffendsSharedZoneScope . Printing.card)
                ps
        HU.assertEqual "no shared-zone scope with a non-EachPlayer ref" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "a card with no enchant ability declares no enchant slot" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let card = Printing.card piker
        HU.assertEqual "no enchant spec" Nothing (Card.Type.enchant card)
        HU.assertBool "not an Aura" (not (Card.isAura card))
        HU.assertEqual "no enchant slot" Map.empty (Card.enchantSpecs card),
      -- CR 303.4 / 702.5a: the biconditional. An Aura without enchant has no legal
      -- target and could never be cast; a non-Aura with enchant declares a restriction
      -- nothing reads. The D4 lint cannot see either, because it walks
      -- Mode.targetSpecs and the enchant slot is not there (#184's shape).
      HU.testCase "a card is an Aura iff it declares an enchant ability" $ do
        ps <- S.allPrintings registry
        let offends c = Card.isAura c /= Maybe.isJust (Card.Type.enchant c)
            offenders = filter (offends . Printing.card) ps
        HU.assertEqual "Aura iff enchant" [] (fmap (Card.Type.name . Printing.card) offenders),
      -- Pawl.Card.allTargetSpecs binds the enchant spec under this name (Task 6), so a
      -- mode declaring it would be silently shadowed.
      -- #199: no card authors a layer-2 control modification into an effect that
      -- RESOLVES. SetControllerToSource is the payload-free constructor and is
      -- INERT when stored: Projection.controllerOfGiven's storedSetter matches only
      -- Modification.SetController, Projection.controlGrants reads control-granting
      -- static abilities off Card.staticAbilities and never off stored effects, and
      -- Projection.applyModification's SetControllerToSource arm is the identity.
      -- A card authoring one would resolve, store the effect, and grant control to
      -- no one -- there is nothing for CR 800.4a to end (see Pawl.Departure's
      -- proofs).
      --
      -- BOTH control constructors, not just the payload-free one: baking a
      -- PlayerId into static card text is equally unreal, since a card cannot
      -- know who is playing. Control on a card belongs on a STATIC ability
      -- (Control Magic), which the projection re-derives and never stores.
      --
      -- Asked as an EQUALITY on Layer through Projection.layer -- the sanctioned
      -- classification -- rather than by casing on Modification, which only
      -- Pawl.Projection may do. Layer.Control is exactly the two control
      -- constructors, so this covers a third one automatically.
      --
      -- A codec-level rejection would be the wrong shape: jsonToModification is
      -- shared with staticAbilities, which Control Magic legitimately uses.
      HU.testCase "no card authors a control modification into a resolving effect (#199)" $ do
        ps <- S.allPrintings registry
        let offends effect = case effect of
              Effect.ModifyTarget _ modification _ -> Projection.layer modification == Layer.Control
              _ -> False
            offenders = filter (any offends . cardResolutionEffects . Printing.card) ps
        HU.assertEqual "control belongs on a static ability, never in a stored effect" [] (fmap (Card.Type.name . Printing.card) offenders),
      HU.testCase "no mode declares a slot named enchant" $ do
        ps <- S.allPrintings registry
        let offends c = any (Map.member Card.enchantSlot . Mode.targetSpecs) (Modal.modes (Card.Type.spell c))
            offenders = filter (offends . Printing.card) ps
        HU.assertEqual "the enchant slot is never hand-declared" [] (fmap (Card.Type.name . Printing.card) offenders)
    ]

m2bCardTests :: Registry.Type.Registry -> Tasty.TestTree
m2bCardTests registry =
  let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      gs0 = Setup.emptyGame S.bothPlayers
   in Tasty.testGroup
        "M2bCards"
        [ HU.testCase "Sabretooth Tiger is a {2}{R} 2/1 Cat with first strike" $ do
            sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
            let c = Printing.card sabretoothTiger
            HU.assertEqual "name" (Text.pack "Sabretooth Tiger") (Card.Type.name c)
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red])) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
            HU.assertEqual "subtypes" (Set.singleton Subtype.Cat) (TypeLine.subtypes (Card.Type.typeLine c))
            HU.assertEqual "keyword" (Set.singleton Keyword.FirstStrike) (Card.Type.keywords c),
          HU.testCase "Ridgetop Raptor is a {3}{R} 2/1 Dinosaur Beast with double strike" $ do
            ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
            let c = Printing.card ridgetopRaptor
            HU.assertEqual "name" (Text.pack "Ridgetop Raptor") (Card.Type.name c)
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Dinosaur, Subtype.Beast]) (TypeLine.subtypes (Card.Type.typeLine c))
            HU.assertEqual "keyword" (Set.singleton Keyword.DoubleStrike) (Card.Type.keywords c),
          HU.testCase "the tiger has first strike through the projection" $ do
            sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
            let (oid, gs) = S.addCreature sabretoothTiger S.alice gs0
            HU.assertBool "first strike" (Projection.hasKeyword Keyword.FirstStrike oid gs)
            HU.assertBool "not double strike" (not (Projection.hasKeyword Keyword.DoubleStrike oid gs)),
          HU.testCase "the raptor has double strike through the projection" $ do
            ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
            let (oid, gs) = S.addCreature ridgetopRaptor S.alice gs0
            HU.assertBool "double strike" (Projection.hasKeyword Keyword.DoubleStrike oid gs)
            HU.assertBool "not first strike" (not (Projection.hasKeyword Keyword.FirstStrike oid gs)),
          HU.testCase "both are 2/1s, the same body as a Piker" $ do
            piker <- Registry.printing registry "Goblin Piker"
            sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
            ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
            let bodyOf p = (Card.Type.power (Printing.card p), Card.Type.toughness (Printing.card p))
            HU.assertEqual "tiger body" (bodyOf piker) (bodyOf sabretoothTiger)
            HU.assertEqual "raptor body" (bodyOf piker) (bodyOf ridgetopRaptor)
        ]

m2cCardTests :: Registry.Type.Registry -> Tasty.TestTree
m2cCardTests registry =
  Tasty.testGroup
    "M2cCards"
    [ HU.testCase "Typhoid Rats is a {B} 1/1 Rat with deathtouch" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let c = Printing.card typhoidRats
        HU.assertEqual "name" (Text.pack "Typhoid Rats") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Deathtouch) (Card.Type.keywords c),
      HU.testCase "War Mammoth is a {3}{G} 3/3 Elephant with trample" $ do
        warMammoth <- Registry.printing registry "War Mammoth"
        let c = Printing.card warMammoth
        HU.assertEqual "name" (Text.pack "War Mammoth") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Trample) (Card.Type.keywords c)
    ]

basicLandTests :: Registry.Type.Registry -> Tasty.TestTree
basicLandTests registry =
  Tasty.testGroup
    "BasicLand"
    [ HU.testCase "CR 305.6 a Swamp's intrinsic ability is black mana" $
        HU.assertEqual
          "black"
          (Just (ManaType.Colored Color.Black))
          (Mana.subtypeMana Subtype.Swamp),
      HU.testCase "CR 305.6 a Forest's intrinsic ability is green mana" $
        HU.assertEqual
          "green"
          (Just (ManaType.Colored Color.Green))
          (Mana.subtypeMana Subtype.Forest),
      HU.testCase "swampPrinting is a basic Swamp land" $ do
        swamp <- Registry.printing registry "Swamp"
        let c = Printing.card swamp
        HU.assertBool "land" (Card.isLand c)
        HU.assertBool
          "swamp subtype"
          (Set.member Subtype.Swamp (TypeLine.subtypes (Card.Type.typeLine c))),
      HU.testCase "forestPrinting is a basic Forest land" $ do
        forest <- Registry.printing registry "Forest"
        let c = Printing.card forest
        HU.assertBool "land" (Card.isLand c)
        HU.assertBool
          "forest subtype"
          (Set.member Subtype.Forest (TypeLine.subtypes (Card.Type.typeLine c)))
    ]

m3cCardTests :: Registry.Type.Registry -> Tasty.TestTree
m3cCardTests registry =
  Tasty.testGroup
    "M3cCards"
    [ HU.testCase "Blood Moon is a {2}{R} enchantment with one SetLandSubtype static ability" $ do
        bloodMoon <- Registry.printing registry "Blood Moon"
        let card = Printing.card bloodMoon
        HU.assertEqual "one static ability" 1 (length (Card.Type.staticAbilities card))
        HU.assertBool "not a permanent target" (Map.null (Card.allTargetSpecs card))
    ]

m3eCardTests :: Registry.Type.Registry -> Tasty.TestTree
m3eCardTests registry =
  Tasty.testGroup
    "M3eCards"
    [ HU.testCase "Prodigal Sorcerer has one non-mana activated ability" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        case Card.Type.activatedAbilities (Printing.card prodigalSorcerer) of
          [ab] -> HU.assertBool "not a mana ability" (not (Mana.isManaAbility ab))
          _ -> HU.assertFailure "expected exactly one ability",
      HU.testCase "Llanowar Elves has one mana activated ability" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        case Card.Type.activatedAbilities (Printing.card llanowarElves) of
          [ab] -> HU.assertBool "mana ability" (Mana.isManaAbility ab)
          _ -> HU.assertFailure "expected exactly one ability"
    ]

m4bCardTests :: Registry.Type.Registry -> Tasty.TestTree
m4bCardTests registry =
  Tasty.testGroup
    "M4bCards"
    [ HU.testCase "Darksteel Myr is a {3} 0/1 Artifact Creature with indestructible" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let c = Printing.card darksteelMyr
        HU.assertEqual "name" (Text.pack "Darksteel Myr") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 0))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
        HU.assertEqual "keyword" (Set.singleton Keyword.Indestructible) (Card.Type.keywords c),
      -- #113: both P9 gate cards end "It can't be regenerated", and the clause was
      -- omitted from their data while nothing could honour it. It is data now.
      HU.testCase "CR 701.19c Terror and Reprisal both carry the can't-be-regenerated rider" $ do
        terror <- Registry.printing registry "Terror"
        reprisal <- Registry.printing registry "Reprisal"
        let riders c = [r | Effect.Destroy _ r <- Card.allEffects (Printing.card c)]
        HU.assertEqual "Terror" [Regenerability.CantBeRegenerated] (riders terror)
        HU.assertEqual "Reprisal" [Regenerability.CantBeRegenerated] (riders reprisal),
      HU.testCase "Murder is a {1}{B}{B} Instant that destroys a target creature" $ do
        murder <- Registry.printing registry "Murder"
        let c = Printing.card murder
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, black, black])) (Card.Type.manaCost c)
        HU.assertBool "an instant" (Card.isInstant c)
        -- Murder carries no CR 701.19c rider, unlike Terror and Reprisal.
        HU.assertEqual "effect destroys the target slot" [Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Regenerability.Regenerable] (Card.allEffects c)
        HU.assertEqual "one CreatureTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures Nothing)) (Card.allTargetSpecs c),
      -- Murder's opposite number on the one axis this pair exists to pin: the
      -- SAME Destroy opcode, with the SAME CR 701.19c rider, differing only in
      -- whether its ObjectRef names a cast-time slot or a resolution-time set.
      -- CR 115.10a is why the second declares no target spec: "Unless that object
      -- or player is identified by the word 'target' ..., it's not a target."
      HU.testCase "Day of Judgment is a {2}{W}{W} Sorcery that destroys every creature and targets nothing" $ do
        dayOfJudgment <- Registry.printing registry "Day of Judgment"
        let c = Printing.card dayOfJudgment
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
        HU.assertEqual "name" (Text.pack "Day of Judgment") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, white, white])) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Sorcery) (TypeLine.types (Card.Type.typeLine c))
        -- CR 109.2 supplies the battlefield and the word "permanent"; the card
        -- text is only "all creatures", so the Filter is only HasCardType.
        HU.assertEqual
          "one Destroy over the creatures, with no can't-be-regenerated rider"
          [Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Regenerability.Regenerable]
          (Card.allEffects c)
        HU.assertEqual "and no target spec at all" Map.empty (Card.allTargetSpecs c),
      -- The pool's counterweight to Day of Judgment: a creature that grants
      -- indestructible to OTHERS and does not have it itself, so it is destroyed
      -- by the same sweep as the permanents it protects. CR 608.2f is what makes
      -- that pairing say something -- the grant is still in force when every
      -- victim's CR 702.12b gate is judged, so the granter dies alone.
      --
      -- "Other permanents you control" needs no new filter vocabulary: `Not
      -- IsSource` is the same spelling of "other" Opalescence's card text uses,
      -- and `ControlledBy You` the same "you control" Ashaya's does.
      HU.testCase "The Walls of Ba Sing Se is an {8} 0/30 Legendary Artifact Creature granting indestructible to OTHER permanents you control" $ do
        walls <- Registry.printing registry "The Walls of Ba Sing Se"
        let c = Printing.card walls
        HU.assertEqual "name" (Text.pack "The Walls of Ba Sing Se") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 8])) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 0))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 30))) (Card.Type.toughness c)
        HU.assertEqual "types" (Set.fromList [CardType.Artifact, CardType.Creature]) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "supertypes" (Set.singleton Supertype.Legendary) (TypeLine.supertypes (Card.Type.typeLine c))
        -- Defender is printed on the card; Indestructible is NOT -- the whole
        -- point is that the granter does not benefit from its own grant.
        HU.assertEqual "printed keywords" (Set.singleton Keyword.Defender) (Card.Type.keywords c)
        HU.assertEqual
          "\"Other permanents you control have indestructible\""
          [ StaticAbility.MkStaticAbility
              ( Affected.Matching
                  ( Filter.Type.And
                      [ Filter.Type.Not Filter.Type.IsSource,
                        Filter.Type.ControlledBy PlayerRelation.You
                      ]
                  )
              )
              (NonEmpty.singleton (Modification.GainKeyword Keyword.Indestructible))
          ]
          (Card.Type.staticAbilities c),
      HU.testCase "Unsummon is a {U} Instant that bounces a target creature to hand" $ do
        unsummon <- Registry.printing registry "Unsummon"
        let c = Printing.card unsummon
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [blue])) (Card.Type.manaCost c)
        HU.assertEqual "effect returns to hand" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Hand] (Card.allEffects c),
      -- Three modifications on ONE target, in printed order. Spelled out rather
      -- than spot-checked because the toxic 1 grant is what makes this card the
      -- CR 702.164b proof in DamageSpec: a card that granted toxic 2 by mistake
      -- would still add up to the poison that test expects.
      HU.testCase "Aspirant's Ascent is a {U} Instant granting +1/+3, flying and toxic 1" $ do
        ascent <- Registry.printing registry "Aspirant's Ascent"
        let c = Printing.card ascent
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
            target = SlotName.MkSlotName (Text.pack "target")
            untilEot = Effect.ModifyTarget Duration.UntilEndOfTurn
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [blue])) (Card.Type.manaCost c)
        HU.assertBool "an instant" (Card.isInstant c)
        HU.assertEqual
          "effects, in printed order"
          [ untilEot (Modification.ModifyPowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 3)) target,
            untilEot (Modification.GainKeyword Keyword.Flying) target,
            untilEot (Modification.GainKeyword (Keyword.Toxic 1)) target
          ]
          (Card.allEffects c)
        HU.assertEqual "one creature slot, shared by all three" (Map.singleton target (TargetSpec.MkTargetSpec Pool.Creatures Nothing)) (Card.allTargetSpecs c),
      HU.testCase "Angelic Edict is a {4}{W} Sorcery exiling a creature or enchantment" $ do
        angelicEdict <- Registry.printing registry "Angelic Edict"
        let c = Printing.card angelicEdict
        HU.assertBool "not an instant" (not (Card.isInstant c))
        HU.assertEqual "effect exiles" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Exile] (Card.allEffects c)
        HU.assertEqual "creature-or-enchantment slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment])))) (Card.allTargetSpecs c),
      HU.testCase "Divination is a {2}{U} Sorcery that draws two cards with no target" $ do
        divination <- Registry.printing registry "Divination"
        let c = Printing.card divination
        HU.assertEqual "effect draws two" [Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 2)] (Card.allEffects c)
        HU.assertBool "no target slots" (Map.null (Card.allTargetSpecs c)),
      HU.testCase "Tome Scour is a {U} Sorcery milling five from a target player" $ do
        tomeScour <- Registry.printing registry "Tome Scour"
        let c = Printing.card tomeScour
        HU.assertEqual "effect mills five" [Effect.Mill (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 5)] (Card.allEffects c)
        HU.assertEqual "one PlayerTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players Nothing)) (Card.allTargetSpecs c),
      HU.testCase "Mind Rot is a {2}{B} Sorcery making a target player discard two" $ do
        mindRot <- Registry.printing registry "Mind Rot"
        let c = Printing.card mindRot
        HU.assertEqual "effect discards two" [Effect.Discard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 2)] (Card.allEffects c)
        HU.assertEqual "one PlayerTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players Nothing)) (Card.allTargetSpecs c),
      -- Two effects of DIFFERENT opcodes reading one slot, in printed order --
      -- the pin that a rewrite reordering the mode's effect list, or splitting
      -- the clauses across two slots, would break.
      HU.testCase "Sign in Blood is a {B}{B} Sorcery drawing two and making one target player lose two life" $ do
        signInBlood <- Registry.printing registry "Sign in Blood"
        let c = Printing.card signInBlood
            target = SlotName.MkSlotName (Text.pack "target")
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [black, black])) (Card.Type.manaCost c)
        HU.assertBool "not an instant" (not (Card.isInstant c))
        HU.assertEqual
          "draws first, then loses life"
          [ Effect.Draw (PlayerRef.InSlot target) (Quantity.Type.Literal 2),
            Effect.LoseLife (PlayerRef.InSlot target) (Quantity.Type.Literal 2)
          ]
          (Card.allEffects c)
        HU.assertEqual "one PlayerTarget slot, shared by both" (Map.singleton target (TargetSpec.MkTargetSpec Pool.Players Nothing)) (Card.allTargetSpecs c),
      -- CR 202.3f: "When calculating the mana value of an object with a hybrid
      -- mana symbol in its mana cost, use the largest component of each hybrid
      -- symbol." Both halves of {R/G} are one mana, so the largest is 1 and
      -- {R/G}{R/G} is mana value 2 -- not 4 (both halves) and not 0 (neither).
      HU.testCase "Burning-Tree Emissary's two hybrid symbols make mana value 2" $ do
        burningTreeEmissary <- Registry.printing registry "Burning-Tree Emissary"
        HU.assertEqual "two" 2 (Quantity.manaValueOf (Printing.card burningTreeEmissary)),
      HU.testCase "Flame Javelin is a {2/R}{2/R}{2/R} Instant dealing 4 to any target" $ do
        flameJavelin <- Registry.printing registry "Flame Javelin"
        let c = Printing.card flameJavelin
            twoOrRed = ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Red)
            target = SlotName.MkSlotName (Text.pack "target")
        HU.assertEqual "name" (Text.pack "Flame Javelin") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [twoOrRed, twoOrRed, twoOrRed]) (Card.Type.manaCost c)
        HU.assertBool "an instant" (Card.isInstant c)
        HU.assertEqual "effect deals four" [Effect.DealDamage target (Quantity.Type.Literal 4)] (Card.allEffects c)
        HU.assertEqual "one AnyTarget slot" (Map.singleton target (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)) (Card.allTargetSpecs c),
      -- CR 202.3f again, whose own worked example is this card's cost: "The mana
      -- value of a card with mana cost {2/B}{2/B}{2/B} is 6." The generic half is
      -- the larger one, so a monocolored hybrid contributes 2 and not the 1 every
      -- other typed symbol contributes -- the detail that silently corrupts every
      -- mana-value reading downstream if it is wrong.
      HU.testCase "Flame Javelin's three monocolored hybrid symbols make mana value 6, not 3" $ do
        flameJavelin <- Registry.printing registry "Flame Javelin"
        HU.assertEqual "six" 6 (Quantity.manaValueOf (Printing.card flameJavelin))
    ]

m45p6CardTests :: Registry.Type.Registry -> Tasty.TestTree
m45p6CardTests registry =
  Tasty.testGroup
    "M45p6Cards"
    [ HU.testCase "Master Thief is a {2}{U}{U} 2/2 Human Rogue whose ETB steals an artifact" $ do
        masterThief <- Registry.printing registry "Master Thief"
        let c = Printing.card masterThief
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
            slot = SlotName.MkSlotName (Text.pack "target")
        HU.assertEqual "name" (Text.pack "Master Thief") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue, blue])) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c)
        HU.assertEqual "types" (Set.singleton CardType.Creature) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Human, Subtype.Rogue]) (TypeLine.subtypes (Card.Type.typeLine c))
        case Card.Type.triggeredAbilities c of
          [ab] -> do
            HU.assertEqual "enters trigger" TriggerCondition.SelfEnters (TriggeredAbility.condition ab)
            case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
              [m] -> do
                HU.assertEqual
                  "one GainControl effect with a conditional duration"
                  [Effect.GainControl (Duration.ForAsLongAs S.youControlSource) slot]
                  (Foldable.toList (Mode.effects m))
                HU.assertEqual
                  "one ArtifactTarget slot"
                  (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))))
                  (Mode.targetSpecs m)
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one triggered ability",
      HU.testCase "Hag of Inner Weakness is a {2}{B} 2/2 Hag Warlock with an upkeep -2/-1 trigger" $ do
        hagOfInnerWeakness <- Registry.printing registry "Hag of Inner Weakness"
        let c = Printing.card hagOfInnerWeakness
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
            slot = SlotName.MkSlotName (Text.pack "target")
        HU.assertEqual "name" (Text.pack "Hag of Inner Weakness") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, black])) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c)
        HU.assertEqual "types" (Set.singleton CardType.Creature) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Hag, Subtype.Warlock]) (TypeLine.subtypes (Card.Type.typeLine c))
        case Card.Type.triggeredAbilities c of
          [ab] -> do
            HU.assertEqual
              "beginning of your upkeep"
              (TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
              (TriggeredAbility.condition ab)
            case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
              [m] -> do
                HU.assertEqual
                  "-2/-1 until your next turn"
                  [Effect.ModifyTarget Duration.UntilYourNextTurn (Modification.ModifyPowerToughness (Quantity.Type.Literal (-2)) (Quantity.Type.Literal (-1))) slot]
                  (Foldable.toList (Mode.effects m))
                HU.assertEqual
                  "one OpponentCreatureTarget slot"
                  (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))))
                  (Mode.targetSpecs m)
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one triggered ability"
    ]

m45p7CardTests :: Registry.Type.Registry -> Tasty.TestTree
m45p7CardTests registry =
  Tasty.testGroup
    "M4.5 P7"
    [ HU.testCase "Rule of Law is a {2}{W} enchantment with one EachPlayer CantCastMoreThan 1 player ability" $ do
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let c = Printing.card ruleOfLaw
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
        HU.assertEqual "name" (Text.pack "Rule of Law") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, white])) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "no object-axis static abilities" [] (Card.Type.staticAbilities c)
        HU.assertEqual
          "one player ability"
          [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1)]
          (Card.Type.playerAbilities c),
      HU.testCase "Thalia is a {1}{W} 2/1 Legendary Human Soldier with first strike and one IncreaseSpellCost ability" $ do
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        let c = Printing.card thalia
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
        HU.assertEqual "name" (Text.pack "Thalia, Guardian of Thraben") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white])) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
        HU.assertEqual "supertypes" (Set.singleton Supertype.Legendary) (TypeLine.supertypes (Card.Type.typeLine c))
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Human, Subtype.Soldier]) (TypeLine.subtypes (Card.Type.typeLine c))
        HU.assertEqual "keywords" (Set.singleton Keyword.FirstStrike) (Card.Type.keywords c)
        HU.assertEqual
          "one player ability"
          [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.IncreaseSpellCost (Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)) 1)]
          (Card.Type.playerAbilities c),
      HU.testCase "Sapphire Medallion is a {2} artifact with one You ReduceSpellCost Blue ability" $ do
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        let c = Printing.card sapphireMedallion
        HU.assertEqual "name" (Text.pack "Sapphire Medallion") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Artifact) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual
          "one player ability"
          [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.ReduceSpellCost (Filter.Type.HasColor Color.Blue) (ManaCost.MkManaCost [ManaSymbol.Generic 1]))]
          (Card.Type.playerAbilities c),
      -- The reduction that NAMES a mana type, as against the Medallion's generic
      -- one: "Cleric spells you cast cost {W}{B} less to cast."
      HU.testCase "Edgewalker is a {1}{W}{B} Human Cleric with one You ReduceSpellCost {W}{B} ability" $ do
        edgewalker <- Registry.printing registry "Edgewalker"
        let c = Printing.card edgewalker
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        HU.assertEqual "name" (Text.pack "Edgewalker") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black])) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Creature) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Human, Subtype.Cleric]) (TypeLine.subtypes (Card.Type.typeLine c))
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c)
        HU.assertEqual
          "one player ability"
          [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.ReduceSpellCost (Filter.Type.HasSubtype Subtype.Cleric) (ManaCost.MkManaCost [white, black]))]
          (Card.Type.playerAbilities c),
      HU.testCase "Reliquary Tower is a land with a You NoMaximumHandSize ability and a {T} colorless mana ability" $ do
        reliquaryTower <- Registry.printing registry "Reliquary Tower"
        let c = Printing.card reliquaryTower
        HU.assertEqual "name" (Text.pack "Reliquary Tower") (Card.Type.name c)
        HU.assertEqual "no mana cost" Nothing (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Land) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "not basic" Set.empty (TypeLine.supertypes (Card.Type.typeLine c))
        HU.assertEqual
          "one player ability"
          [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]
          (Card.Type.playerAbilities c)
        case Card.Type.activatedAbilities c of
          [ab] -> do
            HU.assertEqual "tap cost only" [CostComponent.TapThis] (Cost.Type.components (ActivatedAbility.cost ab))
            HU.assertEqual "a real {0} mana cost, not an unpayable one (CR 118.5a/118.6)" (Just (ManaCost.MkManaCost [])) (Cost.Type.mana (ActivatedAbility.cost ab))
            case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
              [m] -> HU.assertEqual "adds colorless" [Effect.AddMana (ManaProduction.OfType ManaType.Colorless)] (Foldable.toList (Mode.effects m))
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one activated ability",
      -- Radiant Fountain, a Land: "When this land enters, you gain 2 life. /
      -- {T}: Add {C}." A nonbasic land whose whole text box is rules-text
      -- abilities of two different kinds, which is what CR 305.7's strip needs
      -- to reach (Pawl.TriggerSpec).
      HU.testCase "Radiant Fountain is a nonbasic land with a SelfEnters life gain and a {T} colorless mana ability" $ do
        radiantFountain <- Registry.printing registry "Radiant Fountain"
        let c = Printing.card radiantFountain
        HU.assertEqual "name" (Text.pack "Radiant Fountain") (Card.Type.name c)
        HU.assertEqual "no mana cost" Nothing (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Land) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "not basic, so Blood Moon reaches it (CR 305.8)" Set.empty (TypeLine.supertypes (Card.Type.typeLine c))
        HU.assertEqual "no land types of its own" Set.empty (TypeLine.subtypes (Card.Type.typeLine c))
        HU.assertEqual "no player abilities" [] (Card.Type.playerAbilities c)
        case Card.Type.triggeredAbilities c of
          [ab] -> do
            HU.assertEqual "on its own entry (CR 603.6a)" TriggerCondition.SelfEnters (TriggeredAbility.condition ab)
            case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
              [m] -> HU.assertEqual "you gain 2" [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 2)] (Foldable.toList (Mode.effects m))
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one triggered ability"
        case Card.Type.activatedAbilities c of
          [ab] -> do
            HU.assertEqual "tap cost only" [CostComponent.TapThis] (Cost.Type.components (ActivatedAbility.cost ab))
            case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
              [m] -> HU.assertEqual "adds colorless" [Effect.AddMana (ManaProduction.OfType ManaType.Colorless)] (Foldable.toList (Mode.effects m))
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one activated ability",
      HU.testCase "Silence is a {W} instant whose one effect is AffectPlayers UntilEndOfTurn Opponents CantCastSpells" $ do
        silence <- Registry.printing registry "Silence"
        let c = Printing.card silence
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
        HU.assertEqual "name" (Text.pack "Silence") (Card.Type.name c)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [white])) (Card.Type.manaCost c)
        HU.assertBool "an instant" (Card.isInstant c)
        HU.assertEqual "no player abilities: it is not a permanent" [] (Card.Type.playerAbilities c)
        HU.assertEqual
          "one targetless opcode"
          [Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells]
          (Card.allEffects c)
        HU.assertEqual "no target slots" Map.empty (Card.allTargetSpecs c)
    ]

m45p11CardTests :: Registry.Type.Registry -> Tasty.TestTree
m45p11CardTests registry =
  Tasty.testGroup
    "M4.5 P11"
    [ HU.testCase "Palace Jailer is a {2}{W}{W} 2/2 Human Soldier with two ETB triggers" $ do
        palaceJailer <- Registry.printing registry "Palace Jailer"
        let c = Printing.card palaceJailer
        HU.assertEqual "name" (Text.pack "Palace Jailer") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c)
        HU.assertEqual "two triggered abilities" 2 (length (Card.Type.triggeredAbilities c))
    ]

-- M5.5 pinned the migrated per-card conditions at the CODEC level: the decoded
-- card must equal the Condition the fixture spells out, so a decoding regression
-- fails here rather than surfacing as a behavioural oddity somewhere downstream.
-- Master Thief's ForAsLongAs is pinned this way in m45p6CardTests; this is
-- Barbarian Outcast's StateIs, which had only behavioural coverage (#165).
m55CardTests :: Registry.Type.Registry -> Tasty.TestTree
m55CardTests registry =
  Tasty.testGroup
    "M5.5"
    [ HU.testCase "Barbarian Outcast's state trigger is a Count of exactly 0 Swamps you control (CR 603.8)" $ do
        barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
        let c = Printing.card barbarianOutcast
        HU.assertEqual "name" (Text.pack "Barbarian Outcast") (Card.Type.name c)
        case Card.Type.triggeredAbilities c of
          [ab] -> do
            HU.assertEqual
              "the decoded condition is S.youControlNoSwamps"
              (TriggerCondition.StateIs S.youControlNoSwamps)
              (TriggeredAbility.condition ab)
            case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
              [m] -> HU.assertEqual "one Sacrifice self effect" [Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self"))] (Foldable.toList (Mode.effects m))
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one triggered ability",
      -- The Aggregation.Greatest gate card (#254). The pin that matters is the
      -- SHAPE: one Draw whose Quantity is a Count, with the per-member quantity
      -- inside the AGGREGATION rather than beside it -- the arrangement that
      -- makes "the greatest mana value among artifacts you control" one value
      -- and not a card-specific opcode.
      HU.testCase "One with the Machine is a {3}{U} Sorcery drawing the greatest mana value among artifacts you control" $ do
        oneWithTheMachine <- Registry.printing registry "One with the Machine"
        let c = Printing.card oneWithTheMachine
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        HU.assertEqual "name" (Text.pack "One with the Machine") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 3, blue]) (Card.Type.manaCost c)
        HU.assertBool "sorcery, not instant" (not (Card.isInstant c))
        HU.assertEqual
          "one Draw aimed at the caster, counting the battlefield"
          [ Effect.Draw
              (PlayerRef.Relative PlayerRelation.You)
              ( Quantity.Type.Count
                  ( Count.Type.MkCount
                      (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
                      ( Filter.Type.And
                          [ Filter.Type.HasCardType CardType.Artifact,
                            Filter.Type.ControlledBy PlayerRelation.You
                          ]
                      )
                      (Aggregation.Greatest Quantity.Type.ManaValue)
                  )
              )
          ]
          (Card.allEffects c)
        HU.assertEqual "and no target slots" Map.empty (Card.allTargetSpecs c)
    ]

-- The Auras phase (a) gate card: CR 303.4m's Attached affected-set, proven by a
-- real Aura on a real creature rather than a synthetic fixture.
auraCardTests :: Registry.Type.Registry -> Tasty.TestTree
auraCardTests registry =
  Tasty.testGroup
    "Auras"
    [ HU.testCase "Unholy Strength is a {B} Aura enchanting a creature for +2/+1" $ do
        p <- Registry.printing registry "Unholy Strength"
        let card = Printing.card p
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [black])) (Card.Type.manaCost card)
        HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine card))
        HU.assertEqual "subtypes" (Set.singleton Subtype.Aura) (TypeLine.subtypes (Card.Type.typeLine card))
        HU.assertBool "is an Aura" (Card.isAura card)
        -- CR 702.5a: "Enchant creature" -- the whole creature pool, unnarrowed.
        HU.assertEqual "enchant creature" (Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing)) (Card.Type.enchant card)
        -- CR 303.4m: "enchanted creature gets +2/+1" -- layer 7c on whatever it is
        -- attached to.
        HU.assertEqual
          "one +2/+1 static ability on the enchanted permanent"
          [StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 1)))]
          (Card.Type.staticAbilities card)
        -- CR 303.4: an Aura spell has no spell effects; it enters attached.
        HU.assertEqual "no spell effects" [] (Card.allEffects card),
      -- The pool's first CR 509.1c blocking requirement, and the first card whose
      -- whole ability lives on neither staticAbilities nor playerAbilities --
      -- which is the correction this file's presence records.
      HU.testCase "Lure is a {1}{G}{G} Aura whose only ability is a CR 509.1c blocking requirement" $ do
        p <- Registry.printing registry "Lure"
        let card = Printing.card p
            green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        HU.assertEqual "name" (Text.pack "Lure") (Card.Type.name card)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, green, green])) (Card.Type.manaCost card)
        HU.assertEqual "subtypes" (Set.singleton Subtype.Aura) (TypeLine.subtypes (Card.Type.typeLine card))
        HU.assertEqual "enchant creature" (Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing)) (Card.Type.enchant card)
        -- CR 303.4m: "all creatures able to block ENCHANTED CREATURE do so".
        HU.assertEqual
          "one requirement, naming whatever the Aura is attached to"
          [BlockRequirement.MkBlockRequirement Affected.Attached]
          (Card.Type.blockRequirements card)
        HU.assertEqual "and it modifies no characteristic" [] (Card.Type.staticAbilities card)
        HU.assertEqual "no spell effects" [] (Card.allEffects card),
      -- Not an Aura itself, but the only card in the pool that MOVES one: CR
      -- 701.3's Attach keyword action aimed at a permanent already on the
      -- battlefield. Its shape is the whole design argument -- one target slot for
      -- the Aura, no slot at all for the destination.
      HU.testCase "Crown of the Ages is a {2} artifact whose {4},{T} ability moves an Aura" $ do
        p <- Registry.printing registry "Crown of the Ages"
        let c = Printing.card p
            target = SlotName.MkSlotName (Text.pack "target")
        HU.assertEqual "name" (Text.pack "Crown of the Ages") (Card.Type.name c)
        -- The {4} is the ACTIVATION cost; the card itself costs {2}.
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 2]) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Artifact) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "no enchant ability: it is not an Aura" Nothing (Card.Type.enchant c)
        case Card.Type.activatedAbilities c of
          [ab] -> do
            HU.assertEqual "tap cost" [CostComponent.TapThis] (Cost.Type.components (ActivatedAbility.cost ab))
            HU.assertEqual "plus {4}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) (Cost.Type.mana (ActivatedAbility.cost ab))
            case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
              [m] -> do
                -- "to another CREATURE" is the destination filter, and it is a
                -- bare Filter rather than a target spec: CR 701.3 / the card's own
                -- ruling, "this only targets the Aura and not either creature".
                HU.assertEqual
                  "CR 701.3: attach the targeted permanent to a chosen creature"
                  [Effect.AttachTarget target (Filter.Type.HasCardType CardType.Creature)]
                  (Foldable.toList (Mode.effects m))
                -- "target Aura attached to a creature" -- the one slot, and the
                -- only place IsAttachedToCreature appears in the pool.
                HU.assertEqual
                  "CR 115.1: one target slot, an Aura on a creature"
                  (Map.singleton target (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.And [Filter.Type.HasSubtype Subtype.Aura, Filter.Type.IsAttachedToCreature]))))
                  (Mode.targetSpecs m)
              ms -> HU.assertFailure ("expected one mode, got " <> show (length ms))
          abs_ -> HU.assertFailure ("expected one activated ability, got " <> show (length abs_)),
      -- CR 702.5d's gate card: the first Aura in the pool whose enchant ability
      -- names a PLAYER, and the first affected set reached through one.
      HU.testCase "Curse of Death's Hold is a {3}{B}{B} Aura Curse enchanting a player for -1/-1" $ do
        p <- Registry.printing registry "Curse of Death's Hold"
        let card = Printing.card p
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        HU.assertEqual "name" (Text.pack "Curse of Death's Hold") (Card.Type.name card)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 3, black, black]) (Card.Type.manaCost card)
        HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine card))
        -- CR 205.3h: "Enchantment -- Aura Curse" is two enchantment types.
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Aura, Subtype.Curse]) (TypeLine.subtypes (Card.Type.typeLine card))
        HU.assertBool "is an Aura" (Card.isAura card)
        -- CR 702.5d: "Enchant player" -- the whole player pool, unnarrowed, which
        -- is what lets it target and be attached to a player and nothing else.
        HU.assertEqual "enchant player" (Just (TargetSpec.MkTargetSpec Pool.Players Nothing)) (Card.Type.enchant card)
        -- CR 303.4m through the enchanted PLAYER: "creatures enchanted player
        -- controls get -1/-1", layer 7c on a set the Aura is not attached to.
        HU.assertEqual
          "one -1/-1 static ability on the enchanted player's creatures"
          [StaticAbility.MkStaticAbility (Affected.AttachedPlayerControls (Filter.Type.HasCardType CardType.Creature)) (NonEmpty.singleton (Modification.ModifyPowerToughness (Quantity.Type.Literal (-1)) (Quantity.Type.Literal (-1))))]
          (Card.Type.staticAbilities card)
        -- CR 303.4: an Aura spell has no spell effects; it enters attached.
        HU.assertEqual "no spell effects" [] (Card.allEffects card)
    ]

-- Skilled Animator's "for as long as this creature remains on the battlefield",
-- as an ordinary count. The sibling of S.youControlSource with the CR 109.5
-- control conjunct dropped: this card asks only where its source IS, so a Master
-- Thief stealing the Animator would not end its effect. Group-local -- only the
-- pin below reads it.
sourceOnBattlefield :: Condition.Type.Condition
sourceOnBattlefield =
  Condition.Type.MkCondition
    ( Quantity.Type.Count
        ( Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            Filter.Type.IsSource
            Aggregation.Objects
        )
    )
    Comparison.Exactly
    (Quantity.Type.Literal 1)

-- The type-changing gate cards. Skilled Animator animates an ARTIFACT, which is
-- what lets an Equipment become a creature while it still equips one (CR 704.5p);
-- Liquimetal Coating makes any permanent an artifact, which is the only way to
-- feed an AURA to the Animator and so the only route to CR 303.4d's second clause
-- -- every printed enchantment animator excludes Auras by name; and March of the
-- Machines animates every noncreature artifact at once, which is the card CR
-- 613.6 exists for (#233).
animatorCardTests :: Registry.Type.Registry -> Tasty.TestTree
animatorCardTests registry =
  Tasty.testGroup
    "Animators"
    [ -- The CR 613.6 gate card (#233), and the pin that matters is the SHAPE: one
      -- static ability with three parts, not three abilities. Its affected set
      -- reads a card type its own layer-4 part changes, so the parts have to stay
      -- one effect or the layer-7b part loses the set.
      HU.testCase "March of the Machines is a {3}{U} enchantment: ONE ability, three parts, one affected set" $ do
        p <- Registry.printing registry "March of the Machines"
        let c = Printing.card p
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
            noncreatureArtifact =
              Affected.Matching
                ( Filter.Type.And
                    [ Filter.Type.HasCardType CardType.Artifact,
                      Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)
                    ]
                )
        HU.assertEqual "name" (Text.pack "March of the Machines") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 3, blue]) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual
          "\"is an artifact creature with power and toughness each equal to its mana value\""
          [ StaticAbility.MkStaticAbility
              noncreatureArtifact
              ( Modification.AddCardType CardType.Artifact
                  NonEmpty.:| [ Modification.AddCardType CardType.Creature,
                                Modification.SetBasePowerToughness Quantity.Type.ManaValue Quantity.Type.ManaValue
                              ]
              )
          ]
          (Card.Type.staticAbilities c),
      -- The same shape, arrived at from the other direction: Humility and
      -- Opalescence were each TWO abilities before #233 and are now one with two
      -- parts. Nothing observable changed for them -- their filters read card types
      -- they do not themselves change -- but the model has to be uniform, and this
      -- is the pin that keeps a future card from re-splitting them.
      HU.testCase "Humility and Opalescence are each one two-part ability, not two abilities" $ do
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let partsOf p = fmap (NonEmpty.toList . StaticAbility.modifications) (Card.Type.staticAbilities (Printing.card p))
        HU.assertEqual
          "CR 613.1f + CR 613.4b: lose all abilities, base 1/1"
          [[Modification.LoseAllAbilities, Modification.SetBasePowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1)]]
          (partsOf humility)
        HU.assertEqual
          "CR 613.1d + CR 613.4b: becomes a creature, base P/T its mana value"
          [[Modification.AddCardType CardType.Creature, Modification.SetBasePowerToughness Quantity.Type.ManaValue Quantity.Type.ManaValue]]
          (partsOf opalescence),
      HU.testCase "Liquimetal Coating is a {2} artifact whose {T} ability makes any permanent an artifact" $ do
        p <- Registry.printing registry "Liquimetal Coating"
        let c = Printing.card p
            target = SlotName.MkSlotName (Text.pack "target")
        HU.assertEqual "name" (Text.pack "Liquimetal Coating") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 2]) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Artifact) (TypeLine.types (Card.Type.typeLine c))
        case Card.Type.activatedAbilities c of
          [ab] -> do
            -- CR 107.5: the tap symbol is the entire activation cost.
            HU.assertEqual "tap cost only" [CostComponent.TapThis] (Cost.Type.components (ActivatedAbility.cost ab))
            HU.assertEqual "and no mana" (Just (ManaCost.MkManaCost [])) (Cost.Type.mana (ActivatedAbility.cost ab))
            case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
              [m] -> do
                HU.assertEqual
                  "CR 613.1d: one layer-4 addition, until end of turn"
                  [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.AddCardType CardType.Artifact) target]
                  (Foldable.toList (Mode.effects m))
                -- "Target permanent", unnarrowed -- the Aura the CR 303.4d case
                -- needs is a legal target precisely because there is no filter.
                HU.assertEqual
                  "CR 115.1: one target slot, any permanent"
                  (Map.singleton target (TargetSpec.MkTargetSpec Pool.Permanents Nothing))
                  (Mode.targetSpecs m)
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one activated ability",
      HU.testCase "Skilled Animator is a {2}{U} 1/3 Human Artificer whose ETB animates an artifact you control" $ do
        p <- Registry.printing registry "Skilled Animator"
        let c = Printing.card p
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
            target = SlotName.MkSlotName (Text.pack "target")
            duration = Duration.ForAsLongAs sourceOnBattlefield
        HU.assertEqual "name" (Text.pack "Skilled Animator") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 2, blue]) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c)
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Human, Subtype.Artificer]) (TypeLine.subtypes (Card.Type.typeLine c))
        case Card.Type.triggeredAbilities c of
          [ab] -> do
            HU.assertEqual "CR 603.6a: it triggers on entering" TriggerCondition.SelfEnters (TriggeredAbility.condition ab)
            case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
              [m] -> do
                -- "becomes an artifact creature with base power and toughness
                -- 5/5" is one printed effect that applies in two layers -- CR
                -- 613.1d for the added card types, CR 613.4b for the base P/T --
                -- so it decomposes into one Modification per layer. CR 613.6
                -- ("it will continue to be applied to the same set of objects in
                -- each other applicable layer") costs nothing across that split
                -- here: each part locks the same CR 611.2c one-object set at
                -- resolution, so there is no set left to disagree about.
                HU.assertEqual
                  "three parts, all on the target, all for the same duration"
                  [ Effect.ModifyTarget duration (Modification.AddCardType CardType.Artifact) target,
                    Effect.ModifyTarget duration (Modification.AddCardType CardType.Creature) target,
                    Effect.ModifyTarget duration (Modification.SetBasePowerToughness (Quantity.Type.Literal 5) (Quantity.Type.Literal 5)) target
                  ]
                  (Foldable.toList (Mode.effects m))
                HU.assertEqual
                  "CR 115.1: one target slot, an artifact you control"
                  (Map.singleton target (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.And [Filter.Type.HasCardType CardType.Artifact, Filter.Type.ControlledBy PlayerRelation.You]))))
                  (Mode.targetSpecs m)
              _ -> HU.assertFailure "expected exactly one mode"
          _ -> HU.assertFailure "expected exactly one triggered ability"
    ]

-- CR 702.29: the pool's first cycling card. Barkhide Mauler is a vanilla 4/4
-- whose only text is the keyword, so nothing else about it can stand in for the
-- keyword when a cycling test passes.
cyclingCardTests :: Registry.Type.Registry -> Tasty.TestTree
cyclingCardTests registry =
  Tasty.testGroup
    "Cycling"
    [ HU.testCase "Windcaller Aven is a {4}{U}{U} 4/3 with flying, Cycling {U} and a cycling trigger" $ do
        p <- Registry.printing registry "Windcaller Aven"
        let c = Printing.card p
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        HU.assertEqual "name" (Text.pack "Windcaller Aven") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 4, blue, blue]) (Card.Type.manaCost c)
        -- Two keywords, one printed and one that mints an ability: rule 702.9's
        -- flying is read where evasion is asked about, and rule 702.29a's cycling
        -- is minted by Pawl.Keyword.
        HU.assertEqual
          "flying and Cycling {U}"
          (Set.fromList [Keyword.Flying, Keyword.Cycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [blue])) []) Nothing])
          (Card.Type.keywords c)
        case Card.Type.triggeredAbilities c of
          [ability] ->
            HU.assertEqual
              "CR 702.29c: it triggers on being cycled"
              TriggerCondition.SelfCycled
              (TriggeredAbility.condition ability)
          abilities -> HU.assertFailure ("expected one triggered ability, got " <> show (length abilities)),
      HU.testCase "Ash Barrens is a Land with {T}: Add {C} and basic landcycling {1}" $ do
        p <- Registry.printing registry "Ash Barrens"
        let c = Printing.card p
            basicLand = Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic]
        HU.assertEqual "name" (Text.pack "Ash Barrens") (Card.Type.name c)
        HU.assertEqual "a land, with no mana cost (CR 202.1)" Nothing (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Land) (TypeLine.types (Card.Type.typeLine c))
        -- CR 702.29e's "[type]" is a Filter, and "basic land" is why: the same
        -- two-atom filter Evolving Wilds' search carries.
        HU.assertEqual
          "basic landcycling {1}"
          (Set.singleton (Keyword.Cycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []) (Just basicLand)))
          (Card.Type.keywords c)
        HU.assertEqual "one activated ability, its own mana ability" 1 (length (Card.Type.activatedAbilities c)),
      HU.testCase "Barkhide Mauler is a {4}{G} 4/4 Beast whose only text is Cycling {2}" $ do
        p <- Registry.printing registry "Barkhide Mauler"
        let c = Printing.card p
            green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        HU.assertEqual "name" (Text.pack "Barkhide Mauler") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 4, green]) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 4))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 4))) (Card.Type.toughness c)
        HU.assertEqual "subtypes" (Set.singleton Subtype.Beast) (TypeLine.subtypes (Card.Type.typeLine c))
        -- The card data carries the PRINTED cost and nothing else: rule 702.29a's
        -- discard and draw are minted by Pawl.Keyword, never authored here.
        HU.assertEqual
          "\"Cycling {2}\""
          (Set.singleton (Keyword.Cycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []) Nothing))
          (Card.Type.keywords c)
        HU.assertEqual "and no activated ability of its own" [] (Card.Type.activatedAbilities c)
    ]

-- The pool's two world enchantments. Their abilities are ordinary -- a layer-6
-- keyword grant and a layer-4/7b animation, both shapes the pool already had --
-- and it is the SUPERTYPE on the type line that earns them their place: CR
-- 205.4f is what puts them under CR 704.5k's world rule (Pawl.Sba.worldVictims),
-- and nothing else in the corpus carries it.
worldCardTests :: Registry.Type.Registry -> Tasty.TestTree
worldCardTests registry =
  Tasty.testGroup
    "WorldEnchantments"
    [ HU.testCase "Concordant Crossroads is a {G} world enchantment giving all creatures haste" $ do
        p <- Registry.printing registry "Concordant Crossroads"
        let c = Printing.card p
            green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        HU.assertEqual "name" (Text.pack "Concordant Crossroads") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [green]) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "CR 205.4f: the supertype the world rule reads" (Set.singleton Supertype.World) (TypeLine.supertypes (Card.Type.typeLine c))
        HU.assertEqual
          "\"All creatures have haste\""
          [ StaticAbility.MkStaticAbility
              (Affected.Matching (Filter.Type.HasCardType CardType.Creature))
              (Modification.GainKeyword Keyword.Haste NonEmpty.:| [])
          ]
          (Card.Type.staticAbilities c),
      HU.testCase "Living Plane is a {2}{G}{G} world enchantment making every land a 1/1 creature" $ do
        p <- Registry.printing registry "Living Plane"
        let c = Printing.card p
            green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        HU.assertEqual "name" (Text.pack "Living Plane") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 2, green, green]) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "CR 205.4f: the supertype the world rule reads" (Set.singleton Supertype.World) (TypeLine.supertypes (Card.Type.typeLine c))
        -- ONE ability with two parts, not two abilities (#233) -- the shape
        -- every animator in the pool has. CR 613.6 costs this one nothing
        -- either way, unlike March of the Machines: its affected set reads a
        -- card type ("all lands") that its own layer-4 part does not change.
        HU.assertEqual
          "\"All lands are 1/1 creatures that are still lands\""
          [ StaticAbility.MkStaticAbility
              (Affected.Matching (Filter.Type.HasCardType CardType.Land))
              ( Modification.AddCardType CardType.Creature
                  NonEmpty.:| [Modification.SetBasePowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1)]
              )
          ]
          (Card.Type.staticAbilities c)
    ]

-- CR 701.20: the cards that say "reveal" in their own text, as opposed to
-- inheriting it from a keyword the way Ash Barrens' typecycling does.
revealCardTests :: Registry.Type.Registry -> Tasty.TestTree
revealCardTests registry =
  Tasty.testGroup
    "Reveal"
    [ HU.testCase "Braidwood Sextant is a {1} Artifact whose {2}, {T}, Sacrifice fetches a revealed basic land" $ do
        p <- Registry.printing registry "Braidwood Sextant"
        let c = Printing.card p
            basicLand = Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic]
        HU.assertEqual "name" (Text.pack "Braidwood Sextant") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 1]) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Artifact) (TypeLine.types (Card.Type.typeLine c))
        case Card.Type.activatedAbilities c of
          [ability] -> do
            HU.assertEqual
              "\"{2}, {T}, Sacrifice this artifact\""
              (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [CostComponent.TapThis, CostComponent.SacrificeThis])
              (ActivatedAbility.cost ability)
            -- The whole point of the card, in the destination: "reveal that
            -- card, put it into your hand" is ONE instruction (CR 701.23e), and
            -- the same filter Evolving Wilds and Ash Barrens carry.
            case Foldable.toList (Modal.modes (ActivatedAbility.modal ability)) of
              [m] ->
                HU.assertEqual
                  "\"Search your library for a basic land card, reveal that card, put it into your hand\""
                  [Effect.Search basicLand SearchDestination.RevealThenHand]
                  (Foldable.toList (Mode.effects m))
              modes -> HU.assertFailure ("expected one mode, got " <> show (length modes))
          abilities -> HU.assertFailure ("expected one activated ability, got " <> show (length abilities))
    ]

-- CR 603.6a's second written form. Soul Warden is the pool's first card whose
-- ability triggers on a permanent OTHER than itself entering, and its effect
-- names nothing about the newcomer, so the card is a clean witness for the
-- trigger condition alone.
entersCardTests :: Registry.Type.Registry -> Tasty.TestTree
entersCardTests registry =
  Tasty.testGroup
    "Enters"
    [ HU.testCase "Soul Warden is a {W} 1/1 Human Cleric whose trigger reads \"whenever ANOTHER creature enters\"" $ do
        p <- Registry.printing registry "Soul Warden"
        let c = Printing.card p
            white = ManaSymbol.OfType (ManaType.Colored Color.White)
        HU.assertEqual "name" (Text.pack "Soul Warden") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [white]) (Card.Type.manaCost c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
        HU.assertEqual "subtypes" (Set.fromList [Subtype.Human, Subtype.Cleric]) (TypeLine.subtypes (Card.Type.typeLine c))
        case Card.Type.triggeredAbilities c of
          [ability] -> do
            -- "another" is `Not IsSource` INSIDE the condition's Filter, which
            -- is the one spelling Filter.IsSource fixes for it (#163) -- there
            -- is no exclusion flag beside the Filter to get out of step with.
            HU.assertEqual
              "CR 603.6a: whenever another creature enters"
              (TriggerCondition.PermanentEnters (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.IsSource]))
              (TriggeredAbility.condition ability)
            HU.assertEqual "no intervening \"if\" (CR 603.4)" Nothing (TriggeredAbility.intervening ability)
            case Foldable.toList (Modal.modes (TriggeredAbility.modal ability)) of
              [m] -> do
                -- CR 109.5's "you": the ability's controller, and no target
                -- slot at all -- the effect never reads the entering creature.
                HU.assertEqual
                  "\"you gain 1 life\""
                  [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 1)]
                  (Foldable.toList (Mode.effects m))
                HU.assertEqual "targetless" Map.empty (Mode.targetSpecs m)
              modes -> HU.assertFailure ("expected one mode, got " <> show (length modes))
          abilities -> HU.assertFailure ("expected one triggered ability, got " <> show (length abilities))
    ]

unspentManaCardTests :: Registry.Type.Registry -> Tasty.TestTree
unspentManaCardTests registry =
  Tasty.testGroup
    "Unspent mana"
    [ -- The modern Oracle wording is "don't LOSE unspent mana", CR 106.4's verb,
      -- not "mana pools don't empty" -- and it is symmetric, which is why the
      -- scope is EachPlayer and the effect needs no mana-type argument.
      HU.testCase "Upwelling is a {3}{G} Enchantment with one EachPlayer DontLoseUnspentMana ability" $ do
        p <- Registry.printing registry "Upwelling"
        let c = Printing.card p
            green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        HU.assertEqual "name" (Text.pack "Upwelling") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Generic 3, green]) (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "no object-axis static abilities" [] (Card.Type.staticAbilities c)
        HU.assertEqual
          "one player ability"
          [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer PlayerEffect.DontLoseUnspentMana]
          (Card.Type.playerAbilities c)
    ]

phyrexianCardTests :: Registry.Type.Registry -> Tasty.TestTree
phyrexianCardTests registry =
  Tasty.testGroup
    "Phyrexian"
    [ HU.testCase "Mutagenic Growth is a {G/P} Instant giving target creature +2/+2" $ do
        p <- Registry.printing registry "Mutagenic Growth"
        let c = Printing.card p
            target = SlotName.MkSlotName (Text.pack "target")
        HU.assertEqual "name" (Text.pack "Mutagenic Growth") (Card.Type.name c)
        HU.assertEqual "cost" (costOf [ManaSymbol.Phyrexian Color.Green]) (Card.Type.manaCost c)
        HU.assertBool "an instant" (Card.isInstant c)
        HU.assertEqual
          "+2/+2 until end of turn"
          [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 2)) target]
          (Card.allEffects c)
        HU.assertEqual "one creature slot" (Map.singleton target (TargetSpec.MkTargetSpec Pool.Creatures Nothing)) (Card.allTargetSpecs c),
      -- CR 202.3g: "Each Phyrexian mana symbol in a card's mana cost contributes
      -- 1 to its mana value." Not 2 (the life half is not mana at all, so CR
      -- 202.3f's "largest component" reading does not apply) and not 0.
      HU.testCase "CR 202.3g Mutagenic Growth's Phyrexian symbol makes mana value 1" $ do
        p <- Registry.printing registry "Mutagenic Growth"
        HU.assertEqual "one" 1 (Quantity.manaValueOf (Printing.card p))
    ]

-- CR 506.4's "an effect specifically removes it from combat", as printed.
-- Labyrinth of Skophos is a Land with two activated abilities and no mana cost:
-- "{T}: Add {C}. / {4}, {T}: Remove target attacking or blocking creature from
-- combat." (Murders at Karlov Manor Commander; oracle text checked against
-- Scryfall.) The gameplay proof is Pawl.CombatSpec's EffectRemoval group.
removeFromCombatCardTests :: Registry.Type.Registry -> Tasty.TestTree
removeFromCombatCardTests registry =
  Tasty.testGroup
    "RemoveFromCombat"
    [ HU.testCase "Labyrinth of Skophos is a Land with a {T} colorless mana ability and a {4}, {T} removal ability" $ do
        labyrinth <- Registry.printing registry "Labyrinth of Skophos"
        let c = Printing.card labyrinth
            target = SlotName.MkSlotName (Text.pack "target")
        HU.assertEqual "name" (Text.pack "Labyrinth of Skophos") (Card.Type.name c)
        HU.assertEqual "no mana cost" Nothing (Card.Type.manaCost c)
        HU.assertEqual "types" (Set.singleton CardType.Land) (TypeLine.types (Card.Type.typeLine c))
        HU.assertEqual "not basic, so Blood Moon reaches it (CR 305.8)" Set.empty (TypeLine.supertypes (Card.Type.typeLine c))
        HU.assertEqual "no land types of its own" Set.empty (TypeLine.subtypes (Card.Type.typeLine c))
        HU.assertEqual "nothing on the spell half: a land is never cast (CR 305.1)" [] (Card.allEffects c)
        case Card.Type.activatedAbilities c of
          [mana, removal] -> do
            -- CR 605.1a: the mana half. Tap only, one colorless.
            HU.assertEqual "the mana ability's cost is the tap alone" [CostComponent.TapThis] (Cost.Type.components (ActivatedAbility.cost mana))
            HU.assertEqual "and it names no mana" (Just (ManaCost.MkManaCost [])) (Cost.Type.mana (ActivatedAbility.cost mana))
            HU.assertEqual "adds colorless" [Effect.AddMana (ManaProduction.OfType ManaType.Colorless)] (Modal.allEffects (ActivatedAbility.modal mana))
            -- The removal half: {4} on top of the same tap.
            HU.assertEqual "the removal ability taps too" [CostComponent.TapThis] (Cost.Type.components (ActivatedAbility.cost removal))
            HU.assertEqual "and costs {4}" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) (Cost.Type.mana (ActivatedAbility.cost removal))
            HU.assertEqual "removes the target from combat" [Effect.RemoveFromCombat target] (Modal.allEffects (ActivatedAbility.modal removal))
            -- CR 508.1k / CR 509.1g: "attacking or blocking" is two atoms under
            -- one Or, over the creature pool.
            HU.assertEqual
              "target attacking or blocking creature"
              (Map.singleton target (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Or [Filter.Type.IsAttacking, Filter.Type.IsBlocking]))))
              (Modal.allTargetSpecs (ActivatedAbility.modal removal))
          _ -> HU.assertFailure "expected exactly two activated abilities"
    ]

-- CR 509.1c's blocking requirement on a CREATURE card rather than an Aura. Prized
-- Unicorn is a {3}{G} 2/2 Creature -- Unicorn whose whole text is one line: "All
-- creatures able to block this creature do so." (Magic 2010; name, cost, type
-- line, oracle text and P/T checked against Scryfall.) Its shape is the whole
-- point next to Lure's, above: the same field, the other Affected. The gameplay
-- proof, including CR 604.2's layer-6 strip, is Pawl.CombatSpec's
-- BlockRequirements group.
blockRequirementCardTests :: Registry.Type.Registry -> Tasty.TestTree
blockRequirementCardTests registry =
  Tasty.testGroup
    "BlockRequirements"
    [ HU.testCase "Prized Unicorn is a {3}{G} 2/2 Unicorn whose only ability is a requirement naming ITSELF" $ do
        p <- Registry.printing registry "Prized Unicorn"
        let card = Printing.card p
            green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        HU.assertEqual "name" (Text.pack "Prized Unicorn") (Card.Type.name card)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, green])) (Card.Type.manaCost card)
        HU.assertEqual "types" (Set.singleton CardType.Creature) (TypeLine.types (Card.Type.typeLine card))
        HU.assertEqual "subtypes" (Set.singleton Subtype.Unicorn) (TypeLine.subtypes (Card.Type.typeLine card))
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power card)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness card)
        -- "THIS CREATURE", not "enchanted creature": the requirement names its own
        -- source, which the predicate language already spells Filter.IsSource.
        -- Lure's Affected.Attached is the contrast -- same field, the other
        -- Affected -- and it is why Pawl.BlockRequirement resolves the attacker
        -- through Projection.affects rather than reading an ObjectId.
        HU.assertEqual
          "one requirement, naming the source itself"
          [BlockRequirement.MkBlockRequirement (Affected.Matching Filter.Type.IsSource)]
          (Card.Type.blockRequirements card)
        HU.assertEqual "and it modifies no characteristic" [] (Card.Type.staticAbilities card)
        HU.assertEqual "no spell effects" [] (Card.allEffects card)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Card"
    [cardTests registry, lintTests registry, m2aCardTests registry, m2bCardTests registry, m2cCardTests registry, basicLandTests registry, m3cCardTests registry, m3eCardTests registry, m4bCardTests registry, m45p6CardTests registry, m45p7CardTests registry, m45p11CardTests registry, m55CardTests registry, auraCardTests registry, animatorCardTests registry, worldCardTests registry, cyclingCardTests registry, revealCardTests registry, entersCardTests registry, unspentManaCardTests registry, phyrexianCardTests registry, removeFromCombatCardTests registry, blockRequirementCardTests registry]
