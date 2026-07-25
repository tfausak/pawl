-- Covers Pawl.Card: card data, type-line rules, every printing, and the D4
-- dataflow lint.
module Pawl.CardSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
-- The logic module, alongside Pawl.Type.Modal below: unambiguous under one
-- alias because the two modules export disjoint names (TriggerSpec's
-- precedent), and Modal.allEffects is how this lint reaches an activated or
-- triggered ability's effects (Card.allEffects only reaches the spell).
import qualified Pawl.Mana as Mana
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
-- Aliased Condition.Type, matching Pawl.Type.Count below and the project-wide
-- convention (FilterSpec/CardSpec's Filter.Type note): Pawl.Condition may
-- later be imported and must not collide.
import qualified Pawl.Type.Condition as Condition.Type
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Layer as Layer
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerEffect as PlayerEffect
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.PlayerScope as PlayerScope
import qualified Pawl.Type.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

redCost :: [ManaSymbol.ManaSymbol] -> Maybe ManaCost.ManaCost
redCost symbols = Just (ManaCost.MkManaCost symbols)

m2aCardTests :: Registry.Type.Registry -> Tasty.TestTree
m2aCardTests registry =
  let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
   in Tasty.testGroup
        "M2aCards"
        [ HU.testCase "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
            birdMaiden <- Registry.printing registry "Bird Maiden"
            let c = Printing.card birdMaiden
            HU.assertEqual "name" (Text.pack "Bird Maiden") (Card.Type.name c)
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost c)
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
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c),
          HU.testCase "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
            ogreSentry <- Registry.printing registry "Ogre Sentry"
            let c = Printing.card ogreSentry
            HU.assertEqual "name" (Text.pack "Ogre Sentry") (Card.Type.name c)
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c),
          HU.testCase "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
            windseekerCentaur <- Registry.printing registry "Windseeker Centaur"
            let c = Printing.card windseekerCentaur
            HU.assertEqual "name" (Text.pack "Windseeker Centaur") (Card.Type.name c)
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red, red]) (Card.Type.manaCost c)
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c),
          HU.testCase "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
            goblinChariot <- Registry.printing registry "Goblin Chariot"
            let c = Printing.card goblinChariot
            HU.assertEqual "name" (Text.pack "Goblin Chariot") (Card.Type.name c)
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost c)
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
                  Card.Type.spell = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1),
                  Card.Type.activatedAbilities = [],
                  Card.Type.replacementEffects = [],
                  Card.Type.triggeredAbilities = [],
                  Card.Type.delayedAbilities = Map.empty,
                  Card.Type.castingPermissions = [],
                  Card.Type.characteristicPT = Nothing,
                  Card.Type.playerAbilities = [],
                  Card.Type.mulliganAction = [],
                  Card.Type.openingHandAction = [],
                  Card.Type.additionalCosts = [],
                  Card.Type.alternativeCosts = [],
                  Card.Type.enchant = Nothing
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
quantityCounts :: Quantity.Type.Quantity -> [Count.Type.Count]
quantityCounts quantity = case quantity of
  Quantity.Type.Literal _ -> []
  Quantity.Type.ManaValue -> []
  Quantity.Type.X -> []
  Quantity.Type.Star -> []
  Quantity.Type.Plus a b -> quantityCounts a <> quantityCounts b
  Quantity.Type.Count count -> [count]

-- Every Count reachable from a Condition: its own count, plus any inside its
-- threshold Quantity.
conditionCounts :: Condition.Type.Condition -> [Count.Type.Count]
conditionCounts (Condition.Type.MkCondition count _ threshold) =
  count : quantityCounts threshold

-- Every Count reachable from a Duration: only ForAsLongAs (CR 611.2b) carries
-- a Condition.
durationCounts :: Duration.Duration -> [Count.Type.Count]
durationCounts duration = case duration of
  Duration.UntilEndOfTurn -> []
  Duration.Indefinite -> []
  Duration.UntilYourNextTurn -> []
  Duration.ForAsLongAs condition -> conditionCounts condition

-- Every Count reachable from a Modification: only its P/T quantities
-- (layers 7b/7c) carry one.
modificationCounts :: Modification.Modification -> [Count.Type.Count]
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
triggerConditionCounts :: TriggerCondition.TriggerCondition -> [Count.Type.Count]
triggerConditionCounts triggerCondition = case triggerCondition of
  TriggerCondition.SelfEnters -> []
  TriggerCondition.StepBegins _ _ -> []
  TriggerCondition.StateIs condition -> conditionCounts condition
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []

-- Every Count reachable from one effect: its own Quantity/Duration fields,
-- and -- for Create/CreateEmblem -- every Count in the embedded token/emblem
-- card (the same nesting Pawl.Codec's round trip walks).
effectCounts :: Effect.Effect Card.Type.Card -> [Count.Type.Count]
effectCounts effect = case effect of
  Effect.DealDamage _ quantity -> quantityCounts quantity
  Effect.ModifyTarget duration modification _ -> durationCounts duration <> modificationCounts modification
  Effect.ChangeText _ -> []
  Effect.AddMana _ -> []
  Effect.Search _ -> []
  Effect.ExileAllGraveyards -> []
  Effect.ExileHandThenDraw -> []
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy _ -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone _ _ -> []
  Effect.Draw quantity -> quantityCounts quantity
  Effect.Mill _ quantity -> quantityCounts quantity
  Effect.Discard _ quantity -> quantityCounts quantity
  Effect.Create quantity card _ -> quantityCounts quantity <> cardCounts card
  Effect.Replace duration _ _ -> durationCounts duration
  Effect.Counter _ -> []
  Effect.PutCounters _ quantity _ -> quantityCounts quantity
  Effect.GainPlayerCounters _ quantity -> quantityCounts quantity
  Effect.Untap _ -> []
  Effect.GainControl duration _ -> durationCounts duration
  Effect.ArmDelayedTrigger _ -> []
  Effect.AffectPlayers duration _ _ -> durationCounts duration
  Effect.CreateEmblem card -> cardCounts card
  Effect.BecomeMonarch _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.PlaySubgame _ -> []

-- Every Count reachable from one triggered ability (a card's own, or a
-- delayed one -- both TriggeredAbility Card): its TriggerCondition, its
-- intervening "if" clause, and its modes' effects.
triggeredAbilityCounts :: TriggeredAbility.TriggeredAbility Card.Type.Card -> [Count.Type.Count]
triggeredAbilityCounts ability =
  triggerConditionCounts (TriggeredAbility.condition ability)
    <> foldMap conditionCounts (TriggeredAbility.intervening ability)
    <> concatMap effectCounts (Modal.allEffects (TriggeredAbility.modal ability))

-- Every Count reachable from a card: every site a Pawl.Type.Count can be
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

cardCounts :: Card.Type.Card -> [Count.Type.Count]
cardCounts card =
  concatMap quantityCounts (Maybe.maybeToList (Card.Type.characteristicPT card))
    <> concatMap (\(Power.MkPower quantity) -> quantityCounts quantity) (Maybe.maybeToList (Card.Type.power card))
    <> concatMap (\(Toughness.MkToughness quantity) -> quantityCounts quantity) (Maybe.maybeToList (Card.Type.toughness card))
    <> concatMap (modificationCounts . StaticAbility.modification) (Card.Type.staticAbilities card)
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
      -- Every committed file name must therefore already be its own slug, which
      -- Registry.slugs enforces since #167: it raises UnslugifiableFile rather
      -- than enumerating past such a file. Not sweeping over a listing here, then
      -- -- succeeding at all IS the assertion, and it covers the whole directory.
      HU.testCase "every file name in data/cards is already a slug" $ do
        found <- Registry.slugs registry
        HU.assertBool "the corpus is not empty" (not (null found)),
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
      -- RESOLVES, because a stored control grant is the one shape CR 800.4a's
      -- second clause cannot end. Projection.givesControlTo recognizes a stored
      -- grant by its baked PlayerId; SetControllerToSource carries none (its
      -- player is the source's controller, CR 109.5), so a stored one would
      -- survive a departure the rule says ends it. See Pawl.Departure's proofs.
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
      HU.testCase "Murder is a {1}{B}{B} Instant that destroys a target creature" $ do
        murder <- Registry.printing registry "Murder"
        let c = Printing.card murder
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, black, black])) (Card.Type.manaCost c)
        HU.assertBool "an instant" (Card.isInstant c)
        HU.assertEqual "effect destroys the target slot" [Effect.Destroy (SlotName.MkSlotName (Text.pack "target"))] (Card.allEffects c)
        HU.assertEqual "one CreatureTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures Nothing)) (Card.allTargetSpecs c),
      HU.testCase "Unsummon is a {U} Instant that bounces a target creature to hand" $ do
        unsummon <- Registry.printing registry "Unsummon"
        let c = Printing.card unsummon
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        HU.assertEqual "cost" (Just (ManaCost.MkManaCost [blue])) (Card.Type.manaCost c)
        HU.assertEqual "effect returns to hand" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Hand] (Card.allEffects c),
      HU.testCase "Angelic Edict is a {4}{W} Sorcery exiling a creature or enchantment" $ do
        angelicEdict <- Registry.printing registry "Angelic Edict"
        let c = Printing.card angelicEdict
        HU.assertBool "not an instant" (not (Card.isInstant c))
        HU.assertEqual "effect exiles" [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Exile] (Card.allEffects c)
        HU.assertEqual "creature-or-enchantment slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment])))) (Card.allTargetSpecs c),
      HU.testCase "Divination is a {2}{U} Sorcery that draws two cards with no target" $ do
        divination <- Registry.printing registry "Divination"
        let c = Printing.card divination
        HU.assertEqual "effect draws two" [Effect.Draw (Quantity.Type.Literal 2)] (Card.allEffects c)
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
        HU.assertEqual "one PlayerTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players Nothing)) (Card.allTargetSpecs c)
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
          [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.ReduceSpellCost (Filter.Type.HasColor Color.Blue) 1)]
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
              [m] -> HU.assertEqual "adds colorless" [Effect.AddMana ManaType.Colorless] (Foldable.toList (Mode.effects m))
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
          _ -> HU.assertFailure "expected exactly one triggered ability"
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
          [StaticAbility.MkStaticAbility Affected.Attached (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 1))]
          (Card.Type.staticAbilities card)
        -- CR 303.4: an Aura spell has no spell effects; it enters attached.
        HU.assertEqual "no spell effects" [] (Card.allEffects card)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Card"
    [cardTests registry, lintTests registry, m2aCardTests registry, m2bCardTests registry, m2cCardTests registry, basicLandTests registry, m3cCardTests registry, m3eCardTests registry, m4bCardTests registry, m45p6CardTests registry, m45p7CardTests registry, m45p11CardTests registry, m55CardTests registry, auraCardTests registry]
