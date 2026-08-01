-- Covers Pawl.Engine.Card: card data, type-line rules, every printing, and the D4
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
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Subtype (subtypeToJson)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Event as Event
-- The logic module, alongside Pawl.Types.Modal below: unambiguous under one
-- alias because the two modules export disjoint names (TriggerSpec's
-- precedent), and Modal.allEffects is how this lint reaches an activated or
-- triggered ability's effects (Card.allEffects only reaches the spell).
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
-- Aliased Condition.Type, matching Pawl.Types.Count below and the project-wide
-- convention (FilterSpec/CardSpec's Filter.Type note): Pawl.Engine.Condition may
-- later be imported and must not collide.

-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
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

-- Not red-specific despite its first callers: just the Maybe wrapper every
-- printed mana cost needs (CR 202.1).
costOf :: [ManaSymbol.ManaSymbol] -> Maybe ManaCost.ManaCost
costOf symbols = Just (ManaCost.MkManaCost symbols)

m2aCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m2aCardSpec s registry = Spec.describe s "M2aCards" $ do
  let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      green = ManaSymbol.OfType (ManaType.Colored Color.Green)
      black = ManaSymbol.OfType (ManaType.Colored Color.Black)
  Spec.it s "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
    birdMaiden <- Registry.printing registry "Bird Maiden"
    let c = Printing.card birdMaiden
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Bird Maiden")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
    Spec.assertEqWith
      s
      "subtypes"
      (TypeLine.subtypes (Card.Type.typeLine c))
      (Set.fromList [Subtype.Human, Subtype.Bird])
  Spec.it s "Nimble Birdsticker is a {2}{R} 2/3 Goblin with reach" $ do
    nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
    let c = Printing.card nimbleBirdsticker
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Nimble Birdsticker")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 3)))
  Spec.it s "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
    ogreSentry <- Registry.printing registry "Ogre Sentry"
    let c = Printing.card ogreSentry
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Ogre Sentry")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 1, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 3)))
  Spec.it s "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
    windseekerCentaur <- Registry.printing registry "Windseeker Centaur"
    let c = Printing.card windseekerCentaur
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Windseeker Centaur")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 1, red, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
  Spec.it s "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
    goblinChariot <- Registry.printing registry "Goblin Chariot"
    let c = Printing.card goblinChariot
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Goblin Chariot")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
  Spec.it s "Glistener Elf is a {G} 1/1 Phyrexian Elf Warrior with infect" $ do
    glistenerElf <- Registry.printing registry "Glistener Elf"
    let c = Printing.card glistenerElf
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Glistener Elf")
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertBool s (elem Keyword.Infect (Card.Type.keywords c)) "has infect"
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Phyrexian, Subtype.Elf, Subtype.Warrior])
  Spec.it s "Branchblight Stalker is a {1}{G} 3/1 Phyrexian Elf Scout with toxic 2" $ do
    stalker <- Registry.printing registry "Branchblight Stalker"
    let c = Printing.card stalker
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Branchblight Stalker")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 1, green])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toxic 2, and nothing else" (Card.Type.keywords c) (Set.singleton (Keyword.Toxic 2))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Phyrexian, Subtype.Elf, Subtype.Scout])
  -- "Enchanted creature has poisonous 3", so the keyword is on the
  -- Aura's layer-6 GRANT and not in its own printed keyword set -- the
  -- distinction the assertions below draw.
  Spec.it s "Snake Cult Initiation is a {3}{B} Aura granting poisonous 3" $ do
    initiation <- Registry.printing registry "Snake Cult Initiation"
    let c = Printing.card initiation
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Snake Cult Initiation")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 3, black])
    Spec.assertBool s (Card.isAura c) "is an Aura"
    Spec.assertEqWith s "no printed keywords of its own" (Card.Type.keywords c) Set.empty
    Spec.assertEqWith
      s
      "one static ability: the enchanted creature gains poisonous 3"
      (Card.Type.staticAbilities c)
      [StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton (Modification.GainKeyword (Keyword.Poisonous 3)))]
  Spec.it s "every M2a printing carries exactly its keyword" $
    mapM_
      ( \(name, keyword) -> do
          p <- Registry.printing registry name
          let c = Printing.card p
          Spec.assertBool s (Card.isCreature c) "creature"
          Spec.assertBool s (not (Card.isLand c)) "not land"
          Spec.assertEqWith s "exactly this keyword" (Card.Type.keywords c) (Set.singleton keyword)
      )
      S.m2aKeywords

cardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
cardSpec s registry = Spec.describe s "Card" $ do
  Spec.it s "Mountain printing is named Mountain" $ do
    mountain <- Registry.printing registry "Mountain"
    Spec.assertEqWith s "name" (Card.Type.name (Printing.card mountain)) (Text.pack "Mountain")
  Spec.it s "Mountain is a Land" $ do
    mountain <- Registry.printing registry "Mountain"
    Spec.assertBool s (Card.isLand (Printing.card mountain)) "isLand"
  Spec.it s "Mountain has the Mountain subtype" $ do
    mountain <- Registry.printing registry "Mountain"
    Spec.assertBool s (Set.member Subtype.Mountain (TypeLine.subtypes (Card.Type.typeLine (Printing.card mountain)))) "subtype"
  Spec.it s "Mountain type line contains Land" $ do
    mountain <- Registry.printing registry "Mountain"
    Spec.assertBool s (Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card mountain)))) "cardtype"
  -- CR 202.1: a land has no mana cost. Not a zero cost -- no cost at all.
  Spec.it s "Mountain has no mana cost" $ do
    mountain <- Registry.printing registry "Mountain"
    Spec.assertEqWith s "no cost" (Card.Type.manaCost (Printing.card mountain)) Nothing
  Spec.it s "Mountain has no power or toughness" $ do
    mountain <- Registry.printing registry "Mountain"
    Spec.assertEqWith s "power" (Card.Type.power (Printing.card mountain)) Nothing
    Spec.assertEqWith s "toughness" (Card.Type.toughness (Printing.card mountain)) Nothing
  Spec.it s "Piker printing is named Goblin Piker" $ do
    piker <- Registry.printing registry "Goblin Piker"
    Spec.assertEqWith s "name" (Card.Type.name (Printing.card piker)) (Text.pack "Goblin Piker")
  Spec.it s "Piker costs {1}{R}" $ do
    piker <- Registry.printing registry "Goblin Piker"
    Spec.assertEqWith
      s
      "cost"
      (Card.Type.manaCost (Printing.card piker))
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
  Spec.it s "Piker is a 2/1" $ do
    piker <- Registry.printing registry "Goblin Piker"
    Spec.assertEqWith s "power" (Card.Type.power (Printing.card piker)) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness (Printing.card piker)) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
  Spec.it s "Piker is a Goblin Warrior" $ do
    piker <- Registry.printing registry "Goblin Piker"
    Spec.assertEqWith
      s
      "subtypes"
      (TypeLine.subtypes (Card.Type.typeLine (Printing.card piker)))
      (Set.fromList [Subtype.Goblin, Subtype.Warrior])
  Spec.it s "Piker is a creature and not a land" $ do
    piker <- Registry.printing registry "Goblin Piker"
    Spec.assertBool s (Card.isCreature (Printing.card piker)) "creature"
    Spec.assertBool s (not (Card.isLand (Printing.card piker))) "not land"
  -- CR 110.1: the classification resolution turns on. Never card identity.
  Spec.it s "CR 110.1 both a Piker and a Mountain are permanents" $ do
    piker <- Registry.printing registry "Goblin Piker"
    mountain <- Registry.printing registry "Mountain"
    Spec.assertBool s (Card.isPermanent (Printing.card piker)) "piker"
    Spec.assertBool s (Card.isPermanent (Printing.card mountain)) "mountain"
  Spec.it s "CR 110.1 an instant is not a permanent type" $
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
              Card.Type.loyalty = Nothing,
              Card.Type.keywords = Set.empty,
              Card.Type.colorIndicator = Set.empty,
              Card.Type.staticAbilities = [],
              Card.Type.spell = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1),
              Card.Type.activatedAbilities = [],
              Card.Type.replacementEffects = [],
              Card.Type.triggeredAbilities = [],
              Card.Type.delayedAbilities = Map.empty,
              Card.Type.castingPermissions = [],
              Card.Type.castingRestrictions = [],
              Card.Type.characteristicPT = Nothing,
              Card.Type.playerAbilities = [],
              Card.Type.blockRequirements = [],
              Card.Type.attackRequirements = [],
              Card.Type.mulliganAction = [],
              Card.Type.openingHandAction = [],
              Card.Type.additionalCosts = [],
              Card.Type.alternativeCosts = [],
              Card.Type.enchant = Nothing,
              Card.Type.counterability = Counterability.Counterable
            }
     in do
          Spec.assertBool s (not (Card.isPermanent card)) "not a permanent"
          Spec.assertBool s (Card.isInstant card) "an instant"
  Spec.it s "a Piker is not an instant" $ do
    piker <- Registry.printing registry "Goblin Piker"
    Spec.assertBool s (not (Card.isInstant (Printing.card piker))) "creature"

-- Every Count reachable from a Quantity: a leaf Count directly, or one nested
-- through Plus's two children (CR 208.2 composition -- a printed 1+*).
quantityCounts :: Quantity.Type.Quantity -> [Count.Type.Count Quantity.Type.Quantity]
quantityCounts quantity = case quantity of
  Quantity.Type.Literal _ -> []
  Quantity.Type.ManaValue -> []
  Quantity.Type.Power -> []
  Quantity.Type.X -> []
  -- A slot read, not a fold over game state: the value was bound by an earlier
  -- effect of the same resolution and there is no Count inside it.
  Quantity.Type.InSlot _ -> []
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
  -- CR 701.9a's discard condition is a PlayerRelation, which holds no Count.
  TriggerCondition.PlayerDiscards _ -> []
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> []
  TriggerCondition.SelfDies -> []
  -- CR 701.6a's countering condition is a PlayerRelation, which holds no Count,
  -- exactly as the discard condition above.
  TriggerCondition.SpellOrAbilityCounters _ -> []

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
  Effect.Tap _ -> []
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
  Effect.TakeExtraTurn {} -> []

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
-- than chosen: Pawl.Engine.Binding.triggerSource's comment spells out that an
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

-- Pawl.Engine.Binding's reserved slot names in full: the binding keys the engine
-- STAMPS rather than asks a player for. The whole module's list rather than a
-- hand-picked subset, so a new reserved slot joins the declaration sweep below
-- by being added here and nowhere else.
reservedSlots :: Set.Set SlotName.SlotName
reservedSlots =
  Set.fromList
    [ Binding.variableX,
      Binding.chosenModes,
      Binding.copySource,
      Binding.triggerSource,
      Binding.you,
      Binding.triggerPlayer,
      Binding.became
    ]

-- Every slot a card DECLARES as a target: its spell modes plus CR 303.4a's
-- enchant slot (Card.allTargetSpecs), and its activated, triggered and delayed
-- abilities' modes.
--
-- All four carriers, because all four ask the same question at different
-- moments. CR 601.2c is the question ("the player announces their choice of an
-- appropriate object or player for each target the spell requires"); CR 602.2b
-- makes activating an ability follow "rules 601.2b-i"; CR 603.3d makes putting
-- a triggered ability on the stack "identical to the process for casting a
-- spell listed in rules 601.2c-d"; and CR 603.7's delayed abilities are
-- triggered abilities, placed by that same rule. A lint about what DECLARING a
-- slot means therefore has to range over all four, not over the spell alone.
declaredTargetSlots :: Card.Type.Card -> Set.Set SlotName.SlotName
declaredTargetSlots card =
  Set.unions
    ( Map.keysSet (Card.allTargetSpecs card)
        : fmap
          (Map.keysSet . Modal.allTargetSpecs)
          ( fmap ActivatedAbility.modal (Card.Type.activatedAbilities card)
              <> fmap TriggeredAbility.modal (Card.Type.triggeredAbilities card)
              <> fmap TriggeredAbility.modal (Map.elems (Card.Type.delayedAbilities card))
          )
    )

-- The reserved names a card declares as target slots -- empty for every card
-- authored correctly, which is the whole of the sweep's assertion.
--
-- Declaring one is the SECOND INVARIANT's failure mode rather than a dataflow
-- nicety: the slot is prompted (CR 601.2c) and the answer then thrown away. On
-- a triggered or delayed ability, Pawl.Engine.Engine stamps CR 113.7's `self` and CR
-- 109.5's `you` OVER the chosen targets, so the player is asked and overruled.
-- CR 400.7e's `became` and CR 603.2's `thatPlayer` run the other way -- the
-- chosen target wins the union and the event's own stamp is lost, so the
-- payload silently reads the answer to a question about something else. Either
-- way the engine asked a question it did not use.
reservedDeclarations :: Card.Type.Card -> Set.Set SlotName.SlotName
reservedDeclarations = Set.intersection reservedSlots . declaredTargetSlots

-- CR 111.4: "A spell or ability that creates a token sets both its name and its
-- subtype(s). If the spell or ability doesn't specify the name of the token, its
-- name is the same as its subtype(s) plus the word 'Token.'" True of every token
-- this pool creates, because no card in it specifies a token name.
--
-- Compared against every PERMUTATION of the subtypes rather than one rendering,
-- and that is forced rather than chosen: TypeLine.subtypes is a Set, so a
-- multi-subtype token's printed word order ("Zombie Berserker Token", not
-- "Berserker Zombie Token") is not recoverable from the card. The lint therefore
-- pins which subtypes appear and never their order (#477). Permuting also keeps
-- it correct for CR 205.3b's two-WORD creature types, which splitting the name
-- on spaces would not. The factorial is bounded by a token's subtype count, at
-- most two here.
--
-- Narrow this the first time a card DOES specify a token's name, at which point
-- the rule supplies nothing and the name is whatever the card says: CR 111.9's
-- legendary tokens ("create Boo, a legendary 1/1 red Hamster creature token"),
-- CR 111.10's predefined tokens (111.10d's Walker, 111.10j-r's Roles), and the
-- copy tokens of CR 111.4's own Spitting Image example (named Doomed Dissenter,
-- "not Human Token or Doomed Dissenter Token") are each correctly named
-- something this lint would reject.
tokenNameOffends :: Card.Type.Card -> Bool
tokenNameOffends token =
  case traverse (fmap fst . Json.tag . subtypeToJson) (Set.toList (TypeLine.subtypes (Card.Type.typeLine token))) of
    Left _ -> True
    Right subtypes ->
      notElem
        (Card.Type.name token)
        (fmap (\ordering -> Text.unwords (ordering <> [Text.pack "Token"])) (List.permutations subtypes))

-- The D4 dataflow lint: every slot an effect reads is declared, and every
-- declared slot is read. Equality, not subset: a spec no effect reads is a
-- card announcing a target it ignores -- representable in Magic, not in this
-- pool. Loosen to superset if such a card ever lands.
lintSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
lintSpec s registry = Spec.describe s "Lint" $ do
  Spec.it s "every mode's slot reads equal its declared slots" $ do
    ps <- S.allPrintings s
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
    Spec.assertEqWith s "no dangling or unused slots" (fmap (Card.Type.name . Printing.card) offenders) []
  Spec.it s "every file in data/cards loads, and its card is named by its file name" $ do
    -- The registry checks name-against-file-name on each load (Pawl.Registry.load),
    -- so sweeping the listing is the whole assertion: a stray file, a file whose
    -- card was renamed, and a file that no test happens to name all fail here.
    -- A hand-kept list is exactly what forgets the file nobody loads.
    slugs <- S.corpusSlugs
    Spec.assertBool s (not (null slugs)) "the corpus is not empty"
    mapM_ (Registry.card registry) slugs
  -- The other direction: Registry.card slugifies the NAME it is asked for,
  -- then builds a path from that slug -- so a file whose stem is not itself a
  -- slugify fixed point is never opened by that path; a lookup would quietly
  -- open some OTHER file (or none) instead of raising the mismatch above.
  -- Every committed file name must therefore already be its own slug.
  -- Slug.fromText normalizes rather than validates, so the assertion is that
  -- it is the identity on every stem -- read the listing directly, because
  -- Corpus.slugsIn has already normalized the evidence away.
  Spec.it s "every file name in data/cards is already a slug" $ do
    root <- Registry.defaultRoot
    entries <- Directory.listDirectory root
    let stems = fmap (reverse . drop (length ".json") . reverse) (filter (List.isSuffixOf ".json") entries)
    Spec.assertBool s (not (null stems)) "the corpus is not empty"
    mapM_
      ( \stem ->
          Spec.assertEqWith
            s
            ("file name is its own slug: " <> stem)
            (Slug.unwrap (Slug.fromText (Text.pack stem)))
            (Text.pack stem)
      )
      stems
  Spec.it s "Blaze is a {X}{R} Sorcery dealing X to any target" $ do
    blaze <- Registry.printing registry "Blaze"
    let card = Printing.card blaze
        red = ManaSymbol.OfType (ManaType.Colored Color.Red)
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Blaze")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (Just (ManaCost.MkManaCost [ManaSymbol.Variable, red]))
    Spec.assertBool s (not (Card.isInstant card)) "sorcery, not instant"
    Spec.assertEqWith s "one AnyTarget slot" (Card.allTargetSpecs card) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
    Spec.assertEqWith s "effect deals X" (Card.allEffects card) [Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) Quantity.Type.X]
  Spec.it s "the lint itself catches a dangling reference" $
    let bad = Set.unions [Resolve.slotsOf (Effect.DealDamage (SlotName.MkSlotName (Text.pack "ghost")) (Quantity.Type.Literal 3))]
     in Spec.assertBool s (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSpec.TargetSpec)) "misauthored card detected"
  Spec.it s "every printing that reads X declares {X}, and vice versa" $ do
    ps <- S.allPrintings s
    let readsX c = Resolve.readsX (Card.allEffects c)
        hasVariable c = case Card.Type.manaCost c of
          Nothing -> False
          Just (ManaCost.MkManaCost syms) -> elem ManaSymbol.Variable syms
        offenders =
          filter
            (\p -> readsX (Printing.card p) /= hasVariable (Printing.card p))
            ps
    Spec.assertEqWith s "X read iff {X} declared" (fmap (Card.Type.name . Printing.card) offenders) []
  Spec.it s "CR 111.4 every token a card creates is named its subtypes plus \"Token\"" $ do
    ps <- S.allPrintings s
    let tokensOf card = [token | Effect.Create _ token _ _ <- cardResolutionEffects card]
        tokens = concatMap (tokensOf . Printing.card) ps
    -- Guards the sweep against passing vacuously if Create ever moves out
    -- from under cardResolutionEffects.
    Spec.assertBool s (not (null tokens)) "the pool creates tokens"
    Spec.assertEqWith s "no token is misnamed" (fmap Card.Type.name (filter tokenNameOffends tokens)) []
  Spec.it s "the lint itself catches a token named without the suffix" $ do
    doomedTraveler <- Registry.printing registry "Doomed Traveler"
    case [token | Effect.Create _ token _ _ <- cardResolutionEffects (Printing.card doomedTraveler)] of
      [token] -> do
        Spec.assertBool s (not (tokenNameOffends token)) "the real token passes"
        -- The exact misauthoring CR 111.4 forbids: the bare subtype, with
        -- the suffix dropped.
        Spec.assertBool s (tokenNameOffends token {Card.Type.name = Text.pack "Spirit"}) "misnamed token detected"
      other -> Spec.assertFailure s ("expected exactly one Create, got " <> show (length other))
  -- ONE sweep over the whole reserved set, replacing the five per-name
  -- cases this grew out of. Those five each filtered on
  -- Card.allTargetSpecs, so they saw a card's spell modes and enchant slot
  -- and nothing else; they also covered only five of the seven reserved
  -- names, leaving `copySource` and `thatPlayer` with no declaration case
  -- at all. See reservedDeclarations for why declaring one is a discarded
  -- prompt rather than a naming quibble.
  Spec.it s "no reserved binding slot is ever a declared target slot" $ do
    ps <- S.allPrintings s
    let offends = not . Set.null . reservedDeclarations
        offenders = filter (offends . Printing.card) ps
    Spec.assertEqWith s "no card declares a reserved slot" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The sweep above passes VACUOUSLY: no committed card declares a reserved
  -- slot anywhere, so on its own it proves nothing about the lint. Proven
  -- here instead against hand-built offenders, in the posture the
  -- triggered-read self-test below uses -- never a card file, because a
  -- misauthored card must not be loadable -- one per ability carrier the
  -- sweep gained, each grafted onto a real card that has that kind of
  -- ability.
  --
  -- Each carrier is asserted TWICE: the sweep sees the offender, and
  -- Card.allTargetSpecs -- the spell-modes-and-enchant view the five old
  -- cases filtered on -- does not. The second half is the regression guard:
  -- it is the hole itself, and it fails if the sweep is ever narrowed back.
  Spec.it s "the lint itself catches an ability that declares a reserved slot" $ do
    roaches <- Registry.printing registry "Endless Cockroaches"
    sorcerer <- Registry.printing registry "Prodigal Sorcerer"
    tidalWave <- Registry.printing registry "Tidal Wave"
    let -- A one-mode, effectless modal declaring exactly one target slot.
        declaring slot =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode Seq.empty (Map.singleton slot (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)) Optionality.Mandatory))
            (ModeSelection.ChooseExactly 1)
        withTriggered slot card =
          card
            { Card.Type.triggeredAbilities =
                [ TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.SelfDies,
                      TriggeredAbility.modal = declaring slot,
                      TriggeredAbility.intervening = Nothing
                    }
                ]
            }
        withActivated slot card =
          card {Card.Type.activatedAbilities = fmap (\a -> a {ActivatedAbility.modal = declaring slot}) (Card.Type.activatedAbilities card)}
        withDelayed slot card =
          card {Card.Type.delayedAbilities = fmap (\t -> t {TriggeredAbility.modal = declaring slot}) (Card.Type.delayedAbilities card)}
        catches slot graft printing =
          let card = graft slot (Printing.card printing)
           in (reservedDeclarations card, Map.member slot (Card.allTargetSpecs card))
    Spec.assertEqWith
      s
      "CR 109.5 you declared on a triggered ability is caught, and the spell-modes view misses it"
      (catches Binding.you withTriggered roaches)
      (Set.singleton Binding.you, False)
    Spec.assertEqWith
      s
      "CR 113.7 self declared on an activated ability is caught, and the spell-modes view misses it"
      (catches Binding.triggerSource withActivated sorcerer)
      (Set.singleton Binding.triggerSource, False)
    Spec.assertEqWith
      s
      "CR 400.7e became declared on a delayed ability is caught, and the spell-modes view misses it"
      (catches Binding.became withDelayed tidalWave)
      (Set.singleton Binding.became, False)
    -- Not vacuous the other way either: the sweep reaches an ability's
    -- ORDINARY slots, which Card.allTargetSpecs cannot see -- Prodigal
    -- Sorcerer's spell is a creature's empty mode, so its "target" is
    -- declared by its activated ability alone.
    let target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith
      s
      "an activated ability's ordinary slot is in the sweep but not the spell-modes view"
      (Set.member target (declaredTargetSlots (Printing.card sorcerer)), Map.member target (Card.allTargetSpecs (Printing.card sorcerer)))
      (True, False)
    Spec.assertEqWith
      s
      "and the three real cards declare no reserved slot"
      (fmap (reservedDeclarations . Printing.card) [roaches, sorcerer, tidalWave])
      [Set.empty, Set.empty, Set.empty]
  Spec.it s "Lightning Bolt is in the red pool with one AnyTarget slot" $ do
    lightningBolt <- Registry.printing registry "Lightning Bolt"
    let card = Printing.card lightningBolt
    Spec.assertBool s (Card.isInstant card) "an instant"
    Spec.assertEqWith s "one slot" (Card.allTargetSpecs card) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
  -- The AbilityName half of the D4 dataflow lint (CR 603.7): an
  -- ArmDelayedTrigger naming an ability the card does not declare is a FAILING
  -- TEST, never a trigger that silently never fires. Equality, not subset: a
  -- declared ability nothing arms is dead card text.
  --
  -- SCOPE, same posture as Pawl.Engine.Binding's D4-lint-scope comment: this and the
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
  Spec.it s "every armed delayed ability is declared, and every declared one is armed" $ do
    ps <- S.allPrintings s
    let cardOffends card =
          Resolve.armedAbilities (Card.allEffects card) /= Map.keysSet (Card.Type.delayedAbilities card)
        offenders = filter (cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling or unused delayed abilities" (fmap (Card.Type.name . Printing.card) offenders) []
  -- Every slot a delayed ability READS must be one the arming card DEFINES:
  -- the reserved trigger-source slot, or a token bound by a Create.
  Spec.it s "every slot a delayed ability reads is bound by its card" $ do
    ps <- S.allPrintings s
    let cardOffends card =
          let available = Set.insert Binding.triggerSource (Resolve.definedSlots (Card.allEffects card))
              wanted = Set.unions (fmap Resolve.slotsOf (Card.delayedEffects card))
           in not (Set.isSubsetOf wanted available)
        offenders = filter (cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling delayed-ability slot" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The same subset shape over a card's TRIGGERED abilities, which is where
  -- the condition-specific reserved slots live -- CR 400.7e's `became` and
  -- CR 702.70a's `thatPlayer`. See triggeredAbilityOffends for the available
  -- side and for why this cannot be an equality check.
  --
  -- No ACTIVATED-ability counterpart of this read check exists (#479).
  Spec.it s "every slot a triggered ability reads is bound for its condition" $ do
    ps <- S.allPrintings s
    let cardOffends = any triggeredAbilityOffends . Card.Type.triggeredAbilities
        offenders = filter (cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling triggered-ability slot" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The sweep above passes VACUOUSLY: no committed card misauthors the
  -- pairing, so the sweep proves nothing about the lint. Both directions are
  -- proven here instead, against a hand-built offender (never a card file --
  -- a misauthored card must not be loadable) and against the real pairing.
  --
  -- Both reserved event slots, because a classification that answered "every
  -- slot, always" would pass the offending half of either one alone.
  Spec.it s "the lint itself catches a reserved event slot the condition never binds" $ do
    roaches <- Registry.printing registry "Endless Cockroaches"
    let -- Endless Cockroaches' own payload: "return it to its owner's hand".
        returnIt = Effect.MoveToZone Binding.became Zone.Hand
        -- Rule 702.70a's shape, as a targetless read of "that player".
        thatPlayerDraws = Effect.Draw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1)
    Spec.assertBool
      s
      (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfEnters returnIt))
      "CR 400.7e became under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies returnIt)))
      "and under a dies trigger it is accepted"
    Spec.assertBool
      s
      (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies thatPlayerDraws))
      "CR 702.70a thatPlayer under a dies trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDealsCombatDamageToPlayer thatPlayerDraws)))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (not (any triggeredAbilityOffends (Card.Type.triggeredAbilities (Printing.card roaches))))
      "the real card's dies trigger is accepted"
  -- CR 603.7c: binding a slot to a MULTI-token Create would silently name one
  -- of them. Rejected rather than guessed (#53).
  Spec.it s "no Create binds a slot while making more than one token" $ do
    ps <- S.allPrintings s
    let offenders =
          filter
            (Resolve.bindsSeveralTokens . Card.allEffects . Printing.card)
            ps
    Spec.assertEqWith s "no multi-token binding" (fmap (Card.Type.name . Printing.card) offenders) []
  -- CR 400.1: every InZone Count over a shared zone (battlefield, stack,
  -- exile, command) must pair with PlayerRef.EachPlayer -- the type
  -- permits any PlayerRef there, but only EachPlayer is meaningful for a
  -- zone no player owns individually (#161).
  Spec.it s "every InZone Count over a shared zone pairs with EachPlayer" $ do
    ps <- S.allPrintings s
    let offenders =
          filter
            (cardOffendsSharedZoneScope . Printing.card)
            ps
    Spec.assertEqWith s "no shared-zone scope with a non-EachPlayer ref" (fmap (Card.Type.name . Printing.card) offenders) []
  Spec.it s "a card with no enchant ability declares no enchant slot" $ do
    piker <- Registry.printing registry "Goblin Piker"
    let card = Printing.card piker
    Spec.assertEqWith s "no enchant spec" (Card.Type.enchant card) Nothing
    Spec.assertBool s (not (Card.isAura card)) "not an Aura"
    Spec.assertEqWith s "no enchant slot" (Card.enchantSpecs card) Map.empty
  -- CR 303.4 / 702.5a: the biconditional. An Aura without enchant has no legal
  -- target and could never be cast; a non-Aura with enchant declares a restriction
  -- nothing reads. The D4 lint cannot see either, because it walks
  -- Mode.targetSpecs and the enchant slot is not there (#184's shape).
  Spec.it s "a card is an Aura iff it declares an enchant ability" $ do
    ps <- S.allPrintings s
    let offends c = Card.isAura c /= Maybe.isJust (Card.Type.enchant c)
        offenders = filter (offends . Printing.card) ps
    Spec.assertEqWith s "Aura iff enchant" (fmap (Card.Type.name . Printing.card) offenders) []
  -- Pawl.Engine.Card.allTargetSpecs binds the enchant spec under this name (Task 6), so a
  -- mode declaring it would be silently shadowed.
  -- #199: no card authors a layer-2 control modification into an effect that
  -- RESOLVES. SetControllerToSource is the payload-free constructor and is
  -- INERT when stored: Projection.controllerOfGiven's storedSetter matches only
  -- Modification.SetController, Projection.controlGrants reads control-granting
  -- static abilities off Card.staticAbilities and never off stored effects, and
  -- Projection.applyModification's SetControllerToSource arm is the identity.
  -- A card authoring one would resolve, store the effect, and grant control to
  -- no one -- there is nothing for CR 800.4a to end (see Pawl.Engine.Departure's
  -- proofs).
  --
  -- BOTH control constructors, not just the payload-free one: baking a
  -- PlayerId into static card text is equally unreal, since a card cannot
  -- know who is playing. Control on a card belongs on a STATIC ability
  -- (Control Magic), which the projection re-derives and never stores.
  --
  -- Asked as an EQUALITY on Layer through Projection.layer -- the sanctioned
  -- classification -- rather than by casing on Modification, which only
  -- Pawl.Engine.Projection may do. Layer.Control is exactly the two control
  -- constructors, so this covers a third one automatically.
  --
  -- A codec-level rejection would be the wrong shape: jsonToModification is
  -- shared with staticAbilities, which Control Magic legitimately uses.
  Spec.it s "no card authors a control modification into a resolving effect (#199)" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.ModifyTarget _ modification _ -> Projection.layer modification == Layer.Control
          _ -> False
        offenders = filter (any offends . cardResolutionEffects . Printing.card) ps
    Spec.assertEqWith s "control belongs on a static ability, never in a stored effect" (fmap (Card.Type.name . Printing.card) offenders) []
  -- CR 306.5 / 306.5a: the other card-type biconditional, the Aura/enchant
  -- lint's shape. "Loyalty is a characteristic only planeswalkers have", so a
  -- planeswalker without one has nothing for CR 306.5b's intrinsic replacement
  -- to place and would be buried by CR 704.5i the instant it entered; a
  -- non-planeswalker with a printed loyalty carries a number no rule reads.
  --
  -- Projection.intrinsicReplacementsOf's own comment leans on this in both
  -- directions, which is why it is a lint and not a per-card assertion.
  Spec.it s "a card is a planeswalker iff it has a printed loyalty" $ do
    ps <- S.allPrintings s
    let isPlaneswalker c = Set.member CardType.Planeswalker (TypeLine.types (Card.Type.typeLine c))
        offends c = isPlaneswalker c /= Maybe.isJust (Card.Type.loyalty c)
        offenders = filter (offends . Printing.card) ps
    Spec.assertEqWith s "planeswalker iff loyalty" (fmap (Card.Type.name . Printing.card) offenders) []
  Spec.it s "no mode declares a slot named enchant" $ do
    ps <- S.allPrintings s
    let offends c = any (Map.member Card.enchantSlot . Mode.targetSpecs) (Modal.modes (Card.Type.spell c))
        offenders = filter (offends . Printing.card) ps
    Spec.assertEqWith s "the enchant slot is never hand-declared" (fmap (Card.Type.name . Printing.card) offenders) []

m2bCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m2bCardSpec s registry = Spec.describe s "M2bCards" $ do
  let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      gs0 = Setup.emptyGame S.bothPlayers
  Spec.it s "Sabretooth Tiger is a {2}{R} 2/1 Cat with first strike" $ do
    sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
    let c = Printing.card sabretoothTiger
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Sabretooth Tiger")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.singleton Subtype.Cat)
    Spec.assertEqWith s "keyword" (Card.Type.keywords c) (Set.singleton Keyword.FirstStrike)
  Spec.it s "Ridgetop Raptor is a {3}{R} 2/1 Dinosaur Beast with double strike" $ do
    ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
    let c = Printing.card ridgetopRaptor
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Ridgetop Raptor")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, red]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Dinosaur, Subtype.Beast])
    Spec.assertEqWith s "keyword" (Card.Type.keywords c) (Set.singleton Keyword.DoubleStrike)
  Spec.it s "the tiger has first strike through the projection" $ do
    sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
    let (oid, gs) = S.addCreature sabretoothTiger S.alice gs0
    Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike oid gs) "first strike"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.DoubleStrike oid gs)) "not double strike"
  Spec.it s "the raptor has double strike through the projection" $ do
    ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
    let (oid, gs) = S.addCreature ridgetopRaptor S.alice gs0
    Spec.assertBool s (Projection.hasKeyword Keyword.DoubleStrike oid gs) "double strike"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike oid gs)) "not first strike"
  Spec.it s "both are 2/1s, the same body as a Piker" $ do
    piker <- Registry.printing registry "Goblin Piker"
    sabretoothTiger <- Registry.printing registry "Sabretooth Tiger"
    ridgetopRaptor <- Registry.printing registry "Ridgetop Raptor"
    let bodyOf p = (Card.Type.power (Printing.card p), Card.Type.toughness (Printing.card p))
    Spec.assertEqWith s "tiger body" (bodyOf sabretoothTiger) (bodyOf piker)
    Spec.assertEqWith s "raptor body" (bodyOf ridgetopRaptor) (bodyOf piker)

m2cCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m2cCardSpec s registry = Spec.describe s "M2cCards" $ do
  Spec.it s "Typhoid Rats is a {B} 1/1 Rat with deathtouch" $ do
    typhoidRats <- Registry.printing registry "Typhoid Rats"
    let c = Printing.card typhoidRats
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Typhoid Rats")
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "keywords" (Card.Type.keywords c) (Set.singleton Keyword.Deathtouch)
  Spec.it s "War Mammoth is a {3}{G} 3/3 Elephant with trample" $ do
    warMammoth <- Registry.printing registry "War Mammoth"
    let c = Printing.card warMammoth
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "War Mammoth")
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "keywords" (Card.Type.keywords c) (Set.singleton Keyword.Trample)

basicLandSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
basicLandSpec s registry = Spec.describe s "BasicLand" $ do
  Spec.it s "CR 305.6 a Swamp's intrinsic ability is black mana" $
    Spec.assertEqWith
      s
      "black"
      (Mana.subtypeMana Subtype.Swamp)
      (Just (ManaType.Colored Color.Black))
  Spec.it s "CR 305.6 a Forest's intrinsic ability is green mana" $
    Spec.assertEqWith
      s
      "green"
      (Mana.subtypeMana Subtype.Forest)
      (Just (ManaType.Colored Color.Green))
  Spec.it s "swampPrinting is a basic Swamp land" $ do
    swamp <- Registry.printing registry "Swamp"
    let c = Printing.card swamp
    Spec.assertBool s (Card.isLand c) "land"
    Spec.assertBool
      s
      (Set.member Subtype.Swamp (TypeLine.subtypes (Card.Type.typeLine c)))
      "swamp subtype"
  Spec.it s "forestPrinting is a basic Forest land" $ do
    forest <- Registry.printing registry "Forest"
    let c = Printing.card forest
    Spec.assertBool s (Card.isLand c) "land"
    Spec.assertBool
      s
      (Set.member Subtype.Forest (TypeLine.subtypes (Card.Type.typeLine c)))
      "forest subtype"

m3cCardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
m3cCardSpec s registry = Spec.describe s "M3cCards" $ do
  Spec.it s "Blood Moon is a {2}{R} enchantment with one SetLandSubtype static ability" $ do
    bloodMoon <- Registry.printing registry "Blood Moon"
    let card = Printing.card bloodMoon
    Spec.assertEqWith s "one static ability" (length (Card.Type.staticAbilities card)) 1
    Spec.assertBool s (Map.null (Card.allTargetSpecs card)) "not a permanent target"

m3eCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m3eCardSpec s registry = Spec.describe s "M3eCards" $ do
  Spec.it s "Prodigal Sorcerer has one non-mana activated ability" $ do
    prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
    case Card.Type.activatedAbilities (Printing.card prodigalSorcerer) of
      [ab] -> Spec.assertBool s (not (Mana.isManaAbility ab)) "not a mana ability"
      _ -> Spec.assertFailure s "expected exactly one ability"
  Spec.it s "Llanowar Elves has one mana activated ability" $ do
    llanowarElves <- Registry.printing registry "Llanowar Elves"
    case Card.Type.activatedAbilities (Printing.card llanowarElves) of
      [ab] -> Spec.assertBool s (Mana.isManaAbility ab) "mana ability"
      _ -> Spec.assertFailure s "expected exactly one ability"

m4bCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m4bCardSpec s registry = Spec.describe s "M4bCards" $ do
  Spec.it s "Darksteel Myr is a {3} 0/1 Artifact Creature with indestructible" $ do
    darksteelMyr <- Registry.printing registry "Darksteel Myr"
    let c = Printing.card darksteelMyr
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Darksteel Myr")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 0)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "keyword" (Card.Type.keywords c) (Set.singleton Keyword.Indestructible)
  -- #113: both P9 gate cards end "It can't be regenerated", and the clause was
  -- omitted from their data while nothing could honour it. It is data now.
  Spec.it s "CR 701.19c Terror and Reprisal both carry the can't-be-regenerated rider" $ do
    terror <- Registry.printing registry "Terror"
    reprisal <- Registry.printing registry "Reprisal"
    let riders c = [r | Effect.Destroy _ r _ <- Card.allEffects (Printing.card c)]
    Spec.assertEqWith s "Terror" (riders terror) [Regenerability.CantBeRegenerated]
    Spec.assertEqWith s "Reprisal" (riders reprisal) [Regenerability.CantBeRegenerated]
  Spec.it s "Murder is a {1}{B}{B} Instant that destroys a target creature" $ do
    murder <- Registry.printing registry "Murder"
    let c = Printing.card murder
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, black, black]))
    Spec.assertBool s (Card.isInstant c) "an instant"
    -- Murder carries no CR 701.19c rider, unlike Terror and Reprisal.
    Spec.assertEqWith s "effect destroys the target slot" (Card.allEffects c) [Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Regenerability.Regenerable Nothing]
    Spec.assertEqWith s "one CreatureTarget slot" (Card.allTargetSpecs c) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
  -- Murder's opposite number on the one axis this pair exists to pin: the
  -- SAME Destroy opcode, with the SAME CR 701.19c rider, differing only in
  -- whether its ObjectRef names a cast-time slot or a resolution-time set.
  -- CR 115.10a is why the second declares no target spec: "Unless that object
  -- or player is identified by the word 'target' ..., it's not a target."
  Spec.it s "Day of Judgment is a {2}{W}{W} Sorcery that destroys every creature and targets nothing" $ do
    dayOfJudgment <- Registry.printing registry "Day of Judgment"
    let c = Printing.card dayOfJudgment
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Day of Judgment")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, white, white]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Sorcery)
    -- CR 109.2 supplies the battlefield and the word "permanent"; the card
    -- text is only "all creatures", so the Filter is only HasCardType.
    Spec.assertEqWith
      s
      "one Destroy over the creatures, with no can't-be-regenerated rider"
      (Card.allEffects c)
      [Effect.Destroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing]
    Spec.assertEqWith s "and no target spec at all" (Card.allTargetSpecs c) Map.empty
  -- The pool's counterweight to Day of Judgment: a creature that grants
  -- indestructible to OTHERS and does not have it itself, so it is destroyed
  -- by the same sweep as the permanents it protects. CR 608.2f is what makes
  -- that pairing say something -- the grant is still in force when every
  -- victim's CR 702.12b gate is judged, so the granter dies alone.
  --
  -- "Other permanents you control" needs no new filter vocabulary: `Not
  -- IsSource` is the same spelling of "other" Opalescence's card text uses,
  -- and `ControlledBy You` the same "you control" Ashaya's does.
  Spec.it s "The Walls of Ba Sing Se is an {8} 0/30 Legendary Artifact Creature granting indestructible to OTHER permanents you control" $ do
    walls <- Registry.printing registry "The Walls of Ba Sing Se"
    let c = Printing.card walls
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "The Walls of Ba Sing Se")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 8]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 0)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 30)))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.fromList [CardType.Artifact, CardType.Creature])
    Spec.assertEqWith s "supertypes" (TypeLine.supertypes (Card.Type.typeLine c)) (Set.singleton Supertype.Legendary)
    -- Defender is printed on the card; Indestructible is NOT -- the whole
    -- point is that the granter does not benefit from its own grant.
    Spec.assertEqWith s "printed keywords" (Card.Type.keywords c) (Set.singleton Keyword.Defender)
    Spec.assertEqWith
      s
      "\"Other permanents you control have indestructible\""
      (Card.Type.staticAbilities c)
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
  Spec.it s "Unsummon is a {U} Instant that bounces a target creature to hand" $ do
    unsummon <- Registry.printing registry "Unsummon"
    let c = Printing.card unsummon
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [blue]))
    Spec.assertEqWith s "effect returns to hand" (Card.allEffects c) [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Hand]
  -- Three modifications on ONE target, in printed order. Spelled out rather
  -- than spot-checked because the toxic 1 grant is what makes this card the
  -- CR 702.164b proof in DamageSpec: a card that granted toxic 2 by mistake
  -- would still add up to the poison that test expects.
  Spec.it s "Aspirant's Ascent is a {U} Instant granting +1/+3, flying and toxic 1" $ do
    ascent <- Registry.printing registry "Aspirant's Ascent"
    let c = Printing.card ascent
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        target = SlotName.MkSlotName (Text.pack "target")
        untilEot = Effect.ModifyTarget Duration.UntilEndOfTurn
        targetRef = ObjectRef.InSlot target
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [blue]))
    Spec.assertBool s (Card.isInstant c) "an instant"
    Spec.assertEqWith
      s
      "effects, in printed order"
      (Card.allEffects c)
      [ untilEot (Modification.ModifyPowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 3)) targetRef,
        untilEot (Modification.GainKeyword Keyword.Flying) targetRef,
        untilEot (Modification.GainKeyword (Keyword.Toxic 1)) targetRef
      ]
    Spec.assertEqWith s "one creature slot, shared by all three" (Card.allTargetSpecs c) (Map.singleton target (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
  Spec.it s "Angelic Edict is a {4}{W} Sorcery exiling a creature or enchantment" $ do
    angelicEdict <- Registry.printing registry "Angelic Edict"
    let c = Printing.card angelicEdict
    Spec.assertBool s (not (Card.isInstant c)) "not an instant"
    Spec.assertEqWith s "effect exiles" (Card.allEffects c) [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Exile]
    Spec.assertEqWith s "creature-or-enchantment slot" (Card.allTargetSpecs c) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment]))))
  Spec.it s "Divination is a {2}{U} Sorcery that draws two cards with no target" $ do
    divination <- Registry.printing registry "Divination"
    let c = Printing.card divination
    Spec.assertEqWith s "effect draws two" (Card.allEffects c) [Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 2)]
    Spec.assertBool s (Map.null (Card.allTargetSpecs c)) "no target slots"
  Spec.it s "Tome Scour is a {U} Sorcery milling five from a target player" $ do
    tomeScour <- Registry.printing registry "Tome Scour"
    let c = Printing.card tomeScour
    Spec.assertEqWith s "effect mills five" (Card.allEffects c) [Effect.Mill (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 5)]
    Spec.assertEqWith s "one PlayerTarget slot" (Card.allTargetSpecs c) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players Nothing))
  Spec.it s "Mind Rot is a {2}{B} Sorcery making a target player discard two" $ do
    mindRot <- Registry.printing registry "Mind Rot"
    let c = Printing.card mindRot
    Spec.assertEqWith s "effect discards two" (Card.allEffects c) [Effect.Discard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 2)]
    Spec.assertEqWith s "one PlayerTarget slot" (Card.allTargetSpecs c) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players Nothing))
  -- Two effects of DIFFERENT opcodes reading one slot, in printed order --
  -- the pin that a rewrite reordering the mode's effect list, or splitting
  -- the clauses across two slots, would break.
  Spec.it s "Sign in Blood is a {B}{B} Sorcery drawing two and making one target player lose two life" $ do
    signInBlood <- Registry.printing registry "Sign in Blood"
    let c = Printing.card signInBlood
        target = SlotName.MkSlotName (Text.pack "target")
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [black, black]))
    Spec.assertBool s (not (Card.isInstant c)) "not an instant"
    Spec.assertEqWith
      s
      "draws first, then loses life"
      (Card.allEffects c)
      [ Effect.Draw (PlayerRef.InSlot target) (Quantity.Type.Literal 2),
        Effect.LoseLife (PlayerRef.InSlot target) (Quantity.Type.Literal 2)
      ]
    Spec.assertEqWith s "one PlayerTarget slot, shared by both" (Card.allTargetSpecs c) (Map.singleton target (TargetSpec.MkTargetSpec Pool.Players Nothing))
  -- CR 202.3f: "When calculating the mana value of an object with a hybrid
  -- mana symbol in its mana cost, use the largest component of each hybrid
  -- symbol." Both halves of {R/G} are one mana, so the largest is 1 and
  -- {R/G}{R/G} is mana value 2 -- not 4 (both halves) and not 0 (neither).
  Spec.it s "Burning-Tree Emissary's two hybrid symbols make mana value 2" $ do
    burningTreeEmissary <- Registry.printing registry "Burning-Tree Emissary"
    Spec.assertEqWith s "two" (Quantity.manaValueOf (Printing.card burningTreeEmissary)) 2
  Spec.it s "Flame Javelin is a {2/R}{2/R}{2/R} Instant dealing 4 to any target" $ do
    flameJavelin <- Registry.printing registry "Flame Javelin"
    let c = Printing.card flameJavelin
        twoOrRed = ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Red)
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Flame Javelin")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [twoOrRed, twoOrRed, twoOrRed])
    Spec.assertBool s (Card.isInstant c) "an instant"
    Spec.assertEqWith s "effect deals four" (Card.allEffects c) [Effect.DealDamage target (Quantity.Type.Literal 4)]
    Spec.assertEqWith s "one AnyTarget slot" (Card.allTargetSpecs c) (Map.singleton target (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
  -- CR 202.3f again, whose own worked example is this card's cost: "The mana
  -- value of a card with mana cost {2/B}{2/B}{2/B} is 6." The generic half is
  -- the larger one, so a monocolored hybrid contributes 2 and not the 1 every
  -- other typed symbol contributes -- the detail that silently corrupts every
  -- mana-value reading downstream if it is wrong.
  Spec.it s "Flame Javelin's three monocolored hybrid symbols make mana value 6, not 3" $ do
    flameJavelin <- Registry.printing registry "Flame Javelin"
    Spec.assertEqWith s "six" (Quantity.manaValueOf (Printing.card flameJavelin)) 6

m45p6CardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m45p6CardSpec s registry = Spec.describe s "M45p6Cards" $ do
  Spec.it s "Master Thief is a {2}{U}{U} 2/2 Human Rogue whose ETB steals an artifact" $ do
    masterThief <- Registry.printing registry "Master Thief"
    let c = Printing.card masterThief
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        slot = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Master Thief")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue, blue]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Creature)
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Human, Subtype.Rogue])
    case Card.Type.triggeredAbilities c of
      [ab] -> do
        Spec.assertEqWith s "enters trigger" (TriggeredAbility.condition ab) TriggerCondition.SelfEnters
        case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
          [m] -> do
            Spec.assertEqWith
              s
              "one GainControl effect with a conditional duration"
              (Foldable.toList (Mode.effects m))
              [Effect.GainControl (Duration.ForAsLongAs S.youControlSource) (ObjectRef.InSlot slot)]
            Spec.assertEqWith
              s
              "one ArtifactTarget slot"
              (Mode.targetSpecs m)
              (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact))))
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one triggered ability"
  Spec.it s "Hag of Inner Weakness is a {2}{B} 2/2 Hag Warlock with an upkeep -2/-1 trigger" $ do
    hagOfInnerWeakness <- Registry.printing registry "Hag of Inner Weakness"
    let c = Printing.card hagOfInnerWeakness
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        slot = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Hag of Inner Weakness")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, black]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Creature)
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Hag, Subtype.Warlock])
    case Card.Type.triggeredAbilities c of
      [ab] -> do
        Spec.assertEqWith
          s
          "beginning of your upkeep"
          (TriggeredAbility.condition ab)
          (TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
        case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
          [m] -> do
            Spec.assertEqWith
              s
              "-2/-1 until your next turn"
              (Foldable.toList (Mode.effects m))
              [Effect.ModifyTarget Duration.UntilYourNextTurn (Modification.ModifyPowerToughness (Quantity.Type.Literal (-2)) (Quantity.Type.Literal (-1))) (ObjectRef.InSlot slot)]
            Spec.assertEqWith
              s
              "one OpponentCreatureTarget slot"
              (Mode.targetSpecs m)
              (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.ControlledBy PlayerRelation.Opponent))))
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one triggered ability"

m45p7CardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m45p7CardSpec s registry = Spec.describe s "M4.5 P7" $ do
  Spec.it s "Rule of Law is a {2}{W} enchantment with one EachPlayer CantCastMoreThan 1 player ability" $ do
    ruleOfLaw <- Registry.printing registry "Rule of Law"
    let c = Printing.card ruleOfLaw
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Rule of Law")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, white]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith s "no object-axis static abilities" (Card.Type.staticAbilities c) []
    Spec.assertEqWith
      s
      "one player ability"
      (Card.Type.playerAbilities c)
      [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.CantCastMoreThan 1)]
  Spec.it s "Thalia is a {1}{W} 2/1 Legendary Human Soldier with first strike and one IncreaseSpellCost ability" $ do
    thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
    let c = Printing.card thalia
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Thalia, Guardian of Thraben")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "supertypes" (TypeLine.supertypes (Card.Type.typeLine c)) (Set.singleton Supertype.Legendary)
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Human, Subtype.Soldier])
    Spec.assertEqWith s "keywords" (Card.Type.keywords c) (Set.singleton Keyword.FirstStrike)
    Spec.assertEqWith
      s
      "one player ability"
      (Card.Type.playerAbilities c)
      [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer (PlayerEffect.IncreaseSpellCost (Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)) 1)]
  Spec.it s "Sapphire Medallion is a {2} artifact with one You ReduceSpellCost Blue ability" $ do
    sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
    let c = Printing.card sapphireMedallion
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Sapphire Medallion")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Artifact)
    Spec.assertEqWith
      s
      "one player ability"
      (Card.Type.playerAbilities c)
      [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.ReduceSpellCost (Filter.Type.HasColor Color.Blue) (ManaCost.MkManaCost [ManaSymbol.Generic 1]))]
  -- The reduction that NAMES a mana type, as against the Medallion's generic
  -- one: "Cleric spells you cast cost {W}{B} less to cast."
  Spec.it s "Edgewalker is a {1}{W}{B} Human Cleric with one You ReduceSpellCost {W}{B} ability" $ do
    edgewalker <- Registry.printing registry "Edgewalker"
    let c = Printing.card edgewalker
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Edgewalker")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Creature)
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Human, Subtype.Cleric])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
    Spec.assertEqWith
      s
      "one player ability"
      (Card.Type.playerAbilities c)
      [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.ReduceSpellCost (Filter.Type.HasSubtype Subtype.Cleric) (ManaCost.MkManaCost [white, black]))]
  Spec.it s "Reliquary Tower is a land with a You NoMaximumHandSize ability and a {T} colorless mana ability" $ do
    reliquaryTower <- Registry.printing registry "Reliquary Tower"
    let c = Printing.card reliquaryTower
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Reliquary Tower")
    Spec.assertEqWith s "no mana cost" (Card.Type.manaCost c) Nothing
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Land)
    Spec.assertEqWith s "not basic" (TypeLine.supertypes (Card.Type.typeLine c)) Set.empty
    Spec.assertEqWith
      s
      "one player ability"
      (Card.Type.playerAbilities c)
      [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.NoMaximumHandSize]
    case Card.Type.activatedAbilities c of
      [ab] -> do
        Spec.assertEqWith s "tap cost only" (Cost.Type.components (ActivatedAbility.cost ab)) [CostComponent.TapThis]
        Spec.assertEqWith s "a real {0} mana cost, not an unpayable one (CR 118.5a/118.6)" (Cost.Type.mana (ActivatedAbility.cost ab)) (Just (ManaCost.MkManaCost []))
        case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
          [m] -> Spec.assertEqWith s "adds colorless" (Foldable.toList (Mode.effects m)) [Effect.AddMana (ManaProduction.OfType ManaType.Colorless)]
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one activated ability"
  -- Radiant Fountain, a Land: "When this land enters, you gain 2 life. /
  -- {T}: Add {C}." A nonbasic land whose whole text box is rules-text
  -- abilities of two different kinds, which is what CR 305.7's strip needs
  -- to reach (Pawl.TriggerSpec).
  Spec.it s "Radiant Fountain is a nonbasic land with a SelfEnters life gain and a {T} colorless mana ability" $ do
    radiantFountain <- Registry.printing registry "Radiant Fountain"
    let c = Printing.card radiantFountain
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Radiant Fountain")
    Spec.assertEqWith s "no mana cost" (Card.Type.manaCost c) Nothing
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Land)
    Spec.assertEqWith s "not basic, so Blood Moon reaches it (CR 305.8)" (TypeLine.supertypes (Card.Type.typeLine c)) Set.empty
    Spec.assertEqWith s "no land types of its own" (TypeLine.subtypes (Card.Type.typeLine c)) Set.empty
    Spec.assertEqWith s "no player abilities" (Card.Type.playerAbilities c) []
    case Card.Type.triggeredAbilities c of
      [ab] -> do
        Spec.assertEqWith s "on its own entry (CR 603.6a)" (TriggeredAbility.condition ab) TriggerCondition.SelfEnters
        case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
          [m] -> Spec.assertEqWith s "you gain 2" (Foldable.toList (Mode.effects m)) [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 2)]
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one triggered ability"
    case Card.Type.activatedAbilities c of
      [ab] -> do
        Spec.assertEqWith s "tap cost only" (Cost.Type.components (ActivatedAbility.cost ab)) [CostComponent.TapThis]
        case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
          [m] -> Spec.assertEqWith s "adds colorless" (Foldable.toList (Mode.effects m)) [Effect.AddMana (ManaProduction.OfType ManaType.Colorless)]
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one activated ability"
  Spec.it s "Silence is a {W} instant whose one effect is AffectPlayers UntilEndOfTurn Opponents CantCastSpells" $ do
    silence <- Registry.printing registry "Silence"
    let c = Printing.card silence
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Silence")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [white]))
    Spec.assertBool s (Card.isInstant c) "an instant"
    Spec.assertEqWith s "no player abilities: it is not a permanent" (Card.Type.playerAbilities c) []
    Spec.assertEqWith
      s
      "one targetless opcode"
      (Card.allEffects c)
      [Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells]
    Spec.assertEqWith s "no target slots" (Card.allTargetSpecs c) Map.empty

m45p11CardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
m45p11CardSpec s registry = Spec.describe s "M4.5 P11" $ do
  Spec.it s "Palace Jailer is a {2}{W}{W} 2/2 Human Soldier with two ETB triggers" $ do
    palaceJailer <- Registry.printing registry "Palace Jailer"
    let c = Printing.card palaceJailer
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Palace Jailer")
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "two triggered abilities" (length (Card.Type.triggeredAbilities c)) 2

-- M5.5 pinned the migrated per-card conditions at the CODEC level: the decoded
-- card must equal the Condition the fixture spells out, so a decoding regression
-- fails here rather than surfacing as a behavioural oddity somewhere downstream.
-- Master Thief's ForAsLongAs is pinned this way in m45p6CardSpec; this is
-- Barbarian Outcast's StateIs, which had only behavioural coverage (#165).
m55CardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m55CardSpec s registry = Spec.describe s "M5.5" $ do
  Spec.it s "Barbarian Outcast's state trigger is a Count of exactly 0 Swamps you control (CR 603.8)" $ do
    barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
    let c = Printing.card barbarianOutcast
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Barbarian Outcast")
    case Card.Type.triggeredAbilities c of
      [ab] -> do
        Spec.assertEqWith
          s
          "the decoded condition is S.youControlNoSwamps"
          (TriggeredAbility.condition ab)
          (TriggerCondition.StateIs S.youControlNoSwamps)
        case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
          [m] -> Spec.assertEqWith s "one Sacrifice self effect" (Foldable.toList (Mode.effects m)) [Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self"))]
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one triggered ability"
  -- The Aggregation.Greatest gate card (#254). The pin that matters is the
  -- SHAPE: one Draw whose Quantity is a Count, with the per-member quantity
  -- inside the AGGREGATION rather than beside it -- the arrangement that
  -- makes "the greatest mana value among artifacts you control" one value
  -- and not a card-specific opcode.
  Spec.it s "One with the Machine is a {3}{U} Sorcery drawing the greatest mana value among artifacts you control" $ do
    oneWithTheMachine <- Registry.printing registry "One with the Machine"
    let c = Printing.card oneWithTheMachine
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "One with the Machine")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 3, blue])
    Spec.assertBool s (not (Card.isInstant c)) "sorcery, not instant"
    Spec.assertEqWith
      s
      "one Draw aimed at the caster, counting the battlefield"
      (Card.allEffects c)
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
    Spec.assertEqWith s "and no target slots" (Card.allTargetSpecs c) Map.empty

-- The Auras phase (a) gate card: CR 303.4m's Attached affected-set, proven by a
-- real Aura on a real creature rather than a synthetic fixture.
auraCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
auraCardSpec s registry = Spec.describe s "Auras" $ do
  Spec.it s "Unholy Strength is a {B} Aura enchanting a creature for +2/+1" $ do
    p <- Registry.printing registry "Unholy Strength"
    let card = Printing.card p
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (Just (ManaCost.MkManaCost [black]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine card)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine card)) (Set.singleton Subtype.Aura)
    Spec.assertBool s (Card.isAura card) "is an Aura"
    -- CR 702.5a: "Enchant creature" -- the whole creature pool, unnarrowed.
    Spec.assertEqWith s "enchant creature" (Card.Type.enchant card) (Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
    -- CR 303.4m: "enchanted creature gets +2/+1" -- layer 7c on whatever it is
    -- attached to.
    Spec.assertEqWith
      s
      "one +2/+1 static ability on the enchanted permanent"
      (Card.Type.staticAbilities card)
      [StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 1)))]
    -- CR 303.4: an Aura spell has no spell effects; it enters attached.
    Spec.assertEqWith s "no spell effects" (Card.allEffects card) []
  -- The pool's first CR 509.1c blocking requirement, and the first card whose
  -- whole ability lives on neither staticAbilities nor playerAbilities --
  -- which is the correction this file's presence records.
  Spec.it s "Lure is a {1}{G}{G} Aura whose only ability is a CR 509.1c blocking requirement" $ do
    p <- Registry.printing registry "Lure"
    let card = Printing.card p
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Lure")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, green, green]))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine card)) (Set.singleton Subtype.Aura)
    Spec.assertEqWith s "enchant creature" (Card.Type.enchant card) (Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
    -- CR 303.4m: "all creatures able to block ENCHANTED CREATURE do so".
    Spec.assertEqWith
      s
      "one requirement, naming whatever the Aura is attached to"
      (Card.Type.blockRequirements card)
      [BlockRequirement.MkBlockRequirement Affected.Attached]
    Spec.assertEqWith s "and it modifies no characteristic" (Card.Type.staticAbilities card) []
    Spec.assertEqWith s "no spell effects" (Card.allEffects card) []
  -- Not an Aura itself, but the only card in the pool that MOVES one: CR
  -- 701.3's Attach keyword action aimed at a permanent already on the
  -- battlefield. Its shape is the whole design argument -- one target slot for
  -- the Aura, no slot at all for the destination.
  Spec.it s "Crown of the Ages is a {2} artifact whose {4},{T} ability moves an Aura" $ do
    p <- Registry.printing registry "Crown of the Ages"
    let c = Printing.card p
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Crown of the Ages")
    -- The {4} is the ACTIVATION cost; the card itself costs {2}.
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Artifact)
    Spec.assertEqWith s "no enchant ability: it is not an Aura" (Card.Type.enchant c) Nothing
    case Card.Type.activatedAbilities c of
      [ab] -> do
        Spec.assertEqWith s "tap cost" (Cost.Type.components (ActivatedAbility.cost ab)) [CostComponent.TapThis]
        Spec.assertEqWith s "plus {4}" (Cost.Type.mana (ActivatedAbility.cost ab)) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]))
        case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
          [m] -> do
            -- "to another CREATURE" is the destination filter, and it is a
            -- bare Filter rather than a target spec: CR 701.3 / the card's own
            -- ruling, "this only targets the Aura and not either creature".
            Spec.assertEqWith
              s
              "CR 701.3: attach the targeted permanent to a chosen creature"
              (Foldable.toList (Mode.effects m))
              [Effect.AttachTarget target (Filter.Type.HasCardType CardType.Creature)]
            -- "target Aura attached to a creature" -- the one slot, and the
            -- only place IsAttachedToCreature appears in the pool.
            Spec.assertEqWith
              s
              "CR 115.1: one target slot, an Aura on a creature"
              (Mode.targetSpecs m)
              (Map.singleton target (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.And [Filter.Type.HasSubtype Subtype.Aura, Filter.Type.IsAttachedToCreature]))))
          ms -> Spec.assertFailure s ("expected one mode, got " <> show (length ms))
      abs_ -> Spec.assertFailure s ("expected one activated ability, got " <> show (length abs_))
  -- CR 702.5d's gate card: the first Aura in the pool whose enchant ability
  -- names a PLAYER, and the first affected set reached through one.
  Spec.it s "Curse of Death's Hold is a {3}{B}{B} Aura Curse enchanting a player for -1/-1" $ do
    p <- Registry.printing registry "Curse of Death's Hold"
    let card = Printing.card p
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Curse of Death's Hold")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (costOf [ManaSymbol.Generic 3, black, black])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine card)) (Set.singleton CardType.Enchantment)
    -- CR 205.3h: "Enchantment -- Aura Curse" is two enchantment types.
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine card)) (Set.fromList [Subtype.Aura, Subtype.Curse])
    Spec.assertBool s (Card.isAura card) "is an Aura"
    -- CR 702.5d: "Enchant player" -- the whole player pool, unnarrowed, which
    -- is what lets it target and be attached to a player and nothing else.
    Spec.assertEqWith s "enchant player" (Card.Type.enchant card) (Just (TargetSpec.MkTargetSpec Pool.Players Nothing))
    -- CR 303.4m through the enchanted PLAYER: "creatures enchanted player
    -- controls get -1/-1", layer 7c on a set the Aura is not attached to.
    Spec.assertEqWith
      s
      "one -1/-1 static ability on the enchanted player's creatures"
      (Card.Type.staticAbilities card)
      [StaticAbility.MkStaticAbility (Affected.AttachedPlayerControls (Filter.Type.HasCardType CardType.Creature)) (NonEmpty.singleton (Modification.ModifyPowerToughness (Quantity.Type.Literal (-1)) (Quantity.Type.Literal (-1))))]
    -- CR 303.4: an Aura spell has no spell effects; it enters attached.
    Spec.assertEqWith s "no spell effects" (Card.allEffects card) []
  -- The second Effect.AttachTarget producer in the pool, and the shape is the
  -- design argument: ONE target slot for the Aura (CR 601.2c -- Gatherer is
  -- explicit for Crown of the Ages that "this only targets the Aura"), two
  -- effects in the order written (CR 608.2c), and a destination Filter that
  -- asks about the SUBJECT rather than about the candidate.
  Spec.it s "Aura Graft is a {1}{U} instant that gains an Aura and then moves it" $ do
    p <- Registry.printing registry "Aura Graft"
    let card = Printing.card p
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Aura Graft")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (costOf [ManaSymbol.Generic 1, blue])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine card)) (Set.singleton CardType.Instant)
    Spec.assertEqWith s "no enchant ability: it is not an Aura itself" (Card.Type.enchant card) Nothing
    case Foldable.toList (Modal.modes (Card.Type.spell card)) of
      [m] -> do
        -- Gatherer, 2007-07-15: "Aura Graft's effect has no duration", so the
        -- control change is Duration.Indefinite rather than end of turn.
        Spec.assertEqWith
          s
          "gain control indefinitely, then attach"
          (Foldable.toList (Mode.effects m))
          [ Effect.GainControl Duration.Indefinite (ObjectRef.InSlot target),
            Effect.AttachTarget target Filter.Type.CanHostSubject
          ]
        -- "target Aura THAT'S ATTACHED TO A PERMANENT" -- the one slot, and the
        -- only place IsAttachedToPermanent appears in the pool.
        Spec.assertEqWith
          s
          "CR 115.1: one target slot, an Aura on a permanent"
          (Mode.targetSpecs m)
          (Map.singleton target (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.And [Filter.Type.HasSubtype Subtype.Aura, Filter.Type.IsAttachedToPermanent]))))
      ms -> Spec.assertFailure s ("expected one mode, got " <> show (length ms))

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
animatorCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
animatorCardSpec s registry = Spec.describe s "Animators" $ do
  -- The CR 613.6 gate card (#233), and the pin that matters is the SHAPE: one
  -- static ability with three parts, not three abilities. Its affected set
  -- reads a card type its own layer-4 part changes, so the parts have to stay
  -- one effect or the layer-7b part loses the set.
  Spec.it s "March of the Machines is a {3}{U} enchantment: ONE ability, three parts, one affected set" $ do
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
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "March of the Machines")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 3, blue])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith
      s
      "\"is an artifact creature with power and toughness each equal to its mana value\""
      (Card.Type.staticAbilities c)
      [ StaticAbility.MkStaticAbility
          noncreatureArtifact
          ( Modification.AddCardType CardType.Artifact
              NonEmpty.:| [ Modification.AddCardType CardType.Creature,
                            Modification.SetBasePowerToughness Quantity.Type.ManaValue Quantity.Type.ManaValue
                          ]
          )
      ]
  -- The same shape, arrived at from the other direction: Humility and
  -- Opalescence were each TWO abilities before #233 and are now one with two
  -- parts. Nothing observable changed for them -- their filters read card types
  -- they do not themselves change -- but the model has to be uniform, and this
  -- is the pin that keeps a future card from re-splitting them.
  Spec.it s "Humility and Opalescence are each one two-part ability, not two abilities" $ do
    humility <- Registry.printing registry "Humility"
    opalescence <- Registry.printing registry "Opalescence"
    let partsOf p = fmap (NonEmpty.toList . StaticAbility.modifications) (Card.Type.staticAbilities (Printing.card p))
    Spec.assertEqWith
      s
      "CR 613.1f + CR 613.4b: lose all abilities, base 1/1"
      (partsOf humility)
      [[Modification.LoseAllAbilities, Modification.SetBasePowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1)]]
    Spec.assertEqWith
      s
      "CR 613.1d + CR 613.4b: becomes a creature, base P/T its mana value"
      (partsOf opalescence)
      [[Modification.AddCardType CardType.Creature, Modification.SetBasePowerToughness Quantity.Type.ManaValue Quantity.Type.ManaValue]]
  Spec.it s "Liquimetal Coating is a {2} artifact whose {T} ability makes any permanent an artifact" $ do
    p <- Registry.printing registry "Liquimetal Coating"
    let c = Printing.card p
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Liquimetal Coating")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Artifact)
    case Card.Type.activatedAbilities c of
      [ab] -> do
        -- CR 107.5: the tap symbol is the entire activation cost.
        Spec.assertEqWith s "tap cost only" (Cost.Type.components (ActivatedAbility.cost ab)) [CostComponent.TapThis]
        Spec.assertEqWith s "and no mana" (Cost.Type.mana (ActivatedAbility.cost ab)) (Just (ManaCost.MkManaCost []))
        case Foldable.toList (Modal.modes (ActivatedAbility.modal ab)) of
          [m] -> do
            Spec.assertEqWith
              s
              "CR 613.1d: one layer-4 addition, until end of turn"
              (Foldable.toList (Mode.effects m))
              [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.AddCardType CardType.Artifact) (ObjectRef.InSlot target)]
            -- "Target permanent", unnarrowed -- the Aura the CR 303.4d case
            -- needs is a legal target precisely because there is no filter.
            Spec.assertEqWith
              s
              "CR 115.1: one target slot, any permanent"
              (Mode.targetSpecs m)
              (Map.singleton target (TargetSpec.MkTargetSpec Pool.Permanents Nothing))
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one activated ability"
  Spec.it s "Skilled Animator is a {2}{U} 1/3 Human Artificer whose ETB animates an artifact you control" $ do
    p <- Registry.printing registry "Skilled Animator"
    let c = Printing.card p
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        target = SlotName.MkSlotName (Text.pack "target")
        duration = Duration.ForAsLongAs sourceOnBattlefield
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Skilled Animator")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2, blue])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Human, Subtype.Artificer])
    case Card.Type.triggeredAbilities c of
      [ab] -> do
        Spec.assertEqWith s "CR 603.6a: it triggers on entering" (TriggeredAbility.condition ab) TriggerCondition.SelfEnters
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
            Spec.assertEqWith
              s
              "three parts, all on the target, all for the same duration"
              (Foldable.toList (Mode.effects m))
              [ Effect.ModifyTarget duration (Modification.AddCardType CardType.Artifact) (ObjectRef.InSlot target),
                Effect.ModifyTarget duration (Modification.AddCardType CardType.Creature) (ObjectRef.InSlot target),
                Effect.ModifyTarget duration (Modification.SetBasePowerToughness (Quantity.Type.Literal 5) (Quantity.Type.Literal 5)) (ObjectRef.InSlot target)
              ]
            Spec.assertEqWith
              s
              "CR 115.1: one target slot, an artifact you control"
              (Mode.targetSpecs m)
              (Map.singleton target (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.And [Filter.Type.HasCardType CardType.Artifact, Filter.Type.ControlledBy PlayerRelation.You]))))
          _ -> Spec.assertFailure s "expected exactly one mode"
      _ -> Spec.assertFailure s "expected exactly one triggered ability"

-- CR 702.29: the pool's first cycling card. Barkhide Mauler is a vanilla 4/4
-- whose only text is the keyword, so nothing else about it can stand in for the
-- keyword when a cycling test passes.
cyclingCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
cyclingCardSpec s registry = Spec.describe s "Cycling" $ do
  Spec.it s "Windcaller Aven is a {4}{U}{U} 4/3 with flying, Cycling {U} and a cycling trigger" $ do
    p <- Registry.printing registry "Windcaller Aven"
    let c = Printing.card p
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Windcaller Aven")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 4, blue, blue])
    -- Two keywords, one printed and one that mints an ability: rule 702.9's
    -- flying is read where evasion is asked about, and rule 702.29a's cycling
    -- is minted by Pawl.Engine.Keyword.
    Spec.assertEqWith
      s
      "flying and Cycling {U}"
      (Card.Type.keywords c)
      (Set.fromList [Keyword.Flying, Keyword.Cycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [blue])) []) Nothing])
    case Card.Type.triggeredAbilities c of
      [ability] ->
        Spec.assertEqWith
          s
          "CR 702.29c: it triggers on being cycled"
          (TriggeredAbility.condition ability)
          TriggerCondition.SelfCycled
      abilities -> Spec.assertFailure s ("expected one triggered ability, got " <> show (length abilities))
  Spec.it s "Ash Barrens is a Land with {T}: Add {C} and basic landcycling {1}" $ do
    p <- Registry.printing registry "Ash Barrens"
    let c = Printing.card p
        basicLand = Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic]
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Ash Barrens")
    Spec.assertEqWith s "a land, with no mana cost (CR 202.1)" (Card.Type.manaCost c) Nothing
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Land)
    -- CR 702.29e's "[type]" is a Filter, and "basic land" is why: the same
    -- two-atom filter Evolving Wilds' search carries.
    Spec.assertEqWith
      s
      "basic landcycling {1}"
      (Card.Type.keywords c)
      (Set.singleton (Keyword.Cycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []) (Just basicLand)))
    Spec.assertEqWith s "one activated ability, its own mana ability" (length (Card.Type.activatedAbilities c)) 1
  Spec.it s "Barkhide Mauler is a {4}{G} 4/4 Beast whose only text is Cycling {2}" $ do
    p <- Registry.printing registry "Barkhide Mauler"
    let c = Printing.card p
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Barkhide Mauler")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 4, green])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 4)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 4)))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.singleton Subtype.Beast)
    -- The card data carries the PRINTED cost and nothing else: rule 702.29a's
    -- discard and draw are minted by Pawl.Engine.Keyword, never authored here.
    Spec.assertEqWith
      s
      "\"Cycling {2}\""
      (Card.Type.keywords c)
      (Set.singleton (Keyword.Cycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []) Nothing))
    Spec.assertEqWith s "and no activated ability of its own" (Card.Type.activatedAbilities c) []

-- The pool's two world enchantments. Their abilities are ordinary -- a layer-6
-- keyword grant and a layer-4/7b animation, both shapes the pool already had --
-- and it is the SUPERTYPE on the type line that earns them their place: CR
-- 205.4f is what puts them under CR 704.5k's world rule (Pawl.Engine.Sba.worldVictims),
-- and nothing else in the corpus carries it.
worldCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
worldCardSpec s registry = Spec.describe s "WorldEnchantments" $ do
  Spec.it s "Concordant Crossroads is a {G} world enchantment giving all creatures haste" $ do
    p <- Registry.printing registry "Concordant Crossroads"
    let c = Printing.card p
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Concordant Crossroads")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [green])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith s "CR 205.4f: the supertype the world rule reads" (TypeLine.supertypes (Card.Type.typeLine c)) (Set.singleton Supertype.World)
    Spec.assertEqWith
      s
      "\"All creatures have haste\""
      (Card.Type.staticAbilities c)
      [ StaticAbility.MkStaticAbility
          (Affected.Matching (Filter.Type.HasCardType CardType.Creature))
          (Modification.GainKeyword Keyword.Haste NonEmpty.:| [])
      ]
  Spec.it s "Living Plane is a {2}{G}{G} world enchantment making every land a 1/1 creature" $ do
    p <- Registry.printing registry "Living Plane"
    let c = Printing.card p
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Living Plane")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2, green, green])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith s "CR 205.4f: the supertype the world rule reads" (TypeLine.supertypes (Card.Type.typeLine c)) (Set.singleton Supertype.World)
    -- ONE ability with two parts, not two abilities (#233) -- the shape
    -- every animator in the pool has. CR 613.6 costs this one nothing
    -- either way, unlike March of the Machines: its affected set reads a
    -- card type ("all lands") that its own layer-4 part does not change.
    Spec.assertEqWith
      s
      "\"All lands are 1/1 creatures that are still lands\""
      (Card.Type.staticAbilities c)
      [ StaticAbility.MkStaticAbility
          (Affected.Matching (Filter.Type.HasCardType CardType.Land))
          ( Modification.AddCardType CardType.Creature
              NonEmpty.:| [Modification.SetBasePowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1)]
          )
      ]

-- CR 701.20: the cards that say "reveal" in their own text, as opposed to
-- inheriting it from a keyword the way Ash Barrens' typecycling does.
revealCardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
revealCardSpec s registry = Spec.describe s "Reveal" $ do
  Spec.it s "Braidwood Sextant is a {1} Artifact whose {2}, {T}, Sacrifice fetches a revealed basic land" $ do
    p <- Registry.printing registry "Braidwood Sextant"
    let c = Printing.card p
        basicLand = Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic]
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Braidwood Sextant")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 1])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Artifact)
    case Card.Type.activatedAbilities c of
      [ability] -> do
        Spec.assertEqWith
          s
          "\"{2}, {T}, Sacrifice this artifact\""
          (ActivatedAbility.cost ability)
          (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [CostComponent.TapThis, CostComponent.SacrificeThis])
        -- The whole point of the card, in the destination: "reveal that
        -- card, put it into your hand" is ONE instruction (CR 701.23e), and
        -- the same filter Evolving Wilds and Ash Barrens carry.
        case Foldable.toList (Modal.modes (ActivatedAbility.modal ability)) of
          [m] ->
            Spec.assertEqWith
              s
              "\"Search your library for a basic land card, reveal that card, put it into your hand\""
              (Foldable.toList (Mode.effects m))
              [Effect.Search basicLand SearchDestination.RevealThenHand]
          modes -> Spec.assertFailure s ("expected one mode, got " <> show (length modes))
      abilities -> Spec.assertFailure s ("expected one activated ability, got " <> show (length abilities))

-- CR 603.6a's second written form. Soul Warden is the pool's first card whose
-- ability triggers on a permanent OTHER than itself entering, and its effect
-- names nothing about the newcomer, so the card is a clean witness for the
-- trigger condition alone.
entersCardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
entersCardSpec s registry = Spec.describe s "Enters" $ do
  Spec.it s "Soul Warden is a {W} 1/1 Human Cleric whose trigger reads \"whenever ANOTHER creature enters\"" $ do
    p <- Registry.printing registry "Soul Warden"
    let c = Printing.card p
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Soul Warden")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [white])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Human, Subtype.Cleric])
    case Card.Type.triggeredAbilities c of
      [ability] -> do
        -- "another" is `Not IsSource` INSIDE the condition's Filter, which
        -- is the one spelling Filter.IsSource fixes for it (#163) -- there
        -- is no exclusion flag beside the Filter to get out of step with.
        Spec.assertEqWith
          s
          "CR 603.6a: whenever another creature enters"
          (TriggeredAbility.condition ability)
          (TriggerCondition.PermanentEnters (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.IsSource]))
        Spec.assertEqWith s "no intervening \"if\" (CR 603.4)" (TriggeredAbility.intervening ability) Nothing
        case Foldable.toList (Modal.modes (TriggeredAbility.modal ability)) of
          [m] -> do
            -- CR 109.5's "you": the ability's controller, and no target
            -- slot at all -- the effect never reads the entering creature.
            Spec.assertEqWith
              s
              "\"you gain 1 life\""
              (Foldable.toList (Mode.effects m))
              [Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 1)]
            Spec.assertEqWith s "targetless" (Mode.targetSpecs m) Map.empty
          modes -> Spec.assertFailure s ("expected one mode, got " <> show (length modes))
      abilities -> Spec.assertFailure s ("expected one triggered ability, got " <> show (length abilities))

unspentManaCardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
unspentManaCardSpec s registry = Spec.describe s "Unspent mana" $ do
  -- The modern Oracle wording is "don't LOSE unspent mana", CR 106.4's verb,
  -- not "mana pools don't empty" -- and it is symmetric, which is why the
  -- scope is EachPlayer and the effect needs no mana-type argument.
  Spec.it s "Upwelling is a {3}{G} Enchantment with one EachPlayer DontLoseUnspentMana ability" $ do
    p <- Registry.printing registry "Upwelling"
    let c = Printing.card p
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Upwelling")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 3, green])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith s "no object-axis static abilities" (Card.Type.staticAbilities c) []
    Spec.assertEqWith
      s
      "one player ability"
      (Card.Type.playerAbilities c)
      [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer PlayerEffect.DontLoseUnspentMana]

phyrexianCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
phyrexianCardSpec s registry = Spec.describe s "Phyrexian" $ do
  Spec.it s "Mutagenic Growth is a {G/P} Instant giving target creature +2/+2" $ do
    p <- Registry.printing registry "Mutagenic Growth"
    let c = Printing.card p
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Mutagenic Growth")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Phyrexian Color.Green])
    Spec.assertBool s (Card.isInstant c) "an instant"
    Spec.assertEqWith
      s
      "+2/+2 until end of turn"
      (Card.allEffects c)
      [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 2)) (ObjectRef.InSlot target)]
    Spec.assertEqWith s "one creature slot" (Card.allTargetSpecs c) (Map.singleton target (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
  -- CR 202.3g: "Each Phyrexian mana symbol in a card's mana cost contributes
  -- 1 to its mana value." Not 2 (the life half is not mana at all, so CR
  -- 202.3f's "largest component" reading does not apply) and not 0.
  Spec.it s "CR 202.3g Mutagenic Growth's Phyrexian symbol makes mana value 1" $ do
    p <- Registry.printing registry "Mutagenic Growth"
    Spec.assertEqWith s "one" (Quantity.manaValueOf (Printing.card p)) 1

-- CR 506.4's "an effect specifically removes it from combat", as printed.
-- Labyrinth of Skophos is a Land with two activated abilities and no mana cost:
-- "{T}: Add {C}. / {4}, {T}: Remove target attacking or blocking creature from
-- combat." (Murders at Karlov Manor Commander; oracle text checked against
-- Scryfall.) The gameplay proof is Pawl.CombatSpec's EffectRemoval group.
removeFromCombatCardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
removeFromCombatCardSpec s registry = Spec.describe s "RemoveFromCombat" $ do
  Spec.it s "Labyrinth of Skophos is a Land with a {T} colorless mana ability and a {4}, {T} removal ability" $ do
    labyrinth <- Registry.printing registry "Labyrinth of Skophos"
    let c = Printing.card labyrinth
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Labyrinth of Skophos")
    Spec.assertEqWith s "no mana cost" (Card.Type.manaCost c) Nothing
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Land)
    Spec.assertEqWith s "not basic, so Blood Moon reaches it (CR 305.8)" (TypeLine.supertypes (Card.Type.typeLine c)) Set.empty
    Spec.assertEqWith s "no land types of its own" (TypeLine.subtypes (Card.Type.typeLine c)) Set.empty
    Spec.assertEqWith s "nothing on the spell half: a land is never cast (CR 305.1)" (Card.allEffects c) []
    case Card.Type.activatedAbilities c of
      [mana, removal] -> do
        -- CR 605.1a: the mana half. Tap only, one colorless.
        Spec.assertEqWith s "the mana ability's cost is the tap alone" (Cost.Type.components (ActivatedAbility.cost mana)) [CostComponent.TapThis]
        Spec.assertEqWith s "and it names no mana" (Cost.Type.mana (ActivatedAbility.cost mana)) (Just (ManaCost.MkManaCost []))
        Spec.assertEqWith s "adds colorless" (Modal.allEffects (ActivatedAbility.modal mana)) [Effect.AddMana (ManaProduction.OfType ManaType.Colorless)]
        -- The removal half: {4} on top of the same tap.
        Spec.assertEqWith s "the removal ability taps too" (Cost.Type.components (ActivatedAbility.cost removal)) [CostComponent.TapThis]
        Spec.assertEqWith s "and costs {4}" (Cost.Type.mana (ActivatedAbility.cost removal)) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]))
        Spec.assertEqWith s "removes the target from combat" (Modal.allEffects (ActivatedAbility.modal removal)) [Effect.RemoveFromCombat target]
        -- CR 508.1k / CR 509.1g: "attacking or blocking" is two atoms under
        -- one Or, over the creature pool.
        Spec.assertEqWith
          s
          "target attacking or blocking creature"
          (Modal.allTargetSpecs (ActivatedAbility.modal removal))
          (Map.singleton target (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Or [Filter.Type.IsAttacking, Filter.Type.IsBlocking]))))
      _ -> Spec.assertFailure s "expected exactly two activated abilities"

-- CR 509.1c's blocking requirement on a CREATURE card rather than an Aura. Prized
-- Unicorn is a {3}{G} 2/2 Creature -- Unicorn whose whole text is one line: "All
-- creatures able to block this creature do so." (Magic 2010; name, cost, type
-- line, oracle text and P/T checked against Scryfall.) Its shape is the whole
-- point next to Lure's, above: the same field, the other Affected. The gameplay
-- proof, including CR 604.2's layer-6 strip, is Pawl.CombatSpec's
-- BlockRequirements group.
blockRequirementCardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
blockRequirementCardSpec s registry = Spec.describe s "BlockRequirements" $ do
  Spec.it s "Prized Unicorn is a {3}{G} 2/2 Unicorn whose only ability is a requirement naming ITSELF" $ do
    p <- Registry.printing registry "Prized Unicorn"
    let card = Printing.card p
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Prized Unicorn")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, green]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine card)) (Set.singleton CardType.Creature)
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine card)) (Set.singleton Subtype.Unicorn)
    Spec.assertEqWith s "power" (Card.Type.power card) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness card) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
    -- "THIS CREATURE", not "enchanted creature": the requirement names its own
    -- source, which the predicate language already spells Filter.IsSource.
    -- Lure's Affected.Attached is the contrast -- same field, the other
    -- Affected -- and it is why Pawl.Engine.BlockRequirement resolves the attacker
    -- through Projection.affects rather than reading an ObjectId.
    Spec.assertEqWith
      s
      "one requirement, naming the source itself"
      (Card.Type.blockRequirements card)
      [BlockRequirement.MkBlockRequirement (Affected.Matching Filter.Type.IsSource)]
    Spec.assertEqWith s "and it modifies no characteristic" (Card.Type.staticAbilities card) []
    Spec.assertEqWith s "no spell effects" (Card.allEffects card) []

-- CR 508.1d's attacking requirement, the twin of the blocking one above. Curse of
-- the Nightly Hunt is a {2}{R} Enchantment -- Aura Curse reading "Enchant player.
-- Creatures enchanted player controls attack each combat if able." (Commander
-- Anthology 2018; name, cost, type line and oracle text checked against Scryfall.)
-- Its shape is the point next to Curse of Death's Hold's: the same enchant-player
-- Aura reaching the same set through the same Affected, carried on a field the
-- CR 613 layer system never reads. The gameplay proof is Pawl.CombatSpec's
-- AttackRequirements group.
attackRequirementCardSpec :: Spec.Spec IO n -> Registry.Registry -> n ()
attackRequirementCardSpec s registry = Spec.describe s "AttackRequirements" $ do
  Spec.it s "Curse of the Nightly Hunt is a {2}{R} Aura Curse whose only ability is a CR 508.1d attacking requirement" $ do
    p <- Registry.printing registry "Curse of the Nightly Hunt"
    let card = Printing.card p
        red = ManaSymbol.OfType (ManaType.Colored Color.Red)
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Curse of the Nightly Hunt")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine card)) (Set.singleton CardType.Enchantment)
    -- CR 205.3h: "Enchantment -- Aura Curse" is two enchantment types.
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine card)) (Set.fromList [Subtype.Aura, Subtype.Curse])
    Spec.assertBool s (Card.isAura card) "is an Aura"
    -- CR 702.5d: "Enchant player", the whole player pool.
    Spec.assertEqWith s "enchant player" (Card.Type.enchant card) (Just (TargetSpec.MkTargetSpec Pool.Players Nothing))
    -- "CREATURES ENCHANTED PLAYER CONTROLS attack each combat if able": the
    -- requirement names its SUBJECT, where Lure's names the attacker to be
    -- blocked. Same Affected as Curse of Death's Hold, different field --
    -- which is what says this changes no characteristic.
    Spec.assertEqWith
      s
      "one requirement, over the enchanted player's creatures"
      (Card.Type.attackRequirements card)
      [AttackRequirement.MkAttackRequirement (Affected.AttachedPlayerControls (Filter.Type.HasCardType CardType.Creature))]
    Spec.assertEqWith s "and it modifies no characteristic" (Card.Type.staticAbilities card) []
    Spec.assertEqWith s "and requires no block" (Card.Type.blockRequirements card) []
    -- CR 303.4: an Aura spell has no spell effects; it enters attached.
    Spec.assertEqWith s "no spell effects" (Card.allEffects card) []

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Card" $ do
  cardSpec s registry
  lintSpec s registry
  m2aCardSpec s registry
  m2bCardSpec s registry
  m2cCardSpec s registry
  basicLandSpec s registry
  m3cCardSpec s registry
  m3eCardSpec s registry
  m4bCardSpec s registry
  m45p6CardSpec s registry
  m45p7CardSpec s registry
  m45p11CardSpec s registry
  m55CardSpec s registry
  auraCardSpec s registry
  animatorCardSpec s registry
  worldCardSpec s registry
  cyclingCardSpec s registry
  revealCardSpec s registry
  entersCardSpec s registry
  unspentManaCardSpec s registry
  phyrexianCardSpec s registry
  removeFromCombatCardSpec s registry
  blockRequirementCardSpec s registry
  attackRequirementCardSpec s registry
