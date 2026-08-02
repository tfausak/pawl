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
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.Subtype as Subtype
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
-- The json sublibrary's own modules, for the CR 701.3a completeness cross-check
-- alone: it counts the atom in a card's ENCODED form, which is a traversal of the
-- whole card written by somebody else and so an independent witness to the
-- hand-maintained one below.
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterPattern as CounterPattern
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
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Scaling as Scaling
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

m2aCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m2aCardSpec s registry = Spec.describe s "M2aCards" $ do
  let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      green = ManaSymbol.OfType (ManaType.Colored Color.Green)
      black = ManaSymbol.OfType (ManaType.Colored Color.Black)
  Spec.it s "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
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
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    let c = Printing.card nimbleBirdsticker
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Nimble Birdsticker")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 3)))
  Spec.it s "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    let c = Printing.card ogreSentry
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Ogre Sentry")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 1, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 3)))
  Spec.it s "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    let c = Printing.card windseekerCentaur
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Windseeker Centaur")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 1, red, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
  Spec.it s "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
    goblinChariot <- S.printingOf s registry "Goblin Chariot"
    let c = Printing.card goblinChariot
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Goblin Chariot")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 2, red])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
  Spec.it s "Glistener Elf is a {G} 1/1 Phyrexian Elf Warrior with infect" $ do
    glistenerElf <- S.printingOf s registry "Glistener Elf"
    let c = Printing.card glistenerElf
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Glistener Elf")
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertBool s (elem Keyword.Infect (Card.Type.keywords c)) "has infect"
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Phyrexian, Subtype.Elf, Subtype.Warrior])
  Spec.it s "Branchblight Stalker is a {1}{G} 3/1 Phyrexian Elf Scout with toxic 2" $ do
    stalker <- S.printingOf s registry "Branchblight Stalker"
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
    initiation <- S.printingOf s registry "Snake Cult Initiation"
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
          p <- S.printingOf s registry name
          let c = Printing.card p
          Spec.assertBool s (Card.isCreature c) "creature"
          Spec.assertBool s (not (Card.isLand c)) "not land"
          Spec.assertEqWith s "exactly this keyword" (Card.Type.keywords c) (Set.singleton keyword)
      )
      S.m2aKeywords

cardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cardSpec s registry = Spec.describe s "Card" $ do
  Spec.it s "Mountain printing is named Mountain" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "name" (Card.Type.name (Printing.card mountain)) (Text.pack "Mountain")
  Spec.it s "Mountain is a Land" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (Card.isLand (Printing.card mountain)) "isLand"
  Spec.it s "Mountain has the Mountain subtype" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (Set.member Subtype.Mountain (TypeLine.subtypes (Card.Type.typeLine (Printing.card mountain)))) "subtype"
  Spec.it s "Mountain type line contains Land" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card mountain)))) "cardtype"
  -- CR 202.1: a land has no mana cost. Not a zero cost -- no cost at all.
  Spec.it s "Mountain has no mana cost" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "no cost" (Card.Type.manaCost (Printing.card mountain)) Nothing
  Spec.it s "Mountain has no power or toughness" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "power" (Card.Type.power (Printing.card mountain)) Nothing
    Spec.assertEqWith s "toughness" (Card.Type.toughness (Printing.card mountain)) Nothing
  Spec.it s "Piker printing is named Goblin Piker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith s "name" (Card.Type.name (Printing.card piker)) (Text.pack "Goblin Piker")
  Spec.it s "Piker costs {1}{R}" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith
      s
      "cost"
      (Card.Type.manaCost (Printing.card piker))
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
  Spec.it s "Piker is a 2/1" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith s "power" (Card.Type.power (Printing.card piker)) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness (Printing.card piker)) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
  Spec.it s "Piker is a Goblin Warrior" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith
      s
      "subtypes"
      (TypeLine.subtypes (Card.Type.typeLine (Printing.card piker)))
      (Set.fromList [Subtype.Goblin, Subtype.Warrior])
  Spec.it s "Piker is a creature and not a land" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertBool s (Card.isCreature (Printing.card piker)) "creature"
    Spec.assertBool s (not (Card.isLand (Printing.card piker))) "not land"
  -- CR 110.1: the classification resolution turns on. Never card identity.
  Spec.it s "CR 110.1 both a Piker and a Mountain are permanents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
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
              Card.Type.combatRestrictions = [],
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
    piker <- S.printingOf s registry "Goblin Piker"
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
  Duration.UntilEndOfCombat -> []

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
  Modification.SetCreatureSubtype _ -> []
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
  TriggerCondition.SelfLeavesTheBattlefield -> []
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
  Effect.MoveToZone {} -> []
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
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers duration _ _ -> durationCounts duration
  Effect.CreateEmblem card -> cardCounts card
  Effect.BecomeMonarch _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.PlaySubgame _ -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary _ -> []

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
-- CR 107.3: does this mana cost print an {X}? Asked of a CARD's mana cost for a
-- spell and of an ACTIVATION cost's mana part for an ability -- the two costs CR
-- 602.2b calls each other's analog -- so the two halves of the "reads X iff {X}
-- is declared" lint ask it in the same words. Nothing (CR 118.6, an unpayable
-- cost) declares nothing.
declaresVariable :: Maybe ManaCost.ManaCost -> Bool
declaresVariable m = case m of
  Nothing -> False
  Just (ManaCost.MkManaCost syms) -> elem ManaSymbol.Variable syms

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

-- The shared shape of all three ability read lints: every slot an effect of ONE
-- MODE reads must be bound for that mode, given `abilityBound` -- the slots the
-- ABILITY binds whichever mode is chosen, which is the only part that differs
-- between the three.
--
-- PER MODE, never through Modal.allEffects and Modal.allTargetSpecs. Those are
-- unions across every mode, and comparing one union against the other lets a
-- mode read a slot only ANOTHER mode declares -- unbound at runtime, because
-- Pawl.Engine.Activate.activateAbility and Pawl.Engine.Engine's trigger placement
-- both stamp Modal.modesTargetSpecs, the CHOSEN modes' specs alone. CR 700.2c is
-- the rule: "If a spell or ability targets one or more targets only if a
-- particular mode is chosen for it, its controller will need to choose those
-- targets only if they chose that mode" (#570).
--
-- Resolve.definedSlots is per mode for the mirror reason: under a ChooseExactly
-- 1 selection mode B is never resolved alongside mode A, so a token mode A mints
-- is not there for mode B to read. This is the shape the spell lint's modeOffends
-- has had all along; these three now match it.
modalReadOffends :: Set.Set SlotName.SlotName -> Modal.Modal Card.Type.Card -> Bool
modalReadOffends abilityBound modal =
  let modeOffends mode =
        let effects = Foldable.toList (Mode.effects mode)
            available =
              Set.unions
                [ abilityBound,
                  Resolve.definedSlots effects,
                  Map.keysSet (Mode.targetSpecs mode)
                ]
            wanted = Set.unions (fmap Resolve.slotsOf effects)
         in not (Set.isSubsetOf wanted available)
   in any modeOffends (Modal.modes modal)

-- Every ReplacementEffect a card AUTHORS: the ones it PRINTS
-- (Card.replacementEffects, Eon Hub's) and the ones an effect of its own
-- installs (Effect.Replace, a floating one) -- and, through effectReplacements
-- below, everything a token or emblem those effects mint prints in turn. All of
-- them come out of card JSON, which is the whole of what the lint below is
-- about; a replacement the ENGINE bakes reaches GameState without passing
-- through a Card and is not swept here.
cardReplacementEffects :: Card.Type.Card -> [ReplacementEffect.ReplacementEffect]
cardReplacementEffects card =
  Card.Type.replacementEffects card
    <> concatMap effectReplacements (cardResolutionEffects card)

-- Every ReplacementEffect one effect authors: the one an Effect.Replace installs
-- directly, plus everything a minted token (CR 111) or emblem (CR 114.2) prints,
-- since each of those is a whole Card that can carry replacementEffects of its
-- own. The same recursion cardCounts and cardFilters take, and for the same
-- reason -- without it a baked whosePhase could hide one Card deep.
--
-- Exhaustive and hand-maintained, with effectCounts' caveat: a NEW effect
-- carrying a ReplacementEffect or embedding a Card must be added here too, and
-- the build breaks until it is.
effectReplacements :: Effect.Effect Card.Type.Card -> [ReplacementEffect.ReplacementEffect]
effectReplacements effect = case effect of
  Effect.Replace _ _ replacement -> [replacement]
  Effect.Create _ token _ _ -> cardReplacementEffects token
  Effect.CreateEmblem emblem -> cardReplacementEffects emblem
  Effect.DealDamage _ _ -> []
  Effect.ModifyTarget {} -> []
  Effect.AddMana _ -> []
  Effect.Search _ _ -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices {} -> []
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw _ _ -> []
  Effect.Mill _ _ -> []
  Effect.Discard _ _ -> []
  Effect.LoseLife _ _ -> []
  Effect.GainLife _ _ -> []
  Effect.SkipNextPhase _ _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.Counter _ -> []
  Effect.PutCounters {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.AddPhases _ -> []
  Effect.GainControl _ _ -> []
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.BecomeMonarch _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.PlaySubgame _ -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary _ -> []
  Effect.ChangeText _ -> []

-- #437: does this replacement carry a PhasePattern with a BAKED player in it?
--
-- PhasePattern.whosePhase is meant to be runtime-only. Nothing is the value card
-- data writes -- Eon Hub's "players skip their upkeep steps" is symmetric and
-- names nobody -- and Just is baked by the engine out of a player a resolution
-- named (Resolve.applyEffect's SkipNextPhase arm, Fatigue's target) or out of a
-- pending extra turn (Replacement.installTurnSkips). Card data cannot name a
-- player at all.
--
-- Nothing enforced that split. Codec.PhasePattern is structural over the record,
-- so it accepts a Just from card JSON -- and a card file could write
-- `"whosePhase": 1`, which is meaningless. Player 1 is a seat in some game, not
-- a fact about a printed card, and the skip would land on whoever happened to
-- hold that id.
--
-- A LINT rather than a type-level split, which is the call #199 already records
-- for the sibling case (Modification.SetController's baked PlayerId, likewise
-- accepted by its codec and likewise kept out of card data by a lint here).
--
-- What makes the split expensive is NOT the codec -- nothing needs a baked
-- pattern to round-trip, since ActiveReplacement and GameState have no codec at
-- all (#126). It is that ReplacementEffect is the carrier for both halves:
-- Card.replacementEffects, which a card authors, and ActiveReplacement.effect,
-- which the engine bakes. A card-side / runtime-side split the way Duration and
-- Expiry are split would therefore have to split or parameterize that whole sum,
-- not just PhasePattern. Modification is shared exactly the same way, between
-- StaticAbility.modifications and a stored ContinuousEffect, which is why the
-- two cases want ONE answer rather than two -- what the issue asks be decided
-- once.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: a second
-- pattern-carrying replacement must break this build rather than silently pass.
phasePatternOffends :: ReplacementEffect.ReplacementEffect -> Bool
phasePatternOffends replacement = case replacement of
  ReplacementEffect.PhaseR phasePattern -> Maybe.isJust (PhasePattern.whosePhase phasePattern)
  ReplacementEffect.CounterR _ _ -> False
  ReplacementEffect.ZoneChangeR _ _ -> False
  ReplacementEffect.EntryR _ -> False
  ReplacementEffect.DamageR _ _ -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.TokenR _ _ -> False

-- The non-vacuity half of the same lint: is this the replacement that carries a
-- PhasePattern at all? A wildcard is right here, where it is not above -- this
-- asks "did the sweep have anything to look at", not "is it well-formed".
isPhaseR :: ReplacementEffect.ReplacementEffect -> Bool
isPhaseR replacement = case replacement of
  ReplacementEffect.PhaseR _ -> True
  _ -> False

-- Do these slot-name sets overlap? True when any name appears in more than one
-- of them, which is exactly what a Map.unions over them would silently collapse.
slotNamesCollide :: [Set.Set SlotName.SlotName] -> Bool
slotNamesCollide sets = Set.size (Set.unions sets) /= sum (fmap Set.size sets)

-- CR 700.2c: do two modes of one modal declare the same slot NAME -- or does a
-- spell mode collide with CR 303.4a's enchant slot?
--
-- Modal.modesTargetSpecs, Modal.allTargetSpecs and Card.allTargetSpecs are all
-- Map.unions, so a shared name collapses to ONE entry: the controller is
-- prompted once and every mode holding that name reads the one answer. Harmless
-- while exactly one mode can ever be chosen, and a silent wrong answer the
-- moment two can -- CR 702.42a's entwine, "you may choose all modes of this
-- spell instead of just the number specified", is what makes that real (#399).
-- Dream's Grip authored with one shared slot would tap and untap the same
-- permanent, and could not express "tap one, untap another" at all.
--
-- A lint about NAMES, not about CR 601.2c. Two modes naming the same OBJECT as a
-- target is legal and stays legal: the player may aim distinct slots at one
-- permanent, which is what the CR 702.42b resolution-order test in CastSpec
-- does.
--
-- The enchant slot joins the SPELL's modes and no ability's, because the unions
-- that add it -- Card.modesTargetSpecs and Card.allTargetSpecs -- are both over
-- the spell. CR 303.4a's slot is announced when the Aura spell is cast, and
-- Activate stamps Modal.modesTargetSpecs, which has no enchant half. Each ability is checked on
-- its own for the same reason: two abilities are two separate announcements, so
-- a name they share is never fused.
cardSlotNamesCollide :: Card.Type.Card -> Bool
cardSlotNamesCollide card =
  let modeSlots modal = fmap (Map.keysSet . Mode.targetSpecs) (Foldable.toList (Modal.modes modal))
   in slotNamesCollide (Map.keysSet (Card.enchantSpecs card) : modeSlots (Card.Type.spell card))
        || any (slotNamesCollide . modeSlots . ActivatedAbility.modal) (Card.Type.activatedAbilities card)
        || any (slotNamesCollide . modeSlots . TriggeredAbility.modal) (Card.Type.triggeredAbilities card)
        || any (slotNamesCollide . modeSlots . TriggeredAbility.modal) (Map.elems (Card.Type.delayedAbilities card))

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
--     ordinary chosen targets -- contributed by modalReadOffends, one MODE's at
--     a time, so a mode reading a slot only another mode declares is caught.
--
-- The first two are what this passes to modalReadOffends as `abilityBound`: they
-- are stamped for the ability, not for a mode, so every mode gets them.
triggeredAbilityOffends :: TriggeredAbility.TriggeredAbility Card.Type.Card -> Bool
triggeredAbilityOffends ability =
  modalReadOffends
    ( Set.unions
        [ Set.fromList [Binding.triggerSource, Binding.you],
          Event.eventBindingSlots (TriggeredAbility.condition ability)
        ]
    )
    (TriggeredAbility.modal ability)

-- The ACTIVATED-ability half of the same lint: every slot one of an
-- activated ability's effects READS must be a slot the ACTIVATION binds. Without
-- it, an ability naming CR 109.5's `you` loads, activates, misses the lookup and
-- silently no-ops, exactly as an unbound `became` does above.
--
-- A SUBSET check, never an equality, for the reason
-- Pawl.Engine.Binding.triggerSource's comment gives and the two lints around it
-- take.
--
-- The available side is what Pawl.Engine.Activate.activateAbility stamps on the
-- ability object as it goes on the stack, and nothing else:
--
--   * Binding.triggerSource. CR 113.7: "The source of an activated ability on
--     the stack is the object whose ability was activated" -- stamped for every
--     activation, so Longtusk Cub's "put a +1/+1 counter on Longtusk Cub" is a
--     slot read.
--   * the ability's own declared target specs, one MODE's at a time
--     (modalReadOffends). CR 602.2b: "The remainder of the process for
--     activating an ability is identical to the process for casting a spell
--     listed in rules 601.2b-i", which is what routes an activation through CR
--     601.2c's target announcement -- and CR 700.2c scopes it to the chosen
--     mode.
--   * Binding.variableX, and ONLY when the ability's own cost prints an {X}:
--     CR 601.2b's "the player announces the value of that variable", measured
--     against what CR 602.2b calls "an activated ability's analog to a spell's
--     mana cost ... its activation cost" (Cinder Elemental). Nothing reads it as
--     a slot today -- a printed X is Quantity.X, whose own half of the contract
--     is the CR 602.2b sweep below -- but the activation really does bind it, so
--     leaving it out would reject a read that works (#14 is what would make one
--     sayable).
--   * Resolve.definedSlots, the slot an effect of this ability MINTS rather than
--     reads. The same exemption all three sibling lints take.
--
-- What is NOT on it is the point:
--
--   * CR 109.5's `you`, which for an activated ability the rule does define
--     ("For an activated ability, this is the player who activated the
--     ability") -- but Binding.setYou is called only when a TRIGGERED ability is
--     placed (Pawl.Engine.Engine, Pawl.Engine.Monarch), so an activated ability
--     reading the slot reads nothing (#569).
--   * both event slots (CR 400.7e's `became`, CR 702.70a's `thatPlayer`): an
--     activation is not an event, so Pawl.Engine.Event.eventBindings never runs
--     for one.
--   * Binding.chosenModes (CR 700.2), which IS stamped and is still not an
--     exemption: its binding carries a mode set and nothing else, so no effect
--     read can be answered from it -- Resolve reads a slot as a recipient
--     (Binding.targetsOf) or as an amount (Binding.amountOf), and both are
--     Nothing there. Admitting it would exempt a read that silently no-ops,
--     which is the failure this lint exists to catch.
--
-- SCOPE: the abilities that reach Activate. CR 605.3b's mana abilities do not --
-- one "doesn't go on the stack, so it can't be targeted, countered, or otherwise
-- responded to. Rather, it resolves immediately after it is activated" -- and
-- pawl's mana path lifts a route's AddMana effects out rather than activating
-- anything (#238), so NOTHING is bound for one and none of its other effects
-- runs either. No mana ability in the pool reads a slot, so applying the same
-- available side to one is uniformity rather than a claim.
activatedAbilityOffends :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Bool
activatedAbilityOffends ability =
  let announcedX =
        if declaresVariable (Cost.Type.mana (ActivatedAbility.cost ability))
          then Set.singleton Binding.variableX
          else Set.empty
   in modalReadOffends (Set.insert Binding.triggerSource announcedX) (ActivatedAbility.modal ability)

-- CR 603.7 / 109.5: does this card arm a delayed ability "on your next turn"
-- whose condition is not scoped to its controller's turn?
--
-- Pawl.Types.Onset.FromYourNextTurn enforces only the NEXT half of that phrase:
-- Resolve turns it into a turn NUMBER (DelayedTrigger.notBefore) and
-- Event.delayedPending compares the live turn number against it, so the entry
-- cannot fire on the turn that created it. A number cannot say WHOSE turn it is.
-- The YOUR half is delivered by the delayed ability's own
-- TriggerCondition.StepBegins carrying TurnScope.ControllersTurn.
--
-- The two collaborate and neither is redundant -- the scope alone admits the
-- arming turn itself (an extra combat phase would fire the ability early), and
-- the onset alone admits an intervening opponent's turn -- so a card that arms
-- with the onset but scopes with EachTurn has a delayed ability whose printed
-- "your" is a lie. That is what this rejects.
--
-- A dangling name (an onset naming an ability the card does not declare) is
-- ALSO an offence here, and deliberately not silently accepted: the neighbouring
-- "every armed delayed ability is declared" lint is what reports it precisely,
-- and answering False for it here would let a card that offends both pass this
-- one.
onsetOffends :: Card.Type.Card -> Bool
onsetOffends card =
  let scoped name = case Map.lookup name (Card.Type.delayedAbilities card) of
        Nothing -> False
        Just ability -> Event.controllerTurnScoped (TriggeredAbility.condition ability)
   in not (all scoped (Set.toList (Resolve.onsetGatedAbilities (cardResolutionEffects card))))

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

-- oneEffectTrigger's ACTIVATED twin: a one-mode, targetless ability running one
-- effect, and the fixture the read lint's self-test misauthors on purpose. Kept
-- out of data/cards for that lint's reason -- a card that offends a lint must not
-- be loadable.
--
-- The mana cost is a parameter because it is part of the available side: CR
-- 601.2b's announced X is bound only when the ACTIVATION cost prints an {X} (CR
-- 602.2b). No cost components and no timing rider, neither of which the lint
-- reads.
oneEffectActivated ::
  Maybe ManaCost.ManaCost ->
  Effect.Effect Card.Type.Card ->
  ActivatedAbility.ActivatedAbility Card.Type.Card
oneEffectActivated mana effect =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = mana, Cost.Type.components = []},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty Optionality.Mandatory))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.timing = ActivationTiming.AnyTime
    }

-- One CR 700.2 mode for the fixtures below: the effects it runs and the target
-- slots it declares. Always mandatory -- no read lint asks about optionality.
lintMode :: [Effect.Effect Card.Type.Card] -> [SlotName.SlotName] -> Mode.Mode Card.Type.Card
lintMode effects slots =
  Mode.MkMode
    (Seq.fromList effects)
    (Map.fromList (fmap (\slot -> (slot, TargetSpec.MkTargetSpec Pool.AnyTarget Nothing)) slots))
    Optionality.Mandatory

-- oneEffectActivated widened to SEVERAL modes, free, under CR 700.2's
-- ChooseExactly 1. The fixture the per-mode read lint needs and the one-mode
-- helpers cannot express: only a multi-mode ability can have a mode read a slot
-- that only another mode declares (#570). Kept out of data/cards for the same
-- reason they are -- a card that offends a lint must not be loadable.
modalActivated :: [Mode.Mode Card.Type.Card] -> ActivatedAbility.ActivatedAbility Card.Type.Card
modalActivated modes =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
      ActivatedAbility.modal = Modal.MkModal (Seq.fromList modes) (ModeSelection.ChooseExactly 1),
      ActivatedAbility.timing = ActivationTiming.AnyTime
    }

-- modalActivated's TRIGGERED twin, so the per-mode lint can be shown to hand
-- `abilityBound` -- the condition's event slots and CR 109.5's `you` -- to EVERY
-- mode rather than only the first.
modalTrigger ::
  TriggerCondition.TriggerCondition ->
  [Mode.Mode Card.Type.Card] ->
  TriggeredAbility.TriggeredAbility Card.Type.Card
modalTrigger condition modes =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal = Modal.MkModal (Seq.fromList modes) (ModeSelection.ChooseExactly 1),
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
  case traverse (fmap (Text.pack . fst) . Common.asTagged . Subtype.toJson) (Set.toList (TypeLine.subtypes (Card.Type.typeLine token))) of
    Left _ -> True
    Right subtypes ->
      notElem
        (Card.Type.name token)
        (fmap (\ordering -> Text.unwords (ordering <> [Text.pack "Token"])) (List.permutations subtypes))

-- CR 701.3a's Filter.CanHostSubject, counted wherever it appears inside ONE
-- Filter: under And/Or/Not, and inside the typecycling predicate a HasKeyword
-- atom's own keyword may carry (CR 702.29e).
--
-- Counted rather than merely detected, because the completeness cross-check below
-- compares this hand-maintained traversal against the codec's independent one,
-- and that comparison needs a number.
--
-- Written out exhaustively rather than with a catch-all, so a later atom that can
-- hold a Filter fails to compile here instead of silently hiding one.
canHostSubjects :: Filter.Type.Filter Keyword.Keyword -> Int
canHostSubjects predicate = case predicate of
  Filter.Type.CanHostSubject -> 1
  Filter.Type.And fs -> sum (fmap canHostSubjects fs)
  Filter.Type.Or fs -> sum (fmap canHostSubjects fs)
  Filter.Type.Not f -> canHostSubjects f
  -- CR 702.29e's "[type]cycling" carries a Filter of its own, and any Cost a
  -- keyword names can carry one through a Sacrifice component. Never EVALUATED
  -- against a candidate -- HasKeyword asks whether the key is present in the
  -- projection's keyword map, so what is inside the keyword is compared and not
  -- run -- but still a Filter position a card author can write the atom into,
  -- which is the only thing this lint is about.
  Filter.Type.HasKeyword keyword -> sum (fmap canHostSubjects (keywordFilters keyword))
  Filter.Type.HasCardType _ -> 0
  Filter.Type.HasSupertype _ -> 0
  Filter.Type.HasColor _ -> 0
  Filter.Type.HasSubtype _ -> 0
  Filter.Type.PowerAtLeast _ -> 0
  Filter.Type.ControlledBy _ -> 0
  Filter.Type.IsSource -> 0
  Filter.Type.IsPlayer _ -> 0
  Filter.Type.IsAttacking -> 0
  Filter.Type.IsBlocking -> 0
  Filter.Type.AttackedThisTurn -> 0
  Filter.Type.IsAttachedToCreature -> 0
  Filter.Type.IsAttachedToPermanent -> 0
  Filter.Type.IsToken -> 0

-- Every Filter a keyword carries: CR 702.29e's typecycling predicate, CR
-- 702.14c's landwalk criterion, plus the components of any Cost a keyword names
-- (CR 702.29a cycling, 702.34a flashback, 702.42a entwine), since
-- CostComponent.Sacrifice carries one.
--
-- The exhaustiveness guard that protects the Filter traversal is on the FILTER
-- case, not on this one, so a keyword that grows a Filter payload compiles here
-- silently and drops it -- which is exactly what happened when landwalk's
-- Subtype became a Filter (#499). A new Filter-bearing keyword needs its arm
-- added here by hand.
keywordFilters :: Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
keywordFilters keyword = case keyword of
  Keyword.Cycling cost mFilter -> costFilters cost <> Maybe.maybeToList mFilter
  Keyword.Flashback cost -> costFilters cost
  Keyword.Entwine cost -> costFilters cost
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  -- CR 702.8a: flash is a static ability with no payload -- it changes WHEN the
  -- card may be cast, and names nothing to filter.
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Hexproof -> []
  Keyword.Indestructible -> []
  -- CR 702.14c's criterion, which is a Filter since #499.
  Keyword.Landwalk criterion -> [criterion]
  -- CR 702.15a: lifelink is a static ability with no payload -- its rider rides
  -- the damage event, not the keyword.
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Poisonous _ -> []
  Keyword.Infect -> []
  Keyword.Menace -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 118.1: a cost's Filters are its components'; the mana part holds none.
costFilters :: Cost.Type.Cost Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
costFilters = concatMap costComponentFilters . Cost.Type.components

costComponentFilters :: CostComponent.CostComponent Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
costComponentFilters component = case component of
  -- CR 601.2f's "sacrificing permanents": Village Rites' "a creature".
  CostComponent.Sacrifice _ f -> [f]
  CostComponent.TapThis -> []
  CostComponent.UntapThis -> []
  CostComponent.SacrificeThis -> []
  CostComponent.PayLife _ -> []
  CostComponent.DiscardCards _ -> []
  CostComponent.DiscardThis -> []
  CostComponent.PayEnergy _ -> []
  CostComponent.AddLoyaltyToThis _ -> []
  CostComponent.RemoveLoyaltyFromThis _ -> []

-- The Filter narrowing a target slot's CR 115 pool -- "target creature with
-- flying" -- and CR 303.4a's enchant slot, which is a TargetSpec too.
targetSpecFilters :: TargetSpec.TargetSpec -> [Filter.Type.Filter Keyword.Keyword]
targetSpecFilters (TargetSpec.MkTargetSpec _ mFilter) = Maybe.maybeToList mFilter

-- A continuous effect's affected set (Pawl.Types.Affected), wherever one is
-- written -- a static ability, a combat restriction, an attack or block
-- requirement. Only the two predicate arms carry a Filter; the fixed id set (CR
-- 611.2c) and CR 303.4m's "enchanted permanent" carry none.
affectedFilters :: Affected.Affected -> [Filter.Type.Filter Keyword.Keyword]
affectedFilters affected = case affected of
  Affected.TheseObjects _ -> []
  Affected.Matching f -> [f]
  Affected.Attached -> []
  Affected.AttachedPlayerControls f -> [f]

objectRefFilters :: ObjectRef.ObjectRef -> [Filter.Type.Filter Keyword.Keyword]
objectRefFilters ref = case ref of
  ObjectRef.InSlot _ -> []
  -- Day of Judgment's "all creatures", Boil's "all Islands".
  ObjectRef.EachMatching f -> [f]

-- The Filter a Count folds over (CR 608.2h). Delegated to the *Counts family
-- above rather than re-walked: those traversals are already the project's answer
-- to "every Count a card can author", and a Count's Filter is the only Filter it
-- holds. That reuse is also the one seam here that -Werror does not police -- a
-- Count added to a NEW carrier has to be added there, not here.
countFilters :: [Count.Type.Count Quantity.Type.Quantity] -> [Filter.Type.Filter Keyword.Keyword]
countFilters counts = [f | Count.Type.MkCount _ f _ <- counts]

quantityFilters :: Quantity.Type.Quantity -> [Filter.Type.Filter Keyword.Keyword]
quantityFilters = countFilters . quantityCounts

conditionFilters :: Condition.Type.Condition -> [Filter.Type.Filter Keyword.Keyword]
conditionFilters = countFilters . conditionCounts

durationFilters :: Duration.Duration -> [Filter.Type.Filter Keyword.Keyword]
durationFilters = countFilters . durationCounts

-- A Modification reaches a Filter two ways: through its layer-7 quantities (a
-- Count) and through the keyword a layer-6 grant hands out (CR 702.29e again).
modificationFilters :: Modification.Modification -> [Filter.Type.Filter Keyword.Keyword]
modificationFilters modification = case modification of
  Modification.GainKeyword keyword -> keywordFilters keyword
  Modification.SetBasePowerToughness p t -> quantityFilters p <> quantityFilters t
  Modification.ModifyPowerToughness p t -> quantityFilters p <> quantityFilters t
  Modification.LoseAllAbilities -> []
  Modification.SetLandSubtype _ -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCardType _ -> []
  Modification.ChangeSubtypeWord _ _ -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.SwitchPowerToughness -> []

staticAbilityFilters :: StaticAbility.StaticAbility -> [Filter.Type.Filter Keyword.Keyword]
staticAbilityFilters ability =
  affectedFilters (StaticAbility.affected ability)
    <> concatMap modificationFilters (StaticAbility.modifications ability)

-- CR 603.6a's "whenever [a permanent] enters" carries one directly; CR 603.8's
-- state trigger carries one through its Condition's Counts.
triggerConditionFilters :: TriggerCondition.TriggerCondition -> [Filter.Type.Filter Keyword.Keyword]
triggerConditionFilters triggerCondition = case triggerCondition of
  TriggerCondition.PermanentEnters f -> [f]
  TriggerCondition.StateIs condition -> conditionFilters condition
  TriggerCondition.SelfEnters -> []
  TriggerCondition.StepBegins _ _ -> []
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []
  TriggerCondition.SelfAttacks _ -> []
  TriggerCondition.SelfCycled -> []
  TriggerCondition.PlayerDiscards _ -> []
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> []
  TriggerCondition.SelfDies -> []
  TriggerCondition.SelfLeavesTheBattlefield -> []
  TriggerCondition.SpellOrAbilityCounters _ -> []

-- CR 613.11: which spells a cost-modifying player effect applies to.
playerEffectFilters :: PlayerEffect.PlayerEffect -> [Filter.Type.Filter Keyword.Keyword]
playerEffectFilters playerEffect = case playerEffect of
  PlayerEffect.IncreaseSpellCost f _ -> [f]
  PlayerEffect.ReduceSpellCost f _ -> [f]
  PlayerEffect.CantCastSpells -> []
  PlayerEffect.CantCastMoreThan _ -> []
  PlayerEffect.NoMaximumHandSize -> []
  PlayerEffect.DontLoseUnspentMana -> []
  -- CR 702.18a / 702.11c carry a PlayerScope, not a Filter: the set they name is
  -- players, and this traversal is about the spells a cost modifier matches.
  PlayerEffect.CantBeTargetedBy _ -> []

-- CR 614.1: only the counter-placement pattern narrows by a Filter
-- (CounterPattern.onWhat -- "one or more counters would be put on a creature
-- YOU control"). The other six patterns narrow by zone, damage, destruction,
-- entry, token or phase, none of which holds one.
replacementEffectFilters :: ReplacementEffect.ReplacementEffect -> [Filter.Type.Filter Keyword.Keyword]
replacementEffectFilters replacementEffect = case replacementEffect of
  ReplacementEffect.CounterR counterPattern _ -> [CounterPattern.onWhat counterPattern]
  ReplacementEffect.ZoneChangeR _ _ -> []
  ReplacementEffect.EntryR _ -> []
  ReplacementEffect.DamageR _ _ -> []
  ReplacementEffect.DestructionR _ -> []
  ReplacementEffect.TokenR _ _ -> []
  ReplacementEffect.PhaseR _ -> []

combatRestrictionFilters :: CombatRestriction.CombatRestriction -> [Filter.Type.Filter Keyword.Keyword]
combatRestrictionFilters restriction = case restriction of
  CombatRestriction.CantAttack affected -> affectedFilters affected
  CombatRestriction.CantBlock affected -> affectedFilters affected

-- Tag a Filter position as UNFRAMED -- one no attach supplies a subject for,
-- which is every position in the type except the one below.
unframed :: [Filter.Type.Filter Keyword.Keyword] -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
unframed = fmap ((,) False)

-- Every Filter one effect carries, paired with whether an ATTACH frames it.
-- Exactly one arm answers True: Effect.AttachTarget's destination, which is the
-- only Filter position Pawl.Engine.Resolve evaluates against a view whose
-- `canHostSubject` is filled in (the AttachTarget arm of applyEffectWith, from
-- attachmentFor). Everywhere else the field is False by construction --
-- Projection.viewOfCard, Projection.viewOfCharacteristics, Filter.playerView and
-- Count's event snapshot all set it so -- because outside an attach there is no
-- subject for CR 701.3a to be about. Widening the subject so that another
-- position could answer is #572; until a card asks for it, the framed side of
-- this traversal is exactly this one arm.
effectFilters :: Effect.Effect Card.Type.Card -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
effectFilters effect = case effect of
  -- THE one framed position. CR 701.3a: "An Aura, Equipment, or Fortification
  -- can't be attached to an object or player it couldn't enchant, equip, or
  -- fortify, respectively." Aura Graft's "another permanent it can enchant".
  Effect.AttachTarget _ f -> [(True, f)]
  Effect.DealDamage ref quantity -> unframed (objectRefFilters ref <> quantityFilters quantity)
  Effect.ModifyTarget duration modification ref ->
    unframed (durationFilters duration <> modificationFilters modification <> objectRefFilters ref)
  Effect.ChangeText _ -> []
  Effect.AddMana _ -> []
  Effect.Search f _ -> unframed [f]
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices _ f quantity -> unframed (f : quantityFilters quantity)
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy ref _ _ -> unframed (objectRefFilters ref)
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw _ quantity -> unframed (quantityFilters quantity)
  Effect.Mill _ quantity -> unframed (quantityFilters quantity)
  Effect.Discard _ quantity -> unframed (quantityFilters quantity)
  Effect.LoseLife _ quantity -> unframed (quantityFilters quantity)
  Effect.GainLife _ quantity -> unframed (quantityFilters quantity)
  -- CR 111.1's token is a whole card, and every Filter position it has is one a
  -- card author can write -- the same nesting Pawl.Codec's round trip walks.
  Effect.Create quantity card _ _ -> unframed (quantityFilters quantity) <> cardFilters card
  Effect.Replace duration _ replacement -> unframed (durationFilters duration <> replacementEffectFilters replacement)
  Effect.SkipNextPhase _ _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.Counter _ -> []
  Effect.PutCounters _ quantity _ -> unframed (quantityFilters quantity)
  Effect.GainPlayerCounters _ _ quantity -> unframed (quantityFilters quantity)
  Effect.Tap ref -> unframed (objectRefFilters ref)
  Effect.Untap ref -> unframed (objectRefFilters ref)
  Effect.AddPhases _ -> []
  Effect.GainControl duration ref -> unframed (durationFilters duration <> objectRefFilters ref)
  Effect.ArmDelayedTrigger _ _ mDuration -> unframed (concatMap durationFilters (Maybe.maybeToList mDuration))
  Effect.AffectPlayers duration _ playerEffect -> unframed (durationFilters duration <> playerEffectFilters playerEffect)
  -- CR 114.2's emblem is a whole card too.
  Effect.CreateEmblem card -> cardFilters card
  Effect.BecomeMonarch _ -> []
  Effect.ExileUntilMonarch _ -> []
  -- CR 701.3's other attach, which moves the SOURCE rather than a target and
  -- carries no destination filter at all.
  Effect.Attach _ -> []
  Effect.PlaySubgame _ -> []
  Effect.TakeExtraTurn _ _ -> []
  Effect.ShuffleIntoLibrary _ -> []

-- Per MODE rather than through Modal.allTargetSpecs, which is a Map.unions and so
-- collapses two modes declaring the same slot name (#475) -- the cross-check
-- below counts occurrences, and a collapse there would read as a Filter this
-- traversal cannot see.
modalFilters :: Modal.Modal Card.Type.Card -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
modalFilters modal =
  concatMap
    ( \mode ->
        concatMap effectFilters (Mode.effects mode)
          <> unframed (concatMap targetSpecFilters (Map.elems (Mode.targetSpecs mode)))
    )
    (Modal.modes modal)

triggeredAbilityFilters :: TriggeredAbility.TriggeredAbility Card.Type.Card -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
triggeredAbilityFilters ability =
  unframed
    ( triggerConditionFilters (TriggeredAbility.condition ability)
        <> concatMap conditionFilters (Maybe.maybeToList (TriggeredAbility.intervening ability))
    )
    <> modalFilters (TriggeredAbility.modal ability)

activatedAbilityFilters :: ActivatedAbility.ActivatedAbility Card.Type.Card -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
activatedAbilityFilters ability =
  unframed (costFilters (ActivatedAbility.cost ability))
    <> modalFilters (ActivatedAbility.modal ability)

-- EVERY Filter position reachable from a card, each paired with whether an attach
-- frames it. Nineteen of Pawl.Types.Card's twenty-seven fields can hold one, and
-- here is where each one's comes from:
--
--   * `keywords` -- CR 702.29e typecycling (Ash Barrens' landcycling).
--   * `power`, `toughness`, `characteristicPT` -- CR 208.2's printed star,
--     through a Count.
--   * `staticAbilities` -- the affected set, and the layer-6/7 modifications'
--     own keywords and Counts.
--   * `replacementEffects` -- CR 614.1's counter-placement pattern.
--   * `enchant` -- CR 303.4a's enchant ability, a TargetSpec.
--   * `additionalCosts`, `alternativeCosts` -- CR 601.2f's sacrifice component.
--   * `playerAbilities` -- CR 613.11's cost modifiers.
--   * `combatRestrictions` (CR 508.1c / 509.1b), `attackRequirements` (CR
--     508.1d) and `blockRequirements` (CR 509.1c) -- three more affected sets.
--   * `spell`, `activatedAbilities`, `triggeredAbilities`, `delayedAbilities` --
--     every mode's target specs and effects, plus an activation cost, a
--     trigger's own condition and its intervening clause.
--   * `mulliganAction` (CR 103.5b) and `openingHandAction` (CR 103.6) -- the two
--     pregame actions, which `cardResolutionEffects` above does not reach.
--
-- The other eight fields hold none: `name`, `manaCost`, `typeLine`, `loyalty`,
-- `colorIndicator`, `counterability`, `castingPermissions` and
-- `castingRestrictions`. That is checkable rather than asserted: exactly ten
-- modules under Pawl.Types import Pawl.Types.Filter -- Affected, CostComponent,
-- Count, CounterPattern, Effect, Keyword, ObjectRef, PlayerEffect, TargetSpec and
-- TriggerCondition -- and nothing those eight fields reach is one of them.
--
-- Every case BELOW this function is exhaustive with no catch-all, so a new
-- constructor on any of those types fails to compile until it is classified. This
-- record fold is the exception, exactly as cardCounts' own caveat says: a NEW
-- Card field that can hold a Filter would bypass it silently. That is what the
-- codec cross-check in canHostSubjectOffends is for.
cardFilters :: Card.Type.Card -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
cardFilters card =
  unframed
    ( concatMap keywordFilters (Set.toList (Card.Type.keywords card))
        <> concatMap quantityFilters (Maybe.maybeToList (Card.Type.characteristicPT card))
        <> concatMap (\(Power.MkPower quantity) -> quantityFilters quantity) (Maybe.maybeToList (Card.Type.power card))
        <> concatMap (\(Toughness.MkToughness quantity) -> quantityFilters quantity) (Maybe.maybeToList (Card.Type.toughness card))
        <> concatMap staticAbilityFilters (Card.Type.staticAbilities card)
        <> concatMap replacementEffectFilters (Card.Type.replacementEffects card)
        <> concatMap targetSpecFilters (Maybe.maybeToList (Card.Type.enchant card))
        <> concatMap costComponentFilters (Card.Type.additionalCosts card)
        <> concatMap costFilters (Card.Type.alternativeCosts card)
        <> concatMap (playerEffectFilters . PlayerStaticAbility.effect) (Card.Type.playerAbilities card)
        <> concatMap (affectedFilters . BlockRequirement.attacker) (Card.Type.blockRequirements card)
        <> concatMap (affectedFilters . AttackRequirement.subject) (Card.Type.attackRequirements card)
        <> concatMap combatRestrictionFilters (Card.Type.combatRestrictions card)
    )
    <> modalFilters (Card.Type.spell card)
    <> concatMap activatedAbilityFilters (Card.Type.activatedAbilities card)
    <> concatMap triggeredAbilityFilters (Card.Type.triggeredAbilities card)
    <> concatMap triggeredAbilityFilters (Map.elems (Card.Type.delayedAbilities card))
    <> concatMap effectFilters (Card.Type.mulliganAction card)
    <> concatMap effectFilters (Card.Type.openingHandAction card)

-- How many CR 701.3a atoms this card carries in an Effect.AttachTarget's
-- destination filter, and how many anywhere else. The second number is the
-- offence; the first is what Aura Graft legitimately has one of.
canHostSubjectCounts :: Card.Type.Card -> (Int, Int)
canHostSubjectCounts card =
  let total wanted = sum [canHostSubjects f | (framed, f) <- cardFilters card, framed == wanted]
   in (total True, total False)

-- Every occurrence of the atom's codec tag in an ENCODED card. The completeness
-- witness for the traversal above: Pawl.Codec.Card.toJson visits every field
-- of a Card and every type under it, is round-tripped by
-- Pawl.CodecIntegrationSpec's "honesty round-trip over allPrintings", and was
-- written for another purpose entirely -- so a Filter position cardFilters forgets
-- is one this still sees.
--
-- A tag and not a name: Pawl.Codec.Filter spells the atom `Common.nullary
-- "CanHostSubject"`, so the only string equal to this in a card's encoding is
-- that tag (a card NAMED "CanHostSubject" would be a false positive, and a loud
-- one rather than a silent miss).
jsonCanHostSubjects :: Value.Value -> Int
jsonCanHostSubjects value = case value of
  Value.String s -> if String.unwrap s == Text.pack "CanHostSubject" then 1 else 0
  Value.Array a -> sum (fmap jsonCanHostSubjects (Array.unwrap a))
  Value.Object o -> sum (fmap (jsonCanHostSubjects . Pair.value) (Object.unwrap o))
  Value.Null _ -> 0
  Value.Boolean _ -> 0
  Value.Number _ -> 0

-- CR 701.3a is answerable only where an attach FRAMES the match, and
-- Filter.CanHostSubject is vacuously False in every other Filter position. A card
-- author who wrote it into a target spec, a static ability's affected set, a Count
-- filter or a Search filter would otherwise get a False predicate and no failure
-- at all -- neither the codec, the type nor any other lint says a word -- so this
-- is where that is made loud.
--
-- TWO offences under one name, because they are two ways for the same claim to be
-- untrue:
--
--   * the traversal found the atom somewhere no attach frames it -- the misuse
--     itself; and
--   * the traversal and the codec disagree about how many the card holds -- which
--     means cardFilters has a blind spot, and an atom sitting in it would be
--     reported as zero rather than as an offence.
--
-- The second is not hypothetical maintenance theatre: cardFilters' Card-record
-- fold is hand-maintained, and a new field holding a Filter is exactly the kind of
-- change that would otherwise make this lint quietly stop doing its job.
canHostSubjectOffends :: Card.Type.Card -> Bool
canHostSubjectOffends card =
  let (framed, unframedCount) = canHostSubjectCounts card
   in unframedCount /= 0 || framed + unframedCount /= jsonCanHostSubjects (Card.toJson card)

-- The D4 dataflow lint: every slot an effect reads is declared, and every
-- declared slot is read. Equality, not subset: a spec no effect reads is a
-- card announcing a target it ignores -- representable in Magic, not in this
-- pool. Loosen to superset if such a card ever lands.
lintSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
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
  -- The D4 lint above is strictly per mode, so two modes of one card sharing a
  -- slot NAME pass it. This is the missing half, and the check both
  -- Modal.allTargetSpecs and Modal.modesTargetSpecs now name in their own
  -- comments as the thing that lets them union safely (#475). See
  -- cardSlotNamesCollide for what a shared name silently does.
  Spec.it s "no card's modes share a target slot name" $ do
    ps <- S.allPrintings s
    let declaring modal =
          length (filter (not . Map.null . Mode.targetSpecs) (Foldable.toList (Modal.modes modal)))
        -- Every modal cardSlotNamesCollide sweeps, so the guard below ranges over
        -- the same four scopes the lint does rather than over the spell alone.
        modalsOf card =
          Card.Type.spell card
            : fmap ActivatedAbility.modal (Card.Type.activatedAbilities card)
              <> fmap TriggeredAbility.modal (Card.Type.triggeredAbilities card)
              <> fmap TriggeredAbility.modal (Map.elems (Card.Type.delayedAbilities card))
        offenders = filter (cardSlotNamesCollide . Printing.card) ps
    -- Guards against passing vacuously: a pool whose every modal had at most one
    -- slot-declaring mode could not collide whatever the lint said. Dream's Grip
    -- is the spell that makes it real; Aether Channeler's triggered ability is
    -- the multi-mode ability nearest to it, with one declaring mode of three.
    Spec.assertBool s (any (any ((> 1) . declaring) . modalsOf . Printing.card) ps) "the pool has a modal with two slot-declaring modes"
    Spec.assertEqWith s "no fused mode slot" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The sweep above passes because the pool is authored correctly, so it proves
  -- nothing about the lint. The REJECTING direction is proven here, against
  -- hand-built offenders and against Dream's Grip misauthored on purpose --
  -- never a card file, since a card that offends a lint must not be loadable.
  Spec.it s "the lint itself catches two modes declaring one slot name" $ do
    dreamsGrip <- S.printingOf s registry "Dream's Grip"
    let creature = SlotName.MkSlotName (Text.pack "creature")
        victim = SlotName.MkSlotName (Text.pack "victim")
        tap slot = Effect.Tap (ObjectRef.InSlot slot)
        shared = modalActivated [lintMode [tap creature] [creature], lintMode [tap creature] [creature]]
        distinct = modalActivated [lintMode [tap creature] [creature], lintMode [tap victim] [victim]]
        collides = cardSlotNamesCollide . Printing.card
        -- Dream's Grip's own two modes, renamed to one shared slot: the exact
        -- authoring the card avoids, and the CR 702.42a fusion it would cause.
        card = Printing.card dreamsGrip
        fuse mode = mode {Mode.targetSpecs = Map.mapKeys (const creature) (Mode.targetSpecs mode)}
        fused = card {Card.Type.spell = (Card.Type.spell card) {Modal.modes = fmap fuse (Modal.modes (Card.Type.spell card))}}
    Spec.assertBool s (cardSlotNamesCollide (card {Card.Type.activatedAbilities = [shared]})) "two modes sharing one name are rejected"
    Spec.assertBool s (not (cardSlotNamesCollide (card {Card.Type.activatedAbilities = [distinct]}))) "and two modes naming distinct slots are accepted"
    Spec.assertBool s (cardSlotNamesCollide fused) "Dream's Grip with both modes on one slot is rejected"
    Spec.assertBool s (not (collides dreamsGrip)) "and the real card, naming them 'tapped' and 'untapped', is accepted"
  Spec.it s "every file in data/cards loads, and its card is named by its file name" $ do
    -- The registry checks name-against-file-name on each load
    -- (Pawl.Registry.loadFile), so sweeping the listing is the whole assertion:
    -- a stray file, a file whose card was renamed, and a file that no test
    -- happens to name all fail here. A hand-kept list is exactly what forgets
    -- the file nobody loads.
    slugs <- S.corpusSlugs
    Spec.assertBool s (not (null slugs)) "the corpus is not empty"
    mapM_ (S.cardOf s registry) slugs
  -- The other direction: a lookup slugifies the NAME it is asked for,
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
    blaze <- S.printingOf s registry "Blaze"
    let card = Printing.card blaze
        red = ManaSymbol.OfType (ManaType.Colored Color.Red)
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Blaze")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (Just (ManaCost.MkManaCost [ManaSymbol.Variable, red]))
    Spec.assertBool s (not (Card.isInstant card)) "sorcery, not instant"
    Spec.assertEqWith s "one AnyTarget slot" (Card.allTargetSpecs card) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
    Spec.assertEqWith s "effect deals X" (Card.allEffects card) [Effect.DealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Quantity.Type.X]
  Spec.it s "the lint itself catches a dangling reference" $
    let bad = Set.unions [Resolve.slotsOf (Effect.DealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "ghost"))) (Quantity.Type.Literal 3))]
     in Spec.assertBool s (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSpec.TargetSpec)) "misauthored card detected"
  -- The SPELL half of CR 601.2b's contract: what a card's own modes read is
  -- announced against the card's own mana cost.
  Spec.it s "every printing that reads X declares {X}, and vice versa" $ do
    ps <- S.allPrintings s
    let readsX c = Resolve.readsX (Card.allEffects c)
        offenders =
          filter
            (\p -> readsX (Printing.card p) /= declaresVariable (Card.Type.manaCost (Printing.card p)))
            ps
    Spec.assertEqWith s "X read iff {X} declared" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The ACTIVATED-ABILITY half, and it is a separate sweep because it is a
  -- separate cost: CR 602.2b makes "an activated ability's analog to a spell's
  -- mana cost (as referenced in rule 601.2f) ... its activation cost", so an
  -- ability's X is announced against the cost before its own colon and never
  -- against the card's. Cinder Elemental is the pool's producer -- "{X}{R}, {T},
  -- Sacrifice this creature: It deals X damage to any target" reads an X its
  -- CARD's {3}{R} does not declare, which the sweep above would have called an
  -- offender and this one calls correct (#544).
  Spec.it s "CR 602.2b every activated ability that reads X declares {X} in its own cost" $ do
    ps <- S.allPrintings s
    let abilitiesOf p = fmap ((,) (Card.Type.name (Printing.card p))) (Card.Type.activatedAbilities (Printing.card p))
        abilities = concatMap abilitiesOf ps
        offends (_, ab) =
          Resolve.readsX (Modal.allEffects (ActivatedAbility.modal ab))
            /= declaresVariable (Cost.Type.mana (ActivatedAbility.cost ab))
    -- Guards the sweep against passing vacuously, in both directions: an empty
    -- pool of abilities, and a pool in which no activation cost prints an {X} at
    -- all (where the lint would hold for every card by agreeing on False).
    Spec.assertBool s (not (null abilities)) "the pool has activated abilities"
    Spec.assertBool s (any (declaresVariable . Cost.Type.mana . ActivatedAbility.cost . snd) abilities) "and one of them prints an {X}"
    Spec.assertEqWith s "X read iff {X} declared" (fmap fst (filter offends abilities)) []
  Spec.it s "CR 111.4 every token a card creates is named its subtypes plus \"Token\"" $ do
    ps <- S.allPrintings s
    let tokensOf card = [token | Effect.Create _ token _ _ <- cardResolutionEffects card]
        tokens = concatMap (tokensOf . Printing.card) ps
    -- Guards the sweep against passing vacuously if Create ever moves out
    -- from under cardResolutionEffects.
    Spec.assertBool s (not (null tokens)) "the pool creates tokens"
    Spec.assertEqWith s "no token is misnamed" (fmap Card.Type.name (filter tokenNameOffends tokens)) []
  Spec.it s "the lint itself catches a token named without the suffix" $ do
    doomedTraveler <- S.printingOf s registry "Doomed Traveler"
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
    roaches <- S.printingOf s registry "Endless Cockroaches"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    tidalWave <- S.printingOf s registry "Tidal Wave"
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
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let card = Printing.card lightningBolt
    Spec.assertBool s (Card.isInstant card) "an instant"
    Spec.assertEqWith s "one slot" (Card.allTargetSpecs card) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
  -- The AbilityName half of the D4 dataflow lint (CR 603.7): an
  -- ArmDelayedTrigger naming an ability the card does not declare is a FAILING
  -- TEST, never a trigger that silently never fires. Equality, not subset: a
  -- declared ability nothing arms is dead card text.
  --
  -- SCOPE: `cardResolutionEffects`, every carrier a card can execute an effect
  -- from -- its spell modes AND its activated, triggered and delayed abilities'
  -- -- and not `Card.allEffects`, which is the spell modes alone. Meandering
  -- Towershell is what makes the difference load-bearing: it arms from a
  -- TRIGGERED ability, so the narrower view saw a declared entry that nothing
  -- appeared to arm and failed the equality outright.
  --
  -- The multi-token-binding lint below takes the same wide view, for the same
  -- reason: nothing about CR 603.7c's one-of-several question is peculiar to a
  -- spell mode. It sweeps nothing new today -- no ability in this pool creates
  -- tokens and binds one -- so the widening is a hole closed, not a claim.
  Spec.it s "every armed delayed ability is declared, and every declared one is armed" $ do
    ps <- S.allPrintings s
    let cardOffends card =
          Resolve.armedAbilities (cardResolutionEffects card) /= Map.keysSet (Card.Type.delayedAbilities card)
        offenders = filter (cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling or unused delayed abilities" (fmap (Card.Type.name . Printing.card) offenders) []
  -- Every slot a delayed ability READS must be one the arming card DEFINES:
  -- the reserved trigger-source slot, a token bound by a Create, or the
  -- incarnation a MoveToZone bound at its destination (Meandering Towershell's
  -- exiled card). The `abilityBound` side is `cardResolutionEffects` for the
  -- reason the lint above takes it: the binding effect can live in the ability
  -- that arms, not only in a spell mode.
  --
  -- Through modalReadOffends, so a delayed ability with modes is read PER MODE
  -- (#570) -- and so a mode's own declared target specs count, which this lint
  -- omitted entirely. CR 603.3d puts a delayed ability on the stack "identical
  -- to the process for casting a spell listed in rules 601.2c-d", so a slot it
  -- declares really is announced; declaredTargetSlots already counts delayed
  -- abilities' specs on the DECLARING side, and this is the matching read side.
  Spec.it s "every slot a delayed ability reads is bound by its card" $ do
    ps <- S.allPrintings s
    let cardOffends card =
          let bound = Set.insert Binding.triggerSource (Resolve.definedSlots (cardResolutionEffects card))
           in any (modalReadOffends bound . TriggeredAbility.modal) (Map.elems (Card.Type.delayedAbilities card))
        offenders = filter (cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling delayed-ability slot" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The pairing Pawl.Types.Onset.FromYourNextTurn depends on and cannot enforce
  -- alone. See onsetOffends for why the onset and the condition's TurnScope
  -- are two halves of one printed "your next turn", and what goes wrong when a
  -- card supplies only one of them.
  Spec.it s "every delayed ability armed for YOUR next turn is controller-scoped" $ do
    ps <- S.allPrintings s
    let offenders = filter (onsetOffends . Printing.card) ps
    Spec.assertEqWith s "no onset over a condition that admits another player's turn" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The sweep above is NOT vacuous -- Meandering Towershell is a real card with
  -- an onset, so the accepting direction is exercised by the pool -- but nothing
  -- committed offends it, so the REJECTING direction is proven here instead,
  -- against that same card misauthored on purpose. Never a card file: a card
  -- that offends a lint must not be loadable.
  Spec.it s "the lint itself catches an onset over an EachTurn condition" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    let card = Printing.card towershell
        -- The Towershell's own condition with CR 603.2b's OTHER turn scope: "at
        -- the beginning of EACH declare attackers step", which an opponent's
        -- turn satisfies. Built rather than pattern-matched, so this fixture
        -- states the offence outright.
        eachTurn ability =
          ability
            { TriggeredAbility.condition =
                TriggerCondition.StepBegins (Phase.Combat CombatStep.DeclareAttackers) TurnScope.EachTurn
            }
        widened = card {Card.Type.delayedAbilities = fmap eachTurn (Card.Type.delayedAbilities card)}
        -- The other way a card can reach this: an onset naming an ability the
        -- card does not declare at all.
        dangling = card {Card.Type.delayedAbilities = Map.empty}
    Spec.assertBool s (not (onsetOffends card)) "the real card, ControllersTurn, is accepted"
    Spec.assertBool s (onsetOffends widened) "EachTurn under an onset is rejected"
    Spec.assertBool s (onsetOffends dangling) "and so is an onset naming no declared ability"
    -- Not a check that fires for every card: one with no onset at all has
    -- nothing for this to reject, whatever its delayed abilities are scoped to.
    tidalWave <- S.printingOf s registry "Tidal Wave"
    Spec.assertBool s (not (onsetOffends (Printing.card tidalWave))) "a card with no onset is not swept up"
  -- The same subset shape over a card's TRIGGERED abilities, which is where
  -- the condition-specific reserved slots live -- CR 400.7e's `became` and
  -- CR 702.70a's `thatPlayer`. See triggeredAbilityOffends for the available
  -- side and for why this cannot be an equality check.
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
    roaches <- S.printingOf s registry "Endless Cockroaches"
    let -- Endless Cockroaches' own payload: "return it to its owner's hand".
        returnIt = Effect.MoveToZone Binding.became Zone.Hand EntryRiders.defaultValue Nothing
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
  -- The same subset shape over a card's ACTIVATED abilities, whose available
  -- side is the narrowest of the three: an activation has no event, and is never
  -- given CR 109.5's `you`. See activatedAbilityOffends for the available side.
  Spec.it s "every slot an activated ability reads is bound for its activation" $ do
    ps <- S.allPrintings s
    let abilitiesOf p = fmap ((,) (Card.Type.name (Printing.card p))) (Card.Type.activatedAbilities (Printing.card p))
        abilities = concatMap abilitiesOf ps
        readsAnySlot ab = not (Set.null (Set.unions (fmap Resolve.slotsOf (Modal.allEffects (ActivatedAbility.modal ab)))))
    -- Guards the sweep against passing vacuously, in both directions: an empty
    -- pool of abilities, and a pool in which none reads a slot at all (where
    -- every ability would pass on an empty read side whatever the lint said).
    Spec.assertBool s (not (null abilities)) "the pool has activated abilities"
    Spec.assertBool s (any (readsAnySlot . snd) abilities) "and one of them reads a slot"
    Spec.assertEqWith s "no dangling activated-ability slot" (fmap fst (filter (activatedAbilityOffends . snd) abilities)) []
  -- The sweep above passes VACUOUSLY on the rejecting side: no committed
  -- activated ability reads a slot it is not given, so the REJECTING direction is
  -- proven here instead, against hand-built offenders and against three real
  -- cards that exercise each part of the available side.
  --
  -- Every reserved slot an activation does NOT bind gets its own case, because a
  -- classification answering "every slot, always" would pass any one of them
  -- alone. The `you` case is asserted twice over: rejected for an activated
  -- ability AND accepted for a triggered one, which is the whole difference
  -- between the two lints (Binding.setYou is stamped only on the triggered path).
  Spec.it s "the lint itself catches an activated ability reading a slot activation never binds" $ do
    longtuskCub <- S.printingOf s registry "Longtusk Cub"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    cinderElemental <- S.printingOf s registry "Cinder Elemental"
    let free = Just (ManaCost.MkManaCost [])
        variable = Just (ManaCost.MkManaCost [ManaSymbol.Variable])
        -- CR 109.5's "you", in the shape Baral, Chief of Compliance's TRIGGERED
        -- ability uses it: a bare-SlotName opcode (#378) naming the controller.
        youDiscards = Effect.Discard Binding.you (Quantity.Type.Literal 1)
        -- Endless Cockroaches' payload (CR 400.7e) and rule 702.70a's, the two
        -- event slots, neither of which an activation has an event to bind.
        returnIt = Effect.MoveToZone Binding.became Zone.Hand EntryRiders.defaultValue Nothing
        thatPlayerDraws = Effect.Draw (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1)
        -- CR 113.7's source slot, which every activation DOES bind.
        tapSelf = Effect.Tap (ObjectRef.InSlot Binding.triggerSource)
        -- An ordinary slot this ability neither declares nor mints.
        tapGhost = Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "ghost")))
        -- CR 601.2b's announced value, read as a slot rather than as Quantity.X.
        drawX = Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.InSlot Binding.variableX)
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free youDiscards))
      "CR 109.5 you is rejected: an activation never binds it"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (oneEffectTrigger TriggerCondition.SelfDies youDiscards)))
      "and the very same effect is accepted on a triggered ability"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free returnIt))
      "CR 400.7e became is rejected: an activation is not an event"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free thatPlayerDraws))
      "CR 702.70a thatPlayer is rejected for the same reason"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free tapGhost))
      "and so is an ordinary slot the ability never declares"
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated free tapSelf)))
      "CR 113.7 self is accepted, stamped for every activation"
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated variable drawX)))
      "CR 601.2b X is accepted when the activation cost prints {X}"
    Spec.assertBool
      s
      (activatedAbilityOffends (oneEffectActivated free drawX))
      "and rejected when it does not"
    -- The three real cards between them cover every part of the available side
    -- that a committed card reaches: CR 113.7's self, CR 601.2c's declared
    -- target, and an ability whose cost carries CR 601.2b's {X}.
    Spec.assertEqWith
      s
      "Longtusk Cub, Prodigal Sorcerer and Cinder Elemental are all accepted"
      (fmap (any activatedAbilityOffends . Card.Type.activatedAbilities . Printing.card) [longtuskCub, sorcerer, cinderElemental])
      [False, False, False]
  -- The PER-MODE half of all three read lints (#570), which no sweep above can
  -- reach: a one-mode ability cannot have a mode read another mode's slot at
  -- all, and all three multi-mode abilities in the pool have each mode reading
  -- only what that mode declares, so per-mode and the old union shape agree on
  -- every card committed today. Aether Channeler's is the one that comes closest
  -- to exercising the difference and the only non-synthetic one -- three modes,
  -- of which only the middle declares a slot -- and the other two (Synthetic
  -- Modal Activator's, Synthetic Modal Trigger's) declare one apiece. Proven
  -- here against hand-built offenders instead.
  --
  -- Both halves of the union, because closing one and leaving the other would
  -- pass this: the declared TARGET slots (CR 700.2c) and the slots an effect
  -- MINTS (Resolve.definedSlots).
  Spec.it s "the lint itself catches a mode reading a slot only another mode declares" $ do
    let creature = SlotName.MkSlotName (Text.pack "creature")
        victim = SlotName.MkSlotName (Text.pack "victim")
        exiled = SlotName.MkSlotName (Text.pack "exiled")
        tap slot = Effect.Tap (ObjectRef.InSlot slot)
        -- Mode 0 declares `creature` and reads it; mode 1 reads it and declares
        -- nothing. Under ChooseExactly 1, choosing mode 1 alone stamps mode 1's
        -- specs -- which is nothing -- so the read is unbound at runtime.
        crossDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap creature] []]
        -- The same two reads, each mode declaring the slot it reads.
        ownDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap victim] [victim]]
        -- Mode 0 MINTS `exiled` at a MoveToZone's destination; mode 1 reads it.
        -- The two never resolve together, so mode 1's read is dangling.
        exileIt = Effect.MoveToZone creature Zone.Exile EntryRiders.defaultValue (Just exiled)
        crossMinted = modalActivated [lintMode [exileIt] [creature], lintMode [tap exiled] []]
        ownMinted = modalActivated [lintMode [exileIt, tap exiled] [creature], lintMode [tap victim] [victim]]
    Spec.assertBool s (activatedAbilityOffends crossDeclared) "a mode reading a slot only another mode declares is rejected"
    Spec.assertBool s (not (activatedAbilityOffends ownDeclared)) "and each mode reading only what it declares is accepted"
    Spec.assertBool s (activatedAbilityOffends crossMinted) "a mode reading a slot only another mode mints is rejected"
    Spec.assertBool s (not (activatedAbilityOffends ownMinted)) "and a mode reading what it mints itself is accepted"
    -- The ABILITY-scoped side must still reach every mode, not just the first:
    -- CR 400.7e's `became` is bound by the condition for the whole ability, so a
    -- SECOND mode reading it is accepted, and a mode reading it under a
    -- condition that never binds it is rejected however late the mode sits.
    let returnBecame = Effect.MoveToZone Binding.became Zone.Hand EntryRiders.defaultValue Nothing
        secondModeReads condition = modalTrigger condition [lintMode [] [], lintMode [returnBecame] []]
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (secondModeReads TriggerCondition.SelfDies)))
      "the condition's event slots reach a later mode too"
    Spec.assertBool
      s
      (triggeredAbilityOffends (secondModeReads TriggerCondition.SelfEnters))
      "and a later mode is still rejected when the condition binds nothing"
  -- CR 603.7c: binding a slot to a MULTI-token Create would silently name one
  -- of them. Rejected rather than guessed (#53).
  Spec.it s "no Create binds a slot while making more than one token" $ do
    ps <- S.allPrintings s
    let offenders =
          filter
            (Resolve.bindsSeveralTokens . cardResolutionEffects . Printing.card)
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
    piker <- S.printingOf s registry "Goblin Piker"
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
  -- A codec-level rejection would be the wrong shape: Modification.fromJson is
  -- shared with staticAbilities, which Control Magic legitimately uses.
  Spec.it s "no card authors a control modification into a resolving effect (#199)" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.ModifyTarget _ modification _ -> Projection.layer modification == Layer.Control
          _ -> False
        offenders = filter (any offends . cardResolutionEffects . Printing.card) ps
    Spec.assertEqWith s "control belongs on a static ability, never in a stored effect" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The sibling of the lint above, for the OTHER PlayerId the engine bakes and
  -- the codec accepts. See phasePatternOffends for why a card cannot name a
  -- player, and for why this is a lint rather than a type split (#437).
  Spec.it s "no card authors a player-scoped phase skip (#437)" $ do
    ps <- S.allPrintings s
    let offenders = filter (any phasePatternOffends . cardReplacementEffects . Printing.card) ps
    -- Guards against a vacuous sweep: with no PhaseR in the pool at all this
    -- would pass whatever the classification said. Eon Hub is the card that
    -- prints one.
    Spec.assertBool s (any (any isPhaseR . cardReplacementEffects . Printing.card) ps) "the pool has a card printing a phase skip"
    Spec.assertEqWith s "whosePhase is baked by the engine, never authored" (fmap (Card.Type.name . Printing.card) offenders) []
  -- The sweep passes because the pool is authored correctly, so the REJECTING
  -- direction is proven here against Eon Hub with a seat baked into it -- never
  -- a card file, since a card that offends a lint must not be loadable.
  Spec.it s "the lint itself catches a baked whosePhase" $ do
    eonHub <- S.printingOf s registry "Eon Hub"
    let card = Printing.card eonHub
        bake replacement = case replacement of
          ReplacementEffect.PhaseR phasePattern ->
            ReplacementEffect.PhaseR phasePattern {PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)}
          other -> other
        baked = card {Card.Type.replacementEffects = fmap bake (Card.Type.replacementEffects card)}
    Spec.assertBool s (not (any phasePatternOffends (cardReplacementEffects card))) "the real Eon Hub is symmetric and accepted"
    Spec.assertBool s (any phasePatternOffends (cardReplacementEffects baked)) "and the same card naming a seat is rejected"
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
  -- CR 701.3a: "An Aura, Equipment, or Fortification can't be attached to an
  -- object or player it couldn't enchant, equip, or fortify, respectively." The
  -- atom that asks that question is answerable only where an attach frames the
  -- match, and vacuously False everywhere else. See canHostSubjectOffends for the
  -- two offences this one predicate covers.
  Spec.it s "CR 701.3a no card asks CanHostSubject outside an attach's destination" $ do
    ps <- S.allPrintings s
    let offenders = filter (canHostSubjectOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only in an AttachTarget destination" (fmap (Card.Type.name . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the one card that does is
    -- ACCEPTED here rather than skipped. Aura Graft's "another permanent it can
    -- enchant" is the whole legal use, so a lint that swept past it would be
    -- indistinguishable from one that swept past everything.
    graft <- S.printingOf s registry "Aura Graft"
    Spec.assertEqWith
      s
      "Aura Graft's one atom is framed by its own attach"
      (canHostSubjectCounts (Printing.card graft))
      (1, 0)
    Spec.assertEqWith
      s
      "and it is the pool's only one"
      (sum (fmap (\p -> uncurry (+) (canHostSubjectCounts (Printing.card p))) ps))
      1
    -- The traversal reaches a Filter position no effect, target spec or affected
    -- set would have led it to: CR 702.29e's typecycling predicate, on a real
    -- card. Its absence would not show up in the sweep above, because Ash Barrens
    -- does not author the atom -- only in this.
    barrens <- S.printingOf s registry "Ash Barrens"
    Spec.assertBool
      s
      ( elem
          (False, Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic])
          (cardFilters (Printing.card barrens))
      )
      "CR 702.29e landcycling's filter is a position the sweep walks"
  -- The sweep above passes VACUOUSLY for every card but Aura Graft, and Aura
  -- Graft only exercises the ACCEPTING direction, so the rejecting direction is
  -- proven here instead -- hand-built, never a card file, because a card that
  -- offends a lint must not be loadable.
  --
  -- Every fixture plants the atom BURIED under all three combinators rather than
  -- bare, so an implementation that looked only at the top of a Filter would
  -- accept every one of them. And each is asserted through canHostSubjectCounts
  -- as well as through the predicate: the counts say the TRAVERSAL found it in
  -- that position, where the predicate alone would also be satisfied by the codec
  -- half of the cross-check noticing an atom the traversal missed entirely.
  Spec.it s "the lint itself catches CanHostSubject outside an attach's destination" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    graft <- S.printingOf s registry "Aura Graft"
    let base = Printing.card piker
        slot = SlotName.MkSlotName (Text.pack "target")
        atom = Filter.Type.CanHostSubject
        buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not atom]]
        -- A one-mode, mandatory spell running these effects and declaring these
        -- slots -- the smallest carrier that reaches Mode.effects and
        -- Mode.targetSpecs at once.
        spellOf effects specs =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.fromList effects) specs Optionality.Mandatory))
            (ModeSelection.ChooseExactly 1)
        boostedBy quantity =
          StaticAbility.MkStaticAbility
            (Affected.Matching Filter.Type.IsSource)
            (NonEmpty.singleton (Modification.ModifyPowerToughness quantity (Quantity.Type.Literal 0)))
        planted =
          [ ( "a target spec",
              base {Card.Type.spell = spellOf [] (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Permanents (Just buried)))}
            ),
            ( "CR 303.4a's enchant ability",
              base {Card.Type.enchant = Just (TargetSpec.MkTargetSpec Pool.Permanents (Just buried))}
            ),
            ( "a static ability's affected set",
              base
                { Card.Type.staticAbilities =
                    [ StaticAbility.MkStaticAbility
                        (Affected.Matching buried)
                        (NonEmpty.singleton (Modification.ModifyPowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1)))
                    ]
                }
            ),
            ( "a Count's filter",
              base
                { Card.Type.staticAbilities =
                    [ boostedBy
                        ( Quantity.Type.Count
                            (Count.Type.MkCount (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer) buried Aggregation.Objects)
                        )
                    ]
                }
            ),
            ( "a Search filter",
              base {Card.Type.spell = spellOf [Effect.Search buried SearchDestination.RevealThenHand] Map.empty}
            ),
            ( "an ObjectRef.EachMatching set",
              base {Card.Type.spell = spellOf [Effect.Destroy (ObjectRef.EachMatching buried) Regenerability.Regenerable Nothing] Map.empty}
            ),
            ( "CR 603.6a's trigger condition",
              base
                { Card.Type.triggeredAbilities =
                    [ oneEffectTrigger
                        (TriggerCondition.PermanentEnters buried)
                        (Effect.Draw (PlayerRef.InSlot Binding.you) (Quantity.Type.Literal 1))
                    ]
                }
            ),
            ( "CR 601.2f's sacrifice cost component",
              (Printing.card sorcerer)
                { Card.Type.activatedAbilities =
                    fmap
                      (\a -> a {ActivatedAbility.cost = (ActivatedAbility.cost a) {Cost.Type.components = [CostComponent.Sacrifice 1 buried]}})
                      (Card.Type.activatedAbilities (Printing.card sorcerer))
                }
            ),
            ( "CR 702.29e's typecycling predicate",
              base {Card.Type.keywords = Set.singleton (Keyword.Cycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Just buried))}
            ),
            ( "CR 613.11's spell-cost modifier",
              base
                { Card.Type.playerAbilities =
                    [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.IncreaseSpellCost buried 1)]
                }
            ),
            ( "CR 508.1c's combat restriction",
              base {Card.Type.combatRestrictions = [CombatRestriction.CantAttack (Affected.Matching buried)]}
            ),
            ( "CR 614.1's counter-placement pattern",
              base
                { Card.Type.replacementEffects =
                    [ReplacementEffect.CounterR (CounterPattern.MkCounterPattern Nothing ControllerRelation.Yours buried) (Scaling.AddMore 1)]
                }
            ),
            ( "a created token's own static ability",
              base
                { Card.Type.spell =
                    spellOf
                      [ Effect.Create
                          (Quantity.Type.Literal 1)
                          (base {Card.Type.staticAbilities = [StaticAbility.MkStaticAbility (Affected.Matching buried) (NonEmpty.singleton Modification.LoseAllAbilities)]})
                          EntryRiders.defaultValue
                          Nothing
                      ]
                      Map.empty
                }
            ),
            ( "CR 103.5b's pregame action",
              base {Card.Type.mulliganAction = [Effect.Search buried SearchDestination.RevealThenHand]}
            )
          ]
        report (label, card) = (label, canHostSubjectOffends card, canHostSubjectCounts card)
    Spec.assertEqWith
      s
      "every unframed position is rejected, and the traversal is what finds it"
      (fmap report planted)
      (fmap (\(label, _) -> (label, True, (0, 1))) planted)
    -- The cross-check agrees on every fixture, which is what says it reports a
    -- blind spot rather than firing on cards that have none.
    Spec.assertEqWith
      s
      "and the codec counts exactly the atoms the traversal does"
      (fmap (\(_, card) -> jsonCanHostSubjects (Card.toJson card)) planted)
      (fmap (const 1) planted)
    -- The nesting, stated on its own: a top-level-only check would score every
    -- one of these zero but the first.
    Spec.assertEqWith
      s
      "the atom is found at every nesting depth"
      ( fmap
          canHostSubjects
          [ atom,
            Filter.Type.And [atom],
            Filter.Type.Or [atom],
            Filter.Type.Not atom,
            buried,
            Filter.Type.HasKeyword (Keyword.Cycling (Cost.Type.MkCost Nothing []) (Just atom))
          ]
      )
      [1, 1, 1, 1, 1, 1]
    -- The ACCEPTING direction, twice: the real card, and the buried atom in an
    -- AttachTarget destination grafted onto a card with no attach of its own --
    -- so the acceptance is about the POSITION and not about Aura Graft.
    Spec.assertEqWith
      s
      "Aura Graft is accepted"
      (canHostSubjectOffends (Printing.card graft), canHostSubjectCounts (Printing.card graft))
      (False, (1, 0))
    let grafted = base {Card.Type.spell = spellOf [Effect.AttachTarget slot buried] (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Permanents Nothing))}
    Spec.assertEqWith
      s
      "a buried atom in an AttachTarget destination is accepted"
      (canHostSubjectOffends grafted, canHostSubjectCounts grafted)
      (False, (1, 0))
    Spec.assertEqWith
      s
      "and the ungrafted base card carries no atom at all"
      (canHostSubjectOffends base, canHostSubjectCounts base)
      (False, (0, 0))

m2bCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m2bCardSpec s registry = Spec.describe s "M2bCards" $ do
  let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      gs0 = Setup.emptyGame S.bothPlayers
  Spec.it s "Sabretooth Tiger is a {2}{R} 2/1 Cat with first strike" $ do
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    let c = Printing.card sabretoothTiger
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Sabretooth Tiger")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.singleton Subtype.Cat)
    Spec.assertEqWith s "keyword" (Card.Type.keywords c) (Set.singleton Keyword.FirstStrike)
  Spec.it s "Ridgetop Raptor is a {3}{R} 2/1 Dinosaur Beast with double strike" $ do
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    let c = Printing.card ridgetopRaptor
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Ridgetop Raptor")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, red]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine c)) (Set.fromList [Subtype.Dinosaur, Subtype.Beast])
    Spec.assertEqWith s "keyword" (Card.Type.keywords c) (Set.singleton Keyword.DoubleStrike)
  Spec.it s "the tiger has first strike through the projection" $ do
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    let (oid, gs) = S.addCreature sabretoothTiger S.alice gs0
    Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike oid gs) "first strike"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.DoubleStrike oid gs)) "not double strike"
  Spec.it s "the raptor has double strike through the projection" $ do
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    let (oid, gs) = S.addCreature ridgetopRaptor S.alice gs0
    Spec.assertBool s (Projection.hasKeyword Keyword.DoubleStrike oid gs) "double strike"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike oid gs)) "not first strike"
  Spec.it s "both are 2/1s, the same body as a Piker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    let bodyOf p = (Card.Type.power (Printing.card p), Card.Type.toughness (Printing.card p))
    Spec.assertEqWith s "tiger body" (bodyOf sabretoothTiger) (bodyOf piker)
    Spec.assertEqWith s "raptor body" (bodyOf ridgetopRaptor) (bodyOf piker)

m2cCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m2cCardSpec s registry = Spec.describe s "M2cCards" $ do
  Spec.it s "Typhoid Rats is a {B} 1/1 Rat with deathtouch" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let c = Printing.card typhoidRats
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Typhoid Rats")
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "keywords" (Card.Type.keywords c) (Set.singleton Keyword.Deathtouch)
  Spec.it s "War Mammoth is a {3}{G} 3/3 Elephant with trample" $ do
    warMammoth <- S.printingOf s registry "War Mammoth"
    let c = Printing.card warMammoth
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "War Mammoth")
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 3)))
    Spec.assertEqWith s "keywords" (Card.Type.keywords c) (Set.singleton Keyword.Trample)

basicLandSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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
    swamp <- S.printingOf s registry "Swamp"
    let c = Printing.card swamp
    Spec.assertBool s (Card.isLand c) "land"
    Spec.assertBool
      s
      (Set.member Subtype.Swamp (TypeLine.subtypes (Card.Type.typeLine c)))
      "swamp subtype"
  Spec.it s "forestPrinting is a basic Forest land" $ do
    forest <- S.printingOf s registry "Forest"
    let c = Printing.card forest
    Spec.assertBool s (Card.isLand c) "land"
    Spec.assertBool
      s
      (Set.member Subtype.Forest (TypeLine.subtypes (Card.Type.typeLine c)))
      "forest subtype"

m3cCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
m3cCardSpec s registry = Spec.describe s "M3cCards" $ do
  Spec.it s "Blood Moon is a {2}{R} enchantment with one SetLandSubtype static ability" $ do
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let card = Printing.card bloodMoon
    Spec.assertEqWith s "one static ability" (length (Card.Type.staticAbilities card)) 1
    Spec.assertBool s (Map.null (Card.allTargetSpecs card)) "not a permanent target"

m3eCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m3eCardSpec s registry = Spec.describe s "M3eCards" $ do
  Spec.it s "Prodigal Sorcerer has one non-mana activated ability" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    case Card.Type.activatedAbilities (Printing.card prodigalSorcerer) of
      [ab] -> Spec.assertBool s (not (Mana.isManaAbility ab)) "not a mana ability"
      _ -> Spec.assertFailure s "expected exactly one ability"
  Spec.it s "Llanowar Elves has one mana activated ability" $ do
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    case Card.Type.activatedAbilities (Printing.card llanowarElves) of
      [ab] -> Spec.assertBool s (Mana.isManaAbility ab) "mana ability"
      _ -> Spec.assertFailure s "expected exactly one ability"

m4bCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m4bCardSpec s registry = Spec.describe s "M4bCards" $ do
  Spec.it s "Darksteel Myr is a {3} 0/1 Artifact Creature with indestructible" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let c = Printing.card darksteelMyr
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Darksteel Myr")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]))
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 0)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 1)))
    Spec.assertEqWith s "keyword" (Card.Type.keywords c) (Set.singleton Keyword.Indestructible)
  -- #113: both P9 gate cards end "It can't be regenerated", and the clause was
  -- omitted from their data while nothing could honour it. It is data now.
  Spec.it s "CR 701.19c Terror and Reprisal both carry the can't-be-regenerated rider" $ do
    terror <- S.printingOf s registry "Terror"
    reprisal <- S.printingOf s registry "Reprisal"
    let riders c = [r | Effect.Destroy _ r _ <- Card.allEffects (Printing.card c)]
    Spec.assertEqWith s "Terror" (riders terror) [Regenerability.CantBeRegenerated]
    Spec.assertEqWith s "Reprisal" (riders reprisal) [Regenerability.CantBeRegenerated]
  Spec.it s "Murder is a {1}{B}{B} Instant that destroys a target creature" $ do
    murder <- S.printingOf s registry "Murder"
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
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
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
    walls <- S.printingOf s registry "The Walls of Ba Sing Se"
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
    unsummon <- S.printingOf s registry "Unsummon"
    let c = Printing.card unsummon
        blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [blue]))
    Spec.assertEqWith s "effect returns to hand" (Card.allEffects c) [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Hand EntryRiders.defaultValue Nothing]
  -- Unsummon's effect exactly, over a different pool: the same MoveToZone to the
  -- hand, so the whole card is its target spec. CR 115.2's clause (a) admits it
  -- ("specifies that it can target an object in another zone"), CR 400.1 is why
  -- the pool carries a scope, and CR 109.2's default is switched off by the
  -- printed word "card" -- which is why the creature-ness is a Filter over a
  -- ToObject candidate rather than a Pool.Creatures slot.
  Spec.it s "Raise Dead is a {B} Sorcery returning a creature card from your graveyard to your hand" $ do
    raiseDead <- S.printingOf s registry "Raise Dead"
    let c = Printing.card raiseDead
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [black]))
    Spec.assertBool s (Card.isSorcery c) "a sorcery"
    Spec.assertEqWith s "effect returns to hand" (Card.allEffects c) [Effect.MoveToZone target Zone.Hand EntryRiders.defaultValue Nothing]
    Spec.assertEqWith
      s
      "one creature-card-in-your-graveyard slot"
      (Card.allTargetSpecs c)
      (Map.singleton target (TargetSpec.MkTargetSpec (Pool.CardsInGraveyard PlayerScope.You) (Just (Filter.Type.HasCardType CardType.Creature))))
  -- Raise Dead's pool, on the OTHER end of CR 400.1's axis and with no Filter at
  -- all: "{1}: Exile target card from a graveyard" says neither whose graveyard
  -- nor what kind of card, so the scope is EachPlayer and the Filter is Nothing.
  -- Both absences are asserted, because either one written in by mistake would
  -- narrow the card without changing anything else about it.
  Spec.it s "Withered Wretch is a {B}{B} Zombie Cleric exiling any card from any graveyard" $ do
    wretch <- S.printingOf s registry "Withered Wretch"
    let c = Printing.card wretch
        black = ManaSymbol.OfType (ManaType.Colored Color.Black)
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (Just (ManaCost.MkManaCost [black, black]))
    Spec.assertEqWith
      s
      "a Zombie Cleric (CR 205.3m)"
      (TypeLine.subtypes (Card.Type.typeLine c))
      (Set.fromList [Subtype.Zombie, Subtype.Cleric])
    Spec.assertEqWith s "the spell itself does nothing (a vanilla creature spell)" (Card.allEffects c) []
    case Card.Type.activatedAbilities c of
      [ability] -> do
        Spec.assertEqWith
          s
          "one ability, costing {1} and nothing else"
          (ActivatedAbility.cost ability)
          (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])
        Spec.assertEqWith
          s
          "which exiles the target (CR 406.2)"
          (Modal.allEffects (ActivatedAbility.modal ability))
          [Effect.MoveToZone target Zone.Exile EntryRiders.defaultValue Nothing]
        Spec.assertEqWith
          s
          "off one unfiltered any-graveyard slot"
          (Modal.allTargetSpecs (ActivatedAbility.modal ability))
          (Map.singleton target (TargetSpec.MkTargetSpec (Pool.CardsInGraveyard PlayerScope.EachPlayer) Nothing))
      abilities -> Spec.assertFailure s ("expected one activated ability, got " <> show (length abilities))
  -- CR 115.2 clause (a)'s other zone. Riftsweeper's slot is the first
  -- Pool.CardsInExile in the corpus, and its two ABSENCES are the assertions
  -- that matter, since either one filled in by mistake would narrow the card
  -- without changing anything else about it:
  --
  --   * no PlayerScope on the pool, because CR 400.1 says "the other zones are
  --     shared by all players" and exile is one of them -- the graveyard cards
  --     above are the contrast, and both are read in this same group.
  --   * no Filter, because "target face-up exiled card" names no card type, and
  --     "face-up" is CR 406.3's default rather than a narrowing (#557).
  --
  -- The effect is ShuffleIntoLibrary and NOT a MoveToZone to the library: CR
  -- 701.24 makes shuffling a keyword action of its own, with CR 701.24c's
  -- shuffle-even-if-nothing-moved clause riding on it.
  Spec.it s "Riftsweeper is a {1}{G} 2/2 Elf Shaman shuffling an exiled card into its owner's library" $ do
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    let c = Printing.card riftsweeper
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith
      s
      "cost"
      (Card.Type.manaCost c)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Green)]))
    Spec.assertEqWith
      s
      "an Elf Shaman (CR 205.3m)"
      (TypeLine.subtypes (Card.Type.typeLine c))
      (Set.fromList [Subtype.Elf, Subtype.Shaman])
    Spec.assertEqWith s "power" (Card.Type.power c) (Just (Power.MkPower (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "toughness" (Card.Type.toughness c) (Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
    Spec.assertEqWith s "the spell itself does nothing (a vanilla creature spell)" (Card.allEffects c) []
    case Card.Type.triggeredAbilities c of
      [ability] -> do
        Spec.assertEqWith
          s
          "CR 603.6a: it triggers on its own entry"
          (TriggeredAbility.condition ability)
          TriggerCondition.SelfEnters
        Spec.assertEqWith
          s
          "which shuffles the target into its owner's library (CR 701.24)"
          (Modal.allEffects (TriggeredAbility.modal ability))
          [Effect.ShuffleIntoLibrary target]
        Spec.assertEqWith
          s
          "off one unfiltered, scopeless exile slot"
          (Modal.allTargetSpecs (TriggeredAbility.modal ability))
          (Map.singleton target (TargetSpec.MkTargetSpec Pool.CardsInExile Nothing))
      abilities -> Spec.assertFailure s ("expected one triggered ability, got " <> show (length abilities))
  -- Three modifications on ONE target, in printed order. Spelled out rather
  -- than spot-checked because the toxic 1 grant is what makes this card the
  -- CR 702.164b proof in DamageSpec: a card that granted toxic 2 by mistake
  -- would still add up to the poison that test expects.
  Spec.it s "Aspirant's Ascent is a {U} Instant granting +1/+3, flying and toxic 1" $ do
    ascent <- S.printingOf s registry "Aspirant's Ascent"
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
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let c = Printing.card angelicEdict
    Spec.assertBool s (not (Card.isInstant c)) "not an instant"
    Spec.assertEqWith s "effect exiles" (Card.allEffects c) [Effect.MoveToZone (SlotName.MkSlotName (Text.pack "target")) Zone.Exile EntryRiders.defaultValue Nothing]
    Spec.assertEqWith s "creature-or-enchantment slot" (Card.allTargetSpecs c) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment]))))
  Spec.it s "Divination is a {2}{U} Sorcery that draws two cards with no target" $ do
    divination <- S.printingOf s registry "Divination"
    let c = Printing.card divination
    Spec.assertEqWith s "effect draws two" (Card.allEffects c) [Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 2)]
    Spec.assertBool s (Map.null (Card.allTargetSpecs c)) "no target slots"
  Spec.it s "Tome Scour is a {U} Sorcery milling five from a target player" $ do
    tomeScour <- S.printingOf s registry "Tome Scour"
    let c = Printing.card tomeScour
    Spec.assertEqWith s "effect mills five" (Card.allEffects c) [Effect.Mill (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 5)]
    Spec.assertEqWith s "one PlayerTarget slot" (Card.allTargetSpecs c) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players Nothing))
  Spec.it s "Mind Rot is a {2}{B} Sorcery making a target player discard two" $ do
    mindRot <- S.printingOf s registry "Mind Rot"
    let c = Printing.card mindRot
    Spec.assertEqWith s "effect discards two" (Card.allEffects c) [Effect.Discard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Type.Literal 2)]
    Spec.assertEqWith s "one PlayerTarget slot" (Card.allTargetSpecs c) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Players Nothing))
  -- Two effects of DIFFERENT opcodes reading one slot, in printed order --
  -- the pin that a rewrite reordering the mode's effect list, or splitting
  -- the clauses across two slots, would break.
  Spec.it s "Sign in Blood is a {B}{B} Sorcery drawing two and making one target player lose two life" $ do
    signInBlood <- S.printingOf s registry "Sign in Blood"
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
    burningTreeEmissary <- S.printingOf s registry "Burning-Tree Emissary"
    Spec.assertEqWith s "two" (Quantity.manaValueOf (Printing.card burningTreeEmissary)) 2
  Spec.it s "Flame Javelin is a {2/R}{2/R}{2/R} Instant dealing 4 to any target" $ do
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let c = Printing.card flameJavelin
        twoOrRed = ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Red)
        target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Flame Javelin")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [twoOrRed, twoOrRed, twoOrRed])
    Spec.assertBool s (Card.isInstant c) "an instant"
    Spec.assertEqWith s "effect deals four" (Card.allEffects c) [Effect.DealDamage (ObjectRef.InSlot target) (Quantity.Type.Literal 4)]
    Spec.assertEqWith s "one AnyTarget slot" (Card.allTargetSpecs c) (Map.singleton target (TargetSpec.MkTargetSpec Pool.AnyTarget Nothing))
  -- CR 202.3f again, whose own worked example is this card's cost: "The mana
  -- value of a card with mana cost {2/B}{2/B}{2/B} is 6." The generic half is
  -- the larger one, so a monocolored hybrid contributes 2 and not the 1 every
  -- other typed symbol contributes -- the detail that silently corrupts every
  -- mana-value reading downstream if it is wrong.
  Spec.it s "Flame Javelin's three monocolored hybrid symbols make mana value 6, not 3" $ do
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    Spec.assertEqWith s "six" (Quantity.manaValueOf (Printing.card flameJavelin)) 6
  -- CR 202.3 with no subrule of its own to lean on: "a number equal to the total
  -- amount of mana in its mana cost, regardless of color", and CR 107.4h makes
  -- {S} "a cost that can be paid with one mana of any type produced by a snow
  -- source" -- one mana, so 1. Not 0, which is what a symbol mistaken for a
  -- colour marker rather than a mana would give.
  Spec.it s "CR 202.3 Icehide Golem's snow mana symbol makes mana value 1" $ do
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    Spec.assertEqWith s "one" (Quantity.manaValueOf (Printing.card icehideGolem)) 1

m45p6CardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m45p6CardSpec s registry = Spec.describe s "M45p6Cards" $ do
  Spec.it s "Master Thief is a {2}{U}{U} 2/2 Human Rogue whose ETB steals an artifact" $ do
    masterThief <- S.printingOf s registry "Master Thief"
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
    hagOfInnerWeakness <- S.printingOf s registry "Hag of Inner Weakness"
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

m45p7CardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m45p7CardSpec s registry = Spec.describe s "M4.5 P7" $ do
  Spec.it s "Rule of Law is a {2}{W} enchantment with one EachPlayer CantCastMoreThan 1 player ability" $ do
    ruleOfLaw <- S.printingOf s registry "Rule of Law"
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
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
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
    sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
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
    edgewalker <- S.printingOf s registry "Edgewalker"
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
    reliquaryTower <- S.printingOf s registry "Reliquary Tower"
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
    radiantFountain <- S.printingOf s registry "Radiant Fountain"
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
    silence <- S.printingOf s registry "Silence"
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

m45p11CardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
m45p11CardSpec s registry = Spec.describe s "M4.5 P11" $ do
  Spec.it s "Palace Jailer is a {2}{W}{W} 2/2 Human Soldier with two ETB triggers" $ do
    palaceJailer <- S.printingOf s registry "Palace Jailer"
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
m55CardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
m55CardSpec s registry = Spec.describe s "M5.5" $ do
  Spec.it s "Barbarian Outcast's state trigger is a Count of exactly 0 Swamps you control (CR 603.8)" $ do
    barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
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
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
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
auraCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auraCardSpec s registry = Spec.describe s "Auras" $ do
  Spec.it s "Unholy Strength is a {B} Aura enchanting a creature for +2/+1" $ do
    p <- S.printingOf s registry "Unholy Strength"
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
    p <- S.printingOf s registry "Lure"
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
    p <- S.printingOf s registry "Crown of the Ages"
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
    p <- S.printingOf s registry "Curse of Death's Hold"
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
    p <- S.printingOf s registry "Aura Graft"
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
-- -- every printed enchantment animator excludes Auras by name; March of the
-- Machines animates every noncreature artifact at once, which is the card CR
-- 613.6 exists for (#233); and Titania's Song is March with a layer-6 ability
-- removal bolted on, which is what makes CR 613.6's LOWEST layer observable
-- (#326).
animatorCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
animatorCardSpec s registry = Spec.describe s "Animators" $ do
  -- The CR 613.6 gate card (#233), and the pin that matters is the SHAPE: one
  -- static ability with three parts, not three abilities. Its affected set
  -- reads a card type its own layer-4 part changes, so the parts have to stay
  -- one effect or the layer-7b part loses the set.
  Spec.it s "March of the Machines is a {3}{U} enchantment: ONE ability, three parts, one affected set" $ do
    p <- S.printingOf s registry "March of the Machines"
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
    humility <- S.printingOf s registry "Humility"
    opalescence <- S.printingOf s registry "Opalescence"
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
  -- March of the Machines' set, plus the layer-6 removal March does not have --
  -- and the pin that matters is that the four parts are ONE ability. That is what
  -- makes layer 4 the whole effect's CR 613.6 decision point and so the layer its
  -- affected set is fixed at, which is the card #326 exists for. Split into two
  -- abilities the removal would decide its own set at layer 6, by which time this
  -- card's own layer-4 part has animated the artifact out of "each noncreature
  -- artifact".
  --
  -- The printed second sentence ("If this enchantment leaves the battlefield,
  -- this effect continues until end of turn") is not modelled (#511).
  Spec.it s "Titania's Song is a {3}{G} enchantment: ONE ability, four parts, layers 4, 6 and 7b" $ do
    p <- S.printingOf s registry "Titania's Song"
    let c = Printing.card p
        green = ManaSymbol.OfType (ManaType.Colored Color.Green)
        noncreatureArtifact =
          Affected.Matching
            ( Filter.Type.And
                [ Filter.Type.HasCardType CardType.Artifact,
                  Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)
                ]
            )
    Spec.assertEqWith s "name" (Card.Type.name c) (Text.pack "Titania's Song")
    Spec.assertEqWith s "cost" (Card.Type.manaCost c) (costOf [ManaSymbol.Generic 3, green])
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine c)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith
      s
      "\"loses all abilities and becomes an artifact creature with power and toughness each equal to its mana value\""
      (Card.Type.staticAbilities c)
      [ StaticAbility.MkStaticAbility
          noncreatureArtifact
          ( Modification.LoseAllAbilities
              NonEmpty.:| [ Modification.AddCardType CardType.Artifact,
                            Modification.AddCardType CardType.Creature,
                            Modification.SetBasePowerToughness Quantity.Type.ManaValue Quantity.Type.ManaValue
                          ]
          )
      ]
  Spec.it s "Liquimetal Coating is a {2} artifact whose {T} ability makes any permanent an artifact" $ do
    p <- S.printingOf s registry "Liquimetal Coating"
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
    p <- S.printingOf s registry "Skilled Animator"
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
cyclingCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cyclingCardSpec s registry = Spec.describe s "Cycling" $ do
  Spec.it s "Windcaller Aven is a {4}{U}{U} 4/3 with flying, Cycling {U} and a cycling trigger" $ do
    p <- S.printingOf s registry "Windcaller Aven"
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
    p <- S.printingOf s registry "Ash Barrens"
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
    p <- S.printingOf s registry "Barkhide Mauler"
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
worldCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
worldCardSpec s registry = Spec.describe s "WorldEnchantments" $ do
  Spec.it s "Concordant Crossroads is a {G} world enchantment giving all creatures haste" $ do
    p <- S.printingOf s registry "Concordant Crossroads"
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
    p <- S.printingOf s registry "Living Plane"
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
revealCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
revealCardSpec s registry = Spec.describe s "Reveal" $ do
  Spec.it s "Braidwood Sextant is a {1} Artifact whose {2}, {T}, Sacrifice fetches a revealed basic land" $ do
    p <- S.printingOf s registry "Braidwood Sextant"
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
entersCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
entersCardSpec s registry = Spec.describe s "Enters" $ do
  Spec.it s "Soul Warden is a {W} 1/1 Human Cleric whose trigger reads \"whenever ANOTHER creature enters\"" $ do
    p <- S.printingOf s registry "Soul Warden"
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

unspentManaCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
unspentManaCardSpec s registry = Spec.describe s "Unspent mana" $ do
  -- The modern Oracle wording is "don't LOSE unspent mana", CR 106.4's verb,
  -- not "mana pools don't empty" -- and it is symmetric, which is why the
  -- scope is EachPlayer and the effect needs no mana-type argument.
  Spec.it s "Upwelling is a {3}{G} Enchantment with one EachPlayer DontLoseUnspentMana ability" $ do
    p <- S.printingOf s registry "Upwelling"
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

phyrexianCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianCardSpec s registry = Spec.describe s "Phyrexian" $ do
  Spec.it s "Mutagenic Growth is a {G/P} Instant giving target creature +2/+2" $ do
    p <- S.printingOf s registry "Mutagenic Growth"
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
    p <- S.printingOf s registry "Mutagenic Growth"
    Spec.assertEqWith s "one" (Quantity.manaValueOf (Printing.card p)) 1

-- CR 506.4's "an effect specifically removes it from combat", as printed.
-- Labyrinth of Skophos is a Land with two activated abilities and no mana cost:
-- "{T}: Add {C}. / {4}, {T}: Remove target attacking or blocking creature from
-- combat." (Murders at Karlov Manor Commander; oracle text checked against
-- Scryfall.) The gameplay proof is Pawl.CombatSpec's EffectRemoval group.
removeFromCombatCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
removeFromCombatCardSpec s registry = Spec.describe s "RemoveFromCombat" $ do
  Spec.it s "Labyrinth of Skophos is a Land with a {T} colorless mana ability and a {4}, {T} removal ability" $ do
    labyrinth <- S.printingOf s registry "Labyrinth of Skophos"
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
blockRequirementCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
blockRequirementCardSpec s registry = Spec.describe s "BlockRequirements" $ do
  Spec.it s "Prized Unicorn is a {3}{G} 2/2 Unicorn whose only ability is a requirement naming ITSELF" $ do
    p <- S.printingOf s registry "Prized Unicorn"
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
attackRequirementCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
attackRequirementCardSpec s registry = Spec.describe s "AttackRequirements" $ do
  Spec.it s "Curse of the Nightly Hunt is a {2}{R} Aura Curse whose only ability is a CR 508.1d attacking requirement" $ do
    p <- S.printingOf s registry "Curse of the Nightly Hunt"
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

-- CR 508.1c and CR 509.1b's combat restrictions. Pacifism is a {1}{W} Enchantment
-- -- Aura reading "Enchant creature. Enchanted creature can't attack or block."
-- (Marvel Super Heroes Commander; name, cost, type line and oracle text checked
-- against Scryfall.) Its shape is the point next to Lure's and Curse of the
-- Nightly Hunt's: the same enchant-creature Aura, naming the same Affected, on a
-- field that says the creature may NOT act where theirs say it must. The gameplay
-- proof is Pawl.CombatSpec's CombatRestrictions group.
combatRestrictionCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
combatRestrictionCardSpec s registry = Spec.describe s "CombatRestrictions" $ do
  Spec.it s "Pacifism is a {1}{W} Aura whose only ability is a pair of CR 508.1c/509.1b restrictions" $ do
    p <- S.printingOf s registry "Pacifism"
    let card = Printing.card p
        white = ManaSymbol.OfType (ManaType.Colored Color.White)
    Spec.assertEqWith s "name" (Card.Type.name card) (Text.pack "Pacifism")
    Spec.assertEqWith s "cost" (Card.Type.manaCost card) (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white]))
    Spec.assertEqWith s "types" (TypeLine.types (Card.Type.typeLine card)) (Set.singleton CardType.Enchantment)
    Spec.assertEqWith s "subtypes" (TypeLine.subtypes (Card.Type.typeLine card)) (Set.singleton Subtype.Aura)
    Spec.assertBool s (Card.isAura card) "is an Aura"
    Spec.assertEqWith s "enchant creature" (Card.Type.enchant card) (Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing))
    -- "CAN'T ATTACK OR BLOCK" is TWO restrictions on one line, in printed order.
    -- Both name Affected.Attached (CR 303.4m), which is Lure's own Affected --
    -- same set, opposite polarity, different field.
    Spec.assertEqWith
      s
      "two restrictions, both naming whatever the Aura is attached to"
      (Card.Type.combatRestrictions card)
      [ CombatRestriction.CantAttack Affected.Attached,
        CombatRestriction.CantBlock Affected.Attached
      ]
    -- The field is what says this changes no CHARACTERISTIC: a CR 613.1 layer
    -- computes an object's characteristics, and "can't attack" is not one.
    Spec.assertEqWith s "and it modifies no characteristic" (Card.Type.staticAbilities card) []
    -- The opposite polarity is ABSENT, which is what keeps the assertion above
    -- from passing against a carrier that confused the two: this card requires
    -- nothing of anyone.
    Spec.assertEqWith s "and requires no attack" (Card.Type.attackRequirements card) []
    Spec.assertEqWith s "and requires no block" (Card.Type.blockRequirements card) []
    -- CR 303.4: an Aura spell has no spell effects; it enters attached.
    Spec.assertEqWith s "no spell effects" (Card.allEffects card) []

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
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
  combatRestrictionCardSpec s registry
