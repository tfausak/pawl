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
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.Face as Face.Codec
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
-- Dotted, because Pawl.Types.Keyword already holds the short alias here (the
-- reverse of TriggerSpec's split).
import qualified Pawl.Engine.Keyword as Keyword.Engine
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

import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LimitUnless as LimitUnless
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.UntapRestriction as UntapRestriction
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR
import qualified System.Directory as Directory

-- Not red-specific despite its first callers: just the Maybe wrapper every
-- printed mana cost needs (CR 202.1).
costOf :: [ManaSymbol.ManaSymbol] -> Maybe ManaCost.ManaCost
costOf symbols = Just (ManaCost.MkManaCost symbols)

-- A face carrying nothing but a name and a type line, every other field at the
-- value an omitted key decodes to. The cases below are about one field apiece,
-- and a 28-field literal apiece would bury which one.
vanillaFace :: String -> TypeLine.TypeLine -> Face.Face Card.Type.Card
vanillaFace name typeLine =
  Face.MkFace
    { Face.name = CardName.MkCardName $ Text.pack name,
      Face.manaCost = Nothing,
      Face.typeLine = typeLine,
      Face.power = Nothing,
      Face.toughness = Nothing,
      Face.loyalty = Nothing,
      Face.defense = Nothing,
      Face.keywords = Set.empty,
      Face.colorIndicator = Set.empty,
      Face.staticAbilities = [],
      Face.spell = Face.defaultSpell,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.delayedAbilities = Map.empty,
      Face.rooms = Seq.empty,
      Face.castingPermissions = [],
      Face.castingRestrictions = [],
      Face.characteristicPT = Nothing,
      Face.playerAbilities = [],
      Face.blockRequirements = [],
      Face.blockPermissions = [],
      Face.attackRequirements = [],
      Face.combatRestrictions = [],
      Face.sacrificeRestrictions = [],
      Face.untapRestrictions = [],
      Face.attackCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = [],
      Face.specialActions = [],
      Face.additionalCosts = [],
      Face.alternativeCosts = [],
      Face.enchant = [],
      Face.counterability = Counterability.Counterable
    }

-- A spell's printed type line: one card type, plus whatever supertypes and
-- subtypes sit beside it.
spellLine :: CardType.CardType -> Set.Set Supertype.Supertype -> Set.Set Subtype.Subtype -> TypeLine.TypeLine
spellLine cardType supertypes subtypes =
  TypeLine.MkTypeLine
    { TypeLine.supertypes = supertypes,
      TypeLine.types = Set.singleton cardType,
      TypeLine.subtypes = subtypes
    }

instantLine :: TypeLine.TypeLine
instantLine = spellLine CardType.Instant Set.empty Set.empty

-- "You draw this many cards" -- the smallest payload an ability can carry, used
-- below only so that two abilities can be told apart by their effect.
youDraw :: Integer -> Effect.Effect Card.Type.Card
youDraw n = Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal n))

-- "This object has [keyword]" as a static ability (CR 604.1), the smallest
-- carrier Face.staticAbilities takes.
grantsItself :: Keyword.Keyword -> StaticAbility.StaticAbility
grantsItself keyword =
  StaticAbility.MkStaticAbility
    (Affected.Matching Filter.Type.IsSource)
    Nothing
    Nothing
    (NonEmpty.singleton (Modification.GainKeyword keyword))

-- CR 709.4's fixture: two halves that DIFFER on every axis Pawl.Engine.Card.merge2
-- unions, so a merge that dropped a union line -- or took one half's value alone
-- -- fails the group below instead of passing on a value the halves happened to
-- share. Wax is a green Arcane instant, Wane a white snow Trap sorcery.
--
-- Everything past each half's name and mana cost is there to make one merge line
-- observable, and is deliberately INERT rather than plausible: a combat keyword
-- on a spell; a static ability on a card no projection ever gathers, since
-- Pawl.Engine.Projection.gather walks the battlefield and this card only ever
-- reaches a hand or the stack; and a dies trigger with no permanent to die. What
-- is under test is CR 709.4c's union, not what either printing would mean.
--
-- Built by hand for the faceNamed fixture's reason, which the printed Wax // Wane
-- does not retire: this group takes no Registry, so a card file is not reachable
-- from here at all. And the printed card could not stand in even if it were --
-- its two halves are vanilla instants that share every field but name and cost,
-- which is exactly the shape that leaves the union lines unexercised.
splitCard :: Card.Type.Card
splitCard =
  Card.Type.MkCard
    { Card.Type.layout = Layout.Split,
      Card.Type.faces = waxFace NonEmpty.:| [waneFace]
    }

-- splitCard's left half, and the one CastSpec casts: {G} buys it with a single
-- Forest, which is what makes CR 709.3a's per-half pricing observable there.
waxFace :: Face.Face Card.Type.Card
waxFace =
  (vanillaFace "Wax" (spellLine CardType.Instant Set.empty (Set.singleton Subtype.Arcane)))
    { Face.manaCost = costOf [ManaSymbol.OfType (ManaType.Colored Color.Green)],
      Face.keywords = Set.singleton Keyword.Flying,
      Face.staticAbilities = [grantsItself Keyword.Flying],
      Face.activatedAbilities = [oneEffectActivated (costOf []) (youDraw 1)],
      Face.triggeredAbilities = [oneEffectTrigger TriggerCondition.SelfDies (youDraw 1)]
    }

-- Wax's opposite number, differing on every field Wax prints: a different card
-- type, subtype and keyword, a supertype Wax has none of, and abilities that draw
-- a different number of cards so the concatenation's ORDER is checkable too.
waneFace :: Face.Face Card.Type.Card
waneFace =
  (vanillaFace "Wane" (spellLine CardType.Sorcery (Set.singleton Supertype.Snow) (Set.singleton Subtype.Trap)))
    { Face.manaCost = costOf [ManaSymbol.OfType (ManaType.Colored Color.White)],
      Face.keywords = Set.singleton Keyword.Trample,
      Face.staticAbilities = [grantsItself Keyword.Trample],
      Face.activatedAbilities = [oneEffectActivated (costOf []) (youDraw 2)],
      Face.triggeredAbilities = [oneEffectTrigger TriggerCondition.SelfDies (youDraw 2)]
    }

cardSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
cardSpec s = Spec.describe s "Card" $ do
  Spec.it s "CR 110.1 an instant is not a permanent type" $
    let face = vanillaFace "Some Instant" instantLine
     in do
          Spec.assertBool s (not (Card.isPermanent face)) "not a permanent"
          Spec.assertBool s (Card.isInstant face) "an instant"
  -- CR 709.4a: a card's faces are referred to BY NAME, so this is what a rule or
  -- a player naming one half resolves through. Built by hand rather than loaded
  -- because this group takes no Registry, and the fixture is Normal on purpose:
  -- faceNamed reads Card.faces and never Card.layout, which a Split fixture
  -- could not distinguish from a lookup that went through the layout first.
  Spec.it s "CR 709.4a faceNamed finds a two-faced card's faces by their own names" $
    let wax = vanillaFace "Wax" instantLine
        wane = vanillaFace "Wane" instantLine
        card =
          Card.Type.MkCard
            { Card.Type.layout = Layout.Normal,
              Card.Type.faces = wax NonEmpty.:| [wane]
            }
        named = CardName.MkCardName . Text.pack
     in do
          Spec.assertEqWith s "the first face" (Card.faceNamed (named "Wax") card) (Just wax)
          -- The one that matters: a hit on the SECOND face is what says this
          -- reads past Card.combined rather than through it.
          Spec.assertEqWith s "the second face" (Card.faceNamed (named "Wane") card) (Just wane)
          -- CR 709.4a again: a split card has two names and no combined one, so
          -- the joined name is not a face name and must not resolve to a face.
          Spec.assertEqWith s "a name no face carries" (Card.faceNamed (named "Wax // Wane") card) Nothing
  Spec.it s "CR 709.4 a split card's characteristics are its two halves combined" $ do
    let card = splitCard
        c = Card.combined card
    -- CR 709.4b: "The mana cost of a split card is the combined mana costs of
    -- its two halves. A split card's colors and mana value are determined from
    -- its combined mana cost."
    Spec.assertEqWith s "both colours" (Projection.printedColorsOf c) (Set.fromList [Color.Green, Color.White])
    Spec.assertEqWith s "mana value 2" (Quantity.manaValueOf c) 2
    -- The two names RENDERED as one string, unspaced -- the form
    -- docs/rules.txt's own Examples write it in ("Fire//Ice",
    -- "Assault//Battery"). What the card HAS is both names, which is a set and
    -- is asserted through the projection (Pawl.CastSpec's "in hand, the
    -- combined view has both names").
    Spec.assertEqWith s "the joined name" (Face.name c) (CardName.MkCardName (Text.pack "Wax//Wane"))
    -- CR 709.4c: "A split card has each card type specified on either of its
    -- halves and each ability in the text box of each half." Both halves
    -- contribute, and they contribute DIFFERENT values -- a merge that kept one
    -- half's type line, keyword set or ability list fails each of these.
    Spec.assertEqWith s "each card type" (TypeLine.types (Face.typeLine c)) (Set.fromList [CardType.Instant, CardType.Sorcery])
    -- CR 709.4: the rest of the type line is combined for the reason 709.4c's
    -- card types are -- a supertype and a subtype are characteristics (CR 109.3)
    -- and no subrule narrows them. See Pawl.Engine.Card.unionTypeLines.
    Spec.assertEqWith s "each subtype" (TypeLine.subtypes (Face.typeLine c)) (Set.fromList [Subtype.Arcane, Subtype.Trap])
    -- One-sided on purpose: only Wane is snow. A merge line replaced by the LEFT
    -- half's value -- which is what a record update over `l` already does -- would
    -- leave this empty.
    Spec.assertEqWith s "the right half's supertype" (TypeLine.supertypes (Face.typeLine c)) (Set.singleton Supertype.Snow)
    -- CR 709.4c again: a keyword is the printed NAME of an ability (CR 702.1).
    Spec.assertEqWith s "each keyword" (Face.keywords c) (Set.fromList [Keyword.Flying, Keyword.Trample])
    -- The three ability lists CR 709.4c's "each ability in the text box of each
    -- half" reaches, each asserted in PRINTED order (left half then right), so a
    -- merge that concatenated the halves the other way round fails too.
    Spec.assertEqWith
      s
      "each static ability"
      (Face.staticAbilities c)
      [grantsItself Keyword.Flying, grantsItself Keyword.Trample]
    Spec.assertEqWith
      s
      "each activated ability"
      (Face.activatedAbilities c)
      [oneEffectActivated (costOf []) (youDraw 1), oneEffectActivated (costOf []) (youDraw 2)]
    Spec.assertEqWith
      s
      "each triggered ability"
      (Face.triggeredAbilities c)
      [oneEffectTrigger TriggerCondition.SelfDies (youDraw 1), oneEffectTrigger TriggerCondition.SelfDies (youDraw 2)]

-- Every Count reachable from a Quantity: a leaf Count directly, or one nested
-- through Plus's two children (CR 208.2 composition -- a printed 1+*) or
-- Negate's one.
quantityCounts :: Quantity.Type.Quantity -> [Count.Type.Count Quantity.Type.Quantity]
quantityCounts quantity = case quantity of
  Quantity.Type.Literal _ -> []
  Quantity.Type.ManaValue -> []
  Quantity.Type.Power -> []
  Quantity.Type.Toughness -> []
  -- A slot read, not a fold over game state: the value was bound by an earlier
  -- effect of the same resolution and there is no Count inside it.
  Quantity.Type.InSlot _ -> []
  Quantity.Type.Star -> []
  Quantity.Type.Plus (Plus.MkPlus a b) -> quantityCounts a <> quantityCounts b
  -- Plus' descent: CR 107.1a's rounding holds no Count, and the payload it
  -- halves may be one -- Malignus halves a fold over players.
  Quantity.Type.Halved (Halved.MkHalved _ inner) -> quantityCounts inner
  -- Not a leaf: a minus sign hides nothing, so the lints reach through it --
  -- Toxic Deluge's -X, and any negated count a card comes to print.
  Quantity.Type.Negate a -> quantityCounts a
  Quantity.Type.Count count -> count : countCounts count
  -- A fold over a MANA POOL (CR 106.4), not over a zone: it holds no
  -- Pawl.Types.Count and no Pawl.Types.Filter, so the lints below -- which are
  -- about the Filters a card authors -- have nothing to sweep here. See
  -- Pawl.Types.ManaCount.
  Quantity.Type.ManaCount _ -> []
  -- CR 119.1's scalar attached to a PLAYER: it holds neither a Pawl.Types.Count
  -- nor a Pawl.Types.Filter, so these lints have nothing to sweep here either.
  Quantity.Type.LifeTotal _ -> []
  Quantity.Type.Speed _ -> []
  -- CR 725.1's game-wide player designation, read as a 0/1: a PlayerRef and
  -- nothing else, so no Count and no Filter here either.
  Quantity.Type.IsMonarch _ -> []
  Quantity.Type.HasDesignation _ -> []
  Quantity.Type.WasKicked -> []
  -- CR 122.1's per-player counter tally, another such scalar.
  Quantity.Type.PlayerCounters {} -> []
  -- CR 122.1's per-OBJECT tally, read off the object the quantity is evaluated
  -- against: a bare CounterKind with no Count and no Filter beside it.
  Quantity.Type.ObjectCounters _ -> []
  -- CR 508.3b's combat record, read as a tally of players: a PlayerRef and
  -- nothing else, so no Count and no Filter here either.
  Quantity.Type.OpponentsAttacked _ -> []
  -- CR 701.9a's tally of logged discards: a PlayerRef and nothing else, so no
  -- Count and no Filter here either.
  Quantity.Type.CardsDiscardedThisTurn _ -> []
  -- CR 120.1's tally of logged damage: a PlayerRef and nothing else, so no Count
  -- and no Filter here either.
  Quantity.Type.PlayersDealtDamageThisTurn _ -> []
  -- CR 400.7's logged entry, read against the object the quantity is aimed at: no
  -- reference at all, so no Count and no Filter here either.
  Quantity.Type.EnteredThisTurn -> []
  -- CR 509.1h's declaration, read against the object the quantity is evaluated
  -- against: a nullary leaf, so no Count and no Filter here either.
  Quantity.Type.BlockersBeyondFirst -> []
  -- Not a leaf: aiming the evaluation at another object does not stop the payload
  -- from being a Count, so the Filter lints must reach through it.
  Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot _ inner) -> quantityCounts inner

-- Every Count nested inside another Count's AGGREGATION: only Greatest carries
-- a per-member Quantity, and that Quantity may itself be a Count. Without this
-- descent the shared-zone lint below would sweep past a misauthored inner
-- scope.
countCounts :: Count.Type.Count Quantity.Type.Quantity -> [Count.Type.Count Quantity.Type.Quantity]
countCounts count = case Count.Type.aggregation count of
  Aggregation.Members -> []
  Aggregation.DistinctCardTypes -> []
  Aggregation.Greatest quantity -> quantityCounts quantity

-- Every Count reachable from a Condition: both sides of a comparison are
-- Quantities and either may embed one, and a disjunction holds more conditions
-- (Pawl.Types.Condition).
conditionCounts :: Condition.Type.Condition -> [Count.Type.Count Quantity.Type.Quantity]
conditionCounts condition = case condition of
  Condition.Type.Compares (Compares.MkCompares measured _ threshold) ->
    quantityCounts measured <> quantityCounts threshold
  Condition.Type.Any conditions -> concatMap conditionCounts conditions

-- CR 701.46a's per-clause gate. Mode.allEffects and Modal.allEffects drop clause
-- boundaries by design, so every lint that reaches a card through them needs
-- this beside it, or the gate's Counts -- and through them its Filters -- go
-- unswept.
modeClauseConditions :: Mode.Mode Card.Type.Card -> [Condition.Type.Condition]
modeClauseConditions = Maybe.mapMaybe Clause.condition . Foldable.toList . Mode.clauses

modalClauseConditions :: Modal.Modal Card.Type.Card -> [Condition.Type.Condition]
modalClauseConditions = concatMap modeClauseConditions . Modal.modes

-- Every Count reachable from a Duration: only ForAsLongAs (CR 611.2b) carries
-- a Condition.
durationCounts :: Duration.Duration -> [Count.Type.Count Quantity.Type.Quantity]
durationCounts duration = case duration of
  Duration.UntilEndOfTurn -> []
  Duration.Indefinite -> []
  Duration.UntilYourNextTurn -> []
  Duration.UntilEndOfYourNextTurn -> []
  Duration.ForAsLongAs condition -> conditionCounts condition
  Duration.UntilEndOfCombat -> []

-- Every Count reachable from a Modification: only its P/T quantities
-- (layers 7b/7c) carry one.
modificationCounts :: Modification.Modification -> [Count.Type.Count Quantity.Type.Quantity]
modificationCounts modification = case modification of
  Modification.GainKeyword _ -> []
  Modification.LoseAllAbilities -> []
  Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) -> quantityCounts p <> quantityCounts t
  Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) -> quantityCounts p <> quantityCounts t
  Modification.SetLandSubtype _ -> []
  Modification.SetLandSubtypeToChosen -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCreatureSubtype _ -> []
  Modification.AddEveryCreatureSubtype -> []
  Modification.AddCardType _ -> []
  Modification.AddSupertype _ -> []
  Modification.RemoveSupertype _ -> []
  Modification.ChangeSubtypeWord {} -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.AddColor _ -> []
  Modification.AddChosenColor -> []
  Modification.SwitchPowerToughness -> []

-- Every Count reachable from a StaticAbility: its modifications' P/T quantities,
-- plus CR 604.2's "as long as" gate, which is a Condition and so a pair of
-- Quantities -- and the leaves-the-battlefield duration beside it, which is a
-- Duration and so another Condition when it is a CR 611.2b "for as long as".
staticAbilityCounts :: StaticAbility.StaticAbility -> [Count.Type.Count Quantity.Type.Quantity]
staticAbilityCounts ability =
  concatMap conditionCounts (Maybe.maybeToList (StaticAbility.condition ability))
    <> concatMap durationCounts (Maybe.maybeToList (StaticAbility.lingers ability))
    <> concatMap modificationCounts (StaticAbility.modifications ability)

-- Every Count reachable from a TriggerCondition: only StateIs (CR 603.8, a
-- trigger's own condition) carries one.
triggerConditionCounts :: TriggerCondition.TriggerCondition -> [Count.Type.Count Quantity.Type.Quantity]
triggerConditionCounts triggerCondition = case triggerCondition of
  TriggerCondition.SelfEnters -> []
  -- CR 709.5h names a half, which is a CardName and not a Quantity.
  TriggerCondition.SelfHalfUnlocked _ -> []
  -- CR 709.5i names a PlayerRelation, which holds no Count.
  TriggerCondition.RoomFullyUnlocked _ -> []
  -- Recursive: a branch of an AnyOf may be any condition, StateIs included, so
  -- the traversal has to go through rather than stop here.
  TriggerCondition.AnyOf conditions -> concatMap triggerConditionCounts conditions
  -- CR 708.7's condition is nullary, so there is nothing in it to be a Quantity.
  TriggerCondition.SelfTurnedFaceUp -> []
  -- Its watcher-scoped sibling carries a Filter, and a Filter holds no Count for
  -- PermanentEnters' reason.
  TriggerCondition.PermanentTurnedFaceUp _ -> []
  -- CR 702.112b's condition carries a Filter for the same reason, and no Count.
  TriggerCondition.PermanentBecomesDesignated {} -> []
  TriggerCondition.SelfEvolves -> []
  -- CR 702.134c's is nullary too, so it holds no Quantity.
  TriggerCondition.AttachedCreatureMentors -> []
  -- CR 701.21a's is nullary too, so it holds no Quantity either.
  TriggerCondition.PermanentSacrificed -> []
  -- CR 603.3b's carries a PlayerRelation, which holds no Count.
  TriggerCondition.SagaFinalChapterTriggers _ -> []
  -- CR 603.6a's Filter is a predicate over the entering permanent, and a
  -- Filter holds no Count (Pawl.Types.Filter's atoms are all characteristics).
  TriggerCondition.PermanentEnters _ -> []
  TriggerCondition.PermanentDies _ -> []
  TriggerCondition.StepBegins {} -> []
  TriggerCondition.StateIs condition -> conditionCounts condition
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  -- Its watcher-scoped sibling carries a Filter, and a Filter holds no Count.
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> []
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []
  TriggerCondition.OpponentLostLifeDuringYourTurn -> []
  TriggerCondition.SelfAttacks _ -> []
  -- CR 702.149a's Filter holds no Count for PermanentEnters' reason.
  TriggerCondition.SelfAttacksWithAnother _ -> []
  TriggerCondition.CreatureAttacksAlone _ -> []
  -- CR 702.105a compares life totals rather than counting objects, so no Count.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> []
  TriggerCondition.SelfBlocks -> []
  -- CR 509.3b names the attacker without counting anything, so no Count either.
  TriggerCondition.SelfBlocksCreature -> []
  TriggerCondition.SelfBlocksAtLeast _ -> []
  -- CR 509.3e's filtered form spends the number on a quality instead, so its
  -- Filter holds no Count either.
  TriggerCondition.SelfBlocksOneOrMore _ -> []
  TriggerCondition.SelfBecomesBlocked -> []
  -- CR 509.3d's Filter is a predicate over the blocker, and holds no Count for
  -- PermanentEnters' reason.
  TriggerCondition.SelfBecomesBlockedBy _ -> []
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> []
  TriggerCondition.SelfAttacksUnblocked -> []
  TriggerCondition.SelfCycled -> []
  TriggerCondition.SelfRevealedForMiracle -> []
  -- CR 701.9a's discard condition is a PlayerRelation, which holds no Count.
  TriggerCondition.PlayerDiscards _ -> []
  TriggerCondition.PlayerDrawsNthCard {} -> []
  -- CR 725.1's crowning condition is a PlayerRelation too.
  TriggerCondition.PlayerBecomesMonarch _ -> []
  -- CR 603.7's slot-named condition holds a SlotName, which is no Count.
  TriggerCondition.LoseControlOfBound _ -> []
  TriggerCondition.RoomEntered _ -> []
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> []
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> []
  TriggerCondition.SelfDies -> []
  TriggerCondition.SelfLeavesTheBattlefield -> []
  TriggerCondition.HauntedCreatureDies -> []
  -- CR 701.6a's countering condition is a PlayerRelation, which holds no Count,
  -- exactly as the discard condition above.
  TriggerCondition.SpellOrAbilityCounters _ -> []
  -- CR 615.13's prevention condition is a PlayerRelation too.
  TriggerCondition.DamageToPlayerPrevented _ -> []
  TriggerCondition.PlayerGainsLife _ -> []
  TriggerCondition.PlayerLosesLife _ -> []
  -- CR 714.2b carries a counter kind and a Natural, neither of which is a Count.
  TriggerCondition.SelfCountersReached {} -> []
  -- CR 310.12b carries a counter kind alone.
  TriggerCondition.SelfLastCounterRemoved _ -> []
  -- CR 601.2i's Filter is a predicate over the spell that was cast, and a Filter
  -- holds no Count, exactly as CR 603.6a's does above.
  TriggerCondition.SpellCast {} -> []
  -- The same rule read off the spell itself carries nothing at all.
  TriggerCondition.SelfCast -> []
  -- CR 702.21a's condition carries a PlayerRelation and no Count.
  TriggerCondition.SelfBecomesTargeted _ -> []

-- Every Count reachable from one effect: its own Quantity/Duration fields,
-- and -- for Create/CreateEmblem -- every Count in the embedded token/emblem
-- card (the same nesting Pawl.Codec's round trip walks).
effectCounts :: Effect.Effect Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
effectCounts effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage _ quantity) -> quantityCounts quantity
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) -> durationCounts duration <> modificationCounts modification
  Effect.ChangeText {} -> []
  Effect.AddMana _ -> []
  -- The search's count is a Quantity like any other -- Explosive Vegetation's
  -- "up to two" -- so its Counts are reachable from here.
  Effect.Search (Search.MkSearch _ _ quantity _ _) -> quantityCounts quantity
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  -- Bolster's N is a Quantity like the Search's above, so its Counts are
  -- reachable from here.
  Effect.Bolster quantity -> quantityCounts quantity
  -- Amass's N is a Quantity like the Search's above, so its Counts are reachable
  -- from here.
  Effect.Amass (Amass.MkAmass quantity _) -> quantityCounts quantity
  -- Blight's N is a Quantity like bolster's above, so its Counts are reachable
  -- from here.
  Effect.Blight quantity -> quantityCounts quantity
  Effect.TemptWithTheRing -> []
  Effect.Venture -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> quantityCounts quantity
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.Mill (Mill.MkMill _ quantity _) -> quantityCounts quantity
  -- No Quantity: rule 701.20e's look names its cards through an ObjectRef, and
  -- ObjectRef.TopOfLibrary's depth is a literal Natural.
  Effect.LookAt {} -> []
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  -- No Quantity at all: rule 701.44a's counter is a literal one and its card is
  -- the one on top, so there is no number a card author writes.
  Effect.Explore {} -> []
  Effect.Discard (Discard.MkDiscard _ quantity) -> quantityCounts quantity
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  -- The floor beside it is a printed literal and holds no Count.
  Effect.DecreaseSpeed d -> quantityCounts (SpeedDecrease.quantity d)
  Effect.Create (Create.MkCreate quantity card _ _) -> quantityCounts quantity <> overFaces cardCounts card
  -- No embedded card -- the copied permanent supplies the text -- but the count
  -- is card data like Create's.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _) -> quantityCounts quantity
  -- The Condition is Galvanic Blast's "if you control three or more
  -- artifacts", and its Counts are as much card data as a Duration's.
  Effect.Replace (Replace.MkReplace duration _ _ condition replacement) -> durationCounts duration <> foldMap conditionCounts condition <> concatMap effectCounts (replacementEffectRiders replacement)
  -- CR 614.10a's "next" is a use count, not a Duration and not a Quantity.
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- CR 615.5's rider is an effect list a card authors, so its Counts are this
  -- card's Counts -- the same recursion Create takes into a minted token.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ quantity rider) -> durationCounts duration <> quantityCounts quantity <> concatMap effectCounts rider
  Effect.PreventAllDamage (DurationRef.MkDurationRef duration _) -> durationCounts duration
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ _ _) -> durationCounts duration
  Effect.TurnFaceDown _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter _ -> []
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> quantityCounts quantity
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> quantityCounts quantity
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantityCounts quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantityCounts quantity
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.Transform _ -> []
  Effect.AddPhases _ -> []
  Effect.GainControl (DurationRef.MkDurationRef duration _) -> durationCounts duration
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ _) -> durationCounts duration
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration _ _) -> durationCounts duration
  Effect.CreateEmblem card -> overFaces cardCounts card
  Effect.BecomeMonarch _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.Unsuspect _ -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChooseOpponent _ -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary {} -> []
  Effect.OfferCast {} -> []
  -- The Duration's Condition, exactly as GainControl's: Victor Mancha, Runaway's
  -- "for as long as you control this creature" is a Count, and dropping it here
  -- would take its Filters out of the lint with it.
  Effect.GrantPlayFromExile grant -> durationCounts (GrantPlayFromExile.duration grant)
  -- CR 608.2f's body is an effect list a card authors, so its Counts are this
  -- card's -- the rider's recursion one opcode over.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> concatMap effectCounts body

-- Every Count reachable from one triggered ability (a card's own, or a
-- delayed one -- both TriggeredAbility Card): its TriggerCondition, its
-- intervening "if" clause, and its modes' effects.
-- CR 702.178a's "as long as" gate is a Condition like any other, so it reaches a
-- Count and through it a Filter -- triggeredAbilityCounts' treatment of CR 603.4's
-- intervening "if", one field over.
activatedAbilityCounts :: ActivatedAbility.ActivatedAbility Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
activatedAbilityCounts ability =
  foldMap conditionCounts (ActivatedAbility.condition ability)
    <> concatMap effectCounts (Modal.allEffects (ActivatedAbility.modal ability))
    <> concatMap conditionCounts (modalClauseConditions (ActivatedAbility.modal ability))

triggeredAbilityCounts :: TriggeredAbility.TriggeredAbility Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
triggeredAbilityCounts ability =
  triggerConditionCounts (TriggeredAbility.condition ability)
    <> foldMap conditionCounts (TriggeredAbility.intervening ability)
    <> concatMap effectCounts (Modal.allEffects (TriggeredAbility.modal ability))
    <> concatMap conditionCounts (modalClauseConditions (TriggeredAbility.modal ability))

-- Every Count reachable from a card: every site a Pawl.Types.Count can be
-- authored -- Quantity (characteristic-defining P/T, printed P/T, and every
-- effect/modification quantity), Condition (a trigger's own condition, a
-- triggered ability's intervening clause, an activated ability's CR 702.178a
-- gate, a ForAsLongAs duration, CR 701.46a's per-clause gate, and CR 508.1c's /
-- CR 509.1b's "unless some condition is met"), and every effect
-- (spell, activated, triggered, delayed), recursing into a minted token or
-- emblem.
--
-- This traversal is hand-maintained, not derived, so it is NOT enforced
-- exhaustive by -Werror the way the Zone/Effect/Modification cases inside it
-- are: a NEW Face field, or a new CostComponent/PlayerEffect arm, that can carry
-- a Quantity or Count would bypass this lint silently. When you add a field that
-- can hold either, add it here.
-- Every effect a card can RESOLVE: its spell's modes, its activated and
-- triggered abilities' modes, and its delayed abilities' modes. Deliberately
-- wider than Card.allEffects (spell modes only) -- a stored continuous effect
-- can be created from any of these, which is what the control lint below is
-- about. Static abilities are absent on purpose: a static ability's
-- modification is never stored.
--
-- CR 107.3: does this cost declare an X? Asked of a SPELL's cost and of an
-- ABILITY's activation cost -- the two costs CR 602.2b calls each other's analog
-- -- so the two halves of the "reads X iff X is declared" lint ask it in the same
-- words.
--
-- Pawl.Engine.Cost.hasVariable is the ENGINE's own reading, reused rather than
-- restated so the lint and the announcement cannot disagree about which cards get
-- asked. It reads BOTH halves of a cost: CR 601.2b's "such as an {X} in its mana
-- cost" is an example, and CR 107.3a lists the additional cost beside the mana
-- cost -- Hatred's only X is a CostComponent.PayLifeX. Nothing (CR 118.6, an
-- unpayable cost) declares nothing.
declaresVariable :: Cost.Type.Cost Keyword.Keyword -> Bool
declaresVariable = Cost.hasVariable

-- The costs a SPELL can be announced against: the printed one -- mana cost plus
-- CR 118.8's additional costs -- and each alternative cost the card offers, which
-- is the candidate list Pawl.Engine.Cost.costsFor builds. Any one of them
-- declaring X is enough for casting to bind Binding.variableX, so the lint asks
-- `any`.
--
-- The alternatives are taken UNWRAPPED, unlike costsFor's `withAdditional`: the
-- printed cost in the head of this list already carries the additional costs, so
-- wrapping them again would change no `any`.
spellCostsOf :: Face.Face Card.Type.Card -> [Cost.Type.Cost Keyword.Keyword]
spellCostsOf face =
  Cost.Type.MkCost (Face.manaCost face) (Face.additionalCosts face)
    : fmap AlternativeCost.cost (Face.alternativeCosts face)

-- Every Count reachable from a combat restriction: only CR 508.1c's / CR
-- 509.1b's "unless some condition is met" carries one, and the subject beside it
-- is an Affected, which holds a Filter but no Count.
combatRestrictionCounts :: CombatRestriction.CombatRestriction -> [Count.Type.Count Quantity.Type.Quantity]
combatRestrictionCounts restriction = case restriction of
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless _ condition) -> foldMap conditionCounts condition
  CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless _ condition) -> foldMap conditionCounts condition
  -- The blocker Filter beside the gate holds no Count either.
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy _ _ condition) -> foldMap conditionCounts condition
  CombatRestriction.CantAttackAlone (AffectedUnless.MkAffectedUnless _ condition) -> foldMap conditionCounts condition
  CombatRestriction.CantAttackMoreThan (LimitUnless.MkLimitUnless _ condition) -> foldMap conditionCounts condition
  CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless _ condition) -> foldMap conditionCounts condition

-- Every Count reachable from a blocking permission: CR 604.2's "as long as" gate,
-- and the counted arity beside it (Kemba's Legion). The subject is an Affected,
-- which holds a Filter but no Count.
blockPermissionCounts :: BlockPermission.BlockPermission -> [Count.Type.Count Quantity.Type.Quantity]
blockPermissionCounts permission =
  foldMap quantityCounts (BlockPermission.additional permission)
    <> foldMap conditionCounts (BlockPermission.while permission)

-- Hand-maintained, with cardCounts' caveat: a NEW Face field holding effects
-- must be added here too.
cardResolutionEffects :: Face.Face Card.Type.Card -> [Effect.Effect Card.Type.Card]
cardResolutionEffects card =
  Card.allEffects card
    <> concatMap (Modal.allEffects . ActivatedAbility.modal) (Face.activatedAbilities card)
    <> concatMap (Modal.allEffects . TriggeredAbility.modal) (Face.triggeredAbilities card)
    <> concatMap (Modal.allEffects . TriggeredAbility.modal) (Map.elems (Face.delayedAbilities card))
    -- CR 309.4c: a room ability's effects, which no other limb above reaches --
    -- Pawl.Types.Face.rooms is the fifth carrier.
    <> concatMap (Modal.allEffects . DungeonRoom.ability) (Face.rooms card)
    -- CR 615.5: the additional effect a printed prevention carries
    -- (DamageR.riders), the sixth carrier. Not a resolution's effect at all --
    -- it runs from Resolve.runPreventionRiders when the shield applies -- but
    -- it is a card-authored effect list, which is what every lint downstream of
    -- this function is about.
    <> concatMap replacementEffectRiders (Face.replacementEffects card)

cardCounts :: Face.Face Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
cardCounts card =
  concatMap quantityCounts (Maybe.maybeToList (Face.characteristicPT card))
    <> concatMap (\(Power.MkPower quantity) -> quantityCounts quantity) (Maybe.maybeToList (Face.power card))
    <> concatMap (\(Toughness.MkToughness quantity) -> quantityCounts quantity) (Maybe.maybeToList (Face.toughness card))
    <> concatMap staticAbilityCounts (Face.staticAbilities card)
    <> concatMap effectCounts (Card.allEffects card)
    <> concatMap conditionCounts (modalClauseConditions (Face.spell card))
    <> concatMap activatedAbilityCounts (Face.activatedAbilities card)
    <> concatMap triggeredAbilityCounts (Face.triggeredAbilities card)
    <> concatMap triggeredAbilityCounts (Map.elems (Face.delayedAbilities card))
    <> concatMap (concatMap effectCounts . Modal.allEffects . DungeonRoom.ability) (Face.rooms card)
    <> concatMap (concatMap conditionCounts . Maybe.maybeToList . AlternativeCost.condition) (Face.alternativeCosts card)
    <> concatMap combatRestrictionCounts (Face.combatRestrictions card)
    <> concatMap blockPermissionCounts (Face.blockPermissions card)

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
  Scope.InZone (InZone.MkInZone zone ref) -> isSharedZone zone && ref /= PlayerRef.EachPlayer
  Scope.InHistory _ -> False
  -- No zone at all, shared or otherwise: this scope folds the players a
  -- PlayerRef names rather than a copy of a zone each of them owns, so the
  -- pairing the lint rejects cannot arise.
  Scope.OverPlayers _ -> False

cardOffendsSharedZoneScope :: Face.Face Card.Type.Card -> Bool
cardOffendsSharedZoneScope card =
  any (scopeOffends . Count.Type.scope) (cardCounts card)

-- The shared shape of the D4 dataflow lint, over EVERY carrier a card can hang
-- modes off: its spell, and its activated, triggered and delayed abilities. The
-- claim is an EQUALITY -- what ONE MODE reads, less what that mode mints and less
-- what the carrier binds on its own, is exactly what that mode DECLARES. So a
-- read nothing binds is rejected (an effect naming CR 400.7e's `became` under a
-- condition that never binds it would silently no-op), and so is a declared
-- target slot no effect reads (a card announcing a target it ignores).
--
-- `abilityBound` is the only part that differs between the carriers: the slots
-- that carrier binds whichever mode is chosen. It is subtracted from the READ
-- side rather than added to the declared one, and that is forced rather than
-- chosen -- the "no reserved binding slot is ever a declared target slot" sweep
-- below forbids a card DECLARING any of these names, so adding them to the
-- declared side would make the two rules mutually unsatisfiable (#1043).
--
-- PER MODE, never through Modal.allEffects and Modal.allTargetSlots. Those are
-- unions across every mode, and comparing one union against the other lets a
-- mode read a slot only ANOTHER mode declares -- unbound at runtime, because
-- Pawl.Engine.Activate.activateAbility and Pawl.Engine.Engine's trigger placement
-- both stamp Modal.modesTargetSlots, the CHOSEN modes' slots alone. CR 700.2c is
-- the rule: "If a spell or ability targets one or more targets only if a
-- particular mode is chosen for it, its controller will need to choose those
-- targets only if they chose that mode" (#570).
--
-- Resolve.definedSlots is per mode for the mirror reason: under a ChooseExactly
-- 1 selection mode B is never resolved alongside mode A, so a token mode A mints
-- is not there for mode B to read.
modalSlotsOffend :: Set.Set SlotName.SlotName -> Modal.Modal Card.Type.Card -> Bool
modalSlotsOffend abilityBound modal =
  let modeOffends mode =
        let effects = Foldable.toList (Mode.allEffects mode)
            -- A slot DEFINED in this mode (a Create's minted token, or a
            -- PlaySubgame's bound subgame outcome) and then read by a later
            -- effect is legitimate dataflow, not an undeclared target.
            defined = Resolve.definedSlots effects
            -- The whole MODE's reads, not just its effect list's: CR 118.12a's
            -- "unless [a player] pays" names its payer by slot too.
            wanted = Map.keysSet (Resolve.modeSlots mode)
         in Set.difference (Set.difference wanted defined) abilityBound /= Map.keysSet (Mode.targetSlots mode)
   in any modeOffends (Modal.modes modal)

-- CR 601.2c's OTHER dataflow question, asked of the same modes: a slot whose
-- count may exceed one holds a set of recipients, and only a reader that takes a
-- set can see all of them (Pawl.Types.SlotArity). A card aiming "up to two target
-- creatures" at an opcode that names one object would affect NEITHER of them --
-- Pawl.Engine.Binding.onlyOne declines a slot naming several rather than picking
-- one -- and no compiler catches that, so it is caught here.
--
-- Read off the same Resolve.modeSlots the D4 lint reads, whose join keeps the
-- narrower arity: a slot two effects of one mode read both ways is One.
modalCountsOffend :: Modal.Modal Card.Type.Card -> Bool
modalCountsOffend modal =
  let modeOffends mode =
        let read_ = Resolve.modeSlots mode
            plural targetSlot = TargetCount.plural (TargetSlot.count targetSlot)
            offends slot targetSlot = plural targetSlot && Map.lookup slot read_ == Just SlotArity.One
         in or (Map.elems (Map.mapWithKey offends (Mode.targetSlots mode)))
   in any modeOffends (Modal.modes modal)

-- Every ReplacementEffect a card AUTHORS: the ones it PRINTS
-- (Face.replacementEffects, Eon Hub's) and the ones an effect of its own
-- installs (Effect.Replace, a floating one) -- and, through effectReplacements
-- below, everything a token or emblem those effects mint prints in turn. All of
-- them come out of card JSON, which is the whole of what the lint below is
-- about; a replacement the ENGINE bakes reaches GameState without passing
-- through a Card and is not swept here.
cardReplacementEffects :: Face.Face Card.Type.Card -> [ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card)]
cardReplacementEffects card =
  Face.replacementEffects card
    <> concatMap effectReplacements (cardResolutionEffects card)

-- CR 615.5: the additional effect a replacement PRINTS -- DamageR's riders, and
-- nothing else, since no other arm has a field to carry one. The card-authored
-- twin of Effect.PreventNextDamage's `riders`, and swept everywhere that one is.
replacementEffectRiders :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card) -> [Effect.Effect Card.Type.Card]
replacementEffectRiders replacement = case replacement of
  ReplacementEffect.DamageR (DamageR.MkDamageR _ _ riders) -> Foldable.toList riders
  ReplacementEffect.CounterR {} -> []
  ReplacementEffect.ZoneChangeR {} -> []
  ReplacementEffect.EntryR {} -> []
  ReplacementEffect.DestructionR _ -> []
  ReplacementEffect.TokenR {} -> []
  ReplacementEffect.TurnUpR {} -> []
  ReplacementEffect.PhaseR _ -> []

-- Every ReplacementEffect one effect authors: the one an Effect.Replace installs
-- directly, plus everything a minted token (CR 111) or emblem (CR 114.2) prints,
-- since each of those is a whole Card that can carry replacementEffects of its
-- own. The same recursion cardCounts and cardFilters take, and for the same
-- reason -- without it a baked whosePhase could hide one Card deep.
--
-- Exhaustive and hand-maintained, with effectCounts' caveat: a NEW effect
-- carrying a ReplacementEffect or embedding a Card must be added here too, and
-- the build breaks until it is.
effectReplacements :: Effect.Effect Card.Type.Card -> [ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card)]
effectReplacements effect = case effect of
  Effect.Replace (Replace.MkReplace _ _ _ _ replacement) -> replacement : concatMap effectReplacements (replacementEffectRiders replacement)
  Effect.Create (Create.MkCreate _ token _ _) -> overFaces cardReplacementEffects token
  Effect.CreateCopy {} -> []
  Effect.CreateEmblem emblem -> overFaces cardReplacementEffects emblem
  Effect.DealDamage (DealDamage.MkDealDamage _ _) -> []
  Effect.ModifyTarget {} -> []
  Effect.AddMana _ -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.Bolster _ -> []
  Effect.Amass _ -> []
  Effect.Blight _ -> []
  Effect.TemptWithTheRing -> []
  Effect.Venture -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices {} -> []
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.Mill {} -> []
  Effect.LookAt {} -> []
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore {} -> []
  Effect.Discard (Discard.MkDiscard _ _) -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.DecreaseSpeed _ -> []
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- CR 615.5's rider can carry an Effect.Replace, so this descends.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ rider) -> concatMap effectReplacements rider
  -- CR 608.2f's body can too, for the same reason.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> concatMap effectReplacements body
  Effect.PreventAllDamage {} -> []
  Effect.RedirectDamage {} -> []
  Effect.TurnFaceDown _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter _ -> []
  Effect.PutCounters {} -> []
  Effect.RemoveCounters {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.Transform _ -> []
  Effect.AddPhases _ -> []
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> []
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.RequireBlock {} -> []
  Effect.BecomeMonarch _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.Unsuspect _ -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChooseOpponent _ -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary {} -> []
  Effect.OfferCast {} -> []
  Effect.GrantPlayFromExile {} -> []
  Effect.ChangeText {} -> []

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
-- Face.replacementEffects, which a card authors, and ActiveReplacement.effect,
-- which the engine bakes. A card-side / runtime-side split the way Duration and
-- Expiry are split would therefore have to split or parameterize that whole sum,
-- not just PhasePattern. Modification is shared exactly the same way, between
-- StaticAbility.modifications and a stored ContinuousEffect, which is why the
-- two cases want ONE answer rather than two -- what the issue asks be decided
-- once.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: a second
-- pattern-carrying replacement must break this build rather than silently pass.
phasePatternOffends :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card) -> Bool
phasePatternOffends replacement = case replacement of
  ReplacementEffect.PhaseR phasePattern -> Maybe.isJust (PhasePattern.whosePhase phasePattern)
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.ZoneChangeR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.DamageR {} -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.TokenR {} -> False
  ReplacementEffect.TurnUpR {} -> False

-- Every replacement shape the codec accepts and no card may author, for
-- phasePatternOffends' reason and one more. A card cannot name an ObjectId or a
-- PlayerId, so the recipient a shield covers -- CR 615.7's, and CR 615.3's
-- unbounded one -- is for Resolve's prevention arms to write, and CR 615.7's
-- remaining amount rides the same carrier. CR 122.1c's pair is engine-only for a
-- different reason: a RULE creates it off a permanent's counters, so a card
-- printing either half would be claiming an ability no rule gives it.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum.
engineOnlyOffends :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card) -> Bool
engineOnlyOffends replacement = case replacement of
  -- `whatRecipient` beside it is the PRINTED half and is not swept: a card may
  -- describe the recipient it shields (Stormwild Capridor), it just may not name
  -- one by id.
  ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern rewrite _) ->
    Maybe.isJust (DamagePattern.whichRecipient damagePattern) || engineMintedDamage rewrite
  -- CR 122.1c's destruction half is engine-minted for the same reason its damage
  -- half is, so the sweep reaches it through this arm rather than through a lint
  -- of its own.
  ReplacementEffect.DestructionR rewrite -> engineMintedDestruction rewrite
  ReplacementEffect.PhaseR _ -> False
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.ZoneChangeR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.TokenR {} -> False
  ReplacementEffect.TurnUpR {} -> False

-- Is this damage rewrite one the ENGINE mints and no card may print? Two of them,
-- for two rules:
--
--   * CR 615.7 versus CR 615.10 -- a counted shield is generated "by the resolution
--     of a spell or ability", never by the static ability a card prints.
--   * CR 122.1c -- the prevention shield counters create is created by the RULE, off
--     a permanent's counters (Pawl.Engine.Projection.shieldOf), so a card printing
--     it would be claiming a static ability the rule does not give it.
--
-- A printed one either way would be a rule that does not exist.
engineMintedDamage :: DamageRewrite.DamageRewrite -> Bool
engineMintedDamage rewrite = case rewrite of
  DamageRewrite.PreventNext _ -> True
  DamageRewrite.PreventRemovingShieldCounter -> True
  DamageRewrite.PreventAll -> False
  DamageRewrite.SetAmount _ -> False
  DamageRewrite.Scale _ -> False
  -- CR 614.9's redirection is neither counted nor a prevention.
  DamageRewrite.Redirect _ -> False

-- The destruction half of the same question. CR 701.19a's regeneration IS printed
-- (Drudge Skeletons), where CR 122.1c's removal is minted.
engineMintedDestruction :: DestructionRewrite.DestructionRewrite -> Bool
engineMintedDestruction rewrite = case rewrite of
  DestructionRewrite.RemoveShieldCounter -> True
  DestructionRewrite.Regenerate -> False

-- The non-vacuity half of the same lint: is this the replacement that carries a
-- PhasePattern at all? A wildcard is right here, where it is not above -- this
-- asks "did the sweep have anything to look at", not "is it well-formed".
isPhaseR :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card) -> Bool
isPhaseR replacement = case replacement of
  ReplacementEffect.PhaseR _ -> True
  _ -> False

-- The non-vacuity half of engineOnlyOffends' lint, isPhaseR's shape.
isDamageR :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card) -> Bool
isDamageR replacement = case replacement of
  ReplacementEffect.DamageR {} -> True
  _ -> False

-- Do these slot-name sets overlap? True when any name appears in more than one
-- of them, which is exactly what a Map.unions over them would silently collapse.
slotNamesCollide :: [Set.Set SlotName.SlotName] -> Bool
slotNamesCollide sets = Set.size (Set.unions sets) /= sum (fmap Set.size sets)

-- CR 700.2c: do two modes of one modal declare the same slot NAME -- or does a
-- spell mode collide with CR 303.4a's enchant slot?
--
-- Modal.modesTargetSlots, Modal.allTargetSlots and Card.allTargetSlots are all
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
-- that add it -- Card.modesTargetSlots and Card.allTargetSlots -- are both over
-- the spell. CR 303.4a's slot is announced when the Aura spell is cast, and
-- Activate stamps Modal.modesTargetSlots, which has no enchant half. Each ability is checked on
-- its own for the same reason: two abilities are two separate announcements, so
-- a name they share is never fused.
cardSlotNamesCollide :: Face.Face Card.Type.Card -> Bool
cardSlotNamesCollide card =
  let modeSlots modal = fmap (Map.keysSet . Mode.targetSlots) (Foldable.toList (Modal.modes modal))
   in slotNamesCollide (Map.keysSet (Card.enchantSlotMap card) : modeSlots (Face.spell card))
        || any (slotNamesCollide . modeSlots . ActivatedAbility.modal) (Face.activatedAbilities card)
        || any (slotNamesCollide . modeSlots . TriggeredAbility.modal) (Face.triggeredAbilities card)
        || any (slotNamesCollide . modeSlots . TriggeredAbility.modal) (Map.elems (Face.delayedAbilities card))
        || any (slotNamesCollide . modeSlots . DungeonRoom.ability) (Face.rooms card)

-- The TRIGGERED-ability half of the D4 dataflow lint: every slot one of a
-- triggered ability's effects READS must be a slot something binds for that
-- ability, and every slot it DECLARES must be read. Without the first half, an
-- effect naming CR 400.7e's `became` under a condition that never binds it
-- loads, places its trigger, misses the lookup and silently no-ops (Resolve's
-- MoveToZone arm moves nothing for a slot that names no object); without the
-- second, an ability announces a target it ignores.
--
-- The spell lint's EQUALITY, which modalSlotsOffend now applies to every carrier
-- (#1043). It was a subset check for as long as the bound side was UNIONED into
-- the declared one, which Pawl.Engine.Binding.triggerSource's comment shows is
-- mutually unsatisfiable with the "a reserved slot is never a declared target
-- slot" rule; subtracting the bound names from the READ side instead -- what the
-- spell lint always did with its cast-time pair -- is what makes the equality
-- statable here.
--
-- What answers a read, and why each part of it answers one:
--
--   * Binding.triggerSource (CR 113.7, the object whose ability triggered) and
--     Binding.you (CR 109.5, the ability's controller) are stamped for EVERY
--     triggered ability as it is placed (Engine.placeBorne, Binding.setYou), so
--     they need no agreement with the condition. `you` is stamped for every
--     ACTIVATION and every SPELL too, by rule 109.5's other sentences -- which is
--     why the spell lint subtracts it on the read side rather than listing it
--     here.
--   * Event.eventBindingSlots is the condition-SPECIFIC half -- CR 400.7e's
--     `became`, CR 702.70a's `thatPlayer` -- and is the whole point of this
--     lint.
--   * Resolve.definedSlots covers a slot the ability's own effects MINT rather
--     than read: a Create's token (CR 603.7c's "it"), a PlaySubgame's loser.
--     The same exemption every carrier takes.
--   * the ability's own declared target slots (CR 601.2c / 700.2c) are the
--     ordinary chosen targets -- the side modalSlotsOffend compares AGAINST, one
--     MODE's at a time, so a mode reading a slot only another mode declares is
--     caught and so is a mode declaring a slot only another mode reads.
--
-- The first two are what this passes to modalSlotsOffend as `abilityBound`: they
-- are stamped for the ability, not for a mode, so every mode gets them.
triggeredAbilityOffends :: TriggeredAbility.TriggeredAbility Card.Type.Card -> Bool
triggeredAbilityOffends ability =
  modalSlotsOffend
    ( Set.unions
        [ Set.fromList [Binding.triggerSource, Binding.you],
          Event.eventBindingSlots (TriggeredAbility.condition ability)
        ]
    )
    (TriggeredAbility.modal ability)

-- The ACTIVATED-ability half of the same lint: every slot one of an
-- activated ability's effects READS must be a slot the ACTIVATION binds, and
-- every slot it DECLARES must be read. Without the first half, an ability naming
-- CR 109.5's `you` loads, activates, misses the lookup and silently no-ops,
-- exactly as an unbound `became` does above.
--
-- The same EQUALITY as every other carrier (#1043); see modalSlotsOffend and the
-- triggered lint above for why subtracting the bound names from the read side is
-- what makes it statable.
--
-- What answers a read is what Pawl.Engine.Activate.activateAbility stamps on the
-- ability object as it goes on the stack, and nothing else:
--
--   * Binding.triggerSource. CR 113.7: "The source of an activated ability on
--     the stack is the object whose ability was activated" -- stamped for every
--     activation, so Longtusk Cub's "put a +1/+1 counter on Longtusk Cub" is a
--     slot read.
--   * the ability's own declared target slots, one MODE's at a time
--     (modalSlotsOffend). CR 602.2b: "The remainder of the process for
--     activating an ability is identical to the process for casting a spell
--     listed in rules 601.2b-i", which is what routes an activation through CR
--     601.2c's target announcement -- and CR 700.2c scopes it to the chosen
--     mode.
--   * Binding.you. CR 109.5: "For an activated ability, this is the player who
--     activated the ability" -- stamped for every activation alongside the
--     source slot, so Brothers of Fire's "and 1 damage to you" is a slot read.
--     Cast.castSpell stamps it for every SPELL as well (Char), which the spell
--     lint takes on its read side.
--   * Binding.variableX, and ONLY when the ability's own cost prints an {X}:
--     CR 601.2b's "the player announces the value of that variable", measured
--     against what CR 602.2b calls "an activated ability's analog to a spell's
--     mana cost ... its activation cost" (Cinder Elemental). Nothing reads it as
--     a slot today -- a printed X is Quantity.X, whose own half of the contract
--     is the CR 602.2b sweep below -- but the activation really does bind it, so
--     leaving it out would reject a read that works (#14 is what would make one
--     sayable).
--   * Resolve.definedSlots, the slot an effect of this ability MINTS rather than
--     reads. The same exemption every sibling carrier takes.
--
-- What is NOT on it is the point:
--
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
-- pawl's mana path pays a route's cost and lifts its AddMana effects out, so
-- NOTHING is bound for one and none of its other effects runs (#1118). No mana
-- ability in the pool reads a slot, so applying the same
-- available side to one is uniformity rather than a claim.
activatedAbilityOffends :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Bool
activatedAbilityOffends ability =
  let announcedX =
        if declaresVariable (ActivatedAbility.cost ability)
          then Set.singleton Binding.variableX
          else Set.empty
   in modalSlotsOffend (Set.union (Set.fromList [Binding.triggerSource, Binding.you]) announcedX) (ActivatedAbility.modal ability)

-- CR 603.7 / 109.5: does this card arm a delayed ability "on your next turn"
-- whose condition is not scoped to its controller's turn?
--
-- Pawl.Types.Onset.FromYourNextTurn carries BOTH halves of that phrase on its
-- own: Event.armOnset stores TurnWindow.ControllersNextTurn and
-- Event.settleOnsets pins the entry to the one turn that turns out to be, whose
-- active player is the entry's controller. So the ability's own
-- TriggerCondition.StepBegins carrying TurnScope.ControllersTurn is redundant
-- for FIRING.
--
-- It is not redundant in the DATA, which is what this lint is about: a card that
-- arms with the onset but scopes with EachTurn has printed an "each" the window
-- would silently narrow to the controller's turn, so its text would mean
-- something the card does not say. That is what this rejects.
--
-- A dangling name (an onset naming an ability the card does not declare) is
-- ALSO an offence here, and deliberately not silently accepted: the neighbouring
-- "every armed delayed ability is declared" lint is what reports it precisely,
-- and answering False for it here would let a card that offends both pass this
-- one.
onsetOffends :: Face.Face Card.Type.Card -> Bool
onsetOffends card =
  let scoped name = case Map.lookup name (Face.delayedAbilities card) of
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
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
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.restrictions = [],
      ActivatedAbility.condition = Nothing
    }

-- One CR 700.2 mode for the fixtures below: the effects it runs and the target
-- slots it declares. Always mandatory -- no read lint asks about optionality.
lintMode :: [Effect.Effect Card.Type.Card] -> [SlotName.SlotName] -> Mode.Mode Card.Type.Card
lintMode effects slots =
  Mode.MkMode
    (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList effects)))
    (Map.fromList (fmap (\slot -> (slot, TargetSlot.required Pool.AnyTarget Nothing)) slots))

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
      ActivatedAbility.restrictions = [],
      ActivatedAbility.condition = Nothing
    }

-- modalActivated's TRIGGERED twin, so the per-mode lint can be shown to hand
-- `abilityBound` -- the condition's event slots and CR 109.5's `you` -- to EVERY
-- mode rather than only the first.
-- Does any of these abilities DECLARE a target slot under a name already in
-- `defined`? Split from the sweep below so the rejecting direction can be put to
-- a hand-built ability, which no committed card supplies.
shadowsSlots :: Set.Set SlotName.SlotName -> [TriggeredAbility.TriggeredAbility Card.Type.Card] -> Bool
shadowsSlots defined abilities =
  let declaredOf ability =
        foldMap (Map.keysSet . Mode.targetSlots) (Modal.modes (TriggeredAbility.modal ability))
   in not (Set.disjoint defined (foldMap declaredOf abilities))

-- shadowsSlots for one face: the slots its own effects define, against the target
-- slots its delayed abilities declare.
shadowsDefinedSlot :: Face.Face Card.Type.Card -> Bool
shadowsDefinedSlot card =
  shadowsSlots
    (Resolve.definedSlots (cardResolutionEffects card))
    (Map.elems (Face.delayedAbilities card))

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
-- hand-picked subset, so a new reserved slot joins BOTH sweeps below -- the
-- declaration one and the binding one -- by being added here and nowhere else.
reservedSlots :: Set.Set SlotName.SlotName
reservedSlots =
  Set.fromList
    [ Binding.variableX,
      Binding.chosenModes,
      Binding.copySource,
      Binding.triggerSource,
      Binding.you,
      Binding.triggerPlayer,
      Binding.became,
      Binding.eventAmount,
      Binding.sacrificedCount,
      Binding.castSpell,
      Binding.targetingObject,
      Binding.blockingCreature,
      Binding.blockedCreature,
      Binding.attackingCreature,
      Binding.combatDamager,
      Binding.mentoredCreature
    ]

-- The binding slots a card's power, toughness and characteristic-defining P/T
-- READ. The available side is the reserved set alone: CR 604.3 makes a CDA a
-- static ability, so there is no resolution whose earlier effect could mint a
-- slot for it the way Resolve.definedSlots does for a spell's payload.
powerToughnessSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
powerToughnessSlots card =
  Set.unions
    [ maybe Set.empty (Quantity.slots . Power.unwrap) (Face.power card),
      maybe Set.empty (Quantity.slots . Toughness.unwrap) (Face.toughness card),
      maybe Set.empty Quantity.slots (Face.characteristicPT card)
    ]

-- Every face a card MINTS, transitively: the faces of every token (CR 111.1)
-- and emblem (CR 114.1) its own effects create, plus everything those mint in
-- turn. The same recursion effectReplacements takes, and for the same reason --
-- CR 111.3 makes a token's defined characteristics "functionally equivalent to
-- the characteristic values that are printed on a card", and CR 114.4 makes an
-- emblem's abilities function in the command zone, so an ability arriving as an
-- effect's payload is as real as a printed one.
--
-- Deliberately NOT folded into cardResolutionEffects, which several other lints
-- read: those ask what THIS card executes, and widening that view would change
-- all of them at once.
--
-- Not applied to the ability dataflow lints built on modalSlotsOffend, which
-- still stop at the printed face (#1010).
mintedFaces :: Face.Face Card.Type.Card -> [Face.Face Card.Type.Card]
mintedFaces = fmap snd . mintedFacesTagged

-- mintedFaces keeping WHICH KIND of object each face was minted for, because CR
-- 205.2c and CR 114.3 disagree about one of them: "tokens have card types even
-- though they aren't cards", and an emblem has none. One traversal carries both, so the exhaustive case below is
-- the only place that has to know.
mintedFacesTagged :: Face.Face Card.Type.Card -> [(MintedKind, Face.Face Card.Type.Card)]
mintedFacesTagged card =
  let minted = concatMap effectMintedFaces (cardResolutionEffects card)
   in minted <> concatMap (mintedFacesTagged . snd) minted

-- CR 111.1 and CR 114.1: the two kinds of object a card's own effects mint.
data MintedKind
  = MintedToken
  | MintedEmblem
  deriving (Eq, Show)

-- The faces one effect mints. Exhaustive and hand-maintained, with
-- effectReplacements' caveat: a NEW effect embedding a Card must be added here
-- too, and the build breaks until it is.
effectMintedFaces :: Effect.Effect Card.Type.Card -> [(MintedKind, Face.Face Card.Type.Card)]
effectMintedFaces effect = case effect of
  Effect.Create (Create.MkCreate _ token _ _) -> fmap ((,) MintedToken) (NonEmpty.toList (Card.Type.faces token))
  -- Mints no face of its own: the token's text is the copied permanent's.
  Effect.CreateCopy {} -> []
  Effect.CreateEmblem emblem -> fmap ((,) MintedEmblem) (NonEmpty.toList (Card.Type.faces emblem))
  Effect.Replace (Replace.MkReplace _ _ _ _ replacement) -> concatMap effectMintedFaces (replacementEffectRiders replacement)
  Effect.DealDamage (DealDamage.MkDealDamage _ _) -> []
  Effect.ModifyTarget {} -> []
  Effect.AddMana _ -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.Bolster _ -> []
  -- The Army token is Pawl.Engine.Amass.armyToken's, minted from the rulebook
  -- rather than embedded in card data, so this arm mints no face of the card's own.
  Effect.Amass _ -> []
  Effect.Blight _ -> []
  Effect.TemptWithTheRing -> []
  Effect.Venture -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices {} -> []
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.Mill {} -> []
  Effect.LookAt {} -> []
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore {} -> []
  Effect.Discard (Discard.MkDiscard _ _) -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.DecreaseSpeed _ -> []
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- CR 615.5's rider can mint a token or emblem of its own, so this descends.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ rider) -> concatMap effectMintedFaces rider
  -- CR 608.2f's body can too, for the same reason.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> concatMap effectMintedFaces body
  Effect.PreventAllDamage {} -> []
  Effect.RedirectDamage {} -> []
  Effect.TurnFaceDown _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter _ -> []
  Effect.PutCounters {} -> []
  Effect.RemoveCounters {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.Transform _ -> []
  Effect.AddPhases _ -> []
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> []
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.RequireBlock {} -> []
  Effect.BecomeMonarch _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.Unsuspect _ -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChooseOpponent _ -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary {} -> []
  Effect.OfferCast {} -> []
  Effect.GrantPlayFromExile {} -> []
  Effect.ChangeText {} -> []

-- Every slot ONE FACE declares as a target: its spell modes plus CR 303.4a's
-- enchant slot (Card.allTargetSlots), and its activated, triggered and delayed
-- abilities' modes. The base case of declaredTargetSlots below, named so the
-- self-test can hold it against the widened view.
--
-- All four carriers, because all four ask the same question at different
-- moments. CR 601.2c is the question ("the player announces their choice of an
-- appropriate object or player for each target the spell requires"); CR 602.2b
-- makes activating an ability follow "rules 601.2b-i"; CR 603.3d makes putting
-- a triggered ability on the stack "identical to the process for casting a
-- spell listed in rules 601.2c-d"; and CR 603.7's delayed abilities are
-- triggered abilities, placed by that same rule. A lint about what DECLARING a
-- slot means therefore has to range over all four, not over the spell alone.
ownDeclaredTargetSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
ownDeclaredTargetSlots card =
  Set.unions
    ( Map.keysSet (Card.allTargetSlots card)
        : fmap
          (Map.keysSet . Modal.allTargetSlots)
          ( fmap ActivatedAbility.modal (Face.activatedAbilities card)
              <> fmap TriggeredAbility.modal (Face.triggeredAbilities card)
              <> fmap TriggeredAbility.modal (Map.elems (Face.delayedAbilities card))
              <> fmap DungeonRoom.ability (Foldable.toList (Face.rooms card))
          )
    )

-- The slots an ARMING carrier declares as targets: the three carriers above that
-- can arm a CR 603.7 delayed ability, which is all of them except the delayed
-- abilities themselves. CR 603.7c captures the whole environment of the object
-- that armed, so a target chosen for the arming spell is in the delayed ability's
-- bindings -- Ray of Command's "when you lose control of the creature" reads the
-- creature the spell targeted.
--
-- Its own declared target slots are EXCLUDED on purpose: the delayed-ability read lint
-- compares against those separately, per mode, and folding them in here would make
-- that comparison vacuous.
--
-- LOOSE about WHICH carrier armed, because nothing here tracks that: a delayed
-- ability reading a slot declared by an activated ability that does not arm it
-- would pass. Tightening it means threading the arming site through, which no card
-- in the pool needs -- Ray of Command arms from the spell that declares the slot.
armingTargetSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
armingTargetSlots card =
  Set.unions
    ( Map.keysSet (Card.allTargetSlots card)
        : fmap
          (Map.keysSet . Modal.allTargetSlots)
          ( fmap ActivatedAbility.modal (Face.activatedAbilities card)
              <> fmap TriggeredAbility.modal (Face.triggeredAbilities card)
          )
    )

-- Every slot a card DECLARES as a target: the four carriers above, on the card's
-- own face AND on every face it mints. A token's triggered ability
-- declares its targets through CR 603.3d exactly as the minting card's does, so
-- the question the lint asks is a property of the ABILITY rather than of how the
-- card carrying it reached the game.
declaredTargetSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
declaredTargetSlots card = Set.unions (fmap ownDeclaredTargetSlots (card : mintedFaces card))

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
reservedDeclarations :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
reservedDeclarations = Set.intersection reservedSlots . declaredTargetSlots

-- Every slot ONE FACE binds: the names its own effects author for a later effect
-- to read back -- Resolve.definedSlots over every carrier that face can execute
-- an effect from (cardResolutionEffects), which is the spell modes plus the
-- activated, triggered and delayed abilities.
--
-- ownDeclaredTargetSlots' sibling and the other half of the same question.
-- Declaring a target slot is not the only way a card names a slot: MoveToZone and Create
-- name the incarnation CR 400.7 mints, PlaySubgame names CR 729.1b's loser, and
-- Destroy names how many it destroyed.
--
-- The base case of boundSlots below, named so the self-test can hold it against
-- the widened view.
ownBoundSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
ownBoundSlots = Resolve.definedSlots . cardResolutionEffects

-- The same, over the card's own face AND every face it mints --
-- declaredTargetSlots' recursion, for the same reason.
boundSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
boundSlots card = Set.unions (fmap ownBoundSlots (card : mintedFaces card))

-- The reserved names a card BINDS -- reservedDeclarations' sibling, and empty
-- for every card authored correctly for the same reason.
--
-- A DIFFERENT failure from declaring one, and the more dangerous of the two,
-- because nothing about it is a discarded prompt: the card's own write lands on
-- the very key the engine stamps. Pawl.Engine.Quantity.evaluateFor's InSlot arm
-- asks the effect's SOURCE before the stack object precisely because
-- Resolve.bindAmountSlot writes to the source and Event.eventBindings writes
-- where the trigger's bindings live; a card binding CR 615.13's `thatMuch` from
-- a Destroy would therefore SHADOW the amount the event supplied with its own
-- count, and win, silently. The comment there argues the two writers "cannot
-- collide over one name" -- this sweep is what makes that argument true.
reservedBindings :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
reservedBindings = Set.intersection reservedSlots . boundSlots

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
tokenNameOffends :: Face.Face Card.Type.Card -> Bool
tokenNameOffends token =
  case traverse (fmap (Text.pack . fst) . Common.asTagged . Codec.encode Subtype.codec) (Set.toList (TypeLine.subtypes (Face.typeLine token))) of
    Left _ -> True
    Right subtypes ->
      notElem
        (CardName.unwrap $ Face.name token)
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
  -- A Filter position like the combinators above, and the only ATOM that is one:
  -- CR 110.2's comparison carries the description of what is being counted, which
  -- a card author writes exactly as they write any other filter.
  Filter.Type.ControlsMoreThanYou f -> canHostSubjects f
  -- CR 702.29e's "[type]cycling" carries a Filter of its own, and any Cost a
  -- keyword names can carry one through a Sacrifice component. Never EVALUATED
  -- against a candidate -- HasKeyword asks whether the key is present in the
  -- projection's keyword map, so what is inside the keyword is compared and not
  -- run -- but still a Filter position a card author can write the atom into,
  -- which is the only thing this lint is about.
  Filter.Type.HasKeyword keyword -> sum (fmap canHostSubjects (keywordFilters keyword))
  -- CR 122.1b's keyword counter carries a whole Keyword, so a Filter can hide one
  -- level further down than the atom above -- and this lint is about the
  -- positions, not about which of them a card has used.
  Filter.Type.HasCounters kind -> case kind of
    CounterKind.Keyword keyword -> sum (fmap canHostSubjects (keywordFilters keyword))
    CounterKind.PlusOnePlusOne -> 0
    CounterKind.MinusOneMinusOne -> 0
    CounterKind.Loyalty -> 0
    CounterKind.Lore -> 0
    CounterKind.Defense -> 0
    CounterKind.Time -> 0
    CounterKind.Fade -> 0
    CounterKind.Shield -> 0
  -- Zero and not a descent, unlike the atom above: a family is payload-free, so
  -- there is no Filter position inside it for a card author to reach.
  Filter.Type.HasKeywordFamily _ -> 0
  Filter.Type.HasCardType _ -> 0
  Filter.Type.HasSupertype _ -> 0
  Filter.Type.HasColor _ -> 0
  Filter.Type.HasSubtype _ -> 0
  Filter.Type.PowerAtLeast _ -> 0
  Filter.Type.PowerAtMost _ -> 0
  Filter.Type.PowerLessThanSource -> 0
  Filter.Type.PowerGreaterThanSource -> 0
  Filter.Type.ControlledByDefendingPlayer -> 0
  -- Zero for ControlledBy's reason: one carries a slot name and the other a
  -- PlayerId, and neither holds a Filter for a card author to reach.
  Filter.Type.ControlledByBound _ -> 0
  Filter.Type.ControlledByPlayer _ -> 0
  -- Zero for the two above's reason: a nullary atom holds no Filter for a card
  -- author to reach.
  Filter.Type.ControlledByRecipient -> 0
  Filter.Type.ManaValueAtMost _ -> 0
  Filter.Type.ManaValueIsEven -> 0
  Filter.Type.ControlledBy _ -> 0
  -- Zero for ControlledBy's reason: CR 108.3's owner atom carries a
  -- PlayerRelation, which holds no Filter for a card author to reach.
  Filter.Type.OwnedBy _ -> 0
  Filter.Type.IsSource -> 0
  Filter.Type.IsPlayer _ -> 0
  Filter.Type.IsBound _ -> 0
  Filter.Type.IsControllerOfBound _ -> 0
  Filter.Type.IsAttacking -> 0
  Filter.Type.IsBlocking -> 0
  Filter.Type.IsBlocked -> 0
  Filter.Type.AttackedThisTurn -> 0
  Filter.Type.MilledThisTurn -> 0
  Filter.Type.IsAttachedToCreature -> 0
  Filter.Type.IsAttachedToPermanent -> 0
  Filter.Type.IsAttachedToSource -> 0
  Filter.Type.IsToken -> 0
  Filter.Type.IsTapped -> 0
  Filter.Type.HasNonManaActivatedAbility -> 0
  Filter.Type.IsRingBearer -> 0
  Filter.Type.HasDesignation _ -> 0

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
-- CR 122.1b: the one counter kind with a Filter under it, since it carries a
-- whole Keyword. Exhaustive so a new kind with a payload breaks this build.
counterKindFilters :: CounterKind.CounterKind Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
counterKindFilters kind = case kind of
  CounterKind.Keyword keyword -> keywordFilters keyword
  CounterKind.PlusOnePlusOne -> []
  CounterKind.MinusOneMinusOne -> []
  CounterKind.Loyalty -> []
  CounterKind.Lore -> []
  CounterKind.Defense -> []
  CounterKind.Time -> []
  CounterKind.Fade -> []
  CounterKind.Shield -> []

keywordFilters :: Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
keywordFilters keyword = case keyword of
  Keyword.Cycling (Cycling.MkCycling cost mFilter) -> costFilters cost <> Maybe.maybeToList mFilter
  Keyword.Flashback cost -> costFilters cost
  Keyword.Kicker cost -> costFilters cost
  Keyword.Entwine cost -> costFilters cost
  -- CR 702.170a: the plot cost, whose components may hold a Filter exactly as
  -- flashback's and entwine's may.
  Keyword.Plot cost -> costFilters cost
  -- CR 702.143a: the foretell cost, reached the same way.
  Keyword.Foretell cost -> costFilters cost
  Keyword.Ward cost -> costFilters cost
  -- CR 702.94a's payload is a Cost too, so its filters are reached the same way.
  Keyword.Miracle cost -> costFilters cost
  -- CR 702.37a: the morph cost, whose components may hold a Filter exactly as
  -- flashback's and entwine's may.
  Keyword.Morph (Morph.MkMorph cost _) -> costFilters cost
  -- CR 702.22: plain banding names no quality, so it filters nothing.
  Keyword.Banding -> []
  -- CR 702.26a: phasing names no quality -- who phases is "the permanents that
  -- player controls", written into the CR 502.1 turn-based action rather than
  -- into the keyword.
  Keyword.Phasing -> []
  -- CR 702.28b names no quality: the only thing it asks about a blocker is
  -- whether it has shadow too.
  Keyword.Shadow -> []
  -- CR 702.31b names no quality either: the only thing it asks about a blocker is
  -- whether it has horsemanship too.
  Keyword.Horsemanship -> []
  -- CR 702.118b names no quality either: the comparison is against the skulking
  -- creature's own power, written into the rule rather than into the keyword.
  Keyword.Skulk -> []
  -- CR 702.121a names no quality: the bonus is computed from the combat record
  -- by the ability Pawl.Engine.Keyword mints, not from anything the card prints.
  Keyword.Melee -> []
  -- CR 702.23a's payload is a NUMBER, not a quality: the Filter its minted
  -- ability carries is the engine's, never a card's.
  Keyword.Rampage _ -> []
  -- CR 702.25a is payload-free: the Filter its minted ability carries is the
  -- ENGINE's, never a card's.
  Keyword.Flanking -> []
  -- CR 702.127a names no quality: which zone an aftermath half may be cast from is
  -- written into the rule, not into the keyword.
  Keyword.Aftermath -> []
  -- CR 702.133a names no quality either: both the zone and the discard are the
  -- rule's own words, so the card supplies nothing to filter over.
  Keyword.JumpStart -> []
  -- CR 702.130a names no quality either: "defending player loses N life" is
  -- written into the ability Pawl.Engine.Keyword mints, not into the keyword.
  Keyword.Afflict _ -> []
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  -- CR 702.8a: flash is a static ability with no payload -- it changes WHEN the
  -- card may be cast, and names nothing to filter.
  Keyword.Flash -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  -- CR 702.11d's "[quality]", which the variant carries and rule 702.11b's plain
  -- hexproof does not.
  Keyword.Hexproof quality -> Maybe.maybeToList quality
  Keyword.Indestructible -> []
  -- CR 702.14c's criterion, which is a Filter since #499.
  Keyword.Landwalk criterion -> [criterion]
  -- CR 702.15a: lifelink is a static ability with no payload -- its rider rides
  -- the damage event, not the keyword.
  Keyword.Lifelink -> []
  Keyword.Reach -> []
  Keyword.Shroud -> []
  Keyword.Trample -> []
  Keyword.TrampleOverPlaneswalkers -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  -- CR 702.13b names no quality either: the colours a blocker may have are the
  -- ATTACKER's own, read off the projection at declare blockers rather than
  -- written into the keyword.
  Keyword.Intimidate -> []
  Keyword.Poisonous _ -> []
  -- CR 702.45a names no quality either: the "+N/+N" and the two combat events
  -- are written into the abilities Pawl.Engine.Keyword mints, not into the
  -- keyword.
  Keyword.Bushido _ -> []
  -- CR 702.46a names no quality either: "Spirit card with mana value N or less"
  -- is written into the ability Pawl.Engine.Keyword mints, not into the keyword.
  Keyword.Soulshift _ -> []
  -- CR 702.54a names no quality either: "an opponent" is a player and the +1/+1
  -- counters are the rule's own noun, so neither reaches a Filter.
  Keyword.Bloodthirst _ -> []
  -- CR 702.55a names no quality either: its minted ability's bare "target
  -- creature" pool carries no Filter at all.
  Keyword.Haunt -> []
  -- CR 702.61a names no quality: the sentence speaks about what OTHER players
  -- may do, and "mana ability" is CR 605.1a's classification rather than a
  -- Filter.
  Keyword.SplitSecond -> []
  -- CR 702.77a's cost can carry one, as cycling's can; its N and "target
  -- creature" are written into the ability Pawl.Engine.Keyword mints.
  Keyword.Reinforce (Reinforce.MkReinforce _ cost) -> costFilters cost
  -- CR 702.86a names no quality either: "N permanents" is written into the
  -- ability Pawl.Engine.Keyword mints, not into the keyword.
  Keyword.Annihilator _ -> []
  -- CR 702.91a: battle cry names no quality either -- the "each other attacking
  -- creature" set is written into the ability Pawl.Engine.Keyword mints, not
  -- into the keyword.
  Keyword.BattleCry -> []
  -- CR 702.107a's payload is a COST, and a cost's Filters are its components'.
  Keyword.Outlast cost -> costFilters cost
  -- CR 702.108a names no quality either: the "+1/+1" and the noncreature-spell
  -- condition are written into the ability Pawl.Engine.Keyword mints, not into
  -- the keyword.
  Keyword.Prowess -> []
  Keyword.Infect -> []
  -- CR 702.80a names no quality either: what it changes is where damage goes.
  Keyword.Wither -> []
  -- CR 702.83a names no quality: "a creature you control" is written into the
  -- ability Pawl.Engine.Keyword mints, not into the keyword.
  Keyword.Exalted -> []
  -- CR 702.134a is payload-free too: the Filter its minted ability carries -- the
  -- target slot's -- is the ENGINE's, never a card's.
  Keyword.Mentor -> []
  -- CR 702.135a's payload is a count. The token its minted ability creates
  -- carries no Filter either.
  Keyword.Afterlife _ -> []
  Keyword.Provoke -> []
  Keyword.Menace -> []
  Keyword.Renown _ -> []
  Keyword.Changeling -> []
  Keyword.Devoid -> []
  -- CR 702.115a names no quality: which card moves and where it goes are the
  -- rule's own words, written into the ability Pawl.Engine.Keyword mints.
  Keyword.Ingest -> []
  -- CR 702.122a's payload is a threshold, not a Filter: the criterion the crew
  -- ability is built with lives in Pawl.Engine.Keyword and is not card data.
  Keyword.Crew _ -> []
  -- CR 702.123a's payload is a count. Neither the counters its minted ability
  -- puts on nor the Servo token it creates carries a Filter.
  Keyword.Fabricate _ -> []
  Keyword.Riot -> []
  -- CR 702.98a names no quality either: the +1/+1 counter its minted replacement
  -- places and the Filter its minted combat restriction carries are written in
  -- Pawl.Engine.Keyword, not into the keyword.
  Keyword.Unleash -> []
  -- CR 702.147a names no quality either: the Filter its minted combat
  -- restriction carries is written in Pawl.Engine.Keyword, not into the keyword.
  Keyword.Decayed -> []
  Keyword.Daybound -> []
  Keyword.Nightbound -> []
  Keyword.Training -> []
  -- CR 702.100a is payload-free: the Filter its minted ability carries -- the
  -- entering creature's -- is the ENGINE's, never a card's.
  Keyword.Evolve -> []
  -- CR 702.105a is payload-free too, and names no quality at all: what its minted
  -- ability compares is life totals, which no Filter reaches.
  Keyword.Dethrone -> []
  Keyword.StartYourEngines -> []
  -- CR 702.43a names no quality: the +1/+1 counters and the "target artifact
  -- creature" are written into the replacement effect and the ability
  -- Pawl.Engine.Keyword mints, not into the keyword.
  Keyword.Modular _ -> []
  -- CR 702.79a and CR 702.93a name no quality either: the counter kind and the
  -- "if" clause are written into the abilities Pawl.Engine.Keyword mints.
  Keyword.Persist -> []
  Keyword.Undying -> []
  -- CR 702.63a names no quality: the time counters and the upkeep are written
  -- into the replacement effect and the two abilities Pawl.Engine.Keyword mints,
  -- not into the keyword.
  Keyword.Vanishing _ -> []
  -- CR 702.32a names no quality either, for rule 702.63a's reason above: the fade
  -- counters and the upkeep are in what Pawl.Engine.Keyword mints.
  Keyword.Fading _ -> []
  -- CR 702.68a names no quality: the payload is a NUMBER, and the +N/+0 is
  -- written into the ability Pawl.Engine.Keyword.frenzy mints.
  Keyword.Frenzy _ -> []
  Keyword.Toxic _ -> []

-- CR 118.1: a cost's Filters are its components'; the mana part holds none.
costFilters :: Cost.Type.Cost Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
costFilters = concatMap costComponentFilters . Cost.Type.components

-- CR 118.9: an alternative cost reaches a Filter through its components, as any
-- Cost does, and through the Condition CR 604.2 may gate it with.
alternativeCostFilters :: AlternativeCost.AlternativeCost -> [Filter.Type.Filter Keyword.Keyword]
alternativeCostFilters alternative =
  costFilters (AlternativeCost.cost alternative)
    <> concatMap conditionFilters (Maybe.maybeToList (AlternativeCost.condition alternative))

-- CR 116.2: which spells a printed special action names -- only through the cost
-- CR 116.2d's ignore carries.
specialActionFilters :: SpecialAction.SpecialAction -> [Filter.Type.Filter Keyword.Keyword]
specialActionFilters specialAction = case specialAction of
  SpecialAction.IgnoreThisUntilEndOfTurn cost -> costFilters cost
  -- CR 116.2e names one card and nothing about it.
  SpecialAction.DiscardThisAnyTime -> []

costComponentFilters :: CostComponent.CostComponent Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
costComponentFilters component = case component of
  -- CR 601.2f's "sacrificing permanents": Village Rites' "a creature".
  CostComponent.Sacrifice (Sacrifice.MkSacrifice _ f) -> [f]
  -- CR 702.122a's "other untapped creatures you control".
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower _ f) -> [f]
  -- CR 406.2 as a cost: Headless Skaab's "a creature card from your graveyard".
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard _ f) -> [f]
  -- CR 406.2 again: Circling Vultures' "the top creature card of your
  -- graveyard".
  CostComponent.ExileTopFromGraveyard f -> [f]
  CostComponent.TapThis -> []
  CostComponent.UntapThis -> []
  CostComponent.SacrificeThis -> []
  CostComponent.PayLife _ -> []
  CostComponent.PayLifeX -> []
  CostComponent.DiscardCards _ -> []
  CostComponent.DiscardThis -> []
  CostComponent.PayEnergy _ -> []
  CostComponent.AddLoyaltyToThis _ -> []
  CostComponent.RemoveLoyaltyFromThis _ -> []
  CostComponent.PutPlusOneCountersOnThis _ -> []
  CostComponent.ExileThisFromGraveyard -> []

-- The Filter narrowing a target slot's CR 115 pool -- "target creature with
-- flying" -- and CR 303.4a's enchant slot, which is a TargetSlot too.
targetSlotFilters :: TargetSlot.TargetSlot -> [Filter.Type.Filter Keyword.Keyword]
targetSlotFilters = Maybe.maybeToList . TargetSlot.filter

-- A continuous effect's affected set (Pawl.Types.Affected), wherever one is
-- written -- a static ability, a combat restriction, an attack or block
-- requirement. Only the three predicate arms carry a Filter; the fixed id set
-- (CR 611.2c) and CR 303.4m's "enchanted permanent" carry none.
affectedFilters :: Affected.Affected -> [Filter.Type.Filter Keyword.Keyword]
affectedFilters affected = case affected of
  Affected.TheseObjects _ -> []
  Affected.Matching f -> [f]
  Affected.MatchingAnywhere f -> [f]
  Affected.Attached -> []
  Affected.AttachedPlayerControls f -> [f]

objectRefFilters :: ObjectRef.ObjectRef -> [Filter.Type.Filter Keyword.Keyword]
objectRefFilters ref = case ref of
  ObjectRef.InSlot _ -> []
  -- Day of Judgment's "all creatures", Boil's "all Islands".
  ObjectRef.EachMatching f -> [f]
  -- Rise of the Dark Realms' "all creature cards from all graveyards"; its
  -- PlayerScope names players rather than characteristics, so the Filter is the
  -- whole of what there is to lint.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard _ f) -> [f]
  -- Ignorant Bliss' "all cards from your hand" holds none either: CR 400.2
  -- makes a hand hidden, so the arm carries no Filter to lint.
  ObjectRef.EachCardInYourHand -> []
  -- Hoarding Dragon's "the exiled card" holds none either: CR 607.2a's set is
  -- named by which object exiled the cards, never by their characteristics.
  ObjectRef.EachCardExiledWithSource -> []
  -- Molten Disaster's "each player" holds no Filter to lint.
  ObjectRef.EachPlayer -> []
  -- Count on Luck's "the top card of your library" names a POSITION, so it holds
  -- no Filter either; its PlayerRef names players, and its depth counts cards --
  -- neither is a characteristic.
  ObjectRef.TopOfLibrary {} -> []
  -- Port of Karfell's "a creature card from your graveyard"; its PlayerScope and
  -- its Chooser name players, so the Filter is the whole of what there is to
  -- lint, exactly as for the graveyard sweep above.
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard _ _ f) -> [f]

-- The Filter a Count folds over (CR 608.2h). Delegated to the *Counts family
-- above rather than re-walked: those traversals are already the project's answer
-- to "every Count a card can author", and a Count's Filter is the only Filter it
-- holds. That reuse is also the one seam here that -Werror does not police -- a
-- Count added to a NEW carrier has to be added there, not here.
countFilters :: [Count.Type.Count Quantity.Type.Quantity] -> [Filter.Type.Filter Keyword.Keyword]
countFilters = fmap Count.Type.filter

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
  Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) -> quantityFilters p <> quantityFilters t
  Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) -> quantityFilters p <> quantityFilters t
  Modification.LoseAllAbilities -> []
  Modification.SetLandSubtype _ -> []
  Modification.SetLandSubtypeToChosen -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCreatureSubtype _ -> []
  Modification.AddEveryCreatureSubtype -> []
  Modification.AddCardType _ -> []
  Modification.AddSupertype _ -> []
  Modification.RemoveSupertype _ -> []
  Modification.ChangeSubtypeWord {} -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.AddColor _ -> []
  Modification.AddChosenColor -> []
  Modification.SwitchPowerToughness -> []

-- Four Filter positions, not two: the affected set, each modification's own
-- keywords and Counts, -- since CR 604.2's "as long as" gate landed -- the
-- Counts inside that condition, and the leaves-the-battlefield duration's own,
-- which a CR 611.2b "for as long as" would carry.
staticAbilityFilters :: StaticAbility.StaticAbility -> [Filter.Type.Filter Keyword.Keyword]
staticAbilityFilters ability =
  affectedFilters (StaticAbility.affected ability)
    <> concatMap conditionFilters (Maybe.maybeToList (StaticAbility.condition ability))
    <> concatMap durationFilters (Maybe.maybeToList (StaticAbility.lingers ability))
    <> concatMap modificationFilters (StaticAbility.modifications ability)

-- CR 603.6a's "whenever [a permanent] enters" carries one directly; CR 603.8's
-- state trigger carries one through its Condition's Counts.
triggerConditionFilters :: TriggerCondition.TriggerCondition -> [Filter.Type.Filter Keyword.Keyword]
triggerConditionFilters triggerCondition = case triggerCondition of
  TriggerCondition.PermanentEnters f -> [f]
  -- CR 709.5h names a half by name; nothing about the door is a Filter.
  TriggerCondition.SelfHalfUnlocked _ -> []
  -- CR 709.5i names a PlayerRelation; nothing about it is a Filter.
  TriggerCondition.RoomFullyUnlocked _ -> []
  -- Recursive, for triggerConditionCounts' reason: Balemurk Leech's AnyOf holds a
  -- PermanentEnters, whose Filter would otherwise never be swept.
  TriggerCondition.AnyOf conditions -> concatMap triggerConditionFilters conditions
  -- CR 708.7's condition is nullary, so there is nothing in it to be a Filter.
  TriggerCondition.SelfTurnedFaceUp -> []
  -- Its watcher-scoped sibling carries one, and Aven Farseer's is the trivial
  -- `And []` -- which this sweep must still see, an empty Filter being a Filter.
  TriggerCondition.PermanentTurnedFaceUp f -> [f]
  -- CR 702.112b's carries one too -- Valeron Wardens' "a creature you control".
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated _ f) -> [f]
  TriggerCondition.SelfEvolves -> []
  -- CR 702.134c's carries none either: "equipped creature" is CR 301.5f's one
  -- permanent rather than a class of them, and "a creature" narrows by nothing.
  TriggerCondition.AttachedCreatureMentors -> []
  -- CR 701.21a's is nullary too: "a permanent" names no quality, so unlike
  -- PermanentDies below there is no Filter to sweep.
  TriggerCondition.PermanentSacrificed -> []
  -- CR 603.3b's names a PlayerRelation; the Saga is found through CR 714.2d's
  -- final chapter number rather than through a Filter.
  TriggerCondition.SagaFinalChapterTriggers _ -> []
  TriggerCondition.PermanentDies f -> [f]
  TriggerCondition.StateIs condition -> conditionFilters condition
  TriggerCondition.SelfEnters -> []
  TriggerCondition.StepBegins {} -> []
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  -- Its watcher-scoped sibling carries one -- Tovolar's "a Wolf or Werewolf you
  -- control", which the card lint must sweep.
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> [f]
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []
  TriggerCondition.OpponentLostLifeDuringYourTurn -> []
  TriggerCondition.SelfAttacks _ -> []
  -- CR 702.149a names a quality the OTHER attackers must have, so this one DOES
  -- carry a Filter -- "power greater than this creature's power".
  TriggerCondition.SelfAttacksWithAnother f -> [f]
  -- CR 506.5's condition names a quality the ATTACKER must have, so it carries a
  -- Filter -- rule 702.83a's "a creature you control".
  TriggerCondition.CreatureAttacksAlone f -> [f]
  -- CR 702.105a names no quality of the attacker, only a fact about whom it
  -- attacked, so no Filter.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> []
  TriggerCondition.SelfBlocks -> []
  -- CR 509.3b's condition carries no Filter, so there is none to traverse. The
  -- printings that narrow the attacker are not served yet (#1253).
  TriggerCondition.SelfBlocksCreature -> []
  TriggerCondition.SelfBlocksAtLeast _ -> []
  -- CR 509.3e's filtered form names a quality the attackers blocked must have,
  -- so this one DOES carry a Filter.
  TriggerCondition.SelfBlocksOneOrMore f -> [f]
  TriggerCondition.SelfBecomesBlocked -> []
  -- CR 509.3d names a quality the blocker must have, so this one DOES carry a
  -- Filter -- rule 702.25a's "without flanking".
  TriggerCondition.SelfBecomesBlockedBy f -> [f]
  -- The same rule's attacking-side form, whose Filter is a predicate over the
  -- blockers -- Serra Inquisitors' "black".
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> [f]
  TriggerCondition.SelfAttacksUnblocked -> []
  TriggerCondition.SelfCycled -> []
  TriggerCondition.SelfRevealedForMiracle -> []
  TriggerCondition.PlayerDiscards _ -> []
  TriggerCondition.PlayerDrawsNthCard {} -> []
  -- CR 725.1's crowning condition is a PlayerRelation, which holds no Filter.
  TriggerCondition.PlayerBecomesMonarch _ -> []
  -- CR 603.7's slot-named condition holds a SlotName, which is no Filter -- what
  -- the slot holds was selected by the arming spell's own target slot.
  TriggerCondition.LoseControlOfBound _ -> []
  TriggerCondition.RoomEntered _ -> []
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> []
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> []
  TriggerCondition.SelfDies -> []
  TriggerCondition.SelfLeavesTheBattlefield -> []
  TriggerCondition.HauntedCreatureDies -> []
  TriggerCondition.SpellOrAbilityCounters _ -> []
  TriggerCondition.DamageToPlayerPrevented _ -> []
  TriggerCondition.PlayerGainsLife _ -> []
  TriggerCondition.PlayerLosesLife _ -> []
  -- CR 714.2b carries a counter kind and a Natural, neither of which is a Count.
  TriggerCondition.SelfCountersReached {} -> []
  -- CR 310.12b carries a counter kind alone.
  TriggerCondition.SelfLastCounterRemoved _ -> []
  -- CR 601.2i's "whenever you cast a [type] spell" carries one directly, over
  -- the spell rather than over a permanent.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast f _) -> [f]
  -- "This spell" names the bearer and needs no Filter to say so.
  TriggerCondition.SelfCast -> []
  -- Rule 702.21a names the bearer as well, and asks only a relation of the
  -- targeting object's controller -- no Filter over the object itself.
  TriggerCondition.SelfBecomesTargeted _ -> []

-- CR 613.11: which objects a player effect names -- a cost modifier's (CR
-- 601.2f), a timing permission's (CR 601.3b) or a countering prohibition's (CR
-- 701.6a).
playerEffectFilters :: PlayerEffect.PlayerEffect -> [Filter.Type.Filter Keyword.Keyword]
playerEffectFilters playerEffect = case playerEffect of
  PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost f _) -> [f]
  PlayerEffect.ReduceSpellCost (ReduceSpellCost.MkReduceSpellCost f _) -> [f]
  -- CR 601.2f's other moment: Heartstone's Filter narrows the ability's SOURCE
  -- PERMANENT rather than a spell, and is authored the same way.
  PlayerEffect.ReduceActivationCost (ReduceActivationCost.MkReduceActivationCost f _ _) -> [f]
  -- CR 601.2f's addition carries a Filter in two places: its own criterion
  -- ("nontoken Rebels"), and one inside each component it adds ("sacrifice a
  -- land"). Both are authored by the card, so both are linted, and the inner
  -- ones go through costComponentFilters so an added component and a printed
  -- one are held to one standard.
  PlayerEffect.AddActivationCost (AddActivationCost.MkAddActivationCost f components) -> f : concatMap costComponentFilters components
  PlayerEffect.CantCastSpells -> []
  PlayerEffect.CantCastMoreThan _ -> []
  -- CR 601.3 / 305.1: the quality both prohibitions name is a CardName chosen as
  -- the source entered, which is not a Filter and is not written by the card.
  PlayerEffect.CantCastChosenName -> []
  PlayerEffect.CantPlayLandChosenName -> []
  -- CR 305.2 carries a bare count of extra land plays, not a Filter: it names
  -- how many lands, never which spells.
  PlayerEffect.PlayAdditionalLands _ -> []
  PlayerEffect.NoMaximumHandSize -> []
  -- CR 402.2 carries a bare count of cards for the same reason: it names how many
  -- cards a hand may hold, never which spells.
  PlayerEffect.SetMaximumHandSize _ -> []
  -- CR 500.5 carries a ManaFilter, not a Filter: the set it names is MANA, and
  -- this traversal is about the spells a player effect names.
  PlayerEffect.DontLoseUnspentMana _ -> []
  -- CR 702.18a / 702.11c carry a PlayerScope, not a Filter: the set they name is
  -- players, and this traversal is about the spells a player effect names.
  PlayerEffect.CantBeTargetedBy _ -> []
  -- CR 601.3b's "a spell with certain qualities", which is a Filter over the
  -- spell exactly as a cost modifier's is (Vedalken Orrery's is `And []`).
  PlayerEffect.CastAsThoughItHadFlash f -> [f]
  -- CR 701.6a's "a spell or ability", narrowed by the victim's own qualities
  -- exactly as a cost modifier's is (Spider-Punk's is `And []`, Prowling
  -- Serpopard's is HasCardType Creature).
  PlayerEffect.CantBeCountered f -> [f]
  -- CR 615.12 narrows by a DamagePattern, one of whose three axes IS a Filter
  -- over the damage's source (Excruciator's "by this creature", `IsSource`); the
  -- kind and the recipient beside it are not predicates over an object. The
  -- pattern's authorability is linted by unpreventablePatternOffends below.
  PlayerEffect.DamageCantBePrevented pattern_ -> [DamagePattern.whatSource pattern_]
  -- CR 701.23 names no quality of the libraries it stops being searched.
  PlayerEffect.CantSearchLibraries -> []
  -- CR 725 names no quality either: the designation has no parts (Jared
  -- Carthalion, True Heir).
  PlayerEffect.CantBecomeMonarch -> []
  -- CR 601.3a's Filter half, which is exactly a quality of the spell (Damping
  -- Engine's "artifact, creature, or enchantment spells").
  PlayerEffect.CantCastMatching f -> [f]
  -- CR 305.1's unrestricted prohibition narrows nothing: every land is stopped.
  PlayerEffect.CantPlayLands -> []
  -- CR 601.3's zone permission, narrowed by the card's own qualities exactly as
  -- the timing permission beside it is (Yawgmoth's Will's is `And []`).
  PlayerEffect.CastFromGraveyard f -> [f]
  -- CR 305.1's play-side permission narrows nothing: a land play has already
  -- fixed the card type, and Crucible of Worlds' sentence says no more.
  PlayerEffect.PlayLandsFromGraveyard -> []

-- Does this carrier pair CR 615.12's "damage can't be prevented" with a
-- scope narrower than the whole table?
--
-- Pawl.Engine.PlayerEffect.unpreventable asks no player, because CR 615.12's
-- sentence is about a damage EVENT and names no player to ask about. It gathers
-- from the whole board instead -- "which seats have such an effect applying?" --
-- which admits the same events as "it applies" exactly when the scope is
-- PlayerScope.EachPlayer, and reads a narrower one as board-wide. This lint is
-- what makes that exactness a property of the pool rather than a hope: no card
-- may author the scope the fold cannot see.
--
-- The rule, not just the engine, is what backs the ban. Every printed narrowing
-- of CR 615.12 narrows by a quality of the damage EVENT and not by a player:
-- Excruciator's source, Frenzied Baloth's kind, Questing Beast's source
-- relation, Whippoorwill's recipient. That axis is the DamagePattern the
-- constructor now carries, never a carrier scope -- a scope names which players
-- an effect applies TO, and a damage event between two creatures applies to no
-- player at all.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: a second
-- player effect whose reading depends on its scope must break this build.
unpreventableScopeOffends :: AffectedPlayers.AffectedPlayers SlotName.SlotName -> PlayerEffect.PlayerEffect -> Bool
unpreventableScopeOffends scope playerEffect = case playerEffect of
  -- A NAMED seat is a narrowing like any other, and the strictest one there is:
  -- "target player" reaches one player where CR 615.12 reaches the whole table.
  PlayerEffect.DamageCantBePrevented _ -> scope /= AffectedPlayers.Scoped PlayerScope.EachPlayer
  PlayerEffect.CantSearchLibraries -> False
  PlayerEffect.CantBecomeMonarch -> False
  -- Every other arm IS asked about a player, so its scope is read exactly as
  -- written and any of the three is legitimate: Rule of Law and Thalia say
  -- EachPlayer, Silence's stored prohibition says Opponents, and Prowling
  -- Serpopard says You.
  PlayerEffect.IncreaseSpellCost {} -> False
  PlayerEffect.ReduceSpellCost {} -> False
  PlayerEffect.ReduceActivationCost {} -> False
  PlayerEffect.AddActivationCost {} -> False
  PlayerEffect.CantCastSpells -> False
  PlayerEffect.CantCastMoreThan _ -> False
  PlayerEffect.CantCastChosenName -> False
  PlayerEffect.CantPlayLandChosenName -> False
  PlayerEffect.PlayAdditionalLands _ -> False
  PlayerEffect.NoMaximumHandSize -> False
  PlayerEffect.SetMaximumHandSize _ -> False
  PlayerEffect.DontLoseUnspentMana _ -> False
  PlayerEffect.CantBeTargetedBy _ -> False
  PlayerEffect.CastAsThoughItHadFlash _ -> False
  PlayerEffect.CantBeCountered _ -> False
  PlayerEffect.CantCastMatching _ -> False
  PlayerEffect.CantPlayLands -> False
  PlayerEffect.CastFromGraveyard _ -> False
  PlayerEffect.PlayLandsFromGraveyard -> False

-- The OTHER half of the same carrier, now that CR 615.12's narrowing rides in a
-- DamagePattern: does this card author a field of that pattern the engine bakes?
--
-- `whichRecipient` is the one, and for engineOnlyOffends' reason -- a card
-- cannot name an ObjectId or a PlayerId. Whippoorwill's "damage that would be
-- dealt to THAT CREATURE" does name a recipient, but the creature is the one its
-- resolution chose, so the pattern is the engine's to bake and never the card
-- file's to write. `whichKind`, `whatSource` and `whatRecipient` are all
-- authorable here; the first two are exactly what Frenzied Baloth and
-- Excruciator print, and the third describes a recipient rather than naming
-- one, so no card in the pool writes it on THIS carrier.
--
-- Not implemented: no resolution bakes a recipient into THIS pattern, the way
-- Resolve's prevention arms bake one into a shield's, so the field has no
-- producer on either side yet (#845).
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum.
unpreventablePatternOffends :: PlayerEffect.PlayerEffect -> Bool
unpreventablePatternOffends playerEffect = case playerEffect of
  PlayerEffect.DamageCantBePrevented pattern_ -> Maybe.isJust (DamagePattern.whichRecipient pattern_)
  PlayerEffect.CantSearchLibraries -> False
  PlayerEffect.CantBecomeMonarch -> False
  PlayerEffect.IncreaseSpellCost {} -> False
  PlayerEffect.ReduceSpellCost {} -> False
  PlayerEffect.ReduceActivationCost {} -> False
  PlayerEffect.AddActivationCost {} -> False
  PlayerEffect.CantCastSpells -> False
  PlayerEffect.CantCastMoreThan _ -> False
  PlayerEffect.CantCastChosenName -> False
  PlayerEffect.CantPlayLandChosenName -> False
  PlayerEffect.PlayAdditionalLands _ -> False
  PlayerEffect.NoMaximumHandSize -> False
  PlayerEffect.SetMaximumHandSize _ -> False
  PlayerEffect.DontLoseUnspentMana _ -> False
  PlayerEffect.CantBeTargetedBy _ -> False
  PlayerEffect.CastAsThoughItHadFlash _ -> False
  PlayerEffect.CantBeCountered _ -> False
  PlayerEffect.CantCastMatching _ -> False
  PlayerEffect.CantPlayLands -> False
  PlayerEffect.CastFromGraveyard _ -> False
  PlayerEffect.PlayLandsFromGraveyard -> False

-- The non-vacuity half of both lints above: is this CR 615.12's effect at all?
-- A wildcard is right here, where it is not above -- this asks "did the sweep
-- have anything to look at", not "is it well-formed". isPhaseR's shape.
isUnpreventable :: PlayerEffect.PlayerEffect -> Bool
isUnpreventable playerEffect = case playerEffect of
  PlayerEffect.DamageCantBePrevented _ -> True
  _ -> False

-- The pattern that narrows nothing: Spider-Punk's, and what a fixture below
-- restates a card's effect to when the pattern is not the axis under test.
anyDamage :: DamagePattern.DamagePattern
anyDamage =
  DamagePattern.MkDamagePattern
    { DamagePattern.whichKind = Nothing,
      DamagePattern.whatSource = Filter.Type.And [],
      DamagePattern.whatRecipient = Nothing,
      DamagePattern.whichRecipient = Nothing
    }

-- Every (scope, player effect) pair a card authors, on BOTH of the carriers
-- Pawl.Engine.PlayerEffect.applying folds together: the printed static ability
-- (CR 604.2, Spider-Punk) and the stored one a resolution installs (CR 611.2c,
-- Silence -- and Skullcrack's "damage can't be prevented this turn", whenever
-- the pool gains it).
cardPlayerScopes :: Face.Face Card.Type.Card -> [(AffectedPlayers.AffectedPlayers SlotName.SlotName, PlayerEffect.PlayerEffect)]
cardPlayerScopes card =
  fmap printedPlayerScope (Face.playerAbilities card)
    <> Maybe.mapMaybe storedPlayerScope (cardResolutionEffects card)

-- The printed carrier's pair: the record's two fields, in the order the lint
-- above reads them. Wrapped as Scoped so the two carriers read as one list --
-- a static ability has no slot, so it can never be the other arm.
printedPlayerScope :: PlayerStaticAbility.PlayerStaticAbility -> (AffectedPlayers.AffectedPlayers SlotName.SlotName, PlayerEffect.PlayerEffect)
printedPlayerScope ability = (AffectedPlayers.Scoped (PlayerStaticAbility.scope ability), PlayerStaticAbility.effect ability)

-- The stored carrier's pair, or Nothing for the overwhelming majority of
-- effects, which install no continuous effect on the player axis at all. A
-- wildcard here rather than one arm per effect, matching the control lint's own
-- sweep over this sum: Pawl.Types.Effect is the open half's alphabet, and a new
-- resolution effect is not a new player carrier.
storedPlayerScope :: Effect.Effect Card.Type.Card -> Maybe (AffectedPlayers.AffectedPlayers SlotName.SlotName, PlayerEffect.PlayerEffect)
storedPlayerScope effect = case effect of
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers _ scope playerEffect) -> Just (scope, playerEffect)
  _ -> Nothing

-- The Filters an EntryRewrite carries, on four different axes. CR 201.4a's is the
-- restriction on which cards' names an as-enters name choice may name (Null
-- Chamber's "other than a basic land card name"), a predicate over a CARD in the
-- Oracle card reference rather than over an object on the board -- the same shape
-- Effect.Search's is, and why it belongs in this walk. CR 614.1c's as-enters
-- sacrifice carries one of the ordinary kind, over permanents on the battlefield
-- (Shimatsu the Bloodcloaked's "any number of permanents"). CR 614.1c's as-enters
-- reveal carries a third, over a CARD IN A HAND (Rustic Clachan's "a Kithkin
-- card"). CR 707.5's copy choice carries a fourth, over permanents on the
-- battlefield (Copy Enchantment's "any enchantment"). None of the four is framed.
entryRewriteFilters :: EntryRewrite.EntryRewrite -> [Filter.Type.Filter Keyword.Keyword]
entryRewriteFilters entryRewrite = case entryRewrite of
  EntryRewrite.ChooseCardNames f -> [f]
  EntryRewrite.RevealOrTapped f -> [f]
  -- CR 707.5's eligible set -- Clone's "any creature", Copy Enchantment's "any
  -- enchantment" -- is a criterion over permanents on the battlefield, so it
  -- belongs in this walk. CR 707.9's exceptions beside it carry no Filter: an
  -- "except ..." clause states values, never a criterion over objects
  -- (Pawl.Types.CopyException imports no Filter, which is what keeps that
  -- honest).
  EntryRewrite.AsCopy (AsCopy.MkAsCopy f _) -> [f]
  EntryRewrite.ChoiceOf _ -> []
  EntryRewrite.ChooseColor -> []
  EntryRewrite.ChooseBasicLandType -> []
  EntryRewrite.WithCounters {} -> []
  EntryRewrite.UnderSourceControl -> []
  EntryRewrite.Riot -> []
  EntryRewrite.Unleash -> []
  EntryRewrite.Bloodthirst _ -> []
  EntryRewrite.Tapped -> []
  EntryRewrite.PayLifeOrTapped _ -> []
  EntryRewrite.EntersTransformed -> []
  EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber f _) -> [f]

-- The Filter a TurnUpRewrite carries. CR 303.4k's destination text -- Gift of
-- Doom's "you may attach it to a creature" -- and NOT framed, even though an
-- attach is what it describes: the enchant-ability narrowing is added by
-- Pawl.Engine.Attach.turnUpHosts because rule 303.4k mandates it, so a card
-- writing Filter.CanHostSubject here would be stating a rule rather than its own
-- text. That is why this list feeds `unframed` with every other position.
turnUpRewriteFilters :: TurnUpRewrite.TurnUpRewrite -> [Filter.Type.Filter Keyword.Keyword]
turnUpRewriteFilters turnUpRewrite = case turnUpRewrite of
  TurnUpRewrite.WithCounters {} -> []
  TurnUpRewrite.MayAttachTo f -> [f]

-- CR 614.1c-e: four replacement patterns narrow by a Filter. CounterPattern.onWhat
-- is "one or more counters would be put on a creature YOU control"; EntryR's
-- whole pattern is one -- CR 614.1c's "as [THIS PERMANENT] enters" (Filter.IsSource)
-- and CR 614.1d's "[Objects] enter [the battlefield] . . ." (Gather Specimens'
-- creature clause) -- and TurnUpR's is CR 614.1e's "as [THIS PERMANENT] is turned
-- face up"; and DamagePattern.whatSource is CR 615.1's shield naming the source it
-- watches, by characteristic (Luminesce) or by identity (Galvanic Blast). The
-- other three narrow by zone, destruction, token or phase, none of which holds
-- one.
--
-- EntryR's and TurnUpR's REWRITES hold one too, on a second axis: each pattern
-- says which objects the replacement applies to, and entryRewriteFilters and
-- turnUpRewriteFilters above say which cards a name choice inside it may name,
-- which permanents an as-enters sacrifice may take, and where CR 303.4k's
-- attachment may land.
replacementEffectFilters :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Type.Card) -> [Filter.Type.Filter Keyword.Keyword]
replacementEffectFilters replacementEffect = case replacementEffect of
  ReplacementEffect.CounterR (CounterR.MkCounterR counterPattern _) -> [CounterPattern.onWhat counterPattern]
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR zoneChangePattern _) -> [ZoneChangePattern.whatObject zoneChangePattern]
  ReplacementEffect.EntryR (EntryR.MkEntryR entryPattern entryRewrite) -> entryPattern : entryRewriteFilters entryRewrite
  -- CR 615.1's shields narrow by their source, which is a Filter over the object
  -- dealing the damage (Luminesce's "black sources and red sources", Galvanic
  -- Blast's `IsSource`), and by their printed RECIPIENT, which is a second
  -- Filter over the object being dealt to (Stormwild Capridor's `IsSource`). The
  -- kind and the baked recipient beside them are not Filters.
  ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ _) ->
    DamagePattern.whatSource damagePattern : Maybe.maybeToList (DamagePattern.whatRecipient damagePattern)
  ReplacementEffect.DestructionR _ -> []
  ReplacementEffect.TokenR {} -> []
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR turnUpPattern turnUpRewrite) -> turnUpPattern : turnUpRewriteFilters turnUpRewrite
  ReplacementEffect.PhaseR _ -> []

-- Both the subject and CR 508.1c's "unless some condition is met": Blind-Spot
-- Giant's gate carries `Not IsSource`, which is as much card data as the affected
-- set beside it.
--
-- The SIZE-BOUNDING arms have no subject, so they contribute only their gate.
-- Nothing stands in for the missing Affected on purpose: a `Matching Anything`
-- there would report a filter Silent Arbiter does not print.
combatRestrictionFilters :: CombatRestriction.CombatRestriction -> [Filter.Type.Filter Keyword.Keyword]
combatRestrictionFilters restriction = case restriction of
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless affected condition) -> affectedFilters affected <> foldMap conditionFilters condition
  CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless affected condition) -> affectedFilters affected <> foldMap conditionFilters condition
  -- Three positions on the PAIRWISE arm: the attackers restricted, the blockers
  -- barred from them, and the gate.
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy affected blockers condition) -> affectedFilters affected <> [blockers] <> foldMap conditionFilters condition
  CombatRestriction.CantAttackAlone (AffectedUnless.MkAffectedUnless affected condition) -> affectedFilters affected <> foldMap conditionFilters condition
  CombatRestriction.CantAttackMoreThan (LimitUnless.MkLimitUnless _ condition) -> foldMap conditionFilters condition
  CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless _ condition) -> foldMap conditionFilters condition

-- All three of a blocking permission's Filter positions: the subject it names, CR
-- 604.2's "as long as" gate beside it (Entourage of Trest), and the counted arity
-- (Kemba's Legion).
blockPermissionFilters :: BlockPermission.BlockPermission -> [Filter.Type.Filter Keyword.Keyword]
blockPermissionFilters permission =
  affectedFilters (BlockPermission.affected permission)
    <> foldMap quantityFilters (BlockPermission.additional permission)
    <> foldMap conditionFilters (BlockPermission.while permission)

-- Tag a Filter position as UNFRAMED -- one no attach supplies a subject for,
-- which is every position in the type except the one below.
unframed :: [Filter.Type.Filter Keyword.Keyword] -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
unframed = fmap ((,) False)

-- Every Filter one effect carries, paired with whether an ATTACH frames it.
-- Exactly one arm answers True: Effect.AttachTarget's destination, which is the
-- only CARD-AUTHORED Filter position evaluated against a view whose
-- `canHostSubject` is filled in (Pawl.Engine.Attach.hostsFor, from
-- attachmentFor). TurnUpRewrite.MayAttachTo reaches the same evaluator and is
-- still unframed, deliberately: CR 303.4k's enchant-ability conjunct is added by
-- Attach.turnUpHosts because the rule mandates it, so the atom appearing in that
-- card's data would be a card restating a rule. Everywhere else the field is
-- False by construction --
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
  Effect.AttachTarget (AttachTarget.MkAttachTarget _ f) -> [(True, f)]
  Effect.DealDamage (DealDamage.MkDealDamage ref quantity) -> unframed (objectRefFilters ref <> quantityFilters quantity)
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    unframed (durationFilters duration <> modificationFilters modification <> objectRefFilters ref)
  Effect.ChangeText {} -> []
  Effect.AddMana _ -> []
  Effect.Search (Search.MkSearch _ _ _ f _) -> unframed [f]
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  -- Only the count's Filters: rule 701.39a describes the candidate pool, so no
  -- Filter on the card names it.
  Effect.Bolster quantity -> unframed (quantityFilters quantity)
  -- Only the count's Filters: rule 701.47a describes the candidate pool, so no
  -- Filter on the card names it.
  Effect.Amass (Amass.MkAmass quantity _) -> unframed (quantityFilters quantity)
  -- Only the count's Filters: rule 701.68a describes the candidate pool, so no
  -- Filter on the card names it.
  Effect.Blight quantity -> unframed (quantityFilters quantity)
  Effect.TemptWithTheRing -> []
  Effect.Venture -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ f quantity) -> unframed (f : quantityFilters quantity)
  Effect.RestartGame -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy (Destroy.MkDestroy ref _ _) -> unframed (objectRefFilters ref)
  Effect.Sacrifice _ -> []
  -- The riders reach a Filter one level further down than the ObjectRef: CR
  -- 122.6a's counters are keyed by CounterKind, and CR 122.1b's keyword counter
  -- carries a whole Keyword. Swept for the reason canHostSubjects sweeps the
  -- same shape -- the lint is about the positions a card author can write, not
  -- about which of them the pool has used.
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ riders _ _ _) -> unframed (objectRefFilters ref <> concatMap counterKindFilters (Map.keys (EntryRiders.counters riders)))
  Effect.Draw (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  -- The tally's Filter is a position a card author writes, so the lint reaches
  -- it: rule 728.1's "nonland card" is one of these.
  Effect.Mill (Mill.MkMill _ quantity mTally) -> unframed (quantityFilters quantity <> fmap MillTally.filter (Maybe.maybeToList mTally))
  -- The ObjectRef's Filter is a position a card author writes, so the lint
  -- reaches it, as Explore's does.
  Effect.LookAt (LookAt.MkLookAt ref _) -> unframed (objectRefFilters ref)
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  -- The ObjectRef's Filter is a position a card author writes, so the lint
  -- reaches it, as PutCounters' does.
  Effect.Explore ref -> unframed (objectRefFilters ref)
  Effect.Discard (Discard.MkDiscard _ quantity) -> unframed (quantityFilters quantity)
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> unframed (quantityFilters quantity)
  Effect.DecreaseSpeed d -> unframed (quantityFilters (SpeedDecrease.quantity d))
  -- CR 111.1's token is a whole card, and every Filter position it has is one a
  -- card author can write -- the same nesting Pawl.Codec's round trip walks.
  Effect.Create (Create.MkCreate quantity card riders _) -> unframed (quantityFilters quantity <> concatMap counterKindFilters (Map.keys (EntryRiders.counters riders))) <> overFaces cardFilters card
  -- An EachMatching ref's Filter is card text like RequireBlock's below, and the
  -- count's Filters are as much card text as Create's.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref) -> unframed (quantityFilters quantity <> objectRefFilters ref)
  Effect.Replace (Replace.MkReplace duration _ _ condition replacement) -> unframed (durationFilters duration <> foldMap conditionFilters condition <> replacementEffectFilters replacement) <> concatMap effectFilters (replacementEffectRiders replacement)
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- The rider's Filters too, for CR 615.5. This is the traversal that dropped
  -- landwalk's payload once, so a nested effect list is exactly what it must not
  -- stop at.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration ref quantity rider) ->
    unframed (durationFilters duration <> objectRefFilters ref <> quantityFilters quantity) <> concatMap effectFilters rider
  Effect.PreventAllDamage (DurationRef.MkDurationRef duration ref) -> unframed (durationFilters duration <> objectRefFilters ref)
  -- BOTH refs, or a Filter inside a redirect's destination escapes this lint.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ srcRef destRef) ->
    unframed (durationFilters duration <> objectRefFilters srcRef <> objectRefFilters destRef)
  Effect.TurnFaceDown _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter _ -> []
  -- BOTH positions: the ObjectRef carries Renegade Krasis' "each other creature
  -- you control with a +1/+1 counter on it", and a Filter there would otherwise
  -- escape the lint.
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity ref) -> unframed (quantityFilters quantity <> objectRefFilters ref)
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> unframed (quantityFilters quantity)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> unframed (quantityFilters quantity)
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> unframed (quantityFilters quantity)
  Effect.Tap ref -> unframed (objectRefFilters ref)
  Effect.Untap ref -> unframed (objectRefFilters ref)
  Effect.Transform ref -> unframed (objectRefFilters ref)
  Effect.AddPhases _ -> []
  Effect.GainControl (DurationRef.MkDurationRef duration ref) -> unframed (durationFilters duration <> objectRefFilters ref)
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger _ _ mDuration) -> unframed (concatMap durationFilters (Maybe.maybeToList mDuration))
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ playerEffect) -> unframed (durationFilters duration <> playerEffectFilters playerEffect)
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration blocker attacker) -> unframed (durationFilters duration <> objectRefFilters blocker <> objectRefFilters attacker)
  -- CR 114.2's emblem is a whole card too.
  Effect.CreateEmblem card -> overFaces cardFilters card
  Effect.BecomeMonarch _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.Unsuspect ref -> unframed (objectRefFilters ref)
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  -- CR 701.3's other attach, which moves the SOURCE rather than a target and
  -- carries no destination filter at all.
  Effect.Attach _ -> []
  Effect.PlaySubgame _ -> []
  Effect.ChooseOpponent _ -> []
  Effect.TakeExtraTurn (TakeExtraTurn.MkTakeExtraTurn _ _) -> []
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary _ ref) -> unframed (objectRefFilters ref)
  Effect.OfferCast {} -> []
  -- Both, as GainControl's arm does: the Duration's Condition carries Victor
  -- Mancha, Runaway's IsSource and ControlledBy, and an empty list here would
  -- take them out of the lint without failing anything.
  Effect.GrantPlayFromExile grant -> unframed (durationFilters (GrantPlayFromExile.duration grant) <> objectRefFilters (GrantPlayFromExile.ref grant))
  -- The swept ref's Filters AND the body's, the rider's shape: a nested effect
  -- list is exactly what this traversal must not stop at.
  Effect.ForEach (ForEach.MkForEach ref _ body) -> unframed (objectRefFilters ref) <> concatMap effectFilters body

-- Per MODE rather than through Modal.allTargetSlots, which is a Map.unions and so
-- collapses two modes declaring the same slot name (#475) -- the cross-check
-- below counts occurrences, and a collapse there would read as a Filter this
-- traversal cannot see.
modalFilters :: Modal.Modal Card.Type.Card -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
modalFilters modal =
  concatMap
    ( \mode ->
        concatMap effectFilters (Mode.allEffects mode)
          <> unframed (concatMap conditionFilters (modeClauseConditions mode))
          <> unframed (concatMap targetSlotFilters (Map.elems (Mode.targetSlots mode)))
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
  unframed
    ( costFilters (ActivatedAbility.cost ability)
        -- CR 702.178a's "as long as" gate, the triggeredAbilityFilters
        -- treatment of CR 603.4's intervening "if" one field over.
        <> concatMap conditionFilters (Maybe.maybeToList (ActivatedAbility.condition ability))
    )
    <> modalFilters (ActivatedAbility.modal ability)

-- EVERY Filter position reachable from a card, each paired with whether an attach
-- frames it. Twenty-four of Pawl.Types.Face's thirty-three fields can hold one, and
-- here is where each one's comes from:
--
--   * `keywords` -- CR 702.29e typecycling (Ash Barrens' landcycling).
--   * `power`, `toughness`, `characteristicPT` -- CR 208.2's printed star,
--     through a Count.
--   * `staticAbilities` -- the affected set, CR 604.2's "as long as" condition,
--     and the layer-6/7 modifications' own keywords and Counts.
--   * `replacementEffects` -- CR 614.1's counter-placement pattern.
--   * `enchant` -- CR 303.4a's enchant ability, a TargetSlot.
--   * `additionalCosts` -- CR 601.2f's sacrifice component.
--   * `alternativeCosts` -- that same component, plus CR 604.2's "as long as"
--     condition gating one.
--   * `specialActions` -- CR 116.2d's ignore cost, a Cost like the two above.
--   * `playerAbilities` -- CR 613.11's cost modifiers and CR 601.3b's timing
--     permission.
--   * `combatRestrictions` (CR 508.1c / 509.1b), `sacrificeRestrictions` (CR
--     701.21a / 101.2), `untapRestrictions` (CR 502.3 / 101.2),
--     `attackRequirements` (CR 508.1d), `blockRequirements`
--     (CR 509.1c) and `attackCosts` (CR 508.1h) -- six more affected sets.
--   * `spell`, `activatedAbilities`, `triggeredAbilities`, `delayedAbilities` --
--     every mode's target slots and effects, plus an activation cost, a
--     trigger's own condition and its intervening clause.
--   * `mulliganActions` (CR 103.5b) and `openingHandActions` (CR 103.6) -- the two
--     pregame actions, which `cardResolutionEffects` above does not reach.
--
-- The other nine fields hold none: `name`, `manaCost`, `typeLine`, `loyalty`,
-- `defense`, `colorIndicator`, `counterability`, `castingPermissions` and
-- `castingRestrictions`. That is checkable rather than
-- asserted: exactly sixteen modules under Pawl.Types import Pawl.Types.Filter --
-- Affected, CostComponent, Count, CounterPattern, Effect, EntryRewrite, Keyword,
-- MillTally, ObjectRef, PlayerEffect, Prompt, ReplacementEffect, TargetSlot,
-- TriggerCondition, TurnUpRewrite and ZoneChangePattern -- and nothing those nine
-- fields reach is one of them.
--
-- Twenty-four and nine is thirty-three, the whole record.
--
-- Every case BELOW this function is exhaustive with no catch-all, so a new
-- constructor on any of those types fails to compile until it is classified. This
-- record fold is the exception, exactly as cardCounts' own caveat says: a NEW
-- Face field that can hold a Filter would bypass it silently. That is what the
-- codec cross-check in canHostSubjectOffends is for.
cardFilters :: Face.Face Card.Type.Card -> [(Bool, Filter.Type.Filter Keyword.Keyword)]
cardFilters card =
  unframed
    ( concatMap keywordFilters (Set.toList (Face.keywords card))
        <> concatMap quantityFilters (Maybe.maybeToList (Face.characteristicPT card))
        <> concatMap (\(Power.MkPower quantity) -> quantityFilters quantity) (Maybe.maybeToList (Face.power card))
        <> concatMap (\(Toughness.MkToughness quantity) -> quantityFilters quantity) (Maybe.maybeToList (Face.toughness card))
        <> concatMap staticAbilityFilters (Face.staticAbilities card)
        <> concatMap replacementEffectFilters (Face.replacementEffects card)
        <> concatMap targetSlotFilters (Face.enchant card)
        <> concatMap costComponentFilters (Face.additionalCosts card)
        <> concatMap alternativeCostFilters (Face.alternativeCosts card)
        <> concatMap specialActionFilters (Face.specialActions card)
        <> concatMap (playerEffectFilters . PlayerStaticAbility.effect) (Face.playerAbilities card)
        <> concatMap (affectedFilters . BlockRequirement.attacker) (Face.blockRequirements card)
        <> concatMap blockPermissionFilters (Face.blockPermissions card)
        <> concatMap (affectedFilters . AttackRequirement.subject) (Face.attackRequirements card)
        <> concatMap (affectedFilters . AttackCost.subject) (Face.attackCosts card)
        <> concatMap combatRestrictionFilters (Face.combatRestrictions card)
        <> concatMap (affectedFilters . SacrificeRestriction.affected) (Face.sacrificeRestrictions card)
        <> concatMap (affectedFilters . UntapRestriction.affected) (Face.untapRestrictions card)
    )
    <> modalFilters (Face.spell card)
    <> concatMap activatedAbilityFilters (Face.activatedAbilities card)
    <> concatMap triggeredAbilityFilters (Face.triggeredAbilities card)
    <> concatMap triggeredAbilityFilters (Map.elems (Face.delayedAbilities card))
    <> concatMap (modalFilters . DungeonRoom.ability) (Face.rooms card)
    <> concatMap (concatMap effectFilters) (Face.mulliganActions card)
    <> concatMap (concatMap effectFilters) (Face.openingHandActions card)

-- How many CR 701.3a atoms this card carries in an Effect.AttachTarget's
-- destination filter, and how many anywhere else. The second number is the
-- offence; the first is what Aura Graft legitimately has one of.
canHostSubjectCounts :: Face.Face Card.Type.Card -> (Int, Int)
canHostSubjectCounts card =
  let total wanted = sum [canHostSubjects f | (framed, f) <- cardFilters card, framed == wanted]
   in (total True, total False)

-- Every occurrence of one atom's codec tag in an ENCODED face. The completeness
-- witness for the traversal above: Pawl.Codec.Face.toJson visits every field
-- of a Face and every type under it, is round-tripped by
-- Pawl.CodecIntegrationSpec's "honesty round-trip over allPrintings", and was
-- written for another purpose entirely -- so a Filter position cardFilters forgets
-- is one this still sees.
--
-- A tag and not a name: Pawl.Codec.Filter spells a nullary atom
-- `Common.nullary "CanHostSubject"`, so the only string equal to one of these in
-- a card's encoding is that tag (a card NAMED "CanHostSubject" would be a false
-- positive, and a loud one rather than a silent miss).
--
-- Parameterized because two atoms want it: CR 701.3a's, counted here for the
-- traversal cross-check, and CR 702.134a's Filter.PowerLessThanSource, which no
-- card may carry at all.
jsonAtoms :: Text.Text -> Value.Value -> Int
jsonAtoms tag value = case value of
  Value.String s -> if String.unwrap s == tag then 1 else 0
  Value.Array a -> sum (fmap (jsonAtoms tag) (Array.unwrap a))
  Value.Object o -> sum (fmap (jsonAtoms tag . Pair.value) (Object.unwrap o))
  Value.Null _ -> 0
  Value.Boolean _ -> 0
  Value.Number _ -> 0

-- CR 701.3a is answerable only where an attach FRAMES the match, and
-- Filter.CanHostSubject is vacuously False in every other Filter position. A card
-- author who wrote it into a target slot, a static ability's affected set, a Count
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
-- The second is not hypothetical maintenance theatre: cardFilters' Face-record
-- fold is hand-maintained, and a new field holding a Filter is exactly the kind of
-- change that would otherwise make this lint quietly stop doing its job.
canHostSubjectOffends :: Face.Face Card.Type.Card -> Bool
canHostSubjectOffends card =
  let (framed, unframedCount) = canHostSubjectCounts card
   in unframedCount /= 0 || framed + unframedCount /= jsonAtoms (Text.pack "CanHostSubject") (Codec.encode (Face.Codec.codec Card.codec) card)

-- A lint fixture built as a FACE, put back into the one-face card an
-- Effect.Create's token payload has to be.
oneFaced :: Face.Face Card.Type.Card -> Card.Type.Card
oneFaced face = Card.Type.MkCard {Card.Type.layout = Layout.Normal, Card.Type.faces = NonEmpty.singleton face}

-- CR 709.2 / 712.8: every lint below is stated about ONE face's printed text,
-- and a card offends when ANY of its faces does. Every card in the pool but
-- Wax // Wane has exactly one face, so this mostly fans out over a singleton --
-- and it is what keeps the second half of a card going unlinted from being
-- possible at all.
anyFace :: (Face.Face Card.Type.Card -> Bool) -> Card.Type.Card -> Bool
anyFace p = any p . Card.Type.faces

-- The same fanout for a lint that GATHERS rather than decides.
overFaces :: (Face.Face Card.Type.Card -> [a]) -> Card.Type.Card -> [a]
overFaces f = concatMap f . NonEmpty.toList . Card.Type.faces

-- CR 709.4a: a card's faces are referred to BY NAME (Card.faceNamed), so two
-- faces sharing a name make that reference ambiguous -- faceNamed would return
-- the FIRST of them and silently hide the second. Over the whole card rather
-- than through anyFace: this is a claim about the SET of names a card prints,
-- which no per-face predicate can state.
distinctFaceNamesOffends :: Card.Type.Card -> Bool
distinctFaceNamesOffends card =
  let names = fmap Face.name (NonEmpty.toList (Card.Type.faces card))
   in length (List.nub names) /= length names

-- CR 709.5a: "Each half of a split card with a shared type line shares the types
-- and subtypes listed on that card's shared type line." pawl stores that
-- literally -- both faces of a Room carry the whole line -- and
-- Pawl.Engine.Card.roomFace deliberately does not subtract it, citing the rule.
-- Nothing else enforces the duplication: a Room whose faces disagreed would load
-- without complaint, and Pawl.Engine.Card.unionTypeLines (set union) would merge
-- the disagreement into a line NEITHER face prints.
--
-- Which cards the claim is about is Card.hasSharedTypeLine's answer rather than
-- a `== Layout.Room` here, so that the lint and the engine's own subtraction
-- range over exactly the same cards -- and so that a new layout has to state
-- whether its faces share a line in the one place -Werror already asks.
--
-- Full type-line equality, not just the two sets CR 709.5a names. The types and
-- subtypes are 709.5a's; the supertypes come from CR 709.5's premise instead --
-- "permanent cards with a single shared type line" is one printed line, and a
-- supertype on it is on it for both halves. No printed Room has a supertype, so
-- the stricter reading costs the corpus nothing and is the one that keeps two
-- stored copies of one line honest.
--
-- Over the whole card rather than through anyFace, for distinctFaceNamesOffends'
-- reason: this is a claim about the faces as a set.
sharedTypeLineOffends :: Card.Type.Card -> Bool
sharedTypeLineOffends card =
  let lines_ = fmap Face.typeLine (Card.Type.faces card)
   in Card.hasSharedTypeLine card && any (/= NonEmpty.head lines_) lines_

-- Two things a TriggerCondition.AnyOf may not contain, checked at every depth so
-- that a nested one cannot smuggle either in.
--
-- A CR 603.8 STATE trigger, because the two kinds of condition are gathered by
-- different scans: Pawl.Engine.Event.stateTriggers walks the battlefield asking
-- whether the state holds, and Event.matchesTrigger walks the event log. An
-- ability that was both would be gathered by stateTriggers whenever any clause
-- held (that arm answers `any`), and CR 603.8's "not again until the ability has
-- left the stack" would then hold back the EVENT clauses too. Nothing else in the
-- tree catches this: both classifications compile, and each is individually
-- defensible.
--
-- A NESTED AnyOf, because a flat list says everything a nested one could and the
-- nesting only multiplies the shapes the classifications above have to be right
-- for.
anyOfOffends :: TriggerCondition.TriggerCondition -> Bool
anyOfOffends condition = case condition of
  TriggerCondition.AnyOf conditions -> any inside conditions || any anyOfOffends conditions
  _ -> False
  where
    inside c = case c of
      TriggerCondition.StateIs _ -> True
      TriggerCondition.AnyOf _ -> True
      _ -> False

-- Every claim pawl makes about how its own card files are authored, swept over
-- the whole corpus. A sweep alone proves nothing about the lint it runs -- a
-- correctly authored pool passes a lint that never fires -- so most cases here
-- pair their sweep with a hand-built offender proving the REJECTING direction.
lintSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
lintSpec s registry = Spec.describe s "Lint" $ do
  -- The SPELL half of the D4 dataflow lint: every slot an effect reads is
  -- declared, and every declared slot is read. Equality, not subset: a slot no
  -- effect reads is a card announcing a target it ignores -- representable in
  -- Magic, not in this pool. Loosen to superset if such a card ever lands.
  --
  -- The ability carriers get the same equality through the same
  -- modalSlotsOffend, in the three sweeps further down (#1043).
  Spec.it s "every mode's slot reads equal its declared slots" $ do
    ps <- S.allPrintings s
    let -- What CASTING binds rather than a target slot declaring it, subtracted
        -- from the READ side. It cannot be added to the declared side instead:
        -- the "no reserved binding slot is ever a declared target slot" sweep
        -- below forbids a card declaring either of these, so the two rules would
        -- be mutually unsatisfiable.
        --
        --   * Binding.you, unconditionally. CR 109.5's first sentence covers a
        --     spell ("the object's controller, its would-be controller (if a
        --     player is attempting to play, cast, or activate it)"), and
        --     Pawl.Engine.Cast.castSpell stamps the caster for EVERY spell at CR
        --     601.2i -- so Char's "and 2 damage to you" is a slot read on a spell
        --     exactly as Brothers of Fire's is on an activated ability.
        --   * Binding.variableX, and only when the cost declares an X -- in
        --     either half of it, per `declaresVariable`. CR 601.2b's X is an
        --     ordinary slot read since #14 retired Quantity.X, so it arrives here
        --     like any other -- but casting binds it, so it belongs here exactly
        --     when the cost declares it. That IS the "reads X iff the cost
        --     declares X" lint, now falling out of the ordinary comparison
        --     instead of needing its own pass. activatedAbilityOffends says the
        --     same thing about an activation cost.
        cardOffends card =
          let castBound =
                if any declaresVariable (spellCostsOf card)
                  then Set.fromList [Binding.you, Binding.variableX]
                  else Set.singleton Binding.you
           in modalSlotsOffend castBound (Face.spell card)
        offenders =
          filter
            (anyFace cardOffends . Printing.card)
            ps
    Spec.assertEqWith s "no dangling or unused slots" (fmap (S.nameOf . Printing.card) offenders) []
  -- The D4 lint above is strictly per mode, so two modes of one card sharing a
  -- slot NAME pass it. This is the missing half, and the check both
  -- Modal.allTargetSlots and Modal.modesTargetSlots now name in their own
  -- comments as the thing that lets them union safely (#475). See
  -- cardSlotNamesCollide for what a shared name silently does.
  Spec.it s "no card's modes share a target slot name" $ do
    ps <- S.allPrintings s
    let declaring modal =
          length (filter (not . Map.null . Mode.targetSlots) (Foldable.toList (Modal.modes modal)))
        -- Every modal cardSlotNamesCollide sweeps, so the guard below ranges over
        -- the same four scopes the lint does rather than over the spell alone.
        modalsOf card =
          Face.spell card
            : fmap ActivatedAbility.modal (Face.activatedAbilities card)
              <> fmap TriggeredAbility.modal (Face.triggeredAbilities card)
              <> fmap TriggeredAbility.modal (Map.elems (Face.delayedAbilities card))
              <> fmap DungeonRoom.ability (Foldable.toList (Face.rooms card))
        offenders = filter (anyFace cardSlotNamesCollide . Printing.card) ps
    -- Guards against passing vacuously: a pool whose every modal had at most one
    -- slot-declaring mode could not collide whatever the lint said. Dream's Grip
    -- is the spell that makes it real; Aether Channeler's triggered ability is
    -- the multi-mode ability nearest to it, with one declaring mode of three.
    Spec.assertBool s (any (any ((> 1) . declaring) . overFaces modalsOf . Printing.card) ps) "the pool has a modal with two slot-declaring modes"
    Spec.assertEqWith s "no fused mode slot" (fmap (S.nameOf . Printing.card) offenders) []
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
        collides = anyFace cardSlotNamesCollide . Printing.card
        -- Dream's Grip's own two modes, renamed to one shared slot: the exact
        -- authoring the card avoids, and the CR 702.42a fusion it would cause.
        face = S.combinedFace dreamsGrip
        fuse mode = mode {Mode.targetSlots = Map.mapKeys (const creature) (Mode.targetSlots mode)}
        fused = face {Face.spell = (Face.spell face) {Modal.modes = fmap fuse (Modal.modes (Face.spell face))}}
    Spec.assertBool s (cardSlotNamesCollide (face {Face.activatedAbilities = [shared]})) "two modes sharing one name are rejected"
    Spec.assertBool s (not (cardSlotNamesCollide (face {Face.activatedAbilities = [distinct]}))) "and two modes naming distinct slots are accepted"
    Spec.assertBool s (cardSlotNamesCollide fused) "Dream's Grip with both modes on one slot is rejected"
    Spec.assertBool s (not (collides dreamsGrip)) "and the real card, naming them 'tapped' and 'untapped', is accepted"
  -- The filing convention, now that no lookup enforces it (#649): a file's stem
  -- must be the slug Registry.filedAs derives from the card inside it.
  --
  -- A stray file, a file whose card was renamed, and a file that no test
  -- happens to name all fail here. A hand-kept list is exactly what forgets
  -- the file nobody loads.
  Spec.it s "every file in data/cards is filed under its card's joined face names" $ do
    root <- Registry.defaultRoot
    loaded <- Registry.loadRoot root
    Spec.assertBool s (not (null loaded)) "the corpus is not empty"
    let stemOf path =
          let file = reverse (takeWhile (/= '/') (reverse path))
           in reverse (drop (length ".json") (reverse file))
        offends (path, result) = case result of
          Left reason -> Just (path <> ": " <> Text.unpack reason)
          Right card ->
            let belongs = Registry.filedAs card
             in if Slug.fromText (Text.pack (stemOf path)) == belongs
                  then Nothing
                  else Just (path <> ": belongs at " <> Text.unpack (Slug.unwrap belongs) <> ".json")
    Spec.assertEqWith s "every file is filed under its own name" (Maybe.mapMaybe offends loaded) []
  -- The other direction: the sweep above SLUGIFIES the stem before comparing
  -- it to Registry.filedAs, so a committed Wax-Wane.json would still pass it --
  -- Slug.fromText normalizes rather than validates, folding case away before
  -- the comparison ever runs. This case is the only thing pinning the RAW stem
  -- itself to already be a slug, i.e. that Slug.fromText is the identity on
  -- every stem -- read the listing directly, because Registry.loadRoot yields
  -- paths rather than the raw stems this needs.
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
  Spec.it s "the lint itself catches a dangling reference" $
    let bad = Map.keysSet (Resolve.slotsOf (Effect.DealDamage (DealDamage.MkDealDamage (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "ghost"))) (Quantity.Type.Literal 3))))
     in Spec.assertBool s (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSlot.TargetSlot)) "misauthored card detected"
  -- The SPELL half of CR 601.2b's contract: what a card's own modes read is
  -- announced against the card's own cost -- mana cost, additional costs and
  -- alternative costs together (`spellCostsOf`), since CR 107.3a names all of them.
  Spec.it s "every printing that reads X declares X, and vice versa" $ do
    ps <- S.allPrintings s
    let readsX c = Resolve.readsX (Card.allEffects c)
        offenders =
          filter
            (anyFace (\f -> readsX f /= any declaresVariable (spellCostsOf f)) . Printing.card)
            ps
    Spec.assertEqWith s "X read iff X declared" (fmap (S.nameOf . Printing.card) offenders) []
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
    let abilitiesOf p = fmap ((,) (Face.name (S.combinedFace p))) (Face.activatedAbilities (S.combinedFace p))
        abilities = concatMap abilitiesOf ps
        offends (_, ab) =
          Resolve.readsX (Modal.allEffects (ActivatedAbility.modal ab))
            /= declaresVariable (ActivatedAbility.cost ab)
    -- Guards the sweep against passing vacuously, in both directions: an empty
    -- pool of abilities, and a pool in which no activation cost declares an X at
    -- all (where the lint would hold for every card by agreeing on False).
    Spec.assertBool s (not (null abilities)) "the pool has activated abilities"
    Spec.assertBool s (any (declaresVariable . ActivatedAbility.cost . snd) abilities) "and one of them declares an X"
    Spec.assertEqWith s "X read iff X declared" (fmap fst (filter offends abilities)) []
  Spec.it s "CR 111.4 every token a card creates is named its subtypes plus \"Token\"" $ do
    ps <- S.allPrintings s
    -- Every FACE of every token, since CR 707.8a's double-faced token names two.
    let tokensOf face = concatMap (NonEmpty.toList . Card.Type.faces) [token | Effect.Create (Create.MkCreate _ token _ _) <- cardResolutionEffects face]
        tokens = concatMap (overFaces tokensOf . Printing.card) ps
    -- Guards the sweep against passing vacuously if Create ever moves out
    -- from under cardResolutionEffects.
    Spec.assertBool s (not (null tokens)) "the pool creates tokens"
    Spec.assertEqWith s "no token is misnamed" (fmap Face.name (filter tokenNameOffends tokens)) []
  Spec.it s "the lint itself catches a token named without the suffix" $ do
    doomedTraveler <- S.printingOf s registry "Doomed Traveler"
    case concatMap (NonEmpty.toList . Card.Type.faces) [token | Effect.Create (Create.MkCreate _ token _ _) <- cardResolutionEffects (S.combinedFace doomedTraveler)] of
      [token] -> do
        Spec.assertBool s (not (tokenNameOffends token)) "the real token passes"
        -- The exact misauthoring CR 111.4 forbids: the bare subtype, with
        -- the suffix dropped.
        Spec.assertBool s (tokenNameOffends token {Face.name = CardName.MkCardName $ Text.pack "Spirit"}) "misnamed token detected"
      other -> Spec.assertFailure s ("expected exactly one Create, got " <> show (length other))
  -- ONE sweep over the whole reserved set, replacing the five per-name
  -- cases this grew out of. Those five each filtered on
  -- Card.allTargetSlots, so they saw a card's spell modes and enchant slot
  -- and nothing else; they also covered only five of the seven reserved
  -- names, leaving `copySource` and `thatPlayer` with no declaration case
  -- at all. See reservedDeclarations for why declaring one is a discarded
  -- prompt rather than a naming quibble.
  --
  -- SCOPE: every face the card PRINTS, and every face it MINTS -- the tokens and
  -- emblems its effects carry as a payload, which S.allPrintings never offers
  -- the sweep directly. See mintedFaces; the self-test below is what proves that
  -- half, since no card in the pool offends in either position.
  Spec.it s "no reserved binding slot is ever a declared target slot" $ do
    ps <- S.allPrintings s
    let offends = not . Set.null . reservedDeclarations
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "no card declares a reserved slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 700.2d: Pawl.Engine.Modal.instanceSlot derives the slot a REPEATED mode's
  -- later instances fill by suffixing '#' and the occurrence, which is only a
  -- fresh name while no card prints one. This is that condition, checked rather
  -- than asserted -- the two sweeps above cover the names the ENGINE reserves,
  -- and this covers the shape it derives.
  Spec.it s "no declared or bound slot name contains the instance separator" $ do
    ps <- S.allPrintings s
    let hash = Text.pack "#"
        offends face =
          any (Text.isInfixOf hash . SlotName.unwrap) (Set.union (declaredTargetSlots face) (boundSlots face))
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "no card names a slot containing '#'" (fmap (S.nameOf . Printing.card) offenders) []
    -- Not vacuous: the pool declares slots at all, and the check really rejects.
    Spec.assertBool
      s
      (Text.isInfixOf hash (SlotName.unwrap (Modal.instanceSlot (ModeInstance.MkModeInstance (ModeIndex.MkModeIndex 0) 1) (SlotName.MkSlotName (Text.pack "creature")))))
      "instanceSlot's second occurrence really uses the separator"
  -- The sweep above's other half: declaring a reserved slot as a target is not
  -- the only way a card names one. Four opcodes carry a SlotName they
  -- BIND, and a card is free to write a reserved name into any of them, which
  -- the declaration sweep cannot see because none of the four is a target slot.
  -- See reservedBindings for why that is the worse of the two failures.
  --
  -- SCOPE: the declaration sweep's, exactly -- every face the card prints, and
  -- every face it mints.
  Spec.it s "no reserved binding slot is ever bound by a card" $ do
    ps <- S.allPrintings s
    let offends = not . Set.null . reservedBindings
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "no card binds a reserved slot" (fmap (S.nameOf . Printing.card) offenders) []
    -- Guards the sweep against passing vacuously, in both the ways it could.
    -- The pool binds slots at all:
    let poolBinds = Set.unions (concatMap (overFaces (pure . boundSlots) . Printing.card) ps)
    Spec.assertBool s (not (Set.null poolBinds)) "the pool binds slots"
    -- and the sweep reaches an ABILITY's binds, not just a spell mode's. Bane
    -- of Progress binds the count of what it destroyed from a triggered
    -- ability, so Card.allEffects -- the spell-modes view -- sees nothing.
    baneOfProgress <- S.printingOf s registry "Bane of Progress"
    let bane = S.combinedFace baneOfProgress
    Spec.assertEqWith
      s
      "Bane of Progress binds its destroyed count, which the spell-modes view misses"
      (boundSlots bane, Resolve.definedSlots (Card.allEffects bane))
      (Set.singleton (SlotName.MkSlotName (Text.pack "destroyed")), Set.empty)
  -- The sweep above passes VACUOUSLY over the committed pool, exactly as the
  -- declaration sweep does, so it is proven here against a hand-built offender
  -- instead -- never a card file, because a misauthored card must not be
  -- loadable. Bane of Progress already binds a Destroy's count, so renaming
  -- that one slot to a reserved name is the whole graft.
  --
  -- CR 615.13's `thatMuch` is the name chosen because it is the one with teeth:
  -- Pawl.Engine.Quantity.evaluateFor's InSlot arm asks the effect's SOURCE
  -- before the stack object, and Resolve.bindAmountSlot writes a Destroy's count
  -- to the source -- so this graft, under a prevention trigger, would answer
  -- "that much" with the card's own count and never consult the event's.
  --
  -- Asserted TWICE, in the declaration self-test's posture: the new sweep sees
  -- the offender and reservedDeclarations does NOT. The second half is the hole
  -- this test exists to close, and it fails if the binding sweep is ever
  -- narrowed back into the declaration one.
  Spec.it s "the lint itself catches an effect that binds a reserved slot" $ do
    baneOfProgress <- S.printingOf s registry "Bane of Progress"
    let rebind slot effect = case effect of
          Effect.Destroy (Destroy.MkDestroy ref regenerability (Just _)) -> Effect.Destroy (Destroy.MkDestroy ref regenerability (Just slot))
          other -> other
        overModal f modal =
          modal {Modal.modes = fmap (\m -> m {Mode.clauses = fmap (\c -> c {Clause.effects = fmap f (Clause.effects c)}) (Mode.clauses m)}) (Modal.modes modal)}
        withBind slot card =
          card
            { Face.triggeredAbilities =
                fmap (\t -> t {TriggeredAbility.modal = overModal (rebind slot) (TriggeredAbility.modal t)}) (Face.triggeredAbilities card)
            }
        offender = withBind Binding.eventAmount (S.combinedFace baneOfProgress)
    Spec.assertEqWith
      s
      "CR 615.13 thatMuch bound by a Destroy is caught, and the declaration sweep misses it"
      (reservedBindings offender, reservedDeclarations offender)
      (Set.singleton Binding.eventAmount, Set.empty)
    Spec.assertEqWith
      s
      "and the real card binds no reserved slot"
      (reservedBindings (S.combinedFace baneOfProgress))
      Set.empty
  -- Both sweeps above range over a face's MINTED cards as well as the face
  -- itself, and this is what proves that half. CR 111.3 makes the abilities a
  -- token's creator defines "functionally equivalent to the characteristic
  -- values that are printed on a card", and CR 114.4 makes an emblem's function
  -- in the command zone -- so a reserved name on either is the same defect as
  -- one on the minting card, reached through an effect's payload rather than
  -- through a Printing.
  --
  -- Hand-built, in the two self-tests' posture above, because no card in the
  -- pool names a reserved slot anywhere -- and only Ajani, Adversary of Tyrants
  -- prints a CreateEmblem, whose emblem names none either; the corpus sweeps are
  -- a regression guard for this, never its proof. Each
  -- case is asserted TWICE: the sweep sees the grafted offender, and the
  -- minting face's OWN slots stay empty. The second half is what makes this a
  -- test of the RECURSION rather than of a widened base case.
  Spec.it s "the lint itself catches a reserved slot on a minted token's or emblem's face" $ do
    doomedTraveler <- S.printingOf s registry "Doomed Traveler"
    piker <- S.printingOf s registry "Goblin Piker"
    let -- One triggered ability that offends in BOTH positions at once: it
        -- declares CR 109.5's `you` as a target slot and binds CR 615.13's
        -- `thatMuch` from the count of what it destroyed.
        offending =
          TriggeredAbility.MkTriggeredAbility
            { TriggeredAbility.condition = TriggerCondition.SelfDies,
              TriggeredAbility.modal =
                Modal.MkModal
                  ( Seq.singleton
                      ( Mode.MkMode
                          (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot Binding.you) Regenerability.Regenerable (Just Binding.eventAmount))))))
                          (Map.singleton Binding.you (TargetSlot.required Pool.AnyTarget Nothing))
                      )
                  )
                  (ModeSelection.ChooseExactly 1),
              TriggeredAbility.intervening = Nothing
            }
        arm face = face {Face.triggeredAbilities = offending : Face.triggeredAbilities face}
        overModal f modal =
          modal {Modal.modes = fmap (\m -> m {Mode.clauses = fmap (\c -> c {Clause.effects = fmap f (Clause.effects c)}) (Mode.clauses m)}) (Modal.modes modal)}
        -- Doomed Traveler mints its Spirit from a TRIGGERED ability, so this
        -- rewrites the minting effect where the card actually prints it.
        overMint f card =
          card
            { Face.triggeredAbilities =
                fmap (\t -> t {TriggeredAbility.modal = overModal f (TriggeredAbility.modal t)}) (Face.triggeredAbilities card)
            }
        -- Both halves of each case at once: what the sweeps report, and what
        -- the minting face's own carriers report.
        caught face = ((reservedDeclarations face, reservedBindings face), (Set.intersection reservedSlots (ownDeclaredTargetSlots face), Set.intersection reservedSlots (ownBoundSlots face)))
        offended = ((Set.singleton Binding.you, Set.singleton Binding.eventAmount), (Set.empty, Set.empty))
        traveler = S.combinedFace doomedTraveler
        onEveryFace f card = card {Card.Type.faces = fmap f (Card.Type.faces card)}
        -- The graft on the minted TOKEN, on every face of it.
        armToken effect = case effect of
          Effect.Create (Create.MkCreate quantity token riders slot) -> Effect.Create (Create.MkCreate quantity (onEveryFace arm token) riders slot)
          other -> other
        -- The same, on the BACK face of a two-faced token whose front is clean.
        armBackFace effect = case effect of
          Effect.Create (Create.MkCreate quantity token riders slot) ->
            let front = NonEmpty.head (Card.Type.faces token)
             in Effect.Create (Create.MkCreate quantity (token {Card.Type.faces = front NonEmpty.:| [arm front]}) riders slot)
          other -> other
        -- The same, on a minted EMBLEM in place of the token.
        armEmblem effect = case effect of
          Effect.Create {} -> Effect.CreateEmblem (onEveryFace arm (S.anthemEmblemCard piker))
          other -> other
    Spec.assertEqWith
      s
      "the real card offends in neither position"
      (caught traveler)
      ((Set.empty, Set.empty), (Set.empty, Set.empty))
    Spec.assertEqWith
      s
      "a reserved slot on the minted TOKEN's face is caught, and the minting face's own slots stay empty"
      (caught (overMint armToken traveler))
      offended
    -- CR 707.8a's double-faced token is why the recursion takes every face of
    -- the minted card rather than its first: here the offender is the SECOND.
    Spec.assertEqWith
      s
      "a reserved slot on a two-faced token's back face is caught"
      (caught (overMint armBackFace traveler))
      offended
    -- The emblem arm, which the pool's one CreateEmblem does not exercise --
    -- Ajani, Adversary of Tyrants' emblem names no slot at all: the minting
    -- effect is swapped for a CreateEmblem carrying the same graft.
    Spec.assertEqWith
      s
      "a reserved slot on a minted EMBLEM's face is caught"
      (caught (overMint armEmblem traveler))
      offended
  -- The read half of the same dataflow question, for the one carrier that has no
  -- resolution to bind anything: CR 604.3's characteristic-defining P/T. A CDA is
  -- a static ability, so there is no earlier effect of a resolution to mint its
  -- slot -- the only writer is the ENGINE, which is what a reserved name means.
  -- Wood Elemental's is Binding.sacrificedCount.
  --
  -- Not folded into the mode lint below: that one compares reads against a
  -- card's DECLARED slots, and a CDA declares none, so its whole available side
  -- is the reserved set. The printed boxes are swept alongside the CDA because a
  -- slot named there reaches the same evaluator (Pawl.Engine.Projection.seedCharacteristicPT
  -- substitutes only into a Star).
  Spec.it s "a printed or characteristic-defining P/T reads only reserved slots" $ do
    ps <- S.allPrintings s
    let offends card = not (Set.isSubsetOf (powerToughnessSlots card) reservedSlots)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "no card's CDA names a slot nothing fills" (fmap (S.nameOf . Printing.card) offenders) []
    -- The sweep above would pass vacuously if powerToughnessSlots reported
    -- nothing at all, so the one card that does read a slot is named here.
    woodElemental <- S.printingOf s registry "Wood Elemental"
    Spec.assertEqWith
      s
      "Wood Elemental's CDA reads the as-enters sacrifice count"
      (powerToughnessSlots (S.combinedFace woodElemental))
      (Set.singleton Binding.sacrificedCount)
  -- The sweep above passes VACUOUSLY: no committed card declares a reserved
  -- slot anywhere, so on its own it proves nothing about the lint. Proven
  -- here instead against hand-built offenders, in the posture the
  -- triggered-read self-test below uses -- never a card file, because a
  -- misauthored card must not be loadable -- one per ability carrier the
  -- sweep gained, each grafted onto a real card that has that kind of
  -- ability.
  --
  -- Each carrier is asserted TWICE: the sweep sees the offender, and
  -- Card.allTargetSlots -- the spell-modes-and-enchant view the five old
  -- cases filtered on -- does not. The second half is the regression guard:
  -- it is the hole itself, and it fails if the sweep is ever narrowed back.
  Spec.it s "the lint itself catches an ability that declares a reserved slot" $ do
    roaches <- S.printingOf s registry "Endless Cockroaches"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    tidalWave <- S.printingOf s registry "Tidal Wave"
    let -- A one-mode, effectless modal declaring exactly one target slot.
        declaring slot =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode Seq.empty (Map.singleton slot (TargetSlot.required Pool.AnyTarget Nothing))))
            (ModeSelection.ChooseExactly 1)
        withTriggered slot card =
          card
            { Face.triggeredAbilities =
                [ TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.SelfDies,
                      TriggeredAbility.modal = declaring slot,
                      TriggeredAbility.intervening = Nothing
                    }
                ]
            }
        withActivated slot card =
          card {Face.activatedAbilities = fmap (\a -> a {ActivatedAbility.modal = declaring slot}) (Face.activatedAbilities card)}
        withDelayed slot card =
          card {Face.delayedAbilities = fmap (\t -> t {TriggeredAbility.modal = declaring slot}) (Face.delayedAbilities card)}
        catches slot graft printing =
          let face = graft slot (S.combinedFace printing)
           in (reservedDeclarations face, Map.member slot (Card.allTargetSlots face))
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
    -- ORDINARY slots, which Card.allTargetSlots cannot see -- Prodigal
    -- Sorcerer's spell is a creature's empty mode, so its "target" is
    -- declared by its activated ability alone.
    let target = SlotName.MkSlotName (Text.pack "target")
    Spec.assertEqWith
      s
      "an activated ability's ordinary slot is in the sweep but not the spell-modes view"
      (Set.member target (declaredTargetSlots (S.combinedFace sorcerer)), Map.member target (Card.allTargetSlots (S.combinedFace sorcerer)))
      (True, False)
    Spec.assertEqWith
      s
      "and the three real cards declare no reserved slot"
      (fmap (reservedDeclarations . S.combinedFace) [roaches, sorcerer, tidalWave])
      [Set.empty, Set.empty, Set.empty]
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
  -- The "every slot a delayed ability reads is one its card defines" lint below
  -- takes the same wide view, for the same reason: nothing about where a Create
  -- binds its minted tokens is peculiar to a spell mode.
  Spec.it s "every armed delayed ability is declared, and every declared one is armed" $ do
    ps <- S.allPrintings s
    let cardOffends card =
          Resolve.armedAbilities (cardResolutionEffects card) /= Map.keysSet (Face.delayedAbilities card)
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling or unused delayed abilities" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint above joins names WITHIN a card; this one keeps that namespace clear
  -- of rule 702's. Pawl.Engine.Keyword.mintedDelayedAbilities declares decayed's
  -- "sacrifice it at end of combat" under a name of its own, and
  -- Pawl.Engine.Resolve looks a card's declarations up first -- so a card printing
  -- the same name would shadow the rule for any of its permanents holding the
  -- keyword.
  Spec.it s "CR 603.7 no card declares a delayed ability rule 702 already names" $ do
    ps <- S.allPrintings s
    let cardOffends card = not (Map.null (Map.restrictKeys (Face.delayedAbilities card) (Map.keysSet Keyword.Engine.mintedDelayedAbilities)))
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no card shadows a minted delayed ability" (fmap (S.nameOf . Printing.card) offenders) []
  -- Every slot a delayed ability READS must be one the arming card DEFINES:
  -- the reserved trigger-source slot, a token bound by a Create, the
  -- incarnation a MoveToZone bound at its destination (Meandering Towershell's
  -- exiled card), or a TARGET the arming carrier declared (armingTargetSlots, which
  -- is CR 603.7c's captured environment -- Ray of Command's third sentence). The
  -- `abilityBound` side is `cardResolutionEffects` for the
  -- reason the lint above takes it: the binding effect can live in the ability
  -- that arms, not only in a spell mode.
  --
  -- Through modalSlotsOffend, so a delayed ability with modes is read PER MODE
  -- (#570) -- and so a mode's own declared target slots count, which this lint
  -- omitted entirely. CR 603.3d puts a delayed ability on the stack "identical
  -- to the process for casting a spell listed in rules 601.2c-d", so a slot it
  -- declares really is announced; declaredTargetSlots already counts delayed
  -- abilities' target slots on the DECLARING side, and this is the matching read
  -- side. Since #1043 that comparison is the spell lint's EQUALITY, so a delayed
  -- ability declaring a slot no effect of its reads fails here too.
  Spec.it s "every slot a delayed ability reads is bound by its card" $ do
    ps <- S.allPrintings s
    let cardOffends card =
          let bound = Set.insert Binding.triggerSource (Set.union (armingTargetSlots card) (Resolve.definedSlots (cardResolutionEffects card)))
           in any (modalSlotsOffend bound . TriggeredAbility.modal) (Map.elems (Face.delayedAbilities card))
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling delayed-ability slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- A delayed ability may not DECLARE a target slot under a name its own card
  -- already DEFINES, because the two would land in one slot and the reader would
  -- have to pick. Pawl.Engine.Engine.placeOne merges the ability's placement-time
  -- choices with the environment captured when it was armed, per FIELD, so a
  -- collision leaves one Binding carrying both a CR 601.2c target and a Create's
  -- minted group -- and Pawl.Engine.Resolve.slotGroup answers with the group,
  -- silently discarding a target that CR 608.2b was owed a re-validation of.
  --
  -- Rejected rather than resolved by precedence: the card would be saying two
  -- different things under one name, which is a card-data mistake and not a rules
  -- question the engine should have an answer to. The neighbouring "every slot a
  -- delayed ability reads is bound by its card" lint could not catch it while it
  -- was a SUBSET check: both sides were on the available list, so a name
  -- appearing in both passed it twice over. Under #1043's equality it rejects the
  -- same shape as a side effect, whether or not the ability reads the name back
  -- -- a read is answered by the bound side and so cancels, and no read leaves the
  -- read side empty, so either way the declared slot goes unmatched. Kept anyway:
  -- it states the claim that is actually true of the data (a name may not be both
  -- declared and defined) rather than deriving it from a dataflow count, and it
  -- names the offending card outright.
  Spec.it s "no delayed ability declares a target slot under a name its card defines" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace shadowsDefinedSlot . Printing.card) ps
    Spec.assertEqWith s "no delayed ability shadows a defined slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above is vacuous on its own -- no card offends, and none would
  -- under a predicate that always answered False -- so both directions are put
  -- to a hand-built pair. The accepted one is Thatcher Revolt's exact shape,
  -- which must stay legal: reading a defined slot is the whole point, and only
  -- DECLARING one is the mistake.
  Spec.it s "the shadowing lint accepts a delayed ability that only reads the slot" $ do
    let tokens = SlotName.MkSlotName (Text.pack "tokens")
        reads_ = modalTrigger TriggerCondition.SelfEnters [lintMode [Effect.Sacrifice tokens] []]
        declares = modalTrigger TriggerCondition.SelfEnters [lintMode [Effect.Sacrifice tokens] [tokens]]
    Spec.assertBool s (not (shadowsSlots (Set.singleton tokens) [reads_])) "reading a Create's slot is legal"
    Spec.assertBool s (shadowsSlots (Set.singleton tokens) [declares]) "declaring a target slot under the same name is not"
  -- The pairing Pawl.Types.Onset.FromYourNextTurn depends on and cannot enforce
  -- alone. See onsetOffends for why the onset and the condition's TurnScope
  -- are two halves of one printed "your next turn", and what goes wrong when a
  -- card supplies only one of them.
  Spec.it s "every delayed ability armed for YOUR next turn is controller-scoped" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace onsetOffends . Printing.card) ps
    Spec.assertEqWith s "no onset over a condition that admits another player's turn" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above is NOT vacuous -- Meandering Towershell is a real card with
  -- an onset, so the accepting direction is exercised by the pool -- but nothing
  -- committed offends it, so the REJECTING direction is proven here instead,
  -- against that same card misauthored on purpose. Never a card file: a card
  -- that offends a lint must not be loadable.
  Spec.it s "the lint itself catches an onset over an EachTurn condition" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    let face = S.combinedFace towershell
        -- The Towershell's own condition with CR 603.2b's OTHER turn scope: "at
        -- the beginning of EACH declare attackers step", which an opponent's
        -- turn satisfies. Built rather than pattern-matched, so this fixture
        -- states the offence outright.
        eachTurn ability =
          ability
            { TriggeredAbility.condition =
                TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Combat CombatStep.DeclareAttackers) TurnScope.EachTurn)
            }
        widened = face {Face.delayedAbilities = fmap eachTurn (Face.delayedAbilities face)}
        -- The other way a card can reach this: an onset naming an ability the
        -- card does not declare at all.
        dangling = face {Face.delayedAbilities = Map.empty}
    Spec.assertBool s (not (onsetOffends face)) "the real card, ControllersTurn, is accepted"
    Spec.assertBool s (onsetOffends widened) "EachTurn under an onset is rejected"
    Spec.assertBool s (onsetOffends dangling) "and so is an onset naming no declared ability"
    -- Not a check that fires for every card: one with no onset at all has
    -- nothing for this to reject, whatever its delayed abilities are scoped to.
    tidalWave <- S.printingOf s registry "Tidal Wave"
    Spec.assertBool s (not (onsetOffends (S.combinedFace tidalWave))) "a card with no onset is not swept up"
  -- The same equality over a card's TRIGGERED abilities, which is where
  -- the condition-specific reserved slots live -- CR 400.7e's `became` and
  -- CR 702.70a's `thatPlayer`. See triggeredAbilityOffends for what answers a
  -- read there.
  Spec.it s "every slot a triggered ability reads is bound for its condition, and every slot it declares is read" $ do
    ps <- S.allPrintings s
    let cardOffends = any triggeredAbilityOffends . Face.triggeredAbilities
        offenders = filter (anyFace cardOffends . Printing.card) ps
    Spec.assertEqWith s "no dangling triggered-ability slot" (fmap (S.nameOf . Printing.card) offenders) []
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
        returnIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue)
        -- Rule 702.70a's shape, as a targetless read of "that player".
        thatPlayerDraws = Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1))
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
      (not (any triggeredAbilityOffends (Face.triggeredAbilities (S.combinedFace roaches))))
      "the real card's dies trigger is accepted"
  -- The same equality, on the read that is NOT an effect's operand: CR 603.2's
  -- "that player" may be named by a target slot's own FILTER (Trygon Predator),
  -- which Resolve.modeSlots sees through Filter.boundSlots. Without that clause
  -- the pairing below would be invisible and a card could narrow a slot by a
  -- player its condition never binds -- a slot that then admits nothing.
  Spec.it s "the lint itself catches a reserved event slot named by a target filter" $ do
    trygon <- S.printingOf s registry "Trygon Predator"
    let target = SlotName.MkSlotName (Text.pack "target")
        narrowed =
          Mode.MkMode
            (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Tap (ObjectRef.InSlot target)))))
            (Map.singleton target (TargetSlot.required Pool.Permanents (Just (Filter.Type.ControlledByBound Binding.triggerPlayer))))
    Spec.assertBool
      s
      (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfEnters [narrowed]))
      "CR 603.2 thatPlayer in a filter under an enters trigger is rejected"
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfDealsCombatDamageToPlayer [narrowed])))
      "and under a combat-damage trigger it is accepted"
    Spec.assertBool
      s
      (not (any triggeredAbilityOffends (Face.triggeredAbilities (S.combinedFace trygon))))
      "the real card's own trigger is accepted"
  -- The same equality over a card's ACTIVATED abilities, the one carrier with no
  -- event slot answering a read at all: an activation is not an event. See
  -- activatedAbilityOffends for the whole of it.
  --
  -- Brothers of Fire is what this caught: its "and 1 damage to you" reads CR
  -- 109.5's slot from an ACTIVATED ability, which nothing bound until
  -- Activate.activateAbility started stamping it (#569).
  Spec.it s "every slot an activated ability reads is bound for its activation, and every slot it declares is read" $ do
    ps <- S.allPrintings s
    let abilitiesOf p = fmap ((,) (Face.name (S.combinedFace p))) (Face.activatedAbilities (S.combinedFace p))
        abilities = concatMap abilitiesOf ps
        readsAnySlot ab = not (all (Map.null . Resolve.slotsOf) (Modal.allEffects (ActivatedAbility.modal ab)))
    -- Guards the sweep against passing vacuously, in both directions: an empty
    -- pool of abilities, and a pool in which none reads a slot at all (where
    -- every ability would pass on an empty read side whatever the lint said).
    Spec.assertBool s (not (null abilities)) "the pool has activated abilities"
    Spec.assertBool s (any (readsAnySlot . snd) abilities) "and one of them reads a slot"
    Spec.assertEqWith s "no dangling activated-ability slot" (fmap fst (filter (activatedAbilityOffends . snd) abilities)) []
  -- CR 601.2c's count, over every carrier at once: see modalCountsOffend.
  Spec.it s "every slot that may take more than one target is read where a set fits" $ do
    ps <- S.allPrintings s
    let carriers p =
          let face = S.combinedFace p
           in fmap ((,) (Face.name face)) $
                Face.spell face
                  : fmap ActivatedAbility.modal (Face.activatedAbilities face)
                    <> fmap TriggeredAbility.modal (Face.triggeredAbilities face)
        modals = concatMap carriers ps
        takesSeveral (_, modal) =
          any (any (TargetCount.plural . TargetSlot.count) . Mode.targetSlots) (Modal.modes modal)
    -- The pool must actually contain one, or the sweep says nothing.
    Spec.assertBool s (any takesSeveral modals) "the pool has a slot that takes more than one target"
    Spec.assertEqWith s "no multi-target slot is read one at a time" (fmap fst (filter (modalCountsOffend . snd) modals)) []
  -- The rejecting direction, which the sweep above cannot show: a mode whose slot
  -- takes two targets and whose only reader is Effect.Sacrifice, a bare SlotName.
  Spec.it s "the lint itself catches a multi-target slot read one at a time" $ do
    let slot = SlotName.MkSlotName (Text.pack "creature")
        modeWith targetSlot reader =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton reader))) (Map.singleton slot targetSlot)))
            (ModeSelection.ChooseExactly 1)
        two = TargetSlot.upTo 2 Pool.Creatures Nothing
    Spec.assertBool
      s
      (modalCountsOffend (modeWith two (Effect.Sacrifice slot)))
      "a two-target slot read as one object offends"
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith two (Effect.Tap (ObjectRef.InSlot slot)))))
      "and the same slot read through an ObjectRef does not"
    Spec.assertBool
      s
      (not (modalCountsOffend (modeWith (TargetSlot.required Pool.Creatures Nothing) (Effect.Sacrifice slot))))
      "nor does a one-target slot read as one object"
    -- CR 601.2c's "any number of target ...", which states no maximum to compare
    -- against: an unbounded slot is plural, so the same one-object reader offends.
    -- No card in the corpus makes this mistake, so this is the only observer
    -- TargetCount.plural's unbounded arm has.
    Spec.assertBool
      s
      (modalCountsOffend (modeWith (TargetSlot.anyNumber Pool.Creatures Nothing) (Effect.Sacrifice slot)))
      "an unbounded slot read as one object offends too"
  -- The sweep above passes VACUOUSLY on the rejecting side: no committed
  -- activated ability reads a slot it is not given, so the REJECTING direction is
  -- proven here instead, against hand-built offenders and against the four real
  -- cards that between them exercise every part of the available side.
  --
  -- Every reserved slot an activation does NOT bind gets its own case, because a
  -- classification answering "every slot, always" would pass any one of them
  -- alone. CR 109.5's `you` is the case that runs the other way, and is asserted
  -- on BOTH lints: the rule defines the word for an activated ability and for a
  -- triggered one, so the same effect is accepted either way (#569). Leaving it
  -- off one lint's available side is what a card would then fail on, so the pair
  -- is what keeps the two halves of the rule from drifting apart.
  Spec.it s "the lint itself catches an activated ability reading a slot activation never binds" $ do
    longtuskCub <- S.printingOf s registry "Longtusk Cub"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    cinderElemental <- S.printingOf s registry "Cinder Elemental"
    brothers <- S.printingOf s registry "Brothers of Fire"
    let free = Just (ManaCost.MkManaCost [])
        variable = Just (ManaCost.MkManaCost [ManaSymbol.Variable])
        -- CR 109.5's "you", in the shape Baral, Chief of Compliance's TRIGGERED
        -- ability uses it: a bare-SlotName opcode naming the controller.
        youDiscards = Effect.Discard (Discard.MkDiscard Binding.you (Quantity.Type.Literal 1))
        -- Endless Cockroaches' payload (CR 400.7e) and rule 702.70a's, the two
        -- event slots, neither of which an activation has an event to bind.
        returnIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue)
        thatPlayerDraws = Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot Binding.triggerPlayer) (Quantity.Type.Literal 1))
        -- CR 113.7's source slot, which every activation DOES bind.
        tapSelf = Effect.Tap (ObjectRef.InSlot Binding.triggerSource)
        -- An ordinary slot this ability neither declares nor mints.
        tapGhost = Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "ghost")))
        -- CR 601.2b's announced value, read as a slot rather than as Quantity.X.
        drawX = Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.InSlot Binding.variableX))
    Spec.assertBool
      s
      (not (activatedAbilityOffends (oneEffectActivated free youDiscards)))
      "CR 109.5 you is accepted: an activation binds the player who activated it"
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
    -- The four real cards between them cover every part of the available side
    -- that a committed card reaches: CR 113.7's self, CR 601.2c's declared
    -- target, an ability whose cost carries CR 601.2b's {X}, and CR 109.5's
    -- `you`. Brothers of Fire is the last one's only producer, so dropping it
    -- from this list would leave that part of the available side asserted by
    -- the hand-built case alone.
    Spec.assertEqWith
      s
      "Longtusk Cub, Prodigal Sorcerer, Cinder Elemental and Brothers of Fire are all accepted"
      (fmap (any activatedAbilityOffends . Face.activatedAbilities . S.combinedFace) [longtuskCub, sorcerer, cinderElemental, brothers])
      [False, False, False, False]
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
        -- target slots -- which is nothing -- so the read is unbound at runtime.
        crossDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap creature] []]
        -- The same two reads, each mode declaring the slot it reads.
        ownDeclared = modalActivated [lintMode [tap creature] [creature], lintMode [tap victim] [victim]]
        -- Mode 0 MINTS `exiled` at a MoveToZone's destination; mode 1 reads it.
        -- The two never resolve together, so mode 1's read is dangling.
        exileIt = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot creature) Zone.Exile EntryRiders.defaultValue (Just exiled) Nothing LibraryPlacement.defaultValue)
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
    let returnBecame = Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.InSlot Binding.became) Zone.Hand EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue)
        secondModeReads condition = modalTrigger condition [lintMode [] [], lintMode [returnBecame] []]
    Spec.assertBool
      s
      (not (triggeredAbilityOffends (secondModeReads TriggerCondition.SelfDies)))
      "the condition's event slots reach a later mode too"
    Spec.assertBool
      s
      (triggeredAbilityOffends (secondModeReads TriggerCondition.SelfEnters))
      "and a later mode is still rejected when the condition binds nothing"
  -- The DECLARED-BUT-UNREAD half of the ability lints, which the spell lint has
  -- always had and the three ability ones acquired with #1043. Before that they
  -- were subset checks, so an ability announcing a target no effect of its reads
  -- passed -- and so did an Effect whose Resolve.slotsOf arm UNDER-REPORTED,
  -- since a forgotten read only shrinks the read side that the subset check let
  -- be small. Reverting Resolve.slotsOf's BecomeMonarch arm to Set.empty left the
  -- entire suite green when Denethor, Stone Seer landed (#1040); under the
  -- equality it fails, because Denethor's ability declares two slots and would
  -- then read one.
  --
  -- Every carrier gets a case: what makes this an ability-side gap is that the
  -- claim was stated for Face.spell alone, so proving it on one ability would not
  -- show it reaching the others.
  Spec.it s "the lint itself catches an ability declaring a slot no effect reads" $ do
    denethor <- S.printingOf s registry "Denethor, Stone Seer"
    let creature = SlotName.MkSlotName (Text.pack "creature")
        victim = SlotName.MkSlotName (Text.pack "victim")
        tap slot = Effect.Tap (ObjectRef.InSlot slot)
        -- One mode declaring two slots and reading only one of them: Denethor's
        -- exact shape under a slotsOf arm that forgot a read.
        unread = [lintMode [tap creature] [creature, victim]]
        read_ = [lintMode [tap creature] [creature]]
        -- The delayed lint calls modalSlotsOffend itself, with the whole card's
        -- minted slots on the bound side; nothing here mints, so the bound side
        -- is CR 113.7's source alone.
        delayed modes = modalSlotsOffend (Set.singleton Binding.triggerSource) (TriggeredAbility.modal (modalTrigger TriggerCondition.SelfDies modes))
    Spec.assertBool s (activatedAbilityOffends (modalActivated unread)) "an activated ability declaring an unread slot is rejected"
    Spec.assertBool s (not (activatedAbilityOffends (modalActivated read_))) "and reading everything it declares is accepted"
    Spec.assertBool s (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfDies unread)) "a triggered ability declaring an unread slot is rejected"
    Spec.assertBool s (not (triggeredAbilityOffends (modalTrigger TriggerCondition.SelfDies read_))) "and reading everything it declares is accepted"
    Spec.assertBool s (delayed unread) "a delayed ability declaring an unread slot is rejected"
    Spec.assertBool s (not (delayed read_)) "and reading everything it declares is accepted"
    -- The real card whose landing exposed the gap, accepted: its ability declares
    -- a player slot for the crown and an `any target` slot for the damage, and
    -- Resolve.slotsOf reports both.
    Spec.assertBool
      s
      (not (any activatedAbilityOffends (Face.activatedAbilities (S.combinedFace denethor))))
      "Denethor, Stone Seer's two-slot ability is accepted"
  -- CR 400.1: every InZone Count over a shared zone (battlefield, stack,
  -- exile, command) must pair with PlayerRef.EachPlayer -- the type
  -- permits any PlayerRef there, but only EachPlayer is meaningful for a
  -- zone no player owns individually (#161).
  Spec.it s "every InZone Count over a shared zone pairs with EachPlayer" $ do
    ps <- S.allPrintings s
    let offenders =
          filter
            (anyFace cardOffendsSharedZoneScope . Printing.card)
            ps
    Spec.assertEqWith s "no shared-zone scope with a non-EachPlayer ref" (fmap (S.nameOf . Printing.card) offenders) []
  Spec.it s "a card with no enchant ability declares no enchant slot" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let card = S.combinedFace piker
    Spec.assertEqWith s "no enchant slot" (Face.enchant card) []
    Spec.assertBool s (not (Card.isAura card)) "not an Aura"
    Spec.assertEqWith s "no enchant slot" (Card.enchantSlotMap card) Map.empty
  -- CR 303.4 / 702.5a: the biconditional. An Aura without enchant has no legal
  -- target and could never be cast; a non-Aura with enchant declares a restriction
  -- nothing reads. The D4 lint cannot see either, because it walks
  -- Mode.targetSlots and the enchant slot is not there (#184's shape).
  --
  -- "AT LEAST one", since CR 702.5c lets an Aura have several -- the count is not
  -- what makes a card an Aura, only the presence.
  Spec.it s "a card is an Aura iff it declares an enchant ability" $ do
    ps <- S.allPrintings s
    let offends c = Card.isAura c /= not (null (Face.enchant c))
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "Aura iff enchant" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 702.5c makes every instance of enchant apply at once, and its last
  -- sentence -- "The Aura can enchant only objects or players that match all of
  -- its enchant abilities" -- conjoins the POOLS exactly as it does the Filters.
  -- Pawl.Engine.Card.enchantTargetSlot folds the instances into ONE target slot
  -- by Anding their Filters and keeping the FIRST instance's Pool. This lint is
  -- what makes that fold exact, and the rule it enforces is that CR 115's Pool
  -- enum is not closed under intersection. Three shapes, one expressible: a
  -- NESTED pair has a Pool naming the intersection (Creatures against Permanents
  -- is Creatures); a DISJOINT pair intersects to nothing (Creatures against
  -- Players, which is CR 702.5d keeping an enchant-player Aura off permanents),
  -- and no Pool names the empty set; and an OVERLAPPING pair can name a set the
  -- enum simply lacks (AnyTarget against Permanents is
  -- creatures-and-planeswalkers). Taking the first instance is order-dependent
  -- even in the expressible case, so a card whose enchant abilities disagreed
  -- would be silently judged by whichever pool was written down first.
  --
  -- The disjoint case is INCOHERENT rather than merely unrepresentable, and the
  -- CR says what becomes of such an Aura without needing a pool for it: CR 303.4a
  -- makes its spell require a target and CR 601.2c has no appropriate object or
  -- player to announce for it, so it cannot be cast; an effect putting it onto the
  -- battlefield leaves it where it is, or bins it if that zone is the stack (CR
  -- 303.4g); and one that arrived anyway is put into its owner's graveyard on the
  -- next state-based check (CR 704.5m). A card in that shape is dead text.
  --
  -- Unprinted rather than impossible, which is why this lives here rather than
  -- being ruled out: nothing in CR 702.5 requires the instances to agree, and
  -- Animate Dead prints both pools on one card ("enchant creature card in a
  -- graveyard", then "enchant creature put onto the battlefield with this Aura")
  -- -- only its lose-as-it-gains clause keeps the two from applying at once
  -- (#797).
  Spec.it s "every enchant ability on a card draws from the same pool" $ do
    ps <- S.allPrintings s
    let offends c = length (List.nub (fmap TargetSlot.pool (Face.enchant c))) > 1
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "no card mixes enchant pools, since CR 702.5c intersects them and Pool is not closed under intersection" (fmap (S.nameOf . Printing.card) offenders) []
  -- Pawl.Engine.Card.allTargetSlots binds the enchant slot under this name (Task 6), so a
  -- mode declaring it would be silently shadowed.
  -- #199: no card authors a layer-2 control modification into an effect that
  -- RESOLVES. SetControllerToSource is the payload-free constructor and is
  -- INERT when stored: Projection.controllerOfGiven's storedSetter matches only
  -- Modification.SetController, Projection.controlGrants reads control-granting
  -- static abilities off Face.staticAbilities and never off stored effects, and
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
  -- A codec-level rejection would be the wrong shape: (Codec.decode Modification.codec) is
  -- shared with staticAbilities, which Control Magic legitimately uses.
  Spec.it s "no card authors a control modification into a resolving effect (#199)" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ modification _) -> Projection.layer modification == Layer.Control
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    Spec.assertEqWith s "control belongs on a static ability, never in a stored effect" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 712.14a's rider is about a double-faced CARD, and CR 111.1 makes a token
  -- no card at all -- so an Effect.Create that carried it would be asking for a
  -- face-turn no rule performs. Pawl.Engine.Resolve's Create arm accordingly does
  -- not read the field, and this is what holds the corpus to that reading. A lint
  -- rather than a per-opcode rider type: CR 110.5b's tap state and CR 508.4's
  -- attacking genuinely are common to Create and MoveToZone, so splitting the
  -- record to keep one field off one opcode would duplicate the other two.
  Spec.it s "no Create carries CR 712.14a's transformed entry rider" $ do
    ps <- S.allPrintings s
    let creates effect = case effect of
          Effect.Create (Create.MkCreate _ _ riders _) -> EntryRiders.transformed riders
          _ -> False
        moves effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _) -> EntryRiders.transformed riders
          _ -> False
        offenders = filter (anyFace (any creates . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no transformed rider in the pool at
    -- all this would pass whatever Create did. Befriending the Moths is the card
    -- that prints one.
    Spec.assertBool s (any (anyFace (any moves . cardResolutionEffects) . Printing.card) ps) "the pool has a card returning itself transformed"
    Spec.assertEqWith s "a token is not a card, so no token is created transformed" (fmap (S.nameOf . Printing.card) offenders) []
  -- MoveToZone's Maybe SlotName binds what CR 400.7 minted at the destination, in
  -- one of two shapes: ONE incarnation for a move of one card (Befriending the
  -- Moths' "it"), and a GROUP for a move of several (Act on Impulse's "those
  -- cards"). Pawl.Engine.Resolve picks by how many actually arrived, and only the
  -- singular shape is visible to a SINGULAR READER -- Resolve.slotOne reads
  -- Binding.targets, which a group never fills.
  --
  -- So the shape a card must not author is a singular read of a slot a move that
  -- may take SEVERAL cards bound: it would silently name nothing rather than
  -- fail. Effect.OfferCast is the ONE singular reader -- Resolve.offerCast asks
  -- slotOne and nothing else. A MoveToZone whose own ref is an InSlot is not
  -- one, despite reading the slot by hand rather than through
  -- Resolve.objectRefObjects: its branch asks slotGroup FIRST and moves every
  -- member, which is Feral Lightning's "exile them" and Ignorant Bliss' "return
  -- those cards to your hand".
  Spec.it s "no card reads a slot a plural move bound with a singular reader" $ do
    ps <- S.allPrintings s
    let -- The refs that move at most ONE object, and so bind the singular shape
        -- whatever the board holds. A TopOfLibrary is `depth` cards PER LIBRARY,
        -- so it qualifies only at a depth of one over a PlayerRef naming a single
        -- library -- "each player's" and, in a game of three, "each opponent's"
        -- both move several, and so does any depth above one.
        movesAtMostOne ref = case ref of
          ObjectRef.InSlot _ -> True
          ObjectRef.EachMatching _ -> False
          ObjectRef.EachCardInGraveyard {} -> False
          ObjectRef.EachCardInYourHand -> False
          -- CR 607.3 is what makes this one plural even where the card's own
          -- words are singular: an ability referring to "the exiled card" whose
          -- linked ability exiled several performs its action on each of them.
          ObjectRef.EachCardExiledWithSource -> False
          ObjectRef.EachPlayer -> False
          ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player depth) ->
            depth <= 1 && case player of
              PlayerRef.Relative PlayerRelation.You -> True
              PlayerRef.Relative PlayerRelation.Opponent -> False
              PlayerRef.InSlot _ -> True
              PlayerRef.EachPlayer -> False
              -- One seat, so one library -- InSlot's answer. Unreachable from
              -- card data, which the sweep below is what enforces.
              PlayerRef.Specific _ -> True
              -- NO library at all: an ObjectRef is read by a resolution, where
              -- no fold supplies a candidate, so this names nobody and moves
              -- nothing -- which is at most one.
              PlayerRef.Candidate -> True
              -- One seat, so one library -- InSlot's answer, one indirection out.
              PlayerRef.ControllerOfBound _ -> True
          -- One card per CHOOSER: the resolving controller chooses once however
          -- many graveyards the scope draws candidates from, where Exhume's
          -- "each player" is one choice each and so several cards on any board
          -- with more than one stocked graveyard.
          ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser _ _) -> case chooser of
            Chooser.TheController -> True
            Chooser.EachInScope -> False
            -- One seat, so one graveyard and one card -- TheController's answer
            -- with the chooser named by a slot instead of by CR 608.2c.
            Chooser.BoundInSlot _ -> True
        boundPlurally effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ mSlot _ _) | not (movesAtMostOne ref) -> Maybe.maybeToList mSlot
          _ -> []
        readSingly effect = case effect of
          Effect.OfferCast (OfferCast.MkOfferCast slot _) -> [slot]
          _ -> []
        clashes effects =
          not
            . Set.null
            $ Set.intersection
              (Set.fromList (concatMap boundPlurally effects))
              (Set.fromList (concatMap readSingly effects))
        offenders = filter (anyFace (clashes . cardResolutionEffects) . Printing.card) ps
        binds effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ mSlot _ _) -> Maybe.isJust mSlot && not (movesAtMostOne ref)
          _ -> False
        exiledSlot = SlotName.MkSlotName (Text.pack "exiled")
    -- Half the rejected shape is in the pool: Act on Impulse binds a group. The
    -- OTHER half is not, and cannot be -- no card prints an OfferCast at all,
    -- since the only writer of that opcode is Pawl.Engine.Battle's CR 310.12b
    -- offer, which the engine bakes. So the REJECTING direction is proven here
    -- against a hand-built pair rather than by a corpus sweep, the posture the
    -- phase-skip lint below takes against Eon Hub, and the sweep is a fence
    -- against a future card authoring the shape.
    Spec.assertBool s (any (anyFace (any binds . cardResolutionEffects) . Printing.card) ps) "the pool has a card binding what a plural move minted"
    Spec.assertBool
      s
      ( clashes
          [ Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Zone.Exile EntryRiders.defaultValue (Just exiledSlot) Nothing LibraryPlacement.defaultValue),
            Effect.OfferCast (OfferCast.MkOfferCast exiledSlot CastOffer.defaultValue)
          ]
      )
      "a singular read of a plurally bound slot is caught"
    Spec.assertEqWith s "a group binding is invisible to a singular reader" (fmap (S.nameOf . Printing.card) offenders) []
  -- OwnerChooses asks a player which END of a library a card arrives at (CR
  -- 401.2), and only a library HAS ends -- so on any other destination it would
  -- put a question on the wire with no board behind it. A stated position on a
  -- non-library move is merely inert card data; this one is not, which is why it
  -- gets a lint of its own.
  Spec.it s "no MoveToZone leaves the end to an owner off a library" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone _ _ _ LibraryPlacement.OwnerChooses) -> zone /= Zone.Library
          _ -> False
        asks effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ _ _ _ LibraryPlacement.OwnerChooses) -> True
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no owner-chosen end in the pool this
    -- would pass whatever a card said. Aetherspouts is the card that prints one.
    Spec.assertBool s (any (anyFace (any asks . cardResolutionEffects) . Printing.card) ps) "the pool has a card leaving the end to each owner"
    Spec.assertEqWith s "only a library has ends" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 406.3's rider is a rule about the EXILE ZONE, so on any other destination
  -- it is inert card data, and on a Create it is inert outright -- a token is
  -- created onto the battlefield, and CR 111.7 makes one anywhere else cease to
  -- exist, so no Create ever reaches exile. Event.changeZoneAttaching
  -- gates on the destination, so this lints an authoring mistake rather than
  -- guarding the engine.
  Spec.it s "no effect exiles face down anywhere but exile" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone riders _ _ _) -> EntryRiders.exiledFaceDown riders && zone /= Zone.Exile
          Effect.Create (Create.MkCreate _ _ riders _) -> EntryRiders.exiledFaceDown riders
          _ -> False
        hides effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _) -> EntryRiders.exiledFaceDown riders
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no face-down exile in the pool at all
    -- this would pass whatever a card said. Ignorant Bliss is the card that
    -- prints one.
    Spec.assertBool s (any (anyFace (any hides . cardResolutionEffects) . Printing.card) ps) "the pool has a card exiling face down"
    Spec.assertEqWith s "only exile keeps a card face down (CR 406.3)" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sibling lint for the OTHER face-down rider, one field over and pointed
  -- at the opposite zone: CR 708.3 is a rule about entering the BATTLEFIELD, so
  -- on any other destination it is inert card data. Inert on a Create outright,
  -- for the reason CR 712.14a's transformed rider is -- a token is not a card,
  -- and no rule puts one onto the battlefield face down.
  -- Event.changeZoneEntering gates on the destination, so this lints an
  -- authoring mistake rather than guarding the engine.
  Spec.it s "no effect enters face down anywhere but the battlefield" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone riders _ _ _) -> EntryRiders.faceDown riders && zone /= Zone.Battlefield
          Effect.Create (Create.MkCreate _ _ riders _) -> EntryRiders.faceDown riders
          _ -> False
        manifests effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _) -> EntryRiders.faceDown riders
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no face-down entry in the pool at all
    -- this would pass whatever a card said. Soul Summons is the card that
    -- prints one.
    Spec.assertBool s (any (anyFace (any manifests . cardResolutionEffects) . Printing.card) ps) "the pool has a card putting a permanent onto the battlefield face down"
    Spec.assertEqWith s "only the battlefield takes a face-down entry (CR 708.3)" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sibling of the lint above, for the OTHER PlayerId the engine bakes and
  -- the codec accepts. See phasePatternOffends for why a card cannot name a
  -- player, and for why this is a lint rather than a type split (#437).
  Spec.it s "no card authors a player-scoped phase skip (#437)" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any phasePatternOffends . cardReplacementEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no PhaseR in the pool at all this
    -- would pass whatever the classification said. Eon Hub is the card that
    -- prints one.
    Spec.assertBool s (any (anyFace (any isPhaseR . cardReplacementEffects) . Printing.card) ps) "the pool has a card printing a phase skip"
    Spec.assertEqWith s "whosePhase is baked by the engine, never authored" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep passes because the pool is authored correctly, so the REJECTING
  -- direction is proven here against Eon Hub with a seat baked into it -- never
  -- a card file, since a card that offends a lint must not be loadable.
  Spec.it s "the lint itself catches a baked whosePhase" $ do
    eonHub <- S.printingOf s registry "Eon Hub"
    let card = S.combinedFace eonHub
        bake replacement = case replacement of
          ReplacementEffect.PhaseR phasePattern ->
            ReplacementEffect.PhaseR phasePattern {PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)}
          other -> other
        baked = card {Face.replacementEffects = fmap bake (Face.replacementEffects card)}
    Spec.assertBool s (not (any phasePatternOffends (cardReplacementEffects card))) "the real Eon Hub is symmetric and accepted"
    Spec.assertBool s (any phasePatternOffends (cardReplacementEffects baked)) "and the same card naming a seat is rejected"
  -- The same lint one event class over, for the OTHER fields the codec accepts and
  -- only the engine writes: CR 615.7's shielded recipient and its remaining amount,
  -- plus CR 122.1c's minted pair. See engineOnlyOffends.
  Spec.it s "no card authors a recipient-scoped damage pattern or an engine-minted shield" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any engineOnlyOffends . cardReplacementEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: Fog is the card that prints a DamageR.
    Spec.assertBool s (any (anyFace (any isDamageR . cardReplacementEffects) . Printing.card) ps) "the pool has a card printing a damage replacement"
    Spec.assertEqWith s "a shield is baked by the engine, never authored" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Fog rather than a card file, exactly
  -- as the Eon Hub case above is.
  Spec.it s "the lint itself catches a baked shield" $ do
    fog <- S.printingOf s registry "Fog"
    -- Fog's DamageR is installed by a resolution effect rather than printed as a
    -- static replacement ability, so the baking here is on what
    -- cardReplacementEffects reports rather than on Face.replacementEffects --
    -- which is the sweep's own input either way.
    let printed = cardReplacementEffects (S.combinedFace fog)
        bakeRecipient replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern rewrite riders) ->
            ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern {DamagePattern.whichRecipient = Just (Recipient.ToPlayer (PlayerId.MkPlayerId 1))} rewrite riders)
          other -> other
        bakeShield replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ riders) -> ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern (DamageRewrite.PreventNext 4) riders)
          other -> other
        bakeCounterShield replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ riders) -> ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern DamageRewrite.PreventRemovingShieldCounter riders)
          other -> other
    Spec.assertBool s (any isDamageR printed) "setup: Fog prints a damage replacement to bake"
    Spec.assertBool s (not (any engineOnlyOffends printed)) "the real Fog names no recipient and counts nothing"
    Spec.assertBool s (any (engineOnlyOffends . bakeRecipient) printed) "the same effect naming a shielded player is rejected"
    Spec.assertBool s (any (engineOnlyOffends . bakeShield) printed) "and so is one counting a shield down"
    -- CR 122.1c's prevention, which only Projection.shieldOf may mint.
    Spec.assertBool s (any (engineOnlyOffends . bakeCounterShield) printed) "and so is one removing a shield counter"
    Spec.assertBool s (engineOnlyOffends (ReplacementEffect.DestructionR DestructionRewrite.RemoveShieldCounter)) "and so is CR 122.1c's destruction half"
    Spec.assertBool s (not (engineOnlyOffends (ReplacementEffect.DestructionR DestructionRewrite.Regenerate))) "while CR 701.19a's printed regeneration is accepted"
  -- The same shape one axis over, and the thing that makes
  -- Pawl.Engine.PlayerEffect.unpreventable's board fold EXACT rather than
  -- approximate. See unpreventableScopeOffends.
  Spec.it s "no card narrows CR 615.12's \"damage can't be prevented\" by player" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any (uncurry unpreventableScopeOffends) . cardPlayerScopes) . Printing.card) ps
    -- Guards against a vacuous sweep: with no such effect in the pool at all
    -- this would pass whatever the classification said. Spider-Punk is the card
    -- that prints one.
    Spec.assertBool s (any (anyFace (any (isUnpreventable . snd) . cardPlayerScopes) . Printing.card) ps) "the pool has a card printing unpreventable damage"
    Spec.assertEqWith s "CR 615.12 names no player, so its carrier is scoped to every player" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Spider-Punk rescoped rather than
  -- against a card file, exactly as the two cases above are.
  Spec.it s "the lint itself catches a narrowed unpreventable-damage carrier" $ do
    spiderPunk <- S.printingOf s registry "Spider-Punk"
    let card = S.combinedFace spiderPunk
        narrow ability = ability {PlayerStaticAbility.scope = PlayerScope.You}
        narrowed = card {Face.playerAbilities = fmap narrow (Face.playerAbilities card)}
        offends = any (uncurry unpreventableScopeOffends) . cardPlayerScopes
    Spec.assertBool s (not (offends card)) "the real Spider-Punk names nobody and is accepted"
    Spec.assertBool s (offends narrowed) "and the same card scoped to its controller is rejected"
    -- The ban is CR 615.12's alone: rescoping does not condemn a card whose
    -- effects are all asked about a player. Prowling Serpopard is the printing
    -- that legitimately says PlayerScope.You.
    serpopard <- S.printingOf s registry "Prowling Serpopard"
    Spec.assertBool s (not (offends (S.combinedFace serpopard))) "a You-scoped countering prohibition is accepted"
    -- And the STORED carrier, which no printing pairs with CR 615.12 yet:
    -- Silence's own Effect.AffectPlayers, saying "damage can't be prevented"
    -- instead of what it says. Skullcrack is the printing that would make this
    -- shape real, and this is what keeps the sweep honest until it lands.
    silence <- S.printingOf s registry "Silence"
    let unpreventable effect = case effect of
          Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration scope _) -> Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration scope (PlayerEffect.DamageCantBePrevented anyDamage))
          other -> other
        widen effect = case effect of
          Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ playerEffect) -> Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration (AffectedPlayers.Scoped PlayerScope.EachPlayer) playerEffect)
          other -> other
        overSpell f face =
          face
            { Face.spell =
                (Face.spell face)
                  { Modal.modes = fmap (\mode -> mode {Mode.clauses = fmap (\c -> c {Clause.effects = fmap f (Clause.effects c)}) (Mode.clauses mode)}) (Modal.modes (Face.spell face))
                  }
            }
        silenced = S.combinedFace silence
    Spec.assertBool s (not (offends silenced)) "the real Silence, whose stored effect is scoped to its opponents, is accepted"
    Spec.assertBool s (offends (overSpell unpreventable silenced)) "a stored CR 615.12 effect scoped to opponents is rejected"
    Spec.assertBool s (not (offends (overSpell (widen . unpreventable) silenced))) "and the same stored effect scoped to every player is accepted"
  -- The pattern axis of the same carrier, and engineOnlyOffends' twin: a card
  -- may narrow CR 615.12 by kind or by source, and may not name the RECIPIENT,
  -- which only Resolve can bake. See unpreventablePatternOffends.
  Spec.it s "no card authors a recipient into CR 615.12's damage pattern" $ do
    ps <- S.allPrintings s
    let patterns = concatMap (overFaces (fmap snd . cardPlayerScopes) . Printing.card) ps
        offenders = filter (anyFace (any (unpreventablePatternOffends . snd) . cardPlayerScopes) . Printing.card) ps
    -- The non-vacuity guard, and the guard that the AUTHORABLE axes really are
    -- authored: Spider-Punk narrows nothing and Excruciator names TheSource, so
    -- the sweep has both a permissive and a narrowed pattern to look at.
    Spec.assertBool s (any isUnpreventable patterns) "the pool has a card printing unpreventable damage"
    Spec.assertBool s (elem (PlayerEffect.DamageCantBePrevented anyDamage) patterns) "Spider-Punk's pattern narrows nothing"
    Spec.assertBool s (elem (PlayerEffect.DamageCantBePrevented anyDamage {DamagePattern.whatSource = Filter.Type.IsSource}) patterns) "Excruciator's names its own source"
    Spec.assertEqWith s "CR 615.7's recipient is baked, never printed" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Excruciator restated rather than
  -- against a card file, exactly as the cases above are.
  Spec.it s "the lint itself catches a printed recipient on CR 615.12" $ do
    excruciator <- S.printingOf s registry "Excruciator"
    let card = S.combinedFace excruciator
        bake playerEffect = case playerEffect of
          PlayerEffect.DamageCantBePrevented pattern_ ->
            PlayerEffect.DamageCantBePrevented pattern_ {DamagePattern.whichRecipient = Just (Recipient.ToPlayer S.alice)}
          other -> other
        restate f ability = ability {PlayerStaticAbility.effect = f (PlayerStaticAbility.effect ability)}
        over f = card {Face.playerAbilities = fmap (restate f) (Face.playerAbilities card)}
        offends = any (unpreventablePatternOffends . snd) . cardPlayerScopes
        kind playerEffect = case playerEffect of
          PlayerEffect.DamageCantBePrevented pattern_ ->
            PlayerEffect.DamageCantBePrevented pattern_ {DamagePattern.whichKind = Just DamageKind.Combat}
          other -> other
    Spec.assertBool s (not (offends card)) "the real Excruciator names a source and no recipient, and is accepted"
    Spec.assertBool s (offends (over bake)) "and the same clause naming a shielded player is rejected"
    -- Frenzied Baloth's axis, which is authorable and must stay so: narrowing by
    -- KIND is not what this lint bans.
    Spec.assertBool s (not (offends (over kind))) "narrowing the same clause to combat damage is accepted"
  -- CR 205.1 and CR 114.3, which is one biconditional read across the two kinds
  -- of face a corpus file holds. A card's type line "contains the card's card
  -- type(s)", which Pawl.Codec.TypeLine reads as at least one; CR 205.2c says
  -- "tokens have card types even though they aren't cards", so a minted token's
  -- face is held to the same bar. An EMBLEM is the one face with none: CR 114.3
  -- gives it "no characteristics other than the abilities defined by the effect
  -- that created it" and CR 114.5 adds that "Emblem isn't a card type".
  --
  -- A lint rather than a codec rule, because the wire cannot tell the three
  -- apart: Pawl.Codec.Face decodes an absent `typeLine` as the empty one for the
  -- emblem's sake, and this is what stops a card or a token quietly doing the
  -- same. Pawl.Codec.TypeLine still rejects a type line that is PRESENT and empty.
  Spec.it s "CR 205.1 / 114.3 only an emblem's face has no card type" $ do
    ps <- S.allPrintings s
    let typeless c = Set.null (TypeLine.types (Face.typeLine c))
        offends card =
          let printed = NonEmpty.toList (Card.Type.faces card)
              minted = concatMap mintedFacesTagged printed
           in any typeless printed
                || any (\(kind, face) -> typeless face /= (kind == MintedEmblem)) minted
        offenders = filter (offends . Printing.card) ps
    Spec.assertEqWith s "only an emblem is typeless" (fmap (S.nameOf . Printing.card) offenders) []
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
    let isPlaneswalker c = Set.member CardType.Planeswalker (TypeLine.types (Face.typeLine c))
        offends c = isPlaneswalker c /= Maybe.isJust (Face.loyalty c)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "planeswalker iff loyalty" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 310.4 / 210.1: the same biconditional one rule number over. "Defense is a
  -- characteristic that battles have", so a battle without one has nothing for CR
  -- 310.4b's intrinsic replacement to place and would enter with no defense
  -- counters at all; a non-battle with a printed defense carries a number no rule
  -- reads.
  --
  -- Projection.intrinsicReplacementsOf's own comment leans on this in both
  -- directions too, which is why it is a lint and not a per-card assertion.
  Spec.it s "a card is a battle iff it has a printed defense" $ do
    ps <- S.allPrintings s
    let isBattle c = Set.member CardType.Battle (TypeLine.types (Face.typeLine c))
        offends c = isBattle c /= Maybe.isJust (Face.defense c)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "battle iff defense" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 309.4 / 309.1: the third biconditional, on the same terms. A dungeon
  -- without rooms has nowhere for CR 309.4a's venture marker to go; a non-dungeon
  -- with rooms carries a graph no rule reads, since CR 309.4's rooms are printed
  -- on dungeon cards and nowhere else.
  Spec.it s "a card is a dungeon iff it has rooms" $ do
    ps <- S.allPrintings s
    let isDungeon c = Set.member CardType.Dungeon (TypeLine.types (Face.typeLine c))
        offends c = isDungeon c /= not (Seq.null (Face.rooms c))
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "dungeon iff rooms" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 309.4 / 309.5a: every arrow points at a room this card actually has. An
  -- out-of-range exit would be offered by Prompt.ChooseRoom and then move the
  -- marker onto a room that does not exist, which no rule describes.
  Spec.it s "CR 309.4 every room's arrows point at rooms of the same card" $ do
    ps <- S.allPrintings s
    let offends c =
          let rooms = Face.rooms c
              inRange e = RoomIndex.unwrap e < Natural.length rooms
           in not (all (all inRange . DungeonRoom.exits) rooms)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "arrows in range" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 309.6: the BOTTOMMOST room is where the dungeon ends, and
  -- Pawl.Engine.Dungeon.isBottommost reads that off the position. This lint is
  -- what makes position and the absence of arrows agree: the last room has no
  -- arrows out of it, and no earlier room is a dead end the marker could never
  -- leave.
  Spec.it s "CR 309.6 a dungeon's last room is its only one with no arrows" $ do
    ps <- S.allPrintings s
    let offends c = case Seq.viewr (Face.rooms c) of
          Seq.EmptyR -> False
          earlier Seq.:> lastRoom ->
            not (Set.null (DungeonRoom.exits lastRoom))
              || any (Set.null . DungeonRoom.exits) earlier
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "one dead end, and it is last" (fmap (S.nameOf . Printing.card) offenders) []
  -- There is deliberately NO fourth biconditional here pairing the creature card
  -- type with a printed power, and the omission is a decision rather than a gap.
  -- CR 208.3 names the counterexample outright -- "even if it's a card with a
  -- power and toughness printed on it (such as a Vehicle)" -- and CR 301.7a says
  -- the same from the subtype's side, so Consulate Dreadnought is a noncreature
  -- card with a printed 7/11 and is not an offender. What holds instead is CR
  -- 208.1's weaker pairing below: the box in the lower right corner holds two
  -- numbers or none.
  --
  -- The three lints above are safe from that exception because loyalty and
  -- defense have no analogue of rule 208.3 -- no rule prints either number on a
  -- card of another type.
  Spec.it s "CR 208.1 a card has a printed power iff it has a printed toughness" $ do
    ps <- S.allPrintings s
    let offends c = Maybe.isJust (Face.power c) /= Maybe.isJust (Face.toughness c)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "power iff toughness" (fmap (S.nameOf . Printing.card) offenders) []
  -- What makes Pawl.Engine.Card.faceNamed's answer unique, and so what makes
  -- referring to a face BY NAME well-defined (CR 709.4a). Held over the whole
  -- pool rather than by construction, because a card file is data.
  --
  Spec.it s "a card's face names are pairwise distinct" $ do
    ps <- S.allPrintings s
    let offenders = filter (distinctFaceNamesOffends . Printing.card) ps
    -- The guard the sibling lints carry: over a pool of one-face cards this
    -- sweep passes on every card without comparing two names, and so proves
    -- nothing. Wax // Wane is what makes it non-vacuous.
    Spec.assertBool s (any ((> 1) . length . Card.Type.faces . Printing.card) ps) "the pool has a multi-face card to lint"
    Spec.assertEqWith s "no card repeats a face name" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against a hand-built offender rather than a
  -- card file: a card that repeats a face name must not be loadable.
  Spec.it s "the lint itself catches a card that repeats a face name" $ do
    let wax = vanillaFace "Wax" instantLine
        offender = Card.Type.MkCard {Card.Type.layout = Layout.Split, Card.Type.faces = wax NonEmpty.:| [wax]}
    Spec.assertBool s (distinctFaceNamesOffends offender) "two faces sharing one name are rejected"
  -- CR 709.5h names "a PARTICULAR half", and CR 709.5j says which halves there
  -- are: "Some cards refer to a 'door' of a Room permanent. A door is a half of
  -- that permanent." So the door a "when you unlock this door" trigger names has
  -- to be one of its own card's faces -- Pawl.Engine.Event.matchesTrigger
  -- compares the two names, and a condition naming anything else can never fire.
  --
  -- Held over the pool rather than by construction, for the reason the
  -- pairwise-distinct lint above gives: a card file is data. Non-vacuity is
  -- asserted the same way, since a pool with no such condition would pass this
  -- sweep without comparing anything.
  Spec.it s "an unlock trigger names one of its own card's faces" $ do
    ps <- S.allPrintings s
    let doors c = [n | TriggerCondition.SelfHalfUnlocked n <- fmap TriggeredAbility.condition (Face.triggeredAbilities c)]
        offends card = any (any (`notElem` fmap Face.name (NonEmpty.toList (Card.Type.faces card))) . doors) (Card.Type.faces card)
        offenders = filter (offends . Printing.card) ps
    Spec.assertBool
      s
      (not (all (all (null . doors) . Card.Type.faces . Printing.card) ps))
      "the pool has a card with an unlock trigger to lint"
    Spec.assertEqWith s "every door named is a face of the card naming it" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 709.5a, swept over the pool. See sharedTypeLineOffends for what the rule
  -- asks and why the check is full type-line equality.
  Spec.it s "CR 709.5a a Room's faces agree on their shared type line" $ do
    ps <- S.allPrintings s
    let offenders = filter (sharedTypeLineOffends . Printing.card) ps
        rooms = filter (Card.hasSharedTypeLine . Printing.card) ps
    -- The guard the sibling lints carry, and it bites harder here than most: over
    -- a pool with no Room at all this sweep compares nothing, and over a Room with
    -- one face it compares a line against itself.
    Spec.assertBool s (any ((> 1) . length . Card.Type.faces . Printing.card) rooms) "the pool has a multi-face Room to lint"
    Spec.assertEqWith s "no Room's halves disagree" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against the real card restated rather than a card
  -- file, as the repeated-face-name lint above does it: a Room whose halves
  -- disagree must not be loadable. Restating a printed Room rather than building
  -- one is what makes this a claim about the corpus's own shape.
  Spec.it s "the lint itself catches a Room whose halves disagree" $ do
    furnace <- S.printingOf s registry "Roaring Furnace"
    let card = Printing.card furnace
        -- Every half but the left one restated, so each mutation below is a
        -- disagreement rather than a card-wide edit the lint would accept.
        retype f = case Card.Type.faces card of
          x NonEmpty.:| xs -> card {Card.Type.faces = x NonEmpty.:| fmap f xs}
        addType face =
          face
            { Face.typeLine =
                (Face.typeLine face)
                  { TypeLine.types = Set.insert CardType.Artifact (TypeLine.types (Face.typeLine face))
                  }
            }
        dropSubtype face =
          face {Face.typeLine = (Face.typeLine face) {TypeLine.subtypes = Set.empty}}
        addSupertype face =
          face
            { Face.typeLine =
                (Face.typeLine face)
                  { TypeLine.supertypes = Set.singleton Supertype.Legendary
                  }
            }
    Spec.assertBool s (not (sharedTypeLineOffends card)) "the real Roaring Furnace // Steaming Sauna is accepted"
    Spec.assertBool s (sharedTypeLineOffends (retype addType)) "one half gaining a card type is rejected"
    Spec.assertBool s (sharedTypeLineOffends (retype dropSubtype)) "one half losing the Room subtype is rejected"
    -- The set CR 709.5a does not name, and which sharedTypeLineOffends checks
    -- anyway on CR 709.5's premise that the line is one printed line.
    Spec.assertBool s (sharedTypeLineOffends (retype addSupertype)) "one half gaining a supertype is rejected"
    -- NOT an offence on a layout whose halves print their own lines: Onward //
    -- Victory is Instant against Sorcery, and CR 709.4c is what makes that legal
    -- authoring rather than a defect.
    victory <- S.printingOf s registry "Onward"
    Spec.assertBool s (not (sharedTypeLineOffends (Printing.card victory))) "a Split card's halves may differ"
  -- CR 603.2's event triggers and CR 603.8's state triggers are gathered by two
  -- different scans, so one ability may not be both. See anyOfOffends for the two
  -- shapes this rejects and why each would be incoherent rather than merely odd.
  --
  -- Swept over the pool, with the non-vacuity assertion its neighbours carry: a
  -- pool with no AnyOf at all would pass this without examining anything.
  -- Balemurk Leech is the pool's one AnyOf, and it is ACCEPTED here.
  Spec.it s "CR 603.2 no AnyOf mixes in a state trigger or nests another AnyOf" $ do
    ps <- S.allPrintings s
    let conditions c = fmap TriggeredAbility.condition (Face.triggeredAbilities c)
        isAnyOf c = case c of TriggerCondition.AnyOf _ -> True; _ -> False
        offenders = filter (anyFace (any anyOfOffends . conditions) . Printing.card) ps
    Spec.assertBool
      s
      (any (anyFace (any isAnyOf . conditions) . Printing.card) ps)
      "the pool has a card with an AnyOf condition to lint"
    Spec.assertEqWith s "every AnyOf holds only event triggers, flat" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against hand-built offenders rather than card files,
  -- as the repeated-face-name lint above does it.
  Spec.it s "the lint itself catches a state trigger and a nested AnyOf inside an AnyOf" $ do
    let never = Condition.Type.Compares (Compares.MkCompares (Quantity.Type.Literal 0) Comparison.Exactly (Quantity.Type.Literal 1))
        fine = TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.RoomFullyUnlocked PlayerRelation.You]
    Spec.assertBool s (not (anyOfOffends fine)) "the control: two event triggers side by side are fine"
    Spec.assertBool s (anyOfOffends (TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.StateIs never])) "a CR 603.8 state trigger inside an AnyOf is rejected"
    Spec.assertBool s (anyOfOffends (TriggerCondition.AnyOf [fine])) "and so is a nested AnyOf"
    -- The recursion is what the nesting check buys: a state trigger one level
    -- down is caught too.
    Spec.assertBool s (anyOfOffends (TriggerCondition.AnyOf [TriggerCondition.AnyOf [TriggerCondition.StateIs never]])) "including a state trigger buried one level down"
    -- NOT an offence outside an AnyOf: a bare state trigger is how every CR 603.8
    -- card in the pool is written, and this lint must not reject those.
    Spec.assertBool s (not (anyOfOffends (TriggerCondition.StateIs never))) "a bare state trigger is left alone"
  Spec.it s "no mode declares a slot named enchant" $ do
    ps <- S.allPrintings s
    let offends c = any (Map.member Card.enchantSlot . Mode.targetSlots) (Modal.modes (Face.spell c))
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "the enchant slot is never hand-declared" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 701.3a: "An Aura, Equipment, or Fortification can't be attached to an
  -- object or player it couldn't enchant, equip, or fortify, respectively." The
  -- atom that asks that question is answerable only where an attach frames the
  -- match, and vacuously False everywhere else. See canHostSubjectOffends for the
  -- two offences this one predicate covers.
  Spec.it s "CR 701.3a no card asks CanHostSubject outside an attach's destination" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace canHostSubjectOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only in an AttachTarget destination" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the one card that does is
    -- ACCEPTED here rather than skipped. Aura Graft's "another permanent it can
    -- enchant" is the whole legal use, so a lint that swept past it would be
    -- indistinguishable from one that swept past everything.
    graft <- S.printingOf s registry "Aura Graft"
    Spec.assertEqWith
      s
      "Aura Graft's one atom is framed by its own attach"
      (canHostSubjectCounts (S.combinedFace graft))
      (1, 0)
    Spec.assertEqWith
      s
      "and it is the pool's only one"
      (sum (fmap (uncurry (+) . canHostSubjectCounts . S.combinedFace) ps))
      1
    -- The traversal reaches a Filter position no effect, target slot or affected
    -- set would have led it to: CR 702.29e's typecycling predicate, on a real
    -- card. Its absence would not show up in the sweep above, because Ash Barrens
    -- does not author the atom -- only in this.
    barrens <- S.printingOf s registry "Ash Barrens"
    Spec.assertBool
      s
      ( elem
          (False, Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic])
          (cardFilters (S.combinedFace barrens))
      )
      "CR 702.29e landcycling's filter is a position the sweep walks"
  -- The two source-power comparisons are answerable only where the CONTEXT
  -- supplies a source power: Filter.Context.sourcePower is filled by
  -- Pawl.Engine.Target.admittedGiven for a target slot (CR 702.134a), by
  -- Pawl.Engine.Event.matchesTrigger for CR 702.149a's condition and by
  -- Pawl.Engine.CombatRestriction.cantBeBlockedBy for CR 701.54c's blocking
  -- restriction, and is Nothing everywhere else -- so either atom in a card's
  -- affected set, Count filter or search filter would be a silent False. Only
  -- Pawl.Engine.Keyword's mentor and training and Pawl.Engine.Ring's emblem write
  -- them, and this is what keeps that true.
  Spec.it s "CR 702.134a / CR 702.149a no card writes a source-power comparison" $ do
    ps <- S.allPrintings s
    let atoms c = jsonAtoms (Text.pack "PowerLessThanSource") (Codec.encode (Face.Codec.codec Card.codec) c)
        greater c = jsonAtoms (Text.pack "PowerGreaterThanSource") (Codec.encode (Face.Codec.codec Card.codec) c)
        offenders = filter (anyFace (\c -> atoms c /= 0 || greater c /= 0) . Printing.card) ps
    Spec.assertEqWith s "the atoms are the engine's alone" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous, the way the sweep above would be on its own: the same counter
    -- over a hand-built face that DOES carry the atom -- buried under all three
    -- combinators, in a target slot, the one position a card author would reach
    -- for -- finds it.
    piker <- S.printingOf s registry "Goblin Piker"
    let buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.PowerLessThanSource]]
        targetSlot = TargetSlot.required Pool.Creatures (Just buried)
        planted =
          (S.combinedFace piker)
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) targetSlot)))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "a planted atom is seen" (atoms planted) 1
    let buriedGreater = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.PowerGreaterThanSource]]
        plantedGreater =
          planted
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.Creatures (Just buriedGreater)))))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "and so is its sibling" (greater plantedGreater) 1
  -- CR 702.39a's atom is in exactly the position CR 702.134a's is:
  -- Filter.Context.defendingPlayer is filled by Pawl.Engine.Target.admittedGiven
  -- and is Nothing everywhere else, so a card writing it anywhere else would get a
  -- silent False. Only Pawl.Engine.Keyword.provoke writes it.
  Spec.it s "CR 702.39a no card writes ControlledByDefendingPlayer" $ do
    ps <- S.allPrintings s
    let atoms c = jsonAtoms (Text.pack "ControlledByDefendingPlayer") (Codec.encode (Face.Codec.codec Card.codec) c)
        offenders = filter (anyFace ((/= 0) . atoms) . Printing.card) ps
    Spec.assertEqWith s "the atom is the engine's alone" (fmap (S.nameOf . Printing.card) offenders) []
    -- Not vacuous, for the sibling sweep's reason: the same counter over a
    -- hand-built face carrying the atom in a target slot finds it.
    piker <- S.printingOf s registry "Goblin Piker"
    let buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.ControlledByDefendingPlayer]]
        targetSlot = TargetSlot.required Pool.Creatures (Just buried)
        planted =
          (S.combinedFace piker)
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) targetSlot)))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "a planted atom is seen" (atoms planted) 1
  -- CR 603.2's baked half is in Modification.SetController's position rather
  -- than CR 702.39a's: a PlayerId that only a resolution can know, round-tripped
  -- by a total codec, so nothing but this keeps card JSON from naming a seat
  -- (#199). The UNBAKED atom beside it is card data and is not swept -- Trygon
  -- Predator writes it, and Pawl.Engine.Resolve.modeSlots is what checks the slot
  -- it names is one the ability's condition binds.
  Spec.it s "CR 603.2 no card writes ControlledByPlayer" $ do
    ps <- S.allPrintings s
    let atoms c = jsonAtoms (Text.pack "ControlledByPlayer") (Codec.encode (Face.Codec.codec Card.codec) c)
        offenders = filter (anyFace ((/= 0) . atoms) . Printing.card) ps
    Spec.assertEqWith s "the baked atom is the engine's alone" (fmap (S.nameOf . Printing.card) offenders) []
    -- Not vacuous, for the sibling sweep's reason: the same counter over a
    -- hand-built face carrying the atom in a target slot finds it.
    piker <- S.printingOf s registry "Goblin Piker"
    let buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not (Filter.Type.ControlledByPlayer (PlayerId.MkPlayerId 1))]]
        targetSlot = TargetSlot.required Pool.Creatures (Just buried)
        planted =
          (S.combinedFace piker)
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) targetSlot)))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "a planted atom is seen" (atoms planted) 1
  -- CR 611.2b's baked half, in exactly the position the atom above holds: a
  -- PlayerId only a resolution can know, written by Pawl.Engine.Condition.bakeBound
  -- as a "for as long as" duration begins and round-tripped by a total codec
  -- (Pawl.Codec.Expiry serialises a whole stored condition), so nothing but this
  -- keeps card JSON from naming a seat. The UNBAKED PlayerRef.InSlot beside it is
  -- card data -- Garland, Royal Kidnapper writes it -- and is not swept.
  Spec.it s "CR 611.2b no card writes a Specific PlayerRef" $ do
    ps <- S.allPrintings s
    let atoms c = jsonAtoms (Text.pack "Specific") (Codec.encode (Face.Codec.codec Card.codec) c)
        offenders = filter (anyFace ((/= 0) . atoms) . Printing.card) ps
    Spec.assertEqWith s "the baked reference is the engine's alone" (fmap (S.nameOf . Printing.card) offenders) []
    -- Not vacuous, for the sibling sweeps' reason: the same counter over a
    -- hand-built face carrying the reference inside a duration's condition finds
    -- it.
    piker <- S.printingOf s registry "Goblin Piker"
    let crowned =
          Condition.Type.Compares
            ( Compares.MkCompares
                (Quantity.Type.IsMonarch (PlayerRef.Specific (PlayerId.MkPlayerId 1)))
                Comparison.AtLeast
                (Quantity.Type.Literal 1)
            )
        planted =
          (S.combinedFace piker)
            { Face.spell =
                Modal.MkModal
                  ( Seq.singleton
                      ( Mode.MkMode
                          (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.GainControl (DurationRef.MkDurationRef (Duration.ForAsLongAs crowned) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))))))
                          (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.Creatures Nothing))
                      )
                  )
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "a planted reference is seen" (atoms planted) 1
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
    let base = S.combinedFace piker
        slot = SlotName.MkSlotName (Text.pack "target")
        atom = Filter.Type.CanHostSubject
        buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not atom]]
        -- A one-mode, one-clause, mandatory spell running these effects and
        -- declaring these slots -- the smallest carrier that reaches a mode's
        -- clauses and its targetSlots at once.
        spellOf effects slots =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) slots))
            (ModeSelection.ChooseExactly 1)
        boostedBy quantity =
          StaticAbility.MkStaticAbility
            (Affected.Matching Filter.Type.IsSource)
            Nothing
            Nothing
            (NonEmpty.singleton (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness quantity (Quantity.Type.Literal 0))))
        planted =
          [ ( "a target slot",
              base {Face.spell = spellOf [] (Map.singleton slot (TargetSlot.required Pool.Permanents (Just buried)))}
            ),
            ( "CR 303.4a's enchant ability",
              base {Face.enchant = [TargetSlot.required Pool.Permanents (Just buried)]}
            ),
            ( "a static ability's affected set",
              base
                { Face.staticAbilities =
                    [ StaticAbility.MkStaticAbility
                        (Affected.Matching buried)
                        Nothing
                        Nothing
                        (NonEmpty.singleton (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1))))
                    ]
                }
            ),
            ( "a Count's filter",
              base
                { Face.staticAbilities =
                    [ boostedBy
                        ( Quantity.Type.Count
                            (Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer)) buried Aggregation.Members)
                        )
                    ]
                }
            ),
            ( "a Search filter",
              base {Face.spell = spellOf [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Quantity.Type.Literal 1, Search.filter = buried, Search.destination = SearchDestination.RevealThenHand}] Map.empty}
            ),
            ( "an ObjectRef.EachMatching set",
              base {Face.spell = spellOf [Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching buried) Regenerability.Regenerable Nothing)] Map.empty}
            ),
            ( "CR 603.6a's trigger condition",
              base
                { Face.triggeredAbilities =
                    [ oneEffectTrigger
                        (TriggerCondition.PermanentEnters buried)
                        (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.InSlot Binding.you) (Quantity.Type.Literal 1)))
                    ]
                }
            ),
            ( "CR 601.2f's sacrifice cost component",
              (S.combinedFace sorcerer)
                { Face.activatedAbilities =
                    fmap
                      (\a -> a {ActivatedAbility.cost = (ActivatedAbility.cost a) {Cost.Type.components = [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 buried)]}})
                      (Face.activatedAbilities (S.combinedFace sorcerer))
                }
            ),
            ( "CR 702.29e's typecycling predicate",
              base {Face.keywords = Set.singleton (Keyword.Cycling (Cycling.MkCycling (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Just buried)))}
            ),
            ( "CR 613.11's spell-cost modifier",
              base
                { Face.playerAbilities =
                    [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You (PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost buried 1))]
                }
            ),
            ( "CR 508.1c's combat restriction",
              base {Face.combatRestrictions = [CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless (Affected.Matching buried) Nothing)]}
            ),
            ( "CR 508.1h's cost to attack",
              base {Face.attackCosts = [AttackCost.MkAttackCost (Affected.Matching buried) (ManaCost.MkManaCost [ManaSymbol.Generic 2])]}
            ),
            ( "CR 614.1's counter-placement pattern",
              base
                { Face.replacementEffects =
                    [ReplacementEffect.CounterR (CounterR.MkCounterR (CounterPattern.MkCounterPattern Nothing Nothing ControllerRelation.Yours buried Nothing) (Scaling.AddMore 1))]
                }
            ),
            ( "a created token's own static ability",
              base
                { Face.spell =
                    spellOf
                      [ Effect.Create
                          Create.MkCreate
                            { Create.quantity = Quantity.Type.Literal 1,
                              Create.card = oneFaced (base {Face.staticAbilities = [StaticAbility.MkStaticAbility (Affected.Matching buried) Nothing Nothing (NonEmpty.singleton Modification.LoseAllAbilities)]}),
                              Create.riders = EntryRiders.defaultValue,
                              Create.slot = Nothing
                            }
                      ]
                      Map.empty
                }
            ),
            ( "CR 103.5b's pregame action",
              base {Face.mulliganActions = [[Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.quantity = Quantity.Type.Literal 1, Search.filter = buried, Search.destination = SearchDestination.RevealThenHand}]]}
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
      (fmap (\(_, card) -> jsonAtoms (Text.pack "CanHostSubject") (Codec.encode (Face.Codec.codec Card.codec) card)) planted)
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
            Filter.Type.HasKeyword (Keyword.Cycling (Cycling.MkCycling (Cost.Type.MkCost Nothing []) (Just atom)))
          ]
      )
      [1, 1, 1, 1, 1, 1]
    -- The ACCEPTING direction, twice: the real card, and the buried atom in an
    -- AttachTarget destination grafted onto a card with no attach of its own --
    -- so the acceptance is about the POSITION and not about Aura Graft.
    Spec.assertEqWith
      s
      "Aura Graft is accepted"
      (canHostSubjectOffends (S.combinedFace graft), canHostSubjectCounts (S.combinedFace graft))
      (False, (1, 0))
    let grafted = base {Face.spell = spellOf [Effect.AttachTarget (AttachTarget.MkAttachTarget slot buried)] (Map.singleton slot (TargetSlot.required Pool.Permanents Nothing))}
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
  let gs0 = Setup.emptyGame S.bothPlayers
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

basicLandSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
basicLandSpec s = Spec.describe s "BasicLand" $ do
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

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Card" $ do
  cardSpec s
  realSplitCardSpec s registry
  lintSpec s registry

-- CR 709.4 again, on a PRINTED split card rather than the invented `splitCard`
-- fixture above.
--
-- Onward // Victory is the pool's second real split card and the first whose
-- halves are asymmetric in ways the merge has to carry: {2}{R} Instant against
-- {2}{W} Sorcery, so the mana cost, the colours and the card types all come from
-- both halves -- and **aftermath on Victory alone**, so the keyword set, the
-- casting permission and the casting prohibition come from one half only. Wax //
-- Wane, the other real one, is two plain halves and reaches none of that.
--
-- This is what #660 asked for: it makes merges load-bearing that a synthetic
-- fixture could only pretend to exercise, because the behaviour below is read off
-- the same combined view Pawl.Engine.Cast consults.
realSplitCardSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
realSplitCardSpec s registry = Spec.describe s "RealSplitCard" $ do
  Spec.it s "CR 709.4b/709.4c Onward // Victory combines both halves' costs, colours and types" $ do
    printing <- S.printingOf s registry "Onward"
    let c = Card.combined (Printing.card printing)
    -- CR 709.4b: the combined mana cost is {2}{R} plus {2}{W}, so both colours and
    -- a mana value of 6. A merge that kept one half gives 3 and one colour.
    Spec.assertEqWith s "both colours" (Projection.printedColorsOf c) (Set.fromList [Color.Red, Color.White])
    Spec.assertEqWith s "mana value 3 + 3" (Quantity.manaValueOf c) 6
    -- CR 709.4c: "each card type specified on either of its halves" -- and these
    -- halves genuinely differ, unlike a card whose halves are both sorceries.
    Spec.assertEqWith s "instant and sorcery" (TypeLine.types (Face.typeLine c)) (Set.fromList [CardType.Instant, CardType.Sorcery])
    -- One-sided: aftermath is printed on Victory only, so a merge that took the
    -- LEFT half's keyword set -- which a record update over `l` does by default --
    -- leaves this empty.
    Spec.assertEqWith s "aftermath, from the right half alone" (Face.keywords c) (Set.singleton Keyword.Aftermath)
  m2bCardSpec s registry
  basicLandSpec s
