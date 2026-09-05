-- Covers Pawl.Engine.Card: card data, type-line rules, every printing, and the D4
-- dataflow lint.
module Pawl.CardSpec where

-- Aliased Condition.Type, matching Pawl.Types.Count below and the project-wide
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- Dotted, because Pawl.Types.Keyword already holds the short alias here (the
-- The json sublibrary's own modules, for the CR 701.3a completeness cross-check
-- The logic module, alongside Pawl.Types.Modal below: unambiguous under one
-- alias because the two modules export disjoint names (TriggerSpec's
-- alone: it counts the atom in a card's ENCODED form, which is a traversal of the
-- convention (FilterSpec/CardSpec's Filter.Type note): Pawl.Engine.Condition may
-- hand-maintained one below.
-- later be imported and must not collide.
-- precedent), and Modal.allEffects is how this lint reaches an activated or
-- reverse of TriggerSpec's split).
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.
-- triggered ability's effects (Card.allEffects only reaches the spell).
-- whole card written by somebody else and so an independent witness to the
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.QuantitySlot as QuantitySlot
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Resolve.Slots as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.AddSpellCost as AddSpellCost
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
import qualified Pawl.Types.AttachRestriction as AttachRestriction
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.BlockCost as BlockCost
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.CantAttackPlayer as CantAttackPlayer
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardLeavesGraveyard as CardLeavesGraveyard
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastFromZone as CastFromZone
import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.CounterRestriction as CounterRestriction
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryFlip as EntryFlip
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRestriction as EntryRestriction
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Equip as Equip
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.HandAction as HandAction
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.IncreaseActivationCost as IncreaseActivationCost
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LimitUnless as LimitUnless
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaRider as ManaRider
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Meld as Meld
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
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.OrElse as OrElse
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PerCreature as PerCreature
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotCount as SlotCount
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.UntapRestriction as UntapRestriction
import qualified Pawl.Types.WithCounters as WithCounters
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
      Face.vanguard = Nothing,
      Face.keywords = Set.empty,
      Face.colorIndicator = Set.empty,
      Face.staticAbilities = [],
      Face.spell = Face.defaultSpell,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.delayedAbilities = Map.empty,
      Face.rooms = Seq.empty,
      Face.dungeonEntryQuality = Nothing,
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
      Face.attachRestrictions = [],
      Face.counterRestrictions = [],
      Face.entryRestrictions = [],
      Face.attackCosts = [],
      Face.blockCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = [],
      Face.specialActions = [],
      Face.additionalCosts = [],
      Face.maximumX = [],
      Face.alternativeCosts = [],
      Face.costReductions = [],
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
youDraw :: Integer -> Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
youDraw n = Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal n) Nothing)

-- "This object has [keyword]" as a static ability (CR 604.1), the smallest
-- carrier Face.staticAbilities takes.
grantsItself :: Keyword.Keyword -> StaticAbility.StaticAbility Card.Type.Card
grantsItself keyword =
  StaticAbility.MkStaticAbility
    (Affected.Matching Filter.Type.IsSource)
    Nothing
    Set.empty
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
      Face.triggeredAbilities = [oneEffectTrigger TriggerCondition.SelfDies (youDraw 2)],
      -- One-sided like the supertype above, and for a field with a DEFAULT rather
      -- than a printed box: Wax prints nothing, so a fold that reads the left
      -- half's Counterable as an answer -- or a record update that never looks at
      -- the right half at all, which is what merge2 did before fuse landed --
      -- leaves the combined view counterable.
      Face.counterability = Counterability.CantBeCountered
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
    -- CR 709.4c reaches CR 113.6g's clause too -- "this spell can't be countered"
    -- is an ability in a half's text box -- and CR 702.102b hands the combined
    -- characteristics to a fused split spell, which is the object that reads this
    -- field (Pawl.Engine.Event.counterOne, through Game.faceOf). Only Wane prints
    -- it. What this does NOT reach is that reader: no printing pairs fuse with a
    -- can't-be-countered clause, so the fold is what the suite holds.
    Spec.assertEqWith s "the right half's can't-be-countered clause" (Face.counterability c) Counterability.CantBeCountered
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

-- Every Count reachable from an ObjectRef, through the Quantities it carries.
-- Delegated to Resolve.objectRefQuantities so this traversal and the engine's two
-- static-analysis passes (Resolve.slotsAreExhaustive, Resolve.readsX) cannot
-- disagree about which ARMS of an ObjectRef hold a number -- today that is
-- ObjectRef.TopOfLibrary's depth alone -- as Resolve.effectObjectRefs keeps the
-- three from disagreeing about which OPCODES hold a ref.
refCounts :: ObjectRef.ObjectRef -> [Count.Type.Count Quantity.Type.Quantity]
refCounts = concatMap quantityCounts . Resolve.objectRefQuantities

-- One planted effect per position an ObjectRef sits in, each ref naming its own
-- position, paired with what Resolve.effectObjectRefs owes back. The traversal
-- reads neither type parameter, so these are built at `Effect () ()`.
--
-- Written out rather than derived because there is nothing to derive it from:
-- the compiler forces an arm per OPCODE, not a ref per FIELD, so a second ref
-- field on a payload that already has one would answer with the first alone and
-- compile. This list is where that shows up.
objectRefPositions :: [(String, Effect.Effect () (), [ObjectRef.ObjectRef])]
objectRefPositions =
  [ ("deal-damage", Effect.DealDamage (DealDamage.MkDealDamage (Seq.fromList [DamagePart.MkDamagePart (plantedRef "dd1") (Quantity.Type.Literal 1), DamagePart.MkDamagePart (plantedRef "dd2") (Quantity.Type.Literal 1)]) Nothing Nothing), [plantedRef "dd1", plantedRef "dd2"]),
    ("modify-target", Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Flying) (plantedRef "mt")), [plantedRef "mt"]),
    ("restart-game", Effect.RestartGame (Just (plantedRef "rg")), [plantedRef "rg"]),
    ("destroy", Effect.Destroy (Destroy.MkDestroy (plantedRef "de") Regenerability.Regenerable Nothing Nothing Nothing), [plantedRef "de"]),
    ("move-to-zone", Effect.MoveToZone (MoveToZone.MkMoveToZone (plantedRef "mz") Zone.Exile plainRiders Nothing Nothing LibraryPlacement.OwnerChooses Nothing), [plantedRef "mz"]),
    ("reveal", Effect.Reveal (Reveal.MkReveal (plantedRef "rv") Nothing), [plantedRef "rv"]),
    ("look-at", Effect.LookAt (LookAt.MkLookAt (plantedRef "la") (SlotName.MkSlotName (Text.pack "seen"))), [plantedRef "la"]),
    ("explore", Effect.Explore (plantedRef "ex"), [plantedRef "ex"]),
    ("discard-these", Effect.Discard (Discard.These (plantedRef "di")), [plantedRef "di"]),
    ("create-copy", Effect.CreateCopy (CreateCopy.MkCreateCopy (Quantity.Type.Literal 1) (plantedRef "cc") plainRiders), [plantedRef "cc"]),
    ("become-copy", Effect.BecomeCopy (BecomeCopy.MkBecomeCopy (plantedRef "bc-original") (plantedRef "bc-subject")), [plantedRef "bc-original", plantedRef "bc-subject"]),
    ("copy-spell", Effect.CopyStackObject (CopyStackObject.MkCopyStackObject (plantedRef "cs") CopyTargets.Copied), [plantedRef "cs"]),
    -- CR 707.10d names a SECOND ref, the candidates', which the sweep must
    -- reach: a copy effect whose candidate description reads a slot no clause
    -- binds is a dangling read like any other.
    ("copy-spell-for-each", Effect.CopyStackObject (CopyStackObject.MkCopyStackObject (plantedRef "cs-ref") (CopyTargets.ForEach (plantedRef "cs-each"))), [plantedRef "cs-ref", plantedRef "cs-each"]),
    ("prevent-next-damage", Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage Duration.UntilEndOfTurn Nothing (Just (plantedRef "pn")) Nothing Nothing Nothing (Quantity.Type.Literal 1) Seq.empty), [plantedRef "pn"]),
    ("prevent-all-damage", Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage Duration.UntilEndOfTurn Nothing (Just (plantedRef "pa")) Nothing DamageDirection.DealtTo Nothing (Filter.Type.And []) Seq.empty), [plantedRef "pa"]),
    ("redirect-damage", Effect.RedirectDamage (RedirectDamage.MkRedirectDamage Duration.UntilEndOfTurn Nothing Nothing (Just (plantedRef "rd-from")) Nothing Nothing (plantedRef "rd-to") Nothing), [plantedRef "rd-from", plantedRef "rd-to"]),
    ("counter", Effect.Counter (Counter.MkCounter (plantedRef "co") Nothing Nothing), [plantedRef "co"]),
    ("put-counters", Effect.PutCounters (PutCounters.MkPutCounters CounterKind.PlusOnePlusOne (Quantity.Type.Literal 1) (plantedRef "pc")), [plantedRef "pc"]),
    ("move-counters", Effect.MoveCounters (MoveCounters.MkMoveCounters (plantedRef "mc-from") MovedKinds.Every Nothing (plantedRef "mc-to")), [plantedRef "mc-from", plantedRef "mc-to"]),
    ("put-counters-from", Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom (SlotName.MkSlotName (Text.pack "giver")) Nothing (plantedRef "pf")), [plantedRef "pf"]),
    ("tap", Effect.Tap (plantedRef "ta"), [plantedRef "ta"]),
    ("untap", Effect.Untap (plantedRef "un"), [plantedRef "un"]),
    ("detain", Effect.Detain (plantedRef "dt"), [plantedRef "dt"]),
    ("goad", Effect.Goad (plantedRef "go"), [plantedRef "go"]),
    ("does-not-untap-next", Effect.DoesNotUntapNext (plantedRef "du"), [plantedRef "du"]),
    ("transform", Effect.Transform (plantedRef "tr"), [plantedRef "tr"]),
    ("convert", Effect.Convert (plantedRef "cv"), [plantedRef "cv"]),
    ("meld", Effect.Meld (Meld.MkMeld (plantedRef "me") ()), [plantedRef "me"]),
    ("phase-out", Effect.PhaseOut (plantedRef "po"), [plantedRef "po"]),
    ("turn-face-down", Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown (plantedRef "tf") FaceDownCharacteristics.defaultValue), [plantedRef "tf"]),
    ("remove-from-combat", Effect.RemoveFromCombat (plantedRef "rc"), [plantedRef "rc"]),
    ("gain-control", Effect.GainControl (DurationRef.MkDurationRef Duration.UntilEndOfTurn (plantedRef "gc")), [plantedRef "gc"]),
    ("require-block", Effect.RequireBlock (RequireBlock.MkRequireBlock Duration.UntilEndOfTurn (plantedRef "rb-blocker") (plantedRef "rb-attacker")), [plantedRef "rb-blocker", plantedRef "rb-attacker"]),
    ("cant-be-regenerated", Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated Duration.UntilEndOfTurn (plantedRef "cb")), [plantedRef "cb"]),
    ("require-attack", Effect.RequireAttack (RequireAttack.MkRequireAttack Duration.UntilEndOfTurn (plantedRef "ra") (PlayerRef.Relative PlayerRelation.You)), [plantedRef "ra"]),
    ("forbid-block", Effect.ForbidBlock (ForbidBlock.MkForbidBlock Duration.UntilEndOfTurn (plantedRef "fb")), [plantedRef "fb"]),
    ("forbid-attack", Effect.ForbidAttack (ForbidAttack.MkForbidAttack Duration.UntilEndOfTurn (RestrictedCreatures.Named (plantedRef "fa")) Nothing), [plantedRef "fa"]),
    ("unsuspect", Effect.Unsuspect (plantedRef "us"), [plantedRef "us"]),
    ("shuffle-into-library", Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary Nothing (plantedRef "sl")), [plantedRef "sl"]),
    ("grant-play-from-exile", Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile Duration.UntilEndOfTurn (plantedRef "gp") ManaSpending.AsProduced), [plantedRef "gp"]),
    ("make-plotted", Effect.MakePlotted (plantedRef "mp"), [plantedRef "mp"]),
    ("for-each", Effect.ForEach (ForEach.MkForEach (plantedRef "fe") (SlotName.MkSlotName (Text.pack "each")) Seq.empty), [plantedRef "fe"])
  ]
  where
    plainRiders = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}

-- The ref plantedRef at one position, named for it so a position answering with
-- another position's ref is visible rather than merely absent.
plantedRef :: String -> ObjectRef.ObjectRef
plantedRef = ObjectRef.InSlot . SlotName.MkSlotName . Text.pack

-- One planted effect per position a PlayerRef sits in, each reference naming its
-- own position, paired with what Resolve.effectPlayerRefs owes back.
-- objectRefPositions' twin one type over, and written out for its reason: the
-- compiler forces an arm per OPCODE, not a reference per FIELD, so a second
-- PlayerRef field on a payload that already has one would answer with the first
-- alone and compile.
playerRefPositions :: [(String, Effect.Effect () (), [PlayerRef.PlayerRef])]
playerRefPositions =
  [ ("add-mana", Effect.AddMana (ManaAddition.MkManaAddition (plantedPlayer "am") ManaProduction.AnyColor 1 ManaRetention.Ordinary Nothing Nothing), [plantedPlayer "am"]),
    ("search", Effect.Search (Search.MkSearch (plantedPlayer "se-searcher") (plantedPlayer "se-owner") Set.empty Nothing (Filter.Type.And []) False SearchDestination.Battlefield), [plantedPlayer "se-searcher", plantedPlayer "se-owner"]),
    ("draw", Effect.Draw (Draw.MkDraw (plantedPlayer "dr") one Nothing), [plantedPlayer "dr"]),
    ("mill", Effect.Mill (Mill.MkMill (plantedPlayer "mi") one Nothing Nothing), [plantedPlayer "mi"]),
    ("scry", Effect.Scry (playerQuantity "sc"), [plantedPlayer "sc"]),
    ("surveil", Effect.Surveil (playerQuantity "su"), [plantedPlayer "su"]),
    ("fateseal", Effect.Fateseal (playerQuantity "fs"), [plantedPlayer "fs"]),
    ("lose-life", Effect.LoseLife (playerQuantity "ll"), [plantedPlayer "ll"]),
    ("gain-life", Effect.GainLife (playerQuantity "gl"), [plantedPlayer "gl"]),
    ("set-life-total", Effect.SetLifeTotal (playerQuantity "sl"), [plantedPlayer "sl"]),
    ("increase-speed", Effect.IncreaseSpeed (playerQuantity "is"), [plantedPlayer "is"]),
    ("decrease-speed", Effect.DecreaseSpeed (SpeedDecrease.MkSpeedDecrease (plantedPlayer "ds") one 0), [plantedPlayer "ds"]),
    ("create", Effect.Create (Create.MkCreate one () EntryRiders.defaultValue Nothing (plantedPlayer "cr")), [plantedPlayer "cr"]),
    ("skip-next-phase", Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase (plantedPlayer "sn") PhaseSelector.CombatPhase), [plantedPlayer "sn"]),
    ("gain-player-counters", Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters (plantedPlayer "gp") PlayerCounterKind.Rad one), [plantedPlayer "gp"]),
    ("remove-player-counters", Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters (plantedPlayer "rp") PlayerCounterKind.Rad one), [plantedPlayer "rp"]),
    ("require-attack", Effect.RequireAttack (RequireAttack.MkRequireAttack Duration.UntilEndOfTurn (plantedRef "ra") (plantedPlayer "ra-defender")), [plantedPlayer "ra-defender"]),
    ("blight", Effect.Blight (playerQuantity "bl"), [plantedPlayer "bl"]),
    ("take-extra-turn", Effect.TakeExtraTurn TakeExtraTurn.MkTakeExtraTurn {TakeExtraTurn.player = plantedPlayer "te", TakeExtraTurn.skips = Set.empty, TakeExtraTurn.count = Quantity.Type.Literal 1}, [plantedPlayer "te"]),
    ("shuffle-into-library", Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary (Just (plantedPlayer "si")) (plantedRef "si")), [plantedPlayer "si"]),
    ("shuffle", Effect.Shuffle (plantedPlayer "sh"), [plantedPlayer "sh"]),
    ("offer-cast", Effect.OfferCast (OfferCast.MkOfferCast (SlotName.MkSlotName (Text.pack "oc")) (plantedPlayer "oc-caster") CastObligation.Optional CastOffer.defaultValue), [plantedPlayer "oc-caster"]),
    -- CR 400.1's reference nested in the PLAYER EFFECT rather than in a field of
    -- the opcode -- the two CR 601.3 / 305.1 permissions that name whose zone
    -- (Sen Triplets). Both are planted, since Pawl.Engine.PlayerEffect's
    -- traversal is what the AffectPlayers arm delegates to and a missing arm
    -- there answers [] rather than failing to compile.
    ("affect-players-cast-from", affecting (PlayerEffect.CastFrom (CastFromZone.MkCastFromZone (InZone.MkInZone Zone.Hand (plantedPlayer "ap-cast")) (Filter.Type.And []))), [plantedPlayer "ap-cast"]),
    ("affect-players-play-lands-from", affecting (PlayerEffect.PlayLandsFrom (InZone.MkInZone Zone.Graveyard (plantedPlayer "ap-land"))), [plantedPlayer "ap-land"]),
    -- And an arm carrying none, so the traversal is shown answering nothing where
    -- there is nothing to answer.
    ("affect-players-cant-cast", affecting PlayerEffect.CantCastSpells, [])
  ]
  where
    one = Quantity.Type.Literal 1
    playerQuantity stem = PlayerQuantity.MkPlayerQuantity (plantedPlayer stem) one
    affecting effect = Effect.AffectPlayers (AffectPlayers.MkAffectPlayers Duration.UntilEndOfTurn (AffectedPlayers.Scoped PlayerScope.You) effect)

-- The same list one type in, for the four ObjectRef arms that count PER SEAT
-- (CR 400.1's per-player zones). Resolve.objectRefPlayerRefs is what owes these.
objectRefPlayerRefPositions :: [(String, ObjectRef.ObjectRef, [PlayerRef.PlayerRef])]
objectRefPlayerRefPositions =
  [ ("top-of-library", ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (plantedPlayer "tl") (Quantity.Type.Literal 1)), [plantedPlayer "tl"]),
    ("top-of-library-until", ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil (plantedPlayer "tu") (Filter.Type.And []) (Quantity.Type.Literal 1)), [plantedPlayer "tu"]),
    ("chosen-card-in-hand", ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand (plantedPlayer "ch") (Filter.Type.And [])), [plantedPlayer "ch"]),
    ("random-card-in-hand", ObjectRef.RandomCardInHand (plantedPlayer "rh"), [plantedPlayer "rh"])
  ]

-- plantedRef's player half, named for its position for that function's reason.
plantedPlayer :: String -> PlayerRef.PlayerRef
plantedPlayer = PlayerRef.InSlot . SlotName.MkSlotName . Text.pack

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
  -- CR 103.1's, read the same way and holding the same nothing.
  Quantity.Type.IsStartingPlayer _ -> []
  Quantity.Type.IsActivePlayer _ -> []
  Quantity.Type.HasDesignation _ -> []
  Quantity.Type.ClassLevel -> []
  Quantity.Type.WasKicked -> []
  -- No Count under CR 702.33d's per-cost tally: it reads a Cost off the spell's
  -- own announcement. That Cost is checked against the face's kicker keywords by
  -- timesKickedWithOffends, off the ENCODING, because none of this module's three
  -- Quantity traversals can carry a Cost.
  Quantity.Type.TimesKickedWith {} -> []
  Quantity.Type.TagWasSpent {} -> []
  Quantity.Type.WasToken -> []
  Quantity.Type.WasBlocking -> []
  Quantity.Type.DamageDealtToThisTurn -> []
  -- CR 122.1's per-player counter tally, another such scalar.
  Quantity.Type.PlayerCounters {} -> []
  -- CR 122.1's per-OBJECT tally, read off the object the quantity is evaluated
  -- against: a CounterKind with no Count beside it. The KIND may carry a Filter
  -- of its own (CR 122.1b), which quantityKindFilters below is what digs out.
  Quantity.Type.ObjectCounters _ -> []
  -- The kind-agnostic reading of that same tally: not even a CounterKind beside it.
  Quantity.Type.ObjectCountersOfAnyKind -> []
  -- CR 508.3b's combat record, read as a tally of players: a PlayerRef and
  -- nothing else, so no Count and no Filter here either.
  Quantity.Type.OpponentsAttacked _ -> []
  -- CR 701.9a's tally of logged discards: a PlayerRef and nothing else, so no
  -- Count and no Filter here either.
  Quantity.Type.CardsDiscardedThisTurn _ -> []
  Quantity.Type.LifeGainedThisTurn _ -> []
  -- CR 120.1's tally of logged damage: a PlayerRef and nothing else, so no Count
  -- and no Filter here either.
  Quantity.Type.PlayersDealtDamageThisTurn _ -> []
  -- The same log read as a TOTAL rather than a tally: a PlayerRef and nothing
  -- else here either.
  Quantity.Type.DamageDealtToPlayersThisTurn _ -> []
  -- CR 601.2i's tally of casts, read off the handoff snapshot: a PlayerRef and
  -- nothing else, so no Count and no Filter here either.
  Quantity.Type.SpellsCastLastTurn _ -> []
  -- CR 309.7's tally of completed dungeons, read off the player: a PlayerRef and
  -- nothing else, so no Count and no Filter here either.
  Quantity.Type.DungeonsCompleted _ -> []
  Quantity.Type.CompletedDungeon {} -> []
  -- CR 400.7's logged entry, read against the object the quantity is aimed at: no
  -- reference at all, so no Count and no Filter here either.
  Quantity.Type.EnteredThisTurn -> []
  -- CR 400.7's logged origin zone and CR 601.2a's logged cast zone: an InZone and
  -- nothing else, so no Count and no Filter here either. The shared-zone pairing
  -- their InZone could state is rejected at the decoder by
  -- Pawl.Codec.InZone.undividedShared; cardOffendsSharedZoneScope below restates
  -- it only over a Count's scope, so these two are covered once rather than twice
  -- (see #161).
  Quantity.Type.EnteredFrom _ -> []
  Quantity.Type.WasCastFrom _ -> []
  -- CR 509.1h's declaration, read against the object the quantity is evaluated
  -- against: a nullary leaf, so no Count and no Filter here either.
  Quantity.Type.BlockersBeyondFirst -> []
  -- CR 702.184c's engine-only substitution, Power's shape: no Count and no
  -- Filter here either.
  Quantity.Type.StationMeasure -> []
  -- Not a leaf: aiming the evaluation at another object does not stop the payload
  -- from being a Count, so the Filter lints must reach through it.
  Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot _ inner) -> quantityCounts inner

-- Every Count nested inside another Count's AGGREGATION: only Greatest carries
-- a per-member Quantity, and that Quantity may itself be a Count. Without this
-- descent the shared-zone lint below would sweep past a misauthored inner
-- scope.
countCounts :: Count.Type.Count Quantity.Type.Quantity -> [Count.Type.Count Quantity.Type.Quantity]
countCounts = concatMap quantityCounts . countQuantities

-- The Quantities a Count's AGGREGATION carries: only Greatest has one. Named so
-- countCounts above and quantityKindFilters below descend through the same field
-- rather than each spelling the aggregation out.
countQuantities :: Count.Type.Count Quantity.Type.Quantity -> [Quantity.Type.Quantity]
countQuantities count = case Count.Type.aggregation count of
  Aggregation.Members -> []
  Aggregation.DistinctCardTypes -> []
  Aggregation.Greatest quantity -> [quantity]

-- Every Quantity a Condition holds: both sides of a comparison, plus whatever a
-- disjunction or a conjunction nests (Pawl.Types.Condition).
--
-- ONE enumeration, and conditionCounts below and conditionFilters further down
-- both take from it. A Quantity carries card text on two axes -- the Filter of
-- every Count under it, and the Filter a CR 122.1b keyword counter hides under
-- its CounterKind -- so a traversal that reaches a Condition's Counts directly
-- reaches only the first, which is how the second went unswept here (#2740).
conditionQuantities :: Condition.Type.Condition -> [Quantity.Type.Quantity]
conditionQuantities condition = case condition of
  Condition.Type.Compares (Compares.MkCompares measured _ threshold) -> [measured, threshold]
  Condition.Type.Any conditions -> concatMap conditionQuantities conditions
  Condition.Type.All conditions -> concatMap conditionQuantities conditions

-- Every Count reachable from a Condition, off the enumeration above.
conditionCounts :: Condition.Type.Condition -> [Count.Type.Count Quantity.Type.Quantity]
conditionCounts = concatMap quantityCounts . conditionQuantities

-- Every Condition an "activate only ..." clause holds: only CR 602.5's OnlyIf
-- carries one, every other arm naming a window instead. Both card sweeps take
-- from it, conditionQuantities' role one type up.
restrictionConditions :: ActivationRestriction.ActivationRestriction -> [Condition.Type.Condition]
restrictionConditions restriction = case restriction of
  ActivationRestriction.SorcerySpeed -> []
  ActivationRestriction.DuringPhase _ -> []
  ActivationRestriction.DuringTurn _ -> []
  ActivationRestriction.AttackedThisStep -> []
  ActivationRestriction.AfterBlockersDeclared -> []
  ActivationRestriction.BeforeCombatDamage -> []
  ActivationRestriction.OnlyIf condition -> [condition]

-- CR 701.46a's per-clause gate. Mode.allEffects and Modal.allEffects drop clause
-- boundaries by design, so every lint that reaches a card through them needs
-- this beside it, or the gate's Counts -- and through them its Filters -- go
-- unswept.
modeClauseConditions :: Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Condition.Type.Condition]
modeClauseConditions = Maybe.mapMaybe Clause.condition . Foldable.toList . Mode.clauses

modalClauseConditions :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Condition.Type.Condition]
modalClauseConditions = concatMap modeClauseConditions . Modal.modes

-- Every Condition a Duration holds: only ForAsLongAs (CR 611.2b) carries one.
-- conditionQuantities' role one type up -- durationCounts below and
-- durationFilters further down both take from it, so the CR 122.1b kind under a
-- "for as long as" clause's number cannot be dropped where its Count is kept
-- (#2740).
durationConditions :: Duration.Duration -> [Condition.Type.Condition]
durationConditions duration = case duration of
  Duration.UntilEndOfTurn -> []
  Duration.Indefinite -> []
  Duration.Perpetual -> []
  Duration.UntilYourNextTurn -> []
  Duration.UntilEndOfYourNextTurn -> []
  Duration.ForAsLongAs condition -> [condition]
  Duration.UntilEndOfCombat -> []
  -- CR 116.2c's price is a Cost, whose Filters are swept by durationFilters
  -- below and never through a Condition -- an activated ability's own cost takes
  -- exactly that split (activatedAbilityCounts against activatedAbilityFilters).
  Duration.UntilPaid _ -> []
  Duration.UntilUsed -> []

-- Every Count reachable from a Duration, off the enumeration above.
durationCounts :: Duration.Duration -> [Count.Type.Count Quantity.Type.Quantity]
durationCounts = concatMap conditionCounts . durationConditions

-- Every Count reachable from a Modification: only its P/T quantities
-- (layers 7b/7c) carry one.
modificationCounts :: Projection.Modification -> [Count.Type.Count Quantity.Type.Quantity]
modificationCounts modification = case modification of
  Modification.GainKeyword _ -> []
  -- CR 702.34a's computed flashback carries no payload at all, so it reaches
  -- neither a Count nor, below, a Filter.
  Modification.GainFlashbackAtManaCost -> []
  -- CR 702.5a's granted enchant carries a TargetSlot, whose Filter can nest a
  -- Count -- but a Filter's Counts are reached through modificationFilters below
  -- and countFilters, never through this sweep, which is the answer GainKeyword
  -- gives above for the Filter inside its keyword.
  Modification.GainEnchant _ -> []
  -- CR 613.1f's other grant carries a whole ability, so the sweep descends into
  -- it exactly as it does into a printed one, whichever of CR 113.3's kinds it is.
  Modification.GainAbility granted -> case granted of
    GrantedAbility.Activated ability -> activatedAbilityCounts ability
    GrantedAbility.Triggered ability -> triggeredAbilityCounts ability
  Modification.LoseAllAbilities -> []
  -- Carries a name, which reaches no Count.
  Modification.LoseNamedAbility _ -> []
  -- Carries a Keyword, whose Filter is reached through modificationFilters below
  -- rather than through this sweep -- the answer GainKeyword gives above.
  Modification.LoseKeyword _ -> []
  -- Carries a payload-free family, which nests neither a Count nor a Filter.
  Modification.LoseKeywordFamily _ -> []
  Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) -> quantityCounts p <> quantityCounts t
  Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) -> quantityCounts p <> quantityCounts t
  Modification.SetLandSubtype _ -> []
  Modification.SetLandSubtypeToChosen -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCreatureSubtype _ -> []
  Modification.AddEveryCreatureSubtype -> []
  Modification.AddSubtype _ -> []
  Modification.AddCardType _ -> []
  Modification.SetCardType _ -> []
  Modification.AddSupertype _ -> []
  Modification.RemoveSupertype _ -> []
  Modification.ChangeSubtypeWord {} -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.AddColor _ -> []
  Modification.AddChosenColor -> []
  Modification.SwitchPowerToughness -> []
  -- Payload-free, both of them, so there is no Count to sweep.
  Modification.AssignCombatDamageWithToughness -> []
  Modification.GrantsStationToughness -> []

-- Every Count reachable from a StaticAbility: its modifications' P/T quantities,
-- plus CR 604.2's "as long as" gate, which is a Condition and so a pair of
-- Quantities -- and the leaves-the-battlefield duration beside it, which is a
-- Duration and so another Condition when it is a CR 611.2b "for as long as".
staticAbilityCounts :: StaticAbility.StaticAbility Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
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
  -- CR 701.27e's names a face, which is a CardName and not a Quantity.
  TriggerCondition.SelfTransformedInto _ -> []
  -- Its bystander sibling carries a Filter, which holds no Count either.
  TriggerCondition.PermanentTransforms _ -> []
  -- Its watcher-scoped sibling carries a Filter, and a Filter holds no Count for
  -- PermanentEnters' reason.
  TriggerCondition.PermanentTurnedFaceUp _ -> []
  -- CR 702.112b's condition carries a Filter for the same reason, and no Count.
  TriggerCondition.PermanentBecomesDesignated {} -> []
  TriggerCondition.SelfEvolves -> []
  -- CR 702.134c's is nullary too, so it holds no Quantity.
  TriggerCondition.AttachedCreatureMentors -> []
  -- CR 700.4's is nullary as well, for the same reason.
  TriggerCondition.AttachedCreatureDies -> []
  TriggerCondition.AttachedCreatureBecomesTapped -> []
  -- Nor does CR 702.149c's, for the same reason.
  TriggerCondition.SelfTrains -> []
  -- CR 701.21a's carries a PlayerRelation and a Filter, neither of which holds a
  -- Count.
  TriggerCondition.PermanentSacrificed {} -> []
  -- CR 603.3b's carries a PlayerRelation, which holds no Count.
  TriggerCondition.SagaFinalChapterTriggers _ -> []
  -- CR 603.6a's Filter is a predicate over the entering permanent, and a
  -- Filter holds no Count (Pawl.Types.Filter's atoms are all characteristics).
  TriggerCondition.PermanentEnters _ -> []
  TriggerCondition.CardPutIntoGraveyard _ -> []
  TriggerCondition.PermanentDies _ -> []
  TriggerCondition.PermanentsDie _ -> []
  TriggerCondition.PermanentLeavesTheBattlefield _ -> []
  TriggerCondition.PermanentReturnedToHand _ -> []
  TriggerCondition.PermanentsReturnedToHand _ -> []
  -- CR 603.10a's third family carries a Filter and a TurnScope, and neither holds
  -- a Count.
  TriggerCondition.CardLeavesGraveyard {} -> []
  TriggerCondition.StepBegins {} -> []
  TriggerCondition.StateIs condition -> conditionCounts condition
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  TriggerCondition.SelfIsDealtDamage -> []
  -- Its watcher-scoped sibling carries a Filter, and a Filter holds no Count.
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> []
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> []
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> []
  TriggerCondition.PlayerTookInitiative -> []
  TriggerCondition.OpponentLostLifeDuringYourTurn -> []
  TriggerCondition.SelfAttacks _ -> []
  -- CR 702.149a's Filter holds no Count for PermanentEnters' reason.
  TriggerCondition.SelfAttacksWithAnother _ -> []
  TriggerCondition.CreatureAttacksAlone _ -> []
  -- Nullary, so no Count either.
  TriggerCondition.CreatureAttacksYou -> []
  -- Nullary too, and rule 508.3b's "one or more" is the EVENT's grouping rather
  -- than a number this condition counts.
  TriggerCondition.AttachedPlayerIsAttacked -> []
  -- A PlayerRelation holds no Count, and rule 508.3d's "one or more" is the
  -- EVENT's grouping for the arm above's reason, not a number this condition
  -- counts.
  TriggerCondition.PlayerAttacks _ -> []
  -- The floor this one DOES carry is a bare Natural, SelfBlocksAtLeast's shape
  -- below, rather than a Count over Quantities; its Filter holds no Count for
  -- PermanentEnters' reason.
  TriggerCondition.PlayerAttacksWith {} -> []
  -- Two PlayerRelations hold no Count, and rule 508.3e's "one or more" is the
  -- EVENT's grouping for AttachedPlayerIsAttacked's reason.
  TriggerCondition.PlayerAttacksPlayer {} -> []
  -- CR 702.105a compares life totals rather than counting objects, so no Count.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> []
  TriggerCondition.SelfBlocks -> []
  -- CR 509.3b names the attacker without counting anything, and its Filter holds
  -- no Count for PermanentEnters' reason.
  TriggerCondition.SelfBlocksCreature _ -> []
  TriggerCondition.SelfBlocksAtLeast _ -> []
  -- CR 509.3e's filtered form spends the number on a quality instead, so its
  -- Filter holds no Count either.
  TriggerCondition.SelfBlocksOneOrMore _ -> []
  TriggerCondition.SelfBecomesBlocked -> []
  -- CR 509.3d's Filter is a predicate over the blocker, and holds no Count for
  -- PermanentEnters' reason.
  TriggerCondition.SelfBecomesBlockedBy _ -> []
  TriggerCondition.PermanentBecomesBlockedBy _ -> []
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> []
  -- CR 509.3e's bystander form counts BLOCKERS rather than objects a Count
  -- names, and its PlayerRelation is no Count either.
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> []
  TriggerCondition.SelfAttacksUnblocked -> []
  TriggerCondition.SelfCycled -> []
  TriggerCondition.SelfRevealedForMiracle -> []
  TriggerCondition.SelfDiscarded -> []
  -- CR 701.9a's discard condition is a PlayerRelation, which holds no Count.
  TriggerCondition.PlayerDiscards _ -> []
  -- CR 702.29a's cycling read as a discard: the same PlayerRelation, so no Count
  -- either.
  TriggerCondition.PlayerCycles _ -> []
  TriggerCondition.PlayerDrawsNthCard {} -> []
  -- CR 725.1's crowning condition is a PlayerRelation too.
  TriggerCondition.PlayerBecomesMonarch _ -> []
  -- CR 603.7's slot-named condition holds a SlotName, which is no Count.
  TriggerCondition.LoseControlOfBound _ -> []
  TriggerCondition.RoomEntered _ -> []
  -- CR 309.7's condition carries a PlayerRelation, which is no Count.
  TriggerCondition.PlayerCompletesDungeon _ -> []
  -- CR 701.22d and CR 701.25d carry a PlayerRelation and CR 702.170a nothing
  -- at all, so none of the three holds a Count. CR 701.44b holds a Filter, and
  -- a Filter holds no Count for PermanentEnters' reason above.
  TriggerCondition.PlayerScries _ -> []
  TriggerCondition.RingTemptsPlayer _ -> []
  TriggerCondition.PlayerSurveils _ -> []
  TriggerCondition.PlayerRollsDice _ -> []
  TriggerCondition.PlayerWinsCoinFlip _ -> []
  TriggerCondition.SelfBecomesPlotted -> []
  TriggerCondition.PermanentExplores _ -> []
  -- CR 701.43d carries nothing at all, so no Count either.
  TriggerCondition.SelfExerted -> []
  -- CR 701.3a's carries a Filter, and a Filter holds no Count for
  -- PermanentTurnedFaceUp's reason.
  TriggerCondition.SelfBecomesAttachedBy _ -> []
  -- CR 603.12's carries nothing at all, so no Count.
  TriggerCondition.Reflexive -> []
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
  -- Rule 615.13's other reading carries a Filter, which holds no Count either.
  TriggerCondition.SelfPreventsDamage _ -> []
  TriggerCondition.PlayerGainsLife _ -> []
  TriggerCondition.PlayersGainLife _ -> []
  TriggerCondition.PlayerLosesLife _ -> []
  -- CR 714.2b carries a counter kind and a Natural, neither of which is a Count.
  TriggerCondition.SelfCountersReached {} -> []
  TriggerCondition.SelfBecomesClassLevel _ -> []
  -- CR 310.12b carries a counter kind alone.
  TriggerCondition.SelfLastCounterRemoved _ -> []
  -- And so does its any-amount mirror.
  TriggerCondition.SelfCountersRemoved _ -> []
  -- CR 603.2c's batch placement carries a kind and a Filter, and neither holds a
  -- Count. Its per-permanent scope carries the same payload.
  TriggerCondition.PermanentsGetCounters {} -> []
  TriggerCondition.PermanentGetsCounters {} -> []
  -- CR 601.2i's Filter is a predicate over the spell that was cast, and a Filter
  -- holds no Count, exactly as CR 603.6a's does above. Its ordinal is a bare
  -- Natural, as CR 714.2b's is.
  TriggerCondition.SpellCast {} -> []
  -- The same rule read off the spell itself carries nothing at all.
  TriggerCondition.SelfCast -> []
  -- CR 702.21a's condition carries a PlayerRelation and no Count.
  TriggerCondition.SelfBecomesTargeted _ -> []
  -- Its player-side sibling carries a PlayerRelation and a StackObjectKind, and
  -- no Count either.
  TriggerCondition.ControllerBecomesTarget {} -> []

-- Every Count reachable from one effect: the Quantities nested in its
-- ObjectRefs, its own Quantity/Duration fields, and -- for Create/CreateEmblem
-- -- every Count in the embedded token/emblem card (the same nesting
-- Pawl.Codec's round trip walks).
--
-- The refs come off Resolve.effectObjectRefs ahead of the case, so no arm below
-- names one and this traversal cannot fall behind the engine's two
-- static-analysis passes about which opcodes hold a ref.
effectCounts :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Count.Type.Count Quantity.Type.Quantity]
effectCounts effect = concatMap refCounts (Resolve.effectObjectRefs effect) <> ownCounts effect

-- effectCounts' half that is not an ObjectRef's: what this opcode's own fields
-- hold, and what its nested effects hold through the recursion back into
-- effectCounts.
ownCounts :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Count.Type.Count Quantity.Type.Quantity]
ownCounts effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> concatMap (quantityCounts . DamagePart.quantity) parts
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) -> durationCounts duration <> modificationCounts modification
  Effect.ChangeText {} -> []
  Effect.AddMana _ -> []
  -- The search's count is a Quantity like any other -- Explosive Vegetation's
  -- "up to two" -- so its Counts are reachable from here. A search stating no
  -- count (Mana Severance) has none to reach.
  Effect.Search (Search.MkSearch _ _ _ quantity _ _ _) -> foldMap quantityCounts quantity
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName _ -> []
  Effect.FromOutsideTheGame _ -> []
  Effect.ExileThisSpell -> []
  -- Bolster's N is a Quantity like the Search's above, so its Counts are
  -- reachable from here.
  Effect.Bolster quantity -> quantityCounts quantity
  -- Amass's N is a Quantity like the Search's above, so its Counts are reachable
  -- from here.
  Effect.Amass (Amass.MkAmass quantity _) -> quantityCounts quantity
  -- Blight's N is a Quantity like bolster's above, so its Counts are reachable
  -- from here.
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> quantityCounts quantity
  Effect.RestartGame _ -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw (Draw.MkDraw _ quantity _) -> quantityCounts quantity
  Effect.Mill (Mill.MkMill _ quantity _ _) -> quantityCounts quantity
  Effect.Reveal {} -> []
  Effect.LookAt {} -> []
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  -- No Quantity at all: rule 701.44a's counter is a literal one and its card is
  -- the one on top, so there is no number a card author writes.
  Effect.Explore {} -> []
  Effect.Discard subject -> case subject of
    Discard.Counted (CountedDiscard.MkCountedDiscard _ quantity _) -> quantityCounts quantity
    Discard.These {} -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantityCounts quantity
  -- The floor beside it is a printed literal and holds no Count.
  Effect.DecreaseSpeed d -> quantityCounts (SpeedDecrease.quantity d)
  Effect.Create (Create.MkCreate quantity card _ _ _) -> quantityCounts quantity <> overFaces cardCounts card
  Effect.Conjure (Conjure.MkConjure quantity card _) -> quantityCounts quantity <> overFaces cardCounts card
  -- No embedded card -- the copied permanent supplies the text -- but the count
  -- is card data like Create's. The riders are skipped for the reason Create's
  -- arm above skips its own: a rider count is a Quantity, and effectFilters below
  -- is where a Filter under one is swept.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ _) -> quantityCounts quantity
  -- Neither a Quantity nor a Duration, so no Count can hide here; the refs'
  -- Filters are effectFilters' business below.
  Effect.BecomeCopy {} -> []
  -- CR 707.10 copies one spell per named object and prints no count, so the
  -- BecomeCopy arm above answers for this too.
  Effect.CopyStackObject {} -> []
  -- The Condition is Galvanic Blast's and Synthetic Voltaic Surge's "if you
  -- control three or more artifacts", and its Counts are as much card data as a
  -- Duration's.
  Effect.Replace (Replace.MkReplace duration _ _ condition replacement) -> durationCounts duration <> foldMap conditionCounts condition <> concatMap effectCounts (replacementEffectRiders replacement) <> concatMap (overFaces cardCounts) (replacementMintedCards replacement)
  -- CR 614.10a's "next" is a use count, not a Duration and not a Quantity.
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- CR 615.5's rider is an effect list a card authors, so its Counts are this
  -- card's Counts -- the same recursion Create takes into a minted token.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ _ _ _ _ quantity rider) -> durationCounts duration <> quantityCounts quantity <> concatMap effectCounts rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ _ _ _ _ _ rider) -> durationCounts duration <> concatMap effectCounts rider
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ amount _ _ _ _ _) -> durationCounts duration <> foldMap quantityCounts amount
  -- CR 708.2's listed characteristics are card data, so the listed power and
  -- toughness are walked for the reason Create's minted face is. The listed type
  -- line holds no Quantity.
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown _ listed) ->
    concatMap (\(Power.MkPower quantity) -> quantityCounts quantity) (Maybe.maybeToList (FaceDownCharacteristics.power listed))
      <> concatMap (\(Toughness.MkToughness quantity) -> quantityCounts quantity) (Maybe.maybeToList (FaceDownCharacteristics.toughness listed))
  -- CR 708.8 has the permanent regain its own values, so this one lists nothing.
  Effect.TurnFaceUp _ -> []
  -- CR 701.14a fixes both amounts at the fighters' own powers, so no Quantity.
  Effect.Fight _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter {} -> []
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> quantityCounts quantity
  Effect.PutCountersFrom {} -> []
  -- The count the moved kinds may write. CR 122.5's GIVER carries the other one,
  -- through the ObjectRef it became when the first side was widened to a group,
  -- and refCounts reaches it from effectCounts above -- an arm reading the kinds
  -- alone kept compiling (#2729).
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> foldMap quantityCounts (MovedKinds.quantityOf kinds)
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> quantityCounts quantity
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantityCounts quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantityCounts quantity
  Effect.PayAnyEnergy _ -> []
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.Detain _ -> []
  Effect.Goad _ -> []
  Effect.MakePlotted _ -> []
  Effect.DoesNotUntapNext _ -> []
  Effect.Transform _ -> []
  Effect.Convert _ -> []
  -- CR 701.42a's combined back face, Create's token one opcode over: card data
  -- nested in card data, so its own counts are swept.
  Effect.Meld (Meld.MkMeld _ card) -> overFaces cardCounts card
  Effect.PhaseOut _ -> []
  Effect.AddPhases _ -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef duration _) -> durationCounts duration
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ _) -> durationCounts duration
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration _ _) -> durationCounts duration
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration _) -> durationCounts duration
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock duration _) -> durationCounts duration
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration _ _) -> durationCounts duration
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration _ _) -> durationCounts duration
  Effect.CreateEmblem card -> overFaces cardCounts card
  Effect.BecomeMonarch _ -> []
  Effect.TakeTheInitiative _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> []
  Effect.Unsuspect _ -> []
  Effect.SetHalfLocked {} -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.Train _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChoosePlayer _ -> []
  Effect.ChooseOpponentAtRandom _ -> []
  -- CR 706.2's modifier and CR 706.1's count are Quantities, so their Counts
  -- are reachable here.
  Effect.RollDie rollDie -> quantityCounts (RollDie.count rollDie) <> foldMap quantityCounts (RollDie.modifier rollDie)
  -- CR 705.1's number of coins is a Quantity, so its Counts are reachable here.
  Effect.FlipCoin flipCoin -> quantityCounts (FlipCoin.count flipCoin)
  -- CR 500.7's number of turns is a Quantity, so its Counts are reachable here.
  Effect.TakeExtraTurn takeExtraTurn -> quantityCounts (TakeExtraTurn.count takeExtraTurn)
  Effect.ShuffleIntoLibrary {} -> []
  Effect.Shuffle {} -> []
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
activatedAbilityCounts :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Count.Type.Count Quantity.Type.Quantity]
activatedAbilityCounts ability =
  -- CR 101.1's ceiling on the X this ability's own activation announces, the
  -- cardCounts treatment of Face.maximumX one type over (Blighted Nightmare).
  concatMap quantityCounts (ActivatedAbility.maximumX ability)
    <> foldMap conditionCounts (ActivatedAbility.condition ability)
    -- CR 602.5's "activate only if [board condition]" rider, whose Condition is
    -- a Count position like any other -- and the one ActivatedAbility field the
    -- two sweeps reached through nothing until OnlyIf gave it a payload.
    <> concatMap conditionCounts (concatMap restrictionConditions (ActivatedAbility.restrictions ability))
    <> concatMap effectCounts (Modal.allEffects (ActivatedAbility.modal ability))
    <> concatMap conditionCounts (modalClauseConditions (ActivatedAbility.modal ability))

triggeredAbilityCounts :: TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Count.Type.Count Quantity.Type.Quantity]
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

-- Every CR 118.12 cost this payload offers at resolution, over every mode and
-- every clause. A READER of X rather than a declarer: Clash of Wills' "unless its
-- controller pays {X}" spends the value its own {X}{U} announced (CR 107.3a),
-- which is what Pawl.Engine.Resolve.announcedXOn substitutes in.
payGateCostsOf :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Cost.Type.Cost Keyword.Keyword]
payGateCostsOf =
  fmap PayGate.cost
    . concatMap (Maybe.mapMaybe Clause.payGate . Foldable.toList . Mode.clauses)
    . Modal.modes

-- Every Count reachable from a combat restriction: only CR 508.1c's / CR
-- 509.1b's "unless some condition is met" carries one, and the subject beside it
-- is an Affected, which holds a Filter but no Count.
combatRestrictionCounts :: CombatRestriction.CombatRestriction -> [Count.Type.Count Quantity.Type.Quantity]
combatRestrictionCounts restriction = case restriction of
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless _ condition) -> foldMap conditionCounts condition
  CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless _ condition) -> foldMap conditionCounts condition
  -- The blocker Filter beside the gate holds no Count either.
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy _ _ condition) -> foldMap conditionCounts condition
  -- The PlayerScope and the CR 506.3 kinds beside the gate hold no Count either.
  CombatRestriction.CantAttackPlayer (CantAttackPlayer.MkCantAttackPlayer _ _ _ condition) -> foldMap conditionCounts condition
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

-- CR 508.1h's per-attacker share. Only the Counted arm holds anything: the Fixed
-- arm is a Pawl.Types.Cost, whose components carry Naturals rather than
-- Quantities (CR 601.2b's X is a constructor of its own) and Filters rather than
-- Counts.
perCreatureCounts :: PerCreature.PerCreature -> [Count.Type.Count Quantity.Type.Quantity]
perCreatureCounts perCreature = case perCreature of
  PerCreature.Fixed _ -> []
  PerCreature.Counted quantity -> quantityCounts quantity

-- Every effect a card AUTHORS: the ones its carriers hold, plus everything
-- nested inside one of those effects' own payloads, transitively. The nested
-- half is what makes this a CLOSURE rather than the flat concatenation the
-- carriers give -- CR 615.5's rider on a spell's prevention (Inkshield's Create)
-- is an effect the card wrote, and every lint downstream asks whether the CARD
-- authored an effect of some shape rather than which field it sat in.
--
-- Not the effects a token or emblem this card MINTS prints: those belong to
-- another object, and the sweeps that want them take `card : mintedFaces card`
-- and ask this question of each face separately.
cardResolutionEffects :: Face.Face Card.Type.Card -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
cardResolutionEffects = concatMap effectWithNested . cardCarrierEffects

-- One effect and everything nested inside it, transitively. Terminates on card
-- data of any shape: Pawl.Types.Effect nests structurally and a JSON document is
-- finite, so the depth is whatever the card printed.
effectWithNested :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
effectWithNested effect = effect : concatMap effectWithNested (effectNestedEffects effect)

-- The effects one effect carries in its own payload -- exactly the four arms
-- Pawl.Types.Effect declares parametrically in `Effect card`, since an arm with
-- no effect-typed field can hold none.
--
-- A Create's token and a CreateEmblem's emblem are NOT nested effects, though
-- each embeds a whole Card: those effects are the MINTED object's, not this
-- card's, and folding them in would attribute a token's slot binding to the card
-- that created it. mintedFaces is the traversal for that axis.
--
-- Exhaustive and hand-maintained, with effectReplacements' caveat: a NEW effect
-- carrying effects of its own must be added here too, and the build breaks until
-- it is.
effectNestedEffects :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
effectNestedEffects effect = case effect of
  -- CR 615.5's rider and CR 614.1c's as-enters instruction, on the replacement a
  -- resolution INSTALLS; the PRINTED twin arrives through cardCarrierEffects'
  -- last limb instead.
  Effect.Replace (Replace.MkReplace _ _ _ _ replacement) -> replacementPrintedEffects replacement
  -- CR 615.5's rider on the two prevention opcodes a SPELL authors: Test of
  -- Faith's counters, Inkshield's Inklings.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ _ _ riders) -> Foldable.toList riders
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ _ _ _ riders) -> Foldable.toList riders
  -- CR 608.2f's body, run once per member of the fold.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> Foldable.toList body
  Effect.Create {} -> []
  Effect.Conjure {} -> []
  Effect.CreateCopy {} -> []
  Effect.BecomeCopy {} -> []
  Effect.CopyStackObject {} -> []
  Effect.CreateEmblem {} -> []
  Effect.BecomeMonarch {} -> []
  Effect.TakeTheInitiative {} -> []
  Effect.Designate {} -> []
  Effect.SetClassLevel {} -> []
  Effect.DealDamage {} -> []
  Effect.ModifyTarget {} -> []
  Effect.AddMana {} -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName _ -> []
  Effect.FromOutsideTheGame _ -> []
  Effect.ExileThisSpell -> []
  Effect.Bolster {} -> []
  Effect.Amass {} -> []
  Effect.Blight {} -> []
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices {} -> []
  Effect.RestartGame {} -> []
  Effect.ControlPlayerNextTurn {} -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice {} -> []
  Effect.MoveToZone {} -> []
  Effect.Draw {} -> []
  Effect.Mill {} -> []
  Effect.Reveal {} -> []
  Effect.LookAt {} -> []
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore {} -> []
  Effect.Discard {} -> []
  Effect.LoseLife {} -> []
  Effect.GainLife {} -> []
  Effect.ExchangeLifeTotals {} -> []
  Effect.SetLifeTotal {} -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed {} -> []
  Effect.DecreaseSpeed {} -> []
  Effect.SkipNextPhase {} -> []
  Effect.RedirectDamage {} -> []
  Effect.TurnFaceDown {} -> []
  Effect.TurnFaceUp {} -> []
  Effect.Fight {} -> []
  Effect.RemoveFromCombat {} -> []
  Effect.BecomesBlocked {} -> []
  Effect.Counter {} -> []
  Effect.PutCounters {} -> []
  Effect.PutCountersFrom {} -> []
  Effect.MoveCounters {} -> []
  Effect.RemoveCounters {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.PayAnyEnergy _ -> []
  Effect.Tap {} -> []
  Effect.Untap {} -> []
  Effect.Detain {} -> []
  Effect.Goad {} -> []
  Effect.MakePlotted {} -> []
  Effect.DoesNotUntapNext {} -> []
  Effect.Transform {} -> []
  Effect.Convert {} -> []
  -- Create's answer: the combined face's effects belong to ANOTHER object, and
  -- effectMintedFaces is what reaches them.
  Effect.Meld {} -> []
  Effect.PhaseOut {} -> []
  Effect.AddPhases {} -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl {} -> []
  Effect.Unsuspect {} -> []
  Effect.SetHalfLocked {} -> []
  Effect.Evolve {} -> []
  Effect.Mentor {} -> []
  Effect.Train {} -> []
  Effect.ItBecomes {} -> []
  Effect.ExileUntilMonarch {} -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach {} -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.PlaySubgame {} -> []
  Effect.ChoosePlayer {} -> []
  Effect.ChooseOpponentAtRandom {} -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.RequireBlock {} -> []
  Effect.CantBeRegenerated {} -> []
  Effect.ForbidBlock {} -> []
  Effect.ForbidAttack {} -> []
  Effect.RequireAttack {} -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary {} -> []
  Effect.Shuffle {} -> []
  Effect.OfferCast {} -> []
  Effect.GrantPlayFromExile {} -> []
  Effect.ChangeText {} -> []

-- The carriers themselves, before the nesting closure above: one limb per field
-- of a Face that holds effects. Hand-maintained, with cardCounts' caveat: a NEW
-- Face field holding effects must be added here too.
cardCarrierEffects :: Face.Face Card.Type.Card -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
cardCarrierEffects card =
  printedCarrierEffects card
    -- CR 613.1f's quoted abilities, the seventh and eighth carriers: the text is
    -- printed on THIS card even though the ability ends up on another object, so
    -- every lint below has to read it here or nowhere.
    <> concatMap (Modal.allEffects . ActivatedAbility.modal) (grantedActivatedAbilities card)
    <> concatMap (Modal.allEffects . TriggeredAbility.modal) (grantedTriggeredAbilities card)

-- The limbs of cardCarrierEffects that do NOT go through a grant. Split out so
-- grantedModifications below can walk them without closing a loop with the two
-- limbs above, which are defined in terms of it.
printedCarrierEffects :: Face.Face Card.Type.Card -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
printedCarrierEffects card =
  Card.allEffects card
    <> concatMap (Modal.allEffects . ActivatedAbility.modal) (Face.activatedAbilities card)
    <> concatMap (Modal.allEffects . TriggeredAbility.modal) (Face.triggeredAbilities card)
    <> concatMap (Modal.allEffects . TriggeredAbility.modal) (Map.elems (Face.delayedAbilities card))
    -- CR 309.4c: a room ability's effects, which no other limb above reaches --
    -- Pawl.Types.Face.rooms is the fifth carrier.
    <> concatMap (Modal.allEffects . DungeonRoom.ability) (Face.rooms card)
    -- The effects a printed replacement ability carries, the sixth carrier: CR
    -- 615.5's additional effect beside a prevention (DamageR.riders) and CR
    -- 614.1c's as-enters instruction (EntryRewrite.RunEffects). Neither is a
    -- resolution's effect at all -- one runs from Resolve.runPreventionRider and
    -- the other from Resolve.runEntryEffects -- but both are card-authored effect
    -- lists, which is what every lint downstream of this function is about.
    <> concatMap (replacementPrintedEffects . PrintedReplacement.effect) (Face.replacementEffects card)

-- CR 103.5b and CR 103.6: the actions a face grants from a HAND, one effect list
-- per action. The two fields are exactly the two Pawl.Engine.Mulligan passes to
-- handWindow, and hand-maintained with cardResolutionEffects' caveat: a third
-- pregame window would have to be added here.
--
-- A list of ACTIONS rather than one flat list, because each action is performed
-- on its own (Pawl.Engine.Mulligan.handWindow performs one and asks again), so a
-- slot one action defines is not there for another to read.
--
-- Deliberately NOT a limb of cardCarrierEffects: these run from a HAND, before
-- the game begins, and each runs on its own -- so folding them into one flat
-- list would hand a dozen lints a slot-sharing claim that is false of them. The
-- sweep below and ownBoundSlots are the two readers, and both want the split.
handActions :: Face.Face Card.Type.Card -> [[Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]]
handActions card = fmap HandAction.effects (Face.mulliganActions card <> Face.openingHandActions card)

-- Every effect a card AUTHORS, its two pregame windows included: what
-- cardResolutionEffects reaches, plus the hand actions it deliberately leaves
-- out, each closed over its own nesting.
--
-- The view the CR 603.7 delayed-ability lints take, and Chancellor of the Forge
-- is why: it arms its delayed ability from a CR 103.6 opening-hand action, so the
-- narrower view saw a declared entry that nothing appeared to arm. Flattening the
-- actions is sound HERE and not for ownBoundSlots -- these lints ask which names
-- and slots a card MENTIONS, never which of them share a scope.
cardAuthoredEffects :: Face.Face Card.Type.Card -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
cardAuthoredEffects card =
  cardResolutionEffects card <> concatMap effectWithNested (concat (handActions card))

-- Both slots of a face's characteristic-defining P/T: one printed ability, which
-- Pawl.Codec.Face writes into each box's slot, or -- on CR 709.4c's combined view
-- of a split card -- one half's ability per box (Pawl.Engine.Card.definedBox).
-- The walks below want the quantities themselves.
characteristicQuantities :: Face.Face Card.Type.Card -> [Quantity.Type.Quantity]
characteristicQuantities card = case Face.characteristicPT card of
  Nothing -> []
  -- ONE quantity where the two slots agree, which every printed face's do: the
  -- lints below compare a count of what a card mentions against a count of the
  -- atoms in its encoded JSON, and the wire carries the ability once.
  Just cda
    | CharacteristicPT.power cda == CharacteristicPT.toughness cda -> [CharacteristicPT.power cda]
    | otherwise -> [CharacteristicPT.power cda, CharacteristicPT.toughness cda]

cardCounts :: Face.Face Card.Type.Card -> [Count.Type.Count Quantity.Type.Quantity]
cardCounts card =
  concatMap quantityCounts (characteristicQuantities card)
    -- CR 101.1's ceiling on CR 601.2b's X, which every printing states as a
    -- per-board amount (Soul Immolation's "the greatest toughness among
    -- creatures you control").
    <> concatMap quantityCounts (Face.maximumX card)
    <> concatMap (\(Power.MkPower quantity) -> quantityCounts quantity) (Maybe.maybeToList (Face.power card))
    <> concatMap (\(Toughness.MkToughness quantity) -> quantityCounts quantity) (Maybe.maybeToList (Face.toughness card))
    <> concatMap staticAbilityCounts (Face.staticAbilities card)
    -- CR 604.2's "as long as" clause on a printed replacement ability, the
    -- staticAbilityCounts treatment of the same clause one field over.
    <> concatMap (concatMap conditionCounts . Maybe.maybeToList . PrintedReplacement.condition) (Face.replacementEffects card)
    -- CR 614.1a's appended token on a printed row, Create's recursion.
    <> concatMap (overFaces cardCounts) (concatMap (replacementMintedCards . PrintedReplacement.effect) (Face.replacementEffects card))
    <> concatMap effectCounts (Card.allEffects card)
    <> concatMap conditionCounts (modalClauseConditions (Face.spell card))
    <> concatMap activatedAbilityCounts (Face.activatedAbilities card)
    <> concatMap triggeredAbilityCounts (Face.triggeredAbilities card)
    <> concatMap triggeredAbilityCounts (Map.elems (Face.delayedAbilities card))
    <> concatMap (concatMap effectCounts . Modal.allEffects . DungeonRoom.ability) (Face.rooms card)
    <> concatMap (concatMap conditionCounts . Maybe.maybeToList . AlternativeCost.condition) (Face.alternativeCosts card)
    -- CR 604.2's "as long as" clause on the PLAYER-facing static carrier, the
    -- staticAbilityCounts treatment of the same clause on the object-facing one.
    -- Its PlayerEffect beside it holds no Count -- a player effect states a
    -- literal amount or a Filter, never a fold over a zone.
    <> concatMap (concatMap conditionCounts . Maybe.maybeToList . PlayerStaticAbility.condition) (Face.playerAbilities card)
    <> concatMap (quantityCounts . CostReduction.perEach) (Face.costReductions card)
    <> concatMap combatRestrictionCounts (Face.combatRestrictions card)
    <> concatMap blockPermissionCounts (Face.blockPermissions card)
    -- CR 508.1d's "or that it attacks if some condition is met", the same CR
    -- 604.2 clause blockPermissionCounts reads one field over (Otarian
    -- Juggernaut counts its controller's graveyard). The requirement's SUBJECT
    -- is an Affected, which holds a Filter but no Count.
    <> concatMap (concatMap conditionCounts . Maybe.maybeToList . AttackRequirement.while) (Face.attackRequirements card)
    -- CR 508.1h's counted share (Sphere of Safety's "the number of enchantments
    -- you control"), the one Count a cost to attack can hold: its subject is an
    -- Affected, which holds a Filter but no Count.
    <> concatMap (perCreatureCounts . AttackCost.perAttacker) (Face.attackCosts card)
    -- CR 509.1d's counted share, the same Count in the blocking carrier. A FENCE
    -- rather than a proved line: Sphere of Safety fills the attacking one and
    -- pins the positive assertion below, and no cost to block in `data/cards/`
    -- writes a Counted share -- Oppressive Rays' is a literal {3}.
    <> concatMap (perCreatureCounts . BlockCost.perBlocker) (Face.blockCosts card)

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
modalSlotsOffend :: Set.Set SlotName.SlotName -> Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
modalSlotsOffend abilityBound modal =
  let modeOffends mode =
        let effects = Foldable.toList (Mode.allEffects mode)
            -- A slot DEFINED in this mode (a Create's minted token, or a
            -- PlaySubgame's bound subgame outcome) and then read by a later
            -- effect is legitimate dataflow, not an undeclared target.
            -- Plus what this mode's own CR 118.12 gate binds: the players its
            -- answer selected, which is how "each opponent ... unless THEY ..."
            -- says "they" (Binding.gatePlayers).
            -- Plus what this mode's own CR 603.5 "may" binds: the players who
            -- took it, which is how "each player may search THEIR library" says
            -- "they" (Binding.mayPlayers).
            defined = Set.unions [Resolve.definedSlots effects, Resolve.gateDefinedSlots mode, Resolve.mayDefinedSlots mode]
            -- The whole MODE's reads, not just its effect list's: CR 118.12a's
            -- "unless [a player] pays" names its payer by slot too.
            wanted = Map.keysSet (Resolve.modeSlots mode)
         in Set.difference (Set.difference wanted defined) abilityBound /= Map.keysSet (Mode.targetSlots mode)
   in any modeOffends (Modal.modes modal)

-- What performing a hand action binds, and the whole of it:
-- Pawl.Engine.Resolve.Effect.performHandAction hands applyEffect an environment holding
-- CR 113.7's `self` and nothing else.
--
-- NOT the whole reserved set, and `you` is the difference that matters: casting
-- stamps CR 109.5's controller (Pawl.Engine.Cast.castSpell) and performing a hand
-- action does not, so a hand action reading that name would read an empty binding
-- and silently no-op. Subtracting it here would hide exactly the defect this lint
-- is for.
handActionBound :: Set.Set SlotName.SlotName
handActionBound = Set.singleton Binding.triggerSource

-- modalSlotsOffend's claim, asked of a hand action. The declared side is EMPTY by
-- construction: CR 103.5b and CR 103.6 have the player PERFORM the action rather
-- than cast or activate anything, so there is no CR 601.2c announcement and
-- Pawl.Types.Face gives the two windows a bare effect list with no target slots to
-- declare. The equality therefore degenerates to "what an action reads, less what
-- that action defines, less what the performer binds, is nothing".
--
-- Per ACTION for modalSlotsOffend's per-mode reason: two actions never run as one,
-- so a slot the first defines is not there for the second.
handActionSlotsOffend :: Face.Face Card.Type.Card -> Bool
handActionSlotsOffend card =
  let offends effects =
        let wanted = foldMap (Map.keysSet . Resolve.slotsOf) effects
            defined = Resolve.definedSlots effects
         in not (Set.null (Set.difference (Set.difference wanted defined) handActionBound))
   in any offends (handActions card)

-- CR 601.2c with CR 601.2b: does this modal read the announced X through a
-- TARGET SLOT's count -- "each of X target creatures" (Rot-Curse Rakshasa) -- or
-- through a slot's CR 202.3 computed BOUND, "mana value X or less" (Stir the
-- Grave)? Two readers Resolve.readsX cannot see, that one walking effects and
-- both of these sitting on the slot, so the two reads-equal-declares lints below
-- ask all three.
modalReadsAnnouncedX :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
modalReadsAnnouncedX =
  any (\slot -> TargetSlot.count slot == SlotCount.AnnouncedX || any (Set.member Binding.variableX . QuantitySlot.slots) (TargetSlot.amount slot))
    . Modal.allTargetSlots

-- Every ReplacementEffect a card AUTHORS: the ones it PRINTS
-- (Face.replacementEffects, Eon Hub's) and the ones an effect of its own
-- installs (Effect.Replace, a floating one) -- and, through effectReplacements
-- below, everything a token or emblem those effects mint prints in turn. All of
-- them come out of card JSON, which is the whole of what the lint below is
-- about; a replacement the ENGINE bakes reaches GameState without passing
-- through a Card and is not swept here.
cardReplacementEffects :: Face.Face Card.Type.Card -> [ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))]
cardReplacementEffects card =
  fmap PrintedReplacement.effect (Face.replacementEffects card)
    <> concatMap effectReplacements (cardResolutionEffects card)
    -- CR 614.1a's appended token on a printed row prints rows of its own, the
    -- same descent effectReplacements takes into a Create's token.
    <> concatMap (overFaces cardReplacementEffects) (concatMap (replacementMintedCards . PrintedReplacement.effect) (Face.replacementEffects card))

-- Every effect a replacement PRINTS, on the two axes that carry one: CR 615.5's
-- additional effect beside a prevention, and CR 614.1c's "as [this permanent]
-- enters, [do something]". Swept as one list wherever a card's effects are, since
-- what the lints downstream ask is whether a card authored the effect rather than
-- which field it sat in.
replacementPrintedEffects :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
replacementPrintedEffects replacement = replacementEffectRiders replacement <> replacementEntryEffects replacement

-- CR 614.1c: the effects an as-enters rewrite runs -- Monstrous War-Leech's mill.
-- Kept apart from the riders below rather than folded in, because CR 615.5's
-- rider is a lint's subject in its own right (riderWithoutPreventionOffends) and
-- these are not one.
replacementEntryEffects :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
replacementEntryEffects replacement = case replacement of
  ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.RunEffects effects)) -> Foldable.toList effects
  ReplacementEffect.EntryR {} -> []
  ReplacementEffect.DamageR {} -> []
  ReplacementEffect.CounterR {} -> []
  ReplacementEffect.ZoneChangeR {} -> []
  ReplacementEffect.DestructionR _ -> []
  ReplacementEffect.TokenR {} -> []
  ReplacementEffect.TurnUpR {} -> []
  ReplacementEffect.UntapR _ -> []
  ReplacementEffect.LifeLossR {} -> []
  ReplacementEffect.LifeGainR {} -> []
  ReplacementEffect.DrawR {} -> []
  ReplacementEffect.DrawCountR {} -> []
  ReplacementEffect.PhaseR _ -> []

-- CR 615.5: the additional effect a replacement PRINTS -- DamageR's riders, and
-- nothing else, since no other arm has a field to carry one. The card-authored
-- twin of the two prevention opcodes' `riders`.
--
-- Both halves reach cardResolutionEffects: this one as a carrier of its own, and
-- the rider a SPELL authors on Effect.PreventAllDamage or
-- Effect.PreventNextDamage through effectNestedEffects, which is what lets the CR
-- 111.4 naming case below see Inkshield's nested token face.
replacementEffectRiders :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
replacementEffectRiders replacement = case replacement of
  ReplacementEffect.DamageR (DamageR.MkDamageR _ _ riders) -> Foldable.toList riders
  ReplacementEffect.CounterR {} -> []
  ReplacementEffect.ZoneChangeR {} -> []
  ReplacementEffect.EntryR {} -> []
  ReplacementEffect.DestructionR _ -> []
  ReplacementEffect.TokenR {} -> []
  ReplacementEffect.TurnUpR {} -> []
  ReplacementEffect.UntapR _ -> []
  ReplacementEffect.LifeLossR {} -> []
  ReplacementEffect.LifeGainR {} -> []
  ReplacementEffect.DrawR {} -> []
  ReplacementEffect.DrawCountR {} -> []
  ReplacementEffect.PhaseR _ -> []

-- CR 111.1's token a replacement MINTS: TokenR's appended token (Queen Allenal
-- of Ruadach's Soldier), the one arm embedding a whole Card. The axis
-- effectMintedFaces walks for a Create, one carrier over.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: a second
-- card-bearing arm must be classified here.
replacementMintedCards :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [Card.Type.Card]
replacementMintedCards replacement = case replacement of
  ReplacementEffect.TokenR (TokenR.MkTokenR _ _ plus) -> Maybe.maybeToList plus
  ReplacementEffect.DamageR {} -> []
  ReplacementEffect.CounterR {} -> []
  ReplacementEffect.ZoneChangeR {} -> []
  ReplacementEffect.EntryR {} -> []
  ReplacementEffect.DestructionR _ -> []
  ReplacementEffect.TurnUpR {} -> []
  ReplacementEffect.UntapR _ -> []
  ReplacementEffect.LifeLossR {} -> []
  ReplacementEffect.LifeGainR {} -> []
  ReplacementEffect.DrawR {} -> []
  ReplacementEffect.DrawCountR {} -> []
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
effectReplacements :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))]
effectReplacements effect = case effect of
  Effect.Replace (Replace.MkReplace _ _ _ _ replacement) -> replacement : concatMap effectReplacements (replacementPrintedEffects replacement) <> concatMap (overFaces cardReplacementEffects) (replacementMintedCards replacement)
  Effect.Create (Create.MkCreate _ token _ _ _) -> overFaces cardReplacementEffects token
  Effect.Conjure (Conjure.MkConjure _ card _) -> overFaces cardReplacementEffects card
  Effect.CreateCopy {} -> []
  Effect.BecomeCopy {} -> []
  Effect.CopyStackObject {} -> []
  Effect.CreateEmblem emblem -> overFaces cardReplacementEffects emblem
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> []
  Effect.ModifyTarget {} -> []
  Effect.AddMana _ -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName _ -> []
  Effect.FromOutsideTheGame _ -> []
  Effect.ExileThisSpell -> []
  Effect.Bolster _ -> []
  Effect.Amass _ -> []
  Effect.Blight _ -> []
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices {} -> []
  Effect.RestartGame _ -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw {} -> []
  Effect.Mill {} -> []
  Effect.Reveal {} -> []
  Effect.LookAt {} -> []
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore {} -> []
  Effect.Discard {} -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.DecreaseSpeed _ -> []
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- CR 615.5's rider can carry an Effect.Replace, so this descends.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ _ _ rider) -> concatMap effectReplacements rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ _ _ _ rider) -> concatMap effectReplacements rider
  -- CR 608.2f's body can too, for the same reason.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> concatMap effectReplacements body
  Effect.RedirectDamage {} -> []
  -- CR 708.2's listed characteristics hold no replacement effect (gap #1667).
  Effect.TurnFaceDown _ -> []
  Effect.TurnFaceUp _ -> []
  Effect.Fight _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter {} -> []
  Effect.PutCounters {} -> []
  Effect.PutCountersFrom {} -> []
  Effect.MoveCounters {} -> []
  Effect.RemoveCounters {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.PayAnyEnergy _ -> []
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.Detain _ -> []
  Effect.Goad _ -> []
  Effect.MakePlotted _ -> []
  Effect.DoesNotUntapNext _ -> []
  Effect.Transform _ -> []
  Effect.Convert _ -> []
  -- CR 614: the combined back face may print its own entry replacement, so the
  -- sweep descends into it as it does a token's.
  Effect.Meld (Meld.MkMeld _ card) -> overFaces cardReplacementEffects card
  Effect.PhaseOut _ -> []
  Effect.AddPhases _ -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> []
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.RequireBlock {} -> []
  Effect.CantBeRegenerated {} -> []
  Effect.ForbidBlock {} -> []
  Effect.ForbidAttack {} -> []
  Effect.RequireAttack {} -> []
  Effect.BecomeMonarch _ -> []
  Effect.TakeTheInitiative _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> []
  Effect.Unsuspect _ -> []
  Effect.SetHalfLocked {} -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.Train _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChoosePlayer _ -> []
  Effect.ChooseOpponentAtRandom _ -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary {} -> []
  Effect.Shuffle {} -> []
  Effect.OfferCast {} -> []
  Effect.GrantPlayFromExile {} -> []
  Effect.ChangeText {} -> []

-- Every modal one face carries, in the four scopes cardSlotNamesCollide sweeps
-- plus its rooms -- the list that lint spells out inline, hoisted because the
-- either-or lint below needs the same one and a second copy would drift.
faceModals :: Face.Face Card.Type.Card -> [Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
faceModals card =
  Face.spell card
    : fmap ActivatedAbility.modal (Face.activatedAbilities card)
      <> fmap TriggeredAbility.modal (Face.triggeredAbilities card)
      <> fmap TriggeredAbility.modal (Map.elems (Face.delayedAbilities card))
      <> fmap DungeonRoom.ability (Foldable.toList (Face.rooms card))

-- CR 608.2d: does any clause's either-or name a sibling that does not name it
-- back? Clause.orElse is SYMMETRIC by design -- the announcement is made at
-- whichever branch the resolution reaches first, so that one has to know the
-- pair exists -- and nothing in Pawl.Engine.Resolve enforces it.
--
-- What an asymmetric pair does silently, which is why this is a lint and not an
-- elision: a clause naming nobody is never excluded, so it runs whatever the
-- controller announced, and the pair becomes "always the first, and maybe the
-- second too". A clause naming an ordinal no clause has fares worse -- both
-- branches then lose, and the mode does nothing at all.
--
-- Per MODE (CR 700.2d), like every other clause-ordinal reader: Clause.ifTaken
-- is read against the clauses of one mode instance, and an ordinal means nothing
-- across modes.
cardBranchesAreAsymmetric :: Face.Face Card.Type.Card -> Bool
cardBranchesAreAsymmetric = any (any modeBranchesOffend . Modal.modes) . faceModals

-- One mode's half of that lint: a clause naming ITSELF offends (there is no pair
-- to choose between), and so does one whose named sibling is missing or names
-- somebody else. So does a pair whose two halves name different CHOOSERS, the
-- announcement being made once at whichever branch the resolution reaches first
-- (Pawl.Engine.Resolve.chosenBranch) -- the loser's own chooser would be data
-- nothing reads.
modeBranchesOffend :: Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
modeBranchesOffend mode =
  let indexed = zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (Foldable.toList (Mode.clauses mode))
      byIndex = Map.fromList indexed
      names cIdx chooser = Just (Just (OrElse.MkOrElse cIdx chooser))
      offends (cIdx, clause) = case Clause.orElse clause of
        Nothing -> False
        Just orElse ->
          OrElse.sibling orElse == cIdx
            || fmap Clause.orElse (Map.lookup (OrElse.sibling orElse) byIndex) /= names cIdx (OrElse.chooser orElse)
   in any offends indexed

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

-- A one-mode, targetless triggered ability running one effect under one
-- condition -- the fixture the lint's own self-test misauthors on purpose. Kept
-- here rather than in data/cards, because a committed card that offends the lint
-- would fail the corpus sweep: the offender has to live where only the self-test
-- sees it.
oneEffectTrigger ::
  TriggerCondition.TriggerCondition ->
  Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
oneEffectTrigger condition effect =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
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
  Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
oneEffectActivated mana effect =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = mana, Cost.Type.components = []},
      ActivatedAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      ActivatedAbility.maximumX = [],
      ActivatedAbility.restrictions = [],
      ActivatedAbility.activator = Activator.Controller,
      ActivatedAbility.condition = Nothing,
      ActivatedAbility.name = Nothing
    }

-- One CR 700.2 mode for the fixtures below: the effects it runs and the target
-- slots it declares. Always mandatory -- no read lint asks about optionality.
lintMode :: [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> [SlotName.SlotName] -> Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
lintMode effects slots =
  Mode.MkMode
    (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects)))
    (Map.fromList (fmap (\slot -> (slot, TargetSlot.required Pool.AnyTarget Nothing)) slots))

-- oneEffectActivated widened to SEVERAL modes, free, under CR 700.2's
-- ChooseExactly 1. The fixture the per-mode read lint needs and the one-mode
-- helpers cannot express: only a multi-mode ability can have a mode read a slot
-- that only another mode declares (#570). Kept out of data/cards for the same
-- reason they are -- a card that offends a lint must not be loadable.
modalActivated :: [Mode.Mode Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
modalActivated modes =
  ActivatedAbility.MkActivatedAbility
    { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
      ActivatedAbility.modal = Modal.MkModal (Seq.fromList modes) (ModeSelection.ChooseExactly 1),
      ActivatedAbility.maximumX = [],
      ActivatedAbility.restrictions = [],
      ActivatedAbility.activator = Activator.Controller,
      ActivatedAbility.condition = Nothing,
      ActivatedAbility.name = Nothing
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
      Binding.gatePlayers,
      Binding.mayPlayers,
      Binding.became,
      Binding.departedPermanent,
      Binding.eventAmount,
      Binding.sacrificedCount,
      Binding.sacrificedPermanent,
      Binding.tappedPermanent,
      Binding.castSpell,
      Binding.thisAbility,
      Binding.targetingObject,
      Binding.blockingCreature,
      Binding.blockedCreature,
      Binding.attackingCreature,
      Binding.attackingPlayer,
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
    [ maybe Set.empty (QuantitySlot.slots . Power.unwrap) (Face.power card),
      maybe Set.empty (QuantitySlot.slots . Toughness.unwrap) (Face.toughness card),
      Set.unions (fmap QuantitySlot.slots (characteristicQuantities card))
    ]

-- Every face a card MINTS, transitively: the faces of every token (CR 111.1)
-- and emblem (CR 114.1) its own effects create, plus everything those mint in
-- turn. The same recursion effectReplacements takes, and for the same reason --
-- CR 111.3 makes a token's defined characteristics "functionally equivalent to
-- the characteristic values that are printed on a card", and CR 114.4 makes an
-- emblem's abilities function in the command zone, so an ability arriving as an
-- effect's payload is as real as a printed one.
--
-- A separate axis from cardResolutionEffects' nesting closure, and deliberately
-- so: a minted face's effects belong to ANOTHER object, so the sweeps that want
-- them ask their question of each face in turn rather than reading one list that
-- pretends a token's text is the creating card's.
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
  let minted =
        concatMap effectMintedFaces (cardResolutionEffects card)
          -- CR 614.1a's appended token on a printed row (Queen Allenal of
          -- Ruadach), which no resolution effect carries.
          <> concatMap (replacementMintedFaces . PrintedReplacement.effect) (Face.replacementEffects card)
   in minted <> concatMap (mintedFacesTagged . snd) minted

-- The faces a replacement mints, tagged as effectMintedFaces tags a Create's:
-- CR 111.1's token, so MintedToken.
replacementMintedFaces :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [(MintedKind, Face.Face Card.Type.Card)]
replacementMintedFaces replacement = fmap ((,) MintedToken) (concatMap (NonEmpty.toList . Card.Type.faces) (replacementMintedCards replacement))

-- CR 111.1, CR 114.1 and CR 701.42a: the kinds of object a card's own effects
-- mint a face for.
data MintedKind
  = MintedToken
  | MintedEmblem
  | -- | CR 701.42a's combined back face, which the melding ability carries
    -- inline. A face like a token's for this lint's purposes: CR 712.8g gives
    -- the melded permanent that face's characteristics, card types included.
    MintedMeld
  | -- | Alchemy's conjured card, which Pawl.Types.Conjure carries inline. A real
    -- card (it is not CR 111.1's token), so it has card types like any other.
    MintedCard
  deriving (Eq, Ord, Show)

-- The faces one effect mints. Exhaustive and hand-maintained, with
-- effectReplacements' caveat: a NEW effect embedding a Card must be added here
-- too, and the build breaks until it is.
effectMintedFaces :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [(MintedKind, Face.Face Card.Type.Card)]
effectMintedFaces effect = case effect of
  Effect.Create (Create.MkCreate _ token _ _ _) -> fmap ((,) MintedToken) (NonEmpty.toList (Card.Type.faces token))
  Effect.Conjure (Conjure.MkConjure _ card _) -> fmap ((,) MintedCard) (NonEmpty.toList (Card.Type.faces card))
  -- Mints no face of its own: the token's text is the copied permanent's.
  Effect.CreateCopy {} -> []
  -- Mints nothing at all: it rewrites an existing permanent's copiable values.
  Effect.BecomeCopy {} -> []
  -- Mints no face either: the copy's text is the copied spell's.
  Effect.CopyStackObject {} -> []
  Effect.CreateEmblem emblem -> fmap ((,) MintedEmblem) (NonEmpty.toList (Card.Type.faces emblem))
  Effect.Replace (Replace.MkReplace _ _ _ _ replacement) -> concatMap effectMintedFaces (replacementEffectRiders replacement) <> replacementMintedFaces replacement
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> []
  Effect.ModifyTarget {} -> []
  Effect.AddMana _ -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName _ -> []
  Effect.FromOutsideTheGame _ -> []
  Effect.ExileThisSpell -> []
  Effect.Bolster _ -> []
  -- The Army token is Pawl.Engine.Amass.armyToken's, minted from the rulebook
  -- rather than embedded in card data, so this arm mints no face of the card's own.
  Effect.Amass _ -> []
  Effect.Blight _ -> []
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices {} -> []
  Effect.RestartGame _ -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  Effect.MoveToZone {} -> []
  Effect.Draw {} -> []
  Effect.Mill {} -> []
  Effect.Reveal {} -> []
  Effect.LookAt {} -> []
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore {} -> []
  Effect.Discard {} -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ _) -> []
  Effect.DecreaseSpeed _ -> []
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- CR 615.5's rider can mint a token or emblem of its own, so this descends.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ _ _ rider) -> concatMap effectMintedFaces rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ _ _ _ rider) -> concatMap effectMintedFaces rider
  -- CR 608.2f's body can too, for the same reason.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> concatMap effectMintedFaces body
  Effect.RedirectDamage {} -> []
  -- CR 708.2's listed characteristics are not a minted FACE: they replace an
  -- existing object's, and Pawl.Engine.Card.faceDownFace supplies every field
  -- they do not name.
  Effect.TurnFaceDown _ -> []
  Effect.TurnFaceUp _ -> []
  Effect.Fight _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter {} -> []
  Effect.PutCounters {} -> []
  Effect.PutCountersFrom {} -> []
  Effect.MoveCounters {} -> []
  Effect.RemoveCounters {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.PayAnyEnergy _ -> []
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.Detain _ -> []
  Effect.Goad _ -> []
  Effect.MakePlotted _ -> []
  Effect.DoesNotUntapNext _ -> []
  Effect.Transform _ -> []
  Effect.Convert _ -> []
  -- CR 701.42a: the combined back face is a face this card mints, interned at
  -- resolution exactly as a token's card is.
  Effect.Meld (Meld.MkMeld _ card) -> fmap ((,) MintedMeld) (NonEmpty.toList (Card.Type.faces card))
  Effect.PhaseOut _ -> []
  Effect.AddPhases _ -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> []
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.RequireBlock {} -> []
  Effect.CantBeRegenerated {} -> []
  Effect.ForbidBlock {} -> []
  Effect.ForbidAttack {} -> []
  Effect.RequireAttack {} -> []
  Effect.BecomeMonarch _ -> []
  Effect.TakeTheInitiative _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> []
  Effect.Unsuspect _ -> []
  Effect.SetHalfLocked {} -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.Train _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChoosePlayer _ -> []
  Effect.ChooseOpponentAtRandom _ -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary {} -> []
  Effect.Shuffle {} -> []
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
-- to read back -- Resolve.definedSlots over every effect that face authors
-- (cardResolutionEffects), which is every carrier cardCarrierEffects lists and
-- everything nested inside one of their effects.
--
-- ownDeclaredTargetSlots' sibling and the other half of the same question.
-- Declaring a target slot is not the only way a card names a slot: MoveToZone and Create
-- name the incarnation CR 400.7 mints, PlaySubgame names CR 729.1b's winner, and
-- Destroy names how many it destroyed.
--
-- The base case of boundSlots below, named so the self-test can hold it against
-- the widened view.
--
-- The two pregame windows are here too, though cardResolutionEffects does not
-- reach them: a hand action's effects run through the same
-- Pawl.Engine.Resolve.Effect.applyEffect the resolution carriers do, so a MoveToZone in
-- one that names CR 113.7's `self` as the incarnation it minted would overwrite
-- the very binding Pawl.Engine.Resolve.Effect.performHandAction stamped, mid-action.
ownBoundSlots :: Face.Face Card.Type.Card -> Set.Set SlotName.SlotName
ownBoundSlots card = Resolve.definedSlots (cardResolutionEffects card <> concat (handActions card))

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
-- this pool creates whose instruction does not name it, which is all but the two
-- exemptions below.
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
-- EXEMPT: CR 111.9's legendary tokens, "create [name], a . . ." -- Tomb of
-- Annihilation's "create The Atropal, a legendary 4/4 black God Horror creature
-- token with deathtouch". That wording is rule 111.4's "the spell or ability
-- specifies the name", so the rule supplies nothing and the name is whatever the
-- card says. Keyed on the Legendary supertype, which is the only mark the card
-- data carries of having been written in rule 111.9's form.
--
-- EXEMPT TOO: a NONLEGENDARY token the instruction names -- Flock of Rabid
-- Sheep's "create a 2\/2 green Sheep creature token named Rabid Sheep". The same
-- half of rule 111.4 as the legendary case, but nothing in the card data marks
-- it, so this is a hand-kept list rather than a predicate. CR 111.10's predefined
-- tokens (111.10d's Walker, 111.10j-r's Roles) and the copy tokens of CR 111.4's
-- own Spitting Image example (named Doomed Dissenter, "not Human Token or Doomed
-- Dissenter Token") will each want a line here too.
namedTokens :: Set.Set CardName.CardName
namedTokens = Set.singleton (CardName.MkCardName (Text.pack "Rabid Sheep"))

tokenNameOffends :: Face.Face Card.Type.Card -> Bool
tokenNameOffends token
  | Set.member Supertype.Legendary (TypeLine.supertypes (Face.typeLine token)) = False
  | Set.member (Face.name token) namedTokens = False
  | otherwise =
      case traverse (fmap (Text.pack . fst) . Common.asTagged . Codec.encode Subtype.codec) (Set.toList (TypeLine.subtypes (Face.typeLine token))) of
        Left _ -> True
        Right subtypes ->
          notElem
            (CardName.unwrap $ Face.name token)
            (fmap (\ordering -> Text.unwords (ordering <> [Text.pack "Token"])) (List.permutations subtypes))

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
-- Both Filter positions an entry rider has: the counter KINDS it is keyed by (CR
-- 122.1b's keyword counter carries a whole Keyword) and the COUNTS it holds (CR
-- 122.6, each a Quantity, which may carry a Count whose Filter is card text).
-- One function so the three effect arms that carry a rider cannot sweep
-- different halves of it.
riderFilters :: EntryRiders.EntryRiders Quantity.Type.Quantity -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
riderFilters riders =
  concatMap counterKindFilters (Map.keys (EntryRiders.counters riders))
    <> concatMap quantityFilters (Map.elems (EntryRiders.counters riders))

-- Both Filter positions a CR 614.1c counter row has: the KINDS it is keyed by
-- (CR 122.1b's keyword counter carries a whole Keyword) and the COUNTS it holds
-- (each a Quantity, which may carry a Count whose Filter is card text).
-- riderFilters' two, over the payload entryRewriteFilters and
-- turnUpRewriteFilters share -- one function so the two rewrites that carry it
-- cannot sweep different halves.
withCountersFilters :: WithCounters.WithCounters -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
withCountersFilters w =
  concatMap counterKindFilters (Map.keys (WithCounters.counters w))
    <> concatMap quantityFilters (Map.elems (WithCounters.counters w))

-- Both Filter positions a CR 118.12 gate has: the COST the payer is offered,
-- whose components carry card text (Lithophage's "unless you sacrifice a
-- Mountain"), and CR 702.24a's counter KIND, which CR 122.1b lets carry a whole
-- Keyword. One function for the same reason riderFilters is one -- the two
-- halves of a gate must not be swept apart.
--
-- The COST half is SlotlessCostFramed: Pawl.Engine.Resolve.payGatePaidBy pays it
-- through Pawl.Engine.Cost, whose Filter.Context comes from
-- Pawl.Engine.Filter.contextFor and carries none of the resolution's slots, so
-- Filter.IsBound there is a silent False. Unframed promised the opposite; see
-- #2881.
--
-- The KIND half carries whatever counterKindFilters hands out, which is
-- KeywordFramed for CR 122.1b's keyword counter and nothing at all for every
-- other kind -- so it needs no tag of its own.
payGateFilters :: PayGate.PayGate -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
payGateFilters gate =
  slotlessCost (costFilters (PayGate.cost gate))
    <> concatMap counterKindFilters (Maybe.maybeToList (PayGate.perCounter gate))

-- CR 122.1b: the one counter kind with a Filter under it, since it carries a
-- whole Keyword. Exhaustive so a new kind with a payload breaks this build.
--
-- EVERY card-authored CounterKind position goes through this, and the list is
-- greppable rather than asserted: `counterKindFilters` above the effect
-- traversals, plus riderFilters, withCountersFilters and payGateFilters, is the
-- whole of the positions that name a KIND; see #2728, and #2876 for the gate
-- position, which was written and unswept. The positions that name a NUMBER reach
-- it through quantityKindFilters instead, and every traversal that reaches a
-- Quantity goes through quantityFilters to get there; see #2740.
--
-- Tagged pairs and not bare Filters, so that CR 122.1b's keyword counter carries
-- KeywordFramed out through every quoting position rather than inheriting the
-- promise of whichever one quoted it -- `frame` below fills in only the
-- positions still Unframed.
counterKindFilters :: CounterKind.CounterKind Keyword.Keyword -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
counterKindFilters kind = case kind of
  CounterKind.Keyword keyword -> keywordFilters keyword
  CounterKind.PlusOnePlusOne -> []
  CounterKind.MinusOneMinusOne -> []
  CounterKind.Loyalty -> []
  CounterKind.Lore -> []
  CounterKind.Defense -> []
  CounterKind.Time -> []
  CounterKind.Fade -> []
  CounterKind.Age -> []
  CounterKind.Shield -> []
  CounterKind.Finality -> []
  CounterKind.Stun -> []
  CounterKind.Level -> []
  CounterKind.Hone -> []
  CounterKind.Named _ -> []

-- A keyword's own Filters, tagged. Almost all of them are KeywordFramed, whose
-- argument is that no evaluator of a keyword payload supplies a slot-carrying
-- Context; CR 702.6c's equip quality is the exception, because rule 702.6a's
-- minted ability carries it into a TARGET SLOT -- see MintedTargetSlot.
--
-- The exception is drawn HERE rather than inside keywordPayloadFilters below, so
-- that walk stays a bare list and stays the exhaustive one: a Keyword
-- constructor added without an arm there fails to compile, where this selector's
-- fallthrough decides only a FRAMING and answers KeywordFramed for it. A new
-- rule-702 keyword that transplanted its payload into a minted slot the way
-- equip does would owe an arm here, and nothing but this comment says so.
keywordFilters :: Keyword.Keyword -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
keywordFilters keyword = case keyword of
  Keyword.Equip (Equip.MkEquip cost mQuality) -> keywordFramed (costFilters cost) <> mintedTargetSlot (Maybe.maybeToList mQuality)
  _ -> keywordFramed (keywordPayloadFilters keyword)

keywordPayloadFilters :: Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
keywordPayloadFilters keyword = case keyword of
  Keyword.Cycling (Cycling.MkCycling cost mFilter) -> costFilters cost <> Maybe.maybeToList mFilter
  Keyword.Flashback cost -> costFilters cost
  -- CR 702.103a: the bestow cost, whose components may hold a Filter exactly as
  -- flashback's may.
  Keyword.Bestow cost -> costFilters cost
  -- CR 702.162a: the more than meets the eye cost, whose components may hold a
  -- Filter exactly as flashback's and bestow's may.
  Keyword.MoreThanMeetsTheEye cost -> costFilters cost
  Keyword.Kicker cost -> costFilters cost
  Keyword.Multikicker cost -> costFilters cost
  Keyword.Entwine cost -> costFilters cost
  -- CR 702.170a: the plot cost, whose components may hold a Filter exactly as
  -- flashback's and entwine's may.
  Keyword.Plot cost -> costFilters cost
  -- CR 702.168a: the disguise cost, reached the same way as morph's below.
  Keyword.Disguise cost -> costFilters cost
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
  -- CR 702.6a's payload carries a COST, level up's and outlast's shape below, so
  -- its Filters are its components'. The "target creature you control" filter
  -- its minted ability carries is the ENGINE's, never a card's.
  --
  -- The COST HALF ONLY. CR 702.6c's quality is a card's Filter too, but it is
  -- not KeywordFramed and so is handed out by keywordFilters above rather than
  -- here; repeating it in this arm would report it twice.
  Keyword.Equip (Equip.MkEquip cost _) -> costFilters cost
  -- CR 702.67a's payload is equip's, and so is this: the "target land you
  -- control" filter its minted ability carries is the ENGINE's, never a card's.
  Keyword.Fortify cost -> costFilters cost
  -- CR 702.24a carries a whole Cost, so a Filter inside it is the card's.
  Keyword.CumulativeUpkeep cost -> costFilters cost
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
  Keyword.LivingMetal -> []
  -- CR 702.16a's "[quality]", which every protection ability states.
  Keyword.Protection quality -> [quality]
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
  Keyword.LevelUp cost -> costFilters cost
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
  Keyword.Compleated -> []
  Keyword.ReadAhead -> []
  Keyword.Training -> []
  -- CR 702.100a is payload-free: the Filter its minted ability carries -- the
  -- entering creature's -- is the ENGINE's, never a card's.
  Keyword.Evolve -> []
  -- CR 702.105a is payload-free too, and names no quality at all: what its minted
  -- ability compares is life totals, which no Filter reaches.
  Keyword.Dethrone -> []
  -- CR 702.102a is payload-free: the permission names no quality, and the halves
  -- it fuses are the CARD's own faces rather than anything this value carries.
  Keyword.Fuse -> []
  Keyword.StartYourEngines -> []
  -- CR 701.43d is payload-free: the linked trigger it permits is the CARD's own
  -- TriggeredAbility, so any Filter in it is swept there rather than here.
  Keyword.Exert -> []
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
  -- CR 702.184a is payload-free: the "another untapped creature you control" the
  -- cost taps is written into the ability Pawl.Engine.Keyword.station mints, not
  -- into the keyword.
  Keyword.Station -> []

-- CR 118.1: a cost's Filters are its components'; the mana part holds none.
costFilters :: Cost.Type.Cost Keyword.Keyword -> [Filter.Type.Filter Keyword.Keyword]
costFilters = concatMap costComponentFilters . Cost.Type.components

-- CR 118.9: an alternative cost reaches a Filter through its components, as any
-- Cost does, and through the Condition CR 604.2 may gate it with.
--
-- The COST half is Unframed for Face.additionalCosts' reason: CR 118.9's
-- alternative is paid at CR 601.2h, after CR 601.2c's targets exist, and
-- Pawl.Engine.Cost.pay reads them (Cost.announcedSlots). It was
-- SlotlessCostFramed between #2883 and #2924. The CONDITION half is a separate
-- question and keeps whatever the quoting position frames it as.
alternativeCostFilters :: AlternativeCost.AlternativeCost -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
alternativeCostFilters alternative =
  unframed (costFilters (AlternativeCost.cost alternative))
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
  -- CR 601.2f's "tapping permanents": Springleaf Drum's "an untapped creature
  -- you control".
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents _ f) -> [f]
  -- CR 118.1 as a cost: Meloku the Clouded Mirror's "a land you control".
  CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents _ f) -> [f]
  -- CR 406.2 as a cost: Headless Skaab's "a creature card from your graveyard".
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard _ f) -> [f]
  -- CR 406.2 again: Circling Vultures' "the top creature card of your
  -- graveyard".
  CostComponent.ExileTopFromGraveyard f -> [f]
  -- CR 601.2f's discard as a cost: Magmatic Insight's "a land card".
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards _ f) -> [f]
  CostComponent.PutCardFromHandOntoBattlefield f -> [f]
  CostComponent.TapThis -> []
  CostComponent.UntapThis -> []
  CostComponent.SacrificeThis -> []
  CostComponent.ReturnThis -> []
  CostComponent.PayLife _ -> []
  CostComponent.PayLifeX -> []
  CostComponent.PayEnergyX -> []
  CostComponent.DiscardThis _ -> []
  CostComponent.PayEnergy _ -> []
  CostComponent.AddLoyaltyToThis _ -> []
  CostComponent.RemoveLoyaltyFromThis _ -> []
  CostComponent.RemovePlusOneCountersFromThis _ -> []
  CostComponent.PutPlusOneCountersOnThis _ -> []
  CostComponent.Blight _ -> []
  CostComponent.BlightX -> []
  CostComponent.ExileThisFromGraveyard -> []
  CostComponent.ExileThis -> []
  -- CR 701.17a takes the cards off the top, so this component carries no Filter
  -- to narrow -- ExileThisFromGraveyard's answer above and for its reason.
  CostComponent.MillCards _ -> []

-- The Filter narrowing a target slot's CR 115 pool -- "target creature with
-- flying" -- and CR 303.4a's enchant slot, which is a TargetSlot too.
targetSlotFilters :: TargetSlot.TargetSlot -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
targetSlotFilters slot =
  unframed (Maybe.maybeToList (TargetSlot.filter slot))
    -- CR 202.3's computed bound is a Quantity, and a Quantity reaches a Count and
    -- so a Filter -- so the slot's `amount` is a Filter position like any other
    -- and has to be swept, or the cross-checks below would report an atom buried
    -- in one as zero rather than as an offence.
    <> concatMap quantityFilters (Maybe.maybeToList (TargetSlot.amount slot))

-- A continuous effect's affected set (Pawl.Types.Affected), wherever one is
-- written -- a static ability, a combat restriction, an attack or block
-- requirement. Only the predicate arms carry a Filter; the fixed id set
-- (CR 611.2c) and CR 303.4m's "enchanted permanent" carry none.
affectedFilters :: Affected.Affected -> [Filter.Type.Filter Keyword.Keyword]
affectedFilters affected = case affected of
  Affected.TheseObjects _ -> []
  Affected.Matching f -> [f]
  Affected.MatchingAnywhere f -> [f]
  Affected.MatchingOffBattlefield f -> [f]
  Affected.Attached -> []
  Affected.AttachedPlayerControls f -> [f]

-- CR 508.1h's per-attacker share. Both arms reach a Filter: the Counted arm
-- through its Quantity (Sphere of Safety counts "enchantments you control"), and
-- the Fixed arm through its cost's components (Exalted Dragon sacrifices "a
-- land").
--
-- The Fixed arm is SlotlessCostFramed: a declaration announces no targets (CR
-- 508.1h, CR 509.1d), so Pawl.Engine.Cost.payTagged hands the toll no slots and
-- a slot read in one has nothing to be about. Unframed promised otherwise until
-- #2927.
perCreatureFilters :: PerCreature.PerCreature -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
perCreatureFilters perCreature = case perCreature of
  PerCreature.Fixed cost -> slotlessCost (costFilters cost)
  PerCreature.Counted quantity -> quantityFilters quantity

-- refCounts' Filter twin: BOTH axes of every Quantity an ObjectRef holds, off
-- the same Resolve.objectRefQuantities enumeration, so a CR 122.1b counter kind
-- named in a library depth cannot be dropped where its Count is kept (#2740).
refFilters :: ObjectRef.ObjectRef -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
refFilters = concatMap quantityFilters . Resolve.objectRefQuantities

-- The refs a CR 707.10 answer names -- CR 707.10d's candidates, and nothing for
-- the other two. Both sweeps below fold over it, so a candidate description is
-- linted and its slot reads are seen by the dataflow lint.
copyTargetsRefs :: CopyTargets.CopyTargets -> [ObjectRef.ObjectRef]
copyTargetsRefs targets = case targets of
  CopyTargets.Copied -> []
  CopyTargets.ChosenByController -> []
  CopyTargets.ForEach ref -> [ref]

copyTargetsFilters :: CopyTargets.CopyTargets -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
copyTargetsFilters = concatMap objectRefFilters . copyTargetsRefs

-- TAGGED pairs, PR #2739's reader's test: a path from here reaches
-- keywordFilters, through the CounterKind a depth's Quantity may name.
objectRefFilters :: ObjectRef.ObjectRef -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
objectRefFilters ref = case ref of
  ObjectRef.InSlot _ -> []
  -- Day of Judgment's "all creatures", Boil's "all Islands".
  ObjectRef.EachMatching f -> unframed [f]
  -- Rise of the Dark Realms' "all creature cards from all graveyards"; its
  -- ZoneScope names players rather than characteristics, so the Filter is
  -- the whole of what there is to lint.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard _ f) -> unframed [f]
  -- Ignorant Bliss' "all cards from your hand" holds none: the printing takes
  -- the whole hand and states no characteristic, so the arm carries no Filter.
  ObjectRef.EachCardInYourHand -> []
  -- Amnesia's "all nonland cards" does state one, and states it here. Optional,
  -- so its reveal half -- the whole hand -- lints nothing, exactly as the linked
  -- exile arm below does for the printings that take all of their set.
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand _ f) -> unframed (Foldable.toList f)
  -- Leveler's "all cards from your library" holds none, for Ignorant Bliss'
  -- reason: the printing takes the whole zone and states no characteristic.
  -- Caldera Breaker's "all Mountain cards from your library" does state one, and
  -- states it here -- optional, exactly as the hand and linked exile arms are.
  ObjectRef.EachCardInYourLibrary f -> unframed (Foldable.toList f)
  -- Hoarding Dragon's "the exiled card" usually holds none: CR 607.2a's set is
  -- named by which object exiled the cards rather than by their characteristics.
  -- Karn Liberated's "all non-Aura permanent cards exiled with Karn" is the one
  -- printing that also states characteristics, and states them here.
  ObjectRef.EachCardExiledWithSource f -> unframed (Foldable.toList f)
  -- Swift Silence's "all other spells" states its own -- CR 109.2b's set is
  -- named by characteristics exactly as CR 109.2's battlefield sweep is.
  ObjectRef.EachSpell f -> unframed [f]
  -- Glen Elendra's Answer's "all spells your opponents control and all
  -- abilities your opponents control" states its own too, for the arm above's
  -- reason: what the set holds is said in characteristics.
  ObjectRef.EachOnStack f -> unframed [f]
  -- Molten Disaster's "each player" holds no Filter to lint.
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  -- Stuffy Doll's "the chosen player" holds none either: the seat was named by a
  -- choice made on entry, never by characteristics.
  ObjectRef.ChosenPlayer -> []
  -- Count on Luck's "the top card of your library" names a POSITION, so it states
  -- no Filter of its own, and its PlayerRef names players. Its DEPTH is a
  -- Quantity, which reaches one through a Count -- "the top X cards" where X is
  -- itself a fold -- so the depth goes through refFilters for the reason a damage
  -- clause's quantity does, on BOTH of a Quantity's axes.
  ObjectRef.TopOfLibrary {} -> refFilters ref
  -- Treasure Hunt's "until you reveal a nonland card" states its Filter directly
  -- as well as carrying the arm above's count, so both are linted: the
  -- match-defining Filter here, and whatever a Count or a CR 122.1b counter kind
  -- under the count would hold via the arm above's route.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil _ f _) -> unframed [f] <> refFilters ref
  -- Port of Karfell's "a creature card from your graveyard"; its ZoneScope and
  -- its Chooser name players, so the Filter is the whole of what there is to
  -- lint, exactly as for the graveyard sweep above.
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard _ _ f) -> unframed [f]
  -- Elvish Piper's "a creature card from your hand"; its PlayerRef names the
  -- choosers, who are also the hands' owners (CR 402.3), so the Filter is the
  -- whole of what there is to lint -- the chosen graveyard card's arm's answer,
  -- for its reason.
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand _ f) -> unframed [f]
  -- Commune with the Gods' "a creature or enchantment card from among them"; its
  -- slot names the group and holds no characteristic, so the Filter is the whole
  -- of what there is to lint -- the two chosen arms above's answer.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong _ f _ _) -> unframed [f]
  -- The arm above's plural: the same Filter position, saying which members are
  -- taken rather than which may be picked, and linted the same way.
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong _ f) -> unframed [f]
  -- Merfolk Spy's "a card at random from their hand" carries no Filter at all,
  -- only the PlayerRef naming whose hand, so there is nothing here to lint
  -- (gap #1742).
  ObjectRef.RandomCardInHand _ -> []
  -- Tovolar's "any number of Human Werewolves you control": EachMatching's
  -- Filter position exactly -- same zone, same sweep, the chooser standing
  -- between the matches and the set -- so it is framed the same way.
  ObjectRef.AnyNumberMatching f -> unframed [f]
  -- The Garrison in Hanweir Battlements' "If you both own and control this land
  -- and a creature named Hanweir Garrison": the arm above's Filter position, one
  -- permanent instead of a subset, so it is framed the same way -- and the
  -- ownership and control the card prints are conjuncts of that Filter, which is
  -- what this traversal hands to the Filter lints.
  ObjectRef.ChosenPermanent f -> unframed [f]
  -- The arm above with the source riding along: the Filter still says which
  -- counterpart may be picked and nothing about the source, so it is framed the
  -- same way.
  ObjectRef.SourceAndChosenPermanent f -> unframed [f]

-- The Filter a Count folds over (CR 608.2h). Delegated to the *Counts family
-- above rather than re-walked: those traversals are already the project's answer
-- to "every Count a card can author", and a Count's Filter is the only Filter it
-- holds. That reuse is also the one seam here that -Werror does not police -- a
-- Count added to a NEW carrier has to be added there, not here.
countFilters :: [Count.Type.Count Quantity.Type.Quantity] -> [Filter.Type.Filter Keyword.Keyword]
countFilters = fmap Count.Type.filter

-- BOTH axes a Quantity holds card text on: the Filter of every Count reachable
-- from it, and the Filter a CR 122.1b keyword counter hides under a CounterKind;
-- see #2728. The second is a separate walk because the first goes through
-- quantityCounts, whose answer is a list of Counts and so cannot carry it.
--
-- THE funnel: every traversal that reaches a Quantity takes both axes from here
-- rather than from countFilters directly, which is the half conditionFilters,
-- durationFilters and objectRefFilters used to take; see #2740.
quantityFilters :: Quantity.Type.Quantity -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
quantityFilters quantity = unframed (countFilters (quantityCounts quantity)) <> quantityKindFilters quantity

-- Every Filter a CounterKind inside a Quantity carries. CR 122.1b lets a counter's
-- kind be a whole Keyword, and a Keyword may hold a Filter (keywordFilters), so
-- ObjectCounters' kind is card text like any other -- and the shape of the descent
-- is quantityCounts' above, arm for arm, since a nested Quantity may hide one
-- wherever a nested Count may.
--
-- Exhaustive with no catch-all, this file's discipline for a sum: a new Quantity
-- arm that comes to carry a CounterKind must be classified here rather than drop
-- its Filter silently, which is how this position went unswept in the first place.
quantityKindFilters :: Quantity.Type.Quantity -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
quantityKindFilters quantity = case quantity of
  Quantity.Type.Literal _ -> []
  Quantity.Type.ManaValue -> []
  Quantity.Type.Power -> []
  Quantity.Type.Toughness -> []
  Quantity.Type.InSlot _ -> []
  Quantity.Type.Star -> []
  Quantity.Type.Plus (Plus.MkPlus a b) -> quantityKindFilters a <> quantityKindFilters b
  Quantity.Type.Halved (Halved.MkHalved _ inner) -> quantityKindFilters inner
  Quantity.Type.Negate a -> quantityKindFilters a
  -- The Count's own Filter is countFilters' half above; what this half adds is
  -- the CounterKind a Greatest's per-member Quantity may hide, which is
  -- countCounts' descent.
  Quantity.Type.Count count -> concatMap quantityKindFilters (countQuantities count)
  Quantity.Type.ManaCount _ -> []
  Quantity.Type.LifeTotal _ -> []
  Quantity.Type.Speed _ -> []
  Quantity.Type.IsMonarch _ -> []
  Quantity.Type.IsStartingPlayer _ -> []
  Quantity.Type.IsActivePlayer _ -> []
  Quantity.Type.HasDesignation _ -> []
  Quantity.Type.ClassLevel -> []
  Quantity.Type.WasKicked -> []
  Quantity.Type.TimesKickedWith {} -> []
  Quantity.Type.TagWasSpent {} -> []
  Quantity.Type.WasToken -> []
  Quantity.Type.WasBlocking -> []
  Quantity.Type.DamageDealtToThisTurn -> []
  -- CR 122.1's PER-PLAYER tally, whose kind is a Pawl.Types.PlayerCounterKind --
  -- a disjoint domain from the object kinds, carrying no Keyword and so no
  -- Filter. See Pawl.Types.PlayerCounterKind.
  Quantity.Type.PlayerCounters {} -> []
  -- The position this whole function exists for: CR 122.1's per-OBJECT tally
  -- names the kind on the card, and "the number of hexproof-from-Goblins
  -- counters" would carry a Filter under it.
  Quantity.Type.ObjectCounters kind -> counterKindFilters kind
  -- The kind-agnostic reading of that same tally: no CounterKind beside it, so
  -- nothing to dig out.
  Quantity.Type.ObjectCountersOfAnyKind -> []
  Quantity.Type.OpponentsAttacked _ -> []
  Quantity.Type.CardsDiscardedThisTurn _ -> []
  Quantity.Type.LifeGainedThisTurn _ -> []
  Quantity.Type.PlayersDealtDamageThisTurn _ -> []
  Quantity.Type.DamageDealtToPlayersThisTurn _ -> []
  Quantity.Type.SpellsCastLastTurn _ -> []
  Quantity.Type.DungeonsCompleted _ -> []
  Quantity.Type.CompletedDungeon {} -> []
  Quantity.Type.EnteredThisTurn -> []
  Quantity.Type.EnteredFrom _ -> []
  Quantity.Type.WasCastFrom _ -> []
  Quantity.Type.BlockersBeyondFirst -> []
  -- Carries no CounterKind at all, Power's shape.
  Quantity.Type.StationMeasure -> []
  -- quantityCounts' descent: aiming the evaluation at another object does not
  -- stop the payload from naming a kind.
  Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot _ inner) -> quantityKindFilters inner

-- BOTH axes of every Quantity a Condition compares, off conditionQuantities
-- above rather than off conditionCounts beside it: reading the Counts alone drops
-- the Filter a CR 122.1b keyword counter hides under an ObjectCounters kind
-- (#2740). TAGGED for that reason -- a path from here reaches keywordFilters.
conditionFilters :: Condition.Type.Condition -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
conditionFilters = concatMap quantityFilters . conditionQuantities

-- CR 103.5b / CR 103.6: the Filter positions one hand action holds -- its effects'
-- and its own gate's. The gate is a Condition like any other, so it is reached
-- exactly as a static ability's "as long as" clause is.
-- UNFRAMED, unlike a static ability's CR 604.2 clause one field over:
-- Pawl.Engine.Mulligan.allows evaluates this one through Filter.contextFor, which
-- fills no sourceAttachedTo, so CR 303.4b's atom would answer nothing here.
handActionFilters :: HandAction.HandAction Card.Type.Card -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
handActionFilters action =
  concatMap effectFilters (HandAction.effects action)
    <> frame Unframed (concatMap conditionFilters (Maybe.maybeToList (HandAction.condition action)))

-- A Duration reaches a Filter two ways: through a CR 611.2b clause, which is a
-- Condition and so both of a Quantity's axes, and through the non-mana
-- components of CR 116.2c's price. TAGGED, because the first half reaches
-- keywordFilters.
--
-- The price is SlotlessCostFramed: CR 116.2c's is a special action, which uses
-- no stack (CR 116.1) and so announces no target for a slot read to be about --
-- Face.specialActions' CR 116.2d reason exactly, and Pawl.Engine.EndEffect pays
-- it through the same Pawl.Engine.Cost.pay with nothing announced. Unframed
-- promised otherwise until #2927.
durationFilters :: Duration.Duration -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
durationFilters duration =
  concatMap conditionFilters (durationConditions duration)
    <> slotlessCost
      ( case duration of
          Duration.UntilPaid cost -> costFilters cost
          Duration.UntilEndOfTurn -> []
          Duration.Indefinite -> []
          Duration.Perpetual -> []
          Duration.UntilYourNextTurn -> []
          Duration.UntilEndOfYourNextTurn -> []
          Duration.ForAsLongAs _ -> []
          Duration.UntilEndOfCombat -> []
          Duration.UntilUsed -> []
      )

-- A Modification reaches a Filter two ways: through its layer-7 quantities (a
-- Count) and through the keyword a layer-6 grant hands out or takes away (CR
-- 702.29e again).
modificationFilters :: Projection.Modification -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
modificationFilters modification = case modification of
  Modification.GainKeyword keyword -> keywordFilters keyword
  -- Payload-free, so there is no Filter to sweep -- see modificationCounts.
  Modification.GainFlashbackAtManaCost -> []
  -- CR 702.5a again: the granted slot's own Filter, which is card text like any
  -- other and has to be swept. NOT [] -- this, GainKeyword above and LoseKeyword
  -- below are the arms that answer with something, and every other one carries no
  -- Filter at all, LoseKeywordFamily's payload-free family included.
  Modification.GainEnchant slot -> targetSlotFilters slot
  -- Nothing HERE, and that is not a hole: a granted ability's Filters are swept
  -- by grantedActivatedAbilities and grantedTriggeredAbilities below, at the
  -- outer level, so they keep the Framing that a printed ability's do. Answering
  -- here would flatten them to unframed and lose CR 701.3a's attach-destination
  -- distinction.
  Modification.GainAbility _ -> []
  Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness p t) -> quantityFilters p <> quantityFilters t
  Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness p t) -> quantityFilters p <> quantityFilters t
  Modification.LoseAllAbilities -> []
  Modification.LoseNamedAbility _ -> []
  -- CR 702.14a again, from the other side: a removal names the keyword in full,
  -- so its Filter is card text this sweep has to reach.
  Modification.LoseKeyword keyword -> keywordFilters keyword
  -- CR 702.14a's generic term instead of a written instance, and a KeywordFamily
  -- carries no Filter at all -- Hammerheim's "all landwalk abilities" names no
  -- land type for this sweep to reach.
  Modification.LoseKeywordFamily _ -> []
  Modification.SetLandSubtype _ -> []
  Modification.SetLandSubtypeToChosen -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCreatureSubtype _ -> []
  Modification.AddEveryCreatureSubtype -> []
  Modification.AddSubtype _ -> []
  Modification.AddCardType _ -> []
  Modification.SetCardType _ -> []
  Modification.AddSupertype _ -> []
  Modification.RemoveSupertype _ -> []
  Modification.ChangeSubtypeWord {} -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.AddColor _ -> []
  Modification.AddChosenColor -> []
  Modification.SwitchPowerToughness -> []
  -- Payload-free, both of them, so there is no Filter to sweep.
  Modification.AssignCombatDamageWithToughness -> []
  Modification.GrantsStationToughness -> []

-- Four Filter positions, not two: the affected set, each modification's own
-- keywords and Counts, -- since CR 604.2's "as long as" gate landed -- the
-- Counts inside that condition, and the leaves-the-battlefield duration's own,
-- which a CR 611.2b "for as long as" would carry.
-- Tagged rather than flat, because one of the four is framed: CR 604.2's clause is
-- answered by Pawl.Engine.Projection.conditionHolds, which supplies the source's
-- host, and the affected set beside it is not.
staticAbilityFilters :: StaticAbility.StaticAbility Card.Type.Card -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
staticAbilityFilters ability =
  frame
    Unframed
    ( unframed (affectedFilters (StaticAbility.affected ability))
        <> frame Unframed (concatMap durationFilters (Maybe.maybeToList (StaticAbility.lingers ability)))
        <> concatMap modificationFilters (StaticAbility.modifications ability)
    )
    <> frame SourceHostFramed (concatMap conditionFilters (Maybe.maybeToList (StaticAbility.condition ability)))

-- CR 603.6a's "whenever [a permanent] enters" carries one directly; CR 603.8's
-- state trigger carries one through its Condition's Counts.
triggerConditionFilters :: TriggerCondition.TriggerCondition -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
triggerConditionFilters triggerCondition = case triggerCondition of
  TriggerCondition.PermanentEnters f -> unframed [f]
  -- CR 709.5h names a half by name; nothing about the door is a Filter.
  TriggerCondition.SelfHalfUnlocked _ -> []
  -- CR 709.5i names a PlayerRelation; nothing about it is a Filter.
  TriggerCondition.RoomFullyUnlocked _ -> []
  -- Recursive, for triggerConditionCounts' reason: Balemurk Leech's AnyOf holds a
  -- PermanentEnters, whose Filter would otherwise never be swept.
  TriggerCondition.AnyOf conditions -> concatMap triggerConditionFilters conditions
  -- CR 708.7's condition is nullary, so there is nothing in it to be a Filter.
  TriggerCondition.SelfTurnedFaceUp -> []
  -- CR 701.27e's names a face by name; nothing about it is a Filter.
  TriggerCondition.SelfTransformedInto _ -> []
  -- Its bystander sibling carries one -- Cult of the Waxing Moon's "a permanent
  -- you control ... into a non-Human creature".
  TriggerCondition.PermanentTransforms f -> unframed [f]
  -- Its watcher-scoped sibling carries one, and Aven Farseer's is the trivial
  -- `And []` -- which this sweep must still see, an empty Filter being a Filter.
  TriggerCondition.PermanentTurnedFaceUp f -> unframed [f]
  -- CR 702.112b's carries one too -- Valeron Wardens' "a creature you control".
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated _ f) -> unframed [f]
  TriggerCondition.SelfEvolves -> []
  -- CR 702.134c's carries none either: "equipped creature" is CR 301.5f's one
  -- permanent rather than a class of them, and "a creature" narrows by nothing.
  TriggerCondition.AttachedCreatureMentors -> []
  -- CR 303.4b's "enchanted creature" is one permanent rather than a class of
  -- them too, so this condition carries no Filter either.
  TriggerCondition.AttachedCreatureDies -> []
  TriggerCondition.AttachedCreatureBecomesTapped -> []
  -- CR 702.149c's carries none either: it names "this creature" and nothing about
  -- it to narrow by.
  TriggerCondition.SelfTrains -> []
  -- CR 701.21a's Filter narrows the sacrificed permanent -- Vengeful Tracker's
  -- "an artifact" -- and is swept like PermanentDies' below.
  TriggerCondition.PermanentSacrificed payload -> unframed [PermanentSacrificed.filter payload]
  -- CR 603.3b's names a PlayerRelation; the Saga is found through CR 714.2d's
  -- final chapter number rather than through a Filter.
  TriggerCondition.SagaFinalChapterTriggers _ -> []
  TriggerCondition.CardPutIntoGraveyard f -> unframed [f]
  TriggerCondition.PermanentDies f -> unframed [f]
  -- CR 603.2c's batch reading of the same written form carries the same Filter,
  -- so it is swept the same way -- answering [] here would exempt Vengeful
  -- Townsfolk's "other creatures you control" from every corpus filter lint.
  TriggerCondition.PermanentsDie f -> unframed [f]
  -- CR 603.6c's bystander form carries the same kind of Filter one rule wider.
  TriggerCondition.PermanentLeavesTheBattlefield f -> unframed [f]
  -- CR 603.6c's bystander form once more, one destination narrower and with
  -- the same kind of Filter, so it is swept the same way.
  TriggerCondition.PermanentReturnedToHand f -> unframed [f]
  -- CR 603.2c's batch reading of the same form carries the same Filter, swept the
  -- same way for PermanentsDie's reason: answering [] here would exempt Tameshi,
  -- Reality Architect's "noncreature permanents" from every corpus filter lint.
  TriggerCondition.PermanentsReturnedToHand f -> unframed [f]
  -- CR 603.10a's third family carries its Filter inside a record, and it is card
  -- text like any other -- Kishla Skimmer's "your graveyard" is that Filter.
  TriggerCondition.CardLeavesGraveyard payload -> unframed [CardLeavesGraveyard.filter payload]
  TriggerCondition.StateIs condition -> frame Unframed (conditionFilters condition)
  TriggerCondition.SelfEnters -> []
  TriggerCondition.StepBegins {} -> []
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  -- Enrage's condition is nullary: rule 120.3 qualifies the damage in no way, so
  -- there is nothing for a text change to rewrite.
  TriggerCondition.SelfIsDealtDamage -> []
  -- Its watcher-scoped sibling carries one -- Tovolar's "a Wolf or Werewolf you
  -- control", which the card lint must sweep.
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> unframed [f]
  -- CR 603.2c's batch reading of the same written form carries the same Filter,
  -- swept the same way for PermanentsDie's reason: answering [] here would exempt
  -- Pia Nalaar's "artifact creatures you control" from every corpus filter lint.
  TriggerCondition.PermanentsDealCombatDamageToPlayer f -> unframed [f]
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> []
  TriggerCondition.PlayerTookInitiative -> []
  TriggerCondition.OpponentLostLifeDuringYourTurn -> []
  TriggerCondition.SelfAttacks _ -> []
  -- CR 702.149a names a quality the OTHER attackers must have, so this one DOES
  -- carry a Filter -- "power greater than this creature's power".
  TriggerCondition.SelfAttacksWithAnother f -> unframed [f]
  -- CR 506.5's condition names a quality the ATTACKER must have, so it carries a
  -- Filter -- rule 702.83a's "a creature you control".
  TriggerCondition.CreatureAttacksAlone f -> unframed [f]
  -- CR 508.3a's second sentence names no quality of the attacker -- CR 508.1a has
  -- already made it a creature -- so this one carries no Filter to traverse.
  TriggerCondition.CreatureAttacksYou -> []
  -- CR 508.3b names no quality of anything: its subject is the ability's own
  -- attachment, so there is no Filter here either.
  TriggerCondition.AttachedPlayerIsAttacked -> []
  -- CR 508.3d names no quality of anything either: its subject is a player, its
  -- payload is a PlayerRelation and not a Filter, and the creatures it counts
  -- are the DECLARATION's.
  TriggerCondition.PlayerAttacks _ -> []
  -- CR 508.3c names a quality the declared creatures must have, so this one DOES
  -- carry a Filter -- Hermes, Overseer of Elpis' "Birds".
  TriggerCondition.PlayerAttacksWith payload -> unframed [PlayerAttacksWith.filter payload]
  -- CR 508.3e names two players and no quality of anything, so no Filter --
  -- rule 508.3d's answer, twice over.
  TriggerCondition.PlayerAttacksPlayer {} -> []
  -- CR 702.105a names no quality of the attacker, only a fact about whom it
  -- attacked, so no Filter.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> []
  TriggerCondition.SelfBlocks -> []
  -- CR 509.3b names a quality the attacker blocked must have, so this one DOES
  -- carry a Filter -- Netcaster Spider's "with flying".
  TriggerCondition.SelfBlocksCreature f -> unframed [f]
  TriggerCondition.SelfBlocksAtLeast _ -> []
  -- CR 509.3e's filtered form names a quality the attackers blocked must have,
  -- so this one DOES carry a Filter.
  TriggerCondition.SelfBlocksOneOrMore f -> unframed [f]
  TriggerCondition.SelfBecomesBlocked -> []
  -- CR 509.3d names a quality the blocker must have, so this one DOES carry a
  -- Filter -- rule 702.25a's "without flanking".
  TriggerCondition.SelfBecomesBlockedBy f -> unframed [f]
  -- The same rule read by a BYSTANDER, whose Filter is over the ATTACKER instead
  -- -- CR 701.54c's "your Ring-bearer".
  TriggerCondition.PermanentBecomesBlockedBy f -> unframed [f]
  -- The same rule's attacking-side form, whose Filter is a predicate over the
  -- blockers -- Serra Inquisitors' "black".
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> unframed [f]
  -- The same rule read by a bystander names a quality of nothing: whom the
  -- attacker attacked is a PlayerRelation and the blockers are only counted, so
  -- there is no Filter here.
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> []
  TriggerCondition.SelfAttacksUnblocked -> []
  TriggerCondition.SelfCycled -> []
  TriggerCondition.SelfRevealedForMiracle -> []
  TriggerCondition.SelfDiscarded -> []
  TriggerCondition.PlayerDiscards _ -> []
  TriggerCondition.PlayerCycles _ -> []
  TriggerCondition.PlayerDrawsNthCard {} -> []
  -- CR 725.1's crowning condition is a PlayerRelation, which holds no Filter.
  TriggerCondition.PlayerBecomesMonarch _ -> []
  -- CR 603.7's slot-named condition holds a SlotName, which is no Filter -- what
  -- the slot holds was selected by the arming spell's own target slot.
  TriggerCondition.LoseControlOfBound _ -> []
  TriggerCondition.RoomEntered _ -> []
  -- CR 309.7's condition carries a PlayerRelation, which is no Filter.
  TriggerCondition.PlayerCompletesDungeon _ -> []
  -- CR 701.22d and CR 701.25d carry a PlayerRelation and CR 702.170a nothing,
  -- so none of them holds a Filter.
  TriggerCondition.PlayerScries _ -> []
  TriggerCondition.RingTemptsPlayer _ -> []
  TriggerCondition.PlayerSurveils _ -> []
  TriggerCondition.PlayerRollsDice _ -> []
  TriggerCondition.PlayerWinsCoinFlip _ -> []
  TriggerCondition.SelfBecomesPlotted -> []
  -- CR 701.44b DOES carry one, a predicate over the explorer -- Wildgrowth
  -- Walker's "a creature you control" -- which the card lint must sweep.
  TriggerCondition.PermanentExplores f -> unframed [f]
  -- CR 701.43d carries nothing, so no Filter either.
  TriggerCondition.SelfExerted -> []
  -- CR 701.3a's carries one over the ATTACHMENT -- Bramble Elemental's "an
  -- Aura" -- which this sweep must see for PermanentTurnedFaceUp's reason.
  TriggerCondition.SelfBecomesAttachedBy f -> unframed [f]
  -- CR 603.12's carries nothing, so no Filter either.
  TriggerCondition.Reflexive -> []
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> []
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> []
  TriggerCondition.SelfDies -> []
  TriggerCondition.SelfLeavesTheBattlefield -> []
  TriggerCondition.HauntedCreatureDies -> []
  TriggerCondition.SpellOrAbilityCounters _ -> []
  TriggerCondition.DamageToPlayerPrevented _ -> []
  -- Rule 615.13's other reading DOES carry one, a predicate over the damage's
  -- source -- Samite Ministration's "black or red" -- which this sweep must see
  -- for PermanentExplores' reason.
  TriggerCondition.SelfPreventsDamage f -> unframed [f]
  TriggerCondition.PlayerGainsLife _ -> []
  TriggerCondition.PlayersGainLife _ -> []
  TriggerCondition.PlayerLosesLife _ -> []
  -- CR 714.2b's threshold names a counter kind, and CR 122.1b's kind may be a
  -- whole Keyword carrying a Filter -- "whenever the second hexproof-from-
  -- Goblins counter is put on this permanent"; see #2728. The Natural beside it is
  -- no Filter.
  TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached kind _) -> counterKindFilters kind
  TriggerCondition.SelfBecomesClassLevel _ -> []
  -- CR 310.12b names a counter kind alone, swept for the arm above's reason.
  TriggerCondition.SelfLastCounterRemoved kind -> counterKindFilters kind
  -- And so does its any-amount mirror.
  TriggerCondition.SelfCountersRemoved kind -> counterKindFilters kind
  -- CR 603.2c's batch placement carries one, over the permanents the counters
  -- landed on -- swept like PermanentsDie's, so a card's "one or more creatures"
  -- is not exempt from the corpus filter lints. And its KIND beside it, for the
  -- three arms above's reason.
  TriggerCondition.PermanentsGetCounters (CounterPlacement.MkCounterPlacement kind f) -> unframed [f] <> counterKindFilters kind
  -- And its per-permanent scope, whose Filter is read against ONE permanent --
  -- swept all the same, the lints being about the Filter and not the scope.
  TriggerCondition.PermanentGetsCounters (CounterPlacement.MkCounterPlacement kind f) -> unframed [f] <> counterKindFilters kind
  -- CR 601.2i's "whenever you cast a [type] spell" carries one directly, over
  -- the spell rather than over a permanent.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast f _ _ _) -> unframed [f]
  -- "This spell" names the bearer and needs no Filter to say so.
  TriggerCondition.SelfCast -> []
  -- Rule 702.21a names the bearer as well, and asks only a relation of the
  -- targeting object's controller -- no Filter over the object itself.
  TriggerCondition.SelfBecomesTargeted _ -> []
  -- CR 601.2c from the player's side names CR 109.5's "you", a relation over the
  -- targeting object's controller and a kind read off the event, so there is no
  -- Filter here either.
  TriggerCondition.ControllerBecomesTarget {} -> []

-- Every SlotName a TriggerCondition names OUTRIGHT. Exhaustive with no
-- fallthrough, triggerConditionCounts' shape and for its reason: a condition
-- that gains a slot is named by -Werror rather than silently answering [].
--
-- What makes the answer worth having is that the reader is SINGULAR.
-- Pawl.Engine.Event matches CR 603.7's slot-named condition through
-- Binding.objectSlots, which is Binding.onlyOne and so declines a slot naming
-- several objects rather than picking one of them, and which never consults
-- Binding.groupsOf at all -- so a slot bound as a group is not merely declined
-- but invisible, and the delayed ability never fires, in silence.
--
-- A slot a condition names through a FILTER rather than outright is fenced too,
-- by filterSlotsReadSingly below rather than by this walk: clashesIn folds both
-- over a delayed ability's condition.
triggerConditionSlots :: TriggerCondition.TriggerCondition -> [SlotName.SlotName]
triggerConditionSlots triggerCondition = case triggerCondition of
  TriggerCondition.SelfEnters -> []
  TriggerCondition.PermanentEnters _ -> []
  TriggerCondition.StepBegins _ -> []
  -- CR 603.8's state trigger holds a Condition, which is a pair of Quantities
  -- and Filters -- no SlotName of its own.
  TriggerCondition.StateIs _ -> []
  TriggerCondition.SelfDealsCombatDamageToPlayer -> []
  TriggerCondition.SelfIsDealtDamage -> []
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> []
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> []
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> []
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> []
  TriggerCondition.PlayerTookInitiative -> []
  TriggerCondition.OpponentLostLifeDuringYourTurn -> []
  TriggerCondition.SelfCycled -> []
  TriggerCondition.SelfRevealedForMiracle -> []
  TriggerCondition.SelfDiscarded -> []
  TriggerCondition.PlayerDiscards _ -> []
  TriggerCondition.PlayerCycles _ -> []
  TriggerCondition.PlayerDrawsNthCard _ -> []
  TriggerCondition.SelfAttacks _ -> []
  TriggerCondition.SelfAttacksWithAnother _ -> []
  TriggerCondition.CreatureAttacksAlone _ -> []
  TriggerCondition.CreatureAttacksYou -> []
  TriggerCondition.AttachedPlayerIsAttacked -> []
  TriggerCondition.PlayerAttacks _ -> []
  TriggerCondition.PlayerAttacksWith _ -> []
  TriggerCondition.PlayerAttacksPlayer _ -> []
  TriggerCondition.SelfAttacksPlayerWithMostLife -> []
  TriggerCondition.SelfBlocks -> []
  TriggerCondition.SelfBlocksCreature _ -> []
  TriggerCondition.SelfBlocksAtLeast _ -> []
  TriggerCondition.SelfBlocksOneOrMore _ -> []
  TriggerCondition.SelfBecomesBlocked -> []
  TriggerCondition.SelfBecomesBlockedBy _ -> []
  TriggerCondition.PermanentBecomesBlockedBy _ -> []
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> []
  TriggerCondition.CreatureBecomesBlockedByAtLeast _ -> []
  TriggerCondition.SelfAttacksUnblocked -> []
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> []
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> []
  TriggerCondition.SelfDies -> []
  TriggerCondition.CardPutIntoGraveyard _ -> []
  TriggerCondition.PermanentDies _ -> []
  TriggerCondition.PermanentsDie _ -> []
  TriggerCondition.SelfLeavesTheBattlefield -> []
  TriggerCondition.PermanentLeavesTheBattlefield _ -> []
  TriggerCondition.PermanentReturnedToHand _ -> []
  TriggerCondition.PermanentsReturnedToHand _ -> []
  TriggerCondition.CardLeavesGraveyard {} -> []
  TriggerCondition.AttachedCreatureDies -> []
  TriggerCondition.AttachedCreatureBecomesTapped -> []
  -- CR 702.55a names the haunted creature through the haunting object's own
  -- attachment rather than through a slot.
  TriggerCondition.HauntedCreatureDies -> []
  TriggerCondition.SpellOrAbilityCounters _ -> []
  TriggerCondition.DamageToPlayerPrevented _ -> []
  TriggerCondition.SelfPreventsDamage _ -> []
  TriggerCondition.PlayerGainsLife _ -> []
  TriggerCondition.PlayersGainLife _ -> []
  TriggerCondition.PlayerLosesLife _ -> []
  TriggerCondition.SelfCountersReached _ -> []
  TriggerCondition.SelfBecomesClassLevel _ -> []
  TriggerCondition.SelfLastCounterRemoved _ -> []
  TriggerCondition.SelfCountersRemoved _ -> []
  TriggerCondition.PermanentsGetCounters _ -> []
  TriggerCondition.PermanentGetsCounters _ -> []
  TriggerCondition.SpellCast _ -> []
  TriggerCondition.SelfCast -> []
  TriggerCondition.SelfBecomesTargeted _ -> []
  TriggerCondition.ControllerBecomesTarget _ -> []
  TriggerCondition.SelfHalfUnlocked _ -> []
  TriggerCondition.RoomFullyUnlocked _ -> []
  -- Recursive, for triggerConditionCounts' reason: a branch of an AnyOf may be
  -- any condition, the slot-named one included.
  TriggerCondition.AnyOf conditions -> concatMap triggerConditionSlots conditions
  TriggerCondition.SelfTurnedFaceUp -> []
  TriggerCondition.SelfTransformedInto _ -> []
  TriggerCondition.PermanentTransforms _ -> []
  TriggerCondition.PermanentTurnedFaceUp _ -> []
  TriggerCondition.PermanentBecomesDesignated _ -> []
  TriggerCondition.SelfEvolves -> []
  TriggerCondition.AttachedCreatureMentors -> []
  TriggerCondition.SelfTrains -> []
  TriggerCondition.PermanentSacrificed {} -> []
  TriggerCondition.SagaFinalChapterTriggers _ -> []
  TriggerCondition.PlayerBecomesMonarch _ -> []
  -- CR 603.7's slot-named condition, the one arm with an answer: Ray of
  -- Command's "when you lose control of the creature" watches the permanent its
  -- own spell targeted.
  TriggerCondition.LoseControlOfBound slot -> [slot]
  TriggerCondition.RoomEntered _ -> []
  TriggerCondition.PlayerScries _ -> []
  TriggerCondition.RingTemptsPlayer _ -> []
  TriggerCondition.PlayerCompletesDungeon _ -> []
  TriggerCondition.PlayerSurveils _ -> []
  TriggerCondition.PlayerRollsDice _ -> []
  TriggerCondition.PlayerWinsCoinFlip _ -> []
  TriggerCondition.SelfBecomesPlotted -> []
  TriggerCondition.PermanentExplores _ -> []
  TriggerCondition.SelfExerted -> []
  TriggerCondition.SelfBecomesAttachedBy _ -> []
  -- CR 603.7c's captured environment is what a reflexive trigger knows, but the
  -- condition itself admits no event and names nothing.
  TriggerCondition.Reflexive -> []

-- Every SlotName a Filter reads SINGLY -- today exactly the IsControllerOfBound
-- atoms in it. Pawl.Engine.Count answers that one through
-- Pawl.Engine.Filter.slotOneObject, which declines a slot naming several objects
-- rather than picking one of them (Pawl.Engine.Binding.onlyOne's doctrine), so
-- the atom is False for every candidate and the count is zero, in silence.
--
-- Exhaustive with no fallthrough, triggerConditionSlots' shape and for its
-- reason. Pawl.Engine.Filter.boundSlots is deliberately NOT reused: it ends in a
-- catch-all, so a new atom naming a slot would be absorbed there, and it reports
-- IsBound and SameNameAsBound beside the atom wanted -- both of which read the
-- whole bound set through Filter.Context and so tolerate a group.
--
-- ControlledByBound is not one either, though it names a slot:
-- Pawl.Engine.Filter.bakeBound answers it off a map of PLAYER slots, a namespace
-- disjoint from the object slots a binder mints.
--
-- Reported wherever the atom sits, not only where it is ANSWERED -- which is a
-- Scope.OverPlayers count's filter and nothing else, Pawl.Types.Filter's own
-- haddock says, every other position leaving it vacuously False. Within one
-- card's own text that is the conservative direction, an atom in one of those
-- other positions naming no slot at all. It says nothing about a position in
-- ANOTHER object's text, which is read for real in a resolution of its own --
-- see clashesIn, which is where that boundary is kept (#2735).
--
-- NOT descended into: the Filter a Keyword carries (CR 702.29e) and the one a
-- CounterKind hides under a keyword (CR 122.1b). Sound rather than elided --
-- keywordFilters tags both KeywordFramed, a position whose evaluator supplies no
-- slots at all, so nothing there reads a slot singly or plurally and there is no
-- clash to report. That covers the payload as it is READ under Filter.HasKeyword;
-- the one payload rule 702 transplants into a slot instead, CR 702.6c's equip
-- quality, keywordFilters hands out as its own MintedTargetSlot pair, which IS
-- swept. sweptForSingularSlots below is that argument, and every reader
-- reaches this walk through framedSlotsReadSingly beside it, so a keyword's
-- Filter arriving as a TOP-LEVEL tagged pair is dropped exactly as this
-- non-descent drops it nested (#2741).
filterSlotsReadSingly :: Filter.Type.Filter Keyword.Keyword -> [SlotName.SlotName]
filterSlotsReadSingly predicate = case predicate of
  Filter.Type.HasCardType _ -> []
  Filter.Type.HasSupertype _ -> []
  Filter.Type.HasColor _ -> []
  Filter.Type.HasSubtype _ -> []
  Filter.Type.HasName _ -> []
  -- The keyword's own Filter, left alone for the reason above.
  Filter.Type.HasKeyword _ -> []
  Filter.Type.HasKeywordFamily _ -> []
  Filter.Type.PowerAtLeast _ -> []
  Filter.Type.PowerAtMost _ -> []
  Filter.Type.ToughnessGreaterThanPower -> []
  Filter.Type.PowerLessThanSource -> []
  Filter.Type.PowerGreaterThanSource -> []
  -- Not one either, though it names a slot: the slot holds an AMOUNT
  -- (Pawl.Engine.Filter.Context's boundAmounts), a namespace disjoint from the
  -- object slots a binder mints, which is ControlledByBound's position above.
  Filter.Type.PowerIsAmountInSlot _ -> []
  Filter.Type.PowerAtLeastAmountInSlot _ -> []
  Filter.Type.ManaValueAtMost _ -> []
  Filter.Type.ManaValueIsEven -> []
  Filter.Type.ManaValueAtMostAmount -> []
  Filter.Type.ControlledBy _ -> []
  Filter.Type.ControlledByDefendingPlayer -> []
  -- A PLAYER slot, not an object one -- the disjoint namespace above.
  Filter.Type.ControlledByBound _ -> []
  Filter.Type.ControlledByPlayer _ -> []
  Filter.Type.ControlledByRecipient -> []
  Filter.Type.OwnedBy _ -> []
  Filter.Type.IsSource -> []
  Filter.Type.TargetsSource -> []
  Filter.Type.TargetsOnlySource -> []
  Filter.Type.TargetsPlayer _ -> []
  -- Reads the whole bound set off Filter.Context, so a group is every one of its
  -- members rather than nothing -- the atom this lint must NOT report.
  Filter.Type.IsBound _ -> []
  -- Reads the whole set too, one field over.
  Filter.Type.SameNameAsBound _ -> []
  -- Reads the whole set too, one field further over.
  Filter.Type.SameControllerAsBound _ -> []
  Filter.Type.HasChosenName -> []
  -- Reads no slot at all: rule 702.16k's player arrives on Filter.Context.
  Filter.Type.OfChosenPlayer -> []
  Filter.Type.IsPlayer _ -> []
  -- The one arm with an answer: the candidate is the controller of the object
  -- the slot names (CR 608.2h), read through slotOneObject.
  Filter.Type.IsControllerOfBound slot -> [slot]
  -- DESCENT: the nest is card text like any other, and an atom written into it
  -- is read exactly as one written at the top level.
  Filter.Type.ControlsMoreThanYou f -> filterSlotsReadSingly f
  Filter.Type.CardsInGraveyardAtLeast _ -> []
  Filter.Type.IsAttacking -> []
  Filter.Type.IsAttackingPlayer _ -> []
  Filter.Type.IsAttackingPlaneswalker _ -> []
  Filter.Type.IsAttackingBattle _ -> []
  Filter.Type.DeclaredAttackedThisCombat -> []
  Filter.Type.IsBlocking -> []
  Filter.Type.IsBlocked -> []
  Filter.Type.DeclaredAttackerThisCombat -> []
  Filter.Type.DeclaredBlockerThisCombat -> []
  Filter.Type.AttackedThisTurn -> []
  Filter.Type.MilledThisTurn -> []
  Filter.Type.DealtDamageThisTurn -> []
  -- DESCENT, for ControlsMoreThanYou's reason.
  Filter.Type.AttachedTo f -> filterSlotsReadSingly f
  -- DESCENT, for the atom above's reason.
  Filter.Type.HasAttached f -> filterSlotsReadSingly f
  Filter.Type.IsAttachedToSource -> []
  Filter.Type.IsHostOfSource -> []
  Filter.Type.CanHostSubject -> []
  Filter.Type.CanAttachToSubject -> []
  Filter.Type.IsToken -> []
  Filter.Type.IsActivatedAbility -> []
  -- DESCENT, for RepresentedByCard's reason below.
  Filter.Type.FromSource f -> filterSlotsReadSingly f
  Filter.Type.IsTapped -> []
  Filter.Type.IsFaceDown -> []
  -- DESCENT, for the atom above's reason.
  Filter.Type.RepresentedByCard f -> filterSlotsReadSingly f
  Filter.Type.IsExiledFaceDown -> []
  Filter.Type.Transformed -> []
  Filter.Type.IsRingBearer -> []
  Filter.Type.HasDesignation _ -> []
  -- The kind may be a whole Keyword hiding a Filter, left alone for the reason
  -- the keyword atom above is.
  Filter.Type.HasCounters _ -> []
  Filter.Type.HasCountersOfAnyKind -> []
  Filter.Type.HasNonManaActivatedAbility -> []
  Filter.Type.IsInZone _ -> []
  Filter.Type.WasCastFrom _ -> []
  Filter.Type.And fs -> concatMap filterSlotsReadSingly fs
  Filter.Type.Or fs -> concatMap filterSlotsReadSingly fs
  Filter.Type.Not f -> filterSlotsReadSingly f

-- The Filters a DamagePattern carries -- its source half and its printed
-- recipient half, the two axes of that type that ARE predicates over an object.
damagePatternFilters :: DamagePattern.DamagePattern -> [Filter.Type.Filter Keyword.Keyword]
damagePatternFilters pattern_ = DamagePattern.whatSource pattern_ : Maybe.maybeToList (DamagePattern.whatRecipient pattern_)

-- CR 613.11: which objects a player effect names -- a cost modifier's (CR
-- 601.2f), a timing permission's (CR 601.3b) or a countering prohibition's (CR
-- 701.6a).
playerEffectFilters :: PlayerEffect.PlayerEffect -> [Filter.Type.Filter Keyword.Keyword]
playerEffectFilters playerEffect = case playerEffect of
  PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost f _) -> [f]
  -- CR 601.2f at the ACTIVATION moment, Oppressive Rays' third line. Its Filter
  -- names the ability's SOURCE PERMANENT, exactly as ReduceActivationCost's
  -- below does. The whichKind beside it is not returned, for the reason that
  -- arm's grantedBy is not: CR 605.1a's classification is no more a Filter than
  -- a rule-702 family is.
  PlayerEffect.IncreaseActivationCost (IncreaseActivationCost.MkIncreaseActivationCost f _ _) -> [f]
  PlayerEffect.ReduceSpellCost (ReduceSpellCost.MkReduceSpellCost f _ _) -> [f]
  -- CR 601.2f's other moment: Heartstone's Filter narrows the ability's SOURCE
  -- PERMANENT rather than a spell, and is authored the same way. The grantedBy
  -- and whichKind beside it are not returned: neither a KeywordFamily nor CR
  -- 605.1a's classification is a Filter, so the lints this list feeds have
  -- nothing to say about either.
  --
  -- BOTH Filters, and they are held to one standard because they are evaluated
  -- through one context: `whichTargets` (Dwarven Mauler's "that target this
  -- creature") asks about the ability's chosen TARGET rather than its source, but
  -- Pawl.Engine.PlayerEffect.matchesObjectFrom builds the same Context for it, so
  -- the same framing and the same atom vocabulary apply.
  PlayerEffect.ReduceActivationCost (ReduceActivationCost.MkReduceActivationCost f _ _ targets _ _) -> f : Maybe.maybeToList targets
  -- CR 601.2f's addition carries a Filter in two places: its own criterion
  -- ("nontoken Rebels"), and one inside each component it adds ("sacrifice a
  -- land"). Both are authored by the card, so both are linted, and the inner
  -- ones go through costComponentFilters so an added component and a printed
  -- one are held to one standard.
  PlayerEffect.AddActivationCost (AddActivationCost.MkAddActivationCost f components _) -> f : concatMap costComponentFilters components
  -- The spell-side twin, whose Filter names the SPELL (Drought's is universal)
  -- and whose components carry one of their own ("sacrifice a SWAMP").
  PlayerEffect.AddSpellCost (AddSpellCost.MkAddSpellCost f components _) -> f : concatMap costComponentFilters components
  PlayerEffect.CantCastSpells -> []
  PlayerEffect.CantActivateAbilities -> []
  PlayerEffect.CantCastMoreThan _ -> []
  -- CR 601.3 / 305.1: the quality both prohibitions name is a CardName chosen as
  -- the source entered, which is not a Filter and is not written by the card.
  PlayerEffect.CantCastChosenName -> []
  PlayerEffect.CantPlayLandChosenName -> []
  -- CR 305.2 carries a bare count of extra land plays, not a Filter: it names
  -- how many lands, never which spells.
  PlayerEffect.PlayAdditionalLands _ -> []
  PlayerEffect.NoMaximumHandSize -> []
  -- CR 402.2's three number-carrying arms carry a bare count of cards for the
  -- same reason: each names how many cards a hand may hold, or by how much that
  -- number moves, never which spells.
  PlayerEffect.SetMaximumHandSize _ -> []
  PlayerEffect.IncreaseMaximumHandSize _ -> []
  PlayerEffect.ReduceMaximumHandSize _ -> []
  -- CR 500.5 carries a ManaFilter, not a Filter: the set it names is MANA, and
  -- this traversal is about the spells a player effect names.
  PlayerEffect.DontLoseUnspentMana _ -> []
  -- CR 609.4b carries a ManaFilter and a set of mana types, for the same reason:
  -- what it names is MANA.
  PlayerEffect.SpendManaAsThough _ -> []
  -- CR 702.18a / 702.11c carry a PlayerScope, not a Filter: the set they name is
  -- players, and this traversal is about the spells a player effect names.
  PlayerEffect.CantBeTargetedBy _ -> []
  -- CR 601.3b's "a spell with certain qualities", which is a Filter over the
  -- spell exactly as a cost modifier's is (Vedalken Orrery's is `And []`).
  PlayerEffect.CastAsThoughItHadFlash f -> [f]
  -- CR 601.1a / 601.3b's play-scoped sibling, the same shape (Scout's Warning's
  -- is HasCardType Creature).
  PlayerEffect.MayPlayAsThoughItHadFlash f -> [f]
  -- CR 701.6a's "a spell or ability", narrowed by the victim's own qualities
  -- exactly as a cost modifier's is (Spider-Punk's is `And []`, Prowling
  -- Serpopard's is HasCardType Creature).
  PlayerEffect.CantBeCountered f -> [f]
  -- CR 615.12 narrows by a DamagePattern, two of whose axes are Filters over an
  -- object: the damage's SOURCE (Excruciator's "by this creature", `IsSource`)
  -- and its printed RECIPIENT (Lava Burst's "to a creature"). The kind, the
  -- player relation and the two baked fields beside them are not predicates over
  -- an object. The pattern's authorability is linted by
  -- unpreventablePatternOffends below.
  PlayerEffect.DamageCantBePrevented pattern_ -> damagePatternFilters pattern_
  -- CR 614.9's twin, patterned by the same type and read the same way.
  PlayerEffect.DamageCantBeRedirected pattern_ -> damagePatternFilters pattern_
  -- CR 701.23's prohibition narrows by WHOSE library and WHOSE spell or
  -- ability, both PlayerScopes, and by no quality a Filter could state.
  PlayerEffect.CantSearchLibraries _ -> []
  -- CR 702.16a's quality is a chosen card NAME, read off the source's
  -- Object.chosenNames rather than written by the card, so this arm carries no
  -- Filter for the same reason the two chosen-name prohibitions above carry
  -- none.
  PlayerEffect.HasProtectionFromChosenName -> []
  -- CR 725 names no quality either: the designation has no parts (Jared
  -- Carthalion, True Heir).
  PlayerEffect.CantBecomeMonarch -> []
  -- CR 601.3a's Filter half, which is exactly a quality of the spell (Damping
  -- Engine's "artifact, creature, or enchantment spells").
  PlayerEffect.CantCastMatching f -> [f]
  -- CR 307.5 narrows a MOMENT rather than a class of spell, so there is no
  -- quality here either (Teferi, Mage of Zhalfir).
  PlayerEffect.CastOnlyAtSorcerySpeed -> []
  -- CR 305.1's unrestricted prohibition narrows nothing: every land is stopped.
  PlayerEffect.CantPlayLands -> []
  -- CR 601.3's zone permission, narrowed by the card's own qualities exactly as
  -- the timing permission beside it is (Yawgmoth's Will's is `And []`, Garruk's
  -- Horde's "creature spells"). WHOSE zone rides beside the Filter and is no
  -- quality of the card, so it is not a position this lint sweeps.
  PlayerEffect.CastFrom grant -> [CastFromZone.matching grant]
  -- CR 305.1's play-side permission narrows nothing: a land play has already
  -- fixed the card type, and Crucible of Worlds' sentence says no more.
  PlayerEffect.PlayLandsFrom _ -> []
  -- CR 118.9's standing alternative cost, narrowed by the spell's own qualities
  -- exactly as the zone permission above is (Omniscience's is `And []`).
  PlayerEffect.CastFromHandWithoutPayingManaCost f -> [f]
  -- CR 101.2's counter prohibition narrows by counter KIND, not by a Filter over
  -- objects: the object-side half of the same printings is
  -- Pawl.Types.CounterRestriction, whose affected filter this lint reaches
  -- through Face.counterRestrictions instead.
  PlayerEffect.CantGetCounters _ -> []
  -- CR 705.3's statement narrows by nothing at all: it names a face, a win and
  -- a once-per-turn flag, and no Filter over objects.
  PlayerEffect.StateCoinFlip _ -> []

-- CR 707.9's "except ..." clauses. Only the CR 707.9a arm reaches a Filter, and
-- only through the keyword it names; CR 707.9b's two arms name a pair of literals
-- and a set of card types, neither of which narrows anything.
copyExceptionFilters :: CopyException.CopyException -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
copyExceptionFilters exception = case exception of
  CopyException.SetPowerToughness _ -> []
  CopyException.GainKeywords keywords -> concatMap keywordFilters (Set.toList keywords)
  CopyException.AddCardTypes _ -> []

-- CR 208.2b's entry option. The P/T pair narrows nothing; the keywords reach a
-- Filter apiece, copyExceptionFilters' road one rule over.
entryOptionFilters :: EntryOption.EntryOption -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
entryOptionFilters option = concatMap keywordFilters (Set.toList (EntryOption.keywords option))

-- The Filters an EntryRewrite carries, on five different axes. CR 201.4a's is the
-- restriction on which cards' names an as-enters name choice may name (Null
-- Chamber's "other than a basic land card name"), a predicate over a CARD in the
-- Oracle card reference rather than over an object on the board -- the same shape
-- Effect.Search's is, and why it belongs in this walk. CR 614.1c's as-enters
-- sacrifice carries one of the ordinary kind, over permanents on the battlefield
-- (Shimatsu the Bloodcloaked's "any number of permanents"). CR 614.1c's as-enters
-- reveal carries a third, over a CARD IN A HAND (Rustic Clachan's "a Kithkin
-- card"). CR 707.5's copy choice carries a fourth, over permanents on the
-- battlefield (Copy Enchantment's "any enchantment"). CR 707.9a's copy exception
-- carries a fifth, through the keyword it grants (a landwalk's). None of the
-- five is framed.
entryRewriteFilters :: EntryRewrite.EntryRewrite (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
entryRewriteFilters entryRewrite = case entryRewrite of
  EntryRewrite.ChooseCardNames f -> unframed [f]
  EntryRewrite.ChooseCardName f -> unframed [f]
  EntryRewrite.RevealOrTapped f -> unframed [f]
  -- CR 707.5's eligible set -- Clone's "any creature", Copy Enchantment's "any
  -- enchantment" -- is a criterion over permanents on the battlefield, so it
  -- belongs in this walk. So do CR 707.9's exceptions beside it, which reach a
  -- Filter through a KEYWORD rather than by stating a criterion of their own:
  -- CR 707.9a's gained ability is a Pawl.Types.Keyword, and CR 702.14c's
  -- landwalk is one that holds a Filter. The CR 614.1d `tapped` flag beside them
  -- holds none (Vesuva).
  EntryRewrite.AsCopy (AsCopy.MkAsCopy f exceptions _) -> unframed [f] <> concatMap copyExceptionFilters exceptions
  -- CR 208.2b's options grant KEYWORDS, and a keyword may carry a Filter of its
  -- own (CR 702.14c's landwalk) -- the axis the AsCopy arm above reaches through
  -- CR 707.9a, on the payload beside it. Vacuous over `data/cards/` while Primal
  -- Plasma's and Molten Sentry's options grant flying, defender and haste, none of
  -- which carries one; Pawl.FilterPositionLintSpec's "CR 702 a keyword's own
  -- filter is framed by the keyword and not by whatever quotes it" plants one and
  -- is what proves the descent.
  EntryRewrite.ChoiceOf options -> concatMap entryOptionFilters options
  EntryRewrite.ChoiceByCoinFlip f -> entryOptionFilters (EntryFlip.heads f) <> entryOptionFilters (EntryFlip.tails f)
  EntryRewrite.ChooseColor -> []
  EntryRewrite.ChooseBasicLandType -> []
  EntryRewrite.ChoosePlayer -> []
  EntryRewrite.ReadAhead -> []
  -- CR 614.1c's amount is a Quantity (Undergrowth Scavenger's "equal to the number
  -- of creature cards in all graveyards"), so a Count inside it holds card text on
  -- the same axis EntryRiders' counts do -- riderFilters walks those, and this
  -- walks this.
  -- EVERY kind's amount, since the row carries a map of them (#2314) -- and
  -- every KIND, the map's keys being CR 122.1b's, any of which may be a whole
  -- Keyword carrying a Filter; see #2728.
  EntryRewrite.WithCounters w -> withCountersFilters w
  -- CR 614.1c's granted keywords, the option arms' payload without the choice
  -- around it, and reached the same way.
  EntryRewrite.WithKeywords keywords -> concatMap keywordFilters (Set.toList keywords)
  EntryRewrite.UnderSourceControl -> []
  EntryRewrite.Riot -> []
  EntryRewrite.Unleash -> []
  EntryRewrite.Bloodthirst _ -> []
  EntryRewrite.Compleated _ -> []
  EntryRewrite.Tapped -> []
  EntryRewrite.PayLifeOrTapped _ -> []
  EntryRewrite.EntersTransformed -> []
  -- BOTH fields: the permanents the sacrifice may take, and CR 122.1b's kind
  -- the entering permanent takes one of per sacrifice, which may be a whole
  -- Keyword carrying a Filter; see #2728.
  EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber f kind) -> unframed [f] <> concatMap counterKindFilters (Maybe.maybeToList kind)
  -- CR 614.1c's as-enters effects hold no Filter of their own; the ones inside
  -- them are reached as ordinary effect filters, through cardResolutionEffects.
  EntryRewrite.RunEffects _ -> []

-- The Filter a TurnUpRewrite carries. CR 303.4k's destination text -- Gift of
-- Doom's "you may attach it to a creature" -- and NOT framed, even though an
-- attach is what it describes: the enchant-ability narrowing is added by
-- Pawl.Engine.Attach.turnUpHosts because rule 303.4k mandates it, so a card
-- writing Filter.CanHostSubject here would be stating a rule rather than its own
-- text. That is why this position is tagged like any other rather than as an
-- attach destination.
turnUpRewriteFilters :: TurnUpRewrite.TurnUpRewrite -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
turnUpRewriteFilters turnUpRewrite = case turnUpRewrite of
  -- entryRewriteFilters' WithCounters arm, on the rewrite that shares the payload.
  -- Vacuous over `data/cards/` while Bubble Smuggler's four +1/+1 counters are the
  -- pool's only authored turn-up rewrite of this shape -- a plain kind and a
  -- literal count, so neither half carries a Filter -- and walked anyway so the
  -- two halves of one payload cannot be swept differently.
  TurnUpRewrite.WithCounters w -> withCountersFilters w
  TurnUpRewrite.MayAttachTo f -> unframed [f]

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
replacementEffectFilters :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
replacementEffectFilters replacementEffect = case replacementEffect of
  -- BOTH of the pattern's Filter-bearing fields: the permanents it watches, and
  -- CR 122.1b's kind, which may be a whole Keyword carrying a Filter; see #2728.
  -- `whichKind` is a Maybe, and Nothing there is Doubling Season's ANY kind
  -- rather than a kind holding nothing.
  ReplacementEffect.CounterR (CounterR.MkCounterR counterPattern _) ->
    unframed [CounterPattern.onWhat counterPattern] <> concatMap counterKindFilters (Maybe.maybeToList (CounterPattern.whichKind counterPattern))
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR zoneChangePattern _ _ _) -> unframed [ZoneChangePattern.whatObject zoneChangePattern]
  ReplacementEffect.EntryR (EntryR.MkEntryR entryPattern entryRewrite) -> unframed [entryPattern] <> entryRewriteFilters entryRewrite
  -- CR 615.1's shields narrow by their source, which is a Filter over the object
  -- dealing the damage (Luminesce's "black sources and red sources", Galvanic
  -- Blast's `IsSource`), and by their printed RECIPIENT, which is a second
  -- Filter over the object being dealt to (Stormwild Capridor's `IsSource`). The
  -- kind and the baked recipient beside them are not Filters.
  --
  -- The REWRITE holds one too, on a third axis: CR 614.9's printed destination
  -- (Pariah's "enchanted creature"), which damageRewriteFilters below answers.
  ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern rewrite _) ->
    unframed (DamagePattern.whatSource damagePattern : Maybe.maybeToList (DamagePattern.whatRecipient damagePattern) <> damageRewriteFilters rewrite)
  ReplacementEffect.DestructionR _ -> []
  -- CR 111.1: what the token being created is (Queen Allenal of Ruadach's
  -- "creature tokens"). The appended token's own Filters are the MINTED
  -- object's, and reach the sweep through replacementMintedCards instead.
  ReplacementEffect.TokenR (TokenR.MkTokenR tokenPattern _ _) -> unframed [TokenPattern.whatToken tokenPattern]
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR turnUpPattern _ turnUpRewrite) -> unframed [turnUpPattern] <> turnUpRewriteFilters turnUpRewrite
  ReplacementEffect.UntapR _ -> []
  ReplacementEffect.LifeLossR {} -> []
  ReplacementEffect.LifeGainR {} -> []
  -- The pattern is one ControllerRelation; the wish filter a FromOutsideTheGame
  -- rewrite carries is the one Filter a draw row can hold (Ring of Maʼrûf), and
  -- it is CR 400.11c's, so it takes the same framing Effect.FromOutsideTheGame's
  -- does.
  ReplacementEffect.DrawR (DrawR.MkDrawR _ (DrawRewrite.FromOutsideTheGame payload)) -> outsideTheGameFramed [FromOutsideTheGame.filter payload]
  ReplacementEffect.DrawR (DrawR.MkDrawR _ (DrawRewrite.GainLife _)) -> []
  ReplacementEffect.DrawCountR {} -> []
  ReplacementEffect.PhaseR _ -> []

-- CR 614.9's printed destination, the one Filter a damage REWRITE carries.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: a second
-- rewrite that describes something must be classified here rather than have its
-- Filter go unlinted.
damageRewriteFilters :: DamageRewrite.DamageRewrite -> [Filter.Type.Filter Keyword.Keyword]
damageRewriteFilters rewrite = case rewrite of
  DamageRewrite.RedirectMatching f -> [f]
  DamageRewrite.Redirect _ -> []
  DamageRewrite.RedirectNext _ _ -> []
  DamageRewrite.PreventAll -> []
  DamageRewrite.PreventRemovingShieldCounter -> []
  DamageRewrite.PreventNext _ -> []
  DamageRewrite.PreventAllBut _ -> []
  DamageRewrite.SetAmount _ -> []
  DamageRewrite.Scale _ -> []

-- A face's printed replacement ability reaches a Filter on a second axis beside
-- the rewrite's: CR 604.2's "as long as" clause counts objects, exactly as the
-- clause on Effect.Replace does (effectFilters' Replace arm) and as a static
-- ability's does (staticAbilityFilters).
--
-- The two axes are framed DIFFERENTLY, which is why this returns tagged pairs
-- where its neighbours return bare Filters: the row's own Filters are read
-- through Pawl.Engine.Replacement.candidateContext, which supplies the source's
-- host, and the CR 604.2 clause beside them through
-- Pawl.Engine.Projection.replacementsOf, whose bare Filter.contextFor does not.
printedReplacementFilters :: PrintedReplacement.PrintedReplacement Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
printedReplacementFilters printedReplacement =
  frame Unframed (foldMap conditionFilters (PrintedReplacement.condition printedReplacement))
    <> frame ReplacementRowFramed (replacementEffectFilters (PrintedReplacement.effect printedReplacement))
    -- CR 614.1a's appended token is a whole card, Create's recursion.
    <> concatMap (overFaces cardFilters) (replacementMintedCards (PrintedReplacement.effect printedReplacement))

-- Both the subject and CR 508.1c's "unless some condition is met": Blind-Spot
-- Giant's gate carries `Not IsSource`, which is as much card data as the affected
-- set beside it.
--
-- The SIZE-BOUNDING arms have no subject, so they contribute only their gate.
-- Nothing stands in for the missing Affected on purpose: a `Matching Anything`
-- there would report a filter Silent Arbiter does not print.
-- BOTH of an attachment prohibition's Filter positions -- CR 303.4's restricted
-- permanents and the attachers barred from them -- the pairing
-- combatRestrictionFilters' CantBeBlockedBy arm takes over its own two.
attachRestrictionFilters :: AttachRestriction.AttachRestriction -> [Filter.Type.Filter Keyword.Keyword]
attachRestrictionFilters restriction =
  affectedFilters (AttachRestriction.affected restriction)
    <> [AttachRestriction.attachers restriction]

-- CR 508.1d's subject, plus the CR 604.2 clause the second reading of that rule
-- rides on (Otarian Juggernaut's threshold), whose Count holds a Filter.
attackRequirementFilters :: AttackRequirement.AttackRequirement -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
attackRequirementFilters requirement =
  unframed (affectedFilters (AttackRequirement.subject requirement))
    <> foldMap conditionFilters (AttackRequirement.while requirement)

combatRestrictionFilters :: CombatRestriction.CombatRestriction -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
combatRestrictionFilters restriction = case restriction of
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless affected condition) -> unframed (affectedFilters affected) <> foldMap conditionFilters condition
  CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless affected condition) -> unframed (affectedFilters affected) <> foldMap conditionFilters condition
  -- Three positions on the PAIRWISE arm: the attackers restricted, the blockers
  -- barred from them, and the gate.
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy affected blockers condition) -> unframed (affectedFilters affected <> [blockers]) <> foldMap conditionFilters condition
  -- Two on the attacking one: the creatures restricted and the gate. The
  -- players they may not attack are a PlayerScope and the announcements barred
  -- at those seats are CR 506.3 kinds, both card data with no Filter in them --
  -- so nothing stands in for either here, the size-bounding arms' posture.
  CombatRestriction.CantAttackPlayer (CantAttackPlayer.MkCantAttackPlayer affected _ _ condition) -> unframed (affectedFilters affected) <> foldMap conditionFilters condition
  CombatRestriction.CantAttackAlone (AffectedUnless.MkAffectedUnless affected condition) -> unframed (affectedFilters affected) <> foldMap conditionFilters condition
  CombatRestriction.CantAttackMoreThan (LimitUnless.MkLimitUnless _ condition) -> foldMap conditionFilters condition
  CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless _ condition) -> foldMap conditionFilters condition

-- BOTH of a blocking requirement's Filter positions -- CR 509.1c's subject axis
-- (Razorgrass Screen) and its object axis (Lure) -- each optional, and an absent
-- one contributing nothing rather than a stand-in filter, combatRestrictionFilters'
-- posture for its size-bounding arms.
blockRequirementFilters :: BlockRequirement.BlockRequirement -> [Filter.Type.Filter Keyword.Keyword]
blockRequirementFilters requirement =
  foldMap affectedFilters (BlockRequirement.subject requirement)
    <> foldMap affectedFilters (BlockRequirement.attacker requirement)

-- All three of a blocking permission's Filter positions: the subject it names, CR
-- 604.2's "as long as" gate beside it (Entourage of Trest), and the counted arity
-- (Kemba's Legion).
blockPermissionFilters :: BlockPermission.BlockPermission -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
blockPermissionFilters permission =
  unframed (affectedFilters (BlockPermission.affected permission))
    <> foldMap quantityFilters (BlockPermission.additional permission)
    <> frame Unframed (foldMap conditionFilters (BlockPermission.while permission))

-- WHICH position of a card's text a Filter sits in, for the lints that are about
-- position rather than about the atom. Each atom they police is answered off a
-- field only certain callers fill -- a Filter.View's or a Filter.Context's,
-- which the constructors below name one by one -- so the position IS the
-- soundness question. Most name ONE position; CR 303.4b's names several, which
-- is why this is a tag on the position rather than a Bool.
--
--   * AttachDestination -- the destination of Effect.AttachTarget or
--     Effect.AttachTargetToEach, the positions evaluated against a view whose
--     `canHostSubject` is filled in (Pawl.Engine.Attach.hostsFor). CR 701.3a's
--     atom belongs here and nowhere else.
--   * InTargetSlot -- a MODE's target slot filter, the one position matched by
--     Pawl.Engine.Target.admittedGiven, which is the one site that fills
--     Filter.Context.slotControllers and one of the two that fill
--     Filter.Context.slotNames. CR 110.2's Filter.SameControllerAsBound belongs
--     here and nowhere else; CR 709.4a's Filter.SameNameAsBound belongs here and
--     in SearchFramed below, the other position slotNames is filled at. Their
--     vacuous directions differ: an unfilled
--     slotNames answers False, an unfilled slotControllers answers True. So a
--     misplaced SameNameAsBound admits nothing and a misplaced
--     SameControllerAsBound admits everything, so this tag carries more for the
--     second than for the first.
--   * SourceHostFramed -- a position whose evaluator fills
--     Filter.Context.sourceAttachedTo, which is five rather than one: a static
--     ability's CR 604.2 clause (Pawl.Engine.Projection.conditionHolds), a
--     triggered ability's CR 603.4 clause (Pawl.Engine.Event.Trigger.interveningHolds and
--     Pawl.Engine.Stack's CR 608.2a re-check), an effect's
--     Pawl.Types.ObjectRef (Pawl.Engine.Resolve.Slots.objectRefObjects), a printed
--     PLAYER ability's own effect (Pawl.Engine.PlayerEffect.matchesObjectFrom,
--     which takes the source off the row `applying` returns), and a CR 614.1
--     replacement ROW's own Filters -- its pattern's two and CR 614.9's printed
--     destination -- which Pawl.Engine.Replacement.candidateContext reads
--     (Pariah's "enchanted creature"). CR 303.4b's Filter.IsHostOfSource belongs
--     in those five and nowhere else.
--
--     A printed replacement's CR 604.2 clause is NOT one of them, though it sits
--     on the same ability: Pawl.Engine.Projection.replacementsOf evaluates it
--     through a bare Filter.contextFor. printedReplacementFilters is where that
--     split is made.
--
--     The fifth of those positions carries its OWN constructor below rather than
--     this one, for a reason that is about the lints and not about the rule:
--     `hostFramed` treats the two alike, and only the ObjectRef twin lint tells
--     them apart.
--   * KeywordFramed -- a KEYWORD's own Filter, wherever the keyword is written.
--     One of the two tags applied by the leaf that PRODUCES the Filter rather
--     than by the position that quotes it, which `frame` below is what keeps.
--   * MintedTargetSlot -- the other, and the one keyword payload that is a
--     TARGET SLOT filter: CR 702.6c's equip quality. Answered by
--     Pawl.Engine.Target.admittedGiven like InTargetSlot, with the bindings and
--     the chosen player of SlotlessCostFramed, which is to say none.
--   * MillTallyFramed -- CR 701.17's mill tally filter, the second position a
--     card may write CR 201.4's Filter.HasChosenName in (Predict): the
--     Effect.Mill arm overlays Filter.Context.sourceChosenNames onto the
--     resolution's own context, as the search arm does. Not SearchFramed, whose
--     other promise -- a view filling Filter.View.canAttachToSubject -- it does
--     not keep.
--   * Unframed -- everything else.
--
-- CR 303.4a's enchant slot (Face.enchant) is Unframed rather than InTargetSlot,
-- and that is the load-bearing call: it is a TargetSlot, but two of its readers
-- reach Target.admittedRecipients, which passes NO bindings, so slotNames and
-- slotControllers are both empty there -- Pawl.Engine.Attach.attachmentFor's CR
-- 303.4j move and Pawl.Engine.Sba.stillLegalEnchant's CR 303.4c check. A
-- SameNameAsBound written into an enchant ability would answer one way at CR
-- 601.2c and another way at every later reading; a SameControllerAsBound would do
-- the same in the widening direction, admitting every host at those later
-- readings, so this tag is what refuses the atom rather than merely narrowing it.
-- Modification.GainEnchant's granted slot lands Unframed the same way, through
-- modificationFilters below.
data Framing
  = Unframed
  | AttachDestination
  | InTargetSlot
  | SourceHostFramed
  | -- | CR 701.23's search filter, the one position whose evaluator supplies the
    -- object a CR 701.3a question can be asked ABOUT from the candidate's side:
    -- Pawl.Engine.Resolve's Effect.Search arm overlays
    -- Filter.View.canAttachToSubject with the searching ability's own source.
    --
    -- It is also the second position that fills Filter.Context.slotNames: that
    -- arm takes its context from the resolution (Resolve.effectContext), so CR
    -- 709.4a's Filter.SameNameAsBound answers here as it does in a target slot
    -- (Bifurcate).
    --
    -- CR 702.29e's typecycling filter is deliberately NOT one, though
    -- Pawl.Engine.Keyword turns it into an Effect.Search that would answer: the
    -- card writes it as a keyword payload, so it is tagged where cardFilters
    -- reaches it. The mistagging can only REJECT a legal card, never admit an
    -- unanswerable one, and no printing writes the atom there.
    SearchFramed
  | -- | A CR 614.1 replacement ROW's own Filters -- its DamagePattern's source
    -- and printed-recipient halves, CR 614.9's printed destination, an entry
    -- pattern, and every other Filter a ReplacementEffect holds. Every one of
    -- them is read through Pawl.Engine.Replacement.candidateContext, so the
    -- source's host IS supplied and `hostFramed` below admits this exactly as it
    -- admits SourceHostFramed.
    --
    -- A constructor of its own only because effectFilters' SourceHostFramed
    -- positions are compared, tag for tag, against effectObjectRefs' by the twin
    -- lint below -- SourceHostFramed inside an Effect means "an ObjectRef's
    -- filter" to that lint, and a stored Effect.Replace's row would otherwise
    -- appear on one side of the comparison and not the other.
    ReplacementRowFramed
  | -- | CR 400.11c's wish filter -- Effect.FromOutsideTheGame's, the one
    -- card-authored position whose candidates are never objects in the game:
    -- Pawl.Engine.Event.eligible matches it against a printed FACE
    -- (Pawl.Engine.Projection.View.viewOfCard). Marked not because its evaluator
    -- FILLS a field the others leave empty but because its candidate view leaves
    -- one empty that every other position fills -- `identity` -- so
    -- Filter.IsBound is a silent False there and nowhere else.
    OutsideTheGameFramed
  | -- | A KEYWORD's own Filter -- CR 702.29e's typecycling predicate, CR 702.11d's
    -- "hexproof from", a cost-carrying keyword's CostComponent.Sacrifice, and CR
    -- 122.1b's keyword counter, which carries a whole Keyword. Read off a
    -- continuous effect or off the printed face, and never in a context carrying
    -- the resolution's SLOTS -- sweptForSingularSlots below is where that is
    -- argued. `hostFramed` below is the conservative half of the same posture:
    -- the CR 702.16e route does reach a context filling the source's host, and
    -- refusing the atom anyway only narrows what a card may write.
    --
    -- The one constructor applied at the LEAF that produces the Filter rather
    -- than at the position that quotes it, which is what `frame` below exists
    -- for: a keyword counter's Filter is reached through counterKindFilters from
    -- positions that are themselves framed, and the quoting position's promise is
    -- not the keyword's.
    KeywordFramed
  | -- | A COST paid with NO ANNOUNCEMENT behind it, so Pawl.Engine.Cost.pay
    -- hands its pools no slots (Cost.announcedSlots is empty for it) and
    -- Filter.IsBound there is a silent False -- exactly as it is under
    -- OutsideTheGameFramed, and for the mirror-image reason: the candidate is
    -- an object all right, but no slot of any announcement names it.
    --
    -- The positions: CR 118.12's gate cost, paid by
    -- Pawl.Engine.Resolve.payGatePaidBy; CR 116.2d's ignore cost, paid by
    -- Pawl.Engine.Ignore; CR 116.2c's UntilPaid price, paid by
    -- Pawl.Engine.EndEffect; and CR 508.1h's and CR 509.1d's per-creature
    -- combat tolls, paid by Pawl.Engine.Cost.payTagged. Each is unanswerable by
    -- the RULE and not merely by this engine: a special action uses no stack
    -- (CR 116.1), a declaration announces no target, and a gate is paid as
    -- something resolves rather than as it is announced. Card text reaches the
    -- first -- Lithophage's "unless you sacrifice a Mountain" -- so all are
    -- swept rather than dropped. The gate was tagged first, see #2881; the
    -- ignore cost followed, see #2883; the toll and the price last, see #2927.
    --
    -- NOT the costs an announcement pays -- CR 601.2f's additional cost, CR
    -- 118.9's alternative, an activated ability's own, and CR 613.11's two
    -- added lists -- which are Unframed: CR 601.2c chooses the targets before
    -- CR 601.2h pays, Pawl.Engine.Cast and Pawl.Engine.Activate stamp them on
    -- the stack object first, and Cost.announcedSlots reads them there. The two
    -- cast positions were tagged here between #2883 and #2924, when no cost saw
    -- a slot. Synthetic Spiteful Rite and Synthetic Spiteful Altar are the cards
    -- (Pawl.CostSpec's "Synthetic Spiteful Rite" group).
    SlotlessCostFramed
  | -- | A keyword PAYLOAD Filter that rule 702 transplants into the target slot
    -- of an ability it mints -- CR 702.6c's equip quality, the only one, which
    -- Pawl.Engine.Keyword.equip conjoins with rule 702.6a's "you control".
    --
    -- Neither of its neighbours. Not KeywordFramed, whose whole argument is that
    -- no evaluator of a keyword's payload supplies a slot-carrying Context: this
    -- one is answered by Pawl.Engine.Target.admittedGiven like any target slot.
    -- Not InTargetSlot either, because the minted ability has ONE slot and no
    -- announcement behind it, so the bindings that position's atoms compare
    -- against are empty -- Filter.Context.slotNames, slotControllers and
    -- carrierChosenPlayer are all unfilled, exactly as they are for a
    -- SlotlessCostFramed cost.
    --
    -- So every atom lint treats it as an offence and the sweep still reaches it,
    -- which is the conservative direction on both axes: a card writing CR
    -- 702.16k's chosen player, CR 709.4a's bound name, CR 110.2's bound
    -- controller or CR 115.10a's bound object into an equip quality is rejected
    -- rather than silently vacuous.
    MintedTargetSlot
  | -- | CR 701.17's mill TALLY filter -- the one position whose evaluator counts
    -- CARDS a resolution has just moved rather than candidates it is choosing
    -- among, and the second position a CARD may write CR 201.4's chosen name in:
    -- Pawl.Engine.Resolve.Effect's Effect.Mill arm builds its context through
    -- Resolve.Slots.effectContext and overlays Filter.Context.sourceChosenNames,
    -- exactly as the Effect.Search arm does (Predict, see #2141).
    --
    -- Not SearchFramed, whose other promise this position does not keep:
    -- Filter.View.canAttachToSubject is unfilled here, so CR 701.3a's atom would
    -- be a silent False. Not Unframed either, which is what it was until the
    -- chosen name became answerable.
    MillTallyFramed
  -- Bounded and Enum so the framing coverage case below enumerates
  -- [minBound .. maxBound] rather than a hand-kept list: a constructor added
  -- here joins that case with no edit, which is the tripwire a hand-kept list
  -- would not be (#2741).
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Is this position's Filter swept for a slot read singly (filterSlotsReadSingly
-- above)? Every framing but a keyword's own, which is not: no evaluator of a
-- keyword's payload Filter supplies SLOT OBJECTS, so Filter.IsControllerOfBound
-- reads nothing at any of them. That is the test to put a new evaluator to,
-- rather than a list of today's: the question is what the EVALUATING context
-- holds, not how many builders there are -- Pawl.Engine.Filter.contextWithSlots
-- has callers across the engine and Pawl.Engine.Target.slotContext writes a
-- slot-carrying Context out by hand. None reaches a keyword's payload: CR
-- 702.11d and CR 702.16b (Pawl.Engine.Target.targetable), CR 702.14c
-- (Pawl.Engine.Combat), CR 702.16c/d (Pawl.Engine.AttachRestriction), CR
-- 702.16f (Pawl.Engine.CombatRestriction) and the CR 702.29e cycling mint's
-- Search filter all evaluate in a context built with no slots, a keyword cost's
-- criterion (Pawl.Engine.Cost) in one whose slots are an announcement's targets
-- and never the keyword's own, and the CR 702.16e quality Pawl.Engine.Keyword transplants into
-- a DamageR's source half goes through candidateContext on the PERMANENT segment,
-- whose ReplacementCandidate.slots is empty because no resolution installed a
-- printed static ability. Target.slotContext's map is answered only by
-- admittedGiven and lastKnownAdmits, which match a target slot's own Filter --
-- Unframed, InTargetSlot, or, for the one payload rule 702 transplants into a
-- slot (CR 702.6c's equip quality), MintedTargetSlot: never KeywordFramed, which
-- is why that quality carries a framing of its own rather than this one.
-- Filter.HasKeyword is not even a read: it asks Set membership of the whole
-- keyword.
--
-- The lint is conservative everywhere else, reporting the atom wherever it sits
-- rather than only where it is answered, so this is the one exemption and it is
-- stated once. BOTH routes to the sweep now take it: the nested pairs
-- filterSlotsReadSingly's own non-descent into Filter.HasKeyword and
-- Filter.HasCounters already excluded, and the top-level KeywordFramed pairs
-- counterKindFilters hands out, which were swept anyway (#2741).
sweptForSingularSlots :: Framing -> Bool
sweptForSingularSlots framing = case framing of
  KeywordFramed -> False
  Unframed -> True
  AttachDestination -> True
  InTargetSlot -> True
  SourceHostFramed -> True
  SearchFramed -> True
  OutsideTheGameFramed -> True
  ReplacementRowFramed -> True
  -- SWEPT, though no slot is readable here at all: what this walk reports is a
  -- BATCH slot read singly, and the atoms it reads are not only Filter.IsBound,
  -- which isBoundCounts rejects outright at this position. Keeping the sweep is
  -- what Unframed did before #2881, and it can only reject more.
  SlotlessCostFramed -> True
  -- SWEPT for the same reason, and here the slot map really is a target slot's
  -- -- it is only the BINDINGS in it that are empty, so a batch slot read singly
  -- is as reportable as at InTargetSlot.
  MintedTargetSlot -> True
  -- SWEPT, and here the slots are a resolution's own: effectContext fills them,
  -- so a batch slot read singly is as reportable as at Unframed, which is the
  -- framing this position carried before.
  MillTallyFramed -> True

-- filterSlotsReadSingly against a TAGGED position, and the one funnel every
-- reader of that walk goes through, so two routes to the same keyword filter
-- cannot answer differently.
framedSlotsReadSingly :: (Framing, Filter.Type.Filter Keyword.Keyword) -> [SlotName.SlotName]
framedSlotsReadSingly (framing, predicate)
  | sweptForSingularSlots framing = filterSlotsReadSingly predicate
  | otherwise = []

-- Tag a Filter position as UNFRAMED -- one none of the framings above applies
-- to, which is every position in the type except the ones they name.
unframed :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
unframed = fmap ((,) Unframed)

-- Tag a Filter position as one whose evaluator supplies the SOURCE's host -- the
-- first four listed on Framing above, the fifth carrying ReplacementRowFramed
-- instead for the reason that constructor gives. An ObjectRef's own Filter is
-- always one, whatever
-- effect carries it, because Pawl.Engine.Resolve.Slots.objectRefObjects is the single
-- site that turns an ObjectRef into objects.
sourceHosted :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
sourceHosted = fmap ((,) SourceHostFramed)

-- Tag a Filter position as a SEARCH's, the one position whose evaluator supplies
-- Filter.View.canAttachToSubject (CR 701.3a from the candidate's side).
searchFramed :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
searchFramed = fmap ((,) SearchFramed)

-- Tag a Filter position as a WISH's, the one position matched against a printed
-- face rather than against an object (CR 400.11c).
outsideTheGameFramed :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
outsideTheGameFramed = fmap ((,) OutsideTheGameFramed)

-- Tag a Filter position as a COST paid with no announcement behind it -- the
-- positions SlotlessCostFramed names.
slotlessCost :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
slotlessCost = fmap ((,) SlotlessCostFramed)

-- Tag a Filter position as a KEYWORD's own. One of the two tags applied at the
-- leaf that produces the Filter, keywordFilters, rather than at a quoting
-- position.
keywordFramed :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
keywordFramed = fmap ((,) KeywordFramed)

-- Tag a Filter position as a keyword payload rule 702 carries into a MINTED
-- ability's target slot -- CR 702.6c's equip quality, the other tag keywordFilters
-- applies at the leaf.
mintedTargetSlot :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
mintedTargetSlot = fmap ((,) MintedTargetSlot)

-- Tag a Filter position as a mill TALLY's, the one position whose candidates are
-- cards a resolution has just milled (CR 701.17).
millTallyFramed :: [Filter.Type.Filter Keyword.Keyword] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
millTallyFramed = fmap ((,) MillTallyFramed)

-- Apply a quoting position's Framing to filters that are ALREADY tagged, filling
-- in only the ones still Unframed. The lifts above take a bare list, which is
-- the shape of a traversal that cannot reach a keyword; the traversals that can
-- reach one hand back tagged pairs, and a deeper tag wins because the deeper
-- position is the one whose evaluator actually reads the Filter.
--
-- KeywordFramed and MintedTargetSlot are the only tags any of those traversals
-- produces, both from keywordFilters, so "a deeper tag wins" has two cases
-- today; it is written as the general rule because the alternative -- letting
-- the quoting position overwrite -- is the narrowing this replaced, see #2730
-- and #2733.
frame :: Framing -> [(Framing, Filter.Type.Filter Keyword.Keyword)] -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
frame framing = fmap (\(inner, f) -> (if inner == Unframed then framing else inner, f))

-- Both predicates a CR 106.6 restriction can carry. Its own type has two fields
-- and no traversal, so a lint that reached only one of them would go quiet on
-- the other -- which is why this is a function and not an inlined selector.
restrictionFilters :: ManaRestriction.ManaRestriction -> [Filter.Type.Filter Keyword.Keyword]
restrictionFilters restriction =
  Maybe.maybeToList (ManaRestriction.casts restriction)
    <> Maybe.maybeToList (ManaRestriction.activations restriction)

-- The predicate a CR 106.6 rider carries. A function beside restrictionFilters
-- for the same reason: the two clauses ride Pawl.Types.ManaAddition
-- independently, so a lint that reached one would go quiet on the other.
-- Prefixed, because `riderFilters` above is Pawl.Types.EntryRiders -- what a
-- permanent enters WITH -- and the two have nothing to do with each other.
--
-- No lint in this module distinguishes a collected rider condition from an
-- uncollected one today: neutralising this function leaves the suite green, so
-- it is here for the next Filter-wide lint rather than for one that exists.
manaRiderFilters :: ManaRider.ManaRider -> [Filter.Type.Filter Keyword.Keyword]
manaRiderFilters rider = [ManaRider.condition rider]

-- Every Filter one effect carries, paired with its Framing. Two arms answer
-- AttachDestination -- Effect.AttachTarget's destination and
-- Effect.AttachTargetToEach's, which are the only CARD-AUTHORED Filter positions
-- evaluated against a view whose `canHostSubject` is filled in
-- (Pawl.Engine.Attach.hostsFor, from attachmentFor). TurnUpRewrite.MayAttachTo reaches the same evaluator and is
-- still unframed, deliberately: CR 303.4k's enchant-ability conjunct is added by
-- Attach.turnUpHosts because the rule mandates it, so the atom appearing in that
-- card's data would be a card restating a rule. Everywhere else the field is
-- False by construction --
-- Projection.viewOfCard, Projection.viewOfCharacteristics, Filter.playerView and
-- Count's event snapshot all set it so -- because outside an attach there is no
-- subject for CR 701.3a to be about. The MIRROR question, whose fixed object is
-- the host rather than the moving permanent, is Filter.CanAttachToSubject, and
-- SearchFramed marks the one position that answers it.
effectFilters :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
effectFilters effect = case effect of
  -- THE one attach-framed position. CR 701.3a: "An Aura, Equipment, or
  -- Fortification can't be attached to an object or player it couldn't enchant,
  -- equip, or fortify, respectively." Aura Graft's "another permanent it can
  -- enchant".
  Effect.AttachTarget (AttachTarget.MkAttachTarget _ f) -> [(AttachDestination, f)]
  -- Rule 701.3a frames this opcode's destination filter exactly as it frames
  -- AttachTarget's; CR 303.4d only moves whose choice it is.
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget _ f) -> [(AttachDestination, f)]
  -- The dealer is a SlotName and carries no Filter.
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> foldMap (\part -> frame SourceHostFramed (objectRefFilters (DamagePart.ref part)) <> frame Unframed (quantityFilters (DamagePart.quantity part))) parts
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    frame Unframed (durationFilters duration) <> frame Unframed (modificationFilters modification) <> frame SourceHostFramed (objectRefFilters ref)
  Effect.ChangeText {} -> []
  -- CR 106.6's two clauses, and every predicate in them: the restriction's cast
  -- half, its activation half, and the rider's condition are each an ordinary
  -- Filter, and collecting one of them would take every lint in this module off
  -- the others. UNFRAMED: each is evaluated against the object being paid for
  -- (Pawl.Engine.Mana.admitsUnder for the restriction,
  -- Pawl.Engine.ManaRider.uncounterable for the rider), which is neither an
  -- attach destination nor a target slot.
  Effect.AddMana addition ->
    unframed
      ( concatMap restrictionFilters (Maybe.maybeToList (ManaAddition.restriction addition))
          <> concatMap manaRiderFilters (Maybe.maybeToList (ManaAddition.rider addition))
      )
  -- THE one search-framed position. CR 701.3a from the candidate's side:
  -- Auratouched Mage's "an Aura card that could enchant it", where the host is
  -- fixed for the whole evaluation and the Aura varies per candidate.
  Effect.Search (Search.MkSearch _ _ _ _ f _ _) -> searchFramed [f]
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  -- CR 201.4a's restriction, UNFRAMED: it is handed to Prompt.ChooseCardName for
  -- an answerer to obey rather than matched against a candidate at all, so none of
  -- the framings' evaluators is the one that reads it.
  Effect.ChooseCardName restriction -> unframed [restriction]
  -- THE one wish-framed position. CR 400.11c's filter is matched against a
  -- PRINTED FACE (Pawl.Engine.Projection.View.viewOfCard) rather than against an
  -- object any of the other framings' evaluators project, which is what makes
  -- Filter.IsBound a silent False in it.
  Effect.FromOutsideTheGame payload -> outsideTheGameFramed [FromOutsideTheGame.filter payload]
  Effect.ExileThisSpell -> []
  -- Only the count's Filters: rule 701.39a describes the candidate pool, so no
  -- Filter on the card names it.
  Effect.Bolster quantity -> frame Unframed (quantityFilters quantity)
  -- Only the count's Filters: rule 701.47a describes the candidate pool, so no
  -- Filter on the card names it.
  Effect.Amass (Amass.MkAmass quantity _) -> frame Unframed (quantityFilters quantity)
  -- Only the count's Filters: rule 701.68a describes the candidate pool, so no
  -- Filter on the card names it, and a PlayerRef carries none either -- Draw's
  -- arm below answers the same way.
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ f quantity) -> unframed [f] <> frame Unframed (quantityFilters quantity)
  -- CR 727.5's exemption is an ObjectRef like every other, and Karn Liberated's
  -- "all non-Aura permanent cards exiled with Karn" states characteristics in it
  -- -- so an empty list here took a Filter a card author writes out of the lint.
  Effect.RestartGame mRef -> frame SourceHostFramed (concatMap objectRefFilters (Maybe.maybeToList mRef))
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy (Destroy.MkDestroy ref _ _ _ _) -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect ref _) -> frame SourceHostFramed (objectRefFilters ref)
  -- The riders reach a Filter by TWO roads one level further down than the
  -- ObjectRef: CR 122.6's counters are keyed by CounterKind, and CR 122.1b's
  -- keyword counter carries a whole Keyword; and each count is a Quantity, which
  -- may carry a Count whose Filter is card text. KEYS AND ELEMS both, therefore
  -- -- a keys-only sweep would leave the counts unlinted. Swept for the reason
  -- canHostSubjects sweeps the same shape -- the lint is about the positions a
  -- card author can write, not about which of them the pool has used.
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ riders _ _ _ _) -> frame SourceHostFramed (objectRefFilters ref) <> frame Unframed (riderFilters riders)
  Effect.Draw (Draw.MkDraw _ quantity _) -> frame Unframed (quantityFilters quantity)
  -- The tally's Filter is a position a card author writes, so the lint reaches
  -- it: rule 728.1's "nonland card" is one of these, and Predict's chosen name
  -- another -- which is why the tally carries a framing of its own.
  Effect.Mill (Mill.MkMill _ quantity mTally _) -> frame Unframed (quantityFilters quantity) <> millTallyFramed (fmap MillTally.filter (Maybe.maybeToList mTally))
  -- The ObjectRef's Filter is a position a card author writes, so the lint
  -- reaches it, as Explore's does. Both halves of CR 701.20 answer alike.
  Effect.Reveal (Reveal.MkReveal ref _) -> frame SourceHostFramed (objectRefFilters ref)
  Effect.LookAt (LookAt.MkLookAt ref _) -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  -- The ObjectRef's Filter is a position a card author writes, so the lint
  -- reaches it, as PutCounters' does.
  Effect.Explore ref -> frame SourceHostFramed (objectRefFilters ref)
  -- The These arm's ref carries a Filter a card author writes -- Amnesia's
  -- "nonland" -- so the lint reaches it, as Reveal's does.
  Effect.Discard subject -> case subject of
    Discard.Counted (CountedDiscard.MkCountedDiscard _ quantity _) -> frame Unframed (quantityFilters quantity)
    Discard.These ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.DecreaseSpeed d -> frame Unframed (quantityFilters (SpeedDecrease.quantity d))
  -- CR 111.1's token is a whole card, and every Filter position it has is one a
  -- card author can write -- the same nesting Pawl.Codec's round trip walks.
  Effect.Create (Create.MkCreate quantity card riders _ _) -> frame Unframed (quantityFilters quantity <> riderFilters riders) <> overFaces cardFilters card
  Effect.Conjure (Conjure.MkConjure quantity card _) -> frame Unframed (quantityFilters quantity) <> overFaces cardFilters card
  -- An EachMatching ref's Filter is card text like RequireBlock's below, and the
  -- count's and the riders' Filters are as much card text as Create's.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref riders) -> frame Unframed (quantityFilters quantity <> riderFilters riders) <> frame SourceHostFramed (objectRefFilters ref)
  -- BOTH refs, RequireBlock's arm below: each EachMatching Filter is card text.
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject) -> frame SourceHostFramed (objectRefFilters original <> objectRefFilters subject)
  -- BOTH refs, CreateCopy's arm above: an EachMatching Filter is card text, and
  -- CR 707.10d's candidates are named by one.
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets) -> frame SourceHostFramed (objectRefFilters ref <> copyTargetsFilters targets)
  -- The ROW's own Filters are framed for printedReplacementFilters' reason: a
  -- stored row is read through the same
  -- Pawl.Engine.Replacement.candidateContext a printed one is, where the
  -- duration and the CR 604.2 clause beside it are not.
  Effect.Replace (Replace.MkReplace duration _ _ condition replacement) -> frame Unframed (durationFilters duration <> foldMap conditionFilters condition) <> frame ReplacementRowFramed (replacementEffectFilters replacement) <> concatMap effectFilters (replacementEffectRiders replacement) <> concatMap (overFaces cardFilters) (replacementMintedCards replacement)
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  -- The rider's Filters too, for CR 615.5. This is the traversal that dropped
  -- landwalk's payload once, so a nested effect list is exactly what it must not
  -- stop at.
  --
  -- CR 609.7a's chosen source is UNFRAMED: Pawl.Engine.Resolve evaluates it
  -- against each candidate's own view (Projection.viewOfObject), the way
  -- Replacement.matchesDamageSource evaluates the shield's rechecked half.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ ref whatRecipient _ chosenSource quantity rider) ->
    frame Unframed (durationFilters duration) <> frame Unframed (quantityFilters quantity) <> unframed (Maybe.maybeToList chosenSource <> Maybe.maybeToList whatRecipient) <> frame SourceHostFramed (foldMap objectRefFilters ref) <> concatMap effectFilters rider
  -- CR 609.7b's property-named source is UNFRAMED for the chosen source's
  -- reason: Pawl.Engine.Replacement.matchesDamageSource evaluates it against the
  -- damage's own source, not against this card's frame.
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ ref whatRecipient _ chosenSource whatSource rider) ->
    frame Unframed (durationFilters duration) <> unframed (Maybe.maybeToList chosenSource <> Maybe.maybeToList whatRecipient <> [whatSource]) <> frame SourceHostFramed (foldMap objectRefFilters ref) <> concatMap effectFilters rider
  -- BOTH refs, or a Filter inside a redirect's destination escapes this lint.
  -- CR 609.7a's chosen source and the recipient description are UNFRAMED, for
  -- the reason the two prevention arms above give.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ amount srcRef whatRecipient _ destRef chosenSource) ->
    frame Unframed (durationFilters duration) <> frame Unframed (foldMap quantityFilters amount) <> unframed (Maybe.maybeToList chosenSource <> Maybe.maybeToList whatRecipient) <> frame SourceHostFramed (foldMap objectRefFilters srcRef <> objectRefFilters destRef)
  -- CR 708.2's listed characteristics hold no Filter, but the ref does -- Ixidron's
  -- "all other nontoken creatures" is an ObjectRef Filter like Destroy's, so the
  -- lint reaches it.
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown ref _) -> frame SourceHostFramed (objectRefFilters ref)
  Effect.TurnFaceUp _ -> []
  Effect.Fight _ -> []
  Effect.RemoveFromCombat ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.BecomesBlocked _ -> []
  -- Swift Silence's "all other spells" is an ObjectRef Filter like Destroy's,
  -- so the lint reaches it.
  Effect.Counter (Counter.MkCounter ref _ _) -> frame SourceHostFramed (objectRefFilters ref)
  -- All THREE positions: the ObjectRef carries Renegade Krasis' "each other
  -- creature you control with a +1/+1 counter on it", and a Filter there would
  -- otherwise escape the lint; the count is a Quantity like any other; and CR
  -- 122.1b's kind may be a whole Keyword with a Filter under it; see #2728.
  Effect.PutCounters (PutCounters.MkPutCounters kind quantity ref) -> frame Unframed (counterKindFilters kind <> quantityFilters quantity) <> frame SourceHostFramed (objectRefFilters ref)
  -- The destination and CR 122.1b's kind, PutCounters' framing: `from` is a bare
  -- SlotName and carries no Filter, and the kind is the one rule 122.8's second
  -- sentence lets a card name, which may be a whole Keyword with a Filter under
  -- it; see #2728.
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom _ kind ref) -> frame Unframed (foldMap counterKindFilters kind) <> frame SourceHostFramed (objectRefFilters ref)
  -- FOUR positions, PutCounters' framing: BOTH sides are ObjectRefs and carry a
  -- Filter apiece -- Spike Cannibal's "all creatures" on the first, Forgotten
  -- Ancient's "other creatures" on the second. The moved kinds hold the other
  -- two -- a count under the two arms that write one (MovedKinds.quantityOf) and
  -- CR 122.1b's kind under the three that name one (MovedKinds.kindOf); see
  -- #2728.
  Effect.MoveCounters (MoveCounters.MkMoveCounters from kinds _ to) -> frame Unframed (foldMap counterKindFilters (MovedKinds.kindOf kinds) <> foldMap quantityFilters (MovedKinds.quantityOf kinds)) <> frame SourceHostFramed (objectRefFilters from <> objectRefFilters to)
  -- The count and CR 122.1b's kind, PutCounters' two unframed positions: the
  -- slot beside them is a bare SlotName and carries no Filter.
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters kind quantity _) -> frame Unframed (counterKindFilters kind <> quantityFilters quantity)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> frame Unframed (quantityFilters quantity)
  Effect.PayAnyEnergy _ -> []
  Effect.Tap ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Untap ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Detain ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Goad ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.MakePlotted ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.DoesNotUntapNext ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Transform ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Convert ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.Meld (Meld.MkMeld ref card) -> frame SourceHostFramed (objectRefFilters ref) <> overFaces cardFilters card
  Effect.PhaseOut ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.AddPhases _ -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef duration ref) -> frame Unframed (durationFilters duration) <> frame SourceHostFramed (objectRefFilters ref)
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger _ _ mDuration) -> frame Unframed (concatMap durationFilters (Maybe.maybeToList mDuration))
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ playerEffect) -> frame Unframed (durationFilters duration) <> unframed (playerEffectFilters playerEffect)
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration blocker attacker) -> frame Unframed (durationFilters duration) <> frame SourceHostFramed (objectRefFilters blocker <> objectRefFilters attacker)
  -- RequireBlock's arm one axis narrower.
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration ref) -> frame Unframed (durationFilters duration) <> frame SourceHostFramed (objectRefFilters ref)
  -- CantBeRegenerated's arm, the same one axis.
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock duration ref) -> frame Unframed (durationFilters duration) <> frame SourceHostFramed (objectRefFilters ref)
  -- The Named arm is CantBeRegenerated's ref; the Matching arm's class is read
  -- through a bare Filter.contextFor at
  -- Pawl.Engine.CombatRestriction.storedSubjects, so it is Unframed. The AimedAt
  -- is a PlayerScope and CR 506.3 kinds, no Filter in either.
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration affected _) ->
    frame Unframed (durationFilters duration) <> case affected of
      RestrictedCreatures.Named ref -> frame SourceHostFramed (objectRefFilters ref)
      RestrictedCreatures.Matching f -> unframed [f]
  -- RequireBlock's arm one axis over. The PlayerRef carries no Filter.
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration attacker _) -> frame Unframed (durationFilters duration) <> frame SourceHostFramed (objectRefFilters attacker)
  -- CR 114.2's emblem is a whole card too.
  Effect.CreateEmblem card -> overFaces cardFilters card
  Effect.BecomeMonarch _ -> []
  Effect.TakeTheInitiative _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> []
  Effect.Unsuspect ref -> frame SourceHostFramed (objectRefFilters ref)
  Effect.SetHalfLocked {} -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.Train _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  -- CR 701.3's other attach, which moves the SOURCE rather than a target and
  -- carries no destination filter at all.
  Effect.Attach _ -> []
  Effect.AttachBound {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChoosePlayer _ -> []
  Effect.ChooseOpponentAtRandom _ -> []
  -- CR 706.2's modifier is a Quantity, so its filters are reachable here.
  Effect.RollDie rollDie -> frame Unframed (quantityFilters (RollDie.count rollDie) <> foldMap quantityFilters (RollDie.modifier rollDie))
  -- CR 705.1's number of coins is a Quantity, so its filters are reachable here.
  Effect.FlipCoin flipCoin -> frame Unframed (quantityFilters (FlipCoin.count flipCoin))
  -- CR 500.7's number of turns is a Quantity, so its filters are reachable here.
  Effect.TakeExtraTurn takeExtraTurn -> frame Unframed (quantityFilters (TakeExtraTurn.count takeExtraTurn))
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary _ ref) -> frame SourceHostFramed (objectRefFilters ref)
  -- A PlayerRef carries no Filter, exactly as GainPlayerCounters' does not.
  Effect.Shuffle {} -> []
  Effect.OfferCast {} -> []
  -- Both, as GainControl's arm does: the Duration's Condition carries Victor
  -- Mancha, Runaway's IsSource and ControlledBy, and an empty list here would
  -- take them out of the lint without failing anything.
  Effect.GrantPlayFromExile grant -> frame Unframed (durationFilters (GrantPlayFromExile.duration grant)) <> frame SourceHostFramed (objectRefFilters (GrantPlayFromExile.ref grant))
  -- The swept ref's Filters AND the body's, the rider's shape: a nested effect
  -- list is exactly what this traversal must not stop at.
  Effect.ForEach (ForEach.MkForEach ref _ body) -> frame SourceHostFramed (objectRefFilters ref) <> concatMap effectFilters body

-- Per MODE rather than through Modal.allTargetSlots, which is a Map.unions and so
-- collapses two modes declaring the same slot name (#475) -- the cross-check
-- below counts occurrences, and a collapse there would read as a Filter this
-- traversal cannot see.
modalFilters :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
modalFilters modal =
  concatMap
    ( \mode ->
        concatMap effectFilters (Mode.allEffects mode)
          <> frame Unframed (concatMap conditionFilters (modeClauseConditions mode))
          -- CR 118.12's gate, the fourth thing a clause carries that a card
          -- writes filters into; see #2876.
          <> concatMap payGateFilters (Maybe.mapMaybe Clause.payGate (Foldable.toList (Mode.clauses mode)))
          -- THE target-slot-framed position: a mode's own slot, matched by
          -- Pawl.Engine.Target.admittedGiven at both of CR 115's moments.
          <> frame InTargetSlot (concatMap targetSlotFilters (Map.elems (Mode.targetSlots mode)))
    )
    (Modal.modes modal)

-- Every ACTIVATED ability this face's static abilities GRANT to another object
-- (CR 613.1f). Swept alongside the printed ones: the quoted text is this card's,
-- so every corpus lint that reads a printed activated ability has to read these
-- too.
grantedActivatedAbilities :: Face.Face Card.Type.Card -> [ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
grantedActivatedAbilities card =
  [ ability
  | Modification.GainAbility (GrantedAbility.Activated ability) <- grantedModifications card
  ]

-- The TRIGGERED half of the same grant, swept for the same reason: Sixth Sense's
-- quoted "whenever this creature deals combat damage to a player" is text
-- printed on the Aura.
grantedTriggeredAbilities :: Face.Face Card.Type.Card -> [TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
grantedTriggeredAbilities card =
  [ ability
  | Modification.GainAbility (GrantedAbility.Triggered ability) <- grantedModifications card
  ]

-- Every modification this face carries, the shared walk both grant sweeps above
-- index into. TWO sources, not one: a PRINTED static ability's modifications (CR
-- 613.1f as card text), and the modifications a RESOLUTION stores through
-- Effect.ModifyTarget (CR 611.2) -- Retraction Helix's quoted "{T}: Return
-- target nonland permanent to its owner's hand" is the second kind, and reading
-- only the first would let it escape every lint below.
--
-- Iterated, because a granted ability's own effects may store a grant in turn:
-- each round feeds the abilities just found back through the ModifyTarget walk,
-- and it bottoms out because every round descends strictly further into one
-- card's finite text. Seeded from printedCarrierEffects rather than
-- cardCarrierEffects, which is what keeps this out of a loop with the two grant
-- limbs that function ends with.
grantedModifications :: Face.Face Card.Type.Card -> [Projection.Modification]
grantedModifications card =
  let printed =
        [ modification
        | static <- Face.staticAbilities card,
          modification <- Foldable.toList (StaticAbility.modifications static)
        ]
      -- The WILDCARD-free read of the one Effect arm carrying a Modification;
      -- namedRemovals' caveat applies, a second such arm would escape this.
      storedIn effects =
        [ ModifyTarget.modification modify
        | Effect.ModifyTarget modify <- concatMap effectWithNested effects
        ]
      effectsOf modifications =
        concatMap (Modal.allEffects . ActivatedAbility.modal) [a | Modification.GainAbility (GrantedAbility.Activated a) <- modifications]
          <> concatMap (Modal.allEffects . TriggeredAbility.modal) [t | Modification.GainAbility (GrantedAbility.Triggered t) <- modifications]
      deeper modifications =
        if null modifications
          then []
          else modifications <> deeper (storedIn (effectsOf modifications))
   in deeper (printed <> storedIn (printedCarrierEffects card))

-- The enchant slots this face GRANTS rather than prints: CR 613.1f layer 6's
-- Modification.GainEnchant, which Cloudform and Gliding Licid write and CR
-- 702.103b's bestow grants from the engine. Indexed off grantedModifications, so
-- both roads to a modification are covered -- a printed static ability's, and the
-- ones Effect.ModifyTarget stores, which is the road both cards take.
grantedEnchantSlots :: Face.Face Card.Type.Card -> [TargetSlot.TargetSlot]
grantedEnchantSlots card =
  [ slot
  | Modification.GainEnchant slot <- grantedModifications card
  ]

-- Every enchant slot a face declares, by either road. Pawl.Engine.Projection
-- seeds ProjectedCharacteristics.enchant from Face.enchant and appends the grants
-- to it, and Pawl.Engine.Card.foldEnchant conjoins the result into the one slot CR
-- 601.2c answers -- so a lint reading only the printed half judges only half the
-- text that reaches that slot.
enchantSlots :: Face.Face Card.Type.Card -> [TargetSlot.TargetSlot]
enchantSlots card = Face.enchant card <> grantedEnchantSlots card

triggeredAbilityFilters :: TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
triggeredAbilityFilters ability =
  frame Unframed (triggerConditionFilters (TriggeredAbility.condition ability))
    -- THE CR 603.4 position: Pawl.Engine.Event.Trigger.interveningHolds and
    -- Pawl.Engine.Stack's CR 608.2a re-check both supply the source's host here,
    -- and the trigger CONDITION above them is matched by Event.matchesTrigger,
    -- which does not.
    <> frame SourceHostFramed (concatMap conditionFilters (Maybe.maybeToList (TriggeredAbility.intervening ability)))
    <> modalFilters (TriggeredAbility.modal ability)

activatedAbilityFilters :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
activatedAbilityFilters ability =
  -- CR 602.2b sends an activation through CR 601.2c before CR 601.2h, and
  -- Pawl.Engine.Activate stamps the chosen targets on the ability object before
  -- Pawl.Engine.Cost.pay reads them (Cost.announcedSlots) -- so a slot read in
  -- the cost is answered, and Unframed's promise holds (Synthetic Spiteful
  -- Altar). See SlotlessCostFramed for the positions where it does not.
  unframed (costFilters (ActivatedAbility.cost ability))
    -- CR 101.1's ceiling on this ability's own X, through a Count -- cardFilters'
    -- treatment of Face.maximumX one type over (Blighted Nightmare).
    <> frame Unframed (concatMap quantityFilters (ActivatedAbility.maximumX ability))
    -- CR 702.178a's "as long as" gate, the triggeredAbilityFilters treatment of
    -- CR 603.4's intervening "if" one field over.
    <> frame Unframed (concatMap conditionFilters (Maybe.maybeToList (ActivatedAbility.condition ability)))
    -- CR 602.5's rider. Unframed because Pawl.Engine.ActivationRestriction's
    -- OnlyIf arm builds a plain Filter.contextFor: sourceAttachedTo, sourcePower
    -- and slotAmount are Nothing there and slotObjects, slotNames and
    -- slotControllers are empty. The tag is what FENCES the atoms that would read
    -- those rather than leaving them silent -- CR 303.4b's IsHostOfSource, which
    -- belongs to SourceHostFramed, and CR 709.4a's SameNameAsBound and CR 110.2's
    -- SameControllerAsBound, which belong to InTargetSlot and whose vacuous
    -- directions differ (see the Framing haddock). The two source-power
    -- comparisons have no tag of their own and are fenced corpus-wide instead, by
    -- the CR 702.134a / CR 702.149a case below, which refuses them in every
    -- card-authored position rather than in the ones a tag names.
    <> frame Unframed (concatMap conditionFilters (concatMap restrictionConditions (ActivatedAbility.restrictions ability)))
    <> modalFilters (ActivatedAbility.modal ability)

-- EVERY Filter position reachable from a card, each paired with whether an attach
-- frames it. Most of Pawl.Types.Face's fields can hold one, and here is where
-- each one's comes from:
--
--   * `keywords` -- CR 702.29e typecycling (Ash Barrens' landcycling).
--   * `power`, `toughness`, `characteristicPT` -- CR 208.2's printed star,
--     through a Count.
--   * `maximumX` -- CR 101.1's ceiling on X, through a Count (Soul Immolation).
--   * `staticAbilities` -- the affected set, CR 604.2's "as long as" condition,
--     and the layer-6/7 modifications' own keywords and Counts.
--   * `replacementEffects` -- CR 614.1's counter-placement pattern, plus CR
--     604.2's "as long as" condition gating the ability that prints it.
--   * `enchant` -- CR 303.4a's enchant ability, a TargetSlot.
--   * `additionalCosts` -- CR 601.2f's sacrifice component.
--   * `alternativeCosts` -- that same component, plus CR 604.2's "as long as"
--     condition gating one.
--   * `specialActions` -- CR 116.2d's ignore cost, a Cost like the two above.
--   * `playerAbilities` -- CR 613.11's cost modifiers and CR 601.3b's timing
--     permission.
--   * `combatRestrictions` (CR 508.1c / 509.1b), `sacrificeRestrictions` (CR
--     701.21a / 101.2), `untapRestrictions` (CR 502.3 / 101.2),
--     `entryRestrictions` (CR 400.4a / 101.2),
--     `counterRestrictions` (CR 122.6 / 101.2),
--     `attackRequirements` (CR 508.1d), `blockRequirements`
--     (CR 509.1c), `attackCosts` (CR 508.1h) and `blockCosts` (CR 509.1d) --
--     nine more affected sets, plus each combat cost's Counted share, which is a
--     Quantity, plus the CR 604.2 "as long as" clause an attacking requirement
--     may carry (CR 508.1d's second reading).
--   * `spell`, `activatedAbilities`, `triggeredAbilities`, `delayedAbilities` --
--     every mode's target slots and effects, plus an activation cost, a
--     trigger's own condition and its intervening clause.
--   * `mulliganActions` (CR 103.5b) and `openingHandActions` (CR 103.6) -- the two
--     pregame actions, which `cardResolutionEffects` above does not reach.
--
-- The remaining fields hold none: `name`, `manaCost`, `typeLine`, `loyalty`,
-- `defense`, `vanguard`, `colorIndicator`, `counterability`, `castingPermissions`
-- and `castingRestrictions`. That is checkable rather than asserted: none of the
-- types those ten fields reach imports Pawl.Types.Filter, which
-- `grep -rl 'import qualified Pawl.Types.Filter' source/libraries/types/` over
-- each one's closure answers.
--
-- The import graph is a NECESSARY condition and not a sufficient one, and CR
-- 122.1b's keyword counter is why: Pawl.Types.CounterKind is parametric in its
-- keyword and so imports no Filter, yet every card-side use instantiates it at
-- Pawl.Types.Keyword, which does. A type parameterised over a Filter-bearing
-- type is invisible to the grep, so each such carrier is walked by hand instead
-- -- counterKindFilters' callers above are the whole set; see #2728.
--
-- The two lists together are the whole record.
--
-- Every case BELOW this function is exhaustive with no catch-all, so a new
-- constructor on any of those types fails to compile until it is classified. This
-- record fold is the exception, exactly as cardCounts' own caveat says: a NEW
-- Face field that can hold a Filter would bypass it silently. That is what the
-- codec cross-check in canHostSubjectOffends is for.
cardFilters :: Face.Face Card.Type.Card -> [(Framing, Filter.Type.Filter Keyword.Keyword)]
cardFilters card =
  frame
    Unframed
    ( concatMap keywordFilters (Set.toList (Face.keywords card))
        <> concatMap quantityFilters (characteristicQuantities card)
        <> concatMap quantityFilters (Face.maximumX card)
        <> concatMap (\(Power.MkPower quantity) -> quantityFilters quantity) (Maybe.maybeToList (Face.power card))
        <> concatMap (\(Toughness.MkToughness quantity) -> quantityFilters quantity) (Maybe.maybeToList (Face.toughness card))
        <> concatMap targetSlotFilters (Face.enchant card)
        -- CR 601.2f's additional cost is paid AFTER CR 601.2c has chosen the
        -- targets, and Pawl.Engine.Cost.pay reads them off the spell
        -- (Cost.announcedSlots), so a slot read here is answered -- Unframed's
        -- promise, kept. SlotlessCostFramed says which cost positions are not.
        <> concatMap (unframed . costComponentFilters) (Face.additionalCosts card)
        <> concatMap (frame Unframed . alternativeCostFilters) (Face.alternativeCosts card)
        <> concatMap (quantityFilters . CostReduction.perEach) (Face.costReductions card)
        <> concatMap (slotlessCost . specialActionFilters) (Face.specialActions card)
        <> concatMap (unframed . blockRequirementFilters) (Face.blockRequirements card)
        <> concatMap blockPermissionFilters (Face.blockPermissions card)
        <> concatMap (frame Unframed . attackRequirementFilters) (Face.attackRequirements card)
        <> concatMap (unframed . affectedFilters . AttackCost.subject) (Face.attackCosts card)
        <> concatMap (perCreatureFilters . AttackCost.perAttacker) (Face.attackCosts card)
        <> concatMap (unframed . affectedFilters . BlockCost.subject) (Face.blockCosts card)
        <> concatMap (perCreatureFilters . BlockCost.perBlocker) (Face.blockCosts card)
        <> concatMap (frame Unframed . combatRestrictionFilters) (Face.combatRestrictions card)
        <> concatMap (unframed . affectedFilters . SacrificeRestriction.affected) (Face.sacrificeRestrictions card)
        <> concatMap (unframed . affectedFilters . UntapRestriction.affected) (Face.untapRestrictions card)
        <> concatMap (unframed . attachRestrictionFilters) (Face.attachRestrictions card)
        <> concatMap (unframed . affectedFilters . EntryRestriction.affected) (Face.entryRestrictions card)
        <> concatMap (unframed . affectedFilters . CounterRestriction.affected) (Face.counterRestrictions card)
        -- And the KIND the prohibition names (Melira, Sylvok Outcast's -1/-1
        -- counters), which may be a whole Keyword carrying a Filter (CR 122.1b);
        -- see #2728. Nothing there is Solemnity's "counters" -- any kind at all.
        <> concatMap (concatMap counterKindFilters . Maybe.maybeToList . CounterRestriction.kind) (Face.counterRestrictions card)
    )
    <> concatMap printedReplacementFilters (Face.replacementEffects card)
    <> concatMap staticAbilityFilters (Face.staticAbilities card)
    -- SOURCE-HOSTED for staticAbilityFilters' reason: a clause on a static
    -- ability is framed against the permanent that has it, so an IsSource inside
    -- it names that permanent rather than being unframed.
    <> concatMap (frame SourceHostFramed . concatMap conditionFilters . Maybe.maybeToList . PlayerStaticAbility.condition) (Face.playerAbilities card)
    -- And the EFFECT beside that clause, for the same reason and see #1242:
    -- Pawl.Engine.PlayerEffect.matchesObjectFrom is handed the row's own source,
    -- so CR 303.4b's atom is answerable in every arm of a printed player ability
    -- (Oppressive Rays' "enchanted creature"). The STORED CR 611.2c carrier is
    -- not -- Effect.AffectPlayers' own filters stay unframed above, because a
    -- resolved spell has no permanent behind it to be attached to anything.
    <> concatMap (sourceHosted . playerEffectFilters . PlayerStaticAbility.effect) (Face.playerAbilities card)
    <> modalFilters (Face.spell card)
    <> concatMap activatedAbilityFilters (Face.activatedAbilities card)
    <> concatMap activatedAbilityFilters (grantedActivatedAbilities card)
    <> concatMap triggeredAbilityFilters (grantedTriggeredAbilities card)
    <> concatMap triggeredAbilityFilters (Face.triggeredAbilities card)
    <> concatMap triggeredAbilityFilters (Map.elems (Face.delayedAbilities card))
    <> concatMap (modalFilters . DungeonRoom.ability) (Face.rooms card)
    <> concatMap handActionFilters (Face.mulliganActions card)
    <> concatMap handActionFilters (Face.openingHandActions card)

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
  -- The ENCHANT half, which the sweep above cannot reach: CR 303.4a's slot is
  -- declared on the FACE beside the modes (Card.enchantSlotMap), so it is in no
  -- mode's targetSlots and Resolve.modeSlots never folds it.
  --
  -- A one-sided claim rather than the equality above, and the asymmetry is where
  -- the slot is DECLARED: CR 303.4a puts it on the enchant ability, beside the
  -- modes rather than in one, so there is no mode whose declared set it belongs to
  -- and nothing for an equality to compare it against. What CR 601.2c answers it
  -- with is bound under Card.enchantSlot like any other target (Pawl.Engine.Stack
  -- reads it back from there), so "reads nothing" is the whole of what there is to
  -- say. What it fences is a pool, filter or CR 202.3 computed bound naming a
  -- slot: it would compile, pass every lint here, and then be judged in
  -- Target.slotContext against whatever the announcement happened to seed.
  --
  -- BOTH roads to that slot, printed and granted (enchantSlots), and the granted
  -- one owes the claim even harder: a CR 613.1f grant is a CR 611.2 continuous
  -- effect that outlives the resolution that wrote it, so nothing that resolution
  -- bound is in scope when CR 601.2c chooses for a bestowed spell or CR 303.4c's
  -- state-based action re-reads CR 702.5a against a permanent. Declaring the name
  -- in the granting mode would not rescue it, which is why this is a "reads
  -- nothing" claim rather than a fold into Resolve.slotsOf's ModifyTarget arm.
  Spec.it s "an enchant slot reads no slot, printed or granted" $ do
    ps <- S.allPrintings s
    let reads_ face = Set.unions (fmap (Map.keysSet . Resolve.targetSlotSlots) (enchantSlots face))
        offenders = filter (anyFace (not . Set.null . reads_) . Printing.card) ps
        prints field = any (anyFace (not . null . field) . Printing.card) ps
    -- One guard per road, because a pool with no enchant slot at all would pass
    -- saying nothing -- and a pool that only PRINTS them would say nothing about
    -- the granted half.
    Spec.assertBool s (prints Face.enchant) "the pool prints an enchant slot"
    Spec.assertBool s (prints grantedEnchantSlots) "the pool grants an enchant slot"
    Spec.assertEqWith s "no enchant slot names a slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 706.4's "the other result", which only a TWO-die instruction has: with
  -- any other count what the roller did not choose is not one number, and
  -- Pawl.Engine.Resolve leaves the slot unbound rather than guessing which of
  -- them the card meant. So a card writing the slot at another count is asking
  -- for a number nothing will ever bind, which compiles and reads as zero.
  Spec.it s "a roll binds the other result only where it rolls two dice" $ do
    ps <- S.allPrintings s
    let binds effect = case effect of
          Effect.RollDie rollDie -> Maybe.isJust (RollDie.other rollDie)
          _ -> False
        offends effect = case effect of
          Effect.RollDie rollDie -> Maybe.isJust (RollDie.other rollDie) && RollDie.count rollDie /= Quantity.Type.Literal 2
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- A guard, since a pool binding no other result at all would pass saying
    -- nothing (Valiant Endeavor).
    Spec.assertBool s (any (anyFace (any binds . cardResolutionEffects) . Printing.card) ps) "the pool binds a roll's other result"
    Spec.assertEqWith s "no roll binds an other result it cannot name" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against printed Auras restated rather than card
  -- files, as the hand-action lint below does it. Filter.IsBound is the atom, since
  -- Filter.boundSlots is one of the three folds targetSlotSlots joins.
  Spec.it s "the lint itself catches an enchant slot naming a slot" $ do
    pacifism <- S.printingOf s registry "Pacifism"
    let face = S.combinedFace pacifism
        stray = SlotName.MkSlotName (Text.pack "stray")
        named = fmap (\slot -> slot {TargetSlot.filter = Just (Filter.Type.IsBound stray)}) (Face.enchant face)
        reads_ slots = Set.unions (fmap (Map.keysSet . Resolve.targetSlotSlots) slots)
    Spec.assertEqWith s "the real Pacifism's enchant slot reads nothing" (reads_ (Face.enchant face)) Set.empty
    Spec.assertEqWith s "a filter naming a slot is reported" (reads_ named) (Set.singleton stray)
  -- And the same rejection on the GRANTED road, which is a separate case because
  -- it is a separate reader: Gliding Licid prints no enchant ability at all and
  -- writes CR 613.1f's grant from an activated ability's Effect.ModifyTarget, so
  -- the slot arrives through grantedModifications and nothing else.
  Spec.it s "the lint itself catches a granted enchant slot naming a slot" $ do
    licid <- S.printingOf s registry "Gliding Licid"
    let face = S.combinedFace licid
        stray = SlotName.MkSlotName (Text.pack "stray")
        named = fmap (\slot -> slot {TargetSlot.filter = Just (Filter.Type.IsBound stray)}) (enchantSlots face)
        reads_ slots = Set.unions (fmap (Map.keysSet . Resolve.targetSlotSlots) slots)
    -- FIRST, because it is the whole of what this case proves: drop the granted
    -- road from enchantSlots and there is no slot left to plant a filter on, so
    -- this reads Set.empty. The two below are preconditions on the fixture.
    Spec.assertEqWith s "a granted enchant slot naming a slot is reported" (reads_ named) (Set.singleton stray)
    Spec.assertEqWith s "Gliding Licid prints no enchant ability" (Face.enchant face) []
    Spec.assertEqWith s "the real granted slot reads nothing" (reads_ (enchantSlots face)) Set.empty
  -- The same equality over the two PREGAME windows, which the sweep above does not
  -- reach: Card.allEffects is the spell's modes, and CR 103.5b's and CR 103.6's
  -- actions hang off Pawl.Types.Face beside it. See handActionSlotsOffend for why
  -- the declared side is empty and why only `self` comes off the read side.
  Spec.it s "every hand action reads only what performing it binds" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace handActionSlotsOffend . Printing.card) ps
        prints field = any (anyFace (not . null . field) . Printing.card) ps
    -- The guards, because a sweep over an empty traversal passes saying nothing.
    -- One per window, so the sweep cannot go quiet by the pool losing either, and
    -- one for a hand action that actually READS a slot -- without that last the
    -- subtraction above is never exercised and the lint would pass with any
    -- `handActionBound` at all.
    Spec.assertBool s (prints Face.mulliganActions) "the pool prints a mulligan action"
    Spec.assertBool s (prints Face.openingHandActions) "the pool prints an opening-hand action"
    Spec.assertBool
      s
      (any (anyFace (not . all (all (Map.null . Resolve.slotsOf)) . handActions) . Printing.card) ps)
      "the pool has a hand action that reads a slot"
    Spec.assertEqWith s "no hand action reads an unbound slot" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against the printed card restated rather than a card
  -- file, as the Room and repeated-face-name lints do it. Leyline of the Void is
  -- the card that makes the trap concrete: its CR 103.6a action reads `self`, so a
  -- sweep that did not subtract the performer's binding would demand a targetSlots
  -- entry the reserved-name sweep below forbids, and the two would be mutually
  -- unsatisfiable.
  Spec.it s "the lint itself catches a hand action naming a slot nothing binds" $ do
    leyline <- S.printingOf s registry "Leyline of the Void"
    let face = S.combinedFace leyline
        overActions f card = card {Face.openingHandActions = fmap (\action -> action {HandAction.effects = fmap f (HandAction.effects action)}) (Face.openingHandActions card)}
        readSlot slot effect = case effect of
          Effect.MoveToZone move -> Effect.MoveToZone move {MoveToZone.ref = ObjectRef.InSlot slot}
          other -> other
        bindSlot slot effect = case effect of
          Effect.MoveToZone move -> Effect.MoveToZone move {MoveToZone.slot = Just slot}
          other -> other
        stray = SlotName.MkSlotName (Text.pack "stray")
    Spec.assertBool s (not (handActionSlotsOffend face)) "the real Leyline of the Void is accepted"
    Spec.assertBool s (handActionSlotsOffend (overActions (readSlot stray) face)) "a slot nothing binds is rejected"
    -- The dataflow the lint must NOT reject: an action that defines a slot and then
    -- reads it back, which is Resolve.definedSlots' whole job on the resolution
    -- carriers.
    Spec.assertBool
      s
      (not (handActionSlotsOffend (overActions (bindSlot stray . readSlot stray) face)))
      "a slot the action itself defines is accepted"
    -- And the BINDING half, which ownBoundSlots now reaches through the same two
    -- fields: writing CR 113.7's reserved name from inside the action clobbers what
    -- performHandAction stamped.
    Spec.assertEqWith
      s
      "CR 113.7 self bound by a hand action is caught, and the real card binds nothing"
      (reservedBindings (overActions (bindSlot Binding.triggerSource) face), reservedBindings face)
      (Set.singleton Binding.triggerSource, Set.empty)
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
  -- CR 608.2d's either-or, whose two branches must name each other: the corpus
  -- half, with the same shape the slot-name pair above has.
  Spec.it s "no card's either-or names a sibling that does not name it back" $ do
    ps <- S.allPrintings s
    let branching = any (any (Maybe.isJust . Clause.orElse) . Mode.clauses) . Modal.modes
        offenders = filter (anyFace cardBranchesAreAsymmetric . Printing.card) ps
    -- Guards against passing vacuously: a pool where no clause branches at all
    -- could not offend whatever the lint said. Twiddle, Teardrop Kami and Keys to
    -- the House make it real -- a spell and two activated abilities, and Keys is
    -- the one whose pair carries no "may".
    Spec.assertBool s (any (anyFace (any branching . faceModals) . Printing.card) ps) "the pool has a clause carrying an either-or"
    Spec.assertEqWith s "no half-named branch" (fmap (S.nameOf . Printing.card) offenders) []
  -- And the rejecting direction, against Twiddle misauthored on purpose -- never
  -- a card file, since a card that offends a lint must not be loadable.
  Spec.it s "the lint itself catches an either-or whose sibling does not name it back" $ do
    twiddle <- S.printingOf s registry "Twiddle"
    let face = S.combinedFace twiddle
        -- Every offender is built by rewriting BOTH clauses where the defect is
        -- symmetric, so that each assertion fails for the arm it names: leaving
        -- one clause pointing at a healthy sibling would make the OTHER clause
        -- the offender and the assertion pass without the arm under test.
        rewrite :: [(Int, Maybe OrElse.OrElse)] -> Face.Face Card.Type.Card
        rewrite edits =
          let overClauses clauses = foldr (\(i, orElse) -> Seq.adjust' (\clause -> clause {Clause.orElse = orElse}) i) clauses edits
              overMode mode = mode {Mode.clauses = overClauses (Mode.clauses mode)}
           in face {Face.spell = (Face.spell face) {Modal.modes = fmap overMode (Modal.modes (Face.spell face))}}
        -- Twiddle's own chooser, the unmarked "you" (CR 405.4): every offender
        -- below keeps it, so each assertion fails for the arm it names rather
        -- than for the chooser half.
        branchTo n = Just (OrElse.MkOrElse (ClauseIndex.MkClauseIndex n) (PlayerRef.Relative PlayerRelation.You))
        selfNaming, dangling, disagreeing :: [(Int, Maybe OrElse.OrElse)]
        selfNaming = [(0, branchTo 0), (1, branchTo 1)]
        dangling = [(0, branchTo 7), (1, branchTo 7)]
        disagreeing = [(0, Just (OrElse.MkOrElse (ClauseIndex.MkClauseIndex 1) PlayerRef.EachPlayer))]
    Spec.assertBool s (not (cardBranchesAreAsymmetric face)) "Twiddle's tap and untap name each other, and are accepted"
    Spec.assertBool s (cardBranchesAreAsymmetric (rewrite [(1, Nothing)])) "a branch whose sibling names nobody back is rejected"
    Spec.assertBool s (cardBranchesAreAsymmetric (rewrite selfNaming)) "two branches each naming themselves are rejected"
    Spec.assertBool s (cardBranchesAreAsymmetric (rewrite dangling)) "branches naming an ordinal no clause has are rejected"
    Spec.assertBool s (cardBranchesAreAsymmetric (rewrite disagreeing)) "and a pair whose halves name different choosers is rejected"
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
  -- CR 702.102: every card that prints fuse can actually BE fused.
  --
  -- Pawl.Engine.Card.fusedFace answers Nothing for a fuse card whose halves are
  -- modal or whose halves name a target slot alike, and either would be a card
  -- quietly offering two halves where the printing offers three casts. Slot names
  -- are card DATA and never printed, so the second is always the card file's to
  -- fix; the first is a capability nothing in the pool needs yet (gap #2787).
  Spec.it s "CR 702.102 every card with fuse has a fused face" $ do
    root <- Registry.defaultRoot
    loaded <- Registry.loadRoot root
    Spec.assertBool s (not (null loaded)) "the corpus is not empty"
    let offends (path, result) = case result of
          Left reason -> Just (path <> ": " <> Text.unpack reason)
          Right card ->
            if Set.member Keyword.Fuse (Face.keywords (Card.combined card)) && Maybe.isNothing (Card.fusedFace card)
              then Just (path <> ": prints fuse and cannot be fused")
              else Nothing
    Spec.assertEqWith s "every card with fuse fuses" (Maybe.mapMaybe offends loaded) []
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
  -- The checks that read a card's NUMBERS each descend into an effect's
  -- ObjectRefs, and they do it in one place: Resolve.effectObjectRefs. Which
  -- positions that traversal owes is what this case pins, planted rather than
  -- read off a card because a nested Quantity lives only in a library walk's
  -- depth and a library walk under most of these opcodes is a card-data error
  -- rather than a printing.
  Spec.it s "every ObjectRef-taking opcode reports the refs it holds" $ do
    Spec.assertBool s (not (null objectRefPositions)) "the planted positions are not empty"
    Spec.assertEqWith
      s
      "each planted position answers with its own ref"
      (fmap (\(label, effect, _) -> (label, Resolve.effectObjectRefs effect)) objectRefPositions)
      (fmap (\(label, _, refs) -> (label, refs)) objectRefPositions)
  -- The same pinning for the PLAYER half, whose readers are Resolve.slotsOf and
  -- Pawl.CardSpec's plural-slot lint. Planted for objectRefPositions' reason: a
  -- second PlayerRef field on a payload that already has one compiles.
  Spec.it s "every PlayerRef-taking opcode reports the references it holds" $ do
    Spec.assertBool s (not (null playerRefPositions)) "the planted positions are not empty"
    Spec.assertEqWith
      s
      "each planted position answers with its own reference"
      (fmap (\(label, effect, _) -> (label, Resolve.effectPlayerRefs effect)) playerRefPositions)
      (fmap (\(label, _, refs) -> (label, refs)) playerRefPositions)
    Spec.assertEqWith
      s
      "and each per-seat object reference answers with its own"
      (fmap (\(label, ref, _) -> (label, Resolve.objectRefPlayerRefs ref)) objectRefPlayerRefPositions)
      (fmap (\(label, _, refs) -> (label, refs)) objectRefPlayerRefPositions)
  -- And the three readers of a nested Quantity take their refs from there,
  -- shown at an opcode that routed none of its own: Destroy's ref reached
  -- slotsOf and nothing else, so all three answered a constant for it.
  Spec.it s "CR 107.3 a depth nested in an opcode's ref reaches all three checks" $ do
    let ghost = SlotName.MkSlotName (Text.pack "ghost")
        destroying q = Effect.Destroy (Destroy.MkDestroy (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) q)) Regenerability.Regenerable Nothing Nothing Nothing)
        tally = Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))) (Filter.Type.And []) Aggregation.Members
    Spec.assertEqWith
      s
      "CR 603.3b a depth hiding a target slot is not exhaustively reported"
      (Resolve.slotsAreExhaustive (destroying (Quantity.Type.LifeTotal (PlayerRef.InSlot ghost))))
      False
    Spec.assertEqWith
      s
      "CR 107.3 a depth reading X makes the effect an X reader"
      (Resolve.readsX [destroying (Quantity.Type.InSlot Binding.variableX)])
      True
    Spec.assertEqWith
      s
      "a Count in the depth reaches the shared-zone-scope lint"
      (effectCounts (destroying (Quantity.Type.Count tally)))
      [tally]
    -- The negative, one thing changed: a literal depth holds no slot, no X and
    -- no Count, so each answer above is the depth's and not the arm's.
    Spec.assertEqWith
      s
      "and a literal depth fires none of the three"
      (Resolve.slotsAreExhaustive (destroying (Quantity.Type.Literal 3)), Resolve.readsX [destroying (Quantity.Type.Literal 3)], effectCounts (destroying (Quantity.Type.Literal 3)))
      (True, False, [])
  Spec.it s "the lint itself catches a dangling reference" $
    let bad = Map.keysSet (Resolve.slotsOf (Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "ghost"))) (Quantity.Type.Literal 3))) Nothing Nothing)))
     in Spec.assertBool s (bad /= Map.keysSet (Map.empty :: Map.Map SlotName.SlotName TargetSlot.TargetSlot)) "misauthored card detected"
  -- CR 120.2b's dealer is a slot READ like any other, so the same lint has to
  -- reach it. The effect below references no other slot, so the answer is the
  -- dealer's alone.
  Spec.it s "the lint reaches a dangling DEALER reference" $
    let ghost = SlotName.MkSlotName (Text.pack "ghost")
     in Spec.assertEqWith
          s
          "the dealer slot is read"
          (Map.keysSet (Resolve.slotsOf (Effect.DealDamage (DealDamage.MkDealDamage (Seq.singleton (DamagePart.MkDamagePart ObjectRef.EachPlayer (Quantity.Type.Literal 3))) (Just ghost) Nothing))))
          (Set.singleton ghost)
  -- The SPELL half of CR 601.2b's contract: what a card's own modes read is
  -- announced against the card's own cost -- mana cost, additional costs and
  -- alternative costs together (`spellCostsOf`), since CR 107.3a names all of them.
  --
  -- A PRINTED LOYALTY OF X reads X without any effect doing so: CR 306.5b's
  -- intrinsic replacement is the reader, and CR 107.3m values its X at the
  -- announcement the spell made. Nissa, Steward of Elements is the pool's
  -- producer, and without this disjunct the lint would call it an offender for
  -- declaring an {X} nothing reads.
  --
  -- A CR 118.12 COST reads X the same way and by the same rule, and it is not an
  -- effect, so Card.allEffects cannot see it: Clash of Wills' only reader of the
  -- X it announces is the "pays {X}" its clause offers at resolution
  -- (`payGateCostsOf`, substituted in by Pawl.Engine.Resolve.announcedXOn).
  --
  -- A TARGET SLOT counting the announced X is the fourth such reader, and a
  -- slot whose CR 202.3 computed bound names it the fifth (both
  -- modalReadsAnnouncedX): CR 601.2c's number of targets and the bound its filter
  -- compares a candidate against are alike named at the announcement rather than
  -- by anything the modes do.
  Spec.it s "every printing that reads X declares X, and vice versa" $ do
    ps <- S.allPrintings s
    let -- CR 107.3m's other reader, one CR 614.1c row over from the printed
        -- loyalty above: "this creature enters with X +1/+1 counters on it"
        -- (Protean Hydra). The amount is a Quantity on an
        -- EntryRewrite.WithCounters row rather than an effect, so Card.allEffects
        -- cannot see this one either.
        entryCountersReadX c =
          or
            [ Set.member Binding.variableX (QuantitySlot.slots q)
            | ReplacementEffect.EntryR (EntryR.MkEntryR _ (EntryRewrite.WithCounters wc)) <- fmap PrintedReplacement.effect (Face.replacementEffects c),
              q <- Map.elems (WithCounters.counters wc)
            ]
        readsX c =
          Resolve.readsX (Card.allEffects c)
            || Face.loyalty c == Just Loyalty.Variable
            || entryCountersReadX c
            || any declaresVariable (payGateCostsOf (Face.spell c))
            || modalReadsAnnouncedX (Face.spell c)
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
  --
  -- No `payGateCostsOf` disjunct here, unlike the spell half: no ability in the
  -- pool offers a CR 118.12 cost containing {X} (Martyr of Frost, "{2}, Reveal X
  -- blue cards from your hand, Sacrifice this creature: Counter target spell
  -- unless its controller pays {X}", is the printing that would). Adding the
  -- disjunct unexercised would weaken the sweep silently; without it the first
  -- such card reddens this lint, which is where its author wants to be.
  -- CR 101.1's ceiling, as a TOTALITY guard on Pawl.Engine.Cost.greatestPayableX:
  -- that ascending search stops either because the demand eventually outruns the
  -- board (Toxic Deluge's "pay X life", CR 119.4) or because the card states a
  -- maximum (Soul Immolation's blight X, whose payability CR 701.68b does not
  -- tie to the number). A printing with neither would climb forever, so it is a
  -- card-data error rather than an engine one, and this is where it is caught.
  --
  -- Over the SPELL costs and the ACTIVATION costs alike: CR 602.2b routes an
  -- activation through rule 601.2b, so an ability announcing an X the board
  -- cannot refuse needs the same ceiling, read off the ability rather than off
  -- the face (Pawl.Types.ActivatedAbility.maximumX). The two halves are checked
  -- against two different fields for that reason, and neither can stand in for
  -- the other.
  Spec.it s "CR 101.1 every printing whose X the board cannot refuse states a maximum for it" $ do
    ps <- S.allPrintings s
    let unrefusable c = declaresVariable c && not (Cost.demandGrowsWithX c)
        unrefusableX = any unrefusable . spellCostsOf
        unrefusableAbility ab = unrefusable (ActivatedAbility.cost ab)
        unbounded f =
          (unrefusableX f && null (Face.maximumX f))
            || any (\ab -> unrefusableAbility ab && null (ActivatedAbility.maximumX ab)) (Face.activatedAbilities f)
        offenders = filter (anyFace unbounded . Printing.card) ps
    -- Guards each half against passing vacuously: the pool must hold a spell and
    -- an ability whose X reaches a cost only through a component with no growing
    -- demand (Soul Immolation's blight X, Blighted Nightmare's).
    Spec.assertBool
      s
      (any (anyFace unrefusableX . Printing.card) ps)
      "the pool has a printing whose X the board cannot refuse"
    Spec.assertBool
      s
      (any (anyFace (any unrefusableAbility . Face.activatedAbilities) . Printing.card) ps)
      "and an activated ability whose X the board cannot refuse"
    Spec.assertEqWith s "every one of them states a maximum" (fmap (S.nameOf . Printing.card) offenders) []
  Spec.it s "CR 602.2b every activated ability that reads X declares {X} in its own cost" $ do
    ps <- S.allPrintings s
    let abilitiesOf p = fmap ((,) (Face.name (S.combinedFace p))) (Face.activatedAbilities (S.combinedFace p))
        abilities = concatMap abilitiesOf ps
        offends (_, ab) =
          (Resolve.readsX (Modal.allEffects (ActivatedAbility.modal ab)) || modalReadsAnnouncedX (ActivatedAbility.modal ab))
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
    let tokensOf face = concatMap (NonEmpty.toList . Card.Type.faces) [token | Effect.Create (Create.MkCreate _ token _ _ _) <- cardResolutionEffects face]
        tokens = concatMap (overFaces tokensOf . Printing.card) ps
    -- Guards the sweep against passing vacuously if Create ever moves out
    -- from under cardResolutionEffects.
    Spec.assertBool s (not (null tokens)) "the pool creates tokens"
    Spec.assertEqWith s "no token is misnamed" (fmap Face.name (filter tokenNameOffends tokens)) []
  Spec.it s "the lint itself catches a token named without the suffix" $ do
    doomedTraveler <- S.printingOf s registry "Doomed Traveler"
    case concatMap (NonEmpty.toList . Card.Type.faces) [token | Effect.Create (Create.MkCreate _ token _ _ _) <- cardResolutionEffects (S.combinedFace doomedTraveler)] of
      [token] -> do
        Spec.assertBool s (not (tokenNameOffends token)) "the real token passes"
        -- The exact misauthoring CR 111.4 forbids: the bare subtype, with
        -- the suffix dropped.
        Spec.assertBool s (tokenNameOffends token {Face.name = CardName.MkCardName $ Text.pack "Spirit"}) "misnamed token detected"
      other -> Spec.assertFailure s ("expected exactly one Create, got " <> show (length other))
  -- The same self-test one level down, and the proof that cardResolutionEffects
  -- is a CLOSURE rather than the carriers' flat concatenation: Inkshield's Create
  -- sits inside CR 615.5's rider on a spell's prevention, which no carrier limb
  -- reaches. Without effectNestedEffects the corpus sweep above saw no token here
  -- at all, so renaming this one to the bare "Inkling" CR 111.4 forbids left it
  -- green and only a gameplay assertion in Pawl.ReplacementSpec objected.
  Spec.it s "CR 111.4 the sweep reaches a token nested in a prevention rider" $ do
    inkshield <- S.printingOf s registry "Inkshield"
    case concatMap (NonEmpty.toList . Card.Type.faces) [token | Effect.Create (Create.MkCreate _ token _ _ _) <- cardResolutionEffects (S.combinedFace inkshield)] of
      [token] -> do
        Spec.assertBool s (not (tokenNameOffends token)) "the real token passes"
        Spec.assertBool s (tokenNameOffends token {Face.name = CardName.MkCardName $ Text.pack "Inkling"}) "misnamed token detected"
      other -> Spec.assertFailure s ("expected exactly one Create, got " <> show (length other))
  -- CR 111.9's exemption, and the proof it is not a hole: The Atropal is named by
  -- Tomb of Annihilation rather than by rule 111.4, and the SAME face stripped of
  -- its Legendary supertype is an offender -- so the exemption turns on rule
  -- 111.9's mark and nothing else.
  Spec.it s "CR 111.9 a legendary token is named by the card, not by CR 111.4" $ do
    tomb <- S.printingOf s registry "Tomb of Annihilation"
    case concatMap (NonEmpty.toList . Card.Type.faces) [token | Effect.Create (Create.MkCreate _ token _ _ _) <- cardResolutionEffects (S.combinedFace tomb)] of
      [token] -> do
        let typeLine = Face.typeLine token
            mundane = token {Face.typeLine = typeLine {TypeLine.supertypes = Set.delete Supertype.Legendary (TypeLine.supertypes typeLine)}}
        Spec.assertEqWith s "it is The Atropal" (Face.name token) (CardName.MkCardName (Text.pack "The Atropal"))
        Spec.assertBool s (not (tokenNameOffends token)) "the legendary token passes"
        Spec.assertBool s (tokenNameOffends mundane) "and the same face without Legendary does not"
      other -> Spec.assertFailure s ("expected exactly one Create, got " <> show (length other))
  -- The countdown shield's rider, the same limb one opcode over: Test of Faith
  -- hangs CR 615.5's counters off a PreventNextDamage where Inkshield hangs its
  -- tokens off a PreventAllDamage. No lint fires on a PutCounters, so this is
  -- what observes that arm at all.
  Spec.it s "CR 615.5 the sweep reaches a rider on the countdown shield" $ do
    testOfFaith <- S.printingOf s registry "Test of Faith"
    Spec.assertEqWith
      s
      "the shield's rider is swept"
      (length [() | Effect.PutCounters {} <- cardResolutionEffects (S.combinedFace testOfFaith)])
      1
  -- The closure's OTHER limb with a producer in the pool: CR 608.2f's body.
  -- Soulfire Eruption's exile is nested in a ForEach, so the four MoveToZone
  -- sweeps below (CR 406.3's face-down exile, CR 708.3's face-down entry, CR
  -- 401.2's owner-chosen end, and the plural binding) saw nothing of it before
  -- this closure, and no corpus card offends any of them from a nested position
  -- to say so.
  Spec.it s "CR 608.2f the sweep reaches an effect nested in a ForEach body" $ do
    soulfireEruption <- S.printingOf s registry "Soulfire Eruption"
    Spec.assertEqWith
      s
      "the body's move is swept"
      [zone | Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone _ _ _ _ _) <- cardResolutionEffects (S.combinedFace soulfireEruption)]
      [Zone.Exile]
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
          Effect.Destroy (Destroy.MkDestroy ref regenerability (Just _) mBuried mPermanents) -> Effect.Destroy (Destroy.MkDestroy ref regenerability (Just slot) mBuried mPermanents)
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
  -- pool names a reserved slot anywhere -- and every card that prints a
  -- CreateEmblem mints an emblem that names none either; the corpus sweeps are
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
                          (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot Binding.you) Regenerability.Regenerable (Just Binding.eventAmount) Nothing Nothing)))))
                          (Map.singleton Binding.you (TargetSlot.required Pool.AnyTarget Nothing))
                      )
                  )
                  (ModeSelection.ChooseExactly 1),
              TriggeredAbility.intervening = Nothing,
              TriggeredAbility.limit = TriggerLimit.Unlimited
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
          Effect.Create (Create.MkCreate quantity token riders slot creator) -> Effect.Create (Create.MkCreate quantity (onEveryFace arm token) riders slot creator)
          other -> other
        -- The same, on the BACK face of a two-faced token whose front is clean.
        armBackFace effect = case effect of
          Effect.Create (Create.MkCreate quantity token riders slot creator) ->
            let front = NonEmpty.head (Card.Type.faces token)
             in Effect.Create (Create.MkCreate quantity (token {Card.Type.faces = front NonEmpty.:| [arm front]}) riders slot creator)
          other -> other
        -- The same, on a minted EMBLEM in place of the token.
        armEmblem effect = case effect of
          Effect.Create {} -> Effect.CreateEmblem (onEveryFace arm (Printing.card piker))
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
    -- The emblem arm, which neither printed CreateEmblem exercises -- their
    -- emblems name no slot at all: the minting effect is swapped for a
    -- CreateEmblem carrying the same graft, over a card's faces.
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
  -- slot named there reaches the same evaluator (Pawl.Engine.Projection.View.seedCharacteristicPT
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
                      TriggeredAbility.intervening = Nothing,
                      TriggeredAbility.limit = TriggerLimit.Unlimited
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
  splitBoxSpec s registry
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

-- CR 709.4 for the boxes CR 709.4b and CR 709.4c do not reach: power, toughness,
-- loyalty, defense, and the characteristic-defining ability a printed star stands
-- for. Every producer is synthetic, because no printing carries any of them on a
-- split half -- Scryfall `is:split (t:creature or t:planeswalker or t:battle)` and
-- `is:split pow=*`, 2026-09-02, no hit -- nor `is:split is:permanent -t:room`,
-- so the split printings that ARE permanents are Rooms, which print no P/T box.
-- A printed split card with a creature half is what would refute that.
--
-- Every card but the twinned pair puts its box on the RIGHT half alone, which is
-- what makes those proofs rather than restatements: merge2 is a record UPDATE
-- over the left half, so a line that fell out of it -- or one written
-- `Face.power l` -- answers Nothing, and the permanent is a 0/0 (CR 208.5), a
-- planeswalker with no loyalty counters (CR 306.5b) or a battle with no defense
-- counters (CR 310.4b). Synthetic Twinned Colossus // Synthetic Twinned Titan
-- prints a P/T box on BOTH halves, each half defining one of the two boxes, which
-- is the pair CR 709.4c combines.
--
-- The precondition every case asserts on the board: the PERMANENT shows CR 709.4's
-- combined view and not CR 709.3b's single half. Naming the half is what casting
-- does (CR 709.3a), and the name is carried only while the spell is on the stack,
-- so a permanent that showed the cast half alone would read 3/4 here too and the
-- P/T assertion would prove nothing.
splitBoxSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
splitBoxSpec s registry = Spec.describe s "SplitBox" $ do
  Spec.it s "CR 709.4 the combined view has the one half's printed power and toughness" $ do
    plains <- S.printingOf s registry "Plains"
    printing <- S.printingOf s registry "Synthetic Stone Sentinel"
    case castHalf plains printing "Synthetic Stone Sentinel" of
      (_, Nothing) -> Spec.assertFailure s "expected one nonland permanent"
      (after, Just oid) -> do
        Spec.assertEqWith s "the right half's 3/4" (S.powerToughnessOf oid after) (Just (3, 4))
        -- The precondition, after the behaviour: CR 709.4a's two names and CR
        -- 709.4c's two card types are what say this is the combined view.
        Spec.assertEqWith s "and it is the halves combined" (Set.toList (PC.cardTypes (Projection.project oid after))) [CardType.Creature, CardType.Instant]
        Spec.assertEqWith s "under both names" (Set.size (Projection.namesOf oid after)) 2
  Spec.it s "CR 709.4c the combined view keeps the one half's P/T-defining ability" $ do
    mountain <- S.printingOf s registry "Mountain"
    printing <- S.printingOf s registry "Synthetic Mirror Colossus"
    case castHalf mountain printing "Synthetic Mirror Colossus" of
      (_, Nothing) -> Spec.assertFailure s "expected one nonland permanent"
      (after, Just oid) -> do
        -- CR 604.3 / 208.2a: the star stands for "power and toughness are each
        -- equal to this object's mana value", and CR 709.4b makes that the
        -- COMBINED cost, {1}{U} plus {2}{R}. Five, so the CDA and the combined
        -- cost are both load bearing -- the right half alone reads three.
        Spec.assertEqWith s "the combined mana value, 2 + 3" (S.powerToughnessOf oid after) (Just (5, 5))
        Spec.assertEqWith s "and it is the halves combined" (Set.size (Projection.namesOf oid after)) 2
  Spec.it s "CR 709.4c the combined view keeps a P/T-defining ability from EACH half" $ do
    forest <- S.printingOf s registry "Forest"
    printing <- S.printingOf s registry "Synthetic Twinned Colossus"
    case castHalf forest printing "Synthetic Twinned Colossus" of
      (_, Nothing) -> Spec.assertFailure s "expected one nonland permanent"
      (after, Just oid) -> do
        -- CR 709.4c gives the combined card "each ability in the text box of each
        -- half", and these two define DISJOINT boxes: the left half's star is in
        -- its power box and counts the four Forests castHalf put out, the right
        -- half's is in its toughness box and counts the one creature on the
        -- battlefield -- itself. CR 604.3 lets each override the number the other
        -- half prints in that box, which is a 2 both times, so every one of the
        -- four readings is a different number. Keeping the left half's ability
        -- alone reads 4/2.
        Spec.assertEqWith s "the left half's power and the right half's toughness" (S.powerToughnessOf oid after) (Just (4, 1))
        Spec.assertEqWith s "and it is the halves combined" (Set.size (Projection.namesOf oid after)) 2
  Spec.it s "CR 306.5b the combined view has the one half's printed loyalty" $ do
    forest <- S.printingOf s registry "Forest"
    printing <- S.printingOf s registry "Synthetic Warden Ascendant"
    case castHalf forest printing "Synthetic Warden Ascendant" of
      (_, Nothing) -> Spec.assertFailure s "expected one nonland permanent"
      (after, Just oid) -> do
        Spec.assertEqWith s "four loyalty counters" (S.counterOf CounterKind.Loyalty oid after) 4
        Spec.assertEqWith s "and the projection carries the printed number" (PC.loyalty (Projection.project oid after)) (Just (Loyalty.Literal 4))
        Spec.assertEqWith s "and it is the halves combined" (Set.size (Projection.namesOf oid after)) 2
  Spec.it s "CR 310.4b the combined view has the one half's printed defense" $ do
    swamp <- S.printingOf s registry "Swamp"
    printing <- S.printingOf s registry "Synthetic Border Skirmish"
    case castHalf swamp printing "Synthetic Border Skirmish" of
      (_, Nothing) -> Spec.assertFailure s "expected one nonland permanent"
      (after, Just oid) -> do
        Spec.assertEqWith s "five defense counters" (S.counterOf CounterKind.Defense oid after) 5
        Spec.assertEqWith s "and the projection carries the printed number" (PC.defense (Projection.project oid after)) (Just (Defense.MkDefense 5))
        Spec.assertEqWith s "and it is the halves combined" (Set.size (Projection.namesOf oid after)) 2

-- One named half of a split card cast from alice's hand and resolved, plus the
-- permanent that arrived -- a new object, since CR 400.7 mints one as the spell
-- moves. Four lands of one type, which pays every half these cases name.
--
-- Pawl.Support.cast is not usable here: it goes through soleFaceName, which errors
-- on a card offering two castable halves precisely so a split card cannot silently
-- cast the wrong one.
castHalf :: Printing.Printing -> Printing.Printing -> String -> (GameState.GameState, Maybe ObjectId.ObjectId)
castHalf land printing half =
  let (gs, oid) = S.handOne printing (S.landsInPlay land 4)
      name = CardName.MkCardName (Text.pack half)
      cast = S.runPure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice oid name Facing.FaceUp)
      after = S.runPure S.identityAnswer cast Stack.resolveTop
      nonLand o = not (Set.member CardType.Land (PC.cardTypes (Projection.project o after)))
   in ( after,
        case filter nonLand (Set.toList (GameState.battlefield after)) of
          [only] -> Just only
          _ -> Nothing
      )
