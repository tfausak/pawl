-- Pawl.Engine.Card's lints over where a filter atom may appear: the context-
-- relative atoms from CanHostSubject to Specific PlayerRef, each admitted only
-- at the positions whose context fills what it reads, and the lints that catch
-- one elsewhere. Split out of Pawl.CardSpec, which keeps the machinery and the
-- filter traversals these walk.
module Pawl.FilterPositionLintSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Pawl.CardSpec (Framing (AttachDestination, HandSweepFramed, InTargetSlot, KeywordFramed, MillTallyFramed, MintedTargetSlot, OutsideTheGameFramed, ReplacementRowFramed, SearchFramed, SlotlessCostFramed, SourceHostFramed, Unframed), anyFace, cardFilters, cardResolutionEffects, conditionFilters, counterKindFilters, durationFilters, effectFilters, entryRewriteFilters, filterSlotsReadSingly, framedSlotsReadSingly, keywordFilters, objectRefFilters, oneEffectTrigger, oneFaced, payGateFilters, quantityFilters, replacementEffectFilters, riderFilters, triggerConditionFilters, turnUpRewriteFilters)
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Cost as Cost.Codec
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.Face as Face.Codec
import qualified Pawl.Codec.Filter as Filter.Codec
import qualified Pawl.Codec.Keyword as Keyword.Codec
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackCostScope as AttackCostScope
import qualified Pawl.Types.BlockCost as BlockCost
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.CounterRestriction as CounterRestriction
import qualified Pawl.Types.CounterSubject as CounterSubject
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryFlip as EntryFlip
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Equip as Equip
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.HandAction as HandAction
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.PerCreature as PerCreature
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Sacrificer as Sacrificer
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneScope as ZoneScope

-- The zone set of every search a face authors (CR 701.23a). A wildcard rather
-- than one arm per effect, as storedPlayerScope takes: Pawl.Types.Effect is the
-- open half's alphabet, and a new opcode is not a new search.
searchZoneSets :: Face.Face Card.Type.Card -> [Set.Set Zone.Zone]
searchZoneSets = Maybe.mapMaybe zonesOf . cardResolutionEffects
  where
    zonesOf effect = case effect of
      Effect.Search search -> Just (Search.zones search)
      _ -> Nothing

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
  Filter.Type.HasKeyword keyword -> sum (fmap (canHostSubjects . snd) (keywordFilters keyword))
  -- CR 122.1b's keyword counter carries a whole Keyword, so a Filter can hide one
  -- level further down than the atom above -- and this lint is about the
  -- positions, not about which of them a card has used.
  Filter.Type.HasCounters kind -> case kind of
    CounterKind.Keyword keyword -> sum (fmap (canHostSubjects . snd) (keywordFilters keyword))
    CounterKind.PlusOnePlusOne -> 0
    CounterKind.MinusOneMinusOne -> 0
    CounterKind.Loyalty -> 0
    CounterKind.Lore -> 0
    CounterKind.Defense -> 0
    CounterKind.Time -> 0
    CounterKind.Fade -> 0
    CounterKind.Age -> 0
    CounterKind.Shield -> 0
    CounterKind.Finality -> 0
    CounterKind.Stun -> 0
    CounterKind.Level -> 0
    CounterKind.Hone -> 0
    CounterKind.Named _ -> 0
  -- Zero for the reason the FAMILY below is: this atom is payload-free, so there
  -- is no Filter position inside it for a card author to reach -- the descent
  -- above exists only because a CounterKind can carry a Keyword.
  Filter.Type.HasCountersOfAnyKind -> 0
  -- Zero and not a descent, unlike the atom above: a family is payload-free, so
  -- there is no Filter position inside it for a card author to reach.
  Filter.Type.HasKeywordFamily _ -> 0
  Filter.Type.HasCardType _ -> 0
  Filter.Type.HasSupertype _ -> 0
  Filter.Type.HasColor _ -> 0
  Filter.Type.HasSubtype _ -> 0
  Filter.Type.HasName _ -> 0
  Filter.Type.PowerAtLeast _ -> 0
  Filter.Type.PowerAtMost _ -> 0
  Filter.Type.ToughnessGreaterThanPower -> 0
  Filter.Type.PowerLessThanSource -> 0
  Filter.Type.PowerGreaterThanSource -> 0
  Filter.Type.PowerIsAmountInSlot _ -> 0
  Filter.Type.PowerAtLeastAmountInSlot _ -> 0
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
  Filter.Type.ManaValueAtMostAmount -> 0
  Filter.Type.ControlledBy _ -> 0
  -- Zero for ControlledBy's reason: CR 108.3's owner atom carries a
  -- PlayerRelation, which holds no Filter for a card author to reach.
  Filter.Type.OwnedBy _ -> 0
  Filter.Type.IsSource -> 0
  Filter.Type.TargetsSource -> 0
  Filter.Type.TargetsOnlySource -> 0
  -- A DESCENT and not a zero, for AttachedTo's reason below: CR 115.1's atom
  -- carries the one target's description, a Filter position a card author
  -- writes into like any other.
  Filter.Type.TargetsOnlyOne f -> canHostSubjects f
  Filter.Type.TargetsPlayer _ -> 0
  Filter.Type.IsPlayer _ -> 0
  Filter.Type.IsBound _ -> 0
  Filter.Type.SameNameAsBound _ -> 0
  Filter.Type.SameControllerAsBound _ -> 0
  Filter.Type.HasChosenName -> 0
  Filter.Type.OfChosenPlayer -> 0
  Filter.Type.IsControllerOfBound _ -> 0
  -- Zero for the nullary atoms' reason, a payload over: CR 400.1's card count is
  -- a Natural, which holds no Filter for a card author to reach.
  Filter.Type.CardsInGraveyardAtLeast _ -> 0
  Filter.Type.IsAttacking -> 0
  Filter.Type.IsAttackingPlayer _ -> 0
  Filter.Type.IsAttackingPlaneswalker _ -> 0
  Filter.Type.IsAttackingBattle _ -> 0
  Filter.Type.DeclaredAttackedThisCombat -> 0
  Filter.Type.IsBlocking -> 0
  Filter.Type.IsBlocked -> 0
  Filter.Type.AttackedThisTurn -> 0
  Filter.Type.DeclaredAttackerThisCombat -> 0
  Filter.Type.DeclaredBlockerThisCombat -> 0
  Filter.Type.MilledThisTurn -> 0
  Filter.Type.DealtDamageThisTurn -> 0
  -- A DESCENT and not a zero, unlike every other atom here: CR 303.4's atom
  -- carries the host's description, which a card author writes exactly as they
  -- write any other filter. Zero would under-count against jsonAtoms, which counts
  -- the tag at any depth, and turn a legitimate card into a reported offence.
  Filter.Type.AttachedTo f -> canHostSubjects f
  -- A DESCENT for the atom above's reason, the nest describing the ATTACHER: CR
  -- 303.4b's atom is a Filter position a card author writes into like any other.
  Filter.Type.HasAttached f -> canHostSubjects f
  Filter.Type.IsAttachedToSource -> 0
  Filter.Type.IsHostOfSource -> 0
  -- Zero: the MIRROR atom is not this one, and its own lint counts it through the
  -- codec (canAttachToSubjectCounts) rather than through this recursion.
  Filter.Type.CanAttachToSubject -> 0
  Filter.Type.IsToken -> 0
  Filter.Type.IsActivatedAbility -> 0
  Filter.Type.IsAbility -> 0
  Filter.Type.IsEmblem -> 0
  -- A DESCENT for RepresentedByCard's reason below, the nest describing the
  -- ability's SOURCE.
  Filter.Type.FromSource f -> canHostSubjects f
  Filter.Type.IsTapped -> 0
  Filter.Type.IsFaceDown -> 0
  -- A DESCENT for AttachedTo's reason, the nest describing the CARD representing
  -- the candidate: CR 708.12's atom is a Filter position a card author writes
  -- into like any other.
  Filter.Type.RepresentedByCard f -> canHostSubjects f
  Filter.Type.IsExiledFaceDown -> 0
  Filter.Type.Transformed -> 0
  Filter.Type.HasNonManaActivatedAbility -> 0
  Filter.Type.HasActivatedAbility -> 0
  Filter.Type.IsInZone _ -> 0
  Filter.Type.WasCastFrom _ -> 0
  Filter.Type.IsRingBearer -> 0
  Filter.Type.HasDesignation _ -> 0

-- Does this position's evaluator supply CR 303.4b's host? Two constructors
-- answer yes, for the reason ReplacementRowFramed gives.
hostFramed :: Framing -> Bool
hostFramed framing = case framing of
  SourceHostFramed -> True
  ReplacementRowFramed -> True
  Unframed -> False
  AttachDestination -> False
  InTargetSlot -> False
  SearchFramed -> False
  OutsideTheGameFramed -> False
  KeywordFramed -> False
  -- Pawl.Engine.Filter.contextFor leaves `sourceAttachedTo` empty too, so a CR
  -- 303.4b question is as unanswerable here as a slot one.
  SlotlessCostFramed -> False
  -- Pawl.Engine.Target.slotContext leaves it empty as well, so the minted equip
  -- ability's quality cannot ask a CR 303.4b question either.
  MintedTargetSlot -> False
  -- Pawl.Engine.Resolve.Slots.effectContext fills it only where the caller
  -- overlays it (Resolve.Slots.objectRefObjects), and the mill arm does not, so
  -- the tally cannot ask a CR 303.4b question either.
  MillTallyFramed -> False
  -- The hand sweep is the one ObjectRef position whose arm overlays no
  -- `sourceAttachedTo`: no card in a hand names what its Aura's host is
  -- (Pawl.Engine.Resolve.Slots.objectRefObjects).
  HandSweepFramed -> False

-- How many CR 701.3a atoms this card carries in an attach opcode's destination
-- filter -- Effect.AttachTarget's or Effect.AttachTargetToEach's -- and how many
-- anywhere else. The second number is the offence; the first is what Aura Graft
-- and Synthetic Aura Diffusion legitimately have one of each.
canHostSubjectCounts :: Face.Face Card.Type.Card -> (Int, Int)
canHostSubjectCounts card =
  let total wanted = sum [canHostSubjects f | (framing, f) <- cardFilters card, (framing == AttachDestination) == wanted]
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
-- Parameterized because several atoms want it: CR 701.3a's and CR 709.4a's,
-- counted here for their traversal cross-checks, and CR 702.134a's
-- Filter.PowerLessThanSource, which no card may carry at all.
jsonAtoms :: Text.Text -> Value.Value -> Int
jsonAtoms tag value = case value of
  Value.String s -> if String.unwrap s == tag then 1 else 0
  Value.Array a -> sum (fmap (jsonAtoms tag) (Array.unwrap a))
  Value.Object o -> sum (fmap (jsonAtoms tag . Pair.value) (Object.unwrap o))
  Value.Null _ -> 0
  Value.Boolean _ -> 0
  Value.Number _ -> 0

-- The CR 202.3 computed-bound tag, spelled once.
manaValueAtMostAmountTag :: Text.Text
manaValueAtMostAmountTag = Text.pack "ManaValueAtMostAmount"

-- How many CR 202.3 computed-bound atoms sit inside a target slot that NAMES an
-- amount, counted off the encoding rather than off cardFilters: the position this
-- atom needs is a property of the enclosing SLOT rather than of the Framing, and
-- a Framing tag cannot say which slot a filter came out of.
--
-- "pool" is the key that identifies a target slot -- Pawl.Codec.TargetSlot is the
-- only codec in the tree that writes one -- and "amount" beside it is the slot
-- naming its bound. Only that slot's own "filter" subtree is counted, which is
-- sound because a Filter holds no TargetSlot: nothing nests below it to be
-- double-counted.
amountedSlotAtoms :: Value.Value -> Int
amountedSlotAtoms value = case value of
  Value.Array a -> sum (fmap amountedSlotAtoms (Array.unwrap a))
  Value.Object o ->
    let pairs = Object.unwrap o
        keyed k = [Pair.value pair | pair <- pairs, String.unwrap (Pair.name pair) == Text.pack k]
        here =
          if null (keyed "pool") || null (keyed "amount")
            then 0
            else sum (fmap (jsonAtoms manaValueAtMostAmountTag) (keyed "filter"))
     in here + sum (fmap (amountedSlotAtoms . Pair.value) pairs)
  Value.String _ -> 0
  Value.Null _ -> 0
  Value.Boolean _ -> 0
  Value.Number _ -> 0

-- How many CR 202.3 computed-bound atoms this card carries in a target slot that
-- names an amount, and how many anywhere else. The second number is the offence;
-- the first is what Celestine, the Living Saint legitimately has one of.
--
-- Pawl.Engine.Target.slotContext fills Filter.Context.slotAmount off the SLOT's
-- own Quantity, so the atom in a slot that names none -- or in a Count filter, an
-- affected set, a search filter, a cost criterion -- is a silent False rather than
-- a rejected card. This is where that is made loud.
manaValueAtMostAmountCounts :: Face.Face Card.Type.Card -> (Int, Int)
manaValueAtMostAmountCounts card =
  let encoded = Codec.encode (Face.Codec.codec Card.codec) card
      slotted = amountedSlotAtoms encoded
   in (slotted, jsonAtoms manaValueAtMostAmountTag encoded - slotted)

manaValueAtMostAmountOffends :: Face.Face Card.Type.Card -> Bool
manaValueAtMostAmountOffends card = snd (manaValueAtMostAmountCounts card) /= 0

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

-- jsonAtoms narrowed from a whole face to ONE Filter position of it. The atom
-- this is asked about carries a payload (a SlotName), so counting the tag by hand
-- would mean a second copy of canHostSubjects' whole recursion; Pawl.Codec.Filter
-- already walks every arm of it, including the ones a hand-written recursion has
-- dropped before (landwalk's Subtype, now a Filter).
--
-- Sound as a per-position count because a TargetSlot's codec embeds its Filter's
-- encoding verbatim, so the sum over cardFilters' positions and jsonAtoms over
-- the whole encoded face are counting the same occurrences.
filterAtoms :: Text.Text -> Filter.Type.Filter Keyword.Keyword -> Int
filterAtoms tag = jsonAtoms tag . Codec.encode (Filter.Codec.codec Keyword.Codec.codec)

-- The CR 701.3a candidate-side tag, spelled once.
canAttachToSubjectTag :: Text.Text
canAttachToSubjectTag = Text.pack "CanAttachToSubject"

-- How many CR 701.3a candidate-side atoms this card carries inside a SEARCH's
-- filter, and how many anywhere else. The second number is the offence; the first
-- is what Auratouched Mage legitimately has one of.
canAttachToSubjectCounts :: Face.Face Card.Type.Card -> (Int, Int)
canAttachToSubjectCounts card =
  let total wanted = sum [filterAtoms canAttachToSubjectTag f | (framing, f) <- cardFilters card, (framing == SearchFramed) == wanted]
   in (total True, total False)

-- Filter.CanAttachToSubject is answerable only where the evaluator supplies the
-- fixed host, and Pawl.Engine.Resolve's Effect.Search arm is the only one that
-- does. Written into a target slot, an affected set, a Count filter or an attach
-- destination it is a silent False rather than a rejected card, exactly as
-- Filter.CanHostSubject is outside an attach. This is where that is made loud.
--
-- Two offences under one name, for canHostSubjectOffends' two reasons: the
-- traversal found the atom outside a search, or the traversal and the codec
-- disagree about how many the card holds -- the second being a blind spot in
-- cardFilters, in which an atom would be reported as zero rather than as an
-- offence.
canAttachToSubjectOffends :: Face.Face Card.Type.Card -> Bool
canAttachToSubjectOffends card =
  let (framed, unframedCount) = canAttachToSubjectCounts card
   in unframedCount /= 0 || framed + unframedCount /= jsonAtoms canAttachToSubjectTag (Codec.encode (Face.Codec.codec Card.codec) card)

-- The CR 201.4 chosen-name tag, spelled once.
hasChosenNameTag :: Text.Text
hasChosenNameTag = Text.pack "HasChosenName"

-- How many CR 201.4 chosen-name atoms this card carries inside one of the two
-- positions that overlay Filter.Context.sourceChosenNames -- a CR 701.23 search's
-- filter or a CR 701.17 mill's tally -- and how many anywhere else. The second
-- number is the offence; the first is what Ancient Vendetta and Predict
-- legitimately have one each of.
hasChosenNameCounts :: Face.Face Card.Type.Card -> (Int, Int)
hasChosenNameCounts card =
  let total wanted = sum [filterAtoms hasChosenNameTag f | (framing, f) <- cardFilters card, elem framing [SearchFramed, MillTallyFramed] == wanted]
   in (total True, total False)

-- CR 201.4's chosen name is answerable only where Filter.Context.sourceChosenNames
-- is filled, and two sites a CARD can reach fill it: Pawl.Engine.Resolve.Effect's
-- Effect.Search arm and its Effect.Mill arm's tally, each overlaying the field on
-- the resolution's own context -- Pawl.Engine.Replacement.candidateContext is the
-- third, and rule 702.16e's minted shield is the only filter written there.
-- Filter.contextFor, Filter.contextWithSlots, Filter.contextComparingPower and
-- Pawl.Engine.Target.admittedGiven all leave it empty, so Filter.HasChosenName in
-- a target slot, an affected set, a Count filter or a cost criterion is a silent
-- False rather than a rejected card. This is where that is made loud.
--
-- SearchFramed is the framing canAttachToSubjectOffends fences, and for a
-- different rule: that atom needs the search arm's own VIEW as well as its
-- context, so it is admitted in that one position where this atom is admitted in
-- two.
--
-- Two offences under one name, for canHostSubjectOffends' two reasons: the
-- traversal found the atom outside those two positions, or the traversal and the codec
-- disagree about how many the card holds -- the second being a blind spot in
-- cardFilters, in which an atom would be reported as zero rather than as an
-- offence. Unlike its three siblings' the second disjunct is PROVED here rather
-- than a regression fence: Effect.ChooseCardName's own restriction is a Filter
-- position cardFilters walks, so the self-test's fixture for it goes to (0, 0)
-- when effectFilters stops reporting it and the codec half is what still catches
-- the atom.
hasChosenNameOffends :: Face.Face Card.Type.Card -> Bool
hasChosenNameOffends card =
  let (framed, elsewhere) = hasChosenNameCounts card
   in elsewhere /= 0 || framed + elsewhere /= jsonAtoms hasChosenNameTag (Codec.encode (Face.Codec.codec Card.codec) card)

-- The CR 702.16k chosen-player tag, spelled once.
ofChosenPlayerTag :: Text.Text
ofChosenPlayerTag = Text.pack "OfChosenPlayer"

-- How many CR 702.16k chosen-player atoms this card carries inside a KEYWORD's
-- own filter, and how many anywhere else. The second number is the offence; the
-- first is what True-Name Nemesis legitimately has one of.
ofChosenPlayerCounts :: Face.Face Card.Type.Card -> (Int, Int)
ofChosenPlayerCounts card =
  let total wanted = sum [filterAtoms ofChosenPlayerTag f | (framing, f) <- cardFilters card, (framing == KeywordFramed) == wanted]
   in (total True, total False)

-- CR 702.16k's chosen player is answerable only where
-- Filter.Context.carrierChosenPlayer is filled, and the four fillers are the four
-- positions rule 702.16 reads a protection QUALITY in
-- (Pawl.Engine.Replacement.candidateContext, Pawl.Engine.Target.targetable,
-- Pawl.Engine.AttachRestriction.refusesGiven,
-- Pawl.Engine.CombatRestriction.cantBeBlockedBy). Every one of them takes the
-- filter off a keyword, so a target slot, an affected set, a Count filter or a
-- cost criterion asking this atom is a silent False rather than a rejected card.
-- This is where that is made loud.
--
-- Every one of them takes the filter off a keyword AS A KEYWORD, which is why
-- CR 702.6c's equip quality is not in this bucket: rule 702.6a's minted ability
-- carries that payload into a target slot answered by
-- Pawl.Engine.Target.slotContext, which sets carrierChosenPlayer to Nothing. It
-- arrives as MintedTargetSlot rather than KeywordFramed and so offends here,
-- which is the point of that framing; the "CR 702.16k an equip quality is not a
-- protection quality" case below is what proves it.
--
-- The frame is KEYWORD rather than PROTECTION, which is wider than rule 702.16k
-- alone, and the width cuts two ways. CR 702.11d's hexproof quality is the
-- harmless half: Pawl.Engine.Target.targetable evaluates it against the very
-- Context that fills the field, so a hexproof-from-the-chosen-player card would
-- be answerable in that one position too.
--
-- CR 702.14a's landwalk is the other half, and this frame admits it while nothing
-- answers it: Pawl.Engine.Combat.landwalkAllowsGiven matches the criterion
-- through a plain Filter.contextFor, which leaves carrierChosenPlayer Nothing, so
-- `Landwalk OfChosenPlayer` would pass this lint and be vacuously False. No
-- printing can reach it -- rule 702.14a's quality is "usually a land type, but it
-- can also be the card type land plus any combination of land types, card types,
-- and/or supertypes", a list with no player on it -- so the hole is unreachable
-- rather than merely unoccupied. Narrowing the fence to Keyword.Protection is not
-- a one-line change: `keywordFramed` tags every keyword's filter alike, and
-- telling them apart wants a Framing this type does not draw.
--
-- Two offences under one name, for hasChosenNameOffends' two reasons: the
-- traversal found the atom outside a keyword, or the traversal and the codec
-- disagree about how many the card holds.
ofChosenPlayerOffends :: Face.Face Card.Type.Card -> Bool
ofChosenPlayerOffends card =
  let (framed, elsewhere) = ofChosenPlayerCounts card
   in elsewhere /= 0 || framed + elsewhere /= jsonAtoms ofChosenPlayerTag (Codec.encode (Face.Codec.codec Card.codec) card)

-- The CR 709.4a tag, spelled once.
sameNameAsBoundTag :: Text.Text
sameNameAsBoundTag = Text.pack "SameNameAsBound"

-- How many CR 709.4a atoms this card carries in one of the three ADMITTED
-- positions -- a MODE's target slot filter (Harness the Storm), a CR 701.23
-- search filter (Bifurcate) or a CR 608.2c hand sweep's (Hour of Glory) -- and
-- how many anywhere else. The second number is the offence.
--
-- An ALLOWLIST rather than "wherever Filter.Context.slotNames is filled", which
-- since #2141's search half is the wider set: every position a resolution
-- reaches through Pawl.Engine.Resolve.Slots.effectContext has the names -- an
-- ObjectRef's own affected set among them, which is SourceHostFramed inside an
-- effect (Resolve.battlefieldMatching). So this rejects the atom in positions
-- that would in fact answer, and only in that direction -- widen it when a card
-- wants one of them, which is the posture sweptForSingularSlots takes at
-- SlotlessCostFramed and what the hand sweep's own entry is.
sameNameAsBoundCounts :: Face.Face Card.Type.Card -> (Int, Int)
sameNameAsBoundCounts card =
  let total wanted = sum [filterAtoms sameNameAsBoundTag f | (framing, f) <- cardFilters card, elem framing [InTargetSlot, SearchFramed, HandSweepFramed] == wanted]
   in (total True, total False)

-- CR 709.4a's bound-name comparison is answerable only where
-- Filter.Context.slotNames is filled, which two callers do:
-- Pawl.Engine.Target.admittedGiven, matching a MODE's target slot Filter, and
-- Pawl.Engine.Resolve.Slots.effectContext, which all but one of a resolution's
-- positions go through -- the search filter and the mill tally among them.
-- Filter.contextFor and Filter.contextComparingPower leave it empty, so
-- Filter.SameNameAsBound in a Count filter, an affected set or a cost criterion
-- is a silent False rather than a rejected card. This is where that is made loud.
--
-- The two positions sameNameAsBoundCounts admits are narrower than that, on
-- purpose: see its own note. So a card rejected here is not necessarily one the
-- engine would answer wrong.
--
-- Two offences under one name, for canHostSubjectOffends' two reasons: the
-- traversal found the atom outside those two positions, or the traversal and the codec
-- disagree about how many the card holds -- the second being a blind spot in
-- cardFilters, in which an atom would be reported as zero rather than as an
-- offence.
--
-- The second disjunct is a REGRESSION FENCE rather than a proved behaviour, as
-- canHostSubjectOffends' is: every position a fixture can reach today is caught by
-- the first, so neutralizing the cross-check leaves the suite green. What would
-- observe it is a future Face field cardFilters forgets, which is the thing that
-- cannot be written down yet.
sameNameAsBoundOffends :: Face.Face Card.Type.Card -> Bool
sameNameAsBoundOffends card =
  let (slotted, elsewhere) = sameNameAsBoundCounts card
   in elsewhere /= 0 || slotted + elsewhere /= jsonAtoms sameNameAsBoundTag (Codec.encode (Face.Codec.codec Card.codec) card)

-- The CR 110.2 tag, spelled once.
sameControllerAsBoundTag :: Text.Text
sameControllerAsBoundTag = Text.pack "SameControllerAsBound"

-- How many CR 110.2 shared-controller atoms this card carries inside a MODE's
-- target slot filter, and how many anywhere else. The second number is the
-- offence; the first is what Bioshift legitimately has one of.
sameControllerAsBoundCounts :: Face.Face Card.Type.Card -> (Int, Int)
sameControllerAsBoundCounts card =
  let total wanted = sum [filterAtoms sameControllerAsBoundTag f | (framing, f) <- cardFilters card, (framing == InTargetSlot) == wanted]
   in (total True, total False)

-- CR 110.2's shared-controller comparison is answerable only where
-- Filter.Context.slotControllers is filled, and Pawl.Engine.Target.slotContext --
-- the one site that matches a MODE's target slot Filter -- is the one site that
-- fills it. This is sameNameAsBoundOffends' sweep one characteristic over, and
-- the one place the two differ is what makes it load-bearing rather than tidy:
-- that atom is a silent False outside its position and this one is a silent TRUE,
-- so an atom in a Count filter, an affected set or a search filter would ADMIT
-- what the card excludes rather than nothing at all.
--
-- Two offences under one name, sameNameAsBoundOffends' two: the traversal found
-- the atom outside a target slot, or the traversal and the codec disagree about
-- how many the card holds. The second disjunct is a REGRESSION FENCE for that
-- function's reason.
sameControllerAsBoundOffends :: Face.Face Card.Type.Card -> Bool
sameControllerAsBoundOffends card =
  let (slotted, elsewhere) = sameControllerAsBoundCounts card
   in elsewhere /= 0 || slotted + elsewhere /= jsonAtoms sameControllerAsBoundTag (Codec.encode (Face.Codec.codec Card.codec) card)

-- The CR 303.4b tag, spelled once.
hostOfSourceTag :: Text.Text
hostOfSourceTag = Text.pack "IsHostOfSource"

-- How many CR 303.4b atoms this card carries in a position whose evaluator
-- supplies the source's host, and how many anywhere else. The second number is the
-- offence; the first is what Ray of Frost legitimately has three of.
hostOfSourceCounts :: Face.Face Card.Type.Card -> (Int, Int)
hostOfSourceCounts card =
  let total wanted = sum [filterAtoms hostOfSourceTag f | (framing, f) <- cardFilters card, hostFramed framing == wanted]
   in (total True, total False)

-- CR 303.4b's "enchanted" is answerable only where Filter.Context.sourceAttachedTo
-- is filled, which is the positions `hostFramed` admits and nothing else:
-- Filter.contextFor, Filter.contextWithSlots and
-- Filter.contextComparingPower all leave it Nothing, so Filter.IsHostOfSource in an
-- affected set, a target slot or a search filter is a silent False rather than a
-- rejected card. This is where that is made loud.
--
-- Two offences under one name, for canHostSubjectOffends' two reasons: the
-- traversal found the atom outside those three positions, or the traversal and the
-- codec disagree about how many the card holds -- the second being a blind spot in
-- cardFilters, in which an atom would be reported as zero rather than as an
-- offence. The second disjunct is a REGRESSION FENCE for sameNameAsBoundOffends'
-- reason.
hostOfSourceOffends :: Face.Face Card.Type.Card -> Bool
hostOfSourceOffends card =
  let (framed, elsewhere) = hostOfSourceCounts card
   in elsewhere /= 0 || framed + elsewhere /= jsonAtoms hostOfSourceTag (Codec.encode (Face.Codec.codec Card.codec) card)

-- The CR 115.10a bound-object tag, spelled once.
isBoundTag :: Text.Text
isBoundTag = Text.pack "IsBound"

-- How many CR 115.10a bound-object atoms this card carries in a position with no
-- resolution behind it -- a WISH's filter or CR 201.4a's naming restriction,
-- which share OutsideTheGameFramed; a KEYWORD's own; a COST paid with no
-- announcement behind it; or the target slot of an ability rule 702 mints --
-- and how many anywhere else. The FIRST
-- number is the offence here, which is the sibling position lints turned around:
-- the atom is answerable in every position whose evaluator supplies the
-- resolution's slots, and unanswerable in the four that supply none.
isBoundCounts :: Face.Face Card.Type.Card -> (Int, Int)
isBoundCounts card =
  let total wanted = sum [filterAtoms isBoundTag f | (framing, f) <- cardFilters card, elem framing [OutsideTheGameFramed, KeywordFramed, SlotlessCostFramed, MintedTargetSlot] == wanted]
   in (total True, total False)

-- CR 400.11c's candidates are cards outside the game, which no spell or ability
-- affects, so no slot of the resolution names one -- and
-- Pawl.Engine.Projection.View.viewOfCard, the view they are matched through, fills no
-- `identity` for Filter.IsBound to compare in any case. The atom in a wish's
-- filter is therefore a silent False rather than a rejected card. CR 201.4a's
-- naming restriction is matched through the same view by
-- Pawl.Interpreter.legalCardName, so it is silent there too. This is where that
-- is made loud.
--
-- Two offences under one name, for hostOfSourceOffends' two reasons turned
-- around: the traversal found the atom inside a wish's filter, or the traversal
-- and the codec disagree about how many the card holds. The second disjunct is a
-- REGRESSION FENCE for sameNameAsBoundOffends' reason.
--
-- Only IsBound, of the atoms that read a Context's slots. SameNameAsBound is
-- fenced to a mode's target slot by its own lint, so a wish's filter already
-- rejects it; IsControllerOfBound and ControlledByBound are False wherever
-- Pawl.Engine.Filter.matches reaches them -- a fact about every position but a
-- Scope.OverPlayers count's, not about this one.
isBoundOffends :: Face.Face Card.Type.Card -> Bool
isBoundOffends card =
  let (wished, elsewhere) = isBoundCounts card
   in wished /= 0 || wished + elsewhere /= jsonAtoms isBoundTag (Codec.encode (Face.Codec.codec Card.codec) card)

-- The CR 702.33d per-cost tally's codec tag, spelled once.
timesKickedWithTag :: Text.Text
timesKickedWithTag = Text.pack "TimesKickedWith"

-- Every cost a Quantity.TimesKickedWith names in an ENCODED face, as the codec
-- wrote it. Read off the encoding for canHostSubjectOffends' reason turned to a
-- payload: this module's three Quantity traversals answer in Counts and Filters,
-- neither of which can carry a Cost, so the arm is invisible to all three.
--
-- The tagged arm and not the bare tag, jsonAtoms' distinction: only an object
-- whose "type" is the tag contributes, and what it contributes is the "value"
-- beside it -- Pawl.JsonCodec.Common's shape for an Arm.payload.
timesKickedWithCosts :: Value.Value -> [Value.Value]
timesKickedWithCosts value = case value of
  Value.Array a -> concatMap timesKickedWithCosts (Array.unwrap a)
  Value.Object o ->
    let pairs = Object.unwrap o
        keyed k = [Pair.value pair | pair <- pairs, String.unwrap (Pair.name pair) == Text.pack k]
        tagged = [() | Value.String t <- keyed "type", String.unwrap t == timesKickedWithTag]
        here = if null tagged then [] else keyed "value"
     in here <> concatMap (timesKickedWithCosts . Pair.value) pairs
  Value.String _ -> []
  Value.Null _ -> []
  Value.Boolean _ -> []
  Value.Number _ -> []

-- CR 702.33d's per-cost tally, which the rule makes payable more than once for a
-- multikicker. Pawl.Engine.Quantity joins TimesKickedWith's Cost to
-- Pawl.Engine.Cast's tally by structural equality, and that tally is keyed by
-- Pawl.Engine.Keyword.kickerCosts of the face's own keywords -- so a cost this
-- face never prints, or the same symbols in another order, reads 0 for the life
-- of the game with nothing red. This is where that is made loud.
--
-- Compared as ENCODINGS rather than as Costs so the two sides come off the one
-- codec Pawl.CodecIntegrationSpec round-trips; the Cost's own Eq is what the
-- engine joins on, and the codec is injective, so the two agree.
--
-- Stated about the PRINTED keywords, which is what a card file can be wrong
-- about. Nothing in data/cards/ grants one for a CR 613 projection to add
-- instead: each of the six Kicker or Multikicker occurrences sits at a face's own
-- `keywords`, 2026-09-02.
timesKickedWithOffends :: Face.Face Card.Type.Card -> Bool
timesKickedWithOffends card =
  let printed = fmap (Codec.encode (Cost.Codec.codec Keyword.Codec.codec) . fst) (Keyword.Engine.kickerCosts (Face.keywords card))
   in any (`notElem` printed) (timesKickedWithCosts (Codec.encode (Face.Codec.codec Card.codec) card))

-- Two things a TriggerCondition.AnyOf may not contain, checked at every depth so
-- that a nested one cannot smuggle either in.
--
-- A CR 603.8 STATE trigger, because the two kinds of condition are gathered by
-- different scans: Pawl.Engine.Event.Trigger.stateTriggers walks the battlefield asking
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

filterPositionLintSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
filterPositionLintSpec s registry = Spec.describe s "Lint" $ do
  -- CR 603.2's event triggers and CR 603.8's state triggers are gathered by two
  -- different scans, so one ability may not be both. See anyOfOffends for the two
  -- shapes this rejects and why each would be incoherent rather than merely odd.
  --
  -- Swept over the pool, with the non-vacuity assertion its neighbours carry: a
  -- pool with no AnyOf at all would pass this without examining anything.
  -- The pool's AnyOf cards -- Balemurk Leech, Bartered Cow and their like -- are
  -- all ACCEPTED here.
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
    Spec.assertEqWith s "the atom sits only in an attach opcode's destination" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the cards that do are ACCEPTED
    -- here rather than skipped. Aura Graft's "another permanent it can enchant"
    -- is the legal use, so a lint that swept past it would be indistinguishable
    -- from one that swept past everything.
    graft <- S.printingOf s registry "Aura Graft"
    Spec.assertEqWith
      s
      "Aura Graft's one atom is framed by its own attach"
      (canHostSubjectCounts (S.combinedFace graft))
      (1, 0)
    -- The same accepting read for the OTHER opcode that frames one, so a
    -- cardFilters arm that forgot AttachTargetToEach would fail here rather than
    -- silently counting its atom as zero.
    diffusion <- S.printingOf s registry "Synthetic Aura Diffusion"
    Spec.assertEqWith
      s
      "Synthetic Aura Diffusion's atom is framed by its CR 303.4d attach"
      (canHostSubjectCounts (S.combinedFace diffusion))
      (1, 0)
    Spec.assertEqWith
      s
      "and those two cards are the whole of data/cards' authorship of it"
      (sum (fmap (uncurry (+) . canHostSubjectCounts . S.combinedFace) ps))
      2
    -- The traversal reaches a Filter position no effect, target slot or affected
    -- set would have led it to: CR 702.29e's typecycling predicate, on a real
    -- card. Its absence would not show up in the sweep above, because Ash Barrens
    -- does not author the atom -- only in this. KeywordFramed and not Unframed:
    -- the tag is applied by keywordFilters at the leaf, so a printed keyword's
    -- own filter carries it whatever field quoted the keyword.
    barrens <- S.printingOf s registry "Ash Barrens"
    Spec.assertBool
      s
      ( elem
          (KeywordFramed, Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.HasSupertype Supertype.Basic])
          (cardFilters (S.combinedFace barrens))
      )
      "CR 702.29e landcycling's filter is a position the sweep walks"
  -- CR 701.3a from the candidate's side: Filter.CanAttachToSubject is answerable
  -- only where the evaluator supplies the fixed host, which is a search's filter
  -- and nothing else. See canAttachToSubjectOffends for the two offences.
  Spec.it s "CR 701.3a no card asks CanAttachToSubject outside a search's filter" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace canAttachToSubjectOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only in a search's filter" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the one card that does is
    -- ACCEPTED here rather than skipped.
    mage <- S.printingOf s registry "Auratouched Mage"
    Spec.assertEqWith
      s
      "Auratouched Mage's one atom is framed by its own search"
      (canAttachToSubjectCounts (S.combinedFace mage))
      (1, 0)
    Spec.assertEqWith
      s
      "and it is the pool's only one"
      (sum (fmap (uncurry (+) . canAttachToSubjectCounts . S.combinedFace) ps))
      1
    -- The unframed side has room to spare, so a Framing that had stopped marking
    -- searches would fail here rather than pass the sweep above by iterating over
    -- nothing.
    let positions = concatMap (cardFilters . S.combinedFace) ps
    Spec.assertBool s (length (filter ((== SearchFramed) . fst) positions) > 5) "the pool gives the accepted side search filters to be about"
  -- CR 201.4's chosen name is CR 701.3a's atom in nearly the same frame:
  -- answerable only where a resolution overlays the field, which
  -- Pawl.Engine.Resolve.Effect's Effect.Search arm and its Effect.Mill tally each
  -- do, and a silent False everywhere else. See hasChosenNameOffends for the two
  -- offences.
  Spec.it s "CR 201.4 no card asks HasChosenName outside a search's filter or a mill's tally" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace hasChosenNameOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only where the resolution overlays the chosen names" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and BOTH cards that do are ACCEPTED
    -- here rather than skipped -- one per admitted position, so a framing that
    -- stopped marking either would redden.
    vendetta <- S.printingOf s registry "Ancient Vendetta"
    Spec.assertEqWith
      s
      "Ancient Vendetta's one atom is framed by its own search"
      (hasChosenNameCounts (S.combinedFace vendetta))
      (1, 0)
    predict <- S.printingOf s registry "Predict"
    Spec.assertEqWith
      s
      "Predict's one atom is framed by its own mill tally"
      (hasChosenNameCounts (S.combinedFace predict))
      (1, 0)
    Spec.assertEqWith
      s
      "and the two are the pool's only ones"
      (sum (fmap (uncurry (+) . hasChosenNameCounts . S.combinedFace) ps))
      2
  -- CR 702.16k's chosen player in the same frame one atom over: answerable only
  -- where a protection quality is read, and every one of those four positions
  -- takes its filter off a keyword. See ofChosenPlayerOffends for the two
  -- offences.
  Spec.it s "CR 702.16k no card asks OfChosenPlayer outside a keyword's own filter" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace ofChosenPlayerOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only in a keyword's filter" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the one card that does is
    -- ACCEPTED here rather than skipped.
    nemesis <- S.printingOf s registry "True-Name Nemesis"
    Spec.assertEqWith
      s
      "True-Name Nemesis's one atom is framed by its own keyword"
      (ofChosenPlayerCounts (S.combinedFace nemesis))
      (1, 0)
    Spec.assertEqWith
      s
      "and it is the pool's only one"
      (sum (fmap (uncurry (+) . ofChosenPlayerCounts . S.combinedFace) ps))
      1
  -- The hole that bucket had while every keyword payload was KeywordFramed. CR
  -- 702.6c's equip quality is a payload that is NOT read as a keyword: rule
  -- 702.6a's minted ability carries it into a target slot, which
  -- Pawl.Engine.Target.slotContext answers with carrierChosenPlayer unfilled, so
  -- an equip quality asking CR 702.16k's chosen player would have passed the lint
  -- above and then matched nothing. MintedTargetSlot is what makes it loud.
  Spec.it s "CR 702.16k an equip quality is not a protection quality" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    blade <- S.printingOf s registry "Dúnedain Blade"
    let base = S.combinedFace piker
        creatures = Filter.Type.HasCardType CardType.Creature
        -- Buried under both combinators, so a lint reading only the top of a
        -- Filter would miss it.
        buried = Filter.Type.And [Filter.Type.Or [creatures, Filter.Type.OfChosenPlayer]]
        equipping quality = Keyword.Equip (Equip.MkEquip (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) quality)
        offending = base {Face.keywords = Set.singleton (equipping (Just buried))}
        protecting = base {Face.keywords = Set.singleton (Keyword.Protection buried)}
    -- Ordered FIRST, and the assertion this framing exists for.
    Spec.assertBool s (ofChosenPlayerOffends offending) "OfChosenPlayer in an equip quality offends"
    Spec.assertEqWith s "counted outside the keyword bucket" (ofChosenPlayerCounts offending) (0, 1)
    -- The pair differing in exactly ONE thing: the same buried atom on the same
    -- face, moved into a quality rule 702.16 really does read off the keyword.
    Spec.assertBool s (not (ofChosenPlayerOffends protecting)) "the same atom in a protection quality does not"
    Spec.assertEqWith s "counted inside it" (ofChosenPlayerCounts protecting) (1, 0)
    -- So the framing rejects the ATOM and not the position: the printed quality
    -- in the pool carries it and is accepted.
    Spec.assertBool s (not (ofChosenPlayerOffends (S.combinedFace blade))) "Dúnedain Blade's own quality is accepted"
    Spec.assertEqWith
      s
      "and arrives framed as a minted target slot"
      (fmap fst (filter ((==) (Filter.Type.HasSubtype Subtype.Human) . snd) (cardFilters (S.combinedFace blade))))
      [MintedTargetSlot]
    -- The other half of what that framing promises, one atom over: the minted
    -- ability announces nothing, so CR 115.10a's bound object is as unanswerable
    -- in an equip quality as in a keyword's own filter, and isBoundCounts puts
    -- the framing in its offending list for that.
    let slot = SlotName.MkSlotName (Text.pack "target")
        bound = Filter.Type.And [Filter.Type.Or [creatures, Filter.Type.IsBound slot]]
    Spec.assertBool s (isBoundOffends (base {Face.keywords = Set.singleton (equipping (Just bound))})) "IsBound in an equip quality offends too"
    Spec.assertBool s (not (isBoundOffends (base {Face.counterRestrictions = [CounterRestriction.MkCounterRestriction (Affected.Matching bound) Nothing]}))) "and the same atom at an unframed position does not"
  -- CR 701.23a: a search looks through a zone, so one naming none can find
  -- nothing. Nothing else catches an empty set. Search.zones is defaulted-absent
  -- to the library, so a card file has to write "zones": [] on purpose -- and the
  -- result decodes, resolves, finds nothing and shuffles nothing, which reads
  -- exactly like a search whose filter matched nothing.
  Spec.it s "CR 701.23a every search names at least one zone" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any Set.null . searchZoneSets) . Printing.card) ps
    Spec.assertEqWith s "no card's search names an empty zone set" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors searches, and the one naming two zones is
    -- read here rather than skipped.
    moogle <- S.printingOf s registry "Delivery Moogle"
    Spec.assertEqWith
      s
      "Delivery Moogle's search names its library and its graveyard"
      (searchZoneSets (S.combinedFace moogle))
      [Set.fromList [Zone.Library, Zone.Graveyard]]
    Spec.assertBool s (length (concatMap (searchZoneSets . S.combinedFace) ps) > 5) "and the pool gives the sweep other searches to be about"
  -- CR 709.4a's Filter.SameNameAsBound is in CR 701.3a's position one axis over:
  -- answerable only where Filter.Context.slotNames is filled, and unanswerable --
  -- a silent False -- everywhere else. The three positions ADMITTED here are a
  -- MODE's target slot, a CR 701.23 search filter and a CR 608.2c hand sweep's
  -- own filter, the last two since their arms took their context from the
  -- resolution (Pawl.ResolveSpec's Bifurcate and Hour of Glory cases). That set
  -- is narrower than the set that would answer -- sameNameAsBoundCounts says why
  -- -- so this rejects in the safe direction only. See sameNameAsBoundOffends
  -- for the two offences and Framing for why CR 303.4a's enchant slot is not one
  -- of the safe positions.
  Spec.it s "CR 709.4a no card asks SameNameAsBound outside a mode's target slot, a search filter or a hand sweep" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace sameNameAsBoundOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only in a target slot's, a search's or a hand sweep's filter" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous, the way the sibling sweeps would be alone: the pool authors the
    -- atom, and the three cards that do are ACCEPTED here rather than skipped --
    -- one in each admitted position, so no member of the set is unwitnessed.
    harness <- S.printingOf s registry "Harness the Storm"
    Spec.assertEqWith
      s
      "Harness the Storm's one atom is in its trigger's target slot"
      (sameNameAsBoundCounts (S.combinedFace harness))
      (1, 0)
    bifurcate <- S.printingOf s registry "Bifurcate"
    Spec.assertEqWith
      s
      "Bifurcate's one atom is in its search's filter"
      (sameNameAsBoundCounts (S.combinedFace bifurcate))
      (1, 0)
    glory <- S.printingOf s registry "Hour of Glory"
    Spec.assertEqWith
      s
      "Hour of Glory's one atom is in its hand sweep's filter"
      (sameNameAsBoundCounts (S.combinedFace glory))
      (1, 0)
    Spec.assertEqWith
      s
      "and they are the pool's only three"
      (sum (fmap (uncurry (+) . sameNameAsBoundCounts . S.combinedFace) ps))
      3
    -- Both sides of the split, with room to spare under the pool's real figures:
    -- a traversal that had stopped walking, or a Framing that had stopped marking
    -- target slots, would fail here rather than pass the sweep above by iterating
    -- over nothing. Without the second, "elsewhere" would be every occurrence and
    -- this would silently become the sibling atoms' "no card writes it at all".
    let positions = concatMap (cardFilters . S.combinedFace) ps
    Spec.assertBool s (length positions > 100) "the pool gives the traversal Filter positions to walk"
    Spec.assertBool s (length (filter ((== InTargetSlot) . fst) positions) > 10) "and target slot filters for the accepted side to be about"
    Spec.assertBool s (length (filter ((== SearchFramed) . fst) positions) > 10) "and search filters, the accepted side's other half"
  -- CR 110.2's Filter.SameControllerAsBound is CR 709.4a's atom one characteristic
  -- over, in the same position and with the STAKES reversed: it is vacuously TRUE
  -- where Filter.Context.slotControllers has no key for its slot, so an atom
  -- outside a mode's target slot admits every candidate the card meant to exclude
  -- rather than none. See sameControllerAsBoundOffends.
  Spec.it s "CR 110.2 no card asks SameControllerAsBound outside a mode's target slot" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace sameControllerAsBoundOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only in a target slot's filter" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the card that does is ACCEPTED
    -- here rather than skipped.
    bioshift <- S.printingOf s registry "Bioshift"
    Spec.assertEqWith
      s
      "Bioshift's one atom is in its second target slot"
      (sameControllerAsBoundCounts (S.combinedFace bioshift))
      (1, 0)
    Spec.assertEqWith
      s
      "and it is the pool's only one"
      (sum (fmap (uncurry (+) . sameControllerAsBoundCounts . S.combinedFace) ps))
      1
    -- The REJECTING direction, hand-built for the reason every sibling lint's is:
    -- a card that offends must not be loadable, so no file can carry one. Buried
    -- under all three combinators, so an implementation reading only the top of a
    -- Filter would accept it.
    piker <- S.printingOf s registry "Goblin Piker"
    let atom = Filter.Type.SameControllerAsBound (SlotName.MkSlotName (Text.pack "from"))
        buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not atom]]
        planted =
          (S.combinedFace piker)
            { Face.staticAbilities =
                [ StaticAbility.MkStaticAbility
                    (Affected.Matching buried)
                    Nothing
                    Set.empty
                    Nothing
                    (NonEmpty.singleton Modification.LoseAllAbilities)
                ]
            }
    Spec.assertEqWith s "the same atom in a static ability's affected set is an offence" (sameControllerAsBoundCounts planted) (0, 1)
    Spec.assertBool s (sameControllerAsBoundOffends planted) "and the lint says so"
    Spec.assertBool s (not (sameControllerAsBoundOffends (S.combinedFace piker))) "where the ungrafted card is accepted"
  -- CR 202.3's computed bound is CR 709.4a's atom one axis over once more, and the
  -- axis is the SLOT rather than the Framing: Pawl.Engine.Target.slotContext fills
  -- Filter.Context.slotAmount off the target slot's own Quantity, so the atom in a
  -- slot naming no amount -- or anywhere that is not a target slot at all -- is a
  -- silent False. See manaValueAtMostAmountCounts.
  Spec.it s "CR 202.3 no card asks ManaValueAtMostAmount outside a slot that names an amount" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace manaValueAtMostAmountOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only where the slot supplies the bound" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the card that does is ACCEPTED
    -- here rather than skipped.
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    Spec.assertEqWith
      s
      "Celestine's one atom is in the slot that names its bound"
      (manaValueAtMostAmountCounts (S.combinedFace celestine))
      (1, 0)
    -- Ratchet, Field Medic is the pool's second author of the atom, and the one
    -- that reaches it through a DELAYED ability rather than a triggered one --
    -- CR 603.12's reflexive, whose slot is baked and matched exactly as an
    -- ordinary trigger's is.
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    Spec.assertEqWith
      s
      "Ratchet's reflexive slot names its bound too"
      (manaValueAtMostAmountCounts (S.combinedFace ratchet))
      (1, 0)
    -- The pool's third author, and the one whose bound is a slot rather than a
    -- board tally: Venerable Warsinger's X is CR 603.2's own event amount. The
    -- position claim is the same one -- what varies is what answers the Quantity.
    warsinger <- S.printingOf s registry "Venerable Warsinger"
    Spec.assertEqWith
      s
      "the Warsinger's slot names its bound as well"
      (manaValueAtMostAmountCounts (S.combinedFace warsinger))
      (1, 0)
    -- The pool's fourth author, and the first on a SPELL: Stir the Grave's bound
    -- is CR 601.2b's announced X, which the caster names one step before CR
    -- 601.2c chooses the target (Pawl.Engine.Cast.castProposed's seed).
    stir <- S.printingOf s registry "Stir the Grave"
    Spec.assertEqWith
      s
      "Stir the Grave's slot names its bound as well"
      (manaValueAtMostAmountCounts (S.combinedFace stir))
      (1, 0)
    -- The pool's fifth author, and the first whose slot is also JOINTLY JUDGED
    -- (Pawl.Engine.Target.jointlyJudged): Synthetic Borrowed Exhumation reads the
    -- same announced X off a pool scoped to what its sibling slot targets, so CR
    -- 601.2c's joint check re-derives the bound rather than merely offering it.
    exhumation <- S.printingOf s registry "Synthetic Borrowed Exhumation"
    Spec.assertEqWith
      s
      "the Exhumation's slot names its bound as well"
      (manaValueAtMostAmountCounts (S.combinedFace exhumation))
      (1, 0)
    -- The pool's sixth author, jointly judged through the BOUND itself rather
    -- than through the pool beside it: Synthetic Measured Refrain's bound folds
    -- over what a sibling slot names (Scope.OverBound), which is the read CR
    -- 700.2d's per-occurrence rename has to follow -- Pawl.TargetSpec's "CR 700.2d
    -- a repeated mode's computed bound measures its own occurrence's sibling
    -- slot".
    measured <- S.printingOf s registry "Synthetic Measured Refrain"
    Spec.assertEqWith
      s
      "the Refrain's slot names its bound as well"
      (manaValueAtMostAmountCounts (S.combinedFace measured))
      (1, 0)
    -- The pool's seventh author, and the first on an ACTIVATED ability: Blighted
    -- Nightmare's bound is the X announced through CR 602.2b, which its activator
    -- names one step before CR 601.2c chooses the target
    -- (Pawl.Engine.Activate.activateAbility's post-announcement map).
    nightmare <- S.printingOf s registry "Blighted Nightmare"
    Spec.assertEqWith
      s
      "the Nightmare's slot names its bound as well"
      (manaValueAtMostAmountCounts (S.combinedFace nightmare))
      (1, 0)
    -- The eighth, the same position on the paper printing: Tameshi, Reality
    -- Architect's {X}{W} announces through the same road.
    tameshi <- S.printingOf s registry "Tameshi, Reality Architect"
    Spec.assertEqWith
      s
      "Tameshi's slot names its bound as well"
      (manaValueAtMostAmountCounts (S.combinedFace tameshi))
      (1, 0)
    Spec.assertEqWith
      s
      "and they are the pool's only ones"
      (sum (fmap (uncurry (+) . manaValueAtMostAmountCounts . S.combinedFace) ps))
      8
    -- The rejected side, which the sweep above cannot show while the pool has no
    -- offender: the SAME atom, buried under all three combinators, in a target
    -- slot that names no amount -- the position a card author would most plausibly
    -- reach for, since it differs from the accepted one by an absent key alone.
    piker <- S.printingOf s registry "Goblin Piker"
    let buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.ManaValueAtMostAmount]]
        slotWith slot =
          (S.combinedFace piker)
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) slot)))
                  (ModeSelection.ChooseExactly 1)
            }
        amountless = slotWith (TargetSlot.required Pool.Creatures (Just buried))
    Spec.assertEqWith s "a planted atom in an amountless slot is an offence" (manaValueAtMostAmountCounts amountless) (0, 1)
    Spec.assertBool s (manaValueAtMostAmountOffends amountless) "and the lint says so"
    -- And the pair that differs in exactly one thing: the same face with the same
    -- filter, in a slot that DOES name an amount, is accepted -- so the lint is
    -- reading the amount key rather than rejecting every planted atom.
    let amounted = slotWith (TargetSlot.withAmount (Quantity.Type.LifeGainedThisTurn (PlayerRef.Relative PlayerRelation.You)) (TargetSlot.required Pool.Creatures (Just buried)))
    Spec.assertEqWith s "the same atom in a slot that names one is not" (manaValueAtMostAmountCounts amounted) (1, 0)
  -- CR 303.4b's Filter.IsHostOfSource is CR 709.4a's atom one axis over again:
  -- answerable only where Filter.Context.sourceAttachedTo is filled, which is the
  -- positions `hostFramed` admits. See hostOfSourceOffends for the two offences.
  Spec.it s "CR 303.4b no card asks IsHostOfSource where the source's host is unknown" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace hostOfSourceOffends . Printing.card) ps
    Spec.assertEqWith s "the atom sits only where the host is supplied" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the cards that do are ACCEPTED
    -- here rather than skipped. Ray of Frost writes it three times, once per
    -- accepted position -- a CR 604.2 clause, a CR 603.4 clause and an
    -- Effect.Tap's ObjectRef -- so three of the five legs of the tagging are
    -- exercised there.
    ray <- S.printingOf s registry "Ray of Frost"
    Spec.assertEqWith
      s
      "Ray of Frost's three atoms are all framed"
      (hostOfSourceCounts (S.combinedFace ray))
      (3, 0)
    -- The fourth leg: a printed PLAYER ability's own effect, which reads the
    -- source off the row Pawl.Engine.PlayerEffect.applying returns. Its own
    -- behaviour is Pawl.PlayerEffectSpec's Oppressive Rays group; this is the
    -- position claim.
    rays <- S.printingOf s registry "Oppressive Rays"
    Spec.assertEqWith
      s
      "Oppressive Rays' one atom is framed too"
      (hostOfSourceCounts (S.combinedFace rays))
      (1, 0)
    -- The fifth leg: a CR 614.1 replacement ROW's own Filter, read through
    -- Pawl.Engine.Replacement.candidateContext. Pariah writes it as CR 614.9's
    -- printed destination -- "is dealt to enchanted creature instead" -- and its
    -- behaviour is Pawl.ReplacementSpec's Pariah group.
    pariah <- S.printingOf s registry "Pariah"
    Spec.assertEqWith
      s
      "Pariah's redirect destination is framed too"
      (hostOfSourceCounts (S.combinedFace pariah))
      (1, 0)
    Spec.assertEqWith
      s
      "and they are the pool's only ones"
      (sum (fmap (uncurry (+) . hostOfSourceCounts . S.combinedFace) ps))
      5
    -- The rejected side, which the sweep above cannot show while the pool has no
    -- offender: the same atom planted in a target slot -- the position a card
    -- author would most plausibly reach for -- IS counted as elsewhere.
    piker <- S.printingOf s registry "Goblin Piker"
    let buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.IsHostOfSource]]
        planted =
          (S.combinedFace piker)
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.Creatures (Just buried)))))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "a planted atom is an offence" (hostOfSourceCounts planted) (0, 1)
    Spec.assertBool s (hostOfSourceOffends planted) "and the lint says so"
  -- CR 702.33d: a kicker tally names one of the face's OWN kicker costs, joined
  -- to Pawl.Engine.Cast's per-cost map by structural equality. See
  -- timesKickedWithOffends for why this is read off the encoding.
  Spec.it s "CR 702.33d every kicker tally names a cost its face prints" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace timesKickedWithOffends . Printing.card) ps
    Spec.assertEqWith s "no card counts a kicker cost its face never prints" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the arm, and the cards that do are ACCEPTED
    -- here rather than swept past. Gnarlid Pack counts its multikicker once and
    -- Sunscape Battlemage its two CR 702.33b kickers once each.
    gnarlid <- S.printingOf s registry "Gnarlid Pack"
    sunscape <- S.printingOf s registry "Sunscape Battlemage"
    let tallies = length . timesKickedWithCosts . Codec.encode (Face.Codec.codec Card.codec) . S.combinedFace
    Spec.assertEqWith s "the pool writes three tallies for this lint to be about" (fmap tallies [gnarlid, sunscape]) [1, 2]
    -- The REJECTING direction, as faces differing from the printed one in the
    -- kicker cost ALONE: Gnarlid Pack's {1}{G} respelled {G}{1} is a different Map
    -- key, so the tally reads 0 for the life of the game.
    let face = S.combinedFace gnarlid
        reverseMana cost = cost {Cost.Type.mana = fmap (\(ManaCost.MkManaCost symbols) -> ManaCost.MkManaCost (reverse symbols)) (Cost.Type.mana cost)}
        respell keyword = case keyword of
          Keyword.Multikicker cost -> Keyword.Multikicker (reverseMana cost)
          _ -> keyword
    Spec.assertBool s (not (timesKickedWithOffends face)) "the control: Gnarlid Pack as printed is accepted"
    Spec.assertBool s (timesKickedWithOffends (face {Face.keywords = Set.map respell (Face.keywords face)})) "a reordered multikicker cost is rejected"
    Spec.assertBool s (timesKickedWithOffends (face {Face.keywords = Set.empty})) "and so is a face that counts a kicker it prints none of"
  -- CR 115.10a's Filter.IsBound is the position lints' MIRROR: it is answerable
  -- wherever the evaluator hands over the resolution's slots and the candidate is
  -- an object, and unanswerable in the one card-authored position whose
  -- candidates are not objects in the game -- CR 400.11c's wish filter, matched
  -- against a printed face. See isBoundOffends for the two offences.
  Spec.it s "CR 400.11c no card asks IsBound in a wish's filter" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace isBoundOffends . Printing.card) ps
    Spec.assertEqWith s "no card names a bound slot where the candidate is a card outside the game" (fmap (S.nameOf . Printing.card) offenders) []
    -- NOT vacuous: the pool authors the atom, and the cards that do are ACCEPTED
    -- here rather than skipped: Showstopping Surprise writes "each other
    -- creature" as Not (IsBound "target"), and that atom is counted on the
    -- accepted side.
    surprise <- S.printingOf s registry "Showstopping Surprise"
    Spec.assertEqWith
      s
      "Showstopping Surprise's one atom is outside a wish's filter"
      (isBoundCounts (S.combinedFace surprise))
      (0, 1)
    Spec.assertBool s (sum (fmap (snd . isBoundCounts . S.combinedFace) ps) > 5) "and the pool gives the accepted side other atoms to be about"
    -- The rejected side, which the sweep above cannot show while the pool has no
    -- offender: the SAME atom, buried under all three combinators, in a wish's
    -- own filter -- so an implementation reading only the top of a Filter would
    -- accept it.
    let slot = SlotName.MkSlotName (Text.pack "target")
        buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Sorcery, Filter.Type.Not (Filter.Type.IsBound slot)]]
        spellOf effects =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) Map.empty))
            (ModeSelection.ChooseExactly 1)
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.combinedFace piker
        wished = base {Face.spell = spellOf [Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame buried True)]}
    Spec.assertEqWith s "a planted atom in a wish's filter is an offence" (isBoundCounts wished) (1, 0)
    Spec.assertBool s (isBoundOffends wished) "and the lint says so"
    -- And the pair that differs in exactly one thing: the same face carrying the
    -- same filter in a position whose candidates ARE objects is accepted -- so the
    -- lint is reading the position rather than rejecting every planted atom.
    let destroying = base {Face.spell = spellOf [Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching buried) Regenerability.Regenerable Nothing Nothing Nothing)]}
    Spec.assertEqWith s "the same atom over objects is not" (isBoundCounts destroying) (0, 1)
    Spec.assertBool s (not (isBoundOffends destroying)) "and the lint accepts it"
    -- THE THIRD POSITION, and the one this case gained after #2881: a CR 118.12
    -- gate's cost is paid through Pawl.Engine.Filter.contextFor, which fills no
    -- slots, so the same buried atom is as unanswerable there as in a wish's
    -- filter. The shape is the #2141 widening -- Not (IsBound "target") reads as
    -- "each OTHER creature", and against an empty slot map it is vacuously true
    -- of every creature including the bound one.
    --
    -- Written as a gate over the SAME `base` face and the SAME `buried` filter as
    -- the two legs above, so the three differ in position and in nothing else.
    let gated =
          base
            { Face.spell =
                Modal.MkModal
                  ( Seq.singleton
                      ( Mode.MkMode
                          ( Seq.singleton
                              ( Clause.MkClause
                                  Nothing
                                  Nothing
                                  Nothing
                                  Optionality.Mandatory
                                  ( Just
                                      PayGate.MkPayGate
                                        { PayGate.payer = PlayerRef.Relative PlayerRelation.You,
                                          PayGate.cost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 buried)],
                                          PayGate.branch = PayBranch.IfNotPaid,
                                          PayGate.obligation = PayObligation.Optional,
                                          PayGate.perCounter = Nothing,
                                          PayGate.offeredAt = Nothing
                                        }
                                  )
                                  (Seq.singleton (Effect.Sacrifice SacrificeEffect.MkSacrificeEffect {SacrificeEffect.ref = ObjectRef.InSlot slot, SacrificeEffect.sacrificer = Sacrificer.EffectController}))
                              )
                          )
                          Map.empty
                      )
                  )
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "CR 118.12 a planted atom in a gate's cost is an offence" (isBoundCounts gated) (1, 0)
    Spec.assertBool s (isBoundOffends gated) "and the lint says so"
    -- THE OTHER COST POSITIONS, every one paid through the same
    -- Pawl.Engine.Cost.pay, split by whether an announcement is behind them.
    -- Built over the SAME `base` face and the SAME `buried` filter as every leg
    -- above, so they differ from the accepted leg in position and in nothing
    -- else.
    let sacrificing = [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 buried)]
        sacrificeCost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) sacrificing
        -- CR 601.2f, paid as the cast is announced -- after CR 601.2c.
        added = base {Face.additionalCosts = sacrificing}
        -- CR 118.9, the cost half only -- the CR 604.2 condition an alternative
        -- may carry is a separate question and is not tagged with it.
        alternatively = base {Face.alternativeCosts = [AlternativeCost.MkAlternativeCost Nothing sacrificeCost]}
        -- CR 602.2b, paid as the ability is activated -- after CR 601.2c too.
        activating = base {Face.activatedAbilities = [ActivatedAbility.MkActivatedAbility sacrificeCost [] (spellOf []) [] Activator.Controller Nothing Nothing Nothing]}
        -- CR 116.2d: a special action uses no stack (CR 116.1), so no
        -- announcement could answer in any engine.
        ignoring = base {Face.specialActions = [SpecialAction.IgnoreThisUntilEndOfTurn (AbilityName.MkAbilityName (Text.pack "the prohibition")) sacrificeCost]}
        -- CR 116.2c: the same special action, as the price of ending an effect.
        ending = base {Face.spell = spellOf [Effect.AffectPlayers (AffectPlayers.MkAffectPlayers (Duration.UntilPaid sacrificeCost) (AffectedPlayers.Scoped PlayerScope.You) PlayerEffect.CantCastSpells)]}
        -- CR 508.1h and CR 509.1d: a declaration announces no target.
        attacking = base {Face.attackCosts = [AttackCost.MkAttackCost (Affected.Matching (Filter.Type.HasCardType CardType.Creature)) (PerCreature.Fixed sacrificeCost) AttackCostScope.Controller]}
        blocking = base {Face.blockCosts = [BlockCost.MkBlockCost (Affected.Matching (Filter.Type.HasCardType CardType.Creature)) (PerCreature.Fixed sacrificeCost)]}
    -- Ordered FIRST: the positions an announcement pays are accepted, which is
    -- what #2924 changed -- Pawl.Engine.Cost.pay reads CR 601.2c's targets off
    -- the stack object, so the atom is answered there.
    Spec.assertEqWith
      s
      "CR 601.2f / 118.9 / 602.2b a planted atom in a cost an announcement pays is answered"
      (fmap isBoundCounts [added, alternatively, activating])
      [(0, 1), (0, 1), (0, 1)]
    Spec.assertBool s (not (any isBoundOffends [added, alternatively, activating])) "and the lint accepts all three"
    Spec.assertEqWith
      s
      "CR 116.2d / 116.2c / 508.1h / 509.1d a planted atom in a cost nothing announces is an offence"
      (fmap isBoundCounts [ignoring, ending, attacking, blocking])
      [(1, 0), (1, 0), (1, 0), (1, 0)]
    Spec.assertBool s (all isBoundOffends [ignoring, ending, attacking, blocking]) "and the lint says so at all four"
  -- CR 702 / CR 122.1b: a keyword's own Filter is read off a continuous effect or
  -- off the printed face, through Pawl.Engine.Filter.contextFor, which fills
  -- neither the resolution's slots nor the source's host. keywordFilters tags it
  -- KeywordFramed at the LEAF that produces it rather than at the position that
  -- quotes it, so a quoting position's promise is not inherited by the keyword's
  -- text -- which matters because counterKindFilters is reached from positions
  -- that promise more than a keyword can keep: a CR 614.1 replacement ROW, whose
  -- `hostFramed` is True, and a MODE's target-slot amount, which is InTargetSlot.
  Spec.it s "CR 702 a keyword's own filter is framed by the keyword and not by whatever quotes it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.combinedFace piker
        slot = SlotName.MkSlotName (Text.pack "target")
        creatures = Filter.Type.HasCardType CardType.Creature
        -- Buried under all three combinators at every plant, so a lint reading
        -- only the top of a Filter would miss each one.
        bury atom = Filter.Type.And [Filter.Type.Or [creatures, Filter.Type.Not atom]]
        boundAtom = bury (Filter.Type.IsBound slot)
        hostAtom = bury Filter.Type.IsHostOfSource
        nameAtom = bury (Filter.Type.SameNameAsBound slot)
        keywordOf f = Keyword.Hexproof (Just f)
        kindOf f = CounterKind.Keyword (keywordOf f)
        plain = CounterKind.PlusOnePlusOne
        prohibiting kind = [CounterRestriction.MkCounterRestriction (Affected.Matching creatures) (Just kind)]
        granting keyword =
          [ StaticAbility.MkStaticAbility
              (Affected.Matching creatures)
              Nothing
              Set.empty
              Nothing
              (NonEmpty.singleton (Modification.GainKeyword keyword))
          ]
        scaling kind onWhat =
          [ PrintedReplacement.MkPrintedReplacement
              Nothing
              (ReplacementEffect.CounterR (CounterR.MkCounterR (CounterPattern.MkCounterPattern (Just kind) CounterSubject.ByAnything ControllerRelation.Yours onWhat Nothing) (Scaling.AddMore 1)))
              Set.empty
              Nothing
          ]
        spellOf targetSlot =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton slot targetSlot)))
            (ModeSelection.ChooseExactly 1)
        counting kind f = spellOf (TargetSlot.withAmount (Quantity.Type.ObjectCounters kind) (TargetSlot.required Pool.Creatures f))
        -- CR 614.1c's own row, carrying the entry rewrite under test. Filter.IsSource
        -- is what every "[this permanent] enters ..." clause matches on.
        entering rewrite =
          [ PrintedReplacement.MkPrintedReplacement
              Nothing
              (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource rewrite))
              Set.empty
              Nothing
          ]
        -- CR 208.2b's option, distinct P/T so nothing here is a numeric
        -- coincidence, and the keyword is the only thing carrying a Filter.
        optionWith = EntryOption.MkEntryOption 2 5
    -- Ordered FIRST, and CR 115.10a's half of the claim: the atom whose offence
    -- is the mirror of the others' is rejected at all three sites a keyword's
    -- Filter is reached from. A site that lost the tag reddens under its own
    -- label rather than under a total.
    let sited =
          [ ("CR 702.11d's printed keyword", isBoundCounts (base {Face.keywords = Set.singleton (keywordOf boundAtom)})),
            ("CR 613's granted keyword", isBoundCounts (base {Face.staticAbilities = granting (keywordOf boundAtom)})),
            ("CR 122.1b's keyword counter", isBoundCounts (base {Face.counterRestrictions = prohibiting (kindOf boundAtom)})),
            -- CR 208.2b's as-enters options and CR 614.1c's outright grant, the
            -- three positions #3232 named: each carries a Set of whole Keywords,
            -- so each reaches a Filter the same way CR 707.9a's exception does.
            -- ONE option of the flip's two carries the atom, so the count is 1
            -- here as it is at every other site.
            ("CR 208.2b's as-enters option", isBoundCounts (base {Face.replacementEffects = entering (EntryRewrite.ChoiceOf [optionWith (Set.singleton (keywordOf boundAtom))])})),
            ("CR 705.2's flipped option", isBoundCounts (base {Face.replacementEffects = entering (EntryRewrite.ChoiceByCoinFlip EntryFlip.MkEntryFlip {EntryFlip.heads = optionWith (Set.singleton (keywordOf boundAtom)), EntryFlip.tails = optionWith Set.empty})})),
            ("CR 614.1c's granted keyword", isBoundCounts (base {Face.replacementEffects = entering (EntryRewrite.WithKeywords (Set.singleton (keywordOf boundAtom)))}))
          ]
    Spec.assertEqWith s "IsBound under a keyword is an offence at every site that reaches one" sited (fmap (fmap (const (1, 0))) sited)
    -- The pair that differs in exactly ONE thing: the same prohibition, the same
    -- buried atom, moved off the KIND and onto the affected set beside it -- an
    -- ordinary unframed position, where the resolution's slots are supplied and
    -- the atom is accepted. So the lint is reading the keyword and not rejecting
    -- every plant.
    let besideIt = base {Face.counterRestrictions = [CounterRestriction.MkCounterRestriction (Affected.Matching boundAtom) (Just plain)]}
    Spec.assertEqWith s "the same atom beside the kind rather than under it is not" (isBoundCounts besideIt) (0, 1)
    Spec.assertBool s (not (isBoundOffends besideIt)) "and the lint accepts it"
    -- CR 303.4b, the first position #2733 named: a CR 614.1 replacement row's own
    -- Filters ARE read with the source's host supplied, and the keyword counter
    -- inside one is not.
    let rowKeyword = base {Face.replacementEffects = scaling (kindOf hostAtom) creatures}
        rowItself = base {Face.replacementEffects = scaling plain hostAtom}
    Spec.assertEqWith s "IsHostOfSource under a replacement row's keyword counter is an offence" (hostOfSourceCounts rowKeyword) (0, 1)
    Spec.assertBool s (hostOfSourceOffends rowKeyword) "and the lint says so"
    Spec.assertEqWith s "the same atom in the row's own pattern is not" (hostOfSourceCounts rowItself) (1, 0)
    Spec.assertBool s (not (hostOfSourceOffends rowItself)) "and the lint accepts it"
    -- CR 709.4a, the second: a MODE's target slot IS matched with the bound names
    -- supplied, and the keyword counter inside the slot's CR 202.3 amount is not.
    let amountKeyword = base {Face.spell = counting (kindOf nameAtom) Nothing}
        slotItself = base {Face.spell = counting plain (Just nameAtom)}
    Spec.assertEqWith s "SameNameAsBound under a slot amount's keyword counter is an offence" (sameNameAsBoundCounts amountKeyword) (0, 1)
    Spec.assertBool s (sameNameAsBoundOffends amountKeyword) "and the lint says so"
    Spec.assertEqWith s "the same atom in the slot's own filter is not" (sameNameAsBoundCounts slotItself) (1, 0)
    Spec.assertBool s (not (sameNameAsBoundOffends slotItself)) "and the lint accepts it"
    -- NOT vacuous: the codec sees exactly one atom in each of the six faces, so
    -- every pair above is a comparison against a real occurrence rather than
    -- against nothing -- and a (0, 0) from a traversal that stopped digging would
    -- fail the cross-check inside each lint rather than pass quietly.
    let encoded tag card = jsonAtoms tag (Codec.encode (Face.Codec.codec Card.codec) card)
        authored =
          [ ("IsBound beside the kind", encoded isBoundTag besideIt),
            ("IsHostOfSource under the row's keyword counter", encoded hostOfSourceTag rowKeyword),
            ("IsHostOfSource in the row's own pattern", encoded hostOfSourceTag rowItself),
            ("SameNameAsBound under the amount's keyword counter", encoded sameNameAsBoundTag amountKeyword),
            ("SameNameAsBound in the slot's own filter", encoded sameNameAsBoundTag slotItself)
          ]
    Spec.assertEqWith s "each planted face encodes exactly one atom" authored (fmap (fmap (const 1)) authored)
  -- CR 122.1b lets a counter's KIND be a whole Keyword, and a keyword may carry a
  -- Filter of its own -- CR 702.11d's "hexproof from Goblins", CR 702.29e's
  -- typecycling predicate, the components of any Cost a keyword names. So every
  -- card-authored counter-kind position holds card text, and the traversals above
  -- have to walk it or the position lints read the wrong number.
  --
  -- What that costs, exactly: NOT silence. Every position lint above carries a
  -- codec cross-check -- "the traversal and the codec disagree" -- and a Filter
  -- the traversal cannot see makes both counts zero against a non-zero
  -- `jsonAtoms`, so the card was rejected all the same. What was wrong is the
  -- CLASSIFICATION: the offence reported was a blind spot in cardFilters rather
  -- than the atom's position, and an atom that was legitimately placed would have
  -- been rejected by the same arithmetic. #2728 records the enumeration; this is
  -- what holds it.
  --
  -- Proved position by position on the TRAVERSALS rather than on the offence
  -- boolean, which was True before this walk existed and is True after it.
  Spec.it s "CR 122.1b a Filter under a keyword counter is walked at every counter-kind position" $ do
    let -- Buried under all three combinators, so a walk reading only the top of a
        -- keyword's own Filter would miss it.
        buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.CanAttachToSubject]]
        kind = CounterKind.Keyword (Keyword.Hexproof (Just buried))
        one = Quantity.Type.Literal 1
        slot = SlotName.MkSlotName (Text.pack "target")
        anywhere = ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)
        riders = EntryRiders.defaultValue {EntryRiders.counters = Map.singleton kind one}
        -- CR 118.12's gate, whose counter kind is rule 702.24a's multiplier;
        -- see #2876. Its cost is empty, so the row below reports the KIND rather
        -- than a walk that found the cost's filters instead.
        gate =
          PayGate.MkPayGate
            { PayGate.payer = PlayerRef.Relative PlayerRelation.You,
              PayGate.cost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [],
              PayGate.branch = PayBranch.IfNotPaid,
              PayGate.obligation = PayObligation.Optional,
              PayGate.perCounter = Just kind,
              PayGate.offeredAt = Nothing
            }
        -- The PAIR, not just the Filter: PR #2739's reader's test says a keyword's
        -- own Filter carries KeywordFramed out through every quoting position, and
        -- `frame` fills in only the ones still Unframed, so a position that
        -- overwrote the tag is an offence here exactly as a position that lost the
        -- Filter is. What reads that tag is framedSlotsReadSingly (#2741).
        holds :: [(Framing, Filter.Type.Filter Keyword.Keyword)] -> Bool
        holds = elem (KeywordFramed, buried)
        -- The same kind written as a NUMBER rather than as a kind: CR 122.1's
        -- per-object tally, which is what the three traversals below reach.
        counting = Condition.Type.Compares (Compares.MkCompares (Quantity.Type.ObjectCounters kind) Comparison.AtLeast one)
        topDepth = ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.ObjectCounters kind))
        -- Every position a card may write a CounterKind in, each named for the
        -- traversal that has to reach it. An arm that stops digging names itself.
        walked =
          [ ("EntryRiders' kinds", holds (riderFilters riders)),
            ("CR 118.12's gate kind", holds (payGateFilters gate)),
            ("Effect.PutCounters' kind", holds (effectFilters (Effect.PutCounters (PutCounters.MkPutCounters kind one anywhere)))),
            ("Effect.RemoveCounters' kind", holds (effectFilters (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters kind one slot)))),
            ("Effect.MoveCounters' kinds", holds (effectFilters (Effect.MoveCounters (MoveCounters.MkMoveCounters anywhere (MovedKinds.Named kind one) Nothing anywhere)))),
            ("Effect.PutCountersFrom' kind", holds (effectFilters (Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom slot (Just kind) anywhere)))),
            ("Quantity.ObjectCounters' kind", holds (quantityFilters (Quantity.Type.ObjectCounters kind))),
            ("CR 714.2b's threshold", holds (triggerConditionFilters (TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached kind 2)))),
            ("CR 310.12b's last removal", holds (triggerConditionFilters (TriggerCondition.SelfLastCounterRemoved kind))),
            ("its any-amount mirror", holds (triggerConditionFilters (TriggerCondition.SelfCountersRemoved kind))),
            ("CR 603.2c's per-permanent placement", holds (triggerConditionFilters (TriggerCondition.PermanentGetsCounters (CounterPlacement.MkCounterPlacement kind (Filter.Type.HasCardType CardType.Creature))))),
            ("CR 603.2c's batch placement", holds (triggerConditionFilters (TriggerCondition.PermanentsGetCounters (CounterPlacement.MkCounterPlacement kind (Filter.Type.HasCardType CardType.Creature))))),
            ("CR 614.1's scaling pattern", holds (replacementEffectFilters (ReplacementEffect.CounterR (CounterR.MkCounterR (CounterPattern.MkCounterPattern (Just kind) CounterSubject.ByAnything ControllerRelation.Yours (Filter.Type.HasCardType CardType.Creature) Nothing) (Scaling.AddMore 1))))),
            ("CR 614.1c's as-enters sacrifice", holds (entryRewriteFilters (EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber (Filter.Type.HasCardType CardType.Creature) (Just kind))))),
            ("CR 614.1c's as-enters counters", holds (entryRewriteFilters (EntryRewrite.WithCounters (WithCounters.one kind one)))),
            ("CR 614.1e's turn-up counters", holds (turnUpRewriteFilters (TurnUpRewrite.WithCounters (WithCounters.one kind one)))),
            -- The three roads a card writes the kind inside a NUMBER instead, each
            -- reaching a Quantity by its own traversal (#2740). Reading a
            -- Condition's, a Duration's or an ObjectRef's Counts alone answers []
            -- here, since quantityCounts has no Count to hand back for an
            -- ObjectCounters.
            ("a Condition's own number", holds (conditionFilters counting)),
            ("CR 611.2b's for-as-long-as clause", holds (durationFilters (Duration.ForAsLongAs counting))),
            ("a library depth", holds (objectRefFilters topDepth)),
            ("its reveal-until mirror", holds (objectRefFilters (ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil (PlayerRef.Relative PlayerRelation.You) (Filter.Type.HasCardType CardType.Land) (Quantity.Type.ObjectCounters kind))))),
            -- And the same three as cardFilters actually reaches them, one opcode
            -- or trigger condition deep, so the tagging survives the quoting
            -- position rather than only the leaf.
            ("CR 603.8's state trigger", holds (triggerConditionFilters (TriggerCondition.StateIs counting))),
            ("a stored duration's clause", holds (effectFilters (Effect.GainControl (DurationRef.MkDurationRef (Duration.ForAsLongAs counting) anywhere)))),
            ("a depth under an opcode", holds (effectFilters (Effect.Tap topDepth)))
          ]
    -- Ordered FIRST, and the assertion this case exists for: every position digs
    -- the keyword's Filter out. A dropped arm reddens under its own label.
    Spec.assertEqWith s "every counter-kind position is walked" walked (fmap (fmap (const True)) walked)
    -- The pair that differs in exactly ONE thing: the same positions carrying a
    -- kind that is not a keyword hand back no Filter at all, so what the row above
    -- reports is the KIND's payload rather than a walk that returns everything.
    let plain = CounterKind.PlusOnePlusOne
    Spec.assertEqWith
      s
      "a kind carrying no keyword contributes nothing"
      ( counterKindFilters plain,
        effectFilters (Effect.RemoveCounters (RemoveCounters.MkRemoveCounters plain one slot)),
        quantityFilters (Quantity.Type.ObjectCounters plain),
        triggerConditionFilters (TriggerCondition.SelfLastCounterRemoved plain)
      )
      ([], [], [], [])
    -- The same pair over the three number-shaped roads: a plain kind inside the
    -- tally hands back nothing, so what those rows report is the KIND's payload
    -- rather than a walk that returns the whole Quantity.
    let plainNumber = Condition.Type.Compares (Compares.MkCompares (Quantity.Type.ObjectCounters plain) Comparison.AtLeast one)
    Spec.assertEqWith
      s
      "a number naming a plain kind contributes nothing either"
      ( conditionFilters plainNumber,
        durationFilters (Duration.ForAsLongAs plainNumber),
        objectRefFilters (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.ObjectCounters plain)))
      )
      ([], [], [])
    -- And the whole road, end to end, at the ONE position no traversal above
    -- reaches on its own: a CR 122.6 prohibition names its kind on the Face
    -- record, so cardFilters' own fold is what has to carry it. The atom is
    -- CanAttachToSubject, answerable only inside a search (CR 701.3a), so the
    -- counts say the traversal found it and filed it OUTSIDE one -- where before
    -- this walk both counts read zero and only the codec cross-check objected.
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.combinedFace piker
        prohibiting =
          base
            { Face.counterRestrictions =
                [CounterRestriction.MkCounterRestriction (Affected.Matching (Filter.Type.HasCardType CardType.Creature)) (Just kind)]
            }
    Spec.assertEqWith s "a prohibition's kind is counted outside a search" (canAttachToSubjectCounts prohibiting) (0, 1)
    -- NOT vacuous: the codec sees exactly one atom in that face, so the pair above
    -- is being compared against a real occurrence rather than against nothing.
    Spec.assertEqWith s "and the codec agrees there is one" (jsonAtoms canAttachToSubjectTag (Codec.encode (Face.Codec.codec Card.codec) prohibiting)) 1
    Spec.assertBool s (canAttachToSubjectOffends prohibiting) "and the lint says so"
    -- The pair that differs in one thing again: the same prohibition over a
    -- plain kind carries no atom, so the face is accepted.
    let plainly = base {Face.counterRestrictions = [CounterRestriction.MkCounterRestriction (Affected.Matching (Filter.Type.HasCardType CardType.Creature)) (Just plain)]}
    Spec.assertEqWith s "a prohibition naming a plain kind carries none" (canAttachToSubjectCounts plainly) (0, 0)
    Spec.assertBool s (not (canAttachToSubjectOffends plainly)) "and the lint accepts it"
    -- The gate's OTHER position, end to end over a printing rather than a
    -- fixture: Lithophage's "unless you sacrifice a Mountain" writes card text
    -- into PayGate.cost, and it is far from the only printing that does. That
    -- half was unswept before #2876 for the same reason the kind was, and a
    -- corpus card is the evidence a hand-built mode would not be.
    lithophage <- S.printingOf s registry "Lithophage"
    Spec.assertBool
      s
      (elem (SlotlessCostFramed, Filter.Type.HasSubtype Subtype.Mountain) (cardFilters (S.combinedFace lithophage)))
      "CR 118.12 a gate cost's own filter reaches cardFilters, tagged for the slots it does not get"
  -- The batch-bound slot lint used to answer two ways about one piece of text.
  -- filterSlotsReadSingly deliberately does not descend into Filter.HasKeyword or
  -- Filter.HasCounters -- a keyword's own Filter is read through
  -- Pawl.Engine.Filter.contextFor, which fills no slot map -- but the sweep that
  -- folds it over a card's Filters dropped the tag first, so the same keyword
  -- filter arriving as a top-level KeywordFramed pair from counterKindFilters was
  -- walked after all. framedSlotsReadSingly is the one funnel now (#2741).
  Spec.it s "CR 122.1b a keyword's own Filter reads no slot on either route" $ do
    let bound = SlotName.MkSlotName (Text.pack "bound")
        -- The ONE arm of filterSlotsReadSingly that answers with a slot, so a
        -- route that swept the keyword's Filter is visible and no other atom is
        -- on trial.
        atom = Filter.Type.IsControllerOfBound bound
        kind = CounterKind.Keyword (Keyword.Hexproof (Just atom))
        one = Quantity.Type.Literal 1
        anywhere = ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)
        -- NESTED: the atom reached by descending an ordinary Filter, which the
        -- walk stops at.
        nested = filterSlotsReadSingly (Filter.Type.HasCounters kind)
        -- TOP-LEVEL: the same keyword, the same atom, handed back by
        -- counterKindFilters as its own (KeywordFramed, f) pair -- the route that
        -- used to disagree.
        topLevel = concatMap framedSlotsReadSingly (effectFilters (Effect.PutCounters (PutCounters.MkPutCounters kind one anywhere)))
    -- Ordered FIRST, and the assertion this case exists for.
    Spec.assertEqWith s "both routes to a keyword's own Filter read no slot" (nested, topLevel) ([], [])
    -- NOT vacuous, and the pair differing in exactly ONE thing: the same atom in
    -- the same card's text at a position that is not a keyword's own IS reported,
    -- so what the pair above says is that the FRAMING silences it rather than that
    -- the walk answers nothing anywhere.
    Spec.assertEqWith
      s
      "the same atom outside a keyword is read singly"
      (concatMap framedSlotsReadSingly (effectFilters (Effect.Tap (ObjectRef.EachMatching atom))))
      [bound]
    -- One row per Framing constructor, off [minBound .. maxBound] rather than a
    -- hand-kept list: a constructor added to that type lengthens the left side and
    -- reddens here rather than picking up whichever answer the new position
    -- happens to inherit.
    Spec.assertEqWith
      s
      "every framing's answer, one row per constructor"
      (fmap (\framing -> (framing, framedSlotsReadSingly (framing, atom))) [minBound .. maxBound])
      [ (Unframed, [bound]),
        (AttachDestination, [bound]),
        (InTargetSlot, [bound]),
        (SourceHostFramed, [bound]),
        (SearchFramed, [bound]),
        (ReplacementRowFramed, [bound]),
        (OutsideTheGameFramed, [bound]),
        (KeywordFramed, []),
        (SlotlessCostFramed, [bound]),
        (MintedTargetSlot, [bound]),
        (MillTallyFramed, [bound]),
        (HandSweepFramed, [bound])
      ]
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
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) targetSlot)))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "a planted atom is seen" (atoms planted) 1
    let buriedGreater = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.PowerGreaterThanSource]]
        plantedGreater =
          planted
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.Creatures (Just buriedGreater)))))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "and so is its sibling" (greater plantedGreater) 1
  -- CR 508.5's atom is answerable only where the CONTEXT supplies a defending
  -- player, and exactly two callers fill Filter.Context.defendingPlayer:
  -- Pawl.Engine.Target.admittedGiven for a target slot (CR 702.39a's provoke) and
  -- Pawl.Engine.CombatRestriction.inForce for a CR 508.1c gate (Armored Galleon).
  -- It is Nothing everywhere else, so the atom in any OTHER position -- a
  -- static ability's affected set, a search filter, a triggered ability's
  -- condition -- would be a silent False. This is the lint that keeps that true,
  -- narrowed from "no card writes it" the moment the Galleon arrived: what it
  -- sweeps now is the atom OUTSIDE a combat restriction, by re-encoding each face
  -- with its restrictions dropped.
  Spec.it s "CR 508.5 no card writes ControlledByDefendingPlayer outside a combat restriction" $ do
    ps <- S.allPrintings s
    let atoms c = jsonAtoms (Text.pack "ControlledByDefendingPlayer") (Codec.encode (Face.Codec.codec Card.codec) c)
        elsewhere c = atoms (c {Face.combatRestrictions = []})
        offenders = filter (anyFace ((/= 0) . elsewhere) . Printing.card) ps
    Spec.assertEqWith s "the atom is the engine's alone outside a gate" (fmap (S.nameOf . Printing.card) offenders) []
    -- Three legs of anti-vacuity, because the narrowing above could hide a real
    -- offender by blinding the counter rather than by being true.
    --
    -- One: the pool DOES write the atom, in the position the narrowing accepts.
    -- Armored Galleon's CR 508.1c gate is the whole of its text, so the counter
    -- sees it and `elsewhere` does not.
    galleon <- S.printingOf s registry "Armored Galleon"
    Spec.assertEqWith s "the Galleon's gate carries the atom" (atoms (S.combinedFace galleon)) 1
    Spec.assertEqWith s "and nothing outside it does" (elsewhere (S.combinedFace galleon)) 0
    -- Two: the same counter over a hand-built face carrying the atom in a target
    -- slot finds it, the sibling sweep's leg -- so a card smuggling it into a
    -- position the engine cannot answer is still caught.
    piker <- S.printingOf s registry "Goblin Piker"
    let buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.ControlledByDefendingPlayer]]
        targetSlot = TargetSlot.required Pool.Creatures (Just buried)
        planted =
          (S.combinedFace piker)
            { Face.spell =
                Modal.MkModal
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) targetSlot)))
                  (ModeSelection.ChooseExactly 1)
            }
    Spec.assertEqWith s "a planted atom is seen" (atoms planted) 1
    -- Three: and `elsewhere` sees it too, which is what says dropping the
    -- restrictions did not drop the rest of the face with them.
    Spec.assertEqWith s "and outside a combat restriction it is still seen" (elsewhere planted) 1
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
                  (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)) (Map.singleton (SlotName.MkSlotName (Text.pack "target")) targetSlot)))
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
                          (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.GainControl (DurationRef.MkDurationRef (Duration.ForAsLongAs crowned) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))))))
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
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) slots))
            (ModeSelection.ChooseExactly 1)
        boostedBy quantity =
          StaticAbility.MkStaticAbility
            (Affected.Matching Filter.Type.IsSource)
            Nothing
            Set.empty
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
                        Set.empty
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
              base {Face.spell = spellOf [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.quantity = Just (Quantity.Type.Literal 1), Search.filter = buried, Search.upTo = False, Search.destination = SearchDestination.RevealThenHand}] Map.empty}
            ),
            ( "an ObjectRef.EachMatching set",
              base {Face.spell = spellOf [Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching buried) Regenerability.Regenerable Nothing Nothing Nothing)] Map.empty}
            ),
            ( "CR 603.6a's trigger condition",
              base
                { Face.triggeredAbilities =
                    [ oneEffectTrigger
                        (TriggerCondition.PermanentEnters buried)
                        (Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.you) (Quantity.Type.Literal 1) Nothing))
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
                    [PlayerStaticAbility.MkPlayerStaticAbility {PlayerStaticAbility.scope = PlayerScope.You, PlayerStaticAbility.condition = Nothing, PlayerStaticAbility.name = Nothing, PlayerStaticAbility.effect = PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost buried 1)}]
                }
            ),
            ( "CR 508.1c's combat restriction",
              base {Face.combatRestrictions = [CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless (Affected.Matching buried) Nothing Nothing)]}
            ),
            ( "CR 508.1h's cost to attack",
              base {Face.attackCosts = [AttackCost.MkAttackCost (Affected.Matching buried) (PerCreature.Fixed (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [])) AttackCostScope.Controller]}
            ),
            ( "CR 508.1h's counted share",
              base
                { Face.attackCosts =
                    [ AttackCost.MkAttackCost
                        (Affected.Matching (Filter.Type.HasCardType CardType.Creature))
                        ( PerCreature.Counted
                            ( Quantity.Type.Count
                                (Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer)) buried Aggregation.Members)
                            )
                        )
                        AttackCostScope.Controller
                    ]
                }
            ),
            ( "CR 509.1d's cost to block",
              base {Face.blockCosts = [BlockCost.MkBlockCost (Affected.Matching buried) (PerCreature.Fixed (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) []))]}
            ),
            ( "CR 509.1d's counted share",
              base
                { Face.blockCosts =
                    [ BlockCost.MkBlockCost
                        (Affected.Matching (Filter.Type.HasCardType CardType.Creature))
                        ( PerCreature.Counted
                            ( Quantity.Type.Count
                                (Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer)) buried Aggregation.Members)
                            )
                        )
                    ]
                }
            ),
            ( "CR 614.1's counter-placement pattern",
              base
                { Face.replacementEffects =
                    [ PrintedReplacement.MkPrintedReplacement
                        Nothing
                        (ReplacementEffect.CounterR (CounterR.MkCounterR (CounterPattern.MkCounterPattern Nothing CounterSubject.ByAnything ControllerRelation.Yours buried Nothing) (Scaling.AddMore 1)))
                        Set.empty
                        Nothing
                    ]
                }
            ),
            ( "CR 604.2's clause gating a printed replacement ability",
              base
                { Face.replacementEffects =
                    [ PrintedReplacement.MkPrintedReplacement
                        ( Just
                            ( Condition.Type.Compares
                                ( Compares.MkCompares
                                    ( Quantity.Type.Count
                                        (Count.Type.MkCount (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer)) buried Aggregation.Members)
                                    )
                                    Comparison.AtLeast
                                    (Quantity.Type.Literal 1)
                                )
                            )
                        )
                        (ReplacementEffect.DestructionR DestructionRewrite.Regenerate)
                        Set.empty
                        Nothing
                    ]
                }
            ),
            ( "a created token's own static ability",
              base
                { Face.spell =
                    spellOf
                      [ Effect.Create
                          Create.MkCreate
                            { Create.quantity = Quantity.Type.Literal 1,
                              Create.card = oneFaced (base {Face.staticAbilities = [StaticAbility.MkStaticAbility (Affected.Matching buried) Nothing Set.empty Nothing (NonEmpty.singleton Modification.LoseAllAbilities)]}),
                              Create.riders = EntryRiders.defaultValue,
                              Create.slot = Nothing,
                              Create.creator = PlayerRef.Relative PlayerRelation.You
                            }
                      ]
                      Map.empty
                }
            ),
            ( "CR 103.5b's pregame action",
              base {Face.mulliganActions = [HandAction.MkHandAction Nothing [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.quantity = Just (Quantity.Type.Literal 1), Search.filter = buried, Search.upTo = False, Search.destination = SearchDestination.RevealThenHand}]]}
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
            Filter.Type.HasKeyword (Keyword.Cycling (Cycling.MkCycling (Cost.Type.MkCost Nothing []) (Just atom))),
            -- CR 303.4's attachment atom, which carries the HOST's description and
            -- is therefore a Filter position like the combinators above. No card
            -- nests this atom there -- it would be nonsense text -- so this is the
            -- only observer that descent has.
            Filter.Type.AttachedTo atom
          ]
      )
      [1, 1, 1, 1, 1, 1, 1]
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
  -- The rejecting direction for CR 709.4a, for the reason the test above gives:
  -- Harness the Storm only exercises the ACCEPTING one, and a card that offends a
  -- lint must not be loadable, so the offenders are hand-built rather than filed.
  --
  -- Every fixture buries the atom under all three combinators, so an
  -- implementation reading only the top of a Filter would accept all of them, and
  -- each is asserted through sameNameAsBoundCounts as well as the predicate -- the
  -- counts say the TRAVERSAL put it in that position, where the predicate alone is
  -- also satisfied by the codec half noticing an atom the traversal missed.
  Spec.it s "the lint itself catches SameNameAsBound outside a mode's target slot, a search filter or a hand sweep" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    harness <- S.printingOf s registry "Harness the Storm"
    let base = S.combinedFace piker
        slot = SlotName.MkSlotName (Text.pack "target")
        atom = Filter.Type.SameNameAsBound (SlotName.MkSlotName (Text.pack "thatSpell"))
        buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not atom]]
        spellOf effects slots =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) slots))
            (ModeSelection.ChooseExactly 1)
        boostedBy quantity =
          StaticAbility.MkStaticAbility
            (Affected.Matching Filter.Type.IsSource)
            Nothing
            Set.empty
            Nothing
            (NonEmpty.singleton (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness quantity (Quantity.Type.Literal 0))))
        planted =
          [ -- THE one that separates this lint from a copy of the sibling: an
            -- enchant slot is a TargetSlot, and is still not a position that can
            -- answer -- Attach.attachmentFor (CR 303.4j) and
            -- Sba.stillLegalEnchant (CR 303.4c) both re-ask it through
            -- Target.admittedRecipients, which binds nothing.
            ( "CR 303.4a's enchant ability",
              base {Face.enchant = [TargetSlot.required Pool.Permanents (Just buried)]}
            ),
            ( "a static ability's affected set",
              base
                { Face.staticAbilities =
                    [ StaticAbility.MkStaticAbility
                        (Affected.Matching buried)
                        Nothing
                        Set.empty
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
            ( "an ObjectRef.EachMatching set",
              base {Face.spell = spellOf [Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching buried) Regenerability.Regenerable Nothing Nothing Nothing)] Map.empty}
            ),
            ( "CR 603.6a's trigger condition",
              base
                { Face.triggeredAbilities =
                    [ oneEffectTrigger
                        (TriggerCondition.PermanentEnters buried)
                        (Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.you) (Quantity.Type.Literal 1) Nothing))
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
            ( "a created token's own static ability",
              base
                { Face.spell =
                    spellOf
                      [ Effect.Create
                          Create.MkCreate
                            { Create.quantity = Quantity.Type.Literal 1,
                              Create.card = oneFaced (base {Face.staticAbilities = [StaticAbility.MkStaticAbility (Affected.Matching buried) Nothing Set.empty Nothing (NonEmpty.singleton Modification.LoseAllAbilities)]}),
                              Create.riders = EntryRiders.defaultValue,
                              Create.slot = Nothing,
                              Create.creator = PlayerRef.Relative PlayerRelation.You
                            }
                      ]
                      Map.empty
                }
            )
          ]
        report (label, card) = (label, sameNameAsBoundOffends card, sameNameAsBoundCounts card)
    Spec.assertEqWith
      s
      "every position that cannot answer is rejected, and the traversal is what finds it"
      (fmap report planted)
      (fmap (\(label, _) -> (label, True, (0, 1))) planted)
    Spec.assertEqWith
      s
      "and the codec counts exactly the atoms the traversal does"
      (fmap (\(_, card) -> jsonAtoms sameNameAsBoundTag (Codec.encode (Face.Codec.codec Card.codec) card)) planted)
      (fmap (const 1) planted)
    -- The nesting, stated on its own: a top-level-only check would score every one
    -- of these zero but the first.
    Spec.assertEqWith
      s
      "the atom is found at every nesting depth"
      ( fmap
          (filterAtoms sameNameAsBoundTag)
          [ atom,
            Filter.Type.And [atom],
            Filter.Type.Or [atom],
            Filter.Type.Not atom,
            buried,
            Filter.Type.HasKeyword (Keyword.Cycling (Cycling.MkCycling (Cost.Type.MkCost Nothing []) (Just atom)))
          ]
      )
      [1, 1, 1, 1, 1, 1]
    -- The ACCEPTING direction, twice: the real card, and the buried atom in a mode
    -- target slot grafted onto a card that declares none of its own -- so the
    -- acceptance is about the POSITION and not about Harness the Storm.
    Spec.assertEqWith
      s
      "Harness the Storm is accepted"
      (sameNameAsBoundOffends (S.combinedFace harness), sameNameAsBoundCounts (S.combinedFace harness))
      (False, (1, 0))
    let slotted = base {Face.spell = spellOf [] (Map.singleton slot (TargetSlot.required Pool.Permanents (Just buried)))}
    Spec.assertEqWith
      s
      "a buried atom in a mode's target slot is accepted"
      (sameNameAsBoundOffends slotted, sameNameAsBoundCounts slotted)
      (False, (1, 0))
    -- The OTHER accepted position, and the pair to the rejections above: a search
    -- filter is matched in the resolution's own context
    -- (Pawl.Engine.Resolve.Slots.effectContext), so the same buried atom that offends
    -- in every position above is accepted here.
    let searched = base {Face.spell = spellOf [Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.quantity = Just (Quantity.Type.Literal 1), Search.filter = buried, Search.upTo = False, Search.destination = SearchDestination.RevealThenHand}] Map.empty}
    Spec.assertEqWith
      s
      "a buried atom in a search's filter is accepted"
      (sameNameAsBoundOffends searched, sameNameAsBoundCounts searched)
      (False, (1, 0))
    -- The THIRD accepted position, and the one that separates this lint from a
    -- rule about ObjectRefs: EachCardInHand's own filter is matched in the
    -- resolution's context (Resolve.Slots.objectRefObjects), where the
    -- EachMatching set planted above is not a position slotNames reaches -- the
    -- two sit in the same field of the same effect and answer differently.
    let sweptHand = base {Face.spell = spellOf [Effect.Reveal (Reveal.MkReveal (ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand (ZoneScope.ControllerOfBound slot) (Just buried))) Nothing)] Map.empty}
    Spec.assertEqWith
      s
      "a buried atom in a hand sweep's filter is accepted"
      (sameNameAsBoundOffends sweptHand, sameNameAsBoundCounts sweptHand)
      (False, (1, 0))
    Spec.assertEqWith
      s
      "and the ungrafted base card carries no atom at all"
      (sameNameAsBoundOffends base, sameNameAsBoundCounts base)
      (False, (0, 0))
  -- The rejecting direction for CR 201.4, for the reason the two tests above give:
  -- Ancient Vendetta only exercises the ACCEPTING one, and a card that offends a
  -- lint must not be loadable, so the offenders are hand-built rather than filed.
  --
  -- The MODE'S TARGET SLOT is the fixture that separates this lint from the
  -- SameNameAsBound one above: that atom's accepted position is exactly this
  -- lint's offence, and vice versa, because CR 601.2c chooses targets before CR
  -- 608.2c has chosen any name.
  --
  -- Every fixture buries the atom under all three combinators, for the sibling's
  -- reason, and each is asserted through hasChosenNameCounts as well as the
  -- predicate.
  Spec.it s "the lint itself catches HasChosenName outside a search's filter or a mill's tally" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    vendetta <- S.printingOf s registry "Ancient Vendetta"
    let base = S.combinedFace piker
        slot = SlotName.MkSlotName (Text.pack "target")
        atom = Filter.Type.HasChosenName
        buried = Filter.Type.And [Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not atom]]
        spellOf effects slots =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) slots))
            (ModeSelection.ChooseExactly 1)
        searchFor f = Effect.Search Search.MkSearch {Search.searcher = PlayerRef.Relative PlayerRelation.You, Search.owner = PlayerRef.Relative PlayerRelation.You, Search.zones = Set.singleton Zone.Library, Search.quantity = Just (Quantity.Type.Literal 1), Search.filter = f, Search.upTo = False, Search.destination = SearchDestination.Exile}
        planted =
          [ ( "a mode's target slot",
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
                        Set.empty
                        Nothing
                        (NonEmpty.singleton (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1))))
                    ]
                }
            ),
            ( "CR 201.4a's own restriction on the choosing effect",
              base {Face.spell = spellOf [Effect.ChooseCardName buried] Map.empty}
            ),
            ( "an ObjectRef.EachMatching set",
              base {Face.spell = spellOf [Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching buried) Regenerability.Regenerable Nothing Nothing Nothing)] Map.empty}
            ),
            ( "CR 603.6a's trigger condition",
              base
                { Face.triggeredAbilities =
                    [ oneEffectTrigger
                        (TriggerCondition.PermanentEnters buried)
                        (Effect.Draw (Draw.MkDraw (PlayerRef.InSlot Binding.you) (Quantity.Type.Literal 1) Nothing))
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
            )
          ]
        report (label, card) = (label, hasChosenNameOffends card, hasChosenNameCounts card)
    Spec.assertEqWith
      s
      "every position that cannot answer is rejected, and the traversal is what finds it"
      (fmap report planted)
      (fmap (\(label, _) -> (label, True, (0, 1))) planted)
    Spec.assertEqWith
      s
      "and the codec counts exactly the atoms the traversal does"
      (fmap (\(_, card) -> jsonAtoms hasChosenNameTag (Codec.encode (Face.Codec.codec Card.codec) card)) planted)
      (fmap (const 1) planted)
    -- The ACCEPTING direction, twice: the real card, and the buried atom in a
    -- search's filter grafted onto a card that declares none of its own -- so the
    -- acceptance is about the POSITION and not about Ancient Vendetta.
    Spec.assertEqWith
      s
      "Ancient Vendetta is accepted"
      (hasChosenNameOffends (S.combinedFace vendetta), hasChosenNameCounts (S.combinedFace vendetta))
      (False, (1, 0))
    let searched = base {Face.spell = spellOf [searchFor buried] Map.empty}
    Spec.assertEqWith
      s
      "a buried atom in a search's filter is accepted"
      (hasChosenNameOffends searched, hasChosenNameCounts searched)
      (False, (1, 0))
    -- The second accepting position, grafted the same way (#2141): the mill arm
    -- overlays the chosen names onto the resolution's own context, so the tally's
    -- filter answers exactly as the search's does.
    let tallied = base {Face.spell = spellOf [Effect.Mill (Mill.MkMill (PlayerRef.Relative PlayerRelation.You) (Quantity.Type.Literal 1) (Just (MillTally.MkMillTally slot buried)) Nothing)] Map.empty}
    Spec.assertEqWith
      s
      "a buried atom in a mill's tally is accepted"
      (hasChosenNameOffends tallied, hasChosenNameCounts tallied)
      (False, (1, 0))
    Spec.assertEqWith
      s
      "and the ungrafted base card carries no atom at all"
      (hasChosenNameOffends base, hasChosenNameCounts base)
      (False, (0, 0))

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Card" $ do
  filterPositionLintSpec s registry
