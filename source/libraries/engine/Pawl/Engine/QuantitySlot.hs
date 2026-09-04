-- Every slot NAME a Pawl.Types.Quantity carries: the traversal that visits them,
-- the read derived from it and the rename derived from it. Structural only --
-- nothing here touches a GameState, which Pawl.Engine.Quantity's evaluation does.
--
-- A module of its own for a MODULE CYCLE rather than for cohesion.
-- Pawl.Engine.Modal is CR 700.2d's per-occurrence rename and sits below
-- Pawl.Engine.Card, which Pawl.Engine.Game imports; Pawl.Engine.Quantity and
-- Pawl.Engine.Count both read a board and so sit above Game. Splitting the walk
-- out is the repo's parametric-polymorphism escape one step further along: the
-- rename is a function of the syntax alone, so it belongs where the syntax is.
--
-- The Count traversal below is here for the same reason and no other: it is a
-- walk over Pawl.Types.Count's own field, and Pawl.Engine.Count -- where it would
-- otherwise live -- folds over the board.
module Pawl.Engine.QuantitySlot where

import qualified Data.Functor.Const as Const
import qualified Data.Functor.Identity as Identity
import qualified Data.Monoid as Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.CompletedDungeon as CompletedDungeon
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.ManaCount as ManaCount.Type
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Plus as Plus
import Pawl.Types.Quantity (Quantity)
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import Pawl.Types.SlotName (SlotName)

-- The per-member quantity of a count, VISITED -- the one walk the four functions
-- below are each an instance of, so a new Aggregation arm carrying a quantity
-- fails to compile once rather than being silently skipped by whichever of them
-- nobody rewrote. Reading a slot name and renaming one must agree about where a
-- quantity is, which is the whole reason this is a traversal.
--
-- Only Aggregation.Greatest carries a quantity; the other two aggregate the
-- matched set alone, and neither the Scope nor the Filter holds one.
overCount :: (Applicative f) => (quantity -> f quantity) -> Count.Type.Count quantity -> f (Count.Type.Count quantity)
overCount f count = case Count.Type.aggregation count of
  Aggregation.Members -> pure count
  Aggregation.DistinctCardTypes -> pure count
  Aggregation.Greatest quantity -> fmap (\q -> count {Count.Type.aggregation = Aggregation.Greatest q}) (f quantity)

-- overCount as a fold: whatever the per-member quantity contributes. The binding
-- slots it reads, for the two readers in Pawl.Engine.Quantity that want a Set.
foldCount :: (Monoid m) => (quantity -> m) -> Count.Type.Count quantity -> m
foldCount f = Const.getConst . overCount (Const.Const . f)

-- Does the per-member quantity of a count satisfy the predicate?
-- Pawl.Engine.Quantity.readsX is the one caller.
anyCount :: (quantity -> Bool) -> Count.Type.Count quantity -> Bool
anyCount predicate = Monoid.getAny . foldCount (Monoid.Any . predicate)

-- The count with its per-member quantity REWRITTEN. The two aggregations carrying
-- no quantity are returned untouched, there being nothing there to rewrite.
mapCount :: (quantity -> quantity) -> Count.Type.Count quantity -> Count.Type.Count quantity
mapCount f = Identity.runIdentity . overCount (Identity.Identity . f)

-- Every slot NAME a quantity carries in an AMOUNT position, as one traversal:
-- `slots` below READS them and `renameSlots` REWRITES them. One walk rather than
-- two, for the reason Pawl.Engine.Filter.overBoundSlots is one: the two consumers
-- disagreeing about which arms name a slot is a live defect shape, and CR 700.2d's
-- per-occurrence rename skipping an arm this one reported is the shape that
-- shipped.
--
-- The AMOUNT half only. A PlayerRef buried in one of these arms names a TARGET
-- slot rather than an amount, `slots` deliberately leaves it to Resolve.slotsOf,
-- and nestedRefs below is what reports it -- so renameSlots walks that half
-- separately rather than widening this traversal and the lint with it.
overSlots :: (Applicative f) => (SlotName -> f SlotName) -> Quantity -> f Quantity
overSlots f quantity = case quantity of
  Quantity.Literal _ -> pure quantity
  Quantity.ManaValue -> pure quantity
  Quantity.Power -> pure quantity
  Quantity.Toughness -> pure quantity
  Quantity.InSlot slot -> fmap Quantity.InSlot (f slot)
  Quantity.Star -> pure quantity
  Quantity.Plus (Plus.MkPlus a b) -> fmap Quantity.Plus (Plus.MkPlus <$> recur a <*> recur b)
  -- Composition, as Plus is: the rounding names no slot and the payload may name
  -- any.
  Quantity.Halved (Halved.MkHalved rounding inner) -> fmap (Quantity.Halved . Halved.MkHalved rounding) (recur inner)
  -- Whatever the payload names, since a minus sign changes no slot: Toxic
  -- Deluge's -X is a Negate over the InSlot that names X. A REGRESSION FENCE
  -- rather than proven behaviour -- emptying this arm leaves the suite green,
  -- because the consumer that could tell (CR 603.3b's orderInert, through
  -- Resolve.modeSlots) is reached only by a TRIGGERED ability, and no card in
  -- the pool negates a slot read inside one.
  Quantity.Negate a -> fmap Quantity.Negate (recur a)
  -- Terminating for the reason evaluate's Count arm is: a Greatest's payload is
  -- a strictly smaller subterm.
  Quantity.Count c -> fmap Quantity.Count (overCount recur c)
  -- Neither half of a ManaCount holds an AMOUNT slot: a ManaFilter names no slot
  -- at all, and PlayerRef.InSlot names a TARGET slot, which is Resolve's half of
  -- the lint. Count's Scope is in the same position.
  --
  -- Resolve.slotsOf does NOT in fact recover such a nested ref, so inside an
  -- EFFECT's quantity no lint sees it (#1079). nestedRefs below reports these
  -- arms, which is what Resolve.targetSlotSlots reads for a CR 202.3 computed
  -- bound and what slotsAreExhaustive reads so the CR 603.3b elision cannot rest
  -- on the gap.
  Quantity.ManaCount _ -> pure quantity
  -- The same position a third time: this arm's PlayerRef.InSlot names a TARGET
  -- slot, not an amount one.
  Quantity.LifeTotal _ -> pure quantity
  -- And a fourth: LifeTotal's sibling carries a PlayerRef in the same position.
  Quantity.Speed _ -> pure quantity
  -- And a fifth, CR 725.1's designation -- a PlayerRef and nothing else.
  Quantity.IsMonarch _ -> pure quantity
  -- And a sixth, CR 103.1's -- the same position again.
  Quantity.IsStartingPlayer _ -> pure quantity
  -- And a seventh, CR 102.1's -- the same position once more.
  Quantity.IsActivePlayer _ -> pure quantity
  -- And an eighth. The PlayerCounterKind beside it names no slot either.
  Quantity.PlayerCounters {} -> pure quantity
  -- A bare CounterKind, which names no slot at all -- this arm carries no
  -- reference of any sort, the object being the one the evaluation is aimed at.
  Quantity.ObjectCounters _ -> pure quantity
  -- The kind-agnostic reading of that same arm, naming no slot for its reason and
  -- carrying not even a CounterKind.
  Quantity.ObjectCountersOfAnyKind -> pure quantity
  -- The designation, which carries no reference either -- ObjectCounters' position,
  -- with which designation in the kind's place.
  Quantity.HasDesignation _ -> pure quantity
  Quantity.ClassLevel -> pure quantity
  Quantity.WasKicked -> pure quantity
  -- CR 702.33f's read, WasKicked's arm above in every respect: the Cost it
  -- carries is the IDENTIFIER of one kicker ability, matched against the spell's
  -- own record by equality, never an instruction this traversal descends into.
  Quantity.TimesKickedWith _ -> pure quantity
  Quantity.TagWasSpent {} -> pure quantity
  Quantity.WasToken -> pure quantity
  Quantity.WasBlocking -> pure quantity
  -- CR 120.1's damage total, naming no slot either: it carries no reference at
  -- all, the object being the one the evaluation is aimed at.
  Quantity.DamageDealtToThisTurn -> pure quantity
  -- And a ninth PlayerRef in that same position, CR 508.3b's record having
  -- nothing else on it.
  Quantity.OpponentsAttacked _ -> pure quantity
  -- And a tenth, CR 701.9a's tally having nothing beside its PlayerRef either.
  Quantity.CardsDiscardedThisTurn _ -> pure quantity
  -- And another, CR 119.3's life-gain tally likewise.
  Quantity.LifeGainedThisTurn _ -> pure quantity
  -- And another, CR 120.1's damage tally likewise.
  Quantity.PlayersDealtDamageThisTurn _ -> pure quantity
  -- And another, CR 120.1's damage total likewise.
  Quantity.DamageDealtToPlayersThisTurn _ -> pure quantity
  -- And another in that same position, CR 601.2i's cast tally having nothing
  -- beside its PlayerRef either.
  Quantity.SpellsCastLastTurn _ -> pure quantity
  -- And another again, CR 309.7's completion tally having nothing beside its
  -- PlayerRef either -- nor the named read beside it, whose CardName is a printed
  -- name rather than anything a slot could bind.
  Quantity.DungeonsCompleted _ -> pure quantity
  Quantity.CompletedDungeon {} -> pure quantity
  -- And a nullary arm, which names nothing at all: CR 400.7's entry is read
  -- against the object the evaluation is aimed at, as ObjectCounters is.
  Quantity.EnteredThisTurn -> pure quantity
  -- And two more with nothing beside their InZone's PlayerRef, CR 400.7's origin
  -- zone and CR 601.2a's cast zone alike.
  Quantity.EnteredFrom _ -> pure quantity
  Quantity.WasCastFrom _ -> pure quantity
  -- And a nullary arm, which names nothing at all: CR 509.1h's declaration is
  -- read against the object the evaluation is aimed at, as ObjectCounters is.
  Quantity.BlockersBeyondFirst -> pure quantity
  -- And a nullary arm too: CR 702.184c's substitution reads no slot of its
  -- own, the object being the one the evaluation is aimed at, as
  -- BlockersBeyondFirst is.
  Quantity.StationMeasure -> pure quantity
  -- The one arm that names a TARGET slot and is visited here anyway. Every other
  -- nested target slot is a PlayerRef this function leaves to Resolve.slotsOf,
  -- which cannot see it (#1079); reporting this one is what lets slotsOf recover
  -- it, and so what keeps Soul's Majesty's declared target on the read side of
  -- the D4 lint. The payload may hide slots of its own.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> fmap Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot <$> f slot <*> recur inner)
  where
    recur = overSlots f

-- The binding slots a quantity READS. The read half of the dataflow lint whose
-- write half is Resolve.definedSlots -- so a card whose "for each ... destroyed
-- this way" names a slot nothing binds is a failing test, not a silent no-op.
--
-- Binding.variableX is reported like any other slot, which is what #14 bought:
-- the "reads X iff the cost declares {X}" lint is then just the ordinary
-- available-slots comparison, because Pawl.CardSpec puts variableX on the
-- AVAILABLE side exactly when the cost prints an {X}. No arm here has to know
-- that X is special, and nothing subtracts it -- the fact lives where it belongs,
-- in what casting makes available.
slots :: Quantity -> Set SlotName
slots = Const.getConst . overSlots (Const.Const . Set.singleton)

-- Every slot NAME a quantity carries, rewritten -- overSlots' amount half above
-- and the target slots its nested references name, which nestedRefs below is the
-- reading counterpart of. CR 700.2d's per-occurrence rename is the caller
-- (Pawl.Engine.Modal.instanceScope): a target slot's CR 202.3 computed bound may
-- name a SIBLING slot of the same mode, so a mode chosen twice would otherwise
-- judge occurrence 1's bound against occurrence 0's answer. The function it is
-- given is a PARTIAL rename (Modal.ownSlot), so a name the mode does not declare
-- comes back unchanged.
--
-- Two walks composed rather than one, because the two halves are not the same
-- question: widening overSlots to the references would widen `slots` with it and
-- report a target slot to the amount lint. Proved by Pawl.TargetSpec's "CR 700.2d
-- a repeated mode's computed bound measures its own occurrence's sibling slot".
renameSlots :: (SlotName -> SlotName) -> Quantity -> Quantity
renameSlots rename = Identity.runIdentity . overSlots (Identity.Identity . rename) . renameRefSlots rename

-- renameSlots' second half: the slots a quantity names THROUGH a reference -- a
-- PlayerRef, or CR 400.7j's fold over a bound slot. mapPlayerRefs is the shared
-- traversal, so a new arm carrying a reference fails to compile there rather than
-- keeping a printed name here.
renameRefSlots :: (SlotName -> SlotName) -> Quantity -> Quantity
renameRefSlots rename = go
  where
    go =
      mapPlayerRefs
        (renamePlayerRef rename)
        -- Both halves, bakeBound's arm one rewrite over: the Scope says whose
        -- zone, which players or which bound slot, and a Greatest's per-member
        -- quantity may hide a reference of its own.
        (\c -> (mapCount go c) {Count.Type.scope = renameScope rename (mapScope (renamePlayerRef rename) (Count.Type.scope c))})

-- Every read of a slot this quantity makes that `slots` above does not report: a
-- PlayerRef nested inside it, which names a TARGET slot rather than an amount one
-- and which that function leaves to Resolve.slotsOf -- and slotsOf cannot see a
-- reference buried in a quantity (#1079) -- plus CR 400.7j's Scope.OverBound,
-- which names a slot outright. `Left` is a reference, whose ARITY only the reader
-- knows (Resolve.playerRefSlots); `Right` is a slot named directly.
--
-- Two callers want different halves of it, which is why this is the set of reads
-- rather than a Bool: Pawl.Engine.Quantity.slotsAreExhaustive asks whether any of
-- them names a slot at all, and Resolve.targetSlotSlots turns them into the slots a
-- CR 202.3 computed bound reads, so the D4 dataflow lint can see a bound naming a
-- slot its carrier never binds.
--
-- One arm per constructor, no wildcard, for `slots`' reason: a new quantity arm
-- carrying a reference must answer here rather than default to reading nothing,
-- which would both hide a dead bound and license an unsound elision.
nestedRefs :: Quantity -> Set (Either PlayerRef.PlayerRef SlotName)
nestedRefs quantity = case quantity of
  Quantity.Literal _ -> Set.empty
  Quantity.ManaValue -> Set.empty
  Quantity.Power -> Set.empty
  Quantity.Toughness -> Set.empty
  -- The amount reader, which `slots` above reports itself.
  Quantity.InSlot _ -> Set.empty
  Quantity.Star -> Set.empty
  Quantity.Plus (Plus.MkPlus a b) -> Set.union (nestedRefs a) (nestedRefs b)
  -- Plus' answer: the rounding hides no reference, so what the payload hides is
  -- the whole question.
  Quantity.Halved (Halved.MkHalved _ inner) -> nestedRefs inner
  Quantity.Negate a -> nestedRefs a
  -- Both halves `slots` skips: the Scope's own read, and the per-member quantity
  -- of a Greatest, which may hide a reference of its own.
  Quantity.Count c -> Set.union (scopeRefs (Count.Type.scope c)) (foldCount nestedRefs c)
  Quantity.ManaCount c -> Set.singleton (Left (ManaCount.Type.player c))
  Quantity.LifeTotal ref -> Set.singleton (Left ref)
  Quantity.Speed ref -> Set.singleton (Left ref)
  Quantity.IsMonarch ref -> Set.singleton (Left ref)
  Quantity.IsStartingPlayer ref -> Set.singleton (Left ref)
  Quantity.IsActivePlayer ref -> Set.singleton (Left ref)
  Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally ref _) -> Set.singleton (Left ref)
  Quantity.ObjectCounters _ -> Set.empty
  Quantity.ObjectCountersOfAnyKind -> Set.empty
  Quantity.HasDesignation _ -> Set.empty
  Quantity.ClassLevel -> Set.empty
  Quantity.WasKicked -> Set.empty
  -- CR 702.33f's read, WasKicked's arm above in every respect: the Cost it
  -- carries is the IDENTIFIER of one kicker ability, matched against the spell's
  -- own record by equality, never an instruction this traversal descends into.
  Quantity.TimesKickedWith _ -> Set.empty
  Quantity.TagWasSpent {} -> Set.empty
  Quantity.WasToken -> Set.empty
  Quantity.WasBlocking -> Set.empty
  Quantity.DamageDealtToThisTurn -> Set.empty
  Quantity.OpponentsAttacked ref -> Set.singleton (Left ref)
  Quantity.CardsDiscardedThisTurn ref -> Set.singleton (Left ref)
  Quantity.LifeGainedThisTurn ref -> Set.singleton (Left ref)
  Quantity.PlayersDealtDamageThisTurn ref -> Set.singleton (Left ref)
  Quantity.DamageDealtToPlayersThisTurn ref -> Set.singleton (Left ref)
  Quantity.SpellsCastLastTurn ref -> Set.singleton (Left ref)
  Quantity.DungeonsCompleted ref -> Set.singleton (Left ref)
  Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon ref _) -> Set.singleton (Left ref)
  Quantity.EnteredThisTurn -> Set.empty
  Quantity.EnteredFrom inZone -> Set.singleton (Left (InZone.player inZone))
  Quantity.WasCastFrom inZone -> Set.singleton (Left (InZone.player inZone))
  Quantity.BlockersBeyondFirst -> Set.empty
  -- Reads a live grant off the resolving object rather than a bound reference
  -- or slot: nothing here for a target-slot walk to find.
  Quantity.StationMeasure -> Set.empty
  -- Its own slot is left out because `slots` above DOES report it, unlike the
  -- nested PlayerRefs; the payload is walked like any other.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot _ inner) -> nestedRefs inner

-- A scope's own read. Both scopes that name players take a PlayerRef and CR
-- 608.2i's look-back names nothing, so the same question as the arms above --
-- except CR 400.7j's fold, which names a slot rather than a reference.
--
-- The OverBound arm's read is PROVEN rather than fenced: Pawl.CardSpec's "the
-- lint itself catches a computed bound naming a slot through a player" plants a
-- bound scoped this way and asserts the slot is reported.
scopeRefs :: Scope.Scope -> Set (Either PlayerRef.PlayerRef SlotName)
scopeRefs scope = case scope of
  Scope.InZone (InZone.MkInZone _ ref) -> Set.singleton (Left ref)
  Scope.InHistory _ -> Set.empty
  Scope.OverPlayers ref -> Set.singleton (Left ref)
  Scope.OverBound slot -> Set.singleton (Right slot)

-- Every slot NAME renameSlots above would rewrite, as a set: `slots`' amount half
-- plus the target slots the nested references name. The READING partner of that
-- rename, and the one function a caller wants when the question is "which of this
-- announcement's own slots does this bound depend on" --
-- Pawl.Engine.Target.jointlyJudged and legalSetsGiven both ask it, and
-- Pawl.Engine.Resolve.targetSlotSlots asks the same question with arities.
allSlots :: Quantity -> Set SlotName
allSlots quantity = Set.union (slots quantity) (refSlots quantity)

-- allSlots' second half on its own: the slots nestedRefs finds, with each
-- PlayerRef turned into the slots it names. renamePlayerRef's reading partner
-- through playerRefSlots, arm for arm.
refSlots :: Quantity -> Set SlotName
refSlots = foldMap (either playerRefSlots Set.singleton) . nestedRefs

-- The slots one reference names -- renamePlayerRef's reading partner, derived
-- from the same traversal so the two cannot disagree about which arms name one.
-- Pawl.Engine.Resolve.Slots.playerRefSlots is the third reader of this classification
-- and adds the ARITY each read has, which no rename needs.
playerRefSlots :: PlayerRef.PlayerRef -> Set SlotName
playerRefSlots = Const.getConst . overPlayerRefSlots (Const.Const . Set.singleton)

-- One reference's slots, renamed. CR 700.2d's rename reaches a PlayerRef because
-- a target slot's CR 202.3 computed bound may read one ("where X is the amount of
-- life that player gained this turn"), and "that player" is then a sibling slot of
-- the same mode.
renamePlayerRef :: (SlotName -> SlotName) -> PlayerRef.PlayerRef -> PlayerRef.PlayerRef
renamePlayerRef rename = Identity.runIdentity . overPlayerRefSlots (Identity.Identity . rename)

-- Every slot NAME a reference carries, as one traversal -- the two functions above
-- are its Const and Identity instances. Exhaustive, bakePlayerRef's posture: a new
-- reference naming a slot must answer here rather than keep a printed name.
overPlayerRefSlots :: (Applicative f) => (SlotName -> f SlotName) -> PlayerRef.PlayerRef -> f PlayerRef.PlayerRef
overPlayerRefSlots f ref = case ref of
  PlayerRef.InSlot slot -> fmap PlayerRef.InSlot (f slot)
  PlayerRef.EachInSlot slot -> fmap PlayerRef.EachInSlot (f slot)
  -- The slot decides who is left OUT rather than who is in, which changes nothing
  -- about whose namespace the name is in.
  PlayerRef.EachPlayerExcept slot -> fmap PlayerRef.EachPlayerExcept (f slot)
  PlayerRef.ControllerOfBound slot -> fmap PlayerRef.ControllerOfBound (f slot)
  PlayerRef.Attacking attacking -> fmap (\slot -> PlayerRef.Attacking attacking {AttackingPlayers.attacked = slot}) (f (AttackingPlayers.attacked attacking))
  -- The four that name no slot at all: the table, CR 109.5's relation, a baked
  -- seat, and the candidate whichever fold is running supplies.
  PlayerRef.EachPlayer -> pure ref
  PlayerRef.Relative _ -> pure ref
  PlayerRef.Specific _ -> pure ref
  PlayerRef.Candidate -> pure ref

-- A scope's own slot, renamed -- scopeRefs' Right arm turned around, and paired
-- with it arm for arm. Only CR 400.7j's fold names a slot outright; the players
-- the other two name are PlayerRefs, which renameRefSlots rewrites through
-- mapScope instead.
renameScope :: (SlotName -> SlotName) -> Scope.Scope -> Scope.Scope
renameScope rename scope = case scope of
  Scope.OverBound slot -> Scope.OverBound (rename slot)
  Scope.InZone _ -> scope
  Scope.OverPlayers _ -> scope
  Scope.InHistory _ -> scope

-- Every PlayerRef this quantity names, rewritten -- the traversal bakeBound and
-- forCandidate share, so the arm list is written once and a new arm carrying a
-- PlayerRef fails to compile HERE rather than silently keeping an old reference
-- in one of them.
--
-- `intoCount` is the one arm the two callers disagree about, so it is a parameter
-- rather than a recursive call: both rewrite the count's SCOPE through mapScope,
-- and only bakeBound descends into its per-member quantity, a Count being read
-- against an environment of its own (see forCandidate).
mapPlayerRefs ::
  (PlayerRef.PlayerRef -> PlayerRef.PlayerRef) ->
  (Count.Type.Count Quantity -> Count.Type.Count Quantity) ->
  Quantity ->
  Quantity
mapPlayerRefs f intoCount quantity = case quantity of
  Quantity.LifeTotal ref -> Quantity.LifeTotal (f ref)
  Quantity.Speed ref -> Quantity.Speed (f ref)
  Quantity.IsMonarch ref -> Quantity.IsMonarch (f ref)
  Quantity.IsStartingPlayer ref -> Quantity.IsStartingPlayer (f ref)
  Quantity.IsActivePlayer ref -> Quantity.IsActivePlayer (f ref)
  Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally ref kind) -> Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally (f ref) kind)
  Quantity.OpponentsAttacked ref -> Quantity.OpponentsAttacked (f ref)
  Quantity.CardsDiscardedThisTurn ref -> Quantity.CardsDiscardedThisTurn (f ref)
  Quantity.LifeGainedThisTurn ref -> Quantity.LifeGainedThisTurn (f ref)
  Quantity.PlayersDealtDamageThisTurn ref -> Quantity.PlayersDealtDamageThisTurn (f ref)
  Quantity.DamageDealtToPlayersThisTurn ref -> Quantity.DamageDealtToPlayersThisTurn (f ref)
  Quantity.SpellsCastLastTurn ref -> Quantity.SpellsCastLastTurn (f ref)
  Quantity.DungeonsCompleted ref -> Quantity.DungeonsCompleted (f ref)
  Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon ref name) -> Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon (f ref) name)
  Quantity.EnteredFrom z -> Quantity.EnteredFrom z {InZone.player = f (InZone.player z)}
  Quantity.WasCastFrom z -> Quantity.WasCastFrom z {InZone.player = f (InZone.player z)}
  Quantity.ManaCount c -> Quantity.ManaCount c {ManaCount.Type.player = f (ManaCount.Type.player c)}
  Quantity.Count c -> Quantity.Count (intoCount c)
  Quantity.Plus (Plus.MkPlus a b) -> Quantity.Plus (Plus.MkPlus (recur a) (recur b))
  Quantity.Halved (Halved.MkHalved rounding inner) -> Quantity.Halved (Halved.MkHalved rounding (recur inner))
  Quantity.Negate a -> Quantity.Negate (recur a)
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot (recur inner))
  -- Every arm below holds no PlayerRef and no Quantity. InSlot names an AMOUNT
  -- slot rather than a player one, so nothing here substitutes it -- an amount an
  -- earlier effect bound is not a seat.
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.Power -> quantity
  Quantity.Toughness -> quantity
  Quantity.InSlot _ -> quantity
  Quantity.Star -> quantity
  Quantity.ObjectCounters _ -> quantity
  Quantity.ObjectCountersOfAnyKind -> quantity
  Quantity.HasDesignation _ -> quantity
  Quantity.ClassLevel -> quantity
  Quantity.WasKicked -> quantity
  -- CR 702.33f's read, WasKicked's arm above in every respect: the Cost it
  -- carries is the IDENTIFIER of one kicker ability, matched against the spell's
  -- own record by equality, never an instruction this traversal descends into.
  Quantity.TimesKickedWith _ -> quantity
  Quantity.TagWasSpent {} -> quantity
  Quantity.WasToken -> quantity
  Quantity.WasBlocking -> quantity
  Quantity.DamageDealtToThisTurn -> quantity
  Quantity.EnteredThisTurn -> quantity
  Quantity.BlockersBeyondFirst -> quantity
  Quantity.StationMeasure -> quantity
  where
    recur = mapPlayerRefs f intoCount

-- A scope's reference, rewritten. Both scopes that name players take one; CR
-- 608.2i's look-back names none. Shared by bakeBound and forCandidate for
-- mapPlayerRefs' reason: a new scope carrying a reference has to fail to compile
-- here rather than keep an old one in either of them.
mapScope :: (PlayerRef.PlayerRef -> PlayerRef.PlayerRef) -> Scope.Scope -> Scope.Scope
mapScope f scope = case scope of
  Scope.InZone (InZone.MkInZone zone ref) -> Scope.InZone (InZone.MkInZone zone (f ref))
  Scope.OverPlayers ref -> Scope.OverPlayers (f ref)
  Scope.InHistory _ -> scope
  -- CR 400.7j's fold names a SLOT rather than a player reference, so there is
  -- nothing here for either baking to rewrite.
  Scope.OverBound _ -> scope
