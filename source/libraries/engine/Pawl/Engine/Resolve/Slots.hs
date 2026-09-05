-- The static reading of an effect: which slots it defines and reads, which
-- object and player references it carries, whether its slots are exhaustive
-- and whether it reads X. Pure over the effect's shape; Pawl.CardSpec's lints
-- and Pawl.Engine.Resolve.Effect both ask it. Split out of
-- Pawl.Engine.Resolve for size; nothing here resolves anything.
module Pawl.Engine.Resolve.Slots where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.QuantitySlot as QuantitySlot
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Amass as Amass.Type
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.ChoosePlayer as ChoosePlayer
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.ObjectRef (ObjectRef)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import Pawl.Types.PlayerRef (PlayerRef)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity.Type
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import Pawl.Types.SlotArity (SlotArity)
import qualified Pawl.Types.SlotArity as SlotArity
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR
import qualified Pawl.Types.ZoneScope as ZoneScope

-- The read side of the D4 dataflow lint: WHICH slots, and HOW MANY recipients
-- apiece (a slot read through an ObjectRef can hold CR 601.2c's "up to two").
-- These combinators join, taking the conservative arity wherever two reads
-- disagree; written out rather than left to Map's left-biased Monoid.
joinTwo :: Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity
joinTwo = Map.unionWith min

joinSlots :: [Map.Map SlotName SlotArity] -> Map.Map SlotName SlotArity
joinSlots = foldr joinTwo Map.empty

oneSlot :: SlotName -> Map.Map SlotName SlotArity
oneSlot slot = Map.singleton slot SlotArity.One

insertOne :: SlotName -> Map.Map SlotName SlotArity -> Map.Map SlotName SlotArity
insertOne slot = joinTwo (oneSlot slot)

-- The slots a Quantity reads, on both halves of Pawl.Types.SlotName's one flat
-- namespace. Quantity.objectSlots names an OBJECT and evaluates against it, so a
-- slot naming several leaves Pawl.Engine.Filter.slotOneObject with nothing to
-- pick and the whole number unanswered -- SlotArity.One. Every other slot
-- QuantitySlot.slots reports is a Quantity.InSlot, which reads the slot's AMOUNT
-- instead (Pawl.Engine.Binding.amountOf) and cannot be damaged by a plural slot
-- at all -- SlotArity.Amount, an entry stating a read without claiming an arity.
--
-- Both halves are reported so that the KEYS stay QuantitySlot.slots' whole answer:
-- an InSlot read is a read, and the D4 dataflow lint counts it; see #2774.
-- Map.union is left-biased, so a slot read both ways is One.
quantitySlots :: Quantity.Type.Quantity -> Map.Map SlotName SlotArity
quantitySlots quantity =
  Map.union
    (Map.fromSet (const SlotArity.One) (Quantity.objectSlots quantity))
    (Map.fromSet (const SlotArity.Amount) (QuantitySlot.slots quantity))

-- The Quantities an entry rider carries: CR 122.6's count per counter kind, which
-- a card may write as anything a Quantity spells. A position the three walkers
-- below would otherwise pass over -- every arm that reads a rider matches it as
-- `_` -- so the reads are spelled out here and each walker goes through this.
riderQuantities :: EntryRiders.EntryRiders Quantity.Type.Quantity -> [Quantity.Type.Quantity]
riderQuantities = Map.elems . EntryRiders.counters

-- The slot an entry rider READS, which is CR 509.4's blocking rider and only it:
-- every other rider is a flag or a Quantity (riderQuantities above). Read singly
-- -- CR 509.4 names one attacking creature.
--
-- BOTH opcodes reach it, and both apply it: a Create hands its tokens to
-- Pawl.Engine.Combat.putOntoBattlefieldBlocking from the minting loop (Flash
-- Foliage), a MoveToZone hands the card it moved to the same function from
-- moveOne (Aetherplasm). What stays inert is the rider on a destination other
-- than the battlefield, which Pawl.CardSpec lints.
riderSlots :: EntryRiders.EntryRiders count -> Map.Map SlotName SlotArity
riderSlots = maybe Map.empty oneSlot . EntryRiders.blocking

-- The slots a PlayerRef reads. Five arms name one: EachPlayerExcept, InSlot,
-- ControllerOfBound and Attacking at arity One, EachInSlot at arity Many. The
-- other four name none, and the arms below carry the reason for each arity that
-- is not self-evident.
playerRefSlots :: PlayerRef -> Map.Map SlotName SlotArity
playerRefSlots ref = case ref of
  PlayerRef.EachPlayer -> Map.empty
  -- The excluded seat is one player, so one slot read singly.
  PlayerRef.EachPlayerExcept slot -> Map.singleton slot SlotArity.One
  PlayerRef.Relative _ -> Map.empty
  PlayerRef.InSlot slot -> Map.singleton slot SlotArity.One
  -- Read at arity MANY, which is the whole of what parts it from the arm above.
  PlayerRef.EachInSlot slot -> Map.singleton slot SlotArity.Many
  PlayerRef.Specific _ -> Map.empty
  PlayerRef.Candidate -> Map.empty
  -- Read at arity one: a slot naming several objects names no one controller.
  PlayerRef.ControllerOfBound slot -> Map.singleton slot SlotArity.One
  -- Read at arity one for that arm's reason: a slot naming several players names
  -- no one player to have been attacked.
  PlayerRef.Attacking (AttackingPlayers.MkAttackingPlayers _ slot) -> Map.singleton slot SlotArity.One

-- The slots an AffectedPlayers reads. Only Named does, and only ever one player.
affectedPlayersSlots :: AffectedPlayers.AffectedPlayers SlotName -> Map.Map SlotName SlotArity
affectedPlayersSlots affected = case affected of
  AffectedPlayers.Scoped _ -> Map.empty
  AffectedPlayers.Named slot -> Map.singleton slot SlotArity.One

-- The slots a Chooser reads: only BoundInSlot, and only ever one player.
chooserSlots :: Chooser.Chooser -> Map.Map SlotName SlotArity
chooserSlots chooser = case chooser of
  Chooser.TheController -> Map.empty
  Chooser.EachInScope -> Map.empty
  Chooser.BoundInSlot slot -> Map.singleton slot SlotArity.One

-- The slots a ZoneScope reads. Only InSlot names one, and at arity Many: the
-- reader takes the whole recipient set, so "each of up to two target players'
-- graveyards" would be seen whole.
zoneScopeSlots :: ZoneScope.ZoneScope -> Map.Map SlotName SlotArity
zoneScopeSlots scope = case scope of
  ZoneScope.Scoped _ -> Map.empty
  ZoneScope.InSlot slot -> Map.singleton slot SlotArity.Many

-- The slots an ObjectRef reads. InSlot names one directly, and
-- EachCardInGraveyard and EachCardInHand name one through their scope; the other
-- sweeping arms name none. Reporting a scope's slot does not make the SWEPT CARDS targets -- CR
-- 115.10a needs the word "target" against them, and a graveyard scope says it
-- against the PLAYER -- so what CR 608.2b judges is still the card's own target
-- slot, holding that player, and not this read.
--
-- The PlayerRefs the four per-player arms hold are taken from
-- objectRefPlayerRefs below rather than arm by arm, so no arm of the case names
-- one.
objectRefSlots :: ObjectRef -> Map.Map SlotName SlotArity
objectRefSlots ref = joinTwo (joinSlots (fmap playerRefSlots (objectRefPlayerRefs ref))) $ case ref of
  ObjectRef.InSlot slot -> Map.singleton slot SlotArity.Many
  ObjectRef.EachMatching _ -> Map.empty
  -- The sweeping arms that DO name a slot are this one and EachCardInHand below:
  -- CR 400.1's per-player zones leave "whose" to be said, and Angel of Finality
  -- says it by pointing at the player another slot of the same announcement
  -- targets. Reported, not dropped, because the D4 dataflow lint reads this: a
  -- card whose ONLY use of that slot is the scope would otherwise declare a
  -- target nothing reads.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard scope _) -> zoneScopeSlots scope
  ObjectRef.EachCardInYourHand -> Map.empty
  -- The arm above's scope over the other per-player zone, reported for its
  -- reason: Amnesia's ONLY use of its target slot is this scope, so dropping the
  -- read would have the D4 dataflow lint call that target unread.
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand scope _) -> zoneScopeSlots scope
  -- EachCardInYourHand's answer over the other hidden per-player zone: the
  -- seat is CR 109.5's "you", so no slot names it.
  ObjectRef.EachCardInYourLibrary _ -> Map.empty
  ObjectRef.EachCardExiledWithSource {} -> Map.empty
  ObjectRef.EachSpell _ -> Map.empty
  ObjectRef.EachOnStack _ -> Map.empty
  ObjectRef.EachPlayer -> Map.empty
  ObjectRef.EachOpponent -> Map.empty
  -- The seat comes from the source's own entry choice (CR 614.12a), not a slot.
  ObjectRef.ChosenPlayer -> Map.empty
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary _ count) -> quantitySlots count
  -- The arm above's count, and no more: the SEAT is objectRefPlayerRefs' half.
  -- What a MATCH is is a Filter, and no arm here reports the slots a Filter
  -- reads, for the reason the header states.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil _ _ count) -> quantitySlots count
  -- Both halves name a slot: WHO CHOOSES through the Chooser, and WHOSE
  -- graveyards through the scope -- reported for EachCardInGraveyard's reason,
  -- since Grasping Tentacles' scope is a read of the slot its own mill targets.
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser scope _) -> joinTwo (chooserSlots chooser) (zoneScopeSlots scope)
  -- CR 402.3: the choosers own the hands, so the PlayerRef is the whole read --
  -- reported by objectRefPlayerRefs rather than here.
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand _ _) -> Map.empty
  -- The first of the two arms whose CANDIDATES come from a slot -- the plural is
  -- below -- where the two chosen arms above name a slot only through their
  -- chooser. Reported for EachCardInGraveyard's
  -- reason -- the D4 dataflow lint reads this, and a group one clause binds and a
  -- later one reads is exactly the dataflow that lint checks. Many, not One,
  -- which is the arity InSlot reports of the same binding: the ref reads every
  -- member of the group to offer them.
  --
  -- Joined with the COUNT's own slots, TopOfLibrary's arm above and for its
  -- reason; the CHOOSER's are the generic playerRefSlots fold this case is joined
  -- into, which is what makes Animal Magnetism's ChoosePlayer slot a read.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong slot _ count _) -> joinTwo (Map.singleton slot SlotArity.Many) (quantitySlots count)
  -- The arm above's read, for its reasons: the candidates come from a slot, and
  -- the ref reads every member of the group to match them.
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong slot _) -> Map.singleton slot SlotArity.Many
  -- The seats whose hands randomness reads are ChosenCardInHand's, and reported
  -- where that arm's are.
  ObjectRef.RandomCardInHand _ -> Map.empty
  -- EachMatching's answer: the candidates come off the battlefield, so no slot
  -- names them and the chooser is CR 608.2c's resolving controller.
  ObjectRef.AnyNumberMatching _ -> Map.empty
  -- The arm above's answer, for its reason: the candidates come off the
  -- battlefield, so no slot names them and the chooser is CR 608.2c's resolving
  -- controller.
  ObjectRef.ChosenPermanent _ -> Map.empty
  -- The arm above's answer, for its reason: neither the source nor the
  -- candidates come out of a slot.
  ObjectRef.SourceAndChosenPermanent _ -> Map.empty

-- The Quantities an ObjectRef carries: the two library walks' counts.
-- Exhaustive, no wildcard, and every payload destructured positionally rather
-- than as `{}`: slotsAreExhaustive, readsX and Pawl.CardSpec's Count traversal
-- all reach a nested Quantity through this, over effectObjectRefs below rather
-- than arm by arm, so this is the one place a payload gaining a Quantity field
-- has to be revisited -- a `{}` here would keep compiling, which is the shape
-- that let a widened field go unread (#2729).
objectRefQuantities :: ObjectRef -> [Quantity.Type.Quantity]
objectRefQuantities ref = case ref of
  ObjectRef.InSlot _ -> []
  ObjectRef.EachMatching _ -> []
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard _ _) -> []
  ObjectRef.EachCardInYourHand -> []
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand _ _) -> []
  ObjectRef.EachCardInYourLibrary _ -> []
  ObjectRef.EachCardExiledWithSource _ -> []
  ObjectRef.EachSpell _ -> []
  ObjectRef.EachOnStack _ -> []
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  ObjectRef.ChosenPlayer -> []
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary _ count) -> [count]
  -- The arm above's count, measured in MATCHES rather than in cards.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil _ _ count) -> [count]
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard _ _ _) -> []
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand _ _) -> []
  -- How many cards are picked out of the group -- Ancestral Memories' printed
  -- two, the library walks' counts above being the only other ObjectRef numbers.
  -- A REGRESSION FENCE rather than proven behaviour: every count in the pool is a
  -- Literal, which reads no slot, so dropping this leaves the suite green.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong _ _ count _) -> [count]
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong _ _) -> []
  ObjectRef.RandomCardInHand _ -> []
  ObjectRef.AnyNumberMatching _ -> []
  ObjectRef.ChosenPermanent _ -> []
  ObjectRef.SourceAndChosenPermanent _ -> []

-- Every PlayerRef nested in one ObjectRef -- effectPlayerRefs' other half, and
-- the seat a per-player walk counts against. objectRefSlots takes its player
-- reads from here, so a reference dropped here stops being reported there.
--
-- No wildcard, objectRefQuantities' discipline above.
objectRefPlayerRefs :: ObjectRef -> [PlayerRef]
objectRefPlayerRefs ref = case ref of
  ObjectRef.InSlot _ -> []
  ObjectRef.EachMatching _ -> []
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard _ _) -> []
  ObjectRef.EachCardInYourHand -> []
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand _ _) -> []
  ObjectRef.EachCardInYourLibrary _ -> []
  ObjectRef.EachCardExiledWithSource _ -> []
  ObjectRef.EachSpell _ -> []
  ObjectRef.EachOnStack _ -> []
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  ObjectRef.ChosenPlayer -> []
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player _) -> [player]
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil player _ _) -> [player]
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard _ _ _) -> []
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand player _) -> [player]
  -- The seat that picks out of the group -- Animal Magnetism's opponent, and by
  -- default CR 608.2c's resolving controller.
  --
  -- The arm above's regression fence, for a different reason: the D4 dataflow lint
  -- subtracts a slot the mode's own ChoosePlayer DEFINES from both sides of its
  -- equality, so the only card whose chooser names a slot cannot observe this
  -- report. A chooser naming a DECLARED target slot would, and no printing writes
  -- one -- Pawl.Types.Chooser's BoundInSlot note says the same of its own.
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong _ _ _ chooser) -> [chooser]
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong _ _) -> []
  ObjectRef.RandomCardInHand player -> [player]
  ObjectRef.AnyNumberMatching _ -> []
  ObjectRef.ChosenPermanent _ -> []
  ObjectRef.SourceAndChosenPermanent _ -> []

-- The refs a CR 707.10 answer names: rule 707.10d's candidates, and nothing for
-- the other two, neither of which describes anything.
copyTargetsRefs :: CopyTargets.CopyTargets -> [ObjectRef]
copyTargetsRefs targets = case targets of
  CopyTargets.Copied -> []
  CopyTargets.ChosenByController -> []
  CopyTargets.ForEach ref -> [ref]

-- Every ObjectRef this ONE effect holds, its own only: a nested effect's refs
-- are its own answer here, reached by whichever caller recurses.
--
-- The single enumeration of where an ObjectRef sits in an opcode. Its readers
-- -- slotsOf here, and slotsAreExhaustive, readsX and Pawl.CardSpec's Count
-- traversal below -- call this rather than naming the opcodes themselves, so
-- they cannot come to disagree about which opcodes hold a ref. The test a
-- reader can apply: no arm of any of those four names an ObjectRef field.
--
-- No wildcard, and the arms that hold a ref destructure positionally: a new
-- opcode the compiler forces, and so does a new FIELD on a payload that already
-- holds one. What neither the compiler nor this shape catches is an existing
-- field WIDENED to an ObjectRef, which is how a nested Quantity went unread
-- once (#2729). Two things pay for that: slotsOf's corpus lints, since a ref
-- dropped here stops being reported there, and Pawl.CardSpec's planted
-- objectRefPositions, which is the only observer of a position no card writes.
effectObjectRefs :: Effect card ability -> [ObjectRef]
effectObjectRefs effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> Foldable.toList (fmap DamagePart.ref parts)
  Effect.Fight {} -> []
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ _ ref) -> [ref]
  Effect.ChangeText {} -> []
  Effect.AddMana {} -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  -- CR 727.5's exemption, absent when nothing is exempt.
  Effect.RestartGame exempt -> Maybe.maybeToList exempt
  Effect.ControlPlayerNextTurn {} -> []
  Effect.Destroy (Destroy.MkDestroy ref _ _ _ _) -> [ref]
  Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect ref _) -> [ref]
  Effect.Attach {} -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ _ _ _) -> [ref]
  Effect.Draw {} -> []
  Effect.Mill {} -> []
  Effect.Reveal (Reveal.MkReveal ref _) -> [ref]
  Effect.FromOutsideTheGame {} -> []
  Effect.ExileThisSpell -> []
  Effect.LookAt (LookAt.MkLookAt ref _) -> [ref]
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore ref -> [ref]
  Effect.Discard subject -> case subject of
    Discard.Counted {} -> []
    Discard.These ref -> [ref]
  Effect.LoseLife {} -> []
  Effect.GainLife {} -> []
  Effect.ExchangeLifeTotals {} -> []
  Effect.SetLifeTotal {} -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed {} -> []
  Effect.DecreaseSpeed {} -> []
  Effect.Create {} -> []
  Effect.Conjure {} -> []
  Effect.CreateCopy (CreateCopy.MkCreateCopy _ ref _) -> [ref]
  -- Both sides: CR 707.2's copiable values come off one and go onto the other.
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject) -> [original, subject]
  -- BOTH refs: CR 707.10d's candidates are named by a ref of their own, and a
  -- slot it reads is as much a read of this effect's as the copied object's.
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets) -> ref : copyTargetsRefs targets
  Effect.Replace {} -> []
  Effect.SkipNextPhase {} -> []
  -- Absent where the shield's recipients are described rather than named.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ ref _ _ _ _ _) -> Maybe.maybeToList ref
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ ref _ _ _ _ _) -> Maybe.maybeToList ref
  -- CR 614.9's two sides, the damage's old recipient -- absent where the card
  -- describes it instead -- and its new one.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage _ _ _ from _ _ to _) -> Maybe.maybeToList from <> [to]
  Effect.Counter (Counter.MkCounter ref _ _) -> [ref]
  Effect.PutCounters (PutCounters.MkPutCounters _ _ ref) -> [ref]
  Effect.RemoveCounters {} -> []
  -- CR 122.5's two sides, either of which may name a group.
  Effect.MoveCounters (MoveCounters.MkMoveCounters from _ _ to) -> [from, to]
  -- CR 122.8's taker; the giver is a slot.
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom _ _ ref) -> [ref]
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.PayAnyEnergy {} -> []
  Effect.Tap ref -> [ref]
  Effect.Untap ref -> [ref]
  Effect.Detain ref -> [ref]
  Effect.Goad ref -> [ref]
  Effect.DoesNotUntapNext ref -> [ref]
  Effect.Transform ref -> [ref]
  Effect.Convert ref -> [ref]
  -- The components; the combined face beside them is card data, not a ref.
  Effect.Meld (Meld.MkMeld objects _) -> [objects]
  Effect.PhaseOut ref -> [ref]
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown ref _) -> [ref]
  Effect.TurnFaceUp {} -> []
  Effect.RemoveFromCombat ref -> [ref]
  Effect.BecomesBlocked {} -> []
  Effect.AddPhases {} -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef _ ref) -> [ref]
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  -- CR 509.1a's two sides, the creature required to block and what it blocks.
  Effect.RequireBlock (RequireBlock.MkRequireBlock _ blocker attacker) -> [blocker, attacker]
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated _ ref) -> [ref]
  -- One side only: what a creature attacks is a player (CR 508.1b), so the arm
  -- beside this one is a PlayerRef.
  Effect.RequireAttack (RequireAttack.MkRequireAttack _ attacker _) -> [attacker]
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock _ ref) -> [ref]
  -- One side only, and only when a ref names it: the Matching arm is a Filter
  -- (CR 611.2c's class), and what the attack is aimed at is a PlayerScope.
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack _ affected _) -> case affected of
    RestrictedCreatures.Named ref -> [ref]
    RestrictedCreatures.Matching _ -> []
  Effect.CreateEmblem {} -> []
  Effect.BecomeMonarch {} -> []
  Effect.TakeTheInitiative {} -> []
  Effect.Designate {} -> []
  Effect.SetClassLevel {} -> []
  Effect.Unsuspect ref -> [ref]
  Effect.SetHalfLocked {} -> []
  Effect.Evolve {} -> []
  Effect.Mentor {} -> []
  Effect.Train {} -> []
  Effect.ItBecomes {} -> []
  Effect.ExileUntilMonarch {} -> []
  Effect.ExileHaunting {} -> []
  Effect.PlaySubgame {} -> []
  Effect.ChoosePlayer {} -> []
  Effect.ChooseOpponentAtRandom {} -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName {} -> []
  Effect.Bolster {} -> []
  Effect.Amass {} -> []
  Effect.Blight {} -> []
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.PlayerSacrifices {} -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary _ ref) -> [ref]
  Effect.Shuffle {} -> []
  Effect.OfferCast {} -> []
  Effect.GrantPlayFromExile (GrantPlayFromExile.MkGrantPlayFromExile _ ref _) -> [ref]
  Effect.MakePlotted ref -> [ref]
  -- CR 608.2f's set, swept once; the body's own refs are the caller's recursion.
  Effect.ForEach (ForEach.MkForEach ref _ _) -> [ref]

-- Every PlayerRef this ONE effect holds in a field of its own: not the ones
-- nested in an ObjectRef it carries (objectRefPlayerRefs), not the ones nested
-- in a Quantity (Pawl.Engine.Quantity's readers), and not a nested effect's,
-- which are its own answer here.
--
-- The single enumeration of where a PlayerRef sits in an opcode, and
-- effectObjectRefs' twin one type over. Its readers -- slotsOf here, and
-- Pawl.CardSpec's plural-slot lint -- call this rather than naming the opcodes
-- themselves, so they cannot come to disagree about which opcodes hold one. The
-- test a reader can apply: no arm of either names a PlayerRef field.
--
-- No wildcard, and the arms that hold a reference destructure positionally: a
-- new opcode the compiler forces, and so does a new FIELD on a payload that
-- already holds one. What neither the compiler nor this shape catches is an
-- existing field WIDENED to a PlayerRef; slotsOf's corpus lints pay for that,
-- since a reference dropped here stops being reported there, and so does
-- Pawl.CardSpec's planted playerRefPositions.
effectPlayerRefs :: Effect card ability -> [PlayerRef]
effectPlayerRefs effect = case effect of
  Effect.DealDamage {} -> []
  Effect.Fight {} -> []
  Effect.ModifyTarget {} -> []
  Effect.ChangeText {} -> []
  Effect.AddMana (ManaAddition.MkManaAddition ref _ _ _ _ _) -> [ref]
  Effect.Search (Search.MkSearch searcher owner _ _ _ _ _) -> [searcher, owner]
  Effect.ExileAllGraveyards -> []
  Effect.RestartGame {} -> []
  Effect.ControlPlayerNextTurn {} -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice {} -> []
  Effect.Attach {} -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.MoveToZone {} -> []
  Effect.Draw (Draw.MkDraw ref _ _) -> [ref]
  Effect.Mill (Mill.MkMill ref _ _ _) -> [ref]
  Effect.Reveal {} -> []
  Effect.FromOutsideTheGame {} -> []
  Effect.ExileThisSpell -> []
  Effect.LookAt {} -> []
  Effect.Scry (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.Explore {} -> []
  Effect.Discard {} -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.ExchangeLifeTotals {} -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.DecreaseSpeed (SpeedDecrease.MkSpeedDecrease ref _ _) -> [ref]
  Effect.Create (Create.MkCreate _ _ _ _ creator) -> [creator]
  Effect.Conjure {} -> []
  Effect.CreateCopy {} -> []
  Effect.BecomeCopy {} -> []
  Effect.CopyStackObject {} -> []
  Effect.Replace {} -> []
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase ref _) -> [ref]
  Effect.PreventNextDamage {} -> []
  Effect.PreventAllDamage {} -> []
  Effect.RedirectDamage {} -> []
  Effect.Counter {} -> []
  Effect.PutCounters {} -> []
  Effect.RemoveCounters {} -> []
  Effect.MoveCounters {} -> []
  Effect.PutCountersFrom {} -> []
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters ref _ _) -> [ref]
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters ref _ _) -> [ref]
  Effect.PayAnyEnergy {} -> []
  Effect.Tap {} -> []
  Effect.Untap {} -> []
  Effect.Detain {} -> []
  Effect.Goad {} -> []
  Effect.DoesNotUntapNext {} -> []
  Effect.Transform {} -> []
  Effect.Convert {} -> []
  Effect.Meld {} -> []
  Effect.PhaseOut {} -> []
  Effect.TurnFaceDown {} -> []
  Effect.TurnFaceUp {} -> []
  Effect.RemoveFromCombat {} -> []
  Effect.BecomesBlocked {} -> []
  Effect.AddPhases {} -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl {} -> []
  Effect.ArmDelayedTrigger {} -> []
  -- CR 400.1's zone reference, which lives inside the permission payloads rather
  -- than in a field of the opcode -- so the walk is
  -- Pawl.Engine.PlayerEffect.playerRefsIn's and this arm joins it in, which is
  -- what puts Sen Triplets' "that player's hand" slot into slotsOf's answer.
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers _ _ playerEffect) -> PlayerEffect.playerRefsIn playerEffect
  Effect.RequireBlock {} -> []
  Effect.CantBeRegenerated {} -> []
  Effect.RequireAttack (RequireAttack.MkRequireAttack _ _ defender) -> [defender]
  Effect.ForbidBlock {} -> []
  Effect.ForbidAttack {} -> []
  Effect.CreateEmblem {} -> []
  Effect.BecomeMonarch {} -> []
  Effect.TakeTheInitiative {} -> []
  Effect.Designate {} -> []
  Effect.SetClassLevel {} -> []
  Effect.Unsuspect {} -> []
  Effect.SetHalfLocked {} -> []
  Effect.Evolve {} -> []
  Effect.Mentor {} -> []
  Effect.Train {} -> []
  Effect.ItBecomes {} -> []
  Effect.ExileUntilMonarch {} -> []
  Effect.ExileHaunting {} -> []
  Effect.PlaySubgame {} -> []
  Effect.ChoosePlayer {} -> []
  Effect.ChooseOpponentAtRandom {} -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName {} -> []
  Effect.Bolster {} -> []
  Effect.Amass {} -> []
  Effect.Blight (PlayerQuantity.MkPlayerQuantity ref _) -> [ref]
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.PlayerSacrifices {} -> []
  Effect.TakeExtraTurn takeExtraTurn -> [TakeExtraTurn.player takeExtraTurn]
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named _) -> Maybe.maybeToList named
  Effect.Shuffle ref -> [ref]
  Effect.OfferCast (OfferCast.MkOfferCast _ caster _ _) -> [caster]
  Effect.GrantPlayFromExile {} -> []
  Effect.MakePlotted {} -> []
  Effect.ForEach {} -> []

-- The slots a MonarchTarget reads: only the targeted arm names one.
monarchTargetSlots :: MonarchTarget.MonarchTarget -> Map.Map SlotName SlotArity
monarchTargetSlots target = case target of
  MonarchTarget.TheController -> Map.empty
  MonarchTarget.ControllerOfSource -> Map.empty
  MonarchTarget.InSlot slot -> Map.singleton slot SlotArity.One

-- WithController reads one target; BetweenTargets takes both sides out of one
-- slot (CR 601.2c) and so must see the whole set.
exchangeSidesSlots :: ExchangeSides.ExchangeSides -> Map.Map SlotName SlotArity
exchangeSidesSlots sides = case sides of
  ExchangeSides.WithController slot -> Map.singleton slot SlotArity.One
  ExchangeSides.BetweenTargets slot -> Map.singleton slot SlotArity.Many

-- The one legitimate home of `case effect of`: this module is the VM's opcode
-- semantics (design.md section 1). slotsOf is the read half of the dataflow lint;
-- X is not one of its reads, readsX below being X's own half.
--
-- The ObjectRefs and the PlayerRefs this effect holds are taken from
-- effectObjectRefs and effectPlayerRefs at the head rather than arm by arm, so
-- no arm of the case names either.
slotsOf :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Map.Map SlotName SlotArity
slotsOf effect = joinTwo (joinTwo (joinSlots (fmap objectRefSlots (effectObjectRefs effect))) (joinSlots (fmap playerRefSlots (effectPlayerRefs effect)))) $ case effect of
  -- The dealer is a read like any other (CR 120.2b), and one object (CR 120.1).
  Effect.DealDamage (DealDamage.MkDealDamage parts dealer _) ->
    joinTwo
      (joinSlots (fmap (quantitySlots . DamagePart.quantity) (Foldable.toList parts)))
      (maybe Map.empty oneSlot dealer)
  -- BOTH fighters: CR 701.14a reads each one's power against the other, so a
  -- slot named by only one half would still look dangling.
  Effect.Fight (Fight.MkFight first second) -> joinTwo (oneSlot first) (oneSlot second)
  -- The modification's own quantities read slots too, through
  -- Projection.quantitiesOf.
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) ->
    joinTwo (joinSlots (fmap quantitySlots (Projection.quantitiesOf modification))) (durationSlots duration)
  Effect.ChangeText (ChangeText.MkChangeText _ _ slot) -> oneSlot slot
  Effect.AddMana {} -> Map.empty
  -- The count and the FILTER: both references are effectPlayerRefs' half, and a
  -- search filter naming a slot is a read like any other now that the arm
  -- matches it in the resolution's own context -- Bifurcate's "with the same
  -- name as target nontoken creature" is the whole of what its target slot is
  -- for, so without this the D4 dataflow lint would call that slot unread.
  Effect.Search (Search.MkSearch _ _ _ quantity filter_ _ _) ->
    joinTwo (joinSlots (fmap quantitySlots (Maybe.maybeToList quantity))) (filterSlotsOf filter_)
  Effect.ExileAllGraveyards -> Map.empty
  Effect.Proliferate -> Map.empty
  -- CR 201.4's name is not an object, so the choice binds no slot and the
  -- restriction Filter names none either -- a Filter reads a slot only through
  -- Filter.boundSlots, and no card writes one of those atoms here.
  Effect.ChooseCardName _ -> Map.empty
  -- No slot: the card comes from outside the game, where CR 400.11c lets nothing
  -- target and so nothing was announced (CR 601.2c).
  Effect.FromOutsideTheGame _ -> Map.empty
  Effect.ExileThisSpell -> Map.empty
  Effect.Bolster quantity -> quantitySlots quantity
  Effect.Amass (Amass.Type.MkAmass quantity _) -> quantitySlots quantity
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.TemptWithTheRing -> Map.empty
  Effect.Venture {} -> Map.empty
  Effect.ExileHandThenDraw -> Map.empty
  -- CR 101.4's "each player sacrifices": the arm takes every player recipient
  -- the slot holds, so the read is Many.
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot _ quantity) -> joinTwo (Map.singleton slot SlotArity.Many) (quantitySlots quantity)
  Effect.RestartGame _ -> Map.empty
  Effect.ControlPlayerNextTurn slot -> oneSlot slot
  -- The three slot fields are DEFINITIONS, not reads; they belong to boundSlots
  -- below.
  Effect.Destroy {} -> Map.empty
  -- The ref is the whole read, and the head above already took it.
  Effect.Sacrifice {} -> Map.empty
  Effect.TurnFaceDown {} -> Map.empty
  Effect.TurnFaceUp slot -> oneSlot slot
  Effect.RemoveFromCombat _ -> Map.empty
  Effect.BecomesBlocked slot -> oneSlot slot
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> joinTwo (joinSlots (fmap quantitySlots (riderQuantities riders))) (riderSlots riders)
  -- CR 121.1's bound slot is a DEFINITION, not a read: see boundSlots below.
  Effect.Draw (Draw.MkDraw _ quantity _) -> quantitySlots quantity
  -- The tally's slot and CR 701.17c's are DEFINITIONS, not reads: see boundSlots
  -- below.
  Effect.Mill (Mill.MkMill _ quantity _ _) -> quantitySlots quantity
  -- The bound slot is a DEFINITION, not a read.
  Effect.Reveal {} -> Map.empty
  -- The bound slot is a DEFINITION, not a read.
  Effect.LookAt {} -> Map.empty
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.Explore _ -> Map.empty
  Effect.Discard subject -> case subject of
    -- The bound slot is a DEFINITION, not a read, so it is not joined in here.
    -- Many, PlayerSacrifices' arity and for its reason: CR 101.4's worked
    -- example is a table-wide edict, and the resolution arm below folds over
    -- every player the slot names.
    Discard.Counted (CountedDiscard.MkCountedDiscard slot quantity _) -> joinTwo (Map.singleton slot SlotArity.Many) (quantitySlots quantity)
    Discard.These {} -> Map.empty
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.ExchangeLifeTotals sides -> exchangeSidesSlots sides
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.RedistributeLifeTotals -> Map.empty
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> quantitySlots quantity
  Effect.DecreaseSpeed d -> quantitySlots (SpeedDecrease.quantity d)
  -- Create's slot is a DEFINITION, not a read, so the lint must not see it here.
  -- CR 111.2's creator is a READ -- Rampage of the Clans names the controller of
  -- the permanent the loop around it bound -- reported at the head with every
  -- other PlayerRef. So is CR 509.4's blocking rider, which names the attacker
  -- the token enters blocking (Flash Foliage's target), and that one is here.
  Effect.Create (Create.MkCreate quantity _ riders _ _) -> joinSlots [quantitySlots quantity, joinSlots (fmap quantitySlots (riderQuantities riders)), riderSlots riders]
  -- The COUNT only: the conjured card is literal card data, its destination is
  -- a constructor, and the conjurer is the resolving controller.
  Effect.Conjure (Conjure.MkConjure quantity _ _) -> quantitySlots quantity
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ riders) -> joinSlots [quantitySlots quantity, joinSlots (fmap quantitySlots (riderQuantities riders)), riderSlots riders]
  Effect.BecomeCopy {} -> Map.empty
  Effect.CopyStackObject {} -> Map.empty
  -- The Duration and Condition each carry Quantities; a Quantity.InSlot is a read.
  -- The ROW's own Filters and Quantities are READS too (replacementRowReads):
  -- Filter.IsBound in one names an object an earlier effect of this same
  -- resolution defined (Dire Fleet Daredevil's "that spell"), which is what the
  -- row's captured environment answers at CR 616.1.
  Effect.Replace (Replace.MkReplace duration _ _ condition re) ->
    joinSlots [durationSlots duration, joinSlots (fmap conditionSlots (Maybe.maybeToList condition)), replacementRowSlots re]
  Effect.SkipNextPhase {} -> Map.empty
  -- CR 615.5's rider reads slots of its own, so its reads join this effect's,
  -- LESS the reserved amount slot: the prevention binds that one itself
  -- (Event.eventBindingSlots), and Resolve.runPreventionRider is the writer.
  --
  -- The card-authored FILTERS are reads too, replacementRowSlots' answer for
  -- the same DamageR row one carrier over: the recipient description rides the
  -- installed row and is re-asked at each damage event, and CR 609.7a's
  -- chosen-source predicate is asked once as this effect resolves. CR 609.7b's
  -- printed source properties are not among them -- this opcode has no such
  -- field, and its installDamageRow call passes the trivial predicate. A
  -- Filter.IsBound in either names a slot of this resolution.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ _ whatRecipient _ chosenSource quantity rider) ->
    joinSlots
      [ durationSlots duration,
        quantitySlots quantity,
        joinSlots (fmap filterSlotsOf (Maybe.maybeToList whatRecipient <> Maybe.maybeToList chosenSource)),
        Map.delete Binding.eventAmount (joinSlots (fmap slotsOf (Foldable.toList rider)))
      ]
  -- The same reads, minus the shield size this opcode does not carry and plus CR
  -- 609.7b's printed source properties, the one field only this opcode spells
  -- out; they ride the row and are rechecked at the damage event (CR 615.9).
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ _ whatRecipient _ chosenSource whatSource rider) ->
    joinSlots
      [ durationSlots duration,
        joinSlots (fmap filterSlotsOf (Maybe.maybeToList whatRecipient <> Maybe.maybeToList chosenSource <> [whatSource])),
        Map.delete Binding.eventAmount (joinSlots (fmap slotsOf (Foldable.toList rider)))
      ]
  -- CR 614.9's redirection reads what PreventNextDamage reads, minus a rider it
  -- cannot carry: the recipient description rides the row, CR 609.7a's
  -- chosen-source predicate is asked once here, and the counted amount is
  -- evaluated once here too.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ amount _ whatRecipient _ _ chosenSource) ->
    joinSlots
      [ durationSlots duration,
        joinSlots (fmap quantitySlots (Maybe.maybeToList amount)),
        joinSlots (fmap filterSlotsOf (Maybe.maybeToList whatRecipient <> Maybe.maybeToList chosenSource))
      ]
  -- The bound slot is a DEFINITION, not a read: see boundSlots below.
  Effect.Counter {} -> Map.empty
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> quantitySlots quantity
  -- CR 122.8 reads its tally off ONE object, so `from` is read singly, where the
  -- destination is an ObjectRef and may sweep.
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom from _ _) -> oneSlot from
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity slot) -> insertOne slot (quantitySlots quantity)
  -- CR 122.5's pair: BOTH sides are ObjectRefs, joined at slotsOf's head with
  -- every other ref, so neither is read here. The count reads
  -- slots of its own -- Black Panther, Wakandan King's "all +1/+1 counters" is a
  -- Quantity.AgainstSlot aimed at the slot its `from` names -- so it joins in
  -- here rather than being left to look dangling. The bound slot is a
  -- DEFINITION, not a read: see boundSlots below.
  --
  -- Both sides report SlotArity.Many, every ObjectRef.InSlot being a whole-set
  -- read. A slot the COUNT names as an OBJECT still comes out One:
  -- joinTwo is Map.unionWith min and a Quantity.AgainstSlot reads its object
  -- singly, so Black Panther's `land` -- named by both halves -- keeps the arity
  -- that says "up to two target creatures" cannot fill it. A count reading that
  -- same name's AMOUNT would not narrow it, reading no object at all
  -- (quantitySlots above).
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> foldMap quantitySlots (MovedKinds.quantityOf kinds)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantitySlots quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> quantitySlots quantity
  -- The SlotName is a DEFINITION, not a read; it belongs to boundSlots below.
  Effect.PayAnyEnergy _ -> Map.empty
  Effect.Tap _ -> Map.empty
  Effect.Untap _ -> Map.empty
  Effect.Detain _ -> Map.empty
  Effect.Goad _ -> Map.empty
  Effect.MakePlotted _ -> Map.empty
  Effect.DoesNotUntapNext _ -> Map.empty
  Effect.Transform _ -> Map.empty
  Effect.Convert _ -> Map.empty
  -- The combined back face is literal card data and names no slot.
  Effect.Meld {} -> Map.empty
  Effect.PhaseOut _ -> Map.empty
  Effect.AddPhases _ -> Map.empty
  Effect.EndTurn -> Map.empty
  Effect.EndCombatPhase -> Map.empty
  Effect.GainControl {} -> Map.empty
  Effect.ArmDelayedTrigger {} -> Map.empty
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers _ affected _) -> affectedPlayersSlots affected
  Effect.RequireBlock {} -> Map.empty
  Effect.CantBeRegenerated {} -> Map.empty
  Effect.ForbidBlock {} -> Map.empty
  Effect.ForbidAttack {} -> Map.empty
  -- CR 508.1b's two sides are a PlayerRef and an ObjectRef, both reported at the
  -- head, so this arm has nothing of its own.
  Effect.RequireAttack {} -> Map.empty
  Effect.CreateEmblem {} -> Map.empty
  -- CR 725.1's crown names a target slot only in the InSlot arm.
  Effect.BecomeMonarch target -> monarchTargetSlots target
  -- CR 726.1 names no target slot at all: neither InitiativeTarget arm reads one.
  Effect.TakeTheInitiative _ -> Map.empty
  -- A READ: the slot names the permanent gaining the designation.
  Effect.Designate (Designate.MkDesignate _ slot) -> oneSlot slot
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ slot) -> oneSlot slot
  Effect.Unsuspect _ -> Map.empty
  -- A READ, Designate's: the slot names the permanent whose half is locked or
  -- unlocked. WHICH half is chosen at resolution and is no slot of any kind.
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked _ _ slot) -> oneSlot slot
  -- A READ, Designate's: the slot names where rule 702.100a's counter goes.
  Effect.Evolve slot -> oneSlot slot
  Effect.Mentor slot -> oneSlot slot
  Effect.Train slot -> oneSlot slot
  Effect.ItBecomes _ -> Map.empty
  Effect.ExileUntilMonarch slot -> oneSlot slot
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting card slot) -> joinSlots [oneSlot card, oneSlot slot]
  Effect.Attach slot -> oneSlot slot
  Effect.AttachTarget (AttachTarget.MkAttachTarget slot _) -> oneSlot slot
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot _) -> oneSlot slot
  -- Two READS: the binding the entrant sits in and the slot the card targeted.
  -- Both are read rather than bound, so both belong here; CardSpec's
  -- declared-equals-read lint subtracts the reserved `became` from this side.
  Effect.AttachBound (AttachBound.MkAttachBound subject destination) -> joinSlots [oneSlot subject, oneSlot destination]
  -- CR 729.1/729.1b: the slot is a DEFINITION (the subgame's winner), not a read.
  Effect.PlaySubgame _ -> Map.empty
  -- A DEFINITION too: chosen as this effect is applied (CR 608.2d), never read.
  Effect.ChoosePlayer _ -> Map.empty
  Effect.ChooseOpponentAtRandom _ -> Map.empty
  -- A DEFINITION for the result slot (boundSlots below), but CR 706.2's modifier
  -- is a READ: the instruction's own Quantity may name a slot an earlier effect
  -- of this same resolution bound, CR 608.2c following the list in written order.
  --
  -- A SHAPE CORRECTION, not a tested behaviour: every modifier in data/cards/ is
  -- a Count naming no slot (Diviner's Portent) and every count is a Literal
  -- (Valiant Endeavor), so leaving this Map.empty leaves the suite green. A card whose roll added "the number of cards you drew this
  -- way" would refute that. The same holds of the two arms below.
  Effect.RollDie rollDie -> quantitySlots (RollDie.count rollDie) <> maybe Map.empty quantitySlots (RollDie.modifier rollDie)
  -- And a DEFINITION too, on top of the slots the coin count reads.
  Effect.FlipCoin flipCoin -> quantitySlots (FlipCoin.count flipCoin)
  -- The slots the turn count reads (Ral Zarek's tally of heads).
  Effect.TakeExtraTurn takeExtraTurn -> quantitySlots (TakeExtraTurn.count takeExtraTurn)
  Effect.ShuffleIntoLibrary {} -> Map.empty
  -- The arm above's library read, reported at the head; nothing is shuffled into
  -- it, so there is no ref beside it either.
  Effect.Shuffle {} -> Map.empty
  -- The SLOT alone: the caster is a PlayerRef and is reported at the head. This
  -- one is a read, bound by an earlier effect of the list (CR 400.7).
  Effect.OfferCast (OfferCast.MkOfferCast slot _ _ _) -> oneSlot slot
  Effect.GrantPlayFromExile grant -> durationSlots (GrantPlayFromExile.duration grant)
  -- Everything the BODY reads. The loop's own slot is NOT subtracted as the
  -- rider's reserved slot is: boundSlots below defines it.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> joinSlots (fmap slotsOf (Foldable.toList body))

-- CR 611.2b: only ForAsLongAs carries a Quantity, through its Condition.
durationSlots :: Duration.Duration -> Map.Map SlotName SlotArity
durationSlots duration = case duration of
  Duration.UntilEndOfTurn -> Map.empty
  Duration.Indefinite -> Map.empty
  Duration.Perpetual -> Map.empty
  Duration.UntilYourNextTurn -> Map.empty
  Duration.UntilEndOfYourNextTurn -> Map.empty
  Duration.ForAsLongAs condition -> conditionSlots condition
  -- A Cost reads no slot: the activation cost of an ability is not walked by
  -- modeSlots either, and CR 116.2c's price is paid outside any resolution, so
  -- there is no binding environment for it to name.
  Duration.UntilPaid _ -> Map.empty
  Duration.UntilEndOfCombat -> Map.empty
  -- No slot either: Expiry.WhenUsed reads the effect's own Filter, which is
  -- walked wherever that effect's payload is (Pawl.Engine.PlayerEffect), not
  -- here.
  Duration.UntilUsed -> Map.empty

-- Both sides of a comparison are a Quantity, and either may read a slot.
conditionSlots :: Condition.Type.Condition -> Map.Map SlotName SlotArity
conditionSlots condition = case condition of
  Condition.Type.Compares c ->
    joinTwo (quantitySlots (Compares.measured c)) (quantitySlots (Compares.threshold c))
  Condition.Type.Any conditions -> joinSlots (fmap conditionSlots conditions)
  Condition.Type.All conditions -> joinSlots (fmap conditionSlots conditions)

-- Everything one waiting ROW can name a slot with: the Filters its pattern and its
-- rewrite describe things with, and the Quantities its rewrite counts with. A
-- Filter.IsBound in any of them, and a Quantity.InSlot in any of them, is a read
-- of the installing resolution's binding environment
-- (Pawl.Types.ActiveReplacement).
--
-- ONE declaration for THREE consumers, which is why the rewrite is not left to a
-- second function: replacementRowSlots below reports it as what the effect reads
-- (CR 603.3b, through slotsOf's Replace arm); installDamageRow and the
-- Effect.Replace resolution arm restrict what the installed row CAPTURES to it;
-- and referredToSources reads that captured map back out as CR 609.7a's "any
-- object referred to by ... a replacement or prevention effect that's waiting to
-- apply". A read missing here is one the row cannot answer at the event, with no
-- -Werror to catch it and no help from the card dataflow lint, whose read side is
-- this same function.
--
-- The two halves are returned TOGETHER rather than by two traversals, so that
-- ownSlotsAreExhaustive's Replace arm and the slot walk cannot come apart about
-- what an arm holds.
--
-- SYNTACTIC rather than per-reader: a slot NAME anywhere in the row's own data is
-- an object the row refers to, whether or not the arm reading it happens to build
-- a slot-aware Filter.Context today (#2141 names the caller that does not). That is
-- what CR 609.7a asks for, and it is the safe direction for the capture.
--
-- CR 614.9's printed DESTINATION is walked with the damage pattern beside it and
-- is not a pattern: it is read in the same
-- Pawl.Engine.Replacement.candidateContext the pattern is, so IsBound means the
-- same thing in both and a slot declared for one is declared for the other.
--
-- No wildcard: an arm added to Pawl.Types.ReplacementEffect must answer here, and
-- the ones carrying neither Filter nor Quantity say so rather than falling
-- through. Not implemented: the nested EFFECTS an EntryR rewrite or a DamageR
-- rider carries read slots of their own and are not walked (gap #1962).
replacementRowReads :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
replacementRowReads re = case re of
  -- The rewrite is a Zone and two Bools (Pawl.Types.ZoneChangeR): nothing that can
  -- name a slot, so the pattern is the whole of it.
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR pat _ _ _) -> ([ZoneChangePattern.whatObject pat], [])
  ReplacementEffect.EntryR (EntryR.MkEntryR pat rewrite) -> addFilter pat (entryRewriteReads rewrite)
  ReplacementEffect.DamageR (DamageR.MkDamageR pat rewrite _) ->
    ( DamagePattern.whatSource pat : (Maybe.maybeToList (DamagePattern.whatRecipient pat) <> damageRewriteFilters rewrite),
      []
    )
  ReplacementEffect.DestructionR _ -> ([], [])
  -- The rewrite is one Scaling, which is a constructor and a Natural.
  ReplacementEffect.CounterR (CounterR.MkCounterR pat _) -> ([CounterPattern.onWhat pat], [])
  -- The pattern's Filter over what the token is; the scaling is a number and
  -- the appended token is card data of its own, so neither reads a slot.
  ReplacementEffect.TokenR (TokenR.MkTokenR pat _ _) -> ([TokenPattern.whatToken pat], [])
  ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR pat _ rewrite) -> addFilter pat (turnUpRewriteReads rewrite)
  ReplacementEffect.UntapR _ -> ([], [])
  -- A LifeLossPattern is one ControllerRelation and one LifeLossCause, and no arm
  -- of LifeLossRewrite carries a Filter or a Quantity: no read at all.
  ReplacementEffect.LifeLossR {} -> ([], [])
  -- One ControllerRelation and one Scaling: no read at all, LifeLossR's answer.
  ReplacementEffect.LifeGainR {} -> ([], [])
  -- The pattern is one ControllerRelation; a Filter can only ride the REWRITE, and
  -- drawRewriteReads below is what reports it.
  ReplacementEffect.DrawR (DrawR.MkDrawR _ rewrite) -> drawRewriteReads rewrite
  -- A DrawCountR is one ControllerRelation, one threshold and one nullary rewrite.
  -- DrawR's answer, and for its reason.
  ReplacementEffect.DrawCountR {} -> ([], [])
  ReplacementEffect.PhaseR _ -> ([], [])

-- A row's pattern Filter joined onto what its rewrite reads.
addFilter :: Filter.Type.Filter Keyword.Type.Keyword -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity]) -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
addFilter filter_ (filters, quantities) = (filter_ : filters, quantities)

-- What an ENTRY rewrite reads, beside its row's pattern. Total over
-- Pawl.Types.EntryRewrite and no wildcard, replacementRowReads' discipline: an arm
-- gaining a Filter or a Quantity must answer here rather than have its reads go
-- undeclared. The arms answering nothing carry Naturals, constructors and literal
-- card data, none of which can name a slot.
--
-- Every arm here is a REGRESSION FENCE rather than a proven behaviour: no
-- Effect.Replace in data/cards/ carries an EntryR whose rewrite is anything but
-- Tapped or UnderSourceControl (Gather Specimens), and both answer nothing here,
-- so neutralizing any arm leaves the whole suite green. They are written
-- because the narrowing one caller over is only sound if this list is complete --
-- a rewrite read left out is a slot the installed row does not carry, and the
-- Filter or Quantity that wanted it then answers vacuously at the event.
entryRewriteReads :: EntryRewrite.EntryRewrite (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
entryRewriteReads rewrite = case rewrite of
  EntryRewrite.AsCopy asCopy -> ([AsCopy.eligible asCopy], [])
  EntryRewrite.ChoiceOf _ -> ([], [])
  EntryRewrite.ChoiceByCoinFlip _ -> ([], [])
  EntryRewrite.ChooseColor -> ([], [])
  EntryRewrite.ChooseBasicLandType -> ([], [])
  EntryRewrite.ChoosePlayer -> ([], [])
  EntryRewrite.ChooseCardNames restriction -> ([restriction], [])
  EntryRewrite.ChooseCardName restriction -> ([restriction], [])
  -- CR 614.1c's count per kind, evaluated as the row applies and against the ROW's
  -- Context (Pawl.Engine.Event's WithCounters arm), so a Quantity.InSlot in one
  -- reads the captured map. The KINDS beside them cannot name a slot.
  EntryRewrite.WithCounters counters -> ([], Map.elems (WithCounters.counters counters))
  EntryRewrite.UnderSourceControl -> ([], [])
  EntryRewrite.SacrificeAnyNumber sacrifice -> ([SacrificeAnyNumber.filter sacrifice], [])
  EntryRewrite.Riot -> ([], [])
  EntryRewrite.ReadAhead -> ([], [])
  EntryRewrite.Unleash -> ([], [])
  EntryRewrite.Bloodthirst _ -> ([], [])
  EntryRewrite.Compleated _ -> ([], [])
  EntryRewrite.Tapped -> ([], [])
  EntryRewrite.PayLifeOrTapped _ -> ([], [])
  EntryRewrite.RevealOrTapped filter_ -> ([filter_], [])
  EntryRewrite.EntersTransformed -> ([], [])
  -- Not implemented: the nested effects read slots of their own and neither this
  -- answer nor slotsOf reports them (gap #1962).
  EntryRewrite.RunEffects _ -> ([], [])

-- What a TURN-UP rewrite reads. entryRewriteReads' two shapes and its discipline:
-- CR 702.37b's count is evaluated against the row's Context, and CR 303.4k's host
-- description is a Filter.
turnUpRewriteReads :: TurnUpRewrite.TurnUpRewrite -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
turnUpRewriteReads rewrite = case rewrite of
  TurnUpRewrite.WithCounters counters -> ([], Map.elems (WithCounters.counters counters))
  TurnUpRewrite.MayAttachTo filter_ -> ([filter_], [])

-- What a DRAW rewrite reads. entryRewriteReads' discipline again, and the answer
-- is SYNTACTIC rather than per-reader: CR 400.11c keeps a spell or ability from
-- affecting a card outside the game, so no slot of any resolution can name one and
-- Event.eligible's own comment says Filter.IsBound answers nothing there -- but a
-- slot NAME in the row's own data is still an object the row refers to (CR
-- 609.7a), and declaring it is the safe direction for the capture.
drawRewriteReads :: DrawRewrite.DrawRewrite -> ([Filter.Type.Filter Keyword.Type.Keyword], [Quantity.Type.Quantity])
drawRewriteReads rewrite = case rewrite of
  DrawRewrite.GainLife _ -> ([], [])
  DrawRewrite.FromOutsideTheGame payload -> ([FromOutsideTheGame.filter payload], [])

-- replacementRowReads as a slot map. Arity One for every FILTER read, a
-- Filter.IsBound being a membership test rather than a target slot; the QUANTITY
-- half is quantitySlots' answer, so a Quantity.InSlot reports SlotArity.Amount --
-- CR 614.1c's WithCounters and CR 702.37b's are the two rewrites that can carry
-- one. The two capture sites take Map.keysSet, so the arity reaches only slotsOf's
-- Replace arm.
replacementRowSlots :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Map.Map SlotName SlotArity
replacementRowSlots re =
  let (filters, quantities) = replacementRowReads re
   in joinSlots (fmap filterSlotsOf filters <> fmap quantitySlots quantities)

-- The Filters a damage REWRITE holds, which is CR 614.9's printed destination and
-- nothing else. No wildcard, replacementRowSlots' discipline: a later rewrite
-- describing something must answer here rather than have its slot reads go
-- undeclared.
damageRewriteFilters :: DamageRewrite.DamageRewrite -> [Filter.Type.Filter Keyword.Type.Keyword]
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

-- One Filter's slot reads, at arity One -- the same shape modeSlots folds over a
-- mode's target-slot Filters.
filterSlotsOf :: Filter.Type.Filter Keyword.Type.Keyword -> Map.Map SlotName SlotArity
filterSlotsOf = Map.fromSet (const SlotArity.One) . Filter.boundSlots

-- CR 603.3b: is slotsOf's answer the WHOLE of what applying this effect reads off
-- the resolving object's bindings? A classification of effect SHAPE, never of
-- which effect it is; Engine.orderInert may elide CR 603.3b's ordering prompt
-- only for an ability that reads nothing.
--
-- Four ways this and slotsOf come apart, one per False or guard below:
-- ArmDelayedTrigger captures the whole environment (CR 603.7c); a Duration
-- slotsOf's arm drops can still name a slot (CR 611.2b); a PlayerRef nested in a
-- Quantity is Quantity.slotsAreExhaustive's half; CR 725.2's ControllerOfSource
-- reads the trigger-source slot, which is not a target.
--
-- The Quantities nested in this effect's ObjectRefs are taken from
-- effectObjectRefs here rather than arm by arm, so no arm of the case below
-- names a ref.
slotsAreExhaustive :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
slotsAreExhaustive effect = all (all Quantity.slotsAreExhaustive . objectRefQuantities) (effectObjectRefs effect) && ownSlotsAreExhaustive effect

-- slotsAreExhaustive's half that is not an ObjectRef's: this opcode's own
-- fields, and its nested effects through the recursion back into it.
--
-- No wildcard: a new opcode must answer here as well as in slotsOf. The `{}`
-- arms answer a constant, so a new FIELD on one is not forced.
ownSlotsAreExhaustive :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
ownSlotsAreExhaustive effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> all (Quantity.slotsAreExhaustive . DamagePart.quantity) parts
  Effect.Fight {} -> True
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) ->
    durationSlotsAreExhaustive duration
      && all Quantity.slotsAreExhaustive (Projection.quantitiesOf modification)
  Effect.ChangeText {} -> True
  Effect.AddMana _ -> True
  -- The COUNT's own nested reads. The filter's are exhaustive by construction:
  -- slotsOf reports them through Filter.boundSlots, the one walk that enumerates
  -- what a Filter reads, and no Filter atom carries a Quantity for
  -- Quantity.slotsAreExhaustive to be about.
  Effect.Search (Search.MkSearch _ _ _ quantity _ _ _) -> all Quantity.slotsAreExhaustive quantity
  Effect.ExileAllGraveyards -> True
  Effect.Proliferate -> True
  Effect.ChooseCardName _ -> True
  Effect.FromOutsideTheGame _ -> True
  Effect.ExileThisSpell -> True
  Effect.Bolster quantity -> Quantity.slotsAreExhaustive quantity
  Effect.Amass (Amass.Type.MkAmass quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.TemptWithTheRing -> True
  Effect.Venture {} -> True
  Effect.ExileHandThenDraw -> True
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RestartGame _ -> True
  Effect.ControlPlayerNextTurn _ -> True
  Effect.Destroy {} -> True
  Effect.Sacrifice _ -> True
  Effect.TurnFaceDown _ -> True
  Effect.TurnFaceUp _ -> True
  Effect.RemoveFromCombat _ -> True
  Effect.BecomesBlocked _ -> True
  -- The entry rider nests a Quantity of its own, CR 122.6's count per kind; the
  -- ref's is effectObjectRefs' above.
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> all Quantity.slotsAreExhaustive (riderQuantities riders)
  Effect.Draw (Draw.MkDraw _ quantity _) -> Quantity.slotsAreExhaustive quantity
  Effect.Mill (Mill.MkMill _ quantity _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.Reveal {} -> True
  Effect.LookAt {} -> True
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.Explore {} -> True
  Effect.Discard subject -> case subject of
    Discard.Counted (CountedDiscard.MkCountedDiscard _ quantity _) -> Quantity.slotsAreExhaustive quantity
    Discard.These {} -> True
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.ExchangeLifeTotals _ -> True
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RedistributeLifeTotals -> True
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.DecreaseSpeed d -> Quantity.slotsAreExhaustive (SpeedDecrease.quantity d)
  -- CR 111.1's token is minted with empty bindings, so its card is literal text.
  -- Its entry riders are not: CR 122.6's count per kind is the effect speaking,
  -- read in the resolution's own slots.
  Effect.Create (Create.MkCreate quantity _ riders _ _) -> all Quantity.slotsAreExhaustive (quantity : riderQuantities riders)
  -- The conjured card is literal text, Create's token's reason; the COUNT is the
  -- effect speaking, read in the resolution's own slots.
  Effect.Conjure (Conjure.MkConjure quantity _ _) -> Quantity.slotsAreExhaustive quantity
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ riders) -> all Quantity.slotsAreExhaustive (quantity : riderQuantities riders)
  Effect.BecomeCopy {} -> True
  Effect.CopyStackObject {} -> True
  -- The ReplacementEffect's own reads are replacementRowReads', and slotsOf
  -- reports them through replacementRowSlots: its Filters name no target slot, and
  -- the Quantities a counter rewrite counts with are asked here. Not implemented:
  -- the effects a rewrite or a CR 615.5 rider nests under this opcode read slots
  -- of their own and neither this answer nor slotsOf reports them; every
  -- Effect.Replace in data/cards/ nests none (gap #1962).
  Effect.Replace (Replace.MkReplace duration _ _ condition re) ->
    durationSlotsAreExhaustive duration
      && all conditionSlotsAreExhaustive condition
      && all Quantity.slotsAreExhaustive (snd (replacementRowReads re))
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> True
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ _ _ _ _ quantity rider) ->
    durationSlotsAreExhaustive duration && Quantity.slotsAreExhaustive quantity && all slotsAreExhaustive rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ _ _ _ _ _ rider) ->
    durationSlotsAreExhaustive duration && all slotsAreExhaustive rider
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ amount _ _ _ _ _) -> durationSlotsAreExhaustive duration && all Quantity.slotsAreExhaustive amount
  Effect.Counter {} -> True
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  -- No Quantity at all: CR 122.8 names neither a kind nor a count.
  Effect.PutCountersFrom {} -> True
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.slotsAreExhaustive quantity
  -- The count the moved kinds may write. CR 122.5's GIVER carries the other one,
  -- through the ObjectRef it became when the first side was widened to a group,
  -- and it is effectObjectRefs' above -- an arm reading the kinds alone kept
  -- compiling (#2729).
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> all Quantity.slotsAreExhaustive (MovedKinds.quantityOf kinds)
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.slotsAreExhaustive quantity
  Effect.PayAnyEnergy _ -> True
  Effect.Tap _ -> True
  Effect.Untap _ -> True
  Effect.Detain _ -> True
  Effect.Goad _ -> True
  Effect.MakePlotted _ -> True
  Effect.DoesNotUntapNext _ -> True
  Effect.Transform _ -> True
  Effect.Convert _ -> True
  -- The combined face is interned with EMPTY bindings, CreateEmblem's reason, so
  -- its text is literal.
  Effect.Meld _ -> True
  Effect.PhaseOut _ -> True
  Effect.AddPhases _ -> True
  Effect.EndTurn -> True
  Effect.EndCombatPhase -> True
  -- slotsOf's arm drops this Duration, so the slotless test is made here.
  Effect.GainControl (DurationRef.MkDurationRef duration _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CR 603.7c: the armed ability inherits this object's whole environment.
  Effect.ArmDelayedTrigger {} -> False
  -- GainControl's reason for the Duration.
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- slotsOf drops the Duration, so the slotless test is made here.
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- RequireBlock's reason, one axis narrower.
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CantBeRegenerated's reason again.
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock duration _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- RequireBlock's reason, one axis over.
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration _ _) ->
    Map.null (durationSlots duration) && durationSlotsAreExhaustive duration
  -- CR 114.2's emblem is minted with EMPTY bindings, so its card is literal text.
  Effect.CreateEmblem _ -> True
  Effect.BecomeMonarch MonarchTarget.TheController -> True
  -- The one arm answering NO: CR 725.2 reads Binding.triggerSource.
  Effect.BecomeMonarch MonarchTarget.ControllerOfSource -> False
  Effect.BecomeMonarch (MonarchTarget.InSlot _) -> True
  Effect.TakeTheInitiative InitiativeTarget.TheController -> True
  -- CR 726.2 reads Binding.triggerSource, Effect.BecomeMonarch ControllerOfSource's answer.
  Effect.TakeTheInitiative InitiativeTarget.ControllerOfSource -> False
  Effect.Designate (Designate.MkDesignate _ _) -> True
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> True
  Effect.Unsuspect _ -> True
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> True
  Effect.Evolve _ -> True
  Effect.Mentor _ -> True
  Effect.Train _ -> True
  Effect.ItBecomes _ -> True
  Effect.ExileUntilMonarch _ -> True
  Effect.ExileHaunting (ExileHaunting.MkExileHaunting _ _) -> True
  Effect.Attach _ -> True
  Effect.AttachTarget (AttachTarget.MkAttachTarget _ _) -> True
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget _ _) -> True
  Effect.AttachBound (AttachBound.MkAttachBound _ _) -> True
  -- CR 729.1b: a DEFINITION, and the subgame reads no binding of the outer game.
  Effect.PlaySubgame _ -> True
  -- PlaySubgame's answer: a definition reads no slot.
  Effect.ChoosePlayer _ -> True
  Effect.ChooseOpponentAtRandom _ -> True
  Effect.RollDie rollDie -> Quantity.slotsAreExhaustive (RollDie.count rollDie) && all Quantity.slotsAreExhaustive (RollDie.modifier rollDie)
  Effect.FlipCoin flipCoin -> Quantity.slotsAreExhaustive (FlipCoin.count flipCoin)
  Effect.TakeExtraTurn takeExtraTurn -> Quantity.slotsAreExhaustive (TakeExtraTurn.count takeExtraTurn)
  Effect.ShuffleIntoLibrary {} -> True
  Effect.Shuffle {} -> True
  Effect.OfferCast {} -> True
  Effect.GrantPlayFromExile grant -> durationSlotsAreExhaustive (GrantPlayFromExile.duration grant)
  -- PreventNextDamage's answer for the body, plus its own ref's: a PlayerRef
  -- nested in the DEPTH is one slotsOf cannot see.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> all slotsAreExhaustive body

-- CR 611.2b: only ForAsLongAs reads anything, through its Condition.
durationSlotsAreExhaustive :: Duration.Duration -> Bool
durationSlotsAreExhaustive duration = case duration of
  Duration.UntilEndOfTurn -> True
  Duration.Indefinite -> True
  Duration.Perpetual -> True
  Duration.UntilYourNextTurn -> True
  Duration.UntilEndOfYourNextTurn -> True
  Duration.ForAsLongAs condition -> conditionSlotsAreExhaustive condition
  -- durationSlots' answer: a Cost reads no slot, so its enumeration is complete.
  Duration.UntilPaid _ -> True
  Duration.UntilEndOfCombat -> True
  Duration.UntilUsed -> True

-- conditionSlots' mirror: both sides are a Quantity.
conditionSlotsAreExhaustive :: Condition.Type.Condition -> Bool
conditionSlotsAreExhaustive condition = case condition of
  Condition.Type.Compares c ->
    Quantity.slotsAreExhaustive (Compares.measured c) && Quantity.slotsAreExhaustive (Compares.threshold c)
  Condition.Type.Any conditions -> all conditionSlotsAreExhaustive conditions
  Condition.Type.All conditions -> all conditionSlotsAreExhaustive conditions

-- Does any of these effects read X? A card that reads X must declare it in its
-- cost (CR 107.3, CR 107.3a, CR 118.4), the same reads-equal-declares contract
-- slotsOf draws for target slots.
--
-- NOTE: when an opcode gains a Quantity FIELD, add its arm here by hand. A new
-- OPCODE the compiler forces, this case being exhaustive; widening an existing
-- one it does not, since an arm written `{} -> False` keeps compiling. That is
-- how a Quantity nested in an ObjectRef went unread once (#2729), so those are
-- taken from effectObjectRefs ahead of the case and no arm below names one.
readsX :: [Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> Bool
readsX = any effectReadsX
  where
    effectReadsX effect = any (any Quantity.readsX . objectRefQuantities) (effectObjectRefs effect) || effectOwnReadsX effect
    -- effectReadsX's half that is not an ObjectRef's: this opcode's own fields,
    -- and its nested effects through the recursion back into readsX.
    effectOwnReadsX effect = case effect of
      Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> any (Quantity.readsX . DamagePart.quantity) parts
      Effect.Fight {} -> False
      -- Untamed Might's "+X/+X" sits inside the Modification, not on the effect.
      Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ modification _) -> any Quantity.readsX (Projection.quantitiesOf modification)
      Effect.ChangeText {} -> False
      Effect.AddMana _ -> False
      Effect.Search (Search.MkSearch _ _ _ quantity _ _ _) -> any Quantity.readsX quantity
      Effect.ExileAllGraveyards -> False
      Effect.Proliferate -> False
      -- No Quantity: rule 201.4 chooses one name and states no count.
      Effect.ChooseCardName _ -> False
      Effect.FromOutsideTheGame _ -> False
      Effect.ExileThisSpell -> False
      Effect.Bolster quantity -> Quantity.readsX quantity
      Effect.Amass (Amass.Type.MkAmass quantity _) -> Quantity.readsX quantity
      Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.TemptWithTheRing -> False
      Effect.Venture {} -> False
      Effect.ExileHandThenDraw -> False
      Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> Quantity.readsX quantity
      Effect.RestartGame _ -> False
      Effect.ControlPlayerNextTurn _ -> False
      Effect.Destroy {} -> False
      Effect.Sacrifice _ -> False
      Effect.TurnFaceDown _ -> False
      Effect.TurnFaceUp _ -> False
      Effect.RemoveFromCombat _ -> False
      Effect.BecomesBlocked _ -> False
      -- The entry rider is a nested position of its own, CR 122.6's count per
      -- kind, and no ObjectRef holds it.
      Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> any Quantity.readsX (riderQuantities riders)
      Effect.Draw (Draw.MkDraw _ quantity _) -> Quantity.readsX quantity
      Effect.Mill (Mill.MkMill _ quantity _ _) -> Quantity.readsX quantity
      Effect.Reveal {} -> False
      Effect.LookAt {} -> False
      Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.Explore {} -> False
      Effect.Discard subject -> case subject of
        Discard.Counted (CountedDiscard.MkCountedDiscard _ quantity _) -> Quantity.readsX quantity
        Discard.These {} -> False
      Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.ExchangeLifeTotals _ -> False
      Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.RedistributeLifeTotals -> False
      Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> Quantity.readsX quantity
      Effect.DecreaseSpeed d -> Quantity.readsX (SpeedDecrease.quantity d)
      Effect.Create (Create.MkCreate quantity _ riders _ _) -> any Quantity.readsX (quantity : riderQuantities riders)
      Effect.Conjure (Conjure.MkConjure quantity _ _) -> Quantity.readsX quantity
      Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ riders) -> any Quantity.readsX (quantity : riderQuantities riders)
      Effect.BecomeCopy {} -> False
      Effect.CopyStackObject {} -> False
      Effect.Replace {} -> False
      Effect.SkipNextPhase {} -> False
      -- CR 601.2b's X reaches the rider too.
      Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ _ quantity rider) -> Quantity.readsX quantity || readsX (Foldable.toList rider)
      Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ _ _ _ rider) -> readsX (Foldable.toList rider)
      Effect.RedirectDamage (RedirectDamage.MkRedirectDamage _ _ amount _ _ _ _ _) -> any Quantity.readsX amount
      Effect.Counter {} -> False
      Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> Quantity.readsX quantity
      Effect.PutCountersFrom {} -> False
      Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> Quantity.readsX quantity
      Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> any Quantity.readsX (MovedKinds.quantityOf kinds)
      Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> Quantity.readsX quantity
      -- CR 107.14's amount is asked for as the spell resolves, never CR
      -- 601.2b's announced X.
      Effect.PayAnyEnergy _ -> False
      Effect.Tap _ -> False
      Effect.Untap _ -> False
      Effect.Detain _ -> False
      Effect.Goad _ -> False
      Effect.MakePlotted _ -> False
      Effect.DoesNotUntapNext _ -> False
      Effect.Transform _ -> False
      Effect.Convert _ -> False
      Effect.Meld _ -> False
      Effect.PhaseOut _ -> False
      Effect.AddPhases _ -> False
      Effect.EndTurn -> False
      Effect.EndCombatPhase -> False
      Effect.GainControl (DurationRef.MkDurationRef _ _) -> False
      Effect.ArmDelayedTrigger {} -> False
      Effect.AffectPlayers {} -> False
      Effect.RequireBlock {} -> False
      Effect.CantBeRegenerated {} -> False
      Effect.ForbidBlock {} -> False
      Effect.ForbidAttack {} -> False
      Effect.RequireAttack {} -> False
      Effect.CreateEmblem {} -> False
      Effect.BecomeMonarch {} -> False
      Effect.TakeTheInitiative {} -> False
      Effect.Designate (Designate.MkDesignate _ _) -> False
      Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> False
      Effect.Unsuspect _ -> False
      Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> False
      Effect.Evolve _ -> False
      Effect.Mentor _ -> False
      Effect.Train _ -> False
      Effect.ItBecomes _ -> False
      Effect.ExileUntilMonarch _ -> False
      Effect.ExileHaunting {} -> False
      Effect.Attach _ -> False
      Effect.AttachTarget {} -> False
      Effect.AttachTargetToEach {} -> False
      Effect.AttachBound {} -> False
      Effect.PlaySubgame _ -> False
      Effect.ChoosePlayer _ -> False
      Effect.ChooseOpponentAtRandom _ -> False
      -- CR 706.2's modifier and CR 706.1's count are ordinary Quantities, so
      -- either may be the X the caster announced (CR 601.2b; Neverwinter
      -- Hydra's "roll X dice").
      Effect.RollDie rollDie -> Quantity.readsX (RollDie.count rollDie) || any Quantity.readsX (RollDie.modifier rollDie)
      -- The number of coins is an ordinary Quantity, so it may be the X the
      -- caster announced (Flock of Rabid Sheep's "flip X coins").
      Effect.FlipCoin flipCoin -> Quantity.readsX (FlipCoin.count flipCoin)
      -- The number of turns is an ordinary Quantity too.
      Effect.TakeExtraTurn takeExtraTurn -> Quantity.readsX (TakeExtraTurn.count takeExtraTurn)
      Effect.ShuffleIntoLibrary {} -> False
      Effect.Shuffle {} -> False
      Effect.OfferCast {} -> False
      Effect.GrantPlayFromExile {} -> False
      -- CR 608.2f's body is an effect list like any other, so an X inside it counts.
      Effect.ForEach (ForEach.MkForEach _ _ body) -> readsX (Foldable.toList body)

-- slotsOf's mirror for ONE effect: the slots it BINDS rather than reads, which
-- is also the set Pawl.CardSpec's reserved-name sweep ranges over. Exhaustive
-- deliberately: a wildcard would file a new bind position under "binds nothing"
-- in both the dataflow lint and that sweep, with no diagnostic.
boundSlots :: Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Set SlotName
boundSlots effect = case effect of
  -- CR 400.7: the incarnation minted at the destination.
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ _ mSlot _ _ _) -> foldMap Set.singleton mSlot
  -- The tokens this Create minted, for CR 603.7c's delayed trigger to name.
  Effect.Create (Create.MkCreate _ _ _ mSlot _) -> foldMap Set.singleton mSlot
  -- Not implemented: Pawl.Types.Conjure carries no slot, so a printing that DOES
  -- name the conjured card later in its own instruction list (Kari Zev, Crew of
  -- Two's "if that card is on the battlefield, return it to its owner's hand")
  -- cannot be transcribed and this binds nothing (#2638).
  Effect.Conjure {} -> Set.empty
  Effect.CreateCopy {} -> Set.empty
  -- Binds nothing: no new object comes into existence.
  Effect.BecomeCopy {} -> Set.empty
  Effect.CopyStackObject {} -> Set.empty
  -- CR 729.1b: the subgame's winner, reported rather than chosen.
  Effect.PlaySubgame slot -> Set.singleton slot
  -- CR 608.2d: the player this effect chose.
  Effect.ChoosePlayer choice -> Set.singleton (ChoosePlayer.slot choice)
  Effect.ChooseOpponentAtRandom slot -> Set.singleton slot
  -- CR 706.4: the result the roller used, and, where the card reads it, the
  -- other result of the same instruction, for a later effect of this resolution
  -- to read as Quantity.InSlot.
  Effect.RollDie rollDie -> Set.singleton (RollDie.slot rollDie) <> foldMap Set.singleton (RollDie.other rollDie)
  -- CR 705.2: how many of the instruction's flips the flipping player won (or
  -- how many coins came up heads), and, where the card reads it, how many they
  -- lost, for a later effect of this resolution to read as Quantity.InSlot.
  Effect.FlipCoin flipCoin -> Set.singleton (FlipCoin.slot flipCoin) <> foldMap Set.singleton (FlipCoin.misses flipCoin)
  -- Three slots CR 701.8's destruction may define: how many permanents it
  -- ACTUALLY destroyed, for a later "for each ... destroyed this way"; the cards
  -- it put into a graveyard, for a later clause that NAMES them (CR 400.7's
  -- incarnations); and the PERMANENTS it destroyed, for a later clause that
  -- walks them one at a time.
  Effect.Destroy (Destroy.MkDestroy _ _ mSlot mBuried mPermanents) -> foldMap Set.singleton mSlot <> foldMap Set.singleton mBuried <> foldMap Set.singleton mPermanents
  -- How many milled cards matched the tally's filter (CR 728.1), and WHICH cards
  -- the mill put in the graveyard, for a later clause that names them (CR
  -- 701.17c). Two slots and not one: a card may write either without the other.
  Effect.Mill (Mill.MkMill _ _ mTally mSlot) -> foldMap (Set.singleton . MillTally.slot) mTally <> foldMap Set.singleton mSlot
  -- The cards CR 701.20a's reveal showed, where the card named a slot. Optional,
  -- where LookAt's is not: the GameEvent.Revealed in the log is a record already.
  Effect.Reveal (Reveal.MkReveal _ mSlot) -> foldMap Set.singleton mSlot
  -- The cards CR 701.20e's look showed, for a later clause to name.
  Effect.LookAt (LookAt.MkLookAt _ slot) -> Set.singleton slot
  Effect.Scry {} -> Set.empty
  Effect.Surveil {} -> Set.empty
  Effect.Fateseal {} -> Set.empty
  Effect.Explore {} -> Set.empty
  Effect.DealDamage {} -> Set.empty
  Effect.Fight {} -> Set.empty
  Effect.ModifyTarget {} -> Set.empty
  Effect.ChangeText {} -> Set.empty
  Effect.AddMana _ -> Set.empty
  Effect.Search {} -> Set.empty
  Effect.ExileAllGraveyards -> Set.empty
  Effect.Proliferate -> Set.empty
  -- Binds nothing: the name goes on the SOURCE (Object.chosenNames) and is read
  -- back off it by Filter.HasChosenName, so no slot carries it.
  Effect.ChooseCardName _ -> Set.empty
  Effect.FromOutsideTheGame _ -> Set.empty
  Effect.ExileThisSpell -> Set.empty
  Effect.Bolster _ -> Set.empty
  Effect.Amass _ -> Set.empty
  Effect.Blight _ -> Set.empty
  Effect.TemptWithTheRing -> Set.empty
  Effect.Venture {} -> Set.empty
  Effect.ExileHandThenDraw -> Set.empty
  Effect.PlayerSacrifices {} -> Set.empty
  Effect.RestartGame _ -> Set.empty
  Effect.ControlPlayerNextTurn _ -> Set.empty
  Effect.Sacrifice _ -> Set.empty
  Effect.TurnFaceDown _ -> Set.empty
  Effect.TurnFaceUp _ -> Set.empty
  Effect.RemoveFromCombat _ -> Set.empty
  Effect.BecomesBlocked _ -> Set.empty
  -- CR 121.1's cards "drawn this way", as CR 400.7's incarnations in the hand
  -- they arrived in.
  Effect.Draw (Draw.MkDraw _ _ mSlot) -> foldMap Set.singleton mSlot
  -- CR 701.9a's cards "discarded this way", as CR 400.7's incarnations. The
  -- These arm has none, for the reason its type carries.
  --
  -- A REGRESSION FENCE rather than proven behaviour: emptying this arm leaves
  -- the suite green. The only consumer is Pawl.CardSpec's D4 dataflow lint, and
  -- the read it would have to notice is a Filter.IsBound inside a Count inside a
  -- Clause.condition -- which modeSlots does not fold at all, and which
  -- Count.slots would not descend into if it did (#1079).
  Effect.Discard subject -> case subject of
    Discard.Counted (CountedDiscard.MkCountedDiscard _ _ mDiscarded) -> foldMap Set.singleton mDiscarded
    Discard.These _ -> Set.empty
  Effect.LoseLife {} -> Set.empty
  Effect.GainLife {} -> Set.empty
  Effect.ExchangeLifeTotals _ -> Set.empty
  Effect.SetLifeTotal {} -> Set.empty
  Effect.RedistributeLifeTotals -> Set.empty
  Effect.IncreaseSpeed {} -> Set.empty
  Effect.DecreaseSpeed {} -> Set.empty
  Effect.Replace {} -> Set.empty
  Effect.SkipNextPhase {} -> Set.empty
  -- The shield itself binds nothing; CR 615.5's rider is an effect list, so a
  -- name IT authors is a name this card authors. Both shields.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ _ _ _ _ _ rider) -> foldMap boundSlots rider
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ _ _ _ rider) -> foldMap boundSlots rider
  Effect.RedirectDamage {} -> Set.empty
  -- How many spells this countering ACTUALLY countered, for a "for each spell
  -- countered this way", and the permanents whose abilities were (CR 113.7).
  Effect.Counter (Counter.MkCounter _ mSlot mSources) -> foldMap Set.singleton mSlot <> foldMap Set.singleton mSources
  Effect.PutCounters {} -> Set.empty
  Effect.PutCountersFrom {} -> Set.empty
  Effect.RemoveCounters {} -> Set.empty
  -- How many counters CR 122.5 ACTUALLY moved, for a "that much life".
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ _ mSlot _) -> foldMap Set.singleton mSlot
  Effect.GainPlayerCounters {} -> Set.empty
  Effect.RemovePlayerCounters {} -> Set.empty
  -- CR 107.14: how much {E} the payer paid, for a later effect of the same
  -- resolution to read as Quantity.InSlot.
  Effect.PayAnyEnergy slot -> Set.singleton slot
  Effect.Tap _ -> Set.empty
  Effect.Untap _ -> Set.empty
  Effect.Detain _ -> Set.empty
  Effect.Goad _ -> Set.empty
  Effect.MakePlotted _ -> Set.empty
  Effect.DoesNotUntapNext _ -> Set.empty
  Effect.Transform _ -> Set.empty
  Effect.Convert _ -> Set.empty
  -- CR 701.42a's melded permanent is bound to nothing: no printing names it later
  -- in its own instruction list.
  Effect.Meld _ -> Set.empty
  Effect.PhaseOut _ -> Set.empty
  Effect.AddPhases _ -> Set.empty
  Effect.EndTurn -> Set.empty
  Effect.EndCombatPhase -> Set.empty
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> Set.empty
  Effect.ArmDelayedTrigger {} -> Set.empty
  Effect.AffectPlayers {} -> Set.empty
  Effect.RequireBlock {} -> Set.empty
  Effect.CantBeRegenerated {} -> Set.empty
  Effect.ForbidBlock {} -> Set.empty
  Effect.ForbidAttack {} -> Set.empty
  Effect.RequireAttack {} -> Set.empty
  Effect.CreateEmblem {} -> Set.empty
  Effect.BecomeMonarch {} -> Set.empty
  Effect.TakeTheInitiative {} -> Set.empty
  Effect.Designate (Designate.MkDesignate _ _) -> Set.empty
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> Set.empty
  Effect.Unsuspect _ -> Set.empty
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> Set.empty
  Effect.Evolve _ -> Set.empty
  Effect.Mentor _ -> Set.empty
  Effect.Train _ -> Set.empty
  Effect.ItBecomes _ -> Set.empty
  Effect.ExileUntilMonarch _ -> Set.empty
  Effect.ExileHaunting {} -> Set.empty
  Effect.Attach _ -> Set.empty
  Effect.AttachTarget {} -> Set.empty
  Effect.AttachTargetToEach {} -> Set.empty
  Effect.AttachBound {} -> Set.empty
  Effect.TakeExtraTurn {} -> Set.empty
  Effect.ShuffleIntoLibrary {} -> Set.empty
  Effect.Shuffle {} -> Set.empty
  Effect.OfferCast {} -> Set.empty
  Effect.GrantPlayFromExile {} -> Set.empty
  -- The loop's member slot, plus every name the BODY authors.
  Effect.ForEach (ForEach.MkForEach _ slot body) -> Set.insert slot (foldMap boundSlots body)

-- CR 608.2b: the ONE recipient still legal in `slot`, for a reader that can take
-- only one -- nothing when the slot named none, its target became illegal, or it
-- names SEVERAL. Pawl.CardSpec's plural-slot lint keeps a card from aiming one of
-- those at such a reader.
legalOne :: SlotName -> Map.Map SlotName (Set Recipient) -> Maybe Recipient
legalOne slot legal = Binding.onlyOne (Map.findWithDefault Set.empty slot legal)

-- The same read for a reader that takes them ALL, CR 608.2b's illegal ones
-- already dropped.
legalMany :: SlotName -> Map.Map SlotName (Set Recipient) -> [Recipient]
legalMany slot legal = Set.toList (Map.findWithDefault Set.empty slot legal)

-- The players a PlayerRef names DURING a resolution, read from the slots this
-- resolution filled rather than the source's bindings. A slot naming SEVERAL
-- names nobody (`legalOne`).
--
-- CR 102.1: a departed player keeps their row in GameState.players, so `everyone`
-- is Game.stillPlaying rather than the map's keys; whether a departed player can
-- be named from elsewhere is CR 800.4d/800.4i's question (#181). In PlayerId
-- order, a PlayerRef naming an unordered SET, so a caller with an ordering rule
-- imposes it.
playerRefPlayers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> PlayerRef -> [PlayerId]
playerRefPlayers legal controller gs ref = case ref of
  PlayerRef.InSlot slot -> case legalOne slot legal of
    Just (Recipient.ToPlayer pid) -> [pid]
    _ -> [] -- an unfilled, illegal, or non-player slot: no-op
    -- Every player the slot names, InSlot's read without Binding.onlyOne's
    -- collapse -- Binding.mayPlayers, the seats a CR 603.5 "may" selected.
    -- Non-player recipients are dropped, as the arm above drops them.
    --
    -- Binding.gatePlayers is the same shape one question over, and Bellowing
    -- Mauler's "each player loses 4 life unless they sacrifice a nontoken
    -- creature of their choice" reads THAT slot plurally through the arm below.
  PlayerRef.EachInSlot slot -> Maybe.mapMaybe Recipient.playerOf (legalMany slot legal)
  PlayerRef.Relative PlayerRelation.You -> [controller]
  PlayerRef.Relative PlayerRelation.Opponent -> filter (PlayerRelation.holds (Game.teams gs) PlayerRelation.Opponent controller) everyone
  -- CR 102.1's whole table, off the roster rather than by consing the controller
  -- onto the Opponent set, so a departed seat stays out.
  PlayerRef.Relative PlayerRelation.AnyPlayer -> everyone
  PlayerRef.EachPlayer -> everyone
  -- EachPlayer minus the seat the slot names. A slot that is unfilled, illegal,
  -- names several, or names an object excludes NOBODY.
  PlayerRef.EachPlayerExcept slot ->
    let excluded = legalOne slot legal >>= Recipient.playerOf
     in filter (\pid -> Just pid /= excluded) everyone
  -- The baked seat, unreachable from card data. Not filtered against the roster:
  -- it names one specific player who arrived from elsewhere.
  PlayerRef.Specific pid -> [pid]
  -- NOBODY, and not a hole: the reference names whichever player a fold has
  -- reached, and this function is handed no fold. The two positions that DO
  -- answer it never route through here -- Pawl.Engine.Quantity's playersOf reads
  -- it off the view a Count's fold supplies, and the Effect.Search arm's
  -- ownersFor substitutes the searcher for a search whose owner is its own
  -- searcher -- so what reaches this arm is a reference in a position with no
  -- candidate at all, and the opcode is a no-op.
  PlayerRef.Candidate -> []
  -- CR 608.2h: the controller of the object the slot names, through last known
  -- information -- the clause naming the player generally MOVED it first, and CR
  -- 108.4 leaves a card in a hand with no controller at all.
  PlayerRef.ControllerOfBound slot -> case legalOne slot legal of
    Just recipient -> case Recipient.objectOf recipient of
      Just oid -> Maybe.maybeToList (Projection.controllerWithLastKnown oid gs)
      Nothing -> []
    Nothing -> []
  -- CR 508.6: the players controlling a creature that is attacking the player the
  -- slot names, narrowed by the relation the card printed -- Curse of Vitality's
  -- "each opponent attacking that player".
  --
  -- The LIVE combat record, read as this effect applies (CR 608.2c): the sentence
  -- is present tense, so a creature removed from combat (CR 506.4) since the
  -- declaration has taken its controller out of the set. Not the event log, which
  -- is Pawl.Engine.Turn.attackedThisStep's historical reading of the same rule.
  --
  -- AttackTarget.OfPlayer alone, CR 508.1b listing player, planeswalker and
  -- battle separately: a creature attacking a planeswalker that player controls
  -- is not attacking that player.
  --
  -- Filtered out of `everyone` rather than collected from the record, so the
  -- roster order and the CR 102.1 exclusion of a departed seat are the ones every
  -- other arm gives.
  PlayerRef.Attacking (AttackingPlayers.MkAttackingPlayers relation slot) ->
    case legalOne slot legal >>= Recipient.playerOf of
      Nothing -> []
      Just attacked ->
        let sentAt = Map.keys (Map.filter (== AttackTarget.OfPlayer attacked) (Combat.attackers (GameState.combat gs)))
            attackers = Maybe.mapMaybe (\oid -> Projection.controllerOf oid gs) sentAt
         in filter (\pid -> PlayerRelation.holds (Game.teams gs) relation controller pid && pid `elem` attackers) everyone
  where
    everyone = Game.stillPlaying gs

-- CR 109.2's battlefield, narrowed by an effect-borne Filter and sorted into CR
-- 608.2f's APNAP order. ObjectRef.EachMatching's whole answer, and the
-- CANDIDATES ObjectRef.AnyNumberMatching offers -- shared so a card cannot find
-- the sweep and the offer disagreeing about what matches.
--
-- CR 303.4b's host is supplied here and nowhere else in this module: this sweep
-- is the one effect-borne Filter position naming what the SOURCE enchants. Read
-- live, so an Aura moved between the trigger and its resolution acts on the host
-- it has now.
--
-- Through effectContext, so the resolution's own slot bindings ride along and a
-- sweep can exclude what another slot already named: Showstopping Surprise's
-- "each OTHER creature" is `Not (IsBound "target")`. Filter.IsBound answers False
-- for every candidate against an empty slot map, so a bare contextFor here would
-- leave such a card silently sweeping in its own target. It is also what answers
-- a CONTROLLER-relative conjunct -- Tovolar's "Human Werewolves you control" is
-- `ControlledBy You`, read against CR 109.5's perspective.
battlefieldMatching :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
battlefieldMatching legal resolving controller source gs filter_ =
  let context = (effectContext gs controller source legal (slotBindings resolving gs)) {Filter.sourceAttachedTo = Projection.hostOf source gs}
      viewOf = Projection.viewsOf gs
      matching =
        filter
          (\oid -> Filter.matches context (viewOf oid) filter_)
          (Set.toList (GameState.battlefield gs))
      order = Game.apnapOrder gs
      last_ = length order
      seat oid = case Projection.controllerOf oid gs of
        Nothing -> last_
        Just pid -> Maybe.fromMaybe last_ (List.elemIndex pid order)
   in List.sortOn (\oid -> (seat oid, oid)) matching

-- The objects an ObjectRef names DURING a resolution, for every arm whose answer
-- is a READ. The arms that are a CR 608.2d QUESTION answer [] here and are
-- carried out by the opcode arms that reach the Game monad. InSlot takes every
-- recipient CR 608.2b left legal
-- (CR 601.2c); a slot bound to a GROUP is answered before that question, a group
-- being a definition rather than a target (CR 115.10a).
--
-- EachMatching folds the battlefield (CR 109.2) against the projection, so a
-- permanent that is a creature only by a layer-4 effect is in the set -- through
-- battlefieldMatching above, which is that fold and is shared with the
-- AnyNumberMatching offer. The filter context is this effect's own -- CR 109.5's
-- "you" is the ability's controller -- because the filter IS the ability's card
-- text. EachCardInGraveyard is the same
-- fold over CR 400.1's per-player graveyards (CR 109.2a).
--
-- WHEN: at the moment the caller runs (CR 608.2c), and the list is then FIXED --
-- one half of CR 608.2f's simultaneous processing. The other half is the
-- caller's: it hands the whole list to its funnel as one batch rather than
-- calling it per element (Event.destroy's haddock).
--
-- The two callers that store a CONTINUOUS effect -- ModifyTarget and GainControl
-- -- owe CR 611.2c: the set is determined when the effect begins, so those arms
-- freeze this answer as Affected.TheseObjects. Nothing enforces it; a third
-- storing caller must not reach for Affected.Matching, a STATIC ability's set.
--
-- ORDER, for the arms folding over CR 400.1's per-player zones: APNAP (CR
-- 608.2f), then ascending ObjectId. The arms over a SHARED zone keep that zone's
-- own order (CR 101.4). `forEachOrder` is where CR 608.2f's secondary sentence
-- and CR 701.44d's per-seat choice open, and both ask rather than reading this
-- order.
objectRefObjects :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [ObjectId]
objectRefObjects legal resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- The slot names every object bound there as a group, all at once. Ahead of
    -- the target read and not subject to `legal`: a group binding is a definition,
    -- never a target (CR 115.10a). slotGroup says why being ahead is safe.
    Just group -> Foldable.toList group
    Nothing -> Maybe.mapMaybe Recipient.objectOf (legalMany slot legal)
  ObjectRef.EachMatching filter_ -> battlefieldMatching legal resolving controller source gs filter_
  -- A CR 608.2d question, so this pure sweep answers nothing for it: the
  -- candidates are battlefieldMatching's, but WHICH of them the instruction names
  -- is the chooser's, and two gathers reach the Game monad to ask --
  -- turnPermanentsOver, the body Effect.Transform and Effect.Convert share, and
  -- the Effect.MoveToZone gather. Under any other opcode this empty answer is an
  -- inert card-data error, which Pawl.CardSpec's inertChoosers rejects at load
  -- time -- ChosenCardInGraveyard's note below is the shape.
  ObjectRef.AnyNumberMatching _ -> []
  -- The arm above's answer, for its reason: a CR 608.2d question, so this pure
  -- sweep answers nothing for it. The Effect.MoveToZone gather is the one arm
  -- that reaches the Game monad to ask it; under any other opcode this empty
  -- answer is an inert card-data error, which Pawl.CardSpec's inertChoosers
  -- rejects at load time.
  ObjectRef.ChosenPermanent _ -> []
  -- The arm above's answer, for its reason: a CR 608.2d question, so this pure
  -- sweep answers nothing for it -- the source half included, which no reader may
  -- take without the counterpart the one instruction names alongside it.
  ObjectRef.SourceAndChosenPermanent _ -> []
  -- EachMatching's sweep with CR 109.2's battlefield default switched off by the
  -- card's own words (CR 109.2a), over CR 400.1's per-player zone. Whose
  -- graveyards is zoneScopePlayers below -- either the perspective's own
  -- reading of CR 109.5 or the players another slot of this announcement targets
  -- -- and what matches within each is graveyardCardsOf.
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard scope filter_) ->
    concatMap (\pid -> graveyardCardsOf (effectContext gs controller source legal (slotBindings resolving gs)) gs pid filter_) (zoneScopePlayers legal controller gs scope)
  -- CR 400.1's per-player zone again, but only the RESOLVING CONTROLLER's, so no
  -- scope to fold over and no APNAP order to impose. In the zone's own order,
  -- which no rule reads: CR 402.3 leaves a hand's arrangement to its owner.
  ObjectRef.EachCardInYourHand -> Game.zoneMembers Zone.Hand controller gs
  -- The arm above's zone under EachCardInGraveyard's scope and filter: CR
  -- 109.2a's reading again, over the hands zoneScopePlayers names rather
  -- than the resolving controller's alone. In APNAP order (CR 608.2f) across
  -- seats, and within a seat in the hand's own order, which no rule reads (CR
  -- 402.3) -- the arm above's answer.
  --
  -- effectContext and NOT Filter.contextFor, so the resolution's own slot
  -- bindings ride along and a filter reading a slot (Filter.IsBound) is not
  -- vacuously False here. Amnesia's "nonland" reads no slot, so no test on this
  -- module can tell the two apart at this site (#2075); the sibling sweeps are
  -- written the same way for the reason spelled out at EachMatching above.
  --
  -- No sourceAttachedTo override, unlike EachMatching: no card in a hand names
  -- what its Aura's host is.
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand scope mFilter) ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        held pid = case mFilter of
          Nothing -> Game.zoneMembers Zone.Hand pid gs
          Just filter_ -> handCardsOf context gs pid filter_
     in concatMap held (zoneScopePlayers legal controller gs scope)
  -- CR 400.1's other hidden per-player zone, and only the RESOLVING
  -- CONTROLLER's, so no scope to fold over and no APNAP order to impose --
  -- EachCardInYourHand's answer above. CR 400.12 is what makes "from your
  -- library" name every card in it, and CR 109.2a is what a stated Filter reads
  -- -- Caldera Breaker's "all Mountain cards". In the library's own
  -- order, top card first, which CR 401.2 keeps players from looking at or
  -- changing; the narrowing does not reorder, and the cards that did not match
  -- stay where they were.
  --
  -- NOT a search (CR 701.23a) and so no shuffle (CR 701.24): neither producer's
  -- text says "search" or "find", so CR 701.23b's "isn't required to find" and
  -- CR 701.23f's search triggers have nothing to reach. A stated characteristic
  -- does not make it one -- rule 701.23b governs a player who is SEARCHING, and
  -- nothing here asks anyone to.
  ObjectRef.EachCardInYourLibrary mFilter ->
    let inLibrary = Game.zoneMembers Zone.Library controller gs
     in case mFilter of
          Nothing -> inLibrary
          Just filter_ ->
            let context = effectContext gs controller source legal (slotBindings resolving gs)
                viewOf = Projection.viewsOf gs
             in filter (\oid -> Filter.matches context (viewOf oid) filter_) inLibrary
  -- CR 607.2a's linked set: the cards GameState.exiledWith files against this
  -- effect's SOURCE. The relation, not a zone sweep, is the membership test, so a
  -- card exiled by a second copy of the same printing is not named; a stated
  -- Filter then narrows it. `source` and not `resolving`, since rule 607.2a links
  -- two abilities of one OBJECT and for a dies trigger the two ids differ. Read
  -- off GameState.exile directly because CR 400.1 makes exile one SHARED zone --
  -- no player to ask, and no APNAP sort, so ascending id and thus arrival order.
  --
  -- Rule 607.2a's wording ALONE: an ability referring back to what its own
  -- earlier instruction exiled is CR 400.7j in CR 608.2c's written order, and
  -- names the slot that instruction bound instead -- Hanweir Battlements' "exile
  -- them, then meld them into Hanweir, the Writhing Township" is that printing,
  -- and its Meld reads an InSlot.
  ObjectRef.EachCardExiledWithSource mFilter ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        viewOf = Projection.viewsOf gs
        stated oid = case mFilter of
          Nothing -> True
          Just filter_ -> Filter.matches context (viewOf oid) filter_
     in filter
          (\oid -> Map.lookup oid (GameState.exiledWith gs) == Just source && stated oid)
          (Set.toList (GameState.exile gs))
  -- CR 109.2b's reading of a description carrying the word "spell" -- the stack,
  -- not the battlefield. Game.isSpell keeps the abilities sharing the zone out
  -- (CR 112.1), a classification of the object's kind and not of its identity. In
  -- the STACK's own order, top first (CR 405.2), not APNAP: one shared zone has an
  -- order the rules already read. Read LIVE (CR 608.2c).
  ObjectRef.EachSpell filter_ ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        viewOf = Projection.viewsOf gs
     in filter
          (\oid -> Game.isSpell oid gs && Filter.matches context (viewOf oid) filter_)
          (GameState.stack gs)
  -- CR 405.1's whole zone: the arm above without Game.isSpell, since a sentence
  -- naming spells AND abilities names everything the stack holds. Same order,
  -- top first (CR 405.2), and read LIVE (CR 608.2c).
  ObjectRef.EachOnStack filter_ ->
    let context = effectContext gs controller source legal (slotBindings resolving gs)
        viewOf = Projection.viewsOf gs
     in filter
          (\oid -> Filter.matches context (viewOf oid) filter_)
          (GameState.stack gs)
  -- Names players and so no objects at all.
  ObjectRef.EachPlayer -> []
  ObjectRef.EachOpponent -> []
  ObjectRef.ChosenPlayer -> []
  -- CR 401.2's ordered pile, whose head is the top (CR 121.1). The depth is taken
  -- from EACH named library, top first, and a shorter library gives what it has
  -- (CR 609.3). Restricted to the players still in the turn order and delivered in
  -- it (CR 608.2f, CR 101.4), which also drops a player CR 800.4 removed.
  --
  -- The depth is a Quantity evaluated HERE (CR 608.2c), off the announcement CR
  -- 601.2b left on the resolving object -- which is why `resolving` rather than
  -- `source` is the announcedOn id -- and ONCE for the whole ref, no printing
  -- writing a per-library depth. A depth that will not evaluate, or evaluates
  -- negative, is ZERO cards (CR 107.1b): a REGRESSION FENCE, since making the
  -- fallback 5 leaves the suite green.
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player count) ->
    let named = playerRefPlayers legal controller gs player
        viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        depth = maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source count)
     in concatMap
          (\pid -> List.genericTake depth (Game.zoneMembers Zone.Library pid gs))
          (filter (`elem` named) (Game.apnapOrder gs))
  -- The arm above's walk with its Quantity counting MATCHES rather than cards:
  -- the same prefix of CR 401.2's ordered pile taken from its head (CR 121.1),
  -- ended by the card whose match brings the tally up to the count instead of by
  -- a counted depth. That card is IN the prefix, which is what Treasure Hunt's
  -- "until you reveal a nonland card" and Open the Way's "until you reveal X land
  -- cards" both say -- the walk stops having reached it, not before it.
  --
  -- A library holding fewer matches than the count is given up whole (CR 609.3),
  -- which `walkDown` does by running out of cards; the rest of the instruction is
  -- then performed on all of it (CR 101.3). An empty library names nothing, for
  -- the same reason, and so does a count of zero -- an unevaluable or negative
  -- one being clamped to zero here (CR 107.1b), the arm above's clamp.
  --
  -- Per named library and in APNAP order, the arm above's fold, so "each
  -- player's" walks each pile separately rather than one across the table. The
  -- Filter is matched against each card's own projection as the walk reaches it
  -- (CR 608.2c) -- the context is this resolution's, so a filter reading a slot
  -- this resolution bound sees it.
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil player filter_ count) ->
    let named = playerRefPlayers legal controller gs player
        viewOf = effectViewOf source legal gs
        context = effectContext gs controller source legal (slotBindings resolving gs)
        wanted = maybe 0 Integer.toNaturalSaturating (Quantity.evaluateFor viewOf context gs resolving source count)
        viewOfCard = Projection.viewsOf gs
        matches oid = Filter.matches context (viewOfCard oid) filter_
        walkDown remaining oids =
          if remaining <= (0 :: Natural)
            then []
            else case oids of
              [] -> []
              oid : rest -> oid : walkDown (if matches oid then remaining - 1 else remaining) rest
        walk pid = walkDown wanted (Game.zoneMembers Zone.Library pid gs)
     in concatMap walk (filter (`elem` named) (Game.apnapOrder gs))
  -- A card somebody CHOOSES is a QUESTION, and this function cannot ask one; the
  -- MoveToZone arm's own gather does. Under any other opcode this empty answer is
  -- an inert card-data error.
  ObjectRef.ChosenCardInGraveyard {} -> []
  ObjectRef.ChosenCardInHand {} -> []
  ObjectRef.ChosenCardFromAmong {} -> []
  -- The arm above's plural, and answered HERE rather than deferred, which is the
  -- whole difference between them: "all land cards revealed this way" asks
  -- nobody anything, so every opcode reading this function gets the set. The
  -- members are InSlot's own read of the slot -- the arm at the head of this
  -- case, so the sentence naming the matches and a later one naming the rest
  -- cannot see different groups -- narrowed by the ref's Filter against each
  -- member's CR 613 projection, matched when the effect executes (CR 608.2c) in
  -- this effect's own context (CR 109.5). The group's mint order survives, which
  -- CR 608.2f leaves standing.
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong slot filter_) ->
    matchingFromAmong legal resolving controller source gs filter_ $
      objectRefObjects legal resolving controller source gs (ObjectRef.InSlot slot)
  -- Answered for real by the REVEAL arm, over the seats handChoosers names.
  ObjectRef.RandomCardInHand _ -> []

-- The players a ZoneScope names, in APNAP order -- whose graveyards is
-- Target.zoneScopePlayers, the same answer a target pool over CR 400.1's
-- per-player zone gets, and the order imposed on it here is APNAP (CR 608.2f, CR
-- 101.4) restricted to the players still in the game. The seat half of both
-- graveyardCards and ObjectRef.ChosenCardInGraveyard's EachInScope chooser,
-- which asks each seat separately.
--
-- The bindings are the ones the CALLER holds, which is CR 608.2b's re-checked set
-- at resolution: an InSlot scope naming a slot whose target went illegal names
-- nobody, and CR 101.3 ignores that share of the effect.
zoneScopePlayers :: Map.Map SlotName (Set Recipient) -> PlayerId -> GameState -> ZoneScope.ZoneScope -> [PlayerId]
zoneScopePlayers bindings controller gs scope =
  let named = Target.zoneScopePlayers (Just controller) bindings scope gs
   in filter (`elem` named) (Game.apnapOrder gs)

-- The cards in ONE player's graveyard matching the filter, in ascending
-- ObjectId. The filter is matched in THIS EFFECT's context -- the caller's, so
-- CR 109.5's "you" is the resolving controller rather than whoever is choosing,
-- and the resolution's own slots ride along: Midnight Tilling's "from among
-- them" is `IsBound` over the slot its own mill defined, which a bare contextFor
-- would answer False for on every candidate.
graveyardCardsOf :: Filter.Context -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
graveyardCardsOf context gs pid filter_ =
  let viewOf = Projection.viewsOf gs
   in List.sort
        ( filter
            (\oid -> Filter.matches context (viewOf oid) filter_)
            (Game.zoneMembers Zone.Graveyard pid gs)
        )

-- The cards in ONE player's hand matching the filter: graveyardCardsOf one zone
-- over. The filter is matched in THIS EFFECT's context, so `controller` is CR
-- 109.5's "you" rather than whoever is choosing.
--
-- NOT sorted, where the graveyard sibling sorts: the candidates keep the zone's
-- own order, which no rule reads (CR 402.3). Narrowing must not reorder.
handCardsOf :: Filter.Context -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId]
handCardsOf context gs pid filter_ =
  let viewOf = Projection.viewsOf gs
   in filter
        (\oid -> Filter.matches context (viewOf oid) filter_)
        (Game.zoneMembers Zone.Hand pid gs)

-- The objects a Create bound into `slot` as a GROUP, read off the RESOLVING
-- stack object's live bindings rather than out of `chosen`, which projects CR
-- 601.2c's targets only. Live is what lets a later effect of the same resolution
-- name what an earlier Create minted.
--
-- A Just here wins over the slot's target, which would skip a CR 608.2b
-- re-validation. One Binding can carry both fields, but a Pawl.CardSpec lint
-- ("no delayed ability declares a target slot under a name its card defines")
-- rules the case out, so this arm never actually chooses.
slotGroup :: SlotName -> ObjectId -> GameState -> Maybe (Seq.Seq ObjectId)
slotGroup slot resolving gs = Binding.objectsOf slot (maybe Map.empty Object.bindings (Game.lookupObject resolving gs))

-- The context every effect of a resolution evaluates its quantities and its
-- ref-borne filters in: CR 109.5's "you" is the resolving controller, the source
-- frames CR 113.7, and the resolution's slot objects ride along so a
-- Quantity.AgainstSlot can aim at one and a Filter.IsBound can ask whether a
-- candidate is among them.
--
-- Of the TARGET half, only LEGAL recipients and only OBJECT ones, and only where
-- the slot names exactly one (CR 608.2b); all three drop out as an absent key, so
-- a quantity is unanswered rather than answered off the source.
--
-- The GROUP half comes in beside `legal` rather than through it: CR 115.10a makes
-- a group a definition and never a target, so it owes CR 608.2b nothing and is
-- read live off the resolving object (slotBindings) instead. It reaches
-- Filter.IsBound whole, and the singular readers decline it
-- (Filter.slotOneObject).
--
-- The AMOUNTS ride the same live read, which is why the parameter is the whole
-- binding map rather than the groups alone: a number an earlier clause stamped
-- (bindAmountSlot) is on the resolving object exactly as a group is.
--
-- The NAMES of those same objects ride along too, which is why the parameter is a
-- GameState rather than Teams alone: this module can read a board and
-- Pawl.Engine.Filter cannot, so CR 201.2a's SameNameAsBound is answerable at a
-- resolution's positions exactly as it is at a target slot's
-- (Pawl.Engine.Target.slotContext). A THUNK, as it is there: one projection per
-- bound object, paid for only by a filter naming the atom.
effectContext :: GameState -> PlayerId -> ObjectId -> Map.Map SlotName (Set Recipient) -> Map.Map SlotName Binding.Type.Binding -> Filter.Context
effectContext gs controller source legal bindings =
  let objects = Binding.withGroups (effectSlotObjects legal) (Binding.groupsOf bindings)
   in (Filter.contextWithSlots (Game.teams gs) (Just controller) (Just source) objects)
        { -- CR 608.2c: the numbers earlier clauses of THIS resolution stamped on
          -- slots, for the one Filter atom that compares a candidate against one
          -- (Filter.PowerIsAmountInSlot) -- Localized Destruction's "power equal to
          -- the amount of {E} paid this way". Live off the resolving object, the
          -- group half's own read, so a clause reads what the clause before it bound.
          Filter.boundAmounts = Map.mapMaybe Binding.Type.amount bindings,
          -- CR 201.2a's names off the same objects, through CR 608.2h's
          -- last-known reader for slotContext's reason: Bifurcate's bound
          -- creature may have left by the time the search runs, and "that
          -- creature" is a reference the spell already made rather than a target
          -- CR 608.2b re-checks.
          Filter.slotNames = fmap (foldMap (foldMap Filter.names . Projection.viewWithLastKnownAnywhere gs)) objects,
          -- CR 601.2c's PLAYERS out of the same CR 608.2b-filtered map
          -- effectSlotObjects takes the objects from, and the reason the
          -- resolution has to hand them over at all: CR 113.7 makes
          -- Filter.source the ability's SOURCE, while its targets and its
          -- trigger's bindings are stamped on the ability object on the stack,
          -- so Pawl.Engine.Count.playersFor reading the source's own bindings
          -- finds nothing for every ability. Keening Stone's "that player's
          -- graveyard" proves the activated road and Price of Knowledge's "that
          -- player's hand" the triggered one (Pawl.CountSpec).
          Filter.slotPlayers = fmap (Set.fromList . Maybe.mapMaybe Recipient.playerOf . Set.toList) legal
        }

-- The ONE object each of a resolution's TARGET slots names, shared by
-- effectContext above and effectViewOf below so the two cannot disagree about
-- which object a slot is.
effectSlotObjects :: Map.Map SlotName (Set Recipient) -> Map.Map SlotName ObjectId
effectSlotObjects = Map.mapMaybe Recipient.objectOf . Map.mapMaybe Binding.onlyOne

-- The GROUP bindings a resolution has made so far, read LIVE off the resolving
-- object (CR 608.2c): a slot an earlier clause of this same resolution defined is
-- part of the state a later one is read against, which is exactly what "from
-- among them" needs. By the name the effect wrote, as slotGroup above reads it.
slotBindings :: ObjectId -> GameState -> Map.Map SlotName Binding.Type.Binding
slotBindings resolving gs = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)

-- CR 608.2h's reader for one resolution: Projection.viewWithLastKnown, which
-- answers the SOURCE off its last known information, widened to two reserved
-- slots whose object is gone by construction.
--
-- Binding.sacrificedPermanent is the first -- CR 601.2h paid the cost before the
-- ability was on the stack at all, and CR 701.21a put the permanent in a
-- graveyard as a new object (CR 400.7) -- so viewWithLastKnown's blank answer for
-- a non-source object would leave Jarad, Golgari Lich Lord's "the sacrificed
-- creature's power" permanently unanswerable.
--
-- Binding.departedPermanent is the second, and CR 603.10a is why: what a
-- leaves-the-battlefield trigger says "it" about is the permanent as it last
-- existed on the battlefield, which CR 400.7 has already deleted by the time the
-- ability resolves. Resourceful Defense's "put those counters" reads its whole CR
-- 122.8 tally through here.
--
-- The blank is still right for every OTHER non-source id, and that is why this
-- names two slots rather than lifting the scope: those ids are TARGETS, and CR
-- 608.2b wants a target that has left to answer with nothing. Neither slot here
-- was ever a target (CR 115.10a).
effectViewOf :: ObjectId -> Map.Map SlotName (Set Recipient) -> GameState -> ObjectId -> Maybe Filter.View
effectViewOf source legal gs oid =
  let slots = effectSlotObjects legal
      lookBack slot = Map.lookup slot slots == Just oid
   in if lookBack Binding.sacrificedPermanent || lookBack Binding.departedPermanent
        then Projection.viewWithLastKnownAnywhere gs oid
        else Projection.viewWithLastKnown source gs oid

-- The members of a group that a ref's own Filter matches: the shared half of "a
-- card from among them" and "all cards from among them", so the choice one makes
-- and the sweep the other takes cannot come apart.
--
-- Matched in THIS EFFECT's context -- CR 109.5's "you" is the resolving
-- controller and not whoever is choosing, and the resolution's own slot bindings
-- ride along -- against the CR 613 projection, so a card a continuous effect
-- made a creature is a creature card here. The caller's order survives, which
-- for a group is mint order (CR 608.2f).
matchingFromAmong :: Map.Map SlotName (Set Recipient) -> ObjectId -> PlayerId -> ObjectId -> GameState -> Filter.Type.Filter Keyword.Type.Keyword -> [ObjectId] -> [ObjectId]
matchingFromAmong legal resolving controller source gs filter_ members =
  let context = effectContext gs controller source legal (slotBindings resolving gs)
      viewOf = Projection.viewsOf gs
   in filter (\oid -> Filter.matches context (viewOf oid) filter_) members
