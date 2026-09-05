-- The event pipeline (CR 603/614/616). Owns the single zone-change funnel, CR
-- 616.1's loop and the `apply` that carries out a chosen replacement. The
-- trigger side lives beside it: the sole casing on TriggerCondition is
-- Pawl.Engine.Event.Match, what a match binds is Pawl.Engine.Event.Binding,
-- and the scan that gathers triggers is Pawl.Engine.Event.Trigger. All three
-- are imported here under the same Event alias callers already use.
--
-- The loop and the funnel share a module because the rules make them mutually
-- recursive, not because either is convenient here: a zone change raises its
-- event through the loop, because CR 614.1's replacement effects "watch for a
-- particular event that would happen" and a zone change is one of those events,
-- and applying a chosen rewrite can itself
-- change zones -- CR 614.1c's "as this permanent enters, sacrifice any number of
-- permanents" is a replacement whose application is a CR 701.21a sacrifice. The
-- SELECTION half -- which effects exist, which apply, how they bucket, who
-- chooses, how a row is spent -- stays in Pawl.Engine.Replacement, which this
-- module calls down into and which must never import this one.
--
-- changeZone lives here rather than in Pawl.Engine.Game so it can read the
-- projection -- Projection imports Game, so a Game.changeZone reading it would be
-- an import cycle.
module Pawl.Engine.Event where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Attach as Attach
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Coin as Coin
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.CounterRestriction as CounterRestriction
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.EntryRestriction as EntryRestriction
import Pawl.Engine.Event.Match (matchesTriggerGiven)
import Pawl.Engine.Event.Trigger (battlefieldAt, battlefieldCandidates, delayedPending, eventTriggers, interveningHolds, stateTriggers)
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaRider as ManaRider
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.BattlefieldCandidate as BattlefieldCandidate
import qualified Pawl.Types.BecameAttached as BecameAttached
import qualified Pawl.Types.BecameTarget as BecameTarget
import Pawl.Types.CandidateId (CandidateId)
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardLeavesGraveyard as CardLeavesGraveyard
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CarryOver as CarryOver
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Countering as Countering
import Pawl.Types.DamageEvent (DamageEvent)
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import Pawl.Types.DelayedTrigger (DelayedTrigger)
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.DrawCountR as DrawCountR
import qualified Pawl.Types.DrawCountRewrite as DrawCountRewrite
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.Drew as Drew
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.EntryFlip as EntryFlip
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LifeGainR as LifeGainR
import qualified Pawl.Types.LifeGainRewrite as LifeGainRewrite
import qualified Pawl.Types.LifeLossCause as LifeLossCause
import qualified Pawl.Types.LifeLossR as LifeLossR
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.MeldSource as MeldSource
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Moved as Moved
import Pawl.Types.Object (Object)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.Onset (Onset)
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.OutsideObject as OutsideObject
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PermanentWasSacrificed as PermanentWasSacrificed
import Pawl.Types.PhaseSelector (PhaseSelector)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import Pawl.Types.Prevention (Prevention)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.ProposedEvent (ProposedEvent)
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import Pawl.Types.ReplacementCandidate (ReplacementCandidate)
import qualified Pawl.Types.ReplacementCandidate as ReplacementCandidate
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TokenLot as TokenLot
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.Transformed as Transformed
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import Pawl.Types.TurnWindow (TurnWindow)
import qualified Pawl.Types.TurnWindow as TurnWindow
import qualified Pawl.Types.UntapRewrite as UntapRewrite
import qualified Pawl.Types.WithCounters as WithCounters
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- CR 608.2i: append one entry to the turn-scoped log. The single APPEND point,
-- which is also what lets it be where CR 603.10's "immediately after an event" is
-- sampled: the caller has already applied the event to the board by the time it
-- gets here, so the pre-append state IS the board the rule asks about, and
-- GameState.battlefieldWhenTriggered files it under this event's group.
--
-- Sampled at EVERY record, not only at the one that opens a batch, and the
-- overwrite within a group is deliberate rather than waste: a group is CR 608.2f's
-- single event, its members are applied and recorded one at a time, and the rule
-- asks about the board after the whole thing -- so the last member's sample is the
-- right one and the earlier thunks are dropped unforced.
--
-- Cheap for that reason: the thunk a discarded sample left is never projected, and
-- the one that survives is projected at the scan, which would have projected the
-- board there anyway. What the change costs against a per-batch sample is one
-- projection per event GROUP instead of one per batch.
--
-- The group stamped on the entry is CR 704.3 / CR 608.2f's "single event": a
-- fresh one per record, unless a `simultaneously` bracket is open, in which case
-- every event in its body shares the one the bracket froze. Only advanced at
-- depth 0, which is what freezes it.
recordEvent :: GameEvent -> GameState -> GameState
recordEvent event gs =
  let group = GameState.nextEventGroup gs
   in gs
        { GameState.events = GameState.events gs Seq.|> LoggedEvent.MkLoggedEvent {LoggedEvent.group = group, LoggedEvent.event = event},
          GameState.nextEventGroup =
            if GameState.eventGroupDepth gs == 0
              then EventGroup.next group
              else group,
          GameState.battlefieldWhenTriggered =
            Map.insert group (battlefieldCandidates gs) (GameState.battlefieldWhenTriggered gs)
        }

-- CR 725.2 / CR 726.2: the creature that dealt this logged event's COMBAT damage
-- to `victim`, paired with who controlled it. Nothing for any other event, and
-- Nothing for a source that was not a creature then.
--
-- Read off `battlefieldAt` and not the live board -- CR 603.10's first sentence,
-- not its look-back: the damager is judged as it stood immediately after the
-- damage, and the CR 704.5g destruction that kills a trampler its blocker traded
-- with is a LATER event. Engine.performSettle runs that state-based action before
-- placePendingTriggers, and changeZone mints a fresh id, so a live read finds
-- nothing at all where the rules find a creature (CR 608.2h says the same from the
-- other side). Proved by Pawl.InitiativeSpec's "CR 726.2 a trampler that trades
-- with its blocker still hands the initiative over".
--
-- Both halves come from the ONE sample, so "was it a creature?" and "whose was
-- it?" cannot be answered about two different boards.
combatDamagerAgainst :: PlayerId -> GameState -> LoggedEvent.LoggedEvent -> Maybe (ObjectId, PlayerId)
combatDamagerAgainst victim gs logged = case LoggedEvent.event logged of
  GameEvent.DamageDealt ev
    | DamageEvent.kind ev == DamageKind.Combat && DamageEvent.target ev == Recipient.ToPlayer victim ->
        case Map.lookup (DamageEvent.source ev) (battlefieldAt (LoggedEvent.group logged) gs) of
          Just candidate
            | Set.member CardType.Creature (PC.cardTypes (BattlefieldCandidate.characteristics candidate)) ->
                Just (DamageEvent.source ev, BattlefieldCandidate.controller candidate)
          _ -> Nothing
  _ -> Nothing

-- CR 704.3 / CR 608.2f: run `body` as ONE event, so every event it records
-- shares an EventGroup and CR 603.10a's look-back can tell "at the same time"
-- from "one after the other".
--
-- A BRACKET rather than a parameter, because the sites that know a body is one
-- event are not the sites that record its events: CR 704.3's state-based-action
-- pass drives half a dozen funnels, and CR 701.8's batched destruction runs a CR
-- 616.1 replacement loop per member. Everything either reaches is inside the one
-- event by the same rule.
--
-- Every caller must be able to cite the rule that makes its body a single event;
-- a bracket placed wider than such a rule reaches silently fuses two real events,
-- which is exactly the error this type exists to make impossible.
--
-- The OUTERMOST bracket wins. Nesting keeps the depth above zero, so an inner
-- bracket neither mints a group nor ends the outer one's -- the same posture CR
-- 616.1g's nested entry loops already take, where the outer event owns the inner
-- ones.
--
-- The group is spent on exit whether or not the body recorded anything, so a
-- bracket leaves a gap rather than leaking its group to the next event.
--
-- Not bracketed: CR 701.21's fold over a bound group (#757), token creation and
-- CR 508.1's attacker declaration, so the events each records are read as a
-- sequence (see #441). CR 510.2's combat damage IS: Pawl.Engine.Damage.dealWave
-- brackets each combat damage step -- the damage and its CR 120.3 results, with
-- lifelink's gains recorded after the bracket closes, since CR 702.15e makes
-- each source's gain an event of its own.
simultaneously :: Game a -> Game a
simultaneously body = do
  State.modify' openEventGroup
  result <- body
  State.modify' closeEventGroup
  pure result

-- `simultaneously` for a body that is a pure function of the board rather than a
-- Game action. CR 800.4a's FIRST clause is the caller: leaving the game is not a
-- zone change, so what Pawl.Engine.Departure does there is delete objects and
-- record events, which needs no funnel -- and every object it takes leaves at the
-- same instant. That rule's fourth clause IS a zone change and takes the monadic
-- bracket above instead.
--
-- The two halves are shared with the bracket above rather than restated, so a
-- pure caller cannot come to disagree with a monadic one about what a group is.
simultaneouslyPure :: (GameState -> GameState) -> GameState -> GameState
simultaneouslyPure body = closeEventGroup . body . openEventGroup

-- | CR 701.27a, recorded: one GameEvent.Transformed per permanent that turned
-- over. The ONE writer of that event, shared by the two roads that turn a
-- permanent over -- Pawl.Engine.Resolve's CR 701.27a opcode and
-- Pawl.Engine.Daytime's CR 702.145c/f sweep, which reaches it as a function
-- argument because this module already imports that one.
--
-- Called with the ids that ACTUALLY turned (Pawl.Engine.Game.facesTurned), never
-- with the instruction's victims: CR 701.27c/d/f and CR 702.145b each leave an
-- instruction doing nothing, and CR 603.2 has no event to fire on then.
--
-- AFTER the write, which is what makes CR 701.27e's "immediately after it does
-- so" fall out twice over: the characteristics sampled here are the turned
-- permanent's, and recordEvent's own sample of the battlefield sees the back
-- face's abilities, so a trigger printed on the face just turned to is among the
-- candidates that fire.
--
-- ONE event group for the whole set (CR 608.2f): Moonmist transforms every Human
-- at once, and CR 603.10a must not be able to tell two of them apart.
recordTransformed :: [ObjectId] -> GameState -> GameState
recordTransformed oids gs = case oids of
  [] -> gs
  _ ->
    simultaneouslyPure
      ( \g0 ->
          List.foldl'
            (\g oid -> recordEvent (GameEvent.Transformed (Transformed.MkTransformed oid (Projection.project oid g))) g)
            g0
            oids
      )
      gs

openEventGroup :: GameState -> GameState
openEventGroup gs = gs {GameState.eventGroupDepth = GameState.eventGroupDepth gs + 1}

closeEventGroup :: GameState -> GameState
closeEventGroup gs =
  let depth = GameState.eventGroupDepth gs
      left = if depth == 0 then 0 else depth - 1
   in gs
        { GameState.eventGroupDepth = left,
          GameState.nextEventGroup =
            if left == 0
              then EventGroup.next (GameState.nextEventGroup gs)
              else GameState.nextEventGroup gs
        }

-- CR 119.4: a player may pay an amount of life greater than 0 only if their life
-- total is at least that amount.
--
-- Lives HERE, beside recordEvent, and not in Pawl.Engine.Mana or
-- Pawl.Engine.Cost, because paying life is a recorded game event and this module
-- owns those. All three readers of CR 119.4 then share one reading of it: CR
-- 107.4f's Phyrexian symbol and CR 119.4's own PayLife cost component through
-- Pawl.Engine.Mana and Pawl.Engine.Cost, which import this module, and CR
-- 614.1c's as-enters "you may pay N life" in `apply` below, which could not
-- import Pawl.Engine.Mana at all -- that module imports this one.
--
-- CR 119.4b is answered BEFORE the lookup, not by the `>=` that would usually
-- absorb it: players can ALWAYS pay 0 life, whatever their total and even where
-- an effect says they can't pay life. So a player the map does not hold must not
-- turn a zero payment into an unpayable one -- and a cost with no Phyrexian
-- symbol and no PayLife component asks Pawl.Engine.Mana.payableResolutions' life
-- clause about 0 and nothing else, so that clause cannot change any answer such a
-- cost used to give.
canPayLife :: PlayerId -> Natural -> GameState -> Bool
canPayLife pid n gs =
  n == 0 || case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Player.life player >= toInteger n

-- CR 119.4: the payment is subtracted from the player's life total. The CR 704.5a
-- state-based action that may follow is the existing one in Pawl.Engine.Sba --
-- paying to exactly 0 is a legal payment, not a barred one.
--
-- CR 119.4's own last clause, "in other words, the player loses that much life",
-- is why this goes through resolveLifeLoss, CR 614.1's funnel for the class, and
-- carries LifeLossCause.ByPayment: a replacement that watches life loss reaches a
-- payment, and one scoped to damage does not. What is SUBTRACTED is the settled
-- loss, which a row may have grown, shrunk, or removed outright -- Ashiok, Wicked
-- Manipulator exiles cards instead and no life moves at all (CR 614.6). What the
-- cost CHARGED is unchanged either way, and is `canPayLife`'s business rather than
-- this function's, which is Ashiok's own ruling: its ability "doesn't allow you to
-- attempt to pay an amount of life greater than your current life total".
--
-- Monadic, unlike the pure writes beside it, because applying a replacement can
-- ask a player a CR 616.1 question. All three callers -- the mana-and-life
-- settlement and the CostComponent.PayLife arm in Pawl.Engine.Cost, and the CR
-- 614.1c pay-or-enter-tapped rewrite above -- already run in Game.
--
-- CR 119.4b's always-payable 0 loses nothing: resolveLifeLoss declines a zero
-- itself, so it records nothing and writes nothing.
payLife :: PlayerId -> Natural -> Game ()
payLife pid n = do
  settled <- resolveLifeLoss LifeLossCause.ByPayment pid n
  Monad.when (settled /= 0) . State.modify' $ \gs ->
    recordEvent (GameEvent.LifeLost (LifeChange.MkLifeChange pid settled)) $
      gs
        { GameState.players =
            Map.adjust (\p -> p {Player.life = Player.life p - toInteger settled}) pid (GameState.players gs)
        }

-- CR 110.5b: stamp the tapped status onto an entering permanent, the write shared
-- by EntryRewrite.Tapped (CR 614.1d), by the declining half of both
-- EntryRewrite.PayLifeOrTapped and EntryRewrite.RevealOrTapped (CR 614.1c), and
-- by the taking half of EntryRewrite.AsCopy's `tapped` (Vesuva) -- which is why
-- it is one function: those sentences differ in what they charge and not in what
-- they leave on the board.
--
-- ENTERS TAPPED, not "enters, then is tapped". The status goes straight onto the
-- object rather than through `tap` below, so the permanent never transitions from
-- untapped to tapped and nothing watching for that can fire. That is CR 603.2e in
-- as many words: an ability that triggers when a permanent "becomes tapped"
-- doesn't trigger if the permanent enters the battlefield in that state. See the
-- arms in `apply` for why stamping the already-materialized incarnation is
-- observationally the same as minting it tapped.
enterTapped :: ObjectId -> Game ()
enterTapped oid =
  State.modify' $ \gs ->
    let stamp obj = obj {Object.tapped = TapState.Tapped}
     in gs {GameState.objects = Map.adjust stamp oid (GameState.objects gs)}

-- CR 701.26a: turn one permanent sideways from an upright position, and record
-- that it did. The single funnel every route that taps goes through --
-- Pawl.Engine.Cost.tapObject for a cost component, Pawl.Engine.Resolve's
-- Effect.Tap opcode, CR 508.1f's attacker declaration in
-- Pawl.Engine.Combat.declareAttackers, and CR 701.19a's regeneration in `apply`
-- below -- so that a card watching for a tap sees every one of them.
--
-- Rule 701.26a's SECOND sentence is the guard: "only untapped permanents can be
-- tapped", so an already-tapped permanent is left alone and nothing is recorded.
-- The write on its own is idempotent and the guard would be redundant for it; the
-- EVENT is not, and CR 603.2e's "they don't ... retrigger if it persists" is
-- exactly what a second record would break.
--
-- What does NOT come through here is a permanent entering the battlefield tapped
-- (`enterTapped` above, and Pawl.Engine.Resolve.Effect.putTapped) -- CR 603.2e's other
-- sentence, and the reason those two stay direct writes.
--
-- CR 110.5 makes tapped a PERMANENT's status, which every caller above already
-- has: a cost's tap candidates, an attacker, a regenerating permanent and the
-- Effect.Tap opcode's victims are all on the battlefield.
tap :: ObjectId -> Game ()
tap oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Just obj
      | Object.tapped obj == TapState.Untapped ->
          State.put
            . recordEvent (GameEvent.BecameTapped oid)
            $ gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
    _ -> pure ()

-- CR 701.26b: rotate one permanent back upright from a sideways position, and
-- let CR 614 have its say first. The single funnel every route that untaps goes
-- through -- Pawl.Engine.Cost.payComponent's UntapThis for CR 107.6's untap
-- symbol, Pawl.Engine.Resolve's Effect.Untap opcode, and CR 502.3's turn-based
-- action in Pawl.Engine.Engine.untapAll, which calls `proposeUntap` directly so
-- that its own write -- and its own event -- stay simultaneous.
--
-- GameEvent.BecameUntapped is recorded only where `proposeUntap` said the
-- permanent will untap, so rule 701.26b's second sentence and CR 614's
-- replacements both gate the event as well as the write. CR 603.2e's other
-- exclusion is discharged the same way `tap` above discharges it: a permanent
-- that ENTERS untapped never comes through here at all.
untap :: ObjectId -> Game ()
untap oid = do
  survives <- proposeUntap oid
  Monad.when survives . State.modify' $ \gs ->
    recordEvent (GameEvent.BecameUntapped oid) gs {GameState.objects = writeUntappedIn oid (GameState.objects gs)}

-- `untap`'s replacement half, split out so CR 502.3's simultaneous untap can run
-- it over a whole set before writing any of them.
--
-- Rule 701.26b's SECOND sentence is the guard: "only tapped permanents can be
-- untapped", so an already-untapped permanent proposes nothing. The write on its
-- own is idempotent and the guard would be redundant for it; CR 122.1d's counter
-- is not, and an untapped permanent that shed a stun counter every untap step
-- would be the bug that guard prevents. `tap` above states the mirror of this.
--
-- False means a replacement took the event and has already done its own work.
proposeUntap :: ObjectId -> Game Bool
proposeUntap oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Just obj
      | Object.tapped obj == TapState.Tapped -> do
          outcome <- applyReplacements (ProposedEvent.WouldUntap oid)
          pure (Maybe.isJust (outcome >>= Replacement.asUntap))
    _ -> pure False

-- The write itself, shared by `untap` and CR 502.3's batch so the two cannot
-- disagree about what untapping is. Over the OBJECT MAP rather than the whole
-- state, which is what lets Engine.untapAll fold it across a set inside one
-- State.modify' and so untap them simultaneously.
writeUntappedIn :: ObjectId -> Map ObjectId Object -> Map ObjectId Object
writeUntappedIn = Map.adjust (\o -> o {Object.tapped = TapState.Untapped})

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.TookInitiative _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.DungeonCompleted _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing
  GameEvent.BecameUntapped _ -> Nothing
  GameEvent.TappedForMana _ -> Nothing
  GameEvent.CoinFlipped {} -> Nothing
  GameEvent.RingTempted _ -> Nothing
  GameEvent.Blighted _ -> Nothing
  GameEvent.CardArrived _ -> Nothing

-- Who revealed what, if the event is a reveal (CR 701.20a).
revealOf :: GameEvent -> Maybe (PlayerId, PC.ProjectedCharacteristics)
revealOf event = case event of
  GameEvent.Revealed (Revealed.MkRevealed pid _ _ snapshot) -> Just (pid, snapshot)
  GameEvent.Moved {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.TookInitiative _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.DungeonCompleted _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing
  GameEvent.BecameUntapped _ -> Nothing
  GameEvent.TappedForMana _ -> Nothing
  GameEvent.CoinFlipped {} -> Nothing
  GameEvent.RingTempted _ -> Nothing
  GameEvent.Blighted _ -> Nothing
  GameEvent.CardArrived _ -> Nothing

-- CR 117.5: the events the trigger scan has not yet consumed, WITH the
-- EventGroup each belongs to. Only eventTriggers wants the groups; every other
-- reader takes unscannedEvents below.
--
-- The drop counts ELEMENTS, not groups, and must keep doing so: the two
-- watermarks drain this log at different cadences, so a group can be half
-- consumed.
unscannedGrouped :: GameState -> [LoggedEvent.LoggedEvent]
unscannedGrouped gs =
  Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.scannedThrough gs)) (GameState.events gs))

-- CR 117.5: the events the trigger scan has not yet consumed.
unscannedEvents :: GameState -> [GameEvent]
unscannedEvents = fmap LoggedEvent.event . unscannedGrouped

-- The events the STATE-BASED ACTION check has not yet consumed -- CR 704.5h's
-- "since the last state-based action check", and the same boundary CR 903.9a
-- names. Pawl.Engine.Sba hands these to Pawl.Engine.Commander.returnable, the
-- other reader.
unscannedSbaEvents :: GameState -> [GameEvent]
unscannedSbaEvents gs =
  fmap LoggedEvent.event (Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.damageScannedThrough gs)) (GameState.events gs)))

-- CR 704.5h: the damage the state-based-action check has not yet consumed.
unscannedDamage :: GameState -> [DamageEvent]
unscannedDamage gs =
  Maybe.mapMaybe damageOf (unscannedSbaEvents gs)

-- Insert a freshly-built object into `dest` under a new id and timestamp, and
-- return that id. The common tail of changeZone (a moved incarnation) and
-- createTokens (a token from nothing). `mkObj` receives the fresh timestamp so the
-- object records when it entered (CR 613.7d). The Moved event is emitted by the
-- CALLER: only it knows which state the CR 608.2h snapshot must be taken against.
--
-- `position` is CR 401.2's end for a LIBRARY destination and is inert for every
-- other one; see Game.insertIntoZone, which is the only thing that reads it.
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> LibraryPosition.LibraryPosition -> Game ObjectId
placeObject pid mkObj dest position = do
  gs <- State.get
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = mkObj ts
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
  State.put (Game.insertIntoZone dest position pid newId gs3)
  pure newId

-- Mint a CARD -- an object backed by Source.OfCard -- straight into a zone,
-- with no zone change, because it was in no zone to leave.
--
-- Two roads reach it and they are not the same rule. CR 400.11c's wish brings in
-- a card the player already owned outside the game, which rule 400.11 says is
-- not a zone; Alchemy's conjure creates a card that was in nobody's deck, which
-- no rule of the CR covers at all. Both end at Object.MkObject's field list, and
-- sharing it is what keeps the two from drifting -- a field added to Object is
-- answered once here rather than twice.
--
-- Not routed through Event.changeZone for that shared reason: a zone change
-- would announce a departure from a zone the card was never in.
--
-- The MATERIALIZATION only. A battlefield destination is an entry as well, and
-- CR 616.1's loop and the CR 603.6a scan run after this rather than inside it --
-- see conjureOntoBattlefield, which is the one caller passing that zone.
--
-- `position` is CR 401.2's end for a LIBRARY destination and inert elsewhere,
-- placeObject's own note; Game.insertIntoZone is the only reader.
--
-- Sickness.Sick either way: CR 302.6 asks whether the permanent has been under
-- its controller's control continuously since their most recent turn began, and
-- a card minted straight onto the battlefield has not; one minted into any other
-- zone is settled by the entry that eventually puts it there.
--
-- `under` is CR 110.2a's controller for a BATTLEFIELD mint -- "if an effect
-- instructs a player to put an object onto the battlefield, that object enters
-- the battlefield under that player's control" -- and Nothing for every other
-- zone, where CR 110.2 leaves the object no controller to record.
mintCard :: PlayerId -> Maybe PlayerId -> PrintingId.PrintingId -> Zone -> LibraryPosition.LibraryPosition -> GameState.GameState -> (ObjectId, GameState.GameState)
mintCard pid under printingId dest position gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = under,
            Object.source = Source.OfCard printingId,
            Object.zone = dest,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = Map.empty,
            Object.bestowed = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        Game.insertIntoZone
          dest
          position
          pid
          oid
          gs2 {GameState.objects = Map.insert oid obj (GameState.objects gs2)}
      )

-- Alchemy's conjure keyword action: create the given card out of nothing and put
-- it into the conjuring player's zone.
--
-- DIGITAL-ONLY, so there is no rule to cite -- docs/rules.txt has no "conjure".
-- What the CR settles is what this is NOT: CR 111.1's token is not a card, where
-- a conjured object is one, so this interns a Printing and mints Source.OfCard
-- rather than Source.OfToken, and the result is castable and shufflable like any
-- other card.
--
-- Here rather than in Pawl.Engine.Resolve, createEmblem's placement: the mint is
-- what the two roads into mintCard share, and an opcode arm is not the place to
-- decide what an object minted from a card looks like.
--
-- CR 800.4d: "If an object that would be owned by a player who has left the game
-- would be created in any zone, it isn't created." A conjured card is owned by
-- the conjuring player, so a departed one creates nothing -- and the check is
-- ahead of Game.intern, since a printing interned for a card that is never
-- minted is a row nothing points at. Not CR 800.4b's third sentence, which the
-- token funnel createTokens cites: that rule leaves the object in its current
-- zone, and a conjured card is in no zone to remain in.
--
-- Nothing is this funnel's word for a card that was not created, the empty Seq's
-- twin in conjureOntoBattlefield below.
conjure :: PlayerId -> Card -> Zone -> LibraryPosition.LibraryPosition -> Game (Maybe ObjectId)
conjure pid card dest position = do
  gs <- State.get
  if List.notElem pid (Game.stillPlaying gs)
    then pure Nothing
    else do
      printingId <- State.state (Game.intern (Printing.MkPrinting card))
      Just <$> State.state (mintCard pid Nothing printingId dest position)

-- The same keyword action with the BATTLEFIELD as its destination, which is the
-- one arrival that is an entry: `conjure` above puts the card into a zone and
-- stops, where this runs CR 616.1's replacement loop and records the event CR
-- 603.6a's trigger scan reads.
--
-- createTokens is the model, and the whole batch is minted before any member
-- enters for that function's reason: CR 614.12 checks "the characteristics of
-- the permanent as it would exist on the battlefield", and `siblingsOf` is what
-- hands each entry loop the others. Marwyn's Kindred's "conjure a card named
-- Marwyn, the Nurturer and X cards named Llanowar Elves onto the battlefield" is
-- the printed batch; no card in data/cards/ conjures more than one onto the
-- battlefield, so the batch behaviour here is a regression fence rather than a
-- test-backed one.
--
-- CR 110.5b's defaults throughout: nothing here taps the arrival or puts it into
-- combat, which is what this road's producer prints;
-- Pawl.Types.ConjureDestination names the printings that state otherwise.
--
-- CR 110.2a's `Just controller` is correct by construction and unobservable: a
-- conjured card's OWNER is the player who conjured it (mintCard's `pid`, the
-- same seat), and Projection.defaultControllerOf answers the owner when
-- `enteredUnder` is Nothing, so no board can tell the two writings apart.
-- Mutating it to Nothing leaves Pawl.ConjureSpec green, and would stop doing so
-- the day a printing names a conjurer other than the resolving controller
-- (#2638).
--
-- CR 800.4d's guard, `conjure` above's, for the same reason and read off the
-- same parameter -- and CR 800.4b's third sentence agrees for this destination
-- once the card exists, since an object that would be put onto the battlefield
-- under a departed player's control does not arrive. `controller` is also the
-- OWNER (mintCard's `pid` below is the same seat), so one read answers both
-- rules. Reachable in gameplay the day a printing names a conjurer other than
-- the resolving controller (#2638); until then a departed player controls no
-- resolving spell or ability (CR 800.4a, whose three clauses
-- Pawl.Engine.Departure performs), so Pawl.EventSpec's "CR 800.4d no card is
-- conjured under a player who has left the game" drives both funnels directly,
-- as the createTokens case beside it does.
--
-- Inline rather than delegating to a `conjureOntoBattlefieldFor` body, which is
-- createTokens' reason: the project writes no export lists, so a second
-- top-level name would be a public door past the check.
conjureOntoBattlefield :: PlayerId -> Card -> Natural -> Game (Seq.Seq ObjectId)
conjureOntoBattlefield controller card count = do
  gs <- State.get
  if List.notElem controller (Game.stillPlaying gs)
    then pure Seq.empty
    else do
      printingId <- State.state (Game.intern (Printing.MkPrinting card))
      ids <- Monad.replicateM (Natural.toIntSaturating count) (State.state (mintCard controller (Just controller) printingId Zone.Battlefield LibraryPosition.defaultValue))
      let siblingsOf oid = Set.delete oid (Set.fromList ids)
      Monad.mapM_ (\oid -> runEntry (siblingsOf oid) oid) ids
      Monad.mapM_ recordMintedEntry ids
      pure (Seq.fromList ids)

-- CR 114.2: a player gets an emblem with the given abilities, put into the
-- command zone and both owned and controlled by them. CR 613.7a: its entry
-- timestamp is what the projection reads when ordering the continuous effect of
-- any static ability it carries.
--
-- Here rather than in Pawl.Engine.Resolve, because there are two minting sites
-- and only one of them is an opcode: Effect.CreateEmblem is what a CARD says,
-- and Pawl.Engine.Ring.tempt is what CR 701.54c says. An emblem built two ways
-- would be an emblem that could differ.
--
-- Inert per-incarnation fields (an emblem is never tapped, damaged or
-- countered): harmless, nothing reads them here. `enteredUnder = Nothing` is
-- what makes Projection.defaultControllerOf answer the owner, which is CR
-- 109.4c and so CR 114.2's last sentence.
--
-- CR 800.4d's roster read, the conjure funnels above and createTokens below
-- make the same one. An emblem is an object (CR 109.1) owned by the player who
-- gets it (CR 114.2), so a departed one gets nothing -- and the check is ahead
-- of Game.intern, since a printing interned for an emblem that is never minted
-- is a row nothing points at. Not CR 800.4b's third sentence, which the token
-- funnel cites: that rule speaks of the battlefield and the stack, and an
-- emblem goes to the command zone.
--
-- Nothing is this funnel's word for an emblem that was not created, `conjure`'s
-- above; both callers already discarded the id. Inline rather than delegating
-- the mint to a second top-level name, which is conjureOntoBattlefield's reason:
-- the project writes no export lists, so that name would be a public door past
-- the check.
--
-- Both roads into this funnel hand it the resolving controller, who by CR 800.4a
-- controls no resolving spell or ability once they have left, so the guard is
-- reachable only by driving the funnel -- which is what Pawl.EventSpec's "CR
-- 800.4d no emblem is created for a player who has left the game" does, as the
-- conjure and token cases beside it do. Gameplay reaches it the day an effect
-- names a recipient other than the resolving controller.
createEmblem :: PlayerId -> Card -> Game (Maybe ObjectId)
createEmblem pid card = do
  gs <- State.get
  if List.notElem pid (Game.stillPlaying gs)
    then pure Nothing
    else do
      -- An emblem's characteristics are effect-defined (CR 114.3), so its entry is
      -- minted here rather than coming from a deck -- interned once for the one
      -- object it backs, and carrying no print-level data because an emblem is not
      -- a card (CR 114.5).
      emblemId <- State.state (Game.intern (Printing.MkPrinting card))
      let mkObj ts =
            Object.MkObject
              { Object.owner = pid,
                Object.enteredUnder = Nothing,
                Object.source = Source.OfEmblem emblemId,
                Object.zone = Zone.Command,
                Object.tapped = TapState.Untapped,
                Object.facing = Facing.FaceUp,
                Object.exiledFaceDown = False,
                Object.damage = 0,
                Object.sickness = Sickness.Settled pid,
                Object.bindings = Map.empty,
                Object.counters = Map.empty,
                Object.counterTimestamps = Map.empty,
                Object.attachedTo = Nothing,
                Object.chosenColor = Nothing,
                Object.chosenSubtype = Nothing,
                Object.chosenNames = Set.empty,
                Object.chosenPlayer = Nothing,
                Object.timestamp = ts,
                Object.face = Nothing,
                Object.turnedOverAt = Nothing,
                Object.worldSince = Nothing,
                Object.playableFromExile = Nothing,
                Object.plotted = Nothing,
                Object.foretold = Nothing,
                Object.ringBearerFor = Nothing,
                Object.protector = Nothing,
                Object.ventureRoom = Nothing,
                Object.classLevel = Nothing,
                Object.unlockedHalves = Set.empty,
                Object.designations = Set.empty,
                Object.kicked = Map.empty,
                Object.bestowed = False,
                Object.phyrexianLifePaid = 0,
                Object.manaSpent = Mana.MkMana [],
                Object.announcedX = Nothing,
                Object.castFrom = Nothing,
                Object.detainedUntil = Set.empty,
                Object.goadedBy = Set.empty,
                Object.doesNotUntapNext = False,
                Object.exertedBy = Set.empty
              }
      Just <$> placeObject pid mkObj Zone.Command LibraryPosition.defaultValue

-- CR 400.11: the cards a player owns that are not in any of the game's zones,
-- and the roads that bring one in. Outside the game is NOT a zone (CR 400.11),
-- so nothing here holds a zone and no object exists until something takes a card
-- out of the pool; Pawl.Types.Player's outsideTheGame is the pool, and CR 103.2a's
-- sideboard is what fills it through Pawl.Engine.Setup.createDeck.
--
-- These four lived in a Pawl.Engine.OutsideTheGame of their own until the draw
-- replacement in `apply` needed them: that module imported this one for
-- `mintCard` and `reveal`, so the DrawR arm could not reach back into it;
-- see #3100.
--
-- Pawl.Engine.Dungeon is the sibling that does the same for CR 309.2's dungeon
-- cards, which are outside the game too and are deliberately NOT in this pool:
-- rule 309.2 keeps them out of deck and sideboard both, CR 701.49a chooses among
-- them by a rule of its own rather than by a card's filter, and CR 309.2d forbids
-- anything else from bringing one in. Pawl.Types.Player says so at the field.
--
-- What these do NOT decide is which cards are eligible: the Filter comes from the
-- card, and every caller passes it through without asking what effect produced it.

-- CR 400.11c: which of a player's cards outside the game an effect's Filter
-- admits, in interning order.
--
-- The Filter is evaluated against the PRINTED FACE, which is the whole of what a
-- card outside the game has: CR 604.3 makes a characteristic-defining ability the
-- one thing that functions out there, and Projection.viewOfCard reads it off the
-- face. No object exists to project (CR 400.11), so there is nothing else to ask.
--
-- A printing whose count has fallen to zero is not offered. `take` below deletes
-- such an entry rather than leaving it at zero, so the guard is a belt on top of
-- braces -- and cheap enough to keep, since a caller assembling this map by hand
-- would otherwise offer a card that is no longer out there.
--
-- The Context is a BARE Filter.contextFor, carrying no slot bindings even where a
-- resolution is in flight -- and the draw-replacement caller in `apply` has none
-- to carry at all. Honest here rather than #2141's silence: CR 400.11c
-- keeps a spell or ability from affecting a card outside the game, so no slot of
-- the resolution can name one, and the candidate view below is a printed FACE
-- with no `identity` for Filter.IsBound to compare in any case. Pawl.CardSpec's
-- "CR 400.11c no card asks IsBound in a wish's filter" is what keeps a card out
-- of the position; Pawl.OutsideTheGameSpec's "CR 400.11c a wish's filter cannot
-- see what the resolution bound" proves the atom answers nothing here.
--
-- Two sources feed this: CR 103.2a's sideboard pool (OutsideCard.InPool) and,
-- when this game is a subgame, CR 729.4's main-game objects held in
-- GameState.outsideObjects (OutsideCard.InAnotherGame). A face-up entry of
-- either kind is read through the PRINTED FACE, since CR 729.1b gives a
-- main-game effect no meaning inside the subgame -- a main-game object is read
-- as its card, not as its (subgame-invisible) projected characteristics, which
-- is why OutsideObject carries no characteristics of its own.
--
-- A FACE-DOWN main-game object is the one exception, and CR 708.2 is why: "face-
-- down spells and face-down permanents have no characteristics other than those
-- listed by the ability or rules that allowed the spell or permanent to be face
-- down", and those listed values are the object's own COPIABLE values. Copiable
-- values are not an effect or a definition created in the main game, so CR
-- 729.1b does not reach them -- the same reason the entry's owner survives the
-- crossing -- and the object is offered as CR 708.2a's 2/2 creature with no
-- name. So Living Wish reaches a manifested sorcery and Burning Wish does not.
--
-- The alternative rejected: reading every entry through the printed face, on the
-- ground that a face-down permanent's characteristics come from a main-game
-- ability. That collapses CR 729.1b's bar on EFFECTS into a bar on what the
-- object IS -- an object's copiable values would then have to be recomputed from
-- its printing at every frame boundary, which no rule asks for -- and it makes
-- the wish able to name a card by a face the rules say the object does not have.
--
-- Reading it leaks nothing CR 708.5 protects, and less than the printed face
-- did: what the filter is matched against is CR 708.2a's public 2/2, and every
-- entry scanned is one the acting player OWNS (CR 108.3b's guard below), so it
-- is a card they already know even where another player controls it.
eligible :: Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> PlayerId -> GameState.GameState -> [OutsideCard.OutsideCard]
eligible predicate source pid gs =
  let pool = maybe Map.empty Player.outsideTheGame (Map.lookup pid (GameState.players gs))
      context = Filter.contextFor (Game.teams gs) (Just pid) (Just source)
      matchesFace face = Filter.matches context (Projection.viewOfCard face) predicate
      admits printingId = case Game.cardOfPrinting printingId gs of
        Nothing -> False
        Just card -> matchesFace (Game.resolveFaceFor Nothing card)
      -- Pawl.Engine.Card.faceDownFace is the same substitution
      -- Pawl.Engine.Game.faceOfObject performs for an object in this game, so
      -- the two frames cannot disagree about what a face-down object is.
      admitsOutside entry = case OutsideObject.facing entry of
        Facing.FaceDown state -> matchesFace (Card.faceDownFace (FaceDownState.listed state))
        Facing.FaceUp -> admits (OutsideObject.printing entry)
      fromPool = [OutsideCard.InPool printingId | (printingId, n) <- Map.toAscList pool, n > 0, admits printingId]
      -- CR 108.3b scopes the reach to the acting player's OWN cards outside the
      -- game -- the owner guard below is that scope, not an ownership check on
      -- the pool (which is already per-player).
      fromOuter =
        [ OutsideCard.InAnotherGame oid
        | (oid, entry) <- Map.toAscList (GameState.outsideObjects gs),
          OutsideObject.owner entry == pid,
          admitsOutside entry
        ]
   in fromPool <> fromOuter

-- CR 400.11c: put a card this player owns from outside the game matching the
-- Filter into their hand, showing it first (CR 701.20a) where the payload's
-- reveal says the card prints one -- Burning Wish's sentence, and Death Wish's
-- without the reveal.
--
-- The card is MINTED, Pawl.Engine.Dungeon.enter's road: outside the game is not
-- a zone (CR 400.11), so no object stood for the card and the move into the hand
-- is not a zone change. `mintCard` above is where that happens, and its haddock
-- says why the insertion does not go through `changeZone`.
--
-- The pool is SPENT, unlike Pawl.Engine.Dungeon.enter's supply: CR 400.11b keeps
-- a card brought in "in the game until the game ends", so a second Burning Wish
-- cannot find the same copy. A second COPY of the same printing survives the
-- decrement, which is why Player.outsideTheGame counts rather than remembering a
-- set.
--
-- CHOSEN, not targeted (CR 115.10a): CR 601.2c would have announced a target as
-- the spell was cast, and CR 400.11c lets nothing target a card out there.
-- FILTERED, NOT TRUSTED, Pawl.Engine.Dungeon.enter's posture: an answer naming a
-- printing this player does not own out there, or one the Filter does not admit,
-- falls back to the first offered.
--
-- A player with no eligible card reveals nothing and puts nothing into their
-- hand, which is CR 609.3's "if an effect attempts to do something impossible,
-- it does only as much as possible" -- and is why
-- this returns unit rather than the id: nothing about Burning Wish's sentence
-- reads the card back.
--
-- Not implemented: where the reveal is printed it happens as the card ARRIVES in
-- the hand rather than before the move as the card prints it (#2450).
bringInto :: FromOutsideTheGame.FromOutsideTheGame -> ObjectId -> PlayerId -> Game ()
bringInto payload source pid = do
  gs0 <- State.get
  let predicate = FromOutsideTheGame.filter payload
      -- CR 701.20a is a keyword action of its own, so a card that does not print
      -- it moves the card and shows nobody anything.
      showIt oid = Monad.when (FromOutsideTheGame.reveal payload) (reveal RevealCause.Ordinary pid oid)
  case NonEmpty.nonEmpty (eligible predicate source pid gs0) of
    Nothing -> pure ()
    Just offered -> do
      chosen <- case offered of
        only NonEmpty.:| [] -> pure only
        first NonEmpty.:| _ -> do
          answer <- Game.choose (Prompt.ChooseFromOutsideTheGame (Decide.deciderFor pid gs0) pid offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      -- Against the LIVE state and not gs0, Pawl.Engine.Dungeon.enter's care:
      -- Game.choose above wrote the answer into the transcript, and minting off
      -- the state from before the prompt would drop that.
      case chosen of
        OutsideCard.InPool printingId -> do
          oid <- State.state (bringIn pid printingId)
          showIt oid
        OutsideCard.InAnotherGame outerId -> do
          gs1 <- State.get
          case bringInFrom pid outerId gs1 of
            (Nothing, _) -> pure ()
            (Just oid, gs2) -> do
              State.put gs2
              showIt oid

-- CR 400.11b: take one copy of this printing out of the player's pool and mint
-- the card into their hand. Split out from `bringInto` above because it is the half
-- every other road into the game will want -- CR 727.2's restart (#135) and CR
-- 707.13's copy created outside the game (#888) -- and none of those reveals
-- anything. The SPEND is the whole of what it adds over `mintCard`, which
-- Alchemy's conjure reaches with nothing to spend.
bringIn :: PlayerId -> PrintingId.PrintingId -> GameState.GameState -> (ObjectId, GameState.GameState)
bringIn pid printingId gs =
  let (oid, gs1) = mintCard pid Nothing printingId Zone.Hand LibraryPosition.defaultValue gs
      -- One copy, not the entry: CR 100.2a's four-card limit is applied to the
      -- combined deck and sideboard (CR 100.4a), so copies of a card are COUNTED
      -- and a player who set aside two can be brought the second one later.
      spend n = if n <= 1 then Nothing else Just (n - 1)
      spent p = p {Player.outsideTheGame = Map.update spend printingId (Player.outsideTheGame p)}
   in (oid, gs1 {GameState.players = Map.adjust spent pid (GameState.players gs1)})

-- CR 729.4a: bring in a card from a game that is on hold. The entry is dropped
-- and the OUTER id is appended to GameState.broughtIn, which is the whole record
-- the outer frame needs: this game cannot reach that game's state, and must
-- not (CR 729.1a keeps the two apart while the subgame runs). `Nothing` when
-- the id no longer names an outside entry -- a second wish reaching for a card
-- something already brought in -- mirrors `bringIn`'s own belt-on-braces guard
-- for a spent InPool entry.
--
-- The card arrives FACE UP whatever the entry's facing was, and the entry's
-- printing is what is minted. CR 708.9 has the owner reveal a face-down
-- permanent as it leaves the battlefield, and CR 400.7 makes what arrives here a
-- new object; a wish that reaches a manifested card gets the card, not the 2/2
-- `eligible` offered it as.
bringInFrom :: PlayerId -> ObjectId -> GameState.GameState -> (Maybe ObjectId, GameState.GameState)
bringInFrom pid outerId gs = case Map.lookup outerId (GameState.outsideObjects gs) of
  Nothing -> (Nothing, gs)
  Just entry ->
    let (oid, gs1) = mintCard pid Nothing (OutsideObject.printing entry) Zone.Hand LibraryPosition.defaultValue gs
     in ( Just oid,
          gs1
            { GameState.outsideObjects = Map.delete outerId (GameState.outsideObjects gs1),
              GameState.broughtIn = GameState.broughtIn gs1 Seq.|> outerId
            }
        )

-- CR 614: settle a proposed zone change. Nothing means the move does not happen.
-- The typed door changeZoneAttaching below uses, so the funnel itself never cases
-- on a ProposedEvent.
--
-- `asOf` is applyReplacementsIn's: Nothing for a lone move, Just the pre-batch
-- board when this move is one member of a CR 608.2f / 704.3 batch.
--
-- CR 903.9b's rules-based offer is asked here too, once the loop has settled a
-- destination -- see `offerCommandZone`.
--
-- Also reports CR 607.2b's link: the object whose replacement effect is what made
-- the destination exile, or Nothing when the move was headed there on its own
-- instruction. The caller files it once the arriving incarnation has an id.
--
-- And CR 701.24's shuffle: whether any row that applied carries it. Reported
-- rather than performed, because a card cannot be shuffled INTO a library it is
-- not in yet -- the move happens after this returns, so changeZoneAttaching is
-- the earliest moment the action is well defined. The reveal is not reported
-- alongside it because it acts on the card in the zone it is leaving, and
-- `apply` already holds that id -- see the arm there for why that is a
-- convenience rather than a constraint.
resolveZoneChange :: Maybe GameState -> ZoneChange -> Game (Maybe ZoneChange, Maybe ObjectId, Bool, Maybe PrintingId.PrintingId)
resolveZoneChange asOf zc = do
  (outcome, _, _, exiledBy, shuffling) <- applyReplacementsFully asOf Set.empty (ProposedEvent.WouldChangeZone zc)
  case outcome >>= Replacement.asZoneChange of
    Nothing -> pure (Nothing, exiledBy, shuffling, Nothing)
    Just settled -> do
      (redirected, splitOff) <- offerCommandZone settled
      pure (Just redirected, exiledBy, shuffling, splitOff)

-- CR 903.9b: "if a commander would be put into its owner's hand or library from
-- anywhere, its owner may put it into the command zone instead". The question;
-- Pawl.Engine.Commander.commandZoneOffer is the condition, and says why the owner
-- is who is asked.
--
-- A RULES step in the funnel rather than a third segment of
-- Pawl.Engine.Replacement.collect, which holds a battlefield permanent's printed
-- static abilities and the floating rows a resolution installed -- both things a
-- CARD carries. Synthesizing a ReplacementEffect.ZoneChangeR from rule 903.9b
-- would put a rules-invented value into the structure whose whole point is that it
-- came from data, and rule 903.9b's "may" has no home on that type in any case:
-- every ZoneChangeR is unconditional, so the prompt would have to be asked from
-- `apply`'s generic arm under a guard on WHICH candidate this is.
-- Pawl.Engine.Cast.legendaryRestrictionOk takes the same posture toward CR 205.4e:
-- a restriction the rulebook states is asked by the engine, not modelled as
-- something a card carries.
--
-- AFTER the CR 616.1 loop and asked of the SETTLED destination, CR 614.6's
-- reading: a printed redirect that already moved this move off a hand or a library
-- is the event that happens, and rule 903.9b has nothing to say about it.
--
-- Not implemented: a place in CR 616.1's ordering, where CR 616.1e leaves the
-- affected player free to pick among the applicable effects and this offer instead
-- always goes last (#2266). Unobservable while no ZoneChangeR in data/cards/
-- matches a hand or a library -- every row there names a `whenDestination` of
-- the graveyard, the stack or the battlefield -- so no second candidate can be
-- applicable to the same event; a printed redirect naming a hand or a library as
-- the destination it watches (Wheel of Sun and Moon is the shape) would refute
-- that.
--
-- No case on effect identity: the question is a proposed event's destination ZONE
-- and whether its subject is a commander.
offerCommandZone :: ZoneChange -> Game (ZoneChange, Maybe PrintingId.PrintingId)
offerCommandZone zc = do
  gs <- State.get
  case Commander.commandZoneOffer zc gs of
    Nothing -> pure (zc, Nothing)
    Just owner -> do
      decision <- Game.choose (Prompt.ReturnCommander (Decide.deciderFor owner gs) owner (ZoneChange.departed zc))
      pure $ case decision of
        CommandZoneDecision.Leaves -> (zc, Nothing)
        CommandZoneDecision.Returns -> case Commander.commandZoneComponent (ZoneChange.departed zc) gs of
          -- CR 614.6: the modified event is what happens, so the destination is
          -- rewritten rather than the card being moved twice. changeZoneAttaching
          -- places it under Object.owner, which is rule 903.9b's "its owner" for a
          -- stolen commander too.
          Nothing -> (zc {ZoneChange.to = Zone.Command}, Nothing)
          -- CR 903.9c: a melded or merged commander does NOT go to the command
          -- zone whole. "That permanent and each component representing it that
          -- isn't a commander are put into the appropriate zone, and the card
          -- that represents it and is a commander is put into the command zone"
          -- -- so the event's destination is left as it was and only the named
          -- component splits off, which changeZoneAttaching performs where CR
          -- 712.21's own split already places one object per component.
          --
          -- Reported rather than performed here for that reason: no component
          -- exists yet at this point in the funnel, so the card the rule names
          -- can only be identified by its PRINTING, and the id it will get is
          -- minted several steps later.
          Just component -> (zc, Just component)

-- CR 616.1's loop. `Nothing` means the event DOES NOT HAPPEN (CR 615.6, CR
-- 701.19a). A rewrite that cancels an event has already performed its own
-- consequences by the time it returns Nothing.
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = applyReplacementsIn Nothing Set.empty

-- CR 608.2f / 704.3: `asOf` is the board a BATCH's candidates are read from --
-- `Just` the state the batch began in, or `Nothing` for the live board. Only the
-- destroy funnel passes `Just` (`destroy`, `destroyInBatch` below), along with
-- the graveyard moves it and Pawl.Engine.Sba's put-into-graveyard batch make
-- through `changeZoneInBatch`. Everything else is a lone event and wants the
-- live board. CR 608.2f and CR 704.3 make a batch ONE event, so CR 614.4 asks
-- which effects existed before the BATCH, not before the member being processed
-- -- otherwise Rest in Peace animated by Opalescence and swept by Day of
-- Judgment answers according to an order CR 608.2f gives nobody the right to
-- decide.
--
-- `asOf` and `batch` both name a batch, and they are OPPOSITES: `asOf` widens
-- the candidate set to include effects of permanents the batch is itself
-- removing, while `batch` NARROWS the copy-target set to exclude permanents
-- entering beside the loop's subject. Deliberately not one parameter: different
-- batches and different readers. Effect.MoveToZone's CR 608.2f batch
-- (Pawl.Engine.Resolve) supplies both, and even there they hold different values
-- -- the pre-batch board against the ids that have already arrived out of it.
--
-- What `asOf` does NOT freeze: the FLOATING store stays live (see
-- Replacement.collect), because CR 614.3 has Replacement.consume spend a one-shot
-- as it applies and a frozen
-- store would hand a spent regeneration shield to the next member of the batch;
-- the loop still RE-COLLECTS every iteration, so CR 616.1f and CR 616.2 are
-- untouched; and `apply`'s writes and Replacement.choose's chooser lookup read the LIVE
-- state. A permanent that ENTERED after the batch began therefore contributes
-- nothing, which is CR 614.4 read the other way. No producer today, so that half
-- is unexercised.
--
-- `batch` is the set of ids entering the battlefield AT THE SAME TIME as this
-- loop's subject, NOT counting the subject itself -- every door subtracts it
-- before passing the set. THREE readers narrow by it, on three different rules.
-- The loop's own candidate filter is the first (CR 614.12; see `loop`), and two
-- of `apply`'s entry arms are the other two:
--
--   * COPY TARGETS. CR 614.12a puts the choice BEFORE the permanent enters, and
--     an as-enters copy may only copy a permanent already ON the battlefield
--     (Clone's "any creature", Copy Enchantment's "any enchantment"), so a sibling
--     entering in the same batch is not there yet at the moment the choice is
--     made. No rule states that exclusion outright: it follows from 614.12a's
--     timing plus the copy effect's own wording. CR 614.13a is the wrong cite for
--     it -- that rule is about an entry effect moving OTHER objects to a
--     different zone, and a copy target never changes zones.
--   * THE AS-ENTERS SACRIFICE (EntryRewrite.SacrificeAnyNumber). Here CR 614.13a
--     is exactly the rule, because a sacrificed permanent does change zones:
--     "You can't choose the object that will become that permanent or any other
--     object entering the battlefield at the same time as that object."
--
-- Both arms exclude the loop's own subject themselves -- legalCopyTargets'
-- `self`, and the sacrifice arm's `entering` -- rather than leaning on this set
-- being subject-free.
--
-- Every `changeZone` door but changeZoneEnteringIn handles one entering object at
-- a time and passes `Set.empty`. There are two non-empty cases, and they reach
-- the same invariant from opposite directions. `createTokens` below materializes
-- every token of a Create BEFORE running any of their entry loops (CR 614.16's
-- doubled count is settled once, up front), so it knows the whole set and passes
-- each token the rest of it. Effect.MoveToZone's CR 608.2f batch
-- (Pawl.Engine.Resolve) moves its
-- members through the funnel one after another, so it ACCUMULATES the set as
-- each arrives: a member still in its old zone is not on the battlefield for a
-- sweep to find, and one that has arrived is. Either way a later member's entry
-- loop would otherwise find its siblings already sitting on the battlefield.
--
-- Where the CR says the cards are NOT simultaneous the set is empty again and a
-- sibling is a plain battlefield permanent, which is the whole of what CR
-- 701.40e's one-at-a-time manifest buys: Pawl.Engine.Resolve hands each such card
-- its own event rather than one accumulating fold.
--
-- A simultaneously-entering sibling can reach a later member's entry loop through
-- four channels; only the first needs this explicit exclusion:
--   1. Copy targets -- excluded by `batch`. Two boards observe it, one per
--      producer: kicked Rite of Replication on a Clone, where five token Clones
--      enter at once (Pawl.CopySpec's "none may copy a sibling"), and Rise of the
--      Dark Realms over a graveyard holding a Clone, where the batch is a zone
--      change (Pawl.CopySpec's "a reanimated Clone may not copy a creature
--      reanimated beside it"). Both fail without this exclusion.
--   2. Candidate collection -- excluded by `batch` in `loop`, which drops every
--      candidate a sibling SOURCES. Most entry replacements a PERMANENT carries
--      in this pool are CR 614.1c's self-only `IsSource` (Clone, Primal Plasma,
--      CR 306.5b's loyalty), which no sibling can satisfy, so the exclusion bites
--      on the other-objects forms: Kismet's CR 614.1d "enter tapped" and
--      Corpsejack Menace's CR 614.1 counter doubling. Pawl.ReplacementSpec's
--      "a Corpsejack Menace reanimated beside a modular creature doubles nothing"
--      is the proof, over a Rise of the Dark Realms batch; without it an Arcbound
--      Worker returned beside the Menace enters 2/2 instead of 1/1, and which
--      answer it got depended on the order CR 608.2f gives nobody the right to
--      decide.
--   3. Projection -- a sibling's STATIC ABILITIES would otherwise be visible to
--      every projection read a later member's entry loop makes: Kismet's filter
--      through `applies`, the copy arm's Projection.copiableCharacteristics and
--      isCreatureOf, Projection.controllerOf, and the SACRIFICE arm's offer, which
--      narrows the whole battlefield by a Filter and is the read this pool
--      observes. CR 614.12 does not sanction any of them: a simultaneously-
--      entering sibling's continuous effects do not already exist relative to it.
--      Excluded by GameState.enteringBeside, which runEntry writes for the span of
--      one member's loop and Pawl.Engine.Projection.View.abilitySources subtracts, so
--      the exclusion covers every read at once rather than one call site at a
--      time. Pawl.ReplacementSpec's "a Wood Elemental reanimated beside Ashaya
--      sacrifices nothing" is the proof: without it Ashaya, Soul of the Wild makes
--      a bystanding Goblin Piker a Forest land, and the Wood Elemental arriving
--      beside it eats the Piker.
--   4. Board membership -- a CARD-AUTHORED count, either in the clause that gates
--      a row (Pawl.Types.PrintedReplacement's, on a permanent's static ability,
--      and Pawl.Types.ActiveReplacement's, on the floating row a resolution
--      installed) or in the amount a rewrite places (the
--      EntryRewrite.WithCounters arm below). All of them fold over the
--      battlefield, which holds the sibling whether or not its abilities function,
--      so channel 3 does not reach them; and this is the one channel the loop's
--      own SUBJECT is on the wrong side of as well. Excluded by
--      Pawl.Engine.Projection.boardAsEntering, which subtracts
--      GameState.enteringSubjects too -- the same board for every segment, since
--      CR 614.12a's "before the permanent enters the battlefield" is about zone
--      membership rather than about which segment asks. The CLAUSE half is proven
--      in both segments, one pair per direction each: Pawl.ReplacementSpec's
--      Frontier Mastodon for the printed one and its Synthetic Magnetic Lockdown
--      for the floating one, and the AMOUNT half by its Squad Captain.
applyReplacementsIn :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacementsIn asOf batch event = do
  (outcome, _, _, _, _) <- applyReplacementsFully asOf batch event
  pure outcome

-- The same loop, answering CR 615.13's second question as well: WHICH prevention
-- effects applied on the way, and how much each of them prevented.
--
-- A separate entry rather than a wider applyReplacementsIn because only the
-- damage class can answer anything but the empty list -- CR 615.1 makes a
-- prevention effect a thing that watches a DAMAGE event -- so every other caller
-- would be threading a value it knows is empty.
--
-- The survivors are a LIST for the same class's other reason: CR 614.9's
-- counted redirection (Harm's Way) moves part of an event and leaves the rest
-- where it was, so one proposed damage event can come out of the loop as two.
-- Every other class comes out as at most one, which applyReplacementsIn reads
-- off the first component alone.
applyReplacementsReporting :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game ([ProposedEvent], [Prevention])
applyReplacementsReporting asOf batch event = do
  (outcome, residue, prevented, _, _) <- applyReplacementsFully asOf batch event
  pure (Maybe.maybeToList outcome <> residue, prevented)

-- The loop itself, with the side answers its two classes of caller want: CR
-- 615.13's preventions, CR 607.2b's "which object's replacement effect is what
-- exiled this", and CR 701.24's "did a row that applied say to shuffle". Each is
-- empty, Nothing or False for every event class but one -- damage for the first,
-- zone changes for the other two -- which is why the three entries above and
-- around it exist rather than one wide return everywhere.
--
-- The second component is the RESIDUE: the events a partial cover split off
-- this one (Replacement.partialCoverage), each settled through its own
-- continuation of the loop. Empty for every class but damage.
applyReplacementsFully :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent, [ProposedEvent], [Prevention], Maybe ObjectId, Bool)
applyReplacementsFully asOf batch = loop asOf batch Set.empty [] Nothing False

loop :: Maybe GameState -> Set ObjectId -> Set CandidateId -> [Prevention] -> Maybe ObjectId -> Bool -> ProposedEvent -> Game (Maybe ProposedEvent, [ProposedEvent], [Prevention], Maybe ObjectId, Bool)
loop asOf batch applied prevented exiledBy shuffling event = do
  gs <- State.get
  -- From scratch each iteration: collect against the CURRENT state (or, for a
  -- CR 608.2f batch, the state the batch began in), minus CR 614.5's
  -- already-applied set. Re-collecting is what makes CR 616.2 work.
  let unused candidate = not (Set.member (ReplacementCandidate.identity candidate) applied)
      -- CR 614.12: a permanent entering BESIDE this event's subject contributes
      -- nothing. The rule settles which effects apply from "continuous effects
      -- that already exist and would apply to the permanent", and a sibling's
      -- static abilities do not already exist -- they begin to apply once it is on
      -- the battlefield, which is the moment this event happens too. The subject's
      -- OWN abilities are untouched, because `batch` never holds it (see
      -- applyReplacementsIn).
      --
      -- Filtered by SOURCE, which reaches only the permanent segment of `collect`:
      -- a floating row's source is a spell or ability that has already changed
      -- zones and taken a new id, so no id in a batch of entering permanents can
      -- name one.
      --
      -- THE ONLY SEAM the exclusion is enforced at for a permanent's entering
      -- counters, which used to reach it a second time through the counter funnel:
      -- those counters are settled by rows offered against WouldEnter here (see
      -- addEnteringCounters), so a sibling's Corpsejack Menace is filtered out
      -- once, in this loop. Pawl.ReplacementSpec's "a Corpsejack Menace reanimated
      -- beside a modular creature doubles nothing" is the proof.
      notSibling candidate = not (Set.member (ReplacementCandidate.source candidate) batch)
      fresh = filter (\candidate -> unused candidate && notSibling candidate) (Replacement.applicable asOf gs event)
  case Replacement.highestBucket fresh of
    -- CR 616.1f / 614.6: no candidate remains, so the surviving event happens.
    [] -> pure (Just event, [], prevented, exiledBy, shuffling)
    bucket -> do
      picked <- Replacement.choose gs event bucket
      case picked of
        -- Unreachable: highestBucket returns [] for an empty input, so `bucket`
        -- is non-empty and `choose` always picks. Total rather than partial.
        Nothing -> pure (Just event, [], prevented, exiledBy, shuffling)
        -- CR 614.9 over a countdown (CR 615.7's shape, which that rule states
        -- only for preventions; Harm's Way's rulings supply the redirect's):
        -- the chosen effect covers only PART of this event (Harm's Way's
        -- remaining 2 against a 5), so the event is split here and the
        -- candidate applied to the covered half, while the residue -- "any
        -- remaining damage is dealt normally" -- continues through the loop as
        -- an event of its own. It carries the applied set INCLUDING this
        -- candidate, which is CR 614.5: one effect applies to one event once,
        -- and the residue is the rest of that same event rather than a new one
        -- -- so a Furnace of Rath that already doubled it does not double the
        -- residue again. The covered half's own thread runs first, the
        -- residue's after it, both against whatever prompts they raise in that
        -- order.
        --
        -- CR 614.5's other direction is not kept: an effect the covered half's
        -- continuation applies is not recorded for the residue, so it may apply
        -- to both halves. Harmless for every rewrite in the tree, each being
        -- distributive over a split (a doubling of 2 and of 3 is a doubling of
        -- 5); a non-distributive rewrite reaching both halves would be what
        -- refutes it, and none has a producer.
        --
        -- Not implemented: rejoining the two halves when the redirect's
        -- destination IS the residue's recipient, so the one permanent is dealt
        -- two events where the rules deal one and a "whenever this is dealt
        -- damage" trigger fires twice (#3190).
        Just candidate
          | Just covered <- Replacement.partialCoverage gs candidate event,
            Just (front, rest) <- Replacement.splitDamage covered event -> do
              let applied1 = Set.insert (ReplacementCandidate.identity candidate) applied
              (survivor, residue1, prevented1, exiledBy1, shuffling1) <- applyChosen asOf batch applied prevented exiledBy shuffling candidate front
              (leftover, residue2, prevented2, exiledBy2, shuffling2) <- loop asOf batch applied1 prevented1 exiledBy1 shuffling1 rest
              pure (survivor, residue1 <> Maybe.maybeToList leftover <> residue2, prevented2, exiledBy2, shuffling2)
        Just candidate -> applyChosen asOf batch applied prevented exiledBy shuffling candidate event

-- One iteration of `loop`: apply the chosen candidate to the event and continue
-- with what comes back. Split out so the partial-cover branch above and the
-- ordinary one apply a candidate the same way.
applyChosen :: Maybe GameState -> Set ObjectId -> Set CandidateId -> [Prevention] -> Maybe ObjectId -> Bool -> ReplacementCandidate -> ProposedEvent -> Game (Maybe ProposedEvent, [ProposedEvent], [Prevention], Maybe ObjectId, Bool)
applyChosen asOf batch applied prevented exiledBy shuffling candidate event = do
  gs <- State.get
  -- CR 615.12: the chosen effect is a prevention effect and this damage
  -- can't be prevented (Spider-Punk), so it is APPLIED and prevents none
  -- of it. The event comes back undiminished and no shield is written down
  -- -- "existing damage prevention shields won't be reduced by damage
  -- that can't be prevented" -- while the recursive call below still
  -- records it as applied, which is CR 615.12a's "just once" and the
  -- reason this does not spin. What the application still owes is the
  -- rule's middle clause, "any additional effects they have will take
  -- place", which is `applyInertly`'s whole job.
  --
  -- CR 615.3's use count is skipped too, which no card notices: every
  -- prevention row pawl installs is Uses.Unlimited (Resolve.installDamageRow
  -- says why, and Fog's authored row says Unlimited as well), so the
  -- `consume` this bypasses would have been a no-op anyway.
  --
  -- A SEPARATE fold rather than a flag inside `apply`, because CR 615.12 is
  -- a fact about the (effect, event) PAIR and not about any one rewrite:
  -- `apply`'s arms answer "what does this rewrite do", and the rule stops
  -- the rewrite happening at all while leaving its additional effect
  -- standing. CR 614.1a's replacements never come here, so a Furnace of
  -- Rath still doubles unpreventable damage.
  --
  -- CR 615.5's AUTHORED rider is the other half of that middle clause,
  -- and `applyInertly` cannot reach it: the rider rides on the candidate
  -- rather than on the rewrite, and this module cannot run a card's
  -- effects. So the classification is bound here and handed to
  -- `preventionBy` below, which reports a prevention of 0 off the
  -- undiminished event -- enough for Pawl.Engine.Damage to queue the
  -- rider, and not enough for CR 615.13's record. Phantom Tiger loses a
  -- +1/+1 counter to damage it could not prevent (Pawl.ReplacementSpec).
  let inert = Replacement.inertPrevention gs candidate event
  outcome <- case inert of
    Just rewrite -> applyInertly candidate rewrite event
    Nothing -> apply batch candidate event
  -- CR 615.13: read OUTSIDE `apply`, from the event before and after, so
  -- no arm of that fold has to report anything and none can forget to.
  -- What makes it exact rather than a guess is Replacement.prevents: only a
  -- PREVENTION rewrite's shrinkage is prevention, where CR 614.1a's
  -- SetAmount and Scale shrink an event without preventing a point of it.
  let prevented1 = prevented <> Maybe.maybeToList (Replacement.preventionBy inert candidate event outcome)
  case outcome of
    Nothing -> pure (Nothing, [], prevented1, exiledBy, shuffling)
    Just rewritten -> loop asOf batch (Set.insert (ReplacementCandidate.identity candidate) applied) prevented1 (exiledByAfter candidate event rewritten exiledBy) (shuffling || shufflesAfter candidate) rewritten

-- CR 607.2b's link, read OUTSIDE `apply` from the event before and after --
-- `preventionBy`'s posture above, for its reason: no arm of that fold has to
-- report anything, so none can forget to. A rewrite that moved a proposed zone
-- change's destination INTO exile is the "replacement event caused by" the row's
-- own object that the rule links to, so the arriving card is that object's and
-- not the resolving ability's, which CR 607.2a's "as a result of an instruction
-- to exile them in the first ability" already excludes it from.
--
-- A later rewrite that moves the destination back OUT of exile clears the link:
-- no card arrives in exile for the rule to speak of. One that leaves an
-- already-exile destination alone changes nothing, since the earlier row is
-- still what caused the exile. Neither of those two arms is exercised, and not
-- by a claim about Magic: no ReplacementEffect.ZoneChangeR in data/cards/ names
-- exile -- or nothing, which admits every zone -- as the `whenDestination` it
-- watches, so none can fire on a destination an earlier row already moved into
-- exile, and no board can stack two of them into a rewrite chain that leaves it
-- again. A printed row watching exile, or one naming no destination at all
-- (CR 702.34a's "instead of putting it anywhere else" is the wording), would
-- refute that and reach both arms. Only the first has a producer.
--
-- `offerCommandZone` is not a second way to reach them either: it runs after this
-- loop has finished, so no candidate is read off its answer, and rule 903.9b only
-- fires on a destination of hand or library.
--
-- No case on effect identity: the question is a proposed event's destination
-- ZONE, and the answer is the row's source object.
exiledByAfter :: ReplacementCandidate -> ProposedEvent -> ProposedEvent -> Maybe ObjectId -> Maybe ObjectId
exiledByAfter candidate before after exiledBy =
  case (fmap ZoneChange.to (Replacement.asZoneChange before), fmap ZoneChange.to (Replacement.asZoneChange after)) of
    (Just old, Just new)
      | new == Zone.Exile, old /= Zone.Exile -> Just (ReplacementCandidate.source candidate)
      | new /= Zone.Exile -> Nothing
    _ -> exiledBy

-- CR 701.24: did the row that just applied say to shuffle? Read off the
-- candidate rather than off the event, unlike `exiledByAfter` above, because a
-- shuffle leaves no mark on the proposed event for a before/after diff to find.
--
-- Once True, always True: an accumulator, so a second redirect that names no
-- shuffle cannot take back the first one's. CR 701.24c is why -- the library is
-- shuffled even once the object has been moved elsewhere.
--
-- A CLASSIFICATION of the row, not a test of which card it came from. One arm
-- per constructor and no wildcard, `Replacement.readsApplier`'s discipline: a
-- wildcard answering False would let an author who teaches a new arm to carry a
-- shuffle get a silent no-op instead of a build failure.
shufflesAfter :: ReplacementCandidate -> Bool
shufflesAfter candidate = case ReplacementCandidate.effect candidate of
  ReplacementEffect.ZoneChangeR zoneChangeR -> ZoneChangeR.shuffling zoneChangeR
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.DamageR {} -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.TokenR {} -> False
  ReplacementEffect.TurnUpR {} -> False
  ReplacementEffect.UntapR _ -> False
  ReplacementEffect.LifeLossR {} -> False
  ReplacementEffect.LifeGainR {} -> False
  ReplacementEffect.DrawR {} -> False
  ReplacementEffect.DrawCountR {} -> False
  ReplacementEffect.PhaseR _ -> False

-- CR 615.12: apply one chosen PREVENTION effect to damage that can't be
-- prevented. The event comes back undiminished -- "those effects won't prevent
-- any damage" -- and what each arm below does is only the rule's middle clause,
-- "any additional effects they have will take place".
--
-- Separate from `apply` rather than a flag threaded through it, because the two
-- answer different questions: `apply`'s arms say what a rewrite DOES, and CR
-- 615.12 is a fact about the (effect, event) pair that stops the rewrite
-- happening at all. Only what survives the rule is here.
--
-- No `consume` and no `setShield` in any arm, which is CR 615.12's last sentence:
-- "existing damage prevention shields won't be reduced by damage that can't be
-- prevented". CR 615.3's use count is skipped with it, which no card notices --
-- `loop` above says why.
--
-- One arm per DamageRewrite constructor, `apply`'s discipline for `apply`'s
-- reason: a new prevention rewrite carrying an additional effect must break the
-- build here rather than silently losing it. The three that `Replacement.prevents`
-- refuses are unreachable, since `Replacement.inertPrevention` answers Just only
-- for a rewrite that prevents.
applyInertly :: ReplacementCandidate -> DamageRewrite.DamageRewrite -> ProposedEvent -> Game (Maybe ProposedEvent)
applyInertly candidate rewrite event = do
  case rewrite of
    -- CR 122.1c's "prevent that damage and remove a shield counter from it". The
    -- removal is the additional effect, and it is AMOUNT-INDEPENDENT -- the rule
    -- removes one counter per application whatever it prevented, here nothing --
    -- so it is the half of the clause that survives unpreventable damage.
    DamageRewrite.PreventRemovingShieldCounter -> removeCounters (ReplacementCandidate.source candidate) CounterKind.Shield 1
    -- CR 615.7's countdown shield has no additional effect of its own; its
    -- remaining amount is exactly the "existing damage prevention shield" the
    -- rule's last sentence protects.
    DamageRewrite.PreventNext _ -> pure ()
    -- CR 615.10's static shield stores nothing and carries no additional effect
    -- of its own either; a CR 615.5 rider printed beside it rides the CANDIDATE,
    -- as Fog's does below.
    DamageRewrite.PreventAllBut _ -> pure ()
    -- Fog's blanket prevention likewise carries nothing beyond the prevention:
    -- CR 615.5's authored rider rides on the CANDIDATE rather than on the
    -- rewrite, so it is `loop`'s business above -- which queues it through
    -- `preventionBy` -- and not this fold's. Phantom Tiger's counter comes off
    -- that way rather than here.
    DamageRewrite.PreventAll -> pure ()
    -- Unreachable: `Replacement.prevents` refuses these three, so no inert
    -- application ever reaches them. CR 614.1a's replacements are not preventions
    -- and are applied in full to unpreventable damage.
    DamageRewrite.SetAmount _ -> pure ()
    DamageRewrite.Scale _ -> pure ()
    DamageRewrite.Redirect _ -> pure ()
    DamageRewrite.RedirectNext _ _ -> pure ()
    DamageRewrite.RedirectMatching _ -> pure ()
  pure (Just event)

-- CR 614.6: apply one chosen effect. Nothing means the event does not happen.
--
-- One arm per ReplacementEffect constructor, same shape as `applies`, so a new
-- constructor breaks the build HERE too. A wildcard fallback would defeat that:
-- an author who teaches `applies` a new arm but forgets this one gets a silent
-- no-op replacement. Every arm below either rewrites its paired event or, for a
-- pair `applies` already excludes, falls through to `pure (Just event)` --
-- unreachable in practice, but present so the match stays total per constructor
-- rather than total by wildcard.
--
-- The same discipline applies one level down, to each arm's inner SUM type
-- (DamageRewrite, DestructionRewrite, EntryRewrite, Scaling), never to the
-- pattern RECORDS, which are read for their fields rather than cased. An arm
-- must CASE on the inner sum, not bind it with `_`: `_` is exhaustive
-- UNCONDITIONALLY, so it raises no build failure and no warning when a new
-- constructor is added, silently treating a real rewrite as a no-op.
--
-- CounterR's and TokenR's arms delegate Scaling whole to `scale`, which is where
-- the exhaustive case lives -- a new Scaling constructor breaks `scale`'s build
-- and both arms' transitively, so casing it again inline would not strengthen
-- anything.
apply :: Set ObjectId -> ReplacementCandidate -> ProposedEvent -> Game (Maybe ProposedEvent)
apply batch candidate event =
  case (ReplacementCandidate.effect candidate, event) of
    (ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR _ toDest revealing _), ProposedEvent.WouldChangeZone zc) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      -- CR 701.20a: the card is shown in the zone it is LEAVING, so the id the
      -- reveal names is the departing one rather than the incarnation CR 400.7
      -- mints on arrival. Here because that is where the loop already has it in
      -- hand, and because Nexus of Fate's "reveal Nexus of Fate and shuffle it
      -- into its owner's library instead" reads in that order -- not because
      -- nowhere else could: changeZoneAttaching still holds `oid` before
      -- placeObject, and with no reveal trigger and no row that cancels a zone
      -- change in the pool, the two placements are observationally identical
      -- today. The shuffle half genuinely cannot be here; see resolveZoneChange.
      --
      -- The OWNER shows it, which is who CR 400.3's destination library belongs
      -- to and who holds the card: a redirect naming Filter.IsSource acts on a
      -- card that is nobody's permanent in the hidden zones this fires from, so
      -- Projection.controllerOf would answer the owner anyway, and Wheel of Sun
      -- and Moon's "revealed and put on the bottom of that player's library"
      -- names the enchanted player, whose graveyard the card was headed for --
      -- its owner (CR 400.3) even when it dies under another player's control.
      -- Pawl.ZoneChangeSpec's "Wheel of Sun and Moon reroutes only the enchanted
      -- player's cards" reads the reveal off the log; no test drives the stolen
      -- permanent.
      Monad.when revealing $ do
        gs <- State.get
        Monad.forM_ (Game.lookupObject (ZoneChange.departed zc) gs) $ \obj ->
          reveal RevealCause.Ordinary (Object.owner obj) (ZoneChange.departed zc)
      pure (Just (ProposedEvent.WouldChangeZone zc {ZoneChange.to = toDest}))
    -- Unreachable: `applies` admits ZoneChangeR only against WouldChangeZone.
    (ReplacementEffect.ZoneChangeR {}, _) -> pure (Just event)
    -- CR 707.5 / 614.1c / 614.12a: the entering object's controller chooses a
    -- permanent to copy, and its copiable characteristics are stamped as this
    -- object's copy snapshot. Writing to the COPIABLE layer (CR 613.1a) is what
    -- makes CR 707.2 fall out for free: a later Clone of this object copies the
    -- stamped values with no further machinery. Clone's "may" is real: Nothing
    -- leaves the object as its printed self (a 0/0, which CR 704.5f then buries).
    --
    -- The class match is on the OUTER tuple, so the INNER `case rewrite of` is
    -- what carries the exhaustiveness obligation -- a wildcard-bound `_` on the
    -- outer pattern would let a new EntryRewrite constructor fall through
    -- silently whenever it happened to pair with WouldEnter.
    (ReplacementEffect.EntryR (EntryR.MkEntryR _ rewrite), ProposedEvent.WouldEnter oid) -> case rewrite of
      EntryRewrite.AsCopy asCopy -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable: the object is materialized on the battlefield before this
          -- loop runs, so controllerOf falls back to its owner. Defensive: make no
          -- unprompted copy choice.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            -- The eligible set is the CARD's, not this arm's (#1512): Clone says
            -- "any creature" and Copy Enchantment "any enchantment", and the
            -- battlefield walk that supplies "on the battlefield" is
            -- legalCopyTargets'. Read HERE, at CR 614.12a's moment, so an entry
            -- replacement applied earlier in the same batch is already visible.
            let legal = Replacement.legalCopyTargets batch (AsCopy.eligible asCopy) oid gs
            answer <-
              if null legal
                then -- With nothing eligible, declining is the only legal answer
                -- -- a forced selection rather than an elision of options a
                -- player could tell apart -- so the prompt is skipped rather
                -- than asked and overruled. RevealOrTapped's posture below.
                -- One candidate IS still asked: the card's "may" makes
                -- declining a real fork.
                  pure Nothing
                else Game.choose (Prompt.ChooseCopyTarget decider controller oid legal)
            -- FILTERED, NOT TRUSTED (#222). legalCopyTargets is the ONLY thing
            -- enforcing CR 614.12a's same-batch exclusion and the printed noun
            -- phrase, so honouring an unoffered answer would let a Clone copy a
            -- sibling token entering beside it, or a Copy Enchantment copy a
            -- creature.
            let chosen = case answer of
                  Just src | List.elem src legal -> Just src
                  _ -> Nothing
            case chosen of
              Nothing -> pure (Just event)
              Just src2 -> do
                State.modify' $ \g ->
                  -- CR 707.9: the exceptions are applied to the snapshot on the
                  -- way in, so they are part of the copy's own copiable values
                  -- (CR 707.9b) rather than an effect layered over them. Only on
                  -- this branch: a declined copy is no copying process, so its
                  -- exceptions do not happen either.
                  let stamped = Replacement.applyCopyExceptions (AsCopy.exceptions asCopy) (copiedSnapshot src2 g)
                      stamp o = o {Object.bindings = Binding.setCopy stamped (Object.bindings o)}
                   in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
                -- CR 614.1d, inside the same sentence: Vesuva enters TAPPED as a
                -- copy. On this branch alone -- a declined copy is a Vesuva that
                -- entered untapped, since the printed "may" governs both halves
                -- at once.
                Monad.when (AsCopy.tapped asCopy) (enterTapped oid)
                pure (Just event)
      -- CR 614.1c / 208.2b: Primal Plasma's choice of which printed
      -- power/toughness-and-keywords option to become. Written into the COPIABLE
      -- snapshot (applyEntryOption), which is what makes CR 616.2 fall out for
      -- free: a Clone that copies Primal Plasma also copies this ability (CR
      -- 707.5), and the loop's next iteration finds it newly applicable.
      EntryRewrite.ChoiceOf options -> do
        gs <- State.get
        case options of
          -- Malformed card data: an as-enters choice with nothing to choose
          -- from. No-op rather than a partial function, but still consumed -- a
          -- floating one-shot must not survive to apply again.
          [] -> do
            Replacement.consume (ReplacementCandidate.identity candidate)
            pure (Just event)
          first : rest -> do
            picked <-
              if null rest
                then -- One option is not a choice; where the rules leave
                -- nothing to ask, don't prompt.
                  pure first
                else case Projection.controllerOf oid gs of
                  -- Unreachable: the object is materialized on the
                  -- battlefield before this loop runs, so controllerOf falls
                  -- back to its owner. Defensive: make no unprompted choice.
                  Nothing -> pure first
                  Just controller -> do
                    let decider = Decide.deciderFor controller gs
                    answer <- Game.choose (Prompt.ChooseEntryOption decider controller oid options)
                    pure (Replacement.at options answer first)
            Replacement.consume (ReplacementCandidate.identity candidate)
            State.modify' (Replacement.applyEntryOption oid picked)
            pure (Just event)
      -- CR 614.1c decided by CR 705.2's first sentence: Molten Sentry's "as this
      -- creature enters, flip a coin", whose face picks between the same two
      -- shapes ChoiceOf offers a player. Written into the COPIABLE snapshot
      -- (applyEntryOption), ChoiceOf's road and for its reason.
      --
      -- Pawl.Engine.Coin, never Game.ask directly, and NO Prompt.CallCoin: "no
      -- player wins or loses a coin flip for this kind of effect", so there is no
      -- call to make and nothing for a CR 723 controller to usurp. Proved by
      -- Pawl.ReplacementSpec's "CR 705.2 nobody wins Molten Sentry's flip, so its
      -- heads face mints no Treasure", on a board holding a Tavern Scoundrel that
      -- would mint two Treasures off a won one.
      --
      -- ALWAYS ASKED, where ChoiceOf elides a one-option prompt: a flip is not a
      -- choice, so nothing about it is indistinguishable, and a card whose two
      -- faces named the same shape would still have flipped a coin (which
      -- Pawl.Types.EntryFlip's two required faces keep separate anyway).
      --
      -- The flip is recorded as a CR 705.1 event with no outcome, which is what
      -- keeps CR 705.2's first sentence honest against
      -- TriggerCondition.PlayerWinsCoinFlip -- see Pawl.Types.CoinFlipped. CR
      -- 705.3's second clause is the exception the rule itself names: an effect
      -- may state that a player WINS this flip, and Edgar, King of Figaro's
      -- ruling says such an ability reaches even a flip that would ordinarily
      -- have no winner. That is the only road to a winner here.
      --
      -- The FLIPPER is read before the flip rather than after the option is
      -- applied, which is what lets rule 705.3 be asked about the right seat; an
      -- entry option cannot change who controls the entering permanent, so the
      -- record below reads the same seat it always did.
      EntryRewrite.ChoiceByCoinFlip entryFlip -> do
        before <- State.get
        let flipper = Projection.controllerOf oid before
        statements <- Coin.statementsFor flipper
        (face, stated) <- Coin.flipOne statements
        let picked = case face of
              CoinFace.Heads -> EntryFlip.heads entryFlip
              CoinFace.Tails -> EntryFlip.tails entryFlip
        -- CR 614.3, ChoiceOf's call verbatim. UNPROVEN by any board: `consume`
        -- is a no-op for a permanent's own static ability (CandidateId
        -- OfPermanent), and Molten Sentry's row is one, so only a FLOATING row of
        -- this shape could observe the call. It is here because rule 614.3 is
        -- what governs such a row, not because a test holds it in place.
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' (Replacement.applyEntryOption oid picked)
        -- CR 109.5's "you" on the entering permanent, ChoiceOf's chooser and CR
        -- 705.2's only involved seat. The seatless branch is unreachable and
        -- defensive, ChoiceOf's position: the object is materialized on the
        -- battlefield before this loop runs, so controllerOf falls back to its
        -- owner. No event is recorded there rather than a seat being conjured --
        -- the flip still happened and still applied.
        Monad.forM_ flipper $ \seat ->
          State.modify'
            ( recordEvent
                ( GameEvent.CoinFlipped
                    CoinFlipped.MkCoinFlipped
                      { CoinFlipped.flipper = seat,
                        CoinFlipped.won = if stated then Just True else Nothing
                      }
                )
            )
        pure (Just event)
      -- CR 614.1c: Painter's Servant's as-enters colour choice. Unlike ChoiceOf
      -- above, this is asked every time the entering object has a controller to
      -- ask: CR 105.1's five colours are always all legal and always
      -- distinguishable, so there is no one-option case to elide.
      --
      -- Written to Object.chosenColor, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseColor.
      EntryRewrite.ChooseColor -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. A WEAKER fallback than
          -- ChoiceOf's, and the one place on this path the engine would decide
          -- something: there is no colour the card named to default to, so white
          -- is conjured. It stands only because the branch cannot be reached.
          Nothing -> pure Color.White
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Game.choose (Prompt.ChooseColor decider controller oid)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenColor = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c: Convincing Mirage's as-enters basic land type choice. Asked
      -- every time the entering object has a controller to ask, for
      -- ChooseColor's reason just above: CR 305.6's five basic land types are
      -- always all legal and always distinguishable, so there is no one-option
      -- case to elide.
      --
      -- Written to Object.chosenSubtype, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseBasicLandType.
      EntryRewrite.ChooseBasicLandType -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. The same WEAKER fallback
          -- ChooseColor's arm carries, and for the same reason: no type the card
          -- named to default to, so Mountain is conjured.
          Nothing -> pure Subtype.Mountain
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Game.choose (Prompt.ChooseBasicLandType decider controller oid)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenSubtype = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c: Stuffy Doll's as-enters player choice. The two arms above ask
      -- unconditionally because rules 105.1 and 305.6 fix their offers; this one
      -- asks the BOARD who is available, so it can be elided in the one case those
      -- two never reach -- a single player still in the game, where CR 102.1's
      -- offer has one member and nothing is left to decide.
      --
      -- Written to Object.chosenPlayer, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChoosePlayer.
      EntryRewrite.ChoosePlayer -> do
        gs <- State.get
        let candidates = Game.stillPlaying gs
        picked <- case (Projection.controllerOf oid gs, NonEmpty.nonEmpty candidates) of
          -- Nobody left to choose from, a board CR 104.2a has already ended the
          -- game on. Chooses nobody rather than conjuring a seat, the posture
          -- designateProtector takes for a battle with no legal protector.
          (_, Nothing) -> pure Nothing
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Chooses NOBODY rather than
          -- conjuring a seat the way ChooseColor's arm conjures white, because a
          -- player is a real board object where a colour is not -- and CR 101.3
          -- already ignores the share of a later instruction that names nobody.
          (Nothing, _) -> pure Nothing
          (Just controller, Just offer)
            -- One candidate is one outcome, so the options are indistinguishable
            -- and the engine decides nothing by not asking.
            | null (NonEmpty.tail offer) -> pure (Just (NonEmpty.head offer))
            | otherwise -> do
                let decider = Decide.deciderFor controller gs
                answer <- Game.choose (Prompt.ChoosePlayer decider controller oid offer)
                -- Filters rather than trusts the answer, Battle.designateProtector's
                -- posture: an interpreter naming a player who is not in the game
                -- gets the head of the offer instead of an illegal designation.
                pure . Just $
                  if List.elem answer candidates
                    then answer
                    else NonEmpty.head offer
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenPlayer = picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c with CR 201.4: Null Chamber's as-enters name choices. Unlike
      -- the two arms above, this one has to settle WHO is asked before it can
      -- ask anything: the card names its controller and one opponent, and CR
      -- 101.4 puts their two simultaneous choices in APNAP order.
      --
      -- Written to Object.chosenNames, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseCardNames.
      EntryRewrite.ChooseCardNames restriction -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Names NOTHING rather than
          -- conjuring a name, which the two arms above cannot do -- their
          -- fallbacks pick from a fixed five, and CR 201.4's offer is every card
          -- in the Oracle card reference.
          Nothing -> pure Set.empty
          Just controller -> do
            -- CR 102.1 makes a player one of the people IN the game, and CR
            -- 104.3a lets one leave at any time -- so the offer is
            -- Game.stillPlaying and not GameState.turnOrder, which keeps a
            -- departed seat.
            let opponents = Game.opponentsOf controller gs
            opponent <- case opponents of
              -- CR 102.2: a two-player game leaves exactly one opponent, and
              -- one option is not a choice. The empty case is a game whose
              -- other seats have all left (CR 104.2a) -- nobody to ask, and no
              -- second name.
              [] -> pure Nothing
              [sole] -> pure (Just sole)
              first : second : rest -> do
                let offered = first NonEmpty.:| (second : rest)
                answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor controller gs) controller oid offered)
                -- FILTERED, NOT TRUSTED, the posture Sba.chooseLegendVictims
                -- takes: an answer naming somebody who is not an opponent would
                -- otherwise hand a second name to a player the card never asked,
                -- so it falls back to the head.
                pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
            -- CR 101.4: the active player chooses first, then the rest in turn
            -- order. Both names are chosen as one event, so the order is the
            -- rule's and not the card's reading order.
            let choosers = filter (\pid -> pid == controller || Just pid == opponent) (Game.apnapOrder gs)
                ask pid = Game.choose (Prompt.ChooseCardName (Decide.deciderFor pid gs) pid oid restriction)
            fmap Set.fromList (Monad.mapM ask choosers)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenNames = picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c with CR 201.4: Runed Halo's as-enters name choice. The arm
      -- above with ONE chooser -- the card names its controller and nobody else
      -- -- so there is no CR 101.4 ordering to settle and no opponent to pick.
      EntryRewrite.ChooseCardName restriction -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arm above's reason: names NOTHING
          -- rather than conjuring one, CR 201.4's offer being every card in the
          -- Oracle card reference.
          Nothing -> pure Set.empty
          Just controller ->
            fmap Set.singleton (Game.choose (Prompt.ChooseCardName (Decide.deciderFor controller gs) controller oid restriction))
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenNames = picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 306.5b via CR 614.1c: this permanent enters with N counters. Into the
      -- pending map through addEnteringCounters, and NOT a direct write to
      -- Object.counters, because CR 614.16 makes a counter-scaling replacement
      -- apply even when the original event was not itself an effect -- so Doubling
      -- Season has to see these. It sees them in THIS loop, as a row against the
      -- same WouldEnter event, which is what lets CR 616.1 order the two. Consumed
      -- like every other arm, so CR 614.5 keeps the loop's next iteration from
      -- counting them twice.
      --
      -- CR 614.1c also admits "a number of ... counters ... equal to [something]"
      -- (Undergrowth Scavenger), so the amount is a Quantity and is evaluated ONCE
      -- here (CR 608.2h: information the effect requires is determined only once,
      -- when the effect is applied), when this row applies, rather than per
      -- iteration of the entry loop around it. CR 107.1b clamps a negative result
      -- to zero, which is Integer.toNaturalSaturating.
      --
      -- The permanent is already materialized on the battlefield when this loop
      -- runs (see runEntry), so the CR 613 projection answers for it. The Context
      -- is the ROW's, through Replacement.candidateContext: CR 109.5's "you" is
      -- the row's controller rather than the entrant's, and a floating row's
      -- captured slot bindings ride along, which is what a bare Filter.contextFor
      -- would have dropped; see #2141 for the caller that still does.
      --
      -- Consumed unconditionally: CR 614.5 is about the row having applied, and a
      -- row whose amount would not evaluate has still applied. Consuming only on
      -- the evaluable branch loops.
      --
      -- EVERY KIND IN ONE APPLICATION, consuming the candidate once, because the
      -- row carries a map of kinds -- Agent's Toolkit's "+1/+1 counter, a flying
      -- counter, a deathtouch counter, and a shield counter" (#2314). CR 614.5
      -- gives a scaling replacement one opportunity over the whole entry, so a
      -- kind per row would put an ordering in CR 616.1's pool that the card's own
      -- sentence does not have; Pawl.ReplacementSpec's Agent's Toolkit case
      -- reads that off a board where the two orders disagree. The amounts are
      -- evaluated against the SAME board for the same reason: the row applies
      -- once, so its amounts are read once.
      --
      -- CR 107.3m: an X inside the amount is the value announced for the SPELL
      -- that became this permanent, not the permanent's own 0 -- Protean Hydra's
      -- "this creature enters with X +1/+1 counters on it". Substituted in rather
      -- than read as a binding, because CR 400.7 left the permanent none: the
      -- announcement rides across the move on Object.announcedX, which
      -- Quantity.substituteAnnouncedX puts back where the rule says it belongs and
      -- nowhere else.
      --
      -- The AMOUNT is CR 614.12's "how they apply", so it counts over
      -- Projection.boardAsEntering rather than the live battlefield -- Squad
      -- Captain's "a +1/+1 counter on it for each other creature you control"
      -- must not count a creature arriving in the same batch. The VIEW stays the
      -- live one, as Projection.replacementsOf's does and for its reason.
      -- Pawl.ReplacementSpec's Squad Captain pair proves it, over a Rise of the
      -- Dark Realms sweep: mutating this call back to the live board reddens
      -- "no +1/+1 counter".
      EntryRewrite.WithCounters (WithCounters.MkWithCounters counters) -> do
        gs <- State.get
        let viewOf = Projection.viewWithLastKnown oid gs
            context = Replacement.candidateContext gs candidate
            announcedX = Projection.announcedXOf oid gs
        Replacement.consume (ReplacementCandidate.identity candidate)
        Foldable.for_ (Map.toList counters) $ \(kind, quantity) ->
          case Quantity.evaluate viewOf context (Projection.boardAsEntering gs) oid (Quantity.substituteAnnouncedX announcedX quantity) of
            Nothing -> pure () -- unevaluable quantity: no counters (Resolve's PutCounters posture)
            Just n -> addEnteringCounters oid kind (Integer.toNaturalSaturating n)
        pure (Just event)
      -- CR 614.1c: "[This permanent] enters ... with [keywords]" -- Faerie
      -- Squadron's "and with flying", the keyword half of the clause whose counter
      -- half the arm above places.
      --
      -- A STORED continuous effect (CR 611.2), riot's landing and every word of
      -- its argument: the clause says the permanent has the keyword and names no
      -- end, which is CR 611.2a's rest-of-the-game duration, and a stored effect
      -- is what puts the grant in CR 613.1f's layer 6 with a timestamp for
      -- Humility to be ordered against. Its source is the entering permanent
      -- itself (CR 113.7).
      --
      -- NOT a write into the copiable snapshot, which is what tells this arm from
      -- ChoiceOf's: CR 707.2 copies an "as . . . enters" ability's values only
      -- where it SETS POWER AND TOUGHNESS, and this clause sets neither, so a
      -- token copy of the entered Squadron has no flying. Pawl.ReplacementSpec's
      -- "CR 707.2 a token copy of the kicked Squadron has neither the flying nor
      -- the counters" is the half that proves it.
      --
      -- ONE timestamp for the whole set, taken once: CR 613.7a gives a static
      -- ability's continuous effect the timestamp of the object it is on, and
      -- Cetavolver's "with first strike and trample" is one clause, so its two
      -- keywords cannot be ordered against each other.
      --
      -- No prompt, and none is owed: the clause offers nothing to choose.
      EntryRewrite.WithKeywords keywords -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the reason riot's arm gives below: the
          -- object is materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner.
          Nothing -> pure (Just event)
          Just controller -> do
            State.modify' $ \gs2 ->
              -- Armed through Pawl.Engine.Expiry rather than naming Expiry.Never,
              -- riot's posture: Indefinite always arms, so the Nothing branch is
              -- unreachable and is written out only because arm is total over
              -- Duration. No bindings -- CR 614.1c's rewrite is a replacement's,
              -- not a resolution's, so its duration can name no slot.
              case Expiry.arm Map.empty controller oid Duration.Indefinite gs2 of
                Nothing -> gs2
                Just expiry ->
                  let (ts, gs3) = Game.freshTimestamp gs2
                      effectFor keyword =
                        ContinuousEffect.MkContinuousEffect
                          { ContinuousEffect.source = oid,
                            ContinuousEffect.timestamp = ts,
                            ContinuousEffect.expiry = expiry,
                            ContinuousEffect.modification = Modification.GainKeyword keyword,
                            -- CR 611.2c: a fixed set of one, settled here -- the
                            -- permanent that entered.
                            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
                          }
                   in gs3 {GameState.continuousEffects = fmap effectFor (Set.toList keywords) <> GameState.continuousEffects gs3}
            pure (Just event)
      -- CR 616.1b / 110.2: Gather Specimens. The entering object's CR 110.2
      -- DEFAULT controller becomes CR 109.5's "you" -- the candidate's
      -- controller, baked when the row was installed -- and that is a permanent
      -- change: the card's "this turn" bounds how long the REPLACEMENT is around
      -- to catch entries, never how long the creature stays yours.
      --
      -- Written to the object rather than to the surviving ProposedEvent, which
      -- is why WouldEnter still carries only an ObjectId. This engine
      -- materializes the entering permanent BEFORE running the entry loop (see
      -- runEntry, and CR 614.12), so the would-be controller is exactly
      -- Projection.controllerOf on the live board. That is also what makes CR
      -- 616.2 fall out: the loop's next iteration re-matches against a board
      -- where the control has already changed, which a value parked on the event
      -- would not show it. All five arms above land on the object for the same
      -- reason.
      --
      -- No prompt, and none is owed: CR 616.1b's rewrite has no choice in it,
      -- and the choice the rule DOES describe -- which of several
      -- control-modifying effects to apply -- is `choose`'s, one level up.
      --
      -- No roster check on `you`, and none is owed: CR 800.4a's second clause
      -- ends a floating control-on-entry row the moment its controller leaves
      -- (Departure.givesControlOnEntryTo), so no row that reaches here can name a
      -- departed player. Pawl.ReplacementSpec's "CR 800.4a a control-on-entry row
      -- ends when its controller leaves the game" is what proves it.
      EntryRewrite.UnderSourceControl -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        case ReplacementCandidate.controller candidate of
          -- CR 109.5 has no answer: a permanent-sourced instance whose source
          -- has left the board. Defensive, with no producer today, and it leaves
          -- the entry alone rather than guessing at a player.
          Nothing -> pure (Just event)
          Just you -> do
            State.modify' $ \gs ->
              let claim obj = obj {Object.enteredUnder = Just you}
               in gs {GameState.objects = Map.adjust claim oid (GameState.objects gs)}
            pure (Just event)
      -- CR 614.1c: Shimatsu the Bloodcloaked. "As this creature enters, sacrifice
      -- any number of permanents. This creature enters with that many +1/+1
      -- counters on it." -- one rewrite, because the count the second sentence
      -- uses is the answer to the first.
      --
      -- Wood Elemental is the same rewrite reading its count somewhere else: "As
      -- this creature enters, sacrifice any number of untapped Forests. Wood
      -- Elemental's power and toughness are each equal to the number of Forests
      -- sacrificed as it entered." No counters, so `kind` is Nothing and the count
      -- is left for the card's characteristic-defining ability (CR 208.2a) to read
      -- back out of Binding.sacrificedCount.
      --
      -- The only entry arm that PERFORMS a game action rather than stamping a
      -- value, which is why this module and not Pawl.Engine.Replacement holds
      -- `apply`: the sacrifice is `sacrifice` below, CR 701.21a's one funnel, and
      -- the counters go through addEnteringCounters into the pending map, so CR
      -- 614.16 applies to them in this same loop. Both are the ordinary doors, so
      -- Rest in Peace redirects a sacrificed permanent and Doubling Season sees
      -- these counters as it sees riot's, with nothing written here to make
      -- either happen.
      --
      -- THE ENTERING OBJECT IS NOT A CANDIDATE, nor is a permanent entering
      -- beside it. CR 614.13a states it outright -- "you can't choose the object
      -- that will become that permanent or any other object entering the
      -- battlefield at the same time as that object" -- and that rule reaches
      -- this arm and not the copy arm beside it, because a sacrificed permanent
      -- CHANGES ZONES and a copy target does not (see applyReplacementsIn). This
      -- engine materializes the entering object before running the entry loop
      -- (see runEntry), so `sacrifice` would otherwise happily take it: the
      -- exclusion has to be written, not inherited.
      --
      -- CR 614.12b's COMBINED BUDGET across permanents entering simultaneously
      -- needs no check of its own, and this is where that falls out. The choice
      -- is paid for here, inside the entry loop, before the next member of the
      -- batch runs its own (createTokens' mapM_ over runEntry); the offer above
      -- is re-derived from the live board; so what an earlier choice spent is
      -- not there for a later one to choose again, which is CR 614.13b. No joint
      -- answer the rule allows is lost either -- the player answers each prompt
      -- as it comes, so any partition of the supply among the batch is reachable
      -- in this fixed order, and the rule only ever FORBIDS choices.
      --
      -- Kicked Rite of Replication on a Wood Elemental proves it
      -- (Pawl.ReplacementSpec, "One budget for simultaneous entry costs"): five
      -- token copies, one supply of three Forests. `many` counts what was
      -- CHOSEN, so paying any later than this leaves five 3/3s instead of one.
      --
      -- The argument rests on every entry cost in the pool being "any number",
      -- which is never unpayable. An entry cost whose amount is fixed by the
      -- choice and CAN be unpayable (Frankenstein's Monster's X) would need the
      -- forward check the rule literally describes; no EntryRewrite arm carries
      -- one (#1395).
      EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber criterion kind) -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Sacrifices nothing rather than
          -- guessing at a player, and so places no counters and records no count.
          Nothing -> pure (Just event)
          Just controller -> do
            -- The offer narrows the WHOLE battlefield by a Filter, so it is also
            -- where CR 614.12's other half bites: a bystander is judged with the
            -- batch's static abilities suppressed (GameState.enteringBeside),
            -- which is channel 3 of applyReplacementsIn's note and not this
            -- exclusion.
            let entering oid2 = oid2 == oid || Set.member oid2 batch
                offered = filter (not . entering) (Replacement.sacrificeCandidates Map.empty controller (Just oid) criterion gs)
            chosen <-
              -- Where the rules leave nothing to ask, don't prompt: with no
              -- candidate the empty set is the only answer. ONE candidate is
              -- still asked, unlike Prompt.ChooseSacrifices' elision -- "any
              -- number" leaves two distinguishable answers there.
              if null offered
                then pure Set.empty
                else do
                  let decider = Decide.deciderFor controller gs
                  answer <- Game.choose (Prompt.ChooseAnyNumberToSacrifice decider controller oid offered)
                  -- FILTERED, NOT TRUSTED (#222): an answer naming a permanent
                  -- that was never offered would otherwise sacrifice it and pay
                  -- for a counter with it.
                  pure (Set.intersection answer (Set.fromList offered))
            Monad.mapM_ (sacrifice controller) (Set.toAscList chosen)
            -- "That many": the permanents CHOSEN, which is also the permanents
            -- sacrificed -- every member was on the battlefield under this
            -- player's control when it was offered, and nothing between there and
            -- here moves one.
            let many = Natural.length chosen
            -- Recorded on the entering permanent BEFORE the counters, and
            -- unconditionally, so a card that reads the count rather than
            -- spending it on counters has it (Wood Elemental). See
            -- Binding.sacrificedCount for why 0 is recorded rather than left
            -- absent.
            State.modify' $ \gs2 ->
              let note obj = obj {Object.bindings = Map.insert Binding.sacrificedCount (Binding.toAmount many) (Object.bindings obj)}
               in gs2 {GameState.objects = Map.adjust note oid (GameState.objects gs2)}
            Monad.mapM_ (\k -> addEnteringCounters oid k many) kind
            pure (Just event)
      -- CR 702.155b / 714.3b: read ahead's two intrinsic abilities, applied as
      -- one rewrite -- choose a number between one and this Saga's final chapter
      -- number, then enter with that many lore counters.
      --
      -- THE BOUND is CR 714.2d's final chapter number, read off the entering
      -- permanent's own projection rather than off the row: the row is minted
      -- from the projection by Pawl.Engine.Saga.entryReplacementsOf, but a layer
      -- effect could have changed which chapter abilities the Saga has since, and
      -- rule 714.2d asks about the abilities it HAS.
      --
      -- The counters go through addEnteringCounters, so CR 614.16 reaches them in
      -- the entry's own CR 616.1 pool, exactly as it does riot's counter below.
      EntryRewrite.ReadAhead -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        let bound = Saga.finalChapterOf (Projection.project oid gs)
        picked <- case (Projection.controllerOf oid gs, bound) of
          -- CR 714.2d's abilityless Saga: "between one and 0" names an EMPTY
          -- range, so there is nothing to ask and no counter to place. No
          -- printing reaches it -- it needs a read-ahead Saga an effect has
          -- stripped of its chapter abilities -- and this is rule 714.2d read
          -- honestly rather than a question withheld.
          (_, 0) -> pure 0
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Chooses the FLOOR of rule
          -- 702.155b's range rather than conjuring a chapter, which is also
          -- Replay.defaultAnswer's answer.
          (Nothing, _) -> pure 1
          (Just controller, _)
            -- CR 702.155b's range holds one number, so nothing is left to decide
            -- -- EntryRewrite.ChoiceOf's elision, and no player's choice is being
            -- made. At two or more the chapters are distinguishable (entering on
            -- II skips I), so the prompt is owed.
            | bound == 1 -> pure 1
            | otherwise -> do
                let decider = Decide.deciderFor controller gs
                answer <- Game.choose (Prompt.ChooseReadAheadChapter decider controller oid bound)
                -- CLAMPED rather than trusted, Prompt.ChoosePaidEnergy's posture:
                -- rule 702.155b names a closed range and an answerer naming a
                -- chapter outside it would put the Saga past its final chapter
                -- (CR 704.5s) or leave it on none.
                pure (max 1 (min bound answer))
        addEnteringCounters oid CounterKind.Lore picked
        pure (Just event)
      -- CR 702.136a: riot. "You may have this permanent enter with an additional
      -- +1/+1 counter on it. If you don't, it gains haste."
      --
      -- NEVER ELIDED. A +1/+1 counter and haste are two outcomes a player can
      -- tell apart on any board -- the whole reason the keyword exists -- so this
      -- prompt is raised every time the entering object has a controller to ask,
      -- the posture ChooseColor's arm takes and not ChoiceOf's one-option
      -- elision.
      --
      -- The counter goes into the pending map, exactly as the WithCounters arm
      -- above does, so CR 614.16 applies to it in this same loop and Doubling
      -- Season sees riot's counter.
      --
      -- The haste is a STORED continuous effect (CR 611.2) rather than a stamp on
      -- the object: rule 702.136a says the permanent "gains haste" and names no
      -- end, which is CR 611.2a's rest-of-the-game duration, and a stored effect
      -- is what puts the grant in CR 613.1f's layer 6 with a timestamp for
      -- Humility and every other ability-remover to be ordered against. Its
      -- source is the entering permanent itself, the object whose riot ability
      -- generated it (CR 113.7).
      --
      -- The timestamp is a FRESH one, taken here. CR 613.7a would give a static
      -- ability's continuous effect the timestamp of the object the ability is
      -- on, which for this one is the permanent that entered a moment ago and has
      -- the newest object timestamp on the board -- so the two coincide at every
      -- ordering question a card in this pool can ask.
      EntryRewrite.Riot -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Neither half is applied rather
          -- than one being chosen unasked -- the engine makes no player's choice,
          -- and both halves here are choices.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            answer <- Game.choose (Prompt.ChooseRiot decider controller oid)
            case answer of
              OptionalDecision.Exercises -> addEnteringCounters oid CounterKind.PlusOnePlusOne 1
              OptionalDecision.Declines ->
                State.modify' $ \gs2 ->
                  -- CR 611.2a: "gains haste" with no stated end lasts until the
                  -- game does. Armed through Pawl.Engine.Expiry rather than naming
                  -- Expiry.Never here, the posture Resolve's storing arms take;
                  -- Indefinite always arms, so the Nothing branch is unreachable
                  -- and is written out only because arm is total over Duration.
                  -- No bindings: rule 702.136a's riot is a replacement's choice,
                  -- not a resolution, so its duration can name no slot.
                  case Expiry.arm Map.empty controller oid Duration.Indefinite gs2 of
                    Nothing -> gs2
                    Just expiry ->
                      let (ts, gs3) = Game.freshTimestamp gs2
                          eff =
                            ContinuousEffect.MkContinuousEffect
                              { ContinuousEffect.source = oid,
                                ContinuousEffect.timestamp = ts,
                                ContinuousEffect.expiry = expiry,
                                ContinuousEffect.modification = Modification.GainKeyword Keyword.Type.Haste,
                                -- CR 611.2c: a fixed set of one, settled here --
                                -- the permanent that entered, not whatever
                                -- matches a filter later.
                                ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
                              }
                       in gs3 {GameState.continuousEffects = eff : GameState.continuousEffects gs3}
            pure (Just event)
      -- CR 702.98a / 614.1c: unleash. "You may have this permanent enter with an
      -- additional +1/+1 counter on it." Riot's arm with the declining half
      -- deleted: rule 702.98a states no consequence for declining, so Declines
      -- writes nothing.
      --
      -- NEVER ELIDED, for riot's reason: the counter is what turns rule 702.98a's
      -- second static ability on, so the two answers are a bigger creature that
      -- cannot block against a smaller one that can.
      --
      -- Into the pending map, exactly as riot's is, so CR 614.16 applies in this
      -- same loop and Doubling Season sees unleash's counter.
      EntryRewrite.Unleash -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the reason riot's arm gives above.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            answer <- Game.choose (Prompt.ChooseUnleash decider controller oid)
            case answer of
              OptionalDecision.Exercises -> addEnteringCounters oid CounterKind.PlusOnePlusOne 1
              OptionalDecision.Declines -> pure ()
            pure (Just event)
      -- CR 702.54a via CR 614.1c: bloodthirst N on Bloodrage Vampire. The
      -- WithCounters arm above with the kind fixed at +1/+1 by rule 702.54a, and
      -- through addEnteringCounters for that arm's reason -- the pending map is
      -- what makes CR 614.16 reach these in the entry's own CR 616.1 pool, so
      -- Doubling Season sees bloodthirst's counters as it sees riot's.
      --
      -- NO CONDITION HERE. Rule 702.54a's "if an opponent was dealt damage this
      -- turn" is asked by Pawl.Engine.Replacement.admitsEntry, which is why the
      -- row reaching this point already means the condition held; see that
      -- function for why the question is asked there rather than here.
      --
      -- Nothing is rule 702.54b's bloodthirst X, whose count is NOT the row's:
      -- "X is the total damage your opponents have been dealt this turn", read
      -- here rather than at mint time because the mint is handed a keyword and a
      -- count, never a board. That sum is Quantity.DamageDealtToPlayersThisTurn,
      -- the same log rule 702.54a's condition is read off one function over.
      --
      -- CR 109.5's "you" is the ENTERING object's controller, read live off the
      -- board -- `admitsEntry`'s posture for its arm and for its reason, and what
      -- keeps `readsApplier` answering False for this rewrite.
      --
      -- The LIVE board and not Projection.boardAsEntering, unlike the
      -- WithCounters arm above: this quantity folds the event log, which CR
      -- 614.12's "how they apply" does not reach and which no permanent entering
      -- in the same batch can change.
      --
      -- No prompt, and none is owed: neither rule 702.54a nor rule 702.54b states
      -- a choice.
      EntryRewrite.Bloodthirst n -> do
        gs <- State.get
        let count = case n of
              Just printed -> printed
              Nothing ->
                let context = Filter.contextFor (Game.teams gs) (Projection.controllerOf oid gs) (Just oid)
                    quantity = Quantity.Type.DamageDealtToPlayersThisTurn (PlayerRef.Relative PlayerRelation.Opponent)
                 in maybe 0 Integer.toNaturalSaturating (Quantity.evaluate (Projection.fullView gs) context gs oid quantity)
        Replacement.consume (ReplacementCandidate.identity candidate)
        addEnteringCounters oid CounterKind.PlusOnePlusOne count
        pure (Just event)
      -- CR 702.150a via CR 614.1c: compleated on Tamiyo, Compleated Sage. "It
      -- instead enters the battlefield with that many loyalty counters MINUS TWO
      -- FOR EACH OF THOSE MANA SYMBOLS" -- the payload is the symbol count, so the
      -- two is rule 702.150a's and appears only here.
      --
      -- A SUBTRACTION FROM THE PENDING MAP, not a removal from Object.counters:
      -- rule 702.150a removes nothing, it changes how many arrive, so nothing
      -- keyed on counter removal may see this. Sitting in the pending map is also
      -- what puts the row in the entry's own CR 616.1 pool beside CR 614.16's
      -- multipliers, which is the whole of #1996 -- the orders reach 6 and 8 on
      -- Tamiyo under a Doubling Season.
      --
      -- Map.adjust rather than insert, for the CounterR arm's reason: an absent
      -- id means nothing is entering, which `admitsEntry` has already ruled out.
      --
      -- Saturating, because a counter count is a Natural and CR 122.1 knows no
      -- negative number of them. Rule 702.150a's own "would enter with one or more
      -- loyalty counters" -- asked in `admitsEntry` -- is what keeps the floor out
      -- of reach on a printed card anyway.
      --
      -- NO CONDITION HERE. Rule 702.150a's two are asked before this point: "one
      -- or more loyalty counters" in Pawl.Engine.Replacement.admitsEntry, and
      -- "chose to pay life" where the row is minted.
      --
      -- No prompt, and none is owed: rule 702.150a states no choice.
      EntryRewrite.Compleated symbols -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \gs ->
          gs
            { GameState.enteringCounters =
                Map.adjust (Map.adjust (\n -> Natural.minusSaturating n (2 * symbols)) CounterKind.Loyalty) oid (GameState.enteringCounters gs)
            }
        pure (Just event)
      -- CR 614.1d / 110.5b: "This permanent enters tapped" (Zof Bloodbog's land,
      -- Headless Skaab's creature -- the arm gates on no card type). CR 110.5b
      -- has a permanent enter untapped "unless a spell or ability says otherwise",
      -- and this is the permanent's own ability saying otherwise -- which is why
      -- it is here and not in EntryRiders, the rider a SPELL or ability writes
      -- when it puts something onto the battlefield tapped. A land played as CR
      -- 305.1's special action passes through no effect at all, so only this route
      -- reaches it.
      --
      -- ENTERS TAPPED, not "enters, then is tapped", and the difference is the
      -- whole point of the arm: the status is stamped straight onto the object
      -- rather than routed through the tap funnel, so the permanent never
      -- transitions from untapped to tapped and nothing watching for that can
      -- fire -- which is CR 603.2e's own sentence about a permanent that enters in
      -- that state. Stamping the ALREADY-MATERIALIZED incarnation is observationally the same
      -- as minting it tapped, on UnderSourceControl's footing above: runEntry
      -- finishes before the Moved event is recorded, so no trigger scan and no
      -- state-based action can see the interim object.
      --
      -- No prompt, and none is owed: rule 614.1d's rewrite has no choice in it.
      EntryRewrite.Tapped -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        enterTapped oid
        pure (Just event)
      -- CR 614.1c with CR 119.4: "As this land enters, you may pay N life. If you
      -- don't, it enters tapped" (Razorgrass Field). The arm above's write, with a
      -- price on avoiding it -- so declining here leaves exactly the board Zof
      -- Bloodbog's unconditional sentence leaves, down to the same stamp.
      --
      -- NEVER ELIDED where the payment is possible. Life against an untapped land
      -- is a real fork on any board -- it is why the cycle is printed -- so the
      -- prompt is raised every time the entering object has a controller who can
      -- afford it.
      --
      -- Through payLife, CR 119.4's own door, and NOT a subtraction from the life
      -- total: rule 119.4's last clause makes the payment a life loss like any
      -- other, so a card watching for life loss sees this one.
      EntryRewrite.PayLifeOrTapped n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so controllerOf
          -- falls back to its owner. Tapped rather than untapped, because with
          -- nobody to ask nobody paid -- which is the card's own stated default,
          -- "if you don't, it enters tapped".
          Nothing -> do
            enterTapped oid
            pure (Just event)
          Just controller -> do
            -- CR 119.4: a player may pay N life only if their life total is at
            -- least N. Below that, declining is the only legal answer -- a forced
            -- selection, not an elision of options a player could tell apart --
            -- so the prompt is skipped rather than asked and overruled. CR 119.4b
            -- keeps 0 payable at any total, so a zero amount is still asked.
            answer <-
              if canPayLife controller n gs
                then Game.choose (Prompt.ChoosePayLifeOnEntry (Decide.deciderFor controller gs) controller oid n)
                else pure OptionalDecision.Declines
            case answer of
              OptionalDecision.Exercises -> payLife controller n
              OptionalDecision.Declines -> enterTapped oid
            pure (Just event)
      -- CR 614.1c with CR 701.20a: "As this land enters, you may reveal a Kithkin
      -- card from your hand. If you don't, this land enters tapped" (Rustic
      -- Clachan). The arm above with a different price -- showing a card instead
      -- of spending life -- and the same declining half, down to the same stamp.
      --
      -- Through `reveal`, CR 701.20a's own funnel, so the shown card reaches the
      -- public log with the projection a player at the table would see. Nothing
      -- moves and nothing changes (CR 701.20b), which is why the paying half is
      -- the reveal alone: this is not a cost, so no CR 118 payment and no
      -- rollback is involved.
      --
      -- NEVER ELIDED where a matching card is held. Showing a card nobody could
      -- have made you show, against a land that comes in tapped, is a real fork on
      -- any board -- it is why the cycle is printed.
      EntryRewrite.RevealOrTapped filter_ -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arm above's reason: the object is
          -- materialized on the battlefield before this loop runs, so controllerOf
          -- falls back to its owner. Tapped rather than untapped, because with
          -- nobody to ask nobody revealed -- the card's own stated default.
          Nothing -> do
            enterTapped oid
            pure (Just event)
          Just controller -> do
            -- The hand is read HERE, at CR 614.12a's moment, and not off any
            -- earlier snapshot: an entry replacement applied before this one can
            -- have moved a card (CR 614.13), and the offer must be what the
            -- player actually holds as the choice is made.
            let candidates = Replacement.revealableFromHand controller filter_ gs
            answer <- case NonEmpty.nonEmpty candidates of
              -- Holding nothing that matches, declining is the only legal answer
              -- -- a forced selection rather than an elision of options a player
              -- could tell apart -- so the prompt is skipped rather than asked and
              -- overruled.
              Nothing -> pure Nothing
              Just offered -> Game.choose (Prompt.ChooseRevealOnEntry (Decide.deciderFor controller gs) controller oid offered)
            -- FILTERED, NOT TRUSTED, AsCopy's posture above: this list is the only
            -- thing enforcing the printed criterion, so honouring an unoffered
            -- answer would let any card in hand keep the land untapped. A
            -- REGRESSION FENCE rather than proven behaviour -- the offer is the
            -- only thing an ordinary game answers from, so it takes a transcript
            -- naming a card that was never offered to reach the refusal.
            case answer of
              Just shown | List.elem shown candidates -> reveal RevealCause.Ordinary controller shown
              _ -> enterTapped oid
            pure (Just event)
      -- CR 702.145b's first static ability: "if it is night and this permanent
      -- is represented by a double-faced card, it enters transformed." The one
      -- producer CR 616.1d's bucket has, and CR 616.1d names no origin zone, so
      -- this arm runs on an entry from any zone -- not CR 712.13a's, which is the
      -- resolving-spell road alone.
      --
      -- WHICH face is Card.backFace, the same answer CR 712.14a's rider gets in
      -- changeZoneEntering: the card is read, never cased on.
      --
      -- Object.face and NOT Game.turnFaceOver, which is the write for CR 701.27a's
      -- transform: nothing turned over here. So Object.turnedOverAt stays Nothing,
      -- and CR 701.27f's "has already transformed since" (Pawl.Engine.Resolve's
      -- alreadyTurnedFor) still reads the permanent as one that has not.
      --
      -- Stamped on the ALREADY-MATERIALIZED incarnation, Tapped's footing above:
      -- runEntry finishes before the Moved event is recorded, so no trigger scan
      -- and no state-based action can see the interim front face.
      --
      -- Not implemented: CR 712.13a's second sentence, an instant or sorcery back
      -- face sending the spell to its owner's graveyard rather than the
      -- battlefield. The write is unguarded, where Game.turnFaceOver goes through
      -- Card.turnedOver and CR 701.27d's refusal with it, so such a face would be
      -- shown on the battlefield rather than merely fail to reach the graveyard
      -- (#1547).
      EntryRewrite.EntersTransformed -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case fmap Face.name (Game.cardOf oid gs >>= Card.backFace) of
          -- Unreachable: Replacement.admitsEntry admits this rewrite only where
          -- Card.backFace answers. Defensive: leave the front face up.
          Nothing -> pure (Just event)
          Just name -> do
            State.modify' $ \g ->
              g {GameState.objects = Map.adjust (\obj -> obj {Object.face = Just name}) oid (GameState.objects g)}
            pure (Just event)
      -- CR 614.1c: "As [this permanent] enters, [do something]" -- Monstrous
      -- War-Leech's "mill four cards". The one entry rewrite that runs an EFFECT
      -- rather than changing what the permanent is; see #1416.
      --
      -- QUEUED, not run here, and the module boundary is the reason: this module
      -- is below Pawl.Engine.Resolve and cannot run a card's effects, exactly as
      -- Pawl.Engine.Damage cannot run CR 615.5's rider. So the effects go onto
      -- GameState.pendingEntryEffects with the environment they need and
      -- Resolve.runEntryEffects performs them; see that field for where it drains
      -- and what the deferral costs.
      --
      -- CR 109.5's "you" is the ENTERING object's controller, read live off the
      -- board rather than off the candidate -- Bloodthirst's and
      -- SacrificeAnyNumber's posture, and what makes `readsApplier` answer False
      -- for this arm. Read NOW rather than at the drain, because a later rewrite
      -- in the same CR 616.1 loop may hand the permanent to someone else
      -- (UnderSourceControl).
      --
      -- NO CONDITION asked here. Rule 702.54a's is Bloodthirst's and lives in
      -- Replacement.admitsEntry; this arm's producer states its own on CR 604.2's
      -- clause, which Projection.replacementsOf asked before the row was
      -- collected.
      EntryRewrite.RunEffects effects -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the reason riot's arm gives above: the
          -- object is materialized on the battlefield before runEntry, so
          -- controllerOf falls back to its owner. Runs nothing rather than
          -- picking a performer.
          Nothing -> pure (Just event)
          Just controller -> do
            State.modify' $ \g ->
              g
                { GameState.pendingEntryEffects =
                    GameState.pendingEntryEffects g
                      Seq.|> PendingEntryEffect.MkPendingEntryEffect
                        { PendingEntryEffect.object = oid,
                          PendingEntryEffect.controller = controller,
                          PendingEntryEffect.effects = effects
                        }
                }
            pure (Just event)
    -- Unreachable: `applies` admits EntryR only against WouldEnter.
    (ReplacementEffect.EntryR {}, _) -> pure (Just event)
    (ReplacementEffect.DamageR damageR@(DamageR.MkDamageR _ rewrite _), ProposedEvent.WouldDealDamage de) -> case rewrite of
      -- CR 615.6: a prevented event never happens -- it is not marked, not
      -- drained, and never recorded, so no deathtouch bit exists for the CR
      -- 704.5h SBA to read.
      DamageRewrite.PreventAll -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure Nothing
      -- CR 122.1c: "prevent that damage and remove a shield counter from it". The
      -- WHOLE event goes, whatever its amount -- "if a permanent that would be
      -- dealt damage has more than one shield counter on it, that damage is
      -- prevented and only one shield counter is removed" -- so this is
      -- PreventAll's arm plus the removal, and never PreventNext's arithmetic.
      --
      -- The counter comes off the DAMAGED permanent, which is this candidate's own
      -- source: Replacement.admitsRecipient admits the event only when the two are
      -- the same object.
      --
      -- NOT `consume`, for the reason `collect` gives: a permanent's candidate has
      -- no use to spend. The removal is what spends this one instead, since
      -- Projection.shieldOf mints the pair only while a counter is there and CR
      -- 616.1f re-collects.
      DamageRewrite.PreventRemovingShieldCounter -> do
        removeCounters (ReplacementCandidate.source candidate) CounterKind.Shield 1
        pure Nothing
      -- CR 615.7's shield covers as much of THIS event as it has left, and
      -- whatever it could not cover survives as a smaller event of the same
      -- source, recipient and riders. Nothing when it covered all of it, which
      -- is CR 615.6: a prevented event never happens.
      --
      -- No choice is made here, and none is owed: within one event CR 615.7
      -- leaves nothing to decide, since the prevention is neither optional nor
      -- divisible by anyone's say-so. The choice the rule DOES describe -- which
      -- of several simultaneous events the shield covers -- is asked one level
      -- up, in resolveDamageBatch.
      --
      -- NOT `consume`. That spends a row per APPLICATION, while CR 615.7's unit
      -- is the amount of damage rather than the number of events or sources
      -- dealing it. `setShield` writes the remainder back and drops the row at 0.
      DamageRewrite.PreventNext remaining -> do
        let amount = DamageEvent.amount de
            -- Both subtractions below are total on Natural: `prevented` is a min
            -- of the two operands, so it is no greater than either.
            prevented = min remaining amount
        Replacement.setShield (ReplacementCandidate.identity candidate) damageR (DamageRewrite.PreventNext (remaining - prevented))
        if prevented >= amount
          then pure Nothing
          else pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = amount - prevented}))
      -- CR 615.10's static shield with an amount that SURVIVES rather than one
      -- that is stopped -- Temple Altisaur's "prevent all but 1 of that damage".
      -- The event shrinks to the smaller of the two, so an event already at or
      -- under the floor is handed back untouched and `preventionBy` reports no
      -- prevention of it, which is CR 615.13's own condition.
      --
      -- No `setShield` and no arithmetic written back: CR 615.10's shields are
      -- deliberately not reduced, applying separately to each event.
      DamageRewrite.PreventAllBut surviving -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        let left = min surviving (DamageEvent.amount de)
        if left == 0
          then pure Nothing
          else pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = left}))
      -- CR 614.1a's "instead" with a flat amount (Galvanic Blast). Only the
      -- AMOUNT is rewritten, and that is the rule rather than economy: a
      -- replaced damage event keeps its source, its recipient and every
      -- deal-time rider it was proposed with.
      DamageRewrite.SetAmount n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = n}))
      -- CR 614.1a: Furnace of Rath's "it deals double that damage ... instead".
      -- Through the same `scale` the counter and token rewrites use, so a
      -- doubling means one thing across every event class.
      DamageRewrite.Scale scaling -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = Replacement.scale scaling (DamageEvent.amount de)}))
      -- CR 614.9: a redirection effect replaces the damage's RECIPIENT with
      -- another and nothing else -- "the same damage dealt to another battle,
      -- creature, planeswalker, or player". Every other field rides along for
      -- SetAmount's reason: a replaced damage event keeps its source, its amount
      -- and every deal-time rider it was proposed with.
      --
      -- The rule's guard says the effect DOES NOTHING, not that it is
      -- inapplicable -- the CR 615.12 distinction `inertPrevention` already
      -- draws -- so a dead destination consumes the candidate, is marked applied
      -- by the loop, and hands the event straight back. Returning Nothing here
      -- would DROP the damage, which is weaker than printed: the rule leaves it
      -- on its original recipient.
      DamageRewrite.Redirect dest -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        pure . Just $ case Replacement.redirectDestination gs dest of
          Nothing -> event
          Just live -> ProposedEvent.WouldDealDamage de {DamageEvent.target = live}
      -- The arm above with a countdown: Harm's Way moves as much of THIS event
      -- as it has left and writes the rest back, PreventNext's CR 615.7
      -- arithmetic borrowed onto a rule-614 rewrite (that rule governs only
      -- preventions; the redirect's rests on Harm's Way's rulings). The event
      -- arriving here never exceeds
      -- the remainder -- `loop` split anything larger on
      -- Replacement.partialCoverage and sent the residue on as an event of its
      -- own -- so the whole of it moves, and `min` is the guard for the one
      -- route that bypasses the split, a destination CR 614.9's guard has
      -- retired: that one hands the event back unchanged and spends nothing,
      -- CR 609.7b's "if for any reason the shield ... replaces no damage, the
      -- shield isn't used up".
      --
      -- NOT `consume`, for PreventNext's reason: the row is spent in damage
      -- rather than per application, and `setShield` drops it at 0.
      DamageRewrite.RedirectNext remaining dest -> do
        gs <- State.get
        case Replacement.redirectDestination gs dest of
          Nothing -> pure (Just event)
          Just live -> do
            let moved = min remaining (DamageEvent.amount de)
            Replacement.setShield (ReplacementCandidate.identity candidate) damageR (DamageRewrite.RedirectNext (remaining - moved) dest)
            pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = moved, DamageEvent.target = live}))
      -- CR 614.9 with the destination PRINTED rather than baked -- Pariah's
      -- "all damage that would be dealt to you is dealt to enchanted creature
      -- instead". The arm above with `dest` found by description instead of read
      -- off the row, so the rule's guard, the CR 613.1d re-tag and the
      -- unchanged-event answer are all the same three lines
      -- (Replacement.printedDestination).
      --
      -- The Filter is read in the CANDIDATE's Context, which is what makes CR
      -- 303.4b's "enchanted" answerable: the row's source is the Aura, and
      -- Replacement.candidateContext supplies what it is attached to.
      DamageRewrite.RedirectMatching filter_ -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        pure . Just $ case Replacement.printedDestination gs (Replacement.candidateContext gs candidate) filter_ of
          Nothing -> event
          Just live -> ProposedEvent.WouldDealDamage de {DamageEvent.target = live}
    -- Unreachable: `applies` admits DamageR only against WouldDealDamage.
    (ReplacementEffect.DamageR {}, _) -> pure (Just event)
    -- CR 701.19a / 122.1c: under either arm the DESTRUCTION does not happen, so
    -- nothing downstream of it (a put-into-graveyard, and therefore Rest in Peace's
    -- redirect) ever runs. What each does INSTEAD is all that separates them, and
    -- the two do not overlap: regeneration removes marked damage, taps the
    -- permanent and removes it from combat, where a shield counter's removal does
    -- none of those.
    (ReplacementEffect.DestructionR rewrite, ProposedEvent.WouldBeDestroyed oid _ _) -> case rewrite of
      DestructionRewrite.Regenerate -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        -- Rule 701.19a's three instructions in the order it prints them: remove
        -- all marked damage, tap it, then remove it from combat. The ORDER is
        -- observable now that the middle one records an event -- a trigger
        -- gathered off the tap reads the board as it stands, and doing the combat
        -- removal first would show it a creature already out of combat.
        --
        -- Three statements rather than one write because `tap` is a Game action;
        -- the funnel is what makes a regeneration a becomes-tapped event like any
        -- other route.
        State.modify' (\gs -> gs {GameState.objects = Map.adjust (\obj -> obj {Object.damage = 0}) oid (GameState.objects gs)})
        tap oid
        State.modify' (Game.removeFromCombat oid)
        pure Nothing
      -- CR 122.1c: "instead remove a shield counter from it". The destruction does
      -- not happen, and NONE of regeneration's own work above does either --
      -- "removing a shield counter in this way isn't the same as regenerating a
      -- creature", so the permanent keeps its marked damage, its tap state and its
      -- place in combat.
      --
      -- The counter comes off `oid`, which Replacement.applies has already made
      -- this candidate's own source. `consume` is skipped for the damage arm's
      -- reason.
      DestructionRewrite.RemoveShieldCounter -> do
        removeCounters oid CounterKind.Shield 1
        pure Nothing
    -- Unreachable: `applies` admits DestructionR only against WouldBeDestroyed.
    (ReplacementEffect.DestructionR _, _) -> pure (Just event)
    -- CR 122.1d: "instead remove a stun counter from it". The untap does not
    -- happen and nothing else does either -- the permanent keeps its tap state,
    -- and rule 122.1d gives it none of regeneration's own work.
    --
    -- The counter comes off `oid`, which Replacement.applies has already made
    -- this candidate's own source. `consume` is skipped for the shield arm's
    -- reason: the counter IS the resource, so spending the row twice off one
    -- counter is what removing the counter already prevents.
    (ReplacementEffect.UntapR rewrite, ProposedEvent.WouldUntap oid) -> case rewrite of
      UntapRewrite.RemoveStunCounter -> do
        removeCounters oid CounterKind.Stun 1
        pure Nothing
    -- Unreachable: `applies` admits UntapR only against WouldUntap.
    (ReplacementEffect.UntapR _, _) -> pure (Just event)
    -- CR 614.1a: the RESIZING arms leave the event standing at a rewritten loss,
    -- which is what makes them composable: CR 616.2's next iteration re-collects
    -- against the rewritten loss, and a second row can act on it again. The last
    -- arm cancels instead, and is the exception the two above are read against.
    --
    -- No arm here touches the DAMAGE, on the CR 120.4c road. By CR 120.4b it has
    -- already been dealt, and Pawl.Engine.Damage.applyDamage still gains a
    -- lifelink source's controller every point of it -- "any damage rendered
    -- useless by Worship was still dealt".
    (ReplacementEffect.LifeLossR (LifeLossR.MkLifeLossR _ rewrite), ProposedEvent.WouldLoseLife cause pid n) -> case rewrite of
      -- CR 120.4c: Worship's "reduces it to 1 instead". The rewrite names the
      -- resulting TOTAL and this arm converts it into the event's currency,
      -- reading the player's life LIVE. That is the whole reason the field is a
      -- floor rather than an amount -- Worship's own ruling insists on it: "It
      -- reduces your life total to 1, not the damage to 1."
      --
      -- Saturating at zero covers a player already at or below the floor, whom CR
      -- 704.5a has not yet swept: they lose nothing at all.
      --
      -- The proposed amount is not read, and cannot be needed: Replacement.breaches
      -- has already refused every event this arm would not shrink.
      LifeLossRewrite.LeaveAtLeast floor_ -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        let life = maybe 0 Player.life (Map.lookup pid (GameState.players gs))
        pure (Just (ProposedEvent.WouldLoseLife cause pid (Integer.toNaturalSaturating (life - toInteger floor_))))
      -- CR 614.1a: Bloodletter of Aclazotz' "they lose twice that much life
      -- instead". The proposed amount IS the whole input here, where the arm above
      -- reads the board: a scaling is stated on the loss and not on the total.
      --
      -- Pawl.Engine.Replacement.scale is the shared arithmetic, the one CounterR
      -- uses -- a resized life loss and a resized counter placement differ in what
      -- they resize, not in how.
      LifeLossRewrite.Scaled scaling -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldLoseLife cause pid (Replacement.scale scaling n)))
      -- CR 614.6 with CR 119.4: Ashiok, Wicked Manipulator's "exile that many
      -- cards from the top of your library instead". THE ONE ARM THAT CANCELS:
      -- rule 614.6's "if an event is replaced, it never happens", so no life is
      -- lost, no CR 119.4 subtraction is made and no GameEvent.LifeLost is
      -- recorded -- Nothing, not a loss rewritten to nothing. What the payment
      -- COST is unchanged: `canPayLife` still refuses an amount the payer's life
      -- total cannot cover, which is the card's own ruling ("Ashiok's first
      -- ability doesn't allow you to attempt to pay an amount of life greater
      -- than your current life total").
      --
      -- The cards come off CR 109.5's "your" library -- the CANDIDATE's
      -- controller, not the player losing the life, which is why
      -- Replacement.readsApplier answers True for this rewrite alone.
      --
      -- The ids are snapshotted from the pre-move board and then moved one at a
      -- time, `drawCardReturning`'s shape: Game.zoneMembers is top-first, so
      -- `take` off the front is the top `n`, and each move is CR 400.7's funnel
      -- with every replacement of its own. Replacement.breaches has already
      -- refused a library too short, so the take is never partial when it starts
      -- -- and if an intervening move shortened it, exiling what is left is CR
      -- 614.6's own "instructions that can't be carried out ... simply ignored".
      LifeLossRewrite.ExileFromTopOfYourLibrary -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case ReplacementCandidate.controller candidate of
          -- Unreachable: Replacement.breaches refuses a row whose "you" names
          -- nobody, there being no library its clause could measure.
          Nothing -> pure (Just event)
          Just you -> do
            Monad.mapM_ (`changeZone` Zone.Exile) (take (Natural.toIntSaturating n) (Game.zoneMembers Zone.Library you gs))
            pure Nothing
    -- Unreachable: `applies` admits LifeLossR only against WouldLoseLife.
    (ReplacementEffect.LifeLossR {}, _) -> pure (Just event)
    -- CR 614.1a: Boon Reflection's "you gain twice that much life instead". The
    -- event is left STANDING at a rewritten gain rather than cancelled, the
    -- resizing loss arms above for their reason: CR 616.2's next iteration
    -- re-collects against it, so a second Boon Reflection quadruples.
    (ReplacementEffect.LifeGainR (LifeGainR.MkLifeGainR _ rewrite), ProposedEvent.WouldGainLife pid n) -> case rewrite of
      -- The proposed amount IS the whole input: a scaling is stated on the gain
      -- and not on the resulting total, so nothing is read off the board.
      -- Pawl.Engine.Replacement.scale is the shared arithmetic, LifeLossRewrite's
      -- Scaled arm's.
      LifeGainRewrite.Scaled scaling -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldGainLife pid (Replacement.scale scaling n)))
    -- Unreachable: `applies` admits LifeGainR only against WouldGainLife.
    (ReplacementEffect.LifeGainR {}, _) -> pure (Just event)
    -- CR 614.6 with CR 614.11: Words of Worship's "the next time you would draw a
    -- card this turn, you gain 5 life instead". The draw NEVER HAPPENS -- no card
    -- leaves the library, no GameEvent.Drew is recorded and CR 121.2's tally does
    -- not move -- so this arm cancels rather than rewriting.
    --
    -- Rule 614.11's first sentence is why the funnel proposes the event before it
    -- looks at the library: the row applies "even if no cards could be drawn
    -- because there are no cards in the affected player's library", so a player
    -- drawing off an empty library gains the life and never attempts the draw CR
    -- 121.4 and CR 704.5b would kill them for.
    --
    -- The life goes to the player the EVENT named, which for the producer in the
    -- pool is the same seat as CR 109.5's "you"; see Pawl.Types.DrawRewrite.
    (ReplacementEffect.DrawR (DrawR.MkDrawR _ rewrite), ProposedEvent.WouldDraw pid) -> case rewrite of
      -- Through resolveLifeGain, CR 614.1's funnel for the gain class: the life
      -- this rewrite substitutes for the draw is a life gain event like any
      -- other, so a LifeGainR row resizes it.
      DrawRewrite.GainLife n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        settled <- resolveLifeGain pid n
        changeLife pid (toInteger settled)
        pure Nothing
      -- CR 400.11c through the same CR 614.6 cancellation: Ring of Ma'rûf's
      -- "instead put a card you own from outside the game into your hand". The
      -- card is minted rather than drawn, so the library is untouched here too.
      --
      -- `bringInto` above is the whole of the road in, the same function
      -- Pawl.Engine.Resolve's Effect.FromOutsideTheGame arm calls, so the two
      -- roads cannot drift about what a wish may reach.
      --
      -- The SOURCE handed to it is the candidate's, CR 113.7's source of the
      -- ability that installed the row; for the producer that is the artifact its
      -- own activation cost exiled, and nothing about a card in exile stops a
      -- Filter.Context naming it.
      DrawRewrite.FromOutsideTheGame payload -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        bringInto payload (ReplacementCandidate.source candidate) pid
        pure Nothing
    -- Unreachable: `applies` admits DrawR only against WouldDraw.
    (ReplacementEffect.DrawR {}, _) -> pure (Just event)
    -- CR 121.2a with CR 614.6: Alms Collector's "if an opponent would draw two or
    -- more cards, instead you and that player each draw a card". The INSTRUCTION is
    -- replaced outright rather than resized, so this arm cancels and performs the
    -- two draws itself.
    --
    -- Cancelling is what CR 121.6c asks for as well as CR 614.6: a draw effect's
    -- additional action ("draw three cards, then reveal them") is not performed on
    -- cards drawn as a result of the replacement, and Pawl.Engine.Resolve binds its
    -- slot from what the instruction returns -- nothing, once the event is gone.
    --
    -- CR 121.2c orders the two draws: the active player performs all of theirs
    -- first, then each other player in turn order. `apnapOrder` supplies the order
    -- and the filter the membership, Resolve's Effect.Draw arm's shape. A row whose
    -- controller has left the game draws nobody the extra card and the instructed
    -- player still draws one, which is CR 614.6's "instructions that can't be
    -- carried out are simply ignored"; the same filter collapses the two to one
    -- draw where the row's controller IS the instructed player, which the pool's
    -- one producer never is -- its pattern is ControllerRelation.Opponents.
    --
    -- Each of these is an individual draw through `drawCard`, so CR 616.1g's inner
    -- events still raise their own WouldDraw and a per-draw row (Words of Worship)
    -- still applies to them.
    (ReplacementEffect.DrawCountR (DrawCountR.MkDrawCountR _ _ rewrite), ProposedEvent.WouldDrawCards pid _) -> case rewrite of
      DrawCountRewrite.EachDrawOne -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        let drawers = filter (\p -> p == pid || Just p == ReplacementCandidate.controller candidate) (Game.apnapOrder gs)
        Monad.mapM_ drawCard drawers
        pure Nothing
    -- Unreachable: `applies` admits DrawCountR only against WouldDrawCards.
    (ReplacementEffect.DrawCountR {}, _) -> pure (Just event)
    -- CR 122.6/614.1: Hardened Scales/Doubling Season scale a counter placement.
    (ReplacementEffect.CounterR (CounterR.MkCounterR _ scaling), ProposedEvent.WouldPutCounters cause oid kind n) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldPutCounters cause oid kind (Replacement.scale scaling n)))
    -- CR 122.1: the same rewrite against a player -- Vorinclex, Monstrous Raider's
    -- "on a permanent or player". Through the same `scale`, so the two recipients
    -- cannot disagree about what doubling or halving means.
    --
    -- The event SURVIVES at a scaled count of zero rather than being cancelled
    -- here, so CR 616.2's next iteration still sees it and CR 614.5 still spends
    -- this row; putPlayerCounters is what declines to write a zero.
    (ReplacementEffect.CounterR (CounterR.MkCounterR _ scaling), ProposedEvent.WouldPutPlayerCounters cause pid kind n) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldPutPlayerCounters cause pid kind (Replacement.scale scaling n)))
    -- CR 614.16 at the ENTRY level: the counters this permanent is entering with,
    -- scaled where they sit -- in the pending map, before flushEnteringCounters
    -- places them. The same rewrite as the arm above, one level up, which is what
    -- puts this row in the entry's own CR 616.1 pool.
    --
    -- Map.union is left-biased, so the scaled kinds win and a kind the pattern did
    -- not match is left as it was. Map.adjust rather than insert: an absent id
    -- means nothing is entering, which `applies` has already ruled out.
    --
    -- One entry is ONE event -- CR 122.6 covers a permanent given counters as it
    -- enters, and CR 614.12 lets that rider come from a source other than the
    -- permanent itself -- so CR 614.5's "only one opportunity" spends this row
    -- across EVERY pending kind its pattern matches, here, in this one
    -- application. The nested counter-placement loop this arm replaced raised a
    -- WouldPutCounters event per kind and so handed the row an opportunity per
    -- kind: events the rules do not have. Pawl.ReplacementSpec's Perennation
    -- group is the proof -- a permanent returning with a hexproof counter and an
    -- indestructible counter is asked for exactly ONE order between Doubling
    -- Season and an opponent's Vorinclex, and both kinds land on the side that
    -- one order chose.
    --
    -- Two DIFFERENT entry rows feeding one entry are still one event and still
    -- one opportunity apiece. What a row scales is whatever is PENDING when it
    -- is applied, so a row applied before a later entry row has added its kind
    -- never sees that kind, and CR 616.1f offers it no second turn -- which is
    -- the rule's answer, not a shortcut.
    (ReplacementEffect.CounterR (CounterR.MkCounterR pat scaling), ProposedEvent.WouldEnter oid) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      gs <- State.get
      let scaled = fmap (Replacement.scale scaling) (Replacement.matchingEnteringCounters gs pat oid)
      State.modify' $ \gs2 ->
        gs2 {GameState.enteringCounters = Map.adjust (Map.union scaled) oid (GameState.enteringCounters gs2)}
      pure (Just event)
    -- Unreachable: `applies` admits CounterR only against the three counter events
    -- above.
    (ReplacementEffect.CounterR {}, _) -> pure (Just event)
    -- CR 614.16: Doubling Season scales token creation. CR 614.1a: Queen Allenal
    -- of Ruadach's "those tokens plus a 1/1 white Soldier creature token are
    -- created instead" appends a lot to the SAME event -- scaled first, then
    -- appended, so a row applied after this one (CR 616.1) scales the Soldier
    -- too, and the creating effect's riders reach it (Queen Allenal's ruling).
    -- Pawl.ReplacementSpec's Queen Allenal group is the proof, both orders.
    (ReplacementEffect.TokenR (TokenR.MkTokenR _ scaling plus), ProposedEvent.WouldCreateTokens pid lots) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      let scaleLot factor lot = lot {TokenLot.count = Replacement.scale factor (TokenLot.count lot)}
          scaled = maybe lots (\factor -> fmap (scaleLot factor) lots) scaling
          appended = maybe scaled (\card -> scaled Seq.|> TokenLot.MkTokenLot {TokenLot.card = card, TokenLot.copy = Nothing, TokenLot.count = 1}) plus
      pure (Just (ProposedEvent.WouldCreateTokens pid appended))
    -- Unreachable: `applies` admits TokenR only against WouldCreateTokens.
    (ReplacementEffect.TokenR {}, _) -> pure (Just event)
    -- CR 614.1b / 614.10: a skip is "instead of doing X, do nothing", so the
    -- step or phase simply does not begin. Nothing is done first, unlike
    -- DamageRewrite.PreventAll's sibling arm: a skip has no consequence of its
    -- own to perform before it cancels.
    --
    -- The obligation the doc above places on every arm -- case on the inner sum
    -- rather than bind it with `_` -- has nothing to bind here: PhaseR carries a
    -- pattern and no rewrite, because CR 614.1b leaves a skip only one possible
    -- outcome. The day a PhaseRewrite exists, this arm owes it a case.
    --
    -- CR 614.10a's arithmetic -- two skip effects mean two occurrences skipped,
    -- one per instance -- falls out of the floating store's SHAPE rather than
    -- out of care taken here. Two Fatigues prepend two ActiveReplacements, and a
    -- list of instances with distinct timestamps cannot coalesce the way a Set of
    -- patterns or a Boolean flag would; Replacement.consume deletes by (source,
    -- timestamp), so it spends exactly the one that applied; and returning
    -- Nothing ENDS the CR 616.1 loop, so no second skip can be spent on the same
    -- step.
    --
    -- The occurrence skipped is the one the PATTERN named, which for Stonehorn
    -- Dignitary is a whole combat phase rather than a step of one. Nothing here
    -- has to know that: Engine.runStep raises the phase question exactly once per
    -- phase, so a whole-phase skip gets exactly one chance to apply.
    --
    -- Eon Hub's PhaseR reaches the same arm and consumes nothing: it is a
    -- permanent's static ability, so its CandidateId is OfPermanent and `consume`
    -- is a no-op for it. It is the store, not this arm, that tells the two apart.
    (ReplacementEffect.PhaseR _, ProposedEvent.WouldBeginPhase _ _) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure Nothing
    -- Unreachable: `applies` admits PhaseR only against WouldBeginPhase.
    (ReplacementEffect.PhaseR _, _) -> pure (Just event)
    -- CR 614.1e: "as [this permanent] is turned face up, put N counters on it" --
    -- rule 702.37b's megamorph counter and Bubble Smuggler's four +1/+1 counters
    -- alike -- applied WHILE the permanent turns over (CR 708.11) because
    -- FaceDown.performTurnFaceUp raises this event there and nowhere else.
    --
    -- The counters go through putCounters, the CR 122.6 funnel -- so a CR 614.1
    -- counter replacement applies and Hardened Scales sees a megamorph counter
    -- the way it sees a riot one. NOT the pending map the EntryR arms write:
    -- CR 614.1e's turning face up is not an entry, so there is no entry loop
    -- for a row to be ordered in.
    --
    -- The amount is evaluated the same way too, though every row that reaches here
    -- today states its own number: megamorph's Literal 1 and the Smuggler's
    -- Literal 4.
    --
    -- Every kind on the row is placed in this ONE application, the entry arm's
    -- posture (#2314), though both producers write exactly one kind.
    --
    -- The event survives: turning face up is not replaced by the counter, only
    -- accompanied by it, so Just is returned and FaceDown.performTurnFaceUp goes on to
    -- record CR 708.7's event.
    (ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR _ _ rewrite), ProposedEvent.WouldTurnFaceUp oid _) -> case rewrite of
      TurnUpRewrite.WithCounters (WithCounters.MkWithCounters counters) -> do
        gs <- State.get
        let viewOf = Projection.viewWithLastKnown oid gs
            context = Replacement.candidateContext gs candidate
        Replacement.consume (ReplacementCandidate.identity candidate)
        Foldable.for_ (Map.toList counters) $ \(kind, quantity) ->
          case Quantity.evaluate viewOf context gs oid quantity of
            Nothing -> pure ()
            Just n -> Monad.when (n > 0) (Monad.void (putOwnCounters oid kind (Integer.toNaturalSaturating n)))
        pure (Just event)
      -- CR 303.4k with CR 614.1e: Gift of Doom's "as this Aura is turned face
      -- up, you may attach it to a creature", applied WHILE the permanent turns
      -- over (CR 708.11) because FaceDown.performTurnFaceUp raises this event there and
      -- nowhere else -- and, decisively for this rule, AFTER it has written the
      -- face-up status. Every characteristic Attach.turnUpHosts reads is
      -- therefore the Aura's "as it would exist if it were face up"; see there.
      --
      -- CR 303.4k's "an object OR PLAYER" is narrowed to objects here, and by the
      -- rule's own conjunction rather than by this engine: the destinations are
      -- what the card's Filter admits, a Filter matches a battlefield permanent,
      -- and the only printing says "a creature".
      --
      -- The "may" is asked FIRST and separately, since declining and finding no
      -- legal host are different states a transcript must tell apart. Declining
      -- leaves the Aura unattached and CR 704.5m buries it on the next
      -- state-based pass, which is a real and quite bad outcome -- so this is
      -- never elided, the ChooseRiot posture.
      --
      -- The event survives either way: turning face up is not replaced by the
      -- attachment, only accompanied by it, so Just is returned and
      -- FaceDown.performTurnFaceUp goes on to record CR 708.7's event.
      TurnUpRewrite.MayAttachTo filter_ -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the SacrificeAnyNumber arm's reason:
          -- the permanent is on the battlefield, so controllerOf falls back to
          -- its owner. Attaches nothing rather than guessing at a player.
          Nothing -> pure (Just event)
          Just controller -> do
            let hosts = Attach.turnUpHosts controller oid filter_ gs
            -- Where the rules leave nothing to ask, don't prompt: with no legal
            -- host the "may" has no exercisable side.
            Monad.unless (null hosts) $ do
              decision <- Game.choose (Prompt.ChooseTurnUpAttachment (Decide.deciderFor controller gs) controller oid)
              Monad.when (decision == OptionalDecision.Exercises) $ do
                chosen <- Attach.chooseHost controller oid hosts
                Monad.mapM_ (attach oid . Recipient.ToObject) chosen
            pure (Just event)
    -- Unreachable: `applies` admits TurnUpR only against WouldTurnFaceUp.
    (ReplacementEffect.TurnUpR {}, _) -> pure (Just event)

-- CR 707.2 / 202.3b / 202.3c: the copiable values a copy takes off the object it
-- copies. Projection.copiableCharacteristics answers all but one of them; the
-- exception is a mana value, and only when the copied object is a nonmodal
-- double-faced permanent with its back face up, or a melded permanent.
--
-- CR 202.3b's two sentences disagree about that permanent on purpose. Its first
-- calculates the object's OWN mana value "as though it had the mana cost of its
-- front face" -- Pawl.Engine.Card.manaCostFace, which is what
-- copiableCharacteristics already read -- and its second says "if a permanent or
-- spell is a copy of the back face of a nonmodal double-faced object ... the
-- mana value of the copy is 0". So the source and its copy report DIFFERENT
-- numbers, and the copy's cannot be derived from the snapshot alone: the front
-- face's cost is already folded into it and nothing left in the record says
-- which face it came off. The copied OBJECT is where that is knowable, which is
-- why the override lives at the stamp rather than in the projection.
--
-- Everything else in CR 202.3 rides through untouched, including the copy's own
-- printed cost being irrelevant (CR 707.2, the parenthetical's "even if the card
-- representing that copy is itself a double-faced card") -- the snapshot is the
-- COPIED object's throughout, so a Clone that is itself a Transforming card
-- would get 0 here for the same reason a Clone printed {3}{U} does.
--
-- A copy OF A COPY needs no arm: the source's snapshot already carries whatever
-- number this wrote when it entered, and its own card and face are then no
-- longer what it reports.
--
-- CR 202.3b's rule is about "a permanent OR SPELL", and BOTH halves come through
-- here: the entry replacement stamps the permanent's snapshot, and
-- Pawl.Engine.Resolve's CR 707.10 copy stamps the spell's off this same
-- function.
copiedSnapshot :: ObjectId -> GameState -> PC.ProjectedCharacteristics
copiedSnapshot src gs =
  let snapshot = Projection.copiableCharacteristics src gs
      backFace = case (Game.lookupObject src gs, Game.cardOf src gs) of
        (Just obj, Just card) -> Card.showsBackFace card (Object.face obj)
        _ -> False
      -- CR 202.3c's second sentence, the melded twin of CR 202.3b's: "if a
      -- permanent is a copy of a melded permanent (even if that copy is
      -- represented by two other meld cards), the mana value of the copy is 0"
      -- (CR 712.8g again). Same shape as backFace above and for the same reason
      -- -- the copied object's own mana value is the sum CR 202.3c gives it
      -- (Game.manaCostFacesOf), and nothing left in the snapshot says the number
      -- came off two front faces -- so the override is made here, where the
      -- COPIED object is still in hand, rather than in the projection.
      melded = maybe False (not . Seq.null . Game.componentsOf . Object.source) (Game.lookupObject src gs)
   in if backFace || melded then snapshot {PC.manaValue = Just 0} else snapshot

-- CR 608.2h: `copiedSnapshot` for an object that may already be gone -- the
-- record filed as it ceased, which is the same value this function would have
-- returned an instant earlier. Game.cardOfWithLastKnown is the twin fallback for
-- the card behind it, and a copy effect needs both.
--
-- A separate name rather than widening `copiedSnapshot`, for
-- Game.cardOfWithLastKnown's reason: the live reader is asked by the CR 614.1c
-- entry rewrite, whose subject is on the battlefield by construction, and
-- answering there for an object that is not would resurrect it.
copiedSnapshotWithLastKnown :: ObjectId -> GameState -> PC.ProjectedCharacteristics
copiedSnapshotWithLastKnown oid gs = case Projection.lastKnownOf oid gs of
  Just lk -> LastKnown.copiable lk
  Nothing -> copiedSnapshot oid gs

-- CR 614.1c / 614.12: run the entry loop for an object that has just been
-- materialized on the battlefield.
--
-- The object is in GameState.objects and its zone index BEFORE this runs,
-- because CR 614.12 asks for the permanent's characteristics AS IT WOULD EXIST
-- ON THE BATTLEFIELD -- a projection of the object in the state where it has
-- entered, so the cheapest correct implementation is to put it there and project
-- it normally. Nothing observes the interim object: this finishes before the
-- Moved event is recorded, so no trigger scan and no state-based action can see
-- it.
--
-- `Monad.void` discards the `Nothing` that means the event does not happen. Safe
-- here: every EntryR arm always returns `Just`, and only DamageR/DestructionR
-- ever return `Nothing`, neither of which pairs with WouldEnter -- the only
-- event this loop proposes.
--
-- Always the LIVE board (`Nothing`), even when the zone change containing this
-- entry belongs to a CR 608.2f batch: the entering object is not on the
-- pre-batch board at all, and CR 614.12 asks about now rather than about when
-- the containing event began. CR 616.1g recognizes an entry like this as an
-- event CONTAINED within another rather than a second member of the batch, but
-- speaks only to the ORDER the two events' effects are chosen in, not to which
-- board each collects from. That a contained event keeps its own footing is this
-- engine's reading, resting on CR 614.12; no rule states it outright.
runEntry :: Set ObjectId -> ObjectId -> Game ()
runEntry batch oid = do
  -- CR 113.6 / 614.12: for the span of this loop, the batch's OTHER members are
  -- materialized but not entered, so Pawl.Engine.Projection gathers no continuous
  -- effect from their static abilities (see GameState.enteringBeside). The
  -- subject's own are untouched, because `batch` never holds it. Saved and
  -- restored rather than cleared, since an entry rewrite can reach another entry
  -- -- the SacrificeAnyNumber arm below runs a sacrifice, and RunEffects runs a
  -- card's effects -- and the outer batch has to survive that.
  before <- State.gets GameState.enteringBeside
  -- CR 614.12's OTHER half, one field over: the subject is materialized but not
  -- on the battlefield either, so a determination about it counts neither its
  -- siblings nor itself (Projection.boardAsEntering). INSERTED rather than
  -- written, so a nested entry leaves the outer subject uncountable too, and
  -- restored wholesale below for the reason `before` is.
  beforeSubjects <- State.gets GameState.enteringSubjects
  State.modify' (\gs -> gs {GameState.enteringBeside = batch, GameState.enteringSubjects = Set.insert oid beforeSubjects})
  Monad.void (applyReplacementsIn Nothing batch (ProposedEvent.WouldEnter oid))
  flushEnteringCounters oid
  designateProtector oid
  State.modify' (\gs -> gs {GameState.enteringBeside = before, GameState.enteringSubjects = beforeSubjects})

-- CR 614.1c: the counters the entering permanent turned out to be entering WITH,
-- placed once, after CR 616.1's loop has finished deciding how many that is.
--
-- Through settleCounters and NOT putCounters, because the CR 616.1 loop the
-- funnel's door opens has already run at the entry level -- see the CounterR arm
-- of `apply`. Going back through the door would offer every counter-scaling row a
-- second opportunity CR 614.5 has already spent.
--
-- Ascending by kind so the CountersPut events are ordered deterministically; no
-- rule fixes the order, and nothing between them can observe it, since a trigger
-- scan runs only once this whole entry finishes.
--
-- The id is deleted whether or not anything was pending, so a nested entry cannot
-- inherit an outer subject's leftovers.
flushEnteringCounters :: ObjectId -> Game ()
flushEnteringCounters oid = do
  pending <- State.gets (Map.findWithDefault Map.empty oid . GameState.enteringCounters)
  State.modify' (\gs -> gs {GameState.enteringCounters = Map.delete oid (GameState.enteringCounters gs)})
  Monad.mapM_ (\(kind, n) -> Monad.void (settleCounters oid kind n)) (Map.toAscList pending)

-- CR 310.9a: "as a battle enters the battlefield, its controller chooses a player
-- to be its protector." Run for every entering object, and a no-op for all but a
-- battle.
--
-- NOT a replacement effect, and the contrast with the defense counters it enters
-- beside is the rules' own. CR 310.4b says outright that a battle "has the
-- intrinsic ability 'This permanent enters with a number of defense counters on it
-- equal to its printed defense number'" and that "this ability creates a
-- replacement effect (see rule 614.1c)"; CR 310.9a says none of that. It names no
-- ability, cites no rule 614, and reads as a bare instruction about entering. So
-- the counters go through the CR 616.1 loop above and this does not.
--
-- Modelling it as an EntryRewrite anyway was the first cut here, and the rules
-- were right: two intrinsic rows on one entering battle both land in
-- ReplacementBucket.Other, and CR 616.1e then has the controller ORDER them --
-- a prompt on every battle entry whose two answers reach the same board, which is
-- the engine inventing a decision the rules never offer.
--
-- After the loop rather than before it, which is the whole difference between the
-- two positions and is unobservable: nothing between them can see the object (see
-- runEntry's own note -- no Moved event yet, so no trigger scan and no
-- state-based action), and no replacement effect reads a protector. Doing it
-- second also means CR 614.12's "characteristics as it would exist on the
-- battlefield" have already settled, so a permanent that entered AS a copy of a
-- Siege (CR 707.5) designates by the copy's battle types rather than the card's.
designateProtector :: ObjectId -> Game ()
designateProtector oid = do
  gs <- State.get
  let pc = Projection.project oid gs
  Monad.when (Battle.isBattle pc) $ case Projection.controllerOf oid gs of
    -- Unreachable, and defensive for the entry rewrites' reason: the object is
    -- materialized on the battlefield before this runs, so controllerOf falls
    -- back to its owner. Designates NOBODY rather than conjuring a player -- CR
    -- 310.11 is exactly the rule for a battle with no protector, and repairs this
    -- at the next state-based action check.
    Nothing -> pure ()
    Just controller -> do
      picked <- Battle.designateProtector pc controller oid
      State.modify' $ \g ->
        let stamp o = o {Object.protector = picked}
         in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}

-- CR 709.5f / 709.5c: give this permanent the unlocked designations for the
-- halves named -- the single door every unlock goes through, and the only writer
-- of Object.unlockedHalves after the entry designation the move itself writes
-- (CR 709.5d, changeZoneAttaching's mkObj).
--
-- A SET and not one half, which is CR 709.5i's second branch: "when it has
-- neither designation and gains both" is a transition only a write that gives
-- both at once can produce, and rule 709.5f's own singular wording is the
-- instruction's shape rather than this writer's. Every designation lands in one
-- State.modify', so nothing can observe the permanent midway.
--
-- ONE EVENT PER DESIGNATION, since CR 709.5h asks its question about a
-- particular half; CR 709.5i's flag rides on exactly one of them, for the reason
-- Pawl.Types.HalfUnlocked's own comment gives.
--
-- IDEMPOTENT, and that is CR 709.5h rather than defensiveness: the trigger fires
-- "when that permanent IS GIVEN the appropriate unlocked designation", so a
-- permanent that already has it is given nothing and nothing fires. CR 709.5e's
-- special action and CR 709.5f's keyword action both choose a LOCKED half, so
-- neither reaches this with a door already open; an effect that unlocks without
-- choosing can, and the difference is filtered out here rather than by a caller.
--
-- The EVENT is recorded here rather than by the caller, so the two cannot drift:
-- CR 709.5h is a fact about the designation being given, and this is where it is
-- given.
--
-- The ACTOR is CR 709.5h/709.5i's "a player unlocks", passed in because only the
-- caller knows it: rule 709.5e's payer, or the controller of the resolving
-- object carrying out rule 709.5f's instruction.
--
-- CR 709.5g's LOCK is lockHalf below, this function's inverse.
unlockHalves :: PlayerId -> ObjectId -> Set.Set CardName.CardName -> Game ()
unlockHalves actor oid halves = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> do
      let given = Set.difference halves (Object.unlockedHalves obj)
          opened = Set.union (Object.unlockedHalves obj) given
      Monad.unless (Set.null given) $ do
        State.modify' $ \g ->
          let open o = o {Object.unlockedHalves = opened}
           in g {GameState.objects = Map.adjust open oid (GameState.objects g)}
        -- CR 709.5b: the halves the permanent HAS, which for one that copied a
        -- Room are the copied Room's (Game.halvesOf) and not the card printed
        -- underneath it. Off the object's own card this answers False for every
        -- copy, since no copier's printed card has a shared type line.
        let fully = fullyUnlockedAfter opened (Game.halvesOf oid gs)
        Monad.forM_ (Set.toAscList given) $ \half ->
          State.modify' (recordEvent (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked oid actor half (fully && Set.lookupMax given == Just half))))

-- CR 709.5g: take one unlocked designation back away -- "that permanent loses
-- the appropriate unlocked designation". unlockHalves's inverse and the single
-- door every lock goes through, so the two writers of Object.unlockedHalves that
-- ADD a designation have exactly one that removes one.
--
-- NO EVENT, unlike unlockHalves: rules 709.5h and 709.5i are the only rules that
-- ask a trigger about an unlocked designation and both ask about GAINING one, so
-- there is nothing for a lock to fire. GameEvent.HalfUnlocked's own comment
-- carries that argument for the type.
--
-- Unguarded, where unlockHalves guards: the deletion is idempotent on its own, and
-- with no event to suppress there is no transition to detect. CR 709.5g's own
-- "chooses an unlocked half" is what keeps a caller from reaching this with a
-- door already shut.
lockHalf :: ObjectId -> CardName.CardName -> Game ()
lockHalf oid half =
  State.modify' $ \g ->
    let shut o = o {Object.unlockedHalves = Set.delete half (Object.unlockedHalves o)}
     in g {GameState.objects = Map.adjust shut oid (GameState.objects g)}

-- CR 709.5i's "fully unlocks", answered about the designations a permanent has
-- ONCE a write has landed: "such an ability triggers when that permanent has one
-- of the two unlocked designations and gets the other, or when it has neither
-- designation and gains both."
--
-- Taking the designation set rather than the object, so both writers of
-- Object.unlockedHalves -- unlockHalves above and changeZoneAttaching's mkObj
-- below, which is CR 709.5d's entry designation -- can hand over the set they are
-- about to store rather than re-reading a GameState that has or has not been
-- modified yet. THE SAME helper at both, so the two cannot drift apart about what
-- "fully" means; that shared call is the only cover the ENTRY site has, and no
-- board can change that -- CR 709.3 makes a player cast one half, so rule 709.5d
-- gives at most one designation and a two-door Room can never arrive fully
-- unlocked. Only unlockHalves reaches CR 709.5i's second branch, which
-- Pawl.RoomSpec's "CR 709.5i a Room that gains both designations at once fires
-- once" proves. Computed AT THE WRITE and carried on GameEvent.HalfUnlocked for
-- the reason that event's own comment gives: by the time a trigger is matched the
-- board has moved on.
--
-- ALL the halves and not merely two, though CR 709.5 knows only a left and a
-- right: the question the rule asks is whether any half is still locked (CR
-- 709.5c), and asking it of every face is the same answer for a two-faced card
-- and an honest one for anything else. `all` and not `any` is the whole content
-- of this function -- one designation on a two-door Room is exactly the case CR
-- 709.5i does not fire on.
--
-- False for a card with no shared type line, which is CR 709.5's own scope: a
-- card with one face would otherwise be "fully unlocked" by having no locked
-- halves, and it has no halves at all. Nothing calls this for one, but the guard
-- is the rule rather than defensiveness.
--
-- Nothing -- a designation written for an object whose card cannot be found --
-- answers False, there being no faces to compare against.
fullyUnlockedAfter :: Set CardName.CardName -> Maybe Card -> Bool
fullyUnlockedAfter halves card = case card of
  Nothing -> False
  Just c ->
    Card.hasSharedTypeLine c
      && all (\face -> Set.member (Face.name face) halves) (Card.Type.faces c)

-- CR 615: settle one proposed damage event. Empty means it does not happen; two
-- or more is CR 614.9's counted redirection having moved part of it (Harm's
-- Way). The second answer is CR 615.13's, one entry per prevention effect that
-- applied to THIS event and prevented some of it.
resolveDamage :: DamageEvent.DamageEvent -> Game ([DamageEvent.DamageEvent], [Prevention])
resolveDamage de = do
  (outcome, prevented) <- applyReplacementsReporting Nothing Set.empty (ProposedEvent.WouldDealDamage de)
  pure (Maybe.mapMaybe Replacement.asDamageEvent outcome, prevented)

-- CR 608.2f / 510.2: settle a whole batch of SIMULTANEOUS damage events, and
-- answer the survivors. The typed door Pawl.Engine.Damage uses, so Damage never
-- cases on a ProposedEvent or on a ReplacementEffect.
--
-- Each event still runs its OWN CR 616.1 loop and the loop's unit is still one
-- event, which is what CR 614.5 and CR 615.10 both describe. Three things this
-- adds over calling resolveDamage per event, and all three are rules the BATCH is
-- the only place to state:
--
--   * CR 616.1's APNAP clause, because a lone event has one affected object and
--     so one chooser: only a batch can present choices to two players at once.
--     orderBatch settles that order before any of the batch is asked.
--   * CR 615.7's ORDER, because the shield is a single resource allocated across
--     the whole batch and the rule gives that choice to the shielded side -- CR
--     101.4c saying the same of CR 122.1c's shield counters.
--   * CR 615.13's GROUPING, because that rule fires an ability "each time a
--     prevention effect is applied to one or more simultaneous damage events",
--     so one instance reaching three of this batch's events is ONE prevention of
--     the total rather than three.
resolveDamageBatch :: [DamageEvent.DamageEvent] -> Game ([DamageEvent.DamageEvent], [Prevention])
resolveDamageBatch events = do
  ordered <- Replacement.orderBatch events
  settled <- Monad.mapM resolveDamage ordered
  pure (concatMap fst settled, Replacement.groupPreventions (concatMap snd settled))

-- CR 701.8 / 614.8: settle a proposed destruction. `Just` is the object actually
-- destroyed -- which need not be the one asked about, since a rewrite may
-- redirect it; `Nothing` means a replacement took the event (regeneration), and
-- that rewrite has already done its own work.
--
-- `asOf` is applyReplacementsIn's, and the destroy funnel always supplies it: CR
-- 608.2f gives even a single Doom Blade a one-element batch, and when that batch
-- is itself part of a CR 704.3 pass the board is the pass's rather than the
-- batch's (Event.destroyInBatch).
resolveDestruction :: Maybe GameState -> DestructionCause.DestructionCause -> Regenerability.Regenerability -> ObjectId -> Game (Maybe ObjectId)
resolveDestruction asOf cause regenerability oid = do
  live <- State.get
  let settledRegenerability = strengthen (Maybe.fromMaybe live asOf) oid regenerability
  outcome <- applyReplacementsIn asOf Set.empty (ProposedEvent.WouldBeDestroyed oid settledRegenerability cause)
  pure (outcome >>= Replacement.asDestruction)

-- CR 701.19c: a standing prohibition (Hurr Jackal) forbids regeneration of the
-- permanent it names, so the destruction this funnel is about to propose is one
-- that can't be regenerated whatever its cause supplied. The rule's own reading
-- -- the shield is still created and simply "not applied" -- is what makes this
-- the right seam: Replacement.admits refuses the candidate, so CR 616.1 never
-- offers it and the shield is never spent.
--
-- One-directional. A prohibition can only turn Regenerable into
-- CantBeRegenerated; nothing in CR 701.19 permits the converse, so Terror's
-- clause survives an empty store.
--
-- Read off the board applyReplacementsIn is about to gather from -- `asOf` when
-- the caller supplied one (CR 704.3's whole pass, Event.destroyInBatch), the
-- live state otherwise -- so a CR 608.2f batch cannot see the prohibition and
-- the replacement pass disagree about which board they are on.
strengthen :: GameState -> ObjectId -> Regenerability.Regenerability -> Regenerability.Regenerability
strengthen gs oid regenerability =
  if any ((== oid) . ActiveUnregeneratable.object) (GameState.unregeneratables gs)
    then Regenerability.CantBeRegenerated
    else regenerability

-- The single counter-PLACEMENT funnel for an OBJECT recipient (CR 122.6: counters
-- as markers on a permanent -- not to be confused with `counter` below, CR 701.6's
-- countering of a spell; putPlayerCounters below is the player's). CR 122.6 makes
-- this the right single seam, since it covers both counters put on a permanent
-- already on the battlefield and counters an object is given as it enters. A zero
-- count after the loop puts nothing on.
--
-- Beside the other change-and-emit funnels of this module. A copy of the body
-- anywhere else would be a second funnel, which is the one thing a funnel must not
-- have; addEnteringCounters below is not one, since it settles nothing and writes
-- nothing -- flushEnteringCounters shares this door's tail instead.
--
-- The CounterCause is the placement's PROVENANCE and nothing else -- who is putting
-- the counters, and whether an effect is what put them. Only a row reads it; see
-- Replacement.matchesPutter.
--
-- ANSWERS how many counters actually landed, which is not what was asked for: CR
-- 614.16 may have grown or erased the placement, and an object that is no longer
-- there takes none. Only the two rules that say "one or more +1/+1 counters" read
-- it -- CR 702.100b and CR 702.149c, at Pawl.Engine.Resolve's Effect.Evolve and
-- Effect.Train arms; every other caller places and moves on.
putCounters :: CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
putCounters cause oid kind n = do
  resolved <- resolveCounters cause oid kind n
  case resolved of
    Nothing -> pure 0
    Just (target, settledKind, settledCount) -> settleCounters target settledKind settledCount

-- The WRITE-AND-EMIT half of putCounters, with no CR 616.1 loop in front of it:
-- the counters have already been settled and this records them. Its second caller
-- is flushEnteringCounters below, where CR 616.1 ran at the ENTRY level and running
-- it again here would let one row apply twice.
--
-- Not a second funnel. putCounters is still the only door that both settles and
-- writes; this is that door's tail, shared rather than copied.
settleCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
settleCounters target settledKind settledCount
  | settledCount == 0 = pure 0
  | otherwise = do
      gs <- State.get
      -- No write and no event for an object that is not there. Map.adjust on a
      -- missing id is a silent no-op, so proceeding would record a placement
      -- the state does not show. ONE lookup answers both questions -- whether
      -- the object exists, and how many counters of the kind it already had.
      case Game.lookupObject target gs of
        Nothing -> pure 0
        -- CR 101.2 with CR 122.6: a permanent an effect in force says can't have
        -- counters put on it takes none, whatever allowed or directed the
        -- placement (Solemnity, Melira Sylvok Outcast). Asked HERE, on the settled
        -- object and the settled kind, rather than ahead of resolveCounters: CR
        -- 616.1's loop may redirect a placement onto a different object, so the
        -- object the prohibition is about is the one that would actually receive
        -- the counters. Rule 614 replaces events that would happen; rule 101.2
        -- then refuses what rule 614 settled on.
        --
        -- Both of CR 122.6's roads reach this door -- a placement onto a permanent
        -- already on the battlefield, and the counters an object is given as it
        -- enters, which flushEnteringCounters places through here -- so one gate
        -- answers the whole rule.
        --
        -- NOT a CR 614.16 replacement: that rule scales a placement that was
        -- possible, and these cards say "can't". See
        -- Pawl.Types.CounterRestriction.
        Just _ | CounterRestriction.prohibited target settledKind gs -> pure 0
        Just obj -> do
          -- CR 613.7c: the counters arriving get a timestamp, and the ones of
          -- that kind already there get the same one.
          ts <- State.state Game.freshTimestamp
          let before = Map.findWithDefault 0 settledKind (Object.counters obj)
              bump o =
                o
                  { Object.counters = Map.insertWith (+) settledKind settledCount (Object.counters o),
                    Object.counterTimestamps = Map.insert settledKind ts (Object.counterTimestamps o)
                  }
              bumped g = g {GameState.objects = Map.adjust bump target (GameState.objects g)}
          -- CR 122.6's placement, recorded AFTER the write and from the SETTLED
          -- count, so a Doubling Season that turned one counter into two records
          -- the crossing the board actually saw. The before/after pair is what
          -- CR 714.2b's chapter ability reads.
          --
          -- Guarded by the same `settledCount > 0` the write is: an event
          -- recorded for a placement that did not happen would fire a chapter
          -- ability off nothing.
          State.modify' (recordEvent (GameEvent.CountersPut (CounterChange.MkCounterChange target settledKind before (before + settledCount))) . bumped)
          pure settledCount

-- CR 122.6a: putCounters with the rule's DEFAULT putter -- "if the effect doesn't
-- specify a player, the object's controller puts those counters on it". Every
-- placement onto the RECEIVING object's own account goes through here. One caller
-- is left: CR 702.37b's megamorph counter, at `apply`'s TurnUpR arm. The entry
-- roads -- CR 614.1c-d's rewrites and a spell's entry riders -- accumulate through
-- addEnteringCounters below and reach this module's tail by the other route, and
-- rule 122.6a's default putter is what the flush places under either way. No
-- printing in the pool exercises the rule's exception by naming a player, so
-- nothing needs to pass one.
--
-- An object with no controller is one that is not on the battlefield, and
-- putCounters places nothing on such an object anyway -- so answering 0 without
-- raising the event is the same answer, reached one step earlier.
putOwnCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
putOwnCounters oid kind n = do
  gs <- State.get
  case Projection.controllerOf oid gs of
    Nothing -> pure 0
    Just putter -> putCounters (CounterCause.ByEffect putter) oid kind n

-- CR 614.1c: this permanent is going to enter with n more counters of a kind than
-- it was a moment ago. Written to the pending map rather than to the object; see
-- GameState.enteringCounters and flushEnteringCounters, which places them once the
-- entry's own CR 616.1 loop has finished saying how many that is.
--
-- The replacement for putOwnCounters at every ENTRY door. No batch of entering
-- siblings is passed and none is owed: CR 614.12's same-batch exclusion is the
-- entry loop's own `notSibling`, and it now covers these counters too, since the
-- row that would scale them is offered inside that loop.
addEnteringCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game ()
addEnteringCounters oid kind n =
  Monad.when (n > 0) . State.modify' $ \gs ->
    gs
      { GameState.enteringCounters =
          Map.insertWith (Map.unionWith (+)) oid (Map.singleton kind n) (GameState.enteringCounters gs)
      }

-- CR 122: take counters off an object, recording a CountersRemoved event from
-- the before/after pair so a trigger can read the crossing. That event's other
-- producer is CR 120.3h's and CR 120.3c's damage to a battle or a planeswalker;
-- it belongs to no one rule in particular. Two costs come through this door as
-- well: CR 606.4's loyalty (Pawl.Engine.Cost's RemoveLoyaltyFromThis arm) and CR
-- 118.1's +1\/+1 removal (its RemovePlusOneCountersFromThis arm).
--
-- NO CR 614.16 loop, unlike putCounters above, and that asymmetry is the rule's
-- rather than a shortcut: 614.16 replaces a PLACEMENT -- "if an effect would put
-- one or more counters on a permanent" -- and no ReplacementEffect class in
-- Pawl.Types.ReplacementEffect pairs with a removal, so there is nothing for a
-- loop to offer this to.
--
-- SATURATING: removing more than are present leaves the kind absent from the map
-- rather than negative, which is what keeps Object.counters a tally of what is
-- there. CR 122 states no rule making an over-large removal fail.
--
-- This is not the only place counters leave an object, and the two others are
-- each other's opposite. CR 704.5q's annihilation IS a removal (rule 122.3 says
-- so) and records its own CountersRemoved, from a before/after diff rather than
-- through this door -- Pawl.Engine.Sba's `balance` is a pure fold inside the CR
-- 704.3 single-event pass, and its comment gives the argument. CR 122.2's zone
-- change is NOT one and records nothing: "the counters are not 'removed'; they
-- simply cease to exist", so an event there would fire a counter-watching
-- trigger off something the rule denies happened.
removeCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game ()
removeCounters oid kind n =
  Monad.when (n > 0) . State.modify' $ \gs ->
    -- ONE lookup answers both questions -- whether the object is there, and how
    -- many of the kind it has -- for putCounters' reason: Map.adjust on a
    -- missing id is a silent no-op, so proceeding would record a removal the
    -- state does not show.
    case Game.lookupObject oid gs of
      Nothing -> gs
      Just obj ->
        let before = Map.findWithDefault 0 kind (Object.counters obj)
            after = if n >= before then 0 else before - n
            drop_ o =
              o
                { Object.counters =
                    if after == 0
                      then Map.delete kind (Object.counters o)
                      else Map.insert kind after (Object.counters o)
                }
            dropped = gs {GameState.objects = Map.adjust drop_ oid (GameState.objects gs)}
         in -- Nothing to remove is nothing to record: an event for a removal that
            -- did not happen would fire a counter-watching trigger off nothing,
            -- which is the guard putCounters puts on its own write.
            if before == 0
              then gs
              else recordEvent (GameEvent.CountersRemoved (CounterChange.MkCounterChange oid kind before after)) dropped

-- CR 122.6: settle a proposed counter placement. Nothing means none are put on.
--
-- EVERY placement runs the CR 616.1 loop, whatever its cause, and the cause rides
-- the event into Replacement.matchesPutter -- which is where CR 614.16's "if an
-- effect would put" and Vorinclex's "if you would put" part company. A ByRule
-- placement used to skip the loop at this door, an equivalence that held only while
-- every representable counter replacement was one of rule 614.16's; Vorinclex is
-- not one, so the gate moved into the row filter (#847).
--
-- NO ENTERING BATCH is passed, and CR 614.12's sibling exclusion is not asked
-- here: a counter a permanent enters with is part of how it enters (CR 614.1c),
-- so it is settled in the entry loop instead, where `notSibling` answers rule
-- 614.12 for it. What reaches THIS door is a placement onto a permanent that has
-- already entered, which rule 614.12 says nothing about, so the exclusion has no
-- subject here.
resolveCounters :: CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game (Maybe (ObjectId, CounterKind.CounterKind Keyword.Type.Keyword, Natural))
resolveCounters cause oid kind n = do
  outcome <- applyReplacements (ProposedEvent.WouldPutCounters cause oid kind n)
  pure (outcome >>= Replacement.asCounters)

-- CR 122.1 / 122.6: putCounters' player half -- the ONE place a player's counters
-- go up. Answers how many actually landed, which is not what was asked for: the
-- CR 616.1 loop may have grown the placement (Vorinclex, Monstrous Raider's
-- doubling), shrunk it or erased it. Nobody reads the answer yet; it is the shape
-- putCounters already has, and dropping it would make the two funnels disagree
-- about their own event.
--
-- No GameEvent is recorded, where putCounters records CR 122.6's CountersPut: that
-- event carries an ObjectId, and no trigger condition in
-- Pawl.Types.TriggerCondition watches a player's counters -- so an event minted
-- here would have no reader. The card that gives it one brings the condition and
-- the event together; this is where it is recorded.
putPlayerCounters :: CounterCause.CounterCause -> PlayerId -> PlayerCounterKind.PlayerCounterKind -> Natural -> Game Natural
putPlayerCounters cause pid kind n = do
  resolved <- resolvePlayerCounters cause pid kind n
  case resolved of
    Nothing -> pure 0
    Just (target, settledKind, settledCount)
      | settledCount == 0 -> pure 0
      | otherwise -> do
          live <- State.get
          -- CR 101.2 with CR 122.1: a player an effect in force says can't get
          -- counters gets none, whatever allowed or directed the placement
          -- (Solemnity, Melira Sylvok Outcast). settleCounters' gate on the player
          -- axis, read the same way and at the same point -- after CR 616.1's
          -- loop, about the SETTLED player and the settled kind.
          if PlayerEffect.prohibitsCounters target settledKind live
            then pure 0
            else do
              -- Zero is the guard putCounters puts on its own write, and the two
              -- shrinking scalings are what make it reachable here: half of one
              -- counter, rounded down, and one counter minus one, are replacements
              -- that remove the event.
              State.modify' $ \gs ->
                let bump p = p {Player.counters = Map.insertWith (+) settledKind settledCount (Player.counters p)}
                 in gs {GameState.players = Map.adjust bump target (GameState.players gs)}
              pure settledCount

-- CR 122.1: resolveCounters for a player recipient, and read the same way -- the
-- cause rides the event and the rows decide which causes they reach.
resolvePlayerCounters :: CounterCause.CounterCause -> PlayerId -> PlayerCounterKind.PlayerCounterKind -> Natural -> Game (Maybe (PlayerId, PlayerCounterKind.PlayerCounterKind, Natural))
resolvePlayerCounters cause pid kind n = do
  outcome <- applyReplacements (ProposedEvent.WouldPutPlayerCounters cause pid kind n)
  pure (outcome >>= Replacement.asPlayerCounters)

-- CR 119.3 / 119.4 / 119.5 / 120.4c: settle how much life a player actually
-- loses, and answer with the settled amount. The one funnel every cause goes
-- through -- Pawl.Engine.Damage.applyDamage at CR 120.4c's result-processing
-- step, Pawl.Engine.Resolve's Effect.LoseLife arm and its changeLifeByDelta (the
-- downward side of a set, an exchange and a redistribution alike), and payLife
-- above -- so a row cannot reach one road and miss another.
--
-- The SETTLED amount may be larger than what came in, not only smaller: a
-- Pawl.Types.LifeLossRewrite may be a scaling. It is 0 for a CANCELLED event too
-- -- CR 614.6's "it never happens" -- which a caller cannot tell from a loss
-- rewritten to nothing, and need not: either way no life moves.
--
-- It does not WRITE the life total, unlike resolveUntap and the counter funnels
-- above: every caller already owns that write, and the damage one has to fold its
-- answer back into a batch of simultaneous events (CR 510.2) that this module
-- knows nothing about.
--
-- Zero proposes nothing, which is CR 119.9's posture on the gain side taken for
-- the loss: no life loss event, so no row is spent on one.
resolveLifeLoss :: LifeLossCause.LifeLossCause -> PlayerId -> Natural -> Game Natural
resolveLifeLoss cause pid n =
  if n == 0
    then pure 0
    else do
      outcome <- applyReplacements (ProposedEvent.WouldLoseLife cause pid n)
      pure (maybe 0 snd (outcome >>= Replacement.asLifeLoss))

-- CR 119.3 / 119.10 / 120.3f: settle how much life a player actually gains, and
-- answer with the settled amount. resolveLifeLoss the other direction, and the
-- one funnel every gain goes through -- Pawl.Engine.Resolve's Effect.GainLife
-- arm and its changeLifeByDelta (the upward side of a set, an exchange and a
-- redistribution alike), Pawl.Engine.Damage's CR 120.3f lifelink pass, and the
-- DrawRewrite.GainLife arm above -- so a row cannot reach one road and miss
-- another.
--
-- The SETTLED amount may be larger than what came in, and for the pool's only
-- producers always is: Pawl.Types.LifeGainRewrite is a scaling.
--
-- It does not WRITE the life total, resolveLifeLoss's split and for its reason:
-- every caller already owns that write, and the lifelink one has to keep its
-- record out of a CR 510.2 bracket its caller opened.
--
-- Zero proposes nothing, which is CR 119.10 in as many words: "if a player gains
-- 0 life, no life gain event would occur, and these effects won't apply".
--
-- Not implemented: CR 119.7's restriction on a player who CAN'T gain life --
-- under which no gain happens here at all and "a replacement effect that would
-- replace a life gain event affecting that player won't do anything", so the
-- gate belongs ahead of the proposal rather than among the rows. Vacuous:
-- Pawl.Types.PlayerEffect has no such arm to consult (#3078).
resolveLifeGain :: PlayerId -> Natural -> Game Natural
resolveLifeGain pid n =
  if n == 0
    then pure 0
    else do
      outcome <- applyReplacements (ProposedEvent.WouldGainLife pid n)
      pure (maybe 0 snd (outcome >>= Replacement.asLifeGain))

-- CR 111.1: settle a proposed token creation. Nothing means none are created.
resolveTokens :: PlayerId -> Seq.Seq TokenLot.TokenLot -> Game (Maybe (PlayerId, Seq.Seq TokenLot.TokenLot))
resolveTokens pid lots = do
  outcome <- applyReplacements (ProposedEvent.WouldCreateTokens pid lots)
  pure (outcome >>= Replacement.asTokens)

-- CR 500.11 / 614.10: settle whether a step or phase begins at all, on the turn
-- of `pid`. False means a skip took it, and proceeding past it is then the
-- caller's whole obligation -- CR 614.1b replaces a skipped step with nothing, so
-- there is no rewritten event to carry out. How far past it reaches is the
-- caller's too: one schedule entry for a PhaseSelector.Step, the phase's
-- remaining entries for a whole phase (Engine.runStep, Turn.dropRestOfPhase).
--
-- Answers a Bool rather than the settled event, unlike resolveDestruction, whose
-- `Just` had to carry an identity because a rewrite can redirect which object is
-- destroyed. Nothing can rewrite a WouldBeginPhase: PhaseR is the only effect the
-- class admits and it only ever cancels.
--
-- The typed door Pawl.Engine.Engine uses, so Engine never cases on a
-- ProposedEvent.
beginsPhase :: PhaseSelector -> PlayerId -> Game Bool
beginsPhase selector pid = do
  outcome <- applyReplacements (ProposedEvent.WouldBeginPhase selector pid)
  pure (Maybe.isJust (outcome >>= Replacement.asPhaseBegin))

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
-- The Game () wrapper the ~30 existing callers use; changeZoneReturning below
-- carries the same body but hands back the freshly-minted incarnation ids, which
-- Resolve's ExileUntilMonarch arm registers for its return sweep.
--
-- IDS and not one id: CR 712.21 puts a melded permanent's two cards into the
-- destination as one departure and two arrivals, so every returning door below
-- answers with a SEQUENCE -- empty for a move that did not happen, one element
-- for every object but a melded permanent leaving the battlefield, one per
-- component for that. Widening rather than special-casing is what makes CR
-- 712.21c reachable: an effect that finds what the permanent became finds both
-- cards, where a single-id answer would silently drop the second.
changeZone :: ObjectId -> Zone -> Game ()
changeZone oid requestedDest = Monad.void (changeZoneReturning oid requestedDest)

-- changeZoneReturning for a move whose effect says how the object ENTERS -- CR
-- 110.5b's tap state, CR 712.14a's transformed face, CR 708.3's face-down entry
-- and CR 110.2a's controller -- rather than taking the rules' defaults.
--
-- A separate door rather than a fifth parameter on changeZone, as changeZoneInBatch
-- is: the ~30 callers moving under the default have no tap state to name. Handed
-- to the funnel rather than applied after it, so a permanent an effect says is
-- tapped is never untapped for an instant, and so a permanent an effect says
-- enters under someone's control never belongs to its owner for an instant --
-- CR 614.1c's entry replacements run inside this call and read both.
--
-- CR 712.14a is the other rule this door is the only one for: "If a spell or
-- ability puts a double-faced card onto the battlefield 'transformed' or
-- 'converted', it enters the battlefield with its back face up." Which face that
-- is comes from the card's layout (Card.backFace), so the rider stays a Bool and
-- the engine never learns which card is entering. The rule's SECOND sentence is
-- the same refusal CR 712.14b gets below -- "if a player is instructed to put a
-- card that isn't a double-faced card onto the battlefield transformed or
-- converted, that card stays in its current zone" -- which is exactly a card
-- whose layout gives Card.backFace nothing to answer with.
--
-- CR 712.14a and NOT CR 702.145b's entering-transformed ability, which is a
-- different mechanism: an ability causing a permanent to enter transformed is a
-- replacement effect, CR 616.1d's own bucket, and no rider on a move can express
-- it -- EntryRewrite.EntersTransformed is that one, applied by `apply` above.
-- (CR 712.13a is neither: it is the resolving double-faced spell's own rule.)
--
-- The one door CR 712.14b applies to, and that is what the rule's own wording
-- picks out: "If a player is INSTRUCTED to put a modal double-faced card onto
-- the battlefield and its front face isn't a permanent card, the card stays in
-- its current zone." An instruction to put an object onto the battlefield is
-- exactly the move that names how it enters, so the other doors are out of the
-- rule's scope rather than exempted from it -- a permanent spell RESOLVING (CR
-- 712.13, Pawl.Engine.Stack) and a land PLAYED (CR 712.12, Pawl.Engine.Engine)
-- both reach the battlefield without any instruction to put them there.
--
-- The empty answer, the same one a CR 616.1 replacement that cancelled the move
-- gives, so every caller already handles it: "the card stays in its current
-- zone" and "nothing entered" are the same fact. Asked BEFORE the funnel, because CR
-- 712.14b is not a replacement effect -- there is no event for CR 616.1 to
-- choose among, and running the entry loop first would fire CR 614.1c's
-- as-enters abilities for a card that never enters.
changeZoneEntering :: ObjectId -> Zone -> LibraryPosition.LibraryPosition -> EntryRiders.EntryRiders Natural -> Maybe PlayerId -> Game (Seq.Seq ObjectId)
changeZoneEntering = changeZoneEnteringIn Nothing Set.empty

-- changeZoneEntering for ONE MEMBER of a CR 608.2f batch: `asOf` and `batch` are
-- applyReplacementsIn's, and Pawl.Engine.Resolve's Effect.MoveToZone arm is the
-- only caller that supplies either. A separate door rather than two more
-- parameters on changeZoneEntering, changeZoneInBatch's shape one door over: the
-- lone moves have no batch to name.
changeZoneEnteringIn :: Maybe GameState -> Set ObjectId -> ObjectId -> Zone -> LibraryPosition.LibraryPosition -> EntryRiders.EntryRiders Natural -> Maybe PlayerId -> Game (Seq.Seq ObjectId)
changeZoneEnteringIn asOf batch oid requestedDest position riders under = do
  gs <- State.get
  let mCard = Game.cardOf oid gs
      onto = requestedDest == Zone.Battlefield
      -- CR 712.14a's back face, asked only of a move the effect says is
      -- transformed. Nothing means one of two different things, which is why the
      -- rider is read alongside it below: no back face to turn to, or no
      -- instruction to turn at all.
      mBack = if EntryRiders.transformed riders then mCard >>= Card.backFace else Nothing
      refused =
        onto
          && ( maybe False Card.staysWhenPutOntoBattlefield mCard -- CR 712.14b
                 || (EntryRiders.transformed riders && Maybe.isNothing mBack) -- CR 712.14a
             )
      -- CR 712.14: "A double-faced card put onto the battlefield from a zone
      -- other than the stack enters the battlefield with its front face up by
      -- default", which Object.face records as Nothing (CR 712.8a).
      shown = if onto then fmap Face.name mBack else Nothing
      -- CR 110.2a: the effect states otherwise, so the player it instructed stops
      -- being the answer and CR 110.2's default -- the owner, which is what
      -- `Nothing` means to the funnel -- takes over. Undying and persist are what
      -- print it (CR 702.93a, CR 702.79a).
      under' = if EntryRiders.underOwner riders then Nothing else under
      -- CR 708.3: "objects that are put onto the battlefield face down are
      -- turned face down BEFORE they enter the battlefield". Handed to the
      -- funnel rather than written after it for the tap state's reason, and the
      -- rule's whole content is that ordering -- the funnel writes the status in
      -- mkObj, so the CR 614.1c entry loop, the CR 603.2g Moved event and every
      -- trigger scanning it see CR 708.2a's 2/2 with no abilities, and "the
      -- permanent's enters-the-battlefield abilities won't trigger (if
      -- triggered) or have any effect (if static)" needs no branch of its own.
      -- Manifest (CR 701.40a) is what prints it; Pawl.FaceDownSpec's Soul
      -- Summons group is the proof, with Thragtusk's "you gain 5 life" enters
      -- trigger as the card underneath.
      --
      -- BATTLEFIELD-ONLY, which is CR 110.5d's scope for the status and this
      -- door's own responsibility: mkObj's `facing` write serves the stack too
      -- (CR 708.4, changeZoneCasting), so the gate cannot live there. A card
      -- stating the rider on a move anywhere else says something no rule reads,
      -- which Pawl.CardSpec lints.
      -- CR 708.2 and CR 708.6 both come off the rider rather than being minted
      -- here: the effect that put the permanent onto the battlefield face down
      -- is what LISTS its characteristics (CR 708.2a's "unless otherwise
      -- specified") and what CR 708.6 names as the allower, so this door only
      -- says WHERE the value applies. Soul Summons and Cloudform write CR
      -- 701.40a's manifest -- the reason that opens CR 701.40b's turn-face-up
      -- procedure (Pawl.Engine.FaceDown.canTurnFaceUp) -- and Yedora, Grave
      -- Gardener writes CR 708.3's plain putting, which opens nothing.
      facing = if onto then maybe Facing.FaceUp Facing.FaceDown (EntryRiders.faceDown riders) else Facing.FaceUp
  if refused
    then pure Seq.empty
    else changeZoneAttaching asOf batch oid requestedDest position Nothing (EntryRiders.tapped riders) (EntryRiders.counters riders) under' shown facing (EntryRiders.exiledFaceDown riders) CarryOver.NotCarried

-- changeZoneReturning for a move that carries ONE NAMED HALF of the card into
-- its destination: CR 709.3's choice of which half of a split card is being
-- cast, which the rule makes "before putting it onto the stack", so the move is
-- what carries it. CR 712.11b words the modal double-faced card's version of the
-- same choice identically, and CR 712.11c its version of CR 709.3a, so both
-- layouts come through this door.
--
-- A HALF and not always a face that is up, which is CR 709.5d's use of the same
-- parameter: a Room permanent shows both halves at once, so changeZoneAttaching
-- spends the name on an unlocked designation and leaves Object.face empty. Every
-- other caller's half IS the face the object arrives showing.
--
-- The name is a MAYBE, because the third rule asking for this door does not
-- always have one to give: CR 712.13 carries a resolving double-faced spell's
-- face onto the battlefield, and Pawl.Engine.Stack asks that of every permanent
-- spell it resolves, most of which show nothing (Pawl.Engine.Card.enteringFace).
-- Nothing here is exactly changeZoneReturning.
--
-- A separate door rather than a seventh parameter on changeZone, as
-- changeZoneEntering is: the ~30 callers moving an object that shows whatever its
-- layout gives it (CR 709.4's combined view, a single-face card's one face) have
-- no face to name.
--
-- Handed to the funnel rather than written onto the object the move returned,
-- for the reason CR 709.3a states: "only that half is considered to be put onto
-- the stack", so the CR 400.7 incarnation must never exist without it -- and,
-- since only a writer inside the move knows where the move actually landed, a
-- CR 616.1 redirect to another zone drops the face instead of carrying it there.
-- See the `face` note in changeZoneAttaching's mkObj, and Pawl.CastSpec's "a cast
-- redirected off the stack keeps both halves" for the case that proves it.
--
-- CR 110.2's entry controller rides along, because the one caller is a LAND
-- PLAY and rule 305.1's player is not always the card's owner: Sen Triplets lets
-- alice play a land out of bob's hand, and the CR 108.4a fallback this used to
-- pass would have handed bob the permanent; see #2169. Identical wherever the two
-- seats coincide, which is every other land play.
changeZoneShowing :: Maybe PlayerId -> ObjectId -> Zone -> Maybe CardName.CardName -> Game (Seq.Seq ObjectId)
changeZoneShowing under oid requestedDest shown = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty under shown Facing.FaceUp False CarryOver.NotCarried

-- changeZoneShowing for a move that puts the object into its destination FACE
-- DOWN -- the CR 110.5b "unless a spell or ability says otherwise" that morph is.
--
-- The two rules that need it are the two ends of one cast. CR 708.4: "Objects
-- that are cast face down are turned face down BEFORE they are put onto the
-- stack, so effects that care about the characteristics of a spell will see only
-- the face-down spell's characteristics" -- so the facing is part of the move
-- and not a stamp applied to what it hands back, exactly as CR 709.3a's chosen
-- half is. And that rule's last sentence, "the permanent the spell becomes will
-- be a face-down permanent", which is the stack-to-battlefield move
-- Pawl.Engine.Stack makes. CR 708.3's permanent PUT onto the battlefield face
-- down does NOT come through here: it is a move whose effect says how the object
-- enters, riders and all, so it is changeZoneEntering's faceDown rider.
--
-- A separate door rather than an eighth parameter on changeZoneShowing, as
-- changeZoneEntering is: the ordinary move leaves CR 110.5b's default standing
-- and has no facing to name.
--
-- The face name goes along, and is not in tension with CR 708.2a's "no name":
-- Object.face records WHICH HALF of the card is underneath, not what the object
-- is called, and the substitution that empties the name reads it (faceOf) rather
-- than being stored. Turning the permanent face up is what makes it observable
-- again (CR 708.8).
changeZoneFaceDown :: ObjectId -> Zone -> Maybe CardName.CardName -> Game (Seq.Seq ObjectId)
changeZoneFaceDown oid requestedDest shown = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty Nothing shown (Facing.faceDown FaceDownReason.Morphed) False CarryOver.NotCarried

-- CR 601.2a's move: the card goes onto the stack and "that player becomes its
-- controller". The caster is carried BY THE MOVE, for the reason CR 709.3a
-- states about the chosen half above -- the CR 400.7 incarnation must never exist
-- without it, or a reader inside the move (a CR 616.1 replacement, a trigger
-- scanned over the arrival) would see a spell controlled by whoever happens to
-- own the card.
--
-- One caller, Pawl.Engine.Cast, which is the only route a CARD takes onto the
-- stack. changeZoneEntering carries a player too, so an Effect.MoveToZone naming
-- the stack would stamp) one as well -- no) card in the pool does, and CR 800.4b's
-- "put onto the stack under the control of" says such a move would have a
-- controller to name if one did.
--
-- A CR 616.1 redirect that lands the move in any zone but the stack or the
-- battlefield drops the stamp, exactly as it drops the face, because CR 109.4
-- gives an object there no controller to record.
changeZoneCasting :: PlayerId -> ObjectId -> Zone -> Maybe CardName.CardName -> Facing.Facing -> Game (Seq.Seq ObjectId)
changeZoneCasting caster oid requestedDest shown facing = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty (Just caster) shown facing False CarryOver.NotCarried

-- changeZone for one member of a batch of moves CR 608.2f or CR 704.3 processes
-- SIMULTANEOUSLY. `asOf` is the board the batch began in -- or, for a batch inside
-- a larger simultaneous event, that event's -- and is what its members' CR 616.1
-- loops collect replacement candidates from; see applyReplacementsIn above.
--
-- Not the only batch door: an Effect.MoveToZone batch goes through
-- changeZoneEnteringIn, which carries the entry riders, the library position and
-- the entry controller this one has no parameters for. This door carries the
-- batches whose members reach a graveyard, the command zone or exile --
-- Pawl.Engine.Sba's sweeps, the destroy funnel and CR 800.4a's fourth clause --
-- so nothing enters the battlefield and there is no `batch` set for CR 614.12a
-- to narrow.
--
-- A separate door rather than a fourth parameter on changeZone: a batch is the
-- rare case, and for a single move the board it begins on IS the live one.
changeZoneInBatch :: GameState -> ObjectId -> Zone -> Game ()
changeZoneInBatch asOf oid requestedDest = Monad.void (changeZoneInBatchReturning asOf oid requestedDest)

-- changeZoneInBatch, answering with the destination incarnations' ids -- what
-- changeZoneReturning is to changeZone, and the same answer: the CR 400.7 ids,
-- empty when the move was cancelled or the id named no object. The destroy
-- funnel is the one caller, for CR 701.8b's "put into a graveyard this way".
changeZoneInBatchReturning :: GameState -> ObjectId -> Zone -> Game (Seq.Seq ObjectId)
changeZoneInBatchReturning asOf oid requestedDest = changeZoneAttaching (Just asOf) Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty Nothing Nothing Facing.FaceUp False CarryOver.NotCarried

-- changeZoneReturning's body, returning the destination incarnations' ids: one
-- fresh id per arrival on a completed move (CR 400.7), which is one for every
-- object but a melded permanent leaving the battlefield and one per component for
-- that (CR 712.21); empty when the id is unknown or the CR 616.1 replacement loop
-- cancelled the move (`resolved == Nothing`). changeZoneReturning itself is the
-- `seed = Nothing` case below.
changeZoneReturning :: ObjectId -> Zone -> Game (Seq.Seq ObjectId)
changeZoneReturning oid requestedDest = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty Nothing Nothing Facing.FaceUp False CarryOver.NotCarried

-- changeZoneReturning with an attachment seed. Per CR 303.4 attachment is a
-- property of entering, not a step after it: the CR 614.1c entry replacement loop
-- and the Moved event both run before this returns, so an Aura attached afterward
-- would be unattached during both.
--
-- The seed is also what the CR 701.3a attachment event further down is read off,
-- so Bramble Elemental's "whenever an Aura becomes attached to this creature"
-- turns on it -- Pawl.TriggerSpec's "CR 608.3c whole card" case is the proof.
-- The ORDERING against the entry loop and the Moved event is the rule's rather
-- than anything a card in data/cards reads.
--
-- Stack's Aura branch is the only caller supplying a seed. An Aura entering by any
-- other route is CR 303.4f's, and the body below asks its controller what it will
-- enchant rather than letting it enter unattached -- see there.
--
-- `asOf` and `batch` are applyReplacementsIn's, supplied by changeZoneInBatch and
-- by changeZoneEnteringIn; every other door names a lone move and passes Nothing
-- and the empty set. `batch` is read twice below: CR 303.4f's host sweep
-- subtracts it, and the CR 614.1c entry loop is handed it. `tapped` is CR
-- 110.5b's status, Untapped for every door but changeZoneEntering. `entering` is
-- CR 122.6a's counters the object enters the battlefield with, empty for every
-- door but that one and inert for every destination but the battlefield.
-- `under` is the arriving incarnation's default controller -- CR 110.2a's entry
-- controller on the battlefield, CR 405.4's caster on the stack -- Nothing for
-- every door but changeZoneEntering and changeZoneCasting, and Nothing on the
-- first of those too for a move whose effect names no player, which by CR 110.2
-- and CR 108.4a leaves the owner answering. `shown` is CR 709.3's chosen
-- half or CR 712.13's carried face, Nothing for every door but
-- changeZoneShowing. `facing` is CR 110.5's face-up/face-down status, FaceUp for
-- every door but changeZoneFaceDown (CR 708.4) and a changeZoneEntering whose
-- riders say otherwise (CR 708.3), which is CR 110.5b's default standing
-- everywhere else. `position` is CR 401.2's end of
-- a library, the default for every door but changeZoneEntering. `concealed` is
-- CR 406.3's "exiled face down", False for every door but changeZoneEntering and
-- inert for every destination but exile.
-- `carrying` is CR 400.7a's exception, Carried for Pawl.Engine.Stack's two
-- permanent-spell branches and NotCarried for every other door -- see carryOver
-- below, which the body calls before the entry loop, under a `dest ==
-- Battlefield` gate rather than a `dest == requestedDest` one: the rule is about
-- the permanent the spell becomes, and only a battlefield destination makes one.
--
-- `position` needs no `dest == requestedDest` gate, unlike `face` and `facing`
-- below: it is inert everywhere but a library, so a CR 616.1 redirect AWAY from
-- one drops it for free, and a redirect INTO one from a move that named no
-- position carries the default -- which is the right answer, since nothing said
-- top.
changeZoneAttaching :: Maybe GameState -> Set ObjectId -> ObjectId -> Zone -> LibraryPosition.LibraryPosition -> Maybe Recipient.Recipient -> TapState.TapState -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural -> Maybe PlayerId -> Maybe CardName.CardName -> Facing.Facing -> Bool -> CarryOver.CarryOver -> Game (Seq.Seq ObjectId)
changeZoneAttaching asOf batch oid requestedDest position seed tapped entering under shown facing concealed carrying = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure Seq.empty
    -- CR 111.8: "A token that has left the battlefield can't move to another
    -- zone or come back onto the battlefield. If such a token would change
    -- zones, it remains in its current zone instead." The empty answer is this
    -- funnel's word for a move that did not happen -- the object is never touched, so it
    -- is still in the zone it was in, which is what the rule asks for.
    --
    -- Only a WITHIN-ONE-RESOLUTION window reaches this: CR 704.5d removes such a
    -- token the next time state-based actions are checked, and until then a slot
    -- an earlier clause bound to the token's exile or graveyard incarnation
    -- still names it. Flicker of Fate is the pool's producer -- "exile target
    -- creature or enchantment, THEN RETURN IT to the battlefield" -- and
    -- Pawl.ZoneChangeSpec is where the token and the card twin are separated.
    --
    -- Asked BEFORE the CR 616.1 replacement loop rather than after it, which is
    -- CR 101.2: a "can't" beats whatever would allow the move, so there is no
    -- event left for a replacement to choose among. Ordering it after the loop
    -- would answer the same, and not on a claim about Magic: the predicate reads
    -- the object and its CURRENT zone, neither of which any ZoneChangeR rewrites
    -- -- they rewrite the destination -- and none of the three things that loop
    -- does write on this path touches them: CR 701.20's reveal appends to the
    -- event log, Replacement.consume marks a candidate spent, and CR 616.1's
    -- Replacement.choose writes GameState.lastChoice when two rows differ enough
    -- to be worth a prompt (Rest in Peace beside Leyline of the Void, or either
    -- beside Nexus of Fate).
    Just obj | Game.tokenHasLeftTheBattlefield obj -> pure Seq.empty
    Just obj -> do
      let pid = Object.owner obj
          fromZone = Object.zone obj
          -- CR 704.8: inside a batch, last known information is read from the
          -- board the batch began on, not from the live one -- so a permanent
          -- leaving alongside others is not projected against a board its
          -- siblings have already left. Departure.depart reads its own record
          -- the same way for CR 800.4a's first clause.
          --
          -- Live for an id the batch board does not hold: destroyIn follows the
          -- object its replacement loop settled on, which CR 614.6 lets a
          -- rewrite redirect to one that board never held.
          lki = case asOf of
            Just before | Maybe.isJust (Game.lookupObject oid before) -> before
            _ -> gs
          -- CR 608.2h: last known information -- the object as it exists in the
          -- zone it is LEAVING, projected against the pre-move state. Forced
          -- eagerly (Moved's snapshot field is strict) rather than left as a thunk
          -- retaining the whole pre-move GameState for a turn. The price of an
          -- honest history: a token has no printed card to re-derive from (CR
          -- 111.1).
          snapshot = Projection.project oid lki
          -- CR 613.1b: the OTHER half of last known information, read from the
          -- same pre-move state. Control is not a characteristic (CR 109.3's
          -- list does not include it), so it cannot ride `snapshot`; it is kept
          -- because CR 603.3a asks "who controlled its source at the time it
          -- triggered" about sources that are already gone -- see
          -- eventTriggers below.
          --
          -- The `Object.owner` fallback is unreachable rather than a guess:
          -- Projection.controllerOfGiven's own base case returns
          -- `Just (Object.owner obj)` for any id that resolves, and `oid`
          -- resolves in `lki` (either it is `gs`, which this branch matched
          -- `Just obj` against, or the guard above found it). It is written as a
          -- fallback only because controllerOf's type is honest about ids that
          -- do not.
          --
          -- A second board walk on the same hot path as `snapshot` above
          -- (controllerOf rebuilds controlGrants, a walk of the battlefield).
          -- Measured on the tasty-bench suite, this commit's parent vs. this
          -- change (goldfish / casting / fighting / fighting-aura, 2p):
          -- 15.2/133/24.6/569 ms -> 15.5/134/25.2/575 ms -- every move inside
          -- one run-to-run stddev, so no gate was moved to buy it back.
          lastController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid lki)
      -- CR 614.4: replacements exist before the event, so the loop reads them from
      -- the PRE-MOVE state. CR 614.6: the modified event is what actually happens.
      --
      -- `obj` and `snapshot` are read before this runs and still used after it
      -- returns, which is sound despite the loop writing state -- Replacement's
      -- AsCopy arm calls State.modify', and the ZoneChangeR arm records CR
      -- 701.20's reveal: `gs` and `lki` are immutable values, so no downstream
      -- modify' can change what `snapshot` captured. Extending either loop to
      -- mutate state these bindings read would mean re-deriving them after that
      -- loop.
      --
      -- Both ids are `oid` in the PROPOSED event: nothing has moved yet.
      (resolved, exiledBy, shuffling, splitOff) <- resolveZoneChange asOf (ZoneChange.MkZoneChange oid oid fromZone requestedDest)
      case resolved of
        -- CR 614.6: nothing survived the loop, so no zone change happens. No
        -- producer today -- no ReplacementEffect in data/cards cancels a zone
        -- change outright, where the entry PROHIBITION one case down is a CR
        -- 101.2 "can't" rather than a rule 614 replacement -- but Maybe is what
        -- "the event does not happen" means on this path.
        Nothing -> pure Seq.empty
        -- CR 101.2 with CR 400.4a: an effect in force states this object can't
        -- enter the battlefield, and CR 101.2 makes that "can't" beat whatever
        -- allowed or directed the entry. CR 400.4a is the rulebook's own answer
        -- to what happens next -- "it remains in its previous zone" -- stated
        -- there for a card type, and again in CR 701.40f for a prohibited
        -- manifest. Grafdigger's Cage is the pool's printing, and CR 613.11 is
        -- why the prohibition is asked here rather than in Pawl.Engine.Projection.
        --
        -- Nothing for every origin but the stack, this funnel's CR 614.6 cancel
        -- arm one case up and CR 303.4g's answer one case down: for those the
        -- object is never deleted and never re-minted
        -- (CR 400.7), so the card in the graveyard or the library is the SAME
        -- object it always was. That is what separates a refusal from a redirect,
        -- and Pawl.EntryRestrictionSpec's "each card remains in its previous zone,
        -- as the same object" is the proof.
        --
        -- BEFORE placeObject and before the CR 614.1c entry loop, changeZoneEntering's
        -- `refused` reason: running the loop first would fire as-enters abilities
        -- for a card that never enters. Before mkObj too, which is CR 701.40f's
        -- "if it was face up, it remains face up" -- the face-down status
        -- changeZoneEntering computed for a manifest is written in mkObj and
        -- nowhere earlier, so a refused manifest leaves the library card face up
        -- with nothing to undo (Pawl.EntryRestrictionSpec's Soul Summons case).
        --
        -- AFTER the CR 616.1 replacement loop and asked of the SETTLED
        -- destination, `unlocking`'s reading and CR 614.6's: a redirect that sends
        -- the object anywhere but the battlefield is the event that happens, and
        -- an entry prohibition has nothing to say about it.
        --
        -- `fromZone` is the "previous zone" both rules name, read off the object
        -- rather than off `settled`: no ZoneChangeR rewrites an event's origin.
        --
        -- CR 608.3e is the one origin CR 400.4a does not answer for: "if a
        -- permanent spell resolves but its controller can't put it onto the
        -- battlefield, that player puts it into its owner's graveyard." So a
        -- refusal FROM THE STACK moves the object on rather than leaving it where
        -- it was, and the two rules are told apart by `fromZone` alone -- a zone
        -- read, never the resolving card's identity. Nothing but a resolving
        -- permanent spell reaches this funnel from the stack asking for the
        -- battlefield (Pawl.Engine.Stack's two branches).
        --
        -- Through the funnel again rather than by hand, because CR 608.3e's "puts
        -- it into its owner's graveyard" is an ordinary zone change: a
        -- ReplacementEffect that exiles what would go to a graveyard applies to
        -- it. The recursion is one deep -- the second move's destination is the
        -- graveyard, and this guard asks only about the battlefield.
        --
        -- Not implemented: the graveyard id this arm answers is the card's new
        -- incarnation, and a caller that BINDS the result binds a graveyard object
        -- where it asked for a battlefield one. Unreachable today -- both stack
        -- callers void the result, and no card in data/cards/ moves a spell from
        -- the stack to the battlefield (#2869).
        Just settled
          | ZoneChange.to settled == Zone.Battlefield && EntryRestriction.prohibited oid fromZone gs ->
              if fromZone == Zone.Stack
                then changeZoneReturning oid Zone.Graveyard
                else pure Seq.empty
        Just settled -> do
          let dest = ZoneChange.to settled
              -- CR 110.2a: "If an effect instructs a player to put an object onto
              -- the battlefield, that object enters the battlefield under that
              -- player's control unless the effect states otherwise." That
              -- control is BASE STATE -- CR 110.2 makes the entry controller a
              -- permanent's default controller thereafter, which is what
              -- Projection.defaultControllerOf reads -- and not a CR 613.1b
              -- layer-2 effect, which is the distinction CR 800.4c draws and
              -- which decides whether CR 800.4a's second clause can end it
              -- (Pawl.DepartureSpec's Meandering Towershell case is the proof).
              --
              -- BATTLEFIELD ONLY, the rule's own scope (CR 110.2, CR 110.5d):
              -- Projection.controllerOf answers for an object in any zone, so an
              -- ungated write would give a graveyard card a controller.
              --
              -- Gated on the SETTLED destination rather than the requested one,
              -- so a CR 616.1 rewrite that redirects the move decides this too
              -- (CR 614.6: the modified event is what happens). The two now read
              -- different zones on a real board: synthetic-entry-interdiction
              -- sends a battlefield-bound permanent spell to a graveyard, where
              -- the request-gated reading would write an entry controller onto a
              -- graveyard card. The other direction is still unreached -- no
              -- ReplacementEffect.ZoneChangeR in data/cards/ names the
              -- battlefield as its own destination, and the one RULES-based
              -- redirect in this funnel, rule 903.9b's offerCommandZone above,
              -- answers Zone.Command.
              --
              -- CR 400.7: Object.newIncarnation is the whole forgetting -- the
              -- entry controller (CR 110.2), the as-enters choices (CR 614.1c),
              -- damage, counters, bindings and the rest all go back to their
              -- no-memory values there, and Setup's two hand-written moves into
              -- a library call the same function. What is set back here is only
              -- what this MOVE decides: the destination, CR 613.7d's moment of
              -- entry, CR 110.5b's "enters tapped" (meaningful only for a
              -- battlefield destination, CR 110.5a), CR 110.2a's entry
              -- controller, CR 701.3's attach-on-entry seed, and CR 709.3a's
              -- chosen half.
              --
              -- `face` is among what newIncarnation clears, which is right by
              -- default: whichever half CR 709.3b singled out belonged only to
              -- the incarnation that left, and CR 709.4 gives a split card its
              -- two halves combined everywhere but the stack. `shown` is the
              -- exception the rules name -- CR 709.3, where the choice of half
              -- is made BEFORE the card is put onto the stack, and CR 709.3a,
              -- where "only that half is considered to be put onto the stack".
              -- Set HERE rather than written onto the returned incarnation, so
              -- that no reader inside the move can see the CR 400.7 object
              -- without its half.
              --
              -- Gated on the move ARRIVING where it was headed, which is the
              -- reading only a writer inside the move can have: CR 709.3a's half
              -- is "considered to be put onto the stack", so a CR 616.1 redirect
              -- that settles on another destination (CR 614.6: the modified event
              -- is what happens) means the card was never put onto the stack at
              -- all, and CR 709.4 gives it its two halves combined wherever it
              -- did land. Pawl.CastSpec's "a cast redirected off the stack keeps
              -- both halves" is the proof, and fails under the pre-#781 ordering
              -- -- a stamp applied to whatever the move handed back cannot ask
              -- this question, because the caller named a destination the move
              -- was free to overrule.
              --
              -- The SETTLED destination against the REQUESTED one rather than
              -- against Zone.Stack: the rule is that the face describes the move
              -- the caller asked for, not that a face is only ever a stack half.
              -- CR 712.13's face carried OUT of the stack is that same shape with
              -- Battlefield in both slots -- "a resolving double-faced spell that
              -- becomes a permanent is put onto the battlefield with the same face
              -- up that was face up on the stack" -- and Pawl.Engine.Stack's
              -- permanent branch is what passes it, through
              -- Pawl.Engine.Card.enteringFace. Pawl.ModalDoubleFacedSpec's
              -- "casting the back face puts the artifact onto the battlefield"
              -- proves it for a modal card, and Pawl.BattleSpec's "she may then
              -- cast it TRANSFORMED and FREE" for a transforming one.
              -- CR 709.5d: "A permanent with a shared type line is given the
              -- 'left half unlocked' designation as it enters the battlefield if
              -- its left half was cast as a spell. It is given the 'right half
              -- unlocked' designation as it enters the battlefield if its right
              -- half was a cast as a spell." `shown` is that half --
              -- Pawl.Engine.Card.enteringFace carried it off the stack, exactly
              -- as CR 712.13's face is carried -- so a Room spends it on a
              -- DESIGNATION where a double-faced card spends it on `face`, and
              -- the two are mutually exclusive below.
              --
              -- Written INSIDE the move rather than stamped onto what it hands
              -- back, which is CR 709.5d's own "as it enters the battlefield":
              -- the CR 400.7 incarnation must never exist without the
              -- designation, so nothing in the CR 616.1 entry loop and nothing
              -- in the CR 709.5h trigger scan can see a Room whose doors are all
              -- shut when one of them was cast. CR 614.12 is what makes the
              -- first of those observable rather than merely tidy -- an entry
              -- replacement reads the object's characteristics "as it would
              -- exist on the battlefield", and a Room with every door shut has
              -- no text for such a replacement to be printed in. `face` above
              -- takes the same position for CR 709.3a's chosen half.
              --
              -- Gated on the BATTLEFIELD rather than on the move arriving where
              -- it was headed, which the two clauses of the rule name in as many
              -- words. A Room spell countered onto its owner's graveyard, or a
              -- CR 616.1 redirect that lands it anywhere but the battlefield, is
              -- not a permanent and has no designations (CR 709.5c).
              --
              -- Nothing to unlock when neither half was cast, which is CR
              -- 709.5d's own last sentence: "If it's entering the battlefield and
              -- neither half was cast as a spell, it enters with neither unlocked
              -- designation." A Room put onto the battlefield by an effect
              -- reaches this with `shown` Nothing and enters with both doors shut.
              unlocking = dest == Zone.Battlefield && maybe False Card.hasSharedTypeLine (Game.cardOf oid gs)
              -- CR 110.5's status the ARRIVING incarnation will carry. Named
              -- because two readers want it: mkObj's `facing` field below, whose
              -- comment has the reasoning, and the CR 303.4f gate further down,
              -- which must not ask about an Aura card entering FACE DOWN -- CR
              -- 708.2a leaves such a permanent with no subtypes and no enchant
              -- ability, so it is not an Aura the rule speaks about. That read
              -- cannot come off the projection here, which sees the object face up
              -- in the zone it is leaving.
              entryFacing = if dest == requestedDest then facing else Facing.FaceUp
              -- CR 608.3f / 707.10f: a copy of a permanent spell "will become a
              -- token permanent as it is put onto the battlefield" -- so the
              -- arriving incarnation's Source is a token's, decided INSIDE the
              -- move for the reason CR 709.5d's designation is: the CR 400.7
              -- incarnation must never exist as a copy of a spell on the
              -- battlefield, where CR 704.5e would remove it, and the CR 614.1c
              -- entry loop and every CR 603.6a trigger must already see a token
              -- entering. NOT "created" (CR 111.13): this is a zone change and
              -- not Event.createTokens, so no CR 111.3 minted-entry event and no
              -- CR 614.16 count replacement reaches it. Every other Source, and
              -- every other destination, is carried across unchanged.
              --
              -- The token keeps the copy's snapshot (CR 111.13: "the
              -- characteristics of the spell that became that token"), the way
              -- createTokens stamps a token copy's. UNOBSERVED, said plainly
              -- rather than left to look tested: every copy in the pool is of a
              -- card-backed spell, whose snapshot is the printing the token's
              -- Source already names, so dropping the stamp leaves the suite
              -- green. A copy carrying a CR 707.9b exception is what would tell
              -- them apart, and no producer in data/cards/ applies one to a spell.
              arriving = case (Object.source obj, dest) of
                (Source.OfSpellCopy printingId, Zone.Battlefield) ->
                  (Object.newIncarnation obj)
                    { Object.source = Source.OfToken printingId,
                      Object.bindings = foldMap (\pc -> Binding.setCopy pc Map.empty) (Binding.copyOf (Object.bindings obj))
                    }
                _ -> Object.newIncarnation obj
              mkObj entrySeed ts =
                arriving
                  { Object.zone = dest,
                    Object.timestamp = ts,
                    Object.tapped = tapped,
                    Object.attachedTo = entrySeed,
                    -- CR 109.4: only the stack and the battlefield give an object
                    -- a controller, so those are the two destinations that keep
                    -- the caller's player and every other clears it. The stack
                    -- arm is CR 405.4's, written by changeZoneCasting alone; the
                    -- battlefield arm is CR 110.2a's.
                    Object.enteredUnder = if dest == Zone.Battlefield || dest == Zone.Stack then under else Nothing,
                    -- `unlocking` is what excludes a Room here: CR 709.5c gives
                    -- a permanent with a shared type line designations rather
                    -- than a face that is up, and both halves go on being halves
                    -- of the one permanent (CR 709.5b), so there is no half for
                    -- this field to single out. Leaving `shown` here as well
                    -- would give Game.resolveFaceFor two answers to choose
                    -- between, one of which stops being right the moment the
                    -- second door opens.
                    Object.face = if dest == requestedDest && not unlocking then shown else Nothing,
                    Object.unlockedHalves = if unlocking then foldMap Set.singleton shown else Set.empty,
                    -- CR 708.4 / 708.3: the object is turned face down BEFORE it
                    -- is put onto the stack or enters the battlefield, so this is
                    -- part of the move rather than a stamp on what the move
                    -- returned -- CR 709.3a's `face` above, one status over.
                    --
                    -- Gated on the move ARRIVING where it was headed, for
                    -- `face`'s reason and one of its own: CR 708.9 reveals a
                    -- face-down permanent to all players as it leaves the
                    -- battlefield, so a CR 616.1 redirect that lands the object
                    -- anywhere else must leave it face up.
                    Object.facing = entryFacing,
                    -- CR 406.3: an exiled card is kept face up unless the effect
                    -- exiled it face down, so this is part of the move for
                    -- `facing`'s reason -- the card is never face up in exile
                    -- for an instant, not even for the Moved event.
                    --
                    -- Gated on the destination as well as on the move arriving
                    -- there, which the two statuses above need only the second
                    -- half of: CR 406.3 is a rule about the EXILE ZONE, so a CR
                    -- 616.1 redirect into any other zone leaves an ordinary face
                    -- up card, and an effect that names the rider on a move
                    -- somewhere else says something no rule reads.
                    --
                    -- Nothing observes either gate today: the pool's one
                    -- face-down exile names exile, and no replacement redirects
                    -- it. Both are regression fences rather than proven
                    -- behaviour -- dropping them leaves the suite green.
                    Object.exiledFaceDown = concealed && dest == requestedDest && dest == Zone.Exile,
                    -- CR 107.3m: the one thing CR 400.7's forgetting carries
                    -- across, and the rule states it as an exception. The value
                    -- of X for a permanent's enters-the-battlefield replacement
                    -- effect is the value chosen for the spell that became it,
                    -- so the announcement is read off the DEPARTING object's
                    -- CR 601.2b binding -- which `newIncarnation` has just
                    -- cleared -- and written onto the arriving one.
                    --
                    -- Read here rather than passed in by Pawl.Engine.Stack,
                    -- because the funnel already holds the object that has the
                    -- binding, and the rule is about any object that entered
                    -- the battlefield as a resolving spell rather than about
                    -- one caller's route.
                    --
                    -- BATTLEFIELD ONLY, the rule's own scope: a countered spell
                    -- on its way to a graveyard becomes a card, and CR 107.3g
                    -- puts the X of a card outside the stack at 0. Nothing for
                    -- every move whose object bound no X, which is every move
                    -- but a resolving {X} permanent spell's.
                    --
                    -- That gate is a regression fence rather than proven
                    -- behaviour: both readers are entry replacements, which ask
                    -- it of a permanent, so dropping the gate leaves the suite
                    -- green.
                    Object.announcedX = if dest == Zone.Battlefield then Binding.amountOf Binding.variableX (Object.bindings obj) else Nothing,
                    -- CR 400.7d: "an ability of a permanent can reference
                    -- information about the spell that became that permanent as
                    -- it resolved, including WHAT COSTS WERE PAID to cast that
                    -- spell." Rule 702.33d's designation is exactly such a cost
                    -- record, and Monstrous War-Leech's CR 614.1c rewrite is the
                    -- ability that references it -- asked of the PERMANENT, since
                    -- the permanent is what enters.
                    --
                    -- So this is `announcedX` above's second instance rather than
                    -- a new idea: `newIncarnation` has just cleared the record with
                    -- everything else CR 400.7 forgets, and the exception the rule
                    -- names is written back here, off the departing object.
                    --
                    -- BATTLEFIELD ONLY, and here the gate carries weight where
                    -- announcedX's does not: a kicked spell countered on its way
                    -- to a graveyard becomes a card, and rule 400.7d speaks only
                    -- about a permanent. Nothing else reads the record off an
                    -- object outside the stack.
                    Object.kicked = if dest == Zone.Battlefield then Object.kicked obj else Map.empty,
                    -- CR 702.103b: "these effects last until the SPELL OR THE
                    -- PERMANENT IT BECOMES ceases to be bestowed", so the
                    -- designation crosses this one move with the object the rule
                    -- is still about. `kicked`'s route above, but for a stronger
                    -- reason than rule 400.7d's look-back: this is not a memory
                    -- of the cast, it is the effect still running.
                    --
                    -- BATTLEFIELD ONLY, `kicked`'s gate and for a reason the rule
                    -- states rather than implies: what a bestowed spell becomes
                    -- anywhere else is a card, and a card is no bestowed Aura.
                    Object.bestowed = Object.bestowed obj && dest == Zone.Battlefield,
                    -- CR 400.7d a third time, and rule 702.150a is the ability
                    -- that references it: how many of the spell's Phyrexian mana
                    -- symbols were announced to be paid with life (CR 601.2b).
                    --
                    -- BATTLEFIELD ONLY, `kicked`'s gate and for its reason: rule
                    -- 702.150a is about a permanent entering, and a countered
                    -- spell on its way to a graveyard becomes a card no compleated
                    -- ability can be on.
                    Object.phyrexianLifePaid = if dest == Zone.Battlefield then Object.phyrexianLifePaid obj else 0,
                    -- CR 400.7d a fourth time, and this is the clause the rule
                    -- names last: "what mana was spent to pay those costs". CR
                    -- 107.4h's third sentence is what references it, so Berg
                    -- Strider's "if {S} was spent to cast this spell" is asked of
                    -- the permanent that spell became.
                    --
                    -- BATTLEFIELD ONLY, `kicked`'s gate and for its reason: rule
                    -- 400.7d speaks about a permanent, and a countered spell
                    -- becomes a card no such ability can be on.
                    Object.manaSpent = if dest == Zone.Battlefield then Object.manaSpent obj else Mana.MkMana []
                  }
              -- CR 604.2's override, handed over as the permanent leaves the
              -- battlefield. lingeringHandover below is the whole of it; this
              -- road gates on the zone the object is LEAVING, which is what
              -- makes an ordinary zone change pay nothing for the read.
              handover = if fromZone == Zone.Battlefield then lingeringHandover oid lastController gs else []
          -- CR 303.4f: an Aura entering the battlefield "by any means other than
          -- by resolving as an Aura spell", where "the effect putting it onto the
          -- battlefield doesn't specify the object or player the Aura will
          -- enchant". That second clause IS `seed` being Nothing: a door supplies
          -- one exactly when the effect it carries named a destination -- CR
          -- 303.4a's target for Pawl.Engine.Stack's Aura branch, which is also the
          -- only door a resolving Aura spell takes, and the fixed host for
          -- Pawl.Engine.Resolve.Effect.putFound's search destination (Auratouched Mage).
          -- So the rule's own exclusion is carried without this funnel asking what
          -- kind of move it is.
          --
          -- Gated on the SETTLED destination, `unlocking`'s reading one binding up:
          -- a CR 616.1 redirect that sends the Aura anywhere but the battlefield
          -- (CR 614.6: the modified event is what happens) must not raise the
          -- prompt.
          --
          -- Asked of `gs`, the PRE-MOVE board, which is where the Aura still is and
          -- where the hosts already are. Attach.hostsFor sweeps the battlefield and
          -- Attach.attachmentFor reads Projection.subtypesOf and Game.faceOf, both
          -- of which answer for an object in any zone.
          --
          -- MINUS `batch`, which is CR 608.2f: the members of a batch enter
          -- simultaneously, so a would-be host arriving beside this Aura is not on
          -- the battlefield at the moment CR 303.4f's choice is made, however the
          -- funnel happens to order the two moves. Replenish over a graveyard
          -- holding an Aura and an enchantment creature is the board
          -- (Pawl.AuraSpec). Subtracting the SET rather than sweeping the `asOf`
          -- board: the set names exactly "entered beside me", where that board
          -- would also re-offer a host the batch itself removed.
          --
          -- Filter.CanHostSubject ALONE, because CR 303.4f's "a legal object or
          -- player according to the Aura's enchant ability and any other applicable
          -- effects" is the whole restriction -- there is no card text to intersect
          -- it with, which is the difference from CR 303.4k's Attach.turnUpHosts.
          --
          -- The Aura test is the PROJECTION's subtypes (CR 205.3 -- CR 303.4 speaks
          -- about characteristics) rather than the printed type line
          -- Pawl.Engine.Card.isAura reads, and the two now differ: Cloudform GRANTS
          -- itself the Aura subtype (Modification.AddSubtype) and the enchant
          -- ability that goes with it (Modification.GainEnchant). Unobserved here
          -- all the same -- Cloudform grants both while already on the battlefield,
          -- so no board in this pool has a granted Aura ENTERING one.
          --
          -- `entryFacing` and not the projection is what answers CR 708.2a, and the
          -- distinction matters: the projection sees the object in the zone it is
          -- LEAVING, where an Aura card manifested off a library (CR 701.40a) is
          -- still face up and would trip this gate. A permanent entering face down
          -- has no subtypes and no enchant ability, so it is not an Aura CR 303.4f
          -- speaks about -- it enters unattached, and Sba.fallsOff leaves it alone,
          -- reading a projection the substituted face seeds. Pawl.AuraSpec's "a manifested
          -- Aura card is not an Aura" is the proof: Soul Summons manifests an Unholy
          -- Strength over two legal hosts, and dropping the conjunct raises a prompt.
          --
          -- Answering Nothing is CR 303.4g's "the Aura remains in its current
          -- zone": this funnel's CR 614.6 cancel arm above, one case over, so the
          -- object is never deleted and never re-minted (CR 400.7) and the graveyard
          -- card is the same object it always was.
          --
          -- Not implemented: CR 303.4g's other branch, for an Aura whose current
          -- zone is the stack or that is a token, and CR 303.4i's effect that names
          -- an attachment the Aura can't legally enchant (gap #1734).
          settledSeed <-
            if dest == Zone.Battlefield && entryFacing == Facing.FaceUp && Maybe.isNothing seed && Set.member Subtype.Aura (Projection.subtypesOf oid gs)
              then do
                -- CR 303.4f's "that player" is CR 110.2a's entry controller, which
                -- is `under` -- falling back to the owner when the effect named
                -- nobody, per CR 110.2 and CR 108.4a, exactly as the field's own
                -- note above says.
                let chooser = Maybe.fromMaybe pid under
                    hosts = filter (\h -> not (Set.member h batch)) (Attach.hostsFor chooser oid oid Filter.Type.CanHostSubject gs)
                chosen <- Attach.chooseHost chooser oid hosts
                -- THE TAG the Aura's own enchant slot produced, never a hand-built
                -- ToObject: Sba.stillLegalEnchant compares the (pool, tag) pair, so
                -- a ToObject stored where the slot offers a ToCreature falls through
                -- and CR 704.5m buries the Aura on the next pass.
                --
                -- attachmentFor answering Nothing collapses into CR 303.4g's
                -- "remains in its current zone" too, and is unreachable rather than
                -- a second reading: hostsFor's Filter.CanHostSubject conjunct is
                -- that same function, so every candidate it offered admits.
                pure (fmap Just (chosen >>= \h -> Attach.attachmentFor oid (Recipient.ToObject h) gs))
              else pure (Just seed)
          case settledSeed of
            Nothing -> pure Seq.empty
            Just entrySeed -> do
              State.modify' $ \g ->
                let g1 = Game.removeFromZones pid oid g
                 in g1
                      { GameState.objects = Map.delete oid (GameState.objects g1),
                        -- Stored as the permanent goes, in the same write that
                        -- removes it: nothing can observe a board where the
                        -- permanent has left and its effect has not yet been handed
                        -- over.
                        GameState.continuousEffects = handover <> GameState.continuousEffects g1,
                        -- CR 608.2h: the object ceases here, so this is the last
                        -- moment its information is known. Filed under the id it had
                        -- while it existed -- the id an ability on the stack still
                        -- carries as its source (CR 113.7) -- and from the same
                        -- `snapshot` the Moved event below records, so the two
                        -- readings of "what was it" cannot drift apart.
                        --
                        -- The counters come off `obj`, the PRE-MOVE object, and not
                        -- off the incarnation `mkObj` builds: CR 122.2 makes them
                        -- cease to exist on the zone change, so the last moment they
                        -- can be recorded is this one.
                        --
                        -- The COPIABLE snapshot is taken here for the counters'
                        -- reason: it is layer 1 (CR 613.1a) rather than the fold
                        -- `snapshot` is, and the copy binding and face it reads live
                        -- on `obj`, which is about to cease. No third board walk --
                        -- it reads that binding or the printed face and stops.
                        --
                        -- What was ATTACHED is read off `lki`, the pre-batch board
                        -- `snapshot` and `lastController` read, and not the live
                        -- `gs`: in a simultaneous batch an Equipment with a lower
                        -- id moves before its host, and the live board has already
                        -- forgotten it. Pawl.ZoneTriggerSpec's "the Equipment dying
                        -- in the same batch, ahead of its host" is the proof.
                        GameState.lastKnown = Map.insert oid (LastKnown.MkLastKnown snapshot lastController (Object.owner obj) (Object.source obj) (Object.counters obj) (copiedSnapshot oid gs) (Game.attachments oid lki) (Object.chosenNames obj) (Game.isBlocking oid gs) (Object.protector obj)) (GameState.lastKnown g1)
                      }
              -- CR 712.21: "If a melded permanent leaves the battlefield, one
              -- permanent leaves the battlefield and two cards are put into the
              -- appropriate zone." So the leave is one event and the arrival is
              -- one CR 400.7 incarnation PER COMPONENT, each an ordinary card
              -- object naming its own printing.
              --
              -- Read through Game.componentsOf, a classifier over Source and
              -- never a case on Source.OfMeld: CR 730.3 restates this rule for a
              -- merged permanent, so mutate (#874) extends that one function
              -- rather than this branch.
              --
              -- The rule's own scope, both halves of it: FROM the battlefield
              -- ("if a melded permanent leaves the battlefield") and NOT back
              -- onto it ("two cards are put into the appropriate zone"). The
              -- origin conjunct is the rule's condition and the destination
              -- conjunct is what keeps the CR 616.1 entry loop, the CR 122.6a
              -- entering counters and CR 709.5d's designations below reading one
              -- arrival, since none of them can be reached on the split path.
              --
              -- The origin conjunct is not redundant with the destination one
              -- today only because nothing can hold Source.OfMeld outside the
              -- battlefield -- Event.meld places the melded permanent there and
              -- this split is the one road off it. That is an invariant no type
              -- enforces, so the rule's own condition is written out rather than
              -- rested on.
              --
              -- Each component goes through the same `mkObj`, so CR 400.7's
              -- forgetting (Object.newIncarnation) is what the two cards arrive
              -- with; only Object.source differs, since the cards represent
              -- themselves again and not the permanent they were.
              --
              -- CR 712.21d needs nothing here: the CR 616.1 replacement loop
              -- ran ONCE above, against the melded permanent, so a replacement
              -- one player chose settles the destination for both cards --
              -- "applying one of those replacement effects to one of the two
              -- cards affects both cards". Leyline of the Void, the rule's own
              -- Example, is the pool's producer, and its ZoneChangeR names a
              -- destination and an owner rather than card-ness, so it applies to
              -- the melded permanent and both cards follow it to exile.
              --
              -- Not implemented: CR 712.21b's relative timestamp order on exile,
              -- which is the EXILING player's and not the owner's -- the two
              -- cards are stamped in the order arrangeComponents leaves them,
              -- which on that path is the order they melded in (#2508).
              let components = if fromZone /= Zone.Battlefield || dest == Zone.Battlefield then Seq.empty else Game.componentsOf (Object.source obj)
                  -- CR 903.9c, the split offerCommandZone above settled: "that
                  -- permanent and each component representing it that isn't a
                  -- commander are put into the appropriate zone, and the card
                  -- that represents it and is a commander is put into the command
                  -- zone". `splitOff` is Nothing on every other move, and the
                  -- partition then leaves every component headed for `dest`
                  -- exactly as CR 712.21 alone does.
                  --
                  -- The event's own `to` still names the zone the move was
                  -- headed for; the card split off announces its OWN arrival
                  -- below, as a GameEvent.CardArrived naming the command zone,
                  -- which is how CR 903.9c's two arrivals in two different zones
                  -- are both said.
                  (commandComponents, destComponents) = Seq.partition (\component -> Just component == splitOff) components
                  asComponent zone mComponent ts =
                    ( case mComponent of
                        Nothing -> mkObj entrySeed ts
                        Just component -> (mkObj entrySeed ts) {Object.source = Source.OfCard component}
                    )
                      { Object.zone = zone
                      }
              -- CR 712.21a, asked BEFORE the first placeObject: the arrangement is
              -- the order the cards are put down in, so nothing can be placed
              -- until it is settled.
              --
              -- Over the components that actually reach `dest`, which is the
              -- rule's own "the two cards": a melded commander whose owner took
              -- CR 903.9b's offer puts ONE card into the library, and one card
              -- has one arrangement.
              arranged <- arrangeComponents pid dest destComponents
              let (leading, trailing) = case Seq.viewl arranged of
                    Seq.EmptyL -> (Nothing, Seq.empty)
                    c Seq.:< cs -> (Just c, cs)
              newId <- placeObject pid (asComponent dest leading) dest position
              trailingIds0 <- Monad.forM trailing (\component -> placeObject pid (asComponent dest (Just component)) dest position)
              -- CR 408.1's command zone is one shared area rather than one per
              -- player, so placeObject's `pid` decides nothing about where the
              -- card lands here; it is Object.owner all the same, which is rule
              -- 903.9b's "its owner" for a stolen commander.
              commandIds <- Monad.forM commandComponents (\component -> placeObject pid (asComponent Zone.Command (Just component)) Zone.Command position)
              let trailingIds = trailingIds0 <> commandIds
              -- `newId` heads the answer, so a caller that can only act on one
              -- object acts on the first card the arrangement named -- the first
              -- the meld recorded, where nobody was asked for one.
              let arrivals = newId Seq.<| trailingIds
              -- CR 400.7a, and BEFORE the entry loop below rather than after this
              -- funnel returns: CR 614.12 decides an entry row against "continuous
              -- effects that already exist and would apply to the permanent", so a
              -- text change made to the permanent SPELL has to name the new
              -- incarnation before its own row is read. Pawl.ReplacementSpec's
              -- Tidewalker case is the proof: hacked Island -> Swamp on the
              -- stack, its own "enters with a time counter for each Island"
              -- counts Swamps.
              --
              -- Gated on the SETTLED destination, because both exceptions
              -- carryOver makes are for "the permanent that spell becomes" (CR
              -- 400.7a, CR 400.7c) and a permanent spell a CR 614.6 redirect
              -- sent anywhere else becomes no permanent. CR 608.3e is the
              -- rulebook's own case of the same shape. Pawl.ReplacementSpec's
              -- "a redirected permanent spell carries nothing over" is the
              -- proof.
              Monad.when (dest == Zone.Battlefield) (carryOver carrying oid newId)
              -- Alchemy's "perpetually", an exception to CR 400.7 that no
              -- rule of the CR states. UNGATED, unlike carryOver above: the printed
              -- sentence follows the object through every zone change and in
              -- both directions, so the destination decides nothing here and
              -- Pawl.Types.CarryOver is not consulted either -- a perpetual
              -- effect follows an object the caller did not mean to carry
              -- anything else over for.
              --
              -- `arrivals` and not `newId`, which is the other half of the same
              -- contrast: CR 712.21c gives an effect that finds what a melded
              -- permanent became BOTH cards, so a perpetual effect follows the
              -- arrangement's trailing card too.
              perpetuate oid arrivals
              -- CR 701.24a: the redirect said to shuffle the card into a library,
              -- and it is in one now -- the earliest point at which "shuffle it
              -- into its owner's library" is a whole action rather than half of
              -- one. AFTER placeObject for that reason, and before the Moved
              -- event so nothing outside the move sees an unshuffled library.
              --
              -- Ungated on the settled destination, which is CR 701.24c and CR
              -- 701.24d rather than a shortcut: the library is shuffled even when
              -- the object is not in it. Unreachable today in any case -- no row
              -- in data/cards/ watches a destination another row has already
              -- moved into a library.
              --
              -- The OWNER's library, CR 400.3: every zone this funnel places into
              -- is keyed by owner, and `pid` is the id placeObject was handed.
              Monad.when shuffling (shuffleLibrary pid)
              -- CR 614.1c-d: entry replacements apply to BATTLEFIELD entries and
              -- nowhere else. CR 616.1g's nesting of one event inside another is
              -- expressed as call nesting rather than a field. `batch` is the
              -- same-batch siblings, empty for every door but changeZoneEnteringIn
              -- (CR 614.12a; see applyReplacementsIn for why 614.12a and not
              -- 614.13a).
              Monad.when (dest == Zone.Battlefield) $ do
                -- CR 122.6a: the counters the EFFECT says the object enters with --
                -- undying's and persist's "with a +1/+1 counter on it". Into the
                -- pending map before the entry loop, so the loop's own CR 616.1 pool
                -- can scale them and flushEnteringCounters places what it settles on
                -- -- exactly as the EntryRewrite.WithCounters arm inside the loop
                -- does. Still inside the move and before the Moved event below, so
                -- nothing outside the entry can see the permanent without them (the
                -- tap state's reason, one field over).
                --
                -- Before the loop rather than during it, which no card observes: no
                -- entry replacement in the pool reads a counter the entering object
                -- already has, and the two are simultaneous in the rules anyway.
                --
                -- That claim is WIDER than it was. These counters used to be on the
                -- object by the time the loop began; now they sit pending for the
                -- loop's whole duration, so a row reading them would answer
                -- differently at every iteration rather than only before the first.
                -- Checked when the pending map landed: Filter.HasCounters and
                -- Replacement.admitsEntry are two readers a row could reach a
                -- counter through, and a filter naming power or toughness is a
                -- third, via Projection.counterGathered (CR 614.12); none of the
                -- three is fed by an entering permanent's own pending counters
                -- today.
                Monad.mapM_ (uncurry (addEnteringCounters newId)) (Map.toAscList entering)
                runEntry batch newId
              -- CR 709.5h: an ability that triggers on a door opening fires "regardless
              -- of whether it was given that designation while entering the
              -- battlefield or after entering the battlefield", so the entry
              -- designation `unlocking` wrote into mkObj above needs its event too.
              -- Recorded rather than routed through unlockHalves, which would find the
              -- door already open and record nothing: writing the designation inside
              -- the move is what CR 709.5d's "as it enters" asks for, and the event is
              -- what CR 709.5h asks for -- two rules, and the entry is the one place
              -- they are not the same write.
              --
              -- BEFORE the Moved event, so a Room's own "when you unlock this door"
              -- and its "when this enters" are gathered in one scan with the door's
              -- event first. The two are simultaneous and CR 603.3b lets their
              -- controller order them on the stack, so nothing observable rides on
              -- which is logged first.
              --
              -- CR 709.5i's flag is computed here too, through the same
              -- `fullyUnlockedAfter` unlockHalves uses, and against the designations
              -- `mkObj` actually wrote. Reading `shown` back rather than the stored
              -- object, so the two writers answer the question the same way from the
              -- same input. Always False on THIS route, and that is CR 709.5d rather
              -- than a shortcut: an entry gives at most ONE designation, so a
              -- two-door Room can never arrive fully unlocked. CR 709.5i's second
              -- branch is reached from unlockHalves instead, which can give both at
              -- once.
              --
              -- The ACTOR is CR 110.2a's entry controller, the `chooser` above:
              -- rule 709.5d gives the designation with no player taking an action,
              -- and the permanent's own controller is the only player the rule
              -- connects to it -- which is also the player a Room's own "when you
              -- unlock this door" reads as "you" (CR 109.5).
              Monad.forM_ (if unlocking then Maybe.maybeToList shown else []) $ \half ->
                State.modify' (recordEvent (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked newId (Maybe.fromMaybe pid under) half (fullyUnlockedAfter (foldMap Set.singleton shown) (Game.cardOf oid gs)))))
              -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
              -- what an enters trigger scans -- alongside the id it had in `fromZone`,
              -- which is the key `lastKnown` is filed under and so the only route back
              -- once CR 400.7 has minted a new incarnation (CR 603.10a's look-back
              -- reads it). Recorded LAST, so the entry loop's choices are locked in
              -- before any trigger or SBA can observe the object.
              -- CR 608.3c and CR 303.4f: a permanent that ARRIVES attached became
              -- attached, which is the half Event.attach cannot record -- there is
              -- no CR 701.3 move here, the seed goes on the incarnation `mkObj`
              -- mints and the id it names did not exist a moment ago. Bramble
              -- Elemental's "whenever an Aura becomes attached to this creature"
              -- fires for a cast Pacifism through this line and for Crown of the
              -- Ages through the other one.
              --
              -- Gated on the SETTLED destination, `unlocking`'s reading: a seed
              -- rides along on every move (mkObj writes the field whatever the
              -- zone), and only the battlefield has attachments. CR 614.6's
              -- redirect elsewhere leaves it unread and unrecorded.
              --
              -- Carries `newId`, the CR 400.7 incarnation, rather than the id the
              -- Aura spell had on the stack: that is the object a trigger scan
              -- will find attached.
              Monad.forM_ (if dest == Zone.Battlefield then entrySeed else Nothing) $ \host ->
                State.modify'
                  . recordEvent
                  $ GameEvent.BecameAttached
                    BecameAttached.MkBecameAttached
                      { BecameAttached.attachment = newId,
                        BecameAttached.host = host
                      }
              -- CR 607.2b: this move ended in exile because somebody's replacement
              -- effect sent it there, so the arriving card is linked to THAT
              -- object. Filed here, at the one place the CR 400.7 incarnation
              -- first has an id, and before the Moved event so nothing can read
              -- the arrival with the link missing.
              --
              -- Resolve.recordExiledWith's diff runs afterwards and would
              -- otherwise file this same card against whatever effect was
              -- running; its insertWith keeps the entry already present, which is
              -- what makes this write the one that stands.
              --
              -- Every arrival is linked, not just the first: CR 712.21c gives an
              -- effect that finds what a melded permanent becomes both cards, and
              -- CR 607.2b's link is exactly such a finding.
              Monad.forM_ (if dest == Zone.Exile then exiledBy else Nothing) $ \linked ->
                Monad.forM_ arrivals $ \arrival ->
                  State.modify' (\g -> g {GameState.exiledWith = Map.insert arrival linked (GameState.exiledWith g)})
              -- ONE Moved event for the whole move, which is CR 712.21's first
              -- clause: one permanent leaves the battlefield. `newId` is the
              -- first arrival, so a trigger reading the destination end of this
              -- event finds one of the two cards, and the rest are announced by
              -- the CardArrived events below.
              --
              -- CR 712.21e's two halves come out of that split: an effect that
              -- needs the number of OBJECTS that changed zones folds the Moved
              -- events one at a time and sees one, and one that needs the number
              -- of CARDS folds this event and every CardArrived below and sees
              -- two (Pawl.Engine.Count.snapshotView, Pawl.MeldSpec).
              --
              -- The OTHER arrivals ride along in Moved.others as well as in the
              -- CardArrived events below, which is CR 712.21c: "if an effect can
              -- find the new object that a melded permanent becomes as it leaves
              -- the battlefield, it finds both cards." eventBindings reads them
              -- through Moved.arrivals and binds CR 400.7e's `became` slot as a
              -- group, so a trigger's payload acts on each card. Empty for every
              -- move but this one.
              State.modify'
                . recordEvent
                . GameEvent.Moved
                $ Moved.MkMoved
                  { Moved.change = ZoneChange.MkZoneChange oid newId fromZone dest,
                    Moved.characteristics = snapshot,
                    Moved.others = trailingIds
                  }
              -- CR 712.21's second clause: "two cards are put into the
              -- appropriate zone". One event per card AFTER the leading one,
              -- which the Moved event above already announces -- so CR 712.21's
              -- Example comes out as written, a "whenever a creature dies"
              -- ability seeing one event and a "whenever a card is put into a
              -- graveyard from anywhere" ability seeing two.
              --
              -- The departing id is the melded permanent's, as the Moved event's
              -- is: one permanent left, and CR 608.2h's record is filed under
              -- that id for both.
              --
              -- Each card's OWN destination rather than the move's, CR 903.9c
              -- splitting a melded commander's two cards across two zones.
              --
              -- No characteristics snapshot: the departing permanent's is on the
              -- Moved event beside this, and the arriving CARD is a live object
              -- in a public zone for a trigger to read.
              Monad.forM_ (fmap ((,) dest) trailingIds0 <> fmap ((,) Zone.Command) commandIds) $ \(zone, arrival) ->
                State.modify'
                  . recordEvent
                  . GameEvent.CardArrived
                  $ ZoneChange.MkZoneChange oid arrival fromZone zone
              pure arrivals

-- CR 712.21a: "if a melded permanent is put into its owner's graveyard or
-- library, that player may arrange the two cards in any order." Answers the
-- components in the order they are to be PUT INTO the zone;
-- Prompt.OrderComponentCards says how that maps onto which card ends up on top.
--
-- The OWNER is asked, which is the rule's "that player": the sentence's subject
-- is the melded permanent put into ITS OWNER's graveyard or library, and
-- changeZoneAttaching's `pid` is Object.owner for exactly the reason
-- Game.insertIntoZone keys those two zones by it (CR 400.3).
--
-- Read off `dest` and never off the source's identity: the two named zones are
-- the rule's own condition, and CR 730.3a restates the whole sentence for a
-- merged permanent (#874), which reaches this through the same Seq.
--
-- Exile is NOT here. CR 712.21b gives that case to the EXILING player and asks
-- about relative timestamps rather than an arrangement, so it is a different
-- question of a different player (#2508).
--
-- Not asked for fewer than two cards, which is not an elision: with one card
-- there is one arrangement, and with none there is nothing to arrange.
--
-- FILTERED, NOT TRUSTED: Game.permute keeps the melded order for an answer that
-- is not a permutation of the offered indices.
arrangeComponents :: PlayerId -> Zone -> Seq.Seq PrintingId.PrintingId -> Game (Seq.Seq PrintingId.PrintingId)
arrangeComponents pid dest components =
  if Seq.length components < 2 || (dest /= Zone.Graveyard && dest /= Zone.Library)
    then pure components
    else do
      gs <- State.get
      let offered = Foldable.toList components
      answer <- Game.choose (Prompt.OrderComponentCards (Decide.deciderFor pid gs) pid dest offered)
      pure (Seq.fromList (Game.permute offered answer))

-- CR 400.7a: effects that change a permanent spell's characteristics or
-- controller keep applying to the permanent it becomes. CR 400.7 mints a fresh
-- id for that permanent, so an effect stored against the SPELL's id would stop
-- naming it; this re-keys the ones that do.
--
-- THE INVARIANT: no case on any effect's identity. Every stored
-- ContinuousEffect qualifies by CLASSIFICATION -- Projection.layer puts each
-- modification in one of CR 613.1's layers, and every layer either changes a
-- characteristic (CR 109.3) or the controller (CR 613.1b), which are exactly
-- what this rule carries over.
--
-- Only Affected.TheseObjects is re-keyed, because that is the only arm a
-- resolution effect ever stores (CR 611.2c locks the set); the dynamic arms
-- belong to static abilities and are re-derived each projection.
--
-- CR 400.7c is the SECOND half, and rides the same re-keying: a prevention
-- effect that applies to damage from a permanent spell keeps applying to damage
-- from the permanent it becomes. CR 609.7a says the same thing from the choice's
-- side -- "if the player chooses a permanent spell, the effect will apply to any
-- damage dealt by that spell and any damage dealt by the permanent that spell
-- becomes when it resolves" -- and that is the rule the pool's producer plays
-- to: Auriok Replica's chosen source, since Resolve.damageSourceCandidates
-- offers the spells on the stack.
--
-- Called INSIDE changeZoneAttaching, before the CR 614.1c entry loop and only
-- where the settled destination is the battlefield, and Pawl.Types.CarryOver is
-- how the move says whether CR 400.7's exception is the one it is making. Scoped
-- to Pawl.Engine.Stack's two permanent-spell branches. Not implemented: CR
-- 400.7b's static-ability ability grants (CR
-- 611.3d), which this carrier cannot reach at all -- a static grant is derived
-- on every projection rather than stored, so there is no row here to re-key
-- (#2425). CR 400.7i is unimplemented too, on a carrier this one never sees --
-- the land-play path (gap #2398). CR 400.7g is implemented, on
-- Pawl.Engine.Cast.keywordsBefore rather than here.
carryOver :: CarryOver.CarryOver -> ObjectId -> ObjectId -> Game ()
carryOver carrying oldId newId = case carrying of
  CarryOver.NotCarried -> pure ()
  CarryOver.Carried ->
    State.modify' $ \gs ->
      gs
        { GameState.continuousEffects = fmap (reanchor oldId (Set.singleton newId)) (GameState.continuousEffects gs),
          GameState.replacements = fmap (rewatch oldId newId) (GameState.replacements gs)
        }

-- carryOver's per-ROW half, and CR 400.7c's whole of it: a floating damage row
-- watching the spell watches the permanent instead. DamagePattern.whichSource is
-- the one field that can name a permanent spell -- CR 609.7a's chosen source is
-- baked into it, and the rule's own last sentence is what follows the choice
-- across the zone change. Its `whichRecipient` neighbour cannot hold one: CR
-- 120.3's recipient is a player or a permanent, and a spell is neither.
--
-- DamageR is the only arm of Pawl.Types.ReplacementEffect carrying a
-- DamagePattern, which is what makes the fallthrough below exhaustive rather
-- than lazy -- a new arm would have to grow a baked id before it wanted one.
--
-- THE INVARIANT: no case on any effect's identity. DamageR is CR 614.1a's
-- CLASSIFICATION of a replacement -- which damage events it intercepts -- and
-- the arms below ask nothing else.
--
-- The row's TARGETED sibling in the same field (Dovin, Hand of Control's "dealt
-- by target permanent") is unreachable here rather than a second reading: a
-- target is declared on the stack against a permanent (CR 601.2c), so no id it
-- holds is ever a permanent spell's.
--
-- PlayerEffect.DamageCantBePrevented and its CR 614.9 twin carry the same
-- pattern type and are left alone: neither sentence is a prevention effect, so
-- CR 400.7c does not speak to them, and Pawl.CardSpec's engineOnlyOffends keeps
-- card data off that field anyway, so no row of either can name an object at
-- all. Lava Burst's Filter.IsSource is the other half of the same field and is
-- not re-keyed either -- it names a SORCERY spell, which becomes no permanent.
rewatch :: ObjectId -> ObjectId -> ActiveReplacement.ActiveReplacement -> ActiveReplacement.ActiveReplacement
rewatch oldId newId row = case ActiveReplacement.effect row of
  ReplacementEffect.DamageR damage
    | DamagePattern.whichSource (DamageR.matching damage) == Just oldId ->
        let pattern_ = DamageR.matching damage
         in row {ActiveReplacement.effect = ReplacementEffect.DamageR damage {DamageR.matching = pattern_ {DamagePattern.whichSource = Just newId}}}
  _ -> row

-- Alchemy's "perpetually": the stored effects that FOLLOW their objects across a
-- zone change, re-anchored to every incarnation that just arrived. No rule of the
-- CR names the word -- it is digital-only, and the printed sentence is the
-- authority -- so the citation here is for what the rules DO settle: CR 400.7
-- makes the arriving object a new one with no memory of the old, which is why
-- every other stored effect is left pointing at an id nothing answers to, and CR
-- 611.2c is why there is a FIXED id set here to rewrite rather than a filter to
-- re-derive. Neither rule licenses the rewrite; only the printed word does.
--
-- carryOver's shape and reanchor's rewrite, with two differences. It is not
-- gated on Pawl.Types.CarryOver or on the destination, because the sentence
-- names neither: the effect follows the card onto the battlefield, into a
-- graveyard, into a hand and back out again. And it re-anchors only the
-- CONTINUOUS effects, not the replacements beside them -- CR 400.7c's carrier is
-- a different sentence, and no printed perpetual rider is a prevention effect.
--
-- Pawl.Engine.Expiry.follows is asked rather than a case on the arm, since that
-- module is the only one that may case on Pawl.Types.Expiry.
--
-- EVERY arrival and not just the leading one, which is CR 712.21c: "if an effect
-- can find the new object that a melded permanent becomes as it leaves the
-- battlefield, it finds both cards ... if that effect causes actions to be taken
-- upon those cards, the same actions are taken upon each of them". Following an
-- object across the zone change is finding what it became, so a CR 712.21a
-- arrangement's trailing card is named as well as its leading one. That is a
-- THIRD difference from carryOver beside it, which names the head alone because
-- CR 400.7a's exception is about the permanent a SPELL becomes and a spell melds
-- into nothing. Pawl.MeldSpec's departure case is the proof.
--
-- meld calls this too, one component at a time -- the same shape read backwards,
-- two departures with one arrival each. Its own comment has the reason.
perpetuate :: ObjectId -> Seq.Seq ObjectId -> Game ()
perpetuate oldId newIds =
  State.modify' $ \gs ->
    gs
      { GameState.continuousEffects = fmap follow (GameState.continuousEffects gs)
      }
  where
    arrivals = Set.fromList (Foldable.toList newIds)
    follow eff =
      if Expiry.follows (ContinuousEffect.expiry eff)
        then reanchor oldId arrivals eff
        else eff

-- carryOver's and perpetuate's per-effect half: swap oldId for the arriving ids
-- in a locked affected set that names it, and leave every other effect alone. A
-- SET of new ids rather than one, because CR 712.21's split makes a single
-- departure into two arrivals; carryOver passes a singleton, since the road it
-- gates is the one a melded permanent cannot take -- CR 712.21's own condition is
-- a departure FROM the battlefield, and carryOver runs only for an arrival ONTO
-- it.
reanchor :: ObjectId -> Set ObjectId -> ContinuousEffect.ContinuousEffect Card -> ContinuousEffect.ContinuousEffect Card
reanchor oldId newIds eff = case ContinuousEffect.affected eff of
  Affected.TheseObjects oids
    | Set.member oldId oids ->
        eff {ContinuousEffect.affected = Affected.TheseObjects (Set.union newIds (Set.delete oldId oids))}
  _ -> eff

-- CR 604.2 ends a static ability's continuous effect the moment its permanent
-- leaves the battlefield. A card whose own text overrides that -- Titania's
-- Song's "If this enchantment leaves the battlefield, this effect continues
-- until end of turn" -- needs the effect to outlive the ability generating it,
-- and the only way an effect outlives its source is to become a STORED one (CR
-- 611.2). So the abilities this permanent prints with a StaticAbility.lingers
-- duration are handed over here, as it goes.
--
-- Everything is read from `gs`, the board the permanent is still on and still
-- projecting from, so what is handed over is the effect that was actually
-- applying. CR 611.2c then fixes its set for good: the noncreature artifacts
-- Titania's Song names at THIS instant, never the live filter -- an artifact
-- that enters after the Song is gone is not animated. Pawl.ExpirySpec's
-- whole-card case proves both halves, and its Humility case is the control for
-- the gate: an ability with no such clause hands over nothing and ends at once.
--
-- The timestamp is the departing permanent's own, the one CR 613.7a already gave
-- this effect, rather than a fresh one: the card says "this effect CONTINUES",
-- so it is the same effect and nothing reorders it against its neighbours within
-- its layers. CR 613.7b's fresh timestamp belongs to an effect the resolution of
-- a spell or ability creates, and nothing resolved here.
--
-- ONE stored effect per PART, since a stored effect carries a single
-- modification. CR 613.6's "one set for the whole effect" survives that split by
-- construction: frozenStaticParts freezes every part of an ability to the same
-- objects, so asking each of them separately can only get the same answer back.
--
-- `lastController` is CR 109.5's "you" for the arming, read from the same
-- pre-departure board.
--
-- THREE roads reach this, because a permanent leaves the battlefield three ways:
-- changeZoneAttaching above for a zone change, and two that take it out of the
-- GAME without one -- Pawl.Engine.Departure.objectsLeaveWith for CR 800.4a's
-- first clause, and Pawl.Engine.Setup.applyCrossings for CR 729.4a's card a
-- subgame brought in. No gate is written here: this function is asked about a
-- permanent that has already been decided to be leaving, and each caller decides
-- that its own way -- the funnel off Object.zone, and both roads out of the game
-- off GameState.battlefield membership, which is CR 702.26b on those: a
-- phased-out permanent is treated as though it does not exist, so it was
-- generating no effect to continue -- CR 702.26k still takes it out of the game
-- with its owner.
--
-- Short-circuited on the cheap COPIABLE read (Projection.staticAbilitiesOf,
-- which is projection-free), so an ordinary zone change -- every one in the pool
-- but this card's -- never pays for the projection frozenStaticParts spends.
--
-- That read and not the printed face, because `n` is CR 613.6's index into the
-- list Projection.permanentParts tagged, and that walk reads the copiable list
-- (CR 707.2a). Indexing a different list here would join the wrong ability's
-- duration onto the frozen part, with no type error to say so.
lingeringHandover :: ObjectId -> PlayerId -> GameState -> [ContinuousEffect.ContinuousEffect Card]
lingeringHandover oid lastController gs =
  let lingering :: [(Natural, Duration.Duration)]
      lingering =
        [ (n, duration)
        | (n, sa) <- zip [0 ..] (Projection.staticAbilitiesOf oid gs),
          duration <- Maybe.maybeToList (StaticAbility.lingers sa)
        ]
   in if null lingering
        then []
        else
          [ ContinuousEffect.MkContinuousEffect
              { ContinuousEffect.source = oid,
                ContinuousEffect.timestamp = ts,
                ContinuousEffect.expiry = expiry,
                ContinuousEffect.modification = modification,
                ContinuousEffect.affected = Affected.TheseObjects frozen
              }
          | (n, ts, modification, frozen) <- Projection.frozenStaticParts oid gs,
            duration <- fmap snd (filter ((== n) . fst) lingering),
            -- No bindings to bake a CR 611.2b condition against: this duration is
            -- a PRINTED static ability's, and no resolution chose anything for it.
            expiry <- Maybe.maybeToList (Expiry.arm Map.empty lastController oid duration gs)
          ]

-- The single destruction funnel (CR 701.8 / 702.12b): the Destroy opcode and the
-- CR 704.5g/h state-based actions both flow through here.
--
-- Takes the WHOLE BATCH rather than one permanent at a time, because CR 608.2f
-- and CR 704.3 make the batch one simultaneous event. A lone destruction is the
-- one-element batch.
--
-- CR 702.12b's indestructible gate is judged for every member against the state
-- the batch began in, BEFORE any of them moves -- which is why this takes a list.
-- A permanent granting the others indestructible is still on the battlefield at
-- that moment even when it is itself in the batch, so it dies alone. Judging each
-- member against a board its predecessors had left would make the answer depend on
-- an order CR 608.2f gives nobody the right to decide.
--
-- The gate also precedes the replacement loop, per CR 614.7: an event that never
-- happens neither applies nor consumes a regeneration shield. Otherwise the
-- would-be-destroyed event is offered to CR 616.1, and a survivor goes to its
-- owner's graveyard via changeZone. Ungated for CR 701.19c (#42).
--
-- The door for a batch that is a whole event whose caller does not care what died;
-- destroyReturning is the same door for a caller that does, destroyInBatch for a
-- batch nested in a larger event, and destroyIn is the shared body.
--
-- This door and destroyReturning are the EFFECT's, which is why neither takes a
-- Pawl.Types.DestructionCause: a caller with a whole event of its own to destroy in
-- is a resolving spell or ability (CR 609.1), and CR 122.1c's shield reads that.
-- The rules' own destructions come through destroyInBatch, which asks.
destroy :: Regenerability.Regenerability -> [ObjectId] -> Game ()
destroy regenerability oids = Monad.void (destroyIn Nothing DestructionCause.ByEffect regenerability oids)

-- destroy, answering with the permanents it ACTUALLY destroyed (CR 701.8b), which
-- is emphatically not the batch it was handed: an indestructible member never
-- reaches the destruction event (CR 702.12b) and a regenerated one has it replaced
-- (CR 701.8c), so neither is in this answer.
--
-- Each surviving destruction reports the SETTLED object rather than the one asked
-- about, for the reason the graveyard move follows it -- a CR 616.1 rewrite may
-- redirect the destruction. Nothing in the pool does, so the lists are equal today.
--
-- PAIRED with the CR 400.7 incarnation the graveyard move minted, which is a
-- different object from the one destroyed and the only one a later clause can
-- name: "put into a graveyard this way" is a card in a graveyard, and the
-- permanent that was destroyed no longer exists. Nothing for a move the CR 616.1
-- loop cancelled, and the id alone says nothing about WHERE it landed -- a CR
-- 614 replacement may have sent it to exile instead (Rest in Peace), which is
-- why the reader asks the board rather than trusting this answer's presence.
--
-- A second door rather than a return type on `destroy`, the changeZoneReturning
-- posture: only the Destroy opcode's bound slots use the answer.
destroyReturning :: Regenerability.Regenerability -> [ObjectId] -> Game [(ObjectId, Seq.Seq ObjectId)]
destroyReturning = destroyIn Nothing DestructionCause.ByEffect

-- destroy for a batch that is one PART of a larger simultaneous event, whose board
-- is `asOf`. CR 704.3's state-based-action check is that event, and Sba is the
-- only caller: its put-into-graveyard and destruction batches are a sequence only
-- in the implementation, so both stand on the board the pass began in -- an
-- animated Rest in Peace the pass itself buries still exiles the card of the
-- creature the pass destroys.
--
-- A separate door rather than a parameter on `destroy`, for changeZoneInBatch's
-- reason: every other caller has no larger event to name.
--
-- The one door that ASKS for the Pawl.Types.DestructionCause, because it is the one
-- whose caller may be a rule: CR 704.5g/h destroy with no effect involved, which is
-- what CR 122.1c's replacement does not reach.
destroyInBatch :: GameState -> DestructionCause.DestructionCause -> Regenerability.Regenerability -> [ObjectId] -> Game ()
destroyInBatch asOf cause regenerability oids = Monad.void (destroyIn (Just asOf) cause regenerability oids)

-- The shared body. Three readers of a board, and they do NOT all get the same one:
--
--   1. The CR 616.1 replacement loops -- the destruction's and the
--      put-into-graveyard that follows -- collect from `gs`, the containing
--      event's board. That is CR 608.2f / 704.3's "single event" reading: the
--      effects in force are the ones that existed before it, so an effect
--      belonging to a permanent the same event is removing still applies.
--   2. The CR 702.12b gate reads `gs` too. Indestructibility is a fact about the
--      permanent when the event's conditions were judged; letting an earlier part
--      of the same event change it would make the answer depend on an order CR
--      608.2f gives nobody the right to decide.
--   3. The existence filter reads `live`, NOT `gs`, per CR 614.7: an object an
--      earlier part of the event already put into a graveyard is not on the
--      battlefield to be destroyed, so no destruction event happens for it and no
--      regeneration shield may be offered one. The reachable shape is an Aura
--      named by CR 704.5m and CR 704.5g in the same pass.
--
-- CR 122.1c is the first DestructionR for which (1) is not vacuous: a shield
-- counter's replacement is minted from the permanent holding the counter
-- (Projection.shieldOf), so the destruction loop finds it on the FROZEN board rather
-- than on the live one. Every other DestructionR in the pool is a regeneration
-- shield in the floating store, which the frozen board does not hold at all.
--
-- The whole body is ONE event, which is what `simultaneously` stamps on
-- everything it records: CR 608.2f makes an action taken on multiple objects
-- simultaneous, and every caller here hands a batch that one effect named. That
-- is the same sentence the three readings above already rest on -- a batch judged
-- against one board, not a sequence -- so the bracket adds no claim they do not.
-- Day of Judgment's deaths are the case it decides, and CR 603.10a's own Example
-- is why they must not be a sequence.
destroyIn :: Maybe GameState -> DestructionCause.DestructionCause -> Regenerability.Regenerability -> [ObjectId] -> Game [(ObjectId, Seq.Seq ObjectId)]
destroyIn asOf cause regenerability oids = simultaneously $ do
  live <- State.get
  let gs = Maybe.fromMaybe live asOf
      doomed = filter (\oid -> Maybe.isJust (Game.lookupObject oid live) && not (Projection.hasKeyword Keyword.Type.Indestructible oid gs)) oids
  fmap Maybe.catMaybes . Monad.forM doomed $ \oid -> do
    settled <- resolveDestruction (Just gs) cause regenerability oid
    case settled of
      -- CR 701.8c: a regeneration effect REPLACED the destruction, so nothing was
      -- destroyed here and this member is not in the answer.
      Nothing -> pure Nothing
      -- The graveyard move follows the SETTLED object, so a rewrite redirecting
      -- the destruction is honoured. changeZone is a no-op for an object already
      -- gone, which is what makes naming the batch's members up front safe.
      Just target -> do
        arrived <- changeZoneInBatchReturning gs target Zone.Graveyard
        pure (Just (target, arrived))

-- The single countering funnel (CR 701.6a -- not to be confused with putCounters
-- above, CR 122.6's counter markers).
--
-- Two endings, because that rule's last sentence is about a SPELL and its first
-- two about "a spell or ability". Which applies is decided by Game.isAbility, a
-- classification of the object's kind and never a question about the countering
-- effect:
--
--   * a SPELL goes to its owner's graveyard through the changeZone funnel, so
--     Rest in Peace's redirect and CR 400.7's new incarnation still compose;
--   * an ABILITY ceases (CR 608.2n). Not a zone change: an ability has no owner's
--     graveyard, nothing arrives anywhere, and there is no destination for CR 614
--     to replace.
--
-- The ability branch records NO event, so no trigger can watch it (#541). Widening
-- GameEvent.SpellCountered is the wrong direction -- its one reader asks about
-- countering A SPELL and must stay silent here. What a countered ability does
-- leave is counterReturning's answer, which is the RESOLUTION's own tally (Glen
-- Elendra's Answer's "countered this way") and not a look-back at the history.
--
-- TWO "can't be countered" gates, one per carrier. CR 101.2 makes either the whole
-- story: the countering effect resolves and does nothing. Neither is targeting
-- immunity -- neither rule grants shroud -- which is why the gates are here and
-- not in Target. They precede the zone change for destroy's CR 614.7 reason.
--
--   * CR 113.6g's, read off the SPELL's own card (Rending Volley), since there is
--     no battlefield projection of a spell. Self-referential by that rule's own
--     wording -- "an object's ability that states IT can't be countered" -- so it
--     is asked only on the spell branch: an ability on the stack has no card for
--     it to be printed on, and Game.faceOf answers Nothing for one.
--   * CR 611.1 / 613.11's, asked of Pawl.Engine.PlayerEffect (Spider-Punk). That
--     one is an ability of a BATTLEFIELD PERMANENT about other objects, so it is
--     gathered from the battlefield like any other player effect and reaches BOTH
--     of CR 701.6a's subjects -- which is why it, unlike the gate above, is asked
--     ahead of the branch split rather than inside one branch. It is asked of the
--     VICTIM's controller: CR 113.8 for an ability, CR 601.2a for a spell.
--
-- On the spell branch, records a SpellCountered ALONGSIDE the zone change's Moved
-- event, never instead of it: the Moved event is the CR 400.7 change and this one
-- is what the change WAS. That is what distinguishes a countered spell from one
-- that RESOLVED into the same graveyard (CR 608.2n), and what survives Rest in
-- Peace redirecting the destination.
--
-- Nothing is recorded on any of the paths that do NOT counter, which CR 603.2g
-- makes mandatory rather than tidy: an id with no object; any of the three
-- can't-be-countered gates, since through CR 101.2 such a spell was never
-- countered; and a move the CR 616.1 loop cancelled, which leaves the spell on
-- the stack. The ability branch is not one of them -- that countering really
-- happened, and its silence is #541.
--
-- `source` and `controller` are the countering spell or ability and its controller
-- (CR 405.4), taken from the caller rather than re-derived: by the time the CR
-- 117.5 scan reads this event the controller can no longer be asked for exactly
-- (see Pawl.Types.Countering).
counter :: ObjectId -> PlayerId -> ObjectId -> Game ()
counter source controller oid = Monad.void (counterOne source controller oid)

-- counter over a whole batch, answering with the objects it ACTUALLY countered
-- (CR 701.6a) -- which is emphatically not the batch it was handed: an id naming
-- no object, any can't-be-countered gate, and a move the CR 616.1 loop
-- cancelled each leave their victim out of the answer.
--
-- The VICTIMS as the caller named them, not the graveyard incarnations CR 400.7
-- mints: Swift Silence's "draw a card for each spell countered this way" and
-- Glen Elendra's Answer's "for each spell and ability countered this way" want
-- how many, which Pawl.Engine.Resolve binds as an amount, and Green Slime's "if
-- a permanent's ability is countered this way" wants WHICH, walked to its CR
-- 113.7 source under the id the caller named. An ability leaves no new object
-- at all (CR 608.2n), so there is no second id to report for it -- which is why
-- the answer is the victims and not the incarnations.
--
-- A second door rather than a return type on `counter`, the destroyReturning
-- posture: only the Counter opcode's two bound slots use the answer.
counterReturning :: ObjectId -> PlayerId -> [ObjectId] -> Game [ObjectId]
counterReturning source controller = Monad.filterM (counterOne source controller)

-- The shared body of both doors: counter ONE object, answering whether it was.
counterOne :: ObjectId -> PlayerId -> ObjectId -> Game Bool
counterOne source controller oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure False
    -- CR 613.11's gate first, ahead of the branch split, because it is the one
    -- that reaches both of CR 701.6a's subjects.
    Just _ | protectedFromCountering oid gs -> pure False
    -- CR 106.6 through CR 101.2, and ahead of the branch split for CR 613.11's
    -- reason: rule 106.6 says an additional effect "affects the spell or
    -- ability that mana is spent on", so both of CR 701.6a's subjects are in
    -- reach. Both records exist: Pawl.Engine.Cost's recordSpent writes a cast's
    -- units onto the spell and an activation's onto the CR 602.2a ability object.
    --
    -- The typed question again, so this module never sees a ManaRiderEffect
    -- constructor; Pawl.Engine.ManaRider is where the casing lives.
    Just _ | ManaRider.uncounterable oid gs -> pure False
    -- CR 608.2n, reached before the CR 113.6g gate because that gate asks about a
    -- spell's own card and an ability has none -- Game.faceOf answers Nothing for
    -- one, so asking first would fall through to the graveyard move by accident.
    Just _ | Game.isAbility oid gs -> do
      State.modify' (Game.cease oid)
      pure True
    Just _ -> case fmap Face.counterability (Game.faceOf oid gs) of
      Just Counterability.CantBeCountered -> pure False
      _ -> do
        moved <- changeZoneReturning oid Zone.Graveyard
        if Seq.null moved
          then pure False
          else do
            State.modify'
              . recordEvent
              $ GameEvent.SpellCountered
                Countering.MkCountering
                  { Countering.spell = oid,
                    Countering.source = source,
                    Countering.controller = controller
                  }
            pure True

-- CR 601.2c: record that everything just announced as a target BECAME one --
-- "the chosen objects and/or players each become a target of that spell. (Any
-- abilities that trigger when those objects ... become the target of a spell
-- trigger at this point ...)". CR 602.2b and CR 603.3d route an activated and a
-- triggered ability through the same rule, so all three announcement sites call
-- this and none of them knows what watches. Pawl.Engine.Resolve's CopyStackObject arm
-- is the fourth caller and the one that announces nothing: CR 115.1 declares a
-- spell's targets as part of putting it on the stack, and CR 707.10c puts the
-- copy there with the targets its controller settled on.
--
-- ONE EVENT PER SLOT PER RECIPIENT, which is rule 601.2c's own arity: that rule
-- lets one object be chosen once for each instance of the word "target" and
-- forbids it twice within one instance, so the per-slot sets are walked rather
-- than unioned and a spell naming the same permanent under two instances records
-- two.
--
-- Players are kept, CR 115.1 making a player a target in its own right: Dormant
-- Gomazoa watches its controller become one, and rule 601.2c names objects and
-- players in the same breath, so one event over a Recipient beats two events.
--
-- The KIND is passed in rather than derived. Rule 601.2c's parenthetical is about
-- spells (CR 112.1) while CR 602.2b and CR 603.3d bring abilities (CR 113.3) to
-- the same step, and only the caller knows which it is putting on the stack;
-- the matcher that later reads this has no GameState to ask.
--
-- CALLED AFTER THE ANNOUNCEMENT SUCCEEDS rather than at rule 601.2c's own
-- position in the sequence. The rule puts the trigger before the costs are paid
-- but holds the ability off the stack "until the spell has finished being cast",
-- and CR 601.2 rewinds the whole announcement if it does not -- so a trigger
-- recorded here and one recorded earlier differ only in a case where the earlier
-- one would have to be taken back.
becameTarget :: ObjectId -> StackObjectKind.StackObjectKind -> PlayerId -> Map.Map SlotName.SlotName (Set.Set Recipient.Recipient) -> Game ()
becameTarget source kind controller chosen =
  Foldable.for_ (concatMap Set.toAscList (Map.elems chosen)) $ \targeted ->
    State.modify'
      . recordEvent
      $ GameEvent.BecameTarget
        BecameTarget.MkBecameTarget
          { BecameTarget.targeted = targeted,
            BecameTarget.source = source,
            BecameTarget.kind = kind,
            BecameTarget.controller = controller
          }

-- CR 701.3b and CR 701.3c: store the attachment, restamp, and record it.
--
-- CR 303.4j for an Aura -- "the Aura doesn't move" -- and CR 701.3b's first
-- sentence for the rest. A FAILURE MODE, not a fizzle: the only thing that does
-- not happen is the move, and in particular the subject stays attached to its old
-- host rather than becoming unattached, so CR 704.5m has nothing to bury. Nothing
-- is recorded on that branch either, which is CR 701.3b in as many words -- the
-- permanent did not become attached.
--
-- CR 701.3b's SECOND sentence is checked here rather than left to the caller:
-- attaching a permanent to the object or player it is already attached to "does
-- nothing", so there is no restamp and no event. Two of the three callers cannot
-- reach it -- Attach.hostsFor never offers the current host -- and CR 702.6a's
-- equip, whose target is any creature its controller controls, can.
--
-- CR 701.3c: attaching to a DIFFERENT object gives it a new timestamp, which CR
-- 613.7 orders layer effects by.
--
-- LIVES HERE rather than in Pawl.Engine.Attach, which is where the legality
-- reading it asks (Attach.attachmentFor) still lives: Event imports Attach, so a
-- recordEvent call from there would invert the edge. All three call sites -- this
-- module's CR 303.4k rewrite and Pawl.Engine.Resolve's Attach and AttachTarget
-- opcodes -- already see this module.
--
-- The event carries the tag attachmentFor produced rather than the caller's
-- `destination`, so a reader sees the same Recipient Object.attachedTo holds.
attach :: ObjectId -> Recipient.Recipient -> Game ()
attach subject destination = do
  gs <- State.get
  case Attach.attachmentFor subject destination gs of
    Nothing -> pure ()
    Just attachment -> Monad.unless (fmap Object.attachedTo (Game.lookupObject subject gs) == Just (Just attachment)) $ do
      let (ts, gs1) = Game.freshTimestamp gs
          move o = o {Object.attachedTo = Just attachment, Object.timestamp = ts}
      State.put gs1 {GameState.objects = Map.adjust move subject (GameState.objects gs1)}
      State.modify'
        . recordEvent
        $ GameEvent.BecameAttached
          BecameAttached.MkBecameAttached
            { BecameAttached.attachment = subject,
              BecameAttached.host = attachment
            }

-- CR 611.1 / 613.11: does a rules-modifying continuous effect stop this spell or
-- ability from being countered (Spider-Punk, Prowling Serpopard)? The victim's
-- controller is the player the effect is anchored against -- CR 113.8 for an
-- ability on the stack, CR 601.2a for a spell -- and an object with no
-- controller is protected by nothing. The victim's own id rides along too, since
-- a narrowed effect names the victim's characteristics ("creature spells you
-- control can't be countered") and not only its controller.
--
-- The typed question, so this module never sees a PlayerEffect constructor;
-- Pawl.Engine.PlayerEffect.cantBeCountered is where the casing lives.
--
-- Projection.controllerOf, which is what every other reader of an affected
-- object's controller already asks (PlayerEffect.matchesObject, and
-- Replacement.chooserOf for CR 616.1's chooser). For a
-- SPELL that is a re-derivation rather than the stored fact CR 405.4 describes,
-- and it falls back to the owner (#83); a spell cast from a zone its owner does
-- not hold would therefore be read against the wrong player here.
protectedFromCountering :: ObjectId -> GameState -> Bool
protectedFromCountering oid gs =
  maybe False (\pid -> PlayerEffect.cantBeCountered pid oid gs) (Projection.controllerOf oid gs)

-- CR 701.21/701.21a: the single sacrifice funnel. The permanent goes to its
-- OWNER's graveyard through changeZone, and -- unlike destroy -- with no
-- indestructible gate and no regeneration shield consulted, since sacrificing is
-- not destroying. Restricted to permanents on the battlefield, so anything else is
-- a no-op.
--
-- TWO refusals live here, and the arms below cite each: CR 701.21a's "a permanent
-- they don't control", and CR 101.2's "can't" beating a rule or effect's "can" --
-- a permanent under a Pawl.Types.SacrificeRestriction. Both are asked at the
-- funnel, so a caller that named a victim without consulting
-- Pawl.Engine.Replacement.sacrificeCandidates cannot get past either.
--
-- CR 701.21a also forbids sacrificing a permanent you do not control, which is why
-- this takes the sacrificing player. Enforced here at the one funnel rather than
-- trusted from each caller: a cost payment, a triggered ability's own source and
-- `apply`'s CR 614.1c as-enters sacrifice are controlled by the paying player by
-- construction, but an edict's victim is a permanent a PLAYER named.
--
-- Records GameEvent.PermanentSacrificed, which is what makes CR 603.10a's
-- "abilities that trigger when a player sacrifices a permanent" expressible: CR
-- 700.4 makes a sacrifice a death, so the Moved event changeZone writes below is
-- the one a destruction writes too and cannot say what the move WAS. Written HERE
-- rather than at each caller for the reason the controller check is: this is the
-- one funnel.
--
-- BEFORE the zone change, and naming the PRE-MOVE id, which is CR 603.10a's
-- look-back. CR 701.21a's sacrifice is the game action, so a replacement that
-- redirects the move -- Rest in Peace exiling it instead -- does not un-sacrifice
-- the permanent, and an event recorded afterwards would either name an
-- incarnation the redirection never produced or not be recorded at all.
sacrifice :: PlayerId -> ObjectId -> Game ()
sacrifice pid oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Object.zone obj of
      -- CR 701.21a: "A player can't sacrifice something that isn't a permanent, or
      -- something that's a permanent they don't control." The zone case below is
      -- the first clause; this is the second. Enforced HERE, at the one funnel,
      -- rather than trusted from each caller -- the callers are a cost payment, a
      -- trigger's own source, `apply`'s CR 614.1c as-enters sacrifice, and an
      -- edict whose victim a player NAMED, and only the last of those could ever
      -- be wrong. Resolve's Effect.Sacrifice road asks the same question of the
      -- permanent before it gets here (Resolve.sacrificerFor), since a
      -- printed "its controller sacrifices it" names a player this arm could only
      -- refuse.
      Zone.Battlefield
        | Projection.controllerOf oid gs /= Just pid -> pure ()
        -- CR 101.2: "if a rule or effect allows or directs something to happen,
        -- and another effect states that it can't happen, the 'can't' effect
        -- takes precedence" -- Garland, Royal Kidnapper's "can't be sacrificed".
        -- CR 101.3 says what happens instead: "any part of an instruction that's
        -- impossible to perform is ignored", so nothing moves, no event is
        -- recorded, and no destruction is substituted.
        --
        -- The SECOND of the two gates, and it earns its place at the funnel:
        -- Effect.Sacrifice names its victims through an ObjectRef and consults
        -- no candidate list at all, which is how Lightning Skelemental's "at the
        -- beginning of the end step, sacrifice this creature" reaches here.
        -- Pawl.SacrificeRestrictionSpec proves that path goes through this arm
        -- and no other -- gating only the candidate list leaves it green.
        --
        -- The other two callers that name a victim outright answer CR 101.2
        -- before they get here as well, and for their own reasons:
        -- Pawl.Engine.Cost refuses the SacrificeThis cost so CR 118.3 is not
        -- broken, and Pawl.Engine.Sba drops a CR 704.5s Saga so CR 704.3's
        -- repeat terminates. This arm is the backstop under both.
        | SacrificeRestriction.prohibited oid gs -> pure ()
        | otherwise -> do
            State.modify' (recordEvent (GameEvent.PermanentSacrificed (PermanentWasSacrificed.MkPermanentWasSacrificed pid oid)))
            changeZone oid Zone.Graveyard
      Zone.Library -> pure ()
      Zone.Hand -> pure ()
      Zone.Graveyard -> pure ()
      Zone.Stack -> pure ()
      Zone.Exile -> pure ()
      -- CR 408.1: a command-zone object is not a permanent, so it is never
      -- sacrificed.
      Zone.Command -> pure ()

-- CR 111.2: create `n` tokens with the given effect-defined characteristics under
-- `controller`'s control, summoning-sick (CR 302.6). A token is created from
-- nothing, so changeZone cannot mint it. Uses from = Battlefield, where to == from
-- can never read as a leave, and emits the enters event so CR 603.6a triggers fire
-- on the path a resolved permanent uses.
--
-- Plural by rules requirement, not convenience: CR 614.1/614.16 replacements scope
-- to the CREATION EVENT rather than each token, so the count is settled once up
-- front. Every token is then materialized, and only then does each run its OWN
-- entry loop -- CR 616.1g's containment, since creating a token contains that
-- token entering. Each entry loop is handed the whole batch, which excludes
-- simultaneously-entering siblings from any copy choice (CR 614.12a; see
-- applyReplacementsIn for why 614.12a and not 614.13a).
--
-- A token whose text is GIVEN has empty `replacementEffects`, so its entry loop
-- returns immediately; a token copy of a Clone carries the copied permanent's own
-- `EntryR AsCopy`, which is CR 616.1g's worked example and what makes the nesting
-- observable. Pawl.CopySpec's "none may copy a sibling" is the proof: deleting
-- the loop below leaves five 0/0 Clones for CR 704.5f to bury.
--
-- ANSWERS [] rather than the ids it was asked for on three roads, which every
-- caller must read: CR 800.4b's departed player and CR 111.5's prohibition below,
-- plus resolveTokens declining. Pawl.Engine.Resolve's Create arm binds no slot
-- for an empty answer and Pawl.Engine.Amass re-reads the board for CR 701.47b's
-- "impossible choice", so both already say the right thing.
--
-- CR 800.4b / 800.4d: no token is created for a player who has left the game. The
-- two sentences coincide by CR 111.2, which makes a token's owner and controller
-- the same player, so one guard satisfies both. Checked BEFORE resolveTokens: the
-- rule says no token is created, so nothing may be minted and nothing spent
-- getting there -- resolveTokens consumes CR 614.3 use counts.
--
-- Reading the PARAMETER rather than the post-entry controller is sufficient, not
-- merely convenient: the entry loop below can move a token with a CR 616.1b
-- control rewrite, but CR 800.4a's second clause ends such a row when its
-- controller leaves (Departure.givesControlOnEntryTo), so no surviving row can
-- rewrite the token onto a departed player.
--
-- Inline rather than delegating to a `createTokensFor` body: the project writes no
-- export lists, so a second top-level name would be a public door past the check.
--
-- The Maybe snapshot is CR 707.1's other kind of token -- one whose text is a
-- copy of a permanent rather than given (`copiedSnapshot`) -- and it comes
-- through this one funnel rather than a second so that CR 614.16's count
-- replacement, CR 800.4b's departed-player guard and each token's own CR 614.12
-- entry loop are the same code for both kinds.
--
-- `entering` is CR 122.6a's counters the effect says each token enters the
-- battlefield with -- CR 701.53a's incubate -- and it is passed beside `tapped`
-- rather than as a whole EntryRiders for the reason changeZoneAttaching keeps them
-- apart too: the record's other riders are answered elsewhere or are inert here
-- (CR 111.2 for `underOwner`, CR 508.4 for `attacking`, CR 712.14a's card-only
-- scope for `transformed`), and handing this funnel the record would read as
-- though it applied them.
createTokens :: PlayerId -> Card -> Maybe PC.ProjectedCharacteristics -> Natural -> TapState.TapState -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural -> Game [ObjectId]
createTokens controller card copy n tapped entering = do
  gs <- State.get
  if List.notElem controller (Game.stillPlaying gs)
    then pure []
    else do
      resolved <- resolveTokens controller (Seq.singleton TokenLot.MkTokenLot {TokenLot.card = card, TokenLot.copy = copy, TokenLot.count = n})
      case resolved of
        Nothing -> pure []
        Just (owner, lots) -> do
          -- CR 111.5's rollback point below, captured AFTER resolveTokens rather
          -- than before it: rule 614.3's use counts are spent by a replacement
          -- that applied to the creation event, and rule 111.5 unmakes the token
          -- rather than the event.
          unminted <- State.get
          -- ONE lot came in; several come out when a CR 614.1a append (Queen
          -- Allenal of Ruadach) put another shape into the event. Every lot is
          -- minted here, in the one batch, before any token runs its entry loop.
          ids <- fmap concat . Monad.forM (Foldable.toList lots) $ \lot -> do
            -- Interned ONCE per lot, not once per token: a lot's tokens are
            -- copies of one set of effect-defined characteristics (CR 111.3), so
            -- they name one entry.
            tokenId <- State.state (Game.intern (Printing.MkPrinting (TokenLot.card lot)))
            let mkObj ts =
                  Object.MkObject
                    { Object.owner = owner,
                      Object.enteredUnder = Nothing,
                      Object.source = Source.OfToken tokenId,
                      Object.zone = Zone.Battlefield,
                      -- CR 110.5b: untapped unless an effect says otherwise, which
                      -- is why the caller supplies this rather than the default
                      -- being taken and the token tapped after.
                      Object.tapped = tapped,
                      -- CR 110.5b: face up, for the same rule's reason. No effect
                      -- in the pool creates a token face down.
                      Object.facing = Facing.FaceUp,
                      Object.exiledFaceDown = False,
                      Object.damage = 0,
                      Object.sickness = Sickness.Sick,
                      -- CR 707.2 / 111.3: a token copy's copiable values are the
                      -- copied permanent's, stamped into the layer-1 snapshot the
                      -- projection starts from -- the same binding the CR 614.1c
                      -- entry rewrite writes, and the same read
                      -- (Projection.copiableCharacteristics) on the way out.
                      -- Written HERE rather than after the entry loop because CR
                      -- 614.12 asks for the characteristics the permanent would
                      -- have on the battlefield, and for this token those are the
                      -- copy's from the instant it exists.
                      Object.bindings = maybe Map.empty (\pc -> Binding.setCopy pc Map.empty) (TokenLot.copy lot),
                      Object.counters = Map.empty,
                      Object.counterTimestamps = Map.empty,
                      Object.attachedTo = Nothing,
                      Object.chosenColor = Nothing,
                      Object.chosenSubtype = Nothing,
                      Object.chosenNames = Set.empty,
                      Object.chosenPlayer = Nothing,
                      Object.timestamp = ts,
                      Object.face = Nothing,
                      Object.turnedOverAt = Nothing,
                      Object.worldSince = Nothing,
                      Object.playableFromExile = Nothing,
                      Object.plotted = Nothing,
                      Object.foretold = Nothing,
                      Object.ringBearerFor = Nothing,
                      Object.protector = Nothing,
                      Object.ventureRoom = Nothing,
                      Object.classLevel = Nothing,
                      Object.unlockedHalves = Set.empty,
                      Object.designations = Set.empty,
                      Object.kicked = Map.empty,
                      Object.bestowed = False,
                      Object.phyrexianLifePaid = 0,
                      Object.manaSpent = Mana.MkMana [],
                      Object.announcedX = Nothing,
                      Object.castFrom = Nothing,
                      Object.detainedUntil = Set.empty,
                      Object.goadedBy = Set.empty,
                      Object.doesNotUntapNext = False,
                      Object.exertedBy = Set.empty
                    }
            Monad.replicateM (Natural.toIntSaturating (TokenLot.count lot)) (placeObject owner mkObj Zone.Battlefield LibraryPosition.defaultValue)
          minted <- State.get
          -- CR 111.5: "if a spell or ability would create a token, but a rule or
          -- effect states that a permanent with one or more of that token's
          -- characteristics can't enter the battlefield, the token is not
          -- created." The same prohibition changeZoneAttaching asks of a move,
          -- asked here because a token takes no move -- it is minted straight onto
          -- the battlefield -- so the funnel's gate cannot see it.
          --
          -- MINTED AND ROLLED BACK rather than asked of a hypothetical, because
          -- the question is about characteristics the projection answers and
          -- Pawl.Engine.Projection reads an object that exists. Nothing observes
          -- the interval: placeObject writes only GameState.objects and the zone
          -- index, and every road out of this function that could be seen -- the
          -- CR 122.6a counters, each token's CR 616.1 entry loop, the CR 111.3
          -- minted-entry event -- is below this gate. So a refused batch leaves
          -- the board exactly as `unminted` had it, which is rule 111.5's "the
          -- token is not created" and not "a token that dies at once".
          --
          -- Zone.Battlefield is the ORIGIN passed, which is the field's own
          -- reading (Pawl.Types.EntryRestriction.origins: the zone the object is
          -- in when the move is judged) and not a sentinel. It also decides which
          -- printings reach a token, correctly: a prohibition that NAMES zones
          -- names off-battlefield ones -- Grafdigger's Cage's graveyard and
          -- library -- so it cannot reach a token, while one that names no zone
          -- carries the full set and does. Worms of the Earth's "lands can't enter
          -- the battlefield" is the pool's second kind.
          if any (\tok -> EntryRestriction.prohibited tok Zone.Battlefield minted) ids
            then do
              State.put unminted
              pure []
            else do
              -- CR 122.6a: the counters the EFFECT says these tokens enter with, gathered
              -- into the pending map -- so CR 614.16 applies inside each token's own entry
              -- loop and Vorinclex sees them -- exactly as changeZoneEntering's door does
              -- for the move that carries the same rider.
              --
              -- The WHOLE BATCH is dressed before any token runs its entry loop, rather
              -- than each token being dressed and then entered. That order buys CR 614.12
              -- nothing any more and is kept only because it costs nothing: what the
              -- rule's "characteristics of the permanent as it would exist on the
              -- battlefield" reads is the OBJECTS, and these counters are no longer on
              -- them -- Pawl.Engine.Projection cannot see GameState.enteringCounters, and
              -- the map is per-object, so no sibling's loop reads another's pending
              -- counters whenever they were written. What CR 614.12 does rest on is
              -- `placeObject` above having materialized every token first, which is what
              -- `siblingsOf` then hands each entry loop.
              let siblingsOf oid = Set.delete oid (Set.fromList ids)
              Monad.mapM_ (\oid -> Monad.mapM_ (uncurry (addEnteringCounters oid)) (Map.toAscList entering)) ids
              Monad.mapM_ (\oid -> runEntry (siblingsOf oid) oid) ids
              -- No prior incarnation to snapshot, so a token's last known information
              -- IS what it is now (CR 111.3). Recorded after every entry loop, so the
              -- events describe settled objects.
              Monad.mapM_ recordMintedEntry ids
              pure ids

-- Nothing departed, so `departed` is the arrival's own id. Harmless rather than a
-- fiction readers must know about: from == to == Battlefield already fails every
-- departure test (CR 603.6c), and neither road that reaches this has a
-- `lastKnown` entry to find -- a token has no prior incarnation (CR 111.3), and a
-- conjured card was in no zone.
recordMintedEntry :: ObjectId -> Game ()
recordMintedEntry newId = do
  placed <- State.get
  let snapshot = Projection.project newId placed
  State.modify' (recordEvent (GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange newId newId Zone.Battlefield Zone.Battlefield) snapshot)))

-- CR 701.42a: meld these cards -- "put them onto the battlefield with their back
-- faces up and combined. The resulting permanent is a single object represented by
-- two cards". Answers the id the melded permanent arrived under, or Nothing where
-- CR 701.42b refuses, which by CR 701.42c leaves every card in the zone it is in.
--
-- Here rather than beside Pawl.Engine.Resolve's opcode arm for placeObject's and
-- runEntry's sake: this is a battlefield ENTRY, so it mints an object, writes a
-- zone index and runs CR 616.1's entry loop, and those three are this module's
-- and are reached from nowhere else. createTokens is the model throughout -- a
-- permanent that no single prior incarnation maps onto -- and the differences are
-- the two rules meld has and CR 111.2 does not: the result is interned from the
-- COMBINED BACK FACE the ability carried (CR 712.8g), and the cards that were
-- melded stop being objects.
--
-- The EXILE is not here and is not the caller's either: rule 701.42a is only the
-- putting-onto-the-battlefield, and Hanweir Battlements' "exile them, then meld
-- them" is an ordinary earlier instruction. That separation is what makes CR
-- 701.42c's Graf Rats example come out right with no rollback -- a refusal below
-- does nothing at all, so the cards stay wherever that instruction left them.
--
-- CR 712.14c ("those cards enter the battlefield as a single permanent with their
-- back faces up") is why runEntry runs: the entry replacement loop of rule 616.1
-- and the enters-the-battlefield trigger scan must both see one permanent enter.
-- The back faces are up by construction -- CR 712.4b leaves a meld card's own half
-- of the oversized face meaningless alone, so pawl prints neither half's back and
-- the interned result IS the combined face, face up on its only face.
meld :: PlayerId -> [ObjectId] -> Card -> Game (Maybe ObjectId)
meld controller victims resultCard = do
  gs <- State.get
  case meldable victims gs of
    -- CR 701.42b/701.42c. Nothing is written, so nothing moves.
    Nothing -> pure Nothing
    Just (owner, origin, melding) -> do
      -- CR 712.8g: the melded permanent "has only the characteristics of the
      -- combined back face", which Game.cardOfSource answers by resolving
      -- MeldSource.result -- so the face is interned exactly as createTokens
      -- interns a token's card, and every characteristic read past here is the
      -- ordinary one.
      resultId <- State.state (Game.intern (Printing.MkPrinting resultCard))
      -- CR 701.42a's "single object": the melded cards stop being objects BEFORE
      -- the entry loop, so no projection, replacement or trigger scan inside it
      -- can find two cards where one permanent is entering. Each one's CR 608.2h
      -- record is filed as it ceases, exactly as changeZoneAttaching's write does
      -- for every other zone change -- rule 608.2h scopes its look-back by "the
      -- public zone it was expected to be in" rather than by the battlefield, and
      -- exile is such a zone (CR 400.1, CR 406.3). The melding ability's own
      -- source is one of these cards, so without the record a clause of the very
      -- resolution that melded them could no longer say what its source was.
      -- What answers for them AS A GROUP afterwards is Game.componentsOf over the
      -- new permanent's source, the reader CR 202.3c, CR 712.21 and CR 701.27g
      -- share.
      --
      -- The fold is SEQUENTIAL, so each card's CR 608.2h snapshot is taken
      -- against a board the cards before it have already left. That is
      -- unobservable for the objects rule 701.42a can reach: CR 701.42b admits
      -- only cards, meldable requires them off the battlefield, and neither half
      -- of CR 712.5's pairs prints an ability that functions from another zone
      -- (CR 113.6), so no component's projection reads another. One that did
      -- would want a single pre-removal board for all of them.
      State.modify' (\g -> Foldable.foldl' forgetObject g (fmap fst melding))
      let mkObj ts =
            Object.MkObject
              { -- CR 110.2, sentence 1: a permanent's owner is the owner of the
                -- card that represents it, which for a melded permanent is the
                -- one owner all of them share -- `meldable` checked that, since
                -- CR 701.42b's pair has no other reading of "its owner".
                Object.owner = owner,
                -- CR 110.2a: "if an effect instructs a player to put an object
                -- onto the battlefield, that object enters the battlefield under
                -- that player's control", so the resolving controller is stamped
                -- rather than the owner defaulted to. The two coincide for the
                -- pool's only meld pair, whose ability requires the activating
                -- player to own and control both halves.
                Object.enteredUnder = Just controller,
                Object.source = Source.OfMeld (MeldSource.MkMeldSource {MeldSource.result = resultId, MeldSource.components = fmap snd melding}),
                Object.zone = Zone.Battlefield,
                -- CR 110.5b: nothing in rule 701.42 says otherwise.
                Object.tapped = TapState.Untapped,
                -- CR 712.14c's "back faces up", which for the interned combined
                -- face is its only face; Object.face = Nothing is that face.
                Object.facing = Facing.FaceUp,
                Object.exiledFaceDown = False,
                Object.damage = 0,
                -- CR 302.6 through CR 400.7: a permanent that has just entered is
                -- a new object nobody has controlled for any time.
                Object.sickness = Sickness.Sick,
                Object.bindings = Map.empty,
                Object.counters = Map.empty,
                Object.counterTimestamps = Map.empty,
                Object.attachedTo = Nothing,
                Object.chosenColor = Nothing,
                Object.chosenSubtype = Nothing,
                Object.chosenNames = Set.empty,
                Object.chosenPlayer = Nothing,
                Object.timestamp = ts,
                Object.face = Nothing,
                Object.turnedOverAt = Nothing,
                Object.worldSince = Nothing,
                Object.playableFromExile = Nothing,
                Object.plotted = Nothing,
                Object.foretold = Nothing,
                Object.ringBearerFor = Nothing,
                Object.protector = Nothing,
                Object.ventureRoom = Nothing,
                Object.classLevel = Nothing,
                Object.unlockedHalves = Set.empty,
                Object.designations = Set.empty,
                Object.kicked = Map.empty,
                Object.bestowed = False,
                Object.phyrexianLifePaid = 0,
                Object.manaSpent = Mana.MkMana [],
                Object.announcedX = Nothing,
                Object.castFrom = Nothing,
                Object.detainedUntil = Set.empty,
                Object.goadedBy = Set.empty,
                Object.doesNotUntapNext = False,
                Object.exertedBy = Set.empty
              }
      newId <- placeObject owner mkObj Zone.Battlefield LibraryPosition.defaultValue
      -- Alchemy's "perpetually", the ARRIVAL direction of what perpetuate does at
      -- every other zone change. This is the one road that mints an incarnation
      -- outside changeZoneAttaching, so the call is made by hand here.
      --
      -- No rule of the CR names the word, and none needs to. CR 701.42a puts the
      -- two cards ONTO the battlefield, so each is an object that moved and the
      -- permanent is the CR 400.7 new object it became -- which is what
      -- Pawl.Types.Duration's Perpetual arm already says such an effect follows
      -- across. Reading CR 400.7's default as ending the effect HERE would be
      -- selective, the arm overriding that default at every other zone change. CR
      -- 712.21c is the same identity read the other way: one card becoming two,
      -- where this is two becoming one.
      --
      -- EVERY component rather than the first, for perpetuate's own reason one
      -- direction over: CR 701.42a's permanent is represented by both cards, and
      -- CR 712.21e counts a melded permanent as two cards that moved.
      -- Pawl.MeldSpec's arrival case grants on each half in turn to prove it.
      --
      -- Before runEntry, which is changeZoneAttaching's ordering: CR 614.12 reads
      -- an entry replacement against the continuous effects that already apply to
      -- the permanent.
      Foldable.traverse_ (\component -> perpetuate (fst component) (Seq.singleton newId)) melding
      -- CR 712.14c / CR 616.1: ONE permanent enters, so ONE entry loop, with no
      -- simultaneously-entering sibling to exclude. createTokens' order exactly:
      -- the object is materialized first, because CR 614.12 asks for the
      -- characteristics it would have on the battlefield.
      runEntry Set.empty newId
      -- CR 603.6a's enters-the-battlefield triggers scan this, and it is recorded
      -- after the entry loop so the snapshot describes a settled permanent --
      -- recordMintedEntry's reasons, one zone over. `from` is where the cards were,
      -- so an "enters from exile" read sees the truth of rule 701.42a.
      --
      -- Not implemented: a ZoneChange names ONE departing incarnation and ONE
      -- origin where a meld has one of each PER CARD, so a meld whose cards sit in
      -- different zones reports the first card's zone, and only the first card's
      -- id is named as having departed (#2492).
      placed <- State.get
      let snapshot = Projection.project newId placed
      State.modify' (recordEvent (GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange (fst (NonEmpty.head melding)) newId origin Zone.Battlefield) snapshot)))
      pure (Just newId)

-- CR 701.42b: "only two cards belonging to the same meld pair can be melded.
-- Tokens, cards that aren't meld cards, or meld cards that don't form a meld pair
-- can't be melded." What can be read off the board is: every named object is a
-- CARD (CR 108.2, so Source.OfCard and not a token, a copy or an ability), each
-- such card's layout is Meld (CR 712.4), each is somewhere a card can be PUT ONTO
-- the battlefield from, and they share an owner. Answers that owner, the zone the
-- cards are in, and each card's id paired with its printing, in the order the
-- objects were named -- the ids so that the caller's CR 608.2h records and its
-- CR 400.7 event name real departing incarnations rather than re-deriving them,
-- and the pairing so the two cannot fall out of step.
--
-- The BATTLEFIELD conjunct is rule 701.42a's own verb: "put them ONTO the
-- battlefield", which an object already there cannot be. It is not a candidate
-- filter dressed up -- a permanent admitted here would be deleted by the caller
-- with no CR 400.7 zone change and so none of CR 603.6c's leave-the-battlefield
-- triggers, which is a worse answer than rule 701.42c's "they stay in their
-- current zone". CR 712.4a's ability exiles both halves first, so no card in the
-- pool reaches this arm.
--
-- Not implemented: rule 701.42b's PAIR membership. Nothing in a card file says
-- which meld card is whose counterpart -- the melding ability names its
-- counterpart by name and carries the combined face, so the pairing is the
-- ability's rather than the engine's, and an ability naming a meld card that is
-- not its counterpart would be melded here (gap #2497).
--
-- Two or more, from rule 701.42a's "the two cards in a meld pair": one object is
-- not a meld, and a melding ability whose counterpart is gone by resolution
-- refuses here. Not EXACTLY two -- MeldSource and Game.componentsOf both quantify
-- over "each card that represents it" and nothing downstream fixes the number --
-- and the difference is unobservable, CR 712.5's meld pairs being pairs.
meldable :: [ObjectId] -> GameState -> Maybe (PlayerId, Zone, NonEmpty.NonEmpty (ObjectId, PrintingId.PrintingId))
meldable victims gs = do
  (first, rest) <- case victims of
    a : b : cs -> Just (a, b : cs)
    _ -> Nothing
  firstObj <- Game.lookupObject first gs
  let owner = Object.owner firstObj
      origin = Object.zone firstObj
      printingOf oid = do
        obj <- Game.lookupObject oid gs
        case Object.source obj of
          Source.OfCard pid | Object.owner obj == owner && Object.zone obj /= Zone.Battlefield -> do
            card <- Game.cardOfPrinting pid gs
            if Card.Type.layout card == Layout.Meld then Just (oid, pid) else Nothing
          _ -> Nothing
  traverse printingOf (first NonEmpty.:| rest) >>= \melding -> Just (owner, origin, melding)

-- Stop being an object at all, the CR 701.42a half of melding that
-- Game.removeFromZones alone does not do: the id leaves its zone AND the object
-- table, so nothing can look it up afterwards. Unknown ids are left alone.
--
-- CR 608.2h's record is filed in the same write, from the same pre-removal board,
-- for changeZoneAttaching's reason: this is the last moment the object's
-- information is known, and the id it is filed under is the id an ability on the
-- stack still carries as its source (CR 113.7). Every field is read exactly as
-- that funnel reads them, off `obj` rather than off any incarnation, since nothing
-- survives this write to read them from.
forgetObject :: GameState -> ObjectId -> GameState
forgetObject gs oid = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let snapshot = Projection.project oid gs
        lastController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
        cleared = Game.removeFromZones (Object.owner obj) oid gs
     in cleared
          { GameState.objects = Map.delete oid (GameState.objects cleared),
            GameState.lastKnown = Map.insert oid (LastKnown.MkLastKnown snapshot lastController (Object.owner obj) (Object.source obj) (Object.counters obj) (copiedSnapshot oid gs) (Game.attachments oid gs) (Object.chosenNames obj) (Game.isBlocking oid gs) (Object.protector obj)) (GameState.lastKnown cleared)
          }

-- CR 119.3: move one player's life total by this much, and record the CR 608.2i
-- event of the matching sign. The write LoseLife, GainLife and
-- ExchangeLifeTotals share, so a life total moves in exactly one place.
--
-- In this module rather than beside those opcodes because a REPLACEMENT gains
-- life too -- Words of Worship's DrawRewrite.GainLife -- and Pawl.Engine.Resolve
-- is above this one, so an arm of `apply` could not have called it there.
--
-- A zero delta writes nothing at all: CR 119.9 says so for the gain side, and the
-- loss side takes the same posture.
changeLife :: PlayerId -> Integer -> Game ()
changeLife pid delta =
  Monad.when (delta /= 0) . State.modify' $
    recordEvent
      ( if delta > 0
          then GameEvent.LifeGained (LifeChange.MkLifeChange pid (Integer.toNaturalSaturating delta))
          else GameEvent.LifeLost (LifeChange.MkLifeChange pid (Integer.toNaturalSaturating (negate delta)))
      )
      . (\g -> g {GameState.players = Map.adjust (\p -> p {Player.life = Player.life p + delta}) pid (GameState.players g)})

-- CR 121.1, one card at a time per CR 121.2, with CR 614's say first (CR 121.6).
-- An empty library records the failed draw, which CR 704.5b makes a loss at the
-- next state-based-action check. Shared by the draw step, opening hands and the
-- Draw effect.
--
-- The tally and the event are for CR 121.2's "individual card draws": each draw
-- is its own event, carrying which of this player's draws this turn it was, so a
-- card can ask (CR 702.94a). Both happen only when the card actually moved --
-- an empty library draws nothing, and a CR 616.1 loop that cancelled the move
-- means no card reached the hand, so neither is a draw to count.
--
-- CR 121.9's reveal window closes the draw, and it is inside this funnel rather
-- than beside its callers because rule 121.9 says "as they draw it": every route
-- that draws -- the draw step, an opening hand, a Draw effect -- opens the same
-- window, and none of them learns that it did.
drawCard :: PlayerId -> Game ()
drawCard pid = Monad.void (drawCardReturning pid)

-- drawCardReturning is to drawCard what changeZoneReturning is to changeZone, and
-- for CR 121.1's "and reveal IT" (#1899): Just the id the card ARRIVED in the hand
-- under (CR 400.7), which is where a later clause of the same resolution names it.
--
-- Nothing where there is no such card -- a draw a replacement took (CR 614.6), an
-- empty library (CR 104.3c is then the whole of what happened), or a move a
-- replacement effect cancelled -- so a caller binding the answer binds nothing
-- rather than binding a card that is not there.
drawCardReturning :: PlayerId -> Game (Maybe ObjectId)
drawCardReturning pid = do
  outcome <- applyReplacements (ProposedEvent.WouldDraw pid)
  case outcome >>= Replacement.asDraw of
    -- CR 614.6: a replacement took the draw, so it never happens. The row has
    -- already done its own work; nothing is left to do here and nothing is
    -- recorded.
    Nothing -> pure Nothing
    Just drawer -> performDraw drawer

-- The draw itself, once CR 616.1's loop has left it standing. Split out so that
-- rule 614.11's ordering is visible: the proposal above runs BEFORE the library
-- is looked at, which is what makes a row apply "even if no cards could be drawn
-- because there are no cards in the affected player's library".
performDraw :: PlayerId -> Game (Maybe ObjectId)
performDraw pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> do
      State.put gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
      pure Nothing
    top : _ -> do
      moved <- changeZoneReturning top Zone.Hand
      -- ONE card, which is what a draw is (CR 121.1) -- the funnel answers with
      -- more than one arrival only for a melded permanent leaving the
      -- battlefield (CR 712.21), and this move starts in a library. CR 712.21a
      -- puts a melded permanent into a library as TWO CARDS, each of which is
      -- drawn on its own.
      case Seq.viewl moved of
        Seq.EmptyL -> pure Nothing
        drawn Seq.:< _ -> do
          nth <- State.state $ \g ->
            let tally = Map.insertWith (+) pid 1 (GameState.drawsThisTurn g)
             in (Map.findWithDefault 1 pid tally, g {GameState.drawsThisTurn = tally})
          State.modify' (recordEvent (GameEvent.Drew (Drew.MkDrew pid nth)))
          -- CR 702.94a's "if it's the FIRST card you've drawn this turn", asked
          -- off the ordinal this draw was just stamped with rather than off a
          -- second reading of the tally.
          Monad.when (nth == 1) (offerMiracleReveal pid drawn)
          pure (Just drawn)

-- CR 702.94a's static half, and CR 121.9's window: "you may reveal this card from
-- your hand as you draw it". Asked of the card that just arrived in the hand, and
-- only when it has a miracle ability to ask about -- CR 702.94a is the whole of
-- what puts a question here, so an ordinary draw of an ordinary card raises
-- nothing.
--
-- The FIRST-DRAW gate is the caller's, since it is the ordinal the draw already
-- carries. Dropping it would make pawl's miracle cards WEAKER than printed, which
-- is the direction that is never admissible.
--
-- CR 121.9's "may look at that card as they draw it before choosing" is satisfied
-- by the prompt naming the card. pawl has no per-player hidden-information filter
-- at all (#1412), so nothing here could have shown the player less
-- than the rule allows.
--
-- The reveal goes through the ordinary funnel with CR 702.94a's cause, which is
-- what makes the linked trigger (CR 603.11) findable: `eventTriggers`' hand source
-- reads that event and nothing else knows a miracle happened.
--
-- Not implemented: CR 702.94b's LASTING reveal -- the card stays revealed until it
-- leaves the hand or the ability leaves the stack -- which needs a per-object
-- revealed flag (#1408). Nor CR 121.8's face-down drawn card
-- (#1409).
offerMiracleReveal :: PlayerId -> ObjectId -> Game ()
offerMiracleReveal pid drawn = do
  gs <- State.get
  case Game.faceOf drawn gs of
    Just face | Maybe.isJust (Keyword.miracleCost (Face.keywords face)) -> do
      let decider = Decide.deciderFor pid gs
      decision <- Game.choose (Prompt.OfferedMiracleReveal decider pid drawn (Face.name face))
      case decision of
        OptionalDecision.Declines -> pure ()
        OptionalDecision.Exercises -> reveal RevealCause.ForMiracle pid drawn
    _ -> pure ()

-- The single discard funnel (CR 701.9a). `pid` is the discarding player, whom that
-- rule makes the card's owner either way, and `cause` is why (see DiscardCause).
--
-- The move goes through the CR 400.7 funnel, so a discarded card gets a new
-- incarnation and Rest in Peace's redirect composes. The EVENT is what this adds,
-- and it is not redundant with the Moved event: per CR 701.9c a redirected discard
-- is still a discard while its Moved event no longer reads hand-to-graveyard, so a
-- discard trigger reads this record and never the zone pair.
--
-- Recorded only when the move COMPLETED -- an unknown id or a cancelled CR 616.1
-- loop must record nothing (CR 603.2g). The id recorded is the one the funnel
-- MINTED, since CR 702.29c's abilities trigger from wherever the card winds up.
discard :: DiscardCause.DiscardCause -> PlayerId -> ObjectId -> Game ()
discard cause pid oid = Monad.void (discardReturning cause pid oid)

-- A second door rather than a return type on `discard`, the changeZoneReturning
-- and destroyReturning shape: the ~dozen callers that only perform the discard
-- keep their `Game ()`, and the one caller that binds "discarded this way" gets
-- the id without a `Monad.void` at every other site.
--
-- The answer is the id the CR 400.7 funnel MINTED, which is the same id the
-- Discarded event carries and for the same reason: the hand incarnation is gone
-- by the time anything reads the slot, so binding it would name an object no
-- reader of the destination zone can find. Nothing for a move that did not
-- complete -- an unknown id, or a CR 616.1 loop the player cancelled.
discardReturning :: DiscardCause.DiscardCause -> PlayerId -> ObjectId -> Game (Seq.Seq ObjectId)
discardReturning cause pid oid = do
  moved <- changeZoneReturning oid Zone.Graveyard
  -- One record per arrival: a card discarded is a card, so this loop runs once
  -- for every move the funnel makes. A melded permanent is never in a hand, so
  -- the sequence never holds two here.
  Monad.forM_ moved $ \newId -> State.modify' (recordEvent (GameEvent.Discarded (Discarded.MkDiscarded pid newId cause)))
  pure moved

-- Ask the interpreter to shuffle this player's library (CR 103.3 / 701.24).
--
-- Here rather than in Pawl.Engine.Mulligan, which is where it used to live, so
-- that a library is randomized in exactly one place now that a CR 614.6 redirect
-- shuffles one (see changeZoneAttaching). Pawl.Engine.Mulligan sits ABOVE this
-- module, so the setup callers reach down to it and this one does not have to
-- reach up.
shuffleLibrary :: PlayerId -> Game ()
shuffleLibrary pid = do
  gs <- State.get
  let ids = Game.zoneMembers Zone.Library pid gs
  answer <- Game.ask (Prompt.Shuffle ids)
  let shuffled = Game.honourShuffle ids answer
  -- modify' rather than putting `gs` back: it was read before the prompt, and a
  -- prompt may write state -- Game.choose writes GameState.lastChoice. This one
  -- does not (Prompt.Shuffle is bare, being randomness rather than a choice),
  -- so this is defending the invariant rather than fixing a live bug.
  State.modify' (\g -> g {GameState.library = Map.insert pid (Seq.fromList shuffled) (GameState.library g)})

-- The single reveal funnel (CR 701.20a): `pid` shows `oid` to all players, which
-- here means appending what was shown to the public log. No-op for an unknown id.
-- Per CR 701.20b nothing moves and nothing changes, so the event is the whole
-- effect -- the rule, not a shortcut.
--
-- The snapshot is Projection.project -- the object's CR 613 characteristics and
-- deliberately not its printed card, since a reveal has to show what a player at
-- the table would see, on every axis a continuous effect reaches off the
-- battlefield. The readers that judge a card off the battlefield take that same
-- projection -- the explore that follows this very reveal, and rule 728.1's mill
-- tally (see #1911) -- so what is shown and what is judged cannot part
-- company.
--
-- The `cause` is CR 702.94a's "this way" (see RevealCause): every caller but the
-- draw funnel's miracle window shows a card for a reason no rule asks about
-- again, and passes Ordinary.
--
-- CR 708.12 does NOT move this read, and that is the rule rather than an
-- oversight: it governs what a revealing ability READS, not what the log records,
-- and the read is Pawl.Types.Filter's RepresentedByCard -- Hauntwoods Shrieker's
-- "if it's a creature card", proved at Pawl.FaceDownSpec's CR 708.12 group.
--
-- Not implemented: CR 708.9's reveal, which a face-down permanent's owner makes
-- as it leaves the battlefield. Pawl.Types.Object's newIncarnation gets the
-- OUTCOME right -- the status is back to FaceUp -- but no caller reaches this
-- funnel, so nothing can trigger on it, and the snapshot such a reveal would
-- record is unsettled for the same reason nothing reads it (#921).
reveal :: RevealCause.RevealCause -> PlayerId -> ObjectId -> Game ()
reveal cause pid oid = do
  gs <- State.get
  Monad.when (Maybe.isJust (Game.lookupObject oid gs)) $
    State.modify' (recordEvent (GameEvent.Revealed (Revealed.MkRevealed pid oid cause (Projection.project oid gs))))

-- CR 603.2: does this condition fire on this event, for the permanent that bears
-- it? `bearer` is the object whose ability this is and `you` its controller (CR
-- 603.3a, CR 109.5); both are part of the match because the scan visits EVERY
-- permanent, not only the one an event names. The sole home of casing on
-- TriggerCondition for rules purposes -- Pawl.Codec also cases on every
-- constructor, but only at the JSON boundary.
--
-- The BINDINGLESS reading, which is every caller that has no environment to offer
-- and every condition but one. `matchesTriggerGiven` below is the same act with CR
-- 603.7c's captured environment in hand.
matchesTrigger :: GameState -> ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTrigger = matchesTriggerGiven Map.empty

-- CR 603.3b: is this trigger condition "another ability triggering"? The
-- classification the rule's two-part placement turns on -- False puts a trigger
-- in the FIRST pass and True in the second, which is where
-- Pawl.Engine.Engine.placePendingTriggers reads it.
--
-- Exhaustive with no wildcard, for eventBindingSlots' reason and more sharply: a
-- wildcard would silently give every future condition the first pass, and a
-- condition in the wrong pass is a resolution order the player can see.
--
-- `any` for CR 603.1b's multi-condition ability, which is the conservative
-- reading rather than the exact one: rule 603.3b asks about the trigger
-- condition that actually fired, and a PendingTrigger does not record WHICH of an
-- AnyOf's clauses matched (#1027). No printing mixes the two classes in one
-- ability -- Historian's Boon writes its two sentences as two abilities.
reactsToAbilityTriggering :: TriggerCondition -> Bool
reactsToAbilityTriggering cond = case cond of
  -- The one condition of this class in the pool, and the reason the rule's second
  -- pass exists at all.
  TriggerCondition.SagaFinalChapterTriggers _ -> True
  TriggerCondition.AnyOf conditions -> any reactsToAbilityTriggering conditions
  -- CR 603.7's own event is a control change, not an ability triggering, so it
  -- belongs to CR 603.3b's first pass.
  TriggerCondition.LoseControlOfBound _ -> False
  -- CR 309.4c's event is a venture marker moving, not an ability triggering.
  TriggerCondition.RoomEntered _ -> False
  -- CR 309.7's event is a dungeon card leaving the game, not an ability
  -- triggering, so it takes CR 603.3b's first pass too.
  TriggerCondition.PlayerCompletesDungeon _ -> False
  -- Four keyword actions, none of them an ability triggering: CR 701.22a,
  -- CR 701.25a, CR 702.170b and CR 701.44a each describe something a player
  -- or a permanent DOES, so all four take CR 603.3b's first pass.
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.RingTemptsPlayer _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- A fifth keyword action, and the same answer: CR 701.68a is something a
  -- player DOES, so it takes CR 603.3b's first pass too.
  TriggerCondition.PlayerBlights _ -> False
  -- CR 706.1's roll is something a resolving effect INSTRUCTS a player to do,
  -- never an ability triggering, so it takes CR 603.3b's first pass as well.
  TriggerCondition.PlayerRollsDice _ -> False
  TriggerCondition.PlayerWinsCoinFlip _ -> False
  -- The same answer for the same reason: CR 701.43a's exert is a keyword action a
  -- PLAYER takes, and CR 508.1g puts it in a turn-based action rather than in a
  -- resolving ability.
  TriggerCondition.SelfExerted -> False
  -- And CR 701.3a's attach is a keyword ACTION too, taken by a resolving effect
  -- or by a permanent entering the battlefield -- never an ability triggering, so
  -- CR 603.3b's first pass again.
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  -- CR 603.12's trigger event is the action the resolving ability instructed --
  -- a payment, for every printing in the pool -- and never another ability
  -- triggering, so a reflexive takes CR 603.3b's first pass.
  TriggerCondition.Reflexive -> False
  -- Everything else names something that happened to the board or to a player,
  -- which is CR 603.3b's first class in as many words.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StepBegins {} -> False
  -- CR 603.8: a state trigger's condition is a fact about the game state, and a
  -- game state is not an ability triggering.
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> False
  TriggerCondition.PlayerTookInitiative -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  -- CR 121.1's draw is something that happens to a player, not an ability
  -- triggering.
  TriggerCondition.PlayerDrawsNthCard {} -> False
  -- CR 725.1's crowning is something that happens TO a player, which is CR
  -- 603.3b's first class in as many words -- a designation changing hands is not
  -- an ability triggering, whatever put the crown there.
  TriggerCondition.PlayerBecomesMonarch _ -> False
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  TriggerCondition.PlayerAttacks _ -> False
  TriggerCondition.PlayerAttacksWith {} -> False
  TriggerCondition.PlayerAttacksPlayer {} -> False
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.PermanentBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  TriggerCondition.CardPutIntoGraveyard _ -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies _ -> False
  -- CR 603.3b's first class too, for the arm above's reason: a batch of deaths
  -- is not another ability triggering.
  TriggerCondition.PermanentsDie _ -> False
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.PermanentLeavesTheBattlefield _ -> False
  -- The destination-pinned reading of that same form: a zone change is not
  -- another ability triggering either.
  TriggerCondition.PermanentReturnedToHand _ -> False
  TriggerCondition.PermanentsReturnedToHand _ -> False
  -- A card leaving a graveyard is a zone change too, so CR 603.3b's first pass
  -- takes it like the arms above.
  TriggerCondition.CardLeavesGraveyard {} -> False
  -- CR 702.55b watches a death, not another ability triggering.
  TriggerCondition.HauntedCreatureDies -> False
  -- CR 701.6a's countering is a spell or ability DOING something, not one
  -- triggering, so Baral takes the first pass like every other watcher.
  TriggerCondition.SpellOrAbilityCounters _ -> False
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- CR 603.3b again: a prevention is something that happens to a damage event,
  -- not an ability triggering.
  TriggerCondition.SelfPreventsDamage _ -> False
  TriggerCondition.PlayerGainsLife _ -> False
  -- A batch of life gains is not another ability triggering either, PermanentsDie's
  -- answer one event family over.
  TriggerCondition.PlayersGainLife _ -> False
  TriggerCondition.PlayerLosesLife _ -> False
  -- CR 714.2b's own condition is a counter placement, which is what makes a
  -- chapter ability itself a FIRST-pass trigger -- the other half of the pair
  -- this whole classification exists to separate.
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  -- CR 603.3b's first pass: counters landing is a CR 122.6 placement, not an
  -- ability triggering. Both placement scopes answer alike.
  TriggerCondition.PermanentsGetCounters {} -> False
  TriggerCondition.PermanentGetsCounters {} -> False
  TriggerCondition.SpellCast {} -> False
  TriggerCondition.SelfCast -> False
  TriggerCondition.SelfBecomesTargeted _ -> False
  TriggerCondition.ControllerBecomesTarget {} -> False
  TriggerCondition.SelfHalfUnlocked _ -> False
  TriggerCondition.RoomFullyUnlocked _ -> False
  TriggerCondition.SelfTurnedFaceUp -> False
  TriggerCondition.SelfTransformedInto _ -> False
  -- The bystander form of the same event, and False for the same reason.
  TriggerCondition.PermanentTransforms _ -> False
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  TriggerCondition.PermanentBecomesDesignated {} -> False
  TriggerCondition.SelfEvolves -> False
  -- CR 702.134c watches a mentor ability RESOLVING, which is neither of CR 603.3b's
  -- two classes' subjects read carelessly: an ability resolving is something the
  -- rules did, but the rule's second pass is for a condition that IS another
  -- ability TRIGGERING, and a resolution is a first-pass event like any other. A
  -- mentor ability's own trigger has long since been placed by the time this fires.
  TriggerCondition.AttachedCreatureMentors -> False
  -- Rule 702.149c watches a training ability RESOLVING, which is the same
  -- first-pass event the arm above argues rule 702.134c's is.
  TriggerCondition.SelfTrains -> False
  -- CR 700.4's zone change is a first-pass event too, and not an ability
  -- triggering.
  TriggerCondition.AttachedCreatureDies -> False
  -- CR 701.26a's tap is a first-pass event as well, and not an ability
  -- triggering.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  -- CR 701.26b's untap is a first-pass event as well.
  TriggerCondition.SelfBecomesUntapped -> False
  -- CR 106.12a is a first-pass event as well, and not an ability triggering.
  TriggerCondition.AttachedPermanentTappedForMana -> False
  TriggerCondition.PermanentSacrificed {} -> False

-- CR 603.2b / 109.5: does this condition restrict the turn its event may occur
-- on to the ABILITY'S CONTROLLER's turn? True for "at the beginning of YOUR
-- <step>" and for nothing else.
--
-- A CLASSIFICATION of a trigger condition, the third of the same kind as
-- eventBindingSlots and zonesTriggeredFrom above.
--
-- Its customer is the card lint. Onset.FromYourNextTurn delivers both halves of
-- "your next turn" on its own, so this no longer guards the firing -- it guards
-- the DATA: a card arming that onset over an EachTurn condition would have its
-- printed "each" silently narrowed by the window.
--
-- Exhaustive with no wildcard, for eventBindingSlots' reason: a new condition must
-- force a decision rather than defaulting to False.
controllerTurnScoped :: TriggerCondition -> Bool
controllerTurnScoped cond = case cond of
  -- CR 309.4c names no turn at all.
  TriggerCondition.RoomEntered _ -> False
  -- CR 309.7 names no turn either.
  TriggerCondition.PlayerCompletesDungeon _ -> False
  -- None of the four keyword-action conditions names a turn: CR 701.22,
  -- CR 701.25, CR 702.170 and CR 701.44 each state when their event happens
  -- and say nothing about whose turn it is.
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.RingTemptsPlayer _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- CR 701.68 names no turn either.
  TriggerCondition.PlayerBlights _ -> False
  -- CR 706.1 names no turn either.
  TriggerCondition.PlayerRollsDice _ -> False
  TriggerCondition.PlayerWinsCoinFlip _ -> False
  -- False for the SelfAttacks arm's reason below, which is exactly this case one
  -- rule earlier: CR 508.1g exerts on the ACTIVE player's turn, and CR 109.5's
  -- "you" is the ability's controller, so a stolen Glory-Bound Initiate is
  -- exerted on its thief's turn rather than on its owner's.
  TriggerCondition.SelfExerted -> False
  -- CR 701.3a names no turn: an Aura can be cast, and an Equipment equipped, on
  -- any turn its controller has priority for.
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  -- One of the two arms carrying a TurnScope, and the one the lint below this
  -- was written for (CR 603.3a, CR 109.5).
  TriggerCondition.StepBegins (StepBegins.MkStepBegins _ TurnScope.ControllersTurn) -> True
  -- "Each <step>" admits every player's turn, the pairing the lint rejects.
  TriggerCondition.StepBegins (StepBegins.MkStepBegins _ TurnScope.EachTurn) -> False
  -- And "during an opponent's <step>" admits every turn but the controller's,
  -- which is not the controller's turn either. No card prints it, so the lint
  -- cannot reach this arm; answering it any other way would make the
  -- classification wrong for the sake of an unreachable case.
  TriggerCondition.StepBegins (StepBegins.MkStepBegins _ TurnScope.OpponentsTurn) -> False
  -- CR 702.179d's "during YOUR turn" is the same restriction StepBegins spells
  -- with a TurnScope, written into the condition itself because rule 702.179d
  -- states it there. No card bears this condition, so the lint this feeds cannot
  -- reach it; answering False anyway would make the classification wrong for the
  -- sake of an unreachable case.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> True
  -- None of the rest is turn-scoped: each names an event that can happen on
  -- anybody's turn.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  -- CR 709.5e restricts the special action to the taker's own turn, but CR
  -- 709.5f's keyword action and CR 709.5d's entry have no such restriction, and
  -- this classification is about the CONDITION rather than about how the
  -- designation came to be given.
  TriggerCondition.SelfHalfUnlocked _ -> False
  -- CR 709.5i says nothing about whose turn it is, for SelfHalfUnlocked's reason.
  TriggerCondition.RoomFullyUnlocked _ -> False
  -- `all`, because the lint this feeds asks whether the WHOLE ability is already
  -- narrowed to its controller's turn, and one clause that admits every turn is
  -- enough to make the answer no. Vacuously True for the empty list, which no card
  -- writes.
  TriggerCondition.AnyOf conditions -> all controllerTurnScoped conditions
  -- CR 702.37e offers the special action "any time you have priority", which is
  -- every turn and not only the controller's own.
  TriggerCondition.SelfTurnedFaceUp -> False
  -- CR 701.27a names no turn at all: an activated ability can transform a
  -- permanent whenever it can be activated, and CR 702.145c's sweep fires on
  -- whosever turn it becomes night.
  TriggerCondition.SelfTransformedInto _ -> False
  -- The watcher's side of that answer, and False for the same reason, the
  -- watcher not being the permanent that turned over.
  TriggerCondition.PermanentTransforms _ -> False
  -- The same rule from the watcher's side, and doubly so: CR 702.37e lets any
  -- player take the action on any turn, and the watcher is not even the player
  -- taking it.
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  -- CR 702.112a's ability fires on combat damage to a player, which any player's
  -- turn can carry, and the watcher's turn is not asked about at all.
  TriggerCondition.PermanentBecomesDesignated {} -> False
  -- Rule 702.100b names no turn either: a creature can evolve on anyone's.
  TriggerCondition.SelfEvolves -> False
  -- Rule 702.134c names none either. CR 508.1 does make every mentoring happen on
  -- the mentor's controller's turn, but that is a consequence of what mentor
  -- watches rather than a narrowing this condition states, and the Equipment's
  -- controller need not be that player.
  TriggerCondition.AttachedCreatureMentors -> False
  -- CR 303.4 names no turn either: an enchanted creature can die on anyone's.
  TriggerCondition.AttachedCreatureDies -> False
  -- Nor does CR 701.26a: an enchanted creature can be tapped on anyone's turn,
  -- and Betrayal's whole point is that the Aura's controller is not the tapping
  -- creature's.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  -- Nor CR 701.26b: CR 502.3's untap step is the active player's, but an
  -- Effect.Untap and CR 107.6's untap symbol reach any turn.
  TriggerCondition.SelfBecomesUntapped -> False
  -- Nor does CR 106.12a: an enchanted land can be tapped for mana on anyone's
  -- turn, and Wild Growth's whole point is that the mana is its controller's.
  TriggerCondition.AttachedPermanentTappedForMana -> False
  -- Rule 702.149c names no turn either, and the SelfAttacks arm below settles the
  -- consequence: CR 508.1a makes the training happen on the ACTIVE player's turn,
  -- which is not CR 109.5's "you" -- a stolen creature trains on its thief's turn.
  TriggerCondition.SelfTrains -> False
  -- CR 701.21a says nothing about whose turn it is, and neither does the printed
  -- "whenever an opponent sacrifices an artifact" -- the relation names a seat,
  -- not a turn.
  TriggerCondition.PermanentSacrificed {} -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> False
  TriggerCondition.PlayerTookInitiative -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  TriggerCondition.PlayerDrawsNthCard {} -> False
  -- CR 508.1a makes this the ACTIVE player's turn, which is not the same thing:
  -- CR 109.5's "you" is the ability's controller, and a stolen creature attacks on
  -- its thief's turn.
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  -- CR 506.2 makes this one an OPPONENT's turn every time, which is not the
  -- controller's turn either -- StepBegins' OpponentsTurn arm above answers the
  -- same way for the same reason.
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  -- The only arm around here that can answer True, and only on one relation: CR
  -- 508.1 lets only the active player declare attackers, so the declarer named
  -- by the event is always the active player. You therefore pins the event to CR
  -- 109.5's "you"'s own turn; Opponent pins it to somebody else's; AnyPlayer
  -- pins nothing. SelfAttacks answers False because it pins the CREATURE and not
  -- the declarer -- a stolen creature attacks on its thief's turn -- which is
  -- the comparison this arm does make.
  TriggerCondition.PlayerAttacks relation -> relation == PlayerRelation.You
  -- The arm above's comparison, over the same relation: rule 508.3c's Filter
  -- narrows WHICH creatures were declared and says nothing about whose turn it
  -- is.
  TriggerCondition.PlayerAttacksWith payload -> PlayerAttacksWith.player payload == PlayerRelation.You
  -- The same comparison over the ATTACKING side, which is the field rule
  -- 508.3e's event pins to the active player; the attacked side says nothing
  -- about whose turn it is.
  TriggerCondition.PlayerAttacksPlayer subjects -> PlayerAttacksPlayer.attacker subjects == PlayerRelation.You
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.PermanentBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  -- CR 603.2b names no turn, so the bystander reading is unscoped like the
  -- self-scoped one above.
  TriggerCondition.CardPutIntoGraveyard _ -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies _ -> False
  TriggerCondition.PermanentsDie _ -> False
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.PermanentLeavesTheBattlefield _ -> False
  TriggerCondition.PermanentReturnedToHand _ -> False
  TriggerCondition.PermanentsReturnedToHand _ -> False
  -- The third arm carrying a TurnScope, and the classification follows the FIELD
  -- rather than the constructor: Kishla Skimmer prints "during your turn" and a
  -- printing of the same family without it would not.
  TriggerCondition.CardLeavesGraveyard (CardLeavesGraveyard.MkCardLeavesGraveyard _ TurnScope.ControllersTurn) -> True
  TriggerCondition.CardLeavesGraveyard (CardLeavesGraveyard.MkCardLeavesGraveyard _ TurnScope.EachTurn) -> False
  -- No printing of this family says "during an opponent's turn", so this arm is
  -- unreachable from card data; answering True would make the classification wrong
  -- for the sake of that unreachable case.
  TriggerCondition.CardLeavesGraveyard (CardLeavesGraveyard.MkCardLeavesGraveyard _ TurnScope.OpponentsTurn) -> False
  -- Rule 702.55b names no turn.
  TriggerCondition.HauntedCreatureDies -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False
  -- Damage can be prevented on anybody's turn.
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- Damage can be prevented on anybody's turn, the arm above's reason.
  TriggerCondition.SelfPreventsDamage _ -> False
  -- Life can be gained on anybody's turn. CR 702.179d's loss condition above says
  -- "during your turn" and this one does not, which is the two rules' own
  -- difference rather than an omission here.
  TriggerCondition.PlayerGainsLife _ -> False
  -- The batch reading names no turn either.
  TriggerCondition.PlayersGainLife _ -> False
  -- And life can be LOST on anybody's turn. This is the arm where CR 702.179d's
  -- condition is closest to being duplicated and is not: that one reads the very
  -- same GameEvent.LifeLost but only during its controller's turn, which is the
  -- printed speed rule rather than anything a card's "whenever an opponent loses
  -- life" says.
  TriggerCondition.PlayerLosesLife _ -> False
  -- CR 122.6 puts counters on at any time, and a Saga can receive one on an
  -- opponent's turn: CR 714.3a's entry replacement fires whenever the Saga enters,
  -- which a flash effect or an opponent's Sneak Attack could make happen. Only CR
  -- 714.3c's turn-based action is the controller's own turn, and that is the
  -- action's restriction rather than this condition's.
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  TriggerCondition.PermanentsGetCounters {} -> False
  TriggerCondition.PermanentGetsCounters {} -> False
  -- CR 601.2i says nothing about whose turn it is and CR 117.1a lets an instant
  -- be cast on anybody's, so the answer is the condition's own TurnScope --
  -- StepBegins' arms one more time, and for its reason (CR 603.3a, CR 109.5).
  -- Brineborn Cutthroat's OpponentsTurn is turn-scoped and is not the
  -- CONTROLLER's turn, which is the only thing this classification asks.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast _ TurnScope.ControllersTurn _ _) -> True
  TriggerCondition.SpellCast (SpellCast.MkSpellCast _ TurnScope.EachTurn _ _) -> False
  TriggerCondition.SpellCast (SpellCast.MkSpellCast _ TurnScope.OpponentsTurn _ _) -> False
  -- The same rule with no TurnScope to read: a spell can be cast on anybody's
  -- turn, so its own cast trigger is not the controller's-turn kind either.
  TriggerCondition.SelfCast -> False
  -- The condition carries no TurnScope, and an opponent can target the bearer on
  -- anybody's turn.
  TriggerCondition.SelfBecomesTargeted _ -> False
  -- Its player-side sibling likewise: a spell can name its controller on anybody's
  -- turn.
  TriggerCondition.ControllerBecomesTarget {} -> False
  -- CR 714.3c's turn-based action falls on the Saga controller's own turn, but
  -- nothing restricts this CONDITION to it: CR 714.3a's entry replacement can put
  -- a Saga's last lore counter on during anybody's turn, and the watcher is not
  -- even the Saga's controller under the Opponent relation.
  TriggerCondition.SagaFinalChapterTriggers _ -> False
  -- The condition carries no TurnScope, and CR 725 restricts a crowning to no
  -- turn at all: CR 725.1's "an effect instructs a player to become the monarch"
  -- can resolve on anybody's turn, and CR 725.2's crown steal happens in the
  -- combat damage step of whoever is attacking. The watcher may or may not be
  -- that player, which is exactly what makes this not controller-scoped.
  TriggerCondition.PlayerBecomesMonarch _ -> False
  -- Carries no TurnScope at all: CR 514.2 ends the control effect Ray of Command's
  -- delayed ability watches on the turn the spell resolved, but that is the
  -- DURATION's doing rather than a restriction the condition states.
  TriggerCondition.LoseControlOfBound _ -> False
  -- Carries no TurnScope either, and CR 603.12 restricts a reflexive to no turn:
  -- it fires on whatever turn the ability that created it resolved on, which for
  -- an instant-speed creator is an opponent's as readily as its controller's.
  TriggerCondition.Reflexive -> False

-- CR 603.7a: the printed Onset as the game first stores it. The delayed-trigger
-- twin of Expiry.arm, deliberately blind to the board -- unlike a duration, an
-- onset has nothing to bake in when the ability is created, "your next turn" being
-- a boundary that has not happened yet. settleOnsets supplies the number.
armOnset :: Onset -> TurnWindow
armOnset onset = case onset of
  Onset.Immediately -> TurnWindow.AnyTurn
  Onset.FromYourNextTurn -> TurnWindow.ControllersNextTurn

-- CR 603.7a: a turn has BEGUN, so settle every delayed entry waiting for one and
-- drop every entry whose turn is now over. Engine.beginTurnOf calls this once the
-- new turn's number and active player are in place, and only for a turn that
-- actually begins -- CR 614.10a read on the turn axis, so CR 800.4k's turn a
-- departed seat never begins is walked past without settling anything.
--
-- The turn handoff is the ONLY moment either transition can be made, which is what
-- makes this a boundary sweep rather than something derived on demand. Two of
-- them, in this order:
--
-- 1. WAITING -> THIS TURN, for an entry whose controller is the player whose turn
--    this is. The number is sampled here because nothing in GameState remembers
--    which player each earlier turn belonged to, so the question cannot be
--    answered later. Sampled once and thereafter only ever cleared.
--
-- 2. THIS TURN -> GONE, for an entry whose settled turn is behind us. Its trigger
--    event cannot occur again, so CR 603.7a has already decided the matter.
--    Dropping it is hygiene: an entry that can never fire and states no duration
--    would otherwise outlive the game.
--
-- Ordered settle-then-drop within one pass: an entry settled onto THIS turn
-- carries this turn's number, and the drop removes only numbers strictly behind.
--
-- CR 603.7b's one shot is untouched -- this ends entries by the CALENDAR, and
-- triggering still ends them in delayedPending. An entry with a stated duration is
-- dropped here too, and rightly: a duration keeps an ability armed for its event's
-- next occurrence, not for a turn its printed text never named.
settleOnsets :: GameState -> GameState
settleOnsets gs =
  let settled entry = case DelayedTrigger.window entry of
        -- The turn that is beginning IS the one the printed phrase named exactly
        -- when it belongs to the entry's controller (CR 603.7d-f).
        TurnWindow.ControllersNextTurn
          | DelayedTrigger.controller entry == GameState.activePlayer gs ->
              entry {DelayedTrigger.window = TurnWindow.OnTurn (GameState.turnNumber gs)}
        -- Anyone else's turn, including an intervening opponent's: still waiting.
        TurnWindow.ControllersNextTurn -> entry
        TurnWindow.AnyTurn -> entry
        -- Already settled, and this is a later turn -- `live` is what ends it.
        TurnWindow.OnTurn _ -> entry
      live entry = case DelayedTrigger.window entry of
        TurnWindow.AnyTurn -> True
        TurnWindow.ControllersNextTurn -> True
        TurnWindow.OnTurn n -> n >= GameState.turnNumber gs
   in gs {GameState.delayedTriggers = Seq.filter live (fmap settled (GameState.delayedTriggers gs))}

-- Everything that has triggered and is not yet on the stack, from all three
-- sources, plus the delayed store as it stands afterwards. One function, so
-- Pawl.Engine.Engine never needs to know how many sources there are.
--
-- Takes the GROUPED events, because eventTriggers is the one gatherer whose
-- answer depends on which of them were simultaneous (CR 603.10a). The other two
-- take the events alone.
gatherTriggers :: [LoggedEvent.LoggedEvent] -> GameState -> Game ([PendingTrigger], Seq.Seq DelayedTrigger)
gatherTriggers grouped gs = do
  -- Already CR 603.4-filtered: delayedPending has to run the check itself,
  -- since which entries survive in its second component depends on the
  -- answer. Running it over these again would be a redundant no-op, the
  -- GameState being the same one, so only the other two are filtered here.
  --
  -- In Game rather than pure because CR 603.7b's second sentence is a QUESTION
  -- for the entry's controller, and every prompt goes through Game.ask. The
  -- other two gatherers stay pure and are merged in here.
  (fromDelayed, surviving) <- delayedPending grouped gs
  let undecided = eventTriggers grouped gs <> stateTriggers gs
  pure (filter (interveningHolds gs) undecided <> fromDelayed, surviving)

-- | CR 603.3b: the abilities a round of GameEvent.AbilityTriggered records fires,
-- for Pawl.Engine.Engine.reactions to fold into the same batch.
--
-- `gatherTriggers`' event half alone. The other two sources are deliberately not
-- re-run: CR 603.8's state triggers were gathered once for this batch already and
-- match no event at all, so a second call would duplicate every one of them; and
-- the CR 603.7 delayed store's watermark is spent by the same one call, so
-- re-running it would consume entries against events they never matched.
reactionTriggers :: [LoggedEvent.LoggedEvent] -> GameState -> [PendingTrigger]
reactionTriggers events gs = filter (interveningHolds gs) (eventTriggers events gs)
