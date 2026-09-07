{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over countering spells and abilities (CR 701.5), the
-- CR 608.2b fizzle, and the text-changing effects that decide what a spell
-- can still be countered by. The machinery is Pawl.ResolveSpec.
module Pawl.CounterspellSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeInstance as ModeInstance
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

-- Casts every castable spell (targets via lookupMin: creatures first),
-- otherwise passes. Drives the Bolt-vs-Bolt integration falsifier.
boltAnswer :: Prompt.Prompt r -> r
boltAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          A.Cast {} -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> A.Pass
  _ -> S.identityAnswer p

-- bob's Piker on the battlefield; alice holds TWO Bolts and two Mountains, in
-- her main phase. boltAnswer casts both (CR 117.3c keeps priority), both
-- target the Piker (the only creature), and the priority loop resolves them
-- LIFO: B kills the Piker, the mid-loop SBA buries it, A fizzles.
twoBoltState :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
twoBoltState piker mountain lightningBolt =
  let (_, withPiker) = S.addPermanent piker S.bob (S.landsInPlay mountain 2)
      (gs1, _oid1) = S.handOne lightningBolt withPiker
      (boltPrintingId, gs1b) = Game.intern lightningBolt gs1
      (oid2, gs2) = Game.freshObjectId gs1b
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard boltPrintingId,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = Timestamp.MkTimestamp 0,
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
            Object.exertedBy = Set.empty,
            Object.activatedOnce = Set.empty
          }
   in gs2
        { GameState.objects = Map.insert oid2 obj (GameState.objects gs2),
          -- handOne already put oid1 in hand; ADD the second Bolt, oid2.
          GameState.hand = Map.adjust (oid2 Seq.<|) S.alice (GameState.hand gs2)
        }

-- alice has 3 Islands and Cancel in hand; a `victim` spell (bob's) sits on the
-- stack. Returns (victimId, state after alice casts Cancel at it and it resolves).
cancelVictim :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
cancelVictim island cancel victim =
  let base = S.landsInPlay island 3
      (victimId, onStack) = S.spellOnStack victim S.bob base
      (gs, cancelId) = S.handOne cancel onStack
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice cancelId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (victimId, resolved)

-- Append a second card of `printing` to `pid`'s hand (handOne overwrites the hand,
-- so a second in-hand card must be appended, not re-inserted).
handAppend :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
handAppend printing pid gs =
  let (printingId, gsP) = Game.intern printing gs
      (oid, gs1) = Game.freshObjectId gsP
      obj = Object.MkObject pid Nothing (Source.OfCard printingId) Zone.Hand TapState.Untapped Facing.FaceUp False 0 (Sickness.Settled pid) Map.empty Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing (Timestamp.MkTimestamp 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty Map.empty False 0 (Mana.MkMana []) Nothing Nothing Set.empty Set.empty False Set.empty Set.empty
   in ( oid,
        gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs1)
          }
      )

-- alice has 6 Islands and TWO Cancels; a Piker (bob's) sits on the stack. alice
-- casts Cancel A at the Piker, then Cancel B at the Piker (CR 117.3c keeps
-- priority). Stack [B, A, Piker]; resolveTop LIFO: B counters the Piker, then A --
-- its only target gone -- fizzles (CR 608.2b).
racingCounters :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
racingCounters island piker cancel =
  let base = S.landsInPlay island 6
      (victimId, onStack) = S.spellOnStack piker S.bob base
      (gs1, cancelA) = S.handOne cancel onStack
      (cancelB, gs2) = handAppend cancel S.alice gs1
      atVictim :: Prompt.Prompt r -> r
      atVictim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject victimId))) sets
        _ -> S.identityAnswer p
      castA = snd (Engine.runGamePure atVictim gs2 (S.cast S.alice cancelA))
      castB = snd (Engine.runGamePure atVictim castA (S.cast S.alice cancelB))
      r1 = snd (Engine.runGamePure atVictim castB Stack.resolveTop) -- B counters the Piker
      r2 = snd (Engine.runGamePure atVictim r1 Stack.resolveTop) -- A fizzles
   in r2

counterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
counterSpec s registry = Spec.describe s "Counter" $ do
  Spec.it s "CR 701.6 Cancel counters a spell into its owner's graveyard" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, resolved) = cancelVictim island cancel piker
    Spec.assertEqWith s "victim countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 1
    Spec.assertEqWith s "victim never resolved onto the battlefield" (S.creaturesInPlay S.bob resolved) 0
    Spec.assertEqWith s "Cancel in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
  -- CR 113.6g: "an object's ability that states it can't be countered …
  -- functions on the stack", and CR 101.2 makes the "can't" win. The twin is
  -- the case directly above: the same Cancel, cast the same way at a spell
  -- that does not say it, DOES counter -- so this is the card's clause and
  -- not a broken Cancel.
  Spec.it s "CR 113.6g whole card: Cancel resolves but cannot counter Rending Volley" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    rendingVolley <- S.printingOf s registry "Rending Volley"
    let (victimId, resolved) = cancelVictim island cancel rendingVolley
    Spec.assertBool s (elem victimId (GameState.stack resolved)) "Rending Volley is still on the stack"
    Spec.assertEqWith s "and not in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    -- CR 101.2 again, from the other side: the countering spell is not itself
    -- stopped. Cancel targeted legally (CR 113.6g grants no shroud), resolved,
    -- did nothing, and CR 608.2n put it into its owner's graveyard as the
    -- final part of that resolution.
    Spec.assertEqWith s "Cancel resolved into alice's graveyard regardless" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
  Spec.it s "CR 608.2b a Cancel whose target already left the stack fizzles" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    cancel <- S.printingOf s registry "Cancel"
    let after = racingCounters island piker cancel
    Spec.assertEqWith s "the Piker moved exactly once, to bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "both Cancels in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
    Spec.assertEqWith s "the Piker never hit the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "stack cleared" (length (GameState.stack after)) 0
  Spec.it s "CR 614 Cancel under Rest in Peace exiles the countered spell" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    cancel <- S.printingOf s registry "Cancel"
    let (_, ripOut) = S.addPermanent restInPeace S.alice (S.landsInPlay island 3)
        (_victimId, onStack) = S.spellOnStack piker S.bob ripOut
        (gs, cancelId) = S.handOne cancel onStack
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice cancelId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the countered spell is not in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 0
    Spec.assertEqWith s "the countered spell is exiled" (length (Game.zoneMembers Zone.Exile S.bob resolved)) 1

-- The board every Mana Leak case starts from, with only `bobLands` varying.
-- alice has two Islands (Mana Leak's {1}{U}) and a Mana Leak in hand; bob has
-- `bobLands` untapped Islands of his own and a Goblin Piker already on the
-- stack. Returns the Piker's id and the state after alice casts Mana Leak at it.
--
-- The Piker is on the stack BEFORE Mana Leak is cast, so it holds the lower
-- object id and identityAnswer's ChooseTargets -- Set.lookupMin over the legal
-- recipients -- aims the Leak at it. The cancelVictim route above, and the same
-- reason.
manaLeakBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
manaLeakBoard island manaLeak piker bobLands =
  let (victimId, leakId, gs) = manaLeakHand island manaLeak piker bobLands
   in (victimId, snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice leakId)))

-- manaLeakBoard one step earlier, with the Leak still in alice's hand. Split out
-- for the case that has to RECORD the cast as well as the resolution: an engine
-- that offered CR 118.12a's cost at cast time would put its prompt outside a
-- transcript that starts afterwards, and the countered-Leak case below exists to
-- catch exactly that.
manaLeakHand :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
manaLeakHand island manaLeak piker bobLands =
  let base = S.landsInPlay island 2
      withBob = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) base [1 .. bobLands]
      (victimId, onStack) = S.spellOnStack piker S.bob withBob
      (gs, leakId) = S.handOne manaLeak onStack
   in (victimId, leakId, gs)

-- Pays what a resolving spell or ability offers `who`, and takes the identity
-- fallback elsewhere (the hackToIsland liar pattern). Deliberately unlike
-- identityAnswer's Declines, so a test can tell an honoured answer from the
-- fallback -- and so a pair of branches can differ in NOTHING but this.
--
-- Guarded on a NAMED player rather than paying whoever is asked, which is what
-- makes the cases below prove CR 118.12's "who". An engine that offered the cost
-- to the wrong player falls through to Declines and fails, rather than paying and
-- passing: for Mana Leak the payer is the TARGETED spell's controller and not the
-- resolving spell's, and for Whipstitched Zombie it is the ability's own (CR
-- 603.3a). The Decider is checked alongside the player for CR 723.1: nobody is
-- controlling anybody in these fixtures, so the two must agree.
--
-- Rank-1, like Pawl.Support.attackTo: the implicit forall is outermost, so
-- `paysFor S.bob` is the `forall r. Prompt r -> r` that Replay.record wants.
paysFor :: PlayerId.PlayerId -> Prompt.Prompt r -> r
paysFor who p = case p of
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == who && player == who ->
        PaymentDecision.Pays
  _ -> S.identityAnswer p

bobPaysAnswer :: Prompt.Prompt r -> r
bobPaysAnswer = paysFor S.bob

-- manaLeakBoard with a Thalia on bob's side and one more Island each. alice
-- needs the third Island because Thalia taxes HER cast (CR 601.2f), which is
-- what makes the case below a paired assertion rather than one; bob needs three
-- untapped Islands and no more, so a gate cost routed through that same rule
-- would be one mana short.
--
-- Thalia is added BEFORE the Piker, so the Piker still holds the lower stack id
-- and the Leak is aimed as manaLeakBoard describes.
thaliaLeakBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
thaliaLeakBoard island thalia manaLeak piker =
  let base = S.landsInPlay island 3
      (_thaliaId, withThalia) = S.addPermanent thalia S.bob base
      withBob = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) withThalia [1 .. 3 :: Int]
      (victimId, onStack) = S.spellOnStack piker S.bob withBob
      (gs, leakId) = S.handOne manaLeak onStack
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice leakId))
   in (victimId, cast)

-- bobPaysAnswer, plus: BOB's target choices avoid `notThis`. What lets one
-- interpreter drive the whole countered-Leak exchange -- alice's Mana Leak takes
-- identityAnswer's lowest-id recipient and hits the Piker, which was on the stack
-- first, while bob's Cancel skips the Piker and hits the Leak above it. The two
-- casts are told apart by WHO is casting, which is on the prompt.
--
-- Still pays for bob wherever a cost is offered, which is the whole point: the
-- exchange must be able to answer a ChooseToPay, so that a transcript with none
-- in it says the prompt was never raised rather than that nobody would have paid.
bobPaysAndCounters :: ObjectId.ObjectId -> Prompt.Prompt r -> r
bobPaysAndCounters notThis p = case p of
  Prompt.ChooseTargets _ player _ sets
    | player == S.bob ->
        fmap (\(n, legal) -> Set.fromList (take (Natural.toIntSaturating n) (Set.toAscList (Set.filter (\r -> Recipient.objectOf r /= Just notThis) legal)))) sets
  _ -> bobPaysAnswer p

-- The pay-or-not answers in a transcript, in order.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isExileResponse :: Response.Response -> Bool
isExileResponse response = case response of
  Response.ChoseExilesFromGraveyard _ -> True
  _ -> False

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- CR 118.12 / 118.12a: Mana Leak's "Counter target spell unless its controller
-- pays {3}" -- a cost paid when the spell RESOLVES, by a player who is not the
-- resolving spell's controller, with the counter on the refusal branch.
--
-- The first two cases run the SAME board and the SAME cast and differ in NOTHING
-- but bob's answer, so the difference in outcome is the gate and nothing else;
-- the third changes only how many Islands he holds. CR 608.2n's "Mana Leak in
-- alice's graveyard" is asserted in all three: the resolution continues either
-- way, a refusal being the other branch rather than a failure.
manaLeakSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manaLeakSpec s registry = Spec.describe s "ManaLeak" $ do
  Spec.it s "CR 118.12a the targeted spell's controller declines, so it is countered" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, cast) = manaLeakBoard island manaLeak piker 3
        ((_, after), transcript) = Replay.record S.identityAnswer cast Stack.resolveTop
    -- bob COULD have paid -- three untapped Islands -- so he was really asked,
    -- and the refusal is his rather than CR 118.3's.
    Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "the Piker was countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and never reached the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "declining spent nothing: bob's Islands are all untapped" (S.tappedCount S.bob after) 0
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  Spec.it s "CR 118.12a the targeted spell's controller pays, so it is not countered" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = manaLeakBoard island manaLeak piker 3
        ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- The payment really happened: CR 605.3a lets the payer activate mana
    -- abilities "whenever a rule or effect asks for a mana payment, even if
    -- it's in the middle of ... resolving a spell", and three Islands paid {3}.
    Spec.assertEqWith s "paying tapped three of bob's Islands" (S.tappedCount S.bob after) 3
    -- CR 118.12a's other branch: the counter did not happen, and the spell is
    -- still there to resolve. Asserting only "not in the graveyard" would pass
    -- for a spell that never resolved at all, so the next line resolves it.
    Spec.assertBool s (elem victimId (GameState.stack after)) "the Piker is still on the stack"
    Spec.assertEqWith s "nothing of bob's is in his graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- CR 400.7 mints a fresh incarnation on the battlefield, so the permanent
    -- is counted rather than looked up by the spell's id.
    let played = snd (Engine.runGamePure bobPaysAnswer after Stack.resolveTop)
    Spec.assertEqWith s "and the Piker then resolves onto the battlefield" (S.creaturesInPlay S.bob played) 1
  -- CR 118.3 / 118.12: "can't" is the rule's own third case, and its Standstill
  -- example is exactly an unpayable cost. Two Islands cannot pay {3}, so there
  -- is one possible answer and the prompt is not raised -- proved by the
  -- transcript, under an interpreter that WOULD have paid.
  Spec.it s "CR 118.12 a controller who cannot pay {3} is not asked, and is countered" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, cast) = manaLeakBoard island manaLeak piker 2
        ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "bob was never asked" (payResponses transcript) []
    Spec.assertEqWith s "nothing of bob's was tapped" (S.tappedCount S.bob after) 0
    Spec.assertEqWith s "the Piker was countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and never reached the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  -- CR 601.2f totals the cost of a spell being CAST -- "the player determines the
  -- total cost of the spell ... plus all additional costs and cost increases" --
  -- and a cost paid during resolution is not that, so no cost increase reaches
  -- it. ONE Thalia proves both halves at once: bob's tax is live enough to make
  -- alice spend a third Island casting the Leak, and it still leaves the {3} at
  -- {3}. Routed through Cost.total, the gate would ask for {4}, bob's three
  -- Islands would fail CR 118.3, and he would never be asked at all.
  Spec.it s "CR 601.2f a cost increase taxes the CAST and not the resolution payment" $ do
    island <- S.printingOf s registry "Island"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = thaliaLeakBoard island thalia manaLeak piker
    -- The control, and the half that must NOT change: Thalia is on the
    -- battlefield and taxing. Mana Leak is {1}{U}, so an untaxed cast leaves an
    -- Island untapped and this reads 2.
    Spec.assertEqWith s "Thalia taxed alice's cast: all three of her Islands paid {2}{U}" (S.tappedCount S.alice cast) 3
    let ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "bob was asked, so {3} was still payable" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- Three, not four: the same Thalia that just cost alice an Island adds
    -- nothing here. Thalia herself is untapped, so this counts Islands only.
    Spec.assertEqWith s "and {3} cost bob exactly three Islands" (S.tappedCount S.bob after) 3
    Spec.assertBool s (elem victimId (GameState.stack after)) "the Piker was not countered"
    Spec.assertEqWith s "Mana Leak finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  -- CR 608.2d's timing, from the other side of Prompt.ChooseToPay's claim that a
  -- countered Mana Leak never asks: the cost is offered when the spell RESOLVES,
  -- so a spell that never resolves offers nothing. The MagicalHackTiming pair
  -- makes the same argument about a resolution-time choice, and this is built the
  -- same way -- the CAST is inside the recording, because an engine that offered
  -- the cost at CR 601.2b-f would raise its prompt there and a transcript opened
  -- afterwards would never see it.
  Spec.it s "CR 608.2d a countered Mana Leak never offers its cost" $ do
    island <- S.printingOf s registry "Island"
    manaLeak <- S.printingOf s registry "Mana Leak"
    piker <- S.printingOf s registry "Goblin Piker"
    cancel <- S.printingOf s registry "Cancel"
    -- SIX Islands for bob, not three: three pay Cancel's {1}{U}{U} and three are
    -- left over. Without the spare three he could not have paid {3} anyway, and
    -- an empty transcript would prove nothing (CR 118.3 would be the reason).
    let (victimId, leakId, board) = manaLeakHand island manaLeak piker 6
        (cancelId, gs) = S.addHandCard cancel S.bob board
        exchange = do
          S.cast S.alice leakId
          S.cast S.bob cancelId
          Stack.resolveTop
        ((_, after), transcript) = Replay.record (bobPaysAndCounters victimId) gs exchange
    -- The control: the exchange really happened. CR 701.6a puts the countered
    -- Leak into its owner's graveyard, and CR 608.2n puts Cancel into bob's.
    Spec.assertEqWith s "CR 701.6a: the countered Leak is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "CR 608.2n: Cancel resolved into bob's" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertBool s (elem victimId (GameState.stack after)) "and the Piker, never the Leak's business again, is still on the stack"
    -- And the point: a spell that never resolves never offers its cost.
    Spec.assertEqWith s "bob was never offered the {3}" (payResponses transcript) []
    -- Not because he could not have paid it: three Islands are still untapped,
    -- and this interpreter pays whenever it is asked. Offered at either end of
    -- the exchange, this would read 6.
    Spec.assertEqWith s "only Cancel's three Islands are tapped" (S.tappedCount S.bob after) 3

-- Announces `x` for CR 601.2b's variable, and pays whatever a resolving spell
-- offers bob (paysFor, and its reasons). Rank-1 for paysFor's reason too: the
-- implicit forall is outermost, so `announcesXBobPays 2` is the `forall r. Prompt
-- r -> r` that Replay.record wants.
announcesXBobPays :: Natural -> Prompt.Prompt r -> r
announcesXBobPays x p = case p of
  Prompt.ChooseX {} -> x
  _ -> bobPaysAnswer p

-- The X answers in a transcript, in order. Empty during a resolution is the
-- point: CR 107.3a's value was announced at the CAST, so nothing asks again.
xResponses :: [Response.Response] -> [Response.Response]
xResponses = filter isXResponse

isXResponse :: Response.Response -> Bool
isXResponse response = case response of
  Response.ChoseX _ -> True
  _ -> False

-- manaLeakHand's board with Clash of Wills in alice's hand instead, cast at bob's
-- Piker with `x` announced at CR 601.2b. alice holds FIVE Islands, enough for
-- {X}{U} at either X the cases below announce, so the two boards differ in
-- nothing but that announcement; bob holds THREE, which is what makes {2}
-- payable and {4} not.
clashBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Natural -> (ObjectId.ObjectId, GameState.GameState)
clashBoard island clash piker x =
  let base = S.landsInPlay island 5
      withBob = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) base [1 .. 3 :: Int]
      (victimId, onStack) = S.spellOnStack piker S.bob withBob
      (gs, clashId) = S.handOne clash onStack
   in (victimId, snd (Engine.runGamePure (announcesXBobPays x) gs (S.cast S.alice clashId)))

-- The board every Rakshasa's Disdain case starts from. alice has three Islands
-- (the Disdain's {2}{U}), a Disdain in hand and `aliceGraveyard` cards in her
-- graveyard; bob has four untapped Islands, ONE card in his graveyard, and a
-- Goblin Piker already on the stack. Returns the Piker's id and the state after
-- alice casts the Disdain at it.
--
-- The three counts are all distinct on purpose. "Your graveyard" is the RESOLVING
-- spell's controller's (CR 109.5), while CR 118.12's payer is the TARGETED spell's
-- controller, so a gate measured against the payer demands {1} here and taps one
-- Island; one measured as counters on its own source demands {0} and taps none;
-- and bob's fourth Island is the spare that keeps "tapped three" from being "he
-- tapped everything he had".
--
-- manaLeakBoard's shape, and its reason for the Piker's placement: the Piker is
-- on the stack before the Disdain is cast, so it holds the lower object id and
-- identityAnswer's ChooseTargets aims the Disdain at it.
disdainBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
disdainBoard island disdain piker aliceGraveyard =
  let base = S.landsInPlay island 3
      withBob = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) base [1 .. 4 :: Int]
      aliceYard = List.foldl' (\g _ -> snd (S.addGraveyardCard piker S.alice g)) withBob [1 .. aliceGraveyard]
      bobYard = snd (S.addGraveyardCard piker S.bob aliceYard)
      (victimId, onStack) = S.spellOnStack piker S.bob bobYard
      (gs, disdainId) = S.handOne disdain onStack
   in (victimId, snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice disdainId)))

-- CR 118.12: Rakshasa's Disdain's "Counter target spell unless its controller
-- pays {1} for each card in your graveyard" -- the same offer Mana Leak makes,
-- with a cost the resolution MULTIPLIES by something that is not a counter on its
-- own source (Pawl.Types.PayGate.perEach).
--
-- Two graveyard sizes, because one cannot tell a count from a constant: three
-- cards demand {3} and two demand {2}, off boards that differ in nothing else.
rakshasasDisdainSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rakshasasDisdainSpec s registry = Spec.describe s "RakshasasDisdain" $ do
  Spec.it s "CR 118.12 the offer is {1} for each card in the RESOLVING controller's graveyard" $ do
    island <- S.printingOf s registry "Island"
    disdain <- S.printingOf s registry "Rakshasa's Disdain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = disdainBoard island disdain piker 3
        ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "three cards in alice's graveyard made the offer {3}, tapping three of bob's four Islands" (S.tappedCount S.bob after) 3
    Spec.assertBool s (elem victimId (GameState.stack after)) "so the Piker was not countered"
    Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- alice's three plus the Disdain itself (CR 608.2n), which is put there after
    -- the gate was measured rather than before it.
    Spec.assertEqWith s "Rakshasa's Disdain finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 4
    -- CR 400.7 mints a fresh incarnation, so the permanent is counted rather than
    -- looked up by the spell's id.
    let played = snd (Engine.runGamePure bobPaysAnswer after Stack.resolveTop)
    Spec.assertEqWith s "and the Piker then resolves onto the battlefield" (S.creaturesInPlay S.bob played) 1
  -- The same board with one card fewer in alice's graveyard: the demand follows
  -- the count rather than sitting at a number this fixture happened to produce.
  Spec.it s "CR 118.12 a graveyard one card smaller demands one mana less" $ do
    island <- S.printingOf s registry "Island"
    disdain <- S.printingOf s registry "Rakshasa's Disdain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = disdainBoard island disdain piker 2
        ((_, after), transcript) = Replay.record bobPaysAnswer cast Stack.resolveTop
    Spec.assertEqWith s "two cards in alice's graveyard made the offer {2}, tapping two of bob's four Islands" (S.tappedCount S.bob after) 2
    Spec.assertBool s (elem victimId (GameState.stack after)) "so the Piker was not countered"
    Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
  -- CR 118.12a's other branch, off the FIRST case's board with bob's answer the
  -- only difference.
  Spec.it s "CR 118.12a declining the multiplied cost counters the spell" $ do
    island <- S.printingOf s registry "Island"
    disdain <- S.printingOf s registry "Rakshasa's Disdain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, cast) = disdainBoard island disdain piker 3
        ((_, after), transcript) = Replay.record S.identityAnswer cast Stack.resolveTop
    -- bob could have paid {3} out of four Islands, so the refusal is his rather
    -- than CR 118.3's.
    Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "the Piker was countered into bob's graveyard, beside the card already there" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "and never reached the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "declining spent nothing: bob's Islands are all untapped" (S.tappedCount S.bob after) 0

-- CR 118.4 / CR 107.3a: Clash of Wills, {X}{U} Instant, "Counter target spell
-- unless its controller pays {X}." The {X} of a cost paid at RESOLUTION (CR
-- 118.12) is the value the spell's own controller announced as it was cast, so
-- the payer is charged a number he had no say in and is never asked for one.
--
-- The two cases run the same board, the same hand and the same interpreter, and
-- differ in NOTHING but the X alice announced: bob's three Islands cover {2} and
-- cannot cover {4}. A gate that left the symbol unsubstituted reads {0} (see
-- Pawl.Engine.Cost's costGenericOf), which is payable for free -- so both cases
-- would end with the Piker uncountered and nothing of bob's tapped.
clashOfWillsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
clashOfWillsSpec s registry = Spec.describe s "ClashOfWills" $ do
  Spec.it s "CR 107.3a the announced X is what the targeted spell's controller pays" $ do
    island <- S.printingOf s registry "Island"
    clash <- S.printingOf s registry "Clash of Wills"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = clashBoard island clash piker 2
        ((_, after), transcript) = Replay.record (announcesXBobPays 2) cast Stack.resolveTop
    -- The behaviour: {X} was announced as 2, so the offer cost bob exactly two of
    -- his three Islands. Unsubstituted it is {0} and taps none of them.
    Spec.assertEqWith s "paying the announced {2} tapped two of bob's three Islands" (S.tappedCount S.bob after) 2
    Spec.assertBool s (elem victimId (GameState.stack after)) "so the Piker was not countered"
    Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- CR 107.3a's other half: the value came off the announcement, so the
    -- resolution asked nobody for an X -- not the payer, whose choice it never is.
    Spec.assertEqWith s "and nobody was asked to choose an X during resolution" (xResponses transcript) []
    Spec.assertEqWith s "Clash of Wills finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  -- CR 118.3 against the SAME board with a larger announcement: an unpayable cost
  -- takes the "can't" branch with no prompt, exactly as Mana Leak's {3} does
  -- against two Islands.
  Spec.it s "CR 118.3 a larger announced X the controller cannot pay counters the spell" $ do
    island <- S.printingOf s registry "Island"
    clash <- S.printingOf s registry "Clash of Wills"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_victimId, cast) = clashBoard island clash piker 4
        ((_, after), transcript) = Replay.record (announcesXBobPays 4) cast Stack.resolveTop
    Spec.assertEqWith s "the Piker was countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and never reached the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "three Islands cannot pay the announced {4}, so bob was never asked" (payResponses transcript) []
    Spec.assertEqWith s "and nothing of bob's was tapped" (S.tappedCount S.bob after) 0
    Spec.assertEqWith s "Clash of Wills finished resolving into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

-- Puts every looked-at card where a look-and-split keyword action's FIRST list
-- goes -- the bottom of the library for scry (CR 701.22a), the graveyard for
-- surveil (CR 701.25a) -- so the action is observable on the BOARD and not only
-- in the transcript. Everything else declines, S.identityAnswer's answer.
digsAndDeclines :: Prompt.Prompt r -> r
digsAndDeclines p = case p of
  Prompt.ChooseScry _ _ looked -> (looked, [])
  Prompt.ChooseSurveil _ _ looked -> (looked, [])
  _ -> S.identityAnswer p

-- digsAndDeclines' exact pair: the same dig, and bob pays whatever a resolving
-- spell offers him. The two differ in NOTHING else, so a difference in outcome
-- between them is CR 118.12's answer and nothing else.
digsAndBobPays :: Prompt.Prompt r -> r
digsAndBobPays p = case p of
  Prompt.ChooseScry _ _ looked -> (looked, [])
  Prompt.ChooseSurveil _ _ looked -> (looked, [])
  _ -> bobPaysAnswer p

-- manaLeakHand's board with Stymied Hopes in alice's hand instead, cast at bob's
-- Piker, plus TWO cards in alice's library: CR 701.22a leaves nothing to ask
-- about a lone card that is the whole library, so a one-card library would elide
-- the scry prompt and the assertions below would read an unmoved library either
-- way. Returns the Piker, the card that starts SECOND in alice's library, and
-- the state after the cast.
--
-- bob keeps three untapped Islands against a {1}, so a refusal is his own answer
-- and never CR 118.3's.
stymiedBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
stymiedBoard island stymied piker =
  let base = S.landsInPlay island 2
      withBob = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) base [1 .. 3 :: Int]
      (victimId, onStack) = S.spellOnStack piker S.bob withBob
      (withHand, hopesId) = S.handOne stymied onStack
      (deepId, oneDeep) = S.addLibraryCard piker S.alice withHand
      (_topId, stocked) = S.addLibraryCard piker S.alice oneDeep
   in (victimId, deepId, snd (Engine.runGamePure S.identityAnswer stocked (S.cast S.alice hopesId)))

-- CR 118.12a scopes its rewriting to the INSTRUCTION the "unless" is attached to
-- and not to the ability, so a gate over one clause leaves its neighbours to
-- happen on both branches.
--
-- Stymied Hopes, {1}{U} Instant: "Counter target spell unless its controller
-- pays {1}. Scry 1." Two clauses, and only the first carries a payGate. Mana
-- Leak cannot tell the two readings apart -- it is one clause -- which is why
-- this waited on a card rather than on the carrier.
--
-- The two cases run the SAME board and the SAME cast and differ in nothing but
-- bob's answer. The load-bearing assertion is the LAST one in each: the same
-- scry happened either way.
stymiedHopesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stymiedHopesSpec s registry = Spec.describe s "StymiedHopes" $ do
  Spec.it s "CR 118.12a the controller declines, so the spell is countered -- and alice scries" $ do
    island <- S.printingOf s registry "Island"
    stymied <- S.printingOf s registry "Stymied Hopes"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, deepId, cast) = stymiedBoard island stymied piker
    -- cast-gate-tests-pass-vacuously: a "was countered" assertion passes for a
    -- spell that was never on the stack.
    Spec.assertBool s (elem victimId (GameState.stack cast)) "setup: the Piker is on the stack"
    let ((_, after), transcript) = Replay.record digsAndDeclines cast Stack.resolveTop
    Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "the Piker was countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "declining spent nothing" (S.tappedCount S.bob after) 0
    Spec.assertEqWith s "and the scry happened: the card that was second is on top" (take 1 (Game.zoneMembers Zone.Library S.alice after)) [deepId]
  Spec.it s "CR 118.12a the controller pays, so the spell survives -- and alice STILL scries" $ do
    island <- S.printingOf s registry "Island"
    stymied <- S.printingOf s registry "Stymied Hopes"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, deepId, cast) = stymiedBoard island stymied piker
    Spec.assertBool s (elem victimId (GameState.stack cast)) "setup: the Piker is on the stack"
    let ((_, after), transcript) = Replay.record digsAndBobPays cast Stack.resolveTop
    Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- The falsifier for a mode-wide gate: the {1} really left bob.
    Spec.assertEqWith s "the {1} cost bob one Island" (S.tappedCount S.bob after) 1
    Spec.assertBool s (elem victimId (GameState.stack after)) "the Piker was NOT countered"
    Spec.assertEqWith s "and the scry happened ANYWAY -- clause two carries no gate" (take 1 (Game.zoneMembers Zone.Library S.alice after)) [deepId]

-- CR 118.12's MANDATORY limb: "the 'If [a player] [does, doesn't, or can't]'
-- clause checks whether the player chose to pay an optional cost or STARTED TO
-- PAY a mandatory cost". A mandatory cost leaves its payer nothing to choose, so
-- Prompt.ChooseToPay is not raised at all.
--
-- Standstill, {1}{U} Enchantment: "When a player casts a spell, sacrifice this
-- enchantment. If you do, each of that player's opponents draws three cards."
-- No "may" anywhere, unlike Mana Leak (CR 118.12a supplies one) and Merfolk Seer
-- (which prints one). Written as an optional gate its controller could decline
-- and keep the enchantment -- weaker than printed, in their own favour.
--
-- THREE SEATS. At two players "each of that player's opponents" and "you" name
-- the same person, so a two-handed board cannot tell PlayerRef.EachPlayerExcept
-- thatPlayer from a wrong PlayerRef.Relative You. bob casts; alice and carol are
-- the two who draw. (CR 102.2 makes every other player an opponent in a
-- free-for-all, which is what lets the exclusion spell "that player's
-- opponents".)
--
-- THE INTERPRETER DECLINES EVERYTHING (S.identityAnswer), which is what makes
-- this discriminating: an optional gate would be declined, Standstill would
-- survive and nobody would draw. Boil, {3}{R} Instant "Destroy all Islands", is
-- bob's spell for kambalSpec's reason -- it targets nothing and nobody here
-- controls an Island.
standstillSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
standstillSpec s registry =
  Spec.describe
    s
    "Standstill"
    ( Spec.it s "CR 118.12 Standstill's controller is never offered the sacrifice" $ do
        mountain <- S.printingOf s registry "Mountain"
        standstill <- S.printingOf s registry "Standstill"
        boil <- S.printingOf s registry "Boil"
        piker <- S.printingOf s registry "Goblin Piker"
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addPermanent mountain pid g')) g [1 .. (n :: Int)]
            stock pid n g = List.foldl' (\g' _ -> snd (S.addLibraryCard piker pid g')) g [1 .. (n :: Int)]
            (standstillId, withEnchantment) = S.addPermanent standstill S.alice S.threePlayerGame
            -- Four cards each for the two drawers, so CR 104.3c decks nobody.
            stocked = stock S.carol 4 (stock S.alice 4 (addLands S.bob 4 withEnchantment))
            board =
              stocked
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.bob,
                  GameState.priority = Just S.bob
                }
            (boilId, gs) = S.addHandCard boil S.bob board
            exchange = S.cast S.bob boilId >> Engine.priorityLoop
            ((_, after), transcript) = Replay.record S.identityAnswer gs exchange
        Spec.assertBool s (Set.member standstillId (GameState.battlefield gs)) "setup: Standstill is on alice's battlefield"
        Spec.assertEqWith s "setup: nobody is holding cards" (fmap (`S.handSize` gs) [S.alice, S.bob, S.carol]) [0, 1, 0]
        -- THE assertion. A board check cannot tell "paid without asking" from "asked
        -- and answered yes"; this transcript can, and under an interpreter that
        -- declines every offer it also rules out an accidental payment.
        Spec.assertEqWith s "no payment was ever offered" (payResponses transcript) []
        Spec.assertBool s (not (Set.member standstillId (GameState.battlefield after))) "and Standstill was sacrificed anyway"
        Spec.assertEqWith s "CR 701.21a: into its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
        -- Both of bob's opponents, and not bob: the third seat is what separates
        -- "that player's opponents" from "you".
        Spec.assertEqWith s "so each of bob's opponents drew three, and bob drew none" (fmap (`S.handSize` after) [S.alice, S.bob, S.carol]) [3, 0, 3]
    )

-- manaLeakHand's board with Don't Make a Sound in alice's hand, cast at bob's
-- Piker, and three cards in alice's library for the surveil to reach. `bobLands`
-- is the whole variable: two Islands is exactly one payment's worth.
dontMakeASoundBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
dontMakeASoundBoard island sound piker bobLands =
  let base = S.landsInPlay island 2
      withBob = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) base [1 .. bobLands]
      (victimId, onStack) = S.spellOnStack piker S.bob withBob
      (withHand, soundId) = S.handOne sound onStack
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) withHand [1 .. 3 :: Int]
   in (victimId, snd (Engine.runGamePure S.identityAnswer stocked (S.cast S.alice soundId)))

-- CR 118.12 offers a resolution cost ONCE and reads the one answer, so two
-- clauses hanging off one payment are one offer and one charge.
--
-- Don't Make a Sound, {1}{U} Instant: "Counter target spell unless its
-- controller pays {2}. If they do, surveil 2." Stymied Hopes' shape with the
-- second clause moved from ungated to IfPaid, and the two cards together are the
-- minimal pair for the whole span question. The IfPaid clause names the IfNotPaid
-- one through PayGate.offeredAt.
--
-- Divergent in BOTH directions without that, which is why the two boards differ
-- only in bob's Islands: on two he cannot afford a second offer, so the surveil
-- would not happen; on four he can, and he would be charged {4} for a {2}.
dontMakeASoundSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dontMakeASoundSpec s registry = Spec.describe s "DontMakeASound" $ do
  Spec.it s "CR 118.12 one payment answers both clauses, on a board that can afford only one" $ do
    island <- S.printingOf s registry "Island"
    sound <- S.printingOf s registry "Don't Make a Sound"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = dontMakeASoundBoard island sound piker 2
    Spec.assertBool s (elem victimId (GameState.stack cast)) "setup: the Piker is on the stack"
    Spec.assertEqWith s "setup: alice's library holds three cards" (length (Game.zoneMembers Zone.Library S.alice cast)) 3
    let ((_, after), transcript) = Replay.record digsAndBobPays cast Stack.resolveTop
    Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    Spec.assertBool s (elem victimId (GameState.stack after)) "the Piker was NOT countered"
    -- The IfPaid clause ran on the answer the IfNotPaid clause got. A second
    -- offer would have found bob tapped out and taken CR 118.3's branch, leaving
    -- this at three.
    Spec.assertEqWith s "and alice surveilled 2: two cards left her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  Spec.it s "CR 118.12 and bob is charged {2} once, on a board that could afford twice" $ do
    island <- S.printingOf s registry "Island"
    sound <- S.printingOf s registry "Don't Make a Sound"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = dontMakeASoundBoard island sound piker 4
        ((_, after), transcript) = Replay.record digsAndBobPays cast Stack.resolveTop
    Spec.assertBool s (elem victimId (GameState.stack cast)) "setup: the Piker is on the stack"
    Spec.assertEqWith s "bob was asked exactly once" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- Two, not four. This is the assertion the four-Island board exists for: with
    -- an offer per clause bob can afford both and pays twice.
    Spec.assertEqWith s "and {2} cost him exactly two Islands" (S.tappedCount S.bob after) 2
    Spec.assertEqWith s "the surveil still happened" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  Spec.it s "CR 118.12 declining counters the spell, and there is no surveil" $ do
    island <- S.printingOf s registry "Island"
    sound <- S.printingOf s registry "Don't Make a Sound"
    piker <- S.printingOf s registry "Goblin Piker"
    let (victimId, cast) = dontMakeASoundBoard island sound piker 4
        ((_, after), transcript) = Replay.record digsAndDeclines cast Stack.resolveTop
    Spec.assertBool s (elem victimId (GameState.stack cast)) "setup: the Piker is on the stack"
    -- Once here too: the IfPaid clause reads the recorded refusal rather than
    -- offering the {2} again, which bob could have afforded.
    Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "the Piker was countered into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and alice's library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 3

-- Whipstitched Zombie and one untapped Swamp on alice's battlefield, her upkeep
-- begun and the trigger settled onto the stack (CR 603.3b). Returns the Zombie,
-- the Swamp and that state.
--
-- The Bitterblossom fixture in Pawl.TriggerSpec, one step short: the trigger is
-- left ON the stack so a case can assert what the board looked like before it
-- resolved, which is what rules out a Zombie that was never there.
zombieUpkeep :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
zombieUpkeep swamp zombie =
  let (zombieId, g1) = S.addPermanent zombie S.alice (Setup.emptyGame S.bothPlayers)
      (swampId, g2) = S.addPermanent swamp S.alice g1
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (g2 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in (zombieId, swampId, snd (Engine.runGamePure S.identityAnswer begun Engine.settleForPriority))

-- CR 118.12a again, reached from Pawl.Engine.Resolve.resolveModes rather than
-- resolveSpellWith: Whipstitched Zombie's "At the beginning of your upkeep,
-- sacrifice this creature unless you pay {B}" is a TRIGGERED ability, so the
-- ability executor asks the gate instead of the spell path. The seam is one
-- `paid` call shared by both, and this is the half Mana Leak cannot reach.
--
-- The payer is the ability's own controller (CR 603.3a's "your upkeep", bound
-- into Pawl.Engine.Binding.you as the trigger is placed) rather than Mana Leak's
-- targeted-spell controller -- the same slot read answering a different card.
--
-- Both cases start from the SAME board and the SAME settled trigger and differ
-- in NOTHING but alice's answer.
whipstitchedZombieSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
whipstitchedZombieSpec s registry = Spec.describe s "WhipstitchedZombie" $ do
  Spec.it s "CR 118.12a declining the {B} sacrifices the Zombie to its owner's graveyard" $ do
    swamp <- S.printingOf s registry "Swamp"
    zombie <- S.printingOf s registry "Whipstitched Zombie"
    let (zombieId, swampId, onStack) = zombieUpkeep swamp zombie
        ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
    -- The two controls the assertions below would otherwise be satisfied by:
    -- a Zombie that was never on the battlefield, and a trigger that never
    -- fired. Both are ruled out BEFORE the resolution.
    Spec.assertBool s (S.onBattlefield zombieId onStack) "the Zombie is on the battlefield before its upkeep trigger resolves"
    Spec.assertBool s (not (null (GameState.stack onStack))) "and the upkeep trigger really reached the stack"
    -- alice COULD have paid -- one untapped Swamp -- so the refusal is hers
    -- rather than CR 118.3's.
    Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    -- CR 701.21a: sacrificed, so it is IN the graveyard rather than merely gone.
    -- The Swamp is still on the battlefield, so that one graveyard card can only
    -- be the Zombie.
    Spec.assertEqWith s "the Zombie is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertBool s (S.onBattlefield swampId after) "and the Swamp, which is not what was sacrificed, is still in play"
    Spec.assertEqWith s "no creature is left in play" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "declining spent nothing: the Swamp is untapped" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  Spec.it s "CR 118.12a paying the {B} leaves the Zombie on the battlefield" $ do
    swamp <- S.printingOf s registry "Swamp"
    zombie <- S.printingOf s registry "Whipstitched Zombie"
    let (zombieId, _swampId, onStack) = zombieUpkeep swamp zombie
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- CR 605.3a lets her tap the Swamp for it mid-resolution.
    Spec.assertEqWith s "paying tapped the Swamp" (S.tappedCount S.alice after) 1
    -- The same id, not a fresh one: nothing moved zones, so this is the very
    -- permanent the trigger was about.
    Spec.assertBool s (S.onBattlefield zombieId after) "the Zombie is still on the battlefield"
    Spec.assertEqWith s "nothing was sacrificed" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

-- alice's Lithophage, one Mountain, one Island and a Seat of the Synod, with a
-- Magical Hack in hand; when `hacked`, she casts it at the Lithophage and lets it
-- resolve first, swapping Mountain for Island. Her upkeep is then begun and the
-- trigger settled onto the stack -- zombieUpkeep's shape with a text change in
-- front of it. Returns the Lithophage and that state.
--
-- TWO land types is what makes either case discriminating: with only a Mountain
-- out, "the gate asks for an Island" and "the gate kept the printed word" pick
-- the same permanent to sacrifice.
--
-- Seat of the Synod ({T}: Add {U}, an Artifact Land with no land subtype at all,
-- checked against Scryfall) pays for the Hack, and both lands start TAPPED, so it
-- is the only untapped blue source and the hacked and unhacked boards differ in
-- nothing but whether the Hack was cast. Being neither a Mountain nor an Island
-- it can never join the sacrifice candidates either. Tap state gates nothing
-- about a sacrifice (CR 701.21a), so both lands stay eligible.
lithophageUpkeep :: Bool -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
lithophageUpkeep hacked lithophage mountain island seat magicalHack =
  let (lithoId, g1) = S.addPermanent lithophage S.alice (Setup.emptyGame S.bothPlayers)
      (mountainId, g2) = S.addPermanent mountain S.alice g1
      (islandId, g3) = S.addPermanent island S.alice g2
      (_seatId, g4) = S.addPermanent seat S.alice g3
      (hackId, g5) = S.addHandCard magicalHack S.alice g4
      tapped = S.tapObject islandId (S.tapObject mountainId g5)
      ready = tapped {GameState.priority = Just S.alice}
      hackIt g = S.runPure (hackAt lithoId Subtype.Mountain Subtype.Island) g (do S.cast S.alice hackId; Stack.resolveTop)
      board = if hacked then hackIt ready else ready
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (board {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in (lithoId, snd (Engine.runGamePure S.identityAnswer begun Engine.settleForPriority))

-- CR 612.1 reaching the cost a triggered ability OFFERS as it resolves (CR
-- 118.12) -- the half neither Pawl.ActivateSpec's Dark Heart of the Wood (an
-- ACTIVATION cost) nor Pawl.TriggerSpec's Barbarian Outcast (a trigger's own
-- CONDITION) covers.
--
-- Lithophage {3}{R}{R} Creature -- Insect 7/7, "At the beginning of your upkeep,
-- sacrifice this creature unless you sacrifice a Mountain." (checked against
-- Scryfall). CR 118.12a's rewriting makes that an IfNotPaid gate whose clause
-- sacrifices the Lithophage, and CR 612.2 licenses the swap because the gate's
-- criterion reads a land type word used as a land type.
--
-- alice PAYS in both cases, so the Lithophage lives in both and the whole
-- difference is which land died. The graveyard COUNT cannot tell the two
-- readings apart -- one land either way -- so the load-bearing assertions are
-- the two land NAMES, and they come first.
lithophageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lithophageSpec s registry =
  let mountainName = CardName.MkCardName (Text.pack "Mountain")
      islandName = CardName.MkCardName (Text.pack "Island")
      run hacked = do
        lithophage <- S.printingOf s registry "Lithophage"
        mountain <- S.printingOf s registry "Mountain"
        island <- S.printingOf s registry "Island"
        seat <- S.printingOf s registry "Seat of the Synod"
        magicalHack <- S.printingOf s registry "Magical Hack"
        let (lithoId, onStack) = lithophageUpkeep hacked lithophage mountain island seat magicalHack
            ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
        pure (lithoId, onStack, after, transcript)
   in Spec.describe s "Lithophage" $ do
        -- The control, and what keeps the case below from passing vacuously:
        -- unhacked, the printed word stands and the MOUNTAIN is the only thing
        -- that can pay.
        Spec.it s "CR 118.12 whole card: an unhacked Lithophage's gate demands the Mountain" $ do
          (lithoId, onStack, after, transcript) <- run False
          Spec.assertEqWith s "the Mountain is gone" (S.countOnBattlefieldByName mountainName S.alice after) 0
          Spec.assertEqWith s "the Island survives" (S.countOnBattlefieldByName islandName S.alice after) 1
          Spec.assertBool s (elem (Just mountainName) (namesIn Zone.Graveyard S.alice after)) "the Mountain was sacrificed, so it is IN alice's graveyard"
          Spec.assertBool s (S.onBattlefield lithoId onStack) "the Lithophage is on the battlefield before its upkeep trigger resolves"
          Spec.assertBool s (not (null (GameState.stack onStack))) "and the upkeep trigger really reached the stack"
          Spec.assertBool s (S.onBattlefield lithoId after) "paying kept the Lithophage on the battlefield"
          Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
        -- The swap. alice's board did not move -- the same Mountain and the same
        -- Island -- but the gate printed on the Lithophage now offers "sacrifice
        -- an Island", so the Island is what dies and the Mountain is not even
        -- eligible.
        Spec.it s "CR 612.1 whole card: hacking Lithophage moves which land its CR 118.12 gate demands" $ do
          (lithoId, onStack, after, transcript) <- run True
          Spec.assertEqWith s "the Island is gone" (S.countOnBattlefieldByName islandName S.alice after) 0
          Spec.assertEqWith s "the Mountain survives" (S.countOnBattlefieldByName mountainName S.alice after) 1
          Spec.assertBool s (elem (Just islandName) (namesIn Zone.Graveyard S.alice after)) "the Island was sacrificed, so it is IN alice's graveyard"
          Spec.assertBool s (S.onBattlefield lithoId onStack) "the Lithophage is on the battlefield before its upkeep trigger resolves"
          Spec.assertBool s (not (null (GameState.stack onStack))) "and the upkeep trigger really reached the stack"
          Spec.assertBool s (S.onBattlefield lithoId after) "paying kept the Lithophage on the battlefield"
          Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Circling Vultures on alice's battlefield with the given cards already in her
-- graveyard, IN THE ORDER GIVEN so the last one is the top (CR 404.1), her
-- upkeep begun and the trigger settled onto the stack -- zombieUpkeep's shape
-- one card over. Returns the Vultures and that state; the buried cards are
-- asserted on by NAME, since CR 400.7 renames them on the way out.
vulturesUpkeep :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
vulturesUpkeep vultures buried =
  let (vulturesId, g1) = S.addPermanent vultures S.alice (Setup.emptyGame S.bothPlayers)
      bury g printing = snd (S.addGraveyardCard printing S.alice g)
      g2 = List.foldl' bury g1 buried
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (g2 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in (vulturesId, snd (Engine.runGamePure S.identityAnswer begun Engine.settleForPriority))

-- CR 118.12a's gate again, over the cost component that has no choice in it:
-- Circling Vultures' "At the beginning of your upkeep, sacrifice this creature
-- unless you exile the top creature card of your graveyard"
-- (CostComponent.ExileTopFromGraveyard). CR 404.2 keeps a graveyard's order
-- fixed, so "the top creature card" names ONE card and nothing is prompted for
-- -- which is the whole difference from Headless Skaab's chosen exile
-- (Pawl.CostSpec).
--
-- THE FIXTURE SHAPE that makes the last case discriminating: TWO creature cards
-- in the graveyard, buried in a known order. An implementation reading the
-- wrong end exiles the other one and passes every other case here.
circlingVulturesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
circlingVulturesSpec s registry = Spec.describe s "CirclingVultures" $ do
  Spec.it s "CR 118.3 an empty graveyard cannot pay, so the Vultures are sacrificed" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    let (vulturesId, onStack) = vulturesUpkeep vultures []
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (S.onBattlefield vulturesId onStack) "the Vultures are on the battlefield before the trigger resolves"
    Spec.assertBool s (not (null (GameState.stack onStack))) "and the upkeep trigger really reached the stack"
    Spec.assertBool s (not (S.onBattlefield vulturesId after)) "the Vultures were sacrificed"
    Spec.assertEqWith s "CR 701.21a into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
    -- CR 118.3: an unpayable cost is not offered, so this interpreter's
    -- willingness to pay never comes up.
    Spec.assertEqWith s "alice was not asked to pay" (payResponses transcript) []
  -- The primary negative, Headless Skaab's argument unchanged: an
  -- implementation that ignored the Filter still refuses an empty graveyard,
  -- and only a graveyard holding exactly one INELIGIBLE card tells the two
  -- apart.
  Spec.it s "CR 601.2f a noncreature card in the graveyard cannot pay it either" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (vulturesId, onStack) = vulturesUpkeep vultures [bolt]
        ((_, after), _) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (not (S.onBattlefield vulturesId after)) "the Vultures were sacrificed"
    Spec.assertEqWith s "nothing was exiled" (namesIn Zone.Exile S.alice after) []
    -- CR 404.1 again, from the other side: the sacrificed Vultures arrive on top
    -- of the Bolt that could not pay for them.
    Spec.assertEqWith
      s
      "and the Bolt is still in the graveyard, under them"
      (namesIn Zone.Graveyard S.alice after)
      [Just (S.printingName bolt), Just (S.printingName vultures)]
  Spec.it s "CR 118.12a a creature card in the graveyard pays it and the Vultures survive" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    piker <- S.printingOf s registry "Goblin Piker"
    let (vulturesId, onStack) = vulturesUpkeep vultures [piker]
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (S.onBattlefield vulturesId after) "the Vultures are still on the battlefield"
    Spec.assertEqWith s "CR 406.2 the Piker was exiled" (namesIn Zone.Exile S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "and alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "alice was asked whether to pay, and once" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- CR 404.2 leaves nothing to choose, so paying prompts for no exile.
    Spec.assertEqWith s "and never asked WHICH card to exile" (filter isExileResponse transcript) []
  -- CR 404.1 / 404.2: "the TOP creature card". The Bird Maiden went to the
  -- graveyard second, so it is the one that leaves; the Piker underneath it
  -- stays. Nothing is prompted for, which is the claim the assertion on the
  -- Piker carries.
  Spec.it s "CR 404.2 with two creature cards it is the TOP one that is exiled" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    piker <- S.printingOf s registry "Goblin Piker"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (vulturesId, onStack) = vulturesUpkeep vultures [piker, birdMaiden]
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertBool s (S.onBattlefield vulturesId after) "the Vultures are still on the battlefield"
    Spec.assertEqWith s "the Bird Maiden, buried last, was exiled" (namesIn Zone.Exile S.alice after) [Just (S.printingName birdMaiden)]
    Spec.assertEqWith s "and the Piker under it is still in the graveyard" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "with two candidates, alice was still never asked which" (filter isExileResponse transcript) []

-- Merfolk Seer dead and its CR 603.6c trigger settled onto the stack, with two
-- untapped lands of ONE printing under alice and two cards in her library. The
-- Seer is killed by lethal damage (CR 704.5g) rather than put into the graveyard
-- by hand, so the trigger fires the way a game fires it.
--
-- `land` is the only thing the two boards below differ in: two Islands can pay
-- {1}{U} and two Mountains cannot, while the seats, the stock, the library and
-- the damage are the same either way. A negative built by taking the lands AWAY
-- would prove only that a player with no mana pays nothing.
--
-- The library is a Mountain under a Goblin Piker, so the drawn card has a name
-- no other card in the fixture shares and the assertion cannot be satisfied by
-- some other card arriving in hand. Two cards, so the draw does not empty the
-- library and CR 104.3c never enters into it.
seerDies :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
seerDies land mountain piker seer =
  let (seerId, g1) = S.addPermanent seer S.alice (Setup.emptyGame S.bothPlayers)
      g2 = S.landsFor land S.alice 2 g1
      (_, g3) = S.addLibraryCard mountain S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      damaged = S.markDamage seerId 3 g4
   in (seerId, snd (Engine.runGamePure S.identityAnswer damaged Engine.settleForPriority))

-- CR 118.12's OTHER branch, the one CR 118.12a's rewriting does not reach:
-- Merfolk Seer's "When this creature dies, you may pay {1}{U}. If you do, draw a
-- card" -- the instructions run when the cost WAS paid (PayBranch.IfPaid), where
-- Mana Leak's and Whipstitched Zombie's run when it was not.
--
-- THE COMPETING READING these three cases exist to rule out is that the draw is
-- unconditional and the payment merely offered beside it -- which is what an
-- engine ignoring the gate does, and what every board where the payment always
-- succeeds looks like. Two of the three cases have alice NOT draw: one where she
-- could pay and would not, one where she could not pay at all. The other
-- competing reading, that the effects hang off the refusal, is ruled out by the
-- first case drawing.
--
-- The answerer is pinned rather than searching for a legal option -- `paysFor
-- S.alice` pays whoever is alice and declines otherwise, S.identityAnswer
-- declines -- so a mutation cannot be repaired by answering differently.
merfolkSeerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
merfolkSeerSpec s registry = Spec.describe s "MerfolkSeer" $ do
  Spec.it s "CR 118.12 paying the {1}{U} draws the card" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    seer <- S.printingOf s registry "Merfolk Seer"
    let (seerId, onStack) = seerDies island mountain piker seer
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    -- The two controls: the Seer really died, and its trigger really reached the
    -- stack. Without them the draw assertions below would be about nothing.
    Spec.assertBool s (not (S.onBattlefield seerId onStack)) "the Seer died before its trigger resolved"
    Spec.assertBool s (not (null (GameState.stack onStack))) "and the dies trigger reached the stack"
    Spec.assertEqWith s "alice's hand was empty" (S.handSize S.alice onStack) 0
    Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- CR 605.3a lets her tap both Islands for it mid-resolution.
    Spec.assertEqWith s "paying tapped both Islands" (S.tappedCount S.alice after) 2
    -- Named rather than counted: the card that arrived is the one off the top of
    -- her library.
    Spec.assertEqWith s "and she drew the Piker" (namesIn Zone.Hand S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  -- The same board, the same trigger, and NOTHING different but the answer.
  Spec.it s "CR 118.12 declining the {1}{U} draws nothing" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    seer <- S.printingOf s registry "Merfolk Seer"
    let (_seerId, onStack) = seerDies island mountain piker seer
        ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
    -- alice COULD have paid -- two untapped Islands -- so the refusal is hers
    -- rather than CR 118.3's, and the empty hand below is the branch and not the
    -- board.
    Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "declining spent nothing: both Islands are untapped" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "and drew nothing" (namesIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  -- CR 118.3: two Mountains cannot pay {1}{U}, so there is one possible answer
  -- and the prompt is not raised -- proved by the transcript, under the
  -- interpreter that WOULD have paid. Mana is present and only the COLOUR is
  -- wrong, so nothing here passes for want of lands.
  Spec.it s "CR 118.3 a controller who cannot pay {1}{U} is not asked, and draws nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    seer <- S.printingOf s registry "Merfolk Seer"
    let (_seerId, onStack) = seerDies mountain mountain piker seer
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    Spec.assertEqWith s "alice was never asked" (payResponses transcript) []
    Spec.assertEqWith s "nothing was tapped" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "and she drew nothing" (namesIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

-- The battlefield objects whose current face carries this name. Used instead of
-- an id taken before the cast, since CR 400.7 mints a new object on the way in.
byNameOnBattlefield :: String -> GameState.GameState -> [ObjectId.ObjectId]
byNameOnBattlefield name gs =
  [ oid
  | oid <- Set.toList (GameState.battlefield gs),
    fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))
  ]

-- Fortress Kin-Guard cast from alice's hand off two Plains and resolved, with its
-- CR 603.6a enters trigger settled onto the stack but NOT resolved -- zombieUpkeep's
-- shape, so a case can read the board before the endure happens. `others` are put
-- on alice's battlefield first.
kinGuardOnStack :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> GameState.GameState
kinGuardOnStack plains kinGuard others =
  let base = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) (S.landsInPlay plains 2) others
      (gs, spellId) = S.handOne kinGuard base
      cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
   in S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)

-- CR 701.63a's endure, which is CR 118.12a's "unless" over a cost that puts
-- counters rather than one that spends a resource: "that permanent's controller
-- creates an N/N white Spirit creature token UNLESS THEY PUT N +1/+1 COUNTERS ON
-- THAT PERMANENT" (CostComponent.PutPlusOneCountersOnThis). Fortress Kin-Guard
-- ({1}{W} 1/2 Creature -- Dog Soldier, "When this creature enters, it endures 1")
-- is the printing.
--
-- The first two cases start from the SAME board and the SAME settled trigger and
-- differ in NOTHING but alice's answer, Whipstitched Zombie's shape. Endure 1 on a
-- 1/2 keeps every reading distinct: 2/3 with the counter, 1/2 with the token, 3/4
-- under Hardened Scales.
fortressKinGuardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fortressKinGuardSpec s registry = Spec.describe s "FortressKinGuard" $ do
  let kinGuardOf = byNameOnBattlefield "Fortress Kin-Guard"
  Spec.it s "CR 701.63a paying the counters leaves the Kin-Guard a 2/3 and makes no token" $ do
    plains <- S.printingOf s registry "Plains"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    let onStack = kinGuardOnStack plains kinGuard []
        ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
    case kinGuardOf onStack of
      [guardId] -> do
        -- The controls: the Kin-Guard really entered, and its trigger really
        -- reached the stack, before anything below is read.
        Spec.assertEqWith s "it entered as a 1/2 with no counters" (S.powerToughnessOf guardId onStack, S.counterOf CounterKind.PlusOnePlusOne guardId onStack) (Just (1, 2), 0)
        Spec.assertEqWith s "and its enters trigger is on the stack" (length (GameState.stack onStack)) 1
        Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
        Spec.assertEqWith s "CR 122.6: one +1/+1 counter went on" (S.counterOf CounterKind.PlusOnePlusOne guardId after) 1
        Spec.assertEqWith s "CR 613.4c: so it reads 2/3" (S.powerToughnessOf guardId after) (Just (2, 3))
        Spec.assertEqWith s "CR 118.12a: the paid branch made no Spirit" (S.tokensOf after) []
        Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
      other -> Spec.assertFailure s ("expected one Fortress Kin-Guard, got " <> show (length other))
  Spec.it s "CR 701.63a declining creates a 1/1 white Spirit and leaves the Kin-Guard a 1/2" $ do
    plains <- S.printingOf s registry "Plains"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    let onStack = kinGuardOnStack plains kinGuard []
        ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
    case kinGuardOf onStack of
      [guardId] -> do
        Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
        Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne guardId after) 0
        Spec.assertEqWith s "so it is still a 1/2" (S.powerToughnessOf guardId after) (Just (1, 2))
        case S.tokensOf after of
          [spiritId] -> do
            Spec.assertEqWith s "CR 111.4: the token is named Spirit Token" (fmap Face.name (Game.faceOf spiritId after)) (Just . CardName.MkCardName $ Text.pack "Spirit Token")
            Spec.assertEqWith s "a Creature" (Projection.cardTypesOf spiritId after) (Set.singleton CardType.Creature)
            Spec.assertEqWith s "with subtype Spirit" (Projection.subtypesOf spiritId after) (Set.singleton Subtype.Spirit)
            -- CR 202.2e: the token face carries a colour indicator, which is how
            -- "white" is spelled for an object with no mana cost.
            Spec.assertEqWith s "and white" (Projection.colorsOf spiritId after) (Set.singleton Color.White)
            Spec.assertEqWith s "endure 1 makes it 1/1" (S.powerToughnessOf spiritId after) (Just (1, 1))
            Spec.assertEqWith s "CR 111.2: alice created it, so alice controls it" (Projection.controllerOf spiritId after) (Just S.alice)
          other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
      other -> Spec.assertFailure s ("expected one Fortress Kin-Guard, got " <> show (length other))
  -- CR 614.1 over a cost paid DURING a resolution. The board differs from the
  -- first case in NOTHING but the Hardened Scales, and what it proves is that the
  -- payment places its counter through CR 122.6's funnel rather than writing it
  -- onto the object directly: a direct write reads 2/3 here.
  --
  -- NOT the payment moment. Hardened Scales is CR 614.1's passive subject, which
  -- reaches a placement at either moment; the moment's own split is pinned by
  -- Pawl.ReplacementSpec's blight pair and by Pawl.PlaneswalkerSpec's loyalty
  -- case, all three of them CR 614.16 subjects (Doubling Season).
  Spec.it s "CR 614.1 Hardened Scales sees endure's counter, so the Kin-Guard reads 3/4" $ do
    plains <- S.printingOf s registry "Plains"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    scales <- S.printingOf s registry "Hardened Scales"
    let onStack = kinGuardOnStack plains kinGuard [scales]
        after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
    case kinGuardOf onStack of
      [guardId] -> do
        Spec.assertEqWith s "it still entered as a 1/2" (S.powerToughnessOf guardId onStack) (Just (1, 2))
        Spec.assertEqWith s "one counter became two" (S.counterOf CounterKind.PlusOnePlusOne guardId after) 2
        Spec.assertEqWith s "so it reads 3/4" (S.powerToughnessOf guardId after) (Just (3, 4))
        Spec.assertEqWith s "and still no Spirit" (S.tokensOf after) []
      other -> Spec.assertFailure s ("expected one Fortress Kin-Guard, got " <> show (length other))
  -- CR 118.3 plus CR 701.63a's own ruling: "if you can't put +1/+1 counters on the
  -- creature for any reason (for example, if the creature is no longer on the
  -- battlefield), you'll just create a Spirit token." A Lightning Bolt kills the
  -- 1/2 while its endure trigger waits, and CR 113.7a resolves the trigger off a
  -- source that has left.
  --
  -- The interpreter PAYS wherever it is offered a cost, so an empty transcript
  -- says the prompt was never raised rather than that alice would have refused.
  Spec.it s "CR 118.3 a Kin-Guard that has left cannot pay, so the Spirit is created unasked" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    kinGuard <- S.printingOf s registry "Fortress Kin-Guard"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let base = snd (S.addPermanent mountain S.alice (S.landsInPlay plains 2))
        (withGuard, guardSpell) = S.handOne kinGuard base
        (boltSpell, withBolt) = S.addHandCard bolt S.alice withGuard
        cast = S.runPure S.identityAnswer withBolt (S.cast S.alice guardSpell)
        onStack = S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
        -- The Kin-Guard is the only creature on the board, so identityAnswer's
        -- lowest recipient is it; CR 704.5g then buries it in the settle.
        bolted = S.runPure S.identityAnswer onStack (S.cast S.alice boltSpell >> Stack.resolveTop >> Engine.settleForPriority)
        ((_, after), transcript) = Replay.record (paysFor S.alice) bolted Stack.resolveTop
    Spec.assertEqWith s "the Kin-Guard entered and its trigger is on the stack" (length (kinGuardOf onStack), length (GameState.stack onStack)) (1, 1)
    Spec.assertEqWith s "the Bolt killed it, and the endure trigger is still there" (length (kinGuardOf bolted), length (GameState.stack bolted)) (0, 1)
    Spec.assertEqWith s "CR 118.3: alice was never offered the counters" (payResponses transcript) []
    Spec.assertEqWith s "and the Spirit was created anyway" (length (S.tokensOf after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

-- CR 118.12 over a cost that moves a CARD rather than spending a resource:
-- Hakbal of the Surging Soul's "whenever Hakbal attacks, you may put a land card
-- from your hand onto the battlefield. If you don't, draw a card"
-- (CostComponent.PutCardFromHandOntoBattlefield, under PayBranch.IfNotPaid).
-- Merfolk Seer's shape one component over, and the three cases are that group's
-- three: paid, declined, and unpayable.
--
-- alice's hand is a Mountain, a Forest and a Goblin Piker. TWO lands, so the
-- payment is a real choice and the engine has to raise Prompt.ChooseCardInHand
-- rather than short-circuiting at one candidate; the Piker is the criterion's
-- own control, since a filter that admitted it would put a creature onto the
-- battlefield. The answerer names the MOUNTAIN, which is NOT the head of the
-- offered list -- Replay's own fallback for this prompt is the head, so an
-- implementation that dropped the answer would put the Forest there -- and the
-- Forest left behind is what makes "the land she chose" an assertion rather than
-- "a land".
--
-- Her library is a Bird Maiden over a Darksteel Myr, given deepest first since
-- both builders PREPEND: distinct from everything in hand, so "she drew" names a
-- card rather than counting one, and two deep so no case decks her (CR 104.3c).
--
-- Everything is asserted by NAME rather than by an id the fixture held: CR 400.7
-- mints a new object for the land on its way to the battlefield and for the card
-- on its way out of the library.
--
-- THE COMPETING READING these cases rule out is that the draw is unconditional
-- and the land merely offered beside it. The first case has alice NOT draw --
-- her library is untouched -- where the second and third do.
hakbalAttacking :: Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> GameState.GameState
hakbalAttacking hakbal inHand inLibrary =
  let (base, _, _) = S.combatBoardOf [hakbal] []
      withHand = List.foldl' (\g p -> snd (S.addHandCard p S.alice g)) base inHand
      withLibrary = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) withHand inLibrary
   in -- Declared under aggressiveAnswer, which attacks with everything; the
      -- settle is what puts the CR 508.2b attack trigger onto the stack, so a
      -- case can resolve it under its own interpreter.
      S.runPure S.aggressiveAnswer withLibrary (Combat.declareAttackers S.manaPerformer S.alice >> Engine.settleForPriority)

-- paysFor, plus a pinned answer to the card the payment offers: `which` when it
-- was offered, and the head otherwise -- the liar pattern, so a case that reads
-- back `which` is reading the engine's own offer rather than the fallback.
paysWith :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
paysWith who which p = case p of
  Prompt.ChooseCardInHand (Decider.MkDecider d) player _ candidates
    | d == who && player == who && List.elem which (NonEmpty.toList candidates) ->
        which
  _ -> paysFor who p

-- The hand-card answers in a transcript, in order.
handCardResponses :: [Response.Response] -> [Response.Response]
handCardResponses = filter isHandCardResponse

isHandCardResponse :: Response.Response -> Bool
isHandCardResponse response = case response of
  Response.ChoseCardInHand _ -> True
  _ -> False

hakbalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hakbalSpec s registry = Spec.describe s "Hakbal" $ do
  Spec.it s "CR 118.12 putting the land from hand onto the battlefield draws nothing" $ do
    hakbal <- S.printingOf s registry "Hakbal of the Surging Soul"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    myr <- S.printingOf s registry "Darksteel Myr"
    let attacking = hakbalAttacking hakbal [mountain, forest, piker] [myr, maiden]
        -- Both builders PREPEND, so her hand reads Piker, Forest, Mountain and
        -- the offered lands are Forest then Mountain. The Mountain is the LAST,
        -- which is what makes it not the fallback.
        mountainId = case reverse (Game.zoneMembers Zone.Hand S.alice attacking) of
          last_ : _ -> last_
          [] -> ObjectId.MkObjectId 0
        ((_, after), transcript) = Replay.record (paysWith S.alice mountainId) attacking Stack.resolveTop
    -- The two controls: Hakbal really attacked, and its trigger really reached
    -- the stack. Without them the assertions below would be about nothing.
    Spec.assertEqWith s "Hakbal was declared as an attacker" (length (S.attackerDeclarationsOf attacking)) 1
    Spec.assertEqWith s "and the attack trigger reached the stack" (length (GameState.stack attacking)) 1
    -- The gameplay assertion, ahead of every proxy: the land she NAMED is on the
    -- battlefield and the other one is not.
    Spec.assertEqWith
      s
      "the Mountain she chose is on the battlefield, and the Forest is not"
      (length (byNameOnBattlefield "Mountain" after), length (byNameOnBattlefield "Forest" after))
      (1, 0)
    -- CR 118.12's other half: the draw belongs to the branch she did NOT take.
    Spec.assertEqWith s "and no card was drawn: her library still holds both" (length (Game.zoneMembers Zone.Library S.alice after)) 2
    Spec.assertEqWith s "her hand is the Piker and the Forest" (namesIn Zone.Hand S.alice after) [Just (S.printingName piker), Just (S.printingName forest)]
    Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
    Spec.assertEqWith s "and named the Mountain, not the Forest the fallback would take" (handCardResponses transcript) [Response.ChoseCardInHand mountainId]
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  -- The same board, the same trigger, and NOTHING different but the answer.
  Spec.it s "CR 118.12a declining draws a card and leaves both lands in hand" $ do
    hakbal <- S.printingOf s registry "Hakbal of the Surging Soul"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    myr <- S.printingOf s registry "Darksteel Myr"
    let attacking = hakbalAttacking hakbal [mountain, forest, piker] [myr, maiden]
        ((_, after), transcript) = Replay.record S.identityAnswer attacking Stack.resolveTop
    -- The gameplay assertion first: the drawn card is NAMED, so this is the top
    -- of her library arriving and not a count.
    Spec.assertEqWith
      s
      "her hand gained the Bird Maiden off the top"
      (namesIn Zone.Hand S.alice after)
      [Just (S.printingName piker), Just (S.printingName forest), Just (S.printingName mountain), Just (S.printingName maiden)]
    -- alice COULD have paid -- two land cards in hand -- so the refusal is hers
    -- rather than CR 118.3's, and the land-less battlefield is the branch and
    -- not the board.
    Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "declining never asked WHICH card" (handCardResponses transcript) []
    Spec.assertEqWith s "nothing joined Hakbal on her battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1
    Spec.assertEqWith s "and one card left her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
  -- CR 118.3: a hand with no land card cannot pay, so there is one possible
  -- answer and the prompt is not raised -- proved by the transcript, under the
  -- interpreter that WOULD have paid. The board differs from the two above in
  -- exactly one thing: the two lands are gone from her hand.
  Spec.it s "CR 118.3 a controller holding no land card is not asked, and draws" $ do
    hakbal <- S.printingOf s registry "Hakbal of the Surging Soul"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    myr <- S.printingOf s registry "Darksteel Myr"
    let attacking = hakbalAttacking hakbal [piker] [myr, maiden]
        ((_, after), transcript) = Replay.record (paysFor S.alice) attacking Stack.resolveTop
    Spec.assertEqWith
      s
      "she drew the Bird Maiden"
      (namesIn Zone.Hand S.alice after)
      [Just (S.printingName piker), Just (S.printingName maiden)]
    Spec.assertEqWith s "alice was never asked to pay" (payResponses transcript) []
    Spec.assertEqWith s "nor asked which card" (handCardResponses transcript) []
    Spec.assertEqWith s "nothing joined Hakbal on her battlefield" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0

-- The board both Magical Hack timing cases start from. alice has a Mountain --
-- added FIRST, so it holds the lowest object id and identityAnswer's
-- ChooseTargets (Set.lookupMin over the recipients) aims the Hack at it -- plus
-- an Island for the Hack's {U}; bob has three Islands for Cancel's {1}{U}{U} and
-- a Cancel in hand. Returns the Mountain, alice's Magical Hack and bob's Cancel
-- alongside the state.
hackBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hackBoard mountain island magicalHack cancel =
  let (mountainId, g1) = S.addPermanent mountain S.alice (Setup.emptyGame S.bothPlayers)
      (_aliceIsland, g2) = S.addPermanent island S.alice g1
      (g3, hackId) = S.handOne magicalHack g2
      g4 = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) g3 [1 :: Int .. 3]
      (cancelId, g5) = S.addHandCard cancel S.bob g4
   in (mountainId, hackId, cancelId, g5)

-- Hacks Mountain -> Island, and takes the identity fallback elsewhere (the liar
-- pattern). Deliberately unlike identityAnswer's Mountain -> Mountain, so a
-- test can tell an honoured answer from the fallback.
hackToIsland :: Prompt.Prompt r -> r
hackToIsland p = case p of
  Prompt.ChooseLandTypeSwap {} -> (Subtype.Mountain, Subtype.Island)
  _ -> S.identityAnswer p

-- The basic-land-type answers in a transcript, in order.
basicLandTypeResponses :: [Response.Response] -> [Response.Response]
basicLandTypeResponses = filter isBasicLandTypesResponse

isBasicLandTypesResponse :: Response.Response -> Bool
isBasicLandTypesResponse response = case response of
  Response.ChoseLandTypeSwap _ -> True
  _ -> False

-- CR 608.2d: Magical Hack's "replacing all instances of one basic land type
-- with another" is a choice its EFFECT offers, not one CR 601.2b-d makes as the
-- spell is cast, so it is announced while the effect is applied. The two cases
-- below are what makes cast-time and resolution-time binding distinguishable.
magicalHackTimingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magicalHackTimingSpec s registry = Spec.describe s "MagicalHackTiming" $ do
  Spec.it s "CR 608.2d a countered Magical Hack is never asked for its basic land types" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    magicalHack <- S.printingOf s registry "Magical Hack"
    cancel <- S.printingOf s registry "Cancel"
    let (_mountainId, hackId, cancelId, gs) = hackBoard mountain island magicalHack cancel
        exchange = do
          S.cast S.alice hackId
          S.cast S.bob cancelId
          Stack.resolveTop
        ((_, after), transcript) = Replay.record S.identityAnswer gs exchange
    -- The control: the exchange really happened. CR 701.6a puts the countered
    -- spell into its owner's graveyard, and CR 608.2n puts Cancel into bob's.
    Spec.assertEqWith s "Magical Hack countered into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "Cancel resolved into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
    -- And the point: a spell that never resolves never offers its effect's
    -- choice. Bound at cast, this list would hold one response.
    Spec.assertEqWith s "no basic land types were ever asked for" (basicLandTypeResponses transcript) []
  Spec.it s "CR 608.2d an uncountered Magical Hack is asked at resolution, and the swap applies" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    magicalHack <- S.printingOf s registry "Magical Hack"
    cancel <- S.printingOf s registry "Cancel"
    let (mountainId, hackId, _cancelId, gs) = hackBoard mountain island magicalHack cancel
        ((_, cast), castTranscript) = Replay.record hackToIsland gs (S.cast S.alice hackId)
        ((_, resolved), resolveTranscript) = Replay.record hackToIsland cast Stack.resolveTop
    Spec.assertEqWith s "the cast asked nothing about land types" (basicLandTypeResponses castTranscript) []
    Spec.assertEqWith
      s
      "the resolution asked exactly once"
      (basicLandTypeResponses resolveTranscript)
      [Response.ChoseLandTypeSwap (Subtype.Mountain, Subtype.Island)]
    -- CR 612 / 305.6: the answer is honoured, so the choice did not go missing
    -- when it moved. Mountain -> Island, not identityAnswer's Mountain ->
    -- Mountain, is what tells the two apart.
    Spec.assertEqWith s "the hacked Mountain projects Island" (Projection.subtypesOf mountainId resolved) (Set.singleton Subtype.Island)
    -- M0 determinism: the prompt moved, so the recorded stream has to still
    -- feed a replay of the same run back to the same state.
    let ((_, replayed), desync) = Replay.replay resolveTranscript cast Stack.resolveTop
    Spec.assertEqWith s "the resolution replays deterministically" replayed resolved
    Spec.assertEqWith s "and the transcript answered every prompt" desync Nothing

-- Aims every target slot at `oid`, whichever recipient shape the offered set
-- holds, and answers a basic-land-type changer with `from -> to`. The offered
-- set is FILTERED rather than rebuilt, so CR 608.2b's re-read at resolution sees
-- the recipient the engine itself offered.
hackAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
hackAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (\r -> r == Recipient.ToObject oid || r == Recipient.ToCreature oid) candidates) sets
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- CR 611.2b's duration read through CR 612.1. alice casts Synthetic Conditional
-- Theft -- "Gain control of target creature for as long as you control a Swamp",
-- data/cards/synthetic-conditional-theft.json -- at bob's Goblin Piker, over
-- `islands` Islands and `swamps` Swamps; when `hack`, she also resolves a
-- Magical Hack at the Theft SPELL first, swapping Swamp -> Island. Returns the
-- Piker's id and the final state.
--
-- alice's own Blade Instructor is there so the Theft's target is a real choice
-- rather than the pool's only member; being hers, it also cannot be confused
-- with the Piker by a controller assertion.
--
-- The Theft is cast BEFORE the Hack, so the Hack resolves first and the Theft
-- resolves already rewritten -- pietyCharmChain's ordering.
theftChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> Int -> Bool -> m (ObjectId.ObjectId, GameState.GameState)
theftChain s registry islands swamps hack = do
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  piker <- S.printingOf s registry "Goblin Piker"
  bladeInstructor <- S.printingOf s registry "Blade Instructor"
  theft <- S.printingOf s registry "Synthetic Conditional Theft"
  magicalHack <- S.printingOf s registry "Magical Hack"
  let lands = S.landsFor swamp S.alice swamps (S.landsFor island S.alice islands (Setup.emptyGame S.bothPlayers))
      (pikerId, g1) = S.addPermanent piker S.bob lands
      (_instructorId, g2) = S.addPermanent bladeInstructor S.alice g1
      (theftId, g3) = S.addHandCard theft S.alice g2
      (hackId, g4) = S.addHandCard magicalHack S.alice g3
      onStack = S.runPure (hackAt pikerId Subtype.Swamp Subtype.Island) g4 (S.cast S.alice theftId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      hacked =
        if hack
          then S.runPure (hackAt spellId Subtype.Swamp Subtype.Island) onStack $ do
            S.cast S.alice hackId
            Stack.resolveTop
          else onStack
      after = S.runPure S.identityAnswer hacked Stack.resolveTop
  pure (pikerId, after)

-- SYNTHETIC. "Synthetic Conditional Theft" {1}{U} Sorcery: "Gain control of
-- target creature for as long as you control a Swamp." CR 611.2b's duration
-- naming a word CR 612.2 can swap, which no printing reaches: the Scryfall
-- sweep o:"for as long as", 2026-08-18, returns 223 cards, and in every one the
-- duration names its own source -- by card name (Dragonlord Silumgar, Merieke Ri
-- Berit), as "this creature"/"this Saga"/"this Equipment", or by a counter on a
-- land. Where a creature type or a land type does appear beside such a duration
-- it sits in the TARGET filter, which rewriteTargetSlot already rewrites --
-- Olivia Voldaren's "target Vampire", Hivis of the Scale's "target Dragon",
-- Seasinger's "whose controller controls an Island". CR 612.2's closing sentence
-- forbids a subtype swap changing a card name, so a duration naming its source
-- by name is unreachable by construction. Nothing in the CR forbids the card, so
-- it is legitimate and only unprinted; Olivia Voldaren gaining a Vampire-naming
-- DURATION is what would refute the claim.
--
-- CR 612.1 reaching a stored effect's DURATION, which is printed text like any
-- other. The first two cases are what makes the third discriminating: they pin
-- that the Theft works at all, and that its "for as long as" duration genuinely
-- gates on the printed word (CR 611.2b: "if the 'for as long as' duration never
-- starts, the effect does nothing").
magicalHackDurationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magicalHackDurationSpec s registry = Spec.describe s "MagicalHackDuration" $ do
  Spec.it s "CR 611.2b a satisfied duration starts, and the theft takes hold" $ do
    (pikerId, after) <- theftChain s registry 1 1 False
    Spec.assertEqWith s "alice controls the Piker" (Projection.controllerOf pikerId after) (Just S.alice)
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0
  Spec.it s "CR 611.2b a duration that never starts does nothing" $ do
    (pikerId, after) <- theftChain s registry 2 0 False
    Spec.assertEqWith s "bob keeps the Piker" (Projection.controllerOf pikerId after) (Just S.bob)
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0
  -- And the point: the same Swamp-less board, with the word the duration names
  -- swapped for one alice does control.
  Spec.it s "CR 612.1 a Magical Hack on the theft rewrites the duration's own word" $ do
    (pikerId, after) <- theftChain s registry 3 0 True
    Spec.assertEqWith s "alice controls the Piker" (Projection.controllerOf pikerId after) (Just S.alice)
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0

-- Aims every target slot at `oid` as an object (the SpellsAndPermanents pool's
-- recipient shape), and swaps `from` for `to` when the text-changer asks. Every
-- other prompt takes the identity fallback.
evolveAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
evolveAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseCreatureTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- Cast Turn to Frog at alice's Bog Wraith; optionally cast Artificial Evolution
-- at the Turn to Frog SPELL and resolve it, swapping the named creature type
-- words; then resolve the Turn to Frog. Returns the Wraith's id and the final
-- state.
turnToFrogChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, GameState.GameState)
turnToFrogChain s registry swap = do
  island <- S.printingOf s registry "Island"
  bogWraith <- S.printingOf s registry "Bog Wraith"
  turnToFrog <- S.printingOf s registry "Turn to Frog"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let (wraithId, g1) = S.addPermanent bogWraith S.alice (S.landsInPlay island 3)
      (turnToFrogId, g2) = S.addHandCard turnToFrog S.alice g1
      (evolutionId, g3) = S.addHandCard artificialEvolution S.alice g2
      onStack = S.runPure (aimAtCreature wraithId) g3 (S.cast S.alice turnToFrogId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
  pure (wraithId, S.runPure S.identityAnswer evolved Stack.resolveTop)

-- Records the words a swap prompt says the new one may not be, so a test can
-- assert what the player was actually offered. Targets go to `oid` (a
-- text-changer aimed at a permanent needs no second card on the stack), and the
-- swap itself is answered with an identity on a word neither family forbids.
recordingForbidden :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State (Set.Set Subtype.Subtype) r
recordingForbidden oid p = case p of
  Prompt.ChooseCreatureTypeSwap _ _ _ _ forbidden -> do
    State.modify' (Set.union forbidden)
    pure (Subtype.Elf, Subtype.Elf)
  Prompt.ChooseLandTypeSwap _ _ _ _ forbidden -> do
    State.modify' (Set.union forbidden)
    pure (Subtype.Mountain, Subtype.Mountain)
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToObject oid))) sets)
  _ -> pure (S.identityAnswer p)

-- Cast Dragon Fodder; optionally cast Artificial Evolution at the Dragon Fodder
-- SPELL and resolve it, swapping the named creature type words; then resolve the
-- Fodder. Returns the tokens it minted and the final state.
--
-- Two Mountains and two Islands: the Fodder is {1}{R} and the Evolution {U}, and
-- the generic half may be paid from either colour without stranding the
-- Evolution.
dragonFodderChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m ([ObjectId.ObjectId], GameState.GameState)
dragonFodderChain s registry swap = do
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  dragonFodder <- S.printingOf s registry "Dragon Fodder"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let g1 = snd (S.addPermanent island S.alice (snd (S.addPermanent island S.alice (S.landsInPlay mountain 2))))
      (fodderId, g2) = S.addHandCard dragonFodder S.alice g1
      (evolutionId, g3) = S.addHandCard artificialEvolution S.alice g2
      onStack = S.runPure S.identityAnswer g3 (S.cast S.alice fodderId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      after = S.runPure S.identityAnswer evolved Stack.resolveTop
  pure (S.tokensOf after, after)

-- The same shape as dragonFodderChain for a synthetic token-minting sorcery
-- named by `cardName`: cast it, optionally resolve an Artificial Evolution at it
-- on the stack, then resolve it. Returns the tokens and the final state.
--
-- Three Forests and three Islands, so neither the {1}{G} Rite nor the {1}{U}
-- Summons can strand the Evolution's {U}.
syntheticTokenChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m ([ObjectId.ObjectId], GameState.GameState)
syntheticTokenChain s registry cardName swap = do
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  minter <- S.printingOf s registry cardName
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let g1 = S.landsFor island S.alice 3 (S.landsInPlay forest 3)
      (minterId, g2) = S.addHandCard minter S.alice g1
      (evolutionId, g3) = S.addHandCard artificialEvolution S.alice g2
      onStack = S.runPure S.identityAnswer g3 (S.cast S.alice minterId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      after = S.runPure S.identityAnswer evolved Stack.resolveTop
  pure (S.tokensOf after, after)

-- The permanent half of the same rule: alice controls Bitterblossom, optionally
-- has an Artificial Evolution resolved at IT (a permanent, not a spell), and then
-- her upkeep begins so the printed trigger fires and resolves. Returns the tokens
-- and the final state.
bitterblossomChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m ([ObjectId.ObjectId], GameState.GameState)
bitterblossomChain s registry swap = do
  island <- S.printingOf s registry "Island"
  bitterblossom <- S.printingOf s registry "Bitterblossom"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let (blossomId, g1) = S.addPermanent bitterblossom S.alice (S.landsInPlay island 1)
      (evolutionId, g2) = S.addHandCard artificialEvolution S.alice g1
      evolved = case swap of
        Nothing -> g2
        Just (from, to) ->
          S.runPure (evolveAt blossomId from to) g2 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (evolved {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      onStack = S.runPure S.identityAnswer begun Engine.settleForPriority
      after = S.runPure S.identityAnswer onStack Engine.priorityLoop
  pure (S.tokensOf after, after)

-- alice's Ajani, Adversary of Tyrants at seven loyalty, optionally with an
-- Artificial Evolution resolved AT IT first; then she activates the ultimate,
-- the emblem arrives in the command zone, and her end step begins so the
-- emblem's own trigger fires and resolves. Returns the tokens and the state.
--
-- The loyalty is a fixture rather than seven turns of +1, for
-- Pawl.TriggerSpec's reason: CR 306.5b's counters are what the cost pays, and
-- how they got there is no part of what this asks.
--
-- The ability is taken from Projection.abilitiesOf and NOT from the printed
-- face, which is the whole mechanism under test: the layer-3 swap is what puts
-- the rewritten CreateEmblem in front of the activation.
ajaniEmblemChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m ([ObjectId.ObjectId], GameState.GameState)
ajaniEmblemChain s registry swap = do
  island <- S.printingOf s registry "Island"
  ajani <- S.printingOf s registry "Ajani, Adversary of Tyrants"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let (ajaniId, g1) = S.addPermanent ajani S.alice (S.landsInPlay island 1)
      armed = S.addCounter CounterKind.Loyalty 7 ajaniId g1
      (evolutionId, g2) = S.addHandCard artificialEvolution S.alice armed
      evolved = case swap of
        Nothing -> g2
        Just (from, to) ->
          S.runPure (evolveAt ajaniId from to) g2 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      used = case drop 2 (Projection.abilitiesOf ajaniId evolved) of
        ability : _ -> S.runPure S.identityAnswer evolved (do Activate.activateAbility S.alice ajaniId ability; Stack.resolveTop)
        [] -> evolved
      endStep = Phase.Ending EndingStep.EndStep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice))
          (used {GameState.phase = endStep, GameState.activePlayer = S.alice})
      onStack = S.runPure S.identityAnswer begun Engine.settleForPriority
      after = S.runPure S.identityAnswer onStack Engine.priorityLoop
  pure (S.tokensOf after, after)

-- alice's Blade Instructor (3/1 Human Soldier) and Goblin Piker (2/1 Goblin
-- Warrior); she casts Piety Charm's SECOND mode at the Instructor, optionally
-- has an Artificial Evolution resolved at the CHARM ON THE STACK, and then the
-- charm resolves. Returns the Instructor, the Piker and the final state.
--
-- A Plains and an Island, one each: the charm is {W} and the Evolution {U}, so
-- neither payment can strand the other.
--
-- The target set is FILTERED rather than rebuilt, so the recipient shape the
-- engine offered is the one CR 608.2b re-reads.
pietyCharmChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
pietyCharmChain s registry swap = do
  plains <- S.printingOf s registry "Plains"
  island <- S.printingOf s registry "Island"
  bladeInstructor <- S.printingOf s registry "Blade Instructor"
  goblinPiker <- S.printingOf s registry "Goblin Piker"
  pietyCharm <- S.printingOf s registry "Piety Charm"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let lands = S.landsFor island S.alice 1 (S.landsInPlay plains 1)
      (soldierId, g1) = S.addPermanent bladeInstructor S.alice lands
      (gobId, g2) = S.addPermanent goblinPiker S.alice g1
      (charmId, g3) = S.addHandCard pietyCharm S.alice g2
      (evolutionId, g4) = S.addHandCard artificialEvolution S.alice g3
      onStack = S.runPure (charmAt soldierId) g4 (S.cast S.alice charmId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      after = S.runPure S.identityAnswer evolved Stack.resolveTop
  pure (soldierId, gobId, after)

-- Piety Charm's two asks: its CR 700.2a mode -- the second, "target Soldier
-- creature gets +2/+2" -- and that mode's one target.
charmAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
charmAt oid p = case p of
  Prompt.ChooseModes {} -> Seq.singleton (ModeIndex.MkModeIndex 1)
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (\r -> r == Recipient.ToCreature oid || r == Recipient.ToObject oid) candidates) sets
  _ -> S.identityAnswer p

-- CR 612.2a's third carrier, and the one whose word is the RULEBOOK's: alice
-- controls a Ministrant of Obligation ({2}{W} Creature -- Human Cleric 2/1 whose
-- whole text box is "Afterlife 2", checked against Scryfall), optionally has an
-- Artificial Evolution resolved at it, and then Murder kills it so the afterlife
-- trigger fires and resolves. Returns the Ministrant's id, the state in which it
-- was still alive, the tokens and the final state.
--
-- Three Swamps and an Island: the Murder is {1}{B}{B} and the Evolution {U}, and
-- the generic half may be paid from either without stranding the Evolution.
--
-- The MIDDLE state is returned beside the last one, with the Ministrant's id: it
-- is the only place the Evolution's effect on the Ministrant itself can be read,
-- since CR 400.7 has spent that id by the time the tokens exist. A negative case
-- needs it to tell "the swap missed the token" from "the swap never resolved".
ministrantChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, GameState.GameState, [ObjectId.ObjectId], GameState.GameState)
ministrantChain s registry swap = do
  swamp <- S.printingOf s registry "Swamp"
  island <- S.printingOf s registry "Island"
  ministrant <- S.printingOf s registry "Ministrant of Obligation"
  murder <- S.printingOf s registry "Murder"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let g1 = snd (S.addPermanent island S.alice (S.landsInPlay swamp 3))
      (ministrantId, g2) = S.addPermanent ministrant S.alice g1
      (evolutionId, g3) = S.addHandCard artificialEvolution S.alice g2
      (murderId, g4) = S.addHandCard murder S.alice g3
      evolved = case swap of
        Nothing -> g4
        Just (from, to) ->
          S.runPure (evolveAt ministrantId from to) g4 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      -- The Murder's target is named rather than left to the fallback, since its
      -- Pool.Creatures slot wants a Recipient.ToCreature where evolveAt above
      -- answers with the Evolution's Recipient.ToObject.
      killed = S.runPure (aimAtCreature ministrantId) evolved $ do
        S.cast S.alice murderId
        Stack.resolveTop
      -- CR 603.3: the dies trigger goes on the stack the next time a player would
      -- receive priority, and resolving it is what mints the tokens.
      settled = S.runPure S.identityAnswer killed Engine.settleForPriority
      after = S.runPure S.identityAnswer settled Stack.resolveTop
  pure (ministrantId, evolved, S.tokensOf after, after)

-- CR 612.3's half of the same mint: alice controls `host`, optionally resolves an
-- Artificial Evolution at it, then resolves an Afterlife Insurance ({1}{W/B}
-- Instant -- "Creatures you control gain afterlife 1 until end of turn. Draw a
-- card.", checked against Scryfall) and Murders the host. Returns the host's id,
-- the state while it lived, the tokens and the final state.
--
-- The Insurance is cast AFTER the Evolution, but the order is not what settles
-- this: CR 613.1c/613.1f put the text change in layer 3 and the grant in layer 6
-- whichever timestamp is older, so the granted afterlife always arrives after the
-- swap has run.
--
-- Six Swamps and an Island, which is one Swamp more than the chain spends: the
-- Evolution's {U} is the Island's, and {1}{W/B} plus {1}{B}{B} is five.
insuredChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, GameState.GameState, [ObjectId.ObjectId], GameState.GameState)
insuredChain s registry hostName swap = do
  swamp <- S.printingOf s registry "Swamp"
  island <- S.printingOf s registry "Island"
  host <- S.printingOf s registry hostName
  murder <- S.printingOf s registry "Murder"
  insurance <- S.printingOf s registry "Afterlife Insurance"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let g1 = snd (S.addPermanent island S.alice (S.landsInPlay swamp 6))
      (hostId, g2) = S.addPermanent host S.alice g1
      (evolutionId, g3) = S.addHandCard artificialEvolution S.alice g2
      (insuranceId, g4) = S.addHandCard insurance S.alice g3
      (murderId, g5) = S.addHandCard murder S.alice g4
      -- The Insurance draws, so alice needs a library: CR 104.3c would otherwise
      -- lose her the game the next time a player would receive priority, which is
      -- before the dies trigger goes on the stack.
      g6 = snd (S.addLibraryCard swamp S.alice g5)
      evolved = case swap of
        Nothing -> g6
        Just (from, to) ->
          S.runPure (evolveAt hostId from to) g6 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      -- The identity answerer suffices for the Insurance: its EachMatching ref
      -- targets nothing, so there is no slot for a hand-built recipient to miss.
      insured =
        S.runPure S.identityAnswer evolved $ do
          S.cast S.alice insuranceId
          Stack.resolveTop
      -- Named for ministrantChain's reason: Murder's Pool.Creatures slot wants a
      -- Recipient.ToCreature where evolveAt answers with a Recipient.ToObject.
      killed =
        S.runPure (aimAtCreature hostId) insured $
          do
            S.cast S.alice murderId
            Stack.resolveTop
      settled = S.runPure S.identityAnswer killed Engine.settleForPriority
      -- CR 702.135b: a host holding a printed instance AND a granted one puts two
      -- triggers on the stack, so resolving the top once is not enough.
      drain gs = if null (GameState.stack gs) then gs else S.runPure S.identityAnswer gs Stack.resolveTop
      after = List.foldl' (\gs _ -> drain gs) settled [1 :: Int .. 4]
  pure (hostId, insured, S.tokensOf after, after)

-- The subtype lists of `tokens`, sorted, so one assertion carries both the
-- multiset and the count -- an empty token list cannot satisfy it the way a
-- mapM_ over the same assertion would.
tokenSubtypes :: [ObjectId.ObjectId] -> GameState.GameState -> [[Subtype.Subtype]]
tokenSubtypes tokens gs = List.sort (fmap (\oid -> Set.toList (Projection.subtypesOf oid gs)) tokens)

-- The same for CR 111.4's names, which rewriteTokenName reaches by a different
-- path than the subtype above.
tokenNames :: [ObjectId.ObjectId] -> GameState.GameState -> [[CardName.CardName]]
tokenNames tokens gs = List.sort (fmap (\oid -> Set.toList (Projection.namesOf oid gs)) tokens)

-- CR 612.2 reaching the ObjectRef INSIDE an effect: alice controls Agent Phil
-- Coulson ({1}{W} Legendary Creature -- Human Spy Hero 2/2, "Vigilance / {T}:
-- Put a +1/+1 counter on each other Hero you control", checked against
-- Scryfall), a Spider-Punk (Legendary Creature -- Spider Human Hero) and a
-- Goblin Piker (Creature -- Goblin Warrior); optionally an Artificial Evolution
-- is resolved at the Coulson, and then she activates his ability. Returns the
-- Spider-Punk, the Piker and the final state.
--
-- The two other creatures are the referents of the printed word and of the new
-- one, so the two readings of the rule disagree in opposite directions on the
-- same board. Spider-Punk's riot never fires: S.addPermanent writes the object
-- straight onto the battlefield with no counters and no entry, so both its
-- +1/+1 counts start at zero.
--
-- The ability is taken from Projection.abilitiesOf and NOT from the printed
-- face, which is the mechanism under test -- ajaniEmblemChain's route.
coulsonChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
coulsonChain s registry swap = do
  island <- S.printingOf s registry "Island"
  coulson <- S.printingOf s registry "Agent Phil Coulson"
  spiderPunk <- S.printingOf s registry "Spider-Punk"
  goblinPiker <- S.printingOf s registry "Goblin Piker"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let (coulsonId, g1) = S.addPermanent coulson S.alice (S.landsInPlay island 1)
      (punkId, g2) = S.addPermanent spiderPunk S.alice g1
      (pikerId, g3) = S.addPermanent goblinPiker S.alice g2
      (evolutionId, g4) = S.addHandCard artificialEvolution S.alice g3
      evolved = case swap of
        Nothing -> g4
        Just (from, to) ->
          S.runPure (evolveAt coulsonId from to) g4 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      used = case Projection.abilitiesOf coulsonId evolved of
        ability : _ -> S.runPure S.identityAnswer evolved (do Activate.activateAbility S.alice coulsonId ability; Stack.resolveTop)
        [] -> evolved
  pure (punkId, pikerId, used)

-- alice controls two Goblin Pikers and four Glistener Elves; she casts Goblin
-- War Strike at bob, optionally has an Artificial Evolution resolved at the
-- STRIKE ON THE STACK first, and then the Strike resolves. Returns the final
-- state.
--
-- The two counts are deliberately unequal, and neither equals the number of
-- creatures on the board: a Strike that counted everything, or that counted
-- nothing, is a different life total from either reading.
goblinWarStrikeChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m GameState.GameState
goblinWarStrikeChain s registry swap = do
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  goblinPiker <- S.printingOf s registry "Goblin Piker"
  glistenerElf <- S.printingOf s registry "Glistener Elf"
  goblinWarStrike <- S.printingOf s registry "Goblin War Strike"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let board =
        List.foldl'
          (\gs printing -> snd (S.addPermanent printing S.alice gs))
          (S.landsFor island S.alice 1 (S.landsInPlay mountain 1))
          [goblinPiker, goblinPiker, glistenerElf, glistenerElf, glistenerElf, glistenerElf]
      (strikeId, g1) = S.addHandCard goblinWarStrike S.alice board
      (evolutionId, g2) = S.addHandCard artificialEvolution S.alice g1
      onStack = S.runPure aimAtBob g2 (S.cast S.alice strikeId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
  pure (S.runPure S.identityAnswer evolved Stack.resolveTop)

-- CR 612.1 into the SHIELD a resolving spell installs, which is the CR 614.3
-- floating carrier rather than the permanent's static row
-- Projection.rewritePrintedReplacement already descends: Moonmist {1}{G}
-- Instant -- "Transform all Humans. Prevent all combat
-- damage that would be dealt this turn by creatures other than Werewolves and
-- Wolves." (checked against Scryfall). alice casts it, optionally has an
-- Artificial Evolution resolved at the MOONMIST ON THE STACK first, and then the
-- Moonmist resolves; a hand-built combat batch from bob's three creatures is
-- settled against the shield it left behind. Returns the Wolf, the Werewolf, the
-- Piker, the shielded state and the state after the damage.
--
-- Three sources whose readings all differ, Pawl.ReplacementSpec's moonmistSpec
-- reason: Russet Wolves (Creature -- Wolf), Tovolar, Dire Overlord (Creature --
-- Human Werewolf) and Goblin Piker (Creature -- Goblin Warrior). Swapping
-- Werewolf for Goblin moves the Piker out of the exclusion and the Werewolf into
-- it, so the two readings of the rule disagree in opposite directions on one
-- board and neither an all-prevented nor an all-dealt board can pass either.
--
-- Moonmist's OWN first sentence names Human, which the swap does not touch, and
-- CR 702.145b's daybound refuses Tovolar the transform anyway
-- (Pawl.DaytimeSpec's restrictionSpec is the proof) -- and both of his faces are
-- Werewolves, so nothing here rests on which way that goes.
--
-- The DAMAGE BATCH is hand-built and the SPELL is not, moonmistSpec's reason.
-- Five lands so the {1} of the Moonmist cannot strand the {U} of the Evolution.
moonmistShieldChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
moonmistShieldChain s registry swap = do
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  wolfPrinting <- S.printingOf s registry "Russet Wolves"
  werewolfPrinting <- S.printingOf s registry "Tovolar, Dire Overlord"
  pikerPrinting <- S.printingOf s registry "Goblin Piker"
  moonmist <- S.printingOf s registry "Moonmist"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let base = S.landsFor island S.alice 2 (S.landsInPlay forest 3)
      (wolf, g1) = S.addPermanent wolfPrinting S.bob base
      (werewolf, g2) = S.addPermanent werewolfPrinting S.bob g1
      (piker, g3) = S.addPermanent pikerPrinting S.bob g2
      (moonmistId, g4) = S.addHandCard moonmist S.alice g3
      (evolutionId, g5) = S.addHandCard artificialEvolution S.alice g4
      onStack = S.runPure S.identityAnswer g5 (S.cast S.alice moonmistId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      evolved = case swap of
        Nothing -> onStack
        Just (from, to) ->
          S.runPure (evolveAt spellId from to) onStack $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      shielded = S.runPure S.identityAnswer evolved Stack.resolveTop
      hit src n = DamageEvent.MkDamageEvent src (Recipient.ToPlayer S.alice) n False False False 0 Nothing DamageKind.Combat
      after = S.runPure S.identityAnswer shielded (Damage.applyDamage [hit wolf 3, hit werewolf 3, hit piker 2])
  pure (wolf, werewolf, piker, shielded, after)

-- CR 612.1 through a quoted ability handed over by a RESOLUTION rather than by
-- a static ability.
-- Presence of Gond (Pawl.ActivateSpec) is the static half; this is the half that
-- goes through Projection.rewriteEffect's Effect.ModifyTarget arm.
--
-- Clavileño, First of the Blessed {1}{W}{B} Legendary Creature -- Vampire Cleric
-- 2/2, "Whenever you attack, target attacking Vampire that isn't a Demon becomes
-- a Demon in addition to its other types. It gains 'When this creature dies,
-- draw a card and create a tapped 4/3 white and black Vampire Demon creature
-- token with flying.'" (checked against Scryfall). The word Demon rides two
-- positions of that one ability -- a layer-4 Modification.AddCreatureSubtype,
-- and the token defined INSIDE the quoted ability -- and only the token answers
-- for the descent, the subtype add being a sibling arm of rewriteModification
-- that the same walk reaches without ever entering the quoted ability.
--
-- alice attacks with the Clavileño and a Bloodrage Vampire ({2}{B} Creature --
-- Vampire 3/1, checked against Scryfall); the attack trigger targets the
-- Vampire, and a Murder then kills it so the granted dies trigger mints the
-- token. Returns the Clavileño's id, the victim's, the state in which the victim
-- was still alive, the tokens and the final state.
--
-- BOTH attackers match "attacking Vampire that isn't a Demon", so the trigger's
-- target is a real choice rather than one the board forces.
--
-- An Island for the Evolution's {U} and three Swamps for the Murder's {1}{B}{B},
-- plus a library card, since the granted trigger draws and CR 104.3c would
-- otherwise take the game from alice before the token existed.
clavilenoChain :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe (Subtype.Subtype, Subtype.Subtype) -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, [ObjectId.ObjectId], GameState.GameState)
clavilenoChain s registry swap = do
  swamp <- S.printingOf s registry "Swamp"
  island <- S.printingOf s registry "Island"
  clavileno <- S.printingOf s registry "Clavileño, First of the Blessed"
  bloodrageVampire <- S.printingOf s registry "Bloodrage Vampire"
  murder <- S.printingOf s registry "Murder"
  artificialEvolution <- S.printingOf s registry "Artificial Evolution"
  let (board, mine, _) = S.combatBoardOf [clavileno, bloodrageVampire] []
      (clavilenoId, victimId) = case mine of
        [a, b] -> (a, b)
        _ -> (ObjectId.MkObjectId 998, ObjectId.MkObjectId 999)
      withLands = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) board [island, swamp, swamp, swamp]
      (evolutionId, g1) = S.addHandCard artificialEvolution S.alice withLands
      (murderId, g2) = S.addHandCard murder S.alice g1
      g3 = snd (S.addLibraryCard swamp S.alice g2)
      evolved = case swap of
        Nothing -> g3
        Just (from, to) ->
          S.runPure (evolveAt clavilenoId from to) g3 $ do
            S.cast S.alice evolutionId
            Stack.resolveTop
      attacked = S.runPure S.aggressiveAnswer evolved (Combat.declareAttackers S.manaPerformer S.alice)
      -- CR 603.3: the attack trigger goes on the stack the next time a player
      -- would receive priority, and CR 603.3d chooses its target THERE rather
      -- than at resolution -- so the answerer that pins the victim rides this
      -- step and not the resolution below.
      triggered = S.runPure (aimAtCreature victimId) attacked Engine.settleForPriority
      granted = S.runPure S.identityAnswer triggered Stack.resolveTop
      killed =
        S.runPure (aimAtCreature victimId) granted $ do
          S.cast S.alice murderId
          Stack.resolveTop
      settled = S.runPure S.identityAnswer killed Engine.settleForPriority
      after = S.runPure S.identityAnswer settled Stack.resolveTop
  pure (clavilenoId, victimId, granted, S.tokensOf after, after)

-- Narrows every target slot to bob, by FILTERING what the engine offered rather
-- than building a recipient of its own -- a hand-built ToPlayer would be dropped
-- by CR 608.2b's re-read at resolution with no error. The Players pool offers
-- both seats, so this is a real choice.
aimAtBob :: Prompt.Prompt r -> r
aimAtBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) sets
  _ -> S.identityAnswer p

-- Aims every target slot at `oid` as a creature (Turn to Frog's Pool.Creatures
-- recipient shape); the board holds more than one creature, so the choice has to
-- be answered rather than forced by construction.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- CR 612.2's OTHER half, end to end through the real engine: "a creature type
-- word used as a creature type".
--
-- Artificial Evolution {U} Instant -- "Change the text of target spell or
-- permanent by replacing all instances of one creature type with another. The
-- new creature type can't be Wall." (checked against Scryfall) -- is the card
-- that makes the difference observable, and Turn to Frog {1}{U} ("target
-- creature ... becomes a blue Frog with base power and toughness 1/1") is the
-- spell it rewrites: its SetCreatureSubtype names the Frog, so an Evolution
-- resolved at the spell on the stack has to make the target something else.
artificialEvolutionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
artificialEvolutionSpec s registry = Spec.describe s "ArtificialEvolution" $ do
  -- The control: with no Evolution the printed word stands, so this cannot pass
  -- vacuously on a chain that never got as far as resolving the Frog.
  Spec.it s "CR 205.1b whole card: an unevolved Turn to Frog still makes a Frog" $ do
    (wraithId, after) <- turnToFrogChain s registry Nothing
    Spec.assertEqWith s "Creature -- Frog" (Projection.subtypesOf wraithId after) (Set.singleton Subtype.Frog)

  -- And the point: the Evolution's word swap reaches the resolving spell's
  -- SetCreatureSubtype, so the Wraith becomes an Elf and never a Frog.
  Spec.it s "CR 612.2 whole card: Artificial Evolution on the Turn to Frog spell makes an Elf instead" $ do
    (wraithId, after) <- turnToFrogChain s registry (Just (Subtype.Frog, Subtype.Elf))
    Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf wraithId after) (Set.singleton Subtype.Elf)

  -- "The new creature type can't be Wall" is printed card text, so it travels
  -- with the card: the data says it, Effect.ChangeText carries it, and the
  -- prompt offers it. Nothing in the engine knows which card is asking.
  Spec.it s "CR 612 the Evolution's own restriction reaches the player being asked" $ do
    island <- S.printingOf s registry "Island"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    artificialEvolution <- S.printingOf s registry "Artificial Evolution"
    magicalHack <- S.printingOf s registry "Magical Hack"
    let (wraithId, g1) = S.addPermanent bogWraith S.alice (S.landsInPlay island 3)
        forbiddenBy printing =
          let (spellId, g2) = S.addHandCard printing S.alice g1
              cast = do
                S.cast S.alice spellId
                Stack.resolveTop
           in State.execState (Engine.runGame (recordingForbidden wraithId) g2 cast) Set.empty
    Spec.assertEqWith s "the Evolution forbids Wall, and nothing else" (forbiddenBy artificialEvolution) (Set.singleton Subtype.Wall)
    -- The falsifier for "the engine hard-codes Wall somewhere": Magical Hack
    -- prints no restriction, so its swap forbids nothing.
    Spec.assertEqWith s "and the Hack forbids nothing" (forbiddenBy magicalHack) Set.empty

  -- CR 612.1's "any words or symbols printed on that object" reaches a
  -- text-changer's own restriction clause: Wall in "The new creature type can't
  -- be Wall" is a creature type word used as a creature type. Wizards' own
  -- Artificial Evolution ruling puts it plainly -- the swap "alters all
  -- occurrences of the chosen word in the text box and the type line of the
  -- given card" -- so one Evolution aimed at another leaves a spell whose
  -- restriction names the new word.
  Spec.it s "CR 612.1 an Evolution on an Evolution rewrites the restriction itself" $ do
    island <- S.printingOf s registry "Island"
    bogWraith <- S.printingOf s registry "Bog Wraith"
    artificialEvolution <- S.printingOf s registry "Artificial Evolution"
    let (wraithId, g1) = S.addPermanent bogWraith S.alice (S.landsInPlay island 3)
        (firstId, g2) = S.addHandCard artificialEvolution S.alice g1
        (secondId, g3) = S.addHandCard artificialEvolution S.alice g2
        onStack = S.runPure S.identityAnswer g3 (S.cast S.alice secondId)
        spellId = case GameState.stack onStack of
          top : _ -> top
          [] -> ObjectId.MkObjectId 999
        -- The first Evolution replaces the second's every Wall with Frog.
        evolved = S.runPure (evolveAt spellId Subtype.Wall Subtype.Frog) onStack $ do
          S.cast S.alice firstId
          Stack.resolveTop
        forbidden = State.execState (Engine.runGame (recordingForbidden wraithId) evolved Stack.resolveTop) Set.empty
    Spec.assertEqWith s "the evolved Evolution forbids Frog, not Wall" forbidden (Set.singleton Subtype.Frog)

  -- CR 612.2a, the SPELL half: "most spells and abilities that create creature
  -- tokens use creature types to define both the creature types and the names of
  -- the tokens. A text-changing effect that affects such a spell ... can change
  -- these words because they're being used as creature types, even though
  -- they're also being used as names." Dragon Fodder {1}{R} ("Create two 1/1 red
  -- Goblin creature tokens") is the spell; the Evolution is resolved at it on the
  -- stack.
  --
  -- The control first, so the pair cannot pass vacuously on a chain that never
  -- minted anything.
  Spec.it s "CR 111.4 an unevolved Dragon Fodder still mints two Goblins named Goblin Token" $ do
    (tokens, after) <- dragonFodderChain s registry Nothing
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Goblin" (Projection.subtypesOf oid after) (Set.singleton Subtype.Goblin)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Goblin Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Goblin Token")))) tokens

  -- And the point. BOTH halves of CR 612.2a: the type line, and the name those
  -- same words define.
  Spec.it s "CR 612.2a whole card: an evolved Dragon Fodder mints Elves, name and all" $ do
    (tokens, after) <- dragonFodderChain s registry (Just (Subtype.Goblin, Subtype.Elf))
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf oid after) (Set.singleton Subtype.Elf)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Elf Token")))) tokens

  -- CR 612.2a's OTHER half: "or an object with such an ability". Bitterblossom
  -- {1}{B} Kindred Enchantment -- Faerie ("At the beginning of your upkeep, you
  -- lose 1 life and create a 1/1 black Faerie Rogue creature token with flying",
  -- checked against Scryfall) is a PERMANENT whose triggered ability defines a
  -- token by creature type, so the Evolution reaches it through the printed
  -- ability rather than through a spell on the stack. Only the word the swap
  -- names moves: Rogue is untouched, in the type line and in the name alike.
  Spec.it s "CR 111.4 an unevolved Bitterblossom still mints a Faerie Rogue Token" $ do
    (tokens, after) <- bitterblossomChain s registry Nothing
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Faerie Rogue" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Faerie, Subtype.Rogue])) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Faerie Rogue Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Faerie Rogue Token")))) tokens

  Spec.it s "CR 612.2a whole card: an evolved Bitterblossom's trigger mints an Elf Rogue Token" $ do
    (tokens, after) <- bitterblossomChain s registry (Just (Subtype.Faerie, Subtype.Elf))
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf Rogue" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Elf, Subtype.Rogue])) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Rogue Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Elf Rogue Token")))) tokens

  -- The SECOND word of the same CR 111.4 name, so a rewrite that only reached the
  -- first would show here. Rogue -> Assassin, both creature types, both whole
  -- words of "Faerie Rogue Token".
  Spec.it s "CR 612.2a an evolved Bitterblossom's second name word moves too" $ do
    (tokens, after) <- bitterblossomChain s registry (Just (Subtype.Rogue, Subtype.Assassin))
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "named Faerie Assassin Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Faerie Assassin Token")))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Faerie Assassin" (Projection.subtypesOf oid after) (Set.fromList [Subtype.Faerie, Subtype.Assassin])) tokens

  -- CR 612.2's limit on the same rule: a text-changing effect changes only words
  -- "used in the correct way". Synthetic Ursine Rite ({1}{G} Sorcery, "Create a
  -- 2/2 green Bear creature token named Bearer of the Wilds") names its token
  -- itself, so "Bear" sits inside "Bearer", where it is not being used as a
  -- creature type. Synthetic because no printing meets rewriteFace's joint
  -- condition -- a card-authored token name holding one of that token's own
  -- subtypes inside a longer word (#644).
  --
  -- The control first, so the pair cannot pass on a chain that minted nothing.
  Spec.it s "CR 111.4 an unevolved Ursine Rite mints a Bear named Bearer of the Wilds" $ do
    (tokens, after) <- syntheticTokenChain s registry "Synthetic Ursine Rite" Nothing
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "named Bearer of the Wilds" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Bearer of the Wilds")))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Bear" (Projection.subtypesOf oid after) (Set.singleton Subtype.Bear)) tokens

  Spec.it s "CR 612.2 an evolved Ursine Rite's token turns Elf but keeps its name" $ do
    (tokens, after) <- syntheticTokenChain s registry "Synthetic Ursine Rite" (Just (Subtype.Bear, Subtype.Elf))
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "still named Bearer of the Wilds" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Bearer of the Wilds")))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf oid after) (Set.singleton Subtype.Elf)) tokens

  -- CR 205.3m's one two-word creature type, which is why the boundary test cannot
  -- be a whitespace tokenizer: Synthetic Temporal Summons ({1}{U} Sorcery, "Create
  -- a 2/2 blue Time Lord creature token") mints CR 111.4's "Time Lord Token", and
  -- the swap has to match both words as one. Synthetic because no printing creates
  -- a Time Lord token at all (Scryfall is:token t:"time lord", 2026-09-06, no hit).
  Spec.it s "CR 111.4 an unevolved Temporal Summons mints a Time Lord Token" $ do
    (tokens, after) <- syntheticTokenChain s registry "Synthetic Temporal Summons" Nothing
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "named Time Lord Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Time Lord Token")))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Time Lord" (Projection.subtypesOf oid after) (Set.singleton Subtype.TimeLord)) tokens

  Spec.it s "CR 612.2a an evolved Temporal Summons replaces both words at once" $ do
    (tokens, after) <- syntheticTokenChain s registry "Synthetic Temporal Summons" (Just (Subtype.TimeLord, Subtype.Elf))
    Spec.assertEqWith s "one token" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Elf Token")))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf oid after) (Set.singleton Subtype.Elf)) tokens

  -- CR 612.2a's third carrier: an ability rule 702 MINTS. "Afterlife 2" is all
  -- Ministrant of Obligation prints; CR 702.135a is where the word Spirit is
  -- written ("'Afterlife N' means 'When this permanent is put into a graveyard
  -- from the battlefield, create N 1/1 white and black Spirit creature tokens
  -- with flying'"), so the ability the Evolution rewrites does not exist until the
  -- mint runs -- after the CR 613 fold. What the layer fold leaves behind is the
  -- pair, and Projection.mintedTriggeredAbilitiesOf applies it at the mint.
  --
  -- The control first, so the pair below cannot pass on a chain that killed
  -- nothing.
  Spec.it s "CR 702.135a an unevolved Ministrant of Obligation leaves two Spirit Tokens" $ do
    (ministrantId, alive, tokens, after) <- ministrantChain s registry Nothing
    Spec.assertEqWith s "Human Cleric while it lived" (Projection.subtypesOf ministrantId alive) (Set.fromList [Subtype.Human, Subtype.Cleric])
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Spirit" (Projection.subtypesOf oid after) (Set.singleton Subtype.Spirit)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Spirit Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Spirit Token")))) tokens

  -- And the point. The Ministrant is a Human Cleric 2/1 and its tokens are 1/1
  -- Spirits, so no assertion here can be satisfied by the parent's own type line;
  -- Elf is on neither.
  Spec.it s "CR 612.2a whole card: an evolved Ministrant of Obligation leaves Elves" $ do
    (ministrantId, alive, tokens, after) <- ministrantChain s registry (Just (Subtype.Spirit, Subtype.Elf))
    -- The Ministrant prints no Spirit, so its own type line is untouched: what
    -- the Evolution reached is rule 702.135a's word alone.
    Spec.assertEqWith s "Human Cleric still, while it lived" (Projection.subtypesOf ministrantId alive) (Set.fromList [Subtype.Human, Subtype.Cleric])
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Elf" (Projection.subtypesOf oid after) (Set.singleton Subtype.Elf)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Elf Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Elf Token")))) tokens
    -- Everything else rule 702.135a states is untouched, so what moved is the one
    -- word and not the mint.
    mapM_ (\oid -> Spec.assertEqWith s "still 1/1" (Projection.powerOf oid after, Projection.toughnessOf oid after) (Just (1 :: Integer), Just (1 :: Integer))) tokens
    mapM_ (\oid -> Spec.assertBool s (Projection.hasKeyword Keyword.Flying oid after) "and still flying") tokens

  -- CR 612.3's boundary on the same mint: "any abilities that are granted to an
  -- object can't be modified by text-changing effects that affect that object".
  -- Afterlife Insurance grants afterlife 1 at CR 613.1f layer 6, after the
  -- Evolution's layer-3 swap, so rule 702.135a's Spirit survives.
  --
  -- The control first, so the pair below cannot pass on a chain that granted
  -- nothing. A Goblin Piker prints neither afterlife nor Spirit, so every token
  -- here comes from the grant.
  Spec.it s "CR 702.135a a Goblin Piker insured but unevolved leaves a Spirit Token" $ do
    (pikerId, alive, tokens, after) <- insuredChain s registry "Goblin Piker" Nothing
    Spec.assertEqWith s "one Creature -- Spirit" (tokenSubtypes tokens after) [[Subtype.Spirit]]
    Spec.assertEqWith s "named Spirit Token" (tokenNames tokens after) [[CardName.MkCardName (Text.pack "Spirit Token")]]
    Spec.assertBool s (Projection.hasKeyword (Keyword.Afterlife 1) pikerId alive) "the grant reached the Piker"
    Spec.assertEqWith s "Goblin Warrior while it lived" (Projection.subtypesOf pikerId alive) (Set.fromList [Subtype.Goblin, Subtype.Warrior])

  -- And the point. The swap reaches the Piker's projection -- its subtypeWordChanges
  -- carry the pair -- and reaches nothing rule 702.135a mints for a keyword the
  -- Insurance granted afterwards.
  Spec.it s "CR 612.3 an evolved Goblin Piker's GRANTED afterlife still mints a Spirit Token" $ do
    (pikerId, alive, tokens, after) <- insuredChain s registry "Goblin Piker" (Just (Subtype.Spirit, Subtype.Elf))
    Spec.assertEqWith s "one Creature -- Spirit" (tokenSubtypes tokens after) [[Subtype.Spirit]]
    Spec.assertEqWith s "named Spirit Token" (tokenNames tokens after) [[CardName.MkCardName (Text.pack "Spirit Token")]]
    Spec.assertBool s (Projection.hasKeyword (Keyword.Afterlife 1) pikerId alive) "the grant reached the Piker"
    -- The Piker prints no Spirit, so its own type line is untouched either way:
    -- what the Evolution had to reach was rule 702.135a's word alone.
    Spec.assertEqWith s "Goblin Warrior still, while it lived" (Projection.subtypesOf pikerId alive) (Set.fromList [Subtype.Goblin, Subtype.Warrior])

  -- CR 702.135b -- "if a permanent has multiple instances of afterlife, each
  -- triggers separately" -- is what makes this per INSTANCE and not per object. A
  -- Ministrant of Obligation holding its printed afterlife 2 and the Insurance's
  -- granted afterlife 1 mints two Elves off the printed instance and one Spirit
  -- off the granted one. "Never rewrite a minted ability" would give three
  -- Spirits, and today's rewrite-everything gives three Elves.
  Spec.it s "CR 702.135b an evolved Ministrant plus a granted afterlife leaves two Elves and a Spirit" $ do
    (ministrantId, alive, tokens, after) <- insuredChain s registry "Ministrant of Obligation" (Just (Subtype.Spirit, Subtype.Elf))
    Spec.assertEqWith s "two Elves and a Spirit" (tokenSubtypes tokens after) [[Subtype.Elf], [Subtype.Elf], [Subtype.Spirit]]
    Spec.assertEqWith s "named for their own subtypes" (tokenNames tokens after) [[CardName.MkCardName (Text.pack "Elf Token")], [CardName.MkCardName (Text.pack "Elf Token")], [CardName.MkCardName (Text.pack "Spirit Token")]]
    Spec.assertBool s (Projection.hasKeyword (Keyword.Afterlife 1) ministrantId alive) "the granted instance"
    Spec.assertBool s (Projection.hasKeyword (Keyword.Afterlife 2) ministrantId alive) "beside the printed one"

  -- CR 612.2a's fourth carrier, and the one that forces the walk to be
  -- unconditional: an EMBLEM. CR 114.3 leaves it "no characteristics other than
  -- the abilities defined by the effect that created it", so a rewrite gated on
  -- the type line -- which is how the token faces above are reached -- finds
  -- nothing on an emblem's own face and stops before the ability two levels down
  -- where the word actually is.
  --
  -- Ajani, Adversary of Tyrants' "-7: You get an emblem with 'At the beginning
  -- of your end step, create three 1/1 white Cat creature tokens with lifelink'"
  -- (checked against Scryfall). The Evolution is resolved at the AJANI, before
  -- the ultimate is activated, so the swap reaches the CreateEmblem effect
  -- through the projected activated ability.
  --
  -- Cat -> WURM, not Cat -> Wall: the Evolution's own text forbids Wall.
  --
  -- The control first, so neither case can pass on a chain that minted nothing.
  Spec.it s "CR 114.2 an unevolved Ajani's emblem mints three Cat Tokens" $ do
    (tokens, after) <- ajaniEmblemChain s registry Nothing
    Spec.assertEqWith s "three tokens" (length tokens) 3
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Cat" (Projection.subtypesOf oid after) (Set.singleton Subtype.Cat)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Cat Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Cat Token")))) tokens

  Spec.it s "CR 612.1 an evolved Ajani's emblem mints Wurms rather than Cats" $ do
    (tokens, after) <- ajaniEmblemChain s registry (Just (Subtype.Cat, Subtype.Wurm))
    Spec.assertEqWith s "three tokens" (length tokens) 3
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Wurm" (Projection.subtypesOf oid after) (Set.singleton Subtype.Wurm)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Wurm Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Wurm Token")))) tokens
    -- The rest of the minted card is untouched, so what moved is the one word.
    mapM_ (\oid -> Spec.assertEqWith s "still 1/1" (Projection.powerOf oid after, Projection.toughnessOf oid after) (Just (1 :: Integer), Just (1 :: Integer))) tokens
    mapM_ (\oid -> Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink oid after) "and still lifelinking") tokens

  -- The second negative, and the one that pins the swap to the WORD rather than
  -- to the emblem being walked at all: the same board with the Evolution naming
  -- a pair the emblem does not spell.
  Spec.it s "CR 612.2 an Evolution naming a word the emblem lacks leaves the Cats alone" $ do
    (tokens, after) <- ajaniEmblemChain s registry (Just (Subtype.Goblin, Subtype.Wurm))
    Spec.assertEqWith s "three tokens" (length tokens) 3
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Cat" (Projection.subtypesOf oid after) (Set.singleton Subtype.Cat)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Cat Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Cat Token")))) tokens

  -- CR 608.2b names this case in as many words: a target stops being legal when
  -- "an effect may have changed the text of the spell". Piety Charm {W} Instant,
  -- second mode "Target Soldier creature gets +2/+2 until end of turn" (checked
  -- against Scryfall); the target is legal when chosen at CR 601.2c, an
  -- Evolution then rewrites the clause to say Goblin, and the re-check at
  -- resolution finds a Soldier where the clause now asks for a Goblin.
  --
  -- THREE BOARDS DIFFERING IN ONE THING, since "the spell fizzled" is the
  -- repository's most reliable false pass -- a mis-set-up board fizzles too. The
  -- unevolved case and the irrelevant-swap case both resolve and apply the
  -- +2/+2, off the same board and the same two casts.
  --
  -- Blade Instructor is a 3/1 Human Soldier and the Goblin Piker beside it a
  -- 2/1: no assertion here can be met by the wrong creature, and 5/3 is a number
  -- neither prints.
  Spec.it s "CR 608.2b an unevolved Piety Charm resolves and the Soldier is 5/3" $ do
    (soldierId, gobId, after) <- pietyCharmChain s registry Nothing
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0
    Spec.assertEqWith s "3/1 plus 2/2" (S.powerToughnessOf soldierId after) (Just (5, 3))
    Spec.assertEqWith s "and the Goblin beside it is untouched" (S.powerToughnessOf gobId after) (Just (2, 1))

  Spec.it s "CR 608.2b an evolved Piety Charm finds its target illegal and fizzles" $ do
    (soldierId, gobId, after) <- pietyCharmChain s registry (Just (Subtype.Soldier, Subtype.Goblin))
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0
    Spec.assertEqWith s "the Soldier is its printed 3/1" (S.powerToughnessOf soldierId after) (Just (3, 1))
    Spec.assertEqWith s "and the Goblin the clause now names got nothing either" (S.powerToughnessOf gobId after) (Just (2, 1))

  Spec.it s "CR 612.2 an Evolution naming a word the charm lacks leaves it resolving" $ do
    (soldierId, gobId, after) <- pietyCharmChain s registry (Just (Subtype.Elf, Subtype.Goblin))
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0
    Spec.assertEqWith s "still 5/3" (S.powerToughnessOf soldierId after) (Just (5, 3))
    Spec.assertEqWith s "and the Goblin is untouched" (S.powerToughnessOf gobId after) (Just (2, 1))

  -- The falsifier for a word-blind rewrite, on the same board with one word
  -- changed: Human is printed on the Ministrant and nowhere in rule 702.135a, so
  -- an Evolution naming it moves the Ministrant's own type line and leaves the
  -- Spirits alone.
  Spec.it s "CR 612.2 an Evolution naming Human leaves the Spirits Spirits" $ do
    (ministrantId, alive, tokens, after) <- ministrantChain s registry (Just (Subtype.Human, Subtype.Elf))
    -- The Evolution DID resolve and DID land on the Ministrant: without this the
    -- assertions below would pass for a spell that never took effect.
    Spec.assertEqWith s "Elf Cleric while it lived" (Projection.subtypesOf ministrantId alive) (Set.fromList [Subtype.Elf, Subtype.Cleric])
    Spec.assertEqWith s "two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "Creature -- Spirit" (Projection.subtypesOf oid after) (Set.singleton Subtype.Spirit)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "named Spirit Token" (Projection.namesOf oid after) (Set.singleton (CardName.MkCardName (Text.pack "Spirit Token")))) tokens

  -- The BOUNDARY the cases above sit on, and the falsifier for reading them
  -- as "a text change rewrites names": CR 612.2's closing sentence -- "an effect
  -- that changes a color word or a subtype can't change a card name, even if
  -- that name contains a word or a series of letters that is the same as a Magic
  -- color word, basic land type, or creature type". Goblin Piker is Creature --
  -- Goblin Warrior and is NAMED "Goblin Piker", so it is the pool's one card
  -- where the coincidence is real. CR 612.2a's exception does not reach it --
  -- the Piker defines no token -- so the Evolution must make it an Elf Warrior
  -- still named Goblin Piker.
  --
  -- What this pins is the SCOPE of the exception: the swap reaches an object's
  -- name only through the card a Create defines, never through the projection of
  -- the object it is aimed at. Projection.rewriteCard's own gate -- the word must
  -- be a subtype of the card whose name it is rewriting -- has no card in this
  -- pool that makes it observable, since every token here is named after exactly
  -- its own subtypes (CR 111.4).
  Spec.it s "CR 612.2 an evolved Goblin Piker is an Elf Warrior still NAMED Goblin Piker" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    artificialEvolution <- S.printingOf s registry "Artificial Evolution"
    let (pikerId, g1) = S.addPermanent piker S.alice (S.landsInPlay island 1)
        (evolutionId, g2) = S.addHandCard artificialEvolution S.alice g1
        after = S.runPure (evolveAt pikerId Subtype.Goblin Subtype.Elf) g2 $ do
          S.cast S.alice evolutionId
          Stack.resolveTop
    Spec.assertEqWith s "Creature -- Elf Warrior" (Projection.subtypesOf pikerId after) (Set.fromList [Subtype.Elf, Subtype.Warrior])
    Spec.assertEqWith s "and the name is untouched" (Projection.namesOf pikerId after) (Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")))

  -- The control for the pair below, and what rules out "the ability never
  -- resolved": with no Evolution the printed word stands, so the Spider-Punk --
  -- and only it -- takes the counter.
  Spec.it s "CR 612 an unevolved Coulson counters the Hero and not the Goblin" $ do
    (punkId, pikerId, after) <- coulsonChain s registry Nothing
    Spec.assertEqWith s "the Spider-Punk got the counter" (S.counterOf CounterKind.PlusOnePlusOne punkId after) 1
    Spec.assertEqWith s "the Goblin Piker got none" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 0

  -- And the point: CR 612.2's creature-type swap reaches the ObjectRef inside
  -- PutCounters, so "each other Hero you control" becomes "each other Goblin you
  -- control" and the two counts trade places. The Coulson itself is a Goblin by
  -- then and still takes nothing, because "other" is a Not IsSource the swap
  -- does not touch.
  Spec.it s "CR 612.2 an evolved Coulson counters the Goblin and not the Hero" $ do
    (punkId, pikerId, after) <- coulsonChain s registry (Just (Subtype.Hero, Subtype.Goblin))
    Spec.assertEqWith s "the Goblin Piker got the counter" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
    Spec.assertEqWith s "the Spider-Punk got none" (S.counterOf CounterKind.PlusOnePlusOne punkId after) 0

  -- CR 612.1's "any words or symbols printed on that object" reaches a word held
  -- inside a NUMBER the effect computes, not only the ones naming an object.
  -- Goblin War Strike {R} Sorcery -- "Goblin War Strike deals damage to target
  -- player or planeswalker equal to the number of Goblins you control" (checked
  -- against Scryfall) -- puts the changeable word inside its damage clause's own
  -- Count, where every other case in this group puts it in a filter or a subtype.
  --
  -- The chain aims the Strike at a player; Pool.PlayersAndPlaneswalkers is the
  -- slot's own pool, and Pawl.PlaneswalkerSpec proves the planeswalker half.
  --
  -- The control first, so the pair cannot pass on a chain that never resolved the
  -- Strike.
  Spec.it s "CR 120.1 an unevolved Goblin War Strike counts Goblins" $ do
    after <- goblinWarStrikeChain s registry Nothing
    Spec.assertEqWith s "bob took 2, one per Goblin alice controls" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0

  -- And the point: the swap reaches the Count inside the damage clause, so the
  -- resolving Strike counts alice's four Elves instead of her two Goblins.
  Spec.it s "CR 612.1 an evolved Goblin War Strike counts Elves instead" $ do
    after <- goblinWarStrikeChain s registry (Just (Subtype.Goblin, Subtype.Elf))
    Spec.assertEqWith s "bob took 4, one per Elf alice controls" (S.lifeOf S.bob after) (Just 16)
    Spec.assertEqWith s "the stack emptied" (length (GameState.stack after)) 0

  -- CR 612.1 into a quoted ability a RESOLUTION grants: the words are printed on
  -- the GRANTER, so a text change affecting the Clavileño rewrites them before
  -- its trigger hands them over. CR 612.3 bounds the other direction -- an
  -- ability granted TO an object -- and does not reach this one.
  --
  -- The control first, so the pair cannot pass on a chain that killed nothing.
  Spec.it s "CR 111.4 an unevolved Clavileño's granted ability mints a Vampire Demon Token" $ do
    (clavilenoId, victimId, alive, tokens, after) <- clavilenoChain s registry Nothing
    Spec.assertEqWith s "one Creature -- Vampire Demon" (tokenSubtypes tokens after) [[Subtype.Demon, Subtype.Vampire]]
    Spec.assertEqWith s "named Vampire Demon Token" (tokenNames tokens after) [[CardName.MkCardName (Text.pack "Vampire Demon Token")]]
    -- The trigger DID resolve at the victim, so the assertions above are not
    -- reading a board where nothing happened.
    Spec.assertEqWith s "the victim was a Vampire Demon while it lived" (Projection.subtypesOf victimId alive) (Set.fromList [Subtype.Vampire, Subtype.Demon])
    Spec.assertEqWith s "and the Clavileño's own type line prints no Demon" (Projection.subtypesOf clavilenoId alive) (Set.fromList [Subtype.Vampire, Subtype.Cleric])

  -- And the point. The swap reaches the token defined inside the quoted ability,
  -- name and type line alike (CR 612.2a), and the rest of that token is untouched.
  Spec.it s "CR 612.1 an evolved Clavileño's granted ability mints a Vampire Elf Token" $ do
    (clavilenoId, victimId, alive, tokens, after) <- clavilenoChain s registry (Just (Subtype.Demon, Subtype.Elf))
    Spec.assertEqWith s "one Creature -- Vampire Elf" (tokenSubtypes tokens after) [[Subtype.Elf, Subtype.Vampire]]
    Spec.assertEqWith s "named Vampire Elf Token" (tokenNames tokens after) [[CardName.MkCardName (Text.pack "Vampire Elf Token")]]
    -- Only the word moved: the token is the printed 4/3 with flying still.
    mapM_ (\oid -> Spec.assertEqWith s "still 4/3" (Projection.powerOf oid after, Projection.toughnessOf oid after) (Just (4 :: Integer), Just (3 :: Integer))) tokens
    mapM_ (\oid -> Spec.assertBool s (Projection.hasKeyword Keyword.Flying oid after) "and still flying") tokens
    -- The layer-4 half of the same ability took the swap too, by a sibling arm of
    -- rewriteModification -- so it cannot be what the token assertions read.
    Spec.assertEqWith s "the victim was a Vampire Elf while it lived" (Projection.subtypesOf victimId alive) (Set.fromList [Subtype.Vampire, Subtype.Elf])
    Spec.assertEqWith s "and the Clavileño is a Vampire Cleric still" (Projection.subtypesOf clavilenoId alive) (Set.fromList [Subtype.Vampire, Subtype.Cleric])

  -- The falsifier for a word-blind rewrite, on the same board with one word
  -- changed: Cleric is printed on the Clavileño's type line and nowhere in the
  -- ability, so an Evolution naming it moves that one word and leaves the quoted
  -- ability's Demon standing. Vampire would not serve -- it is the trigger's own
  -- target filter, and swapping it leaves the trigger with no legal target when it
  -- goes on the stack (CR 603.3d), which is a different reason for a Demon to
  -- survive.
  Spec.it s "CR 612.2 an Evolution naming Cleric leaves the token's Demon a Demon" $ do
    (clavilenoId, victimId, alive, tokens, after) <- clavilenoChain s registry (Just (Subtype.Cleric, Subtype.Zombie))
    Spec.assertEqWith s "one Creature -- Vampire Demon still" (tokenSubtypes tokens after) [[Subtype.Demon, Subtype.Vampire]]
    Spec.assertEqWith s "named Vampire Demon Token" (tokenNames tokens after) [[CardName.MkCardName (Text.pack "Vampire Demon Token")]]
    Spec.assertEqWith s "the victim was a Vampire Demon while it lived" (Projection.subtypesOf victimId alive) (Set.fromList [Subtype.Vampire, Subtype.Demon])
    -- The Evolution DID resolve and DID land on the Clavileño, so the assertions
    -- above are not reading a board where the swap never happened.
    Spec.assertEqWith s "and the Clavileño itself is a Vampire Zombie" (Projection.subtypesOf clavilenoId alive) (Set.fromList [Subtype.Vampire, Subtype.Zombie])

  -- CR 612.1 reaching a FLOATING replacement effect (CR 614.3), which no case
  -- above installs: the other chains here rewrite what the resolution itself
  -- does, while a Moonmist leaves a shield behind that is asked again at every
  -- later damage event (CR 609.7b). The words that decide what it spares are
  -- printed on the spell, so the swap has to land before the shield is built.
  --
  -- The control first, so the pair cannot pass on a chain whose Moonmist never
  -- resolved.
  Spec.it s "CR 615.1 an unevolved Moonmist spares the Werewolf and the Wolf" $ do
    (wolf, werewolf, _, shielded, after) <- moonmistShieldChain s registry Nothing
    Spec.assertEqWith s "the Wolf's and the Werewolf's 3s landed and the Piker's 2 did not" (fmap DamageEvent.source (S.damageEventsOf after)) [wolf, werewolf]
    Spec.assertEqWith s "so alice is on 14" (S.lifeOf S.alice after) (Just 14)
    Spec.assertEqWith s "supporting: the Moonmist left one floating shield" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "supporting: alice started on 20" (S.lifeOf S.alice shielded) (Just 20)

  -- And the point: with Werewolf replaced by Goblin, the shield the SAME spell
  -- installs stops the Werewolf's damage and lets the Goblin's through. The
  -- Piker and the Werewolf trade places and the untouched Wolf keeps its
  -- exemption, so a shield built from the printed text produces the control's
  -- board -- both a different event list and a different life total.
  Spec.it s "CR 612.1 an evolved Moonmist's shield spares Goblins instead, so the Werewolf's damage is the prevented one" $ do
    (wolf, _, piker, shielded, after) <- moonmistShieldChain s registry (Just (Subtype.Werewolf, Subtype.Goblin))
    Spec.assertEqWith s "the Wolf's 3 and the Piker's 2 landed and the Werewolf's 3 did not" (fmap DamageEvent.source (S.damageEventsOf after)) [wolf, piker]
    Spec.assertEqWith s "so alice is on 15" (S.lifeOf S.alice after) (Just 15)
    Spec.assertEqWith s "supporting: the Moonmist left one floating shield" (length (GameState.replacements shielded)) 1
    Spec.assertEqWith s "supporting: alice started on 20" (S.lifeOf S.alice shielded) (Just 20)

-- The one activated ability of a printing that declares exactly one -- Prodigal
-- Sorcerer's {T}, which is all these fixtures reach for. Nothing for any other
-- printing, so a card that grew a second ability fails the case that names it
-- rather than silently picking whichever came first (Pawl.TargetSpec's
-- soleTargetSlot is the same shape for the same reason).
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
soleActivatedAbility p = case Face.activatedAbilities (S.combinedFace p) of
  [only] -> Just only
  _ -> Nothing

-- bob has a settled Prodigal Sorcerer ("{T}: This creature deals 1 damage to any
-- target"); alice has `lands` Islands and `stifles` Stifles in hand. bob
-- activates the Sorcerer at ALICE, so the ability's effect is observable as her
-- life total, and the returned state is the one where the ability is on the
-- stack, waiting.
stifleBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> Maybe ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
stifleBoard island stifle sorcerer lands stifles = case soleActivatedAbility sorcerer of
  Nothing -> Nothing
  Just ability ->
    let (srcId, withSorcerer) = S.addPermanent sorcerer S.bob (Setup.emptyGame S.bothPlayers)
        -- CR 302.6: the Sorcerer must have settled under bob before its {T} may
        -- be activated at all.
        settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.bob)
        withLands = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) settled [1 .. lands]
        (stifleIds, withStifles) =
          List.foldl'
            (\(ids, g) _ -> let (i, g') = S.addHandCard stifle S.alice g in (ids <> [i], g'))
            ([], withLands)
            [1 .. stifles]
        atAlice :: Prompt.Prompt r -> r
        atAlice p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
          _ -> S.identityAnswer p
        activated = S.runPure atAlice (withStifles {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob srcId ability)
     in Just (stifleIds, srcId, activated)

-- CR 701.6a covers "a spell or ability", and Stifle ({U} Instant, "Counter
-- target activated or triggered ability. (Mana abilities can't be targeted.)")
-- is the first card in the pool that reaches the second half. Cancel proved the
-- spell half above; these cases are the ability half, and what makes them a
-- different test rather than the same one twice is rule 701.6a's LAST sentence:
-- "a countered spell is put into its owner's graveyard." Only a spell. CR 608.2n
-- says how an ability leaves instead -- "the ability is removed from the stack
-- and ceases to exist" -- so the graveyard assertions here are the load-bearing
-- ones, and they are stated as counts of what did NOT arrive.
--
-- CR 113.9 is why one card cannot do both: "activated and triggered abilities on
-- the stack aren't spells, and therefore can't be countered by anything that
-- counters only spells. Activated and triggered abilities on the stack can be
-- countered by effects that specifically counter abilities." Pawl.TargetSpec
-- holds that half, as the two disjoint pools.
--
-- Stifle's parenthetical needs nothing implemented and is not tested for: CR
-- 605.3b ("an activated mana ability doesn't go on the stack, so it can't be
-- targeted, countered, or otherwise responded to") and CR 605.4a keep a mana
-- ability off the stack in the first place, so it is never a candidate. See
-- Pawl.Types.Pool.Abilities.
stifleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stifleSpec s registry = Spec.describe s "Stifle" $ do
  -- The ACTIVATED half (CR 113.3b). The discriminating assertions are alice's
  -- untouched life -- rule 701.6a's "it doesn't resolve and none of its effects
  -- occur" -- and bob's EMPTY graveyard, which is what tells a cease (CR 608.2n)
  -- apart from the graveyard move Cancel makes.
  Spec.it s "CR 701.6a whole cards: Stifle counters Prodigal Sorcerer's activated ability, which ceases (CR 608.2n)" $ do
    island <- S.printingOf s registry "Island"
    stifle <- S.printingOf s registry "Stifle"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    case stifleBoard island stifle sorcerer 1 1 of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (stifleIds, srcId, activated) -> do
        let abilIds = GameState.stack activated
            cast = List.foldl' (\g oid -> S.runPure S.identityAnswer g (S.cast S.alice oid)) activated stifleIds
            countered = S.runPure S.identityAnswer cast Stack.resolveTop
        Spec.assertEqWith s "the activation put exactly one ability on the stack" (length abilIds) 1
        Spec.assertEqWith s "and both it and the Stifle are gone from the stack" (GameState.stack countered) []
        -- CR 701.6a: "it doesn't resolve and none of its effects occur."
        Spec.assertEqWith s "alice took no damage: the ability never resolved" (S.lifeOf S.alice countered) (Just 20)
        -- CR 608.2n: the ability ceased. It is not in a graveyard -- an ability
        -- is not a card and has no owner's graveyard to be put into -- and it is
        -- not an object at all any more.
        Spec.assertEqWith s "nothing arrived in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob countered)) 0
        Spec.assertEqWith s "alice's graveyard holds the spent Stifle and nothing else" (length (Game.zoneMembers Zone.Graveyard S.alice countered)) 1
        Spec.assertEqWith s "the ability object ceased to exist" (fmap (\oid -> Game.lookupObject oid countered) abilIds) [Nothing]
        -- CR 113.7a: the ability was its own object, so countering it leaves the
        -- SOURCE alone -- and CR 701.6b gives no refund, so the Sorcerer stays
        -- tapped for a {T} that bought nothing.
        Spec.assertBool s (Set.member srcId (GameState.battlefield countered)) "the Prodigal Sorcerer is untouched on the battlefield"
        Spec.assertEqWith s "still tapped: CR 701.6b refunds no cost" (fmap Object.tapped (Game.lookupObject srcId countered)) (Just TapState.Tapped)
  -- The TRIGGERED half (CR 113.3c), and a different observation: Aether Flash's
  -- trigger is what kills a Goblin Piker in Pawl.TriggerSpec's own case, so the
  -- Piker being ALIVE with no damage marked is the same effect not occurring.
  Spec.it s "CR 701.6a whole cards: Stifle counters Aether Flash's triggered ability, so the Piker lives" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    aetherFlash <- S.printingOf s registry "Aether Flash"
    piker <- S.printingOf s registry "Goblin Piker"
    stifle <- S.printingOf s registry "Stifle"
    let (flashId, withFlash) = S.addPermanent aetherFlash S.alice (Setup.emptyGame S.bothPlayers)
        withMountains = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) withFlash [1 .. (2 :: Int)]
        (_, withIsland) = S.addPermanent island S.bob withMountains
        (stifleId, withStifle) = S.addHandCard stifle S.bob withIsland
        (pikerId, gs) = S.addHandCard piker S.alice withStifle
        cast = S.runPure S.identityAnswer gs (S.cast S.alice pikerId)
        -- The Piker resolves and enters; CR 603.3 then puts Aether Flash's
        -- trigger on the stack the next time a player would receive priority.
        entered = S.runPure S.identityAnswer cast Stack.resolveTop
        placed = S.runPure S.identityAnswer entered Engine.settleForPriority
        stifled = S.runPure S.identityAnswer placed (S.cast S.bob stifleId)
        countered = S.runPure S.identityAnswer stifled Stack.resolveTop
        after = S.runPure S.identityAnswer countered Engine.settleForPriority
        entrantId = case filter (\oid -> fmap Face.name (Game.faceOf oid after) == Just (CardName.MkCardName $ Text.pack "Goblin Piker")) (Set.toList (GameState.battlefield after)) of
          [only] -> Just only
          _ -> Nothing
    Spec.assertEqWith s "the trigger is the only thing on the stack before the Stifle" (length (GameState.stack placed)) 1
    Spec.assertEqWith s "the stack is empty afterwards" (GameState.stack after) []
    -- The falsifier is Pawl.TriggerSpec's aetherFlashSpec, where the same
    -- Aether Flash's 2 damage kills the same 2/1 (CR 704.5g): a Piker alive with
    -- NO damage marked is rule 701.6a's "none of its effects occur".
    Spec.assertEqWith s "the Piker survived" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice after) 1
    Spec.assertEqWith s "with no damage marked on it at all" (fmap (\oid -> fmap Object.damage (Game.lookupObject oid after)) entrantId) (Just (Just 0))
    Spec.assertEqWith s "no damage was ever dealt" (fmap DamageEvent.amount (Maybe.mapMaybe Event.damageOf (S.eventsOf after))) []
    -- CR 608.2n again: the countered trigger went nowhere. alice's graveyard is
    -- empty (no Piker corpse, and no residue of the trigger), and bob's holds
    -- only the Stifle that did the countering.
    Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "bob's holds the spent Stifle alone" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertBool s (Set.member flashId (GameState.battlefield after)) "and Aether Flash itself is untouched"
  -- CR 608.2b, for a target that CEASED rather than moved: "a target that's no
  -- longer in the zone it was in when it was targeted is illegal ... If all its
  -- targets ... are now illegal, the spell or ability doesn't resolve. It's
  -- removed from the stack and, IF IT'S A SPELL, put into its owner's
  -- graveyard." Stifle is a spell, so the fizzled one is buried; the ability it
  -- was aimed at left by ceasing, which is not a zone change at all.
  --
  -- The twin of the racing Cancels above, one card over.
  Spec.it s "CR 608.2b a Stifle whose ability already ceased fizzles" $ do
    island <- S.printingOf s registry "Island"
    stifle <- S.printingOf s registry "Stifle"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    case stifleBoard island stifle sorcerer 2 2 of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (stifleIds, _, activated) -> do
        let castAll g oid = S.runPure S.identityAnswer g (S.cast S.alice oid)
            bothCast = List.foldl' castAll activated stifleIds
            first' = S.runPure S.identityAnswer bothCast Stack.resolveTop
            second' = S.runPure S.identityAnswer first' Stack.resolveTop
        Spec.assertEqWith s "two Stifles were cast onto the ability" (length (GameState.stack bothCast)) 3
        Spec.assertEqWith s "the first counters the ability" (length (GameState.stack first')) 1
        Spec.assertEqWith s "and the second fizzles off the stack" (GameState.stack second') []
        Spec.assertEqWith s "both Stifles are in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice second')) 2
        Spec.assertEqWith s "bob's graveyard stayed empty throughout" (length (Game.zoneMembers Zone.Graveyard S.bob second')) 0
        Spec.assertEqWith s "and alice never took the damage" (S.lifeOf S.alice second') (Just 20)

-- CR 113.3b against CR 113.3c, one kind of ability at a time. Squelch ({1}{U}
-- Instant, "Counter target activated ability. (Mana abilities can't be
-- targeted.) / Draw a card.") narrows Stifle's Pool.Abilities to the activated
-- half with Filter.IsActivatedAbility; the parenthetical needs nothing
-- implemented, for stifleSpec's reason (CR 605.3b).
--
-- ONE board carrying one ability of each kind, so neither half can pass by the
-- pool being empty, and the answerer takes the SMALLEST recipient offered --
-- Recipient's derived Ord orders ToObject by ObjectId, and Game.freshObjectId
-- hands out increasing ones, so the Aether Flash trigger (created when the Piker
-- entered) is the one a Squelch that could reach a trigger would take. That is
-- what makes alice's life total discriminating: countering the ping is the only
-- way it stays 20.
--
-- Pawl.TargetSpec holds the pool-level twin, where the offered sets are read
-- directly and Stifle's is the union of both.
squelchSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
squelchSpec s registry = Spec.describe s "Squelch" $ do
  Spec.it s "CR 113.3b Squelch counters the activated ability and leaves the trigger, which still kills the Piker" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    aetherFlash <- S.printingOf s registry "Aether Flash"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    squelch <- S.printingOf s registry "Squelch"
    case soleActivatedAbility sorcerer of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just ping -> do
        let (srcId, withSorcerer) = S.addPermanent sorcerer S.bob (Setup.emptyGame S.bothPlayers)
            -- CR 302.6: the Sorcerer must have settled before its {T} is legal.
            settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.bob)
            (flashId, withFlash) = S.addPermanent aetherFlash S.alice settled
            -- Two Mountains for the Piker and two Islands for the Squelch.
            withLands =
              List.foldl'
                (\g p -> snd (S.addPermanent p S.alice g))
                withFlash
                [mountain, mountain, island, island]
            -- CR 121.1: the draw takes the top card of a library, and CR 104.3c
            -- would lose alice the game before the assertions run if there were
            -- none.
            withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) withLands [1 .. (3 :: Int)]
            (squelchId, withSquelch) = S.addHandCard squelch S.alice withLibrary
            (pikerId, gs) = S.addHandCard piker S.alice withSquelch
            cast = S.runPure S.identityAnswer gs (S.cast S.alice pikerId)
            -- CR 603.3: the Piker enters, and Aether Flash's trigger goes on the
            -- stack the next time a player would receive priority.
            entered = S.runPure S.identityAnswer cast Stack.resolveTop
            placed = S.runPure S.identityAnswer entered Engine.settleForPriority
            atAlice :: Prompt.Prompt r -> r
            atAlice p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
              _ -> S.identityAnswer p
            -- bob's ping is aimed at alice, so whether it resolved is readable
            -- as her life total.
            pinging = S.runPure atAlice (placed {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob srcId ping)
            takeOldest :: Prompt.Prompt r -> r
            takeOldest p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> maybe Set.empty Set.singleton (Set.lookupMin legal)) sets
              _ -> S.identityAnswer p
            squelched = S.runPure takeOldest (pinging {GameState.priority = Just S.alice}) (S.cast S.alice squelchId)
            countered = S.runPure S.identityAnswer squelched Stack.resolveTop
            after = S.runPure S.identityAnswer (S.runPure S.identityAnswer countered Stack.resolveTop) Engine.settleForPriority
        Spec.assertEqWith s "the stack held one trigger and one activated ability before the Squelch" (length (GameState.stack pinging)) 2
        -- The discriminating pair. A Squelch that could reach a trigger would
        -- have taken the older object -- the Aether Flash trigger -- and then the
        -- ping resolves for 1 and the Piker lives.
        Spec.assertEqWith s "alice's life is untouched: the ping was countered (CR 701.6a)" (S.lifeOf S.alice after) (Just 20)
        Spec.assertEqWith s "and the Piker took the Aether Flash's 2: the trigger was not countered" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice after) 0
        -- Squelch's second sentence, and the reason the resolution got that far.
        Spec.assertEqWith s "alice drew a card" (S.handSize S.alice after) 1
        -- Supporting: CR 608.2n's cease for the ability, CR 701.6a's graveyard
        -- for the spell that did it.
        Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
        Spec.assertEqWith s "bob's graveyard is empty: an ability ceases rather than moving" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
        Spec.assertBool s (Set.member flashId (GameState.battlefield after)) "and Aether Flash itself is untouched"

-- CR 113.7 through CR 701.6a and back: Green Slime ({2}{G} Creature -- Ooze,
-- flash, "When this creature enters, counter target activated or triggered
-- ability from an artifact or enchantment source. If a permanent's ability is
-- countered this way, destroy that permanent."). Its target is Stifle's
-- Pool.Abilities under Filter.FromSource, whose nest is matched against the
-- ability's SOURCE rather than the ability; its rider reads
-- Pawl.Types.Counter's `sources` slot, the permanents walked to from what the
-- funnel countered.
--
-- Three boards, one per half of the rule. The first carries an ability of an
-- artifact and one of a creature and the answerer takes the SMALLEST recipient
-- offered, squelchSpec's posture: the creature's ability is the older object,
-- so a FromSource that admitted it would take it, and a FromSource that admitted
-- nothing would leave the trigger targetless. The second is CR 113.7a: the
-- artifact was sacrificed to activate the ability, so the source is read from
-- last known information -- and, being no permanent, is not destroyed. The
-- third is the enchantment half and the TRIGGERED half at once, on bob's turn
-- so CR 603.3b puts his Aether Flash trigger under alice's.
greenSlimeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
greenSlimeSpec s registry = Spec.describe s "Green Slime" $ do
  Spec.it s "CR 113.7 the trigger reaches the artifact's ability and not the creature's, and the artifact is destroyed" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    ball <- S.printingOf s registry "Crystal Ball"
    slime <- S.printingOf s registry "Green Slime"
    case (soleActivatedAbility sorcerer, soleActivatedAbility ball) of
      (Just ping, Just scry) -> do
        let (sorcererId, withSorcerer) = S.addPermanent sorcerer S.bob (Setup.emptyGame S.bothPlayers)
            (ballId, withBall) = S.addPermanent ball S.bob withSorcerer
            -- CR 302.6: the Sorcerer must have settled before its {T} is legal.
            settled = S.runPure S.identityAnswer withBall (Engine.settleAll S.bob)
            -- A Mountain for the Ball's {1}, three Forests for the Slime.
            withLands = List.foldl' (\g (p, pid) -> snd (S.addPermanent p pid g)) settled [(mountain, S.bob), (forest, S.alice), (forest, S.alice), (forest, S.alice)]
            -- CR 701.22a: the scry looks at bob's library, so stock it.
            withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.bob g)) withLands [1 .. (3 :: Int)]
            (slimeId, gs) = S.addHandCard slime S.alice withLibrary
            atAlice :: Prompt.Prompt r -> r
            atAlice p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
              _ -> S.identityAnswer p
            -- The Sorcerer's ping first, so its ability is the OLDER object; the
            -- Ball's scry on top of it.
            pinging = S.runPure atAlice (gs {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob sorcererId ping)
            scrying = S.runPure S.identityAnswer (pinging {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob ballId scry)
            cast = S.runPure S.identityAnswer (scrying {GameState.priority = Just S.alice}) (S.cast S.alice slimeId)
            entered = S.runPure S.identityAnswer cast Stack.resolveTop
            takeOldest :: Prompt.Prompt r -> r
            takeOldest p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> maybe Set.empty Set.singleton (Set.lookupMin legal)) sets
              _ -> S.identityAnswer p
            -- CR 603.3: the Slime's trigger goes on the stack, choosing its target.
            placed = S.runPure takeOldest entered Engine.settleForPriority
            -- The trigger, then whatever of bob's two abilities survived it.
            resolved = List.foldl' (\g _ -> S.runPure S.identityAnswer g Stack.resolveTop) placed [1 .. (3 :: Int)]
            after = S.runPure S.identityAnswer resolved Engine.settleForPriority
        -- The discriminating pair, AHEAD of the proxies: which ability the
        -- trigger took is readable as which permanent its rider destroyed (CR
        -- 113.7) and whether the ping landed.
        Spec.assertEqWith s "Crystal Ball is in bob's graveyard: its ability was countered and the rider destroyed it (CR 113.7)" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Crystal Ball")) S.bob after) 0
        Spec.assertEqWith s "alice took the Sorcerer's ping: a creature's ability is not from an artifact or enchantment source" (S.lifeOf S.alice after) (Just 19)
        Spec.assertEqWith s "the Sorcerer is untouched" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Prodigal Sorcerer")) S.bob after) 1
        Spec.assertEqWith s "bob's graveyard holds the Ball alone" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
        Spec.assertEqWith s "the stack held the Sorcerer's ping, the Ball's scry and the Slime" (length (GameState.stack cast)) 3
        Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
      _ -> Spec.assertFailure s "Prodigal Sorcerer and Crystal Ball should each declare one activated ability"
  Spec.it s "CR 113.7a the ability of an artifact sacrificed to activate it is still from an artifact source, and nothing is destroyed" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    egg <- S.printingOf s registry "Golden Egg"
    slime <- S.printingOf s registry "Green Slime"
    -- Golden Egg's second ability: "{2}, {T}, Sacrifice this artifact: You gain
    -- 3 life." The first is a mana ability, which never reaches the stack.
    case Face.activatedAbilities (S.combinedFace egg) of
      [_, gain] -> do
        -- The lands BEFORE the Egg, so S.identityAnswer's first-offered mana
        -- source for the {2} is a Mountain and never the Egg's own mana ability,
        -- whose sacrifice would take the source out from under the activation.
        let withLands = List.foldl' (\g (p, pid) -> snd (S.addPermanent p pid g)) (Setup.emptyGame S.bothPlayers) [(mountain, S.bob), (mountain, S.bob), (forest, S.alice), (forest, S.alice), (forest, S.alice)]
            (eggId, withEgg) = S.addPermanent egg S.bob withLands
            (slimeId, gs) = S.addHandCard slime S.alice withEgg
            -- CR 601.2h: the Egg is sacrificed as the cost, so by the time the
            -- ability waits on the stack its source is in bob's graveyard.
            activated = S.runPure S.identityAnswer (gs {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob eggId gain)
            cast = S.runPure S.identityAnswer (activated {GameState.priority = Just S.alice}) (S.cast S.alice slimeId)
            entered = S.runPure S.identityAnswer cast Stack.resolveTop
            placed = S.runPure S.identityAnswer entered Engine.settleForPriority
            resolved = List.foldl' (\g _ -> S.runPure S.identityAnswer g Stack.resolveTop) placed [1 .. (2 :: Int)]
            after = S.runPure S.identityAnswer resolved Engine.settleForPriority
        Spec.assertBool s (Set.notMember eggId (GameState.battlefield activated)) "the Egg left the battlefield as the ability's cost"
        -- The discriminating assertion: a source read live would find nothing
        -- and the trigger would have no target, so the Egg's ability would
        -- resolve for 3.
        Spec.assertEqWith s "bob's life is untouched: the departed Egg's ability was still from an artifact source (CR 113.7a) and was countered" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "bob's graveyard holds the Egg alone: a source that is no permanent is not destroyed" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
        Spec.assertEqWith s "bob's Mountains are untouched" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Mountain")) S.bob after) 2
        Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
      _ -> Spec.assertFailure s "Golden Egg should declare two activated abilities"
  Spec.it s "CR 113.7 an enchantment's triggered ability is a target too: the Slime survives Aether Flash and destroys it" $ do
    forest <- S.printingOf s registry "Forest"
    aetherFlash <- S.printingOf s registry "Aether Flash"
    slime <- S.printingOf s registry "Green Slime"
    let (flashId, withFlash) = S.addPermanent aetherFlash S.bob (Setup.emptyGame S.bothPlayers)
        withLands = List.foldl' (\g _ -> snd (S.addPermanent forest S.alice g)) withFlash [1 .. (3 :: Int)]
        (slimeId, gs) = S.addHandCard slime S.alice withLands
        -- bob's turn, so that CR 603.3b puts his Aether Flash trigger on the
        -- stack FIRST and alice's Slime trigger above it, where it can counter
        -- the trigger before the 2 damage is dealt. Flash is what lets alice
        -- cast the Slime here at all (CR 702.8).
        cast = S.runPure S.identityAnswer (gs {GameState.activePlayer = S.bob, GameState.priority = Just S.alice}) (S.cast S.alice slimeId)
        entered = S.runPure S.identityAnswer cast Stack.resolveTop
        placed = S.runPure S.identityAnswer entered Engine.settleForPriority
        resolved = List.foldl' (\g _ -> S.runPure S.identityAnswer g Stack.resolveTop) placed [1 .. (2 :: Int)]
        after = S.runPure S.identityAnswer resolved Engine.settleForPriority
    -- The discriminating pair, AHEAD of the stack-size proxy that would
    -- otherwise absorb a trigger left targetless: a Slime trigger that could
    -- not reach the enchantment's trigger would have let Aether Flash deal its
    -- 2 to a 2/2.
    Spec.assertEqWith s "Green Slime survives: the enchantment's trigger was countered (CR 701.6a)" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Green Slime")) S.alice after) 1
    Spec.assertBool s (Set.notMember flashId (GameState.battlefield after)) "and Aether Flash was destroyed: a permanent's ability was countered this way (CR 113.7)"
    Spec.assertEqWith s "Aether Flash is in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "both triggers went on the stack" (length (GameState.stack placed)) 2
    Spec.assertEqWith s "the stack is empty" (GameState.stack after) []

fizzleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fizzleSpec s registry = Spec.describe s "Fizzle" $ do
  Spec.it s "CR 608.2b Bolt-vs-Bolt through the priority loop: the second fizzles" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let after = snd (Engine.runGamePure boltAnswer (twoBoltState piker mountain lightningBolt) Engine.priorityLoop)
    Spec.assertEqWith s "stack cleared" (length (GameState.stack after)) 0
    Spec.assertEqWith s "Piker dead" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "both Bolts in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
    Spec.assertEqWith s "the Piker in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "bob's life untouched: the fizzled Bolt hit nothing" (S.lifeOf S.bob after) (Just 20)
  -- CR 608.2b pins the `targeted` restriction Task 3 added (Resolve.hs's
  -- resolveEffects/resolveSpell): a reserved slot (Binding.triggerSource)
  -- is vacuously legal, since CR 608.2b is about TARGETS and a reserved
  -- slot was never one -- but its vacuous legality must not rescue a
  -- fizzle whose one genuinely-targeted slot IS illegal. This needs an
  -- ability with BOTH kinds of slot at once, plus a second, targetless
  -- effect (Draw) whose execution is the only way to observe whether the
  -- fizzle happened: with a single targeted slot alone, fizzling and
  -- resolving-with-the-slot-skipped are indistinguishable (Destroy's own
  -- per-slot legality check already no-ops it either way).
  Spec.it s "CR 608.2b the reserved trigger-source slot does not rescue a fizzle: the targetless Draw after the ability's only real target dies does not run" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    forest <- S.printingOf s registry "Forest"
    let base0 = Setup.emptyGame S.bothPlayers
        (source, base1) = S.addPermanent piker S.alice base0
        (victim, base2) = S.addPermanent piker S.bob base1
        (_, base3) = S.addLibraryCard forest S.alice base2
        handBefore = S.handSize S.alice base3
        targetSlot = SlotName.MkSlotName (Text.pack "target")
        slots = Map.singleton targetSlot (TargetSlot.required Pool.Creatures Nothing)
        (abilId, base4) = S.spellOnStack piker S.alice base3
        -- Mirrors Engine.placeOne's own construction: a real chosen
        -- target under `targetSlot`, plus the reserved self slot every
        -- placed trigger carries (Binding.setTriggerSource).
        bindings =
          Binding.setTriggerSource
            source
            (Binding.fromChoices (Map.singleton targetSlot (Set.singleton (Recipient.ToCreature victim))) Nothing Seq.empty)
        withBindings = base4 {GameState.objects = Map.adjust (\o -> o {Object.bindings = bindings}) abilId (GameState.objects base4)}
        -- Kill the sole real target before resolution: CR 608.2b makes it
        -- illegal (it's no longer a legal CreatureTarget), while the
        -- reserved slot -- never targeted -- stays vacuously legal.
        gone = S.runPure S.identityAnswer withBindings (Event.changeZone victim Zone.Graveyard)
        mode = Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList [Effect.Destroy (Destroy.MkDestroy (ObjectRef.InSlot targetSlot) Regenerability.Regenerable Nothing Nothing Nothing), Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1) Nothing)]))) slots
        run = Resolve.resolveModes abilId source [(ModeInstance.MkModeInstance (ModeIndex.MkModeIndex 0) 0, mode)]
        after = snd (Engine.runGamePure S.identityAnswer gone run)
    Spec.assertEqWith s "the targetless Draw did not run: the ability fizzled" (S.handSize S.alice after) handBefore
  Spec.it s "CR 704.5a a Bolt can end the game mid-step" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        lowBob =
          gs {GameState.players = Map.adjust (\pl -> pl {Player.life = 3}) S.bob (GameState.players gs)}
        atBob :: Prompt.Prompt r -> r
        atBob p = case p of
          Prompt.ChooseTargets _ _ _ sets ->
            fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          Prompt.ChooseAction _ _ actions ->
            case filter (S.isCastOf oid) actions of
              h : _ -> h
              [] -> A.Pass
          _ -> S.identityAnswer p
        after = snd (Engine.runGamePure atBob lowBob Engine.priorityLoop)
    Spec.assertEqWith s "alice wins" (GameState.result after) (Just (Result.Won S.alice))
    Spec.assertEqWith s "the loop released priority" (GameState.priority after) Nothing

indestructibleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
indestructibleSpec s registry = Spec.describe s "Indestructible" $ do
  Spec.it s "CR 704.5g an indestructible creature survives lethal marked damage" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myrId, gs) = S.addPermanent darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        -- Myr is 0/1; 3 marked damage is lethal (704.5g) but indestructible saves it.
        after = S.settleSba (S.markDamage myrId 3 gs)
    Spec.assertEqWith s "Myr still on the battlefield" (S.creaturesInPlay S.bob after) 1
    Spec.assertEqWith s "Myr not in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
  Spec.it s "CR 704.5h an indestructible creature survives deathtouch" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myrId, gs) = S.addPermanent darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        -- Zero marked damage (so 704.5g is silent) plus a deathtouch event isolates
        -- the 704.5h path; indestructible must guard it too (CR 700.4).
        wounded = S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True False False 0 Nothing DamageKind.Combat)] gs
        after = S.settleSba wounded
    Spec.assertEqWith s "Myr survives deathtouch" (S.creaturesInPlay S.bob after) 1
  Spec.it s "CR 704.5f indestructible does NOT save a creature with toughness <= 0" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (myrId, gs) = S.addPermanent darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
        -- A real -1/-1 counter drops Myr (0/1) to 0/0 (CR 122.1a); 704.5f is a
        -- put-into-graveyard, not a destroy, so indestructible does not apply
        -- (Myr's own reminder text).
        zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 myrId gs
        after = S.settleSba zeroed
    Spec.assertEqWith s "Myr left the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Myr in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
  Spec.it s "CR 704.5f regeneration does NOT save a creature with toughness <= 0" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (victim, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers) -- 2/1
    -- A real -1/-1 counter drops the toughness to 0 (CR 122.1a); 704.5f is a
    -- put-into-graveyard, not a destruction, so a regeneration shield cannot
    -- save it.
        zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs
        shielded = S.addRegenShield victim zeroed
        after = S.settleSba shielded
    Spec.assertEqWith s "died despite the shield (704.5f is not a destruction)" (S.creaturesInPlay S.bob after) 0

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  fizzleSpec s registry
  indestructibleSpec s registry
  counterSpec s registry
  manaLeakSpec s registry
  rakshasasDisdainSpec s registry
  clashOfWillsSpec s registry
  stymiedHopesSpec s registry
  standstillSpec s registry
  dontMakeASoundSpec s registry
  whipstitchedZombieSpec s registry
  lithophageSpec s registry
  circlingVulturesSpec s registry
  merfolkSeerSpec s registry
  fortressKinGuardSpec s registry
  hakbalSpec s registry
  magicalHackTimingSpec s registry
  magicalHackDurationSpec s registry
  artificialEvolutionSpec s registry
  stifleSpec s registry
  squelchSpec s registry
  greenSlimeSpec s registry
