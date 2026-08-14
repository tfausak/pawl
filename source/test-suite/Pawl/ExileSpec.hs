-- Covers: CR 406.3's face-down exile -- Object.exiledFaceDown, the
-- EntryRiders.exiledFaceDown rider Pawl.Engine.Event.changeZoneEntering reads,
-- Pawl.Engine.Target.exileRecipients' CR 406.4 gate, and
-- ObjectRef.EachCardInYourHand as Pawl.Engine.Resolve sweeps it.
--
-- Gameplay-level, off one producer. Ignorant Bliss {1}{R} Instant -- "Exile all
-- cards from your hand face down. At the beginning of the next end step, return
-- those cards to your hand, then draw a card" -- is the pool's only card that
-- exiles anything face down, and Riftsweeper's "choose target FACE-UP exiled
-- card" is what reads the difference back out.
--
-- The two of them share one board, which is the point: exile holds the same
-- three cards either way, and only how two of them got there differs.
module Pawl.ExileSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Face-down exile" $ do
  Spec.describe s "Ignorant Bliss" $ do
    -- CR 406.3a and CR 406.4's first half, read through the pool that offers
    -- exiled cards as targets. The board is deliberately one board: alice's two
    -- hand cards go to exile face down by casting the card, and a THIRD card is
    -- already sitting there face up. A gate that had not been written would offer
    -- all three; a gate that emptied the pool outright would offer none.
    Spec.it s "CR 406.4 a card exiled face down is a legal target for nobody, while the face-up one still is" $ do
      board <- castBliss s registry
      riftsweeper <- S.printingOf s registry "Riftsweeper"
      case triggerTargetSlot riftsweeper of
        Just theSlot -> do
          Spec.assertEqWith s "all three cards are in the exile zone" (Set.size (GameState.exile board)) 3
          Spec.assertEqWith
            s
            "but only the face-up one may be chosen (CR 406.4)"
            (Target.legalRecipients (Just S.alice) S.noSource theSlot board)
            (Set.fromList (fmap Recipient.ToObject (faceUpExiled board)))
          Spec.assertEqWith s "and that is exactly one card" (length (faceUpExiled board)) 1
        Nothing -> Spec.assertFailure s "Riftsweeper should print one triggered ability with one target slot"
    -- The same gate from the other side, on a board differing in ONE thing: the
    -- two cards reach exile FACE UP instead, by the same route every other test
    -- puts a card there. Same printings, same seats, same zone, same count -- so a
    -- pool that had simply lost its exile candidates could not pass this.
    Spec.it s "CR 406.3 the same two cards exiled FACE UP are all legal targets" $ do
      riftsweeper <- S.printingOf s registry "Riftsweeper"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      sentry <- S.printingOf s registry "Ogre Sentry"
      let (upA, g1) = S.addExiledCard piker S.alice (Setup.emptyGame S.bothPlayers)
          (upB, g2) = S.addExiledCard bolt S.alice g1
          (upC, board) = S.addExiledCard sentry S.bob g2
      case triggerTargetSlot riftsweeper of
        Just theSlot ->
          Spec.assertEqWith
            s
            "CR 406.3's face-up default leaves every exiled card choosable"
            (Target.legalRecipients (Just S.alice) S.noSource theSlot board)
            (Set.fromList (fmap Recipient.ToObject [upA, upB, upC]))
        Nothing -> Spec.assertFailure s "Riftsweeper should print one triggered ability with one target slot"
    -- CR 400.7 through the delayed ability: "return THOSE CARDS to your hand, then
    -- draw a card". The slot the exiling move bound is what survives the
    -- resolution (CR 603.7c), and the cards come back as ordinary face-up cards in
    -- a hand -- which is Object.exiledFaceDown being per-incarnation state.
    Spec.it s "CR 603.7 at the next end step the exiled cards return to hand and alice draws" $ do
      board <- castBliss s registry
      let after = endStep board
      Spec.assertEqWith s "alice's hand was emptied by the spell" (S.handSize S.alice board) 0
      Spec.assertEqWith s "two cards returned, plus the draw" (S.handSize S.alice after) 3
      Spec.assertEqWith s "only the face-up card is left in exile" (Set.size (GameState.exile after)) 1
      -- A REGRESSION FENCE, not a proof: the returning move carries the default
      -- rider, so its arrival is face up by construction rather than by any
      -- guard. Dropping Event.changeZoneAttaching's destination gate leaves
      -- this green.
      Spec.assertEqWith s "and nothing in a hand is face down in exile" (concealedIn Zone.Hand S.alice after) []
      Spec.assertEqWith s "the delayed ability was spent" (length (GameState.delayedTriggers after)) 0

-- alice casts Ignorant Bliss off two Mountains with Goblin Piker and Lightning
-- Bolt in hand, and one Ogre Sentry already exiled face up under bob. Her
-- library is stocked so the delayed ability's draw has a card and CR 104.3c
-- never fires.
castBliss :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
castBliss s registry = do
  bliss <- S.printingOf s registry "Ignorant Bliss"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  bolt <- S.printingOf s registry "Lightning Bolt"
  sentry <- S.printingOf s registry "Ogre Sentry"
  let (g1, blissId) = S.handOne bliss (S.landsInPlay mountain 2)
      (_, g2) = S.addHandCard piker S.alice g1
      (_, g3) = S.addHandCard bolt S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      (_, g5) = S.addExiledCard sentry S.bob g4
  pure (S.runPure S.identityAnswer (S.runPure S.identityAnswer g5 (S.cast S.alice blissId)) Engine.priorityLoop)

-- The end step of alice's turn, settled and resolved -- Pawl.TriggerSpec's
-- delayed-ability group takes the same route.
endStep :: GameState.GameState -> GameState.GameState
endStep gs =
  let phase = Phase.Ending EndingStep.EndStep
      began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan phase S.alice)) (gs {GameState.phase = phase})
   in S.runPure S.identityAnswer (S.runPure S.identityAnswer began Engine.settleForPriority) Engine.priorityLoop

-- The exiled cards CR 406.3's default left face up.
faceUpExiled :: GameState.GameState -> [ObjectId.ObjectId]
faceUpExiled gs = filter (\oid -> not (any Object.exiledFaceDown (Game.lookupObject oid gs))) (Set.toList (GameState.exile gs))

-- The objects in a zone still carrying CR 406.3's face-down flag, which outside
-- exile must always be none.
concealedIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
concealedIn zone pid gs = filter (\oid -> any Object.exiledFaceDown (Game.lookupObject oid gs)) (Game.zoneMembers zone pid gs)

-- The one target slot of the one triggered ability a printing declares --
-- Pawl.TargetSpec's triggerTargetSlot, kept local so this group reads Riftsweeper
-- out of the committed card rather than a hand-built TargetSlot.
triggerTargetSlot :: Printing.Printing -> Maybe TargetSlot.TargetSlot
triggerTargetSlot printing = case Face.triggeredAbilities (S.combinedFace printing) of
  [ability] -> case Map.elems (Modal.allTargetSlots (TriggeredAbility.modal ability)) of
    [only] -> Just only
    _ -> Nothing
  _ -> Nothing
