-- Pawl.Engine.Reversal: CR 733.1's partition as a state, composed from where
-- the action began, where the mana window opened, and where it closed.
module Pawl.ReversalSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Reversal as Reversal
import qualified Pawl.Engine.Stack as Stack
import Pawl.ManaSourceSpec (plainOf)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Reversal" $ do
  withoutAnnouncementSpec s registry

-- A whole cast -- announcement, payment and resolution together -- standing in
-- for an announcement that writes a great deal. The merge owes a rule for every
-- field a real transition touches, and this one moves objects, both zones, the
-- stack, the log, the taps, the pool and the counters; a field it forgot shows
-- up here and in no hand-built triple.
transition :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, GameState.GameState)
transition s registry = do
  piker <- S.printingOf s registry "Goblin Piker"
  mountain <- S.printingOf s registry "Mountain"
  let (began, spell) = S.handOne piker (S.landsInPlay mountain 3)
      cast = S.runPure S.castAnswer began (S.cast S.alice spell)
  pure (began, S.runPure S.castAnswer cast Stack.resolveTop)

withoutAnnouncementSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
withoutAnnouncementSpec s registry = Spec.describe s "withoutAnnouncement" $ do
  -- The window's side alone. An announcement that wrote nothing leaves the state
  -- the window closed on untouched, which is what every caller that already
  -- honours CR 733.1 hands down today.
  Spec.it s "CR 733.1 an announcement that wrote nothing keeps the window whole" $ do
    (began, closed) <- transition s registry
    Spec.assertBool s (Reversal.withoutAnnouncement began began closed == Just closed) "the window's state comes back unchanged"

  -- The announcement's side alone, and the law that finds a field the merge
  -- mishandles: with nothing written after the window opened, everything the
  -- announcement wrote goes back. The counters do not -- CR 733.1 reverses a
  -- game action, not an id or a timestamp already handed out.
  Spec.it s "CR 733.1 a window that wrote nothing undoes the announcement" $ do
    (began, closed) <- transition s registry
    let expected =
          began
            { GameState.nextEventGroup = GameState.nextEventGroup closed,
              GameState.nextObjectId = GameState.nextObjectId closed,
              GameState.printings = GameState.printings closed,
              GameState.printingIds = GameState.printingIds closed,
              GameState.nextPrintingId = GameState.nextPrintingId closed,
              GameState.nextTimestamp = GameState.nextTimestamp closed,
              GameState.lastChoice = GameState.lastChoice closed
            }
    Spec.assertBool s (Reversal.withoutAnnouncement began closed closed == Just expected) "every field the cast wrote is back at where the action began, the monotone counters apart"
    Spec.assertNeWith s "the transition wrote something, so the law above is not vacuous" (Seq.length (GameState.events closed)) (Seq.length (GameState.events began))

  -- Both sides at once, which is the whole point: the announcement took the card
  -- out of the hand and put it on the stack, the window tapped a land for mana,
  -- and a payer who declines the reversal ends with the card back in hand AND
  -- the mana still floating.
  Spec.it s "CR 733.1 the announcement goes back and the window stands" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (began, spell) = S.handOne piker (S.landsInPlay mountain 3)
        red = plainOf (ManaType.Colored Color.Red)
    case Set.lookupMin (GameState.battlefield began) of
      Nothing -> Spec.assertFailure s "the board seated no land to tap"
      Just land -> do
        -- CR 601.2a: the card leaves the hand for the stack.
        let entry =
              began
                { GameState.hand = Map.adjust (Seq.filter (\oid -> oid /= spell)) S.alice (GameState.hand began),
                  GameState.stack = spell : GameState.stack began
                }
            -- CR 605.3a: one land tapped for one red mana.
            closed =
              Mana.setPool
                S.alice
                (Mana.Type.MkMana [red])
                entry
                  { GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) land (GameState.objects entry)
                  }
        case Reversal.withoutAnnouncement began entry closed of
          Nothing -> Spec.assertFailure s "the two sides share no leaf, so a kept state does exist"
          Just kept -> do
            Spec.assertEqWith s "the mana the window made is still floating" (Game.poolOf S.alice kept) (Mana.Type.MkMana [red])
            Spec.assertEqWith s "and the land it came from is still tapped" (fmap Object.tapped (Map.lookup land (GameState.objects kept))) (Just TapState.Tapped)
            Spec.assertEqWith s "the spell is off the stack" (GameState.stack kept) []
            Spec.assertEqWith s "and back in the hand it was announced from" (Map.lookup S.alice (GameState.hand kept)) (Map.lookup S.alice (GameState.hand began))

  -- CR 508.1g exerts an attacker and the toll's window taps that same creature
  -- for Springleaf Drum's cost: one permanent, two fields, one written by each
  -- side. A whole-Object leaf has no answer here.
  Spec.it s "CR 733.1 a permanent both sides wrote keeps the window's field and loses the announcement's" $ do
    (began, _) <- transition s registry
    case Set.lookupMin (GameState.battlefield began) of
      Nothing -> Spec.assertFailure s "the board seated no permanent"
      Just land -> do
        let entry = began {GameState.objects = Map.adjust (\o -> o {Object.exertedBy = Set.singleton S.alice}) land (GameState.objects began)}
            closed = entry {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) land (GameState.objects entry)}
            keptField :: (Object.Object -> a) -> Maybe (Maybe a)
            keptField field = fmap (fmap field . Map.lookup land . GameState.objects) (Reversal.withoutAnnouncement began entry closed)
        Spec.assertEqWith s "the window's tap stands" (keptField Object.tapped) (Just (Just TapState.Tapped))
        Spec.assertEqWith s "and the announcement's exert goes" (keptField Object.exertedBy) (Just (Just Set.empty))

  -- CR 733.1's last sentence on a library both sides touched: the announcement
  -- cast its second card (the stack exception), and the window shuffled. The
  -- window's order stands and the cast card goes back where it was.
  Spec.it s "CR 733.1 a shuffle stands where the announcement also moved a library card" $ do
    let before = [1, 2, 3, 4] :: [Int]
        entry = [1, 3, 4] :: [Int]
        closed = [4, 3, 1] :: [Int]
    Spec.assertEqWith s "the window's order, with the cast card back at its index" (Reversal.libraryOrder before entry closed) [4, 2, 3, 1]
    Spec.assertEqWith s "and a card the window milled stays milled" (Reversal.libraryOrder before before closed) closed

  -- The WHOLE reversal's library (Pawl.Engine.Cost.keepingLibraryActions): the
  -- cast card goes back at its index and the window's shuffle stands.
  Spec.it s "CR 733.1 reversing the whole action still keeps the window's shuffle" $ do
    let snapshot = [1, 2, 3, 4] :: [Int]
        since = [4, 3, 1] :: [Int]
    Spec.assertEqWith s "the window's order, with the cast card back at its index" (Reversal.restoredOrder snapshot since) [4, 2, 3, 1]
    Spec.assertEqWith s "and an unchanged membership is the window's order outright" (Reversal.restoredOrder snapshot [3, 1, 4, 2]) [3, 1, 4, 2]
