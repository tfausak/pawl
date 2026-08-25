{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Resolve's Effect.MoveCounters arm -- CR 122.5's move of a
-- counter from one object onto a second, and the atomicity that makes it one
-- action rather than a removal written beside a placement.
module Pawl.MoveCounterSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- Agent's Toolkit {1}{G}{U} Artifact - Clue (New Capenna Commander; name, cost,
-- type line and oracle text checked against Scryfall 2026-08-25):
--
--   This artifact enters with a +1/+1 counter, a flying counter, a deathtouch
--   counter, and a shield counter on it.
--   Whenever a creature you control enters, you may move a counter from this
--   artifact onto that creature.
--   {2}, Sacrifice this artifact: Draw a card.
--
-- The middle line is this module's subject, and the card is why the opcode
-- exists: it names no kind, so the player chooses which counter moves, and it
-- names one object on each side, which is what CR 122.5's impossibilities are
-- stated about. Its entry line is Pawl.ReplacementSpec's (CR 614.1c / 614.5).
spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ moveCounterSpec s registry

-- Which counter the answerer takes, and whether it takes the printed "may" at
-- all. Pinned by POSITION in the offered list rather than by naming a kind, so
-- an answerer that searched for a legal option cannot silently repair a
-- mutation: `Lowest` is CR 122.1a's +1/+1 counter (the least CounterKind) and
-- `Highest` is CR 122.1c's shield counter (the greatest of the four the card
-- names).
data Pick = Lowest | Highest | Decline
  deriving (Eq, Show)

-- Counts every ChooseMovedCounter raised, so a case whose point is that NOTHING
-- was asked can say so. A pure @Prompt r -> r@ could not: several boards below
-- raise structurally identical prompts and differ only in how many.
toolkitAnswer :: Pick -> Prompt.Prompt r -> State.State Int r
toolkitAnswer pick p = case p of
  Prompt.ChooseOptional {} ->
    pure (case pick of Decline -> OptionalDecision.Declines; _ -> OptionalDecision.Exercises)
  Prompt.ChooseMovedCounter _ _ _ _ offered -> do
    State.modify' (+ 1)
    pure (case pick of Highest -> NonEmpty.last offered; _ -> NonEmpty.head offered)
  _ -> pure (S.identityAnswer p)

-- The four kinds Agent's Toolkit's entry line names, read off one object.
kindsOn :: ObjectId.ObjectId -> GameState.GameState -> (Natural, Natural, Natural, Natural)
kindsOn oid gs =
  ( S.counterOf CounterKind.PlusOnePlusOne oid gs,
    S.counterOf (CounterKind.Keyword Keyword.Deathtouch) oid gs,
    S.counterOf (CounterKind.Keyword Keyword.Flying) oid gs,
    S.counterOf CounterKind.Shield oid gs
  )

-- The newest battlefield object whose printed card has this name.
newestNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

toolkitName :: CardName.CardName
toolkitName = CardName.MkCardName (Text.pack "Agent's Toolkit")

pikerName :: CardName.CardName
pikerName = CardName.MkCardName (Text.pack "Goblin Piker")

moveCounterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
moveCounterSpec s registry = Spec.describe s "CR 122.5 moving a counter" $ do
  let -- alice: seven untapped lands, Agent's Toolkit and one other spell in
      -- hand. `extra` is what a case adds on top of that, and is the ONLY
      -- difference between the boards below.
      board other extra = do
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        mountain <- S.printingOf s registry "Mountain"
        plains <- S.printingOf s registry "Plains"
        toolkit <- S.printingOf s registry "Agent's Toolkit"
        second <- S.printingOf s registry other
        let base =
              S.landsFor forest S.alice 1
                . S.landsFor island S.alice 2
                . S.landsFor mountain S.alice 1
                $ S.landsInPlay plains 3
            (heldToolkit, g1) = S.addHandCard toolkit S.alice base
            (heldSecond, g2) = S.addHandCard second S.alice g1
        withExtra <- extra g2
        pure (heldToolkit, heldSecond, withExtra)
      -- Cast the artifact, then the other spell, then let the artifact's trigger
      -- reach the stack and resolve. settleForPriority is where CR 704's
      -- state-based actions run and the trigger is placed, in that order.
      play pick (heldToolkit, heldSecond, ready) =
        let run =
              Engine.runGame
                (toolkitAnswer pick)
                ready
                ( S.cast S.alice heldToolkit
                    >> Stack.resolveTop
                    >> S.cast S.alice heldSecond
                    >> Stack.resolveTop
                    >> Engine.settleForPriority
                    >> Stack.resolveTop
                )
            ((_, after), asked) = State.runState run 0
         in (asked, newestNamed toolkitName after, after)
      -- The artifact alone: its own entry is the creature entering, so no second
      -- spell is cast at all.
      playAlone pick (heldToolkit, _, ready) =
        let run =
              Engine.runGame
                (toolkitAnswer pick)
                ready
                (S.cast S.alice heldToolkit >> Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop)
            ((_, after), asked) = State.runState run 0
         in (asked, newestNamed toolkitName after, after)
  -- The control, and the card's second line end to end: the counter the player
  -- picked leaves the artifact and arrives on the creature that entered.
  Spec.it s "the chosen counter leaves the artifact and lands on the creature" $ do
    built <- board "Goblin Piker" pure
    case play Lowest built of
      (asked, Just toolkit, after) -> do
        Spec.assertEqWith s "the +1/+1 counter is gone from the artifact and the other three stayed" (kindsOn toolkit after) (0, 1, 1, 1)
        Spec.assertEqWith s "and it is on the creature that entered" (fmap (`kindsOn` after) (newestNamed pikerName after)) (Just (1, 0, 0, 0))
        Spec.assertEqWith s "the player was asked which counter, the four kinds being distinguishable" asked 1
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- The same board, differing ONLY in which offered counter the answer names. A
  -- move that ignored the answer would leave the case above's board.
  Spec.it s "the kind moved is the player's choice and not the engine's" $ do
    built <- board "Goblin Piker" pure
    case play Highest built of
      (_, Just toolkit, after) -> do
        Spec.assertEqWith s "the shield counter is the one gone from the artifact" (kindsOn toolkit after) (1, 1, 1, 0)
        Spec.assertEqWith s "and the shield counter is what the creature got" (fmap (`kindsOn` after) (newestNamed pikerName after)) (Just (0, 0, 0, 1))
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- CR 603.5's printed "may", declined: the engine asks rather than deciding.
  Spec.it s "CR 603.5 declining the may moves nothing" $ do
    built <- board "Goblin Piker" pure
    case play Decline built of
      (asked, Just toolkit, after) -> do
        Spec.assertEqWith s "all four counters stayed on the artifact" (kindsOn toolkit after) (1, 1, 1, 1)
        Spec.assertEqWith s "and the creature that entered got none" (fmap (`kindsOn` after) (newestNamed pikerName after)) (Just (0, 0, 0, 0))
        Spec.assertEqWith s "and a declined clause never reached the counter question" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- CR 122.5's SECOND impossibility -- "the first object doesn't have the
  -- appropriate kind of counter on it". bob's Vorinclex, Monstrous Raider halves
  -- the entry line's four single counters to none, so the artifact reaches the
  -- battlefield bearing nothing to move. Differs from the control in that one
  -- permanent.
  Spec.it s "an artifact with no counters on it moves nothing and asks nothing" $ do
    let withPraetor gs = do
          vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
          pure (snd (S.addCreature vorinclex S.bob gs))
    built <- board "Goblin Piker" withPraetor
    case play Lowest built of
      (asked, Just toolkit, after) -> do
        Spec.assertEqWith s "the praetor halved every kind away, so the artifact bears none" (kindsOn toolkit after) (0, 0, 0, 0)
        Spec.assertEqWith s "and nothing was put on the creature that entered" (fmap (`kindsOn` after) (newestNamed pikerName after)) (Just (0, 0, 0, 0))
        Spec.assertEqWith s "and with no kind to offer the player was not asked" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- CR 122.5's FOURTH impossibility, on the SECOND object -- "either object is no
  -- longer in the correct zone". Clone enters with no creature on the battlefield
  -- to copy, so it is a 0/0 and CR 704.5f buries it before the trigger it caused
  -- resolves. THE ASSERTION THIS UNIT EXISTS FOR: the artifact still has all four
  -- counters, where a removal written beside a placement would have taken one off
  -- and dropped it on the floor.
  Spec.it s "a creature that died before the trigger resolved leaves every counter where it was" $ do
    built <- board "Clone" pure
    case play Lowest built of
      (asked, Just toolkit, after) -> do
        Spec.assertEqWith s "no counter was removed from the artifact" (kindsOn toolkit after) (1, 1, 1, 1)
        Spec.assertEqWith s "the copy is not on the battlefield to have received one" (newestNamed (CardName.MkCardName (Text.pack "Clone")) after) Nothing
        Spec.assertEqWith s "and an impossible move is settled before the player is asked anything" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- CR 122.5's FOURTH impossibility again, this time on the FIRST object, and
  -- the card's third line ("{2}, Sacrifice this artifact: Draw a card") is what
  -- reaches it: the artifact is sacrificed in response to its own trigger, so the
  -- counters it held ceased to exist (CR 122.2) before the move was attempted.
  -- A removal written beside a placement would have found nothing to remove and
  -- put a counter on the creature anyway.
  Spec.it s "an artifact sacrificed in response to its own trigger moves nothing" $ do
    let stocked gs = do
          plains <- S.printingOf s registry "Plains"
          pure (snd (S.addLibraryCard plains S.alice (snd (S.addLibraryCard plains S.alice gs))))
    (heldToolkit, heldPiker, ready) <- board "Goblin Piker" stocked
    let ((_, mid), askedFirst) =
          State.runState
            ( Engine.runGame
                (toolkitAnswer Lowest)
                ready
                (S.cast S.alice heldToolkit >> Stack.resolveTop >> S.cast S.alice heldPiker >> Stack.resolveTop >> Engine.settleForPriority)
            )
            0
    case (newestNamed toolkitName mid, Projection.abilitiesOf `flip` mid) of
      (Just toolkit, abilitiesIn) -> case abilitiesIn toolkit of
        [] -> Spec.assertFailure s "Agent's Toolkit should offer its Clue ability"
        clue : _ -> do
          let ((_, after), asked) =
                State.runState
                  ( Engine.runGame
                      (toolkitAnswer Lowest)
                      mid
                      (Activate.activateAbility S.alice toolkit clue >> Stack.resolveTop >> Stack.resolveTop)
                  )
                  askedFirst
          Spec.assertEqWith s "the Clue ability drew a card" (length (Game.zoneMembers Zone.Hand S.alice after)) (length (Game.zoneMembers Zone.Hand S.alice mid) + 1)
          Spec.assertEqWith s "nothing was put on the creature the trigger named" (fmap (`kindsOn` after) (newestNamed pikerName after)) (Just (0, 0, 0, 0))
          Spec.assertEqWith s "the sacrificed artifact is off the battlefield" (newestNamed toolkitName after) Nothing
          Spec.assertEqWith s "and an impossible move is settled before the player is asked anything" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- CR 122.5's FIRST impossibility -- "the first and second objects are the same
  -- object". March of the Machines makes the artifact a creature, so its own
  -- entry is a creature entering under alice's control and its trigger names
  -- itself on both sides. Doubling Season is what makes the case OBSERVABLE: a
  -- move onto itself would take one counter off and put twice one back.
  Spec.it s "an artifact whose trigger names itself on both sides moves nothing" $ do
    let animated gs = do
          march <- S.printingOf s registry "March of the Machines"
          season <- S.printingOf s registry "Doubling Season"
          pure (snd (S.addCreature season S.alice (snd (S.addCreature march S.alice gs))))
    built <- board "Goblin Piker" animated
    case playAlone Lowest built of
      (asked, Just toolkit, after) -> do
        Spec.assertEqWith s "the doubled entry counters are untouched -- none removed and none added" (kindsOn toolkit after) (2, 2, 2, 2)
        Spec.assertEqWith s "and a move onto the object the counter is already on asks nothing" asked 0
      _ -> Spec.assertFailure s "the artifact did not reach the battlefield"
