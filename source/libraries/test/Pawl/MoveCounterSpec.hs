{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Resolve's Effect.MoveCounters arm -- CR 122.5's move of
-- counters from one object onto a second, and the atomicity that makes it one
-- action rather than a removal written beside a placement.
module Pawl.MoveCounterSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
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
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  moveCounterSpec s registry
  namedKindSpec s registry
  batchSpec s registry
  everyKindSpec s registry
  anyNumberSpec s registry
  namedAnyNumberSpec s registry
  absentKindSpec s registry
  groupSourceSpec s registry
  upToOneSpec s registry

-- Which counter the answerer takes, and whether it takes the printed "may" at
-- all. Pinned by POSITION in the offered list rather than by naming a kind, so
-- an answerer that searched for a legal option cannot silently repair a
-- mutation: `Lowest` is CR 122.1a's +1/+1 counter (the least CounterKind) and
-- `Highest` is CR 122.1c's shield counter (the greatest of the four the card
-- names). takesiesAnswer at the foot of this module reads the same three, where
-- `Decline` is the printed "up to one" declined rather than a "may".
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

-- toolkitAnswer aiming Reality Ripple's one target at a named object, for the
-- phasing case below. Everything else is toolkitAnswer's, counter included, so
-- the two runs stay one answerer.
rippleAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
rippleAnswer victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> pure (S.preferring ((== Just victim) . Recipient.objectOf) sets)
  _ -> toolkitAnswer Lowest p

-- The newest battlefield object whose printed card has this name.
newestNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

-- The same read of alice's hand: a card `board` stocked through its `extra`,
-- which hands back a state and not the id it minted.
handNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
handNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in List.find named (Game.zoneMembers Zone.Hand S.alice gs)

toolkitName :: CardName.CardName
toolkitName = CardName.MkCardName (Text.pack "Agent's Toolkit")

pikerName :: CardName.CardName
pikerName = CardName.MkCardName (Text.pack "Goblin Piker")

rippleName :: CardName.CardName
rippleName = CardName.MkCardName (Text.pack "Reality Ripple")

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
        -- Exactly one, not the first of however many: the card prints one
        -- activated ability, and a second appearing must fail here rather than
        -- silently redirect this case at whichever sorted first.
        [clue] -> do
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
        other -> Spec.assertFailure s ("expected Agent's Toolkit to offer exactly its one Clue ability, got " <> show (length other))
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

  -- CR 122.5's FOURTH impossibility on the FIRST object again, reached WITHOUT a
  -- zone change, which is what makes it a different board from the sacrifice case
  -- above rather than a restatement of it. Reality Ripple ({1}{U} instant, "phase
  -- out target artifact, creature, or land") phases the artifact out in response
  -- to its own trigger. CR 702.26d says the phasing event causes no zone change
  -- and that counters remain on a permanent while it is phased out, so unlike the
  -- sacrificed artifact this source still bears all four kinds when the trigger
  -- resolves -- and CR 702.26b says it is treated as though it does not exist, so
  -- none of them may be taken. Pawl.Engine.Phasing spells rule 702.26b by moving
  -- the object out of GameState.battlefield, which is exactly what the arm's
  -- source-zone read consults.
  Spec.it s "CR 702.26b an artifact phased out in response to its own trigger moves nothing" $ do
    let withRipple gs = do
          ripple <- S.printingOf s registry "Reality Ripple"
          pure (snd (S.addHandCard ripple S.alice gs))
    (heldToolkit, heldPiker, ready) <- board "Goblin Piker" withRipple
    let ((_, mid), askedFirst) =
          State.runState
            ( Engine.runGame
                (toolkitAnswer Lowest)
                ready
                (S.cast S.alice heldToolkit >> Stack.resolveTop >> S.cast S.alice heldPiker >> Stack.resolveTop >> Engine.settleForPriority)
            )
            0
    case (newestNamed toolkitName mid, handNamed rippleName mid) of
      (Just toolkit, Just heldRipple) -> do
        let ((_, after), asked) =
              State.runState
                ( Engine.runGame
                    (rippleAnswer toolkit)
                    mid
                    (S.cast S.alice heldRipple >> Stack.resolveTop >> Stack.resolveTop)
                )
                askedFirst
        -- Without this the counter assertion below is vacuous: an artifact that
        -- never phased out reads four either way.
        Spec.assertEqWith s "CR 702.26b the artifact left GameState.battlefield, phased out" (Set.member toolkit (GameState.battlefield after), Phasing.isPhasedOut toolkit after) (False, True)
        -- THE ASSERTION THIS CASE EXISTS FOR.
        Spec.assertEqWith s "CR 702.26d its four counters rode along and none was removed" (kindsOn toolkit after) (1, 1, 1, 1)
        Spec.assertEqWith s "and the creature the trigger named received none" (fmap (`kindsOn` after) (newestNamed pikerName after)) (Just (0, 0, 0, 0))
        Spec.assertEqWith s "and an impossible move is settled before the player is asked anything" asked 0
      _ -> Spec.assertFailure s "expected the artifact on the battlefield and Reality Ripple in hand"

-- Explorer's Cache {1}{G} Artifact (The Lost Caverns of Ixalan; name, cost, type
-- line and oracle text checked against Scryfall 2026-08-25):
--
--   This artifact enters with two +1/+1 counters on it.
--   Whenever a creature you control with a +1/+1 counter on it dies, put a +1/+1
--   counter on this artifact.
--   {T}: Move a +1/+1 counter from this artifact onto target creature. Activate
--   only as a sorcery.
--
-- The third line is this group's subject: it NAMES the kind, where Agent's
-- Toolkit above leaves it to the player, so the two cards are CR 122.5's two
-- readings and this module holds both.
cacheName :: CardName.CardName
cacheName = CardName.MkCardName (Text.pack "Explorer's Cache")

-- Takes the LAST kind offered, which is what makes the cases below
-- discriminating: the artifact bears a +1/+1 counter and a shield counter, and CR
-- 122.1a's +1/+1 counter is the LEAST CounterKind, so an answerer taking the
-- first would move a +1/+1 counter whether the card named a kind or not. Counts
-- its calls for toolkitAnswer's reason, so a case whose point is that nothing was
-- asked can say so.
cacheAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
cacheAnswer wanted p = case p of
  Prompt.ChooseMovedCounter _ _ _ _ offered -> do
    State.modify' (+ 1)
    pure (NonEmpty.last offered)
  Prompt.ChooseTargets _ _ _ sets -> pure (S.preferring ((== Just wanted) . Recipient.objectOf) sets)
  _ -> pure (S.identityAnswer p)

-- The +1/+1 and shield tallies on one object -- the pair every case below reads,
-- and the two kinds the artifact bears when its ability resolves.
pairOn :: ObjectId.ObjectId -> GameState.GameState -> (Natural, Natural)
pairOn oid gs = (S.counterOf CounterKind.PlusOnePlusOne oid gs, S.counterOf CounterKind.Shield oid gs)

-- S.tapObject's inverse, and a fixture for the same reason: the third activation
-- below needs the artifact untapped again, and nothing in this group's board
-- untaps one. Touches the tap state and nothing else, so what the assertions read
-- -- counters -- is the engine's.
untapObject :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
untapObject oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) oid (GameState.objects gs)}

namedKindSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
namedKindSpec s registry = Spec.describe s "CR 122.5 moving a counter of a named kind" $ do
  let -- alice: six lands (a Mountain among them, so Lightning Bolt is castable),
      -- a Goblin Piker settled on the battlefield, Explorer's Cache and a
      -- Lightning Bolt in hand, and two Plains in the library so nothing decks
      -- her (CR 104.3c). `extra` is what a case adds on top of that.
      board extra = do
        forest <- S.printingOf s registry "Forest"
        mountain <- S.printingOf s registry "Mountain"
        plains <- S.printingOf s registry "Plains"
        cache <- S.printingOf s registry "Explorer's Cache"
        piker <- S.printingOf s registry "Goblin Piker"
        bolt <- S.printingOf s registry "Lightning Bolt"
        let lands = S.landsFor mountain S.alice 1 (S.landsFor forest S.alice 2 (S.landsInPlay plains 3))
            (target, g1) = S.addCreature piker S.alice lands
            (heldCache, g2) = S.addHandCard cache S.alice g1
            (heldBolt, g3) = S.addHandCard bolt S.alice g2
            (_, g4) = S.addLibraryCard plains S.alice (snd (S.addLibraryCard plains S.alice g3))
        withExtra <- extra g4
        pure (target, heldCache, heldBolt, withExtra)
      -- Cast the artifact, let it resolve, and hand back the object it became
      -- with a shield counter placed on it. THE SHIELD COUNTER IS THE POINT: with
      -- only +1/+1 counters on the artifact both readings of the opcode name the
      -- same kind and the case proves nothing.
      enter target heldCache ready =
        let ((_, mid), asked) =
              State.runState (Engine.runGame (cacheAnswer target) ready (S.cast S.alice heldCache >> Stack.resolveTop)) 0
         in fmap (\cache -> (cache, asked, S.addCounter CounterKind.Shield 1 cache mid)) (newestNamed cacheName mid)
      -- Activate the artifact's one printed ability and resolve it. Exactly one,
      -- not the first of however many, for the reason the Clue case above gives.
      activate target cache (asked, gs) = case Projection.abilitiesOf cache gs of
        [only] ->
          let ((_, after), asked') =
                State.runState
                  (Engine.runGame (cacheAnswer target) gs (Activate.activateAbility S.alice cache only >> Stack.resolveTop))
                  asked
           in Just (asked', after)
        _ -> Nothing
  -- THE CASE THIS UNIT EXISTS FOR. The artifact bears two kinds, the card names
  -- one, and the answerer above would take the other if it were ever asked.
  Spec.it s "the kind the card names is the one that moves, and nothing is asked" $ do
    (target, heldCache, _, ready) <- board pure
    case enter target heldCache ready of
      Just (cache, asked0, staged) -> do
        Spec.assertEqWith s "the artifact entered with two +1/+1 counters and was given a shield counter" (pairOn cache staged) (2, 1)
        case activate target cache (asked0, staged) of
          Just (asked, after) -> do
            -- THE GAMEPLAY-LEVEL ASSERTION: a +1/+1 counter, not the shield
            -- counter the answerer prefers, is what the creature received.
            Spec.assertEqWith s "the creature got a +1/+1 counter and no shield counter" (pairOn target after) (1, 0)
            Spec.assertEqWith s "and the artifact is down one +1/+1 counter with its shield counter untouched" (pairOn cache after) (1, 1)
            Spec.assertEqWith s "and a card that names the kind leaves nothing to ask" asked 0
          Nothing -> Spec.assertFailure s "expected Explorer's Cache to offer exactly its one printed ability"
      Nothing -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- CR 122.5's SECOND impossibility read against a NAMED kind -- "the first object
  -- doesn't have the appropriate kind of counter on it". The same board, activated
  -- until the two +1/+1 counters are spent: the shield counter is still there and
  -- is still not appropriate, so the third activation moves nothing rather than
  -- putting a counter on the creature that came off nothing.
  Spec.it s "an artifact left bearing only the wrong kind moves nothing" $ do
    (target, heldCache, _, ready) <- board pure
    case enter target heldCache ready of
      Just (cache, asked0, staged) ->
        case activate target cache (asked0, staged) >>= (activate target cache . fmap (untapObject cache)) of
          Just spent -> do
            Spec.assertEqWith s "two activations moved both +1/+1 counters, leaving only the shield counter" (pairOn cache (snd spent)) (0, 1)
            Spec.assertEqWith s "and the creature holds both of them" (pairOn target (snd spent)) (2, 0)
            case activate target cache (fmap (untapObject cache) spent) of
              Just (asked, after) -> do
                Spec.assertEqWith s "the third activation put nothing on the creature" (pairOn target after) (2, 0)
                Spec.assertEqWith s "and took nothing off the artifact, the shield counter included" (pairOn cache after) (0, 1)
                Spec.assertEqWith s "and an impossible move is settled before the player is asked anything" asked 0
              Nothing -> Spec.assertFailure s "expected Explorer's Cache to offer exactly its one printed ability"
          Nothing -> Spec.assertFailure s "expected Explorer's Cache to offer exactly its one printed ability"
      Nothing -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- The card's SECOND line, and the pair of boards that shows its filter reads the
  -- counter and not merely the creature: one Lightning Bolt, aimed from the same
  -- staged board at a Goblin Piker bearing a +1/+1 counter and at one bearing
  -- none.
  Spec.it s "the dies trigger reads the counter on the creature that died" $ do
    let secondPiker gs = do
          piker <- S.printingOf s registry "Goblin Piker"
          pure (snd (S.addCreature piker S.alice gs))
    (bare, heldCache, heldBolt, ready) <- board secondPiker
    case enter bare heldCache ready of
      Just (cache, asked0, staged) ->
        case filter (/= bare) (filter (\o -> fmap Face.name (Game.faceOf o staged) == Just pikerName) (Set.toList (GameState.battlefield staged))) of
          [countered] -> do
            let bolted victim =
                  snd
                    ( fst
                        ( State.runState
                            ( Engine.runGame
                                (cacheAnswer victim)
                                (S.addCounter CounterKind.PlusOnePlusOne 1 countered staged)
                                (S.cast S.alice heldBolt >> Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop)
                            )
                            asked0
                        )
                    )
            Spec.assertEqWith s "the piker bearing a +1/+1 counter died and grew the artifact" (pairOn cache (bolted countered), Set.member countered (GameState.battlefield (bolted countered))) ((3, 1), False)
            Spec.assertEqWith s "the piker bearing none died and did not" (pairOn cache (bolted bare), Set.member bare (GameState.battlefield (bolted bare))) ((2, 1), False)
          _ -> Spec.assertFailure s "expected exactly two Goblin Pikers on the battlefield"
      Nothing -> Spec.assertFailure s "the artifact did not reach the battlefield"

-- Black Panther, Wakandan King {G}{W} 2/2 Legendary Creature - Human Noble Hero
-- (Marvel's Spider-Man; name, cost, type line and oracle text checked against
-- Scryfall 2026-08-27), data/cards/black-panther-wakandan-king.json:
--
--   First strike
--   Survey the Realm - Whenever Black Panther or another creature you control
--   enters, put a +1/+1 counter on target land you control.
--   Mine Vibranium - {3}: Move all +1/+1 counters from target land you control
--   onto target creature. If one or more +1/+1 counters are moved this way, you
--   gain that much life and draw a card.
--
-- The third line is this group's subject, and the card is why the count exists:
-- "all +1/+1 counters" is a whole tally crossing in one batch, where both cards
-- above move exactly one. Its two target slots have DISJOINT pools -- a land you
-- control and a creature -- so it needs no distinctness between them; the group
-- below is the card that does, and Pawl.TargetSpec's Fall of the Hammer case is
-- where a slot excluding a sibling's object is proven.
pantherName :: CardName.CardName
pantherName = CardName.MkCardName (Text.pack "Black Panther, Wakandan King")

mountainName :: CardName.CardName
mountainName = CardName.MkCardName (Text.pack "Mountain")

-- Aims both target slots at once. The two pools are disjoint, so one predicate
-- over both is exact rather than a search that could repair a mutation: the land
-- slot offers only lands and the creature slot only creatures, and the Mountain
-- and the Piker are one of each. FILTERS the offered set rather than building a
-- recipient, so CR 608.2b's re-read at resolution still finds what was named.
pantherAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
pantherAnswer land creature p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (maybe False (`elem` [land, creature]) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

batchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
batchSpec s registry = Spec.describe s "CR 122.5 moving a whole tally of counters" $ do
  let -- alice: four Forests (the {3} has to come from somewhere), one Mountain,
      -- Black Panther and a Goblin Piker settled on the battlefield, and two
      -- Plains in her library so the card the rider draws is there and nothing
      -- decks her (CR 104.3c). `counters` is what a case puts on the Mountain and
      -- `extra` seats a further printing by name; between them they are the ONLY
      -- difference between the three boards below.
      board extra counters = do
        forest <- S.printingOf s registry "Forest"
        mountain <- S.printingOf s registry "Mountain"
        plains <- S.printingOf s registry "Plains"
        panther <- S.printingOf s registry "Black Panther, Wakandan King"
        piker <- S.printingOf s registry "Goblin Piker"
        extras <- mapM (\(name, pid) -> fmap (\p -> (p, pid)) (S.printingOf s registry name)) extra
        let lands = S.landsFor mountain S.alice 1 (S.landsInPlay forest 4)
            (pantherId, g1) = S.addCreature panther S.alice lands
            (pikerId, g2) = S.addCreature piker S.alice g1
            (_, g3) = S.addLibraryCard plains S.alice (snd (S.addLibraryCard plains S.alice g2))
            seated = foldl (\gs (p, pid) -> snd (S.addCreature p pid gs)) g3 extras
            ready = seated {GameState.priority = Just S.alice}
        pure $ do
          landId <- newestNamed mountainName ready
          -- The SHIELD counter is why the land bears two kinds: the card names
          -- +1/+1, so a batch that took everything on the object would be
          -- indistinguishable from one that took the named kind on a land bearing
          -- only that kind.
          let staged = S.addCounter CounterKind.Shield 1 landId (counters landId ready)
          pure (pantherId, pikerId, landId, staged)
      -- Mine Vibranium, activated once and resolved. Exactly one printed
      -- activated ability, not the first of however many.
      mine (pantherId, pikerId, landId, staged) = case Activate.abilitiesFor pantherId staged of
        [only] -> Just (S.runPure (pantherAnswer landId pikerId) staged (Activate.activateAbility S.alice pantherId only >> Stack.resolveTop))
        _ -> Nothing
  -- THE CASE THIS UNIT EXISTS FOR. THREE +1/+1 counters, so a move of one and a
  -- move of the tally are three different boards apart, and both ends are read:
  -- a fix that put three without taking three leaves the land at three.
  Spec.it s "all three +1/+1 counters cross in one batch, and the rider counts them" $ do
    built <- board [] (S.addCounter CounterKind.PlusOnePlusOne 3)
    case built of
      Just staged@(_, pikerId, landId, before) -> do
        Spec.assertEqWith s "the land bears three +1/+1 counters and a shield counter" (pairOn landId before) (3, 1)
        Spec.assertEqWith s "and the creature bears none of either" (pairOn pikerId before) (0, 0)
        case mine staged of
          Just after -> do
            -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of the rider's: the whole
            -- tally of the named kind crossed, and only that kind.
            Spec.assertEqWith s "all three +1/+1 counters are on the creature and the shield counter is not" (pairOn pikerId after) (3, 0)
            Spec.assertEqWith s "and the land is down all three, its shield counter untouched" (pairOn landId after) (0, 1)
            Spec.assertEqWith s "alice gained one life for each counter moved this way" (S.lifeOf S.alice after) (fmap (+ 3) (S.lifeOf S.alice before))
            Spec.assertEqWith s "and drew the one card the rider names" (S.handSize S.alice after) (S.handSize S.alice before + 1)
          Nothing -> Spec.assertFailure s "expected Black Panther to offer exactly its one printed activated ability"
      Nothing -> Spec.assertFailure s "the Mountain did not reach the battlefield"
  -- The same board differing in exactly one thing: the land bears no +1/+1
  -- counter, only the shield counter. CR 122.5's second impossibility, and the
  -- rider's "if one or more" read against the zero the move binds -- an
  -- unbound slot would leave the gate unevaluable rather than false.
  Spec.it s "a land bearing no counter of the named kind moves nothing and pays no rider" $ do
    built <- board [] (const id)
    case built of
      Just staged@(_, pikerId, landId, before) -> do
        Spec.assertEqWith s "the land bears the shield counter and no +1/+1 counter" (pairOn landId before) (0, 1)
        case mine staged of
          Just after -> do
            Spec.assertEqWith s "the creature received nothing" (pairOn pikerId after) (0, 0)
            Spec.assertEqWith s "and the land kept its shield counter" (pairOn landId after) (0, 1)
            Spec.assertEqWith s "alice gained no life" (S.lifeOf S.alice after) (S.lifeOf S.alice before)
            Spec.assertEqWith s "and drew nothing" (S.handSize S.alice after) (S.handSize S.alice before)
          Nothing -> Spec.assertFailure s "expected Black Panther to offer exactly its one printed activated ability"
      Nothing -> Spec.assertFailure s "the Mountain did not reach the battlefield"
  -- CR 614.16 read at gameplay level, and the case that makes the BATCH visible:
  -- the rule replaces a placement of "one or more counters" once, so bob's
  -- Vorinclex sees one placement of three and halves it to one. Three placements
  -- of one each would each halve to nothing and the creature would end at zero, so
  -- the two readings are two different boards -- which they are not on any board
  -- without a counter-scaling row. The removal is not replaced (no CR 614 class
  -- pairs with one), so the land still loses all three, and "moved this way" is
  -- what completed the journey rather than what came off.
  Spec.it s "CR 614.16 an opponent's Vorinclex halves the whole batch once, not each counter" $ do
    built <- board [("Vorinclex, Monstrous Raider", S.bob)] (S.addCounter CounterKind.PlusOnePlusOne 3)
    case built of
      Just staged@(_, pikerId, landId, before) -> do
        Spec.assertEqWith s "the land bears three +1/+1 counters and a shield counter" (pairOn landId before) (3, 1)
        case mine staged of
          Just after -> do
            Spec.assertEqWith s "half of three, rounded down, landed on the creature" (pairOn pikerId after) (1, 0)
            Spec.assertEqWith s "and the land lost all three, the removal being unreplaceable" (pairOn landId after) (0, 1)
            Spec.assertEqWith s "alice gained one life, the counters that completed the move" (S.lifeOf S.alice after) (fmap (+ 1) (S.lifeOf S.alice before))
            Spec.assertEqWith s "and drew the one card the rider names" (S.handSize S.alice after) (S.handSize S.alice before + 1)
          Nothing -> Spec.assertFailure s "expected Black Panther to offer exactly its one printed activated ability"
      Nothing -> Spec.assertFailure s "the Mountain did not reach the battlefield"

-- Fate Transfer {1}{U/B} Instant (Shadowmoor; name, cost, type line and oracle
-- text checked against Scryfall 2026-08-29), data/cards/fate-transfer.json:
--
--   Move all counters from target creature onto another target creature.
--
-- This group's subject, and the card is why "every kind" exists: the sentence
-- names no kind and asks NOTHING, where Agent's Toolkit names none and asks --
-- so it is not that card's count raised, it is the kind question deleted. Its
-- "another target creature" is Filter.Not (Filter.IsBound "from"), the shape
-- Pawl.TargetSpec proves on Fall of the Hammer.
--
-- Both permanents are Wall of Stone, whose 0/8 body survives a -1/-1 counter on
-- either side of the move; a 2/1 would be buried by CR 704.5f before the
-- assertion ran.
transferFrom, transferTo :: SlotName.SlotName
transferFrom = SlotName.MkSlotName (Text.pack "from")
transferTo = SlotName.MkSlotName (Text.pack "to")

-- CR 601.2c's whole announcement for Fate Transfer: `giver` in the `from` slot
-- and `taker` in the `to` slot. Both pools are Pool.Creatures, so one predicate
-- over both slots could not tell them apart and the slot NAME is what settles
-- which. FILTERS the offered set rather than building a recipient, so CR 608.2b's
-- re-read at resolution still finds what was named -- Pawl.TargetSpec's
-- aimingHammer posture.
aimingTransfer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingTransfer giver taker p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == transferFrom then giver else taker
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

-- The three kinds these boards use, read off one object: CR 122.1a's +1/+1 and
-- -1/-1 counters and CR 122.1c's shield counter. Three, not pairOn's two,
-- because a move of every kind and a move of one kind are only different boards
-- where the object bears more than one kind.
tripleOn :: ObjectId.ObjectId -> GameState.GameState -> (Natural, Natural, Natural)
tripleOn oid gs =
  ( S.counterOf CounterKind.PlusOnePlusOne oid gs,
    S.counterOf CounterKind.Shield oid gs,
    S.counterOf CounterKind.MinusOneMinusOne oid gs
  )

everyKindSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
everyKindSpec s registry = Spec.describe s "CR 122.5 moving every kind of counter" $ do
  let -- alice: two Islands and a Swamp, so either half of CR 107.4e's {U/B} is
      -- payable off the board and the answer to the hybrid announcement cannot
      -- decide a case; Fate Transfer in her hand; her own Wall of Stone as the
      -- second object and bob's as the first, which keeps the two slots apart by
      -- controller as well as by identity. `counters` is what a case puts on the
      -- first object and `extras` seats a further printing by name; between them
      -- they are the ONLY difference between the boards below.
      board extras counters = do
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        wall <- S.printingOf s registry "Wall of Stone"
        fateTransfer <- S.printingOf s registry "Fate Transfer"
        seats <- mapM (\(name, pid) -> fmap (\p -> (p, pid)) (S.printingOf s registry name)) extras
        let lands = S.landsFor swamp S.alice 1 (S.landsInPlay island 2)
            (giverId, g1) = S.addCreature wall S.bob lands
            (takerId, g2) = S.addCreature wall S.alice g1
            seated = foldl (\gs (p, pid) -> snd (S.addCreature p pid gs)) g2 seats
            (staged, spellId) = S.handOne fateTransfer seated
        pure (giverId, takerId, spellId, counters giverId staged)
      -- Cast and resolve, the whole spell.
      transfer (giverId, takerId, spellId, staged) =
        S.runPure (aimingTransfer giverId takerId) staged (S.cast S.alice spellId >> Stack.resolveTop)
  -- THE CASE THIS UNIT EXISTS FOR. TWO kinds in different counts, so "the whole
  -- tally of every kind" is a different board from "the whole tally of one kind"
  -- and from "one counter", and both ends are read: a fix that put without taking
  -- leaves the first object where it was.
  Spec.it s "every kind on the first creature crosses at once, whole tally and all" $ do
    built <- board [] (\oid -> S.addCounter CounterKind.Shield 2 oid . S.addCounter CounterKind.PlusOnePlusOne 3 oid)
    let (giverId, takerId, _, before) = built
    Spec.assertEqWith s "bob's wall bears three +1/+1 counters and two shield counters" (tripleOn giverId before) (3, 2, 0)
    Spec.assertEqWith s "and alice's bears none of any kind" (tripleOn takerId before) (0, 0, 0)
    let after = transfer built
    -- THE GAMEPLAY-LEVEL ASSERTIONS: both kinds arrived, in the counts they
    -- left in, and neither stayed behind.
    Spec.assertEqWith s "alice's wall has both kinds, three of one and two of the other" (tripleOn takerId after) (3, 2, 0)
    Spec.assertEqWith s "and bob's is down every counter it had" (tripleOn giverId after) (0, 0, 0)
  -- CR 122.5's THIRD impossibility read against "all counters" -- "the second
  -- object can't have counters put onto it" -- which is asked PER KIND: alice's
  -- Melira, Sylvok Outcast refuses -1/-1 counters on her creatures and says
  -- nothing about shield counters, so one kind crosses and the other does not.
  -- A whole-sentence reading of the rule's atomicity would move nothing at all.
  Spec.it s "CR 122.5 a kind the second creature refuses stays behind, and the rest still crosses" $ do
    let stock oid = S.addCounter CounterKind.MinusOneMinusOne 1 oid . S.addCounter CounterKind.Shield 2 oid
    built <- board [("Melira, Sylvok Outcast", S.alice)] stock
    let (giverId, takerId, _, before) = built
    Spec.assertEqWith s "bob's wall bears two shield counters and one -1/-1 counter" (tripleOn giverId before) (0, 2, 1)
    let after = transfer built
    Spec.assertEqWith s "the shield counters crossed and the -1/-1 counter did not" (tripleOn takerId after) (0, 2, 0)
    Spec.assertEqWith s "and bob's wall kept the one counter alice's creature could not take" (tripleOn giverId after) (0, 0, 1)
  -- The same board differing in exactly ONE permanent: without Melira there is no
  -- prohibition, and the -1/-1 counter crosses with the rest. Without this pair
  -- the case above would pass on a move that dropped every -1/-1 counter for
  -- reasons of its own.
  Spec.it s "and with no prohibition on the board the same -1/-1 counter crosses" $ do
    let stock oid = S.addCounter CounterKind.MinusOneMinusOne 1 oid . S.addCounter CounterKind.Shield 2 oid
    built <- board [] stock
    let (giverId, takerId, _, before) = built
    Spec.assertEqWith s "bob's wall bears two shield counters and one -1/-1 counter" (tripleOn giverId before) (0, 2, 1)
    let after = transfer built
    Spec.assertEqWith s "every kind crossed, the -1/-1 counter included" (tripleOn takerId after) (0, 2, 1)
    Spec.assertEqWith s "and bob's wall is left with nothing" (tripleOn giverId after) (0, 0, 0)

-- Resourceful Defense {2}{W} Enchantment (Edge of Eternities Commander; name,
-- cost, type line and oracle text checked against Scryfall 2026-08-30),
-- data/cards/resourceful-defense.json:
--
--   Whenever a permanent you control leaves the battlefield, if it had counters
--   on it, put those counters on target permanent you control.
--   {4}{W}: Move any number of counters from target permanent you control onto
--   a second target permanent you control.
--
-- The second line is this group's subject, and the card is why "any number"
-- exists: it names neither the kind nor the count, so ONE answer settles both
-- and may take one counter of each of two kinds -- where Agent's Toolkit's
-- printed count of one comes out of the one kind the player picks. NO printing
-- states a fixed count above one without naming a kind. The sweep behind that is
-- recorded ONCE, on Pawl.Types.MovedKinds, with the Scryfall query, the
-- include_extras parameter without which it misses two printings, and its date.
-- The per-arm lists are not restated here: two copies is how one goes stale.
--
-- pawl's Resourceful Defense omits the triggered ability entirely. CR 122.8's
-- placement itself is writable -- Effect.PutCountersFrom, which Iron Apprentice
-- proves in Pawl.PutCounterSpec -- but that opcode reads its tally off a SLOT,
-- and no reserved slot names the departing BYSTANDER this card's condition
-- watches (#2694). The omission is stricter than printed -- a departing
-- permanent's counters simply cease -- and never weaker in its controller's
-- favour.
--
-- Both target pools are Pool.Permanents and both slots accept every permanent
-- alice controls, so one predicate over both could not tell them apart and the
-- slot NAME is what settles which -- Fate Transfer's aimingTransfer posture, and
-- FILTERING the offered set rather than building a recipient so CR 608.2b's
-- re-read at resolution still finds what was named.
--
-- `wanted` is answered VERBATIM rather than derived from what is offered, so an
-- answerer cannot silently repair a mutation by re-deriving a legal answer, and
-- the count of prompts raised is threaded so a case whose point is that nothing
-- was asked can say so.
defenseFrom, defenseTo :: SlotName.SlotName
defenseFrom = SlotName.MkSlotName (Text.pack "from")
defenseTo = SlotName.MkSlotName (Text.pack "to")

defenseAnswer ::
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural ->
  Prompt.Prompt r ->
  State.State Int r
defenseAnswer giver taker wanted p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    pure
      ( Map.mapWithKey
          ( \slot (_, offered) ->
              let target
                    | slot == defenseFrom = Just giver
                    | slot == defenseTo = Just taker
                    | otherwise = Nothing
               in Set.filter ((==) target . Recipient.objectOf) offered
          )
          asked
      )
  Prompt.ChooseMovedCounters {} -> do
    State.modify' (+ 1)
    pure wanted
  _ -> pure (S.identityAnswer p)

anyNumberSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
anyNumberSpec s registry = Spec.describe s "CR 122.5 moving any number of counters" $ do
  let -- alice: five untapped Plains (the {4}{W} has to come from somewhere),
      -- Resourceful Defense, and two Goblin Pikers -- one to move counters off
      -- and one to move them onto. Every one of those is a permanent she
      -- controls, so both target slots are offered far more candidates than they
      -- need. `counters` is what a case puts on the first Piker, and is the ONLY
      -- difference between the boards below.
      board counters = do
        plains <- S.printingOf s registry "Plains"
        defense <- S.printingOf s registry "Resourceful Defense"
        piker <- S.printingOf s registry "Goblin Piker"
        let (defenseId, g1) = S.addCreature defense S.alice (S.landsInPlay plains 5)
            (giverId, g2) = S.addCreature piker S.alice g1
            (takerId, g3) = S.addCreature piker S.alice g2
            ready = (counters giverId g3) {GameState.priority = Just S.alice}
        pure (defenseId, giverId, takerId, ready)
      -- The ability, activated once and resolved, answering the counter question
      -- with `wanted`. Exactly one printed activated ability, not the first of
      -- however many.
      spend wanted (defenseId, giverId, takerId, ready) = case Activate.abilitiesFor defenseId ready of
        [only] ->
          let run =
                Engine.runGame
                  (defenseAnswer giverId takerId wanted)
                  ready
                  (Activate.activateAbility S.alice defenseId only >> Stack.resolveTop)
              ((_, after), asked) = State.runState run 0
           in Just (asked, after)
        _ -> Nothing
      stocked oid = S.addCounter CounterKind.Shield 2 oid . S.addCounter CounterKind.PlusOnePlusOne 3 oid
  -- THE CASE THIS UNIT EXISTS FOR. The first Piker bears TWO kinds, and the
  -- answer takes one counter of each -- fewer than either pile holds, so "one
  -- counter out of each of two kinds" is a different board from any count taken
  -- out of one kind, which is what a move settling on a single kind would have
  -- produced.
  Spec.it s "one counter of each of two kinds crosses on one answer" $ do
    built <- board stocked
    let (_, giverId, takerId, before) = built
    Spec.assertEqWith s "the first creature bears three +1/+1 counters and two shield counters" (tripleOn giverId before) (3, 2, 0)
    Spec.assertEqWith s "and the second bears none of any kind" (tripleOn takerId before) (0, 0, 0)
    case spend (Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Shield, 1)]) built of
      Just (asked, after) -> do
        -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of the prompt count: one of each
        -- kind arrived, and the rest of both piles stayed where it was.
        Spec.assertEqWith s "the second creature has one counter of each kind" (tripleOn takerId after) (1, 1, 0)
        Spec.assertEqWith s "and the first kept two +1/+1 counters and one shield counter" (tripleOn giverId after) (2, 1, 0)
        Spec.assertEqWith s "the player was asked which counters, both kinds being distinguishable" asked 1
      Nothing -> Spec.assertFailure s "expected Resourceful Defense to offer exactly its one printed activated ability"
  -- The same board differing in exactly ONE thing, the answer: two counters of a
  -- single kind. Without this pair the case above would pass on a move that
  -- always took one of everything offered, and it is also the shape Agent's
  -- Toolkit's Chosen arm can already write -- so "any number" widens that arm
  -- rather than replacing it.
  Spec.it s "and an answer naming one kind twice takes both out of that kind" $ do
    built <- board stocked
    let (_, giverId, takerId, _) = built
    case spend (Map.singleton CounterKind.PlusOnePlusOne 2) built of
      Just (_, after) -> do
        Spec.assertEqWith s "the second creature has two +1/+1 counters and no shield counter" (tripleOn takerId after) (2, 0, 0)
        Spec.assertEqWith s "and the first kept one +1/+1 counter and both shield counters" (tripleOn giverId after) (1, 2, 0)
      Nothing -> Spec.assertFailure s "expected Resourceful Defense to offer exactly its one printed activated ability"
  -- CR 609.3 -- "it does only as much as possible" -- and rule 122.5's second
  -- impossibility, both against an answer no card could have written: nine +1/+1
  -- counters off a creature bearing three, and four -1/-1 counters off one
  -- bearing none. This is the only board on which the count asked for and the
  -- tally present can differ at all, since a card's own count is read off the
  -- very object the removal reads.
  Spec.it s "an answer asking for more counters than the permanent has moves only what is there" $ do
    built <- board stocked
    let (_, giverId, takerId, _) = built
    case spend (Map.fromList [(CounterKind.PlusOnePlusOne, 9), (CounterKind.MinusOneMinusOne, 4)]) built of
      Just (_, after) -> do
        Spec.assertEqWith s "three +1/+1 counters crossed and no more, and no -1/-1 counter appeared" (tripleOn takerId after) (3, 0, 0)
        Spec.assertEqWith s "and the first creature is down all three, its shield counters untouched" (tripleOn giverId after) (0, 2, 0)
      Nothing -> Spec.assertFailure s "expected Resourceful Defense to offer exactly its one printed activated ability"
  -- "Any number" includes NONE, so a lone kind bearing a lone counter is still a
  -- real choice and the prompt IS raised -- unlike Agent's Toolkit's Chosen arm
  -- above, which elides it at one candidate because the card's own count then
  -- settles everything. The two runs differ in exactly one thing, the answer, so
  -- an engine that moved the counter without asking would fail the second.
  Spec.it s "a single counter of a single kind is still asked about, and may be left where it is" $ do
    built <- board (S.addCounter CounterKind.PlusOnePlusOne 1)
    let (_, giverId, takerId, _) = built
    case (spend (Map.singleton CounterKind.PlusOnePlusOne 1) built, spend Map.empty built) of
      (Just (askedMoving, moving), Just (askedLeaving, leaving)) -> do
        Spec.assertEqWith s "the answer that names the counter moves it" (tripleOn takerId moving, tripleOn giverId moving) ((1, 0, 0), (0, 0, 0))
        Spec.assertEqWith s "and the answer that names nothing leaves it where it was" (tripleOn takerId leaving, tripleOn giverId leaving) ((0, 0, 0), (1, 0, 0))
        Spec.assertEqWith s "both runs asked, a lone counter being a choice between moving it and not" (askedMoving, askedLeaving) (1, 1)
      _ -> Spec.assertFailure s "expected Resourceful Defense to offer exactly its one printed activated ability"
  -- Rule 122.5's second impossibility with nothing to ask about: a first object
  -- bearing no counter at all has no candidate kind, so the move is settled
  -- before the player is asked anything.
  Spec.it s "a permanent bearing no counter moves nothing and asks nothing" $ do
    built <- board (const id)
    let (_, giverId, takerId, _) = built
    case spend (Map.singleton CounterKind.PlusOnePlusOne 1) built of
      Just (asked, after) -> do
        Spec.assertEqWith s "the second creature received nothing" (tripleOn takerId after) (0, 0, 0)
        Spec.assertEqWith s "and the first still bears nothing" (tripleOn giverId after) (0, 0, 0)
        Spec.assertEqWith s "and with no kind to offer the player was not asked" asked 0
      Nothing -> Spec.assertFailure s "expected Resourceful Defense to offer exactly its one printed activated ability"

-- Scrounging Bandar {1}{G} Creature - Cat Monkey 0/0 (Commander Legends; name,
-- cost, type line, power, toughness and oracle text checked against Scryfall
-- 2026-08-30), data/cards/scrounging-bandar.json:
--
--   This creature enters with two +1/+1 counters on it.
--   At the beginning of your upkeep, you may move any number of +1/+1 counters
--   from this creature onto another target creature.
--
-- The second line is this group's subject, and the card is why "any number of a
-- named kind" exists: the card settles the KIND and leaves the COUNT open, so the
-- prompt is raised over the one kind the card named -- where Resourceful
-- Defense's "any number of counters" offers every kind the first creature bears,
-- which here would let the answerer move a shield counter Scrounging Bandar never
-- mentions. Its "another target creature" is Filter.Not Filter.IsSource, Joraga
-- Auxiliary's shape, so no clause of the printed sentence is omitted.
--
-- Bioshift prints the same spelling on an instant and is cheaper to drive, but
-- its "with the same controller" is a demand on a sibling OBJECT slot that no
-- filter can make, and dropping it would be weaker than printed (#2722).
--
-- The counters go on by hand rather than through the printed entry rider, whose
-- own road is Pawl.ReplacementSpec's: these boards need a tally the printed two
-- cannot give -- three of the named kind beside two of another -- so that "any
-- number of +1/+1 counters" is a different board from "all the +1/+1 counters"
-- and from "any number of counters" at once. The Bandar is a printed 0/0 and its
-- own +1/+1 counters are what keep it off CR 704.5f, so every board below leaves
-- it at least one.
bandarAnswer ::
  ObjectId.ObjectId ->
  Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural ->
  Prompt.Prompt r ->
  State.State Int r
bandarAnswer taker wanted p = case p of
  -- The printed "you may", taken every time: a declined trigger would move
  -- nothing for a reason no case here is about.
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  -- CR 603.3d's one target slot, FILTERED out of what the engine offered rather
  -- than built by hand, so CR 608.2b's re-read at resolution finds what was named
  -- -- aimingTransfer's posture.
  Prompt.ChooseTargets _ _ _ asked ->
    pure (Map.map (Set.filter ((==) (Just taker) . Recipient.objectOf) . snd) asked)
  -- Answered VERBATIM rather than derived from what is offered, so an answerer
  -- cannot repair a mutation by re-deriving a legal answer, and COUNTED, because
  -- one case below asserts that nothing was asked.
  Prompt.ChooseMovedCounters {} -> do
    State.modify' (+ 1)
    pure wanted
  _ -> pure (S.identityAnswer p)

namedAnyNumberSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
namedAnyNumberSpec s registry = Spec.describe s "CR 122.5 moving any number of counters of the kind the card names" $ do
  let -- alice: a Forest, her Scrounging Bandar and a Wall of Stone for the trigger
      -- to aim at; bob a second Wall, so the one target slot is offered more
      -- candidates than it needs. `extras` seats a further printing under alice by
      -- name and `counters` is what a case puts on the Bandar; between them they
      -- are the ONLY difference between the boards below.
      --
      -- Wall of Stone at the destination for everyKindSpec's reason: a 0/8 body
      -- is unmoved by whatever these boards carry onto it.
      board extras counters = do
        forest <- S.printingOf s registry "Forest"
        wall <- S.printingOf s registry "Wall of Stone"
        bandar <- S.printingOf s registry "Scrounging Bandar"
        seats <- mapM (S.printingOf s registry) extras
        let (bandarId, g1) = S.addCreature bandar S.alice (S.landsInPlay forest 1)
            (takerId, g2) = S.addCreature wall S.alice g1
            (_, g3) = S.addCreature wall S.bob g2
            seated = foldl (\gs p -> snd (S.addCreature p S.alice gs)) g3 seats
        pure (bandarId, takerId, counters bandarId seated)
      -- alice's upkeep begins, the printed trigger goes on the stack and resolves
      -- -- Pawl.CounterspellSpec's bitterblossomChain, with the prompt count
      -- threaded so a case whose point is that NOTHING was asked can say so.
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begin wanted (bandarId, takerId, ready) =
        let begun =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
                (ready {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            run = Engine.runGame (bandarAnswer takerId wanted) begun (Engine.settleForPriority >> Engine.priorityLoop)
            ((_, after), asked) = State.runState run 0
         in (bandarId, takerId, asked, after)
      stocked oid = S.addCounter CounterKind.Shield 2 oid . S.addCounter CounterKind.PlusOnePlusOne 3 oid
      -- Names both kinds, so the arm's own filter is what keeps the shield
      -- counters home. An arm reading MovedKinds.AnyNumber would honour both.
      both :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural
      both = Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 2)]
  -- THE CASE THIS UNIT EXISTS FOR. Two of the three +1/+1 counters cross -- fewer
  -- than the pile holds, so this is not "all the +1/+1 counters" -- and the two
  -- shield counters the same answer named do not, so it is not "any number of
  -- counters" either.
  Spec.it s "the card settles the kind and the player settles the count" $ do
    built <- board [] stocked
    let (bandarId, takerId, before) = built
    Spec.assertEqWith s "the Bandar bears three +1/+1 counters and two shield counters" (tripleOn bandarId before) (3, 2, 0)
    Spec.assertEqWith s "and the Wall bears none of any kind" (tripleOn takerId before) (0, 0, 0)
    let (_, _, asked, after) = begin both built
    -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of the prompt count.
    Spec.assertEqWith s "two +1/+1 counters crossed and no shield counter did" (tripleOn takerId after) (2, 0, 0)
    Spec.assertEqWith s "and the Bandar kept its third +1/+1 counter and both shield counters" (tripleOn bandarId after) (1, 2, 0)
    Spec.assertEqWith s "the player was asked how many of the one kind" asked 1
  -- The same board differing in exactly ONE thing, the answer: none. "Any number"
  -- includes none, so the counters stay where they are and the question was still
  -- a question -- without this pair the case above would pass on an arm that moved
  -- whatever it liked.
  Spec.it s "and an answer naming none leaves every counter where it was" $ do
    built <- board [] stocked
    let (bandarId, takerId, asked, after) = begin Map.empty built
    Spec.assertEqWith s "the Wall received nothing" (tripleOn takerId after) (0, 0, 0)
    Spec.assertEqWith s "and the Bandar kept all five counters" (tripleOn bandarId after) (3, 2, 0)
    Spec.assertEqWith s "and it was asked anyway, none being one of the numbers" asked 1
  -- CR 122.5's third impossibility over the ONE kind the card names: alice's
  -- Solemnity refuses every counter on a creature, so there is no number the
  -- answer could give that would move anything, and the engine asks nothing. The
  -- same board as the headline and the same answer, differing in that one
  -- permanent.
  Spec.it s "CR 122.5 a destination that refuses the named kind is not asked about" $ do
    built <- board ["Solemnity"] stocked
    let (bandarId, takerId, asked, after) = begin both built
    Spec.assertEqWith s "the Wall received nothing, Solemnity refusing it" (tripleOn takerId after) (0, 0, 0)
    Spec.assertEqWith s "and the Bandar kept all five counters" (tripleOn bandarId after) (3, 2, 0)
    Spec.assertEqWith s "and with the card's one kind unmovable the player was not asked" asked 0

-- Goldberry, River-Daughter {1}{U} Legendary Creature - Nymph (The Lord of the
-- Rings: Tales of Middle-earth; name, cost, type line, power, toughness and
-- oracle text checked against Scryfall 2026-08-30),
-- data/cards/goldberry-river-daughter.json:
--
--   {T}: Move a counter of each kind not on Goldberry from another target
--   permanent you control onto Goldberry.
--   {U}, {T}: Move one or more counters from Goldberry onto another target
--   permanent you control. If you do, draw a card.
--
-- The first line is this group's subject, and the card is why "each absent kind"
-- exists: it names no kind, prints no count and asks nothing, yet it is neither
-- Fate Transfer's "all counters" (which takes the whole tally of every kind) nor
-- Agent's Toolkit's "a counter" (which takes one kind out of however many) --
-- the DESTINATION's own tally is what narrows the kinds, a read no other
-- spelling makes.
--
-- pawl's Goldberry omits the second ability. Its "one or more counters" is
-- MovedKinds.AnyNumber with zero excluded, and AnyNumber admits an empty answer
-- (the group above proves it does, deliberately), so writing it as AnyNumber
-- would be WEAKER than printed in the controller's favour rather than stricter
-- (#2702). Omitting it is stricter: alice simply has one fewer ability.
--
-- The counter kinds are three that do not interact -- CR 122.1a's +1/+1, CR
-- 122.1c's shield and CR 122.1h's finality. Not CR 122.1a's -1/-1 beside its
-- +1/+1, which CR 122.3 would annihilate as a state-based action before any
-- assertion ran.
finalityTripleOn :: ObjectId.ObjectId -> GameState.GameState -> (Natural, Natural, Natural)
finalityTripleOn oid gs =
  ( S.counterOf CounterKind.PlusOnePlusOne oid gs,
    S.counterOf CounterKind.Shield oid gs,
    S.counterOf CounterKind.Finality oid gs
  )

-- CR 602.2b through CR 601.2c: the Piker in the ability's one target slot.
-- FILTERS the offered set rather than building a recipient, so CR 608.2b's
-- re-read at resolution still finds what was named -- aimingTransfer's posture.
-- Every counter prompt is COUNTED, because what this group asserts about the
-- arm is that it raises none: a pure @Prompt r -> r@ could not say so.
goldberryAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
goldberryAnswer giver p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    pure (Map.map (Set.filter ((==) (Just giver) . Recipient.objectOf) . snd) asked)
  Prompt.ChooseMovedCounter {} -> do
    State.modify' (+ 1)
    pure (S.identityAnswer p)
  Prompt.ChooseMovedCounters {} -> do
    State.modify' (+ 1)
    pure (S.identityAnswer p)
  _ -> pure (S.identityAnswer p)

-- Records the candidates the ability's one target slot OFFERED, so the printed
-- "another target permanent you control" is read off the engine's list rather
-- than off the answer. Names the Piker as well, so the activation still goes
-- through.
goldberryOffered :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State (Set.Set ObjectId.ObjectId) r
goldberryOffered giver p = case p of
  Prompt.ChooseTargets _ _ _ asked -> do
    State.modify' (Set.union (Set.fromList (concatMap (Maybe.mapMaybe Recipient.objectOf . Set.toList . snd) (Map.elems asked))))
    pure (Map.map (Set.filter ((==) (Just giver) . Recipient.objectOf) . snd) asked)
  _ -> pure (S.identityAnswer p)

absentKindSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
absentKindSpec s registry = Spec.describe s "CR 122.5 moving a counter of each kind the second permanent lacks" $ do
  let -- alice: Goldberry, two Islands and a Goblin Piker. The Piker bears THREE
      -- kinds in three different counts, so "one of each kind" and "the whole
      -- tally of each kind" are different boards; the Islands make the one target
      -- slot offer more candidates than it needs, and bob's own Piker is a
      -- permanent the printed "you control" has to keep off the list.
      -- `onGoldberry` is what a case puts on the DESTINATION, and is the ONLY
      -- difference between the boards below.
      board onGoldberry = do
        island <- S.printingOf s registry "Island"
        goldberry <- S.printingOf s registry "Goldberry, River-Daughter"
        piker <- S.printingOf s registry "Goblin Piker"
        let (goldberryId, g1) = S.addCreature goldberry S.alice (S.landsInPlay island 2)
            (giverId, g2) = S.addCreature piker S.alice g1
            (_, g3) = S.addCreature piker S.bob g2
            stocked =
              S.addCounter CounterKind.Finality 2 giverId
                . S.addCounter CounterKind.Shield 4 giverId
                . S.addCounter CounterKind.PlusOnePlusOne 3 giverId
            ready = (onGoldberry goldberryId (stocked g3)) {GameState.priority = Just S.alice}
        pure (goldberryId, giverId, ready)
      -- The ability, activated once and resolved. Exactly one printed activated
      -- ability, not the first of however many -- pawl's Goldberry omits the
      -- second (#2702).
      tap (goldberryId, giverId, ready) = case Activate.abilitiesFor goldberryId ready of
        [only] ->
          let run =
                Engine.runGame
                  (goldberryAnswer giverId)
                  ready
                  (Activate.activateAbility S.alice goldberryId only >> Stack.resolveTop)
              ((_, after), asked) = State.runState run 0
           in Just (asked, after)
        _ -> Nothing
  -- THE CASE THIS UNIT EXISTS FOR, and BOTH halves of the spelling are
  -- load-bearing on it. Goldberry already bears shield counters and the Piker
  -- bears three kinds: the shield counters do not cross at all (the destination
  -- HAS that kind), and of the two kinds that do cross exactly ONE counter each
  -- goes, though the Piker holds three of one and two of the other.
  Spec.it s "one counter of each kind Goldberry lacks crosses, and the kind she has does not" $ do
    built <- board (S.addCounter CounterKind.Shield 2)
    let (goldberryId, giverId, before) = built
    Spec.assertEqWith s "the Piker bears three +1/+1, four shield and two finality counters" (finalityTripleOn giverId before) (3, 4, 2)
    Spec.assertEqWith s "and Goldberry bears two shield counters and nothing else" (finalityTripleOn goldberryId before) (0, 2, 0)
    case tap built of
      Just (asked, after) -> do
        -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of the prompt count.
        Spec.assertEqWith s "Goldberry gained one +1/+1 and one finality counter and no shield counter" (finalityTripleOn goldberryId after) (1, 2, 1)
        Spec.assertEqWith s "and the Piker is down one of each of those two kinds, its shield counters untouched" (finalityTripleOn giverId after) (2, 4, 1)
        Spec.assertEqWith s "and nothing was asked, the card settling both the kinds and the count" asked 0
      Nothing -> Spec.assertFailure s "expected Goldberry to offer exactly its one transcribed activated ability"
  -- The same board differing in exactly ONE thing, what Goldberry already bears:
  -- with no shield counter on her the shield kind is absent too and one shield
  -- counter crosses with the rest. Without this pair the case above would pass on
  -- a move that dropped shield counters for reasons of its own.
  Spec.it s "and with that kind gone from Goldberry the same shield counter crosses" $ do
    built <- board (const id)
    let (goldberryId, giverId, before) = built
    Spec.assertEqWith s "Goldberry bears no counter of any kind" (finalityTripleOn goldberryId before) (0, 0, 0)
    case tap built of
      Just (_, after) -> do
        Spec.assertEqWith s "Goldberry gained one counter of all three kinds" (finalityTripleOn goldberryId after) (1, 1, 1)
        Spec.assertEqWith s "and the Piker is down one of each" (finalityTripleOn giverId after) (2, 3, 1)
      Nothing -> Spec.assertFailure s "expected Goldberry to offer exactly its one transcribed activated ability"
  -- The card's own targeting, which the answerer above cannot prove because it
  -- names the Piker rather than reading what was offered: "ANOTHER target
  -- permanent YOU control" is Filter.Not Filter.IsSource beside
  -- Filter.ControlledBy Filter.You, so Goldberry herself and bob's Piker are both
  -- off the list while alice's two Islands -- permanents she controls that bear
  -- no counter -- stay on it.
  Spec.it s "CR 602.2b / 601.2c the ability offers every other permanent alice controls and neither Goldberry nor bob's" $ do
    (goldberryId, giverId, ready) <- board (const id)
    case Activate.abilitiesFor goldberryId ready of
      [only] -> do
        let run = Engine.runGame (goldberryOffered giverId) ready (Activate.activateAbility S.alice goldberryId only)
            (_, offered) = State.runState run Set.empty
        Spec.assertEqWith s "Goldberry is not among her own ability's candidates" (Set.member goldberryId offered) False
        Spec.assertEqWith s "alice's Piker is" (Set.member giverId offered) True
        Spec.assertEqWith s "and the candidates are exactly the three other permanents alice controls, bob's Piker excluded" (Set.size offered) 3
      _ -> Spec.assertFailure s "expected Goldberry to offer exactly its one transcribed activated ability"
  -- The other end of the same pair: a destination bearing every kind the first
  -- object has leaves no appropriate kind at all, so the move is empty -- and
  -- still asks nothing, since there was never a question.
  Spec.it s "a Goldberry bearing every kind the permanent has moves nothing and asks nothing" $ do
    built <-
      board
        ( \oid ->
            S.addCounter CounterKind.Finality 1 oid
              . S.addCounter CounterKind.Shield 2 oid
              . S.addCounter CounterKind.PlusOnePlusOne 1 oid
        )
    let (goldberryId, giverId, _) = built
    case tap built of
      Just (asked, after) -> do
        Spec.assertEqWith s "Goldberry is left with exactly what she started with" (finalityTripleOn goldberryId after) (1, 2, 1)
        Spec.assertEqWith s "and the Piker kept every counter it had" (finalityTripleOn giverId after) (3, 4, 2)
        Spec.assertEqWith s "and with no absent kind the player was not asked" asked 0
      Nothing -> Spec.assertFailure s "expected Goldberry to offer exactly its one transcribed activated ability"

-- Spike Cannibal {1}{B}{B} Creature - Spike (Exodus; name, cost, type line,
-- power, toughness and oracle text checked against Scryfall 2026-08-30),
-- data/cards/spike-cannibal.json:
--
--   This creature enters with a +1/+1 counter on it.
--   When this creature enters, move all +1/+1 counters from all creatures onto
--   it.
--
-- The second line is this group's subject, and the card is why a GROUP-valued
-- first side exists: "from all creatures" names every creature on the
-- battlefield rather than one permanent, so a single sentence performs one CR
-- 122.5 pair per creature. It is also the cheapest producer of that shape --
-- the kind is printed and the whole tally crosses, so nothing is asked, no
-- answer is shaped and no distribution is decided.
--
-- The counters are its own +1/+1 (CR 122.1a) beside CR 122.1c's shield counter,
-- which nothing in the sentence names: the shield counters are what tell "all
-- +1/+1 counters" apart from Fate Transfer's "all counters", and `pairOn` above
-- is the tally this group reads.

-- Every counter prompt is COUNTED, because what this group asserts about the
-- arm is that it raises none however many first objects the sweep named: a pure
-- @Prompt r -> r@ could not say so.
cannibalAnswer :: Prompt.Prompt r -> State.State Int r
cannibalAnswer p = case p of
  Prompt.ChooseMovedCounter {} -> do
    State.modify' (+ 1)
    pure (S.identityAnswer p)
  Prompt.ChooseMovedCounters {} -> do
    State.modify' (+ 1)
    pure (S.identityAnswer p)
  _ -> pure (S.identityAnswer p)

groupSourceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
groupSourceSpec s registry = Spec.describe s "CR 122.5 moving counters off a group of permanents" $ do
  let -- FOUR permanents bearing +1/+1 counters in four different counts, so
      -- "took from all of them" is a different number from "took from any one of
      -- them" and from every partial sweep in between. Two of the three creatures
      -- are BOB's, since "all creatures" says nothing about control; the Piker
      -- also bears shield counters, which the printed kind leaves alone; and
      -- alice's Island is the control leg -- a permanent that is not a creature,
      -- on the same board, differing from the three givers in card type alone.
      --
      -- `stock` is what the case puts on those four, and is the ONLY difference
      -- between the two boards below.
      board extras stock = do
        island <- S.printingOf s registry "Island"
        wall <- S.printingOf s registry "Wall of Stone"
        piker <- S.printingOf s registry "Goblin Piker"
        cannibal <- S.printingOf s registry "Spike Cannibal"
        seats <- mapM (S.printingOf s registry) extras
        let (aliceWall, g1) = S.addCreature wall S.alice S.threePlayerGame
            (bobWall, g2) = S.addCreature wall S.bob g1
            (bobPiker, g3) = S.addCreature piker S.bob g2
            (aliceIsland, g4) = S.addCreature island S.alice g3
            seated = foldl (\gs p -> snd (S.addCreature p S.alice gs)) g4 seats
            (cannibalId, g5) = S.entersWithTrigger cannibal S.alice (stock aliceWall bobWall bobPiker aliceIsland seated)
            -- The card's own entry rider, supplied by hand: S.addCreature places
            -- a permanent without running CR 614.1c's replacement, and a 0/0
            -- Spike Cannibal would be buried by CR 704.5f before its own trigger
            -- resolved. Pawl.ReplacementSpec is where an entry rider is proven.
            entered = S.addCounter CounterKind.PlusOnePlusOne 1 cannibalId g5
        pure (cannibalId, aliceWall, bobWall, bobPiker, aliceIsland, S.settleSba entered)
      -- Every giver bears +1/+1 counters, in counts nothing else on the board
      -- repeats; only the Piker bears the kind the sentence does not name.
      stocked aliceWall bobWall bobPiker aliceIsland =
        S.addCounter CounterKind.PlusOnePlusOne 6 aliceIsland
          . S.addCounter CounterKind.Shield 5 bobPiker
          . S.addCounter CounterKind.PlusOnePlusOne 2 bobPiker
          . S.addCounter CounterKind.PlusOnePlusOne 3 bobWall
          . S.addCounter CounterKind.PlusOnePlusOne 4 aliceWall
      -- The CR 603.6a trigger, placed by the settle and then resolved. The
      -- narrowest path that shows the behaviour.
      onStack gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      gathered gs =
        let ((_, after), asked) = State.runState (Engine.runGame cannibalAnswer gs Stack.resolveTop) 0
         in (asked, after)
  -- THE CASE THIS UNIT EXISTS FOR. Every creature on the battlefield is a first
  -- object of the one sentence, so the destination ends up with the sum of three
  -- separate tallies plus the one it entered with -- a number no reading that
  -- moved from a single permanent can produce.
  Spec.it s "every creature's +1/+1 counters cross at once, whichever seat controls it" $ do
    (cannibalId, aliceWall, bobWall, bobPiker, aliceIsland, before) <- board [] stocked
    let staged = onStack before
    Spec.assertEqWith s "alice's Wall bears four +1/+1 counters" (pairOn aliceWall before) (4, 0)
    Spec.assertEqWith s "bob's Wall bears three" (pairOn bobWall before) (3, 0)
    Spec.assertEqWith s "bob's Piker bears two, beside five shield counters" (pairOn bobPiker before) (2, 5)
    Spec.assertEqWith s "and the Cannibal bears only the one it entered with" (pairOn cannibalId before) (1, 0)
    Spec.assertBool s (not (null (GameState.stack staged))) "the Cannibal's enters trigger really was on the stack"
    let (asked, after) = gathered staged
    -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of every proxy: the whole board's
    -- +1/+1 counters gathered onto one permanent, and each giver emptied.
    Spec.assertEqWith s "the Cannibal has its own counter plus all nine, and no shield counter" (pairOn cannibalId after) (10, 0)
    Spec.assertEqWith s "alice's Wall is down every +1/+1 counter it had" (pairOn aliceWall after) (0, 0)
    Spec.assertEqWith s "so is bob's Wall" (pairOn bobWall after) (0, 0)
    Spec.assertEqWith s "and so is bob's Piker, its five shield counters untouched" (pairOn bobPiker after) (0, 5)
    -- The control leg, on the SAME board: a permanent that is not a creature is
    -- not a first object, so its six +1/+1 counters stay where they are. Without
    -- it the case above would pass on a sweep that took every +1/+1 counter in
    -- play whatever bore it.
    Spec.assertEqWith s "and alice's Island, which is no creature, keeps all six of its own" (pairOn aliceIsland after) (6, 0)
    Spec.assertEqWith s "and nothing was asked, the card settling the kind, the count and the givers" asked 0
  -- The same board differing in exactly ONE thing, what the givers bear: with the
  -- +1/+1 counters off the three creatures, "all +1/+1 counters" finds none on
  -- any of them and the Cannibal is left with what it entered with. Without this
  -- pair the case above would pass on a sweep that credited the destination a
  -- number of its own rather than what it took.
  Spec.it s "a board whose creatures bear no +1/+1 counter moves nothing and still asks nothing" $ do
    (cannibalId, aliceWall, bobWall, bobPiker, aliceIsland, before) <- board [] (\_ _ bobPiker' aliceIsland' -> S.addCounter CounterKind.PlusOnePlusOne 6 aliceIsland' . S.addCounter CounterKind.Shield 5 bobPiker')
    let (asked, after) = gathered (onStack before)
    Spec.assertEqWith s "the Cannibal still bears the one counter it entered with" (pairOn cannibalId after) (1, 0)
    Spec.assertEqWith s "alice's Wall bears none either way" (pairOn aliceWall after) (0, 0)
    Spec.assertEqWith s "so does bob's" (pairOn bobWall after) (0, 0)
    Spec.assertEqWith s "bob's Piker keeps the five shield counters the sentence never named" (pairOn bobPiker after) (0, 5)
    Spec.assertEqWith s "and the Island keeps its six, as in the case above" (pairOn aliceIsland after) (6, 0)
    Spec.assertEqWith s "and with nothing to move the player was not asked" asked 0
  -- CR 608.2f's FIRST branch, made observable. Hardened Scales grows each
  -- placement of one or more +1/+1 counters onto a creature alice controls by one
  -- (CR 614.16), so the number of PLACEMENTS the sentence makes is readable off
  -- the board: nine counters arriving as one batch land as ten, where the same
  -- nine arriving as three batches of four, three and two would land as twelve.
  -- The removals are untouched either way, which is what separates "the arrival
  -- was grown once" from "more was taken".
  --
  -- The Cannibal's own counter is added by hand and so escapes the replacement,
  -- which is what keeps the arithmetic below about the move alone.
  Spec.it s "CR 608.2f the whole sweep arrives as one placement per kind, not one per giver" $ do
    (cannibalId, aliceWall, bobWall, bobPiker, _, before) <- board ["Hardened Scales"] stocked
    let (_, after) = gathered (onStack before)
    -- THE GAMEPLAY-LEVEL ASSERTION: one batch of nine grown by one, not three
    -- batches grown by one apiece.
    Spec.assertEqWith s "the nine counters arrived as one batch, so Hardened Scales grew them once" (pairOn cannibalId after) (11, 0)
    Spec.assertEqWith s "and the givers are down exactly what they had, the replacement having grown the arrival and not the departure" (fmap (\oid -> pairOn oid after) [aliceWall, bobWall, bobPiker]) [(0, 0), (0, 0), (0, 5)]

-- Takesies {2}{U} Instant, the front half of Takesies // Backsies (Unknown
-- Event, set type funny; name, cost, type line and oracle text checked against
-- Scryfall 2026-08-30), data/cards/takesies-backsies.json:
--
--   Move up to one counter from each permanent onto target permanent.
--   Fuse (You may cast one or both halves of this card from your hand.)
--
-- The first line is this group's subject, and the card is the only printing that
-- writes it -- Pawl.Types.MovedKinds' haddock has the sweep. "Up to one" is what
-- no other arm can say: rule 122.5 moves a counter wherever it can, so a player
-- who may leave a given first object alone is being asked a question 'Chosen'
-- has no room for. The PER-SOURCE part is not new, `from` having been an
-- ObjectRef since #2717.
--
-- Not implemented: fuse, so the card cannot be cast as both halves at once, and
-- the Backsies half's "Until end of turn, treat all counters as -1/-1 counters",
-- so that half resolves doing nothing (#2725). Both omissions leave pawl's card
-- strictly less able than the printing, never more.
takesiesName :: CardName.CardName
takesiesName = CardName.MkCardName (Text.pack "Takesies")

-- Which counter the answerer takes off a given first object, and whether it
-- takes one at all. The kind is pinned by POSITION in the offered list for
-- toolkitAnswer's reason, and the object is named because the whole point of the
-- sentence is that each first object is asked about separately: a pure
-- @Prompt r -> r@ could not answer one permanent differently from the next, and
-- every prompt this arm raises is COUNTED so a case can say which permanents
-- were never asked about at all.
takesiesAnswer :: ObjectId.ObjectId -> Map.Map ObjectId.ObjectId Pick -> Prompt.Prompt r -> State.State Int r
takesiesAnswer destination picks p = case p of
  -- FILTERED, not built: the answer is one of the recipients the engine offered,
  -- so CR 608.2b's re-read at resolution still finds the target.
  Prompt.ChooseTargets _ _ _ sets -> pure (S.preferring ((== Just destination) . Recipient.objectOf) sets)
  Prompt.ChooseMovedCounterOrNone _ _ from _ offered -> do
    State.modify' (+ 1)
    pure
      ( case Map.findWithDefault Decline from picks of
          Lowest -> Just (NonEmpty.head offered)
          Highest -> Just (NonEmpty.last offered)
          Decline -> Nothing
      )
  _ -> pure (S.identityAnswer p)

upToOneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
upToOneSpec s registry = Spec.describe s "CR 122.5 moving up to one counter off each permanent" $ do
  let -- FOUR permanents bearing counters in counts nothing else on the board
      -- repeats, spread over all three seats, since "each permanent" says
      -- nothing about control -- and three of the four bear MORE THAN ONE
      -- counter, which is what makes "one from each" a different board from
      -- "all from each". Two of them bear two KINDS, which is what makes the
      -- kind the player's to pick rather than the only one there is.
      --
      -- The destination is a fifth permanent bearing counters of its own, alice's
      -- Wall of Stone, and alice's three Islands are the counterless leg: a
      -- permanent the sweep reaches and has nothing to ask about.
      board extras = do
        island <- S.printingOf s registry "Island"
        wall <- S.printingOf s registry "Wall of Stone"
        piker <- S.printingOf s registry "Goblin Piker"
        takesies <- S.printingOf s registry "Takesies"
        seats <- mapM (S.printingOf s registry) extras
        let (destination, g1) = S.addCreature wall S.alice (S.landsFor island S.alice 3 S.threePlayerGame)
            (alicePiker, g2) = S.addCreature piker S.alice g1
            (bobWall, g3) = S.addCreature wall S.bob g2
            (carolPiker, g4) = S.addCreature piker S.carol g3
            (carolWall, g5) = S.addCreature wall S.carol g4
            seated = foldl (\gs pr -> snd (S.addCreature pr S.alice gs)) g5 seats
            (held, g6) = S.addHandCard takesies S.alice seated
            stocked =
              S.addCounter CounterKind.Shield 9 destination
                . S.addCounter CounterKind.Shield 3 alicePiker
                . S.addCounter CounterKind.PlusOnePlusOne 4 alicePiker
                . S.addCounter CounterKind.Shield 2 bobWall
                . S.addCounter CounterKind.PlusOnePlusOne 5 bobWall
                . S.addCounter CounterKind.PlusOnePlusOne 6 carolPiker
                $ S.addCounter CounterKind.PlusOnePlusOne 7 carolWall g6
        pure (held, destination, alicePiker, bobWall, carolPiker, carolWall, S.settleSba stocked)
      -- alice's Piker gives its LEAST kind, bob's Wall its GREATEST, carol's Wall
      -- its only one, and carol's Piker gives nothing at all. Three different
      -- answers to one sentence, which is what the arm asks per first object.
      picksFor alicePiker bobWall carolPiker carolWall =
        Map.fromList [(alicePiker, Lowest), (bobWall, Highest), (carolPiker, Decline), (carolWall, Lowest)]
      -- Pawl.Support.cast cannot name a half, so the cast goes through
      -- Pawl.Engine.Cast directly (Pawl.Support.soleFaceName errors on a card
      -- offering two castable halves). The narrowest path that shows the
      -- behaviour: one cast, one resolution.
      play picks destination held ready =
        let run =
              Engine.runGame
                (takesiesAnswer destination picks)
                ready
                (Cast.castSpell S.alice held takesiesName Facing.FaceUp >> Stack.resolveTop)
            ((_, after), asked) = State.runState run 0
         in (asked, after)
  -- THE CASE THIS UNIT EXISTS FOR. Every permanent on the battlefield is a first
  -- object of the one sentence, each gives AT MOST ONE counter, and which one --
  -- or whether any -- is the player's answer for that permanent alone.
  Spec.it s "each permanent gives up to one counter, of the kind the player picked for it" $ do
    (held, destination, alicePiker, bobWall, carolPiker, carolWall, before) <- board []
    Spec.assertEqWith s "alice's Piker bears four +1/+1 counters and three shield counters" (pairOn alicePiker before) (4, 3)
    Spec.assertEqWith s "bob's Wall bears five and two" (pairOn bobWall before) (5, 2)
    Spec.assertEqWith s "carol's Piker bears six of one kind" (pairOn carolPiker before) (6, 0)
    Spec.assertEqWith s "carol's Wall bears seven" (pairOn carolWall before) (7, 0)
    Spec.assertEqWith s "and the destination bears nine shield counters of its own" (pairOn destination before) (0, 9)
    let (asked, after) = play (picksFor alicePiker bobWall carolPiker carolWall) destination held before
    -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of every proxy: one counter off each
    -- permanent that gave, of the kind that permanent's answer named, and the
    -- destination holding exactly the sum of them.
    Spec.assertEqWith s "alice's Piker is down one +1/+1 counter and keeps every shield counter" (pairOn alicePiker after) (3, 3)
    Spec.assertEqWith s "bob's Wall is down one SHIELD counter and keeps all five +1/+1 counters" (pairOn bobWall after) (5, 1)
    Spec.assertEqWith s "carol's Wall is down one of its seven" (pairOn carolWall after) (6, 0)
    Spec.assertEqWith s "carol's Piker, whose answer declined, keeps all six" (pairOn carolPiker after) (6, 0)
    Spec.assertEqWith s "and the destination gained the two +1/+1 counters and the one shield counter that crossed, beside its own nine" (pairOn destination after) (2, 10)
    -- The prompt count, last: four permanents bearing a counter were asked about,
    -- while the destination -- rule 122.5's first impossibility, the two objects
    -- being one -- and alice's three counterless Islands were not.
    Spec.assertEqWith s "each permanent bearing a counter was asked about, and the destination and the Islands were not" asked 4
  -- The same board differing in exactly ONE thing, the answers: every permanent
  -- declines. Without this pair the case above would pass on an arm that moved a
  -- counter whatever the player said.
  Spec.it s "a player who declines every permanent moves nothing at all" $ do
    (held, destination, alicePiker, bobWall, carolPiker, carolWall, before) <- board []
    let declining = Map.fromList (fmap (\oid -> (oid, Decline)) [alicePiker, bobWall, carolPiker, carolWall])
        (asked, after) = play declining destination held before
    Spec.assertEqWith s "alice's Piker keeps everything it had" (pairOn alicePiker after) (4, 3)
    Spec.assertEqWith s "so does bob's Wall" (pairOn bobWall after) (5, 2)
    Spec.assertEqWith s "so does carol's Piker" (pairOn carolPiker after) (6, 0)
    Spec.assertEqWith s "so does carol's Wall" (pairOn carolWall after) (7, 0)
    Spec.assertEqWith s "and the destination is left with the nine shield counters it started with" (pairOn destination after) (0, 9)
    Spec.assertEqWith s "and the player was asked about the same four permanents, having answered all four differently" asked 4
  -- CR 608.2f's FIRST branch, made observable, and the reason a test of the
  -- givers alone would not settle it: Hardened Scales grows each placement of one
  -- or more +1/+1 counters onto a creature alice controls by one (CR 614.16), so
  -- TWO +1/+1 counters gathered off two permanents land as three when they arrive
  -- as one batch and as four when they arrive as two. The removals are untouched
  -- either way.
  Spec.it s "CR 608.2f the counters gathered off many permanents arrive as one placement per kind" $ do
    (held, destination, alicePiker, bobWall, carolPiker, carolWall, before) <- board ["Hardened Scales"]
    let (_, after) = play (picksFor alicePiker bobWall carolPiker carolWall) destination held before
    -- THE GAMEPLAY-LEVEL ASSERTION: one batch of two grown by one, not two
    -- batches of one grown by one apiece.
    Spec.assertEqWith s "the two +1/+1 counters arrived as one batch, so Hardened Scales grew them once" (pairOn destination after) (3, 10)
    Spec.assertEqWith s "and the givers are down exactly one apiece, the replacement having grown the arrival and not the departure" (fmap (`pairOn` after) [alicePiker, bobWall, carolWall]) [(3, 3), (5, 1), (6, 0)]
