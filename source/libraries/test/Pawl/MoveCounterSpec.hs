{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Resolve's Effect.MoveCounters arm -- CR 122.5's move of a
-- counter from one object onto a second, and the atomicity that makes it one
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
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Phasing as Phasing
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
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
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
