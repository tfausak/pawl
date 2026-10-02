{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Resolve's Effect.MoveCounters arm -- CR 122.5's move of
-- counters from one object onto a second, and the atomicity that makes it one
-- action rather than a removal written beside a placement.
module Pawl.MoveCounterSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan

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
  namedAnyNumberSpec s registry
  absentKindSpec s registry
  atLeastOneSpec s registry
  groupSourceSpec s registry
  groupDestinationSpec s registry
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

-- The +1/+1 and shield tallies on one object -- the pair every case below reads,
-- and the two kinds the artifact bears when its ability resolves.
pairOn :: ObjectId.ObjectId -> GameState.GameState -> (Natural, Natural)
pairOn oid gs = (S.counterOf CounterKind.PlusOnePlusOne oid gs, S.counterOf CounterKind.Shield oid gs)

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
-- Bioshift prints the same spelling on an instant and is cheaper to drive;
-- Pawl.TargetSpec is where it is, its "with the same controller" being what that
-- case exists to prove. The Bandar stays this group's subject because the boards
-- below are about which KINDS the prompt offers, which an instant with a second
-- target slot would only make harder to read.
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
        let (bandarId, g1) = S.addPermanent bandar S.alice (S.landsInPlay forest 1)
            (takerId, g2) = S.addPermanent wall S.alice g1
            (_, g3) = S.addPermanent wall S.bob g2
            seated = List.foldl' (\gs p -> snd (S.addPermanent p S.alice gs)) g3 seats
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
-- The second line is MovedKinds.AtLeastOne -- "any number of counters" with the
-- empty answer struck out -- and atLeastOneSpec below is where it is proved.
-- Every case here activates the FIRST of the card's two printed abilities by
-- position, so a Goldberry that lost one of them would fail this group rather
-- than quietly test the other.
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
        let (goldberryId, g1) = S.addPermanent goldberry S.alice (S.landsInPlay island 2)
            (giverId, g2) = S.addPermanent piker S.alice g1
            (_, g3) = S.addPermanent piker S.bob g2
            stocked =
              S.addCounter CounterKind.Finality 2 giverId
                . S.addCounter CounterKind.Shield 4 giverId
                . S.addCounter CounterKind.PlusOnePlusOne 3 giverId
            ready = (onGoldberry goldberryId (stocked g3)) {GameState.priority = Just S.alice}
        pure (goldberryId, giverId, ready)
      -- The FIRST of the card's two printed activated abilities, activated once
      -- and resolved. Both are named rather than one taken off the front, so this
      -- group cannot drift onto the other.
      tap (goldberryId, giverId, ready) = case Activatable.abilitiesFor goldberryId ready of
        [only, _] ->
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
      Nothing -> Spec.assertFailure s "expected Goldberry to offer both of her printed activated abilities"
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
      Nothing -> Spec.assertFailure s "expected Goldberry to offer both of her printed activated abilities"
  -- The card's own targeting, which the answerer above cannot prove because it
  -- names the Piker rather than reading what was offered: "ANOTHER target
  -- permanent YOU control" is Filter.Not Filter.IsSource beside
  -- Filter.ControlledBy Filter.You, so Goldberry herself and bob's Piker are both
  -- off the list while alice's two Islands -- permanents she controls that bear
  -- no counter -- stay on it.
  Spec.it s "CR 602.2b / 601.2c the ability offers every other permanent alice controls and neither Goldberry nor bob's" $ do
    (goldberryId, giverId, ready) <- board (const id)
    case Activatable.abilitiesFor goldberryId ready of
      [only, _] -> do
        let run = Engine.runGame (goldberryOffered giverId) ready (Activate.activateAbility S.alice goldberryId only)
            (_, offered) = State.runState run Set.empty
        Spec.assertEqWith s "Goldberry is not among her own ability's candidates" (Set.member goldberryId offered) False
        Spec.assertEqWith s "alice's Piker is" (Set.member giverId offered) True
        Spec.assertEqWith s "and the candidates are exactly the three other permanents alice controls, bob's Piker excluded" (Set.size offered) 3
      _ -> Spec.assertFailure s "expected Goldberry to offer both of her printed activated abilities"
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
      Nothing -> Spec.assertFailure s "expected Goldberry to offer both of her printed activated abilities"

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
        let (aliceWall, g1) = S.addPermanent wall S.alice S.threePlayerGame
            (bobWall, g2) = S.addPermanent wall S.bob g1
            (bobPiker, g3) = S.addPermanent piker S.bob g2
            (aliceIsland, g4) = S.addPermanent island S.alice g3
            seated = List.foldl' (\gs p -> snd (S.addPermanent p S.alice gs)) g4 seats
            (cannibalId, g5) = S.entersWithTrigger cannibal S.alice (stock aliceWall bobWall bobPiker aliceIsland seated)
            -- The card's own entry rider, supplied by hand: S.addPermanent places
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
    (cannibalId, aliceWall, bobWall, bobPiker, aliceIsland, before) <- board [] (\_ _ bobPiker2 aliceIsland2 -> S.addCounter CounterKind.PlusOnePlusOne 6 aliceIsland2 . S.addCounter CounterKind.Shield 5 bobPiker2)
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
-- writes it -- Pawl.Types.MovedKinds' haddock records that sweep, its query and
-- its date, and the card that would refute it. "Up to one" is what
-- no other arm can say: rule 122.5 moves a counter wherever it can, so a player
-- who may leave a given first object alone is being asked a question 'Chosen'
-- has no room for. The PER-SOURCE part is not new, `from` having been an
-- ObjectRef since #2717.
--
-- "Each permanent" narrows by nothing, so the card writes ObjectRef.EachMatching
-- over an EMPTY Filter.And -- a conjunction of no conditions, true of every
-- candidate. That arm sweeps the battlefield, and CR 110.1 makes every object on
-- the battlefield a permanent, so the empty conjunction is the sentence rather
-- than a filter left unwritten.
--
-- Fuse is transcribed (CR 702.102), so the card offers a fused cast of both
-- halves from a hand -- Pawl.CastSpec's WearTear group is where that is proved,
-- against Wear // Tear. Not implemented: the Backsies half's "Until end of turn,
-- treat all counters as -1/-1 counters", so the right half of a fused cast, like
-- a cast of Backsies alone, resolves doing nothing (#2725). The omission leaves
-- pawl's card strictly less able than the printing, never more.
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
      -- nothing about control -- and every one of them bears MORE THAN ONE
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
        let (destination, g1) = S.addPermanent wall S.alice (S.landsFor island S.alice 3 S.threePlayerGame)
            (alicePiker, g2) = S.addPermanent piker S.alice g1
            (bobWall, g3) = S.addPermanent wall S.bob g2
            (carolPiker, g4) = S.addPermanent piker S.carol g3
            (carolWall, g5) = S.addPermanent wall S.carol g4
            seated = List.foldl' (\gs pr -> snd (S.addPermanent pr S.alice gs)) g5 seats
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
                (Cast.castSpell S.manaPerformer S.alice held takesiesName Facing.FaceUp >> Stack.resolveTop)
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
    Spec.assertEqWith s "and the same four permanents were asked about, the count being the one thing this board shares with the case above" asked 4
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

-- Goldberry, River-Daughter's SECOND ability, the one this group exists for:
--
--   {U}, {T}: Move one or more counters from Goldberry onto another target
--   permanent you control. If you do, draw a card.
--
-- The card is why "one or more" exists as its own arm: it is "any number of
-- counters" with the empty answer struck out, and the difference is observable
-- through the printed rider -- an answer of none would leave "if you do" unfired,
-- which is weaker than printed in the controller's favour rather than stricter.
-- The rider itself is the existing `slot` read, Black Panther, Wakandan King's
-- shape.
--
-- The counter kinds are CR 122.1a's +1/+1 beside CR 122.1c's shield, two kinds
-- that do not interact, so an answer naming one of each is a different board from
-- any count out of either alone -- and the +1/+1 counter is the LESSER of the two
-- as a CounterKind, which is what the floor's repair falls back on.
goldberryFloorAnswer ::
  ObjectId.ObjectId ->
  Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural ->
  Prompt.Prompt r ->
  State.State (Int, Int) r
goldberryFloorAnswer taker wanted p = case p of
  -- CR 602.2b through CR 601.2c, FILTERED out of what the engine offered rather
  -- than built by hand, so CR 608.2b's re-read at resolution finds what was named.
  Prompt.ChooseTargets _ _ _ asked ->
    pure (Map.map (Set.filter ((==) (Just taker) . Recipient.objectOf) . snd) asked)
  -- Answered VERBATIM, so an answerer cannot repair a mutation by re-deriving a
  -- legal answer, and counted in the FIRST slot of the pair.
  Prompt.ChooseMovedCountersAtLeastOne {} -> do
    State.modify' (\(floored, any_) -> (floored + 1, any_))
    pure wanted
  -- Counted in the SECOND slot, because which of the two prompts was raised is
  -- half of what this group asserts: the two carry the same payload and the same
  -- answer, and only the floor tells them apart.
  Prompt.ChooseMovedCounters {} -> do
    State.modify' (\(floored, any_) -> (floored, any_ + 1))
    pure wanted
  _ -> pure (S.identityAnswer p)

atLeastOneSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
atLeastOneSpec s registry = Spec.describe s "CR 122.5 moving one or more counters" $ do
  let -- alice: Goldberry, two Islands (the {U} has to come from somewhere), a
      -- Goblin Piker to move counters onto and a library to draw from. bob's own
      -- Piker is a permanent the printed "you control" has to keep off the target
      -- list, and the Islands make the slot offer more candidates than it needs.
      -- `counters` is what a case puts on GOLDBERRY, the first object here, and is
      -- the ONLY difference between the boards below.
      board counters = do
        island <- S.printingOf s registry "Island"
        goldberry <- S.printingOf s registry "Goldberry, River-Daughter"
        piker <- S.printingOf s registry "Goblin Piker"
        let (goldberryId, g1) = S.addPermanent goldberry S.alice (S.landsInPlay island 2)
            (takerId, g2) = S.addPermanent piker S.alice g1
            (_, g3) = S.addPermanent piker S.bob g2
            -- Stocked so the printed draw has a card to take and CR 104.3c cannot
            -- end the game before an assertion runs.
            stockedLibrary = List.foldl' (\gs _ -> snd (S.addLibraryCard piker S.alice gs)) g3 [1 :: Int .. 3]
            ready = (counters goldberryId stockedLibrary) {GameState.priority = Just S.alice}
        pure (goldberryId, takerId, ready)
      -- The SECOND printed activated ability, activated once and resolved. Both
      -- are named, which is itself an assertion: pawl's Goldberry used to carry
      -- the first alone.
      tap wanted (goldberryId, takerId, ready) = case Activatable.abilitiesFor goldberryId ready of
        [_, second] ->
          let run =
                Engine.runGame
                  (goldberryFloorAnswer takerId wanted)
                  ready
                  (Activate.activateAbility S.alice goldberryId second >> Stack.resolveTop)
              ((_, after), asked) = State.runState run (0, 0)
           in Just (asked, after)
        _ -> Nothing
  -- The other end of the pair: the floor is what the CARD may ask for, not what
  -- rule 122.5 can perform. A Goldberry bearing no counter has no appropriate kind
  -- (rule 122.5's second impossibility), so nothing crosses, nothing is asked, and
  -- the rider's gate reads the zero this opcode still writes into its slot.
  Spec.it s "a Goldberry bearing no counter moves nothing, asks nothing and draws nothing" $ do
    built <- board (const id)
    let (goldberryId, takerId, _) = built
    case tap (Map.singleton CounterKind.PlusOnePlusOne 1) built of
      Just (asked, after) -> do
        Spec.assertEqWith s "the Piker received nothing" (tripleOn takerId after) (0, 0, 0)
        Spec.assertEqWith s "and Goldberry still bears nothing" (tripleOn goldberryId after) (0, 0, 0)
        Spec.assertEqWith s "and alice drew nothing, the rider's gate reading zero" (S.handSize S.alice after) 0
        Spec.assertEqWith s "and with no kind to offer neither prompt was raised" asked (0, 0)
      Nothing -> Spec.assertFailure s "expected Goldberry to offer both of her printed activated abilities"

-- Forgotten Ancient {3}{G} Creature - Elemental 0/3 (Ravnica: City of Guilds;
-- name, cost, type line, power, toughness and oracle text checked against
-- Scryfall 2026-08-31), data/cards/forgotten-ancient.json:
--
--   Whenever a player casts a spell, you may put a +1/+1 counter on this
--   creature.
--   At the beginning of your upkeep, you may move any number of +1/+1 counters
--   from this creature onto other creatures.
--
-- The second line is this group's subject, and the card is why a GROUP-valued
-- DESTINATION exists: "onto other creatures" names every creature on the
-- battlefield but this one, so one sentence performs one CR 122.5 pair per
-- recipient and the player says how many counters each of them gets. Spike
-- Cannibal's group is on the FIRST side and gathers counters in; this one spreads
-- them out, and the difference is that the count per object is a question rather
-- than a tally.
--
-- "Other creatures" says nothing about control, so bob's creature is a recipient
-- and the boards below prove it -- a filter reading "you control" would pass every
-- assertion but that one.
--
-- The counters go on by hand rather than through the printed cast trigger, which
-- gives one counter per spell: these boards need five at once, so that an answer
-- spreading four of them unequally over two of three candidates is a different
-- board from every even split and from every single recipient.
forgottenAnswer ::
  Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural) ->
  Prompt.Prompt r ->
  State.State (Int, Set.Set ObjectId.ObjectId) r
forgottenAnswer wanted p = case p of
  -- The printed "you may", taken every time: a declined trigger would move
  -- nothing for a reason no case here is about.
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  -- Answered VERBATIM, so an answerer cannot repair a mutation by re-deriving a
  -- legal answer; COUNTED, because one case asserts that nothing was asked; and
  -- the offered recipients are RECORDED, so which objects the engine put on the
  -- list is read off the engine rather than off the answer.
  Prompt.ChooseDistributedMovedCounters _ _ _ _ offered -> do
    State.modify' (\(asked, seen) -> (asked + 1, Set.union seen (Set.fromList (NonEmpty.toList offered))))
    pure wanted
  _ -> pure (S.identityAnswer p)

groupDestinationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
groupDestinationSpec s registry = Spec.describe s "CR 122.5 moving counters onto a group of permanents" $ do
  let -- alice: her Forgotten Ancient and two other creatures; bob a third, since
      -- "other creatures" names his as well. Wall of Stone at two of the three
      -- destinations, a 0/8 body being unmoved by whatever these boards carry onto
      -- it, and a Goblin Piker at the third so the recipients are told apart by
      -- more than their ids. `counters` is what a case puts on the Ancient and is
      -- the ONLY difference between the boards below.
      board counters = do
        forest <- S.printingOf s registry "Forest"
        wall <- S.printingOf s registry "Wall of Stone"
        piker <- S.printingOf s registry "Goblin Piker"
        ancient <- S.printingOf s registry "Forgotten Ancient"
        let (ancientId, g1) = S.addPermanent ancient S.alice (S.landsInPlay forest 1)
            (aliceWall, g2) = S.addPermanent wall S.alice g1
            (alicePiker, g3) = S.addPermanent piker S.alice g2
            (bobWall, g4) = S.addPermanent wall S.bob g3
        pure (ancientId, aliceWall, alicePiker, bobWall, counters ancientId g4)
      -- alice's upkeep begins, the printed trigger goes on the stack and resolves
      -- -- namedAnyNumberSpec's `begin`, with the prompt count and the offered
      -- recipients threaded out of the answerer.
      upkeep = Phase.Beginning BeginningStep.Upkeep
      begin wanted (ancientId, aliceWall, alicePiker, bobWall, ready) =
        let begun =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
                (ready {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            run = Engine.runGame (forgottenAnswer wanted) begun (Engine.settleForPriority >> Engine.priorityLoop)
            ((_, after), asked) = State.runState run (0, Set.empty)
         in (ancientId, aliceWall, alicePiker, bobWall, asked, after)
      plussed = S.counterOf CounterKind.PlusOnePlusOne
  -- THE CASE THIS UNIT EXISTS FOR. Four of the five counters cross, THREE onto one
  -- creature and ONE onto another, and the third candidate gets none -- so this is
  -- neither an even split, nor a single recipient, nor the whole pile, and no
  -- move naming one destination could have produced it.
  Spec.it s "the player spreads counters unevenly over some of the other creatures" $ do
    built <- board (S.addCounter CounterKind.PlusOnePlusOne 5)
    let (ancientId, aliceWall, alicePiker, bobWall, before) = built
    Spec.assertEqWith s "the Ancient bears five +1/+1 counters and the others none" (fmap (`plussed` before) [ancientId, aliceWall, alicePiker, bobWall]) [5, 0, 0, 0]
    let wanted =
          Map.fromList
            [ (aliceWall, Map.singleton CounterKind.PlusOnePlusOne 3),
              (bobWall, Map.singleton CounterKind.PlusOnePlusOne 1)
            ]
        (_, _, _, _, (asked, offered), after) = begin wanted built
    -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of the prompt count.
    Spec.assertEqWith s "three counters landed on alice's Wall, one on bob's, none on the Piker, and the Ancient kept one" (fmap (`plussed` after) [ancientId, aliceWall, alicePiker, bobWall]) [1, 3, 0, 1]
    Spec.assertEqWith s "and the recipients offered were the three other creatures, bob's included and the Ancient itself not" (offered, Set.member ancientId offered) (Set.fromList [aliceWall, alicePiker, bobWall], False)
    Spec.assertEqWith s "and one distribution was asked for" asked 1
  -- The same board differing in exactly ONE thing, the answer: an allocation onto
  -- the Ancient itself, which rule 122.5's first impossibility keeps off the
  -- offered list. FILTERED, not trusted -- the counter named for it stays where it
  -- is, and the one counter the answer aims at a real recipient still crosses.
  Spec.it s "an answer naming an object that was not offered moves nothing to it" $ do
    built <- board (S.addCounter CounterKind.PlusOnePlusOne 5)
    let (ancientId, aliceWall, alicePiker, bobWall, _) = built
        wanted =
          Map.fromList
            [ (ancientId, Map.singleton CounterKind.PlusOnePlusOne 2),
              (aliceWall, Map.singleton CounterKind.PlusOnePlusOne 1)
            ]
        (_, _, _, _, _, after) = begin wanted built
    Spec.assertEqWith s "only the one counter aimed at a real recipient crossed" (fmap (`plussed` after) [ancientId, aliceWall, alicePiker, bobWall]) [4, 1, 0, 0]
  -- CR 609.3's "only as much as possible" over a group: an answer asking for more
  -- counters than the Ancient holds is clamped in the order the recipients were
  -- offered, so five counters cross and no sixth is invented.
  Spec.it s "an answer asking for more counters than the creature has moves only what is there" $ do
    built <- board (S.addCounter CounterKind.PlusOnePlusOne 5)
    let (ancientId, aliceWall, alicePiker, bobWall, _) = built
        wanted =
          Map.fromList
            [ (aliceWall, Map.singleton CounterKind.PlusOnePlusOne 4),
              (alicePiker, Map.singleton CounterKind.PlusOnePlusOne 4),
              (bobWall, Map.singleton CounterKind.PlusOnePlusOne 4)
            ]
        (_, _, _, _, _, after) = begin wanted built
    Spec.assertEqWith s "the Ancient is emptied and exactly its five counters landed" (plussed ancientId after, sum (fmap (`plussed` after) [aliceWall, alicePiker, bobWall])) (0, 5)
  -- "Any number" includes none whatever the destination looks like, so an answer
  -- allocating nothing leaves every counter where it was -- and the question was
  -- still a question.
  Spec.it s "an answer allocating nothing leaves every counter where it was" $ do
    built <- board (S.addCounter CounterKind.PlusOnePlusOne 5)
    let (ancientId, aliceWall, alicePiker, bobWall, _) = built
        (_, _, _, _, (asked, _), after) = begin Map.empty built
    Spec.assertEqWith s "the Ancient kept all five and the other creatures got none" (fmap (`plussed` after) [ancientId, aliceWall, alicePiker, bobWall]) [5, 0, 0, 0]
    Spec.assertEqWith s "and it was asked anyway, none being one of the distributions" asked 1
  -- Rule 122.5's second impossibility over a group: an Ancient bearing no counter
  -- of the named kind has nothing to spread, so no distribution is asked for
  -- however many creatures are standing beside it.
  Spec.it s "an Ancient bearing no counter asks for no distribution" $ do
    built <- board (const id)
    let (ancientId, aliceWall, alicePiker, bobWall, _) = built
        (_, _, _, _, (asked, _), after) = begin Map.empty built
    Spec.assertEqWith s "nothing moved" (fmap (`plussed` after) [ancientId, aliceWall, alicePiker, bobWall]) [0, 0, 0, 0]
    Spec.assertEqWith s "and with nothing to spread the player was not asked" asked 0
