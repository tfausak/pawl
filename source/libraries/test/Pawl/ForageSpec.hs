{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 701.61 FORAGE -- Pawl.Engine.Forage, Effect.Forage's two arms in
-- Pawl.Engine.Resolve.Effect (the executing one and effectIsImpossible's),
-- Prompt.ChooseForage, CostComponent.Forage's arms in Pawl.Engine.Cost, and the
-- GameEvent.Foraged that TriggerCondition.PlayerForages watches.
--
-- THREE FIXTURES, one per provenance rule 701.61a can be reached from. Treetop
-- Sentries for the instructed forage (below); Thornvault Forager ({1}{G}
-- Creature -- Squirrel Ranger 2/2, "{T}, Forage: Add two mana in any combination
-- of colors") for the CR 602.1a activation cost; Corpseberry Cultivator
-- ({1}{B/G}{B/G} Creature -- Squirrel Warlock 2/3, "At the beginning of combat on
-- your turn, you may forage." / "Whenever you forage, put a +1/+1 counter on this
-- creature.") for the event, its two abilities making one card both the forager
-- and the watcher.
--
-- Treetop Sentries ({3}{G} Creature -- Squirrel Archer 2/4, "Reach. When this
-- creature enters, you may forage. If you do, draw a card.") is the fixture: its
-- forage clause states nothing the rulebook does not, and the draw hanging off
-- CR 608.2c's "if you do" is what makes the ANSWER to the "may" observable
-- separately from what the forage moved.
--
-- THE BOARD SHAPE that makes the cases discriminating: alice's graveyard holds
-- FIVE cards of five different printings, so the three she exiles are three she
-- CHOSE and the two left behind are the proof -- a forage that exiled the first
-- three, or all five, reads differently. Five and not three: at exactly three
-- the choice is forced and no prompt is raised, which would make the chooser's
-- answer unobservable. The Food is a Golden Egg, the one permanent on the board
-- with the subtype, so a sacrifice cannot be mistaken for anything else leaving.
--
-- Asserted by NAME and not by ObjectId: CR 400.7 makes the exiled card a new
-- object, so the id the board handed out no longer names it. The library card is
-- a Mountain, of no printing in the graveyard, so a drawn card cannot be
-- mistaken for a card that stayed put.
module Pawl.ForageSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ForageMode as ForageMode
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- alice's board: `buried` printings in her graveyard, `foods` Golden Eggs on her
-- battlefield, one Mountain in her library to draw, and Treetop Sentries
-- entering with its CR 603.6a event alongside it.
sentriesBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Int -> GameState.GameState
sentriesBoard sentries egg library buried foods =
  let withGraveyard = List.foldl' (\gs printing -> snd (S.addGraveyardCard printing S.alice gs)) (Setup.emptyGame S.bothPlayers) buried
      withFoods = List.foldl' (\gs _ -> snd (S.addPermanent egg S.alice gs)) withGraveyard [1 .. foods]
      -- Something to draw: an empty library makes the draw a no-op and a CR
      -- 104.3c loss, which would hide whether the "if you do" clause ran.
      (_, withLibrary) = S.addLibraryCard library S.alice withFoods
   in snd (S.entersWithTrigger sentries S.alice withLibrary)

-- The entering trigger placed and resolved under one answerer, with every prompt
-- it raised recorded in order.
--
-- Recorded rather than counted: the cases below turn on WHICH question was
-- asked, and a count cannot tell a branch offer from a card chooser.
foraged :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ([Text.Text], GameState.GameState)
foraged answer gs =
  let recording :: Prompt.Prompt r -> State.State [Text.Text] r
      recording p = do
        State.modify (<> [S.promptKind p])
        pure (answer p)
      (after, asked) = State.runState (Engine.runGame recording gs (Engine.placePendingTriggers *> Stack.resolveTop)) []
   in (asked, snd after)

-- Take the "may", and exile the graveyard cards at these positions in the
-- offered list. PINNED by position rather than searched: an answerer that took
-- whatever was legal would find three cards again after a mutation and keep the
-- case green.
takingExiles :: [Int] -> Prompt.Prompt r -> r
takingExiles positions p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.ChooseForage {} -> ForageMode.ExileCards
  Prompt.ChooseExilesFromGraveyard _ _ _ candidates _ -> Set.fromList (fmap (candidates !!) positions)
  _ -> S.identityAnswer p

-- Take the "may" and the Food half of rule 701.61a.
takingFood :: Prompt.Prompt r -> r
takingFood p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.ChooseForage {} -> ForageMode.SacrificeFood
  _ -> S.identityAnswer p

-- The names of the cards in one of alice's zones, sorted.
namesIn :: Zone.Zone -> GameState.GameState -> [CardName.CardName]
namesIn zone gs = List.sort (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone S.alice gs))

-- The five graveyard printings, and the two of them a forage taking positions
-- 0, 2 and 4 leaves behind.
names :: [Printing.Printing] -> [CardName.CardName]
names = List.sort . fmap S.printingName

-- Decline the "may" and answer nothing else: the paired board for the
-- Corpseberry Cultivator cases, differing from `takingExiles` in the one answer.
decliningForage :: Prompt.Prompt r -> r
decliningForage p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  _ -> S.identityAnswer p

-- alice, active and holding priority in her beginning of combat step (CR 506.1,
-- rule 507) with `buried` printings in her graveyard and one Corpseberry
-- Cultivator on the battlefield. Staged directly, Pawl.CardTriggerSpec's Ezuri
-- posture: Engine.runStep is what writes the CR 603.2b StepBegan record the
-- card's first ability matches.
cultivatorBoard :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
cultivatorBoard cultivator buried =
  let withGraveyard = List.foldl' (\acc printing -> snd (S.addGraveyardCard printing S.alice acc)) (Setup.emptyGame S.bothPlayers) buried
      (oid, gs) = S.addPermanent cultivator S.alice withGraveyard
   in ( oid,
        gs
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- alice, active and holding priority in her precombat main phase with `buried`
-- printings in her graveyard and one Thornvault Forager on the battlefield,
-- settled and untapped. Pawl.ManaSpec's Phyrexian Tower board: the only mana
-- that can reach her pool is mana she activated a mana ability for, and the
-- Forager's plain "{T}: Add {G}" keeps it on the menu whether or not its forage
-- ability can be paid for.
foragerBoard :: Printing.Printing -> [Printing.Printing] -> GameState.GameState
foragerBoard forager buried =
  let withGraveyard = List.foldl' (\acc printing -> snd (S.addGraveyardCard printing S.alice acc)) (Setup.emptyGame S.bothPlayers) buried
      gs = snd (S.addPermanent forager S.alice withGraveyard)
   in gs {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}

-- The mana types in a player's pool, in the order the units went in.
poolTypes :: GameState.GameState -> [ManaType.ManaType]
poolTypes gs = case Game.poolOf S.alice gs of
  Mana.Type.MkMana units -> fmap ManaUnit.manaType units

-- Take any mana activation offered, ask for a yield of one blue and one red --
-- which only Thornvault Forager's forage ability can produce -- and pay the
-- forage by exiling the graveyard cards at these positions.
--
-- TWO DIFFERENT COLOURS, which is what "add two mana in any combination of
-- colors" means and what a single AnyColor instruction of count two could not
-- offer. The yield falls back to the candidates' own head where it is not on
-- offer (S.optionYielding), which is the negative board's whole difference.
tappingForTwoColors :: [Int] -> Prompt.Prompt r -> r
tappingForTwoColors positions p = case p of
  Prompt.ChooseAction _ _ actions -> case filter isManaActivation actions of
    h : _ -> h
    [] -> Action.Type.Pass
  Prompt.ChooseManaYield _ _ _ candidates -> S.optionYielding (Mana.Type.MkMana [unitOf Color.Blue, unitOf Color.Red]) candidates
  _ -> takingExiles positions p

unitOf :: Color.Color -> ManaUnit.ManaUnit
unitOf color =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = ManaType.Colored color,
      ManaUnit.tags = Set.empty,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction = Nothing,
      ManaUnit.rider = Nothing,
      ManaUnit.sourceChosenSubtype = Nothing
    }

-- CR 605.1a's activations alone, Pawl.ManaSpec's discriminator: the priority
-- window offers nothing else on these boards, and Pass ends the loop.
isManaActivation :: Action.Type.Action -> Bool
isManaActivation action = case action of
  Action.Type.ActivateManaAbility _ -> True
  _ -> False

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Forage" $ do
  Spec.it s "CR 701.61a foraging exiles the three cards the forager chose" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    cow <- S.printingOf s registry "Bartered Cow"
    let gs = sentriesBoard sentries egg mountain [forest, spider, piker, bolt, cow] 0
        (asked, after) = foraged (takingExiles [0, 2, 4]) gs
    -- The gameplay reading first, and both directions of it: exactly the three
    -- chosen cards left the graveyard for exile, and the two she kept did not.
    Spec.assertEqWith s "CR 701.61a the three chosen cards are in exile" (namesIn Zone.Exile after) (names [forest, piker, cow])
    Spec.assertEqWith s "CR 701.61a the two the forager kept are still in the graveyard" (namesIn Zone.Graveyard after) (names [spider, bolt])
    -- CR 608.2c's "if you do": the forage happened, so the draw did.
    Spec.assertEqWith s "CR 608.2c the draw hanging off the forage happened" (S.handSize S.alice after) 1
    Spec.assertBool s (elem (Text.pack "ChooseExilesFromGraveyard") asked) "CR 701.61a five candidates for three cards: the forager chose"
    -- Nothing to ask about the branch: she controls no Food.
    Spec.assertBool s (notElem (Text.pack "ChooseForage") asked) "CR 608.2d with no Food there is no branch to offer"
  Spec.it s "CR 701.61a a forager who cannot exile three cards sacrifices the Food" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    let gs = sentriesBoard sentries egg mountain [forest, spider] 1
        (asked, after) = foraged (takingExiles [0]) gs
    -- The Egg in the graveyard beside the two untouched cards is the whole
    -- action: CR 701.21a's sacrifice happened and rule 701.61a's exile did not.
    Spec.assertEqWith s "CR 701.21a the Food was sacrificed and the two graveyard cards stayed" (namesIn Zone.Graveyard after) (names [forest, spider, egg])
    Spec.assertEqWith s "CR 701.61a nothing was exiled" (namesIn Zone.Exile after) []
    Spec.assertEqWith s "CR 608.2c the draw hanging off the forage happened" (S.handSize S.alice after) 1
    Spec.assertBool s (notElem (Text.pack "ChooseForage") asked) "CR 608.2d one half cannot be carried out, so there is no branch to offer"
    Spec.assertBool s (notElem (Text.pack "ChooseExilesFromGraveyard") asked) "and no cards were offered for exile"
  Spec.it s "CR 701.61a with both halves available the forager's answer decides which" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    cow <- S.printingOf s registry "Bartered Cow"
    let gs = sentriesBoard sentries egg mountain [forest, spider, piker, bolt, cow] 1
        (asked, exiling) = foraged (takingExiles [0, 1, 2]) gs
        (_, eating) = foraged takingFood gs
    Spec.assertBool s (elem (Text.pack "ChooseForage") asked) "CR 701.61a both halves can be carried out, so the forager is asked which"
    -- One board, two answers, opposite boards afterwards.
    Spec.assertEqWith s "CR 701.61a exiling takes the three cards and leaves the Food alone" (namesIn Zone.Exile exiling) (names [forest, spider, piker])
    Spec.assertEqWith s "CR 701.61a and sacrificing leaves the graveyard alone" (namesIn Zone.Graveyard eating) (names [forest, spider, piker, bolt, cow, egg])
    Spec.assertEqWith s "CR 701.21a the sacrifice took the Food" (S.countOnBattlefieldByName (S.printingName egg) S.alice eating) 0
    Spec.assertEqWith s "CR 701.61a and the exile left it on the battlefield" (S.countOnBattlefieldByName (S.printingName egg) S.alice exiling) 1
  Spec.it s "CR 608.2d a forager who can do neither half is not offered the forage" $ do
    sentries <- S.printingOf s registry "Treetop Sentries"
    egg <- S.printingOf s registry "Golden Egg"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    let gs = sentriesBoard sentries egg mountain [forest, spider] 0
        (asked, after) = foraged (takingExiles [0]) gs
    Spec.assertEqWith s "CR 608.2d nothing was drawn, so the forage never happened" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and the graveyard is untouched" (namesIn Zone.Graveyard after) (names [forest, spider])
    Spec.assertBool s (notElem (Text.pack "ChooseOptional") asked) "CR 608.2d an impossible option is not offered"

  -- CR 701.61a's OTHER provenance, and the event that records it. The module
  -- haddock's fixture is Treetop Sentries, whose forage an effect instructs;
  -- these two cases drive the cost (Thornvault Forager) and the trigger
  -- (Corpseberry Cultivator) instead.
  Spec.it s "CR 601.2h / 602.1a a forage paid as an activation cost exiles the three cards the forager chose" $ do
    forager <- S.printingOf s registry "Thornvault Forager"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    cow <- S.printingOf s registry "Bartered Cow"
    let rich = foragerBoard forager [forest, spider, piker, bolt, cow]
        -- The paired board, differing in ONE thing: two cards in the graveyard
        -- rather than five, so rule 701.61a's exile half cannot be carried out
        -- and no Food stands in for it.
        poor = foragerBoard forager [forest, spider]
        run gs = S.runPure (tappingForTwoColors [0, 2, 4]) gs Engine.priorityLoop
    -- The gameplay reading first: the forage ability was payable, so two mana of
    -- the forager's OWN two colours reached the pool.
    Spec.assertEqWith s "CR 106.3 two mana in a combination of colors alice chose" (poolTypes (run rich)) [ManaType.Colored Color.Blue, ManaType.Colored Color.Red]
    Spec.assertEqWith s "CR 608.2d with two cards and no Food the cost is unpayable, so only the printed {G} is on offer" (poolTypes (run poor)) [ManaType.Colored Color.Green]
    -- CR 601.2h's payment, both directions of it.
    Spec.assertEqWith s "CR 701.61a the three chosen cards paid for it" (namesIn Zone.Exile (run rich)) (names [forest, piker, cow])
    Spec.assertEqWith s "CR 701.61a and the two the forager kept are still in the graveyard" (namesIn Zone.Graveyard (run rich)) (names [spider, bolt])
    Spec.assertEqWith s "CR 608.2d and the unpayable board spent nothing" (namesIn Zone.Graveyard (run poor)) (names [forest, spider])
  Spec.it s "CR 701.61a a forage records the event a \"whenever you forage\" trigger watches" $ do
    cultivator <- S.printingOf s registry "Corpseberry Cultivator"
    forest <- S.printingOf s registry "Forest"
    spider <- S.printingOf s registry "Giant Spider"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    cow <- S.printingOf s registry "Bartered Cow"
    let (cultivatorId, board) = cultivatorBoard cultivator [forest, spider, piker, bolt, cow]
        run :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
        run answer = S.runPure answer board (Engine.runStep *> Engine.priorityLoop)
        foraged' = run (takingExiles [0, 2, 4])
        -- One board, two answers: the "may" taken and the "may" declined.
        declined = run decliningForage
    -- The gameplay reading first: the second ability triggered off the forage and
    -- put its counter on, which the Cultivator's printed 2/3 reading 3/4 is.
    Spec.assertEqWith s "CR 701.61a the forage triggered the counter, so the printed 2/3 reads 3/4" (S.powerToughnessOf cultivatorId foraged') (Just (3, 4))
    Spec.assertEqWith s "and declining the may leaves it the printed 2/3" (S.powerToughnessOf cultivatorId declined) (Just (2, 3))
    Spec.assertEqWith s "CR 701.61a the three chosen cards were exiled" (namesIn Zone.Exile foraged') (names [forest, piker, cow])
    Spec.assertEqWith s "and the declined board exiled nothing" (namesIn Zone.Exile declined) []
