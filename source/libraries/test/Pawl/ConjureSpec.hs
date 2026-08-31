{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Alchemy's conjure keyword action -- Pawl.Types.Conjure and
-- Pawl.Types.ConjureDestination, Pawl.Engine.Resolve's Effect.Conjure arm, and
-- Pawl.Engine.Event's conjure and mintCard (the mint CR 400.11c's wish shares,
-- which Pawl.OutsideTheGameSpec drives from the other side).
--
-- Gameplay-level throughout: the first two cases put a printed Emporium
-- Thopterist on the battlefield and begin its controller's upkeep so the printed
-- trigger fires and resolves; the third declares a printed Toralf's Disciple as
-- an attacker; the fourth enters a printed Shellfish Scholar; the fifth casts a
-- noncreature spell under a printed Lam, Storm Crane Elder. Every case CASTS or
-- otherwise uses what the conjure created, which is the point -- conjure creates
-- a CARD and not CR 111.1's token, and a token outside the battlefield would be
-- swept up by CR 111.7 first.
module Pawl.ConjureSpec where

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Zone as Zone

-- Pawl.CounterspellSpec's bitterblossomChain, which is the shape both cases
-- want: record the step's beginning, settle the trigger onto the stack, then run
-- the priority loop so it resolves.
upkeepOf :: GameState.GameState -> GameState.GameState
upkeepOf gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in settleTriggers begun

-- upkeepOf's tail on its own: settle whatever is pending onto the stack (CR
-- 603.3b) and run the priority loop until it has all resolved. The two cases
-- below need it without a step beginning, their triggers firing off an entry and
-- off a cast.
settleTriggers :: GameState.GameState -> GameState.GameState
settleTriggers gs =
  let onStack = S.runPure S.identityAnswer gs Engine.settleForPriority
   in S.runPure S.identityAnswer onStack Engine.priorityLoop

ornithopter :: CardName.CardName
ornithopter = CardName.MkCardName (Text.pack "Ornithopter")

lightningBolt :: CardName.CardName
lightningBolt = CardName.MkCardName (Text.pack "Lightning Bolt")

thinkTwice :: CardName.CardName
thinkTwice = CardName.MkCardName (Text.pack "Think Twice")

monasteryMentor :: CardName.CardName
monasteryMentor = CardName.MkCardName (Text.pack "Monastery Mentor")

islandName :: CardName.CardName
islandName = CardName.MkCardName (Text.pack "Island")

namesIn :: Zone.Zone -> GameState.GameState -> [CardName.CardName]
namesIn zone gs = fmap (\oid -> S.soleFaceName oid gs) (Game.zoneMembers zone S.alice gs)

namedIn :: CardName.CardName -> Zone.Zone -> GameState.GameState -> [ObjectId.ObjectId]
namedIn name zone gs = [oid | oid <- Game.zoneMembers zone S.alice gs, S.soleFaceName oid gs == name]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Conjure" $ do
  -- Emporium Thopterist ({1}{U} Creature -- Vedalken Artificer, "Thopters you
  -- control get +2/+0. At the beginning of your upkeep, conjure a card named
  -- Ornithopter into your hand.").
  --
  -- The Thopterist's own static ability is what makes the first assertion
  -- discriminating: Ornithopter is printed 0/2, so a 2/2 on the battlefield is
  -- the conjured card being a Thopter its controller controls, seen by layer 7c
  -- -- not merely something with the right name.
  Spec.it s "conjure puts a castable card named Ornithopter into the conjuring player's hand" $ do
    island <- S.printingOf s registry "Island"
    thopterist <- S.printingOf s registry "Emporium Thopterist"
    let (_, board) = S.addCreature thopterist S.alice (S.landsInPlay island 1)
        conjured = upkeepOf board
        inHand = namedIn ornithopter Zone.Hand conjured
        -- CR 302.1: a creature card is cast from a hand during a main phase with
        -- the stack empty, so `castable` below is asked there rather than in the
        -- upkeep the card arrived in.
        main_ = conjured {GameState.phase = Phase.PrecombatMain}
        cast_ = case inHand of
          oid : _ -> S.runPure S.identityAnswer main_ (S.cast S.alice oid >> Stack.resolveTop)
          [] -> main_
    Spec.assertEqWith
      s
      "the conjured card was cast and is a 2/2 Thopter on the battlefield"
      (fmap (\oid -> S.powerToughnessOf oid cast_) (namedIn ornithopter Zone.Battlefield cast_))
      [Just (2, 2)]
    Spec.assertEqWith
      s
      "it was a card its owner could cast"
      (fmap (\oid -> S.castable S.alice oid main_) inHand)
      [True]
    Spec.assertEqWith
      s
      "exactly one Ornithopter reached alice's hand"
      (length inHand)
      1
  -- Conjure creates the card out of nothing, so nothing is SPENT -- the half
  -- Pawl.Engine.OutsideTheGame.bringIn adds over the shared mint, where CR
  -- 400.11b keeps a wish from finding the same copy twice. Two upkeeps, two
  -- distinct Ornithopters.
  Spec.it s "a second upkeep conjures a second Ornithopter" $ do
    island <- S.printingOf s registry "Island"
    thopterist <- S.printingOf s registry "Emporium Thopterist"
    let (_, board) = S.addCreature thopterist S.alice (S.landsInPlay island 1)
        twice = upkeepOf (upkeepOf board)
    Spec.assertEqWith
      s
      "two Ornithopters, and they are two objects"
      (length (namedIn ornithopter Zone.Hand twice))
      2
  -- Toralf's Disciple ({2}{R} Creature -- Human Warrior, 3/3, "Haste. Whenever
  -- Toralf's Disciple attacks, conjure four cards named Lightning Bolt into your
  -- library, then shuffle."), which is the count and the library destination in
  -- one printed sentence.
  --
  -- bob blocks with a Goblin Piker, so combat deals him nothing and the only
  -- thing that can move his life total is the Bolt cast below -- three distinct
  -- numbers (four cards, three damage, one blocker) with no coincidence between
  -- them.
  Spec.it s "conjure four into a library puts four drawable, castable Lightning Bolts there" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    disciple <- S.printingOf s registry "Toralf's Disciple"
    let (combat, _, _) = S.combatBoardOf [disciple] [piker]
        board = S.landsFor mountain S.alice 1 combat
        attacked = S.runCombat S.aggressiveAnswer board
        inLibrary = namedIn lightningBolt Zone.Library attacked
        inHand = namedIn lightningBolt Zone.Hand attacked
        -- CR 121.1 takes the TOP card of the library, which is what makes the
        -- draw evidence about the library rather than about the mint: a card that
        -- did not reach the ordered pile cannot be drawn out of it.
        drawn = S.runPure S.identityAnswer attacked (Monad.replicateM_ 4 (Event.drawCardReturning S.alice))
        drawnBolts = namedIn lightningBolt Zone.Hand drawn
        main_ = drawn {GameState.phase = Phase.PrecombatMain}
        -- FILTERED, not hand-built: CR 608.2b re-reads the targets at resolution,
        -- and a recipient assembled here would be a different one than the prompt
        -- offered.
        targetsBob :: Prompt.Prompt r -> r
        targetsBob p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) sets
          _ -> S.identityAnswer p
        cast_ = case drawnBolts of
          oid : _ -> S.runPure targetsBob main_ (S.cast S.alice oid >> Stack.resolveTop)
          [] -> main_
    Spec.assertEqWith
      s
      "four Lightning Bolts in alice's library and none in her hand"
      (length inLibrary, length inHand)
      (4, 0)
    -- The control for the pair below: the Piker ate the attack, so nothing but
    -- the Bolt can move bob's life total.
    Spec.assertEqWith
      s
      "combat left bob's life alone"
      (S.lifeOf S.bob attacked)
      (Just 20)
    Spec.assertEqWith
      s
      "one of them was drawn and cast, so bob took its three damage"
      (S.lifeOf S.bob cast_)
      (Just 17)
    Spec.assertEqWith
      s
      "all four were drawable out of the library"
      (length drawnBolts)
      4

  -- Shellfish Scholar ({1}{U} Creature -- Rat Wizard, 2/2, "Whenever Shellfish
  -- Scholar or another Rat you control enters, conjure a card named Think Twice
  -- into your graveyard."). The Scholar is itself a Rat alice controls, so the
  -- one filter covers both halves of the printed sentence (CR 603.6a).
  --
  -- Think Twice ({1}{U} Instant, "Draw a card." / "Flashback {2}{U}") is what
  -- makes the graveyard arrival discriminating: CR 702.34a casts it FROM the
  -- graveyard, which a token could not be -- CR 111.7 would have swept it up as
  -- a state-based action long before -- and CR 702.34a's exile then moves it
  -- somewhere no other clause here could put it.
  --
  -- Pawl's Shellfish Scholar omits the printed "Threshold -- {T}: Spells you cast
  -- from your graveyard this turn cost {2} less to cast", which wants a filter
  -- over the zone a spell was CAST FROM that pawl does not have (#2799). The
  -- omission is stricter than printed: alice pays every cost in full.
  Spec.it s "conjure puts a card into the conjuring player's graveyard, castable out of it" $ do
    islandPrinting <- S.printingOf s registry "Island"
    scholar <- S.printingOf s registry "Shellfish Scholar"
    let (_, entered) = S.entersWithTrigger scholar S.alice (S.landsInPlay islandPrinting 3)
        -- Think Twice draws, and CR 104.3c would lose alice the game out from
        -- under the assertions on an empty library.
        (_, stocked) = S.addLibraryCard islandPrinting S.alice entered
        conjured = settleTriggers stocked
        inYard = namedIn thinkTwice Zone.Graveyard conjured
        -- CR 307.1: an instant is castable in a main phase as readily as
        -- anywhere, and this is where the existing cases cast from.
        main_ = conjured {GameState.phase = Phase.PrecombatMain}
        flashedBack = case inYard of
          oid : _ -> S.runPure S.identityAnswer main_ (S.cast S.alice oid >> Stack.resolveTop)
          [] -> main_
    -- The hand is read BY NAME rather than by size: a Think Twice that reached
    -- the hand instead of the graveyard would leave the size right and the name
    -- wrong.
    Spec.assertEqWith
      s
      "it was cast out of the graveyard for its flashback cost, so alice drew the Island, and CR 702.34a exiled it"
      (namesIn Zone.Hand flashedBack, namesIn Zone.Exile flashedBack)
      ([islandName], [thinkTwice])
    Spec.assertEqWith
      s
      "exactly one Think Twice reached alice's graveyard and none reached her hand"
      (length inYard, length (namedIn thinkTwice Zone.Hand conjured))
      (1, 0)
  -- Lam, Storm Crane Elder ({2}{W}{W} Legendary Creature -- Human Monk, 3/3,
  -- "Prowess. Whenever you cast a noncreature spell, conjure a card named
  -- Monastery Mentor onto the battlefield."), the one destination that is an
  -- ENTRY rather than a bare arrival.
  --
  -- Soul Warden ("Whenever another creature enters, you gain 1 life") is the
  -- witness for the entry half: it reads the Moved event
  -- Pawl.Engine.Event.recordMintedEntry files, so a conjure that placed the card
  -- without running an entry would leave alice on 20. The projected power and
  -- toughness are the other half -- the arrival is a permanent CR 613 answers
  -- for, not merely an object with the right name.
  Spec.it s "conjure onto the battlefield enters as a permanent an enters trigger sees" $ do
    islandPrinting <- S.printingOf s registry "Island"
    lam <- S.printingOf s registry "Lam, Storm Crane Elder"
    warden <- S.printingOf s registry "Soul Warden"
    twice <- S.printingOf s registry "Think Twice"
    let board0 = S.landsInPlay islandPrinting 2
        (_, board1) = S.addCreature lam S.alice board0
        (_, board2) = S.addCreature warden S.alice board1
        (spell, board3) = S.addHandCard twice S.alice board2
        (_, board4) = S.addLibraryCard islandPrinting S.alice board3
        cast_ = S.runPure S.identityAnswer (board4 {GameState.phase = Phase.PrecombatMain}) (S.cast S.alice spell)
        final = settleTriggers cast_
        mentors = namedIn monasteryMentor Zone.Battlefield final
    Spec.assertEqWith
      s
      "one Monastery Mentor entered the battlefield as a projected 2/2"
      (fmap (\oid -> S.powerToughnessOf oid final) mentors)
      [Just (2, 2)]
    Spec.assertEqWith
      s
      "Soul Warden saw it ENTER, so alice gained 1 life"
      (S.lifeOf S.alice final)
      (Just 21)
    Spec.assertEqWith
      s
      "and it reached no other zone of alice's"
      (length (namedIn monasteryMentor Zone.Hand final), length (namedIn monasteryMentor Zone.Graveyard final))
      (0, 0)
