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
-- an attacker; the fourth and fifth enter a printed Shellfish Scholar; the sixth
-- casts a noncreature spell under a printed Lam, Storm Crane Elder; the seventh
-- activates a printed Tome of the Infinite, whose card file writes a printed
-- SPELLBOOK rather than one card.
--
-- The first four CAST what the conjure created, which is the point -- conjure
-- creates a CARD and not CR 111.1's token, and a token in a hand, a library or a
-- graveyard would be swept up by CR 111.7 before any cast. The BATTLEFIELD case
-- proves the entry rather than the cardness: rule 111.7 sweeps up nothing there,
-- so no board of that shape tells a conjured card from a token. The fifth is the
-- graveyard arrival's SHAPE rather than its cardness, read off a printed Planar
-- Void that watches the graveyard.
module Pawl.ConjureSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
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
namedIn name zone gs = filter (\oid -> S.soleFaceName oid gs == name) (Game.zoneMembers zone S.alice gs)

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
    let (_, board) = S.addPermanent thopterist S.alice (S.landsInPlay island 1)
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
  -- Pawl.Engine.Event.bringIn adds over the shared mint, where CR
  -- 400.11b keeps a wish from finding the same copy twice. Two upkeeps, two
  -- distinct Ornithopters.
  Spec.it s "a second upkeep conjures a second Ornithopter" $ do
    island <- S.printingOf s registry "Island"
    thopterist <- S.printingOf s registry "Emporium Thopterist"
    let (_, board) = S.addPermanent thopterist S.alice (S.landsInPlay island 1)
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
  -- The Scholar's other printed line -- "Threshold -- {T}: Spells you cast from
  -- your graveyard this turn cost {2} less to cast. Activate only if seven or
  -- more cards are in your graveyard." -- is carried too, and this case does not
  -- reach it: three Islands pay Think Twice's {2}{U} flashback with no reduction,
  -- and the Scholar is never activated here. Pawl.ActivateSpec's
  -- PrintedActivationThresholdReduction group is what proves that half. The
  -- reduction's filter is Filter.WasCastFrom, proved by Pawl.PlayerEffectSpec's
  -- Patrician Geist group.
  Spec.it s "conjure puts a card into the conjuring player's graveyard, castable out of it" $ do
    islandPrinting <- S.printingOf s registry "Island"
    scholar <- S.printingOf s registry "Shellfish Scholar"
    let (_, entered) = S.entersWithTrigger scholar S.alice (S.landsInPlay islandPrinting 3)
        -- Think Twice draws, and CR 104.3c would lose alice the game out from
        -- under the assertions on an empty library.
        (_, stocked) = S.addLibraryCard islandPrinting S.alice entered
        conjured = settleTriggers stocked
        inYard = namedIn thinkTwice Zone.Graveyard conjured
        -- CR 304.1: an instant needs only priority, so the main phase is where
        -- the cases above cast from rather than something this one requires.
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
  -- The same conjure watched by a bystander, which is what makes the arrival's
  -- SHAPE observable: Planar Void ({B} Enchantment, "Whenever another card is put
  -- into a graveyard from anywhere, exile that card"), Oracle text verified on
  -- Scryfall 2026-09-10. Pawl.ZoneTriggerSpec's Planar Void group is where it
  -- fires on a card put into a graveyard from a HAND.
  --
  -- One board, two Think Twices, differing in exactly one thing -- how each
  -- reached the graveyard. The conjured one is materialized there in no zone
  -- change, so CR 603.6's zone-change trigger has no event to match (CR 400.6)
  -- and the Void leaves it alone; the cast one is put there from the stack and
  -- the Void exiles it. The second assertion is what keeps the first from
  -- passing because the Void is dead on this board.
  Spec.it s "CR 603.6 a conjure into a graveyard is no zone change, so a graveyard trigger does not see it" $ do
    islandPrinting <- S.printingOf s registry "Island"
    scholar <- S.printingOf s registry "Shellfish Scholar"
    planarVoid <- S.printingOf s registry "Planar Void"
    twicePrinting <- S.printingOf s registry "Think Twice"
    let (_, withVoid) = S.addPermanent planarVoid S.alice (S.landsInPlay islandPrinting 3)
        (handCard, withHand) = S.addHandCard twicePrinting S.alice withVoid
        -- Think Twice draws, and CR 104.3c would lose alice the game out from
        -- under the assertions on an empty library.
        (_, stocked) = S.addLibraryCard islandPrinting S.alice withHand
        (_, entered) = S.entersWithTrigger scholar S.alice stocked
        conjured = settleTriggers entered
        inYard = namedIn thinkTwice Zone.Graveyard conjured
        -- CR 304.1: an instant needs only priority, and the cast is what puts the
        -- SECOND Think Twice into the graveyard out of a zone the card was in.
        main_ = conjured {GameState.phase = Phase.PrecombatMain}
        cast_ = settleTriggers (S.runPure S.identityAnswer main_ (S.cast S.alice handCard >> Stack.resolveTop))
    Spec.assertEqWith
      s
      "CR 603.6 the Void saw no arrival, so the conjured Think Twice stayed in the graveyard and nothing was exiled"
      (length inYard, namesIn Zone.Exile conjured)
      (1, [])
    Spec.assertEqWith
      s
      "CR 603.6 the CAST one was put into the graveyard from the stack, and the same Void exiled that one alone"
      (namedIn thinkTwice Zone.Graveyard cast_, length (namedIn thinkTwice Zone.Exile cast_))
      (inYard, 1)
  -- Lam, Storm Crane Elder ({2}{W}{W} Legendary Creature -- Human Monk, 3/3,
  -- "Prowess. Whenever you cast a noncreature spell, conjure a card named
  -- Monastery Mentor onto the battlefield."), the one destination that is an
  -- ENTRY rather than a bare arrival.
  --
  -- Three assertions for the three halves of an entry the other destinations do
  -- not have. Soul Warden ("Whenever another creature enters, you gain 1 life")
  -- reads the Moved event Pawl.Engine.Event.recordMintedEntry files, so a
  -- conjure that placed the card without announcing an entry leaves alice on 20
  -- (CR 603.6a). Bob's Kismet ("Artifacts, creatures, and lands your opponents
  -- control enter tapped") is a replacement effect the arrival only meets inside
  -- CR 616.1's loop, which is what runEntry runs. The projected power and
  -- toughness are the third -- the arrival is a permanent CR 613 answers for,
  -- not merely an object with the right name.
  --
  -- Kismet is BOB's, so it reaches the conjured Mentor and nothing alice's
  -- fixture placed; addPermanent arranges a board rather than entering anything.
  Spec.it s "conjure onto the battlefield enters as a permanent, seen by an enters trigger and an entry replacement" $ do
    islandPrinting <- S.printingOf s registry "Island"
    lam <- S.printingOf s registry "Lam, Storm Crane Elder"
    warden <- S.printingOf s registry "Soul Warden"
    kismet <- S.printingOf s registry "Kismet"
    twice <- S.printingOf s registry "Think Twice"
    let board0 = S.landsInPlay islandPrinting 2
        (_, board1) = S.addPermanent lam S.alice board0
        (_, board2) = S.addPermanent warden S.alice board1
        (_, board3) = S.addPermanent kismet S.bob board2
        (spell, board4) = S.addHandCard twice S.alice board3
        -- Think Twice draws, and CR 104.3c would lose alice the game out from
        -- under the assertions on an empty library.
        (_, board5) = S.addLibraryCard islandPrinting S.alice board4
        cast_ = S.runPure S.identityAnswer (board5 {GameState.phase = Phase.PrecombatMain}) (S.cast S.alice spell)
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
      "CR 616.1 ran over the arrival, so bob's Kismet had it enter tapped"
      (fmap (\oid -> fmap Object.tapped (Game.lookupObject oid final)) mentors)
      [Just TapState.Tapped]
    Spec.assertEqWith
      s
      "and it reached no other zone of alice's"
      (length (namedIn monasteryMentor Zone.Hand final), length (namedIn monasteryMentor Zone.Graveyard final))
      (0, 0)
  -- Tome of the Infinite ({2}{U} Legendary Artifact -- Book, "{U}, {T}: Conjure
  -- a random card from Tome of the Infinite's spellbook into your hand."), the
  -- printed SPELLBOOK: ten candidates in the card file and one pick over them.
  --
  -- Not implemented: the rider, "It perpetually gains 'You may spend mana as
  -- though it were mana of any color to cast this spell.'" A conjure binds its
  -- card to no slot, so no later clause can name it; pawl's Tome is stricter
  -- than the printing, never weaker (#2638).
  --
  -- Not implemented: the spellbook's Ponder puts the three cards it looks at
  -- back in the order they were in, where the printing lets its controller
  -- reorder them. Stricter, and the optional shuffle beside it is written
  -- (#3649).
  --
  -- The answerer pins the pick to the LAST candidate, which the offered list's
  -- head is not: Pawl.Engine.Replay.defaultAnswer takes the head, so an engine
  -- that rolled the pick itself rather than honouring the answer lands on
  -- Assault Strobe.
  Spec.it s "a printed spellbook is offered whole, and the card randomness named is the one conjured" $ do
    islandPrinting <- S.printingOf s registry "Island"
    tome <- S.printingOf s registry "Tome of the Infinite"
    let board0 = S.landsInPlay islandPrinting 1
        (tomeId, board1) = S.addPermanent tome S.alice board0
        board = board1 {GameState.phase = Phase.PrecombatMain}
        logging :: Prompt.Prompt r -> State.State [[CardName.CardName]] r
        logging p = case p of
          Prompt.RandomCard offered -> do
            State.modify' (NonEmpty.toList offered :)
            pure (tomeAnswer tomeId swordsToPlowshares p)
          _ -> pure (tomeAnswer tomeId swordsToPlowshares p)
        (offers, final) = case State.runState (Engine.runGame logging board Engine.priorityLoop) [] of
          ((_, gs), asked) -> (reverse asked, gs)
    -- THE GAMEPLAY ASSERTION: the card in alice's hand is the one the answer
    -- named, and it is a card of the spellbook rather than of her deck.
    Spec.assertEqWith
      s
      "the conjured card in alice's hand is the one randomness named"
      (namesIn Zone.Hand final)
      [swordsToPlowshares]
    -- Supporting, and LAST so it cannot absorb a mutation the assertion above
    -- should catch: the whole spellbook was offered, once, in the card file's
    -- order. Recorded off the prompt, since the candidate list is not readable
    -- off the resulting board.
    Spec.assertEqWith
      s
      "asked once, offering every card of the printed spellbook"
      offers
      [tomeSpellbook]

-- The ten cards data/cards/tome-of-the-infinite.json prints as the Tome's
-- spellbook, in the order the card file writes them.
tomeSpellbook :: [CardName.CardName]
tomeSpellbook =
  fmap
    (CardName.MkCardName . Text.pack)
    [ "Assault Strobe",
      "Dark Ritual",
      "Duress",
      "Fog",
      "Force Spike",
      "Giant Growth",
      "Lightning Bolt",
      "Light of Hope",
      "Ponder",
      "Swords to Plowshares"
    ]

swordsToPlowshares :: CardName.CardName
swordsToPlowshares = CardName.MkCardName (Text.pack "Swords to Plowshares")

-- Taps the Island for {U}, activates the Tome the first time its ability is
-- offered -- once, since the activation taps it -- and pins the random pick to
-- `who`. FILTERED out of the offered candidates rather than built, so a name
-- the engine never offered cannot slip through, falling back to the head.
tomeAnswer :: ObjectId.ObjectId -> CardName.CardName -> Prompt.Prompt r -> r
tomeAnswer tome who p = case p of
  Prompt.ChooseAction _ _ actions -> case List.find (activationOf tome) actions of
    Just action -> action
    Nothing -> case List.find manaActivation actions of
      Just action -> action
      Nothing -> Action.Pass
  Prompt.RandomCard offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== who) (NonEmpty.toList offered))
  _ -> S.identityAnswer p

activationOf :: ObjectId.ObjectId -> Action.Action -> Bool
activationOf oid action = case action of
  Action.Activate o _ -> o == oid
  _ -> False

manaActivation :: Action.Action -> Bool
manaActivation action = case action of
  Action.ActivateManaAbility _ -> True
  _ -> False
