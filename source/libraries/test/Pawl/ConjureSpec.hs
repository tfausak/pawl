{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Alchemy's conjure keyword action -- Pawl.Types.Conjure,
-- Pawl.Types.ConjureDestination and Pawl.Types.ConjureSelection,
-- Pawl.Engine.Resolve's Effect.Conjure arm, and Pawl.Engine.Event's conjure and
-- mintCard (the mint CR 400.11c's wish shares, which Pawl.OutsideTheGameSpec
-- drives from the other side).
--
-- Gameplay-level throughout: the first two cases put a printed Emporium
-- Thopterist on the battlefield and begin its controller's upkeep so the printed
-- trigger fires and resolves; the third declares a printed Toralf's Disciple as
-- an attacker; the fourth and fifth enter a printed Shellfish Scholar; the sixth
-- casts a noncreature spell under a printed Lam, Storm Crane Elder; the seventh
-- activates a printed Tome of the Infinite, whose card file writes a printed
-- SPELLBOOK rather than one card; the eighth casts a printed Follow the Tracks,
-- the same spellbook shape with the other question asked of it; the ninth enters
-- a printed Foundry Groundbreaker, whose conjure STATES the status its arrivals
-- take; the tenth to thirteenth cast a printed Sinister Reflections, whose
-- conjure names an object already in the game rather than writing its card out;
-- the last two begin alice's second main phase under a printed Pearl Collector,
-- the one conjure in the corpus behind CR 603.4's intervening "if".
--
-- The first four CAST what the conjure created, which is the point -- conjure
-- creates a CARD and not CR 111.1's token, and a token in a hand, a library or a
-- graveyard would be swept up by CR 111.7 before any cast. The BATTLEFIELD case
-- proves the entry rather than the cardness: rule 111.7 sweeps up nothing there,
-- so no board of that shape tells a conjured card from a token. The fifth is the
-- graveyard arrival's SHAPE rather than its cardness, read off a printed Planar
-- Void that watches the graveyard.
--
-- The DUPLICATE cases are the other axis: the first of them reads the duplicates
-- once the originals are in the graveyard, which is what tells a card from CR
-- 707.1's token, and the second points the conjure at a Clone, which is where
-- the printed card under an object and its CR 707.2 copiable values disagree.
-- The third carries that duplicate through three zone changes (CR 400.7), and
-- the fourth prices it off its copiable mana cost.
module Pawl.ConjureSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
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
import qualified Pawl.Types.Regenerability as Regenerability
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

mishrasFoundry :: CardName.CardName
mishrasFoundry = CardName.MkCardName (Text.pack "Mishra's Foundry")

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
        -- CR 117.1a: an instant needs only priority, so the main phase is where
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
  -- Foundry Groundbreaker ({3}{G} Creature -- Human Artificer, 3/4, "When
  -- Foundry Groundbreaker enters, sacrifice a land. Then conjure two cards named
  -- Mishra's Foundry onto the battlefield tapped."), Oracle text verified on
  -- Scryfall 2026-09-21. The same entry the Lam case drives, with the one thing
  -- that arm could not say until now: CR 110.5b's status, STATED by the sentence.
  --
  -- The gameplay assertion is the mana, not the flag. A land is tapped for mana
  -- (CR 605.1a), so two Foundries that arrived untapped would be two more
  -- Action.ActivateManaAbility offers in alice's menu; the surviving Island is
  -- what says the menu is read at all and that the board is one where a land CAN
  -- be tapped. The status read off the objects is beside it rather than instead
  -- of it.
  --
  -- TWO conjured, which is the count and the status together: a batch is minted
  -- whole before any member enters, so a status applied to the arrival rather
  -- than to the mint could reach one of them and not the other.
  --
  -- The sacrifice is the rest of the printed trigger, and alice is left one
  -- Island of the two: an effect list that stopped at the conjure would leave
  -- both.
  Spec.it s "CR 110.5b a conjure that states tapped puts the card onto the battlefield tapped" $ do
    islandPrinting <- S.printingOf s registry "Island"
    groundbreaker <- S.printingOf s registry "Foundry Groundbreaker"
    let board0 = S.landsInPlay islandPrinting 2
        (_, entered) = S.entersWithTrigger groundbreaker S.alice board0
        final = (settleTriggers entered) {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain}
        foundries = namedIn mishrasFoundry Zone.Battlefield final
        islands = namedIn islandName Zone.Battlefield final
        manaOffers = Maybe.mapMaybe manaSource (Action.legalActions S.alice final)
    Spec.assertEqWith
      s
      "CR 605.1a neither conjured Mishra's Foundry can be tapped for mana, where the Island alice kept can"
      (filter (`elem` foundries) manaOffers, filter (`elem` islands) manaOffers)
      ([], islands)
    Spec.assertEqWith
      s
      "both of them arrived tapped"
      (fmap (\oid -> fmap Object.tapped (Game.lookupObject oid final)) foundries)
      [Just TapState.Tapped, Just TapState.Tapped]
    Spec.assertEqWith
      s
      "the trigger's own sacrifice ran, so one of alice's two Islands is gone"
      (length islands)
      1
  -- Tome of the Infinite ({2}{U} Legendary Artifact -- Book, "{U}, {T}: Conjure
  -- a random card from Tome of the Infinite's spellbook into your hand."), the
  -- printed SPELLBOOK: ten candidates in the card file and one pick over them.
  --
  -- Not implemented: the rider, "It perpetually gains 'You may spend mana as
  -- though it were mana of any color to cast this spell.'" A conjure binds its
  -- card to no slot, so no later clause can name it; pawl's Tome is stricter
  -- than the printing, never weaker (#3971).
  --
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
  -- Follow the Tracks ({2}{G} Sorcery, "Conjure a card of your choice from
  -- Follow the Tracks's spellbook onto the battlefield."), Oracle text verified
  -- on Scryfall 2026-09-14. The Tome's case one group above with the other
  -- question asked: the same printed spellbook shape, picked BY CHOICE.
  --
  -- Not implemented: each of the five Gates prints "{3}{C}, {T}: Seek a nonland
  -- card. Activate only once." No effect seeks, so pawl's Gates are stricter
  -- than the printing, never weaker (#3734).
  --
  -- The answerer pins the pick to Gate to Seatower, which the offered list's
  -- head is not: Pawl.Engine.Replay.defaultAnswer takes the head, so an engine
  -- that picked for alice -- or asked for randomness here, whose default answer
  -- is also the head -- lands on Gate of the Black Dragon instead.
  Spec.it s "a printed spellbook picked by choice is offered whole, and the card its controller named is the one conjured" $ do
    forest <- S.printingOf s registry "Forest"
    tracks <- S.printingOf s registry "Follow the Tracks"
    let (spell, board0) = S.addHandCard tracks S.alice (S.landsInPlay forest 3)
        board = board0 {GameState.phase = Phase.PrecombatMain}
        logging :: Prompt.Prompt r -> State.State [[CardName.CardName]] r
        logging p = case p of
          Prompt.ChooseConjuredCard _ _ offered -> do
            State.modify' (NonEmpty.toList offered :)
            pure (tracksAnswer gateToSeatower p)
          _ -> pure (tracksAnswer gateToSeatower p)
        (offers, final) = case State.runState (Engine.runGame logging board (S.cast S.alice spell >> Stack.resolveTop)) [] of
          ((_, gs), asked) -> (reverse asked, gs)
        gates = filter (/= forestName) (namesIn Zone.Battlefield final)
    -- THE GAMEPLAY ASSERTION: the Gate on the battlefield is the one alice's
    -- answer named, and no other member of the spellbook came with it.
    Spec.assertEqWith
      s
      "the conjured Gate on the battlefield is the one alice chose"
      gates
      [gateToSeatower]
    -- The conjured card carries its own printed text rather than just its name:
    -- the Gate's "enters the battlefield tapped" is CR 614.1d's replacement
    -- effect, and it applied to the arrival.
    Spec.assertEqWith
      s
      "and it entered tapped, as its own printed replacement effect says"
      (fmap (\oid -> fmap Object.tapped (Game.lookupObject oid final)) (namedIn gateToSeatower Zone.Battlefield final))
      [Just TapState.Tapped]
    -- Supporting, and LAST so it cannot absorb a mutation the assertions above
    -- should catch: the whole spellbook was offered, once, in the card file's
    -- order.
    Spec.assertEqWith
      s
      "asked once, offering every card of the printed spellbook"
      offers
      [tracksSpellbook]
  -- Sinister Reflections ({1}{U} Instant, "Conjure a duplicate of each of up to
  -- two target nontoken creatures you control into your hand."), Oracle text
  -- verified on Scryfall 2026-09-21. The conjure whose card is no longer written
  -- out in the card file: the opcode names an object already in the game and
  -- takes the card from there.
  --
  -- TWO DIFFERENT creatures targeted, which is what says the duplicate is read
  -- per named object: one card taken twice would leave two of one name.
  --
  -- The originals are DESTROYED after the spell resolves, and the assertions are
  -- read off that board. A CreateCopy token whose copiable values point at the
  -- original would have nothing left to point at; a conjured duplicate is a card
  -- of its own and outlives it.
  Spec.it s "a conjured duplicate of each targeted creature reaches the hand and outlives the original" $ do
    islandPrinting <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    reflections <- S.printingOf s registry "Sinister Reflections"
    -- Enough red mana left over to CAST both duplicates once the two Islands
    -- have paid for the instant, which the last assertion asks for.
    let board0 = S.landsFor mountain S.alice 6 (S.landsInPlay islandPrinting 2)
        (pikerId, board1) = S.addPermanent piker S.alice board0
        (giantId, board2) = S.addPermanent giant S.alice board1
        (spell, board3) = S.addHandCard reflections S.alice board2
        board = board3 {GameState.phase = Phase.PrecombatMain}
        resolved = S.runPure (aimingAtAll [pikerId, giantId]) board (S.cast S.alice spell >> Stack.resolveTop)
        -- CR 704.5g's destroy, driven directly: the point is the board AFTER the
        -- originals are gone, and nothing about how they went.
        gone = S.settleSba (S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [pikerId, giantId]))
        duplicates = List.sort (filter (`notElem` [islandName, mountainName]) (namesIn Zone.Hand gone))
    Spec.assertEqWith
      s
      "one duplicate of each targeted creature is in alice's hand once both originals are in the graveyard"
      (duplicates, List.sort (namesIn Zone.Graveyard gone))
      ([goblinPiker, hillGiant], [goblinPiker, hillGiant, sinisterReflections])
    -- Cardness, the first four cases' own assertion: a duplicate is castable out
    -- of the hand it landed in, where CR 111.7 would have swept up a token.
    Spec.assertEqWith
      s
      "each duplicate is a card its owner could cast"
      (fmap (\oid -> S.castable S.alice oid gone) (namedIn goblinPiker Zone.Hand gone <> namedIn hillGiant Zone.Hand gone))
      [True, True]
  -- The COPIABLE-VALUES tripwire. A Clone (CR 707.2) is a printed Clone whose
  -- copiable values are the Piker's, so the two roads a duplicate could take
  -- answer different names: the printed card under the object says Clone, and
  -- its copiable values say Goblin Piker.
  --
  -- READ THROUGH THE PROJECTION, which is where CR 707.2's copiable values live
  -- (CR 613.1a): the duplicate's printed card still says Clone -- it is minted
  -- off the named object's printing -- and every characteristic a rule asks of it
  -- says Goblin Piker. The two are asserted side by side, so a duplicate that
  -- took the printed card instead would fail on the projection while the
  -- printed-name read went on passing.
  --
  -- The Piker is DEAD when the assertions are read, which is what makes this a
  -- duplicate rather than CreateCopy's token: a snapshot is a value (CR 707.2b)
  -- and does not go looking for the object it came from.
  --
  -- The next case casts the duplicate out of that hand.
  Spec.it s "CR 707.2 a duplicate of a Clone is a duplicate of what the Clone copies" $ do
    islandPrinting <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    let board0 = S.landsFor mountain S.alice 3 (S.landsInPlay islandPrinting 2)
        (pikerId, board1) = S.addPermanent piker S.alice board0
        -- Pawl.CopySpec's road onto the battlefield: the Clone is RESOLVED, with
        -- CR 614.12's as-enters copy answered by naming the Piker, so it is on
        -- the battlefield as a copy rather than as the 0/0 CR 704.5f sweeps up.
        (_, staged) = S.spellOnStack clone S.alice board1
        entered = S.settleSba (copyingPiker pikerId staged)
    case clonesOnBattlefield entered of
      [] -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      cloneId : _ -> do
        let (spell, board2) = S.addHandCard reflections S.alice entered
            board = board2 {GameState.phase = Phase.PrecombatMain}
            resolved = S.runPure (aimingAtAll [cloneId]) board (S.cast S.alice spell >> Stack.resolveTop)
            gone = S.settleSba (S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [pikerId, cloneId]))
            duplicates = filter (\oid -> S.soleFaceName oid gone `notElem` [islandName, mountainName]) (Game.zoneMembers Zone.Hand S.alice gone)
        Spec.assertEqWith
          s
          "CR 707.2 the duplicate's copiable values are the Piker's, where its printed card is the Clone's"
          (fmap (\oid -> (Set.toList (Projection.namesOf oid gone), S.soleFaceName oid gone)) duplicates)
          [([goblinPiker], cloneName)]
        Spec.assertEqWith
          s
          "and it reads the Piker's 2/1 with the Piker itself in the graveyard"
          (fmap (\oid -> S.powerToughnessOf oid gone) duplicates, List.sort (namesIn Zone.Graveyard gone))
          ([Just (2, 1)], List.sort [cloneName, goblinPiker, sinisterReflections])
  -- The same duplicate across THREE zone changes, each a new object (CR 400.7):
  -- cast out of the hand, resolved onto the battlefield, destroyed into the
  -- graveyard. The copiable values are the card's own, so every incarnation is
  -- the Piker. A duplicate that forgot them would be a Clone spell that
  -- resolves into a Clone choosing its CR 614.12 copy afresh -- and with
  -- nothing answered it enters as the 0/0 CR 704.5f sweeps up.
  --
  -- Islands and Mountains both, so the cast is affordable at the Clone's
  -- {3}{U} as well as the Piker's {1}{R}, and the assertion on the permanent
  -- is what goes red rather than the cast.
  Spec.it s "a duplicate of a Clone cast and resolved is the Piker" $ do
    islandPrinting <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    let board0 = S.landsFor mountain S.alice 4 (S.landsInPlay islandPrinting 4)
        (pikerId, board1) = S.addPermanent piker S.alice board0
        (_, staged) = S.spellOnStack clone S.alice board1
        entered = S.settleSba (copyingPiker pikerId staged)
    case clonesOnBattlefield entered of
      [] -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      cloneId : _ -> do
        let (spell, board2) = S.addHandCard reflections S.alice entered
            board = board2 {GameState.phase = Phase.PrecombatMain}
            resolved = S.runPure (aimingAtAll [cloneId]) board (S.cast S.alice spell >> Stack.resolveTop)
            gone = S.settleSba (S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [pikerId, cloneId]))
            inHand = namedIn cloneName Zone.Hand gone
            played = S.settleSba (S.runPure S.identityAnswer gone (Monad.mapM_ (\oid -> S.cast S.alice oid >> Stack.resolveTop) inHand))
            permanents = clonesOnBattlefield played
            died = S.settleSba (S.runPure S.identityAnswer played (Event.destroy Regenerability.Regenerable permanents))
        Spec.assertEqWith
          s
          "CR 400.7 the resolved duplicate is a Goblin Piker on the battlefield, printed Clone beneath"
          (fmap (\oid -> (Set.toList (Projection.namesOf oid played), S.powerToughnessOf oid played)) permanents)
          [([goblinPiker], Just (2, 1))]
        Spec.assertEqWith
          s
          "and a Goblin Piker again in the graveyard, beside the original Clone's own name"
          (List.sort (fmap (\oid -> Set.toList (Projection.namesOf oid died)) (namedIn cloneName Zone.Graveyard died)))
          (List.sort [[cloneName], [goblinPiker]])
  -- CR 707.2 lists mana cost among the copiable values, so the duplicate in
  -- hand costs the Piker's {1}{R} and not the printed Clone's {3}{U}. Two
  -- Islands and two Mountains: whatever pays Sinister Reflections' {1}{U}
  -- leaves {1}{R} payable and {3}{U} not.
  Spec.it s "CR 707.2 a duplicate of a Clone costs what the Clone copies" $ do
    islandPrinting <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    let board0 = S.landsFor mountain S.alice 2 (S.landsInPlay islandPrinting 2)
        (pikerId, board1) = S.addPermanent piker S.alice board0
        (_, staged) = S.spellOnStack clone S.alice board1
        entered = S.settleSba (copyingPiker pikerId staged)
    case clonesOnBattlefield entered of
      [] -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      cloneId : _ -> do
        let (spell, board2) = S.addHandCard reflections S.alice entered
            board = board2 {GameState.phase = Phase.PrecombatMain}
            resolved = S.runPure (aimingAtAll [cloneId]) board (S.cast S.alice spell >> Stack.resolveTop)
            inHand = namedIn cloneName Zone.Hand resolved
            played = S.settleSba (S.runPure S.identityAnswer resolved (Monad.mapM_ (\oid -> S.cast S.alice oid >> Stack.resolveTop) inHand))
        Spec.assertEqWith
          s
          "CR 707.2 the duplicate is castable off two lands, at the Piker's {1}{R}"
          (fmap (\oid -> S.castable S.alice oid resolved) inHand)
          [True]
        Spec.assertEqWith
          s
          "and resolves into a second Goblin Piker beside the Clone"
          (fmap (\oid -> Set.toList (Projection.namesOf oid played)) (clonesOnBattlefield played))
          [[goblinPiker], [goblinPiker]]
  -- Pearl Collector ({2}{B} Creature -- Human Warlock 3/3, "Deathtouch,
  -- Lifelink. At the beginning of your second main phase, if you gained 4 or
  -- more life this turn, conjure a card named Mox Pearl into your hand. This
  -- ability triggers only once. {2}{W}: Another target creature perpetually
  -- gains lifelink."), the only conjure in the corpus behind CR 603.4's
  -- intervening "if" -- and the threshold is the life GAINED this turn, so the
  -- spell alice casts in her precombat main is what decides whether the ability
  -- triggers at all.
  --
  -- The pair is one cast apart. Sun's Bounty ({1}{W} Instant, "You gain 4 life")
  -- meets the threshold exactly; Morsel Theft ({2}{B}{B} Kindred Sorcery --
  -- Rogue, "Target player loses 3 life and you gain 3 life") falls one short.
  -- Both runs share one fixture -- the same Collector, the same six lands, both
  -- spells in hand -- so the only difference between them is which spell was
  -- cast, and an engine reading the threshold as three, or not reading the "if"
  -- at all, would conjure in both.
  --
  -- The Mox is CAST, the Thopterist case's reason: conjure creates a CARD rather
  -- than CR 111.1's token, and a token in a hand would be swept up by CR 111.7
  -- before any cast. What this case adds is reading the permanent it became for
  -- its own mana ability, which is what makes the assertion about the card the
  -- file writes rather than about a name -- an empty face under the right name
  -- would be castable too, and would offer nothing to tap.
  Spec.it s "CR 603.4 conjure behind an intervening if: 4 life gained puts a castable Mox Pearl in hand" $ do
    (bountyId, _, gs) <- pearlBoard s registry
    let gained = S.runPure S.identityAnswer gs (S.cast S.alice bountyId >> Stack.resolveTop)
        secondMain = postcombatMainOf gained
        fired = oneStep secondMain
        inHand = namedIn moxPearl Zone.Hand fired
        -- CR 117.1a: a noninstant spell is cast during a main phase with the
        -- stack empty, so the cast below is asked at one rather than in the step
        -- Engine.runStep left the board in.
        main_ = fired {GameState.phase = Phase.PostcombatMain, GameState.priority = Just S.alice}
        cast_ = case inHand of
          oid : _ -> S.runPure S.identityAnswer main_ (S.cast S.alice oid >> Stack.resolveTop)
          [] -> main_
        tappable g oid = elem oid (Maybe.mapMaybe manaSource (Action.legalActions S.alice g))
    Spec.assertEqWith
      s
      "the conjured Mox was cast and is a mana source alice may tap"
      (fmap (tappable cast_) (namedIn moxPearl Zone.Battlefield cast_))
      [True]
    Spec.assertEqWith
      s
      "it was a card its owner could cast"
      (fmap (\oid -> S.castable S.alice oid main_) inHand)
      [True]
    Spec.assertEqWith
      s
      "exactly one Mox Pearl reached alice's hand"
      (length inHand)
      1
    Spec.assertEqWith
      s
      "the Bounty gained exactly the threshold, at a second main phase the trigger saw"
      (S.lifeOf S.alice fired, GameState.phase secondMain)
      (Just 24, Phase.PostcombatMain)
    Spec.assertEqWith
      s
      "the printed rider is spent, so a later second main phase has nothing left to fire"
      (Set.size (GameState.triggeredThisGame fired))
      1
  -- The control, one cast apart: three life gained is one short of the printed
  -- four, so CR 603.4 keeps the ability off the stack entirely -- which the
  -- unspent rider is the second reading of. bob's life is what tells the Theft
  -- resolving from the Theft being countered on the way: 3 lost there against 3
  -- gained here.
  Spec.it s "CR 603.4 and three life is one short, so nothing is conjured" $ do
    (_, theftId, gs) <- pearlBoard s registry
    let gained = S.runPure aimedAtBob gs (S.cast S.alice theftId >> Stack.resolveTop)
        secondMain = postcombatMainOf gained
        fired = oneStep secondMain
    Spec.assertEqWith
      s
      "no Mox Pearl in hand, and the second main phase really ran"
      (namedIn moxPearl Zone.Hand fired, GameState.phase secondMain)
      ([], Phase.PostcombatMain)
    Spec.assertEqWith
      s
      "the Theft gained three and took three"
      (S.lifeOf S.alice fired, S.lifeOf S.bob fired)
      (Just 23, Just 17)
    Spec.assertEqWith
      s
      "the intervening if is what held it back: the rider is unspent"
      (Set.size (GameState.triggeredThisGame fired))
      0

moxPearl :: CardName.CardName
moxPearl = CardName.MkCardName (Text.pack "Mox Pearl")

-- Pearl Collector's fixture: the Collector on alice's battlefield, both
-- life-gain spells in her hand, and the six lands that pay for either, at the
-- precombat main of her own turn.
--
-- `remaining` is rewritten for Pawl.EventTriggerSpec's reason: Setup.emptyGame
-- still holds the beginning phase and the precombat main, so stepping from a
-- board already set to the precombat main would begin it a SECOND time -- and CR
-- 505.1b counts the main phases that have begun, which would make the postcombat
-- main this turn's third.
pearlBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
pearlBoard s registry = do
  collector <- S.printingOf s registry "Pearl Collector"
  bounty <- S.printingOf s registry "Sun's Bounty"
  theft <- S.printingOf s registry "Morsel Theft"
  plains <- S.printingOf s registry "Plains"
  swamp <- S.printingOf s registry "Swamp"
  let (_, withCollector) = S.addPermanent collector S.alice (Setup.emptyGame S.bothPlayers)
      withLands = S.landsFor swamp S.alice 4 (S.landsFor plains S.alice 2 withCollector)
      (bountyId, withBounty) = S.addHandCard bounty S.alice withLands
      (theftId, withTheft) = S.addHandCard theft S.alice withBounty
  pure
    ( bountyId,
      theftId,
      withTheft
        { GameState.phase = Phase.PrecombatMain,
          GameState.remaining = Seq.drop 1 (Turn.dropRestOfPhase (Phase.Beginning BeginningStep.Upkeep) Turn.laterPhases),
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Pawl.EventTriggerSpec's stepUntil: run whole steps until the postcombat main
-- is the current phase and has NOT yet run -- Engine.runStep runs
-- GameState.phase and only then advances -- so a case can read the board as that
-- step begins. Bounded so a fixture that never reaches it ends rather than
-- hangs. S.identityAnswer declares no attackers, so nothing but the cast spell
-- moves a life total on the way.
postcombatMainOf :: GameState.GameState -> GameState.GameState
postcombatMainOf gs0 =
  let go n g =
        if n <= (0 :: Int) || GameState.phase g == Phase.PostcombatMain
          then g
          else go (n - 1) (oneStep g)
   in go 24 gs0

oneStep :: GameState.GameState -> GameState.GameState
oneStep g = snd (Engine.runGamePure S.identityAnswer g Engine.runStep)

-- Morsel Theft's target. FILTERED out of the offered recipients rather than
-- built, the Toralf's Disciple case's reason: CR 608.2b re-reads the targets at
-- resolution, and a recipient assembled here would be a different one than the
-- prompt offered.
aimedAtBob :: Prompt.Prompt r -> r
aimedAtBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.bob) . snd) sets
  _ -> S.identityAnswer p

goblinPiker :: CardName.CardName
goblinPiker = CardName.MkCardName (Text.pack "Goblin Piker")

hillGiant :: CardName.CardName
hillGiant = CardName.MkCardName (Text.pack "Hill Giant")

sinisterReflections :: CardName.CardName
sinisterReflections = CardName.MkCardName (Text.pack "Sinister Reflections")

-- CR 601.2c's announcement and choice in one, pinned to the named objects:
-- announce as many as there are and hand back exactly those. Pinned rather than
-- searched, Pawl.CopySpec's posture, so a mutation cannot be repaired by the
-- answerer finding some other legal target.
aimingAtAll :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimingAtAll oids p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const (Natural.length oids)) offers
  -- FILTERED out of the offered recipients rather than built, the Toralf's
  -- Disciple case's reason: CR 608.2b re-reads the targets at resolution, and a
  -- recipient assembled here would carry the wrong tag for the slot's pool.
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, rs) -> Set.filter (maybe False (`elem` oids) . Recipient.objectOf) rs) asked
  _ -> S.identityAnswer p

-- Resolves the staged Clone with its CR 614.12 as-enters copy pinned to the
-- named permanent, Pawl.CopySpec's copyNamed for its reason: a searching
-- answerer could repair a mutation by finding some other legal source.
copyingPiker :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
copyingPiker oid gs =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseCopyTarget {} -> Just oid
        _ -> S.identityAnswer p
   in S.runPure answer gs Stack.resolveTop

-- The battlefield objects whose PRINTED card is Clone, which is what tells the
-- copy from the Piker it copied -- every projected characteristic of the two is
-- the same.
clonesOnBattlefield :: GameState.GameState -> [ObjectId.ObjectId]
clonesOnBattlefield gs = filter (\oid -> S.soleFaceName oid gs == cloneName) (Game.zoneMembers Zone.Battlefield S.alice gs)

cloneName :: CardName.CardName
cloneName = CardName.MkCardName (Text.pack "Clone")

mountainName :: CardName.CardName
mountainName = CardName.MkCardName (Text.pack "Mountain")

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

-- The object a mana activation names, which `manaActivation` below only asks
-- the existence of: the tapped case reads WHICH permanents alice may tap.
manaSource :: Action.Action -> Maybe ObjectId.ObjectId
manaSource action = case action of
  Action.ActivateManaAbility oid -> Just oid
  _ -> Nothing

manaActivation :: Action.Action -> Bool
manaActivation action = case action of
  Action.ActivateManaAbility _ -> True
  _ -> False

-- The five Gates data/cards/follow-the-tracks.json prints as the spellbook, in
-- the order the card file writes them.
tracksSpellbook :: [CardName.CardName]
tracksSpellbook =
  fmap
    (CardName.MkCardName . Text.pack)
    [ "Gate of the Black Dragon",
      "Gate to Manorborn",
      "Gate to Seatower",
      "Gate to the Citadel",
      "Gate to Tumbledown"
    ]

gateToSeatower :: CardName.CardName
gateToSeatower = CardName.MkCardName (Text.pack "Gate to Seatower")

forestName :: CardName.CardName
forestName = CardName.MkCardName (Text.pack "Forest")

-- Pins the chosen pick to `who`, FILTERED out of the offered candidates rather
-- than built, tomeAnswer's reason: a name the engine never offered cannot slip
-- through, and the fallback is the head.
tracksAnswer :: CardName.CardName -> Prompt.Prompt r -> r
tracksAnswer who p = case p of
  Prompt.ChooseConjuredCard _ _ offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== who) (NonEmpty.toList offered))
  _ -> S.identityAnswer p
