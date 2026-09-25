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
-- the same spellbook shape with the other question asked of it, and a case
-- beside it activates the seek one of its Gates prints; the ninth enters
-- a printed Foundry Groundbreaker, whose conjure STATES the status its arrivals
-- take; the tenth to sixteenth cast a printed Sinister Reflections, whose
-- conjure names an object already in the game rather than writing its card out;
-- the next two begin alice's second main phase under a printed Pearl Collector,
-- the one conjure in the corpus behind CR 603.4's intervening "if"; the next three
-- enter a printed Fear of Change, whose conjure picks from the Oracle card
-- reference an interpreter holds; the last three connect with a printed Smog
-- Smasher, whose conjure puts its duplicate into exile.
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
-- the fourth prices it off its copiable mana cost. The last three merge it with
-- a printed Cubwarden and split it back out (CR 730.3, CR 727.2).
module Pawl.ConjureSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Foretell as Foretell
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Room as Room
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Interpreter as Interpreter
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureCards as ConjureCards
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.MutateSide as MutateSide
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
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

ragavanName :: CardName.CardName
ragavanName = CardName.MkCardName (Text.pack "Ragavan, Nimble Pilferer")

-- The cards a printing's triggered abilities conjure, written out in its file.
conjuredBy :: Printing.Printing -> [Card.Card]
conjuredBy printing =
  [ card
  | face <- NonEmpty.toList (Card.faces (Printing.card printing)),
    ability <- Face.triggeredAbilities face,
    mode <- Foldable.toList (Modal.modes (TriggeredAbility.modal ability)),
    clause <- Foldable.toList (Mode.clauses mode),
    Effect.Conjure conjure <- Foldable.toList (Clause.effects clause),
    card <- ConjureCards.written (Conjure.cards conjure)
  ]

-- Whole steps until `phase` is current, bounded like Pawl.Support's combatGame.
runStepsUntil :: Phase.Phase -> Game.Type.Game ()
runStepsUntil phase =
  let go n = do
        gs <- State.get
        Monad.unless (n <= (0 :: Int) || GameState.phase gs == phase || Maybe.isJust (GameState.result gs)) (Engine.runStep >> go (n - 1))
   in go 24

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
  -- though it were mana of any color to cast this spell.'" A perpetual effect
  -- over a card in a hand, granting it an ability about paying for itself; pawl's
  -- Tome is stricter than the printing, never weaker (#3291).
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
  -- The answerer pins the pick to Gate to Seatower, which the offered list's
  -- head is not: Pawl.Engine.Replay.defaultAnswer takes the head, so an engine
  -- that picked for alice -- or asked for randomness here, whose default answer
  -- is also the head -- lands on Gate of the Black Dragon instead.
  -- Kari Zev, Crew of Two ({2}{R}{R} 3/3, menace, haste): "Whenever Kari Zev
  -- attacks while you don't control a legendary Monkey, conjure a card named
  -- Ragavan, Nimble Pilferer onto the battlefield tapped and attacking. At the
  -- beginning of the next end step, if that card is on the battlefield, return
  -- it to its owner's hand."
  --
  -- bob's Marchesa's Decree ("whenever a creature attacks you ..., that
  -- creature's controller loses 1 life") is the CR 508.3a witness: Kari was
  -- declared and costs alice 1, while a Ragavan put onto the battlefield
  -- attacking (CR 508.4) was never declared and costs her nothing. The paired
  -- board differs only in a Ragavan alice already controls, which is the
  -- "while" failing, so no second Ragavan arrives and bob takes Kari's 3 alone.
  Spec.it s "Kari Zev's Ragavan attacks without being declared and goes home at the next end step" $ do
    kari <- S.printingOf s registry "Kari Zev, Crew of Two"
    let ragavans = conjuredBy kari
        setup = S.duel S.beginningOfCombat [S.settled "kari" "Kari Zev, Crew of Two"] [S.permanent "Marchesa's Decree"]
        script = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "kari"])]
        throughEndStep = runStepsUntil (Phase.Ending EndingStep.Cleanup)
    built <- S.buildBoardOrFail s registry setup
    (_, after) <- S.runScriptOrFail s script built throughEndStep
    (_, withMonkey) <- case ragavans of
      [ragavan] -> S.runScriptOrFail s script built {S.builtState = snd (S.addPermanent (Printing.ofCard ragavan) S.alice (S.builtState built))} throughEndStep
      _ -> pure ((), after)
    Spec.assertEqWith
      s
      "CR 508.4 the conjured Ragavan dealt 2 beside Kari's 3 without costing alice a declared attacker's life, and none came while she controlled a legendary Monkey"
      (S.lifeOf S.bob after, S.lifeOf S.alice after, S.lifeOf S.bob withMonkey)
      (Just 15, Just 19, Just 17)
    Spec.assertEqWith
      s
      "CR 603.7c the end step returned the bound card to alice's hand"
      (length (namedIn ragavanName Zone.Hand after), length (namedIn ragavanName Zone.Battlefield after))
      (1, 0)
  -- The conjured Ragavan's own trigger: "Whenever Ragavan deals combat damage to
  -- a player, create a Treasure token and exile the top card of that player's
  -- library. Until end of turn, you may cast that card." CR 601.3 / 305.9: the
  -- permission's verb is Cast, so a nonland card is castable (the Treasure pays
  -- its {R}) and a land is not playable. The pair differs only in bob's top card.
  Spec.it s "CR 305.9 Ragavan's exiled card may be cast, and an exiled land may not be played" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let setup = S.duel S.beginningOfCombat [S.settled "kari" "Kari Zev, Crew of Two"] []
        script = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "kari"])]
        exiledAfter top = do
          built <- S.buildBoardOrFail s registry setup
          let stocked = built {S.builtState = snd (S.addLibraryCard top S.bob (S.builtState built))}
          (_, gs) <- S.runScriptOrFail s script stocked (runStepsUntil S.postcombatMain)
          pure (Game.zoneMembers Zone.Exile S.bob gs, gs {GameState.priority = Just S.alice})
        playsLand oid gs = any (\action -> case action of Action.Play played _ -> played == oid; _ -> False) (Action.legalActions S.alice gs)
    (bolts, boltGame) <- exiledAfter bolt
    (mountains, mountainGame) <- exiledAfter mountain
    Spec.assertEqWith
      s
      "CR 601.3 the exiled Lightning Bolt is castable and the exiled Mountain is not playable"
      (fmap (\oid -> castOffered oid lightningBolt boltGame) bolts, fmap (`playsLand` mountainGame) mountains)
      ([True], [False])
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
  -- Gate to Seatower, the spellbook's Island Gate, Oracle text verified on
  -- Scryfall 2026-09-25: "{3}{U}, {T}: Seek a nonland card. Activate only once."
  -- Seek is Alchemy's: a card at random from your library matching the
  -- description goes to your hand, with no search, no reveal and no shuffle.
  --
  -- alice's library holds three nonland cards among three Islands, and the pick
  -- is pinned to Think Twice, which is neither the offer's head (the default
  -- answer), nor the library's top or bottom card (a draw, or a walk from
  -- either end), nor a land (an unfiltered pick). A shuffle is answered
  -- reversed, so one would move every card left behind.
  Spec.it s "Gate to Seatower's seek puts the nonland card randomness named into the hand, leaving the library's order" $ do
    islandPrinting <- S.printingOf s registry "Island"
    tracks <- S.printingOf s registry "Follow the Tracks"
    stock <- Monad.mapM (S.printingOf s registry) ["Island", "Lightning Bolt", "Island", "Think Twice", "Ornithopter", "Island"]
    gate <- case filter ((== gateToSeatower) . Face.name . NonEmpty.head . Card.faces) (spellConjures tracks) of
      [card] -> pure (Printing.ofCard card)
      _ -> Spec.assertFailure s "expected one Gate to Seatower in Follow the Tracks's spellbook"
    let (gateId, board0) = S.addPermanent gate S.alice (S.landsInPlay islandPrinting 4)
        stocked = List.foldl' (\gs printing -> snd (S.addLibraryCard printing S.alice gs)) board0 stock
        board = stocked {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
        before = Game.zoneMembers Zone.Library S.alice board
        pinned = namedIn thinkTwice Zone.Library board
        who = Maybe.fromMaybe gateId (Maybe.listToMaybe pinned)
        logging :: Prompt.Prompt r -> State.State [[CardName.CardName]] r
        logging p = case p of
          Prompt.RandomObject offered -> do
            State.modify' (fmap (`S.soleFaceName` board) (NonEmpty.toList offered) :)
            pure (gateAnswer gateId who p)
          _ -> pure (gateAnswer gateId who p)
        (offers, final) = case State.runState (Engine.runGame logging board Engine.priorityLoop) [] of
          ((_, gs), asked) -> (reverse asked, gs)
    -- THE GAMEPLAY ASSERTION: the card randomness named is in alice's hand, and
    -- every other card is still in her library in the order it was.
    Spec.assertEqWith
      s
      "the sought Think Twice is in alice's hand, and her library is the rest in its old order"
      (namesIn Zone.Hand final, Game.zoneMembers Zone.Library S.alice final)
      ([thinkTwice], filter (`notElem` pinned) before)
    -- Supporting, and LAST so they cannot absorb a mutation the assertion above
    -- should catch: randomness was asked once, over the nonland cards alone, and
    -- nothing was revealed.
    Spec.assertEqWith
      s
      "asked once, offering the three nonland cards and no land, and revealing nothing"
      (offers, [() | GameEvent.Revealed _ <- S.eventsOf final])
      ([[lightningBolt, thinkTwice, ornithopter]], [])
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
  -- CR 707.2 / 601.2b: the additional cost is rules text, so it is copiable too.
  -- A Clone copying Headless Skaab ({2}{U}, "As an additional cost to cast this
  -- spell, exile a creature card from your graveyard") is duplicated, and the
  -- duplicate owes the exile. The pair differs only in a Goblin Piker in the
  -- graveyard; three Islands pay the Skaab's {2}{U} and not the Clone's {3}{U}.
  Spec.it s "CR 707.2/601.2b a duplicate of a Clone owes the additional cost of what the Clone copies" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    skaab <- S.printingOf s registry "Headless Skaab"
    let (skaabId, board) = S.addPermanent skaab S.alice (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections skaabId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) -> do
        let bare = S.landsFor island S.alice 3 conjured
            (_, stocked) = S.addGraveyardCard piker S.alice bare
        Spec.assertEqWith
          s
          "CR 601.2b castable only with a creature card to exile: (empty graveyard, a Piker in it)"
          (S.castable S.alice duplicate bare, S.castable S.alice duplicate stocked)
          (False, True)
  -- CR 707.2 / 118.9: the alternative cost is copiable for the same reason. A
  -- Clone copying Asmoranomardicadaistinaculdacar (no mana cost; "As long as
  -- you've discarded a card this turn, you may pay {B/R} to cast this spell") is
  -- duplicated, and the duplicate is castable for {B/R} once a card is
  -- discarded. Bob controls the original, so CR 704.5j leaves both legends be.
  Spec.it s "CR 707.2/118.9 a duplicate of a Clone offers the alternative cost of what the Clone copies" $ do
    island <- S.printingOf s registry "Island"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    asmor <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
    let (asmorId, board) = S.addPermanent asmor S.bob (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections asmorId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) -> do
        let (pikerId, undiscarded) = S.addHandCard piker S.alice (S.landsFor swamp S.alice 1 conjured)
            discarded = S.runPure S.identityAnswer undiscarded (Event.discard DiscardCause.Ordinary S.alice pikerId)
        Spec.assertEqWith
          s
          "CR 118.9 castable for {B/R} only once a card is discarded: (undiscarded, discarded)"
          (S.castable S.alice duplicate undiscarded, S.castable S.alice duplicate discarded)
          (False, True)
  -- CR 707.2 / 601.2f: the self-reduction is copiable for the same reason. A
  -- Clone copying Thrasta, Tempest's Roar ({10}{G}{G}, "This spell costs {3} less
  -- to cast for each other spell cast this turn") is duplicated by the one spell
  -- cast this turn, so the duplicate costs {7}{G}{G}: nine Forests pay it and
  -- eight do not. Bob controls the original, the Asmor case's reason.
  Spec.it s "CR 707.2/601.2f a duplicate of a Clone takes the cost reduction of what the Clone copies" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    let (thrastaId, board) = S.addPermanent thrasta S.bob (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections thrastaId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) ->
        Spec.assertEqWith
          s
          "CR 601.2f castable at {7}{G}{G}: (eight Forests, nine Forests)"
          (S.castable S.alice duplicate (S.landsFor forest S.alice 8 conjured), S.castable S.alice duplicate (S.landsFor forest S.alice 9 conjured))
          (False, True)
  -- CR 707.2 / 702.41a: keywords are copiable, affinity among them. A Clone
  -- copying Frogmite ({4}, "Affinity for artifacts") is duplicated, and the
  -- duplicate costs {1} less for each of the two artifacts alice controls --
  -- Frogmite and the Clone copying it -- so two fresh Islands pay it and one
  -- does not.
  Spec.it s "CR 707.2/702.41a a duplicate of a Clone has the affinity of what the Clone copies" $ do
    island <- S.printingOf s registry "Island"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    frogmite <- S.printingOf s registry "Frogmite"
    let (frogmiteId, board) = S.addPermanent frogmite S.alice (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections frogmiteId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) ->
        Spec.assertEqWith
          s
          "CR 702.41a castable at {2} off two artifacts: (one Island, two Islands)"
          (S.castable S.alice duplicate (S.landsFor island S.alice 1 conjured), S.castable S.alice duplicate (S.landsFor island S.alice 2 conjured))
          (False, True)
  -- CR 707.2 / 702.51a: convoke, likewise. A Clone copying Siege Wurm
  -- ({5}{G}{G}, convoke, trample) is duplicated, and alice's two green creatures
  -- -- the Wurm and the Clone copying it -- pay two of the seven, so five Forests
  -- are enough and four are not.
  Spec.it s "CR 707.2/702.51a a duplicate of a Clone has the convoke of what the Clone copies" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    wurm <- S.printingOf s registry "Siege Wurm"
    let (wurmId, board) = S.addPermanent wurm S.alice (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections wurmId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) ->
        Spec.assertEqWith
          s
          "CR 702.51a castable with two creatures convoking: (four Forests, five Forests)"
          (S.castable S.alice duplicate (S.landsFor forest S.alice 4 conjured), S.castable S.alice duplicate (S.landsFor forest S.alice 5 conjured))
          (False, True)
  -- CR 707.2 / 702.143a: foretell, likewise. A Clone copying Augury Raven
  -- ({3}{U}, flying, foretell {1}{U}) is duplicated, and the duplicate is a card
  -- with foretell in alice's hand, so two fresh Islands let her foretell it.
  Spec.it s "CR 707.2/702.143a a duplicate of a Clone has the foretell of what the Clone copies" $ do
    island <- S.printingOf s registry "Island"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    raven <- S.printingOf s registry "Augury Raven"
    let (ravenId, board) = S.addPermanent raven S.alice (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections ravenId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) ->
        Spec.assertEqWith
          s
          "CR 702.143a the duplicate is among the cards alice may foretell"
          (elem duplicate (Foretell.foretellable S.alice (S.landsFor island S.alice 2 conjured)))
          True
  -- CR 702.140e: a mutated permanent has every component's abilities, and the
  -- Skaab's additional cost is one. Cubwarden mutates over alice's Headless
  -- Skaab and Sinister Reflections duplicates the merged creature, so the
  -- duplicate costs the Cubwarden's {3}{W} and still owes the Skaab's exile. The
  -- pair differs only in a Goblin Piker in the graveyard.
  Spec.it s "CR 702.140e a duplicate of a mutated Headless Skaab owes the Skaab's additional cost" $ do
    (island, _, plains, piker, _, reflections, cubwarden) <- duplicatePrintings s registry
    skaab <- S.printingOf s registry "Headless Skaab"
    let (skaabId, board0) = S.addPermanent skaab S.alice (S.landsInPlay plains 4)
        (withCubwarden, cubwardenSpell) = S.handOne cubwarden board0
        merged = mergingOnto skaabId withCubwarden cubwardenSpell
        (spell, board1) = S.addHandCard reflections S.alice (S.landsFor island S.alice 2 merged)
        before = Game.zoneMembers Zone.Hand S.alice board1
        resolved = S.settleSba (S.runPure (aimingAtAll [skaabId]) board1 {GameState.phase = Phase.PrecombatMain} (S.cast S.alice spell >> Stack.resolveTop))
    case filter (`notElem` before) (Game.zoneMembers Zone.Hand S.alice resolved) of
      [duplicate] -> do
        let bare = S.landsFor plains S.alice 4 resolved
            (_, stocked) = S.addGraveyardCard piker S.alice bare
        Spec.assertEqWith
          s
          "CR 702.140e castable only with a creature card to exile: (empty graveyard, a Piker in it)"
          (S.castable S.alice duplicate bare, S.castable S.alice duplicate stocked)
          (False, True)
        Spec.assertEqWith s "setup: the merged permanent held the Cubwarden and the Skaab" (fmap (Seq.length . Game.componentsOf . Object.source) (Game.lookupObject skaabId merged)) (Just 2)
      other -> Spec.assertFailure s ("expected one duplicate in hand, got " <> show (length other))
  -- CR 715.3a: an Adventure is cast with its own characteristics alone, so a
  -- duplicate of Flaxen Intruder ({G}) cast as Welcome Home still costs
  -- {5}{G}{G}. One Forest pays the creature half and not the Adventure.
  Spec.it s "CR 715.3a a duplicate of Flaxen Intruder cast as Welcome Home costs Welcome Home's cost" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    reflections <- S.printingOf s registry "Sinister Reflections"
    intruder <- S.printingOf s registry "Flaxen Intruder"
    let (intruderId, board0) = S.addPermanent intruder S.alice (S.landsInPlay island 2)
        (spell, board1) = S.addHandCard reflections S.alice board0
        before = Game.zoneMembers Zone.Hand S.alice board1
        resolved = S.settleSba (S.runPure (aimingAtAll [intruderId]) board1 {GameState.phase = Phase.PrecombatMain} (S.cast S.alice spell >> Stack.resolveTop))
        paid = S.landsFor forest S.alice 1 resolved
        castableAs name oid = Cast.castable S.alice oid (CardName.MkCardName (Text.pack name)) Facing.FaceUp paid
    case filter (`notElem` before) (Game.zoneMembers Zone.Hand S.alice resolved) of
      [duplicate] ->
        Spec.assertEqWith
          s
          "CR 715.3a off one Forest: (as Flaxen Intruder, as Welcome Home)"
          (castableAs "Flaxen Intruder" duplicate, castableAs "Welcome Home" duplicate)
          (True, False)
      other -> Spec.assertFailure s ("expected one duplicate in hand, got " <> show (length other))
  -- CR 715.2b / 707.2: the Adventure half is a copiable value, so a duplicate of
  -- a Clone copying Flaxen Intruder -- printed Clone beneath -- is castable as
  -- Welcome Home ({5}{G}{G}), and resolves as it: three Bears, and the card
  -- exiled by CR 715.3d. Seven Forests pay the Adventure; the printed Clone
  -- offers no Welcome Home at all.
  Spec.it s "CR 707.2/715.2b a duplicate of a Clone of Flaxen Intruder is cast as Welcome Home" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    intruder <- S.printingOf s registry "Flaxen Intruder"
    let (intruderId, board0) = S.addPermanent intruder S.alice (S.landsInPlay island 2) {GameState.phase = Phase.PrecombatMain}
    case duplicateOfCopy clone reflections intruderId board0 of
      Just (duplicate, conjured) -> do
        let paid = S.landsFor forest S.alice 7 conjured
            cast = S.runPure S.identityAnswer paid (Cast.castSpell S.manaPerformer S.alice duplicate welcomeHome Facing.FaceUp)
            played = S.settleSba (S.runPure S.identityAnswer cast Stack.resolveTop)
            bears = filter (\oid -> Projection.namesOf oid played == Set.singleton bearToken) (Game.zoneMembers Zone.Battlefield S.alice played)
        Spec.assertEqWith
          s
          "CR 715.3a/715.3b/715.3d (Welcome Home offered as a cast, lands it taps, the spell's names, Bears, cards in exile)"
          (castOffered duplicate welcomeHome paid, S.tappedCount S.alice cast - S.tappedCount S.alice paid, fmap (\oid -> Projection.namesOf oid cast) (GameState.stack cast), length bears, length (Game.zoneMembers Zone.Exile S.alice played))
          (True, 7, [Set.singleton welcomeHome], 3, 1)
      Nothing -> Spec.assertFailure s "expected one Clone and one duplicate"
  -- CR 305.1 / 707.2: card types are copiable, so a duplicate of a Clone copying
  -- Dryad Arbor -- printed Clone beneath -- is a land card its owner may play,
  -- and enters as Dryad Arbor. With no mana cost it has no cast to fall back on
  -- (CR 118.6), so the land play is the only way it leaves the hand.
  Spec.it s "CR 707.2/305.1 a duplicate of a Clone of Dryad Arbor is played as a land" $ do
    island <- S.printingOf s registry "Island"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    arbor <- S.printingOf s registry "Dryad Arbor"
    let (arborId, board0) = S.addPermanent arbor S.alice (S.landsInPlay island 2) {GameState.phase = Phase.PrecombatMain}
    case duplicateOfCopy clone reflections arborId board0 of
      Just (duplicate, conjured) -> do
        let played = S.settleSba (S.runPure S.identityAnswer conjured (Cast.playLand False S.alice duplicate Nothing))
            arrived = Set.toList (Set.difference (GameState.battlefield played) (GameState.battlefield conjured))
        Spec.assertEqWith
          s
          "CR 305.1 (the land play offered, the names it enters with)"
          (elem (Action.Play duplicate Nothing) (Action.legalActions S.alice conjured {GameState.priority = Just S.alice}), fmap (\oid -> Projection.namesOf oid played) arrived)
          (True, [Set.singleton dryadArbor])
      Nothing -> Spec.assertFailure s "expected one Clone and one duplicate"
  -- CR 707.2 / 702.37c / 702.37e: morph is copiable, so a duplicate of a Clone
  -- copying Ainok Tracker ({5}{R}, "Morph {4}{R}") -- printed Clone beneath --
  -- is offered face down for {3} and turned face up for {4}{R}, becoming the
  -- Tracker. The Mountains arrive after Sinister Reflections, so every land the
  -- two steps tap is counted.
  Spec.it s "CR 707.2/702.37e a duplicate of a Clone of Ainok Tracker is cast face down and turned up for its morph cost" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    tracker <- S.printingOf s registry "Ainok Tracker"
    let (trackerId, board0) = S.addPermanent tracker S.alice (S.landsInPlay island 2) {GameState.phase = Phase.PrecombatMain}
        morphed = Facing.faceDown FaceDownReason.Morphed
    case duplicateOfCopy clone reflections trackerId board0 of
      Just (duplicate, conjured) -> do
        let paid = S.landsFor mountain S.alice 8 conjured
            offeredDown = elem (Action.Cast duplicate cloneName morphed) (Action.legalActions S.alice paid {GameState.priority = Just S.alice})
            cast = S.settleSba (S.runPure S.identityAnswer paid (Cast.castSpell S.manaPerformer S.alice duplicate cloneName morphed >> Stack.resolveTop))
            arrived = Set.toList (Set.difference (GameState.battlefield cast) (GameState.battlefield paid))
            turnable = FaceDown.turnableFaceUp S.alice cast
            up = S.settleSba (S.runPure S.identityAnswer cast (Monad.mapM_ (FaceDown.turnFaceUp S.manaPerformer S.alice TurnUpProcedure.Morph) arrived))
        Spec.assertEqWith
          s
          "CR 702.37c/702.37e (face-down cast offered, lands it taps, turn-up offered, lands that taps, names face up)"
          (offeredDown, S.tappedCount S.alice cast - S.tappedCount S.alice paid, turnable, S.tappedCount S.alice up - S.tappedCount S.alice cast, fmap (\oid -> Projection.namesOf oid up) arrived)
          (True, 3, fmap (\oid -> (oid, TurnUpProcedure.Morph)) arrived, 5, [Set.singleton ainokTracker])
      Nothing -> Spec.assertFailure s "expected one Clone and one duplicate"
  -- CR 709.5b / 707.2: a Room's halves are copiable values, so a Copy
  -- Enchantment copying Spiked Corridor // Torture Pit, made a creature by
  -- Opalescence and duplicated by Sinister Reflections, is a card in hand printed
  -- Copy Enchantment that is cast as a door -- and enters with that door unlocked
  -- (CR 709.5d). The copy's Torture Pit is unlocked first so Opalescence does not
  -- make it a 0/0.
  Spec.it s "CR 707.2/709.5b a duplicate of a copied Room is cast as a door" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    room <- S.printingOf s registry "Spiked Corridor"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    opalescence <- S.printingOf s registry "Opalescence"
    reflections <- S.printingOf s registry "Sinister Reflections"
    let board0 = (S.landsInPlay mountain 8) {GameState.phase = Phase.PrecombatMain}
        (roomId, board1) = S.addPermanent room S.alice board0
        (_, staged) = S.spellOnStack copyEnchantment S.alice board1
        entered = S.settleSba (copyingPiker roomId staged)
    case Set.toList (Set.difference (GameState.battlefield entered) (GameState.battlefield board1)) of
      [copyId] -> do
        let unlocked = S.settleSba (S.runPure S.identityAnswer entered (Room.unlock S.manaPerformer S.alice copyId torturePit))
            -- The Islands arrive after the unlock, so its {3} cannot spend the
            -- {U} Sinister Reflections needs.
            animated = S.landsFor island S.alice 2 (S.settleSba (snd (S.addPermanent opalescence S.alice unlocked)))
        case duplicateOf reflections copyId animated of
          Just (duplicate, conjured) -> do
            let played = S.settleSba (S.runPure S.identityAnswer conjured (Cast.castSpell S.manaPerformer S.alice duplicate spikedCorridor Facing.FaceUp >> Stack.resolveTop))
                arrived = Set.toList (Set.difference (GameState.battlefield played) (GameState.battlefield conjured))
            Spec.assertEqWith
              s
              "CR 709.3/709.5d (the door offered as a cast, the names the duplicate enters showing)"
              (castOffered duplicate spikedCorridor conjured, fmap (\oid -> Projection.namesOf oid played) arrived)
              (True, [Set.singleton spikedCorridor])
          Nothing -> Spec.assertFailure s "expected one duplicate in hand"
      other -> Spec.assertFailure s ("expected one Copy Enchantment, got " <> show (length other))
  -- CR 730.3 / 400.7: the duplicate, cast and resolved as the Piker, is the
  -- permanent Cubwarden ({3}{W} 3/5 Cat, "Mutate {2}{W}{W}", lifelink) mutates
  -- OVER, and the merged permanent is then destroyed. Its two components are put
  -- into the graveyard as two new objects, and the duplicate's copiable values
  -- are its own card's (CR 707.2), so the one printed Clone is the Piker again
  -- there. Read beside the original Clone, which is a Clone in the graveyard.
  Spec.it s "CR 730.3/707.2 a duplicate of a Clone split out of a merge is the Piker again" $ do
    fixture <- duplicateFixture s registry
    case mergedOntoDuplicate fixture of
      Nothing -> Spec.assertFailure s "the duplicate did not resolve onto the battlefield"
      Just (host, merged) -> do
        let died = S.settleSba (S.runPure S.identityAnswer merged (Event.destroy Regenerability.Regenerable [host]))
        Spec.assertEqWith
          s
          "CR 730.3 the duplicate component is a Goblin Piker in the graveyard, beside the original Clone"
          (List.sort (fmap (\oid -> Set.toList (Projection.namesOf oid died)) (namedIn cloneName Zone.Graveyard died)))
          (List.sort [[cloneName], [goblinPiker]])
        -- The values are the duplicate's alone: the Cubwarden, the merged
        -- permanent's other component, is its printed self.
        Spec.assertEqWith
          s
          "CR 730.3 and the Cubwarden component is a Cubwarden there"
          (fmap (\oid -> Set.toList (Projection.namesOf oid died)) (namedIn cubwardenName Zone.Graveyard died))
          [[cubwardenName]]
        Spec.assertEqWith s "setup: the merged permanent held the Cubwarden and the duplicate" (fmap (Seq.length . Game.componentsOf . Object.source) (Game.lookupObject host merged)) (Just 2)
  -- CR 727.2's rebuild, the other road a merged permanent is split on
  -- (Pawl.Engine.Setup.splitComponents): the restart shuffles every card into
  -- its owner's library and draws, so the duplicate is a card among them and is
  -- still the Piker. Hand and library both, since the draw decides
  -- which it is in.
  Spec.it s "CR 727.2/707.2 a duplicate of a Clone rebuilt out of a merge by a restart is the Piker again" $ do
    fixture <- duplicateFixture s registry
    case mergedOntoDuplicate fixture of
      Nothing -> Spec.assertFailure s "the duplicate did not resolve onto the battlefield"
      Just (_, merged) -> do
        let restarted = snd (Engine.runGamePure S.identityAnswer merged (Setup.restartGame S.performer Set.empty S.alice))
            clones = namedIn cloneName Zone.Library restarted <> namedIn cloneName Zone.Hand restarted
        Spec.assertEqWith
          s
          "CR 727.2 the duplicate is a Goblin Piker in the new game, beside the original Clone"
          (List.sort (fmap (\oid -> Set.toList (Projection.namesOf oid restarted)) clones))
          (List.sort [[cloneName], [goblinPiker]])
  -- The other side of CR 730.2: the duplicate is the mutating SPELL. A Clone
  -- copies a Cubwarden, Sinister Reflections duplicates it, so the card in hand
  -- is a Cubwarden printed Clone, and it is cast for its mutate cost over a
  -- Goblin Piker. CR 730.2b takes the spell's object away, so what carries its
  -- values through the merge is the component alone; destroyed, it is a
  -- Cubwarden in the graveyard.
  Spec.it s "CR 730.2/730.3 a duplicate of a Clone that merged as the spell is the Cubwarden again" $ do
    (island, mountain, plains, piker, clone, reflections, cubwarden) <- duplicatePrintings s registry
    let board0 = S.landsFor plains S.alice 8 (S.landsFor mountain S.alice 4 (S.landsInPlay island 4))
        (cubwardenId, board1) = S.addPermanent cubwarden S.alice board0
        (pikerId, board2) = S.addPermanent piker S.alice board1
    case duplicateInHand clone reflections cubwardenId board2 of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, gone) -> do
        let merged = mergingOnto pikerId gone duplicate
            died = S.settleSba (S.runPure S.identityAnswer merged (Event.destroy Regenerability.Regenerable [pikerId]))
        Spec.assertEqWith
          s
          "CR 730.3 the duplicate component is a Cubwarden in the graveyard, beside the original Clone"
          (List.sort (fmap (\oid -> Set.toList (Projection.namesOf oid died)) (namedIn cloneName Zone.Graveyard died)))
          (List.sort [[cloneName], [cubwardenName]])
        Spec.assertEqWith s "setup: CR 730.2a the merged permanent was the Cubwarden on top" (Set.toList (Projection.namesOf pikerId merged)) [cubwardenName]
        Spec.assertEqWith s "setup: and the Piker under it" (fmap (Seq.length . Game.componentsOf . Object.source) (Game.lookupObject pikerId merged)) (Just 2)
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
  -- Fear of Change ({G}{U} Enchantment Creature -- Nightmare, 2/3, "When this
  -- creature enters or dies, exile another creature you control. If you do,
  -- conjure a duplicate of a random creature card with mana value X onto the
  -- battlefield, where X is 2 plus the exiled creature's mana value."), Oracle
  -- text verified on Scryfall 2026-09-24. The REFERENCE pick (CR 108.1): no card
  -- is written out, and the candidates come off the interpreter's registry.
  --
  -- The reference is a fixture of four, so the filter has something to refuse
  -- on each axis: a Goblin Piker is a creature at the exiled Piker's own mana
  -- value, 2 rather than 2 plus 2, and Ancient Vendetta is mana value 4 but no
  -- creature. The answerer names the first of Goblin Piker, Ancient Vendetta and
  -- Giant Spider it is offered, so an offer the filter did not narrow conjures
  -- no Spider, and an engine that rolled the pick itself lands on the offer's
  -- head, Hill Giant.
  Spec.it s "a conjure from the reference offers the creature cards at 2 plus the exiled mana value, and conjures the one randomness named" $ do
    (final, offers) <- fearOfChangeRun s registry True False
    Spec.assertEqWith
      s
      "the Giant Spider randomness named entered the battlefield under alice"
      (length (namedIn giantSpider Zone.Battlefield final))
      1
    Spec.assertEqWith
      s
      "the other Goblin Piker is what the trigger exiled"
      (length (namedIn goblinPiker Zone.Exile final), length (namedIn goblinPiker Zone.Battlefield final))
      (1, 0)
    -- Supporting, and LAST: the offer, recorded off the prompt since the
    -- candidate list is not readable off the board.
    Spec.assertEqWith
      s
      "offered the mana value 4 creature cards of the reference, once"
      offers
      [[hillGiant, giantSpider]]
  -- The pair above with the one difference: no other creature to exile, so the
  -- "if you do" fails and nothing is conjured.
  Spec.it s "a conjure from the reference behind an exile that found nothing conjures nothing" $ do
    (final, _) <- fearOfChangeRun s registry False False
    Spec.assertEqWith
      s
      "nothing but Fear of Change is on alice's battlefield"
      (namesIn Zone.Battlefield final)
      [fearOfChange]
  -- The first case's board with a reference that LIES, offering all four cards;
  -- randomness names the Goblin Piker, which the filter does not admit. Filtered,
  -- not trusted: the conjure checks the card it is handed back and conjures
  -- nothing.
  Spec.it s "a conjure from the reference refuses a card the reference offered outside its filter" $ do
    (final, offers) <- fearOfChangeRun s registry True True
    Spec.assertEqWith
      s
      "the offered Goblin Piker did not enter, and nothing else did"
      (namesIn Zone.Battlefield final)
      [fearOfChange]
    Spec.assertEqWith
      s
      "the lying reference was what offered it"
      offers
      [[goblinPiker, hillGiant, ancientVendetta, giantSpider]]
  -- The first case's board over the suite's own FILE registry, beside a
  -- Llanowar Elves so X is 3, a mana value synthetic creature cards in
  -- data/cards/ share. Randomness names a synthetic wherever one is offered: a
  -- synthetic is a test fixture and no card of the Oracle card reference, so
  -- none is, and a real card enters.
  Spec.it s "a conjure from the file registry's reference never conjures a synthetic card" $ do
    fear <- S.printingOf s registry "Fear of Change"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (_, withElves) = S.addPermanent elves S.alice (Setup.emptyGame S.bothPlayers)
        (_, entered) = S.entersWithTrigger fear S.alice withElves
        lifted = Registry.MkRegistry {Registry.fetchCard = Trans.lift . Registry.fetchCard registry, Registry.cards = Trans.lift (Registry.cards registry)}
        synthetic name = Text.pack "Synthetic " `Text.isPrefixOf` CardName.unwrap name
        answer :: (Monad m) => Asked.Asked r -> State.StateT [[CardName.CardName]] m r
        answer asked = case Asked.prompt asked of
          Prompt.RandomCard offered -> do
            State.modify' (NonEmpty.toList offered :)
            pure (Maybe.fromMaybe (NonEmpty.head offered) (List.find synthetic (NonEmpty.toList offered)))
          p -> pure (S.identityAnswer p)
        run = do
          (_, settled) <- Engine.runGameAsked (Interpreter.lookingUpCards lifted answer) entered Engine.settleForPriority
          Engine.runGameAsked (Interpreter.lookingUpCards lifted answer) settled Engine.priorityLoop
    ((_, final), logged) <- State.runStateT run []
    let arrived = filter (`notElem` [fearOfChange, llanowarElves]) (namesIn Zone.Battlefield final)
    Spec.assertEqWith
      s
      "one creature card entered, and it is no synthetic"
      (length arrived, filter synthetic arrived)
      (1, [])
    Spec.assertEqWith
      s
      "the reference offered no synthetic"
      (concatMap (filter synthetic) logged, length logged)
      ([], 1)
  -- Smog Smasher ({4}{R} Creature -- Mutant Berserker, 4/4, "Menace / Start
  -- your engines! / Whenever one or more creatures you control deal combat
  -- damage to a player, conjure a duplicate of target nontoken creature into
  -- exile. / Max speed -- At the beginning of combat on your turn, put all cards
  -- exiled with this creature onto the battlefield. They gain haste. Sacrifice
  -- them at the beginning of the next end step."), Oracle text verified on
  -- Scryfall 2026-09-24. The EXILE arrival: the Smasher connects, and the
  -- duplicate of bob's Hill Giant lands in alice's exile while the original
  -- stays on bob's battlefield.
  Spec.it s "conjure into exile puts the duplicate in the conjurer's exile, exiled with the conjuring creature" $ do
    (final, smasher, giant) <- smogSmasherCombat s registry
    let duplicates = namedIn hillGiant Zone.Exile final
    Spec.assertEqWith
      s
      "one Hill Giant is in alice's exile, and it is no Hill Giant of bob's"
      (length duplicates, fmap Object.zone (Game.lookupObject giant final))
      (1, Just Zone.Battlefield)
    -- CR 607.2a: exiled BY the ability, so "exiled with this creature" names it.
    Spec.assertEqWith
      s
      "CR 607.2a it is exiled with Smog Smasher"
      (fmap (`Map.lookup` GameState.exiledWith final) duplicates)
      [Just smasher]
  -- The card the exile arm is for, the rest of its text: at max speed (CR
  -- 702.178a) the beginning of alice's combat returns what the Smasher exiled,
  -- with haste, and the next end step sacrifices it.
  Spec.it s "CR 702.178a at max speed the Smasher returns the duplicate with haste, and the end step sacrifices it" $ do
    (exiled, _, _) <- smogSmasherCombat s registry
    let returned = stepBegins (Phase.Combat CombatStep.BeginningOfCombat) (atSpeed 4 exiled)
        onBattlefield = namedIn hillGiant Zone.Battlefield returned
        ended = stepBegins (Phase.Ending EndingStep.EndStep) returned
    Spec.assertEqWith
      s
      "the duplicate entered alice's battlefield, hasty"
      (fmap (\oid -> Projection.hasKeyword Keyword.Haste oid returned) onBattlefield, namedIn hillGiant Zone.Exile returned)
      ([True], [])
    Spec.assertEqWith
      s
      "and the end step sacrificed it into alice's graveyard"
      (namedIn hillGiant Zone.Battlefield ended, length (namedIn hillGiant Zone.Graveyard ended))
      ([], 1)
  -- The pair: speed 3 is one short, so the Smasher has no such ability.
  Spec.it s "CR 702.178a one short of max speed, the duplicate stays in exile" $ do
    (exiled, _, _) <- smogSmasherCombat s registry
    let returned = stepBegins (Phase.Combat CombatStep.BeginningOfCombat) (atSpeed 3 exiled)
    Spec.assertEqWith
      s
      "the duplicate is still in alice's exile"
      (length (namedIn hillGiant Zone.Exile returned), namedIn hillGiant Zone.Battlefield returned)
      (1, [])

moxPearl :: CardName.CardName
moxPearl = CardName.MkCardName (Text.pack "Mox Pearl")

fearOfChange :: CardName.CardName
fearOfChange = CardName.MkCardName (Text.pack "Fear of Change")

llanowarElves :: CardName.CardName
llanowarElves = CardName.MkCardName (Text.pack "Llanowar Elves")

-- Smog Smasher attacks bob alone, unblocked, beside bob's Hill Giant, and its
-- combat damage trigger aims at the Giant, FILTERED out of the offered
-- recipients. The finished board comes back with the Smasher's id and the
-- Giant's.
smogSmasherCombat :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
smogSmasherCombat s registry = do
  smasher <- S.printingOf s registry "Smog Smasher"
  giant <- S.printingOf s registry "Hill Giant"
  let (gs, mine, theirs) = S.combatBoardOf [smasher] [giant]
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.DeclareAttackers _ _ ids -> ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> aimingAtAll theirs p
  case (mine, theirs) of
    ([smasherId], [giantId]) -> pure (S.runCombat answer gs, smasherId, giantId)
    _ -> Spec.assertFailure s "combatBoardOf placed one creature a side"

-- The step begins on alice's turn (CR 500.2's StepBegan is what a "beginning of"
-- trigger reads) and whatever triggered resolves.
stepBegins :: Phase.Phase -> GameState.GameState -> GameState.GameState
stepBegins phase gs =
  settleTriggers
    ( Event.recordEvent
        (GameEvent.StepBegan (StepBegan.MkStepBegan phase S.alice))
        (gs {GameState.phase = phase, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})
    )

-- Pawl.SpeedSpec's atSpeed: alice at exactly this speed.
atSpeed :: Numeric.Natural.Natural -> GameState.GameState -> GameState.GameState
atSpeed n gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.speed = Just n}) S.alice (GameState.players gs)}

giantSpider :: CardName.CardName
giantSpider = CardName.MkCardName (Text.pack "Giant Spider")

ancientVendetta :: CardName.CardName
ancientVendetta = CardName.MkCardName (Text.pack "Ancient Vendetta")

-- Fear of Change enters under alice, beside a Goblin Piker of hers where
-- `withPiker` says so, and its trigger resolves through
-- Pawl.Interpreter.lookingUpCards over a FIXTURE reference: Goblin Piker, Hill
-- Giant, Ancient Vendetta and Giant Spider, in that order. Where `lying` says
-- so, Prompt.ReferenceCards is answered with all four instead. Randomness is
-- answered with the first of Goblin Piker, Ancient Vendetta and Giant Spider
-- offered, FILTERED out of the offer, and the head otherwise; every offer is
-- logged.
fearOfChangeRun :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> Bool -> m (GameState.GameState, [[CardName.CardName]])
fearOfChangeRun s registry withPiker lying = do
  fear <- S.printingOf s registry "Fear of Change"
  piker <- S.printingOf s registry "Goblin Piker"
  reference <- mapM (S.cardOf s registry) ["Goblin Piker", "Hill Giant", "Ancient Vendetta", "Giant Spider"]
  let board0 = if withPiker then snd (S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)) else Setup.emptyGame S.bothPlayers
      (_, entered) = S.entersWithTrigger fear S.alice board0
      fixture =
        Registry.MkRegistry
          { Registry.fetchCard = \name -> pure (List.find (\card -> S.nameOf card == name) reference),
            Registry.cards = pure reference
          }
      answer :: Asked.Asked r -> State.State [[CardName.CardName]] r
      answer asked = case Asked.prompt asked of
        Prompt.RandomCard offered -> do
          State.modify' (NonEmpty.toList offered :)
          pure (Maybe.fromMaybe (NonEmpty.head offered) (List.find (`elem` NonEmpty.toList offered) [goblinPiker, ancientVendetta, giantSpider]))
        p -> pure (S.identityAnswer p)
      asking :: Asked.Asked r -> State.State [[CardName.CardName]] r
      asking asked = case Asked.prompt asked of
        Prompt.ReferenceCards {}
          | lying -> pure (fmap S.nameOf reference)
        _ -> Interpreter.lookingUpCards fixture answer asked
      run = do
        (_, settled) <- Engine.runGameAsked asking entered Engine.settleForPriority
        Engine.runGameAsked asking settled Engine.priorityLoop
      ((_, final), logged) = State.runState run []
  pure (final, reverse logged)

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

-- Island, Mountain, Plains, Goblin Piker, Clone, Sinister Reflections and
-- Cubwarden, which the merge-split cases share.
duplicatePrintings ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (Printing.Printing, Printing.Printing, Printing.Printing, Printing.Printing, Printing.Printing, Printing.Printing, Printing.Printing)
duplicatePrintings s registry =
  (,,,,,,)
    <$> S.printingOf s registry "Island"
    <*> S.printingOf s registry "Mountain"
    <*> S.printingOf s registry "Plains"
    <*> S.printingOf s registry "Goblin Piker"
    <*> S.printingOf s registry "Clone"
    <*> S.printingOf s registry "Sinister Reflections"
    <*> S.printingOf s registry "Cubwarden"

-- The Piker case's board, with Cubwarden's printing beside it and the duplicate
-- of a Clone copying the Piker in alice's hand.
duplicateFixture :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Printing.Printing, Maybe (ObjectId.ObjectId, GameState.GameState))
duplicateFixture s registry = do
  (island, mountain, plains, piker, clone, reflections, cubwarden) <- duplicatePrintings s registry
  let board0 = S.landsFor plains S.alice 8 (S.landsFor mountain S.alice 4 (S.landsInPlay island 4))
      (pikerId, board1) = S.addPermanent piker S.alice board0
  pure (cubwarden, duplicateInHand clone reflections pikerId board1)

-- The duplicate cast and resolved as the Piker, with Cubwarden then cast from
-- hand for its mutate cost OVER it: the merged permanent's id and the board.
mergedOntoDuplicate :: (Printing.Printing, Maybe (ObjectId.ObjectId, GameState.GameState)) -> Maybe (ObjectId.ObjectId, GameState.GameState)
mergedOntoDuplicate (cubwarden, found) = do
  (duplicate, gone) <- found
  let played = S.settleSba (S.runPure S.identityAnswer gone (S.cast S.alice duplicate >> Stack.resolveTop))
  host <- Maybe.listToMaybe (clonesOnBattlefield played)
  let (withCubwarden, spellId) = S.handOne cubwarden played
  pure (host, mergingOnto host withCubwarden spellId)

-- A Clone resolved as a copy of `original`, duplicated into alice's hand by
-- Sinister Reflections, then `original` and the Clone destroyed, so the
-- duplicate's printed card says Clone and nothing left on the battlefield
-- carries the values it was conjured with. Nothing where the Clone did not stay.
duplicateInHand :: Printing.Printing -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ObjectId.ObjectId, GameState.GameState)
duplicateInHand clone reflections original board0 = do
  (_, conjured) <- conjuredDuplicate clone reflections original board0
  cloneId <- Maybe.listToMaybe (clonesOnBattlefield conjured)
  let gone = S.settleSba (S.runPure S.identityAnswer conjured (Event.destroy Regenerability.Regenerable [original, cloneId]))
  duplicate <- Maybe.listToMaybe (namedIn cloneName Zone.Hand gone)
  pure (duplicate, gone)

-- duplicateInHand above short of the destruction: the original and the Clone
-- stay on the battlefield, and the one spell cast is Sinister Reflections.
conjuredDuplicate :: Printing.Printing -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ObjectId.ObjectId, GameState.GameState)
conjuredDuplicate clone reflections original board0 = do
  let (_, staged) = S.spellOnStack clone S.alice board0
      entered = S.settleSba (copyingPiker original staged)
  cloneId <- Maybe.listToMaybe (clonesOnBattlefield entered)
  let (spell, board1) = S.addHandCard reflections S.alice entered
      board = board1 {GameState.phase = Phase.PrecombatMain}
      resolved = S.settleSba (S.runPure (aimingAtAll [cloneId]) board (S.cast S.alice spell >> Stack.resolveTop))
  duplicate <- Maybe.listToMaybe (namedIn cloneName Zone.Hand resolved)
  pure (duplicate, resolved)

-- Casts `spellId` for Cubwarden's mutate cost at `host`, merging OVER, and
-- drains the stack. Pawl.MutateSpec's `merging`, duplicated rather than hoisted:
-- the cost is NAMED rather than indexed and the target FILTERED out of the
-- offered set, since CR 608.2b's re-read drops a hand-built recipient.
mergingOnto :: ObjectId.ObjectId -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
mergingOnto host board spellId =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseCost _ _ _ candidates ->
          Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just cubwardenMutateCost) . Cost.Type.mana) candidates)
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just host) . Recipient.objectOf) . snd) sets
        Prompt.ChooseMutateSide {} -> MutateSide.Over
        _ -> S.identityAnswer p
      cast = S.runPure answer board (S.cast S.alice spellId)
   in S.runPure answer cast (Monad.replicateM_ 6 (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)

-- Cubwarden's mutate cost, {2}{W}{W}, as the announcement names it.
cubwardenMutateCost :: ManaCost.ManaCost
cubwardenMutateCost = ManaCost.MkManaCost [ManaSymbol.Generic 2, white, white]
  where
    white = ManaSymbol.OfType (ManaType.Colored Color.White)

cubwardenName :: CardName.CardName
cubwardenName = CardName.MkCardName (Text.pack "Cubwarden")

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

-- A copier (Clone) resolved as a copy of `original`, then duplicated into
-- alice's hand by Sinister Reflections: the duplicate and the board. Found by
-- the board's difference rather than by name, since the copier offers the
-- copied card's faces.
duplicateOfCopy :: Printing.Printing -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ObjectId.ObjectId, GameState.GameState)
duplicateOfCopy copier reflections original board0 = do
  let (_, staged) = S.spellOnStack copier S.alice board0
      entered = S.settleSba (copyingPiker original staged)
  copyId <- Maybe.listToMaybe (Set.toList (Set.difference (GameState.battlefield entered) (GameState.battlefield board0)))
  duplicateOf reflections copyId entered

-- Sinister Reflections cast at `target` and resolved: the one card it added to
-- alice's hand, and the board.
duplicateOf :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ObjectId.ObjectId, GameState.GameState)
duplicateOf reflections target board0 = do
  let (spell, board1) = S.addHandCard reflections S.alice board0
      resolved = S.settleSba (S.runPure (aimingAtAll [target]) board1 (S.cast S.alice spell >> Stack.resolveTop))
  case filter (`notElem` Game.zoneMembers Zone.Hand S.alice board1) (Game.zoneMembers Zone.Hand S.alice resolved) of
    [duplicate] -> Just (duplicate, resolved)
    _ -> Nothing

-- Is casting this card as the named half among alice's legal actions, with
-- priority hers? The offer, which Cast.castable asked of a name does not check.
castOffered :: ObjectId.ObjectId -> CardName.CardName -> GameState.GameState -> Bool
castOffered oid name gs = elem (Action.Cast oid name Facing.FaceUp) (Action.legalActions S.alice gs {GameState.priority = Just S.alice})

dryadArbor :: CardName.CardName
dryadArbor = CardName.MkCardName (Text.pack "Dryad Arbor")

ainokTracker :: CardName.CardName
ainokTracker = CardName.MkCardName (Text.pack "Ainok Tracker")

welcomeHome :: CardName.CardName
welcomeHome = CardName.MkCardName (Text.pack "Welcome Home")

bearToken :: CardName.CardName
bearToken = CardName.MkCardName (Text.pack "Bear Token")

spikedCorridor :: CardName.CardName
spikedCorridor = CardName.MkCardName (Text.pack "Spiked Corridor")

torturePit :: CardName.CardName
torturePit = CardName.MkCardName (Text.pack "Torture Pit")

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

-- The cards a printing's SPELL conjures, conjuredBy's read one ability kind over.
spellConjures :: Printing.Printing -> [Card.Card]
spellConjures printing =
  [ card
  | face <- NonEmpty.toList (Card.faces (Printing.card printing)),
    mode <- Foldable.toList (Modal.modes (Face.spell face)),
    clause <- Foldable.toList (Mode.clauses mode),
    Effect.Conjure conjure <- Foldable.toList (Clause.effects clause),
    card <- ConjureCards.written (Conjure.cards conjure)
  ]

-- Activates the Gate the first time its ability is offered, taps any OTHER land
-- for mana until it is, and pins the random pick to `who`, filtered out of the
-- offer for tomeAnswer's reason. Never the Gate's own mana ability, which would
-- tap away its {T} cost. A shuffle is answered REVERSED, so one the engine asked
-- for would show in the library's order.
gateAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
gateAnswer gate who p = case p of
  Prompt.ChooseAction _ _ actions -> case List.find (activationOf gate) actions of
    Just action -> action
    Nothing -> case List.find (maybe False (/= gate) . manaSource) actions of
      Just action -> action
      Nothing -> Action.Pass
  Prompt.RandomObject offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find (== who) (NonEmpty.toList offered))
  Prompt.Shuffle cards -> reverse cards
  _ -> S.identityAnswer p

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
