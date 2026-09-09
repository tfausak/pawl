{-# LANGUAGE GADTs #-}

-- Covers: CR 406.3's face-down exile -- Object.exiledFaceDown, CR 406.3a's
-- absent characteristics as Pawl.Engine.Projection.gatherGiven's exile walk
-- reads them, the EntryRiders.exiledFaceDown rider
-- Pawl.Engine.Event.changeZoneEntering reads,
-- CR 406.3's instruction-given look permission -- Object.exileLookers as
-- Effect.GrantLookAtExiled writes it, whether a spell's own effect writes it or
-- CR 702.75a's hideaway does, and as Pawl.Engine.Exile.accrueLookers samples rule
-- 406.3's continuing permission onto it --
-- CR 406.4's two halves over Pawl.Engine.Target's Pool.CardsInExile arm -- the
-- permission Pawl.Engine.Exile.mayLookAt answers, the pile
-- Pawl.Engine.Exile.pileOf sorts a card into and Pawl.Engine.Target's piledOffer
-- offers instead, and the draw drawFromPiles takes out of it, taken over the
-- WHOLE pile, elided at a one-card pile, filtered back against the pile and asked
-- with Game.ask rather than Game.choose (CR 104.4b) -- and
-- ObjectRef.EachCardInYourHand as Pawl.Engine.Resolve sweeps it.
--
-- CR 406.4's separate piles are read off the same Ignorant Bliss: one casting
-- makes one pile of what it hid, two castings make two, and which pile a chooser
-- names is which card the draw can hand them.
--
-- Gameplay-level, off three producers that exile face down and differ in exactly
-- the permission. Ignorant Bliss {1}{R} Instant -- "Exile all cards from your
-- hand face down" -- grants nobody a look, so not even the owner may choose what
-- it exiled; foretell (CR 702.143a) grants the owner one, so she may; Extract
-- Power looks at the cards before hiding them, which rule 406.3 turns into a
-- permission for the LOOKER, who owns only one of them. Synthetic
-- Blind Reclamation's unqualified "target exiled card" is what reads the
-- difference back out, and Riftsweeper's printed "face-up exiled card" is the
-- third reading -- a card whose own words refuse what rule 406.4 offers. Runic
-- Repetition is the fourth: a restriction a pile only half satisfies, which is
-- what the draw runs over the whole pile for. Windbrisk Heights is the reading
-- no spell reaches: CR 702.75a's keyword names the permanent's controller, and
-- rule 406.3 then keeps the look with every seat that read has ever named.
--
-- Each group shares ONE board across its readings, which is the point: exile
-- holds the same cards either way, and only how they got there differs.
module Pawl.ExileSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Exile as Exile
import qualified Pawl.Engine.Foretell as Foretell
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Pile as Pile
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Face-down exile" $ do
  foretold s registry
  runicRepetition s registry
  extractPower s registry
  windbriskHeights s registry
  Spec.describe s "Ignorant Bliss" $ do
    -- CR 406.3a and CR 406.4's first half, read through the pool that offers
    -- exiled cards as targets. The board is deliberately one board: alice's two
    -- hand cards go to exile face down by casting the card, and a THIRD card is
    -- already sitting there face up. A gate that had not been written would offer
    -- all three; a gate that emptied the pool outright would offer none.
    --
    -- Read through the UNQUALIFIED slot, so what refuses the two face-down cards
    -- is rule 406.4 rather than a card's printed word -- and refuses them to
    -- ALICE, who owns them and cast the spell that hid them. Ignorant Bliss
    -- grants no look, and CR 406.3's permission comes only from an instruction
    -- that gives one; owning the card is not one.
    Spec.it s "CR 406.4 a card exiled face down is named by nobody, not even its owner, and offered as the pile it is in" $ do
      board <- castBliss s registry
      reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
      case S.spellTargetSlot reclamation of
        Just theSlot -> do
          Spec.assertEqWith s "all three cards are in the exile zone" (Set.size (GameState.exile board)) 3
          Spec.assertEqWith
            s
            "the offer names the face-up card and the PILE the two face-down ones are in (CR 406.4)"
            (offerTo S.alice theSlot board)
            (Set.union (Set.fromList (fmap Recipient.ToPile (pilesIn board))) (Set.fromList (fmap Recipient.ToObject (faceUpExiled board))))
          Spec.assertEqWith s "and that is ONE pile, both cards having been hidden by one casting" (length (pilesIn board)) 1
          Spec.assertEqWith s "and the face-up card is exactly one" (length (faceUpExiled board)) 1
          -- The other half of the same rule: the pile stands for cards that are
          -- LEGAL targets -- CR 406.4 restricts the announcement, not legality,
          -- and CR 608.2b re-derives this set at resolution.
          Spec.assertEqWith
            s
            "while all three remain legal targets, which is what the drawn card needs at CR 608.2b"
            (Target.legalRecipients (Just S.alice) S.noSource theSlot board)
            (Set.fromList (fmap Recipient.ToObject (Set.toList (GameState.exile board))))
        Nothing -> Spec.assertFailure s "Synthetic Blind Reclamation should print one target slot"
    -- CR 406.4's second half at gameplay level, and the only board in the tree
    -- with a pile of MORE THAN ONE card -- Ignorant Bliss exiles a whole hand in
    -- one go, where each foretold card is a pile of its own (CR 702.143e). So the
    -- draw is a real draw here: the answerer takes the pile, randomness is
    -- answered with the LAST of the two cards it holds, and which card left exile
    -- is what tells that answer from the first one.
    Spec.it s "CR 406.4 choosing the pile shuffles the card the random draw named, not the other one" $ do
      (drawn, other, board) <- pileBoard s registry
      sentry <- S.printingOf s registry "Ogre Sentry"
      let after = S.runPure S.identityAnswer board Engine.priorityLoop
      Spec.assertEqWith
        s
        "the card the draw named joined its owner's library, and the other card of its pile did not"
        (namesIn Zone.Library S.alice after)
        (Set.fromList [drawn, S.printingName sentry])
      -- Proxies, AFTER the behaviour so neither can absorb a mutation: the spell
      -- did resolve, and it took exactly one card out of exile.
      Spec.assertEqWith s "the other card of the pile is the one still in exile face down" (namesOf (faceDownExiled after) after) (Set.singleton other)
      Spec.assertEqWith s "one card left exile, of the three that were there" (Set.size (GameState.exile after)) 2
    -- CR 406.4's FIRST sentence, which the case above leaves untested: "face-down
    -- cards in exile should be kept in separate piles based on when they were
    -- exiled and how they were exiled". Two castings of Ignorant Bliss are two
    -- instructions at two times, so the three cards they hide are two piles and
    -- not one, and a chooser who may not look picks WHICH pile to draw out of.
    --
    -- The pile that is named holds ONE card and it is not the one a merged pile
    -- would hand over: the answerer takes the draw's LAST candidate, which over
    -- the first casting's pile is Goblin Piker and over all three cards is the
    -- last card the second casting hid. So the card that reaches alice's library
    -- is what tells two piles from one.
    Spec.it s "CR 406.4 two castings of one spell make two piles, and the draw comes out of the pile that was named" $ do
      (pile, hidden, spellId, board) <- twoPileBoard s registry
      sentry <- S.printingOf s registry "Ogre Sentry"
      reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
      case (pile, S.spellTargetSlot reclamation) of
        (Just firstPile, Just theSlot) -> do
          let after = S.runPure S.identityAnswer (S.runPure (throughPileOf firstPile) board (S.cast S.alice spellId)) Engine.priorityLoop
          Spec.assertEqWith
            s
            "the one card of the named pile joined alice's library, and neither card of the other pile did"
            (namesIn Zone.Library S.alice after)
            (Set.fromList [hidden, S.printingName sentry])
          -- Proxies, AFTER the behaviour so neither can absorb a mutation: the
          -- three hidden cards reach the offer as two candidates, and the spell
          -- took one card out of exile.
          Spec.assertEqWith s "all three cards are hidden in exile" (length (faceDownExiled board)) 3
          Spec.assertEqWith s "and they are offered as the two piles the two castings made" (Set.size (offerTo S.alice theSlot board)) 2
          Spec.assertEqWith s "the other pile's two cards are still in exile" (Set.size (GameState.exile after)) 2
        _ -> Spec.assertFailure s "the first casting should make a pile, and Synthetic Blind Reclamation print one target slot"
    -- CR 406.4's draw is a QUESTION only where the pile holds more than one
    -- card, which is CLAUDE.md's second invariant: where the rules leave nothing
    -- to ask, don't prompt. A pair of piles on ONE board differing in exactly
    -- that -- the first casting of Ignorant Bliss hid one card, the second hid
    -- two -- so a board with no pile at all cannot pass for "not asked".
    --
    -- CR 702.143e is why this is not a corner: every foretold card is a pile of
    -- its own, so every draw out of one would otherwise raise a prompt with one
    -- candidate.
    Spec.it s "CR 406.4 a pile of one card is drawn from without asking, and a pile of two is asked about" $ do
      (pile, hidden, spellId, board) <- twoPileBoard s registry
      sentry <- S.printingOf s registry "Ogre Sentry"
      case (pile, otherPileThan pile board) of
        (Just onePile, Just twoPile) -> do
          let draws named = State.execState (Engine.runGame (countingDraws named) board (S.cast S.alice spellId)) (0 :: Int)
          Spec.assertEqWith s "the pile of one card leaves nothing to draw, so the draw is not raised" (draws onePile) 0
          Spec.assertEqWith s "the pile of two on the same board is a real draw, and is asked about once" (draws twoPile) 1
          -- Proxies, AFTER the behaviour so neither can absorb a mutation: the
          -- elided draw still hands over the pile's one card, which is what
          -- makes the two options indistinguishable.
          let after = S.runPure S.identityAnswer (S.runPure (throughPileOf onePile) board (S.cast S.alice spellId)) Engine.priorityLoop
          Spec.assertEqWith s "and the unasked pile's one card is still what the draw hands over" (namesIn Zone.Library S.alice after) (Set.fromList [hidden, S.printingName sentry])
          Spec.assertEqWith s "the two piles hold one card and two" (fmap (length . membersOfPile board) [onePile, twoPile]) [1, 2]
        _ -> Spec.assertFailure s "the two castings should make two piles"
    -- CR 406.4's draw is answered by the interpreter, so the answer is FILTERED
    -- back against the pile rather than trusted -- the posture
    -- Pawl.Engine.Resolve's RandomObject and RandomOpponent arms and
    -- Pawl.Engine.Engine's RandomFirstPlayer all take.
    --
    -- The smuggled card is the OTHER pile's, which is the reading a legality
    -- check cannot catch: rule 406.4 keeps every exiled card a legal target
    -- (the case above), so Target.selectionLegal admits it and only this filter
    -- refuses it.
    Spec.it s "CR 406.4 a draw answered with a card outside the named pile falls back to a card in it" $ do
      (pile, hidden, spellId, board) <- twoPileBoard s registry
      sentry <- S.printingOf s registry "Ogre Sentry"
      case (pile, otherPileThan pile board) of
        (Just onePile, Just twoPile) -> case (membersOfPile board onePile, membersOfPile board twoPile) of
          ([smuggled], firstOfTwo : _) -> do
            let after = S.runPure S.identityAnswer (S.runPure (namingOutside twoPile smuggled) board (S.cast S.alice spellId)) Engine.priorityLoop
            Spec.assertEqWith
              s
              "the card that joined alice's library is the named pile's own, not the card the answer smuggled in"
              (namesIn Zone.Library S.alice after)
              (Set.insert (S.printingName sentry) (namesOf [firstOfTwo] board))
            -- Proxies, AFTER the behaviour so neither can absorb a mutation: the
            -- smuggled card is the whole of the other pile and stayed put, and
            -- the spell took one card out of exile.
            Spec.assertBool s (Set.member hidden (namesOf (faceDownExiled after) after)) "the smuggled card, the whole of the other pile, is still hidden in exile"
            Spec.assertEqWith s "and one card of the three left exile" (Set.size (GameState.exile after)) 2
          _ -> Spec.assertFailure s "the first casting should make a pile of one card and the second a pile of two"
        _ -> Spec.assertFailure s "the two castings should make two piles"
    -- CR 104.4b, Pawl.InvestigateSpec's random reveal one rule over: being asked
    -- for randomness is not being offered a CHOICE, so CR 406.4's draw goes
    -- through Game.ask and leaves GameState.lastChoice alone. Otherwise a loop
    -- of mandatory actions containing such a draw would look interruptible and
    -- Pawl.Engine.Engine.checkMandatoryLoop could never call it a draw.
    --
    -- Driven through drawFromPiles itself rather than through a cast, because
    -- announcing the target is a choice (Prompt.ChooseTargets) and would stamp
    -- the field either way.
    Spec.it s "CR 104.4b the draw out of a pile is not an optional action" $ do
      exiled <- castBliss s registry
      case pilesIn exiled of
        [pile] -> do
          let board = exiled {GameState.lastChoice = Timestamp.MkTimestamp 0}
              (drawn, after) = S.runPureWith throughPile board (Target.drawFromPiles (Just S.alice) (Set.singleton (Recipient.ToPile pile)))
          case membersOfPile board pile of
            [_, second] -> Spec.assertEqWith s "the draw was honoured, naming the last card of the pile" (Set.toList drawn) [Recipient.ToObject second]
            _ -> Spec.assertFailure s "the casting should hide two cards in one pile"
          Spec.assertEqWith s "and nobody was recorded as having been offered a choice" (GameState.lastChoice after) (Timestamp.MkTimestamp 0)
        _ -> Spec.assertFailure s "one casting should make one pile"
    -- The same gate from the other side, on a board differing in ONE thing: the
    -- two cards reach exile FACE UP instead, by the same route every other test
    -- puts a card there. Same printings, same seats, same zone, same count -- so a
    -- pool that had simply lost its exile candidates could not pass this.
    Spec.it s "CR 406.3 the same two cards exiled FACE UP are all legal targets" $ do
      riftsweeper <- S.printingOf s registry "Riftsweeper"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      sentry <- S.printingOf s registry "Ogre Sentry"
      let (upA, g1) = S.addExiledCard piker S.alice (Setup.emptyGame S.bothPlayers)
          (upB, g2) = S.addExiledCard bolt S.alice g1
          (upC, board) = S.addExiledCard sentry S.bob g2
      case triggerTargetSlot riftsweeper of
        Just theSlot ->
          Spec.assertEqWith
            s
            "CR 406.3's face-up default leaves every exiled card choosable"
            (Target.legalRecipients (Just S.alice) S.noSource theSlot board)
            (Set.fromList (fmap Recipient.ToObject [upA, upB, upC]))
        Nothing -> Spec.assertFailure s "Riftsweeper should print one triggered ability with one target slot"
    -- CR 400.7 through the delayed ability: "return THOSE CARDS to your hand, then
    -- draw a card". The slot the exiling move bound is what survives the
    -- resolution (CR 603.7c), and the cards come back as ordinary face-up cards in
    -- a hand -- which is Object.exiledFaceDown being per-incarnation state.
    Spec.it s "CR 603.7 at the next end step the exiled cards return to hand and alice draws" $ do
      board <- castBliss s registry
      let after = endStep board
      Spec.assertEqWith s "alice's hand was emptied by the spell" (S.handSize S.alice board) 0
      Spec.assertEqWith s "two cards returned, plus the draw" (S.handSize S.alice after) 3
      Spec.assertEqWith s "only the face-up card is left in exile" (Set.size (GameState.exile after)) 1
      -- A REGRESSION FENCE, not a proof: the returning move carries the default
      -- rider, so its arrival is face up by construction rather than by any
      -- guard. Dropping Event.changeZoneAttaching's destination gate leaves
      -- this green.
      Spec.assertEqWith s "and nothing in a hand is face down in exile" (concealedIn Zone.Hand S.alice after) []
      Spec.assertEqWith s "the delayed ability was spent" (length (GameState.delayedTriggers after)) 0
    -- CR 406.3a: a card exiled face down has NO characteristics, so a CR 113.6c
    -- ability that functions from exile does not function from under one.
    -- Grist, the Hunger Tide is a 1/1 Insect creature card in every zone but the
    -- battlefield, which Pawl.ProjectionSpec's HiddenZoneStatics group reads off
    -- a face-up exiled Grist; here the same card reaches the same zone by
    -- Ignorant Bliss instead, and the pair differs in exactly one thing -- CR
    -- 406.3's face-down flag.
    Spec.it s "CR 406.3a a Grist card exiled FACE DOWN has no characteristics to function from" $ do
      bliss <- S.printingOf s registry "Ignorant Bliss"
      mountain <- S.printingOf s registry "Mountain"
      grist <- S.printingOf s registry "Grist, the Hunger Tide"
      piker <- S.printingOf s registry "Goblin Piker"
      let (g1, blissId) = S.handOne bliss (S.landsInPlay mountain 2)
          (_, g2) = S.addHandCard grist S.alice g1
          (_, g3) = S.addLibraryCard piker S.alice g2
          hidden = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (S.cast S.alice blissId)) Engine.priorityLoop
          (faceUpGrist, shown) = S.addExiledCard grist S.alice (Setup.emptyGame S.bothPlayers)
      case faceDownExiled hidden of
        [downGrist] -> do
          Spec.assertEqWith
            s
            "no 1/1 under the face-down flag, where the same card exiled face up is one"
            (S.powerToughnessOf downGrist hidden, S.powerToughnessOf faceUpGrist shown)
            (Nothing, Just (1, 1))
          Spec.assertEqWith s "and it is the Grist that Ignorant Bliss hid" (namesOf [downGrist] hidden) (Set.singleton (S.printingName grist))
        _ -> Spec.assertFailure s "the casting should hide exactly the Grist"

-- CR 702.143a's foretold card is the pool's one grant of CR 406.3's permission to
-- look at a card exiled face down, and CR 406.4 is what turns a permission to
-- LOOK into a permission to CHOOSE: "the player may choose a specific face-down
-- card only if the player is allowed to look at that card".
--
-- ONE board, cast twice. alice foretells Augury Raven and bob has an Ogre Sentry
-- exiled face up, so exile holds two cards under either reading and a pool that
-- had simply lost its exile candidates could not pass. Both players then cast the
-- same instant with the SAME answerer, aimed at the foretold card: alice reaches
-- it and bob's aim falls through to the face-up card, which is CR 406.4's whole
-- content and the one thing the two casts differ in.
foretold :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
foretold s registry = Spec.describe s "Augury Raven" $ do
  Spec.it s "CR 406.4 the owner of a foretold card shuffles it out of exile and an opponent aiming at it by name gets the face-up one" $ do
    (downId, upId, aliceSpell, bobSpell, board) <- foretoldBoard s registry
    let aliceAfter = resolveCast S.alice aliceSpell downId board
        bobAfter = resolveCast S.bob bobSpell downId board
    Spec.assertEqWith
      s
      "alice may look at her foretold card, so her spell takes it and leaves the face-up one"
      (Set.toList (GameState.exile aliceAfter))
      [upId]
    Spec.assertEqWith
      s
      "bob may not, so the same spell aimed the same way takes the face-up card instead"
      (Set.toList (GameState.exile bobAfter))
      [downId]
    -- Two proxies AFTER the behaviour, so neither can absorb a mutation: the
    -- shuffle is into the chosen card's OWNER's library, so the two casts stock
    -- different libraries.
    Spec.assertEqWith s "the card alice took went to her own library" (length (Game.zoneMembers Zone.Library S.alice aliceAfter)) 1
    Spec.assertEqWith s "and the card bob took went to bob's, its owner's" (length (Game.zoneMembers Zone.Library S.bob bobAfter)) 1
  -- The same permission read off the pool rather than off a resolution, and the
  -- third leg is what keeps CR 406.4's permission and Riftsweeper's printed
  -- qualifier from being one question: rule 406.4 OFFERS alice her foretold card,
  -- and Riftsweeper's own "face-up exiled card" then refuses it.
  Spec.it s "CR 406.4 the pool names a face-down exiled card to a player who may look at it, and offers the others its pile" $ do
    reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    (downId, upId, _, _, board) <- foretoldBoard s registry
    case (S.spellTargetSlot reclamation, triggerTargetSlot riftsweeper) of
      (Just unqualified, Just faceUpOnly) -> do
        Spec.assertEqWith
          s
          "alice is offered both exiled cards by name"
          (offerTo S.alice unqualified board)
          (Set.fromList (fmap Recipient.ToObject [downId, upId]))
        -- CR 702.143e keeps a foretold card differentiable from every other
        -- face-down card its owner owns, so its pile holds it alone -- and the
        -- pile bob is offered is that one rather than alice's face-down cards at
        -- large.
        Spec.assertEqWith
          s
          "bob is offered the face-up one by name and the foretold card only as its own pile"
          (offerTo S.bob unqualified board)
          (Set.fromList [Recipient.ToObject upId, Recipient.ToPile (Pile.OfForetold (timestampOf downId board))])
        Spec.assertEqWith
          s
          "and Riftsweeper's printed face-up qualifier refuses the foretold card even to alice, offering no pile in its place"
          (offerTo S.alice faceUpOnly board)
          (Set.singleton (Recipient.ToObject upId))
      _ -> Spec.assertFailure s "Riftsweeper and Synthetic Blind Reclamation should each print one target slot"

-- alice holds four Islands and foretells Augury Raven off two of them (CR
-- 116.2h), leaving exactly the {1}{U} her copy of the instant costs; bob holds
-- two Islands and a copy of his own. An Ogre Sentry sits in exile face up under
-- bob, so the pool is never empty and the shuffle's destination tells the two
-- cards apart by owner.
--
-- Returns the face-down foretold card, the face-up one, and each player's spell.
foretoldBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
foretoldBoard s registry = do
  island <- S.printingOf s registry "Island"
  raven <- S.printingOf s registry "Augury Raven"
  sentry <- S.printingOf s registry "Ogre Sentry"
  reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
  let (ravenId, g1) = S.addHandCard raven S.alice (S.landsInPlay island 4)
      (upId, g2) = S.addExiledCard sentry S.bob g1
      (aliceSpell, g3) = S.addHandCard reclamation S.alice g2
      (bobSpell, g4) = S.addHandCard reclamation S.bob g3
      g5 = S.landsFor island S.bob 2 g4
      before =
        g5
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      board = S.runPure S.identityAnswer before (Foretell.foretell S.alice ravenId)
      downId = case faceDownExiled board of
        [only] -> only
        _ -> S.noSource
  pure (downId, upId, aliceSpell, bobSpell, board)

-- CR 406.3's OTHER permission, the one an instruction gives: "if a player is
-- instructed to look at a card and then exile it face down ... that player may
-- continue to look at that card until it leaves the exile zone". Extract Power
-- {5}{U} Sorcery -- "Look at the top card of each player's library, then exile
-- those cards face down. You may play them without paying their mana costs for
-- as long as they remain exiled" (Oracle text checked 2026-09-08) -- is the
-- producer, and it is the one shape that tells the permission apart from
-- OWNERSHIP: alice is instructed to look at bob's card too, so she may name a
-- card she does not own, while bob may name neither -- not even his own.
--
-- Against Ignorant Bliss above, which is the same board minus the instruction:
-- that spell exiles cards face down without looking at them first, and its
-- owner may name none of them.
--
-- The card is transcribed whole: its GrantPlayFromExile carries CR 118.9's
-- waiver, which the second case below proves by casting one of the exiled cards
-- off a board with no untapped land on it.
--
-- Not implemented: CR 406.3a's turn face up as the card is played. Both cards
-- ARE offered and cast today -- Pawl.Engine.Cast.proposedFace branches on
-- Object.facing, which no exile rider writes, and never on
-- Object.exiledFaceDown -- so what is missing is the ordering: the cast is
-- announced while the card is still exiled face down, and nothing consults CR
-- 406.3b or CR 601.3f there (#3434).
extractPower :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extractPower s registry = Spec.describe s "Extract Power" $ do
  Spec.it s "CR 406.3 the player the exiling instruction let look names both cards, and the owner who was shown nothing gets their pile" $ do
    reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
    board <- castExtractPower s registry
    case S.spellTargetSlot reclamation of
      Just theSlot -> do
        Spec.assertEqWith
          s
          "alice looked at both cards as they were exiled, so both are offered to her by name -- bob's included"
          (offerTo S.alice theSlot board)
          (Set.fromList (fmap Recipient.ToObject (faceDownExiled board)))
        Spec.assertEqWith
          s
          "bob was shown nothing, so neither card is offered to him by name, his own included (CR 406.4)"
          (offerTo S.bob theSlot board)
          (Set.fromList (fmap Recipient.ToPile (pilesIn board)))
        -- Proxies, AFTER the two behavioural assertions so neither can absorb a
        -- mutation: one instruction hid two cards, so they are ONE pile, and
        -- both seats own one of them.
        Spec.assertEqWith s "two cards were exiled face down, one from each library" (length (faceDownExiled board)) 2
        Spec.assertEqWith s "and one instruction hid them, so they are one pile" (length (pilesIn board)) 1
        Spec.assertEqWith
          s
          "the two cards are owned by different seats, so ownership cannot be what the offer read"
          (Set.fromList (Maybe.mapMaybe (\oid -> fmap Object.owner (Game.lookupObject oid board)) (faceDownExiled board)))
          (Set.fromList [S.alice, S.bob])
      Nothing -> Spec.assertFailure s "Synthetic Blind Reclamation should print one target slot"
  -- CR 118.9's other clause of the same sentence: "You may play them WITHOUT
  -- PAYING THEIR MANA COSTS for as long as they remain exiled". The permission
  -- Pawl.Engine.Cost prices the cast from is the only thing making it legal, so
  -- the waiver replaces the printed cost rather than joining it.
  --
  -- The board is the group's own, which is what makes the claim about the COST
  -- rather than about mana: Extract Power {5}{U} took every one of alice's six
  -- Islands, so she has nothing at all to spend, and the Goblin Piker she
  -- exiled off her own library prints {1}{R} -- a cost those Islands could not
  -- have paid even untapped. Casting it is therefore possible only because
  -- nothing is owed.
  Spec.it s "CR 118.9 the exiled card is cast for nothing, off a board with no untapped land and no red source" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    board <- castExtractPower s registry
    case filter (\oid -> fmap S.nameOf (Game.cardOf oid board) == Just (S.printingName piker)) (faceDownExiled board) of
      [pikerId] -> do
        let resolved = S.runPure S.castAnswer board (S.cast S.alice pikerId >> Stack.resolveTop)
        -- The GATE and the CAST are two roads through Cost.candidateCostsGiven
        -- and both are asserted, so a waiver honoured by one and not the other
        -- cannot pass. Each was mutated red on its own: the gate here, and the
        -- battlefield below with this line ordered after it.
        Spec.assertBool s (S.castable S.alice pikerId board) "the cast is offered: CR 601.3's permission with a cost alice can pay"
        Spec.assertEqWith
          s
          "and the Goblin Piker is on the battlefield, so the announcement found that cost too"
          (S.countOnBattlefieldByName (S.printingName piker) S.alice resolved)
          1
        -- A proxy, AFTER both behavioural assertions so neither can absorb a
        -- mutation: the six Islands that paid for Extract Power are still the
        -- only tapped permanents alice has.
        Spec.assertEqWith s "alice's six Islands are still the only tapped permanents she has" (S.tappedCount S.alice resolved) 6
      _ -> Spec.assertFailure s "Extract Power should exile alice's Goblin Piker face down"

-- alice casts Extract Power off six Islands. Each library's top card is a
-- DIFFERENT one, so a failure names which seat's card came out where, and each
-- holds a second card beneath it so neither library empties (CR 104.3c).
castExtractPower :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
castExtractPower s registry = do
  power <- S.printingOf s registry "Extract Power"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  bolt <- S.printingOf s registry "Lightning Bolt"
  let (g1, powerId) = S.handOne power (S.landsInPlay island 6)
      (_, g2) = S.addLibraryCard bolt S.alice g1
      (_, g3) = S.addLibraryCard piker S.alice g2
      (_, g4) = S.addLibraryCard bolt S.bob g3
      (_, g5) = S.addLibraryCard sentry S.bob g4
  pure (S.runPure S.identityAnswer (S.runPure S.identityAnswer g5 (S.cast S.alice powerId)) Engine.priorityLoop)

-- CR 702.75a's hideaway N, an instruction-given look that no spell gives:
-- "When this permanent enters, look at the top N cards of your library. Exile one
-- of them face down and put the rest on the bottom of your library in a random
-- order. The exiled card gains 'The player who controls the permanent that
-- exiled this card may look at this card in the exile zone.'" Windbrisk Heights
-- Land -- "Hideaway 4 ... This land enters tapped. {T}: Add {W}. {W}, {T}: You
-- may play the exiled card without paying its mana cost if you attacked with
-- three or more creatures this turn" (Oracle text checked 2026-09-08) -- is the
-- producer.
--
-- The ability printed under it is Effect.OfferCast over
-- ObjectRef.EachCardExiledWithSource -- CR 607.2a's linked set -- under a
-- Clause.condition comparing Quantity.AttackersDeclaredThisTurn against 3.
--
-- Not implemented: CR 116.2a's land drop, which CR 305.2a reaches during a
-- resolution where Effect.OfferCast reaches only CR 601's cast, so a hidden LAND
-- cannot be played and pawl's Windbrisk Heights is stricter than printed there
-- (#3347).
--
-- Where Extract Power above tells the look apart from OWNERSHIP, this group
-- tells it apart from CONTROL OF THE SPELL: the looker is named by a keyword
-- ability nobody cast.
windbriskHeights :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
windbriskHeights s registry = Spec.describe s "Windbrisk Heights" $ do
  Spec.it s "CR 702.75a hideaway 4 hides the card its controller named and bottoms the other three, and only she may name it afterwards" $ do
    reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
    (fifth, hidden, bottomed, _, board) <- playHeights s registry Nothing
    case S.spellTargetSlot reclamation of
      Just theSlot -> do
        Spec.assertEqWith
          s
          "the card alice named out of the four she looked at is the one in exile face down"
          (namesOf (faceDownExiled board) board)
          (Set.singleton hidden)
        Spec.assertEqWith
          s
          "the other three sit under the card the look never reached, in the order the random-order channel handed back"
          (orderedNames (Game.zoneMembers Zone.Library S.alice board) board)
          (fifth : bottomed)
        Spec.assertEqWith
          s
          "and alice, who controls the land that exiled it, is the one seat offered the card by name"
          (offerTo S.alice theSlot board)
          (Set.fromList (fmap Recipient.ToObject (faceDownExiled board)))
        -- Proxies, AFTER the three behavioural assertions so none of them can
        -- absorb a mutation: bob is the seat rule 702.75a does not name, and one
        -- card of the four went to exile rather than four or none.
        Spec.assertEqWith
          s
          "bob controls nothing that exiled it, so he is offered the pile instead (CR 406.4)"
          (offerTo S.bob theSlot board)
          (Set.fromList (fmap Recipient.ToPile (pilesIn board)))
        Spec.assertEqWith s "exactly one card is in exile face down" (length (faceDownExiled board)) 1
      Nothing -> Spec.assertFailure s "Synthetic Blind Reclamation should print one target slot"

  -- THE COPY TRIPWIRE. Vesuva enters as a copy of the Windbrisk Heights already
  -- on the battlefield, so its hideaway is read off CR 707.2's copiable values
  -- rather than off the printed card -- which Vesuva's own face does not have.
  -- A read of Game.cardOf anywhere on this path answers "Vesuva", which has no
  -- keywords at all, and nothing is exiled.
  Spec.it s "CR 707.2 a land that entered as a copy of Windbrisk Heights hides a card of its own" $ do
    (_, hidden, _, _, board) <- playHeights s registry (Just "Vesuva")
    Spec.assertEqWith
      s
      "the copy's hideaway ran, and the card its controller named is in exile face down"
      (namesOf (faceDownExiled board) board)
      (Set.singleton hidden)

  -- CR 702.75a's granted ability names whoever controls the exiling permanent
  -- when the question is ASKED, so a steal moves the look with the land.
  -- Confiscate {4}{U}{U} Enchantment -- Aura -- "Enchant permanent / You control
  -- enchanted permanent" (Oracle text checked 2026-09-09) -- is the pool's steal
  -- that reaches a land; it is placed and attached rather than cast, control of
  -- the land being all the rule reads.
  --
  -- Alice keeps her look through the steal, and CR 406.3 is why: she was
  -- instructed to look at the four cards and then exiled one of them face down,
  -- which is that rule's own continuing permission and owes the land nothing.
  Spec.it s "CR 702.75a the look follows control of the land that exiled the card, and CR 406.3's does not" $ do
    reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
    confiscate <- S.printingOf s registry "Confiscate"
    (_, _, _, land, board) <- playHeights s registry Nothing
    let stolen = steal confiscate land board
    case S.spellTargetSlot reclamation of
      Just theSlot -> do
        Spec.assertEqWith
          s
          "bob, who controls the land now, is offered the card it exiled by name"
          (offerTo S.bob theSlot stolen)
          (Set.fromList (fmap Recipient.ToObject (faceDownExiled stolen)))
        Spec.assertEqWith
          s
          "CR 406.3 and alice, who looked at the card before exiling it face down, still is"
          (offerTo S.alice theSlot stolen)
          (Set.fromList (fmap Recipient.ToObject (faceDownExiled stolen)))
        -- Proxies, AFTER the behaviour: control really moved, and the same board
        -- offered bob the pile before it did.
        Spec.assertEqWith s "CR 613.1b the Aura moved control of the land" (Projection.controllerOf land stolen) (Just S.bob)
        Spec.assertEqWith
          s
          "and before the steal bob was offered the pile instead (CR 406.4)"
          (offerTo S.bob theSlot board)
          (Set.fromList (fmap Recipient.ToPile (pilesIn board)))
      Nothing -> Spec.assertFailure s "Synthetic Blind Reclamation should print one target slot"

  -- THE COPY TRIPWIRE for the read above, as a PAIR of boards differing in which
  -- land bob takes: the Vesuva that copied Windbrisk Heights and exiled the card,
  -- or the Windbrisk Heights it copied, which exiled nothing. Rule 702.75a names
  -- "the permanent that exiled this card" and not a permanent with hideaway, so
  -- only the first gives bob the look.
  Spec.it s "CR 707.2 the look follows the copy that exiled the card, not the Windbrisk Heights it copied" $ do
    reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
    confiscate <- S.printingOf s registry "Confiscate"
    (_, _, _, copy, board) <- playHeights s registry (Just "Vesuva")
    let copied = case Set.toList (Set.delete copy (GameState.battlefield board)) of
          [only] -> only
          _ -> S.noSource
        tookCopy = steal confiscate copy board
        tookCopied = steal confiscate copied board
    case S.spellTargetSlot reclamation of
      Just theSlot -> do
        Spec.assertEqWith
          s
          "taking the copy whose hideaway exiled the card hands bob the look"
          (offerTo S.bob theSlot tookCopy)
          (Set.fromList (fmap Recipient.ToObject (faceDownExiled tookCopy)))
        Spec.assertEqWith
          s
          "taking the land it copied leaves him the pile (CR 406.4)"
          (offerTo S.bob theSlot tookCopied)
          (Set.fromList (fmap Recipient.ToPile (pilesIn tookCopied)))
        -- Proxy, AFTER the pair: both boards really moved control, so what they
        -- differ in is which permanent bob took.
        Spec.assertEqWith
          s
          "CR 613.1b both steals landed"
          [Projection.controllerOf copy tookCopy, Projection.controllerOf copied tookCopied]
          [Just S.bob, Just S.bob]
      Nothing -> Spec.assertFailure s "Synthetic Blind Reclamation should print one target slot"

  -- CR 406.3's second sentence, the STICKY half of the live read above: "once a
  -- player is allowed to look at a card exiled face down, that player may
  -- continue to look at that card until it leaves the exile zone ... even if the
  -- instruction allowing the player to do so no longer applies". Bob's look came
  -- from rule 702.75a's control read alone, so losing the land takes it back
  -- unless the permission it once gave him was recorded. Aura Graft {1}{U}
  -- Instant -- "Gain control of target Aura that's attached to a permanent.
  -- Attach it to another permanent it can enchant" (Oracle text checked
  -- 2026-09-09) -- takes the Confiscate back off, and alice controls the land
  -- again.
  --
  -- A PAIR off ONE board differing in exactly which land bob took, on the Vesuva
  -- board so the permanent that exiled the card is a COPY: either the Vesuva
  -- whose hideaway ran, or the Windbrisk Heights it copied, which exiled nothing.
  -- Both legs END with alice controlling every permanent, so no live read of
  -- control can tell them apart -- what differs is only who was ever allowed to
  -- look.
  Spec.it s "CR 406.3 the look bob had while he controlled the land survives losing it, and a land that exiled nothing gives him none" $ do
    reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
    confiscate <- S.printingOf s registry "Confiscate"
    graft <- S.printingOf s registry "Aura Graft"
    island <- S.printingOf s registry "Island"
    (_, _, _, copy, board) <- playHeights s registry (Just "Vesuva")
    let copied = case Set.toList (Set.delete copy (GameState.battlefield board)) of
          [only] -> only
          _ -> S.noSource
        -- Two Islands for the Graft's {1}{U}, and a third for it to move the
        -- Confiscate onto -- a permanent alice already controls, so the move
        -- changes nothing but which land bob is holding.
        stocked = foldr (\_ g -> snd (S.addPermanent island S.alice g)) board [1 .. (2 :: Int)]
        (perch, g1) = S.addPermanent island S.alice stocked
        (spell, g2) = S.addHandCard graft S.alice g1
        -- bob holds the land through a CR 117.5 settle, which is the moment CR
        -- 406.3's permission is recorded; then alice's Aura Graft moves the
        -- Confiscate onto an Island of hers and the land comes home.
        leg host =
          let (aura, taken) = stealing confiscate host g2
              held = S.runPure S.identityAnswer taken Engine.settleForPriority
           in S.runPure (grafting aura perch) held $ do
                S.cast S.alice spell
                Stack.resolveTop
                Engine.settleForPriority
        tookCopy = leg copy
        tookCopied = leg copied
    case S.spellTargetSlot reclamation of
      Just theSlot -> do
        -- GAMEPLAY FIRST, and the pair is the negative: bob controls nothing on
        -- either leg, so a live read of rule 702.75a offers him the pile on both.
        Spec.assertEqWith
          s
          "CR 406.3: bob held the land that exiled the card through a settle, so he is still offered it by name after losing it"
          (offerTo S.bob theSlot tookCopy)
          (Set.fromList (fmap Recipient.ToObject (faceDownExiled tookCopy)))
        Spec.assertEqWith
          s
          "and having held the land that exiled NOTHING he is offered the pile instead (CR 406.4)"
          (offerTo S.bob theSlot tookCopied)
          (Set.fromList (fmap Recipient.ToPile (pilesIn tookCopied)))
        -- Anti-vacuity, AFTER the behaviour: the Graft really put both lands back
        -- under alice, so nothing bob controls answers rule 702.75a on either leg.
        Spec.assertEqWith
          s
          "CR 613.1b the Aura came off, so alice controls the land again on both legs"
          [Projection.controllerOf copy tookCopy, Projection.controllerOf copied tookCopied]
          [Just S.alice, Just S.alice]
      Nothing -> Spec.assertFailure s "Synthetic Blind Reclamation should print one target slot"

  -- CR 608.2h: "the answer is determined only once, when the effect is applied",
  -- which for this card is the ability's own resolution -- so the threshold is a
  -- Clause.condition rather than an activation restriction, and the land taps
  -- either way. The pair differs in ONE thing: how many of alice's three
  -- creatures she declared, three being the printed threshold and two one short.
  Spec.it s "CR 608.2h the play ability reads the turn's declarations as it resolves, so two attackers is one short and three is not" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    (_, land, leg) <- attackedHeights s registry Nothing
    let declared = leg 3 land
        refused = leg 2 land
    -- GAMEPLAY FIRST. The Piker costs {1}{R} and alice controls one Plains, so
    -- the leg that put it onto the battlefield took CR 118.9's waiver as well.
    Spec.assertEqWith
      s
      "CR 608.2h: with three attackers declared the hidden Goblin Piker was played, and with two it was not"
      (fmap (pikersFor piker) [declared, refused])
      [1, 0]
    -- Proxy, AFTER the behaviour: the ability was activated on BOTH legs, so what
    -- the two boards differ in is the clause's condition and not the activation.
    Spec.assertEqWith
      s
      "CR 601.2h via 602.2b: the {W} and the {T} were paid either way"
      (fmap (tapStateOf land) [declared, refused])
      [Just TapState.Tapped, Just TapState.Tapped]

  -- CR 607.2a's link and the copy tripwire in one pair. Vesuva entered as a copy
  -- of the Windbrisk Heights already on the battlefield, so it is the COPY whose
  -- hideaway ran and the copy the exiled card is filed against; the printed
  -- Windbrisk Heights beside it exiled nothing, having been placed rather than
  -- played. Both offer the same {W}, {T} ability and both are activated with the
  -- same three attackers declared, so the linked set is the only difference: an
  -- ability reading "any card in exile" plays the Piker off either land, and one
  -- reading Game.cardOf finds no ability on Vesuva at all.
  Spec.it s "CR 607.2a the play ability names only what THIS permanent exiled, and a copy names what the copy exiled" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    (original, copy, leg) <- attackedHeights s registry (Just "Vesuva")
    let viaCopy = leg 3 copy
        viaOriginal = leg 3 original
    Spec.assertEqWith
      s
      "CR 607.2a / 707.2: the Vesuva that exiled the Piker plays it, and the Windbrisk Heights that exiled nothing plays nothing"
      (fmap (pikersFor piker) [viaCopy, viaOriginal])
      [1, 0]
    Spec.assertEqWith
      s
      "and the card is still in exile on the leg that found no link, so the ability resolved and named nothing"
      (fmap (length . faceDownExiled) [viaCopy, viaOriginal])
      [0, 1]
    -- Proxy, AFTER the behaviour: each leg really paid its own {W} and {T}.
    Spec.assertEqWith
      s
      "CR 601.2h via 602.2b: both lands paid the same cost"
      [tapStateOf copy viaCopy, tapStateOf original viaOriginal]
      [Just TapState.Tapped, Just TapState.Tapped]

-- How many of that printing alice has on the battlefield.
pikersFor :: Printing.Printing -> GameState.GameState -> Int
pikersFor piker = S.countOnBattlefieldByName (S.printingName piker) S.alice

-- The tap state of one object, Nothing where it is not on the battlefield at all
-- -- which an assertion about a tap cost must be able to say apart from
-- "untapped".
tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- The objects on the battlefield whose PRINTED card carries that name. Printed
-- and not projected on purpose: this is how the fixture tells Vesuva from the
-- Windbrisk Heights it copied, which no rules read may do.
battlefieldNamed :: String -> GameState.GameState -> [ObjectId.ObjectId]
battlefieldNamed name gs =
  let wanted = CardName.MkCardName (Text.pack name)
   in filter
        (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just wanted)
        (Set.toAscList (GameState.battlefield gs))

-- ONE board, replayed from alice's beginning of combat: playHeights' library and
-- hideaway choice, plus a Plains for the {W}, three creatures to attack with, and
-- a second turn so the land that entered tapped (CR 702.75b) has untapped.
--
-- Returns the Windbrisk Heights PLACED on the battlefield (S.noSource where the
-- caller asked for no copier), the land alice PLAYED, and a leg: how many
-- attackers she declares, which land she then activates in her postcombat main
-- phase, and the board that comes back.
attackedHeights ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Maybe String ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, Int -> ObjectId.ObjectId -> GameState.GameState)
attackedHeights s registry copier = do
  heights <- S.printingOf s registry "Windbrisk Heights"
  played <- maybe (pure heights) (S.printingOf s registry) copier
  plains <- S.printingOf s registry "Plains"
  evangel <- S.printingOf s registry "Cabal Evangel"
  bolt <- S.printingOf s registry "Lightning Bolt"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  cancel <- S.printingOf s registry "Cancel"
  think <- S.printingOf s registry "Think Twice"
  let base = Setup.emptyGame S.bothPlayers
      -- playHeights' library, bottom first: the look reaches four of the five and
      -- `hiding` pins its choice to the second of them, the Goblin Piker.
      (_, g1) = S.addLibraryCard think S.alice base
      (_, g2) = S.addLibraryCard cancel S.alice g1
      (_, g3) = S.addLibraryCard sentry S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      (_, g5) = S.addLibraryCard bolt S.alice g4
      (original, g6) = case copier of
        Nothing -> (S.noSource, g5)
        Just _ -> S.addPermanent heights S.alice g5
      (plainsId, g7) = S.addPermanent plains S.alice g6
      g8 = foldr (\_ g -> snd (S.addPermanent evangel S.alice g)) g7 [1 .. (3 :: Int)]
      (_, g9) = S.addHandCard played S.alice g8
      before =
        g9
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      -- PLAYED and not placed, so CR 603.6a's entry event fires and hideaway
      -- runs. `original` was placed, so its own hideaway never triggered.
      afterPlay = S.runPure (hiding original) before Engine.priorityLoop
      landId = case battlefieldNamed (Maybe.fromMaybe "Windbrisk Heights" copier) afterPlay of
        [only] -> only
        _ -> S.noSource
      -- CR 502.3 on a later turn: the land entered tapped, so nothing can pay its
      -- {T} until alice's next untap step. Two handoffs and that step's
      -- turn-based actions alone, which is ManaSpec's nextTurnOfAlice -- a whole
      -- turn of priority would draw this fixture's library empty (CR 104.3c).
      untapped =
        S.runPure
          S.identityAnswer
          (Engine.beginTurnOf S.alice (Engine.beginTurnOf S.bob afterPlay))
          (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
      combatReady =
        untapped
          { GameState.phase = S.beginningOfCombat,
            GameState.remaining = S.phasesAfterThroughPostcombatMain S.beginningOfCombat,
            GameState.priority = Just S.alice
          }
      leg attackers activated =
        S.runPure
          (activating activated plainsId)
          (S.runCombat (attackingWith attackers) combatReady)
          Engine.priorityLoop
  pure (original, landId, leg)

-- Declares the first `n` creatures CR 508.1a offers and takes no other action:
-- the legs of a pair differ in this number and in nothing else.
attackingWith :: Int -> Prompt.Prompt r -> r
attackingWith n p = case p of
  Prompt.DeclareAttackers _ _ candidates -> take n candidates
  _ -> S.identityAnswer p

-- Activates that permanent's one non-mana ability, pays the {W} from that Plains
-- rather than from whatever the engine offers first, and takes CR 601.3's offer.
-- The source is PINNED because the hideaway land's own "{T}: Add {W}" is a
-- candidate too, and tapping it for the mana would spend the cost's own {T}.
activating :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
activating landId plainsId p = case p of
  Prompt.ChooseAction _ _ actions ->
    Maybe.fromMaybe A.Pass (List.find (isActivationOf landId) actions)
  Prompt.ChooseManaSource _ _ candidates ->
    Just (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== plainsId) (NonEmpty.toList candidates)))
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

isActivationOf :: ObjectId.ObjectId -> A.Action -> Bool
isActivationOf oid action = case action of
  A.Activate candidate _ -> candidate == oid
  _ -> False

-- alice plays a land with hideaway 4 with FIVE distinct cards in her library, so
-- the fifth is the one the look never reaches and every name below says which
-- card ended where. Passing a copier's name puts a Windbrisk Heights on the
-- battlefield already and plays that card instead, entering as a copy of it.
-- That one is PLACED rather than played (S.addPermanent fires no entry event),
-- so its own hideaway never triggers and one card is hidden either way.
--
-- The choice is pinned to the SECOND card of the offered four rather than
-- searched for, so a mutation cannot be silently repaired, and the random order
-- is answered by ROTATING the batch: a rotation of three is neither the identity
-- nor its own inverse, so an engine that never consulted the channel bottoms the
-- three in a different order, which the assertion reads. Reversing would not
-- discriminate -- settleArrivals reverses the answer again to perform the moves
-- from the stated end inward, and the two cancel.
--
-- Returns the name of the card left on top, the name alice hid, the names of the
-- three bottomed cards in the order they end up in, and the land that was
-- played -- which on the copier's board is the copy and not the land it copied.
playHeights :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe String -> m (CardName.CardName, CardName.CardName, [CardName.CardName], ObjectId.ObjectId, GameState.GameState)
playHeights s registry copier = do
  heights <- S.printingOf s registry "Windbrisk Heights"
  played <- maybe (pure heights) (S.printingOf s registry) copier
  bolt <- S.printingOf s registry "Lightning Bolt"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  cancel <- S.printingOf s registry "Cancel"
  think <- S.printingOf s registry "Think Twice"
  let base = Setup.emptyGame S.bothPlayers
      -- S.addLibraryCard puts each new card on top, so this stocks the library
      -- bottom first: Think Twice is the fifth card, under the four the look
      -- reaches.
      (_, g1) = S.addLibraryCard think S.alice base
      (_, g2) = S.addLibraryCard cancel S.alice g1
      (_, g3) = S.addLibraryCard sentry S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      (_, g5) = S.addLibraryCard bolt S.alice g4
      (copiedId, g6) = case copier of
        Nothing -> (S.noSource, g5)
        Just _ -> S.addPermanent heights S.alice g5
      (_, g7) = S.addHandCard played S.alice g6
      before =
        g7
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      after = S.runPure (hiding copiedId) before Engine.priorityLoop
      -- The land alice played is the battlefield's one member the copier's
      -- board did not start with; on the plain board copiedId is on no
      -- battlefield at all and the deletion is a no-op.
      landed = case Set.toList (Set.delete copiedId (GameState.battlefield after)) of
        [only] -> only
        _ -> S.noSource
  pure (S.printingName think, S.printingName piker, [S.printingName bolt, S.printingName cancel, S.printingName sentry], landed, after)

-- bob takes one permanent with a Confiscate of his own, placed and attached
-- rather than cast: CR 702.75a reads control of the land, and how it moved is
-- not part of the question. Settled so no state-based action undoes it.
steal :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
steal confiscate host gs = snd (stealing confiscate host gs)

-- `steal`, also naming the Aura it placed -- which a test that then moves the
-- Aura off again has to pin its target to.
stealing :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
stealing confiscate host gs =
  let (aura, g1) = S.addPermanent confiscate S.bob gs
   in (aura, S.settleSba (S.attachTo aura (Recipient.ToObject host) g1))

-- Choose `aura` for Aura Graft's one target slot and `destination` for the host
-- CR 701.3a then moves it to. FILTERED rather than replaced, so a leg whose slot
-- does not admit the Aura takes no target at all rather than succeeding on a
-- hand-built recipient. Pawl.BattleSpec holds its own copy for its battle pair.
grafting :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
grafting aura destination p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just aura) . Recipient.objectOf) legal) sets
  Prompt.ChooseAttachment _ _ _ offered -> if List.elem destination (NonEmpty.toList offered) then destination else NonEmpty.head offered
  _ -> S.aggressiveAnswer p

-- Plays whatever land the board offers, copies the named permanent when one is
-- passed, pins hideaway's choice to the SECOND card offered, and rotates the
-- batch the random order asks about.
hiding :: ObjectId.ObjectId -> Prompt.Prompt r -> r
hiding copied p = case p of
  Prompt.ChooseCopyTarget {} -> Just copied
  Prompt.ChooseCardFromAmong _ _ _ offered ->
    Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop 1 (NonEmpty.toList offered)))
  Prompt.Shuffle ids -> case ids of
    h : t -> t <> [h]
    [] -> []
  _ -> S.playLandAnswer p

-- namesOf, keeping the order it was given -- which is a library's own (CR 401.2).
orderedNames :: [ObjectId.ObjectId] -> GameState.GameState -> [CardName.CardName]
orderedNames oids gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) oids

-- CR 406.4's draw runs over the WHOLE pile, and the spell's own restriction is
-- judged on the card the draw named rather than before it: Runic Repetition
-- {2}{U} -- "return target exiled card with flashback you own to your hand"
-- (Oracle text checked 2026-09-01) -- over a pile Ignorant Bliss made out of a
-- hand holding one card with flashback and one without.
--
-- The pile is still OFFERED, CR 601.2c wanting an announcement that can be legal
-- and this pile holding a card that is; what no longer narrows is the pile the
-- draw runs over. So the draw can name the Goblin Piker, and a spell whose one
-- target is illegal was never cast (CR 601.2e).
--
-- ONE board through both cases. The first casts the sorcery twice with the same
-- answerer but for the draw, so the only thing that can account for the two
-- outcomes is which card came out of the pile; the second copies it with
-- Twincast, which is where CR 707.10c reads the same draw.
runicRepetition :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
runicRepetition s registry = Spec.describe s "Runic Repetition" $ do
  Spec.it s "CR 406.4 the draw reaches a card the spell's own restriction refuses, and CR 601.2e reverses the casting" $ do
    think <- S.printingOf s registry "Think Twice"
    repetition <- S.printingOf s registry "Runic Repetition"
    twincast <- S.printingOf s registry "Twincast"
    cancel <- S.printingOf s registry "Cancel"
    (hasFlashback, hasNone, spellId, _, _, board) <- flashbackPileBoard s registry
    case (hasFlashback, hasNone, S.spellTargetSlot repetition) of
      (Just wanted, Just refused, Just theSlot) -> do
        let castDrawing oid = resolveAll (S.runPure (drawing oid) board (S.cast S.alice spellId))
        Spec.assertEqWith
          s
          "the draw named the card with no flashback, so the casting was reversed and the sorcery is back in alice's hand"
          (namesIn Zone.Hand S.alice (castDrawing refused))
          (Set.fromList [S.printingName repetition, S.printingName twincast, S.printingName cancel])
        Spec.assertEqWith
          s
          "and the same board whose draw named the flashback card returns that card to her hand instead"
          (namesIn Zone.Hand S.alice (castDrawing wanted))
          (Set.fromList [S.printingName think, S.printingName twincast, S.printingName cancel])
        -- Proxies, AFTER the behaviour so none of them can absorb a mutation:
        -- the reversed casting took nothing out of exile, the honoured one took
        -- one card, and what alice announced was the pile -- which is the pile
        -- being offered at all despite holding a card the slot refuses.
        Spec.assertEqWith s "the reversed casting left both cards of the pile in exile" (Set.size (GameState.exile (castDrawing refused))) 2
        Spec.assertEqWith s "and the honoured one took exactly the card the draw named" (Set.size (GameState.exile (castDrawing wanted))) 1
        Spec.assertEqWith
          s
          "alice was offered the pile and no card of it by name"
          (offerTo S.alice theSlot board)
          (Set.fromList (fmap Recipient.ToPile (pilesIn board)))
      _ -> Spec.assertFailure s "the casting should hide a card with flashback and a card without, and Runic Repetition print one target slot"
  -- CR 707.10c's re-target is the draw's OTHER caller, and it judges the drawn
  -- card too: "if the player chooses to change some or all of the targets, the
  -- new targets must be legal". alice's copy of Runic Repetition is offered the
  -- pile again, the draw hands it the Goblin Piker, and the copy is left holding
  -- the target CR 707.10 gave it -- where recording the Piker would leave the
  -- copy with one illegal target and CR 608.2b would counter it on resolution.
  --
  -- THE ORIGINAL IS CANCELLED for exactly that reason: with both spells left to
  -- resolve, the two readings agree -- one of them returns Think Twice and the
  -- other fizzles, whichever way round -- so the board cannot tell them apart.
  -- Countering the original leaves the copy the only spell that can act.
  Spec.it s "CR 707.10c a copy's re-target keeps its old target when the draw names a card the slot refuses" $ do
    think <- S.printingOf s registry "Think Twice"
    piker <- S.printingOf s registry "Goblin Piker"
    (hasFlashback, hasNone, spellId, twincastId, cancelId, board) <- flashbackPileBoard s registry
    case (hasFlashback, hasNone) of
      (Just wanted, Just refused) -> do
        let cast1 = S.runPure (drawing wanted) board (S.cast S.alice spellId)
        case Maybe.listToMaybe (GameState.stack cast1) of
          Nothing -> Spec.assertFailure s "the sorcery should reach the stack"
          Just original -> do
            let twincasted = S.runPure (pinTarget (Recipient.ToObject original)) cast1 (S.cast S.alice twincastId)
                -- Twincast alone, so the re-target prompt CR 707.10c raises is
                -- the one `drawing refused` answers and the copy is still on the
                -- stack afterwards.
                copied = S.runPure (drawing refused) twincasted (Stack.resolveTop >> Engine.settleForPriority)
                after = resolveAll (S.runPure (pinTarget (Recipient.ToObject original)) copied (S.cast S.alice cancelId))
            Spec.assertEqWith
              s
              "the copy kept the flashback card it was copied with and returned it, the original having been countered"
              (namesIn Zone.Hand S.alice after)
              (Set.singleton (S.printingName think))
            -- Proxies, AFTER the behaviour so none of them can absorb a
            -- mutation: the card the draw named never left exile, the copy did
            -- resolve, and the pile it drew from held both cards.
            Spec.assertEqWith s "the card the copy's draw named is the one still in exile" (namesOf (Set.toList (GameState.exile after)) after) (Set.singleton (S.printingName piker))
            Spec.assertEqWith s "and nothing is left on the stack" (length (GameState.stack after)) 0
            Spec.assertEqWith s "the pile the copy drew from held both cards" (fmap (length . membersOfPile board) (pilesIn board)) [2]
      _ -> Spec.assertFailure s "the casting should hide a card with flashback and a card without"

-- alice casts Ignorant Bliss with Think Twice (flashback {2}{U}) and Goblin Piker
-- in hand, so ONE pile holds a card Runic Repetition's slot admits and a card it
-- refuses. Thirteen lands, which is the {1}{R} the Bliss costs plus the {2}{U},
-- {U}{U} and {1}{U}{U} the three spells left in her hand cost however the
-- payments fall, and her library is stocked so CR 104.3c never fires.
--
-- Returns the exiled Think Twice, the exiled Goblin Piker, her Runic Repetition,
-- her Twincast and her Cancel -- the three spells added AFTER the Bliss resolved,
-- being cards that hand would otherwise have hidden.
flashbackPileBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (Maybe ObjectId.ObjectId, Maybe ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
flashbackPileBoard s registry = do
  bliss <- S.printingOf s registry "Ignorant Bliss"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  think <- S.printingOf s registry "Think Twice"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  repetition <- S.printingOf s registry "Runic Repetition"
  twincast <- S.printingOf s registry "Twincast"
  cancel <- S.printingOf s registry "Cancel"
  let (g1, blissId) = S.handOne bliss (S.landsFor island S.alice 10 (S.landsInPlay mountain 3))
      (_, g2) = S.addHandCard think S.alice g1
      (_, g3) = S.addHandCard piker S.alice g2
      (_, g4) = S.addLibraryCard sentry S.alice g3
      blissed = resolveAll (S.runPure S.identityAnswer g4 (S.cast S.alice blissId))
      (spellId, g5a) = S.addHandCard repetition S.alice blissed
      (twincastId, g5b) = S.addHandCard twincast S.alice g5a
      (cancelId, g5) = S.addHandCard cancel S.alice g5b
      board =
        g5
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      -- Read off the board rather than assumed: CR 400.7 mints a fresh
      -- incarnation as each card is exiled, so the ids the hand held are gone.
      idOf printing =
        List.find (\oid -> namesOf [oid] board == Set.singleton (S.printingName printing)) (faceDownExiled board)
   in pure (idOf think, idOf piker, spellId, twincastId, cancelId, board)

-- throughPile, with CR 406.4's draw answered by the card NAMED rather than by
-- position. The answer is filtered back against the pile the engine offers, so
-- naming a card the draw was never offered cannot smuggle one in.
drawing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
drawing oid p = case p of
  Prompt.RandomObject _ -> oid
  _ -> throughPile p

-- Pawl.CopySpec's pinTarget: an announcement answered by FILTERING the offer down
-- to one recipient, never by building one, since CR 608.2b re-reads what was
-- chosen and a hand-built recipient of the same object is a different one.
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (== recipient) offered) asked
  _ -> S.identityAnswer p

-- What CR 601.2c would put in front of this player: the slot's legal set with CR
-- 406.4's substitution taken over it, which is the pair Pawl.Engine.Target's
-- chooseTargets raises a prompt with.
offerTo :: PlayerId.PlayerId -> TargetSlot.TargetSlot -> GameState.GameState -> Set.Set Recipient.Recipient
offerTo pid slot gs = Target.piledOffer (Just pid) gs (Target.legalRecipients (Just pid) S.noSource slot gs)

-- CR 613.7d's stamp, which is what names a foretold card's pile.
timestampOf :: ObjectId.ObjectId -> GameState.GameState -> Timestamp.Timestamp
timestampOf oid gs = maybe (Timestamp.MkTimestamp 0) Object.timestamp (Game.lookupObject oid gs)

-- castBliss' board with distinct card names and the {1}{U} alice needs for a copy
-- of the instant, cast with the PILE chosen. Every name is its own card, so the
-- two cards of the pile can be told apart in the zone each ends up in. Returns
-- the name of the card the draw named and of the other card of the same pile,
-- with the spell still on the stack.
pileBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (CardName.CardName, CardName.CardName, GameState.GameState)
pileBoard s registry = do
  bliss <- S.printingOf s registry "Ignorant Bliss"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  bolt <- S.printingOf s registry "Lightning Bolt"
  sentry <- S.printingOf s registry "Ogre Sentry"
  riftsweeper <- S.printingOf s registry "Riftsweeper"
  reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
  let (g1, blissId) = S.handOne bliss (S.landsFor island S.alice 2 (S.landsInPlay mountain 2))
      (_, g2) = S.addHandCard piker S.alice g1
      (_, g3) = S.addHandCard bolt S.alice g2
      (_, g4) = S.addLibraryCard sentry S.alice g3
      (_, g5) = S.addExiledCard riftsweeper S.bob g4
      blissed = S.runPure S.identityAnswer (S.runPure S.identityAnswer g5 (S.cast S.alice blissId)) Engine.priorityLoop
      (spellId, g6) = S.addHandCard reclamation S.alice blissed
      before =
        g6
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      -- WHICH card is which is read off the board rather than assumed: CR 400.7
      -- mints a fresh incarnation as each card is exiled, so the ids the pile
      -- holds are not the ids the hand held, and it is the pile's own order that
      -- throughPile's draw answers with.
      nameAt i = Set.toList (namesOf (take 1 (drop i (faceDownExiled before))) before)
   in pure $ case (nameAt 1, nameAt 0) of
        ([last_], [first_]) -> (last_, first_, S.runPure throughPile before (S.cast S.alice spellId))
        _ -> (S.printingName bolt, S.printingName piker, before)

-- alice casts Ignorant Bliss twice, off four Mountains and with a different hand
-- each time: the first casting hides Goblin Piker alone, the second hides
-- Lightning Bolt and Riftsweeper. So exile holds three face-down cards that CR
-- 406.4 keeps in two piles, and a board pooling them per owner would hold one.
--
-- Two CASTINGS rather than two cards, because that is what the rule separates:
-- the same printed instruction run twice, at two times. Her library is stocked
-- with an Ogre Sentry, which is also what shows the shuffle put a card there
-- rather than merely leaving one.
--
-- Returns the pile the FIRST casting made, the name of the one card in it, and
-- her copy of Synthetic Blind Reclamation, still in hand.
twoPileBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (Maybe Pile.Pile, CardName.CardName, ObjectId.ObjectId, GameState.GameState)
twoPileBoard s registry = do
  bliss <- S.printingOf s registry "Ignorant Bliss"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  bolt <- S.printingOf s registry "Lightning Bolt"
  riftsweeper <- S.printingOf s registry "Riftsweeper"
  sentry <- S.printingOf s registry "Ogre Sentry"
  reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
  let (_, g0) = S.addLibraryCard sentry S.alice (S.landsInPlay mountain 4)
      (g1, firstBliss) = S.handOne bliss (S.landsFor island S.alice 2 g0)
      (_, g2) = S.addHandCard piker S.alice g1
      afterFirst = resolveAll (S.runPure S.identityAnswer g2 (S.cast S.alice firstBliss))
      (secondBliss, g3) = S.addHandCard bliss S.alice afterFirst
      (_, g4) = S.addHandCard bolt S.alice g3
      (_, g5) = S.addHandCard riftsweeper S.alice g4
      afterSecond = resolveAll (S.runPure S.identityAnswer g5 (S.cast S.alice secondBliss))
      (spellId, g6) = S.addHandCard reclamation S.alice afterSecond
      board =
        g6
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      hidden = S.printingName piker
      -- WHICH pile is which is read off the board rather than assumed: CR 400.7
      -- mints a fresh incarnation as each card is exiled, so the card is followed
      -- by name and its pile asked of Pawl.Engine.Exile.
      pile =
        Maybe.listToMaybe
          ( do
              oid <- faceDownExiled board
              Monad.guard (namesOf [oid] board == Set.singleton hidden)
              Maybe.maybeToList (Exile.pileOf oid board)
          )
   in pure (pile, hidden, spellId, board)

-- Every object on the stack resolved, with nobody taking an action.
resolveAll :: GameState.GameState -> GameState.GameState
resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

-- throughPile, but naming ONE pile of the several a board may offer: the offer is
-- filtered down to it rather than answered with a hand-built recipient, so a
-- candidate the engine never offered cannot be smuggled past CR 601.2c. The draw
-- is answered with the LAST card of whatever pile it is given.
throughPileOf :: Pile.Pile -> Prompt.Prompt r -> r
throughPileOf pile p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (Recipient.ToPile pile ==) sets
  Prompt.RandomObject members -> NonEmpty.last members
  _ -> S.identityAnswer p

-- throughPileOf, counting the draws it is asked for -- Pawl.CopySpec's
-- countingAnswer shape, since a pure answerer cannot tell "asked once" from
-- "not asked at all".
countingDraws :: Pile.Pile -> Prompt.Prompt r -> State.State Int r
countingDraws pile p = case p of
  Prompt.RandomObject _ -> do
    State.modify' (+ 1)
    pure (throughPileOf pile p)
  _ -> pure (throughPileOf pile p)

-- throughPileOf, but answering CR 406.4's draw with an object the named pile
-- does NOT hold. The pile is still filtered out of the engine's own offer, so
-- only the draw's answer is the smuggled one.
namingOutside :: Pile.Pile -> ObjectId.ObjectId -> Prompt.Prompt r -> r
namingOutside pile oid p = case p of
  Prompt.RandomObject _ -> oid
  _ -> throughPileOf pile p

-- The face-down exiled cards of one pile, in the order Pawl.Engine.Target's
-- pileMembers offers them -- ascending by object id, which is Set.toList's.
membersOfPile :: GameState.GameState -> Pile.Pile -> [ObjectId.ObjectId]
membersOfPile gs pile = filter (\oid -> Exile.pileOf oid gs == Just pile) (faceDownExiled gs)

-- The one other pile of a board that has exactly two.
otherPileThan :: Maybe Pile.Pile -> GameState.GameState -> Maybe Pile.Pile
otherPileThan pile gs = case filter (\p -> Just p /= pile) (pilesIn gs) of
  [only] -> Just only
  _ -> Nothing

-- Answers CR 601.2c with the PILE -- filtered out of the offer rather than built,
-- so a candidate the engine never offered cannot be smuggled in -- and answers CR
-- 406.4's draw with the LAST card of the pile, which is the one answer a draw
-- that took the first would not produce.
throughPile :: Prompt.Prompt r -> r
throughPile p =
  let isPile recipient = case recipient of
        Recipient.ToPile _ -> True
        _ -> False
   in case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter isPile . snd) sets
        Prompt.RandomObject members -> NonEmpty.last members
        _ -> S.identityAnswer p

-- One player casts their copy of the instant with the slot's whole offer FILTERED
-- down to the foretold card, then the stack is resolved. Filtered rather than
-- answered with a hand-built recipient, so a candidate the engine never offered
-- cannot be smuggled past CR 601.2c; where the filter admits nothing S.preferring
-- falls through to the offer's minimum, which is the face-up card and is exactly
-- what a player refused the permission should get.
resolveCast :: PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
resolveCast pid spell wanted gs =
  S.runPure S.identityAnswer (S.runPure (aimedAt wanted) gs (S.cast pid spell)) Engine.priorityLoop

aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((==) (Just oid) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- The card names in one player's copy of a zone, and the same over a list of
-- objects -- a shuffle mints a new incarnation (CR 400.7), so a card that moved
-- is followed by name rather than by object id.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> Set.Set CardName.CardName
namesIn zone pid gs = namesOf (Game.zoneMembers zone pid gs) gs

namesOf :: [ObjectId.ObjectId] -> GameState.GameState -> Set.Set CardName.CardName
namesOf oids gs = Set.fromList (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) oids)

-- The distinct piles the face-down exiled cards of a board are in (CR 406.4).
pilesIn :: GameState.GameState -> [Pile.Pile]
pilesIn gs = Set.toList (Set.fromList (Maybe.mapMaybe (\oid -> Exile.pileOf oid gs) (faceDownExiled gs)))

-- The exiled cards CR 406.3's rider left face down.
faceDownExiled :: GameState.GameState -> [ObjectId.ObjectId]
faceDownExiled gs = filter (\oid -> any Object.exiledFaceDown (Game.lookupObject oid gs)) (Set.toList (GameState.exile gs))

-- alice casts Ignorant Bliss off two Mountains with Goblin Piker and Lightning
-- Bolt in hand, and one Ogre Sentry already exiled face up under bob. Her
-- library is stocked so the delayed ability's draw has a card and CR 104.3c
-- never fires.
castBliss :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
castBliss s registry = do
  bliss <- S.printingOf s registry "Ignorant Bliss"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  bolt <- S.printingOf s registry "Lightning Bolt"
  sentry <- S.printingOf s registry "Ogre Sentry"
  let (g1, blissId) = S.handOne bliss (S.landsInPlay mountain 2)
      (_, g2) = S.addHandCard piker S.alice g1
      (_, g3) = S.addHandCard bolt S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      (_, g5) = S.addExiledCard sentry S.bob g4
  pure (S.runPure S.identityAnswer (S.runPure S.identityAnswer g5 (S.cast S.alice blissId)) Engine.priorityLoop)

-- The end step of alice's turn, settled and resolved -- Pawl.TriggerSpec's
-- delayed-ability group takes the same route.
endStep :: GameState.GameState -> GameState.GameState
endStep gs =
  let phase = Phase.Ending EndingStep.EndStep
      began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan phase S.alice)) (gs {GameState.phase = phase})
   in S.runPure S.identityAnswer (S.runPure S.identityAnswer began Engine.settleForPriority) Engine.priorityLoop

-- The exiled cards CR 406.3's default left face up.
faceUpExiled :: GameState.GameState -> [ObjectId.ObjectId]
faceUpExiled gs = filter (\oid -> not (any Object.exiledFaceDown (Game.lookupObject oid gs))) (Set.toList (GameState.exile gs))

-- The objects in a zone still carrying CR 406.3's face-down flag, which outside
-- exile must always be none.
concealedIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
concealedIn zone pid gs = filter (\oid -> any Object.exiledFaceDown (Game.lookupObject oid gs)) (Game.zoneMembers zone pid gs)

-- The one target slot of the one triggered ability a printing declares --
-- Pawl.TargetSpec's triggerTargetSlot, kept local so this group reads Riftsweeper
-- out of the committed card rather than a hand-built TargetSlot.
triggerTargetSlot :: Printing.Printing -> Maybe TargetSlot.TargetSlot
triggerTargetSlot printing = case Face.triggeredAbilities (S.combinedFace printing) of
  [ability] -> case Map.elems (Modal.allTargetSlots (TriggeredAbility.modal ability)) of
    [only] -> Just only
    _ -> Nothing
  _ -> Nothing
