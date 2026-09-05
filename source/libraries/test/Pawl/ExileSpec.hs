{-# LANGUAGE GADTs #-}

-- Covers: CR 406.3's face-down exile -- Object.exiledFaceDown, CR 406.3a's
-- absent characteristics as Pawl.Engine.Projection.gatherGiven's exile walk
-- reads them, the EntryRiders.exiledFaceDown rider
-- Pawl.Engine.Event.changeZoneEntering reads,
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
-- Gameplay-level, off two producers that exile face down and differ in exactly
-- the permission. Ignorant Bliss {1}{R} Instant -- "Exile all cards from your
-- hand face down" -- grants nobody a look, so not even the owner may choose what
-- it exiled; foretell (CR 702.143a) grants the owner one, so she may. Synthetic
-- Blind Reclamation's unqualified "target exiled card" is what reads the
-- difference back out, and Riftsweeper's printed "face-up exiled card" is the
-- third reading -- a card whose own words refuse what rule 406.4 offers. Runic
-- Repetition is the fourth: a restriction a pile only half satisfies, which is
-- what the draw runs over the whole pile for.
--
-- Each group shares ONE board across its readings, which is the point: exile
-- holds the same cards either way, and only how they got there differs.
module Pawl.ExileSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Exile as Exile
import qualified Pawl.Engine.Foretell as Foretell
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Pile as Pile
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Face-down exile" $ do
  foretold s registry
  runicRepetition s registry
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
        Maybe.listToMaybe
          [ oid
          | oid <- faceDownExiled board,
            namesOf [oid] board == Set.singleton (S.printingName printing)
          ]
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
          [ p
          | oid <- faceDownExiled board,
            namesOf [oid] board == Set.singleton hidden,
            p <- Maybe.maybeToList (Exile.pileOf oid board)
          ]
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
throughPile p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter isPile . snd) sets
  Prompt.RandomObject members -> NonEmpty.last members
  _ -> S.identityAnswer p
  where
    isPile recipient = case recipient of
      Recipient.ToPile _ -> True
      _ -> False

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
