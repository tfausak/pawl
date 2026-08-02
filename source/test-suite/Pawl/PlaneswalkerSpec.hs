{-# LANGUAGE GADTs #-}

-- Covers CR 306, the planeswalker card type, across the six modules it reaches:
-- Pawl.Engine.Projection's intrinsic CR 306.5b enters-with replacement and
-- Pawl.Engine.Replacement's EntryR arm that carries it out, Pawl.Engine.Cost's
-- two CR 606.4 loyalty cost components, Pawl.Engine.Activate's CR 606.3 gate,
-- Pawl.Engine.Sba's CR 704.5i zero-loyalty state-based action, Pawl.Engine.Target's
-- CR 115.4 "any target" pool, and Pawl.Engine.Damage's CR 306.8 / CR 120.3c
-- loyalty removal.
--
-- CR 306.6 -- "Planeswalkers can be attacked" -- is the one clause of rule 306
-- covered elsewhere: it is combat's, so it lives in CombatSpec's
-- AttackingAPlaneswalker group, on the same card.
--
-- Jace Beleren is the whole proof: {1}{U}{U} Legendary Planeswalker -- Jace, with
-- printed loyalty 3 and three loyalty abilities (+2, -1, -10). Its -10 is what
-- makes CR 606.6 observable at 3 loyalty, and three -1s across three of alice's
-- turns are what drive it to 0 for CR 704.5i. Lightning Bolt's 3 and Firebolt's 2
-- are the burn half: the first takes exactly the printed loyalty (CR 704.5i
-- follows), the second takes less (it does not).
module Pawl.PlaneswalkerSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

-- Jace Beleren's abilities in printed order: +2, -1, -10. Indexed rather than
-- taken with a `head` the way every other spec's `theAbility` is, because this is
-- the first card in the pool with more than one activated ability and CR 606.6 is
-- a claim about WHICH of them is offered.
abilityAt :: Int -> Printing.Printing -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilityAt i p = take 1 (drop i (Card.Type.activatedAbilities (Printing.card p)))

-- The Activate action for one of Jace's abilities, as a one-or-zero-element list
-- so a fixture that finds no such ability produces an assertion failure rather
-- than a partial pattern.
activation :: ObjectId.ObjectId -> Int -> Printing.Printing -> [A.Action]
activation oid i p = fmap (A.Activate oid) (abilityAt i p)

plusTwo, minusOne, minusTen :: Int
plusTwo = 0
minusOne = 1
minusTen = 2

-- Activate one of Jace's abilities and let it resolve.
useAbility :: Int -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
useAbility i p oid gs = case abilityAt i p of
  ability : _ -> S.runPure S.identityAnswer gs (do Activate.activateAbility S.alice oid ability; Stack.resolveTop)
  [] -> gs

-- alice with three Islands untapped and Jace Beleren in hand, in her precombat
-- main phase with priority -- CR 306.1's window ("during a main phase of their
-- turn when the stack is empty") and enough mana for {1}{U}{U}.
--
-- Jace is then cast and resolved through the ordinary path, so the loyalty
-- counters come from CR 306.5b's replacement rather than from a fixture.
jaceOnBattlefield :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
jaceOnBattlefield island jace =
  let (gs, handId) = S.handOne jace (stockLibraries island (S.landsInPlay island 3))
      after = S.runPure S.identityAnswer gs (do Cast.castSpell S.alice handId; Stack.resolveTop)
   in (theJace after, after)

-- Four cards in each library. Jace's abilities all draw, and CR 704.5b would end
-- the game on an empty one -- so the libraries are stocked to keep every
-- assertion about loyalty and about who drew from resting on a deck-out.
stockLibraries :: Printing.Printing -> GameState.GameState -> GameState.GameState
stockLibraries island base =
  List.foldl' (\gs pid -> snd (S.addLibraryCard island pid gs)) base (concat (replicate 4 [S.alice, S.bob]))

-- The planeswalker on the battlefield, found by name because CR 400.7 mints a new
-- object as the spell moves and the hand's id does not survive the cast.
theJace :: GameState.GameState -> ObjectId.ObjectId
theJace gs =
  let isJace oid = fmap Card.Type.name (Game.cardOf oid gs) == Just (CardName.MkCardName $ Text.pack "Jace Beleren")
   in case filter isJace (Set.toList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> S.noSource

-- Hand the turn round the table until alice is active again, and put her back in
-- her precombat main phase with priority. Two handoffs in a two-player game.
--
-- CR 606.3's limit is "that turn", and Pawl.Engine.Engine.beginTurnOf clears the
-- CR 608.2i log the limit is read out of -- so this is also what proves the limit
-- expires rather than merely existing.
alicesNextTurn :: GameState.GameState -> GameState.GameState
alicesNextTurn gs =
  let bobs = S.runPure S.identityAnswer gs Engine.handoffTurn
      back = S.runPure S.identityAnswer bobs Engine.handoffTurn
   in back {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}

-- Jace on the battlefield with his three CR 306.5b counters, an untapped Mountain
-- beside him for the {R}, and one burn spell in alice's hand. The three Islands
-- jaceOnBattlefield paid with are tapped, so the Mountain is the only mana left
-- and the burn spell is the only castable thing.
--
-- The planeswalker alice burns is her own. CR 115.4 admits "planeswalkers" with
-- no controller clause, and nothing on the damage path reads whose it is, so
-- aiming across the table would prove the same thing at the cost of a second
-- board.
burnAtJace ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
burnAtJace island mountain jace burn =
  let (jaceId, board) = jaceOnBattlefield island jace
      (_, withMountain) = S.addCreature mountain S.alice board
      (burnId, gs) = S.addHandCard burn S.alice withMountain
   in (jaceId, burnId, gs)

-- How many cards of a given name are in alice's graveyard.
graveyardCount :: String -> GameState.GameState -> Int
graveyardCount name gs =
  let named oid = fmap Card.Type.name (Game.cardOf oid gs) == Just (CardName.MkCardName $ Text.pack name)
   in length (filter named (Game.zoneMembers Zone.Graveyard S.alice gs))

-- Fill every target slot with the candidate that NAMES `oid`, whatever tag the
-- pool produced for it -- so the fixture asks for the planeswalker without
-- asserting how CR 115.4's pool tags one.
--
-- Falls back to the set's minimum, which keeps the interpreter total: a board
-- where the pool never offers `oid` then burns something else and fails on the
-- loyalty assertions, rather than on a partial match.
aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    let naming candidates = case filter (\r -> Recipient.objectOf r == Just oid) (Set.toList candidates) of
          r : _ -> Just r
          [] -> Set.lookupMin candidates
     in Map.mapMaybe naming sets
  _ -> S.identityAnswer p

-- Cast the burn spell at the planeswalker and resolve it. NOT settled: CR 120.5
-- says damage does not destroy, so the pre-SBA state is where CR 306.8's counter
-- removal is observable on its own.
burnResolved :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
burnResolved jaceId burnId gs =
  S.runPure (aimedAt jaceId) gs $ do
    Cast.castSpell S.alice burnId
    Stack.resolveTop

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Planeswalker" $ do
  Spec.it s "CR 306.5b Jace Beleren enters with three loyalty counters" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, after) = jaceOnBattlefield island jace
    Spec.assertBool s (Set.member jaceId (GameState.battlefield after)) "on the battlefield"
    Spec.assertEqWith s "loyalty 3" (S.counterOf CounterKind.Loyalty jaceId after) 3

  Spec.it s "CR 606.4 the +2 adds two loyalty counters and each player draws" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, board) = jaceOnBattlefield island jace
        after = useAbility plusTwo jace jaceId board
    Spec.assertEqWith s "loyalty 3 + 2" (S.counterOf CounterKind.Loyalty jaceId after) 5
    Spec.assertEqWith s "alice drew" (S.handSize S.alice after) 1
    Spec.assertEqWith s "bob drew too" (S.handSize S.bob after) 1

  Spec.it s "CR 606.4 the -1 removes a loyalty counter and the targeted player draws" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, board) = jaceOnBattlefield island jace
        after = useAbility minusOne jace jaceId board
    Spec.assertEqWith s "loyalty 3 - 1" (S.counterOf CounterKind.Loyalty jaceId after) 2
    Spec.assertEqWith s "exactly one player drew exactly one card" (S.handSize S.alice after + S.handSize S.bob after) 1

  Spec.it s "CR 606.6 the -10 is not offered at 3 loyalty, while the other two are" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, board) = jaceOnBattlefield island jace
        offered = Action.legalActions S.alice board
        isOffered i = not (null (activation jaceId i jace)) && all (`elem` offered) (activation jaceId i jace)
    Spec.assertBool s (isOffered plusTwo) "the +2 is offered"
    Spec.assertBool s (isOffered minusOne) "the -1 is offered"
    Spec.assertBool s (not (isOffered minusTen)) "the -10 is NOT offered"
    Spec.assertBool
      s
      (not (any (\ab -> Activate.activatable S.alice jaceId ab board) (abilityAt minusTen jace)))
      "and it is not activatable either"

  Spec.it s "CR 606.3 a second loyalty ability is not offered in the same turn" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, board) = jaceOnBattlefield island jace
        after = useAbility plusTwo jace jaceId board
        offered = Action.legalActions S.alice after
    Spec.assertEqWith s "the +2 resolved" (S.counterOf CounterKind.Loyalty jaceId after) 5
    Spec.assertBool s (all (`notElem` offered) (activation jaceId plusTwo jace)) "the +2 is not offered again"
    Spec.assertBool s (all (`notElem` offered) (activation jaceId minusOne jace)) "and neither is the -1"
    Spec.assertBool
      s
      (all (`elem` Action.legalActions S.alice (alicesNextTurn after)) (activation jaceId plusTwo jace))
      "but the limit expires with the turn"

  Spec.it s "CR 606.3 a loyalty ability is not offered on an opponent's turn" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, board) = jaceOnBattlefield island jace
        theirTurn = board {GameState.activePlayer = S.bob, GameState.priority = Just S.alice}
    Spec.assertBool
      s
      (all (`notElem` Action.legalActions S.alice theirTurn) (activation jaceId plusTwo jace))
      "the +2 is not offered"

  -- The proof that CR 306.5b's counters go through the CR 122.6 funnel rather
  -- than straight onto the object. CR 614.16's second sentence is the rule:
  -- a counter-scaling replacement applies "even if the original event being
  -- modified wasn't itself an effect", and CR 306.5b's entry counters are
  -- placed by a replacement effect.
  Spec.it s "CR 614.16 Doubling Season doubles a planeswalker's starting loyalty" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    let (gs, handId) = S.handOne jace (snd (S.addCreature doublingSeason S.alice (stockLibraries island (S.landsInPlay island 3))))
        after = S.runPure S.identityAnswer gs (do Cast.castSpell S.alice handId; Stack.resolveTop)
    Spec.assertEqWith s "three doubled to six" (S.counterOf CounterKind.Loyalty (theJace after) after) 6

  -- The other half of the same rule, and the reason the two placements are
  -- deliberately different code paths: CR 614.16's FIRST sentence limits a
  -- counter-scaling replacement to counters an EFFECT puts on, and CR 606.4's
  -- loyalty symbol is a cost. Doubling Season's own ruling says so.
  Spec.it s "CR 614.16 Doubling Season does not double a loyalty ability's own cost" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    let (gs, handId) = S.handOne jace (snd (S.addCreature doublingSeason S.alice (stockLibraries island (S.landsInPlay island 3))))
        board = S.runPure S.identityAnswer gs (do Cast.castSpell S.alice handId; Stack.resolveTop)
        jaceId = theJace board
        after = useAbility plusTwo jace jaceId board
    Spec.assertEqWith s "six plus two, not six plus four" (S.counterOf CounterKind.Loyalty jaceId after) 8

  -- CR 115.4: "These targets may be creatures, players, planeswalkers, or
  -- battles." Read off Lightning Bolt's OWN committed spec rather than a
  -- hand-built one, so what is under test is the pool the card data selects.
  Spec.it s "CR 115.4 an 'any target' spell offers the planeswalker alongside the players" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    jace <- S.printingOf s registry "Jace Beleren"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (jaceId, _, gs) = burnAtJace island mountain jace lightningBolt
        offered = fmap (\theSpec -> Target.legalRecipients (Just S.alice) S.noSource theSpec gs) (S.spellTargetSpec lightningBolt)
    Spec.assertEqWith s "the planeswalker is a legal target" (fmap (Set.member (Recipient.ToPlaneswalker jaceId)) offered) (Just True)
    Spec.assertEqWith s "and so are both players" (fmap (Set.isSubsetOf (Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob])) offered) (Just True)
    -- CR 115.4's other half: "Other game objects, such as noncreature artifacts
    -- or spells, can't be chosen." The Mountain and the Islands are on the same
    -- battlefield, so widening the pool to planeswalkers must not have widened
    -- it to permanents.
    Spec.assertEqWith s "and nothing else on the battlefield is" (fmap (Set.size . Set.filter (Maybe.isJust . Recipient.objectOf)) offered) (Just 1)

  Spec.it s "CR 306.8 Lightning Bolt's 3 damage removes all three loyalty counters, and CR 704.5i buries Jace" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    jace <- S.printingOf s registry "Jace Beleren"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (jaceId, boltId, gs) = burnAtJace island mountain jace lightningBolt
        resolved = burnResolved jaceId boltId gs
        after = S.settleSba resolved
    Spec.assertEqWith s "CR 306.8: three loyalty counters removed" (S.counterOf CounterKind.Loyalty jaceId resolved) 0
    -- CR 120.3c is the whole result: a planeswalker is not a creature, so CR
    -- 120.3e's marked damage is not among the results damage to it has.
    Spec.assertEqWith s "CR 120.3e does not apply, so nothing is marked on it" (S.damageOf jaceId resolved) (Just 0)
    Spec.assertBool s (Set.member jaceId (GameState.battlefield resolved)) "CR 120.5: the damage did not itself destroy it"
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "CR 704.5i: loyalty 0, so off the battlefield"
    -- By NAME, not by id: CR 400.7 mints a new object as the card moves, so
    -- jaceId names nothing once the SBA has buried it.
    Spec.assertEqWith s "CR 704.5i: in its owner's graveyard" (graveyardCount "Jace Beleren" after) 1

  Spec.it s "CR 306.8 Firebolt's 2 damage removes two of the three loyalty counters and Jace lives" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    jace <- S.printingOf s registry "Jace Beleren"
    firebolt <- S.printingOf s registry "Firebolt"
    let (jaceId, fireboltId, gs) = burnAtJace island mountain jace firebolt
        after = S.settleSba (burnResolved jaceId fireboltId gs)
    Spec.assertEqWith s "CR 306.8: 3 - 2, not 0 and not 3" (S.counterOf CounterKind.Loyalty jaceId after) 1
    Spec.assertBool s (Set.member jaceId (GameState.battlefield after)) "CR 704.5i does not apply at loyalty 1"

  Spec.it s "CR 704.5i three -1s across three turns bury Jace in his owner's graveyard" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, board) = jaceOnBattlefield island jace
        turn gs = S.settleSba (alicesNextTurn (useAbility minusOne jace jaceId gs))
        afterOne = turn board
        afterTwo = turn afterOne
        afterThree = turn afterTwo
    Spec.assertEqWith s "loyalty 2 after one activation" (S.counterOf CounterKind.Loyalty jaceId afterOne) 2
    Spec.assertEqWith s "loyalty 1 after two" (S.counterOf CounterKind.Loyalty jaceId afterTwo) 1
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield afterThree))) "off the battlefield after three"
    Spec.assertEqWith s "in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice afterThree)) 1
