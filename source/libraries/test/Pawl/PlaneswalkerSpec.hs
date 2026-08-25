{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 306, the planeswalker card type, across the seven modules it reaches:
-- Pawl.Engine.Projection's intrinsic CR 306.5b enters-with replacement and
-- Pawl.Engine.Replacement's EntryR arm that carries it out, Pawl.Engine.Cost's
-- two CR 606.4 loyalty cost components and CR 606.5's combining of them,
-- Pawl.Engine.Activate's CR 606.3 gate,
-- Pawl.Engine.Sba's CR 704.5i zero-loyalty state-based action, Pawl.Engine.Target's
-- CR 115.4 "any target" pool, and Pawl.Engine.Damage's CR 306.8 / CR 120.3c
-- loyalty removal -- which Pawl.Engine.Event records as a GameEvent.CountersRemoved
-- alongside Pawl.Engine.Cost's.
--
-- CR 306.6 -- "Planeswalkers can be attacked" -- is covered elsewhere: it is
-- combat's, so it lives in Pawl.CombatEffectSpec's AttackingAPlaneswalker group,
-- on Jace. The CountersRemoved group below declares an attack at a planeswalker
-- too, and for a different rule: CR 510.2's simultaneity is what makes one batch
-- of combat damage one counter-removal record, and combat is the only producer of
-- a batch with two damage events in it.
--
-- A group per planeswalker, each group's card named in its own comment. Nissa,
-- Steward of Elements -- {X}{G}{U} Legendary
-- Planeswalker -- Nissa, whose lower right corner prints CR 107.3's X -- is the
-- VariableLoyalty group's alone, where CR 107.3m decides what that X is worth.
-- Chandra, Fire Artisan -- {2}{R}{R} Legendary Planeswalker -- Chandra, printed
-- loyalty 4 -- is the CountersRemoved group's alone, where CR 606.4's cost and CR
-- 306.8's damage are read as EVENTS rather than as writes.
-- Grist, the Hunger Tide -- {1}{B}{G} Legendary Planeswalker -- Grist, printed
-- loyalty 3 -- is the GristLoyalty group's, where a loyalty ability's EFFECT is
-- what is read rather than its cost or the permanent's counters.
-- Jace Beleren is the rest: {1}{U}{U} Legendary Planeswalker -- Jace, with
-- printed loyalty 3 and three loyalty abilities (+2, -1, -10). Its -10 is what
-- makes CR 606.6 observable at 3 loyalty, and three -1s across three of alice's
-- turns are what drive it to 0 for CR 704.5i. Lightning Bolt's 3 and Firebolt's 2
-- are the burn half: the first takes exactly the printed loyalty (CR 704.5i
-- follows), the second takes less (it does not).
--
-- Carth the Lion -- {2}{B}{G} Legendary Creature -- Human Warrior, 3/5 -- is the
-- CombinedLoyaltyCost group's alone: it is the one card in the pool that adds a
-- loyalty cost to somebody else's loyalty ability, which is what makes CR 606.5
-- observable. Only the second sentence is read here; the first -- the
-- enters-or-planeswalker-dies trigger -- is Pawl.MassEffectSpec's CarthTheLion
-- group. One thing about pawl's Carth is not the printed card: its "loyalty
-- abilities" is transcribed as "abilities of a planeswalker", because
-- AddActivationCost.whichAbilities filters the ability's source permanent rather
-- than the ability (gap #1698). That leaves the tax at least as expensive as
-- printed, so it cannot make an activation legal that the real card refuses.
module Pawl.PlaneswalkerSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- Jace Beleren's abilities in printed order: +2, -1, -10. Indexed rather than
-- taken with a `head` the way every other spec's `theAbility` is, because this is
-- the first card in the pool with more than one activated ability and CR 606.6 is
-- a claim about WHICH of them is offered.
abilityAt :: Int -> Printing.Printing -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilityAt i p = take 1 (drop i (Face.activatedAbilities (S.combinedFace p)))

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
      after = S.runPure S.identityAnswer gs (do S.cast S.alice handId; Stack.resolveTop)
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
  let isJace oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Jace Beleren")
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
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack name)
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
    let naming (n, candidates) =
          Set.fromList
            . take (Natural.toIntSaturating n)
            $ filter (\r -> Recipient.objectOf r == Just oid) (Set.toList candidates) <> Set.toList candidates
     in fmap naming sets
  _ -> S.identityAnswer p

-- Cast the burn spell at the planeswalker and resolve it. NOT settled: CR 120.5
-- says damage does not destroy, so the pre-SBA state is where CR 306.8's counter
-- removal is observable on its own.
burnResolved :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
burnResolved jaceId burnId gs =
  S.runPure (aimedAt jaceId) gs $ do
    S.cast S.alice burnId
    Stack.resolveTop

-- Nissa, Steward of Elements' abilities in printed order: +2, 0, -6. Indexed for
-- the reason Jace's are.
plusTwoScry, zeroLook, minusSix :: Int
plusTwoScry = 0
zeroLook = 1
minusSix = 2

-- The one planeswalker printed with a loyalty of X, found on the battlefield by
-- name for theJace's reason.
theNissa :: GameState.GameState -> ObjectId.ObjectId
theNissa gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Nissa, Steward of Elements")
   in case filter named (Set.toList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> S.noSource

-- CR 601.2b's announcement and CR 608.2d's "may", both FIXED rather than derived
-- from the prompt: an answerer that computed either from what it was offered
-- would go on answering legally after a mutation, and what these cases are about
-- is which number the engine itself reached.
announcingX :: Natural -> Prompt.Prompt r -> r
announcingX x p = case p of
  Prompt.ChooseX {} -> x
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- alice with six Forests and three Islands untapped, `deck` stocked from the top
-- down, and Nissa, Steward of Elements cast for `x` and resolved through the
-- ordinary path -- so her loyalty counters come from CR 306.5b's replacement
-- reading CR 107.3m's announced value and never from a fixture.
--
-- Nine lands covers the largest X below ({6}{G}{U} is eight), so the BOARD is
-- what every pair here holds constant and the announcement is the only thing that
-- moves between the halves.
nissaCastFor :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Natural -> (ObjectId.ObjectId, GameState.GameState)
nissaCastFor forest island nissa deck x =
  let lands = S.landsFor forest S.alice 6 (S.landsInPlay island 3)
      -- addLibraryCard puts its card ON TOP, so the deepest is stocked first.
      deal board printing = snd (S.addLibraryCard printing S.alice board)
      stocked = List.foldl' deal lands (reverse deck)
      (gs, handId) = S.handOne nissa stocked
      after = S.runPure (announcingX x) gs (do S.cast S.alice handId; Stack.resolveTop)
   in (theNissa after, after)

-- useAbility with an answerer that exercises CR 608.2d's "may", which the `0`
-- ability's second clause raises. Its own X is never asked: no loyalty cost here
-- declares one.
useNissaAbility :: Int -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
useNissaAbility i p oid gs = case abilityAt i p of
  ability : _ -> S.runPure (announcingX 0) gs (do Activate.activateAbility S.alice oid ability; Stack.resolveTop)
  [] -> gs

-- Chandra, Fire Artisan's abilities in printed order: +1, -7. Indexed for the
-- reason Jace's are.
plusOne, minusSeven :: Int
plusOne = 0
minusSeven = 1

-- The planeswalker on the battlefield, found by name for theJace's reason.
theChandra :: GameState.GameState -> ObjectId.ObjectId
theChandra gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Chandra, Fire Artisan")
   in case filter named (Set.toList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> S.noSource

-- alice with `spare` Mountains left over after casting Chandra, Fire Artisan for
-- {2}{R}{R} through the ordinary path -- so her four loyalty counters come from
-- CR 306.5b's replacement rather than from a fixture.
--
-- Eight cards in alice's library, which the -7 needs: it exiles seven, and CR
-- 104.3c would end the game on an empty one before an assertion about the trigger
-- could run.
chandraOnBattlefield :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
chandraOnBattlefield mountain chandra spare =
  let lands = S.landsInPlay mountain (4 + spare)
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.alice g)) lands [1 :: Int .. 8]
      (gs, handId) = S.handOne chandra stocked
      after = S.runPure S.identityAnswer gs (do S.cast S.alice handId; Stack.resolveTop)
   in (theChandra after, after)

-- Fill every target slot with the candidate that names this PLAYER, filtering the
-- set the engine offered rather than building a Recipient by hand -- aimedAt's
-- posture one recipient shape over.
--
-- Chandra's trigger says "target opponent or planeswalker", and SHE is a legal
-- planeswalker for it: an answerer that took the head of the offered set could put
-- the damage back on her and leave the life assertion reading 20 for a reason that
-- has nothing to do with the counters.
aimedAtPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimedAtPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    let naming (n, candidates) =
          Set.fromList
            . take (Natural.toIntSaturating n)
            $ filter (== Recipient.ToPlayer pid) (Set.toList candidates) <> Set.toList candidates
     in fmap naming sets
  _ -> S.identityAnswer p

-- Gather the triggers the log has earned and resolve the top of the stack, with
-- the trigger's own target answered. Two steps and not one: CR 603.3b puts the
-- ability on the stack at the next CR 117.5 boundary, and CR 608.1 resolves it
-- only once a player would receive priority with it there.
firedTrigger :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
firedTrigger answer gs = S.runPure answer gs (do Engine.settleForPriority; Stack.resolveTop)

isPlaneswalkerTarget :: AttackTarget.AttackTarget -> Bool
isPlaneswalkerTarget target = case target of
  AttackTarget.OfPlaneswalker _ -> True
  AttackTarget.OfPlayer _ -> False
  AttackTarget.OfBattle _ -> False

-- Announce every attack at the planeswalker, answer Chandra's own trigger at
-- alice, and everything else aggressively.
attackingChandra :: Prompt.Prompt r -> r
attackingChandra p = case p of
  Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalkerTarget (NonEmpty.toList options) of
    target : _ -> target
    [] -> NonEmpty.head options
  Prompt.ChooseTargets {} -> aimedAtPlayer S.alice p
  _ -> S.aggressiveAnswer p

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

  -- The proof that CR 306.5b's counters accumulate into GameState.enteringCounters
  -- rather than landing straight onto the object, so CR 614.16 reaches them in the
  -- entry's own CR 616.1 pool. CR 614.16's second sentence is the rule: a
  -- counter-scaling replacement applies "even if the original event being
  -- modified wasn't itself an effect", and CR 306.5b's entry counters are
  -- placed by a replacement effect.
  Spec.it s "CR 614.16 Doubling Season doubles a planeswalker's starting loyalty" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    let (gs, handId) = S.handOne jace (snd (S.addCreature doublingSeason S.alice (stockLibraries island (S.landsInPlay island 3))))
        after = S.runPure S.identityAnswer gs (do S.cast S.alice handId; Stack.resolveTop)
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
        board = S.runPure S.identityAnswer gs (do S.cast S.alice handId; Stack.resolveTop)
        jaceId = theJace board
        after = useAbility plusTwo jace jaceId board
    Spec.assertEqWith s "six plus two, not six plus four" (S.counterOf CounterKind.Loyalty jaceId after) 8

  -- The control above with ONE thing changed: Vorinclex, Monstrous Raider's
  -- clause names a PLAYER ("if you would put") rather than an effect, and the
  -- payer is a player whatever moment they pay at -- so CR 614.1 reaches the
  -- cost CR 606.4 charges where CR 614.16 does not. Mirrors
  -- Pawl.ReplacementSpec's "CR 614.1 Vorinclex DOES double the same blight".
  --
  -- Vorinclex is on the battlefield BEFORE Jace resolves on purpose. CR 306.5b's
  -- entry counters go through the same funnel, so the six witnesses that the row
  -- is live, is gathered, resolves its "yours" against alice and reaches this
  -- very object -- without which the ten could not tell a fix from a Vorinclex
  -- that never applied at all. It is asserted SECOND so that it cannot absorb a
  -- mutation of the cost placement the ten exists to prove.
  Spec.it s "CR 614.1 Vorinclex doubles a loyalty ability's own cost" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    let (gs, handId) = S.handOne jace (snd (S.addCreature vorinclex S.alice (stockLibraries island (S.landsInPlay island 3))))
        board = S.runPure S.identityAnswer gs (do S.cast S.alice handId; Stack.resolveTop)
        jaceId = theJace board
        after = useAbility plusTwo jace jaceId board
    Spec.assertEqWith s "six plus four, not six plus two" (S.counterOf CounterKind.Loyalty jaceId after) 10
    Spec.assertEqWith s "and the row was live on the way in: three doubled to six" (S.counterOf CounterKind.Loyalty jaceId board) 6

  -- CR 115.4: "These targets may be creatures, players, planeswalkers, or
  -- battles." Read off Lightning Bolt's OWN committed target slot rather than a
  -- hand-built one, so what is under test is the pool the card data selects.
  Spec.it s "CR 115.4 an 'any target' spell offers the planeswalker alongside the players" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    jace <- S.printingOf s registry "Jace Beleren"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (jaceId, _, gs) = burnAtJace island mountain jace lightningBolt
        offered = fmap (\theSlot -> Target.legalRecipients (Just S.alice) S.noSource theSlot gs) (S.spellTargetSlot lightningBolt)
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

-- CR 122's counter REMOVAL as an event a trigger can see, through the two
-- removals a planeswalker performs: CR 606.4's loyalty cost (Pawl.Engine.Cost,
-- routed through Pawl.Engine.Event.removeCounters) and CR 120.3c / 306.8's damage
-- (Pawl.Engine.Damage, which diffs the boards instead, for the reason its own
-- comment gives).
--
-- Chandra, Fire Artisan -- {2}{R}{R} Legendary Planeswalker -- Chandra, printed
-- loyalty 4 -- is the group's card and the pool's only producer of
-- TriggerCondition.SelfCountersRemoved: "whenever one or more loyalty counters are
-- removed from Chandra, she deals that much damage to target opponent or
-- planeswalker". Her +1 and -7 exile the top of the library and grant CR 601.1a's
-- permission to play what was exiled; the -7 is what drives the cost half.
--
-- Every board here leaves counters BEHIND, which is what separates this condition
-- from TriggerCondition.SelfLastCounterRemoved: an implementation that read the
-- after-count would match none of them.
countersRemovedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
countersRemovedSpec s registry = Spec.describe s "CountersRemoved" $ do
  -- The DAMAGE half. Lightning Bolt's 3 against printed loyalty 4: three counters
  -- come off, one stays, and bob takes three. Every number here is distinct from
  -- every other reading of the rule this board admits -- the loyalty is 4, the
  -- damage 3, the remainder 1 -- so a trigger stamped with the wrong one cannot
  -- land on 17.
  Spec.it s "CR 306.8 / 603.2 the three loyalty counters Lightning Bolt takes off Chandra deal three to bob" $ do
    mountain <- S.printingOf s registry "Mountain"
    chandra <- S.printingOf s registry "Chandra, Fire Artisan"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (chandraId, board) = chandraOnBattlefield mountain chandra 1
        (boltId, withBolt) = S.addHandCard lightningBolt S.alice board
        resolved = S.runPure (aimedAt chandraId) withBolt (do S.cast S.alice boltId; Stack.resolveTop)
        after = firedTrigger (aimedAtPlayer S.bob) resolved
    Spec.assertEqWith s "bob took three, the number of counters that came off" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "CR 306.8: 4 - 3, so this was NOT the last counter" (S.counterOf CounterKind.Loyalty chandraId after) 1
    Spec.assertBool s (Set.member chandraId (GameState.battlefield after)) "and Chandra is still on the battlefield"

  -- The COST half, on the same card and a different removal path entirely. Ten
  -- loyalty and not seven: CR 606.6 admits the -7 at either, but seven would take
  -- her to zero and CR 704.5i would bury her mid-trigger, which is a case about
  -- last known information rather than about the funnel.
  Spec.it s "CR 606.4 / 603.2 paying Chandra's -7 removes seven loyalty counters and deals seven to bob" $ do
    mountain <- S.printingOf s registry "Mountain"
    chandra <- S.printingOf s registry "Chandra, Fire Artisan"
    let (chandraId, board) = chandraOnBattlefield mountain chandra 0
        atTen = S.addCounter CounterKind.Loyalty 6 chandraId board
        spent = useAbility minusSeven chandra chandraId atTen
        after = firedTrigger (aimedAtPlayer S.bob) spent
    Spec.assertEqWith s "bob took seven" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith s "CR 606.4: 10 - 7" (S.counterOf CounterKind.Loyalty chandraId after) 3
    Spec.assertEqWith s "and the -7 did resolve: seven cards left alice's library" (length (Game.zoneMembers Zone.Library S.alice after)) 1

  -- The control for the pair above: the +1 ADDS counters, so no removal happens
  -- and the trigger does not fire. One board, one card, and the only difference
  -- from the case above is which loyalty ability was activated -- which is what
  -- makes the seven damage there the removal's and not the activation's.
  Spec.it s "CR 606.4 Chandra's +1 removes nothing, so the trigger does not fire" $ do
    mountain <- S.printingOf s registry "Mountain"
    chandra <- S.printingOf s registry "Chandra, Fire Artisan"
    let (chandraId, board) = chandraOnBattlefield mountain chandra 0
        atTen = S.addCounter CounterKind.Loyalty 6 chandraId board
        after = firedTrigger (aimedAtPlayer S.bob) (useAbility plusOne chandra chandraId atTen)
    Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "CR 606.4: 10 + 1" (S.counterOf CounterKind.Loyalty chandraId after) 11
    Spec.assertEqWith s "and the +1 did resolve: one card left alice's library" (length (Game.zoneMembers Zone.Library S.alice after)) 7

  -- CR 510.2's simultaneity, which is the property the board diff in
  -- Pawl.Engine.Damage exists to keep and the reason that site is not routed
  -- through Pawl.Engine.Event.removeCounters. Two 2/1 Goblin Pikers attacking one
  -- six-loyalty Chandra remove four counters BETWEEN them, in one batch: one
  -- record of four, so one trigger for four.
  --
  -- The life total cannot see the difference on its own -- two triggers of two
  -- also total four -- so the trigger COUNT is asserted off the stack, before it
  -- resolves. The life total is asserted first all the same, because it is what
  -- catches the other wrong reading: a single trigger stamped with one damage
  -- event's two rather than the pair's four.
  Spec.it s "CR 510.2 two attackers taking four loyalty counters off Chandra together fire her trigger once, for four" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    chandra <- S.printingOf s registry "Chandra, Fire Artisan"
    let (board, _, theirs) = S.combatBoardOf [piker, piker] [chandra]
        chandraId = case theirs of
          oid : _ -> oid
          [] -> S.noSource
        gs = S.addCounter CounterKind.Loyalty 6 chandraId board
        atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) attackingChandra gs
        dealt = S.runPure attackingChandra atDamage (do Engine.runTurnBasedActions (Phase.Combat CombatStep.CombatDamage); Engine.settleForPriority)
        -- The WHOLE stack, not its top: with one trigger the second call finds
        -- nothing, and with two it resolves the other -- which is what lets the
        -- life total below tell the two readings apart rather than reading the
        -- top trigger's damage under either.
        after = S.runPure attackingChandra dealt (Monad.replicateM_ 2 Stack.resolveTop)
    Spec.assertEqWith s "alice took four: 2 + 2, once and not once per damage event" (S.lifeOf S.alice after) (Just 16)
    Spec.assertEqWith s "one trigger on the stack, not one per damage event" (length (GameState.stack dealt)) 1
    Spec.assertEqWith s "CR 306.8: 6 - 4" (S.counterOf CounterKind.Loyalty chandraId dealt) 2
    Spec.assertEqWith s "CR 510.1b: none of it reached the defending player" (S.lifeOf S.bob after) (Just 20)

-- CR 606.5: "If the total cost to activate a loyalty ability contains multiple
-- costs to add or remove loyalty counters, those costs are combined into a single
-- cost to add or remove loyalty counters, as appropriate."
--
-- Every pair here is one board differing in exactly one permanent: Carth. The
-- numbers are deliberately distinct -- printed loyalty 3, six added counters, 9
-- on the permanent, a printed cost of -10, an added cost of +1, a combined -9 --
-- so no two readings of the rule answer alike on this board.
combinedLoyaltyCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
combinedLoyaltyCostSpec s registry = Spec.describe s "CombinedLoyaltyCost" $ do
  -- The direction that actually diverges: pawl was STRICTER than the rules here.
  -- The -10 and the +1 asked separately refuse at 9 loyalty, because CR 606.6's
  -- check reads the counters present before any of the cost is paid; combined,
  -- the cost is -9 and 9 pays it.
  Spec.it s "CR 606.5 Carth's added +1 makes Jace's -10 activatable at 9 loyalty" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    carth <- S.printingOf s registry "Carth the Lion"
    let (jaceId, board) = jaceOnBattlefield island jace
        atNine = S.addCounter CounterKind.Loyalty 6 jaceId board
        withCarth = snd (S.addCreature carth S.alice atNine)
    Spec.assertEqWith s "3 printed plus 6 is 9" (S.counterOf CounterKind.Loyalty jaceId withCarth) 9
    Spec.assertBool s (not (null (activation jaceId minusTen jace))) "the -10 is a real ability"
    Spec.assertBool
      s
      (all (`elem` Action.legalActions S.alice withCarth) (activation jaceId minusTen jace))
      "the -10 is offered, because the total cost is -9"

  -- The control, on the same fixture minus Carth alone. Without the addition the
  -- cost is a bare -10 and CR 606.6 refuses it at 9 -- so the difference between
  -- the two cases is Carth and nothing about mana, timing, seats or the log.
  Spec.it s "CR 606.6 without Carth the same Jace at 9 loyalty is not offered its -10" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (jaceId, board) = jaceOnBattlefield island jace
        atNine = S.addCounter CounterKind.Loyalty 6 jaceId board
    Spec.assertEqWith s "the same 9 loyalty" (S.counterOf CounterKind.Loyalty jaceId atNine) 9
    Spec.assertBool
      s
      (not (any (`elem` Action.legalActions S.alice atNine) (activation jaceId minusTen jace)))
      "a bare -10 needs 10"
    -- And the +2 still is, so the board's refusal is about this ability's cost
    -- rather than about CR 606.3's window having closed.
    Spec.assertBool
      s
      (all (`elem` Action.legalActions S.alice atNine) (activation jaceId plusTwo jace))
      "while the +2 is offered on the very same board"

  -- The PAYMENT and not just the gate. 9 - 10 + 1 is 0, so CR 704.5i buries Jace;
  -- a fix that combined for the gate while paying the components one at a time
  -- would leave the removal unpayable and the activation rejected outright.
  Spec.it s "CR 606.5 / 704.5i paying the combined -9 spends all nine counters and buries Jace" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    carth <- S.printingOf s registry "Carth the Lion"
    let (jaceId, board) = jaceOnBattlefield island jace
        atNine = S.addCounter CounterKind.Loyalty 6 jaceId board
        withCarth = snd (S.addCreature carth S.alice atNine)
        after = useAbility minusTen jace jaceId withCarth
    Spec.assertEqWith s "nine counters spent, not ten and not nine less one added back" (S.counterOf CounterKind.Loyalty jaceId after) 0
    Spec.assertBool s (Set.member jaceId (GameState.battlefield after)) "CR 120.5: still there before the state-based action"
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield (S.settleSba after)))) "CR 704.5i: loyalty 0, so buried"

  -- Carth taxes the ADDING half too, which is the other side of "as appropriate":
  -- +2 and +1 combine to a single +3, so one activation of the +2 leaves Jace at
  -- 3 + 3 rather than 3 + 2. This is the case a fix that only ever emitted a
  -- RemoveLoyaltyFromThis would fail.
  Spec.it s "CR 606.5 the +2 and the added +1 combine to a single +3" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    carth <- S.printingOf s registry "Carth the Lion"
    let (jaceId, board) = jaceOnBattlefield island jace
        withCarth = snd (S.addCreature carth S.alice board)
        after = useAbility plusTwo jace jaceId withCarth
        without = useAbility plusTwo jace jaceId board
    Spec.assertEqWith s "3 + 3 with Carth" (S.counterOf CounterKind.Loyalty jaceId after) 6
    Spec.assertEqWith s "3 + 2 without him" (S.counterOf CounterKind.Loyalty jaceId without) 5

  -- The net-zero combination, which Jace's -1 and Carth's +1 reach: the single
  -- cost adjusts nothing, so the ability is free and the loyalty is untouched. A
  -- FENCE rather than a proof of the choice Cost.combineLoyalty makes there --
  -- emitting nothing instead of a zero component answers the same on every board
  -- the rules admit, because the one place the two could differ is CR 606.6 at 0
  -- loyalty and CR 704.5i has already buried a planeswalker there.
  Spec.it s "CR 606.5 the -1 and the added +1 combine to a cost that adjusts nothing" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    carth <- S.printingOf s registry "Carth the Lion"
    let (jaceId, board) = jaceOnBattlefield island jace
        withCarth = snd (S.addCreature carth S.alice board)
        after = useAbility minusOne jace jaceId withCarth
    Spec.assertEqWith s "still 3, neither 2 nor 4" (S.counterOf CounterKind.Loyalty jaceId after) 3
    Spec.assertEqWith s "and the ability did resolve: exactly one player drew" (S.handSize S.alice after + S.handSize S.bob after) 1

-- CR 306.5a's printed loyalty is a number on every planeswalker but one. Nissa,
-- Steward of Elements prints CR 107.3's X there, and CR 107.3m says what it is
-- worth: the value chosen for the spell that became the permanent, "although the
-- value of X for that permanent is 0".
--
-- Every case below reads the loyalty COUNTERS on the permanent (CR 306.5c) and
-- not merely that the spell resolved, and every pair holds the board fixed and
-- moves only the announcement.
variableLoyaltySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
variableLoyaltySpec s registry = Spec.describe s "VariableLoyalty" $ do
  Spec.it s "CR 306.5b / 107.3m Nissa enters with as many loyalty counters as the X she was cast for" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    let (nissaId, after) = nissaCastFor forest island nissa [] 5
    Spec.assertBool s (Set.member nissaId (GameState.battlefield after)) "on the battlefield"
    -- Five, and no other reading of the rule this board admits answers five: the
    -- spell's mana value on the stack was seven, its printed symbols number
    -- three, an unread announcement is zero, and nine lands were available.
    Spec.assertEqWith s "loyalty 5" (S.counterOf CounterKind.Loyalty nissaId after) 5

  -- The pair. One board, one card, one difference -- the announced X -- so an
  -- implementation reading anything else off the spell (its mana value, its
  -- generic cost, a constant) cannot pass both halves.
  Spec.it s "CR 107.3m the same board announced at X=3 enters with three instead" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    let (atFive, five) = nissaCastFor forest island nissa [] 5
        (atThree, three) = nissaCastFor forest island nissa [] 3
    Spec.assertEqWith s "loyalty 3" (S.counterOf CounterKind.Loyalty atThree three) 3
    Spec.assertBool
      s
      (S.counterOf CounterKind.Loyalty atFive five /= S.counterOf CounterKind.Loyalty atThree three)
      "the two boards disagree about the loyalty"

  -- CR 107.1b forbids a negative X and nothing forbids zero, so {0}{G}{U} is a
  -- legal announcement -- and CR 306.5b then puts no counters on at all.
  Spec.it s "CR 107.1b / 704.5i announced at X=0 she enters with no loyalty and is buried" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    let (nissaId, after) = nissaCastFor forest island nissa [] 0
        settled = S.settleSba after
    Spec.assertEqWith s "no loyalty counters" (S.counterOf CounterKind.Loyalty nissaId after) 0
    Spec.assertBool s (Set.member nissaId (GameState.battlefield after)) "she did enter the battlefield"
    Spec.assertBool s (not (Set.member nissaId (GameState.battlefield settled))) "CR 704.5i takes her off it"
    -- By NAME, not by id: CR 400.7 mints a new object as the card moves.
    Spec.assertEqWith s "CR 704.5i: in her owner's graveyard" (graveyardCount "Nissa, Steward of Elements" settled) 1

  -- The abilities read the loyalty back, which is what makes the number have to
  -- be right rather than merely present. CR 606.6 gates the -6 on the permanent
  -- having that many loyalty counters, so the announcement decides whether it is
  -- offered at all -- and the +2 and the 0 are the control, offered either way.
  Spec.it s "CR 606.6 the -6 is offered at X=6 and not at X=5" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    let (atSix, six) = nissaCastFor forest island nissa [] 6
        (atFive, five) = nissaCastFor forest island nissa [] 5
        offers oid gs i = not (null (activation oid i nissa)) && all (`elem` Action.legalActions S.alice gs) (activation oid i nissa)
    Spec.assertEqWith s "X=6 is six loyalty" (S.counterOf CounterKind.Loyalty atSix six) 6
    Spec.assertBool s (offers atSix six minusSix) "the -6 is offered at 6"
    Spec.assertBool s (not (offers atFive five minusSix)) "and NOT at 5"
    Spec.assertBool s (offers atFive five plusTwoScry && offers atFive five zeroLook) "while the +2 and the 0 are offered at 5"

  -- CR 107.3m through the card's own text: the `0` reads "a creature card with
  -- mana value less than or equal to the number of loyalty counters on Nissa",
  -- and those counters are the ones X put there. Goblin Piker's mana value is 2.
  Spec.it s "the 0 ability puts a creature card within the X-derived loyalty onto the battlefield" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    piker <- S.printingOf s registry "Goblin Piker"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (nissaId, board) = nissaCastFor forest island nissa [piker, birdMaiden] 5
        after = useNissaAbility zeroLook nissa nissaId board
    Spec.assertEqWith s "loyalty 5, and the Piker's mana value is 2" (S.counterOf CounterKind.Loyalty nissaId after) 5
    Spec.assertEqWith s "the Piker is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
    Spec.assertEqWith s "only the Bird Maiden is left in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 1

  -- The pair's other half, and the ONE thing changed is the X announced: at
  -- loyalty 1 the Piker's mana value of 2 is too high, so the conjunction inside
  -- the card's disjunction is false and the clause does nothing.
  Spec.it s "and leaves it in the library when the loyalty is below its mana value" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    piker <- S.printingOf s registry "Goblin Piker"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (nissaId, board) = nissaCastFor forest island nissa [piker, birdMaiden] 1
        after = useNissaAbility zeroLook nissa nissaId board
    Spec.assertEqWith s "loyalty 1, below the Piker's mana value of 2" (S.counterOf CounterKind.Loyalty nissaId after) 1
    Spec.assertEqWith s "nothing entered the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 0
    Spec.assertEqWith s "both cards are still in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 2

  -- The land half of the same disjunction, which no mana value gates: CR 107.3m's
  -- X is irrelevant to it, so a Forest on top goes to the battlefield at the
  -- loyalty that just refused the Piker.
  Spec.it s "the 0 ability puts a land card onto the battlefield whatever the loyalty" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (nissaId, board) = nissaCastFor forest island nissa [forest, birdMaiden] 1
        after = useNissaAbility zeroLook nissa nissaId board
    Spec.assertEqWith s "loyalty 1" (S.counterOf CounterKind.Loyalty nissaId after) 1
    Spec.assertEqWith s "seven Forests: the six paid with plus the one put onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Forest")) S.alice after) 7
    Spec.assertEqWith s "only the Bird Maiden is left in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 1

  -- The -6, paid for out of the X-derived loyalty. CR 205.1b splits the card's
  -- two sentences: "they're still lands" is why the CREATURE card type is added
  -- rather than set, and the same rule's last clause is why the creature TYPE is
  -- set rather than added.
  Spec.it s "CR 205.1b the -6 untaps two lands and makes them 5/5 Elemental creature lands with flying and haste" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    nissa <- S.printingOf s registry "Nissa, Steward of Elements"
    let (nissaId, board) = nissaCastFor forest island nissa [] 6
        after = useNissaAbility minusSix nissa nissaId board
        animated = filter (Set.member CardType.Creature . PC.cardTypes) (fmap (`Projection.project` after) (Set.toList (GameState.battlefield after)))
    Spec.assertEqWith s "the -6 spent the six counters it cost" (S.counterOf CounterKind.Loyalty nissaId after) 0
    -- Eight of the nine lands paid for {6}{G}{U}; two of those eight are untapped
    -- again, and nothing else on this board taps or untaps.
    Spec.assertEqWith s "eight lands were tapped to cast her" (S.tappedCount S.alice board) 8
    Spec.assertEqWith s "and two of them are untapped again" (S.tappedCount S.alice after) 6
    Spec.assertEqWith s "two lands were animated, not one and not every land" (length animated) 2
    Spec.assertEqWith s "each is 5/5" (fmap (\pc -> (PC.power pc, PC.toughness pc)) animated) [(Just 5, Just 5), (Just 5, Just 5)]
    Spec.assertBool s (all (Set.member CardType.Land . PC.cardTypes) animated) "they're still lands"
    Spec.assertBool s (all (Set.member Subtype.Elemental . PC.subtypes) animated) "each is an Elemental"
    Spec.assertBool s (all (\pc -> Map.member Keyword.Flying (PC.keywords pc) && Map.member Keyword.Haste (PC.keywords pc)) animated) "with flying and haste"

-- Grist, the Hunger Tide's abilities in the order the card file carries them: the
-- -2 then the -5. Indexed for the reason Jace's are. The printed +1 has no index
-- because pawl's Grist does not carry it (#1932).
minusTwo, minusFive :: Int
minusTwo = 0
minusFive = 1

-- Grist on the battlefield under alice's control with this many loyalty counters.
-- PLACED and not cast, unlike jaceOnBattlefield: no case below is about CR 306.5b,
-- and the -5 needs more loyalty than the printed 3 -- the +1 that would climb
-- there is the half of the card pawl cannot write (#1932). The -5 case is the one
-- whose loyalty is not the printed 3, so it asserts the six the fixture put on
-- before reading what the cost took off; the -2 boards keep the printed number.
gristWith :: Natural -> Printing.Printing -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
gristWith loyalty grist gs =
  let (oid, placed) = S.addCreature grist S.alice gs
   in (oid, S.addCounter CounterKind.Loyalty loyalty oid placed)

-- useAbility with an answerer of the caller's choosing, which the -2 needs: its CR
-- 118.12 gate and the reflexive ability's target are both real choices here.
useGristAbility ::
  (forall r. Prompt.Prompt r -> r) ->
  Int ->
  Printing.Printing ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  GameState.GameState
useGristAbility answer i p oid gs = case abilityAt i p of
  ability : _ -> S.runPure answer gs (do Activate.activateAbility S.alice oid ability; Stack.resolveTop)
  [] -> gs

-- `decision` for the -2's CR 118.12 gate, and a target chosen by PREFERENCE over
-- the set the engine offered -- aimedAt's posture with a ranking instead of one
-- id, so a case can ask for a permanent it expects the pool to withhold and find
-- out. Filtering the offered set rather than building a Recipient is what keeps
-- CR 608.2b's re-read from dropping the choice.
--
-- The whole offered set follows as a fallback, which keeps the answerer total; a
-- board offering none of `prefer` then targets whatever the pool's Ord puts first,
-- and the case's own assertions are what catch it.
gristAnswer :: PaymentDecision.PaymentDecision -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
gristAnswer decision prefer p = case p of
  Prompt.ChooseToPay {} -> decision
  Prompt.ChooseTargets _ _ _ sets ->
    let ranked candidates = concatMap (\oid -> filter (\r -> Recipient.objectOf r == Just oid) (Set.toList candidates)) prefer
        naming (n, candidates) =
          Set.fromList
            . take (Natural.toIntSaturating n)
            $ ranked candidates <> Set.toList candidates
     in fmap naming sets
  _ -> S.identityAnswer p

-- alice holds Grist at 3 loyalty and ONE Goblin Piker; bob holds Jace Beleren at 3
-- loyalty, a Villainous Ogre and a Mountain. Returned as
-- (Grist, alice's Piker, bob's Jace, bob's Mountain).
--
-- One creature for alice because CR 118.12's cost then has exactly its count of
-- candidates and Pawl.Types.Prompt.ChooseSacrifices is elided -- there is nothing
-- to choose. Grist is not among them: its "as long as Grist isn't on the
-- battlefield" ability (CR 113.6c) is switched off exactly here.
--
-- The reflexive ability's slot then has more candidates than its count of one --
-- bob's Ogre and his Jace, and Grist itself, which is a planeswalker on the
-- battlefield and so a legal choice for its own ability -- so a prompt offered
-- exactly its count cannot short-circuit. The Mountain is the permanent the slot
-- must NOT offer, which is how "creature or planeswalker" is read as a
-- restriction rather than assumed.
gristMinusTwoBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
gristMinusTwoBoard grist piker jace ogre mountain =
  let (pikerId, withPiker) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      (jaceId, withJace) = S.addCreature jace S.bob withPiker
      loyal = S.addCounter CounterKind.Loyalty 3 jaceId withJace
      (_, withOgre) = S.addCreature ogre S.bob loyal
      (mountainId, withMountain) = S.addCreature mountain S.bob withOgre
      (gristId, board) = gristWith 3 grist withMountain
   in (gristId, pikerId, jaceId, mountainId, board)

-- Grist, the Hunger Tide -- {1}{B}{G} Legendary Planeswalker -- Grist, printed
-- loyalty 3 (Oracle text fetched from Scryfall 2026-08-25) -- carries two of its
-- three loyalty abilities here:
--
--   -2: "You may sacrifice a creature. When you do, destroy target creature or
--       planeswalker."
--   -5: "Each opponent loses life equal to the number of creature cards in your
--       graveyard."
--
-- Not implemented: the +1, "create a 1/1 black and green Insect creature token,
-- then mill a card. If an Insect card was milled this way, put a loyalty counter
-- on Grist and repeat this process" -- the repeat is a loop the effect DSL has no
-- shape for (#1932). That leaves pawl's Grist STRICTER than printed.
--
-- The -2 is a CR 603.12 reflexive trigger whose armed ability targets a
-- PERMANENT; Pawl.CastSpec's FugitiveDoctor group reads the shape against a card
-- in a graveyard. The -5 is read at three seats, which is what separates
-- "each opponent" from "each player" and from "target opponent" at once.
gristLoyaltySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gristLoyaltySpec s registry = Spec.describe s "GristLoyalty" $ do
  -- Three graveyards, no two of which agree. alice's holds three creature cards
  -- and two Lightning Bolts, bob's four creature cards and carol's one -- so
  -- "creature cards in your graveyard" is 3, "cards in your graveyard" is 5, and
  -- "creature cards in every graveyard" is 8. Only the first lands on 17.
  Spec.it s "CR 606.4 the -5 takes each opponent for the creature cards in alice's graveyard alone" $ do
    grist <- S.printingOf s registry "Grist, the Hunger Tide"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let bury n printing pid gs = List.foldl' (\g _ -> snd (S.addGraveyardCard printing pid g)) gs [1 :: Int .. n]
        stocked =
          bury 1 piker S.carol
            . bury 4 piker S.bob
            . bury 2 bolt S.alice
            $ bury 3 piker S.alice S.threePlayerGame
        (gristId, board) = gristWith 6 grist stocked
        after = useGristAbility S.identityAnswer minusFive grist gristId board
    Spec.assertEqWith
      s
      "both opponents lost three and alice lost nothing"
      (S.lifeOf S.alice after, S.lifeOf S.bob after, S.lifeOf S.carol after)
      (Just 20, Just 17, Just 17)
    Spec.assertEqWith s "the fixture's six loyalty" (S.counterOf CounterKind.Loyalty gristId board) 6
    Spec.assertEqWith s "CR 606.4: five of them came off" (S.counterOf CounterKind.Loyalty gristId after) 1

  -- The answerer PREFERS the Mountain and settles for the Jace. So a slot that
  -- offered every permanent would destroy the land and leave the planeswalker
  -- standing, which is the reading this case rules out; and one that offered only
  -- creatures would leave the Jace standing too.
  Spec.it s "CR 603.12 / 701.8a the -2's reflexive trigger destroys the planeswalker it targets, and no land is offered" $ do
    grist <- S.printingOf s registry "Grist, the Hunger Tide"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    ogre <- S.printingOf s registry "Villainous Ogre"
    mountain <- S.printingOf s registry "Mountain"
    let (gristId, pikerId, jaceId, mountainId, board) = gristMinusTwoBoard grist piker jace ogre mountain
        answer :: Prompt.Prompt r -> r
        answer = gristAnswer PaymentDecision.Pays [mountainId, jaceId]
        after = firedTrigger answer (useGristAbility answer minusTwo grist gristId board)
    Spec.assertEqWith
      s
      "the Jace died and the Mountain the answerer asked for first did not"
      (S.onBattlefield jaceId after, S.onBattlefield mountainId after)
      (False, True)
    Spec.assertEqWith s "CR 701.8a: into its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertBool s (not (S.onBattlefield pikerId after)) "CR 701.21a: alice's own creature paid for it"
    Spec.assertEqWith s "CR 606.4: two of the three loyalty came off" (S.counterOf CounterKind.Loyalty gristId after) 1

  -- The pair. One thing differs -- alice declines CR 118.12's optional cost -- so
  -- the same board, the same seats and the same answerer for every other prompt.
  Spec.it s "CR 118.12 declining the -2's sacrifice arms nothing and destroys nothing" $ do
    grist <- S.printingOf s registry "Grist, the Hunger Tide"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    ogre <- S.printingOf s registry "Villainous Ogre"
    mountain <- S.printingOf s registry "Mountain"
    let (gristId, pikerId, jaceId, mountainId, board) = gristMinusTwoBoard grist piker jace ogre mountain
        answer :: Prompt.Prompt r -> r
        answer = gristAnswer PaymentDecision.Declines [mountainId, jaceId]
        after = firedTrigger answer (useGristAbility answer minusTwo grist gristId board)
    Spec.assertEqWith
      s
      "the Jace the paying board destroyed is untouched, and so is alice's creature"
      (S.onBattlefield jaceId after, S.onBattlefield pikerId after)
      (True, True)
    Spec.assertEqWith s "bob's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "CR 606.4: the loyalty cost was paid either way" (S.counterOf CounterKind.Loyalty gristId after) 1
