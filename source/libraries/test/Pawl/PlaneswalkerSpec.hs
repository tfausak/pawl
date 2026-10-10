{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 306, the planeswalker card type, across the seven modules it reaches:
-- Pawl.Engine.Projection's intrinsic CR 306.5b enters-with replacement and
-- Pawl.Engine.Replacement's EntryR arm that carries it out, Pawl.Engine.Cost's
-- two CR 606.4 loyalty cost components and CR 606.5's combining of them,
-- Pawl.Engine.Activate's CR 606.3 gate,
-- Pawl.Engine.Sba's CR 704.5i zero-loyalty state-based action, Pawl.Engine.Target's
-- CR 115.4 "any target" and CR 115.2 "player or planeswalker" pools -- the two
-- pools that offer a planeswalker at all -- and Pawl.Engine.Damage's CR 306.8 / CR 120.3c
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
-- group. Its "loyalty abilities" is ActivationCriteria.whichLoyalty, which asks CR
-- 606.2 of the ability being activated where whichAbilities can only ask about
-- the source permanent; data/scenarios/planeswalker is what proves the two are
-- not the same question.
--
-- A Realm Reborn -- {4}{G}{G} Enchantment, "Other permanents you control have
-- '{T}: Add one mana of any color.'" (name, cost, type line and Oracle text
-- checked against api.scryfall.com, 2026-09-06) -- is that group's second card,
-- and it is there to put a NON-loyalty activated ability on the very
-- planeswalker Carth taxes. Scryfall
-- `t:planeswalker (o:/^\{T\}:/ or o:/^\{[0-9WUBRGCXSP\/]+\}(, \{T\})?:/) include:extras`,
-- 2026-09-06, three hits and all of them flip cards whose front face is the
-- creature: no printing gives a planeswalker such an ability of its own, so a
-- GRANTED one is the only board on which both halves of CR 606.2 sit on a single
-- permanent. Gideon Blackblade under Presence of Gond would refute that, and
-- would give the group a non-mana ability besides.
module Pawl.PlaneswalkerSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Scenario as Scenario
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostAmount as CostAmount
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Move as Move
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Placement as Placement
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ScenarioFailure as ScenarioFailure
import qualified Pawl.Types.Seat as Seat
import qualified Pawl.Types.Staged as Staged
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Zone as Zone

-- Jace Beleren's abilities in printed order: +2, -1, -10. Indexed rather than
-- taken with a `head` the way every other spec's `theAbility` is, because this is
-- the first card in the pool with more than one activated ability and CR 606.6 is
-- a claim about WHICH of them is offered.
abilityAt :: Int -> Printing.Printing -> [ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)]
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
      (_, withMountain) = S.addPermanent mountain S.alice board
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
  wanderingEmperorSpec s registry

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

  -- The other half of the same rule, and the reason the two placements are
  -- deliberately different code paths: CR 614.16's FIRST sentence limits a
  -- counter-scaling replacement to counters an EFFECT puts on, and CR 606.4's
  -- loyalty symbol is a cost. Doubling Season's own ruling says so.
  Spec.it s "CR 614.16 Doubling Season does not double a loyalty ability's own cost" $ do
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    let (gs, handId) = S.handOne jace (snd (S.addPermanent doublingSeason S.alice (stockLibraries island (S.landsInPlay island 3))))
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
    let (gs, handId) = S.handOne jace (snd (S.addPermanent vorinclex S.alice (stockLibraries island (S.landsInPlay island 3))))
        board = S.runPure S.identityAnswer gs (do S.cast S.alice handId; Stack.resolveTop)
        jaceId = theJace board
        after = useAbility plusTwo jace jaceId board
    Spec.assertEqWith s "six plus four, not six plus two" (S.counterOf CounterKind.Loyalty jaceId after) 10
    Spec.assertEqWith s "and the row was live on the way in: three doubled to six" (S.counterOf CounterKind.Loyalty jaceId board) 6

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

-- CR 122's counter REMOVAL as an event a trigger can see, through the two
-- removals a planeswalker performs: CR 606.4's loyalty cost (Pawl.Engine.Cost,
-- routed through Pawl.Engine.Event.removeCounters) and CR 120.3c / 306.8's damage
-- (Pawl.Engine.Damage, which diffs the boards instead, for the reason its own
-- comment gives).
--
-- Chandra, Fire Artisan -- {2}{R}{R} Legendary Planeswalker -- Chandra, printed
-- loyalty 4 -- is the group's card, and the pool's loyalty producer of
-- TriggerCondition.SelfCountersRemoved: "whenever one or more loyalty counters are
-- removed from Chandra, she deals that much damage to target opponent or
-- planeswalker". Her +1 and -7 exile the top of the library and grant CR 601.1a's
-- permission to play what was exiled; the -7 is what drives the cost half.
--
-- Every board here leaves counters BEHIND, which is what separates this condition
-- from TriggerCondition.SelfLastCounterRemoved: an implementation that read the
-- after-count would match none of them.
countersRemovedSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
countersRemovedSpec s registry = Spec.describe s "CountersRemoved" $ do
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
        withCarth = snd (S.addPermanent carth S.alice atNine)
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
        withCarth = snd (S.addPermanent carth S.alice atNine)
        after = useAbility minusTen jace jaceId withCarth
    Spec.assertEqWith s "nine counters spent, not ten and not nine less one added back" (S.counterOf CounterKind.Loyalty jaceId after) 0
    Spec.assertBool s (Set.member jaceId (GameState.battlefield after)) "CR 120.5: still there before the state-based action"
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield (S.settleSba after)))) "CR 704.5i: loyalty 0, so buried"

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

-- Grist, the Hunger Tide's abilities in the order the card file carries them:
-- the +1, the -2, then the -5. Indexed for the reason Jace's are; the +1 is
-- prefixed apart from Chandra's.
gristPlusOne, minusTwo, minusFive :: Int
gristPlusOne = 0
minusTwo = 1
minusFive = 2

-- Grist on the battlefield under alice's control with this many loyalty counters.
-- PLACED and not cast, unlike jaceOnBattlefield: no case below is about CR 306.5b,
-- and the -5 needs more loyalty than the printed 3. The -5 case is the one
-- whose loyalty is not the printed 3, so it asserts the six the fixture put on
-- before reading what the cost took off; the other boards keep the printed number.
gristWith :: Natural -> Printing.Printing -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
gristWith loyalty grist gs =
  let (oid, placed) = S.addPermanent grist S.alice gs
   in (oid, S.addCounter CounterKind.Loyalty loyalty oid placed)

-- Activate alice's `i`th loyalty ability and resolve it, with an answerer of the
-- caller's choosing -- which Grist's -2 needs for its CR 118.12 gate and its
-- reflexive ability's target, and Ashiok's +1 for its CR 608.2d choice.
useLoyaltyAbility ::
  (forall r. Prompt.Prompt r -> r) ->
  Int ->
  Printing.Printing ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  GameState.GameState
useLoyaltyAbility answer i p oid gs = case abilityAt i p of
  ability : _ -> S.runPure answer gs (do Activate.activateAbility S.alice oid ability; Stack.resolveTop)
  [] -> gs

-- Grist, the Hunger Tide -- {1}{B}{G} Legendary Planeswalker -- Grist, printed
-- loyalty 3 (Oracle text fetched from Scryfall 2026-09-29) -- carries all three
-- of its loyalty abilities here:
--
--   +1: "Create a 1/1 black and green Insect creature token, then mill a card.
--       If an Insect card was milled this way, put a loyalty counter on Grist
--       and repeat this process."
--   -2: "You may sacrifice a creature. When you do, destroy target creature or
--       planeswalker."
--   -5: "Each opponent loses life equal to the number of creature cards in your
--       graveyard."
--
-- The +1 is Effect.RepeatIf, CR 608.2c's condition-gated loop.
--
-- The -2 is a CR 603.12 reflexive trigger whose armed ability targets a
-- PERMANENT; Pawl.CastSpec's FugitiveDoctor group reads the shape against a card
-- in a graveyard. The -5 is read at three seats, which is what separates
-- "each opponent" from "each player" and from "target opponent" at once.
gristLoyaltySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gristLoyaltySpec s registry = Spec.describe s "GristLoyalty" $ do
  -- alice's library, top first: a Mind Maggots, a Grist card, a Goblin Piker and a
  -- Lightning Bolt. Two Insect cards and then one that is not, so the process runs
  -- three times and stops with the Bolt unmilled. The Grist card is an Insect card
  -- in a library by its own CR 113.6c ability, so a tally blind to that stops a
  -- run early.
  Spec.it s "CR 608.2c the +1 repeats while an Insect card is milled, and stops at the first that is not" $ do
    grist <- S.printingOf s registry "Grist, the Hunger Tide"
    maggots <- S.printingOf s registry "Mind Maggots"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (boltId, withBolt) = S.addLibraryCard bolt S.alice (Setup.emptyGame S.bothPlayers)
        stocked =
          snd
            . S.addLibraryCard maggots S.alice
            . snd
            . S.addLibraryCard grist S.alice
            . snd
            $ S.addLibraryCard piker S.alice withBolt
        (gristId, board) = gristWith 3 grist stocked
        after = useLoyaltyAbility S.identityAnswer gristPlusOne grist gristId board
    Spec.assertEqWith
      s
      "three runs: three Insect tokens, and a loyalty counter for each of the two Insect cards on top of the +1's"
      (length (S.tokensOf after), S.counterOf CounterKind.Loyalty gristId after)
      (3, 6)
    Spec.assertEqWith s "CR 701.17a: the Bolt under the Piker was never milled" (Game.zoneMembers Zone.Library S.alice after) [boltId]
    Spec.assertEqWith s "the three milled cards are in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 3

  -- Two Insect cards and nothing under them: the third run mills nothing, which
  -- mills no Insect card, so the loop ends on an empty library rather than
  -- reading the second run's tally again.
  Spec.it s "CR 701.17b the +1 stops when the library runs out" $ do
    grist <- S.printingOf s registry "Grist, the Hunger Tide"
    maggots <- S.printingOf s registry "Mind Maggots"
    lithophage <- S.printingOf s registry "Lithophage"
    let stocked =
          snd
            . S.addLibraryCard maggots S.alice
            . snd
            $ S.addLibraryCard lithophage S.alice (Setup.emptyGame S.bothPlayers)
        (gristId, board) = gristWith 3 grist stocked
        after = useLoyaltyAbility S.identityAnswer gristPlusOne grist gristId board
    Spec.assertEqWith
      s
      "three runs, and loyalty for the two Insect cards alone"
      (length (S.tokensOf after), S.counterOf CounterKind.Loyalty gristId after)
      (3, 6)
    Spec.assertEqWith s "the library is empty" (Game.zoneMembers Zone.Library S.alice after) []

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
        after = useLoyaltyAbility S.identityAnswer minusFive grist gristId board
    Spec.assertEqWith
      s
      "both opponents lost three and alice lost nothing"
      (S.lifeOf S.alice after, S.lifeOf S.bob after, S.lifeOf S.carol after)
      (Just 20, Just 17, Just 17)
    Spec.assertEqWith s "the fixture's six loyalty" (S.counterOf CounterKind.Loyalty gristId board) 6
    Spec.assertEqWith s "CR 606.4: five of them came off" (S.counterOf CounterKind.Loyalty gristId after) 1

-- Ashiok, Wicked Manipulator -- {3}{B}{B} Legendary Planeswalker -- Ashiok,
-- printed loyalty 5 (name, cost, type line, loyalty and Oracle text checked
-- against api.scryfall.com 2026-09-05) -- carries two of its three loyalty
-- abilities here:
--
--   +1: "Look at the top two cards of your library. Exile one of them and put the
--       other into your hand."
--   -2: "Create two 1/1 black Nightmare creature tokens with 'At the beginning of
--       combat on your turn, if a card was put into exile this turn, put a +1/+1
--       counter on this token.'"
--   -7: "Target player exiles the top X cards of their library, where X is the
--       total mana value of cards you own in exile."
--
-- Its static replacement -- "if you would pay life while your library has at
-- least that many cards in it, exile that many cards from the top of your library
-- instead" -- is Pawl.LifeReplacementSpec's, and stays there.
--
-- The -7's X is Aggregation.Total over Scope.InZone Exile: "cards you own" is
-- CR 108.3's ownership, so the filter carries Filter.OwnedBy You and the scope
-- names the whole shared exile (CR 400.1), which is the only way an InZone scope
-- may name it. The scenario
-- data/scenarios/planeswalker/cr-107-3-202-3-the-7-s-x-is-the-total-mana-value-of-the-cards.json
-- is what proves the SUM: its exile holds two cards of unequal mana value,
-- so Members, Greatest and Total each answer a different number.
--
-- CR 108.4a is why that case cannot tell OwnedBy from ControlledBy: an exiled
-- card is no permanent and no spell, so anything asking its controller gets its
-- owner. OwnedBy is here because it is the printed word, and the pair the case
-- CAN tell apart is You against any player -- bob's exiled card is what that
-- turns on. That same -7 filter's Not IsToken is the printed "cards" (CR 111.6) and
-- no case can read it either: CR 111.7's state-based action takes a token out of
-- exile before any ability could count it.
--
-- The -2's token clause reads EventShape.CardArrivedIn as an intervening "if":
-- the printed sentence names only where the card ARRIVED, so its exclusion set is
-- empty and the origin is not part of the question. The trigger board below exiles from a HAND, never a
-- library, which is what separates "put into exile" from the library-to-exile
-- move Ashiok's own replacement makes.
--
-- The count's filter is Not IsToken and not the empty one, because the printed
-- word is "a CARD was put into exile" and CR 111.6 says a token is not one --
-- data/cards/synthetic-grave-census.json's spelling of the same shape. The shape
-- itself cannot say so: EventShape.CardArrivedIn tests the destination and, where
-- one is named, the origin, so the card-versus-token half is the filter's. The CR 111.6 pair below is what
-- proves it: two boards holding the same objects, differing only in whether the
-- one permanent exiled that turn was a card.
ashiokMinusTwo :: Int
ashiokMinusTwo = 1

-- Ashiok on the battlefield under alice's control at the printed loyalty 5, with
-- `stock` in her library BOTTOM FIRST (S.addLibraryCard puts each new card on
-- top). PLACED rather than cast, as gristWith is and for the same reason: no case
-- here is about CR 306.5b.
ashiokBoard :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
ashiokBoard ashiok stock =
  let stocked = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) S.threePlayerGame stock
      (oid, placed) = S.addPermanent ashiok S.alice stocked
   in (oid, S.addCounter CounterKind.Loyalty 5 oid placed)

-- Whether a logged event is CR 603.2's record that an ability triggered.
isAbilityTriggered :: GameEvent.GameEvent -> Bool
isAbilityTriggered event = case event of
  GameEvent.AbilityTriggered {} -> True
  _ -> False

-- The names of pid's cards in a zone, MassEffectSpec's reader of the same name.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Rule 507's beginning of combat step on alice's turn, staged and then RUN:
-- Engine.runStep writes the CR 603.2b StepBegan record the token's trigger
-- matches, and the priority loop resolves what it put on the stack.
throughBeginningOfCombat :: GameState.GameState -> GameState.GameState
throughBeginningOfCombat gs =
  S.runPure
    S.identityAnswer
    ( S.runPure
        S.identityAnswer
        gs
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
        Engine.runStep
    )
    Engine.priorityLoop

ashiokLoyaltySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ashiokLoyaltySpec s registry = Spec.describe s "AshiokLoyalty" $ do
  -- The token's own trigger. The exile is out of alice's HAND, which is what makes
  -- this a claim about "put into exile" rather than about the library-to-exile
  -- move the rest of the card makes: a condition that pinned the ORIGIN would find
  -- no match here.
  Spec.it s "CR 603.4 a card put into exile from a hand satisfies the token's intervening if" $ do
    ashiok <- S.printingOf s registry "Ashiok, Wicked Manipulator"
    swamp <- S.printingOf s registry "Swamp"
    maiden <- S.printingOf s registry "Bird Maiden"
    let (ashiokId, board) = ashiokBoard ashiok [swamp]
        (heldId, withHeld) = S.addHandCard maiden S.alice board
        minted = useLoyaltyAbility S.identityAnswer ashiokMinusTwo ashiok ashiokId withHeld
        exiled = S.runPure S.identityAnswer minted (Event.changeZone heldId Zone.Exile)
        after = throughBeginningOfCombat exiled
        tokens = S.tokensOf after
    Spec.assertEqWith s "CR 603.4: the intervening if holds, so both tokens' abilities trigger" (length (filter isAbilityTriggered (S.eventsOf after))) 2
    Spec.assertEqWith s "the held card left the hand for exile" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
    Spec.assertEqWith s "still two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "each token is 2/2" (S.powerToughnessOf oid after) (Just (2, 2))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "CR 122.6: one +1/+1 counter each" (S.counterOf CounterKind.PlusOnePlusOne oid after) 1) tokens

  -- The pair, differing in exactly one thing: nothing was exiled. CR 603.4 says the
  -- ability does not trigger at all, which is a stronger claim than "no counter
  -- appeared" -- an ability that triggered and then did nothing would leave the
  -- same counters.
  Spec.it s "CR 603.4 with nothing exiled this turn the token's ability never triggers" $ do
    ashiok <- S.printingOf s registry "Ashiok, Wicked Manipulator"
    swamp <- S.printingOf s registry "Swamp"
    maiden <- S.printingOf s registry "Bird Maiden"
    let (ashiokId, board) = ashiokBoard ashiok [swamp]
        (_, withHeld) = S.addHandCard maiden S.alice board
        minted = useLoyaltyAbility S.identityAnswer ashiokMinusTwo ashiok ashiokId withHeld
        after = throughBeginningOfCombat minted
        tokens = S.tokensOf after
    Spec.assertEqWith s "CR 603.4: the ability did not trigger at all" (length (filter isAbilityTriggered (S.eventsOf after))) 0
    Spec.assertEqWith s "alice's exile is empty" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
    Spec.assertEqWith s "still two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "each token is still 1/1" (S.powerToughnessOf oid after) (Just (1, 1))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "no +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne oid after) 0) tokens

  -- CR 111.6's pair. Both boards hold the same objects -- Ashiok, its two
  -- Nightmares, and a Goblin Piker card permanent -- and both exile exactly one
  -- battlefield permanent this turn. The ONE thing that differs is whether that
  -- permanent was a card, so an empty filter over the count agrees with the card
  -- leg and contradicts the token leg. The Nightmare is the token victim rather
  -- than a Piker token because a Nightmare is already on the board: nothing about
  -- the victim but its tokenhood is read.
  --
  -- CR 111.7's parenthetical is what makes a token victim observable at all: the
  -- token changes zones, applicable abilities trigger, and only then does it cease
  -- to exist -- so the arrival IS in the log for the count to fold or refuse.
  Spec.it s "CR 111.6 a card put into exile from the battlefield satisfies the token's intervening if" $ do
    ashiok <- S.printingOf s registry "Ashiok, Wicked Manipulator"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (ashiokId, board) = ashiokBoard ashiok [swamp]
        (pikerId, withPiker) = S.addPermanent piker S.alice board
        minted = useLoyaltyAbility S.identityAnswer ashiokMinusTwo ashiok ashiokId withPiker
        exiled = S.runPure S.identityAnswer minted (Event.changeZone pikerId Zone.Exile)
        after = throughBeginningOfCombat exiled
        tokens = S.tokensOf after
    Spec.assertEqWith s "CR 603.4: a card arrived in exile, so both tokens' abilities trigger" (length (filter isAbilityTriggered (S.eventsOf after))) 2
    Spec.assertEqWith s "the Piker is the one thing in alice's exile" (namesIn Zone.Exile S.alice after) [Just (CardName.MkCardName (Text.pack "Goblin Piker"))]
    Spec.assertEqWith s "still two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "each token is 2/2" (S.powerToughnessOf oid after) (Just (2, 2))) tokens

  Spec.it s "CR 111.6 a TOKEN put into exile is not a card, so the intervening if fails" $ do
    ashiok <- S.printingOf s registry "Ashiok, Wicked Manipulator"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (ashiokId, board) = ashiokBoard ashiok [swamp]
        (pikerId, withPiker) = S.addPermanent piker S.alice board
        minted = useLoyaltyAbility S.identityAnswer ashiokMinusTwo ashiok ashiokId withPiker
        victim = Maybe.listToMaybe (S.tokensOf minted)
        exiled = S.runPure S.identityAnswer minted (mapM_ (`Event.changeZone` Zone.Exile) victim)
        after = throughBeginningOfCombat exiled
        tokens = S.tokensOf after
    Spec.assertEqWith s "CR 603.4: no CARD arrived in exile, so neither surviving ability triggers" (length (filter isAbilityTriggered (S.eventsOf after))) 0
    Spec.assertBool s (Maybe.isJust victim) "a Nightmare was there to exile"
    Spec.assertEqWith s "CR 111.7: the exiled token ceased to exist, and the Piker stayed put" (S.onBattlefield pikerId after, length (Game.zoneMembers Zone.Exile S.alice after)) (True, 0)
    Spec.assertEqWith s "one Nightmare left" (length tokens) 1
    mapM_ (\oid -> Spec.assertEqWith s "which is still 1/1" (S.powerToughnessOf oid after) (Just (1, 1))) tokens
    mapM_ (\oid -> Spec.assertEqWith s "with no +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne oid after) 0) tokens

-- Tamiyo, Compleated Sage's -7: "Create Tamiyo's Notebook, a legendary colorless
-- Book artifact token with 'Spells you cast cost {2} less to cast' and '{T}: Draw
-- a card.'" Alice holds her at EIGHT loyalty, so the cost leaves a survivor, with
-- one Island, Divination ({2}{U}) in hand and two cards in her library, in her
-- precombat main phase with priority.
tamiyoNotebookSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
tamiyoNotebookSpec s registry = Spec.describe s "TamiyoNotebook" $ do
  Spec.it s "CR 606.4 / 111.3 the -7 creates a Notebook that discounts spells and draws" $ do
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    ornithopter <- S.printingOf s registry "Ornithopter"
    let stocked = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) (S.landsInPlay island 1) [ornithopter, ornithopter]
        (divinationId, held) = S.addHandCard divination S.alice stocked
        (tamiyoId, placed) = S.addPermanent tamiyo S.alice held
        board =
          (S.addCounter CounterKind.Loyalty 8 tamiyoId placed)
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        ultimate = filter (elem (CostComponent.RemoveLoyaltyFromThis (CostAmount.Fixed 7)) . Cost.Type.components . ActivatedAbility.cost) (Face.activatedAbilities (S.combinedFace tamiyo))
        created = S.runPure S.identityAnswer board (do mapM_ (Activate.activateAbility S.alice tamiyoId) ultimate; Stack.resolveTop)
        notebooks = Set.toList (Set.difference (GameState.battlefield created) (GameState.battlefield board))
        drawn = S.runPure S.identityAnswer created (do Monad.forM_ notebooks (\oid -> mapM_ (Activate.activateAbility S.alice oid) (Projection.abilitiesOf oid created)); Stack.resolveTop)
        shape oid = (Game.isToken oid created, Projection.supertypesOf oid created, Projection.cardTypesOf oid created, Projection.subtypesOf oid created, Projection.colorsOf oid created)
    Spec.assertEqWith
      s
      "CR 111.3: one legendary colorless Book artifact token"
      (fmap shape notebooks)
      [(True, Set.singleton Supertype.Legendary, Set.singleton CardType.Artifact, Set.singleton Subtype.Book, Set.empty)]
    Spec.assertBool s (S.castable S.alice divinationId created) "CR 601.2f: {2}{U} Divination costs {U}, so the one Island casts it"
    Spec.assertEqWith s "{T}: Draw a card -- Divination plus the drawn card" (S.handSize S.alice drawn) 2

-- Tamiyo, Compleated Sage's -X: "Exile target nonland permanent card with mana
-- value X from your graveyard. Create a token that's a copy of that card."
-- Oracle text checked against Scryfall 2026-09-30. Alice holds her at FIVE
-- loyalty with Kalakscion, Hunger Tyrant (mana value 3) and Hill Giant (4) in
-- her graveyard, in her precombat main phase with priority.
tamiyoMinusXBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Printing.Printing, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
tamiyoMinusXBoard s registry = do
  tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
  tyrant <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
  giant <- S.printingOf s registry "Hill Giant"
  let (tamiyoId, placed) = S.addPermanent tamiyo S.alice (Setup.emptyGame S.bothPlayers)
      (tyrantId, g1) = S.addGraveyardCard tyrant S.alice placed
      (_, g2) = S.addGraveyardCard giant S.alice g1
      board =
        (S.addCounter CounterKind.Loyalty 5 tamiyoId g2)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
  pure (tamiyo, tamiyoId, tyrantId, board)

-- Announces X and narrows the target slot to one card by FILTERING the offered
-- set, so CR 608.2b's re-read keeps the recipient.
answerXAt :: Natural -> ObjectId.ObjectId -> Prompt.Prompt r -> r
answerXAt x wanted p = case p of
  Prompt.ChooseX {} -> x
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just wanted) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- Records the bound Prompt.ChooseX carries and announces it.
answerAtBoundX :: Prompt.Prompt r -> State.State [Natural] r
answerAtBoundX p = case p of
  Prompt.ChooseX _ _ _ _ bound -> do
    State.modify' (<> [bound])
    pure bound
  _ -> pure (S.identityAnswer p)

tamiyoMinusXSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tamiyoMinusXSpec s registry = Spec.describe s "TamiyoMinusX" $ do
  Spec.it s "CR 606.4 / 107.3a her -X at X=3 removes three loyalty and copies the mana value 3 card" $ do
    (tamiyo, tamiyoId, tyrantId, board) <- tamiyoMinusXBoard s registry
    let after = useLoyaltyAbility (answerXAt 3 tyrantId) 1 tamiyo tamiyoId board
        minted = Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield board))
    Spec.assertEqWith
      s
      "CR 707.2: one token copy of the exiled card"
      (fmap (\oid -> (Game.isToken oid after, Projection.namesOf oid after)) minted)
      [(True, Set.singleton (CardName.MkCardName (Text.pack "Kalakscion, Hunger Tyrant")))]
    Spec.assertEqWith s "CR 606.4: the announced 3 loyalty counters came off her 5" (S.counterOf CounterKind.Loyalty tamiyoId after) 2
    let plusOneOffered gs = any (\a -> Activatable.activatable S.alice tamiyoId a gs) (abilityAt 0 tamiyo)
    Spec.assertEqWith s "CR 606.3: the -X was her one loyalty activation this turn, so the +1 offered before it is not offered after" (plusOneOffered board, plusOneOffered after) (True, False)
    Spec.assertEqWith s "the card itself was exiled" (namesIn Zone.Exile S.alice after) [Just (CardName.MkCardName (Text.pack "Kalakscion, Hunger Tyrant"))]

  -- CR 606.6 is what bounds the announcement: the X the prompt offers is her
  -- loyalty, since a greater one is a cost she cannot pay.
  Spec.it s "CR 606.6 the ChooseX bound is the loyalty on her" $ do
    (tamiyo, tamiyoId, _, board) <- tamiyoMinusXBoard s registry
    let bounds = State.execState (Engine.runGame answerAtBoundX board (mapM_ (Activate.activateAbility S.alice tamiyoId) (abilityAt 1 tamiyo))) []
    Spec.assertEqWith s "five loyalty bounds X at 5" bounds [5]

-- The Wandering Emperor {2}{W}{W}, loyalty 3: "Flash. As long as The Wandering
-- Emperor entered this turn, you may activate her loyalty abilities any time
-- you could cast an instant. ... -2: Exile target tapped creature. You gain 2
-- life." Her play: flashed in after bob declares an attacker, then the -2 on it
-- in the same step -- bob's turn, a combat step and a stack that just held her,
-- each outside CR 606.3's window.
--
-- The pair differs in whether she entered this turn: the refusal's board has her
-- on the battlefield already, with her loyalty and the same four Plains.
wanderingEmperorSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
wanderingEmperorSpec s registry = Spec.describe s "WanderingEmperor" $ do
  let giant = S.aliasRef "giant"
      plains = fmap (\i -> S.settled ("plains" <> show i) "Plains") [1 .. 4 :: Int]
      setup battlefield hand =
        S.board
          ( (S.battlefield S.alice (plains <> battlefield)) {Seat.hand = Seq.fromList hand}
              NonEmpty.:| [S.battlefield S.bob [S.settled "giant" "Hill Giant"]]
          )
          S.bob
          S.beginningOfCombat
      -- The -2, her third printed ability.
      exiling = Choices.none {Choices.targets = Just [giant]}
      attacking = S.on S.declareAttackers S.bob (S.attack [giant])
      toEndOfCombat =
        let go n = do
              gs <- State.get
              Monad.unless (n <= (0 :: Int) || GameState.phase gs == S.endOfCombat || Maybe.isJust (GameState.result gs)) (Engine.runStep >> go (n - 1))
         in go 8
  Spec.it s "CR 606.3 already on the battlefield, she is not offered on bob's turn" $ do
    let resident = (S.aliased "emperor" (S.permanent "The Wandering Emperor")) {Placement.counters = Map.singleton CounterKind.Loyalty 3}
        -- She is attackable now, so bob also names where the Giant attacks.
        script = S.turn 1 [attacking, S.on S.declareAttackers S.bob (S.attackPlayer S.alice), S.on S.declareAttackers S.alice (S.activateAbility (S.aliasRef "emperor") 2 exiling)]
    built <- S.buildBoardOrFail s registry (setup [resident] [])
    let emperorId = Map.lookup (Label.MkLabel (Text.pack "emperor")) (Staged.objects built)
    Spec.assertEqWith s "setup: she stands with three loyalty" (fmap (\oid -> S.counterOf CounterKind.Loyalty oid (Staged.state built)) emperorId) (Just 3)
    case Scenario.rehearse script built toEndOfCombat of
      Left (ScenarioFailure.MkActionNotOffered _ (Move.Activate {}) _) -> pure ()
      Left failure -> Spec.assertFailure s (S.renderFailure failure)
      Right _ -> Spec.assertFailure s "the -2 was offered anyway"
