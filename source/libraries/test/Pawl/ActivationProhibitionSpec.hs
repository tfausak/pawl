{-# LANGUAGE GADTs #-}

-- Covers: CR 602.2 / CR 101.2's ACTIVATION PROHIBITION --
-- Pawl.Types.ActivationProhibition, the answer Pawl.Engine.ActivationProhibition
-- gives, and the two windows that read it (Pawl.Engine.Activate.activatableGiven
-- for CR 602.2, Pawl.Engine.Cost's manaActivations for CR 605.3a). CR 605.1a's
-- division is the second axis, since one of the two producers writes it into the
-- printed sentence.
--
-- TWO AURAS, and the pair is the whole point. Arrest's "its activated abilities
-- can't be activated" names no kind; Realmbreaker's Grasp's "unless they're mana
-- abilities" names one by exclusion. Every case below is asked on both, so a
-- gate that ignored the kind would answer one of them wrongly -- and the mana
-- half is where it would: an implementation reading Arrest's row for
-- Realmbreaker's would take the mana ability too.
--
-- THE BOARD SHAPE that makes every case discriminating: bob controls TWO of one
-- printing, identical in every respect, and alice's Aura is attached to one of
-- them. So the victim failing while the twin beside it succeeds is the printed
-- sentence and nothing else -- not the phase, not the controller, and not a
-- board on which nobody could have activated anything.
--
-- TWO PRINTINGS for the two windows, DetainSpec's pair and for its reason:
-- Llanowar Elves' "{T}: Add {G}" is a mana ability, which CR 605.3b keeps off
-- the stack and Pawl.Engine.Activate refuses on every board, so it can only be
-- observed through the mana window; Prodigal Sorcerer's "{T}: deals 1 damage" is
-- the ordinary one.
module Pawl.ActivationProhibitionSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActiveActivationProhibition as ActiveActivationProhibition
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TapState as TapState

-- The permanents an activation action names, of either shape: CR 602.2's
-- ordinary activation and CR 605.3a's mana one go on one list, so no case can
-- pass because it looked at the wrong window. DetainSpec's helper, which the
-- same two windows make the right question here.
activatableIds :: [A.Action] -> [ObjectId.ObjectId]
activatableIds =
  Maybe.mapMaybe
    ( \a -> case a of
        A.Activate oid _ -> Just oid
        A.ActivateManaAbility oid -> Just oid
        _ -> Nothing
    )

-- Bob's two twins of one printing, with alice's Aura attached to the first.
-- Returns (victim, twin, state).
enchanted :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> String -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
enchanted s registry auraName printingName = do
  aura <- S.printingOf s registry auraName
  printing <- S.printingOf s registry printingName
  let (victim, g1) = S.addPermanent printing S.bob (Setup.emptyGame S.bothPlayers)
      (twin, g2) = S.addPermanent printing S.bob g1
      (auraId, g3) = S.addPermanent aura S.alice g2
  pure (victim, twin, S.attach auraId victim g3)

-- The same two twins with the Aura never placed. The paired control for every
-- case below: same seats, same creatures, same phase -- only the printed
-- sentence is missing.
unenchanted :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
unenchanted s registry printingName = do
  printing <- S.printingOf s registry printingName
  let (victim, g1) = S.addPermanent printing S.bob (Setup.emptyGame S.bothPlayers)
      (twin, g2) = S.addPermanent printing S.bob g1
  pure (victim, twin, g2)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "ActivationProhibition" $ do
  arrestSpec s registry
  manaAbilitySpec s registry
  unenchantedSpec s registry
  storedSpec s registry

-- CR 602.2's window, which both producers close.
arrestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
arrestSpec s registry = Spec.describe s "NonManaAbility" $ do
  Spec.it s "CR 602.2/101.2 whole cards: an Arrested creature's ability is not offered, and its twin's is" $ do
    (victim, twin, gs) <- enchanted s registry "Arrest" "Prodigal Sorcerer"
    let offered = activatableIds (Action.legalActions S.bob gs)
    Spec.assertBool s (notElem victim offered) "the enchanted Prodigal Sorcerer's ability is withheld"
    Spec.assertEqWith s "and its twin's is the one offer left" offered [twin]
  -- Realmbreaker's Grasp exempts a MANA ability and nothing else, so its answer
  -- here is Arrest's: this is the half of CR 605.1a's division it still forbids.
  Spec.it s "CR 602.2/101.2 whole cards: Realmbreaker's Grasp withholds the same non-mana ability" $ do
    (victim, twin, gs) <- enchanted s registry "Realmbreaker's Grasp" "Prodigal Sorcerer"
    let offered = activatableIds (Action.legalActions S.bob gs)
    Spec.assertBool s (notElem victim offered) "the enchanted Prodigal Sorcerer's ability is withheld"
    Spec.assertEqWith s "and its twin's is the one offer left" offered [twin]

-- CR 605.3a's windows, where the two producers part company. THE PAIR IS THE
-- UNIT: two boards differing in exactly one thing -- which Aura is on the victim
-- -- so neither answer can be passing for a reason about the Elves.
manaAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manaAbilitySpec s registry = Spec.describe s "ManaAbility" $ do
  Spec.it s "CR 605.3a/101.2 whole cards: an Arrested Llanowar Elves is not a mana source, and its twin is" $ do
    (victim, twin, gs) <- enchanted s registry "Arrest" "Llanowar Elves"
    let offered = activatableIds (Action.legalActions S.bob gs)
    Spec.assertBool s (notElem victim offered) "Arrest names no kind, so the mana ability goes too"
    Spec.assertEqWith s "and its twin is the one offer left" offered [twin]
  Spec.it s "CR 605.1a whole cards: Realmbreaker's Grasp leaves the same Elves' mana ability alone" $ do
    (victim, twin, gs) <- enchanted s registry "Realmbreaker's Grasp" "Llanowar Elves"
    Spec.assertEqWith
      s
      "both Elves are still mana sources"
      (Set.fromList (activatableIds (Action.legalActions S.bob gs)))
      (Set.fromList [victim, twin])

-- The controls: with no Aura on the board both twins act, so nothing above can
-- be passing because bob could never have activated anything.
unenchantedSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
unenchantedSpec s registry = Spec.describe s "Unenchanted" $ do
  Spec.it s "with nothing enchanted both twins' abilities are offered" $ do
    (victim, twin, sorcerers) <- unenchanted s registry "Prodigal Sorcerer"
    (elfVictim, elfTwin, elves) <- unenchanted s registry "Llanowar Elves"
    Spec.assertEqWith
      s
      "both Prodigal Sorcerers"
      (Set.fromList (activatableIds (Action.legalActions S.bob sorcerers)))
      (Set.fromList [victim, twin])
    Spec.assertEqWith
      s
      "both Llanowar Elves"
      (Set.fromList (activatableIds (Action.legalActions S.bob elves)))
      (Set.fromList [elfVictim, elfTwin])

-- CR 602.2 / 611.1 / 613.11: the group above's STORED counterpart -- a
-- prohibition a resolution leaves behind, which no printed row can state once
-- its source has stopped saying it. Deadlock Trap's "{T}, Pay {E}: Tap target
-- creature or planeswalker. Its activated abilities can't be activated this
-- turn" (checked against Scryfall, 2026-09-05) is the pool's printing; the row
-- lands in GameState.activationProhibitions and
-- Pawl.Engine.ActivationProhibition.cantActivate is what reads it.
--
-- Every clause of the card is transcribed, the entry rewrite and the energy
-- trigger included. The sentence's second producer, Dovin Baan's "until your
-- next turn", is left for a card of its own; its duration arm is the one
-- Chronomantic Escape already exercises.
--
-- UTHDEN TROLL and not the group above's Prodigal Sorcerer, and the choice is
-- load-bearing: the Trap TAPS what it names, so a victim whose ability costs
-- {T} would be silent whatever the prohibition did. The Troll's "{R}:
-- Regenerate" pays mana and nothing else, so the tap cannot answer for the
-- prohibition.
--
-- THE PAIR: the same activation is made on both boards, for the same tap and
-- the same {E}, and they differ only in which of bob's two identical Trolls the
-- one target slot named. A green control leg therefore cannot come from an
-- unpaid cost, a missing Mountain, or a board on which bob could never have
-- activated anything.
storedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storedSpec s registry = Spec.describe s "Stored" $ do
  Spec.it s "CR 602.2 whole cards: the Troll Deadlock Trap named cannot be activated, and its twin can" $ do
    (victim, twin, resolved) <- deadlockResolved s registry "Uthden Troll" const
    let offered = twinOffers victim twin resolved
    Spec.assertBool s (notElem victim offered) "the named Troll's regeneration is withheld"
    Spec.assertEqWith s "and its twin's is the one Troll offer left" offered [twin]
    Spec.assertEqWith s "one prohibition was stored, over the Troll named" (fmap ActiveActivationProhibition.object (GameState.activationProhibitions resolved)) [victim]
    Spec.assertEqWith s "and the ability's other clause tapped that same Troll" (fmap Object.tapped (Game.lookupObject victim resolved)) (Just TapState.Tapped)
  -- The pair's other half: the same activation, aimed at the twin.
  Spec.it s "CR 602.2 aimed at the twin, the first Troll is the one still offered" $ do
    (victim, twin, resolved) <- deadlockResolved s registry "Uthden Troll" (\_ t -> t)
    let offered = twinOffers victim twin resolved
    Spec.assertBool s (notElem twin offered) "the twin's regeneration is withheld instead"
    Spec.assertEqWith s "and the first Troll's is the one Troll offer left" offered [victim]
  -- CR 514.2 / 611.2a: "this turn" arms Expiry.AtCleanup, so the cleanup sweep
  -- drops the row and the ability is offered again. Through the sweep directly,
  -- which is the narrowest path that shows it.
  Spec.it s "CR 514.2 the prohibition ends at cleanup" $ do
    (victim, twin, resolved) <- deadlockResolved s registry "Uthden Troll" const
    let swept = Expiry.dropAtCleanup resolved
    Spec.assertEqWith
      s
      "both Trolls may be activated once the turn's cleanup has run"
      (Set.fromList (twinOffers victim twin swept))
      (Set.fromList [victim, twin])
    Spec.assertEqWith s "with nothing left stored" (GameState.activationProhibitions swept) []
  -- CR 605.3a's window, which the STORED row closes too: Pawl.Types.ForbidActivation
  -- carries no CR 605.1a kind, so a mana ability goes the way every other
  -- activated ability does. Treasonous Ogre's "Pay 3 life: Add {R}" is the pool's
  -- mana ability that costs no tap, which the Trap's own tap would otherwise
  -- answer for. The pair is the same activation aimed at the other Ogre.
  Spec.it s "CR 605.3a whole cards: the Ogre Deadlock Trap named is no longer a mana source, and its twin is" $ do
    (victim, twin, resolved) <- deadlockResolved s registry "Treasonous Ogre" const
    Spec.assertEqWith s "only the twin is offered as a mana source" (twinOffers victim twin resolved) [twin]
  Spec.it s "CR 605.3a aimed at the twin, the first Ogre is the mana source left" $ do
    (victim, twin, resolved) <- deadlockResolved s registry "Treasonous Ogre" (\_ t -> t)
    Spec.assertEqWith s "only the first Ogre is offered as a mana source" (twinOffers victim twin resolved) [victim]

-- The two twins bob may activate right now, narrowed off `activatableIds` so
-- bob's Mountain -- which CR 605.3a offers him on every board here -- cannot
-- stand in for either of them.
twinOffers :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
twinOffers victim twin gs = filter (\oid -> oid == victim || oid == twin) (activatableIds (Action.legalActions S.bob gs))

-- Alice's Deadlock Trap, activated once and resolved, over a board of bob's two
-- identical Uthden Trolls and the Mountain that pays for either's ability.
-- `pick` chooses which Troll the one target slot names, and is the only thing
-- the boards differ in. The two energy counters are the ones the Trap's own
-- entry trigger would have given alice, placed directly because S.addPermanent
-- puts the artifact onto the battlefield rather than resolving it there.
deadlockResolved ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  String ->
  (ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId) ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
deadlockResolved s registry printingName pick = do
  trap <- S.printingOf s registry "Deadlock Trap"
  troll <- S.printingOf s registry printingName
  mountain <- S.printingOf s registry "Mountain"
  let (victim, withVictim) = S.addPermanent troll S.bob (S.landsFor mountain S.bob 1 (Setup.emptyGame S.bothPlayers))
      (twin, withTwin) = S.addPermanent troll S.bob withVictim
      (trapId, withTrap) = S.addPermanent trap S.alice withTwin
      board = mainPhaseForAlice (S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice withTrap)
      abilities = Activate.abilitiesFor trapId board
      resolved = case abilities of
        [ability] -> S.runPure (namingTarget (pick victim twin)) board (Activate.activateAbility S.alice trapId ability >> Stack.resolveTop)
        _ -> board
  Spec.assertEqWith s "Deadlock Trap states exactly one activated ability" (length abilities) 1
  pure (victim, twin, resolved)

-- Aim the Trap's one target slot at this permanent, PINNED by filtering the
-- offered set rather than built from the id: a hand-built recipient is a
-- different one from the pool's, and CR 608.2b's re-read drops it silently.
-- Filtering also stops the answerer repairing a mutation by finding whatever is
-- still legal.
namingTarget :: ObjectId.ObjectId -> Prompt.Prompt r -> r
namingTarget oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) sets
  _ -> S.identityAnswer p

-- Alice active with priority in her precombat main phase, which is when the
-- activation above is made.
mainPhaseForAlice :: GameState.GameState -> GameState.GameState
mainPhaseForAlice gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.PrecombatMain,
      GameState.priority = Just S.alice
    }
