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
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId

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
