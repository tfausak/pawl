{-# LANGUAGE GADTs #-}

-- Covers: CR 701.36 POPULATE -- Pawl.Engine.Populate and Effect.Populate's arm
-- in Pawl.Engine.Resolve.Effect.
--
-- Wake the Reflections ({W} Sorcery, "Populate.") is the fixture: the keyword
-- action is its whole text, so every assertion below is about rule 701.36a.
--
-- THE BOARD SHAPE that makes the cases discriminating. Rule 701.36a's candidate
-- has to be a creature, a token, and one this player controls, and the board
-- carries a counterexample to each: alice controls a Goblin Piker TOKEN (the
-- candidate), a NONTOKEN Hill Giant, and bob controls an Ornithopter token of
-- his own. A populate that read any one of the three qualities wrongly copies
-- something the assertions below name by printing.
module Pawl.PopulateSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- alice: two Plains and Wake the Reflections in hand, a nontoken Hill Giant, and
-- -- when `withToken` -- a Goblin Piker token. bob: an Ornithopter token.
--
-- The `withToken` flag is the ONE thing the paired boards below differ in, so
-- the negative case fails for rule 701.36b and not for the mana, the timing or
-- the spell.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, GameState.GameState)
board plains wake piker giant flier withToken =
  let (_, g1) = S.addPermanent giant S.alice (S.landsInPlay plains 2)
      g2 = if withToken then snd (S.addToken (Printing.card piker) S.alice g1) else g1
      (_, g3) = S.addToken (Printing.card flier) S.bob g2
      (spell, g4) = S.addHandCard wake S.alice g3
   in (spell, g4)

-- alice casts Wake the Reflections and it resolves.
populated :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
populated spell gs =
  let cast = S.runPure S.castAnswer gs (S.cast S.alice spell)
   in S.runPure S.castAnswer cast Stack.resolveTop

-- `S.castAnswer`, but answering the populate's Prompt.ChoosePermanent with the
-- LAST token offered. Pinned by position, so a populate that ignored the answer
-- and took the offered set's front copies the other token.
choosingLast :: Prompt.Prompt r -> r
choosingLast p = case p of
  Prompt.ChoosePermanent _ _ _ candidates -> NonEmpty.last candidates
  _ -> S.castAnswer p

-- How many of this player's battlefield permanents are tokens (CR 111.6).
tokensOf :: GameState.GameState -> Int
tokensOf gs = length (filter (`Game.isToken` gs) (Game.zoneMembers Zone.Battlefield S.alice gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Populate" $ do
  Spec.it s "CR 701.36a the creature token is copied, and neither the nontoken beside it nor the opponent's token is" $ do
    plains <- S.printingOf s registry "Plains"
    wake <- S.printingOf s registry "Wake the Reflections"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    flier <- S.printingOf s registry "Ornithopter"
    let (spell, before) = board plains wake piker giant flier True
        after = populated spell before
    -- THE gameplay reading, and first: a second Goblin Piker is on the
    -- battlefield, where one stood before the spell.
    Spec.assertEqWith s "CR 701.36a a copy of the creature token was created" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 2
    Spec.assertEqWith s "and there was one before the spell" (S.countOnBattlefieldByName (S.printingName piker) S.alice before) 1
    -- CR 111.1: what rule 701.36a creates is a token, not a card.
    Spec.assertEqWith s "CR 111.1 both of alice's Pikers are tokens" (tokensOf after) 2
    -- The nontoken on the SAME board: rule 701.36a's "token" is a real gate.
    Spec.assertEqWith s "the nontoken Hill Giant was not copied" (S.countOnBattlefieldByName (S.printingName giant) S.alice after) 1
    -- bob's token on the SAME board: rule 701.36a's "you control" is too.
    Spec.assertEqWith s "and neither was the opponent's creature token" (S.countOnBattlefieldByName (S.printingName flier) S.bob after) 1
  -- Two creature tokens of different printings, so WHICH one was copied is
  -- readable by name. The Ornithopter token is minted after the Piker, so it is
  -- the last of the ascending candidates and the answer below names it.
  Spec.it s "CR 701.36a with two creature tokens the populating player chooses which is copied" $ do
    plains <- S.printingOf s registry "Plains"
    wake <- S.printingOf s registry "Wake the Reflections"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    flier <- S.printingOf s registry "Ornithopter"
    let (spell, bare) = board plains wake piker giant flier True
        (_, before) = S.addToken (Printing.card flier) S.alice bare
        cast = S.runPure choosingLast before (S.cast S.alice spell)
        after = S.runPure choosingLast cast Stack.resolveTop
    Spec.assertEqWith s "CR 701.36a the chosen Ornithopter token was copied" (S.countOnBattlefieldByName (S.printingName flier) S.alice after) 2
    Spec.assertEqWith s "and the Goblin Piker token beside it was not" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
