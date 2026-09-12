{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 700.5 DEVOTION -- Pawl.Types.Devotion, Pawl.Types.Quantity's
-- Devotion arm, Pawl.Engine.Quantity's devotionOf and the Pawl.Engine.Filter
-- View field it reads (manaCost), which Pawl.Engine.Projection.View and
-- Pawl.Engine.Count fill.
--
-- Fanatic of Mogis ({3}{R}, "When this creature enters, it deals damage to each
-- opponent equal to your devotion to red") is the fixture: the cheapest printing
-- that reads devotion, since the number is the whole payload of an ordinary entry
-- trigger and nothing else about the card is new.
--
-- A TEST-LOCAL ANSWERER rather than Pawl.Support's Board harness, because the
-- board turns on CR 614.12a's as-enters copy choice and the harness has no
-- vocabulary for Prompt.ChooseCopyTarget. The answer is PINNED to one named
-- permanent so a mutation cannot be repaired by an answerer finding another
-- legal source.
--
-- TWO BOARDS, differing in where the red symbols come from:
--
--   * `deals one` -- alice's Fanatic is the only permanent she controls with a
--     coloured symbol. Her Mountain contributes nothing (CR 202.1b: a land has
--     no mana cost) and bob's Excruciator ({6}{R}{R}) contributes nothing
--     because CR 700.5 counts the permanents THAT PLAYER controls. Counting
--     bob's would read 3 and counting no symbols at all would read 0, so the
--     one number tells three readings apart.
--   * `deals two` -- the same board with a Clone resolving as a copy of the
--     Fanatic. Clone's PRINTED cost is {3}{U}, which carries no red symbol at
--     all, so the copied {R} is the only thing that can make this 2: CR 707.2
--     makes mana cost copiable and CR 700.5 counts the cost the permanent HAS.
module Pawl.DevotionSpec where

import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Prompt as Prompt

-- Resolve the stack top and run the settle boundary, so the permanent enters and
-- the entry trigger it raises is on the stack for the next call.
resolveAndSettle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAndSettle answer gs =
  snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- CR 614.12a's as-enters copy choice, PINNED to one named permanent: an answerer
-- that searched for something legal would find the other creature on the board
-- after a mutation and keep the case green.
copyNamed :: ObjectId -> Prompt.Prompt r -> r
copyNamed wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Devotion" $ do
  Spec.it s "CR 700.5: Fanatic of Mogis counts only the symbols alice controls" $ do
    fanatic <- S.printingOf s registry "Fanatic of Mogis"
    mountain <- S.printingOf s registry "Mountain"
    excruciator <- S.printingOf s registry "Excruciator"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withOpponent) = S.addPermanent excruciator S.bob gs0
        (_, withLand) = S.addPermanent mountain S.alice withOpponent
        (_, staged) = S.spellOnStack fanatic S.alice withLand
        entered = resolveAndSettle S.identityAnswer staged
        after = resolveAndSettle S.identityAnswer entered
    Spec.assertEqWith s "CR 700.5: alice's devotion to red is her Fanatic's own {R}, so bob loses 1" (S.lifeOf S.bob after) (Just 19)

  Spec.it s "CR 700.5 / 707.2: a Clone of the Fanatic contributes its COPIED {R}" $ do
    fanatic <- S.printingOf s registry "Fanatic of Mogis"
    mountain <- S.printingOf s registry "Mountain"
    excruciator <- S.printingOf s registry "Excruciator"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withOpponent) = S.addPermanent excruciator S.bob gs0
        (_, withLand) = S.addPermanent mountain S.alice withOpponent
        (fanaticId, withFanatic) = S.addPermanent fanatic S.alice withLand
        (_, staged) = S.spellOnStack clone S.alice withFanatic
        entered = resolveAndSettle (copyNamed fanaticId) staged
        after = resolveAndSettle (copyNamed fanaticId) entered
    Spec.assertEqWith s "CR 707.2: the Clone's copied {3}{R} counts beside the Fanatic's, so bob loses 2" (S.lifeOf S.bob after) (Just 18)
