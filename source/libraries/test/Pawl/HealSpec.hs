{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 701.69 HEAL -- Effect.Heal's arm in Pawl.Engine.Resolve.Effect.
--
-- Synthetic Mending Light ({W} Instant, "Damage already dealt to target
-- creature is healed.") is the fixture: the keyword action is its whole text,
-- so every assertion below is about rule 701.69a. It is synthetic because the
-- two printings that state the action -- Wolverine, Fierce Fighter (gap #3906) and
-- Pyramids (gap #3907) -- each state it inside a replacement shape pawl cannot yet
-- express.
--
-- THE BOARD SHAPE that makes the pair discriminating. Barkhide Mauler is a 4/4
-- carrying 2 marked damage, and the same Lightning Bolt deals it 3 on both
-- boards: 2 + 3 is CR 704.5g lethal and 0 + 3 is not, so the ONE thing the two
-- boards differ in -- whether the heal resolved first -- is the difference
-- between a creature on the battlefield and one in the graveyard.
module Pawl.HealSpec where

import qualified Data.Set as Set
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

-- alice: a Plains and a Mountain in play, a Barkhide Mauler (4/4) carrying 2
-- marked damage, and both spells in hand. The lands are one of each colour so
-- the two casts cannot compete for the same source.
board ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
board plains mountain mauler light bolt =
  let (_, g1) = S.addPermanent mountain S.alice (S.landsInPlay plains 1)
      (victim, g2) = S.addPermanent mauler S.alice g1
      (heal, g3) = S.addHandCard light S.alice (S.markDamage victim 2 g2)
      (shock, g4) = S.addHandCard bolt S.alice g3
   in (victim, heal, shock, g4)

-- Targets are FILTERED out of the offered set rather than built, so the
-- recipient the engine offered is the one it reads back at CR 608.2b.
atVictim :: ObjectId.ObjectId -> Prompt.Prompt r -> r
atVictim oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.castAnswer p

-- alice casts that spell at the Mauler and it resolves.
castAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castAt victim spell gs =
  let cast = S.runPure (atVictim victim) gs (S.cast S.alice spell)
   in S.runPure (atVictim victim) cast Stack.resolveTop

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Heal" $ do
  Spec.it s "CR 701.69a the marked damage comes off, and the creature survives damage that would otherwise be lethal" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    mauler <- S.printingOf s registry "Barkhide Mauler"
    light <- S.printingOf s registry "Synthetic Mending Light"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (victim, heal, shock, before) = board plains mountain mauler light bolt
        healed = castAt victim heal before
        after = S.settleSba (castAt victim shock healed)
    -- THE gameplay reading, and first: the Mauler is alive after damage that
    -- 2 marked damage would have made lethal (CR 704.5g).
    Spec.assertEqWith s "CR 704.5g the Mauler survives the Bolt" (S.creaturesInPlay S.alice after) 1
    -- And it carries the Bolt's 3 alone: rule 701.69a removed all of the mark,
    -- so the total is 3 rather than 5.
    Spec.assertEqWith s "CR 701.69a only the new damage is marked" (S.damageOf victim after) (Just 3)
  -- The paired negative, differing in ONE thing: the same board, the same
  -- Bolt, the same target -- but the heal is never cast.
  Spec.it s "CR 704.5g without the heal the same Bolt is lethal" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    mauler <- S.printingOf s registry "Barkhide Mauler"
    light <- S.printingOf s registry "Synthetic Mending Light"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (victim, _, shock, before) = board plains mountain mauler light bolt
        after = S.settleSba (castAt victim shock before)
    Spec.assertEqWith s "CR 704.5g the Mauler dies" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "and it carried 2 marked damage before the Bolt" (S.damageOf victim before) (Just 2)
