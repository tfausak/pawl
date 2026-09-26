{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 701.69 HEAL -- Effect.Heal's arm in Pawl.Engine.Resolve.Effect,
-- and Pawl.Types.DestructionRewrite's Heal arm in Pawl.Engine.Event.
--
-- Synthetic Mending Light ({W} Instant, "Damage already dealt to target
-- creature is healed.") is the fixture: the keyword action is its whole text,
-- so every assertion below is about rule 701.69a. It is synthetic because
-- Wolverine, Fierce Fighter (gap #3906) states the action inside a replacement
-- shape pawl cannot yet express. Pyramids, the destruction-replacement printing,
-- has its own group below.
--
-- THE BOARD SHAPE that makes the pair discriminating. Barkhide Mauler is a 4/4
-- carrying 2 marked damage, and the same Lightning Bolt deals it 3 on both
-- boards: 2 + 3 is CR 704.5g lethal and 0 + 3 is not, so the ONE thing the two
-- boards differ in -- whether the heal resolved first -- is the difference
-- between a creature on the battlefield and one in the graveyard.
module Pawl.HealSpec where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TapState as TapState

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

-- alice: two Plains to pay Pyramids' {2}, and Pyramids. bob: Dryad Arbor, a 1/1
-- land creature, so the shield's "target land" can carry marked damage, and
-- Wild Growth on it for the first mode. bob's rather than alice's, so paying
-- the {2} cannot tap the Arbor for mana and spoil its tap state.
pyramidsBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
pyramidsBoard plains pyramids arbor growth =
  let (source, g1) = S.addPermanent pyramids S.alice (S.landsInPlay plains 2)
      (land, g2) = S.addPermanent arbor S.bob g1
      (aura, g3) = S.addPermanent growth S.bob g2
   in (source, land, aura, (S.attach aura land g3) {GameState.priority = Just S.alice})

-- Activate Pyramids' one ability in mode `mode` at `target` and resolve it.
pyramidsMode :: Printing.Printing -> ObjectId.ObjectId -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
pyramidsMode pyramids source mode target gs = case Face.activatedAbilities (S.combinedFace pyramids) of
  [] -> gs
  ability : _ ->
    let answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseModes {} -> Seq.singleton (ModeIndex.MkModeIndex mode)
          _ -> atVictim target p
     in S.runPure answer gs (Activate.activateAbility S.alice source ability >> Stack.resolveTop)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Heal" $ do
  -- Pyramids' second mode. The Arbor is handed lethal damage AFTER the shield is
  -- up, and the check runs twice (CR 704.3 repeats it), so a shield that stopped
  -- the destruction without removing the damage loses the Arbor on the second
  -- pass. Its tap state is what parts the heal from regeneration (CR 701.19a).
  Spec.it s "CR 614.1a / 701.69a whole card: Pyramids' shield heals the land instead of letting it be destroyed" $ do
    plains <- S.printingOf s registry "Plains"
    pyramids <- S.printingOf s registry "Pyramids"
    arbor <- S.printingOf s registry "Dryad Arbor"
    growth <- S.printingOf s registry "Wild Growth"
    let (source, land, _, before) = pyramidsBoard plains pyramids arbor growth
        shielded = pyramidsMode pyramids source 1 land before
        after = S.settleSba (S.settleSba (S.markDamage land 1 shielded))
    Spec.assertBool s (Set.member land (GameState.battlefield after)) "CR 704.5g the Arbor survives lethal damage"
    Spec.assertEqWith s "CR 701.69a with its damage removed and, unlike a regeneration, untapped" (fmap (\o -> (Object.damage o, Object.tapped o)) (Game.lookupObject land after)) (Just (0, TapState.Untapped))
  -- Pyramids' first mode: "Destroy target Aura attached to a land".
  Spec.it s "CR 701.8a Pyramids' first mode destroys the Aura on a land" $ do
    plains <- S.printingOf s registry "Plains"
    pyramids <- S.printingOf s registry "Pyramids"
    arbor <- S.printingOf s registry "Dryad Arbor"
    growth <- S.printingOf s registry "Wild Growth"
    let (source, _, aura, before) = pyramidsBoard plains pyramids arbor growth
        after = pyramidsMode pyramids source 0 aura before
    Spec.assertBool s (not (Set.member aura (GameState.battlefield after))) "Wild Growth is destroyed"
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
