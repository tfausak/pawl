{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 712.4's meld cards: Pawl.Types.Layout's Meld arm and the
-- Pawl.Engine.Card functions that read it. A meld card carries its front face
-- alone (CR 712.4b), so what this spec pins first is that the front face plays
-- exactly as a one-faced card's does -- the mana ability and the targeted grant
-- printed on Hanweir Battlements, reached through Pawl.Engine.Cost.tapForMana
-- and Pawl.Engine.Activate.activateAbility.
--
-- Hanweir Battlements and Hanweir Garrison are the pool's only meld pair, and
-- the Battlements is the half CR 712.4a puts the melding ability on.
module Pawl.MeldSpec where

import qualified Data.Set as Set
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

-- The offered set FILTERED rather than a hand-built recipient, so CR 608.2b's
-- re-read at resolution cannot drop a target the engine never offered.
aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    S.preferring (\r -> Recipient.objectOf r == Just oid) sets
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Meld" $ do
  -- CR 712.8's second sentence gives a meld card's front face "its own set of
  -- characteristics", and CR 712.8d makes those the live ones here: "While a
  -- double-faced permanent has its front face up, it has only the characteristics
  -- of its front face." CR 712.4b is why there is nothing else to show -- the
  -- back face determines nothing off a melded permanent, so pawl prints each half
  -- of the pair as its front face alone. The printed "{T}: Add {C}" is an
  -- ordinary activated mana ability (CR 605.1a).
  Spec.it s "CR 712.8d a meld card on the battlefield has its front face's characteristics: Hanweir Battlements taps for {C}" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    let (battlementsId, gs) = S.addCreature battlements S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer gs (Cost.tapForMana battlementsId)
    Spec.assertEqWith
      s
      "pool"
      (Game.poolOf S.alice after)
      (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}])
    Spec.assertEqWith s "and the land is tapped" (S.tappedCount S.alice after) 1
  -- The same front face's second printed ability, which targets: CR 712.8d again,
  -- and the case that shows the front face's TEXT is live rather than just its
  -- type line. The Mountain is what pays the {R}.
  Spec.it s "CR 712.8d Hanweir Battlements' {R}, {T} grants haste to the creature it targets" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (battlementsId, g1) = S.addCreature battlements S.alice base
        (_, g2) = S.addCreature mountain S.alice g1
        (pikerId, g3) = S.addCreature piker S.alice g2
        ready = g3 {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace battlements) of
      _ : haste : _ -> do
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste pikerId ready)) "the Piker starts without haste"
        let after = S.runPure (aimedAt pikerId) ready (do Activate.activateAbility S.alice battlementsId haste; Stack.resolveTop)
        Spec.assertBool s (Projection.hasKeyword Keyword.Haste pikerId after) "CR 702.10: the targeted creature gains haste"
      _ -> Spec.assertFailure s "Hanweir Battlements should print two activated abilities"
