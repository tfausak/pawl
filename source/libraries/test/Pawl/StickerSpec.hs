{-# LANGUAGE GADTs #-}

-- Covers CR 107.17's ticket counters, CR 103.2d's sheet draw
-- (Pawl.Engine.Setup), and CR 123's stickers: Pawl.Engine.Sticker,
-- Effect.PutSticker (Pawl.Engine.Resolve.Effect), the CR 123.5 write-back in
-- Pawl.Engine.Event, Filter.HasSticker and Filter.Stickered, and
-- TriggerCondition.PlacesSticker.
module Pawl.StickerSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Prompt as Prompt

-- Alice active with priority in her precombat main phase.
mainPhaseForAlice :: GameState.GameState -> GameState.GameState
mainPhaseForAlice gs = gs {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}

-- Aetheric Amplifier's second mode, "each kind of counter you have".
secondMode :: Prompt.Prompt r -> r
secondMode p = case p of
  Prompt.ChooseModes {} -> Seq.singleton (ModeIndex.MkModeIndex 1)
  _ -> S.identityAnswer p

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Sticker" $ do
  Spec.it s "CR 107.17 Blorbian Buddy's {G}, {T} gets alice a ticket counter" $ do
    buddy <- S.printingOf s registry "Blorbian Buddy"
    forest <- S.printingOf s registry "Forest"
    let (buddyId, board) = S.addPermanent buddy S.alice (mainPhaseForAlice (S.landsFor forest S.alice 1 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        after = case Activatable.abilitiesFor buddyId board of
          [ability] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice buddyId ability >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 107.17 alice has one ticket counter" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 1
  Spec.it s "CR 107.17 Ticket Turbotubes' {3}, {T} gets alice a ticket counter" $ do
    tubes <- S.printingOf s registry "Ticket Turbotubes"
    mountain <- S.printingOf s registry "Mountain"
    let (tubesId, board) = S.addPermanent tubes S.alice (mainPhaseForAlice (S.landsFor mountain S.alice 3 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        after = case Activatable.abilitiesFor tubesId board of
          [_, tickets] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice tubesId tickets >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 107.17 alice has one ticket counter" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 1
  -- Three, so a doubling reads six and a no-op three.
  Spec.it s "CR 701.10e Aetheric Amplifier doubles alice's ticket counters" $ do
    amplifier <- S.printingOf s registry "Aetheric Amplifier"
    mountain <- S.printingOf s registry "Mountain"
    let (ampId, board) = S.addPermanent amplifier S.alice (mainPhaseForAlice (S.addPlayerCounter PlayerCounterKind.Ticket 3 S.alice (S.landsFor mountain S.alice 4 (Setup.gameWith GameSettings.plain S.bothPlayers))))
        after = case Activatable.abilitiesFor ampId board of
          [_, doubling] -> S.runPure secondMode board (Activate.activateAbility S.alice ampId doubling >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 701.10e alice has six ticket counters" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 6
