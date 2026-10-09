{-# LANGUAGE GADTs #-}

-- Covers CR 107.17's ticket counters, CR 103.2d's sheet draw
-- (Pawl.Engine.Setup), and CR 123's stickers: Pawl.Engine.Sticker,
-- Effect.PutSticker (Pawl.Engine.Resolve.Effect), the CR 123.5 write-back in
-- Pawl.Engine.Event, Filter.HasSticker and Filter.Stickered, and
-- TriggerCondition.PlacesSticker.
module Pawl.StickerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Oracle as Oracle
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.StickerSheets as StickerSheets
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilitySticker as AbilitySticker
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.StickerSheet as StickerSheet

-- Alice active with priority in her precombat main phase.
mainPhaseForAlice :: GameState.GameState -> GameState.GameState
mainPhaseForAlice gs = gs {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}

-- Aetheric Amplifier's second mode, "each kind of counter you have".
secondMode :: Prompt.Prompt r -> r
secondMode p = case p of
  Prompt.ChooseModes {} -> Seq.singleton (ModeIndex.MkModeIndex 1)
  _ -> S.identityAnswer p

-- The four committed sheets, in this order; fewer if one is missing, which
-- each case's first assertion catches.
committedSheets :: IO [StickerSheet.StickerSheet]
committedSheets = do
  root <- StickerSheets.defaultRoot
  loaded <- StickerSheets.loadRoot root
  let byName = Map.fromList [(StickerSheet.name sheet, sheet) | (_, Right sheet) <- loaded]
  pure (Maybe.mapMaybe (\n -> Map.lookup (Text.pack n) byName) ["Night Brushwagg Ringmaster", "Slimy Burrito Illusion", "Contortionist Otter Storm", "Ancestral Hot Dog Minotaur"])

-- One Oracle line, "{TK}{TK} — rest", as its ticket count and its rest.
ticketLine :: Text.Text -> (Natural, Text.Text)
ticketLine line =
  let (cost, rest) = Text.breakOn (Text.pack " \8212 ") line
   in (Natural.length (drop 1 (Text.splitOn (Text.pack "{TK}") cost)), Text.drop 3 rest)

-- Keeps every hand and records each CR 103.2d draw's candidates, answering
-- with the LAST, pinned by position.
sheetDraws :: Prompt.Prompt r -> State.State [[Natural]] r
sheetDraws p = case p of
  Prompt.RandomStickerSheet slots -> do
    State.modify' (NonEmpty.toList slots :)
    pure (NonEmpty.last slots)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- Setup.newGame over this matchup, with what sheetDraws was asked, in order.
startedWith :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [[Natural]])
startedWith matchup =
  let ((_, gs), asked) = State.runState (Engine.runGame sheetDraws (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, reverse asked)

chosenOf :: PlayerId.PlayerId -> GameState.GameState -> Set.Set Natural
chosenOf pid gs = foldMap Player.chosenStickerSheets (Map.lookup pid (GameState.players gs))

withSheetsDeck :: [StickerSheet.StickerSheet] -> Deck.Deck -> Deck.Deck
withSheetsDeck sheets deck = deck {Deck.stickerSheets = Seq.fromList sheets}

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
  -- CR 123.2: a sheet is three name, three art, two ability and two P/T
  -- stickers, and each structured field says what MTGJSON's text says.
  Spec.it s "CR 123.2 every sticker sheet says what its Oracle text says" $ do
    root <- StickerSheets.defaultRoot
    loaded <- StickerSheets.loadRoot root
    Spec.assertBool s (length loaded >= 4) "at least four sheets are committed"
    let offends (path, result) = case result of
          Left reason -> Just (path <> ": " <> Text.unpack reason)
          Right sheet ->
            let lines_ = foldMap Text.lines (StickerSheet.oracleText sheet)
                abilityLines = fmap ticketLine (take 2 lines_)
                ptLines = fmap ticketLine (drop 2 lines_)
                abilityStickers = Foldable.toList (StickerSheet.abilities sheet)
                ptStickers = Foldable.toList (StickerSheet.powerToughness sheet)
                ptText p = Text.pack (show (PowerToughnessSticker.power p) <> "/" <> show (PowerToughnessSticker.toughness p))
                keywordsAgree a (_, rest) =
                  let printed = [Oracle.printed k | k <- Map.keys (AbilitySticker.keywords a)]
                   in not (null (AbilitySticker.abilities a)) || any Maybe.isNothing printed || List.sort (Oracle.normalise rest) == List.sort (fmap Text.toLower (Maybe.catMaybes printed))
                checks =
                  [ ("file named for the sheet", Slug.unwrap (Slug.fromText (StickerSheet.name sheet)) <> Text.pack ".json" == Text.pack (reverse (takeWhile (/= '/') (reverse path)))),
                    ("names join to the name", Text.unwords (Foldable.toList (StickerSheet.names sheet)) == StickerSheet.name sheet),
                    ("three name, three art, two ability, two P/T stickers", (length (StickerSheet.names sheet), StickerSheet.art sheet, length abilityStickers, length ptStickers) == (3, 3, 2, 2)),
                    ("four Oracle lines", length lines_ == 4),
                    ("ability ticket costs", fmap AbilitySticker.tickets abilityStickers == fmap fst abilityLines),
                    ("P/T ticket costs", fmap PowerToughnessSticker.tickets ptStickers == fmap fst ptLines),
                    ("P/T values", fmap ptText ptStickers == fmap snd ptLines),
                    ("keyword stickers", and (zipWith keywordsAgree abilityStickers abilityLines))
                  ]
             in case [what | (what, False) <- checks] of
                  [] -> Nothing
                  failed -> Just (path <> ": " <> show failed)
    Spec.assertEqWith s "every sheet agrees with its text" (Maybe.mapMaybe offends loaded) []
  Spec.it s "CR 103.2d four sheets: three are drawn at random, one at a time" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    let plain = Deck.fromCards (Map.singleton mountain 10)
        (gs, asked) = startedWith ((S.alice, withSheetsDeck sheets plain) NonEmpty.:| [(S.bob, plain)])
    Spec.assertEqWith s "the four committed sheets load" (length sheets) 4
    Spec.assertEqWith s "CR 103.2d three of the four sheets are chosen" (chosenOf S.alice gs) (Set.fromList [1, 2, 3])
    Spec.assertEqWith s "each drawn from those not yet drawn" asked [[0, 1, 2, 3], [0, 1, 2], [0, 1]]
    Spec.assertEqWith s "and bob, who brought none, plays without stickers" (chosenOf S.bob gs) Set.empty
  Spec.it s "CR 103.2d/123.2b three sheets are all kept, unasked" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    let plain = Deck.fromCards (Map.singleton mountain 10)
        (gs, asked) = startedWith ((S.alice, withSheetsDeck (take 3 sheets) plain) NonEmpty.:| [(S.bob, plain)])
    Spec.assertEqWith s "CR 123.2b all three are chosen" (chosenOf S.alice gs) (Set.fromList [0, 1, 2])
    Spec.assertEqWith s "and nothing was drawn at random" asked []
