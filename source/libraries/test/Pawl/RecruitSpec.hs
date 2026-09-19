{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 701.70 RECRUIT -- Pawl.Engine.Recruit and Effect.Recruit's arm in
-- Pawl.Engine.Resolve.Effect.
--
-- Long Lake Nuisance ({3}{U} Creature -- Bird 3/1, "Flying / When this creature
-- enters, recruit.") is the fixture. (Oracle text checked against
-- api.scryfall.com, 2026-09-18.)
--
-- ONE board, TWO answers, Pawl.CardTriggerSpec's Raffine's Informant shape --
-- rule 701.70a is rule 701.50d's three sentences with a token where connive has
-- a counter, so the discriminating board is the same one. alice holds a Hill
-- Giant and a Mountain, draws the Forest on top of her library, and discards
-- whichever card the answer pins by id. Nothing else differs between the two
-- cases, so the token is the nonland question alone.
--
-- THREE cards in hand when the discard is asked, against a count of one, so the
-- prompt is a real choice rather than CR 609.3's forced discard -- and the card
-- pinned is not the hand's first, so a reading that ignored the answer would bin
-- a different card.
module Pawl.RecruitSpec where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- CR 111.4's name for rule 701.70a's token, which names none itself: its
-- subtypes plus the word "Token".
soldier :: CardName.CardName
soldier = CardName.MkCardName (Text.pack "Human Soldier Token")

-- alice's battlefield objects named for rule 701.70a's token.
soldiersOf :: GameState.GameState -> [ObjectId.ObjectId]
soldiersOf gs =
  filter (\oid -> S.soleFaceName oid gs == soldier) (Game.zoneMembers Zone.Battlefield S.alice gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Recruit" $ do
  let board = do
        nuisance <- S.printingOf s registry "Long Lake Nuisance"
        giant <- S.printingOf s registry "Hill Giant"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        let (_, g1) = S.addLibraryCard forest S.alice (Setup.emptyGame S.bothPlayers)
            (giantId, g2) = S.addHandCard giant S.alice g1
            (mountainId, g3) = S.addHandCard mountain S.alice g2
            (_, g4) = S.entersWithTrigger nuisance S.alice g3
        pure (giantId, mountainId, g4)
      -- CR 701.9b's choice pinned by id rather than left to whichever card the
      -- hand offers first, so the answerer cannot repair the assertion.
      discarding :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      discarding pick p = case p of
        Prompt.ChooseDiscard {} -> [pick]
        _ -> S.identityAnswer p
      settle pick gs = S.runPure (discarding pick) gs Engine.priorityLoop
      graveyardNames gs = List.sort (fmap (\oid -> Text.unpack (CardName.unwrap (S.soleFaceName oid gs))) (Game.zoneMembers Zone.Graveyard S.alice gs))
      handNames gs = List.sort (fmap (\oid -> Text.unpack (CardName.unwrap (S.soleFaceName oid gs))) (Game.zoneMembers Zone.Hand S.alice gs))
  Spec.it s "CR 701.70a discarding a nonland card creates the 1/1 white Human Soldier token" $ do
    (giantId, _, gs) <- board
    let after = settle giantId gs
    -- THE gameplay reading, and first: the token rule 701.70a prints is on the
    -- battlefield, and it is a 1/1.
    Spec.assertEqWith s "CR 701.70a one Human Soldier token was created" (length (soldiersOf after)) 1
    Spec.assertEqWith s "CR 701.70a and it is a 1/1" (fmap (`S.powerToughnessOf` after) (List.take 1 (soldiersOf after))) [Just (1, 1)]
    Spec.assertEqWith s "the Hill Giant was the card discarded" (graveyardNames after) ["Hill Giant"]
    Spec.assertEqWith s "and alice drew the Forest first" (handNames after) ["Forest", "Mountain"]
  -- The paired case, differing in ONE thing: the same board, the same trigger,
  -- the same three cards offered -- but the discard alice pins is the land.
  Spec.it s "CR 701.70a discarding a land card creates no token" $ do
    (_, mountainId, gs) <- board
    let after = settle mountainId gs
    Spec.assertEqWith s "CR 701.70a no Human Soldier token was created" (length (soldiersOf after)) 0
    Spec.assertEqWith s "the Mountain was the card discarded" (graveyardNames after) ["Mountain"]
    Spec.assertEqWith s "and alice drew the Forest first" (handNames after) ["Forest", "Hill Giant"]
