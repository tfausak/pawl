{-# LANGUAGE GADTs #-}

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as TextIO
import Numeric.Natural (Natural)
import qualified Pawl.Codec as Codec
import qualified Pawl.Engine as Engine
import qualified Pawl.Json as Json
import qualified Pawl.Setup as Setup
import qualified Pawl.Type.Action as Action
import qualified Pawl.Type.Deck as Deck
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.Result (Result)
import qualified Pawl.Type.Subtype as Subtype
import qualified Test.Tasty.Bench as Bench

isCreatureRecipient :: Recipient.Recipient -> Bool
isCreatureRecipient r = case r of
  Recipient.ToCreature _ -> True
  Recipient.ToPlayer _ -> False
  Recipient.ToObject _ -> False

alwaysPass :: Prompt.Prompt r -> r
alwaysPass p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> Action.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing

-- Casts when legal, otherwise passes: the benchmark that actually exercises the
-- stack, mana payment, and resolution.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing

-- Casts, attacks, and blocks: the benchmark that exercises combat.
fightAnswer :: Prompt.Prompt r -> r
fightAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (map (\b -> (b, a)) mine)
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing

-- Two players, seeded from the benchmark's argument.
playersFrom :: Natural -> NonEmpty.NonEmpty PlayerId
playersFrom n = PlayerId.MkPlayerId n NonEmpty.:| [PlayerId.MkPlayerId (n + 1)]

-- Takes the first player's id so the whole game genuinely depends on the
-- benchmark's argument; otherwise GHC floats the result out and times a cached
-- value rather than the game. NOINLINE keeps it from being folded back in.
goldfish :: Deck.Deck -> Natural -> Result
goldfish deck n =
  let players = playersFrom n
   in fst (Engine.runMatchPure alwaysPass (Setup.mirror deck players))
{-# NOINLINE goldfish #-}

-- Parameterized for the same reason as 'goldfish'.
casting :: Deck.Deck -> Natural -> Result
casting deck n =
  let players = playersFrom n
   in fst (Engine.runMatchPure castAnswer (Setup.mirror deck players))
{-# NOINLINE casting #-}

fighting :: Deck.Deck -> Natural -> Result
fighting deck n =
  let players = playersFrom n
   in fst (Engine.runMatchPure fightAnswer (Setup.mirror deck players))
{-# NOINLINE fighting #-}

-- Load one card from its committed JSON file, failing loudly (in IO, never
-- `error` in pure code) on a missing or malformed file.
loadPrinting :: String -> IO Printing.Printing
loadPrinting slug = do
  contents <- TextIO.readFile ("data/cards/" <> slug <> ".json")
  case Json.parse contents >>= Codec.jsonToPrinting of
    Right p -> pure p
    Left err -> ioError (userError ("card " <> slug <> ": " <> show err))

-- The redDeck recipe (slug -> count), loaded from files -- the proof that
-- files -> parse -> a real game works end-to-end. Duplicated here rather than
-- shared through a compiled card module (the non-scalable path design rejects).
loadRedDeck :: IO Deck.Deck
loadRedDeck = do
  mountain <- loadPrinting "mountain"
  piker <- loadPrinting "goblin-piker"
  maiden <- loadPrinting "bird-maiden"
  bolt <- loadPrinting "lightning-bolt"
  pure (Deck.MkDeck (Map.fromList [(mountain, 36), (piker, 12), (maiden, 8), (bolt, 4)]))

main :: IO ()
main = do
  deck <- loadRedDeck
  Bench.defaultMain
    [ Bench.bench "goldfish 2p" $ Bench.whnf (goldfish deck) 0,
      Bench.bench "casting 2p" $ Bench.whnf (casting deck) 0,
      Bench.bench "fighting 2p" $ Bench.whnf (fighting deck) 0
    ]
