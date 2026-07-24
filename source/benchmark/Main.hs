{-# LANGUAGE GADTs #-}

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as TextIO
import Numeric.Natural (Natural)
import qualified Pawl.Codec as Codec
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Json as Json
import qualified Pawl.Setup as Setup
import qualified Pawl.Type.Action as Action
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.MulliganDecision as MulliganDecision
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

-- Cast if anything is castable, else play a land, else pass. Mirrors the test
-- suite's Pawl.Support.castAnswer. The land fallback is load-bearing: without it
-- no land ever reaches the battlefield, so no mana ever exists, so
-- Action.legalActions never offers a Cast and the filter is always empty -- which
-- is how all three benchmarks came to execute the same goldfish game (#66).
castElsePlay :: [Action.Action] -> Action.Action
castElsePlay actions =
  let isCast a = case a of
        Action.Cast _ -> True
        _ -> False
      isPlay a = case a of
        Action.Play _ -> True
        _ -> False
   in case filter isCast actions of
        h : _ -> h
        [] -> case filter isPlay actions of
          h : _ -> h
          [] -> Action.Pass

-- CR 514.1 cleanup discard, answered from the END of the hand.
--
-- Also load-bearing for #66, and not interchangeable with taking the front. The
-- hand is oldest-first, and a player may play only one land per turn (CR 305.2),
-- so surplus lands pile up as the oldest cards held. Discarding from the front
-- therefore pitches precisely the lands the script needs, and the board never
-- develops even with the cast/play fallback above in place: measured over one
-- match, front-discard yields 4 land plays and no combat, back-discard yields 72
-- land plays and 66 declare-attackers prompts.
discardNewest :: [a] -> Natural -> [a]
discardNewest ids n = take (fromIntegral n) (reverse ids)

alwaysPass :: Prompt.Prompt r -> r
alwaysPass p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseAction {} -> Action.Pass
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> discardNewest ids n
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
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> fmap fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> take (fromIntegral count) hand

-- Plays lands and casts when legal, otherwise passes: the benchmark that actually
-- exercises the stack, mana payment, and resolution.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> discardNewest ids n
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction _ _ actions -> castElsePlay actions
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> fmap fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> take (fromIntegral count) hand

-- Plays lands, casts, attacks, and blocks: the benchmark that exercises combat.
fightAnswer :: Prompt.Prompt r -> r
fightAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> discardNewest ids n
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (fmap (\b -> (b, a)) mine)
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction _ _ actions -> castElsePlay actions
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> fmap fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> take (fromIntegral count) hand

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
