{-# LANGUAGE GADTs #-}

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Type.Action as Action
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.OptionalDecision as OptionalDecision
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
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
discardNewest ids n = List.genericTake n (reverse ids)

alwaysPass :: Prompt.Prompt r -> r
alwaysPass p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseAction {} -> Action.Pass
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> discardNewest ids n
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
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
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a printed "may" is the least-eventful answer, and keeps
  -- the benchmark's script deterministic.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines

-- Plays lands and casts when legal, otherwise passes: the benchmark that actually
-- exercises the stack, mana payment, and resolution.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> discardNewest ids n
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
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
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a printed "may" is the least-eventful answer, and keeps
  -- the benchmark's script deterministic.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines

-- Plays lands, casts, attacks, and blocks: the benchmark that exercises combat.
fightAnswer :: Prompt.Prompt r -> r
fightAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> discardNewest ids n
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
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
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a printed "may" is the least-eventful answer, and keeps
  -- the benchmark's script deterministic.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines

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

-- The redDeck recipe (name -> count), loaded from the registry -- the proof
-- that files -> parse -> a real game works end-to-end.
loadRedDeck :: Registry.Type.Registry -> IO Deck.Deck
loadRedDeck registry = do
  mountain <- Registry.printing registry "Mountain"
  piker <- Registry.printing registry "Goblin Piker"
  maiden <- Registry.printing registry "Bird Maiden"
  bolt <- Registry.printing registry "Lightning Bolt"
  pure (Deck.MkDeck (Map.fromList [(mountain, 36), (piker, 12), (maiden, 8), (bolt, 4)]))

-- A deck that reliably attaches several Auras to a populated battlefield, so
-- Sba.fallsOff (per-Aura board re-derivation) and Projection.controllerOf
-- (walking the battlefield for control-granting statics, the hot path
-- Projection.controls calls at every SBA sweep) both get exercised here -- no
-- prior benchmark deck contained an Aura at all.
--
-- Setup.createDeck builds each player's library from 'Data.Map.Strict.toList',
-- so it is grouped by Printing's derived Ord -- primarily 'name' (Text, so
-- ordinary lexicographic order) -- NOT interleaved, and Prompt.Shuffle is the
-- identity in every answer function above, so the library is never actually
-- shuffled. "Control Magic" < "Darksteel Myr" < "Island" alphabetically, so the
-- library is exactly [Control Magic x3][Darksteel Myr x4][Island x53] with no
-- randomness at all. The opening hand is the library's first 7 cards -- all 3
-- Control Magic and all 4 Darksteel Myr, i.e. the entire non-land half of the
-- deck -- and every card kept in a hand at the maximum size stays there until
-- something makes room, per discardNewest above (CR 514.1 discards the newest
-- cards first, keeping this same original 7). Nothing else ever gets a turn at
-- being kept: every OTHER card this deck draws arrives during a mana-less
-- stretch with nothing to spend it on, so it is discarded again by the next
-- cleanup step -- which is why this deck carries not one spare copy of either
-- spell. Darksteel Myr (colourless, {3}) is castable off the Islands alone, so
-- once enough of the 53 Islands have entered play as one-a-turn land drops
-- (CR 305.2), all 4 Myr resolve as legal Cast targets for Control Magic
-- ({2}{U}{U}, also Island-payable) to enchant, in turn making all 6 Control
-- Magic across the mirror match legal casts. Observed (via 'cabal repl',
-- reading 'GameState.turnNumber' after 'Engine.runMatchPure'): this deck
-- decks out on turn 108. Darksteel Myr is 0/1 Indestructible, so once all 8
-- are on the battlefield, "fighting 2p aura" runs a full 4v4 attack/block/
-- damage cycle on essentially every one of those 108 turns -- nothing ever
-- dies, so combat never tapers off the way it does against the red mirror's
-- 1/2 Bird Maidens, which gang-block and kill the lead attacker.
loadControlDeck :: Registry.Type.Registry -> IO Deck.Deck
loadControlDeck registry = do
  island <- Registry.printing registry "Island"
  myr <- Registry.printing registry "Darksteel Myr"
  control <- Registry.printing registry "Control Magic"
  pure (Deck.MkDeck (Map.fromList [(island, 53), (myr, 4), (control, 3)]))

main :: IO ()
main = do
  root <- Registry.defaultRoot
  registry <- Registry.new root
  deck <- loadRedDeck registry
  controlDeck <- loadControlDeck registry
  Bench.defaultMain
    [ Bench.bench "goldfish 2p" $ Bench.whnf (goldfish deck) 0,
      Bench.bench "casting 2p" $ Bench.whnf (casting deck) 0,
      Bench.bench "fighting 2p" $ Bench.whnf (fighting deck) 0,
      Bench.bench "fighting 2p aura" $ Bench.whnf (fighting controlDeck) 0
    ]
