-- | CR 732.3: a fragmented loop, where each player involved takes an
-- independent action and the game returns to a state it was in before. The
-- active player, or the first involved player in turn order from them, must
-- then make a different game choice -- at the priority grain CR 732.1 names,
-- a different action -- so the loop does not continue.
--
-- Not a choice made for anyone: the rule narrows the named player's menu, and
-- CR 732.5 keeps Pass on it.
--
-- UNDER-DETECTION is the safe direction throughout. A state this module fails
-- to recognise as a repeat leaves the menu as it was, so every approximation
-- here errs towards seeing no loop.
module Pawl.Engine.Fragmented where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Planechase as Planechase
import Pawl.Types.Action (Action)
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.EventGroup as EventGroup
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.LoopTrail (LoopTrail)
import qualified Pawl.Types.LoopTrail as LoopTrail
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import Pawl.Types.StateDigest (StateDigest)
import qualified Pawl.Types.StateDigest as StateDigest
import qualified Pawl.Types.Timestamp as Timestamp

-- | The game state as CR 732.3 compares it: the whole record, by its derived
-- Eq, less the fields below. A RECORD UPDATE rather than a field list, so a
-- field added later is compared by default and can only ever prevent a
-- detection -- Pawl.Engine.Interchangeable.objects' posture, whole-Object Eq
-- less the timestamp.
--
-- Dropped as bookkeeping no card reads: the id, timestamp, printing and event
-- group supplies; CR 104.4b's `lastChoice` and `loopInvolvement`; the
-- grow-only printing interning; and the scan state keyed off the log
-- (`scannedThrough`, `damageScannedThrough`, `battlefieldWhenTriggered`,
-- `controlSample`). `lastKnown` and `stackArchive` are keyed by ids that have
-- left (CR 400.7), so an entry is reachable only through a kept field holding
-- that id.
--
-- Dropped as history a card CAN read: `events` and `activationsThisTurn`.
-- Kept, no state could recur, since every action appends to both; dropped, a
-- difference only they hold is one `readsHistory` must vouch nothing reads.
--
-- Object ids are compared as they stand, so a loop that moves a card (CR
-- 400.7) is never detected. Not implemented: detecting a loop whose undo
-- leaves a stored row behind -- a continuous effect (CR 611.2a), a counter's
-- timestamp -- which is CR 732.3's own example (#4650).
digest :: GameState -> StateDigest
digest gs =
  StateDigest.MkStateDigest
    gs
      { GameState.events = Seq.empty,
        GameState.activationsThisTurn = Seq.empty,
        GameState.nextObjectId = ObjectId.MkObjectId 0,
        GameState.nextTimestamp = Timestamp.MkTimestamp 0,
        GameState.nextPrintingId = PrintingId.MkPrintingId 0,
        GameState.nextEventGroup = EventGroup.MkEventGroup 0,
        GameState.eventGroupDepth = 0,
        GameState.lastChoice = Timestamp.MkTimestamp 0,
        GameState.loopInvolvement = Map.empty,
        GameState.printings = Map.empty,
        GameState.printingIds = Map.empty,
        GameState.scannedThrough = 0,
        GameState.damageScannedThrough = 0,
        GameState.battlefieldWhenTriggered = Map.empty,
        GameState.controlSample = Map.empty,
        GameState.lastKnown = Map.empty,
        GameState.stackArchive = Map.empty,
        -- Canonical rather than dropped: a payment leaves an empty pool behind
        -- where there was no entry, and no card tells the two apart.
        GameState.manaPool = Map.filter (not . null . Mana.unwrap) (GameState.manaPool gs)
      }

-- | CR 732.3's example makes "nothing in the game cares how many times an
-- ability has been activated" the precondition: does anything in the game
-- read the history `digest` drops? Every printing, in every zone and outside
-- the game, through GameState.printings, which interns them all; a token or
-- copy spec nests inside its creator's face.
--
-- A walk over each card's derived Show rather than a hand-written traversal
-- of Card, which is where nesting gets missed. A single-word entry matches any
-- identifier containing it, a two-word entry an adjacent pair, such as a
-- record field and its value. A false match costs only a detection.
--
-- Not implemented: filtering per event shape, keeping only what some card
-- reads, rather than refusing every detection once any reader is present
-- (#4651).
readsHistory :: GameState -> Bool
readsHistory gs =
  let readsLog printing =
        let words_ = identifiers (show (Printing.card printing))
            pairs = Set.fromList (zip words_ (drop 1 words_))
            unique = Set.toList (Set.fromList words_)
            matches entry = case entry of
              [word] -> any (word `List.isInfixOf`) unique
              [first, second] -> Set.member (first, second) pairs
              _ -> False
         in any matches historyReaders
   in -- The rules' own readers: Pawl.Engine.Planechase.rollCost (CR 901.9),
      -- and Pawl.Engine.Speed.increaseAbility's once-each-turn limit (CR
      -- 702.179d), minted for any player with a speed.
      Planechase.isPlanechase gs
        || any (Maybe.isJust . Player.speed) (GameState.players gs)
        || any readsLog (Map.elems (GameState.printings gs))

-- The identifiers in a derived Show, in order.
identifiers :: String -> [String]
identifiers text = case dropWhile (not . isIdentifier) text of
  [] -> []
  rest ->
    let (word, more) = span isIdentifier rest
     in word : identifiers more
  where
    isIdentifier c = Char.isAlphaNum c || c == '_'

-- | Every construct whose engine read site consults GameState.events or
-- GameState.activationsThisTurn as history, named as Show spells it. A new
-- reader of either field owes an entry here -- and so does a construct the
-- engine EXPANDS into one (a keyword's minted ability or rider, as
-- Pawl.Engine.Keyword builds them), since the card's Show then spells only
-- the keyword.
--
-- Read sites needing none: the readers of the CURRENT action's own events
-- (Pawl.Engine.Resolve.Effect, Pawl.Engine.Cost's reversal and mana
-- triggers, the trigger scan, the CR 704.5h watermark); readers keyed by an
-- id that has moved (Pawl.Engine.Count.arrivalsOf, revealedArriving,
-- Pawl.Engine.MoveDuration.departedIn, Pawl.Engine.Event.Trigger.entryGroup),
-- which a lap cannot move again under the same id; and
-- Pawl.Engine.Engine.beginTurnOf's fold of the casts and attacks, which a
-- priority-only lap without a zone change cannot write.
historyReaders :: [[String]]
historyReaders =
  [ -- Pawl.Engine.Count.evaluate's Scope.InHistory arm (CR 608.2i).
    ["InHistory"],
    -- Pawl.Engine.Quantity.evaluateAgainst's arms over Pawl.Engine.Game's
    -- folds: WasBlockedThisTurn, DamageDealtToThisTurn,
    -- AttackersDeclaredThisTurn, CardsDiscardedThisTurn, BendingsThisTurn,
    -- LifeGainedThisTurn, PlayersDealtDamageThisTurn,
    -- DamageDealtToPlayersThisTurn, SpellsCastThisTurn,
    -- TimesResolvedThisTurn, PermanentsDiedThisTurn, SpellsCastUsingThisTurn
    -- and EnteredThisTurn; and Pawl.Engine.Filter's AttackedThisTurn,
    -- MilledThisTurn, DealtDamageThisTurn (both views), EnteredThisTurn,
    -- CrewedSourceThisTurn, ConvokedSourceThisTurn and SaddledSourceThisTurn,
    -- off Pawl.Engine.Projection.View's and Pawl.Engine.Count.playerView's
    -- folds.
    ["ThisTurn"],
    -- Quantity.AttackersInTheirLastTurn (Game.attackersInTheirLastTurn).
    ["AttackersInTheirLastTurn"],
    -- Quantity.SpellsCastBefore, EnteredFrom and WasCastFrom
    -- (Pawl.Engine.Quantity's entriesOf).
    ["SpellsCastBefore"],
    ["EnteredFrom"],
    ["WasCastFrom"],
    -- Pawl.Engine.Cost.candidateCostsGiven's surge, spectacle, prowl,
    -- freerunning and mayhem clauses, and
    -- Pawl.Engine.PlayerEffect.mayPlayByMayhem.
    ["Surge"],
    ["Spectacle"],
    ["Prowl"],
    ["Freerunning"],
    ["Mayhem"],
    -- Pawl.Engine.PlayerEffect.prohibitsCasting's CantCastMoreThan, through
    -- Game.castsPerPlayer.
    ["CantCastMoreThan"],
    -- Pawl.Engine.Activatable.loyaltyOk (CR 606.3), every loyalty cost.
    ["Loyalty"],
    -- Pawl.Engine.Event.Match's StepBegins ordinal (mainPhasesBegun) and
    -- SpellCast ordinal (castOrdinal).
    ["ordinal", "Just"],
    -- Pawl.Engine.Event.Match's declarationsOf and crewingsOf.
    ["FirstTimeEachTurn"],
    -- Pawl.Engine.Event.withinTriggerLimit.
    ["OncePerTurn"],
    -- Pawl.Engine.Saga.readAheadRestricted (CR 702.155a).
    ["ReadAhead"],
    -- Pawl.Engine.Coin.statedFor's flipsThisTurn.
    ["firstEachTurn", "True"],
    -- Pawl.Engine.PlayerEffect.firstActivation, over activationsThisTurn.
    ["onlyFirst", "Just"],
    -- Keyword expansions, whose readers the printed keyword does not spell:
    -- Pawl.Engine.Keyword.storm's Quantity.SpellsCastBefore (CR 702.40a),
    -- Keyword.gravestorm's Quantity.PermanentsDiedThisTurn (CR 702.69a), and
    -- Keyword.printedRiders' Boast arm, a Count over Filter.AttackedThisTurn
    -- (CR 702.142a). Separate entries, the walk being case-sensitive.
    ["Storm"],
    ["Gravestorm"],
    ["Boast"]
  ]

-- | CR 732.3: the actions `pid`, asked at `gs` (digested as `here`), may not
-- take, so a fragmented loop does not continue. Empty unless `here` repeats a
-- state this loop has seen and `pid` is the player the rule names.
--
-- "Involved in the loop" is the players who took a non-Pass action since the
-- state FIRST occurred: a shorter loop nested inside a longer one through the
-- same state (a no-op lap by one player) must not hide the longer one's
-- players. Forbidden is every `choiceless` action `pid` took at ANY prior
-- occurrence, not only the last, so A, B, A, B through one state is refused
-- too. Pass never is (CR 732.5). Pawl.FragmentedSpec's control case is the
-- board where the nesting happens.
forbidden :: StateDigest -> GameState -> PlayerId -> LoopTrail -> Set.Set Action
forbidden here gs pid loopTrail =
  case Map.findWithDefault [] here (LoopTrail.seen loopTrail) of
    [] -> Set.empty
    occurrences ->
      let choices = LoopTrail.choices loopTrail
          acted (_, action) = action /= Action.Pass
          involved = Set.fromList (fmap fst (filter acted (Foldable.toList (Seq.drop (minimum occurrences) choices))))
          named = List.find (`Set.member` involved) (Game.turnOrderFrom (GameState.activePlayer gs) gs)
          taken = Set.fromList [action | (who, action) <- Maybe.mapMaybe (`Seq.lookup` choices) occurrences, who == pid, choiceless action]
       in if named == Just pid && not (Set.null taken) && not (readsHistory gs) then taken else Set.empty

-- | Is `action` the whole of the game choice (CR 732.1), so refusing it
-- refuses only the choice that continued the loop? An activation with no
-- target, a single mode, no X and no cost but nothing to pay; never Pass (CR
-- 732.5).
--
-- Not implemented: refusing an activation by the choices it was made with --
-- its targets (CR 601.2c), modes, X or payment -- so one that leaves any of
-- them open is never refused, and a loop through it is not broken (#4663).
choiceless :: Action -> Bool
choiceless action = case action of
  Action.Activate _ ability ->
    let modal = ActivatedAbility.modal ability
        cost = ActivatedAbility.cost ability
     in all (Map.null . Mode.targetSlots) (Modal.modes modal)
          && Seq.length (Modal.modes modal) == 1
          && Modal.selection modal == ModeSelection.ChooseExactly 1
          && null (ActivatedAbility.maximumX ability)
          && maybe True (null . ManaCost.unwrap) (Cost.mana cost)
          && null (Cost.components cost)
  _ -> False

-- | Note that `pid`, asked at `here`, chose `action`. Held as pending until
-- the next prompt, since an action CR 733.1 reversed leaves the game in the
-- state it was taken in and never happened.
record :: StateDigest -> PlayerId -> Action -> LoopTrail -> LoopTrail
record here pid action loopTrail = loopTrail {LoopTrail.pending = Just (here, pid, action)}

-- | Commit the pending choice, unless the game is still in the state it was
-- made in. Both functions above take a trail settled against `here`.
settle :: StateDigest -> LoopTrail -> LoopTrail
settle here loopTrail = case LoopTrail.pending loopTrail of
  Nothing -> loopTrail
  Just (there, pid, action)
    | there == here -> loopTrail {LoopTrail.pending = Nothing}
    | otherwise ->
        let at = Seq.length (LoopTrail.choices loopTrail)
         in LoopTrail.MkLoopTrail
              { LoopTrail.choices = LoopTrail.choices loopTrail Seq.|> (pid, action),
                LoopTrail.seen = Map.insertWith (<>) there [at] (LoopTrail.seen loopTrail),
                LoopTrail.pending = Nothing
              }
