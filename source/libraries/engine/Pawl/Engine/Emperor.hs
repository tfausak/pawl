-- | CR 809: the Emperor variant -- its options, set over a seating, and the
-- team that leaves the game with its emperor.
module Pawl.Engine.Emperor where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.Emperors as Emperors
import qualified Pawl.Types.GameSettings as GameSettings
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Teams as Teams

-- | CR 809.3: an Emperor game's options over this game's seating
-- (GameState.turnOrder), its teams and each team's emperor -- CR 809.3a's
-- ranges, CR 809.3b's deploy creatures option and CR 809.3c's attack option.
--
-- The ranges are CR 809.6a's minima, which at three players a team are CR
-- 809.3a's 1 for a general and 2 for an emperor.
--
-- Not implemented: CR 809.2's and CR 809.6a's seating, which is the caller's
-- and unchecked (#4497). CR 809.4's starting emperor is
-- Pawl.Engine.Setup.randomEmperorFirst.
setUp :: Teams.Teams -> Emperors.Emperors -> GameState -> GameState
setUp teams emperors gs =
  gs
    { GameState.settings =
        (GameState.settings gs)
          { GameSettings.teams = teams,
            GameSettings.emperors = emperors,
            GameSettings.rangeOfInfluence = RangeOfInfluence.MkRangeOfInfluence (ranges teams emperors (GameState.turnOrder gs)),
            GameSettings.deployCreatures = True,
            GameSettings.attackOption = Just AttackOption.Adjacent
          }
    }

-- CR 809.6a: each general's range reaches the nearest general of an opposing
-- team, and each emperor's the second nearest. A player with too few opposing
-- generals at the table gets no entry, an unlimited range.
ranges :: Teams.Teams -> Emperors.Emperors -> [PlayerId.PlayerId] -> Map.Map PlayerId.PlayerId Natural.Natural
ranges teams emperors seats =
  let apart a b = case (List.elemIndex a seats, List.elemIndex b seats) of
        (Just i, Just j) -> let d = abs (i - j) in Just (min d (length seats - d))
        _ -> Nothing
      general pid = Maybe.isJust (Teams.teamOf teams pid) && not (Emperors.isEmperor emperors pid)
      opposingGenerals pid = filter (\other -> general other && Teams.areOpponents teams pid other) seats
      needed pid = if Emperors.isEmperor emperors pid then 2 else 1
      rangeOf pid = case drop (needed pid - 1) (List.sort (Maybe.mapMaybe (apart pid) (opposingGenerals pid))) of
        d : _ -> Int.toNatural d
        [] -> Nothing
   in Map.fromList [(pid, r) | pid <- seats, Maybe.isJust (Teams.teamOf teams pid), Just r <- [rangeOf pid]]

-- | CR 809.5b / 809.5c: the players who leave the game alongside these
-- departures -- the teammates still playing of each departing emperor, losing
-- when the emperor loses or concedes and drawing when the emperor draws. None
-- outside the Emperor variant.
fallsWith :: Departure.Departure -> [PlayerId.PlayerId] -> GameState -> [(Departure.Departure, PlayerId.PlayerId)]
fallsWith reason leaving gs =
  let settings = GameState.settings gs
      emperors = GameSettings.emperors settings
      teams = GameSettings.teams settings
      falling = filter (Emperors.isEmperor emperors) leaving
      followed pid = List.notElem pid leaving && any (\emperor -> Teams.sameTeam teams emperor pid) falling
      -- CR 104.3a: conceding is the emperor's own act; the team loses.
      theirs = case reason of
        Departure.Drew -> Departure.Drew
        Departure.Lost -> Departure.Lost
        Departure.Conceded -> Departure.Lost
   in [(theirs, pid) | pid <- GameState.turnOrder gs, List.elem pid (Game.stillPlaying gs), followed pid]
