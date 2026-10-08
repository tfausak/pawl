-- Covers: CR 811's Alternating Teams variant -- Pawl.Engine.AlternatingTeams'
-- setUp (CR 811.2b) and seated (CR 811.1, 811.3). CR 811.4's attack limit, in
-- Pawl.Engine.Combat's attackableOpponents, is proved by the scenarios under
-- data/scenarios/alternating-teams.
module Pawl.AlternatingTeamsSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.AlternatingTeams as AlternatingTeams
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s _ = Spec.describe s "AlternatingTeams" $ do
  -- CR 811.3: four seats, two teams. Alternating is accepted and switches the
  -- variant on; the same players with each team side by side are refused.
  Spec.it s "CR 811.3 no one is seated next to a teammate" $ do
    Spec.assertEqWith
      s
      "CR 811.3 alternating seats start an Alternating Teams game"
      (fmap (GameSettings.alternatingTeams . GameState.settings) (AlternatingTeams.setUp (teamsOf [0, 1, 0, 1]) (game 4)))
      (Just True)
    Spec.assertEqWith s "CR 811.3 teammates side by side are refused" (Maybe.isJust (AlternatingTeams.setUp (teamsOf [0, 0, 1, 1]) (game 4))) False

  -- CR 811.1 / 811.3: six seats. Three teams of two in CR 811.3's example's
  -- order are accepted; teams of three, two and one, nobody beside a teammate,
  -- are refused, and so is a table that is all one team.
  Spec.it s "CR 811.1 teams are of equal size and equally spaced" $ do
    Spec.assertEqWith s "CR 811.3's example seating" (AlternatingTeams.seated (teamsOf [0, 1, 2, 0, 1, 2]) (seats 6)) True
    Spec.assertEqWith s "CR 811.1 unequal teams are refused" (AlternatingTeams.seated (teamsOf [0, 1, 0, 1, 0, 2]) (seats 6)) False
    Spec.assertEqWith s "CR 811.1 one team is refused" (AlternatingTeams.seated (teamsOf [0, 0, 0, 0, 0, 0]) (seats 6)) False

  -- CR 811.2b: exactly one of attack left, attack right and attack multiple
  -- players; CR 809.3c's option is the Emperor variant's, not one of them.
  Spec.it s "CR 811.2b the Emperor's attack option is refused" $
    Spec.assertEqWith
      s
      "CR 811.2b"
      (Maybe.isJust (AlternatingTeams.setUp (teamsOf [0, 1, 0, 1]) (withOption (Just AttackOption.Adjacent) (game 4))))
      False
  where
    seats n = fmap PlayerId.MkPlayerId [0 .. n - 1]
    game n = Setup.emptyGame (S.alice NonEmpty.:| drop 1 (seats n))
    teamsOf ts = Teams.MkTeams (Map.fromList (zip (fmap PlayerId.MkPlayerId [0 ..]) (fmap TeamId.MkTeamId ts)))
    withOption option gs = gs {GameState.settings = (GameState.settings gs) {GameSettings.attackOption = option}}
