-- Covers Pawl.Engine.Engine's match driver at whole-game scale: a game played
-- from Setup through to a Result, asserted on the state it ends in. Anything
-- that can be pinned on a hand-built board belongs in the spec for the
-- subsystem that owns it -- what is here is only what needs a WHOLE game,
-- because the claim is about the turn loop, the SBA sweep and the departure
-- gate agreeing with each other over hundreds of turns.
--
-- Every case runs at least one full game, so this is the suite's most expensive
-- module by a wide margin. Main.hs wires it with a timeout for the same reason
-- Pawl.ReplacementSpec has one: every case here depends on a game ENDING, and a
-- driver that fails to terminate hangs the suite rather than failing it.
--
-- These are deterministic on purpose. They replace a QuickCheck suite that
-- played 96 random games per run and cost more than the rest of the tests
-- put together; the seed bought variety but not discrimination, since shrinking
-- an Int seed yields an unrelated game rather than a smaller one. What is left
-- here is the part that genuinely needed a whole game. The rest of what those
-- properties asserted was already covered deterministically: CR 500.5's mana
-- emptying by ManaSpec and CastSpec, CR 704.5b by GameSpec, and CR 104.2a's
-- "one departure does not decide a three-player game" by DepartureSpec.
module Pawl.EngineSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Zone as Zone

-- How many card-backed objects the game state holds. CR 400.7 mints a fresh id
-- per zone change but never a new card, so this is conserved across a game
-- except where CR 800.4a removes a departed player's objects. Tokens and the
-- other non-card sources legitimately come and go, so a surviving one must not
-- read as a conservation break.
cardBackedCount :: GameState.GameState -> Int
cardBackedCount gs =
  let fromCard obj = case Object.source obj of
        Source.OfCard _ -> True
        Source.OfToken _ -> False
        Source.OfAbility _ _ -> False
        Source.OfTrigger _ _ -> False
        Source.OfEmblem _ -> False
        Source.OfInherentTrigger _ _ -> False
   in Map.size (Map.filter fromCard (GameState.objects gs))

battlefieldCount :: GameState.GameState -> Int
battlefieldCount gs =
  Map.size (Map.filter ((==) Zone.Battlefield . Object.zone) (GameState.objects gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Engine" $ do
  -- A 60-basic-land mirror can cast nothing and attack with nothing, so the
  -- only loss condition reachable is CR 704.5b and the only end is CR 104.2a's
  -- last player standing. That forces the shape of a three-seat game: CR 704.5b
  -- takes the first player, the game CONTINUES with two, CR 704.5b takes a
  -- second, and only then is there a winner.
  --
  -- The count is what discriminates, not an isJust: a two-player-shaped driver
  -- -- one where any departure decides the game -- stops at the FIRST deck-out
  -- and passes "a result exists and someone decked out".
  --
  -- Determinism is free here rather than assumed: Prompt.Shuffle is the
  -- identity under castAnswer, and 60 identical Mountains make the order
  -- irrelevant anyway, so there is nothing for a seed to vary.
  Spec.it s "CR 704.5b/104.2a a three-seat lands-only game needs TWO deck-outs to find a winner" $ do
    matchup <- S.threePlayerLandsOnly (S.printingOf s registry)
    let final = snd (Engine.runMatchPure S.castAnswer matchup)
        decked = GameState.drewFromEmpty final
    Spec.assertEqWith s "CR 704.5b exactly two players drew from an empty library" (Set.size decked) 2
    case GameState.result final of
      Just (Result.Won w) ->
        Spec.assertBool s (not (Set.member w decked)) "CR 104.2a the winner is the one player who did not deck out"
      other -> Spec.assertFailure s ("CR 104.2a expected a won game, got " <> show other)
    -- CR 800.4a: "When a player leaves the game, all objects (see rule 109)
    -- owned by that player leave the game." Leaving is not a zone change, so
    -- those objects are not moved anywhere -- they are deleted -- and the
    -- conserved quantity is one deck per player STILL IN THE GAME. Two
    -- departures at three seats therefore end at 60, not 180.
    Spec.assertEqWith s "CR 800.4a only the survivor's deck outlives the game" (cardBackedCount final) 60
  -- The paired control for CR 800.4a's count, and the reason 60 above is a
  -- claim rather than an accident. CR 800.1: "A multiplayer game is a game that
  -- begins with more than two players." Only such a game continues after a
  -- departure, so at two seats CR 800.4a's removal never runs and BOTH decks
  -- outlive the game's end -- 120, not 60. A gate that fired at two seats would
  -- delete the loser's 60 cards and this is what would catch it.
  Spec.it s "CR 800.1 a two-seat lands-only game ends on the FIRST deck-out, and keeps both decks" $ do
    matchup <- S.landsOnly (S.printingOf s registry)
    let final = snd (Engine.runMatchPure S.castAnswer matchup)
        decked = GameState.drewFromEmpty final
    Spec.assertEqWith s "CR 704.5b exactly one player drew from an empty library" (Set.size decked) 1
    case GameState.result final of
      Just (Result.Won w) ->
        Spec.assertBool s (not (Set.member w decked)) "CR 104.2a the other player wins"
      other -> Spec.assertFailure s ("CR 104.2a expected a won game, got " <> show other)
    Spec.assertEqWith s "CR 800.1 no removal at two seats, so both decks survive" (cardBackedCount final) 120
  -- Combat is the first thing that can end a game before a library runs out, and
  -- the driver has to survive hundreds of turns of it. S.fightAnswer differs
  -- from S.castAnswer in exactly the two combat declarations (CR 508.1a, CR
  -- 509.1a), so the control is the same game with combat switched off.
  --
  -- The battlefield comparison is what makes this a combat test rather than a
  -- second copy of CastSpec's casting game: creatures die in the fought game
  -- that survive in the control, so combat demonstrably ran. Without it the
  -- case would still pass if DeclareAttackers were ignored outright.
  Spec.it s "a game driven through combat terminates, and combat actually happened" $ do
    matchup <- S.greenBlack (S.printingOf s registry)
    let fought = snd (Engine.runMatchPure S.fightAnswer matchup)
        control = snd (Engine.runMatchPure S.castAnswer matchup)
    Spec.assertBool s (Maybe.isJust (GameState.result fought)) "the fought game reaches a result"
    Spec.assertBool s (Maybe.isJust (GameState.result control)) "and so does its no-combat control"
    -- CR 510.2 deals the combat damage and CR 704.5g destroys what it was
    -- lethal to, so a fought game ends with a smaller battlefield than the
    -- control that declared no attackers.
    Spec.assertLtWith s "CR 704.5g combat killed permanents the control kept" (battlefieldCount fought) (battlefieldCount control)
    -- CR 400.7 again, on the branch the lands-only cases cannot reach: dying,
    -- being cast and resolving are all zone changes, and none of them may mint
    -- or lose a card.
    Spec.assertEqWith s "CR 400.7 the fought game still conserves its cards" (cardBackedCount fought) 120
    Spec.assertEqWith s "CR 400.7 and so does the control" (cardBackedCount control) 120
