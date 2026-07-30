-- Covers cross-cutting universal QuickCheck invariants (true for every seed).
module Pawl.PropertySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Registry as Registry.Type
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Source as Source
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QC

-- Playing one game out is this suite's entire cost (~66 ms; the other 948 tests
-- together take ~1 s), so the iteration count is the only real dial. 16 is the
-- cheap end of the curve: these are coarse whole-game invariants, so a bug that
-- breaks one breaks nearly every seed -- a mutation that drops CR 500.5's mana
-- emptying is caught by the third seed. To crank it for a milestone gate, edit
-- this number: both a localOption and an in-property withNumTests take
-- precedence over --quickcheck-tests, so there is no command-line override.
iterations :: QC.QuickCheckTests
iterations = 16

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

-- How many card-backed objects (Source.OfCard) the game state holds. CR 400.7
-- mints a fresh id per zone change but never a new card, so this is conserved
-- across a game except where CR 800.4a removes a departed player's objects --
-- see expectedCardBacked. Tokens (Source.OfToken) legitimately come and go, so they
-- are excluded -- a surviving token at game end must not read as a conservation
-- break (M4c).
cardBackedCount :: GameState.GameState -> Integer
cardBackedCount gs =
  let fromCard obj = case Object.source obj of
        Source.OfCard _ -> True
        Source.OfToken _ -> False
        Source.OfAbility _ _ -> False
        Source.OfTrigger _ _ -> False
        Source.OfEmblem _ -> False
        Source.OfInherentTrigger _ _ -> False
   in toInteger (length (filter fromCard (Map.elems (GameState.objects gs))))

-- Every card the matchup deals out. Ids are only ever minted, never reclaimed,
-- so this is the minted-id floor no matter who leaves.
deckTotal :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Integer
deckTotal matchup = sum (fmap (toInteger . Setup.deckSize . snd) (NonEmpty.toList matchup))

-- How many card-backed objects the finished game SHOULD hold, which is not a
-- constant once there can be more than two seats.
--
-- CR 800.4a: "When a player leaves the game, all objects (see rule 109) owned by
-- that player leave the game". Leaving the game is not a zone change, so those
-- objects are not moved anywhere -- Departure.objectsLeaveWith deletes them --
-- and the conserved quantity is therefore one deck per player STILL IN THE GAME,
-- not one per seat. A free-for-all (CR 806.1) ends when CR 104.2a leaves one
-- player, i.e. after two departures at three seats, so a three-way mirror ends
-- at 60 rather than 180.
--
-- CR 800.1: "A multiplayer game is a game that begins with more than two
-- players." Only such a game continues after a departure
-- (Departure.continuesAfterDeparture), so at two seats CR 800.4a's removal never
-- runs, both players' cards outlive the game's end, and the whole matchup is
-- conserved -- which is why every two-player expectation here is still 120. That
-- asymmetry is deliberate on the engine's side too: a two-player SUBGAME is read
-- after it ends (CR 729.5), and deleting the loser's cards would destroy them.
--
-- The seat count is counted from the MATCHUP rather than asked of
-- Departure.continuesAfterDeparture on purpose. Reusing the engine's own gate
-- here would make this expectation agree with a wrong gate instead of catching
-- one: a gate that fired at two seats would delete the loser's 60 cards and this
-- would happily expect 60.
--
-- The per-seat SET, by contrast, is Game.stillPlaying, which is engine
-- state. That is deliberate and it is still a cross-check, because the two
-- sides read DIFFERENT fields of GameState: stillPlaying folds Player.status,
-- while cardBackedCount folds GameState.objects. Whichever half of a departure
-- goes wrong on its own is caught -- CR 800.4a removing an undeparted player's
-- objects, or removing too few or too many of a departed one's, moves the
-- actual count without moving the expectation, and a status flip with no
-- removal moves the expectation without moving the count.
--
-- What this arm cannot see is a LOCKSTEP bug: something that marks the wrong
-- player departed AND deletes exactly that player's objects moves both sides
-- together and stays green. That case is covered by the three-seat lands-only
-- property below, whose `Set.size decked === 2` is computed from
-- GameState.drewFromEmpty -- a third, independent field -- and whose winner
-- check reads GameState.result.
expectedCardBacked :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> GameState.GameState -> Integer
expectedCardBacked matchup gs =
  let seats = NonEmpty.toList matchup
      sized pid = case lookup pid seats of
        Nothing -> 0
        Just deck -> toInteger (Setup.deckSize deck)
   in if NonEmpty.length matchup > 2
        then sum (fmap sized (Game.stillPlaying gs))
        else deckTotal matchup

-- Every universal invariant, judged against ONE played-out game. They share the
-- fixture deliberately: five separate properties each calling runRandomGame
-- played five times the games to answer the same five questions. Each arm is
-- labelled so a failure names the invariant that broke.
universalInvariants :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> GameState.GameState -> QC.Property
universalInvariants matchup gs =
  QC.conjoin
    [ QC.counterexample "conservation: card-backed objects still owned by a player in the game (CR 800.4a)" $
        cardBackedCount gs QC.=== expectedCardBacked matchup gs,
      -- The invariant that matters most now. Combat is the first thing that can
      -- end a game before the library runs out.
      QC.counterexample "every game terminates with a result" $
        QC.property (Maybe.isJust (GameState.result gs)),
      QC.counterexample ("at least " <> show (deckTotal matchup) <> " ids were minted") $
        QC.property (nextIdOf gs >= deckTotal matchup),
      -- CR 500.5, and no longer universal: an Upwelling on the battlefield keeps
      -- every player's unspent mana through every step and phase end. It holds
      -- across these matchups because none of Pawl.Cards' decks plays one.
      QC.counterexample "no mana floats at the end" $
        GameState.manaPool gs QC.=== Map.empty,
      -- Replaces M0's "no life changes". Nothing here GAINS life, so any
      -- increase is a bug. Dies at lifelink (still unscheduled -- see the
      -- design doc's punchlist), the same way this invariant's ancestor
      -- announced M1b.
      QC.counterexample "life never increases" $
        QC.property (all (\pl -> Player.life pl <= Setup.startingLife) (Map.elems (GameState.players gs)))
    ]

propertyTests :: Registry.Type.Registry -> Tasty.TestTree
propertyTests registry =
  Tasty.localOption iterations
    . Tasty.testGroup "Properties"
    $ [ QC.testProperty "every matchup upholds every universal invariant" $
          \s -> QC.ioProperty $ do
            ms <- S.matchups registry
            pure (QC.conjoin (fmap (\m -> universalInvariants m (S.runRandomGame m s)) ms)),
        -- Durable structural property: with a deck that can only ever deck out (60
        -- basic lands, no spells, no attackers), every seed's game ends AND ends by
        -- a player drawing from an empty library (CR 704.5b) -- never by any other
        -- loss condition. Stays true no matter what cards later exist.
        QC.testProperty "a lands-only mirror always ends by deck-out" $
          \s -> QC.ioProperty $ do
            decks <- S.landsOnly registry
            let final = S.runRandomGame decks s
            pure $
              QC.property
                ( Maybe.isJust (GameState.result final)
                    && not (Set.null (GameState.drewFromEmpty final))
                ),
        -- The milestone's headline falsifier (spec sections 0 and 4). Three
        -- lands-only seats can only ever deck out, so the game's shape is
        -- forced: CR 704.5b takes the first player, the game CONTINUES with two
        -- (CR 104.2a does not fire yet), CR 704.5b takes a second, and only then
        -- is there a winner. So exactly two players drew from an empty library
        -- and the winner is the third.
        --
        -- DISCRIMINATING against the two-player-shaped implementation this
        -- milestone replaced -- one where a departure decides the game -- which
        -- gives exactly one. It is a count, not an isJust: the old property's
        -- shape (a result exists AND someone decked out) passes under that
        -- implementation on the first deck-out.
        QC.testProperty "a three-seat lands-only mirror needs TWO deck-outs to find a winner" $
          \s -> QC.ioProperty $ do
            decks <- S.threePlayerLandsOnly registry
            let final = S.runRandomGame decks s
                decked = GameState.drewFromEmpty final
            pure $
              QC.conjoin
                [ QC.counterexample "exactly two players decked out" $
                    Set.size decked QC.=== 2,
                  QC.counterexample "and the game was won, not drawn" $
                    QC.property (case GameState.result final of Just (Result.Won _) -> True; _ -> False),
                  QC.counterexample "by the one player who did not deck out" $
                    QC.property
                      ( case GameState.result final of
                          Just (Result.Won w) -> not (Set.member w decked)
                          _ -> False
                      )
                ]
      ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Properties" [propertyTests registry]
