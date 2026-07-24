-- Covers cross-cutting universal QuickCheck invariants (true for every seed).
module Pawl.PropertySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Source as Source
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QC

-- Playing one game out is this suite's entire cost (~66 ms; the other 948 tests
-- together take ~1 s), so the iteration count is the only real dial. 16 is the
-- cheap end of the curve: these are coarse whole-game invariants, so a bug that
-- breaks one breaks nearly every seed -- a mutation that drops CR 500.4's mana
-- emptying is caught by the third seed. To crank it for a milestone gate, edit
-- this number: both a localOption and an in-property withNumTests take
-- precedence over --quickcheck-tests, so there is no command-line override.
iterations :: QC.QuickCheckTests
iterations = 16

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

-- The card-backed objects (Source.OfCard) are conserved at 120 across a game
-- (CR 400.7 mints a fresh id per zone change but never a new card). Tokens
-- (Source.OfToken) legitimately come and go, so they are excluded -- a surviving
-- token at game end must not read as a conservation break (M4c).
cardBackedCount :: GameState.GameState -> Int
cardBackedCount gs =
  let fromCard obj = case Object.source obj of
        Source.OfCard _ -> True
        Source.OfToken _ -> False
        Source.OfAbility _ _ -> False
        Source.OfTrigger _ _ -> False
        Source.OfEmblem _ -> False
        Source.OfInherentTrigger _ _ -> False
   in length (filter fromCard (Map.elems (GameState.objects gs)))

-- Every universal invariant, judged against ONE played-out game. They share the
-- fixture deliberately: five separate properties each calling runRandomGame
-- played five times the games to answer the same five questions. Each arm is
-- labelled so a failure names the invariant that broke.
universalInvariants :: GameState.GameState -> QC.Property
universalInvariants gs =
  QC.conjoin
    [ QC.counterexample "conservation: 120 card-backed objects at end" $
        cardBackedCount gs QC.=== 120,
      -- The invariant that matters most now. Combat is the first thing that can
      -- end a game before the library runs out.
      QC.counterexample "every game terminates with a result" $
        QC.property (Maybe.isJust (GameState.result gs)),
      QC.counterexample "at least 120 ids were minted" $
        QC.property (nextIdOf gs >= 120),
      QC.counterexample "no mana floats at the end" $
        GameState.manaPool gs QC.=== Map.empty,
      -- Replaces M0's "no life changes". Nothing here GAINS life, so any
      -- increase is a bug. Dies at lifelink (still unscheduled -- see the
      -- design doc's punchlist), the same way this invariant's ancestor
      -- announced M1b.
      QC.counterexample "life never increases" $
        QC.property (all (\pl -> Player.life pl <= Setup.startingLife) (Map.elems (GameState.players gs)))
    ]

propertyTests :: Cards.Cards -> Tasty.TestTree
propertyTests cards =
  Tasty.localOption iterations
    . Tasty.testGroup "Properties"
    $ [ QC.testProperty "every matchup upholds every universal invariant" $
          \s -> QC.conjoin (fmap (\m -> universalInvariants (S.runRandomGame m s)) (S.matchups cards)),
        -- Durable structural property: with a deck that can only ever deck out (60
        -- basic lands, no spells, no attackers), every seed's game ends AND ends by
        -- a player drawing from an empty library (CR 704.5b) -- never by any other
        -- loss condition. Stays true no matter what cards later exist.
        QC.testProperty "a lands-only mirror always ends by deck-out" $
          \s ->
            let final = S.runRandomGame (S.landsOnly cards) s
             in QC.property
                  ( Maybe.isJust (GameState.result final)
                      && not (Set.null (GameState.drewFromEmpty final))
                  )
      ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Properties" [propertyTests cards]
