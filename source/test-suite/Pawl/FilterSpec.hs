-- Covers Pawl.Type.Filter, Pawl.Type.PlayerRelation, Pawl.Filter.
module Pawl.FilterSpec where

import qualified Data.Set as Set
import qualified Pawl.Filter as Filter
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
-- Aliased Filter.Type, not Type, because the evaluator module Pawl.Filter
-- already claims the alias Filter (a documented exception to alias-to-last-
-- component, per the M4.5 P9 plan's global constraints).
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Subtype as Subtype
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A projected black creature controlled by player 0.
blackCreature :: Filter.View
blackCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.singleton Color.Black,
      Filter.subtypes = Set.singleton Subtype.Zombie,
      Filter.power = Just 2,
      Filter.controller = Just (PlayerId.MkPlayerId 0),
      Filter.identity = Just (ObjectId.MkObjectId 7)
    }

-- A colourless (devoid) creature with power 5, no controller recorded.
devoidBigCreature :: Filter.View
devoidBigCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.empty,
      Filter.subtypes = Set.empty,
      Filter.power = Just 5,
      Filter.controller = Nothing,
      Filter.identity = Nothing
    }

self :: Filter.Context
self = Filter.MkContext (Just (PlayerId.MkPlayerId 0)) Nothing

other :: Filter.Context
other = Filter.MkContext (Just (PlayerId.MkPlayerId 1)) Nothing

noPerspective :: Filter.Context
noPerspective = Filter.MkContext Nothing Nothing

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Filter"
    [ HU.testCase "HasCardType matches when present" $
        HU.assertBool "creature" (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Creature)),
      HU.testCase "HasCardType fails when absent" $
        HU.assertBool "not land" (not (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Land))),
      HU.testCase "HasColor matches Black creature" $
        HU.assertBool "black" (Filter.matches self blackCreature (Filter.Type.HasColor Color.Black)),
      HU.testCase "Not HasColor Black is Doom Blade's narrowing" $ do
        HU.assertBool "black is illegal" (not (Filter.matches self blackCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black))))
        HU.assertBool "devoid is legal" (Filter.matches self devoidBigCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black))),
      HU.testCase "And [] is the trivial predicate (matches everything)" $
        HU.assertBool "trivial" (Filter.matches self blackCreature (Filter.Type.And [])),
      HU.testCase "Terror: And of two negated atoms" $ do
        let terror = Filter.Type.And [Filter.Type.Not (Filter.Type.HasColor Color.Black), Filter.Type.Not (Filter.Type.HasCardType CardType.Artifact)]
        HU.assertBool "black creature fails" (not (Filter.matches self blackCreature terror))
        HU.assertBool "devoid creature passes" (Filter.matches self devoidBigCreature terror),
      HU.testCase "Or matches when either arm matches" $
        HU.assertBool "creature or enchantment" (Filter.matches self blackCreature (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment])),
      HU.testCase "PowerAtLeast compares projected power" $ do
        HU.assertBool "power 2 < 4" (not (Filter.matches self blackCreature (Filter.Type.PowerAtLeast 4)))
        HU.assertBool "power 5 >= 4" (Filter.matches self devoidBigCreature (Filter.Type.PowerAtLeast 4)),
      HU.testCase "PowerAtLeast is False when power is Nothing" $ do
        let noPower = blackCreature {Filter.power = Nothing}
        HU.assertBool "no power" (not (Filter.matches self noPower (Filter.Type.PowerAtLeast 1))),
      HU.testCase "ControlledBy You holds for own object" $
        HU.assertBool "you" (Filter.matches self blackCreature (Filter.Type.ControlledBy PlayerRelation.You)),
      HU.testCase "ControlledBy You fails from an opponent's perspective" $
        HU.assertBool "not you" (not (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.You))),
      HU.testCase "ControlledBy Opponent holds across differing players" $
        HU.assertBool "opponent" (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.Opponent)),
      HU.testCase "ControlledBy is False when the object has no controller" $
        HU.assertBool "no controller" (not (Filter.matches self devoidBigCreature (Filter.Type.ControlledBy PlayerRelation.Opponent))),
      HU.testCase "ControlledBy is False when the context has no perspective" $
        HU.assertBool "no perspective" (not (Filter.matches noPerspective blackCreature (Filter.Type.ControlledBy PlayerRelation.You))),
      Tasty.testGroup
        "IsSource"
        [ HU.testCase "matches the context's source"
            . HU.assertBool "is the source"
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7)))
              blackCreature
              Filter.Type.IsSource,
          HU.testCase "does not match a different object"
            . HU.assertBool "not the source"
            . not
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 8)))
              blackCreature
              Filter.Type.IsSource,
          HU.testCase "no source in context is vacuously false"
            . HU.assertBool "no source"
            . not
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) Nothing)
              blackCreature
              Filter.Type.IsSource,
          HU.testCase "no identity in view is vacuously false"
            . HU.assertBool "no identity"
            . not
            $ Filter.matches
              (Filter.MkContext (Just (PlayerId.MkPlayerId 0)) (Just (ObjectId.MkObjectId 7)))
              devoidBigCreature
              Filter.Type.IsSource
        ]
    ]
