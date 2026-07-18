module Pawl.Projection where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Type.Card as Card.Type
import Pawl.Type.GameState (GameState)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Toughness as Toughness

-- The projected characteristics of an object. Task 4 makes these a layer fold;
-- for now they are the base printing, moved out of Pawl.Game so the projection
-- has one home (and so Game does not depend on the projection -- Projection
-- depends on Game, not the reverse).
powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf oid gs = case fmap Card.Type.power (Game.cardOf oid gs) of
  Just (Just (Power.MkPower quantity)) -> Quantity.evaluate gs oid quantity
  _ -> Nothing

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = case fmap Card.Type.toughness (Game.cardOf oid gs) of
  Just (Just (Toughness.MkToughness quantity)) -> Quantity.evaluate gs oid quantity
  _ -> Nothing

keywordsOf :: ObjectId -> GameState -> Set Keyword
keywordsOf oid gs = maybe Set.empty Card.Type.keywords (Game.cardOf oid gs)

hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword keyword oid gs = Set.member keyword (keywordsOf oid gs)
