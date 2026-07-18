module Pawl.Projection where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.Layer (Layer)
import qualified Pawl.Type.Layer as Layer
import Pawl.Type.Modification (Modification)
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Power as Power
import Pawl.Type.ProjectedCharacteristics (ProjectedCharacteristics)
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import Pawl.Type.Timestamp (Timestamp)
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TypeLine as TypeLine

-- CR 613.1: the layer a modification applies in. THE ABI classification the
-- rules core would ask -- never the modification's identity. One of two case-on-
-- Modification functions this module is the sole home of.
layer :: Modification -> Layer
layer m = case m of
  Modification.GainKeyword _ -> Layer.Ability
  Modification.LoseAllAbilities -> Layer.Ability
  Modification.SetBasePowerToughness _ _ -> Layer.SetPT
  Modification.ModifyPowerToughness _ _ -> Layer.ModifyPT

-- Apply one modification to characteristics-in-progress. THE ONE applier
-- (Resolve : Effect :: Projection : Modification). P/T quantities are evaluated
-- here against the state; CR 611.2b's freeze-at-creation is a no-op while every
-- Quantity is a Literal (identical value either way). When X lands, Resolve must
-- freeze the value into the stored effect and this reads the frozen Literal.
applyModification :: GameState -> ObjectId -> Modification -> ProjectedCharacteristics -> ProjectedCharacteristics
applyModification gs oid m pc = case m of
  Modification.GainKeyword k ->
    pc {PC.keywords = Set.insert k (PC.keywords pc)}
  Modification.LoseAllAbilities ->
    pc {PC.keywords = Set.empty}
  Modification.SetBasePowerToughness p t ->
    pc
      { PC.power = setPT (PC.power pc) (Quantity.evaluate gs oid p),
        PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate gs oid t)
      }
  Modification.ModifyPowerToughness p t ->
    pc
      { PC.power = addPT (PC.power pc) (Quantity.evaluate gs oid p),
        PC.toughness = addPT (PC.toughness pc) (Quantity.evaluate gs oid t)
      }

-- Layer 7b sets P/T only on an object that HAS P/T; a land stays without.
setPT :: Maybe Integer -> Maybe Integer -> Maybe Integer
setPT base new = case (base, new) of
  (Just _, Just n) -> Just n
  (Just b, Nothing) -> Just b
  (Nothing, _) -> Nothing

-- Layer 7c adds; an unevaluable delta leaves the value, a land stays without.
addPT :: Maybe Integer -> Maybe Integer -> Maybe Integer
addPT base delta = case (base, delta) of
  (Just b, Just d) -> Just (b + d)
  (Just b, Nothing) -> Just b
  (Nothing, _) -> Nothing

-- CR 611.2c: does this effect's set include the object? A fixed set is a
-- membership test; AllCreatures is re-evaluated live (creatures on the
-- battlefield).
affects :: ObjectId -> Affected.Affected -> GameState -> Bool
affects oid a gs = case a of
  Affected.TheseObjects s -> Set.member oid s
  Affected.AllCreatures ->
    Set.member oid (GameState.battlefield gs)
      && fmap Card.isCreature (Game.cardOf oid gs) == Just True

-- Printed characteristics before any effect (CR 613.2/613.4 starting point).
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.cardOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { PC.keywords = Set.empty,
        PC.power = Nothing,
        PC.toughness = Nothing,
        PC.cardTypes = Set.empty,
        PC.subtypes = Set.empty,
        PC.rulesTextActive = True
      }
  Just card ->
    PC.MkProjectedCharacteristics
      { PC.keywords = Card.Type.keywords card,
        PC.power = case Card.Type.power card of
          Nothing -> Nothing
          Just (Power.MkPower q) -> Quantity.evaluate gs oid q,
        PC.toughness = case Card.Type.toughness card of
          Nothing -> Nothing
          Just (Toughness.MkToughness q) -> Quantity.evaluate gs oid q,
        PC.cardTypes = TypeLine.types (Card.Type.typeLine card),
        PC.subtypes = TypeLine.subtypes (Card.Type.typeLine card),
        PC.rulesTextActive = True
      }

-- Every continuous effect touching this object, from BOTH sources, tagged with
-- its layer and timestamp: stored resolution effects, plus the static abilities
-- of every battlefield permanent (CR 613.7a: a static ability's continuous
-- effect has the same timestamp as the object it is on).
gather :: ObjectId -> GameState -> [(Layer, Timestamp, Modification)]
gather oid gs =
  let fromStored eff =
        if affects oid (ContinuousEffect.affected eff) gs
          then [(layer (ContinuousEffect.modification eff), ContinuousEffect.timestamp eff, ContinuousEffect.modification eff)]
          else []
      stored = concatMap fromStored (GameState.continuousEffects gs)
      fromStatic permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card ->
            let one sa =
                  if affects oid (StaticAbility.affected sa) gs
                    then [(layer (StaticAbility.modification sa), Object.timestamp permObj, StaticAbility.modification sa)]
                    else []
             in concatMap one (Card.Type.staticAbilities card)
      static_ = concatMap fromStatic (Set.toList (GameState.battlefield gs))
   in stored ++ static_

-- CR 613: apply every continuous effect to the base characteristics in layer
-- order, ties broken by CR 613.7 timestamp. A linear fold -- no CR 613.8
-- dependency (that is M3c's trial application). design.md §2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs =
  let sorted = List.sortOn (\(l, ts, _) -> (l, ts)) (gather oid gs)
      step pc (_, _, m) = applyModification gs oid m pc
   in List.foldl' step (baseCharacteristics oid gs) sorted

powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf oid gs = PC.power (project oid gs)

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = PC.toughness (project oid gs)

keywordsOf :: ObjectId -> GameState -> Set Keyword
keywordsOf oid gs = PC.keywords (project oid gs)

subtypesOf :: ObjectId -> GameState -> Set Subtype.Subtype
subtypesOf oid gs = PC.subtypes (project oid gs)

cardTypesOf :: ObjectId -> GameState -> Set CardType.CardType
cardTypesOf oid gs = PC.cardTypes (project oid gs)

-- CR 305.2 / 613.1d: creature-ness is the projected card-type question, the same
-- projection posture as keywordsOf. An Opalescence'd enchantment is a creature.
isCreatureOf :: ObjectId -> GameState -> Bool
isCreatureOf oid gs = Set.member CardType.Creature (cardTypesOf oid gs)

hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword keyword oid gs = Set.member keyword (keywordsOf oid gs)

-- CR 514.2: during cleanup, "all 'until end of turn' and 'this turn' effects
-- end". Delete-and-recompute (design.md §2.5): dropping the stored effect makes
-- the next projection revert -- nothing is explicitly undone.
dropEndOfTurnEffects :: GameState -> GameState
dropEndOfTurnEffects gs =
  let keep eff = ContinuousEffect.duration eff /= Duration.UntilEndOfTurn
   in gs {GameState.continuousEffects = filter keep (GameState.continuousEffects gs)}
