module Pawl.Projection where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
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
import qualified Pawl.Type.Supertype as Supertype
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
  Modification.SetLandSubtype _ -> Layer.Type
  Modification.AddLandSubtype _ -> Layer.Type
  Modification.AddCardType _ -> Layer.Type

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
  Modification.AddLandSubtype s ->
    pc {PC.subtypes = Set.insert s (PC.subtypes pc)}
  Modification.AddCardType t ->
    pc {PC.cardTypes = Set.insert t (PC.cardTypes pc)}
  -- CR 305.7: setting a land's subtype to a basic type removes its old land
  -- types AND strips its rules-text abilities (here: keywords and, via
  -- rulesTextActive, its static abilities -- see gather). It gains the new mana
  -- ability from the subtype (CR 305.6, read at the mana call site).
  Modification.SetLandSubtype s ->
    pc
      { PC.subtypes = Set.singleton s,
        PC.keywords = Set.empty,
        PC.rulesTextActive = False
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

-- A continuous effect ready to fold: its source (for OtherNonAuraEnchantments
-- self-exclusion), the set it affects, its layer, its timestamp, and the
-- modification. Projection-internal; not a domain type.
data Gathered = MkGathered
  { gSource :: ObjectId,
    gAffected :: Affected.Affected,
    gLayer :: Layer,
    gTimestamp :: Timestamp,
    gModification :: Modification
  }

-- CR 611.2c / 613: does the effect from `source` apply to `oid`, given the
-- partial projection built by the layers below this one? Fixed sets are a
-- membership test; dynamic sets read the PARTIAL type line, so a layer-4 type
-- change is visible to a later layer. Supertype (nonbasic) is read from the
-- printed type line -- no effect changes a supertype at M3c.
affects :: ObjectId -> ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affects source oid a partial gs =
  let onBattlefield = Set.member oid (GameState.battlefield gs)
      hasType t = Set.member t (PC.cardTypes partial)
   in case a of
        Affected.TheseObjects s -> Set.member oid s
        Affected.AllCreatures -> onBattlefield && hasType CardType.Creature
        Affected.AllLands -> onBattlefield && hasType CardType.Land
        Affected.AllNonbasicLands -> onBattlefield && hasType CardType.Land && not (isBasic oid gs)
        Affected.OtherNonAuraEnchantments -> onBattlefield && oid /= source && hasType CardType.Enchantment

-- CR 205.4a: a basic land is one with the Basic supertype. Read from the printed
-- type line (supertypes are not projected at M3c).
isBasic :: ObjectId -> GameState -> Bool
isBasic oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> Set.member Supertype.Basic (TypeLine.supertypes (Card.Type.typeLine card))

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

-- Every continuous effect in the game: stored resolution effects, plus the
-- static abilities of every battlefield permanent (CR 613.7a: with the
-- permanent's own timestamp). NOT filtered by object here -- project filters
-- per layer against the partial. (Task 6 adds the CR 305.7 source-liveness gate.)
gather :: GameState -> [Gathered]
gather gs =
  let fromStored eff =
        MkGathered
          { gSource = ContinuousEffect.source eff,
            gAffected = ContinuousEffect.affected eff,
            gLayer = layer (ContinuousEffect.modification eff),
            gTimestamp = ContinuousEffect.timestamp eff,
            gModification = ContinuousEffect.modification eff
          }
      stored = map fromStored (GameState.continuousEffects gs)
      fromStatic permId permObj sa =
        MkGathered
          { gSource = permId,
            gAffected = StaticAbility.affected sa,
            gLayer = layer (StaticAbility.modification sa),
            gTimestamp = Object.timestamp permObj,
            gModification = StaticAbility.modification sa
          }
      fromPermanent permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card -> map (fromStatic permId permObj) (Card.Type.staticAbilities card)
      static_ = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
   in stored ++ static_

-- CR 613: apply continuous effects layer by layer (only the layers with effects,
-- ascending). Within a layer, CR 613.7 timestamp order. An effect's affected set
-- is evaluated against the partial projection through the previous layers.
-- CR 613.8 EXISTENCE dependency is handled by source-liveness in Task 6, not a
-- within-layer reorder; the topological CR 613.8b applies-to reorder is deferred
-- (spec §6, git-bug). design.md §2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs =
  let cands = gather gs
      layers = Set.toAscList (Set.fromList (map gLayer cands))
      applyLayer partial lyr =
        let here = filter (\c -> gLayer c == lyr && affects (gSource c) oid (gAffected c) partial gs) cands
            ordered = List.sortOn gTimestamp here
            step pc c = applyModification gs oid (gModification c) pc
         in List.foldl' step partial ordered
   in List.foldl' applyLayer (baseCharacteristics oid gs) layers

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
