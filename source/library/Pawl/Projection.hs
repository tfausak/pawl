module Pawl.Projection where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
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
import qualified Pawl.Type.Quantity as Quantity.Type
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import Pawl.Type.Timestamp (Timestamp)
import qualified Pawl.Type.Toughness as Toughness
import Pawl.Type.TriggeredAbility (TriggeredAbility)
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
  Modification.ChangeSubtypeWord _ _ -> Layer.Text

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
    pc {PC.keywords = Set.empty, PC.activatedAbilities = [], PC.replacementEffects = [], PC.triggeredAbilities = []}
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
  -- CR 612.1/612.2: a text-changing effect replaces one basic land type word with
  -- another where the word is used AS a land type -- here, in the projected type
  -- line. Layer 3, so it folds before layer 4 (Type): a hacked basic Mountain is
  -- an Island by the time mana (CR 305.6) reads its subtypes. Absent `from` is a
  -- no-op.
  Modification.ChangeSubtypeWord from to ->
    if Set.member from (PC.subtypes pc)
      then pc {PC.subtypes = Set.insert to (Set.delete from (PC.subtypes pc))}
      else pc

-- CR 613.4b: layer 7b SETS base P/T to a specific value -- it ESTABLISHES P/T, so
-- an object with no printed P/T that is set (an Opalescence-animated enchantment)
-- gains it. When the effect sets only one axis (Nothing on the other), a base
-- without P/T stays without on that axis (nothing sets it). Contrast addPT (7c),
-- which only MODIFIES and so never gives P/T to something that has none.
setPT :: Maybe Integer -> Maybe Integer -> Maybe Integer
setPT base new = case (base, new) of
  (_, Just n) -> Just n
  (Just b, Nothing) -> Just b
  (Nothing, Nothing) -> Nothing

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
        PC.rulesTextActive = True,
        PC.activatedAbilities = [],
        PC.replacementEffects = [],
        PC.triggeredAbilities = []
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
        PC.rulesTextActive = True,
        PC.activatedAbilities = Card.Type.activatedAbilities card,
        PC.replacementEffects = Card.Type.replacementEffects card,
        PC.triggeredAbilities = Card.Type.triggeredAbilities card
      }

-- affects evaluated against an object's BASE characteristics (used by
-- source-liveness, which must not recurse into the projection it feeds).
affectsBase :: ObjectId -> ObjectId -> Affected.Affected -> GameState -> Bool
affectsBase source oid a gs = affects source oid a (baseCharacteristics oid gs) gs

-- Every SetLandSubtype effect in the game, each with its source and affected set
-- (from stored effects and battlefield permanents' static abilities). This is a
-- legitimate case-on-Modification -- Projection is its sole home.
setLandSubtypeEffects :: GameState -> [(ObjectId, Affected.Affected)]
setLandSubtypeEffects gs =
  let isSet m = case m of
        Modification.SetLandSubtype _ -> True
        _ -> False
      fromStored eff =
        if isSet (ContinuousEffect.modification eff)
          then [(ContinuousEffect.source eff, ContinuousEffect.affected eff)]
          else []
      fromPerm permId = case Game.cardOf permId gs of
        Nothing -> []
        Just card ->
          map (\sa -> (permId, StaticAbility.affected sa)) $
            filter (isSet . StaticAbility.modification) (Card.Type.staticAbilities card)
   in concatMap fromStored (GameState.continuousEffects gs)
        ++ concatMap fromPerm (Set.toList (GameState.battlefield gs))

-- CR 305.7: a land whose subtype is SET to a basic type loses its rules-text
-- abilities. So an object's static abilities are live unless a live SetLandSubtype
-- applies to it. "Live" recurses on the stripper's own source; "applies to" reads
-- BASE characteristics (nonbasic is a printed supertype; card-type Land is
-- unchanged by any M3c effect), so nothing recurses into the projection and the
-- result is order-INDEPENDENT. A cycle trips the visited set (both treated as
-- live -- the CR 613.8b loop-escape analog; expiry in the spec).
staticAbilitiesLive :: ObjectId -> GameState -> Bool
staticAbilitiesLive oid gs = liveGiven (setLandSubtypeEffects gs) Set.empty oid gs

-- The liveness fixpoint given the game's SetLandSubtype effects PRECOMPUTED. The
-- list is hoisted here (rather than recomputed inside) so gather can compute it
-- once per projection instead of once per permanent -- recomputing it per
-- permanent made project O(permanents^3) per state-based-action sweep. An empty
-- list means no stripper exists, so any strips [] is False and oid is live.
liveGiven :: [(ObjectId, Affected.Affected)] -> Set ObjectId -> ObjectId -> GameState -> Bool
liveGiven setEffs visited oid gs =
  Set.member oid visited
    || let visited' = Set.insert oid visited
           strips (src, aff) =
             liveGiven setEffs visited' src gs
               && affectsBase src oid aff gs
        in not (any strips setEffs)

-- Every basic-land-type pair a ChangeSubtypeWord continuous effect imposes on
-- `oid` (CR 612). Stored resolution effects only (a text-change is stored by
-- Resolve's ChangeText, never a static ability at M3d); read against BASE
-- characteristics since ChangeSubtypeWord always uses a TheseObjects fixed set,
-- so no projection recursion is needed and nothing loops.
textChangesAffecting :: ObjectId -> GameState -> [(Subtype.Subtype, Subtype.Subtype)]
textChangesAffecting oid gs =
  let pairOf eff = case ContinuousEffect.modification eff of
        Modification.ChangeSubtypeWord from to ->
          if affects (ContinuousEffect.source eff) oid (ContinuousEffect.affected eff) (baseCharacteristics oid gs) gs
            then Just (from, to)
            else Nothing
        _ -> Nothing
   in Maybe.mapMaybe pairOf (GameState.continuousEffects gs)

-- Apply text-changes to a modification's basic-land-type words (CR 612.1/612.2):
-- SetLandSubtype/AddLandSubtype carry a land-type word; every other modification
-- has none to rewrite here. Projection's charter (it cases on Modification); it is
-- delegated to by Resolve.rewriteEffect for the inner modification of ModifyTarget.
rewriteModification :: [(Subtype.Subtype, Subtype.Subtype)] -> Modification -> Modification
rewriteModification pairs m =
  let swap from to s = if s == from then to else s
      apply1 acc (from, to) = case acc of
        Modification.SetLandSubtype s -> Modification.SetLandSubtype (swap from to s)
        Modification.AddLandSubtype s -> Modification.AddLandSubtype (swap from to s)
        _ -> acc
   in List.foldl' apply1 m pairs

-- Every continuous effect in the game: stored resolution effects, plus the
-- static abilities of every battlefield permanent (CR 613.7a: with the
-- permanent's own timestamp), dropping a permanent whose static abilities are
-- not live (CR 305.7 stripped). NOT filtered by object here -- project filters
-- per layer against the partial.
gather :: GameState -> [Gathered]
gather gs =
  let setEffs = setLandSubtypeEffects gs
      fromStored eff =
        MkGathered
          { gSource = ContinuousEffect.source eff,
            gAffected = ContinuousEffect.affected eff,
            gLayer = layer (ContinuousEffect.modification eff),
            gTimestamp = ContinuousEffect.timestamp eff,
            gModification = ContinuousEffect.modification eff
          }
      stored = map fromStored (GameState.continuousEffects gs)
      fromPermanent permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card ->
            if null setEffs || liveGiven setEffs Set.empty permId gs
              then
                -- Read-point 2 (CR 612): rewrite each static ability's basic-land-
                -- type words by the text-changes affecting THIS source, before its
                -- effect is folded onto any other object. Hack Blood Moon's
                -- SetLandSubtype Mountain -> SetLandSubtype Island.
                let changes = textChangesAffecting permId gs
                    gatherOne sa =
                      let m = rewriteModification changes (StaticAbility.modification sa)
                       in MkGathered
                            { gSource = permId,
                              gAffected = StaticAbility.affected sa,
                              gLayer = layer m,
                              gTimestamp = Object.timestamp permObj,
                              gModification = m
                            }
                 in map gatherOne (Card.Type.staticAbilities card)
              else []
      static_ = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
   in stored ++ static_ ++ counterGathered gs

-- CR 122.1a / 613.4c: a +1/+1 counter adds +1/+1 and a -1/-1 counter adds -1/-1,
-- in layer 7c. Emit each battlefield object's counters as ONE synthetic 7c
-- ModifyPowerToughness with net delta d = (#PlusOnePlusOne - #MinusOneMinusOne) on
-- each axis, folded by the same path as Giant Growth. Constructed HERE (Projection
-- is the sole home that may name a Modification constructor). Layer 7c is purely
-- additive, so pre-combining the counters and the object's own timestamp are both
-- unobservable (spec section 4). d == 0 emits nothing.
counterGathered :: GameState -> [Gathered]
counterGathered gs = Maybe.mapMaybe fromObject (Set.toList (GameState.battlefield gs))
  where
    fromObject oid = case Game.lookupObject oid gs of
      Nothing -> Nothing
      Just obj ->
        let cs = Object.counters obj
            plus = toInteger (Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs)
            minus = toInteger (Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs)
            d = plus - minus
         in if d == 0
              then Nothing
              else
                Just
                  MkGathered
                    { gSource = oid,
                      gAffected = Affected.TheseObjects (Set.singleton oid),
                      gLayer = Layer.ModifyPT,
                      gTimestamp = Object.timestamp obj,
                      gModification = Modification.ModifyPowerToughness (Quantity.Type.Literal d) (Quantity.Type.Literal d)
                    }

-- CR 613: apply continuous effects layer by layer (only the layers with effects,
-- ascending). Within a layer, CR 613.7 timestamp order. An effect's affected set
-- is evaluated against the partial projection through the previous layers.
-- CR 613.8 EXISTENCE dependency is handled by source-liveness in Task 6, not a
-- within-layer reorder; the topological CR 613.8b applies-to reorder is deferred
-- (spec §6, git-bug). design.md §2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs = projectFrom (gather gs) oid gs

-- Project one object against a PRECOMPUTED candidate list. gather is
-- oid-independent, so a whole-board sweep gathers once and folds each object
-- (projectAll) instead of re-gathering per object.
projectFrom :: [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectFrom cands oid gs =
  let layers = Set.toAscList (Set.fromList (map gLayer cands))
      applyLayer partial lyr =
        let here = filter (\c -> gLayer c == lyr && affects (gSource c) oid (gAffected c) partial gs) cands
            -- CR 613.7 timestamp order within a layer. EXPIRES: CR 613.8b dependency
            -- (a same-layer effect that changes which objects another applies to)
            -- would override this. Deferred -- no M3c card falsifies it; existence
            -- dependencies are handled by staticAbilitiesLive. git-bug f90e0c4.
            ordered = List.sortOn gTimestamp here
            step pc c = applyModification gs oid (gModification c) pc
         in List.foldl' step partial ordered
   in List.foldl' applyLayer (baseCharacteristics oid gs) layers

-- Project every battlefield object against ONE gather: O(gather + P*fold) instead
-- of the O(P*(gather+fold)) of calling project per object. The hot path for SBA
-- sweeps and combat, which query many objects against the same state.
projectAll :: GameState -> Map ObjectId ProjectedCharacteristics
projectAll gs =
  let cands = gather gs
   in Map.fromSet (\oid -> projectFrom cands oid gs) (GameState.battlefield gs)

powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf oid gs = PC.power (project oid gs)

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = PC.toughness (project oid gs)

keywordsOf :: ObjectId -> GameState -> Set Keyword
keywordsOf oid gs = PC.keywords (project oid gs)

-- CR 602 / 613.1f: an object's activated abilities after the layer system, the
-- same projection posture as keywordsOf. A Humility'd creature has none.
abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesOf oid gs = PC.activatedAbilities (project oid gs)

-- CR 614 / 613 layer 6: an object's replacement effects after the layer system,
-- the same projection posture as abilitiesOf. A Humility'd creature has none.
replacementsOf :: ObjectId -> GameState -> [ReplacementEffect]
replacementsOf oid gs = PC.replacementEffects (project oid gs)

-- CR 614.6: every replacement effect active on the battlefield. Short-circuits
-- when no permanent has one in its base card, so an ordinary zone change (a draw,
-- a land entering) does NOT project the whole board -- projection runs only once
-- a replacement source is actually present.
replacementsAffecting :: GameState -> [ReplacementEffect]
replacementsAffecting gs =
  let onBattlefield = Set.toList (GameState.battlefield gs)
      baseHas oid = case Game.cardOf oid gs of
        Nothing -> False
        Just card -> not (null (Card.Type.replacementEffects card))
   in if not (any baseHas onBattlefield)
        then []
        else concatMap (\oid -> replacementsOf oid gs) onBattlefield

-- CR 603 / 613 layer 6: an object's triggered abilities after the layer system,
-- the same projection posture as abilitiesOf. A Humility'd creature has none.
triggeredAbilitiesOf :: ObjectId -> GameState -> [TriggeredAbility Card.Type.Card]
triggeredAbilitiesOf oid gs = PC.triggeredAbilities (project oid gs)

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
