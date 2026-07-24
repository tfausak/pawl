module Pawl.Projection where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.Exclusion as Exclusion
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Keyword (Keyword)
import qualified Pawl.Type.Keyword as Keyword
import Pawl.Type.Layer (Layer)
import qualified Pawl.Type.Layer as Layer
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import Pawl.Type.Modification (Modification)
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.PlayerId as PlayerId
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
  Modification.SetController _ -> Layer.Control
  Modification.SetColor _ -> Layer.Color
  Modification.SwitchPowerToughness -> Layer.SwitchPT

-- Apply one modification to characteristics-in-progress. THE ONE applier
-- (Resolve : Effect :: Projection : Modification). P/T quantities are evaluated
-- here against the CURRENT state, which is correct for a static ability's
-- continuous effect (CR 604.2 -- Opalescence's mana value is re-read per affected
-- object every projection). A continuous effect created by a spell's RESOLUTION
-- must not be re-read (CR 608.2h / 611.2d); it is frozen to Literals at store
-- time by Resolve, via freezeQuantities.
--
-- The `you` passed to Quantity.evaluate here is the AFFECTED object's controller.
-- That is correct for a characteristic-defining ability (CR 604.3a(3): a CDA does
-- not affect other objects, so its "you" is the object's own controller). It is
-- WRONG for a static ability carrying a player-scoped Count -- "the number of
-- cards in your hand" on a static ability means the SOURCE's controller's hand,
-- and Gathered.gSource is dropped before this function sees it, so the right
-- player is not available here at all (#34). This is the static-ability twin of
-- the stored-effect residual documented in freezeQuantities.
applyModification :: GameState -> ObjectId -> Modification -> ProjectedCharacteristics -> ProjectedCharacteristics
applyModification gs oid m pc = case m of
  Modification.GainKeyword k ->
    pc {PC.keywords = Set.insert k (PC.keywords pc)}
  -- CR 604.3: a characteristic-defining ability IS a static ability, so losing
  -- all abilities loses it too. Layer 6, which is BEFORE 7a -- the reason the CDA
  -- is folded in place from the partial rather than gathered up front.
  Modification.LoseAllAbilities ->
    pc
      { PC.keywords = Set.empty,
        PC.characteristicPT = Nothing,
        PC.activatedAbilities = [],
        PC.replacementEffects = [],
        PC.triggeredAbilities = []
      }
  Modification.SetBasePowerToughness p t ->
    pc
      { PC.power = setPT (PC.power pc) (Quantity.evaluate gs oid (controllerOf oid gs) p),
        PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate gs oid (controllerOf oid gs) t)
      }
  Modification.ModifyPowerToughness p t ->
    pc
      { PC.power = addPT (PC.power pc) (Quantity.evaluate gs oid (controllerOf oid gs) p),
        PC.toughness = addPT (PC.toughness pc) (Quantity.evaluate gs oid (controllerOf oid gs) t)
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
  -- CR 613.1b layer 2: control-changing effects apply here, but
  -- ProjectedCharacteristics carries no controller field -- controllerOf reads
  -- GameState.continuousEffects directly (a lean fold, not the full layer pass;
  -- see its comment). This arm is identity so gather/project's uniform walk over
  -- every stored effect stays total once a SetController effect exists.
  Modification.SetController _ -> pc
  Modification.SetColor cs ->
    -- CR 105.3: the new colours replace all previous ones.
    pc {PC.colors = cs}
  -- CR 613.4d: "take the value of power and apply it to the creature's toughness,
  -- and take the value of toughness and apply it to the creature's power."
  Modification.SwitchPowerToughness ->
    pc {PC.power = PC.toughness pc, PC.toughness = PC.power pc}

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

-- A continuous effect ready to fold: its source (for the Matching ExcludesSource
-- self-exclusion, CR 305.2), the set it affects, its layer, its timestamp, and the
-- modification. Projection-internal; not a domain type.
data Gathered = MkGathered
  { gSource :: ObjectId,
    gAffected :: Affected.Affected,
    gLayer :: Layer,
    gTimestamp :: Timestamp,
    gModification :: Modification
  }

-- CR 611.2c / 613: does the effect from `source` apply to `oid`, given the
-- PARTIAL projection built by the layers below this one? A fixed set is a
-- membership test; a dynamic set is a Filter evaluated against the PARTIAL
-- characteristics, so a layer-4 type change is visible to a later layer.
-- ExcludesSource applies CR 305.2's "each other" (Opalescence does not animate
-- itself). CR 109.5: an affected-set filter's "you" is the effect's SOURCE's
-- controller (the perspective), which ControlledBy compares against the affected
-- object's own controller (the View's controller) -- the emblem anthem's
-- "creatures you control" is the first affected set to reference a player.
-- Supertypes read from the printed type line (CR 205.4a; not projected at M3c)
-- via viewOfCharacteristics.
affects :: ObjectId -> ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affects source oid a partial gs = case a of
  Affected.TheseObjects s -> Set.member oid s
  Affected.Matching exclusion f ->
    let notExcluded = case exclusion of
          Exclusion.ExcludesSource -> oid /= source
          Exclusion.IncludesSource -> True
        -- CR 109.5: "you" on a continuous effect is the effect's SOURCE's
        -- controller; ControlledBy compares the affected object's controller to
        -- it. controllerOf is a lean fold (owner overridden by SetController
        -- effects) that never recurses into project, so calling it here cannot
        -- loop.
        perspective = controllerOf source gs
     in Set.member oid (GameState.battlefield gs)
          && notExcluded
          && Filter.matches (Filter.MkContext perspective (Just source)) (viewOfCharacteristics oid partial (controllerOf oid gs) gs) f

-- CR 205.4a: supertypes are read from the printed type line (no modelled effect
-- changes a supertype). Empty when the object has no underlying card.
printedSupertypes :: ObjectId -> GameState -> Set Supertype.Supertype
printedSupertypes oid gs = case Game.cardOf oid gs of
  Nothing -> Set.empty
  Just card -> TypeLine.supertypes (Card.Type.typeLine card)

-- The characteristics view of a battlefield/stack object: its projection (CR 613
-- layer system, so a colour-changer or type-changer is seen), its printed
-- supertypes (supertypes are not projected -- CR 205.4a basic-ness is read from
-- the printed type line), and its projected controller (CR 613.1b; Nothing when
-- the id is unknown -- e.g. a source that has left the battlefield).
viewOfObject :: ObjectId -> GameState -> Filter.View
viewOfObject oid gs = viewOfCharacteristics oid (project oid gs) (controllerOf oid gs) gs

-- The characteristics view of a PRINTED card off the battlefield (a card in a
-- library/graveyard/hand being matched by a search). No projection exists off the
-- battlefield, so every axis is read from the printed card: types/supertypes/
-- subtypes from the type line, colours from baseColorsOf (devoid -> empty), and
-- power/controller are Nothing (a card in a library has neither under the rules
-- that matter here). This is what lets a Filter read an object's colour outside
-- the battlefield without a projection that does not exist there.
viewOfCard :: Card.Type.Card -> Filter.View
viewOfCard card =
  let typeLine = Card.Type.typeLine card
   in Filter.MkView
        { Filter.cardTypes = TypeLine.types typeLine,
          Filter.supertypes = TypeLine.supertypes typeLine,
          Filter.colors = baseColorsOf card,
          Filter.subtypes = TypeLine.subtypes typeLine,
          Filter.power = Nothing,
          Filter.controller = Nothing,
          -- A printed card off the battlefield is not an object, so it has no
          -- identity for IsSource to compare -- the same vacuous posture power
          -- and controller already take here.
          Filter.identity = Nothing
        }

-- Shared assembly: fill a View from a projection's characteristics plus the
-- printed supertypes (not projected) and a supplied controller.
viewOfCharacteristics :: ObjectId -> ProjectedCharacteristics -> Maybe PlayerId.PlayerId -> GameState -> Filter.View
viewOfCharacteristics oid pc controller gs =
  Filter.MkView
    { Filter.cardTypes = PC.cardTypes pc,
      Filter.supertypes = printedSupertypes oid gs,
      Filter.colors = PC.colors pc,
      Filter.subtypes = PC.subtypes pc,
      Filter.power = PC.power pc,
      Filter.controller = controller,
      Filter.identity = Just oid
    }

-- CR 707.2 / 613.1a: an object's layer-1 (copy) result -- the value the layer fold
-- STARTS from. If the object carries a copy snapshot in its bindings (stamped as it
-- entered, CR 707.5, by Replacement.apply's EntryR AsCopy arm), that snapshot IS its copiable
-- value; otherwise it is the printed base. Only base-or-snapshot, so counters (7c),
-- pumps (7c), control (2), and ability grants (6) -- all folded ABOVE this -- are
-- never part of a copied object's own copiable value (the P2 falsifier, made
-- structural). Not a recursion: a copy of a copy stores the underlying creature's
-- values at entry, so the snapshot is already resolved.
copiableCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
copiableCharacteristics oid gs =
  case Game.lookupObject oid gs >>= (Binding.copyOf . Object.bindings) of
    Just snapshot -> snapshot
    Nothing -> baseCharacteristics oid gs

-- CR 208.2 / 604.3: the card's characteristic-defining P/T as a pair of
-- quantities, with the printed star resolved to what the CDA counts. Nothing
-- unless the card declares a CDA *and* has a printed power and toughness box for
-- the star to sit in (CR 208.1) -- a card with one and not the other is
-- malformed data, and yields no CDA rather than a partial one.
seedCharacteristicPT :: Card.Type.Card -> Maybe (Quantity.Type.Quantity, Quantity.Type.Quantity)
seedCharacteristicPT card =
  case (Card.Type.characteristicPT card, Card.Type.power card, Card.Type.toughness card) of
    (Just star, Just (Power.MkPower p), Just (Toughness.MkToughness t)) ->
      Just (Quantity.substituteStar star p, Quantity.substituteStar star t)
    _ -> Nothing

-- Printed characteristics before any effect (CR 613.2/613.4 starting point).
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.cardOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { PC.keywords = Set.empty,
        PC.colors = Set.empty,
        PC.power = Nothing,
        PC.toughness = Nothing,
        PC.characteristicPT = Nothing,
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
        PC.colors = baseColorsOf card,
        PC.power = case Card.Type.power card of
          Nothing -> Nothing
          Just (Power.MkPower q) -> Quantity.evaluate gs oid (controllerOf oid gs) q,
        PC.toughness = case Card.Type.toughness card of
          Nothing -> Nothing
          Just (Toughness.MkToughness q) -> Quantity.evaluate gs oid (controllerOf oid gs) q,
        PC.characteristicPT = seedCharacteristicPT card,
        PC.cardTypes = TypeLine.types (Card.Type.typeLine card),
        PC.subtypes = TypeLine.subtypes (Card.Type.typeLine card),
        PC.rulesTextActive = True,
        PC.activatedAbilities = Card.Type.activatedAbilities card,
        PC.replacementEffects = Card.Type.replacementEffects card,
        PC.triggeredAbilities = Card.Type.triggeredAbilities card
      }

-- CR 202.2 / 204.2: an object's PRINTED colours -- the colours of the coloured
-- mana symbols in its mana cost, together with the colours its colour indicator
-- denotes. CR 202.2b: an object with no coloured mana symbols and no indicator is
-- colourless.
--
-- CR 702.114a: devoid is a CHARACTERISTIC-DEFINING ability meaning "this object is
-- colourless", and it wins over both sources above.
--
-- Devoid is applied HERE, at the seed, rather than as a CDA inside layer 5. CR
-- 613.3 says that within layers 2-6 characteristic-defining abilities apply first
-- and only then other effects in timestamp order, which would mean a precedence
-- key on Gathered. That machinery is not built because the two orderings are
-- observably indistinguishable for everything IN THE CARD POOL TODAY (not
-- "everything this engine can reach" -- see the fifth bullet, which names the
-- gap the first four don't cover):
--
--   * every layer-5 effect in the vocabulary is SetColor, which REPLACES (CR
--     105.3), so "CDA first, then the replacers" and "CDA before layer 5, then the
--     replacers" agree on the final set, always;
--   * a copy of a devoid object snapshots the printed Devoid keyword among its
--     copiable values (CR 613.2c), so the copy recomputes colourless from its own
--     seed;
--   * Humility's LoseAllAbilities is layer 6, AFTER layer 5, and CR 613.8a scopes
--     dependency to effects in the same layer -- so a Humility'd devoid object
--     stays colourless under either ordering;
--   * CR 604.3: a CDA functions in ALL zones. The seed is computed from the card
--     and is zone-independent; a battlefield-only gather pass would not be.
--   * the four bullets above all reason about what WRITES colour. None covers
--     what READS it. Seeding devoid also moves it earlier relative to colour
--     READERS: under CR 613.3, devoid applies at the START of layer 5, so at
--     layers 2, 3 and 4 the CR says a devoid object with {B} in its mana cost is
--     still black, while this seed-based implementation already says colourless.
--     A Matching Exclusion (And [HasCardType Creature, HasColor c]) affected set
--     (this phase's Affected/Filter) makes that gap expressible open-half data
--     TODAY: a card pairing {"affected": {"type":"Matching", ...}} with a
--     layer-4 AddCardType, a layer-3 ChangeSubtypeWord, or a layer-2 SetController
--     would have its affected set evaluated against PC.colors with devoid already
--     applied, which is the wrong answer per 613.3. No card in the pool does
--     this, so it stays unobserved -- but it is the case that retires this
--     shortcut, not a hypothetical.
--
-- The CR 613.3 CDA-vs-timestamp precedence key this would need is not built, and
-- neither is the colour-keyed-affected-set path of the fifth bullet (#35). P3a's
-- spec section 2.2 carries the full argument.
--
-- P3b does NOT reopen this question. Devoid is a CONSTANT CDA (its value doesn't
-- depend on game state), so seeding it is sound: a copy snapshot recomputes the
-- same constant. Tarmogoyf's characteristic-defining P/T (P3b, layer 7a) is a
-- DYNAMIC CDA -- it reads the graveyards' card types, which change over time.
-- Seeding a dynamic CDA would freeze it into Binding.copy at entry (see
-- Engine.hs's as-enters drain, which snapshots Projection.copiableCharacteristics)
-- -- a Clone of a Tarmogoyf would keep whatever P/T the graveyards held at the
-- moment it entered, instead of recomputing, which violates CR 707.2 (a copy
-- acquires the ABILITY, not its computed value). So P3b must fold Tarmogoyf's
-- CDA in-place at Layer.CharacteristicPT (7a, which already exists in
-- Pawl.Type.Layer as the CR's own dedicated sublayer for this), not at the seed.
-- The precedent below (baseCharacteristics already evaluating a printed `*` P/T
-- at the seed via Quantity.evaluate) is harmless ONLY for the one card that
-- has `*` P/T with NO characteristic-defining ability behind it -- Primal Plasma
-- (P5), whose star is given its value by an as-enters REPLACEMENT (CR 208.2b),
-- not by a CDA. Quantity.evaluate returns Nothing for a bare Star, so such a card
-- projects NO power or toughness until its entry choice applies, where CR 208.2b
-- says to use 0. That is unobservable on the battlefield -- the entry loop always
-- applies the choice before the Moved event exists -- but a Primal Plasma CARD in
-- a hand, library or graveyard reports Nothing where the rule says 0 (#76).
baseColorsOf :: Card.Type.Card -> Set Color.Color
baseColorsOf card =
  if Set.member Keyword.Devoid (Card.Type.keywords card)
    then Set.empty
    else
      Set.union
        (Card.Type.colorIndicator card)
        (manaCostColors (Card.Type.manaCost card))

-- CR 202.1b: a land has no mana cost at all, so it contributes no colours.
manaCostColors :: Maybe ManaCost.ManaCost -> Set Color.Color
manaCostColors mc = case mc of
  Nothing -> Set.empty
  Just (ManaCost.MkManaCost symbols) -> Set.fromList (Maybe.mapMaybe symbolColor symbols)

-- CR 202.2b: only a coloured mana symbol carries a colour ("Objects with no
-- colored mana symbols in their mana costs are colorless"). Generic ({2}), {X},
-- and the colourless symbol ({C}) carry none -- {C} is colourless mana, and
-- CR 105.2c says colourless is not a colour.
symbolColor :: ManaSymbol.ManaSymbol -> Maybe Color.Color
symbolColor symbol = case symbol of
  ManaSymbol.OfType (ManaType.Colored c) -> Just c
  ManaSymbol.OfType ManaType.Colorless -> Nothing
  ManaSymbol.Generic _ -> Nothing
  ManaSymbol.Variable -> Nothing

-- affects evaluated against an object's BASE characteristics (used by
-- source-liveness, which must not recurse into the projection it feeds).
affectsBase :: ObjectId -> ObjectId -> Affected.Affected -> GameState -> Bool
affectsBase source oid a gs = affects source oid a (baseCharacteristics oid gs) gs

-- CR 608.2h / 611.2d: evaluate a modification's quantities ONCE and rewrite them
-- to Literals. Called by Resolve when a spell's resolution STORES a continuous
-- effect -- "if an effect requires information from the game ... the answer is
-- determined only once, when the effect is applied."
--
-- `oid` is the SOURCE (the resolving spell), not the affected object: the source
-- is what holds a chosen X in its bindings, and `you` is the source's controller,
-- whose hand a player-scoped count counts.
--
-- Deliberately NOT applied to a static ability's effect: CR 611.2 scopes 611.2a-d
-- to "a continuous effect generated by the resolution of a spell or ability", and
-- a static ability's effect (CR 604.2) is regenerated every projection and
-- evaluated per affected object -- Opalescence's mana value must keep moving.
--
-- Cases on Modification, so it lives HERE (Projection is the sole home), the same
-- standing rewriteModification has. An unevaluable quantity is left alone.
freezeQuantities :: GameState -> ObjectId -> Maybe PlayerId.PlayerId -> Modification -> Modification
freezeQuantities gs oid you m =
  -- The Nothing fallback leaves the quantity in the store rather than dropping
  -- the effect. That is deliberate, but it leaves a RESIDUAL: an unevaluable
  -- quantity (an X with no binding on the source, or a bare Star) survives into
  -- the stored effect, where applyModification later evaluates it against the
  -- AFFECTED object and that object's controller -- the very mis-evaluation this
  -- freeze exists to prevent (#36).
  let freeze q = maybe q Quantity.Type.Literal $ Quantity.evaluate gs oid you q
   in case m of
        Modification.SetBasePowerToughness p t -> Modification.SetBasePowerToughness (freeze p) (freeze t)
        Modification.ModifyPowerToughness p t -> Modification.ModifyPowerToughness (freeze p) (freeze t)
        -- Every other modification carries no quantity to freeze; named
        -- explicitly per Modification's exhaustiveness discipline.
        Modification.GainKeyword _ -> m
        Modification.LoseAllAbilities -> m
        Modification.SetLandSubtype _ -> m
        Modification.AddLandSubtype _ -> m
        Modification.AddCardType _ -> m
        Modification.ChangeSubtypeWord _ _ -> m
        Modification.SetController _ -> m
        Modification.SetColor _ -> m
        Modification.SwitchPowerToughness -> m

-- Every SetLandSubtype effect in the game, each with its source and affected set
-- (from stored effects and battlefield permanents' static abilities). This is a
-- legitimate case-on-Modification -- Projection is its sole home.
setLandSubtypeEffects :: GameState -> [(ObjectId, Affected.Affected)]
setLandSubtypeEffects gs =
  let isSet m = case m of
        Modification.SetLandSubtype _ -> True
        -- Not the CR 305.7 land-subtype "set" this predicate gates (a control
        -- op, not a type change); the existing wildcard already covers it, but
        -- named explicitly per Modification's exhaustiveness discipline.
        Modification.SetController _ -> False
        _ -> False
      fromStored eff =
        if isSet (ContinuousEffect.modification eff)
          then [(ContinuousEffect.source eff, ContinuousEffect.affected eff)]
          else []
      fromPerm permId = case Game.cardOf permId gs of
        Nothing -> []
        Just card ->
          fmap (\sa -> (permId, StaticAbility.affected sa)) $
            filter (isSet . StaticAbility.modification) (Card.Type.staticAbilities card)
   in concatMap fromStored (GameState.continuousEffects gs)
        <> concatMap fromPerm (Set.toList (GameState.battlefield gs))

-- CR 305.7: a land whose subtype is SET to a basic type loses its rules-text
-- abilities. So an object's static abilities are live unless a live SetLandSubtype
-- applies to it. "Live" recurses on the stripper's own source; "applies to" reads
-- BASE characteristics (nonbasic is a printed supertype; card-type Land is
-- unchanged by any M3c effect), so nothing recurses into the projection and the
-- result is order-INDEPENDENT. A cycle trips the visited set (both treated as
-- live -- the CR 613.8b loop-escape analog, not an implementation of it, #37).
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
        -- A control op carries no subtype word for CR 612 to rewrite: identity.
        Modification.SetController _ -> acc
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
      stored = fmap fromStored (GameState.continuousEffects gs)
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
                 in fmap gatherOne (Card.Type.staticAbilities card)
              else []
      static_ = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
      fromEmblem emblemId = case Game.lookupObject emblemId gs of
        Nothing -> []
        Just emblemObj -> case Game.cardOf emblemId gs of
          Nothing -> []
          Just card ->
            -- CR 114.4 / 113.6: an emblem's abilities function in the command
            -- zone. Its static ability's continuous effect shares the emblem's
            -- entry timestamp (CR 613.7a). No liveness/text-change pass: nothing
            -- in scope strips an emblem's abilities or rewrites land types.
            fmap
              ( \sa ->
                  MkGathered
                    { gSource = emblemId,
                      gAffected = StaticAbility.affected sa,
                      gLayer = layer (StaticAbility.modification sa),
                      gTimestamp = Object.timestamp emblemObj,
                      gModification = StaticAbility.modification sa
                    }
              )
              (Card.Type.staticAbilities card)
      emblems = concatMap fromEmblem (Set.toList (GameState.command gs))
   in stored <> static_ <> emblems <> counterGathered gs

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
-- CR 613.8 EXISTENCE dependency is handled by source-liveness, not a within-layer
-- reorder; the topological CR 613.8b applies-to reorder is not implemented (#11).
-- design.md §2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs = projectFrom (gather gs) oid gs

-- CR 613.4a layer 7a: apply the object's own characteristic-defining P/T ability.
-- Read from the PARTIAL projection (post-layer-6), so LoseAllAbilities can strip
-- it first; evaluated against the CURRENT state, so it recomputes on every
-- projection.
--
-- Folded IN PLACE rather than emitted as a synthetic Gathered the way
-- counterGathered emits layer-7c counters, for three reasons:
--
--   * gather runs BEFORE the fold and has no partial to read, so a pre-gathered
--     CDA could never be removed by Humility at layer 6;
--   * CR 604.3 (and CR 208.2a for P/T specifically) says a CDA functions in ALL
--     zones. gather walks the battlefield only; projectFrom is not zone-scoped, so
--     in-place gets all-zones behaviour for free -- a Tarmogoyf in a graveyard has
--     a power, and counts itself;
--   * a CDA has no source object and no timestamp, so it has nothing to sort on
--     under CR 613.7 and does not belong in the candidate list at all.
--
-- setPT (not a bare assignment) so an unevaluable quantity leaves the value
-- alone, the powerOf posture used throughout this module.
--
-- NOTE: CR 208.2a has a stricter rule for that case -- "If the ability needs to
-- use a number that can't be determined, including inside a calculation, use 0
-- instead of that number" -- which neither setPT nor a bare assignment
-- implements, and neither does its CR 208.5 sibling at the read points (#65).
applyCharacteristicPT :: GameState -> ObjectId -> ProjectedCharacteristics -> ProjectedCharacteristics
applyCharacteristicPT gs oid pc = case PC.characteristicPT pc of
  Nothing -> pc
  Just (p, t) ->
    let you = controllerOf oid gs
     in pc
          { PC.power = setPT (PC.power pc) (Quantity.evaluate gs oid you p),
            PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate gs oid you t)
          }

-- Project one object against a PRECOMPUTED candidate list. gather is
-- oid-independent, so a whole-board sweep gathers once and folds each object
-- (projectAll) instead of re-gathering per object.
--
-- Layer 7a is ALWAYS in the layer list, even when no gathered effect lives there:
-- an object's own characteristic-defining ability is not a gathered candidate
-- (see applyCharacteristicPT). For an object with no CDA the extra pass is an
-- identity function over an empty candidate filter.
projectFrom :: [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectFrom cands oid gs =
  let layers = Set.toAscList (Set.insert Layer.CharacteristicPT (Set.fromList (fmap gLayer cands)))
      applyLayer partial lyr =
        let seeded =
              if lyr == Layer.CharacteristicPT
                then applyCharacteristicPT gs oid partial
                else partial
            here = filter (\c -> gLayer c == lyr && affects (gSource c) oid (gAffected c) seeded gs) cands
            -- CR 613.7 timestamp order within a layer; the CR 613.8b dependency
            -- reorder (a same-layer effect that changes which objects another
            -- applies to) is not implemented (#11). Existence dependencies are
            -- handled separately by staticAbilitiesLive.
            ordered = List.sortOn gTimestamp here
            step pc c = applyModification gs oid (gModification c) pc
         in List.foldl' step seeded ordered
   in List.foldl' applyLayer (copiableCharacteristics oid gs) layers

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

-- CR 105.2 / 613.1e: an object's colours after the layer fold. The SOLE read
-- point -- the closed half never reads Card.manaCost for colour, the same
-- discipline keywordsOf established at M2a.
colorsOf :: ObjectId -> GameState -> Set Color.Color
colorsOf oid gs = PC.colors (project oid gs)

-- CR 602 / 613.1f: an object's activated abilities after the layer system, the
-- same projection posture as keywordsOf. A Humility'd creature has none.
abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesOf oid gs = PC.activatedAbilities (project oid gs)

-- CR 614 / 613 layer 6: an object's replacement effects after the layer system,
-- the same projection posture as abilitiesOf. A Humility'd creature has none.
replacementsOf :: ObjectId -> GameState -> [ReplacementEffect]
replacementsOf oid gs = PC.replacementEffects (project oid gs)

-- CR 614.6: every replacement effect active on the battlefield, PAIRED WITH ITS
-- SOURCE -- a ControllerRelation pattern (CR 109.5's "you") is unanswerable
-- without it. Short-circuits when no permanent has one in its base card, so an
-- ordinary zone change (a draw, a land entering) does NOT project the whole
-- board.
--
-- The short-circuit reads BASE cards while the result reads the PROJECTION, which
-- is sound only because the one way to acquire a replacement effect you were not
-- printed with is `EntryR AsCopy` -- and a card with that arm is itself a base
-- card with a replacement effect, so it keeps `baseHas` true for its own object.
replacementsAffecting :: GameState -> [(ObjectId, ReplacementEffect)]
replacementsAffecting gs =
  let onBattlefield = Set.toList (GameState.battlefield gs)
      baseHas oid = case Game.cardOf oid gs of
        Nothing -> False
        Just card -> not (null (Card.Type.replacementEffects card))
      forOne oid = fmap (\re -> (oid, re)) (replacementsOf oid gs)
   in if not (any baseHas onBattlefield)
        then []
        else concatMap forOne onBattlefield

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

-- CR 108.4 / 613.1b: the controller of an object is its owner, overridden by
-- layer-2 SetController continuous effects (last timestamp wins, CR 613.7). A
-- lean fold, not the full ProjectedCharacteristics pass -- control feeds combat,
-- mana and priority and is needed before P/T. Projection is the sole applier of
-- SetController (the case-on-Modification invariant). Nothing when the id is
-- unknown. Replaces Game.controllerOf (the M1b owner stand-in, now cashed).
controllerOf :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOf oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj ->
    let names a = case a of
          Affected.TheseObjects s -> Set.member oid s
          _ -> False
        setter eff = case ContinuousEffect.modification eff of
          Modification.SetController pid
            | names (ContinuousEffect.affected eff) -> Just (ContinuousEffect.timestamp eff, pid)
          _ -> Nothing
        setters = Maybe.mapMaybe setter (GameState.continuousEffects gs)
     in case setters of
          [] -> Just (Object.owner obj)
          _ -> Just (snd (List.maximumBy (Ord.comparing fst) setters))

-- The battlefield permanents a player controls (CR 108.4). The control-based
-- "your permanents" enumerator; consumers use it wherever they mean "you
-- control", replacing the owner-based Game.zoneMembers Battlefield.
controls :: PlayerId.PlayerId -> GameState -> [ObjectId]
controls pid gs = filter (\oid -> controllerOf oid gs == Just pid) (Set.toList (GameState.battlefield gs))
