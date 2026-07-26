module Pawl.Projection where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import qualified Pawl.Count as Count
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
  Modification.SetControllerToSource -> Layer.Control
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
-- CR 109.5: "for a static ability, this is the current controller of the object
-- it's on" -- the effect's SOURCE's controller, not the affected object's. `src`
-- (the Gathered candidate's own source) supplies both the
-- perspective and the InSlot binding source for the built Filter.Context. `lyr`
-- is the layer bound the Pawl.Type.Count fold sees (viewUpTo) -- the layers
-- already applied when this modification is folded in. This is the #34 fix: a
-- characteristic-defining ability is the OTHER case (CR 604.3a(3): a CDA does
-- not directly affect the characteristics of any other object), and it is
-- applied by applyCharacteristicPT, which builds its context from the
-- object's OWN controller instead.
--
-- No card in the pool is a static ability carrying a Count, so this corrected
-- branch has no producer and no test exercises it (#155).
applyModification :: Layer -> ObjectId -> [Gathered] -> GameState -> ObjectId -> Modification -> ProjectedCharacteristics -> ProjectedCharacteristics
applyModification lyr src cands gs oid m pc =
  let context = Filter.MkContext (controllerOf src gs) (Just src)
      viewOf = viewUpTo lyr cands gs
   in case m of
        -- CR 613.1f layer 6: a grant ADDS an ability. Two grants of the same
        -- keyword are two abilities (rule 702.164 has no redundancy clause of
        -- the CR 702.3c/702.9c kind), so the count goes up rather than the
        -- second grant being absorbed.
        Modification.GainKeyword k ->
          pc {PC.keywords = Map.insertWith (+) k 1 (PC.keywords pc)}
        -- CR 604.3: a characteristic-defining ability IS a static ability, so
        -- losing all abilities loses it too. Layer 6, which is BEFORE 7a -- the
        -- reason the CDA is folded in place from the partial rather than
        -- gathered up front.
        Modification.LoseAllAbilities ->
          pc
            { PC.keywords = Map.empty,
              PC.characteristicPT = Nothing,
              PC.activatedAbilities = [],
              PC.replacementEffects = [],
              PC.triggeredAbilities = []
            }
        Modification.SetBasePowerToughness p t ->
          pc
            { PC.power = setPT (PC.power pc) (Quantity.evaluate viewOf context gs oid p),
              PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate viewOf context gs oid t)
            }
        Modification.ModifyPowerToughness p t ->
          pc
            { PC.power = addPT (PC.power pc) (Quantity.evaluate viewOf context gs oid p),
              PC.toughness = addPT (PC.toughness pc) (Quantity.evaluate viewOf context gs oid t)
            }
        Modification.AddLandSubtype s ->
          pc {PC.subtypes = Set.insert s (PC.subtypes pc)}
        Modification.AddCardType t ->
          pc {PC.cardTypes = Set.insert t (PC.cardTypes pc)}
        -- CR 305.7: setting a land's subtype to a basic type removes its old
        -- land types AND strips its rules-text abilities (here: keywords and,
        -- via rulesTextActive, its static abilities -- see gather). It gains
        -- the new mana ability from the subtype (CR 305.6, read at the mana
        -- call site).
        Modification.SetLandSubtype s ->
          pc
            { PC.subtypes = Set.singleton s,
              PC.keywords = Map.empty,
              PC.rulesTextActive = False
            }
        -- CR 612.1/612.2: a text-changing effect replaces one basic land type
        -- word with another where the word is used AS a land type -- here, in
        -- the projected type line. Layer 3, so it folds before layer 4 (Type):
        -- a hacked basic Mountain is an Island by the time mana (CR 305.6)
        -- reads its subtypes. Absent `from` is a no-op.
        Modification.ChangeSubtypeWord from to ->
          if Set.member from (PC.subtypes pc)
            then pc {PC.subtypes = Set.insert to (Set.delete from (PC.subtypes pc))}
            else pc
        -- CR 613.1b layer 2: control-changing effects apply here, but
        -- ProjectedCharacteristics carries no controller field -- controllerOf
        -- reads GameState.continuousEffects directly (a lean fold, not the full
        -- layer pass; see its comment). This arm is identity so gather/project's
        -- uniform walk over every stored effect stays total once a
        -- SetController effect exists.
        Modification.SetController _ -> pc
        -- Same identity treatment as SetController just above, and for the same
        -- reason: ProjectedCharacteristics carries no controller field.
        -- controllerOf reads GameState.continuousEffects (and, once a static
        -- ability can produce this, the battlefield's static abilities) directly.
        Modification.SetControllerToSource -> pc
        Modification.SetColor cs ->
          -- CR 105.3: the new colours replace all previous ones.
          pc {PC.colors = cs}
        -- CR 613.4d: "take the value of power and apply it to the creature's
        -- toughness, and take the value of toughness and apply it to the
        -- creature's power."
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
-- PARTIAL projection built by the layers below this one? A fixed set is a
-- membership test; a dynamic set is a Filter evaluated against the PARTIAL
-- characteristics, so a layer-4 type change is visible to a later layer.
-- A Not IsSource conjunct in that Filter is Opalescence's own "each other"
-- (card text, not a rule -- Opalescence does not animate itself), matched
-- against this View's own identity. CR 109.5: an affected-set filter's "you"
-- is the effect's SOURCE's controller (the perspective), which ControlledBy
-- compares against the affected object's own controller (the View's
-- controller) -- the emblem anthem's "creatures you control" is the first
-- affected set to reference a player.
-- Supertypes read from the printed type line (CR 205.4a; not projected at M3c)
-- via viewOfCharacteristics.
affects :: ObjectId -> ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affects source oid a partial gs = case a of
  Affected.TheseObjects s -> Set.member oid s
  -- CR 303.4m: read the SOURCE's attachment, not the candidate's. An unattached
  -- source names nothing, so the set is empty and the effect applies to no one.
  Affected.Attached -> case Game.lookupObject source gs of
    Nothing -> False
    Just src -> Object.attachedTo src == Just oid
  Affected.Matching f ->
    let -- CR 109.5: "you" on a continuous effect is the effect's SOURCE's
        -- controller; ControlledBy compares the affected object's controller to
        -- it. controllerOf no longer merely reads GameState.continuousEffects --
        -- since CR 613.1b's control-granting static abilities (controlGrants), it
        -- also folds a battlefield liveness gate (liveGiven/CR 305.7) that itself
        -- calls THIS function for any Matching set. So this call is safe only
        -- because `perspective` is an unforced thunk until a filter conjunct
        -- actually needs it (ControlledBy is the only one that does), and no
        -- SetLandSubtype -- static OR stored, setLandSubtypeEffects gathers
        -- both -- in the pool pairs Matching with ControlledBy. A stored one
        -- is additionally safe today because Pawl.Resolve constructs every
        -- stored ContinuousEffect with Affected.TheseObjects, never Matching;
        -- that is a fact about Resolve's current call sites, not something
        -- this fold enforces. That is a laziness accident, not a structural
        -- guarantee -- such a card would force `perspective`, which recurses
        -- back into controllerOf -> controlGrants -> this same liveness gate,
        -- and hangs rather than answering wrong (#197).
        perspective = controllerOf source gs
     in Set.member oid (GameState.battlefield gs)
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

-- The ViewOf for callers OUTSIDE the CR 613 layer fold: a full projection of
-- every object, layers fully applied. This is what every count wants once the
-- fold itself has finished -- static abilities' Filters, cost/replacement
-- filtering, expiry conditions, and the like all read the settled state.
--
-- `viewUpTo`, right below, is its bounded counterpart for callers INSIDE the
-- fold, where a count must see candidates only through the layers already
-- applied (CR 613.1-613.10 apply layers in order; a count evaluated mid-layer
-- must not see the effects of layers still to come). Picking the wrong one is
-- not a type error -- both are `Count.ViewOf` -- so it is a SILENT wrong
-- answer: a count fed `viewUpTo` outside the fold under-reads (misses layers
-- that already settled), and a count fed `fullView` inside the fold over-reads
-- (sees layers that have not applied yet). Always reach for this by name
-- rather than re-deriving the lambda at the call site.
fullView :: GameState -> Count.ViewOf
fullView gs oid = Just (viewOfObject oid gs)

-- The ViewOf a count gets when it is evaluated while `bound` is being applied:
-- candidates projected through the layers BEFORE that one. Off-battlefield
-- candidates have no projection at all (gather walks the battlefield only), so
-- they fall back to the printed card -- a library/hand/graveyard candidate is
-- matched against its PRINTED characteristics, never a projected view (#160).
viewUpTo :: Layer -> [Gathered] -> GameState -> Count.ViewOf
viewUpTo bound cands gs oid =
  if Set.member oid (GameState.battlefield gs)
    then Just (viewOfCharacteristics oid (projectUpTo bound cands oid gs) (controllerOf oid gs) gs)
    else fmap viewOfCard (Game.cardOf oid gs)

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
          Filter.identity = Nothing,
          Filter.playerIdentity = Nothing
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
      Filter.identity = Just oid,
      Filter.playerIdentity = Nothing
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
      { PC.keywords = Map.empty,
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
    -- The seed predates every layer, including Copy (1) -- there is no
    -- established view of ANYTHING yet, so a Count reached from here (a
    -- printed power/toughness Quantity, never a real card's) gets a viewOf
    -- that determines nothing rather than one that recurses back into
    -- copiableCharacteristics/baseCharacteristics through viewUpTo/projectUpTo.
    -- The tradeoff: a Count reached from here folds an empty candidate set and
    -- aggregates to Just 0 -- a plausible wrong answer -- rather than the
    -- honest Nothing a printed-P/T Count deserves; no card in the pool has one
    -- (#156).
    -- The context is still the object's own controller, the CDA posture (CR
    -- 604.3a(3)) this seed already shares with applyCharacteristicPT.
    let seedViewOf = const Nothing
        seedContext = Filter.MkContext (controllerOf oid gs) (Just oid)
     in PC.MkProjectedCharacteristics
          { -- CR 702: a printed keyword appears once in the card's text, so the
            -- seed's count is 1 apiece. Multiplicity is what layer-6 grants add
            -- on top (CR 702.164b).
            PC.keywords = Map.fromSet (const 1) (Card.Type.keywords card),
            PC.colors = baseColorsOf card,
            PC.power = case Card.Type.power card of
              Nothing -> Nothing
              Just (Power.MkPower q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
            PC.toughness = case Card.Type.toughness card of
              Nothing -> Nothing
              Just (Toughness.MkToughness q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
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
--     A Matching (And [HasCardType Creature, HasColor c]) affected set
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
  -- the stored effect, where applyModification later evaluates it live against
  -- the CURRENT game state on every projection, using the effect's SOURCE's
  -- controller as perspective (CR 109.5) -- correct on WHO, but still wrong on
  -- WHEN: CR 608.2h/611.2d call for a single read at store time, which is the
  -- very mis-evaluation this freeze exists to prevent (#36).
  --
  -- CR 608.2h / 611.2d: read the CURRENT state through the real projection --
  -- `oid` is the source, `you` its controller, matching the doc above.
  let viewOf = fullView gs
      context = Filter.MkContext you (Just oid)
      freeze q = maybe q Quantity.Type.Literal $ Quantity.evaluate viewOf context gs oid q
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
        Modification.SetControllerToSource -> m
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
        Modification.SetControllerToSource -> False
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
--
-- CR 604.3a(3): a characteristic-defining ability does not directly affect the
-- characteristics of any OTHER object, so the built Filter.Context is the
-- object's OWN controller -- contrast applyModification, whose context is the
-- effect's SOURCE's controller (CR 109.5). `lyr`/`cands` are the same pair
-- applyModification receives; the caller always passes Layer.CharacteristicPT
-- for `lyr` here (see projectWith), so a CDA's count sees layers 1-6 -- control
-- at layer 2 and type at layer 4, which is what Nightmare needs to see Urborg
-- and Act of Treason.
applyCharacteristicPT :: Layer -> [Gathered] -> GameState -> ObjectId -> ProjectedCharacteristics -> ProjectedCharacteristics
applyCharacteristicPT lyr cands gs oid pc = case PC.characteristicPT pc of
  Nothing -> pc
  Just (p, t) ->
    let context = Filter.MkContext (controllerOf oid gs) (Just oid)
        viewOf = viewUpTo lyr cands gs
     in pc
          { PC.power = setPT (PC.power pc) (Quantity.evaluate viewOf context gs oid p),
            PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate viewOf context gs oid t)
          }

-- Project one object against a PRECOMBINED candidate list, applying only the
-- layers the predicate admits. CR 613.1 applies layers in order and Layer's
-- derived Ord IS that order, so `(< bound)` is exactly "the layers before this
-- one".
--
-- The bound exists for counting: a Pawl.Type.Count evaluated while layer L is
-- being applied sees its candidates through `< L`, so a count encountered inside
-- THAT fold is applied at some K < L and sees `< K`. The bound strictly
-- decreases and Layer is finite, so the nesting terminates.
--
-- This is a terminating APPROXIMATION of CR 613.8's dependency system, not an
-- implementation of it: exact whenever a count reads layers strictly earlier
-- than its consumer's, and it under-reads a count over its own layer or later
-- (#157; #11 is the missing CR 613.8b reorder).
projectWith :: (Layer -> Bool) -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectWith admits cands oid gs =
  let layers = filter admits (Set.toAscList (Set.insert Layer.CharacteristicPT (Set.fromList (fmap gLayer cands))))
      applyLayer partial lyr =
        let seeded =
              if lyr == Layer.CharacteristicPT
                then applyCharacteristicPT lyr cands gs oid partial
                else partial
            here = filter (\c -> gLayer c == lyr && affects (gSource c) oid (gAffected c) seeded gs) cands
            -- CR 613.7 timestamp order within a layer; the CR 613.8b dependency
            -- reorder (a same-layer effect that changes which objects another
            -- applies to) is not implemented (#11). Existence dependencies are
            -- handled separately by staticAbilitiesLive.
            ordered = List.sortOn gTimestamp here
            step pc c = applyModification lyr (gSource c) cands gs oid (gModification c) pc
         in List.foldl' step seeded ordered
   in List.foldl' applyLayer (copiableCharacteristics oid gs) layers

-- Project one object against a PRECOMPUTED candidate list. gather is
-- oid-independent, so a whole-board sweep gathers once and folds each object
-- (projectAll) instead of re-gathering per object.
--
-- Layer 7a is ALWAYS in the layer list, even when no gathered effect lives there:
-- an object's own characteristic-defining ability is not a gathered candidate
-- (see applyCharacteristicPT). For an object with no CDA the extra pass is an
-- identity function over an empty candidate filter.
projectFrom :: [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectFrom = projectWith (const True)

-- CR 613.1: a projection bounded to the layers BEFORE `bound` -- the fold a
-- Pawl.Type.Count sees while layer `bound` is being applied. See projectWith's
-- comment for the termination argument this exists to serve.
projectUpTo :: Layer -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectUpTo bound = projectWith (< bound)

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

-- CR 702: an object's keyword abilities after the layer fold, counted per
-- keyword (see ProjectedCharacteristics.keywords for why the count is kept).
-- Most readers want hasKeyword or totalToxic rather than the raw counts.
keywordsOf :: ObjectId -> GameState -> Map Keyword Natural
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

-- CR 603 / 613 layer 6: an object's PRINTED-AND-GRANTED triggered abilities after
-- the layer system, the same projection posture as abilitiesOf. A Humility'd
-- creature has none.
--
-- NOT the whole list: rule 702.70's poisonous is a triggered ability the RULES
-- give an object for holding a keyword, and Pawl.Keyword.triggeredAbilitiesOf
-- mints those from PC.keywords instead. Pawl.Event's event scan adds them; a
-- reader that wants every triggered ability an object has must do the same.
-- Deliberately not folded into PC.triggeredAbilities: that field is built DURING
-- the layer fold, while the mint has to read the FINISHED keyword counts, which
-- only exist once the fold is over. Deriving after the fold is also what makes
-- Humility free -- LoseAllAbilities empties PC.keywords, so there is nothing left
-- to mint from.
triggeredAbilitiesOf :: ObjectId -> GameState -> [TriggeredAbility Card.Type.Card]
triggeredAbilitiesOf oid gs = PC.triggeredAbilities (project oid gs)

subtypesOf :: ObjectId -> GameState -> Set Subtype.Subtype
subtypesOf oid gs = PC.subtypes (project oid gs)

cardTypesOf :: ObjectId -> GameState -> Set CardType.CardType
cardTypesOf oid gs = PC.cardTypes (project oid gs)

-- CR 613.1d: creature-ness is the projected card-type question, the same
-- projection posture as keywordsOf. An Opalescence'd enchantment is a creature.
isCreatureOf :: ObjectId -> GameState -> Bool
isCreatureOf oid gs = Set.member CardType.Creature (cardTypesOf oid gs)

-- Membership, which DISCARDS the count -- and is exactly right for every
-- keyword whose multiple instances the rules call redundant (CR 702.3c
-- defender, CR 702.9c flying). A keyword that stacks is asked about with its
-- own reader instead; totalToxic just below is the first.
hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword keyword oid gs = Map.member keyword (keywordsOf oid gs)

-- CR 702.164b: "A creature's total toxic value is the sum of all N values of
-- toxic abilities that creature has." Not hasKeyword's question -- toxic is
-- parameterized, so there is no single member to ask about -- but the same
-- projection posture: the sum is taken over the POST-LAYER keywords, so a
-- Humility'd creature has none.
--
-- Each toxic ability contributes its own N, so a creature with the same N twice
-- counts it twice -- rule 702.164 has no redundancy clause. That is why the
-- projection counts keywords instead of setting them.
totalToxic :: ObjectId -> GameState -> Natural
totalToxic oid gs =
  let value keyword count = case keyword of
        Keyword.Toxic n -> n * count
        _ -> 0
   in sum (Map.elems (Map.mapWithKey value (keywordsOf oid gs)))

-- One control-granting static ability, flattened: the source that carries it and
-- the timestamp its effect takes (CR 613.7a: a static ability's continuous effect
-- has the timestamp of the object it is on).
data ControlGrant = MkControlGrant
  { cgSource :: ObjectId,
    cgAffected :: Affected.Affected,
    cgTimestamp :: Timestamp
  }
  deriving (Eq, Ord, Show)

-- Every layer-2 control-granting STATIC ability on the battlefield, gathered once.
--
-- NOT `gather`. This must not project, and cannot: Projection.affects reads
-- controllerOf to supply CR 109.5's "you" when matching a Filter, so a
-- controllerOf built on gather would be mutually recursive with it. That
-- restriction is exactly why Affected.Matching is unsupported below (#195).
--
-- Hoisted for the reason liveGiven's list is hoisted: controllerOf feeds combat,
-- priority, mana and Projection.controls, and `controls` calls it once per
-- battlefield object. Recomputing this list inside controllerOf would make
-- `controls` quadratic in the battlefield, inside a loop the state-based-action
-- sweep runs at every priority boundary.
--
-- Layer 6/Humility is invisible to this fold: a control-granting static ability
-- stripped by LoseAllAbilities still appears here, because this walk reads the
-- battlefield's printed cards directly rather than a layer-ordered projection
-- (#196).
--
-- INVARIANT this liveness gate depends on (#197): the `liveGiven` call below
-- must never FORCE a control-dependent Filter. `liveGiven` -> affectsBase ->
-- affects's Matching arm calls `controllerOf`, which calls BACK into
-- `controlGrants` -- so if that Matching filter's evaluation ever forces
-- `affects`'s `perspective` thunk (a ControlledBy conjunct is the only thing
-- that does), this loops rather than answering wrong. Nothing here prevents
-- that; it holds only because no SetLandSubtype in the pool -- static OR
-- stored, setLandSubtypeEffects gathers both -- carries a Matching filter with
-- ControlledBy in it: the pool's one static example (Blood Moon) has none, and
-- every stored ContinuousEffect Pawl.Resolve constructs carries
-- Affected.TheseObjects rather than Matching at all. See #197 for the card
-- shape that would break this and the deferred structural fix.
controlGrants :: GameState -> [ControlGrant]
controlGrants gs =
  let setEffs = setLandSubtypeEffects gs
      grantsOf permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card ->
            -- CR 305.7: a land whose subtype was SET has lost its rules text, so
            -- it grants nothing. Same gate gather applies to every static ability.
            if not (null setEffs) && not (liveGiven setEffs Set.empty permId gs)
              then []
              else
                let isControl sa = case StaticAbility.modification sa of
                      Modification.SetControllerToSource -> True
                      _ -> False
                    toGrant sa =
                      MkControlGrant
                        { cgSource = permId,
                          cgAffected = StaticAbility.affected sa,
                          cgTimestamp = Object.timestamp permObj
                        }
                 in fmap toGrant (filter isControl (Card.Type.staticAbilities card))
   in concatMap grantsOf (Set.toList (GameState.battlefield gs))

-- CR 108.4 / 613.1b: an object's controller is its owner, overridden by layer-2
-- control effects, last timestamp wins (CR 613.7). TWO sources now: stored
-- continuous effects (Effect.GainControl's baked SetController) and control-
-- granting static abilities (Control Magic's derived SetControllerToSource).
-- Both carry a Timestamp, so they merge into one maximum.
--
-- Still a lean fold, not the full ProjectedCharacteristics pass -- control feeds
-- combat, mana and priority and is needed before P/T.
controllerOf :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOf oid gs = controllerOfGiven (controlGrants gs) Set.empty oid gs

-- controllerOf with the grant list PRECOMPUTED and a visited set.
--
-- The visited set is the CR 613.8b loop-escape analog liveGiven already uses
-- (#37), not an implementation of it: deriving a grant's player asks for its
-- SOURCE's controller, which can re-enter this function. Re-entering an object
-- already under question returns its owner, so a cycle grants nothing and every
-- object in it keeps its own owner. Unreachable in practice today ("enchant
-- creature" forbids two Auras attached to each other, which rules out the
-- Attached route; a second route -- two static abilities each naming the
-- other's fixed ObjectId via Affected.TheseObjects, which is codec-round-
-- trippable and legal on a static ability -- is unauthorable by any real card
-- but not excluded by the types), and unobservable even if it existed: a
-- direct controllerOf query on any object in the cycle returns THAT object's
-- owner either way, whether or not it was first reached by recursing through
-- another object in the cycle -- which is the only kind of query `controls`
-- (or anything else) ever makes.
controllerOfGiven :: [ControlGrant] -> Set ObjectId -> ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOfGiven grants visited oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj ->
    if Set.member oid visited
      then Just (Object.owner obj)
      else
        let visited' = Set.insert oid visited
            -- Does an affected set carried by `source` name `oid`? Parameterized
            -- by the source because Affected.Attached is a question about the
            -- SOURCE's state, and the stored and derived paths carry different
            -- sources.
            namesFrom source a = case a of
              Affected.TheseObjects s -> Set.member oid s
              -- CR 303.4m: the source's own attachment. No projection needed,
              -- which is what keeps this fold lean.
              Affected.Attached -> case Game.lookupObject source gs of
                Nothing -> False
                Just src -> Object.attachedTo src == Just oid
              -- Needs a projection to evaluate, and this fold must not project
              -- (see controlGrants). No card produces one (#195).
              Affected.Matching _ -> False
            storedSetter eff = case ContinuousEffect.modification eff of
              Modification.SetController pid
                | namesFrom (ContinuousEffect.source eff) (ContinuousEffect.affected eff) ->
                    Just (ContinuousEffect.timestamp eff, pid)
              _ -> Nothing
            stored = Maybe.mapMaybe storedSetter (GameState.continuousEffects gs)
            fromGrant g =
              if not (namesFrom (cgSource g) (cgAffected g))
                then Nothing
                else case controllerOfGiven grants visited' (cgSource g) gs of
                  Nothing -> Nothing
                  Just who -> Just (cgTimestamp g, who)
            derived = Maybe.mapMaybe fromGrant grants
         in case stored <> derived of
              [] -> Just (Object.owner obj)
              setters -> Just (snd (List.maximumBy (Ord.comparing fst) setters))

-- The battlefield permanents a player controls (CR 108.4). Computes the grant
-- list ONCE and threads it, rather than letting each controllerOf rebuild it --
-- the difference between linear and quadratic in the battlefield, in a function
-- the state-based-action sweep calls at every priority boundary.
controls :: PlayerId.PlayerId -> GameState -> [ObjectId]
controls pid gs =
  let grants = controlGrants gs
   in filter (\oid -> controllerOfGiven grants Set.empty oid gs == Just pid) (Set.toList (GameState.battlefield gs))

-- CR 800.4a: does this stored effect give `pid` control of an object? The
-- control-granting classification Pawl.Departure asks, so that the case on
-- Modification stays in the one module allowed to make it (see
-- Pawl.Type.Modification). Modification.SetController's payload IS the player who
-- gains control -- it is baked at effect creation and is the effect's source's
-- controller -- so the payload is what "that player" names.
givesControlTo :: PlayerId.PlayerId -> ContinuousEffect.ContinuousEffect -> Bool
givesControlTo pid eff = case ContinuousEffect.modification eff of
  Modification.SetController who -> who == pid
  -- This one names no player, so it cannot be classified from the effect alone:
  -- CR 109.5 makes its player the current controller of the effect's SOURCE,
  -- which needs a GameState this function does not take. False is right for
  -- every state pawl can reach -- the constructor is authored only on Control
  -- Magic's static ability, which the projection re-derives and never stores, so
  -- no stored effect carries it. The residual case (card JSON authoring one into
  -- an Effect.ModifyTarget) is #199, and it does not endanger CR 800.4a's third
  -- and fourth clauses either way, because a derived player is the source's
  -- controller -- see the induction in Pawl.Departure.
  Modification.SetControllerToSource -> False
  _ -> False
