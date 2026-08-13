module Pawl.Engine.Projection where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import Pawl.Types.Layer (Layer)
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.Modification (Modification)
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Power as Power
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSpec as TargetSpec
import Pawl.Types.Timestamp (Timestamp)
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TypeLine as TypeLine

-- CR 613.1: the layer a modification applies in. A classification, never the
-- modification's identity.
layer :: Modification -> Layer
layer m = case m of
  Modification.GainKeyword _ -> Layer.Ability
  Modification.LoseAllAbilities -> Layer.Ability
  Modification.SetBasePowerToughness _ _ -> Layer.SetPT
  Modification.ModifyPowerToughness _ _ -> Layer.ModifyPT
  Modification.SetLandSubtype _ -> Layer.Type
  Modification.SetLandSubtypeToChosen -> Layer.Type
  Modification.AddLandSubtype _ -> Layer.Type
  Modification.SetCreatureSubtype _ -> Layer.Type
  Modification.AddCreatureSubtype _ -> Layer.Type
  Modification.AddEveryCreatureSubtype -> Layer.Type
  Modification.AddCardType _ -> Layer.Type
  Modification.AddSupertype _ -> Layer.Type
  Modification.RemoveSupertype _ -> Layer.Type
  Modification.ChangeSubtypeWord _ _ -> Layer.Text
  Modification.SetController _ -> Layer.Control
  Modification.SetControllerToSource -> Layer.Control
  Modification.SetColor _ -> Layer.Color
  Modification.AddColor _ -> Layer.Color
  Modification.AddChosenColor -> Layer.Color
  Modification.SwitchPowerToughness -> Layer.SwitchPT

-- Apply one modification to characteristics-in-progress. P/T quantities are
-- evaluated against the CURRENT state, which is correct for a static ability
-- (CR 604.2). A continuous effect from a RESOLUTION must not be re-read (CR
-- 608.2h / 611.2d); Resolve freezes those to literals at store time, so this
-- evaluation is the identity on them.
--
-- CR 109.5: a static ability's perspective is its SOURCE's controller, so `src`
-- supplies both that and the InSlot binding source of the Filter.Context. The
-- ViewOf a Count folds over is the CALLER's: projectDeciding hands in the board
-- as it stands mid-layer, so a count reads the same-layer effects that have
-- already applied (CR 613.8b), and the bounded viewUpTo only where no such
-- effect exists. A characteristic-defining ability is the other case (CR
-- 604.3a(3)) and is applied by applyCharacteristicPT, which builds its context
-- from the object's own controller.
--
-- What discriminates the two perspectives is Empyrial Armor on an OPPONENT's
-- creature, in Pawl.PowerToughnessSpec's "Empyrial Armor" group: "+1/+1 for each
-- card in your hand" reads the Aura controller's hand and not the enchanted
-- creature's controller's, and the two hands are deliberately different sizes.
-- Building the context from `oid` instead of `src` fails that test alone.
applyModification :: Count.ViewOf -> ObjectId -> GameState -> ObjectId -> Modification -> ProjectedCharacteristics -> ProjectedCharacteristics
applyModification viewOf src gs oid m pc =
  let context = Filter.contextFor (controllerOf src gs) (Just src)
   in case m of
        -- CR 613.1f layer 6: a grant adds an ability, so two grants of the same
        -- keyword count twice rather than the second being absorbed.
        Modification.GainKeyword k ->
          pc {PC.keywords = Map.insertWith (+) k 1 (PC.keywords pc)}
        -- CR 604.3: a CDA is a static ability, so this loses it too. Layer 6 is
        -- before 7a, which is why the CDA is folded in place from the partial
        -- rather than gathered up front.
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
        -- CR 205.1a/205.1b: setting a subtype replaces only the appropriate set,
        -- here the creature types (CR 205.3m), so an animated permanent's land
        -- type survives becoming a Frog. Strips no abilities -- CR 305.7's
        -- ability clause is about lands, not about setting a subtype -- and
        -- touches no card type, which a card says separately with AddCardType.
        --
        -- Not checked: CR 205.3d, which none of the layer-4 subtype arms asks
        -- (#530).
        Modification.SetCreatureSubtype s ->
          pc {PC.subtypes = Set.insert s (Set.filter (not . Subtype.isCreatureType) (PC.subtypes pc))}
        -- CR 205.1b's add: every creature type already present is kept, which is
        -- the one line separating this arm from the set above.
        Modification.AddCreatureSubtype s ->
          pc {PC.subtypes = Set.insert s (PC.subtypes pc)}
        -- The same add over CR 205.3m's whole list. Identical to what
        -- applySubtypeDefining writes at the start of this layer, and deliberately
        -- so: rule 702.73a says one thing, and the two sites differ only in WHEN
        -- they run -- the CDA first (CR 613.3), this in timestamp order (CR
        -- 613.7).
        Modification.AddEveryCreatureSubtype ->
          pc {PC.subtypes = Set.union Subtype.everyCreatureType (PC.subtypes pc)}
        Modification.AddCardType t ->
          pc {PC.cardTypes = Set.insert t (PC.cardTypes pc)}
        -- CR 205.4b: a gain is an INSERT into the supertype set, so every other
        -- supertype the object had survives, and neither its card types nor its
        -- subtypes are touched. Granting one the object already has is the
        -- identity -- CR 205.4 gives an object a SET of supertypes, so a second
        -- Leyline of Singularity does not make a creature legendary twice the way
        -- CR 613.1f's two keyword grants stack.
        Modification.AddSupertype t ->
          pc {PC.supertypes = Set.insert t (PC.supertypes pc)}
        -- CR 205.4b's other direction, a DELETE for the same reason: removing one
        -- supertype leaves the rest, and removing one the object never had is the
        -- identity rather than an error.
        Modification.RemoveSupertype t ->
          pc {PC.supertypes = Set.delete t (PC.supertypes pc)}
        -- CR 305.7's set, with the type written into card data.
        Modification.SetLandSubtype s -> setLandSubtypeTo s pc
        -- CR 305.7's set again, with the type read off the source's own entry
        -- choice (CR 614.1c). An unchosen source sets and strips nothing rather
        -- than guessing, which leaves this arm disagreeing with
        -- setLandSubtypeEffects' constructor-keyed gate; unreachable, since the
        -- rewrite always writes the field before the permanent is projected
        -- (#391).
        Modification.SetLandSubtypeToChosen ->
          case Game.lookupObject src gs >>= Object.chosenSubtype of
            Nothing -> pc
            Just s -> setLandSubtypeTo s pc
        -- CR 612.1/612.2: a text-changing effect swaps a subtype word both in
        -- the projected type line and in the object's rules text. Layer 3, so it
        -- folds before layer 4 -- a hacked Mountain is an Island by the time
        -- mana (CR 305.6) reads its subtypes.
        --
        -- CR 612.2's family gate needs no statement here, unlike in
        -- rewriteModification: this position holds subtype words of every family
        -- at once, so membership already is CR 612.2's question. The swap does
        -- not reach a subtype another effect GRANTED, since a grant is layer 4
        -- (CR 613.1c/613.1d) -- CR 612.1's "printed on that object" falling out
        -- of the layer order rather than being checked for.
        --
        -- The rules-text half is applied unconditionally rather than under the
        -- same Set.member guard: a word swapped in the text box has nothing to
        -- do with whether the type line carries it.
        --
        -- The KEYWORD half is CR 702.14a's: swampwalk holds a land-type word, and
        -- Magical Hack's own reminder text is that example. It reaches only the
        -- keywords the object PRINTS, which is CR 612.3 falling out of the layer
        -- order again -- a granted keyword arrives at layer 6, after this.
        --
        -- Map.mapKeysWith (+) rather than Map.mapKeys, because the swap can
        -- collide two keys: a creature with both islandwalk and swampwalk hacked
        -- Island -> Swamp has one kind of landwalk twice. Summing rather than
        -- dropping one matches what two GainKeywords of the same keyword already
        -- do, and CR 702.14e makes the total unobservable for landwalk anyway --
        -- Combat.landwalkAllowsGiven reads membership, never the count.
        --
        -- Not implemented: the swap does not reach PC.replacementEffects, a
        -- mode's targetSpecs, or an activated ability's cost (#635).
        Modification.ChangeSubtypeWord from to ->
          let pairs = [(from, to)]
              pc' =
                pc
                  { PC.keywords = Map.mapKeysWith (+) (Filter.rewriteKeyword pairs) (PC.keywords pc),
                    PC.activatedAbilities = fmap (rewriteActivatedAbility pairs) (PC.activatedAbilities pc),
                    PC.triggeredAbilities = fmap (rewriteTriggeredAbility pairs) (PC.triggeredAbilities pc)
                  }
           in if Set.member from (PC.subtypes pc')
                then pc' {PC.subtypes = Set.insert to (Set.delete from (PC.subtypes pc'))}
                else pc'
        -- CR 613.1b layer 2: ProjectedCharacteristics carries no controller
        -- field, so controllerOf reads GameState.continuousEffects directly.
        -- These arms are identity to keep gather/project's walk total.
        Modification.SetController _ -> pc
        Modification.SetControllerToSource -> pc
        Modification.SetColor cs ->
          -- CR 105.3: the new colours replace all previous ones.
          pc {PC.colors = cs}
        -- CR 105.3's parenthetical: an "in addition" colour adds rather than
        -- replaces.
        Modification.AddColor cs ->
          pc {PC.colors = Set.union cs (PC.colors pc)}
        -- CR 105.3's parenthetical, with the colour read off the source's own
        -- entry choice. An unchosen source adds nothing rather than guessing.
        Modification.AddChosenColor ->
          case Game.lookupObject src gs >>= Object.chosenColor of
            Nothing -> pc
            Just c -> pc {PC.colors = Set.insert c (PC.colors pc)}
        -- CR 613.4d.
        Modification.SwitchPowerToughness ->
          pc {PC.power = PC.toughness pc, PC.toughness = PC.power pc}

-- CR 305.7's strip, shared by both modifications that set a land's subtype so
-- the rule cannot be implemented twice and drift. Of its three clauses this does
-- the subtype and ability ones; the new basic type's mana ability rides the
-- subtype and is read at the mana call site (CR 305.6).
--
-- The subtype clause takes the land types (CR 205.3i) and nothing else, so a
-- creature type on an animated permanent survives. Taking exactly one type is
-- not a narrowing: no printed card sets a land's subtype to more than one.
--
-- The ability clause strips keywords and the four fields below. An ability
-- landing on OTHER objects is decided by a gate instead, because it must be kept
-- out of the candidate list rather than erased from its own projection: gather
-- uses liveGiven, and every reader after the layers uses liveAfterLayers. Only
-- liveGiven still reads base where this reads the projection, so the halves
-- disagree on an object that became a land at layer 4 (#391).
--
-- CR 305.7 spares abilities GRANTED to the land, which needs no guard: every
-- field cleared here is seeded from the card, and GainKeyword is layer 6, after
-- this layer-4 strip. characteristicPT goes with the rest per CR 604.3.
--
-- Not stripped: CR 305.7's copiable-effects clause, a layer-1 question this
-- layer-4 strip cannot answer (#406).
setLandSubtypeTo :: Subtype.Type.Subtype -> ProjectedCharacteristics -> ProjectedCharacteristics
setLandSubtypeTo s pc =
  pc
    { PC.subtypes = Set.insert s (Set.filter (not . Subtype.isLandType) (PC.subtypes pc)),
      PC.keywords = Map.empty,
      PC.characteristicPT = Nothing,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = []
    }

-- CR 613.4b: layer 7b establishes base P/T, so an object with no printed P/T
-- gains it. Contrast addPT (7c), which only modifies.
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

-- One layer's worth of a continuous effect: its source (for the Matching
-- ExcludesSource self-exclusion), the set it affects, its timestamp, and the one
-- modification that applies in `gLayer`. Projection-internal; not a domain type.
data Gathered = MkGathered
  { -- Which effect this part belongs to: Just (source, the ability's index) for
    -- a static ability with parts in more than one layer, Nothing otherwise. CR
    -- 613.6's affected-set decision is keyed on this pair, made once and reused
    -- (projectWith). Two parts of one ability share it; two abilities never do.
    gEffect :: !(Maybe (ObjectId, Natural)),
    gSource :: ObjectId,
    gAffected :: Affected.Affected,
    gLayer :: Layer,
    -- CR 613.6's decision point: the lowest layer reached by any part of this
    -- part's effect. The layer fold gets this for free by visiting layers in
    -- order and memoizing on gEffect; a caller outside the fold
    -- (abilitiesRemoved) carries it instead of re-deriving it at the wrong layer
    -- (#326), and uses it as the bound it runs the fold to when it wants that
    -- memo (decisionsUpTo). Equal to gLayer for a one-part effect.
    gLowest :: Layer,
    gTimestamp :: Timestamp,
    gModification :: Modification
  }

-- CR 611.2c / 613: does the effect from `source` apply to `oid`, given the
-- PARTIAL projection built by the layers below this one? A fixed set is a
-- membership test; a dynamic set is a Filter evaluated against the partial
-- characteristics, so a layer-4 type change is visible to a later layer. CR
-- 109.5: an affected-set filter's "you" is the SOURCE's controller, which
-- ControlledBy compares against the affected object's own controller.
affects :: ObjectId -> ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affects source oid a partial gs = case a of
  Affected.TheseObjects s -> Set.member oid s
  -- CR 303.4m: read the SOURCE's attachment, not the candidate's. An unattached
  -- source names nothing, so the set is empty and the effect applies to no one.
  -- A source attached to a PLAYER names no object, so the set is empty here too
  -- -- CR 702.5d's enchant-player Auras reach the battlefield through
  -- AttachedPlayerControls below instead.
  Affected.Attached -> (Game.lookupObject source gs >>= Object.attachedTo >>= Recipient.objectOf) == Just oid
  Affected.Matching f ->
    let -- CR 109.5: "you" is the SOURCE's controller. Safe to force from any
        -- affected set, subtype-setting ones included, because controlGrants
        -- consults no liveness gate and so cannot re-enter this function -- see
        -- the CR 613.1 argument there. Celestial Dawn is the card that proves it,
        -- via Pawl.ProjectionSpec's "Celestial Dawn sets only the lands its own
        -- controller controls".
        perspective = controllerOf source gs
     in Set.member oid (GameState.battlefield gs)
          && Filter.matches (Filter.contextFor perspective (Just source)) (viewOfCharacteristics oid partial (controllerOf oid gs) (countersOf oid gs) gs) f
  -- Matching's body without the battlefield conjunct.
  Affected.MatchingAnywhere f ->
    let perspective = controllerOf source gs
     in Filter.matches (Filter.contextFor perspective (Just source)) (viewOfCharacteristics oid partial (controllerOf oid gs) (countersOf oid gs) gs) f
  -- CR 303.4b / 303.4m: the source's attachment again, read for the PLAYER it
  -- names. A source that is unattached, or attached to an object, names no player
  -- and affects nobody. The controller comparison is CR 613.1b's layer 2, already
  -- applied by the time this set is asked.
  --
  -- Unlike the Matching arm's `perspective`, controllerOf is FORCED here on every
  -- candidate, which costs nothing now that controlGrants cannot re-enter this
  -- function; controllerOfGiven's namesFrom answers False for this arm besides.
  -- The Filter's perspective stays the source's controller per CR 109.5, not the
  -- enchanted player's.
  --
  -- The candidate's controller is bound once and used twice, since controllerOf
  -- rebuilds controlGrants -- a whole-board walk -- on every call.
  Affected.AttachedPlayerControls f -> case Game.lookupObject source gs >>= Object.attachedTo of
    Just (Recipient.ToPlayer pid) ->
      let controller = controllerOf oid gs
       in Set.member oid (GameState.battlefield gs)
            && controller == Just pid
            && Filter.matches (Filter.contextFor (controllerOf source gs) (Just source)) (viewOfCharacteristics oid partial controller (countersOf oid gs) gs) f
    _ -> False

-- The characteristics view of a battlefield/stack object: its CR 613 projection
-- and its projected controller (CR 613.1b; Nothing when the id is unknown).
viewOfObject :: ObjectId -> GameState -> Filter.View
viewOfObject oid gs = viewOfObjectGiven Map.empty (controlGrants gs) oid gs

-- viewOfObject against a pre-projected board and a precomputed grant list, so
-- one projection and one grant walk serve a whole pool of candidates. See
-- projectGiven for what the board is and when it is valid.
viewOfObjectGiven :: Map ObjectId ProjectedCharacteristics -> [ControlGrant] -> ObjectId -> GameState -> Filter.View
viewOfObjectGiven pcs grants oid gs =
  viewOfCharacteristics oid (projectGiven pcs oid gs) (controllerOfGiven grants Set.empty oid gs) (countersOf oid gs) gs

-- CR 112.2 / 601.2a: the view of a SPELL on the stack, whose controller is "by
-- default, the player who put it on the stack" -- the player casting it, which
-- CR 601.2a says the same way. The caster is passed in rather than rediscovered,
-- because it is what GameEvent.SpellCast records and this view is built for that
-- event's readers -- and because CR 601.2a's move is what stamps the caster onto
-- the stack object, so a caller reading the arrival cannot be made to depend on
-- having already seen it land.
--
-- Characteristics come from the live projection, which is CR 601.2i's own order:
-- effects that modify the spell as it is cast are applied BEFORE it becomes cast,
-- so by the time a trigger reads this they are already on the stack object.
viewOfSpell :: PlayerId.PlayerId -> ObjectId -> GameState -> Filter.View
viewOfSpell caster oid gs = viewOfCharacteristics oid (project oid gs) (Just caster) (countersOf oid gs) gs

-- The ViewOf for callers OUTSIDE the CR 613 layer fold: every object projected
-- with all layers applied. `viewUpTo` below is the bounded counterpart for
-- callers INSIDE the fold. Picking the wrong one is not a type error -- both are
-- Count.ViewOf -- so it is a silent wrong answer in either direction.
fullView :: GameState -> Count.ViewOf
fullView gs oid = Just (viewOfObject oid gs)

-- CR 113.7a / 608.2h: `fullView`, except that the one object named by `src` is
-- read from last known information once it no longer exists -- what a resolving
-- spell wants for anything it reads about its own source.
--
-- Scoped to `src` alone by design, and the scope is a rules decision: the OTHER
-- object a resolving effect reads is its target, and CR 608.2b answers the same
-- question the other way there -- an effect needing information about a target
-- that has left "fails to determine any such information", which is the blank
-- answer fullView gives. The trigger is that the id names no object, which per CR
-- 400.7 is exactly CR 608.2h's condition.
--
-- viewWithLastKnownAnywhere below is the unscoped counterpart, and where the
-- record read and its no-record answer are accounted for. Picking the wrong one
-- of the two is not a type error -- both are Count.ViewOf -- so it is a silent
-- wrong answer for any id but the source's.
viewWithLastKnown :: ObjectId -> GameState -> Count.ViewOf
viewWithLastKnown src gs oid =
  if oid == src
    then viewWithLastKnownAnywhere gs oid
    else fullView gs oid

-- CR 608.2h for EVERY id the reader is aimed at rather than for one named object:
-- what an intervening "if" wants, because CR 603.4's clause may be about the
-- object the EVENT named rather than about the source. Rule 702.100a's evolve is
-- the case -- "that creature's power" is the entrant, reached through
-- Quantity.AgainstSlot at Binding.became, and an entrant killed while the trigger
-- waits is still compared at the power and toughness it last had on the
-- battlefield (rule 702.100a's own rulings say so).
--
-- viewWithLastKnown above is this function scoped to one id, and delegates rather
-- than restating the record read, so the two cannot drift. Widening THAT function
-- instead of adding this one would be wrong rather than merely broader: its
-- callers hand it the source of a RESOLVING spell or ability, and the other object
-- such an effect reads is its TARGET -- for which CR 608.2b says the opposite,
-- that an effect needing information about a target that has left "fails to
-- determine any such information". A blank answer is the rule there.
--
-- Aiming it at an id no ability named is safe, which is what lets it be a view
-- rather than a lookup: the ids that reach it are the ones a reference or a slot
-- names, since a Quantity.Count draws its candidates from zones and a zone holds
-- no id that names nothing (#160).
--
-- Nothing when the object is gone and nothing was filed for it, which lands on
-- the no-op every caller already gives an unevaluable quantity -- where fullView
-- would hand back a Just over an empty projection. The controller comes from the
-- record rather than Nothing, so a ControlledBy filter read against a gone object
-- still names whoever last controlled it, and so do the COUNTERS: CR 122.2 made
-- them cease to exist with the object, and the record is the only place
-- Quantity.ObjectCounters can still find them.
viewWithLastKnownAnywhere :: GameState -> Count.ViewOf
viewWithLastKnownAnywhere gs oid =
  if Map.member oid (GameState.objects gs)
    then fullView gs oid
    else
      fmap
        (\lk -> viewOfCharacteristics oid (LastKnown.characteristics lk) (Just (LastKnown.controller lk)) (LastKnown.counters lk) gs)
        (Map.lookup oid (GameState.lastKnown gs))

-- CR 608.2h: this object's last known information, and only when the id names
-- nothing -- Nothing while the object is still there, so a caller falls through
-- to its live reader. The shared liveness test for the two readers below, so the
-- rule cannot mean one thing for keywords and another for control.
-- viewWithLastKnown makes the same test inline: it wants Nothing when nothing was
-- filed, where fullView would hand back a Just over an empty projection.
lastKnownOf :: ObjectId -> GameState -> Maybe LastKnown.LastKnown
lastKnownOf oid gs =
  if Map.member oid (GameState.objects gs)
    then Nothing
    else Map.lookup oid (GameState.lastKnown gs)

-- keywordsOf with CR 608.2h's fallback, as CR 702.2e, CR 702.15c and CR 702.90d
-- ask for; toxic (rule 702.164) has no such clause and rides this by uniformity.
-- Falling back to the whole keyword map rather than one keyword is what keeps
-- the four answers from drifting.
keywordsWithLastKnown :: ObjectId -> GameState -> Map Keyword Natural
keywordsWithLastKnown oid gs = case lastKnownOf oid gs of
  Just lk -> PC.keywords (LastKnown.characteristics lk)
  Nothing -> keywordsOf oid gs

-- controllerOf with the same fallback. CR 702.15b is why a controller is wanted;
-- the authority for taking it from last known information is CR 608.2h's general
-- clause, not CR 702.15c, which licenses the fallback only for whether the
-- source had lifelink. LastKnown.controller is a PlayerId rather than a Maybe,
-- so this answers Just wherever the live reader would answer Nothing for a gone
-- source; Nothing survives only for an id nothing was filed under.
controllerWithLastKnown :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerWithLastKnown oid gs = case lastKnownOf oid gs of
  Just lk -> Just (LastKnown.controller lk)
  Nothing -> controllerOf oid gs

-- powerGiven with the same fallback, on the authority of CR 608.2b's own
-- sentence: "if the source of an ability has left the zone it was in, its last
-- known information is used during this process". The process is target
-- re-validation, and the source's power is what CR 702.134a's slot compares
-- against -- so a mentor killed in response leaves its trigger's target legal
-- rather than fizzling it. Pawl.Engine.Target.admittedGiven is the caller.
powerWithLastKnownGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Maybe Integer
powerWithLastKnownGiven pcs oid gs = case lastKnownOf oid gs of
  Just lk -> PC.power (LastKnown.characteristics lk)
  Nothing -> powerGiven pcs oid gs

-- The ViewOf a count gets when it is evaluated while `bound` is being applied:
-- candidates projected through the layers BEFORE that one. A candidate in a
-- library, hand, graveyard or exile has no projection at all, since gather does
-- not walk those zones, so it falls back to the printed card and is matched
-- against its PRINTED characteristics (#160). Its POWER is the one axis a rule
-- reaches into there: CR 208.2a makes a characteristic-defining one function everywhere,
-- which is why the fallback is viewOfCardIn rather than viewOfCard.
--
-- The STACK is projected, not fallen back on, and gatherGiven's CR 113.6 walk is
-- what makes that worth doing: a spell can carry a static ability of its own, so
-- a clause read against it must see the grant. It also brings this in line with
-- viewOfObject, which has never had a zone gate -- so a "target spell" filter
-- and an "as long as" clause now read one spell the same way. The axes that only
-- an object has (its controller, CR 702.33d's kicked flag) come with that, and
-- Molten Disaster's own clause is the reader of the second.
viewUpTo :: Layer -> [Gathered] -> GameState -> Count.ViewOf
viewUpTo bound cands gs oid =
  if Set.member oid (GameState.battlefield gs) || elem oid (GameState.stack gs)
    then Just (viewOfCharacteristics oid (projectUpTo bound cands oid gs) (controllerOf oid gs) (countersOf oid gs) gs)
    else fmap (viewOfCardIn gs oid) (Game.faceOf oid gs)

-- The characteristics view of a printed card off the battlefield, from the FACE
-- alone. No projection exists there, so every axis is read from the printed
-- face; the axes that only an OBJECT can have -- a controller, counters, an
-- attacking flag -- are Nothing or empty, and each says so at its field.
--
-- A reader holding a game state wants viewOfCardIn below instead, which is this
-- with CR 208.2a's characteristic-defining power filled in. This one survives
-- for the callers that hold no object id, and as that one's inner view.
viewOfCard :: Face.Face Card.Type.Card -> Filter.View
viewOfCard face =
  let typeLine = Face.typeLine face
   in Filter.MkView
        { Filter.cardTypes = TypeLine.types typeLine,
          Filter.supertypes = TypeLine.supertypes typeLine,
          -- CR 604.3 / 702.114a: a CDA functions in all zones, and nothing off
          -- the battlefield is projected (#160), so devoid is applied here
          -- rather than inherited from a fold this object never enters.
          Filter.colors =
            if definesColorless (Face.keywords face)
              then Set.empty
              else printedColorsOf face,
          -- CR 604.3 / 702.73a, the line above's reason one layer down: rule
          -- 702.73a says outright that changeling "works everywhere", and a
          -- card in a graveyard is never projected (#160), so it is applied
          -- here too.
          Filter.subtypes =
            if definesEveryCreatureType (Face.keywords face)
              then Set.union Subtype.everyCreatureType (TypeLine.subtypes typeLine)
              else TypeLine.subtypes typeLine,
          -- CR 702: read off the printed face, like the type line above.
          Filter.keywords = Face.keywords face,
          -- CR 208.1 read off the PRINTED power box -- see printedPower below.
          Filter.power = printedPower face,
          -- CR 208.1's other half, read the same way off the printed toughness
          -- box.
          Filter.toughness = printedToughness face,
          -- CR 202.3: answerable here for the same reason power above is -- the
          -- mana cost is printed on the card and rule 202.3 names no zone. This
          -- is the arm Ojutai's Command's "mana value 2 or less" reads, its
          -- candidates being cards in a graveyard.
          Filter.manaValue = Just (Quantity.manaValueOf face),
          Filter.controller = Nothing,
          -- CR 108.3 gives an owner to a card IN THE GAME, which is an object in
          -- some zone; this builder describes a printed FACE, so there is no
          -- object to read Object.owner off. Nothing, and OwnedBy vacuously
          -- False -- the posture the controller above takes, though for a
          -- different reason: `controller` is Nothing because CR 108.4 gives a
          -- card in a library none, where an owner it certainly has.
          --
          -- Not implemented: the owner of a card matched OFF the battlefield,
          -- where viewUpTo falls back here with an object id in hand and CR
          -- 400.3 would answer from Object.owner (#1068).
          Filter.owner = Nothing,
          -- Not an object, so no identity for IsSource to compare.
          Filter.identity = Nothing,
          Filter.playerIdentity = Nothing,
          -- CR 506.3 / 509.1a: a card off the battlefield is not a creature that
          -- can attack or block, and was never declared as an attacker; CR
          -- 303.4b: nor is it a permanent attached to anything.
          Filter.attacking = False,
          Filter.blocking = False,
          Filter.attackedThisTurn = False,
          Filter.attachedToCreature = False,
          Filter.attachedToPermanent = False,
          Filter.attachedTo = Nothing,
          -- CR 701.3a: only Pawl.Engine.Resolve's AttachTarget arm fills this field, and
          -- its candidates are battlefield permanents, so a card in a library or a
          -- hand is never asked whether an attach could land on it.
          Filter.canHostSubject = False,
          -- CR 111.6: "A token isn't a card." This builder describes a card in a
          -- zone the battlefield is not (a library search, viewUpTo's fallback),
          -- and CR 704.5d already made a token in any such zone cease to exist --
          -- so no token can reach here, and False is not a lost distinction.
          Filter.token = False,
          Filter.tapped = False,
          -- CR 122.1a-b: a counter can sit on a CARD in a zone other than the
          -- battlefield, but this builder describes a printed FACE rather than
          -- an object, so there is nothing here for one to be on. The vacuous
          -- posture the controller above already takes.
          Filter.counters = Map.empty,
          -- CR 701.54b: the designation rides an OBJECT (Object.ringBearerFor),
          -- and this builder describes a printed face. Nothing is not a lost
          -- distinction either: CR 701.54a designates a creature its controller
          -- controls, so only a battlefield permanent ever carries one.
          Filter.ringBearerFor = Nothing,
          -- The designations ride an OBJECT (Object.designations), and this builder
          -- describes a printed face. Not a lost distinction either: each of those
          -- rules gives its designation only to a permanent.
          Filter.designations = Set.empty,
          Filter.kicked = False,
          -- CR 602.1 / 605.1a off the PRINTED face, the posture `keywords` above
          -- takes: nothing off the battlefield is projected (#160).
          --
          -- The card's printed abilities plus rule 702's HAND ones, which CR 702.29b
          -- and CR 702.77b keep in existence in every zone. NOT the battlefield ones
          -- (crew, CR 702.122a): those are minted from the post-layer keyword map for
          -- a permanent, and CR 113.6's default already keeps them on the battlefield.
          Filter.nonManaActivatedAbility =
            not
              ( all
                  ManaAbility.isManaAbility
                  (Face.activatedAbilities face <> Keyword.handAbilitiesOf (Face.keywords face))
              )
        }

-- viewOfCard for a card that IS an object in some zone, so a
-- characteristic-defining power can be evaluated: CR 604.3 and CR 208.2a make a
-- CDA function in all zones, so Tarmogoyf in a library has the power its count
-- says, which is what Imperial Recruiter's "creature card with power 2 or less"
-- must read. The view every off-battlefield reader gets; viewOfCard survives for
-- the FACE-only callers, and as the blind inner view below.
--
-- Only the power and toughness axes differ, CR 208.2a naming both.
viewOfCardIn :: GameState -> ObjectId -> Face.Face Card.Type.Card -> Filter.View
viewOfCardIn gs oid face =
  (viewOfCard face)
    { Filter.power = characteristicPowerIn gs oid face,
      Filter.toughness = characteristicToughnessIn gs oid face
    }

-- CR 208.2a's power for a face whose CDA sets it, and printedPower's answer for
-- every other face -- including a face with a characteristicPT but no printed
-- power box, which seedCharacteristicPT rejects as malformed.
--
-- Quantity.determine rather than Quantity.evaluate, so CR 208.2a's last sentence
-- ("if the ability needs to use a number that can't be determined ... use 0")
-- applies here exactly as it does on the battlefield in applyCharacteristicPT.
--
-- The context is the CDA's OWN and never the reading effect's: CR 604.3a(3) says
-- a CDA affects no other object, and CR 109.5 resolves its "you" to the object's
-- controller or, having none in a library or a graveyard, its owner -- which is
-- what controllerOf answers there (see defaultControllerOf and CR 108.4a). A
-- searching player's perspective would be a different player's "you".
--
-- The injected ViewOf is the CDA-BLIND viewOfCard, which is what bounds the
-- descent: Tarmogoyf's count sweeps every graveyard, so a Tarmogoyf in a
-- graveyard is one of its own candidates, and injecting this function would make
-- a CDA that read a candidate's POWER recur without end. Nothing in the pool
-- does -- swapping the injection leaves the suite green, since DistinctCardTypes
-- forces only Filter.cardTypes -- so the bound is unobserved today and is here
-- for the CDA that would observe it. Blind is also the posture viewUpTo already
-- takes for an off-battlefield candidate (#160).
--
-- Not implemented: a BATTLEFIELD candidate that a CDA evaluated here sweeps is
-- read as a printed card rather than through its CR 613 projection (#1080).
characteristicPowerIn :: GameState -> ObjectId -> Face.Face Card.Type.Card -> Maybe Integer
characteristicPowerIn gs oid face = case seedCharacteristicPT face of
  Nothing -> printedPower face
  Just (power, _) ->
    let context = Filter.contextFor (controllerOf oid gs) (Just oid)
        viewOf candidate = fmap viewOfCard (Game.faceOf candidate gs)
     in Just (Quantity.determine viewOf context gs oid power)

-- characteristicPowerIn's mirror, reading CR 208.2a's other half. Every argument
-- that function's comment makes is this one's too, the two differing only in
-- which member of the pair they take.
characteristicToughnessIn :: GameState -> ObjectId -> Face.Face Card.Type.Card -> Maybe Integer
characteristicToughnessIn gs oid face = case seedCharacteristicPT face of
  Nothing -> printedToughness face
  Just (_, toughness) ->
    let context = Filter.contextFor (controllerOf oid gs) (Just oid)
        viewOf candidate = fmap viewOfCard (Game.faceOf candidate gs)
     in Just (Quantity.determine viewOf context gs oid toughness)

-- CR 208.1's PRINTED power box, for a card off the battlefield where there is no
-- projection to read one from. Nothing for a face with no power box:
-- CR 208.1 gives power only to creature cards, so a land or an instant has none
-- to report, and Filter.PowerAtMost/PowerAtLeast answer False for it either way.
--
-- CR 208.2b's zero is the STAR's answer here, and only here: "While the card
-- isn't on the battlefield, its power and toughness are each considered to be
-- 0." Primal Plasma is the card that sentence is about -- its star is set by an
-- as-enters replacement effect, so off the battlefield nothing has set it. That
-- is deliberately NOT Quantity.evaluate's Star arm, which stays Nothing: that
-- arm answers for the projection seed, where a star that survived
-- baseCharacteristics is a hole rather than a zero.
--
-- A face with a characteristicPT answers Nothing HERE because a face alone
-- cannot evaluate one -- CR 208.2a's number is characteristicPowerIn's above,
-- which every reader that holds a game state goes through.
printedPower :: Face.Face Card.Type.Card -> Maybe Integer
printedPower face = case Face.characteristicPT face of
  Just _ -> Nothing
  Nothing -> case fmap Power.unwrap (Face.power face) of
    Just (Quantity.Type.Literal n) -> Just n
    Just Quantity.Type.Star -> Just 0
    -- Every other shape: no power box at all, or a printed box holding something
    -- neither a number nor CR 208.2's bare star. The latter is unreachable --
    -- CR 208.2 makes a star stand for a characteristic-defining ability, so a
    -- composite box like 1+* comes with a characteristicPT and left through the
    -- arm above -- and Nothing is the honest answer rather than a guessed number.
    _ -> Nothing

-- printedPower's mirror, arm for arm, on the printed toughness box. CR 208.2b's
-- sentence names power and toughness together, so the star reads 0 here too.
printedToughness :: Face.Face Card.Type.Card -> Maybe Integer
printedToughness face = case Face.characteristicPT face of
  Just _ -> Nothing
  Nothing -> case fmap Toughness.unwrap (Face.toughness face) of
    Just (Quantity.Type.Literal n) -> Just n
    Just Quantity.Type.Star -> Just 0
    _ -> Nothing

-- CR 508.3a: does this event record THIS object being declared as an attacker?
-- Only Combat.declareAttackers appends one, which is what keeps CR 508.4's
-- creature put onto the battlefield attacking -- one that "never attacked" --
-- out of the answer.
declaredIt :: ObjectId -> GameEvent.GameEvent -> Bool
declaredIt oid event = case event of
  GameEvent.AttackerDeclared declared _ _ -> declared == oid
  _ -> False

-- Shared assembly: fill a View from a projection's characteristics, a supplied
-- controller and supplied counters.
--
-- The COUNTERS come in as an argument for the reason the controller does: CR
-- 109.3's characteristic list has no counters in it, so a ProjectedCharacteristics
-- carries none -- CR 613.4c folded them into the power and toughness and then the
-- counters themselves are gone from that record. The caller has to say what was
-- on the object, and only the caller knows whether it is reading a live one or CR
-- 608.2h's record of one that is not.
viewOfCharacteristics :: ObjectId -> ProjectedCharacteristics -> Maybe PlayerId.PlayerId -> Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural -> GameState -> Filter.View
viewOfCharacteristics oid pc controller counters gs =
  Filter.MkView
    { Filter.cardTypes = PC.cardTypes pc,
      -- CR 205.4 / 613.1d off the PROJECTION, beside cardTypes rather than off
      -- the printed type line: layer 4 writes supertypes too, so a permanent
      -- Leyline of Singularity made legendary matches "legendary permanent".
      Filter.supertypes = PC.supertypes pc,
      Filter.colors = PC.colors pc,
      Filter.subtypes = PC.subtypes pc,
      -- CR 109.3 / 613.1f: abilities are characteristics and layer 6 writes them,
      -- so this comes off the PROJECTION alongside cardTypes and colors -- a
      -- creature that gained flying matches, and one under Humility does not.
      -- Map.keysSet because PC.keywords counts instances (CR 702) and
      -- Filter.HasKeyword asks only membership.
      Filter.keywords = Map.keysSet (PC.keywords pc),
      Filter.power = PC.power pc,
      Filter.toughness = PC.toughness pc,
      -- CR 202.3 / 707.2 off the PROJECTION, not the printed face: mana cost is
      -- one of the copiable values, so layer 1 replaces it and a Clone entering
      -- as a copy of Darksteel Myr reports 3 rather than its own printed 4. The
      -- seed (baseCharacteristics) is where the printed cost is read and where
      -- CR 712.8e's front-face rule and CR 708.2a's face-down rule are honoured.
      Filter.manaValue = PC.manaValue pc,
      Filter.controller = controller,
      -- CR 108.3 / 110.2 / 111.2: read straight off the OBJECT rather than off
      -- the projection beside it and rather than through the `controller`
      -- parameter, which is what makes Garland's "you control but don't own" a
      -- real question -- CR 613.1b's layer 2 has moved control by the time this
      -- is assembled, and nothing has moved ownership. A token's owner is the
      -- player who created it (CR 111.2), which Event.createTokens wrote into
      -- Object.owner, so it needs no separate case.
      --
      -- Nothing when the id names nothing, which is CR 608.2h's territory: this
      -- is reached with a last-known record for an object that has left, and
      -- Pawl.Types.LastKnown carries a controller but no owner (#1069).
      Filter.owner = fmap Object.owner (Game.lookupObject oid gs),
      Filter.identity = Just oid,
      Filter.playerIdentity = Nothing,
      -- CR 508.1k: attacking is a combat status, not a characteristic (CR 109.3),
      -- so it comes off the combat record rather than the projection.
      Filter.attacking = Map.member oid (Combat.attackers (GameState.combat gs)),
      -- CR 509.1g: likewise. Combat.blockers is keyed by ATTACKER, so blocking is
      -- membership in some attacker's set rather than a key lookup -- Map.member
      -- would be Pawl.Engine.Combat.isBlocked's question instead (CR 509.1h).
      Filter.blocking = any (Set.member oid) (Map.elems (Combat.blockers (GameState.combat gs))),
      -- CR 608.2i: read from the turn's event log, not the combat record, which
      -- CR 511.3 clears at end of combat. The log spans the turn.
      Filter.attackedThisTurn = any (declaredIt oid . snd) (GameState.events gs),
      -- CR 701.3a: also not a characteristic, so the attachment comes off
      -- Object.attachedTo -- but the HOST's creature-ness is projected (layer 4
      -- can make a land a creature), so it goes through isCreatureOf. That is why
      -- this field must stay lazy: `affects` calls this function from inside a
      -- projection, and forcing a second one would recurse. A player host answers
      -- False, which is Recipient.objectOf's Nothing.
      Filter.attachedToCreature = case Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf of
        Nothing -> False
        Just host -> isCreatureOf host gs,
      -- CR 303.4 / 110.1: the same attachment, asked whether it names an object
      -- on the battlefield. The membership test rules out a stale attachment to a
      -- host that has already left -- CR 704.5m buries such an Aura, but only on
      -- the next pass. No projection of another object, so no recursion hazard.
      Filter.attachedToPermanent = case Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf of
        Nothing -> False
        Just host -> Set.member host (GameState.battlefield gs),
      -- CR 701.3a / 301.5a: the same attachment a third time, kept as the HOST'S ID
      -- rather than collapsed to a Bool -- IsAttachedToSource compares it against
      -- the match's source, which this builder does not know. Deliberately NOT
      -- narrowed to a host on the battlefield the way `attachedToPermanent` above
      -- is: an id is answered or it is not, and the atom's own comparison against a
      -- source already rules out a host that has left.
      Filter.attachedTo = Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf,
      -- CR 701.3a: filled only by Resolve's AttachTarget arm, the one place that
      -- knows what is being moved. "Could the subject be attached here" is not a
      -- question about the candidate alone.
      Filter.canHostSubject = False,
      -- CR 111.6: fixed for the life of the object (CR 400.7 mints a new one on
      -- every zone change), so it is a constant input to the projection.
      Filter.token = Game.isToken oid gs,
      Filter.tapped = Game.isTapped oid gs,
      Filter.counters = counters,
      -- CR 701.54b: a designation rather than a characteristic, so it comes off
      -- Object.ringBearerFor rather than off `pc` -- the posture `tapped` and
      -- `token` already take. Nothing for an id naming no object, which is what
      -- viewWithLastKnown's CR 608.2h path hands this function: a designation dies
      -- with the permanent (CR 400.7), and no last-known record keeps one.
      Filter.ringBearerFor = Game.lookupObject oid gs >>= Object.ringBearerFor,
      -- Designations rather than characteristics, so they come off
      -- Object.designations -- ringBearerFor's posture just above, including for the
      -- CR 608.2h path: an id naming no object answers with the empty set, a
      -- designation dying with the permanent (CR 400.7) and no last-known record
      -- keeping one.
      Filter.designations = maybe Set.empty Object.designations (Game.lookupObject oid gs),
      -- CR 702.33d: `designations` above in every respect -- read live off the object,
      -- so the CR 608.2h path answers False for a spell that has left the stack.
      -- Nothing asks it there: the only reader is the kicked spell's own clause
      -- condition, gated while it is still on the stack
      -- (Pawl.Engine.Resolve.gateHolds).
      Filter.kicked = maybe False Object.kicked (Game.lookupObject oid gs),
      -- CR 602.1 / 605.1a, off the PROJECTION like `keywords` above: abilities are
      -- characteristics (CR 109.3) written by layer 6, so a land under Humility has
      -- none and Tsabo's Web stops seeing it.
      --
      -- The whole list the object HAS, which abilitiesFromCharacteristics assembles
      -- -- printed, rule 702's battlefield ones, and rule 702's hand ones, the last
      -- of which CR 702.29b and CR 702.77b keep in existence here. Not the list it
      -- can activate here: that is Activate.abilitiesForGiven's narrower question.
      --
      -- LAZY -- see the field's own comment in Pawl.Engine.Filter.
      Filter.nonManaActivatedAbility = not (all ManaAbility.isManaAbility (abilitiesFromCharacteristics pc oid gs))
    }

-- CR 122.1: the counters on an object right now, and none for an id that names
-- nothing. The LIVE half of the pair whose other half is LastKnown.counters --
-- every viewOfCharacteristics caller but viewWithLastKnown passes this.
countersOf :: ObjectId -> GameState -> Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
countersOf oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)

-- CR 707.2 / 613.1a: an object's layer-1 (copy) result, the value the layer fold
-- starts from -- the entry-stamped snapshot (CR 707.5) when it has one, the
-- printed base otherwise. Base-or-snapshot only, so counters, pumps, control and
-- ability grants are structurally never part of a copied object's copiable value.
-- Not a recursion: a copy of a copy already stored resolved values at entry.
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
seedCharacteristicPT :: Face.Face Card.Type.Card -> Maybe (Quantity.Type.Quantity, Quantity.Type.Quantity)
seedCharacteristicPT face =
  case (Face.characteristicPT face, Face.power face, Face.toughness face) of
    (Just star, Just (Power.MkPower p), Just (Toughness.MkToughness t)) ->
      Just (Quantity.substituteStar star p, Quantity.substituteStar star t)
    _ -> Nothing

-- Printed characteristics before any effect: CR 613.1's starting point.
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.faceOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { -- No card behind this object (an ability on the stack): nothing to seed
        -- from.
        PC.names = Set.empty,
        PC.supertypes = Set.empty,
        PC.keywords = Map.empty,
        PC.colors = Set.empty,
        -- No card, so no mana cost to read -- which is not CR 202.3a's 0 (#674).
        PC.manaValue = Nothing,
        PC.power = Nothing,
        PC.toughness = Nothing,
        PC.loyalty = Nothing,
        PC.defense = Nothing,
        PC.characteristicPT = Nothing,
        PC.cardTypes = Set.empty,
        PC.subtypes = Set.empty,
        PC.activatedAbilities = [],
        PC.replacementEffects = [],
        PC.triggeredAbilities = []
      }
  Just face ->
    -- The seed predates every layer, so a Count reached from here gets a viewOf
    -- that determines nothing rather than one recursing back into
    -- copiableCharacteristics. The tradeoff is that such a Count folds an empty
    -- candidate set and aggregates to Just 0 rather than an honest Nothing; no
    -- card in the pool has one (#156). The context is the object's own
    -- controller, the CR 604.3a(3) posture this shares with
    -- applyCharacteristicPT.
    let seedViewOf = const Nothing
        seedContext = Filter.contextFor (controllerOf oid gs) (Just oid)
     in PC.MkProjectedCharacteristics
          { -- CR 709.4a: the names the object shows, which the `face` bound
            -- above cannot carry -- its own name is the halves' joined
            -- rendering. Game.namesOf is resolveFaceFor's plural companion and
            -- decides which halves show by the same arms.
            PC.names = Game.namesOf oid gs,
            PC.supertypes = TypeLine.supertypes (Face.typeLine face),
            -- CR 702: a printed keyword appears once, so the seed's count is 1
            -- apiece; layer-6 grants add multiplicity on top (CR 702.164b).
            PC.keywords = Map.fromSet (const 1) (Face.keywords face),
            PC.colors = printedColorsOf face,
            -- CR 202.3, derived here so the rest of the fold -- and every copy
            -- taken off it (CR 707.2) -- reads a number rather than re-deriving
            -- one. Game.manaCostFaceOf rather than the `face` bound above, which
            -- is Game.faceOf's: CR 712.8e reads a transformed permanent's mana
            -- value off its FRONT face's cost while every other characteristic
            -- here comes off its back, and CR 708.2a's face-down face (no mana
            -- cost, so CR 202.3a's 0) is the other case that read handles. A
            -- TOKEN's effect-defined card has no mana cost either, so it reaches
            -- 0 down the same path.
            PC.manaValue = fmap Quantity.manaValueOf (Game.manaCostFaceOf oid gs),
            -- Quantity.evaluate, not Quantity.determine: CR 208.2a's "use 0
            -- instead" belongs to a CDA, and a printed star with none behind it
            -- evaluates to Nothing. A star given its value by an as-enters
            -- replacement (CR 208.2b) therefore reports Nothing off the
            -- battlefield where that rule says 0 (#76). A star that does have a
            -- CDA is Nothing only until layer 7a, where applyCharacteristicPT
            -- determines the pair seedCharacteristicPT left in characteristicPT.
            PC.power = case Face.power face of
              Nothing -> Nothing
              Just (Power.MkPower q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
            PC.toughness = case Face.toughness face of
              Nothing -> Nothing
              Just (Toughness.MkToughness q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
            -- CR 306.5a: a literal number, so copied through rather than
            -- evaluated like the two fields above.
            PC.loyalty = Face.loyalty face,
            -- CR 310.4a: a literal number, copied through for CR 306.5a's reason.
            PC.defense = Face.defense face,
            PC.characteristicPT = seedCharacteristicPT face,
            PC.cardTypes = TypeLine.types (Face.typeLine face),
            PC.subtypes = TypeLine.subtypes (Face.typeLine face),
            PC.activatedAbilities = Face.activatedAbilities face,
            PC.replacementEffects = Face.replacementEffects face,
            PC.triggeredAbilities = Face.triggeredAbilities face
          }

-- CR 202.2 / 204.2 / 202.2b: an object's printed colours, from its mana cost's
-- coloured symbols and its colour indicator.
--
-- No devoid here: CR 702.114a makes it a CDA, and CR 613.3 puts CDAs at the start
-- of their layer (5 for colour, CR 613.1e), not before the fold begins.
-- applyColorDefining is where it lands.
printedColorsOf :: Face.Face Card.Type.Card -> Set Color.Color
printedColorsOf face =
  Set.union
    (Face.colorIndicator face)
    (manaCostColors (Face.manaCost face))

-- CR 702.114a. The one place that decides what devoid means, so the fold and the
-- off-battlefield card view cannot drift apart on it.
definesColorless :: Set Keyword -> Bool
definesColorless = Set.member Keyword.Type.Devoid

-- CR 702.73a, definesColorless' twin one layer down: the one place that decides
-- what changeling means, for the same reason.
definesEveryCreatureType :: Set Keyword -> Bool
definesEveryCreatureType = Set.member Keyword.Type.Changeling

-- CR 613.3 / 613.1e: the object's own colour-defining ability, applied at the
-- start of layer 5.
--
-- Folded in place rather than emitted as a synthetic Gathered, the shape
-- applyCharacteristicPT also takes: a CDA affects only its own object (CR
-- 604.3a(3)) so there is no affected set to gather; CR 604.3 makes it function in
-- all zones while gather walks the battlefield only; and it has no timestamp to
-- sort on under CR 613.7.
--
-- Read from the partial projection's keywords rather than the card, because at
-- layer 5 that map holds exactly CR 604.3a(2)'s sources: the printed and
-- copy-effect halves arrive in the seed, no pre-layer-5 modification adds a
-- keyword (the only ones touching the map go through setLandSubtypeTo, which only
-- empties it), and layer 6 has not applied yet. So the rule holds by construction.
-- Humility cannot remove it either, LoseAllAbilities being layer 6.
--
-- A devoid GRANTED by another object's static ability deliberately never reaches
-- here: CR 604.3a denies it CDA status, so grantedDefiningParts routes it into layer
-- 5 as a timestamped colour effect instead. Pawl.ColorSpec's "CR 613.7a a granted
-- devoid clears an OLDER 'in addition' colour" is what holds the two routes apart:
-- widening this fold to reach a granted instance answers that case blue.
applyColorDefining :: ProjectedCharacteristics -> ProjectedCharacteristics
applyColorDefining pc =
  if definesColorless (Map.keysSet (PC.keywords pc))
    then pc {PC.colors = Set.empty}
    else pc

-- CR 613.3 / 613.1d: the object's own subtype-defining ability, CR 702.73a's
-- changeling, applied at the start of layer 4. applyColorDefining's shape one
-- layer down, and every reason in its header carries over unchanged.
--
-- A UNION rather than a set: rule 702.73a says the object IS every creature
-- type, and CR 205.1b keeps the subtypes of its other families -- a Vehicle
-- with changeling stays a Vehicle. Adding types the object already had is the
-- identity, so a second instance changes nothing.
--
-- Being at the START of layer 4 is what makes Turn to Frog's SetCreatureSubtype
-- win over it: that effect strips every creature type and inserts Frog, and it
-- folds after this. Humility cannot undo it either, LoseAllAbilities being
-- layer 6.
--
-- A changeling GRANTED by another object's static ability deliberately never
-- reaches here, devoid's posture above verbatim: CR 604.3a denies it CDA status,
-- so grantedDefiningParts routes it into layer 4 as a timestamped
-- AddEveryCreatureSubtype instead. Pawl.ProjectionSpec's "CR 613.7a a granted
-- changeling beats an OLDER Turn to Frog, where a printed one loses to it" is
-- what holds the two routes apart: widening this fold to reach a granted instance
-- answers that case Frog.
applySubtypeDefining :: ProjectedCharacteristics -> ProjectedCharacteristics
applySubtypeDefining pc =
  if definesEveryCreatureType (Map.keysSet (PC.keywords pc))
    then pc {PC.subtypes = Set.union Subtype.everyCreatureType (PC.subtypes pc)}
    else pc

-- CR 202.1b: a land has no mana cost at all, so it contributes no colours.
manaCostColors :: Maybe ManaCost.ManaCost -> Set Color.Color
manaCostColors mc = case mc of
  Nothing -> Set.empty
  Just (ManaCost.MkManaCost symbols) -> Set.fromList (concatMap symbolColors symbols)

-- CR 202.2b: only a coloured mana symbol carries a colour. Generic, {X} and {C}
-- carry none, colourless not being a colour (CR 105.2c).
--
-- A list rather than a Maybe because a hybrid symbol is all of its component
-- colours (CR 107.4e / 202.2d). Contributions rather than a set: the caller is
-- the one building the set, and manaCostColors absorbs repeats there.
symbolColors :: ManaSymbol.ManaSymbol -> [Color.Color]
symbolColors symbol = case symbol of
  ManaSymbol.OfType (ManaType.Colored c) -> [c]
  ManaSymbol.OfType ManaType.Colorless -> []
  ManaSymbol.Hybrid a b -> Maybe.mapMaybe colorOfManaType [a, b]
  -- CR 107.4b/107.4e: a monocolored hybrid's other half is generic, which is not
  -- one of CR 107.4a's coloured symbols, so the named half is the whole
  -- contribution.
  ManaSymbol.MonocoloredHybrid t -> Maybe.maybeToList (colorOfManaType t)
  -- CR 107.4f / 202.2d: Phyrexian symbols are coloured mana symbols. Total `[c]`
  -- rather than a mapMaybe, since Phyrexian carries a Color and not a ManaType --
  -- there is no colourless Phyrexian symbol. Its other half is life, which is no
  -- mana and so no colour.
  ManaSymbol.Phyrexian c -> [c]
  -- CR 107.4h: snow is neither a colour nor a type of mana, and CR 202.2d's list
  -- does not name it.
  ManaSymbol.Snow -> []
  ManaSymbol.Generic _ -> []
  ManaSymbol.Variable -> []

-- CR 105.2c: colourless is not a colour, so a colourless half of a hybrid symbol
-- (CR 107.4e allows one) contributes none.
colorOfManaType :: ManaType.ManaType -> Maybe Color.Color
colorOfManaType manaType = case manaType of
  ManaType.Colored c -> Just c
  ManaType.Colorless -> Nothing

-- affects evaluated against an object's BASE characteristics (used by
-- source-liveness, which must not recurse into the projection it feeds).
affectsBase :: ObjectId -> ObjectId -> Affected.Affected -> GameState -> Bool
affectsBase source oid a gs = affects source oid a (baseCharacteristics oid gs) gs

-- CR 608.2h / 611.2d: evaluate a modification's quantities once and rewrite them
-- to literals. Called by Resolve when a spell's resolution stores a continuous
-- effect.
--
-- `oid` is the SOURCE, not the affected object: for a spell that is also where CR
-- 601.2b's chosen X was stamped, and `you` is its controller. Not the announcing
-- object for an activated ability, where the two differ -- an X-cost activation
-- storing a continuous effect measured by its X would freeze nothing here, and no
-- card in the pool does (#550).
--
-- Deliberately not applied to a static ability's effect: CR 611.2 scopes 611.2a-d
-- to a spell or ability's resolution, while a static ability's effect (CR 604.2)
-- is regenerated per projection and must keep moving.
--
-- Nor to the effect frozenStaticParts hands over as a permanent leaves, which
-- is the same reason one step on: CR 611.2d fixes a variable "only once, on
-- resolution", and nothing resolved there. So Titania's Song's "power and
-- toughness each equal to its mana value" keeps reading mana value live after
-- the Song is gone, exactly as it did before -- which is what "this effect
-- continues" asks for. CR 611.2c's affected set is the one thing that DOES get
-- fixed there, and it is fixed because the rule names it.
--
-- Nothing when any quantity cannot be evaluated at store time -- a value
-- undeterminable at that one moment cannot be determined later either, so
-- re-reading it live would be wrong rather than deferred, and Resolve stores
-- nothing. Not Literal 0, which would invent an answer: CR 208.2a's "use 0
-- instead" is scoped to a CDA, and this is not one.
freezeQuantities :: GameState -> ObjectId -> Maybe PlayerId.PlayerId -> Modification -> Maybe Modification
freezeQuantities gs oid you m =
  let viewOf = fullView gs
      context = Filter.contextFor you (Just oid)
      freeze q = fmap Quantity.Type.Literal (Quantity.evaluate viewOf context gs oid q)
   in case m of
        Modification.SetBasePowerToughness p t -> Modification.SetBasePowerToughness <$> freeze p <*> freeze t
        Modification.ModifyPowerToughness p t -> Modification.ModifyPowerToughness <$> freeze p <*> freeze t
        -- Every other modification carries no quantity to freeze, so it stores as
        -- written; named explicitly per Modification's exhaustiveness discipline.
        Modification.GainKeyword _ -> Just m
        Modification.LoseAllAbilities -> Just m
        Modification.SetLandSubtype _ -> Just m
        Modification.SetLandSubtypeToChosen -> Just m
        Modification.AddLandSubtype _ -> Just m
        Modification.SetCreatureSubtype _ -> Just m
        Modification.AddCreatureSubtype _ -> Just m
        Modification.AddEveryCreatureSubtype -> Just m
        Modification.AddCardType _ -> Just m
        Modification.AddSupertype _ -> Just m
        Modification.RemoveSupertype _ -> Just m
        Modification.ChangeSubtypeWord _ _ -> Just m
        Modification.SetController _ -> Just m
        Modification.SetControllerToSource -> Just m
        Modification.SetColor _ -> Just m
        Modification.AddColor _ -> Just m
        Modification.AddChosenColor -> Just m
        Modification.SwitchPowerToughness -> Just m

-- Every Quantity a modification carries, in order. When a Modification gains a
-- Quantity field, add it here as well as to freezeQuantities -- the compiler
-- forces the arm to exist, not to be right.
quantitiesOf :: Modification -> [Quantity.Type.Quantity]
quantitiesOf m = case m of
  Modification.SetBasePowerToughness p t -> [p, t]
  Modification.ModifyPowerToughness p t -> [p, t]
  Modification.GainKeyword _ -> []
  Modification.LoseAllAbilities -> []
  Modification.SetLandSubtype _ -> []
  Modification.SetLandSubtypeToChosen -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCreatureSubtype _ -> []
  Modification.AddEveryCreatureSubtype -> []
  Modification.AddCardType _ -> []
  Modification.AddSupertype _ -> []
  Modification.RemoveSupertype _ -> []
  Modification.ChangeSubtypeWord _ _ -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.AddColor _ -> []
  Modification.AddChosenColor -> []
  Modification.SwitchPowerToughness -> []

-- Every SetLandSubtype and SetLandSubtypeToChosen effect in the game, each with
-- its source and affected set (from stored effects and battlefield permanents'
-- static abilities). This is a legitimate case-on-Modification -- Projection is
-- its sole home.
setLandSubtypeEffects :: GameState -> [(ObjectId, Affected.Affected)]
setLandSubtypeEffects gs =
  let isSet m = case m of
        Modification.SetLandSubtype _ -> True
        -- CR 305.7 does not care where the type came from: a type chosen as the
        -- source entered (CR 614.1c) strips rules text as a printed one does.
        -- Answering False would strip the land inside the fold while leaving its
        -- static abilities in the candidate list, the two halves of one rule
        -- disagreeing -- and the wildcard below means the compiler cannot name a
        -- new arm that belongs here.
        Modification.SetLandSubtypeToChosen -> True
        -- A control op, not a type change; named explicitly per Modification's
        -- exhaustiveness discipline.
        Modification.SetController _ -> False
        Modification.SetControllerToSource -> False
        -- The OTHER subtype set, and the arm most at risk of being read as this
        -- one: CR 305.7's ability strip is about a LAND whose subtype is set, and
        -- CR 205.1a/205.1b's creature-type set carries no such clause.
        Modification.SetCreatureSubtype _ -> False
        Modification.AddCreatureSubtype _ -> False
        Modification.AddEveryCreatureSubtype -> False
        _ -> False
      fromStored eff =
        if isSet (ContinuousEffect.modification eff)
          then [(ContinuousEffect.source eff, ContinuousEffect.affected eff)]
          else []
      -- The affected set is REWRITTEN here, the same CR 612 word swap gatherStatic
      -- applies to the same ability's set (#624). The two must agree: this gate
      -- decides whose rules-text abilities CR 305.7 strips, and the layer fold
      -- decides what the subtype becomes, so a text change reaching only one of
      -- them would have the halves of one rule disagreeing about which permanents
      -- an ability names.
      --
      -- Conversion -- "All Mountains are Plains" -- is the pool's first static
      -- ability whose affected set names a basic land type, so it is the first
      -- card a Magical Hack could aim at this read-point at all.
      --
      -- Proved by Pawl.ProjectionSpec's "Conversion strips the Estuary's ability,
      -- and CR 612.1 hands it back". Seeing this rewrite at all takes a permanent
      -- whose PRINTED type line carries a basic land type AND which has a
      -- rules-text static ability reaching other objects -- affectsBase reads base
      -- characteristics, so both sides have to be true of one card. Synthetic
      -- Volcanic Estuary is that card, and the spec's comment says why it is
      -- written rather than found.
      --
      -- textChangesAffecting folds the whole effect list, and this function is
      -- hoisted out of gather's walk to keep that off the per-permanent path, so
      -- the fold runs once per battlefield permanent here rather than once per
      -- permanent per projection.
      --
      -- The MODIFICATIONS stay unrewritten, and the isSet filter is why that is
      -- sound: a word swap rewrites which subtype a set arm names, never whether
      -- it is a set arm, so the filter's answer cannot change. The fold does the
      -- rewriting that decides the resulting subtype.
      fromPerm permId = case Game.faceOf permId gs of
        Nothing -> []
        Just face ->
          let changes = textChangesAffecting permId gs
           in fmap (\sa -> (permId, rewriteAffected changes (StaticAbility.affected sa))) $
                filter (any isSet . StaticAbility.modifications) (Face.staticAbilities face)
   in concatMap fromStored (GameState.continuousEffects gs)
        <> concatMap fromPerm (Set.toList (GameState.battlefield gs))

-- CR 305.7: a land whose subtype is set to a basic type loses its rules-text
-- abilities. This is the GATE half of that rule, for the readers whose ability
-- lands on objects other than the bearer, since such an ability must be kept out
-- of the candidate list rather than erased from the bearer's projection
-- afterwards. Every other rules-text ability is stripped inside the fold by
-- setLandSubtypeTo.
--
-- The effect list is hoisted so a caller computes it once rather than once per
-- permanent, which made project O(permanents^3) per SBA sweep. `oid`'s rules-text
-- abilities survive iff no effect that ACTUALLY APPLIES reaches it. Which ones
-- apply is rule 613.8's question, answered once by `appliedSetEffects` below --
-- dependency order, falling back to timestamp order inside a dependency loop (CR
-- 613.8b). This function only asks the applied ones whether they name `oid`.
--
-- This is the INSIDE-THE-FOLD gate, and gather (via permanentParts) is its one
-- caller. "Applies to" reads BASE characteristics so nothing recurses into the
-- projection being built. That restriction costs a real case: a permanent that
-- becomes a land only through a layer-4 type change is invisible here while the
-- fold's own arm reaches it, so the two halves of CR 305.7 still disagree about
-- its STATIC abilities -- Lord of Atlantis under Ashaya and Blood Moon goes on
-- pumping other Merfolk (#391). Every reader OUTSIDE the fold uses
-- liveAfterLayers below, which has no such disagreement, so a PLAYER ability on
-- the same board is already right -- Pawl.PlayerEffectSpec's "CR 305.7 Ashaya
-- animates Thalia into a Mountain, so Blood Moon takes her tax" proves it.
--
-- The layer-2 control fold deliberately asks NEITHER gate -- see controlGrants for
-- the CR 613.1 argument, and for the divergence a gate there caused.
liveGiven :: [(ObjectId, Affected.Affected)] -> ObjectId -> GameState -> Bool
liveGiven setEffs oid gs =
  not (any (\(src, aff) -> affectsBase src oid aff gs) (appliedSetEffects setEffs gs))

-- The same CR 305.7 gate for a reader OUTSIDE the layer fold. CR 613.10 applies a
-- player effect "after the determination of objects' characteristics" and CR
-- 613.11 a rules-modifying effect "after all other continuous effects have been
-- applied", so by the time such a reader runs the projection is finished and there
-- is nothing to be circular about -- which is why this may read the projection
-- where liveGiven, called from inside the fold, must read base.
--
-- Not a fixpoint: the readers below call project, project's fold calls liveGiven,
-- and liveGiven bottoms out at baseCharacteristics, which folds nothing. Nothing
-- lower on that ladder calls anything higher.
--
-- WHICH effects apply is still liveGiven's layer-4 question, answered by
-- appliedSetEffects against base characteristics. Only the final membership test
-- -- does an applied setter's affected set name THIS object? -- moves to the
-- finished projection, which is what keeps CR 613.8's intra-layer ordering out of
-- here.
--
-- Every reader after the layers shares this, and each is pinned SEPARATELY, so
-- re-scoping one of them cannot hide behind the others: Pawl.CombatSpec's
-- landSubtypeStripSpec has a case per combat reader, Pawl.SacrificeRestrictionSpec
-- has the prohibition, and Pawl.PlayerEffectSpec's Thalia case, cited above, has
-- the player ability. Rewriting this body to read base fails all of them; swapping
-- one caller's call for liveGiven fails that caller's case alone.
liveAfterLayers :: [(ObjectId, Affected.Affected)] -> ObjectId -> GameState -> Bool
liveAfterLayers setEffs oid gs =
  let view = project oid gs
   in not (any (\(src, aff) -> affects src oid aff view gs) (appliedSetEffects setEffs gs))

-- CR 613.8: which of the CR 305.7 subtype-setting effects actually apply, in the
-- order the rule applies them.
--
-- The rule pawl used to skip. An effect that sets a land's subtype strips that
-- land's rules-text abilities, so it can switch OFF another such effect -- which
-- by CR 613.8a is a dependency, since "applying the other would change the text or
-- the EXISTENCE of the first effect". Two of them can depend on each other, and
-- rule 613.8b says what to do then: "if several dependent effects form a
-- dependency loop, then this rule is ignored and the effects in the dependency
-- loop are applied in timestamp order."
--
-- The walk, which is rules 613.8a-c in order:
--
--   * 613.8a -- e1 depends on e2 when e2's affected set names e1's SOURCE, so
--     applying e2 would take e1's ability away.
--   * 613.8b's first sentence -- an effect with an unapplied dependency waits, so
--     each step picks only from the effects that have none.
--   * 613.8b's last sentence -- if EVERY remaining effect is waiting, they are all
--     in loops, so the rule is ignored and the earliest timestamp goes next.
--   * 613.8c -- the order is recomputed after each application, which is what
--     makes this a loop rather than a sort: `remaining` shrinks and the
--     dependency test is asked afresh each time.
--
-- An effect whose source has already been stripped by an applied effect is
-- dropped rather than applied: its ability no longer exists, so it generates
-- nothing. An effect that strips its OWN source still applies -- it is applied
-- before it can remove anything, and rule 613.8 never un-applies an effect.
--
-- Timestamps are the SOURCE PERMANENT's (CR 613.7d). A source that has left the
-- battlefield has none; it sorts last and is otherwise harmless, since a missing
-- object's affected set names nothing.
--
-- Indices carry the identity, not the (ObjectId, Affected) pairs themselves: two
-- permanents can generate equal pairs, and a walk that deleted by value would drop
-- both at once.
appliedSetEffects :: [(ObjectId, Affected.Affected)] -> GameState -> [(ObjectId, Affected.Affected)]
appliedSetEffects setEffs gs =
  let indexed = zip [0 :: Int ..] setEffs
      stampOf (_, (src, _)) = fmap Object.timestamp (Game.lookupObject src gs)
      -- CR 613.8a, for these effects: does `other` strip `e`'s source?
      dependsOn (_, (src, _)) (_, (osrc, oaff)) = affectsBase osrc src oaff gs
      earliest :: [(Int, (ObjectId, Affected.Affected))] -> (Int, (ObjectId, Affected.Affected))
      earliest = List.minimumBy (Ord.comparing (\e -> (stampOf e, fst e)))
      go remaining applied = case remaining of
        [] -> reverse applied
        _ ->
          let waiting e = any (dependsOn e) (filter (\o -> fst o /= fst e) remaining)
              ready = filter (not . waiting) remaining
              -- CR 613.8b's last sentence: nothing ready means every remaining
              -- effect is in a loop, so dependency order is ignored here.
              next = earliest (if null ready then remaining else ready)
              (nsrc, _) = snd next
              stripped = any (\(src, aff) -> affectsBase src nsrc aff gs) applied
           in go (filter (\o -> fst o /= fst next) remaining) (if stripped then applied else snd next : applied)
   in go indexed []

-- Every subtype-word pair a ChangeSubtypeWord continuous effect imposes on `oid`
-- (CR 612). CR 612.2's family gate is applied where a pair meets a word, not
-- here. Stored resolution effects only, read against BASE characteristics since
-- ChangeSubtypeWord always uses a TheseObjects fixed set, so nothing recurses.
textChangesAffecting :: ObjectId -> GameState -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)]
textChangesAffecting oid gs =
  let pairOf eff = case ContinuousEffect.modification eff of
        Modification.ChangeSubtypeWord from to ->
          if affects (ContinuousEffect.source eff) oid (ContinuousEffect.affected eff) (baseCharacteristics oid gs) gs
            then Just (from, to)
            else Nothing
        _ -> Nothing
   in Maybe.mapMaybe pairOf (GameState.continuousEffects gs)

-- Apply text-changes to a modification's subtype words (CR 612.1/612.2).
--
-- CR 612.2 is the gate on each arm: a pair naming land types may rewrite only a
-- land-type position, and one naming creature types only a creature-type one. The
-- arm's family is fixed by its constructor; the PAIR's family is read off the
-- word being replaced, which is why no family tag rides on the stored
-- ChangeSubtypeWord.
--
-- The gate is not the exact-match test restated. `swap` already requires
-- `s == from`, and pawl's two families share no word, so on well-formed card data
-- the gate changes no answer. What it removes is the dependence on that: the rule
-- is stated rather than inferred from the payload, so malformed data cannot
-- smuggle a cross-family rewrite through.
--
-- Exhaustive rather than a catch-all, which is what had let GainKeyword go
-- unrewritten while carrying CR 702.14a's land-type word: a later arm that can
-- hold a word must break this build instead of falling through.
rewriteModification :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modification -> Modification
rewriteModification pairs m =
  let -- `inFamily from` is CR 612.2's gate.
      swap inFamily from to s = if s == from && inFamily from then to else s
      apply1 acc (from, to) = case acc of
        Modification.SetLandSubtype s -> Modification.SetLandSubtype (swap Subtype.isLandType from to s)
        Modification.AddLandSubtype s -> Modification.AddLandSubtype (swap Subtype.isLandType from to s)
        -- CR 612.2's other named example: the swap rewrites a Turn to Frog's Frog
        -- on the stack, so the spell resolves making its target the new type.
        Modification.SetCreatureSubtype s -> Modification.SetCreatureSubtype (swap Subtype.isCreatureType from to s)
        Modification.AddCreatureSubtype s -> Modification.AddCreatureSubtype (swap Subtype.isCreatureType from to s)
        -- Holds no word to swap: it names CR 205.3m's list, not a member of it.
        -- CR 612.2 changes which types a word MEANS, and a text change that
        -- rewrote one creature type into another would leave this add covering
        -- the same list either way.
        Modification.AddEveryCreatureSubtype -> acc
        -- CR 702.14a: "[type]walk" holds a land-type word, so a hacked Lord of
        -- Atlantis grants swampwalk rather than the printed islandwalk. The
        -- GRANTER's text is what this reads -- gatherStatic calls it with the
        -- source's own changes -- which is CR 612.3: a text change on the
        -- creature that RECEIVED the keyword may not touch it, and the layer
        -- order keeps that true (this is layer 6, the swap is layer 3).
        --
        -- Filter.rewriteKeyword rather than a swap here, since the word is inside
        -- a Filter. No family gate is restated at that descent, for the reason
        -- Filter.rewrite's own comment gives: a HasSubtype atom may name a word of
        -- any family, so the family the word is used AS is the family it belongs
        -- to, and the exact lookup already asks CR 612.2's question.
        Modification.GainKeyword k -> Modification.GainKeyword (Filter.rewriteKeyword [(from, to)] k)
        -- Carries no word: the type is read off the source at projection time,
        -- not printed on the card, so CR 612.1 has nothing here to reach.
        Modification.SetLandSubtypeToChosen -> acc
        -- A control op carries no subtype word either.
        Modification.SetController _ -> acc
        Modification.SetControllerToSource -> acc
        -- An ability wipe names nothing at all, and neither does a P/T switch.
        Modification.LoseAllAbilities -> acc
        Modification.SwitchPowerToughness -> acc
        -- Not implemented: a Quantity.Count carries a Filter, and it is left
        -- unrewritten, so a "+1/+1 for each Swamp you control" would keep
        -- counting Swamps after a swap (#711).
        Modification.SetBasePowerToughness _ _ -> acc
        Modification.ModifyPowerToughness _ _ -> acc
        -- CR 205.2a's card types are a different list from CR 205.3's subtypes,
        -- so this position holds no word a subtype pair could name. CR 205.4a's
        -- supertypes are a third list, and the two arms below hold one of those.
        Modification.AddCardType _ -> acc
        Modification.AddSupertype _ -> acc
        Modification.RemoveSupertype _ -> acc
        -- The two words of a STORED text change are the choice its own
        -- resolution announced (CR 608.2d), not words printed on the object this
        -- rewrite walks. A text changer's PRINTED clause is reached instead, by
        -- rewriteEffect's ChangeText arm.
        Modification.ChangeSubtypeWord _ _ -> acc
        -- CR 612.2 names colour words as a family a text change can swap, but
        -- pawl's only text changer swaps subtypes (Modification.ChangeSubtypeWord),
        -- so no pair reaching here holds a colour word to write.
        Modification.SetColor _ -> acc
        Modification.AddColor _ -> acc
        Modification.AddChosenColor -> acc
   in List.foldl' apply1 m pairs

-- rewriteModification's sibling for the other half of a static ability. Under CR
-- 612.1 an ability's affected clause is rules text like any other, so a hacked
-- Kormus Bell animates Islands after the swap and stops animating Swamps.
--
-- Exhaustive over Affected rather than a wildcard: the three arms carrying a
-- Filter are the three that could hide a subtype word, and a new arm carrying one
-- must break this build rather than silently keep the old word.
rewriteAffected :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Affected.Affected -> Affected.Affected
rewriteAffected pairs a = case a of
  Affected.Matching f -> Affected.Matching (Filter.rewrite pairs f)
  Affected.MatchingAnywhere f -> Affected.MatchingAnywhere (Filter.rewrite pairs f)
  Affected.AttachedPlayerControls f -> Affected.AttachedPlayerControls (Filter.rewrite pairs f)
  -- A frozen id set names no word (CR 611.2c locks it at resolution), and an
  -- attachment names none either -- both read the SOURCE's own state.
  Affected.TheseObjects _ -> a
  Affected.Attached -> a

-- CR 612's subtype word swap over an effect's AST. Delegates the inner
-- Modification of ModifyTarget to rewriteModification and every carried Filter to
-- Filter.rewrite, so no module touches another's constructors.
--
-- CR 612.1 reaches any printed word, so a Filter an effect carries is not exempt.
--
-- The invariant: this cases on an effect's STRUCTURE -- does this arm carry a
-- word a swap could reach -- never on which effect it is.
rewriteEffect :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Effect.Effect Card.Type.Card -> Effect.Effect Card.Type.Card
rewriteEffect pairs effect = case effect of
  Effect.ModifyTarget duration modification ref ->
    Effect.ModifyTarget duration (rewriteModification pairs modification) (rewriteObjectRef pairs ref)
  Effect.DealDamage ref quantity -> Effect.DealDamage (rewriteObjectRef pairs ref) quantity
  -- A text-changer's own restriction clause is text like any other (CR 612.1), so
  -- an Artificial Evolution aimed at another on the stack leaves a spell that
  -- forbids the NEW word instead. The CR 612.2 family gate is read off the
  -- ChangeText's own family rather than off a constructor.
  Effect.ChangeText family forbidden slot ->
    Effect.ChangeText family (Set.map (swapWordIn family pairs) forbidden) slot
  Effect.AddMana _ -> effect
  Effect.Search searcher owner quantity filter_ destination -> Effect.Search searcher owner quantity (Filter.rewrite pairs filter_) destination
  Effect.ExileAllGraveyards -> effect
  Effect.Proliferate -> effect
  Effect.TemptWithTheRing -> effect
  Effect.Venture -> effect
  Effect.ExileHandThenDraw -> effect
  Effect.PlayerSacrifices slot filter_ quantity -> Effect.PlayerSacrifices slot (Filter.rewrite pairs filter_) quantity
  Effect.RestartGame -> effect
  Effect.ControlPlayerNextTurn _ -> effect
  Effect.Destroy (Destroy.MkDestroy ref regenerability mSlot) -> Effect.Destroy (Destroy.MkDestroy (rewriteObjectRef pairs ref) regenerability mSlot)
  Effect.Sacrifice _ -> effect
  Effect.TurnFaceDown _ -> effect
  Effect.RemoveFromCombat _ -> effect
  -- Not implemented: a CR 122.1b keyword counter named in the riders keeps its
  -- printed keyword through the swap (#1190).
  Effect.MoveToZone ref zone riders mSlot mOrigin position -> Effect.MoveToZone (rewriteObjectRef pairs ref) zone riders mSlot mOrigin position
  Effect.Draw {} -> effect
  -- The tally's Filter is text like the Search arm's above (CR 612.1), so a
  -- swap reaches it; the slot it binds to is a name no card prints.
  Effect.Mill (Mill.MkMill ref quantity mTally) ->
    Effect.Mill (Mill.MkMill ref quantity (fmap (\t -> t {MillTally.filter = Filter.rewrite pairs (MillTally.filter t)}) mTally))
  Effect.Scry {} -> effect
  Effect.Surveil {} -> effect
  Effect.Fateseal {} -> effect
  -- The ObjectRef alone, as CreateCopy and Tap below: an EachMatching's Filter is
  -- a word a CR 612.1 swap reaches, and rule 701.44a's own text is the rulebook's
  -- rather than the card's.
  Effect.Explore ref -> Effect.Explore (rewriteObjectRef pairs ref)
  Effect.Discard {} -> effect
  Effect.LoseLife {} -> effect
  Effect.GainLife {} -> effect
  Effect.ExchangeLifeTotals _ -> effect
  Effect.IncreaseSpeed {} -> effect
  -- CR 612.2a: a token-creating spell defines the token's creature types and its
  -- name with the same words, so a text change reaches both. Those words live in
  -- the token's defining card, which this arm hands to rewriteCard.
  --
  -- Not implemented: the riders go through unchanged, so a CR 122.1b keyword
  -- counter named among them keeps its printed keyword (#1190) -- the MoveToZone
  -- arm above and this one now both read that field.
  Effect.Create quantity card riders slot -> Effect.Create quantity (rewriteCard pairs card) riders slot
  -- The ObjectRef alone, exactly as Tap above: an EachMatching's Filter is a
  -- word a CR 612.1 swap reaches. What the token BECOMES is not text on this
  -- card at all -- it is the copied permanent's copiable values, and CR 707.2
  -- excludes text-changing effects from those, so nothing here rewrites them.
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref) -> Effect.CreateCopy (CreateCopy.MkCreateCopy quantity (rewriteObjectRef pairs ref))
  Effect.Replace {} -> effect
  Effect.SkipNextPhase {} -> effect
  -- CR 612.1: a rider's text is as changeable as any other, so the recursion
  -- reaches it. The shield's own three fields hold nothing a text change
  -- touches, which is why the arm was a no-op before.
  Effect.PreventNextDamage duration ref quantity rider ->
    Effect.PreventNextDamage duration ref quantity (fmap (rewriteEffect pairs) rider)
  Effect.PreventAllDamage {} -> effect
  Effect.RedirectDamage {} -> effect
  Effect.Counter _ -> effect
  Effect.PutCounters {} -> effect
  Effect.RemoveCounters {} -> effect
  Effect.GainPlayerCounters {} -> effect
  Effect.RemovePlayerCounters {} -> effect
  Effect.Tap ref -> Effect.Tap (rewriteObjectRef pairs ref)
  Effect.Untap ref -> Effect.Untap (rewriteObjectRef pairs ref)
  Effect.Transform ref -> Effect.Transform (rewriteObjectRef pairs ref)
  Effect.AddPhases _ -> effect
  Effect.GainControl duration ref -> Effect.GainControl duration (rewriteObjectRef pairs ref)
  Effect.ArmDelayedTrigger {} -> effect
  -- Not implemented: the Filter inside the PlayerEffect keeps its printed word
  -- while the spell is on the stack (#1370).
  Effect.AffectPlayers {} -> effect
  -- CR 612.1 swaps a WORD, and both refs can carry one: an EachMatching's
  -- Filter is subtype-shaped exactly as Untap's is.
  Effect.RequireBlock duration blocker attacker ->
    Effect.RequireBlock duration (rewriteObjectRef pairs blocker) (rewriteObjectRef pairs attacker)
  -- Identity, not a rewriteCard call: CR 114.3 leaves an emblem no type line and
  -- no name, the two things rewriteCard reaches. What CR 612.1 could reach on one
  -- is its abilities, which nothing here walks (#643).
  Effect.CreateEmblem {} -> effect
  Effect.BecomeMonarch {} -> effect
  Effect.Designate _ _ -> effect
  Effect.Unsuspect ref -> Effect.Unsuspect (rewriteObjectRef pairs ref)
  Effect.Evolve _ -> effect
  Effect.Mentor _ -> effect
  Effect.ItBecomes _ -> effect
  Effect.ExileUntilMonarch _ -> effect
  Effect.Attach _ -> effect
  Effect.AttachTarget slot filter_ -> Effect.AttachTarget slot (Filter.rewrite pairs filter_)
  Effect.PlaySubgame _ -> effect
  Effect.TakeExtraTurn {} -> effect
  Effect.ShuffleIntoLibrary named ref -> Effect.ShuffleIntoLibrary named (rewriteObjectRef pairs ref)
  Effect.OfferCast {} -> effect
  -- The ObjectRef alone, exactly as GainControl above: the Duration is left
  -- untouched, so a ForAsLongAs Condition's Filters are not rewritten. That
  -- asymmetry is GainControl's and predates this opcode; matching it keeps the
  -- two arms one behaviour rather than two.
  Effect.GrantPlayFromExile duration ref -> Effect.GrantPlayFromExile duration (rewriteObjectRef pairs ref)

-- CR 612.2 over one word whose family a card's text names rather than a
-- constructor -- a ChangeText's forbidden-word set.
swapWordIn :: SubtypeFamily.SubtypeFamily -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Subtype.Type.Subtype -> Subtype.Type.Subtype
swapWordIn family pairs word = List.foldl' step word pairs
  where
    step s (from, to) = if s == from && Subtype.inFamily family from then to else s

-- CR 612.1 through an ObjectRef. InSlot names an object chosen at cast time
-- rather than a word on the card; the two sweeping arms' Filters are card text
-- like any other, and a graveyard sweep's PlayerScope names players rather than
-- subtypes. EachPlayer and TopOfLibrary carry no word at all -- CR 612.1 changes
-- subtype words, and "each player" and "the top card of your library" have none.
rewriteObjectRef :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ObjectRef.ObjectRef -> ObjectRef.ObjectRef
rewriteObjectRef pairs ref = case ref of
  ObjectRef.InSlot _ -> ref
  ObjectRef.EachMatching f -> ObjectRef.EachMatching (Filter.rewrite pairs f)
  ObjectRef.EachCardInGraveyard s f -> ObjectRef.EachCardInGraveyard s (Filter.rewrite pairs f)
  ObjectRef.EachPlayer -> ref
  ObjectRef.TopOfLibrary _ _ -> ref

-- CR 612.2a through the CARD a Create defines its token with. Two fields.
--
-- The TYPE LINE is CR 612.1's, and the exact-match test is already CR 612.2's
-- family gate for the reason applyModification's ChangeSubtypeWord arm gives.
--
-- The NAME is CR 612.2a, the licensed exception to CR 612.2's bar on changing a
-- card name. CR 111.4 is why the exception exists: an unnamed token's name is its
-- subtypes plus "Token", so the name holds those same words.
--
-- Conditional on the type line, never unconditional: CR 612.2a licenses the name
-- change only where the word is being used as a creature type, so a Create whose
-- token name merely happens to contain the word falls under CR 612.2's
-- prohibition. Hence one membership test for both fields.
--
-- Not CR 612.4, which is a swap aimed at the token itself and reaches the
-- projected object rather than this card.
--
-- Not implemented: the swap does not reach the defined card's ability carriers,
-- so the card is walked rather than recursed into (#643). That also keeps
-- rewriteEffect non-recursive.
--
-- Every FACE, because a card's printed subtypes and name are per-face (CR
-- 712.8) and the swap is aimed at the card as a whole; the gate below then
-- leaves untouched any face whose type line lacks the word.
rewriteCard :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Card.Type.Card -> Card.Type.Card
rewriteCard pairs card = card {Card.Type.faces = fmap (rewriteFace pairs) (Card.Type.faces card)}

rewriteFace :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Face.Face Card.Type.Card -> Face.Face Card.Type.Card
rewriteFace pairs face = List.foldl' apply1 face pairs
  where
    apply1 f (from, to) =
      let typeLine = Face.typeLine f
          subtypes = TypeLine.subtypes typeLine
       in if Set.notMember from subtypes
            then f
            else
              f
                { Face.typeLine = typeLine {TypeLine.subtypes = Set.insert to (Set.delete from subtypes)},
                  Face.name = rewriteTokenName from to (Face.name f)
                }

-- CR 612.2a's name half. Both words are looked up in CR 205.3m's list, since a
-- name is TEXT and writing the new one needs the word itself, not just the family
-- test the rest of this family gets by with.
--
-- The family gate is stated here rather than inherited: a pair whose words are
-- not creature types has nothing to write, and the Nothing arm leaves the name as
-- printed -- CR 612.2's prohibition holding for every other family.
--
-- Text.replace rather than a whole-name test, since CR 111.4's derived name holds
-- one word per subtype. It matches a substring, so a name holding the word inside
-- a longer one is over-reached (#644); rewriteCard's type-line gate keeps every
-- name pawl was not asked to change out of reach.
rewriteTokenName :: Subtype.Type.Subtype -> Subtype.Type.Subtype -> CardName.CardName -> CardName.CardName
rewriteTokenName from to name = case (Subtype.creatureTypeWord from, Subtype.creatureTypeWord to) of
  (Just f, Just t) -> CardName.MkCardName (Text.replace f t (CardName.unwrap name))
  _ -> name

-- CR 612.1: the same word swap over an ACTIVATED ability printed on a permanent,
-- whose text box the rule reaches. THREE parts:
--
-- The payload, and CR 702.178a's "as long as" gate -- which shares
-- rewriteCondition with CR 603.4's intervening "if" one function down, for that
-- function's reason: a rewrite reaching only the mode's clauses would leave the
-- ability gated on the printed word.
--
-- And the ACTIVATION COST, printed in that same text box (CR 118.1, CR 602.1a's
-- "everything before the colon"): Dark Heart of the Wood's "Sacrifice a Forest:"
-- becomes "Sacrifice an Island:" under a Magical Hack naming Forest.
-- Filter.rewriteCost is the descent, shared with the costs rule 702 states inside
-- a keyword. Proven by Pawl.ActivateSpec's "CR 612.1 whole card: hacking Dark
-- Heart of the Wood moves which land its cost demands".
rewriteActivatedAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ActivatedAbility.ActivatedAbility Card.Type.Card -> ActivatedAbility.ActivatedAbility Card.Type.Card
rewriteActivatedAbility pairs ability =
  ability
    { ActivatedAbility.modal = rewriteModal pairs (ActivatedAbility.modal ability),
      ActivatedAbility.condition = fmap (rewriteCondition pairs) (ActivatedAbility.condition ability),
      ActivatedAbility.cost = Filter.rewriteCost pairs (ActivatedAbility.cost ability)
    }

-- CR 612.1 over a TRIGGERED ability printed on a permanent. Three parts, not just
-- the payload: the CR 603.8 condition is where the pool's word actually is, so a
-- rewrite reaching only the mode's clauses would leave the card asking the
-- printed question. CR 603.4's intervening "if" shares rewriteCondition.
rewriteTriggeredAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TriggeredAbility Card.Type.Card -> TriggeredAbility Card.Type.Card
rewriteTriggeredAbility pairs ability =
  ability
    { TriggeredAbility.condition = rewriteTriggerCondition pairs (TriggeredAbility.condition ability),
      TriggeredAbility.intervening = fmap (rewriteCondition pairs) (TriggeredAbility.intervening ability),
      TriggeredAbility.modal = rewriteModal pairs (TriggeredAbility.modal ability)
    }

-- The modal payload both abilities carry, rewritten once so the two cannot drift.
-- Both halves of a mode: its clauses' effects, and its TARGET SPECS -- Arbor Elf's
-- "{T}: Untap target Forest" must ask for an Island once a Magical Hack has named
-- Forest, and CR 601.2c (imported by CR 602.2b) is the step whose candidate set
-- the spec defines. Proven by Pawl.ActivateSpec's "CR 612.1 whole card: hacking
-- Arbor Elf moves which land its ability may target".
--
-- Only the TargetSpec's Filter is rewritten, and the Pool is not an omission: a
-- Pool names a rules category (CR 115's "any target", the permanents on the
-- battlefield) rather than a word printed on the card, so CR 612.2's "used in the
-- correct way" finds nothing in it to swap.
rewriteModal :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modal.Modal Card.Type.Card -> Modal.Modal Card.Type.Card
rewriteModal pairs modal =
  let rewriteClause c =
        c
          { Clause.effects = fmap (rewriteEffect pairs) (Clause.effects c),
            -- CR 701.46a's clause gate is a Condition, so its Filters are
            -- printed words a text change must reach too.
            Clause.condition = fmap (rewriteCondition pairs) (Clause.condition c)
          }
      rewriteTargetSpec ts = ts {TargetSpec.filter = fmap (Filter.rewrite pairs) (TargetSpec.filter ts)}
      rewriteMode m =
        m
          { Mode.clauses = fmap rewriteClause (Mode.clauses m),
            Mode.targetSpecs = fmap rewriteTargetSpec (Mode.targetSpecs m)
          }
   in modal {Modal.modes = fmap rewriteMode (Modal.modes modal)}

-- CR 612.1 through a trigger's own condition. Exhaustive rather than a wildcard,
-- so a later condition carrying a Filter fails to compile here instead of
-- silently keeping the printed word.
rewriteTriggerCondition :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TriggerCondition.TriggerCondition -> TriggerCondition.TriggerCondition
rewriteTriggerCondition pairs condition = case condition of
  TriggerCondition.StateIs c -> TriggerCondition.StateIs (rewriteCondition pairs c)
  TriggerCondition.PermanentEnters f -> TriggerCondition.PermanentEnters (Filter.rewrite pairs f)
  TriggerCondition.PermanentDies f -> TriggerCondition.PermanentDies (Filter.rewrite pairs f)
  -- The TurnScope is carried through UNTOUCHED and not dropped: CR 612.1 changes
  -- a subtype word, and "during an opponent's turn" holds no subtype -- so a
  -- rebuild that forgot the field would silently reset a text-changed Brineborn
  -- Cutthroat to firing on every turn.
  TriggerCondition.SpellCast f scope -> TriggerCondition.SpellCast (Filter.rewrite pairs f) scope
  TriggerCondition.SelfEnters -> condition
  TriggerCondition.StepBegins _ _ -> condition
  TriggerCondition.SelfDealsCombatDamageToPlayer -> condition
  -- Its watcher-scoped sibling carries a Filter, so CR 612.1 reaches it:
  -- Tovolar's "a Wolf or Werewolf you control" is two subtype words a text
  -- change can swap.
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.rewrite pairs f)
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> condition
  TriggerCondition.OpponentLostLifeDuringYourTurn -> condition
  TriggerCondition.SelfCycled -> condition
  TriggerCondition.SelfCast -> condition
  TriggerCondition.PlayerDiscards _ -> condition
  TriggerCondition.PlayerBecomesMonarch _ -> condition
  TriggerCondition.SelfAttacks _ -> condition
  -- CR 702.149a's Filter is a predicate over the OTHER attackers, so a subtype
  -- rewrite reaches it as it reaches CreatureAttacksAlone's below.
  TriggerCondition.SelfAttacksWithAnother f -> TriggerCondition.SelfAttacksWithAnother (Filter.rewrite pairs f)
  -- CR 506.5's Filter is a predicate over the ATTACKER, so a subtype rewrite
  -- reaches it as it reaches SelfBecomesBlockedBy's below.
  TriggerCondition.CreatureAttacksAlone f -> TriggerCondition.CreatureAttacksAlone (Filter.rewrite pairs f)
  -- CR 702.105a's comparison is over LIFE TOTALS, so there is no card text in it
  -- for CR 612.1 to swap.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> condition
  TriggerCondition.SelfBlocks -> condition
  TriggerCondition.SelfBlocksCreature -> condition
  TriggerCondition.SelfBlocksAtLeast _ -> condition
  -- CR 509.3e's Filter is a predicate over the attackers blocked, so a subtype
  -- rewrite reaches it too.
  TriggerCondition.SelfBlocksOneOrMore f -> TriggerCondition.SelfBlocksOneOrMore (Filter.rewrite pairs f)
  TriggerCondition.SelfBecomesBlocked -> condition
  -- CR 509.3d's Filter is a predicate over the blocker, so a subtype rewrite
  -- reaches it exactly as it reaches SpellCast's above.
  TriggerCondition.SelfBecomesBlockedBy f -> TriggerCondition.SelfBecomesBlockedBy (Filter.rewrite pairs f)
  -- The same rule's attacking-side form, whose Filter is a predicate over the
  -- blockers.
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> TriggerCondition.SelfBecomesBlockedByOneOrMore (Filter.rewrite pairs f)
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> condition
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> condition
  TriggerCondition.SelfDies -> condition
  TriggerCondition.SelfLeavesTheBattlefield -> condition
  TriggerCondition.SpellOrAbilityCounters _ -> condition
  TriggerCondition.DamageToPlayerPrevented _ -> condition
  TriggerCondition.PlayerGainsLife _ -> condition
  TriggerCondition.PlayerLosesLife _ -> condition
  -- CR 714.2b names a counter KIND and a number, neither of which is a subtype
  -- CR 612.1 can change: a text-changing effect swapping Merfolk for Knight
  -- leaves a chapter symbol reading the same chapter.
  TriggerCondition.SelfCountersReached _ _ -> condition
  TriggerCondition.SelfLastCounterRemoved _ -> condition
  -- CR 709.5h names a HALF by its own name (CR 709.4a), and a name is not a
  -- subtype: CR 612.1 changes subtype words, so nothing here can move a door.
  TriggerCondition.SelfHalfUnlocked _ -> condition
  -- CR 709.5i names a PlayerRelation and nothing else; CR 612.1 changes subtype
  -- words and cannot reach one.
  TriggerCondition.RoomFullyUnlocked _ -> condition
  -- Recursive, since CR 612.1 reaches whatever the branches hold: an AnyOf over a
  -- PermanentEnters has a Filter inside it that a text-changing effect can rewrite.
  TriggerCondition.AnyOf conditions -> TriggerCondition.AnyOf (fmap (rewriteTriggerCondition pairs) conditions)
  -- CR 708.7 names no subtype at all -- the condition is nullary -- so a CR 612.1
  -- text change has nothing in it to rewrite.
  TriggerCondition.SelfTurnedFaceUp -> condition
  -- Its watcher-scoped sibling DOES carry a Filter, so a text change reaches this
  -- one: "whenever a Dragon is turned face up" becomes "whenever a Knight is"
  -- under CR 612.1, exactly as PermanentEnters' does.
  TriggerCondition.PermanentTurnedFaceUp f -> TriggerCondition.PermanentTurnedFaceUp (Filter.rewrite pairs f)
  -- The same, one condition over: Valeron Wardens' "a creature you control" is a
  -- Filter, so CR 612.1 reaches it too.
  TriggerCondition.PermanentBecomesDesignated d f -> TriggerCondition.PermanentBecomesDesignated d (Filter.rewrite pairs f)
  TriggerCondition.SelfEvolves -> condition
  -- CR 702.134c's is nullary as well: the mentor is found through the source's
  -- attachment and the mentored creature through the event, so no subtype word of
  -- the card's is in it for CR 612.1 to swap.
  TriggerCondition.AttachedCreatureMentors -> condition
  -- CR 701.21a's condition is nullary too: "a player" and "a permanent" name no
  -- subtype word for CR 612.1 to swap.
  TriggerCondition.PermanentSacrificed -> condition
  -- CR 603.3b's second class carries only a PlayerRelation. The word "Saga" is in
  -- the rule (CR 714.1) rather than in the card's data here, so there is no
  -- subtype word for CR 612.1 to swap -- and a text-changing effect that made a
  -- permanent stop being a Saga would be read off the projection this rewrites,
  -- not out of the condition.
  TriggerCondition.SagaFinalChapterTriggers _ -> condition
  -- CR 603.7's slot-named condition carries a SLOT NAME, which is card data but not
  -- card TEXT: it names an entry in the ability's own environment, so no CR 612.1
  -- subtype swap can reach it. What the slot holds is a creature whose subtypes a
  -- text change may well have rewritten, and that is read off the projection rather
  -- than out of the condition.
  TriggerCondition.LoseControlOfBound _ -> condition
  TriggerCondition.RoomEntered _ -> condition

-- CR 612.1 through Condition's predicate vocabulary, at the four clauses a
-- PRINTED ability carries one in: a triggered ability's CR 603.8 state trigger
-- and its CR 603.4 intervening "if", a static ability's CR 604.2 "as long as"
-- clause (gatherStatic), and CR 508.1c / CR 509.1b's "unless some condition is
-- met" gate on a combat restriction (Pawl.Engine.CombatRestriction.restricted).
-- A CR 611.2b duration is stored rather than printed, so no text change reaches
-- it here. Both sides are rewritten, both being full Quantities.
rewriteCondition :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Condition.Type.Condition -> Condition.Type.Condition
rewriteCondition pairs condition = case condition of
  Condition.Type.Compares c ->
    Condition.Type.Compares
      c
        { Compares.measured = rewriteQuantity pairs (Compares.measured c),
          Compares.threshold = rewriteQuantity pairs (Compares.threshold c)
        }
  Condition.Type.Any conditions -> Condition.Type.Any (fmap (rewriteCondition pairs) conditions)

-- CR 612.1 through a Quantity. A Count's Filter is where the subtype word hides,
-- and its Aggregation may name a further Quantity; the descent is structural and
-- terminates for the same reason evaluation does. Every remaining arm is a leaf.
rewriteQuantity :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Quantity.Type.Quantity -> Quantity.Type.Quantity
rewriteQuantity pairs quantity = case quantity of
  Quantity.Type.Count c ->
    Quantity.Type.Count
      c
        { Count.Type.filter = Filter.rewrite pairs (Count.Type.filter c),
          Count.Type.aggregation = rewriteAggregation pairs (Count.Type.aggregation c)
        }
  Quantity.Type.Plus x y -> Quantity.Type.Plus (rewriteQuantity pairs x) (rewriteQuantity pairs y)
  Quantity.Type.Literal _ -> quantity
  Quantity.Type.ManaValue -> quantity
  Quantity.Type.Power -> quantity
  Quantity.Type.Toughness -> quantity
  Quantity.Type.InSlot _ -> quantity
  Quantity.Type.Star -> quantity
  Quantity.Type.ManaCount _ -> quantity
  Quantity.Type.LifeTotal _ -> quantity
  Quantity.Type.Speed _ -> quantity
  Quantity.Type.IsMonarch _ -> quantity
  Quantity.Type.HasDesignation _ -> quantity
  Quantity.Type.WasKicked -> quantity
  Quantity.Type.PlayerCounters _ _ -> quantity
  -- A leaf too: CR 122.1's counter kinds are their own closed enumeration and
  -- name no subtype word, not even the CR 122.1b keyword one.
  Quantity.Type.ObjectCounters _ -> quantity
  -- And a leaf: CR 508.3b's record holds attack targets, which name players and
  -- object ids rather than any word CR 612.1 could swap.
  Quantity.Type.OpponentsAttacked _ -> quantity
  -- And a leaf: CR 701.9a's tally holds logged events, which name players and
  -- object ids rather than any word CR 612.1 could swap.
  Quantity.Type.CardsDiscardedThisTurn _ -> quantity
  -- And a leaf: CR 509.1h's declaration names object ids, not a word CR 612.1
  -- could swap.
  Quantity.Type.BlockersBeyondFirst -> quantity
  -- Not a leaf: the slot names no word CR 612.1 could swap, but the payload is a
  -- whole Quantity and may hide a Count whose Filter does.
  Quantity.Type.AgainstSlot slot inner -> Quantity.Type.AgainstSlot slot (rewriteQuantity pairs inner)

-- rewriteQuantity's other half: Greatest is the only Aggregation carrying a
-- Quantity, and the set it aggregates over is the Count's own Filter.
rewriteAggregation :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Aggregation.Aggregation Quantity.Type.Quantity -> Aggregation.Aggregation Quantity.Type.Quantity
rewriteAggregation pairs aggregation = case aggregation of
  Aggregation.Greatest q -> Aggregation.Greatest (rewriteQuantity pairs q)
  Aggregation.Members -> aggregation
  Aggregation.DistinctCardTypes -> aggregation

-- Every continuous effect in the game: stored resolution effects, plus every
-- battlefield permanent's static abilities (CR 613.7a, with the permanent's own
-- timestamp), dropping a permanent whose static abilities are not live (CR
-- 305.7). Not filtered by object here -- project filters per layer against the
-- partial.
--
-- CR 613.6: the affected set belongs to the EFFECT rather than each of its parts,
-- so the parts of one static ability all carry that ability's key, which lets
-- projectWith decide their set once. A stored effect or counter is a single part
-- and carries none.
--
-- Two ability losses are asked about, and only about a permanent's OWN static
-- abilities: CR 305.7's land-subtype strip (liveGiven) drops the permanent
-- outright, and CR 613.1f's layer-6 removal (abilitiesRemoved) drops only an
-- ability whose every part lands after layer 6. Neither touches a stored effect
-- or a counter, since neither is an ability for layer 6 to remove (CR 611.2a; CR
-- 122.1a/613.4c) -- Humility removes neither.
--
-- A THIRD loss, and the reason this needs a second pass of its own: CR 604.2's
-- "as long as" gate (StaticAbility.condition) drops an ability whose clause is
-- currently false. Evaluating a Condition needs a projection, and a projection
-- needs this list, so the gate is answered against the SEED list below rather
-- than against itself -- the same shape abilitiesRemoved already has, and
-- well-founded for the same reason: the seed is built with every gate open, so
-- nothing here re-enters gather.
gather :: GameState -> [Gathered]
gather gs =
  let ungated = gatherGiven (const False) alwaysFunctioning gs
   in -- Almost every board has neither an ability-removing effect nor a
      -- conditional static ability, and then the gathered list IS the ungated one
      -- -- no second walk and no projection spent on either question. A board that
      -- has one pays for the stored effects, emblems and counters twice, none of
      -- which costs a projection.
      if any (removesAbilities . gModification) ungated || anyConditional gs
        then gatherGiven (abilitiesRemoved ungated gs) (conditionHolds ungated gs) gs
        else ungated

-- The open CR 604.2 gate: every "as long as" clause answered true without being
-- looked at. What the seed pass gets, so that the list the real gate reads is the
-- widest one -- an ability wrongly kept there can only over-project the state a
-- condition is judged against, never leave gather to re-enter itself.
--
-- Not implemented: the CR 613.1f removal question the seed answers is therefore
-- asked of a conditional ability whose clause is false, as is abilityRemoval's,
-- and setLandSubtypeEffects and controlGrants read the printed list without the
-- gate at all (#727).
alwaysFunctioning :: ObjectId -> Layer -> Condition.Type.Condition -> Bool
alwaysFunctioning _ _ _ = True

-- Does any static ability in play carry a CR 604.2 "as long as" clause at all?
-- gather's cheap structural precondition, asked instead of the second walk -- a
-- pure read of the printed faces, with no projection behind it, mirroring the
-- ability-removal test it sits beside.
--
-- Emblems are included for the reason gatherGiven gathers them (CR 114.4 / 113.6);
-- no emblem in the pool carries a condition, and a walk that skipped them would
-- silently ungate the first one that did.
--
-- So is the stack, and there the first one HAS arrived: Molten Disaster's "if
-- this spell was kicked, it has split second" is a CR 604.2 clause on an ability
-- gatherGiven gathers from the stack. Skipping the stack here would leave that
-- clause wired open by alwaysFunctioning, so an UNKICKED Molten Disaster would
-- carry split second.
anyConditional :: GameState -> Bool
anyConditional gs =
  let conditional oid = case Game.faceOf oid gs of
        Nothing -> False
        Just face -> any (Maybe.isJust . StaticAbility.condition) (Face.staticAbilities face)
   in any conditional (Set.toList (GameState.battlefield gs))
        || any conditional (Set.toList (GameState.command gs))
        || any conditional (GameState.stack gs)

-- CR 604.2: is this static ability's "as long as" clause true right now?
--
-- The VIEW is bounded at the ability's own lowest layer -- the point CR 613.6
-- makes the effect start to apply, and the same point abilitiesRemoved judges a
-- remover's affected set at. So Kird Ape's layer-7c clause reads a Forest through
-- layers 1-6, and a Convincing Mirage'd Forest has stopped being one by then.
-- Bounded rather than full for projectWith's reason: a condition read against a
-- projection that included its OWN layer would be circular, and the descending
-- bound is what makes the nesting terminate. Exact for a clause reading a
-- strictly earlier layer, which every "as long as" clause in the pool does.
--
-- CR 109.5: "you" is the SOURCE's controller, as it is for the affected set. The
-- condition is evaluated AGAINST the source too, so a clause reading Quantity.Power
-- reads the permanent the ability is printed on.
conditionHolds :: [Gathered] -> GameState -> ObjectId -> Layer -> Condition.Type.Condition -> Bool
conditionHolds cands gs src lowest =
  Condition.holds (viewUpTo lowest cands gs) (Filter.contextFor (controllerOf src gs) (Just src)) gs src

-- gather's body with both ability gates left open: `stripped` answers whether a
-- permanent's abilities were removed by the time layer 6 finished, and
-- `functioning` whether a static ability's CR 604.2 "as long as" clause holds.
-- Called twice by gather -- once wired shut to build the list the gates read, once
-- with the real answers.
gatherGiven :: (ObjectId -> Bool) -> (ObjectId -> Layer -> Condition.Type.Condition -> Bool) -> GameState -> [Gathered]
gatherGiven stripped functioning gs =
  let setEffs = setLandSubtypeEffects gs
      -- A stored effect carries exactly one modification, so CR 613.6 has nothing
      -- to hold together -- and every stored effect's set is CR 611.2c's
      -- TheseObjects, locked when the effect began: at resolution for the ones a
      -- spell or ability made, and at the departure for the ones
      -- frozenStaticParts froze.
      fromStored eff =
        MkGathered
          { gEffect = Nothing,
            gSource = ContinuousEffect.source eff,
            gAffected = ContinuousEffect.affected eff,
            gLayer = layer (ContinuousEffect.modification eff),
            gLowest = layer (ContinuousEffect.modification eff),
            gTimestamp = ContinuousEffect.timestamp eff,
            gModification = ContinuousEffect.modification eff
          }
      stored = fmap fromStored (GameState.continuousEffects gs)
      static = concatMap (fmap snd . permanentParts stripped functioning setEffs gs) (Set.toList (GameState.battlefield gs))
      fromEmblem emblemId = case Game.lookupObject emblemId gs of
        Nothing -> []
        Just emblemObj -> case Game.faceOf emblemId gs of
          Nothing -> []
          Just face ->
            -- CR 114.4 / 113.6: an emblem's abilities function in the command
            -- zone, its effect sharing the emblem's entry timestamp (CR 613.7a).
            -- No liveness or text-change pass and never stripped: the pool's CR
            -- 613.1f removers reach creatures, and an emblem is not one (CR
            -- 114.5).
            concat (zipWith (gatherStatic (functioning emblemId) emblemId (Object.timestamp emblemObj) [] False) [0 ..] (Face.staticAbilities face))
      emblems = concatMap fromEmblem (Set.toList (GameState.command gs))
      fromSpell spellId = case Game.lookupObject spellId gs of
        Nothing -> []
        Just spellObj -> case Game.faceOf spellId gs of
          Nothing -> []
          Just face ->
            -- CR 604.2's second limb -- "or as long as the object with the
            -- ability remains in the appropriate zone, as described in rule
            -- 113.6" -- and CR 113.6's first sentence names that zone:
            -- "Abilities of an instant or sorcery spell usually function only
            -- while that object is on the stack."
            --
            -- Read off the CARD TYPES, which is a classification and not an
            -- identity -- the same act as the battlefield walk above reading
            -- Face.staticAbilities. A permanent spell's static abilities are
            -- NOT gathered here: rule 113.6's second sentence keeps them on the
            -- battlefield, and the exceptions that do reach the stack (CR
            -- 113.6d/e/g) are asked elsewhere, off the printed face.
            --
            -- CR 613.7a again: the effect shares the stack object's timestamp.
            -- No liveness or text-change pass and never stripped, for the
            -- emblem branch's reason -- the pool's CR 613.1f removers reach
            -- creatures on the battlefield, and a spell on the stack is not
            -- one.
            if Set.null (Set.intersection spellStaticTypes (TypeLine.types (Face.typeLine face)))
              then []
              else concat (zipWith (gatherStatic (functioning spellId) spellId (Object.timestamp spellObj) [] False) [0 ..] (Face.staticAbilities face))
      spells = concatMap fromSpell (GameState.stack gs)
      counters = counterGathered gs
      designations = designationGathered gs
   in stored <> static <> emblems <> spells <> counters <> designations

-- CR 113.6's first sentence: the card types whose object has its abilities
-- function on the stack rather than on the battlefield. Its own binding rather
-- than a literal in gatherGiven, so the rule reads as a rule.
--
-- anyConditional deliberately does NOT narrow by this: it is a structural
-- precondition, and a superset of the abilities gatherGiven will actually
-- gather can only cost a second walk, where a subset would leave a CR 604.2
-- clause wired open.
spellStaticTypes :: Set CardType.CardType
spellStaticTypes = Set.fromList [CardType.Instant, CardType.Sorcery]

-- ONE battlefield permanent's static-ability parts, each tagged with the index
-- of the ability it came from -- gatherStatic's `n`, and so the key half CR
-- 613.6's decision memo is filed under.
--
-- Hoisted out of gatherGiven's battlefield walk so that frozenStaticParts below
-- gathers a permanent's parts by exactly the walk the fold uses, gates and CR
-- 612 rewrite included. A second copy of this body is the one thing the
-- departure handover must not have: it would freeze a set the fold never
-- applied.
--
-- The index is dropped by gatherGiven, which has no use for it -- gatherStatic
-- has already spent it on `gEffect`.
permanentParts :: (ObjectId -> Bool) -> (ObjectId -> Layer -> Condition.Type.Condition -> Bool) -> [(ObjectId, Affected.Affected)] -> GameState -> ObjectId -> [(Natural, Gathered)]
permanentParts stripped functioning setEffs gs permId = case Game.lookupObject permId gs of
  Nothing -> []
  Just permObj -> case Game.faceOf permId gs of
    Nothing -> []
    Just face ->
      if null setEffs || liveGiven setEffs permId gs
        then
          -- CR 612: rewrite each static ability's subtype words by the text
          -- changes affecting THIS source, before its effect is folded onto
          -- any other object.
          let changes = textChangesAffecting permId gs
              -- One thunk per permanent, shared by all its abilities and
              -- forced by none unless an ability is entirely above layer 6,
              -- so the projection costs at most one per permanent. Bound
              -- here, OUTSIDE the zipWith, which is what shares it.
              partsOf = gatherStatic (functioning permId) permId (Object.timestamp permObj) changes (stripped permId)
              tagged n sa = fmap ((,) n) (partsOf n sa)
           in concat (zipWith tagged [0 ..] (Face.staticAbilities face))
        else []

-- CR 611.2c, applied to a static ability's effect: the parts `src`'s own static
-- abilities are generating RIGHT NOW, each with the index of the ability it
-- belongs to, the timestamp CR 613.7a gave it, and its affected set resolved
-- from a live predicate to the concrete objects it names at this instant.
--
-- The one reader is Pawl.Engine.Event's departure handover, for a card whose
-- text says its effect outlives its own permanent (StaticAbility.lingers). Such
-- an effect stops being a static ability's and becomes a STORED one, and CR
-- 611.2c fixes a stored effect's set when it begins -- which is here, on the
-- board the permanent is still on. Never called for an ordinary ability, whose
-- set goes on being re-derived every projection (CR 613.5).
--
-- The set is asked ONCE PER ABILITY and copied onto each of its parts, which is
-- CR 613.6: the affected set belongs to the effect, not to its parts. Every
-- part of one ability therefore freezes to the same objects, which is what lets
-- the caller store them as separate single-modification effects without the
-- split being observable.
--
-- Each part's answer is taken at CR 613.6's decision point -- gLowest -- and by
-- the same two routes abilitiesRemoved takes: projectWith's own memo for a
-- multi-part effect, so the two cannot drift, and a bounded projection for a
-- single-part one.
frozenStaticParts :: ObjectId -> GameState -> [(Natural, Timestamp, Modification, Set ObjectId)]
frozenStaticParts src gs =
  let cands = gather gs
      -- gather's own seed list, and the same one it feeds its two gates.
      ungated = gatherGiven (const False) alwaysFunctioning gs
      parts = permanentParts (abilitiesRemoved ungated gs) (conditionHolds ungated gs) (setLandSubtypeEffects gs) gs src
      applies c oid =
        let lyr = gLowest c
            partial = projectUpTo lyr cands oid gs
            decided = decisionsUpTo lyr cands oid gs
         in case gEffect c >>= (`Map.lookup` decided) of
              Just answer -> answer
              Nothing -> affects (gSource c) oid (gAffected c) partial gs
      -- One entry per ability rather than per part, so the set is computed once
      -- and shared -- the parts of an ability agree on both inputs, gAffected
      -- and gLowest, so whichever part Map.fromList keeps asks the same
      -- question. Lazy, so only the retained thunk is ever forced.
      byAbility = Map.fromList [(n, Set.filter (applies c) (candidatesFor (gAffected c) gs)) | (n, c) <- parts]
   in -- `parts` order, which is the card's PRINTED order (gatherStatic's), and
      -- load-bearing once these become separate stored effects: the fold applies
      -- same-layer parts sharing one timestamp in list order, so reordering them
      -- here would reorder the ability against itself.
      [(n, gTimestamp c, gModification c, Map.findWithDefault Set.empty n byAbility) | (n, c) <- parts]

-- The objects an affected set could possibly name, so freezing one need not
-- project every object in the game. Battlefield-scoped for every arm the
-- battlefield bounds -- `affects` gates Matching and AttachedPlayerControls on
-- membership itself, and CR 303.4's attachment only ever names a permanent --
-- and the whole pool for MatchingAnywhere, which is the arm that exists because
-- its set is not scoped to the battlefield.
candidatesFor :: Affected.Affected -> GameState -> Set ObjectId
candidatesFor a gs = case a of
  Affected.Matching _ -> GameState.battlefield gs
  Affected.Attached -> GameState.battlefield gs
  Affected.AttachedPlayerControls _ -> GameState.battlefield gs
  Affected.MatchingAnywhere _ -> Map.keysSet (GameState.objects gs)
  -- Already fixed (CR 611.2c), so `affects` answers True for exactly these and
  -- freezing it is the identity. Returned rather than special-cased away, so
  -- the caller has one shape to filter.
  Affected.TheseObjects s -> s

-- CR 613.1f, hoisted over the whole game: "were THIS object's abilities removed
-- by the time layer 6 finished?", as one predicate. For a caller OUTSIDE the
-- layer machine that must ask it once per battlefield permanent
-- (Pawl.Engine.PlayerEffect.applying, for CR 604.2's "and has the ability"), so that the
-- candidate list is built once per read rather than once per permanent -- the
-- same posture setLandSubtypeEffects has for CR 305.7's liveGiven.
--
-- The list is gathered with the layer-6 gate OFF, which is what the gate itself
-- reads, and why this needs an extra pass rather than a fixpoint: deciding
-- whether a source's abilities were removed means projecting it up to CR 613.6's
-- decision point, which is never above layer 6, and such a projection cannot see
-- the layer-7 parts the gate drops.
--
-- Well-founded because nothing reachable from here reads a player effect back:
-- the layer machine's only inputs are static abilities, stored continuous effects
-- and counters, and a CR 613.10/613.11 effect is a sibling tier applied after it.
-- The module graph enforces this -- Projection does not import PlayerEffect.
abilityRemoval :: GameState -> ObjectId -> Bool
abilityRemoval gs =
  let ungated = gatherGiven (const False) alwaysFunctioning gs
   in -- Almost every board has no ability-removing effect, and then no projection
      -- is spent on the question.
      if any (removesAbilities . gModification) ungated
        then abilitiesRemoved ungated gs
        else const False

-- CR 613.1f: does this modification remove abilities? Total: a new
-- ability-removing Modification must break this build rather than silently answer
-- False and reopen #297.
removesAbilities :: Modification -> Bool
removesAbilities m = case m of
  Modification.LoseAllAbilities -> True
  Modification.GainKeyword _ -> False
  -- CR 305.7 strips a land's rules text, which IS an ability loss -- but as a
  -- layer-4 type change performed by setLandSubtypeTo and liveGiven, never a
  -- layer-6 removal. True here would double-count it into a layer whose ordering
  -- it does not have.
  Modification.SetLandSubtype _ -> False
  Modification.SetLandSubtypeToChosen -> False
  -- CR 205.1a/205.1b's creature-type set has no ability clause at all; CR 305.7's
  -- strip belongs to the land arm above.
  Modification.SetCreatureSubtype _ -> False
  Modification.AddCreatureSubtype _ -> False
  Modification.AddEveryCreatureSubtype -> False
  Modification.SetBasePowerToughness _ _ -> False
  Modification.ModifyPowerToughness _ _ -> False
  Modification.SwitchPowerToughness -> False
  Modification.AddLandSubtype _ -> False
  Modification.ChangeSubtypeWord _ _ -> False
  Modification.AddCardType _ -> False
  -- CR 205.4b changes a supertype and says nothing about abilities. CR 305.7's
  -- strip is the land arms' alone, and a permanent that becomes legendary or
  -- stops being snow keeps every ability it had.
  Modification.AddSupertype _ -> False
  Modification.RemoveSupertype _ -> False
  Modification.SetColor _ -> False
  Modification.AddColor _ -> False
  Modification.AddChosenColor -> False
  Modification.SetController _ -> False
  Modification.SetControllerToSource -> False

-- CR 613.1f / 613.1g: were `oid`'s abilities removed by the time layer 6 finished?
-- Layer 6 is applied before layer 7, so an ability removed there generates no
-- layer-7 effect, and CR 613.6's rescue cannot reach one whose only parts are in
-- layer 7 -- it never started to apply in an earlier layer. gatherStatic draws
-- that distinction; this only answers the removal question.
--
-- The removers are read off the SAME candidate list, which is where CR 613.6's
-- rescue lands: an ability-removing effect is itself a layer-6 part, so an ability
-- carrying one is never gated by this and a Humility'd Humility keeps applying its
-- own layer-7b 1/1.
--
-- Each remover's affected set is judged at CR 613.6's decision point -- gLowest,
-- not layer 6 -- and this gate must reach the same answer the fold did, or the
-- fold strips an object this gate did not. For a MULTI-PART remover it is not
-- re-derived at all: decisionsUpTo hands back projectWith's own `decided` memo,
-- so the two cannot drift.
--
-- Re-deriving it is what went wrong before. projectUpTo excludes the decision
-- layer entirely, while the fold decides against the state that layer's earlier
-- effects have already produced (CR 613.7 timestamp order, CR 613.8 dependency
-- order). Humility cannot tell those readings apart -- layer 6 is its lowest, and
-- its set is Matching (HasCardType Creature), which reads card types that layer 6
-- does not write. Titania's Song beside a Liquimetal Coating can: the Coating's
-- layer-4 AddCardType puts a permanent inside "each noncreature artifact" for the
-- fold, which animated it, and outside it for a pre-layer-4 reading, which then
-- let it keep its abilities.
--
-- Widening projectUpTo to `<= lyr` instead would be exactly wrong, and CR 613.6's
-- third Example says why: "All noncreature artifacts become 2/2 artifact
-- creatures until end of turn ... the power- and toughness-setting effect is
-- applied to those same permanents in layer 7b, even though those permanents
-- aren't noncreature artifacts by then." Once the whole of layer 4 has run, the
-- Song's own AddCardType has applied and its set answers False about every
-- permanent it just animated. The memo is the only reading that is neither too
-- early nor too late, because it was taken at the instant the effect applied.
--
-- SINGLE-PART removers (gEffect = Nothing) are not memoized by the fold, so they
-- keep the projectUpTo reading. Grouped by decision layer so one projection and
-- one memo serve every remover deciding there, and lazily enough that `any`
-- short-circuits before building either for a layer it never reaches.
--
-- Not implemented: a single-part remover whose affected set another effect in its
-- own decision layer moves (#1008).
--
-- Not asked of the remover's own source: whether a stripper was itself stripped is
-- a question about order WITHIN layer 6, which the fold settles by CR 613.7
-- timestamp. CR 305.7's gate asks a related question one level up and settles it
-- by CR 613.8 instead -- see appliedSetEffects -- because there the strip decides
-- whether the effect EXISTS rather than merely when it lands.
abilitiesRemoved :: [Gathered] -> GameState -> ObjectId -> Bool
abilitiesRemoved cands gs oid =
  let byLowest = Map.fromListWith (<>) [(gLowest c, [c]) | c <- cands, removesAbilities (gModification c)]
      removesAt (lyr, cs) =
        let partial = projectUpTo lyr cands oid gs
            decided = decisionsUpTo lyr cands oid gs
            removes c = case gEffect c >>= (`Map.lookup` decided) of
              Just answer -> answer
              Nothing -> affects (gSource c) oid (gAffected c) partial gs
         in any removes cs
   in any removesAt (Map.toList byLowest)

-- The two keywords rule 702 states as a characteristic-defining ability, expanded
-- for the case where CR 604.3a denies them that status. CR 604.3a's clause (2)
-- limits CDA status to an ability printed on the card it affects, granted to a
-- token by the effect that created the token, or acquired by a copy or
-- text-changing effect, and clauses (3) and (4) exclude an ability that directly
-- affects other objects and one an object grants to itself. So an instance
-- another object's static ability GRANTS is an ordinary continuous effect,
-- applied in its own layer in timestamp order rather than at the start of it (CR
-- 613.3).
--
--   * CR 702.114a, devoid: "this object is colorless" is exactly SetColor with no
--     colours (CR 105.3), layer 5 (CR 613.1e). applyColorDefining is the printed
--     instance's route.
--   * CR 702.73a, changeling: "this object is every creature type" is exactly
--     AddEveryCreatureSubtype, CR 205.1b's add, layer 4 (CR 613.1d).
--     applySubtypeDefining is the printed instance's route.
--
-- Emitted as a second PART of the granting ability rather than as an effect of its
-- own, which is what CR 613.6 and CR 613.7a ask for: one ability, so one affected
-- set decided once, and one timestamp -- the granting permanent's. An effect of its
-- own would have neither, since there is no second ability to hang them on.
--
-- Neither minted part is the last word on the characteristic. Each applies in
-- timestamp order like any other effect in its layer, so a NEWER colour-changing
-- or type-changing effect lands on top of it and the object ends up with the
-- keyword and a colour, or with the keyword and one creature type. That is CR
-- 613.7, not a leak.
--
-- Casing on a KEYWORD, which rule 702 makes part of the rulebook -- the same act
-- as reading Keyword.Type.StartYourEngines off a projection. It is not a case on an
-- effect's identity: the caller still routes by `layer`, and the modifications this
-- produces are the ones Moonlace and Wings of Velis Vel already store.
--
-- Only a static ability's grant is expanded. A devoid granted by the resolution of
-- a spell or ability is not (#793), nor a changeling so granted (#1288).
-- counterGathered's grants need no arm at all: CR 122.1b enumerates the keywords a
-- keyword counter can be and neither is among them, so no board can put one there.
grantedDefiningParts :: Modification -> NonEmpty.NonEmpty Modification
grantedDefiningParts m = case m of
  Modification.GainKeyword Keyword.Type.Devoid -> m NonEmpty.:| [Modification.SetColor Set.empty]
  Modification.GainKeyword Keyword.Type.Changeling -> m NonEmpty.:| [Modification.AddEveryCreatureSubtype]
  _ -> m NonEmpty.:| []

-- One static ability's parts, ready to fold: CR 613.6's unit. `n` is the ability's
-- index on its source, and (src, n) is the key every part of a MULTI-part ability
-- carries, so projectWith can tell that a layer-4 part and a layer-7b part are one
-- effect sharing one affected set. A one-part ability carries no key.
--
-- CR 612: the text changes affecting the SOURCE rewrite each part's subtype words
-- before the part is folded onto any other object.
--
-- `stripped` is CR 613.1f's answer for the source. It costs an ability all of its
-- parts, and only when every one applies AFTER layer 6. Both clauses are
-- load-bearing:
--
--   * ALL parts after 6, so nothing CR 613.6 protects is retracted -- an ability
--     with a part in layers 1-5 had already started to apply.
--   * AFTER 6, not at-or-after: an ability whose own part is IN layer 6 starts to
--     apply in the very layer that removes it, which is the same rescue.
--
-- The whole ability is dropped rather than only its layer-7 parts, which is the
-- same statement -- the branch is taken only when every part is a layer-7 one.
--
-- `functioning` is CR 604.2's "as long as" gate, answered by conditionHolds at the
-- ability's lowest layer. It costs the ability ALL of its parts unconditionally,
-- where `stripped` costs them only under the two clauses above: CR 613.6's rescue
-- is about an ability being REMOVED mid-fold, and a clause that is false never let
-- the effect start to apply at all.
gatherStatic :: (Layer -> Condition.Type.Condition -> Bool) -> ObjectId -> Timestamp -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Bool -> Natural -> StaticAbility.StaticAbility -> [Gathered]
gatherStatic functioning src ts changes stripped n sa =
  let -- CR 612 rewrites each printed modification, and grantedDefiningParts then
      -- expands what the rewrite produced -- in that order, since the expansion
      -- emits an engine-minted part that is not card text for CR 612 to reach.
      ms = StaticAbility.modifications sa >>= grantedDefiningParts . rewriteModification changes
      key = case ms of
        _ NonEmpty.:| (_ : _) -> Just (src, n)
        _ -> Nothing
      -- CR 613.6's decision point, computed once for the whole ability and
      -- copied onto each of its parts. Total: an ability has at least one
      -- modification, so this minimum is over a NonEmpty.
      lowest = minimum (fmap layer ms)
      -- CR 612.1: rewritten because the affected clause is rules text too (#402).
      -- Hoisted out of `one` and short-circuited, since this runs inside gather,
      -- which the SBA sweep reruns at every priority boundary: inside `one` it
      -- would rebuild the filter per part, and Filter.rewrite walks the whole tree
      -- even for an empty pair list.
      affected =
        if null changes
          then StaticAbility.affected sa
          else rewriteAffected changes (StaticAbility.affected sa)
      one m' =
        MkGathered
          { gEffect = key,
            gSource = src,
            gAffected = affected,
            gLayer = layer m',
            gLowest = lowest,
            gTimestamp = ts,
            gModification = m'
          }
      parts = fmap one (NonEmpty.toList ms)
      -- Free for an unconditional ability, which is all but Kird Ape's: the Maybe
      -- answers before `functioning` -- and so before conditionHolds' projection --
      -- is ever forced.
      --
      -- CR 612.1: the CR 604.2 "as long as" clause is printed in the text box
      -- just as the affected clause beside it is, so the same word swap reaches
      -- it (#765). Short-circuited on `null changes` for the same reason
      -- `affected` is -- Filter.rewrite walks the whole tree either way -- but
      -- kept inside the maybe, since an unconditional ability has no clause to
      -- rewrite at all.
      lives = maybe True (\c -> functioning lowest (if null changes then c else rewriteCondition changes c)) (StaticAbility.condition sa)
   in -- The cheap structural test first, so `stripped`'s projection is forced only
      -- for an ability the rest of the rule could reach.
      if (lowest > Layer.Ability && stripped) || not lives then [] else parts

-- CR 122.1a / 613.4c: +1/+1 and -1/-1 counters modify P/T in layer 7c. Each
-- object's counters are emitted as ONE synthetic 7c ModifyPowerToughness carrying
-- the net delta, folded by the same path as Giant Growth. Layer 7c is purely
-- additive, so pre-combining the counters is unobservable; a zero delta emits
-- nothing.
--
-- CR 122.1b / 613.1f: a keyword counter grants its keyword instead, in layer 6.
-- One grant per counter rather than per kind, since the layer-6 arm counts
-- instances.
counterGathered :: GameState -> [Gathered]
counterGathered gs = concatMap fromObject (Set.toList (GameState.battlefield gs))
  where
    fromObject oid = case Game.lookupObject oid gs of
      Nothing -> []
      Just obj ->
        let cs = Object.counters obj
            at lyr m =
              MkGathered
                { gEffect = Nothing,
                  gSource = oid,
                  gAffected = Affected.TheseObjects (Set.singleton oid),
                  gLayer = lyr,
                  gLowest = lyr,
                  gTimestamp = Object.timestamp obj,
                  gModification = m
                }
            plus = toInteger (Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs)
            minus = toInteger (Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs)
            d = plus - minus
            pt =
              [ at Layer.ModifyPT (Modification.ModifyPowerToughness (Quantity.Type.Literal d) (Quantity.Type.Literal d))
              | d /= 0
              ]
            grantOf (kind, n) = case kind of
              CounterKind.Keyword kw -> List.genericReplicate n (at Layer.Ability (Modification.GainKeyword kw))
              CounterKind.PlusOnePlusOne -> []
              CounterKind.MinusOneMinusOne -> []
              -- CR 122.1e: a loyalty counter grants nothing, and no CR 613 layer
              -- reads loyalty -- CR 704.5i and CR 606.6 count Object.counters
              -- directly instead.
              CounterKind.Loyalty -> []
              -- CR 714.3: a lore counter grants nothing either. CR 714.2b's
              -- chapter trigger, CR 714.3c's turn-based action and CR 704.5s's
              -- state-based action count Object.counters directly, the same way
              -- loyalty's readers do.
              CounterKind.Lore -> []
              -- Nor does a defense counter, for the reason loyalty's and lore's
              -- arms give: no CR 613 layer reads defense. CR 310.6's removal, CR
              -- 310.11b's trigger and CR 704.5v's state-based action all count
              -- Object.counters directly, exactly as those two do.
              CounterKind.Defense -> []
              -- Nor a time counter (CR 702.63a): vanishing's three minted
              -- abilities count Object.counters directly, as those three do.
              CounterKind.Time -> []
              -- Nor a shield counter (CR 122.1c), and that one is worth saying
              -- twice: "'shield' is not an ability that creatures have and shield
              -- counters are not keyword counters", so the grant CR 122.1b's arm
              -- above makes is exactly what this kind must NOT make. What it
              -- creates instead is a replacement and a prevention effect, minted
              -- by shieldOf below.
              CounterKind.Shield -> []
         in pt <> concatMap grantOf (Map.toList cs)

-- CR 701.60c / 613.1f: a SUSPECTED permanent has menace, so the designation is
-- emitted as a layer-6 grant on the permanent itself. counterGathered's shape and
-- its reasons -- rule 701.60c is rulebook text rather than a card's ability, as
-- CR 122.1b's keyword counter is, and neither is a case on an effect's identity.
--
-- Read off Object.designations on every projection rather than stamped when the
-- designation is set, which IS rule 701.60c's "for as long as it's suspected": a
-- permanent that stops being suspected loses the keyword with nothing to unwind.
-- Rule 701.60c's other half, "this creature can't block", is a combat restriction
-- rather than a characteristic, and lives in Pawl.Engine.CombatRestriction.
--
-- The permanent's own timestamp, counterGathered's choice: the grant has no
-- source of its own to take one from. It is load-bearing only against a CR 613.1f
-- removal, layer-6 grants being commutative among themselves.
designationGathered :: GameState -> [Gathered]
designationGathered gs = concatMap fromObject (Set.toList (GameState.battlefield gs))
  where
    fromObject oid = case Game.lookupObject oid gs of
      Nothing -> []
      Just obj
        | Set.member Designation.Suspected (Object.designations obj) ->
            [ MkGathered
                { gEffect = Nothing,
                  gSource = oid,
                  gAffected = Affected.TheseObjects (Set.singleton oid),
                  gLayer = Layer.Ability,
                  gLowest = Layer.Ability,
                  gTimestamp = Object.timestamp obj,
                  gModification = Modification.GainKeyword Keyword.Type.Menace
                }
            ]
        | otherwise -> []

-- A characteristic a projection holds, at the coarseness CR 613.8a's dependency
-- question needs: applying one effect can only change what another applies to if
-- it WRITES something that one READS. Projection-internal, and deliberately
-- coarser than ProjectedCharacteristics.
data Aspect
  = Types
  | Subtypes
  | -- CR 205.4 / 613.1d: layer 4 writes supertypes as well as card types and
    -- subtypes, so an effect whose affected set names one ("each nonbasic land")
    -- can be moved by another effect the way HasCardType's is. A third aspect
    -- rather than a share of Types, because CR 205.4b keeps the two independent:
    -- nothing that writes a card type writes a supertype.
    Supertypes
  | Colors
  | -- CR 109.3 counts abilities among an object's characteristics and CR 613.1f
    -- writes them, so a keyword is an aspect exactly as a subtype is.
    Keywords
  | PowerA
  | Controller
  deriving (Eq, Ord)

-- Which aspects a Filter reads. Exhaustive on purpose: a new Filter arm reading a
-- projected characteristic must be classified here, or CR 613.8a would silently
-- stop seeing dependencies through it.
--
-- Several arms read nothing a modification can write. IsSource and IsPlayer ask
-- who the candidate is, and IsToken asks what it is represented by.
--
-- IsAttacking reads nothing either, which CR 506.4 makes look otherwise: that rule
-- removes a permanent from combat when its controller changes or it stops being a
-- creature, so an engine deriving attacking-ness from those characteristics would
-- read Controller and Types here. pawl stores it as a combat record instead (CR
-- 109.3), and every writer of that record -- the CR 508.1 declaration, CR 506.4's
-- removals via Combat.removeChanged, Effect.RemoveFromCombat via a resolution,
-- and CR 511.3's clear -- runs BETWEEN projections. So the record is a fixed
-- input to any single projection, which is exactly what CR 613.8a asks about.
-- What that costs is timing, not dependency: the rules remove the permanent the
-- instant control or creature-ness changes, and pawl removes it at the next
-- settle.
--
-- CR 506.4's phasing clause arrives by one of those same doors:
-- Pawl.Engine.Phasing.phaseOut calls Game.removeFromCombat directly, so a
-- permanent that phases out leaves the record without this function's help. The
-- attacked-planeswalker and attacked-battle clauses
-- are answered where the attack target is read (Combat.stillAttacked and
-- Combat.stillAttackedBattle) and never edit the record at all.
filterReads :: Filter.Type.Filter Keyword.Type.Keyword -> Set Aspect
filterReads f = case f of
  Filter.Type.HasCardType _ -> Set.singleton Types
  Filter.Type.HasSupertype _ -> Set.singleton Supertypes
  Filter.Type.HasColor _ -> Set.singleton Colors
  Filter.Type.HasSubtype _ -> Set.singleton Subtypes
  -- CR 613.1f: layer 6 adds and removes abilities, so this atom's answer moves
  -- under the fold as HasCardType's moves under layer 4.
  Filter.Type.HasKeyword _ -> Set.singleton Keywords
  -- The same aspect, since it reads the same set one step coarser: a creature
  -- granted toxic 1 at layer 6 starts satisfying "with toxic" as surely as one
  -- granted flying starts satisfying "with flying".
  Filter.Type.HasKeywordFamily _ -> Set.singleton Keywords
  Filter.Type.PowerAtLeast _ -> Set.singleton PowerA
  Filter.Type.PowerAtMost _ -> Set.singleton PowerA
  -- The same aspect the two above read, covering BOTH powers the atom compares:
  -- Aspect names an aspect of one object's projection, so there is no way to say
  -- "the source's power" separately, and PowerA is what an effect writing either
  -- one writes. Nothing in the pool puts this atom in an affected set -- CR
  -- 702.134a writes it into a target slot, which no CR 613.8a dependency reads.
  Filter.Type.PowerLessThanSource -> Set.singleton PowerA
  -- Its sibling, reading the same aspect for the same reason. CR 702.149a writes
  -- it into a trigger condition, which no CR 613.8a dependency reads either.
  Filter.Type.PowerGreaterThanSource -> Set.singleton PowerA
  Filter.Type.ControlledBy _ -> Set.singleton Controller
  -- The same aspect its sibling above reads: the atom compares the CANDIDATE's
  -- controller, and who defends is a fact about the combat record rather than
  -- about any projection. Nothing in the pool puts this atom in an affected set
  -- either -- CR 702.39a writes it into a target slot.
  Filter.Type.ControlledByDefendingPlayer -> Set.singleton Controller
  -- The same aspect again, and for the same reason both siblings above read it:
  -- each compares the CANDIDATE's controller against a player the surrounding
  -- frame supplies. Neither of these is in an affected set in the pool either --
  -- CR 603.2's atom is written into a target slot.
  Filter.Type.ControlledByBound _ -> Set.singleton Controller
  Filter.Type.ControlledByPlayer _ -> Set.singleton Controller
  -- Reads NOTHING, where its sibling above reads Controller, and the contrast is
  -- CR 108.3's: no Modification writes Object.owner, because no rule changes an
  -- owner at all -- CR 613.1b's layer 2 moves control and rule 108.3 has no
  -- counterpart. So CR 613.8a's clause (b) can never hold on this atom's account.
  -- The position IsToken is in, and for the same immutability. A set the atom
  -- appears in may still be movable through its OTHER conjuncts: Garland, Royal
  -- Kidnapper pairs it with ControlledBy, so its set moves with layer 2 as it
  -- should.
  Filter.Type.OwnedBy _ -> Set.empty
  Filter.Type.IsSource -> Set.empty
  Filter.Type.IsPlayer _ -> Set.empty
  -- Over-declared deliberately, the posture CanHostSubject takes below: the atom
  -- reads the CONTROLLER of every permanent on the battlefield, plus whatever its
  -- nested filter reads of each, and Aspect names an aspect of ONE object's
  -- projection with no way to say "another object's". Nothing in the pool puts
  -- this atom in an affected set -- Oreskos Explorer writes it into a count.
  Filter.Type.ControlsMoreThanYou g -> Set.insert Controller (filterReads g)
  Filter.Type.IsAttacking -> Set.empty
  -- Reads nothing, for IsAttacking's reason: Combat.blockers is the same kind of
  -- record, written by the CR 509.1 declaration, edited by CR 506.4's removals and
  -- emptied at CR 511.3 -- all between projections. CR 509.1b's evasion
  -- restrictions gate the declaration on projected characteristics, but the
  -- declaration is a turn-based action that writes the record once and is over.
  Filter.Type.IsBlocking -> Set.empty
  -- Reads nothing: no Modification writes GameState.events, so no CR 613 layer can
  -- move a set this atom selects.
  Filter.Type.AttackedThisTurn -> Set.empty
  -- Declared as reading Types even though the types are the HOST's. Aspect names
  -- an aspect of ONE object's projection, so there is no way to say "another
  -- object's card types"; over-declaring is the conservative direction. Nothing in
  -- the pool puts this atom in an affected set (#357).
  Filter.Type.IsAttachedToCreature -> Set.singleton Types
  -- Reads nothing, unlike its sibling above, which is why the two are separate
  -- atoms: this one stops at Object.attachedTo (CR 303.4), and no Modification
  -- writes that field -- CR 701.3's attach is a keyword action performed by a
  -- resolution.
  Filter.Type.IsAttachedToPermanent -> Set.empty
  -- Reads nothing, for IsAttachedToPermanent's reason: it stops at
  -- Object.attachedTo and compares an id, and no Modification writes that field.
  Filter.Type.IsAttachedToSource -> Set.empty
  -- Over-declared deliberately: the characteristics behind this atom are the
  -- candidate's (CR 301.5) and the subject's (CR 702.5a), and Aspect cannot say
  -- "another object's". Declaring everything is the conservative direction, and no
  -- card in the pool puts this atom in an affected set (#357).
  Filter.Type.CanHostSubject -> Set.fromList [Types, Subtypes, Colors, Keywords, PowerA, Controller]
  -- Reads nothing: no Modification writes Object.source, so no effect can move a
  -- "nontoken" set.
  Filter.Type.IsToken -> Set.empty
  -- CR 110.5: tap status is not a characteristic, so no layer writes it.
  Filter.Type.IsTapped -> Set.empty
  -- CR 109.3 counts abilities among the characteristics and CR 613.1f writes them,
  -- so this reads the aspect LoseAllAbilities writes -- the same one HasKeyword
  -- reads, Aspect having no finer grain than "the abilities".
  Filter.Type.HasNonManaActivatedAbility -> Set.singleton Keywords
  -- Reads nothing, which is a claim about the rules and not a default: no
  -- Modification writes Object.ringBearerFor -- CR 701.54a's designation is made by
  -- a keyword action a resolution performs, and CR 701.54b keeps it off the
  -- copiable values -- so no CR 613 layer can move a set this atom selects, and CR
  -- 613.8a's clause (b) can never hold on its account. The position IsToken is in.
  -- The set the atom appears in may still be movable through its OTHER conjuncts:
  -- Pawl.Engine.Ring.theRingIsLegendary pairs it with ControlledBy, which reads
  -- Controller, so the emblem's set moves with CR 613.1b's layer 2 as it should.
  Filter.Type.IsRingBearer -> Set.empty
  -- Reads nothing for that atom's reason, restated by each designation's own rule:
  -- the mark is "neither an ability nor part of the permanent's copiable values", so
  -- no Modification writes Object.designations and no CR 613 layer can move a set
  -- this atom selects. What CR 701.60c hangs off `Suspected` IS in layer 6
  -- (designationGathered's menace grant), but that is what the designation WRITES
  -- rather than what this atom reads.
  Filter.Type.HasDesignation _ -> Set.empty
  -- Reads nothing: CR 109.3's characteristics do not include counters, so no
  -- Modification writes Object.counters and no CR 613 layer can move a set this
  -- atom selects. The P/T a +1/+1 counter grants is CR 613.4c's, applied over the
  -- top of the set rather than deciding it.
  Filter.Type.HasCounters _ -> Set.empty
  -- CR 202.3 reads the printed mana cost, and no Modification writes one -- there
  -- is no mana-cost Aspect for this to name, because nothing could change it.
  Filter.Type.ManaValueAtMost _ -> Set.empty
  -- Reads nothing for the atom above's reason: parity is read off the same
  -- printed mana cost no Modification can write.
  Filter.Type.ManaValueIsEven -> Set.empty
  Filter.Type.And fs -> foldMap filterReads fs
  Filter.Type.Or fs -> foldMap filterReads fs
  Filter.Type.Not g -> filterReads g

-- Which aspects a Modification writes -- the other half of the pair above.
--
-- Five arms write Keywords, each writing PC.keywords in applyModification:
-- GainKeyword and LoseAllAbilities per CR 613.1f, both subtype-setting arms per
-- CR 305.7, and ChangeSubtypeWord per CR 612.1 -- a text change reaches the land
-- type inside a landwalk keyword, so it rewrites the map's KEYS.
-- Filter.HasKeyword reads that map, so all five can move an affected set. Keyword
-- counters need no arm, arriving here as synthetic GainKeywords.
--
-- An ability change can also matter to CR 613.8 by changing an effect's EXISTENCE,
-- a different clause that lives in liveGiven.
modificationWrites :: Modification -> Set Aspect
modificationWrites m = case m of
  Modification.GainKeyword _ -> Set.singleton Keywords
  Modification.LoseAllAbilities -> Set.singleton Keywords
  Modification.SetBasePowerToughness _ _ -> Set.singleton PowerA
  Modification.ModifyPowerToughness _ _ -> Set.singleton PowerA
  Modification.SwitchPowerToughness -> Set.singleton PowerA
  Modification.SetLandSubtype _ -> Set.fromList [Subtypes, Keywords]
  Modification.SetLandSubtypeToChosen -> Set.fromList [Subtypes, Keywords]
  Modification.AddLandSubtype _ -> Set.singleton Subtypes
  Modification.SetCreatureSubtype _ -> Set.singleton Subtypes
  Modification.AddCreatureSubtype _ -> Set.singleton Subtypes
  Modification.AddEveryCreatureSubtype -> Set.singleton Subtypes
  Modification.ChangeSubtypeWord _ _ -> Set.fromList [Subtypes, Keywords]
  Modification.AddCardType _ -> Set.singleton Types
  Modification.AddSupertype _ -> Set.singleton Supertypes
  Modification.RemoveSupertype _ -> Set.singleton Supertypes
  Modification.SetColor _ -> Set.singleton Colors
  Modification.AddColor _ -> Set.singleton Colors
  Modification.AddChosenColor -> Set.singleton Colors
  Modification.SetController _ -> Set.singleton Controller
  Modification.SetControllerToSource -> Set.singleton Controller

-- Which aspects a Modification's own QUANTITIES read -- the aspects that decide
-- how much it does, where modificationWrites above says what it changes. The
-- pair of them is CR 613.8a clause (b)'s last limb: applying another effect can
-- change "what it does to any of the things it applies to" without touching what
-- it applies to at all.
--
-- Only the two power/toughness arms carry a quantity, so only they can read
-- anything; every other arm's payload is a card type, a colour, a subtype or a
-- player, all of them literal. A new arm carrying a Quantity has to be
-- classified here, or the dependency would silently stop being seen.
modificationReads :: Modification -> Set Aspect
modificationReads m = case m of
  Modification.SetBasePowerToughness p t -> quantityReads p <> quantityReads t
  Modification.ModifyPowerToughness p t -> quantityReads p <> quantityReads t
  Modification.GainKeyword _ -> Set.empty
  Modification.LoseAllAbilities -> Set.empty
  Modification.SwitchPowerToughness -> Set.empty
  Modification.SetLandSubtype _ -> Set.empty
  Modification.SetLandSubtypeToChosen -> Set.empty
  Modification.AddLandSubtype _ -> Set.empty
  Modification.SetCreatureSubtype _ -> Set.empty
  Modification.AddCreatureSubtype _ -> Set.empty
  Modification.AddEveryCreatureSubtype -> Set.empty
  Modification.ChangeSubtypeWord _ _ -> Set.empty
  Modification.AddCardType _ -> Set.empty
  Modification.AddSupertype _ -> Set.empty
  Modification.RemoveSupertype _ -> Set.empty
  Modification.SetColor _ -> Set.empty
  Modification.AddColor _ -> Set.empty
  Modification.AddChosenColor -> Set.empty
  Modification.SetController _ -> Set.empty
  Modification.SetControllerToSource -> Set.empty

-- Which aspects evaluating a Quantity reads off a projection. The counterpart of
-- filterReads for the OTHER half of a P/T modification, and exhaustive for the
-- same reason.
--
-- Three ways to read one. A Count folds a population its Filter keeps, so it
-- reads whatever that filter reads (plus whatever its aggregation reads); Power
-- and Toughness read CR 613.4's own layer off the object the evaluation is aimed
-- at; and Plus and AgainstSlot are composition, reading what they contain.
--
-- Everything else reads nothing a Modification writes, each for the reason its
-- sibling filter atom gives: ManaValue is computed from the printed mana cost
-- (CR 202.3), ObjectCounters and PlayerCounters read counter stores that CR
-- 109.3 keeps out of the characteristics, and the rest are facts about a player,
-- a binding, a mana pool or the combat record. A Star is notation and evaluates
-- to nothing at all.
--
-- Over-declaring is the safe direction: this is only a SCREEN, and the
-- dependency itself is settled by re-evaluating the modification (see
-- projectDeciding's resolve).
quantityReads :: Quantity.Type.Quantity -> Set Aspect
quantityReads q = case q of
  Quantity.Type.Count c -> filterReads (Count.Type.filter c) <> aggregationReads (Count.Type.aggregation c)
  Quantity.Type.Power -> Set.singleton PowerA
  Quantity.Type.Toughness -> Set.singleton PowerA
  Quantity.Type.Plus a b -> quantityReads a <> quantityReads b
  Quantity.Type.AgainstSlot _ a -> quantityReads a
  Quantity.Type.Literal _ -> Set.empty
  Quantity.Type.ManaValue -> Set.empty
  Quantity.Type.InSlot _ -> Set.empty
  Quantity.Type.Star -> Set.empty
  Quantity.Type.ManaCount _ -> Set.empty
  Quantity.Type.LifeTotal _ -> Set.empty
  Quantity.Type.Speed _ -> Set.empty
  Quantity.Type.IsMonarch _ -> Set.empty
  Quantity.Type.PlayerCounters _ _ -> Set.empty
  Quantity.Type.ObjectCounters _ -> Set.empty
  Quantity.Type.HasDesignation _ -> Set.empty
  Quantity.Type.WasKicked -> Set.empty
  Quantity.Type.OpponentsAttacked _ -> Set.empty
  Quantity.Type.CardsDiscardedThisTurn _ -> Set.empty
  Quantity.Type.BlockersBeyondFirst -> Set.empty

-- What an Aggregation reads off each member the Filter kept. DistinctCardTypes is
-- CR 208.2a's Tarmogoyf fold over card types (CR 613.1d layer 4); Greatest names
-- a per-member Quantity and reads whatever that reads; Members reads nothing but
-- how many survived, which filterReads has already accounted for.
aggregationReads :: Aggregation.Aggregation Quantity.Type.Quantity -> Set Aspect
aggregationReads a = case a of
  Aggregation.Members -> Set.empty
  Aggregation.DistinctCardTypes -> Set.singleton Types
  Aggregation.Greatest q -> quantityReads q

-- What could move `c`'s affected set, as the aspects its filter reads -- Nothing
-- when nothing can move it. Two ways to be immovable, each CR 613.8a's own
-- precondition made cheap to test: a TheseObjects set names ids (CR 611.2c) and an
-- Attached one reads its source's attachment (CR 303.4m), neither of which a
-- modification writes; and a filter reading no projected aspect has nothing to
-- change. CR 613.6's memo is a third, and stays in the fold because it is per
-- object.
movableAspects :: Gathered -> Maybe (Set Aspect)
movableAspects c =
  let readsOf f = let aspects = filterReads f in if Set.null aspects then Nothing else Just aspects
   in case gAffected c of
        Affected.TheseObjects _ -> Nothing
        Affected.Attached -> Nothing
        Affected.Matching f -> readsOf f
        Affected.MatchingAnywhere f -> readsOf f
        -- Always movable, whatever the filter reads: the set is narrowed by WHO
        -- CONTROLS each candidate, and Controller is an aspect layer 2 writes
        -- (CR 613.1b).
        Affected.AttachedPlayerControls f -> Just (Set.insert Controller (filterReads f))

-- Could another effect move this one's affected set at all? movableAspects above
-- with the aspects thrown away and an empty filter still counted movable: a set
-- is movable when something a modification
-- writes selects it -- a Matching or MatchingAnywhere set's predicate over
-- characteristics, or an AttachedPlayerControls set's controller (CR 613.1b). A
-- TheseObjects set names ids (CR 611.2c) and an Attached one reads its source's
-- attachment off the game state (CR 303.4m), and no modification writes either.
staticallyMovable :: Gathered -> Bool
staticallyMovable c = case gAffected c of
  Affected.Matching _ -> True
  Affected.MatchingAnywhere _ -> True
  Affected.TheseObjects _ -> False
  Affected.Attached -> False
  -- Movable, unlike Attached: the attachment half is immovable for Attached's
  -- reason, but WHO CONTROLS a candidate is a layer-2 effect's business
  -- (CR 613.1b), so a control change moves this set.
  Affected.AttachedPlayerControls _ -> True

-- The aspects one effect's parts at a layer write, and the aspects their
-- quantities read. Both halves of CR 613.8a's clause (b) are asked of the whole
-- unit rather than of a single modification, that being CR 613.8's own unit --
-- see effectUnits below.
unitWrites :: NonEmpty.NonEmpty Gathered -> Set Aspect
unitWrites = foldMap (modificationWrites . gModification)

unitReads :: NonEmpty.NonEmpty Gathered -> Set Aspect
unitReads = foldMap (modificationReads . gModification)

-- CR 613.8's unit is an EFFECT, not a modification, and CR 613.6 calls one
-- ability's modifications the parts of that effect. Two parts landing in the SAME
-- layer are applied together with nothing allowed between them, so the reorder
-- groups a layer's candidates into effects before ordering them.
--
-- gatherStatic emits one ability's parts contiguously and keys them alike, and
-- filtering by layer preserves that order, so adjacency finds a unit. A Nothing
-- key is always a unit of one.
--
-- Ashaya, Soul of the Wild + Blood Moon is the pair that needs it: Ashaya's one
-- ability adds a card type AND a subtype, and Blood Moon depends (CR 613.8a) on
-- the first part only. Ordered per modification, an older Blood Moon slots in
-- between them and its set is overwritten by the very subtype it meant to replace.
effectUnits :: [Gathered] -> [NonEmpty.NonEmpty Gathered]
effectUnits =
  let sameEffect a b = case (gEffect a, gEffect b) of
        (Just x, Just y) -> x == y
        _ -> False
   in NonEmpty.groupBy sameEffect

-- CR 613: apply continuous effects layer by layer, ascending. Within a layer, CR
-- 613.8's dependency ordering falling back to CR 613.7 timestamp order. An
-- effect's affected set is evaluated against the partial projection as it stands
-- when that effect applies. CR 613.8's EXISTENCE dependency is the exception,
-- handled by source-liveness rather than the reorder. design.md section 2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs = projectFrom (gather gs) oid gs

-- CR 613.4a layer 7a: apply the object's own characteristic-defining P/T ability.
-- Read from the PARTIAL projection (post-layer-6), so LoseAllAbilities can strip
-- it first, and evaluated against the current state so it recomputes every
-- projection.
--
-- Folded in place rather than emitted as a synthetic Gathered, for three reasons:
--
--   * gather runs BEFORE the fold and has no partial to read, so a pre-gathered
--     CDA could never be removed by Humility at layer 6;
--   * CR 604.3 / 208.2a make a CDA function in ALL zones, while gather walks the
--     battlefield only -- in-place gets all-zones behaviour for free;
--   * a CDA has no source object and no timestamp to sort on under CR 613.7.
--
-- A fourth reason applies here and not to applyColorDefining: this CDA is DYNAMIC.
-- Computing it before the fold would freeze the computed number into Binding.copy
-- at entry, so a Clone of a Tarmogoyf would keep whatever P/T the graveyards held
-- as it entered. CR 707.2 makes a copy acquire values derived from the printed
-- TEXT, so the copy has to recompute.
--
-- Quantity.determine and a bare assignment rather than setPT, because CR 208.2a
-- makes a CDA always produce a number, leaving setPT's keep-the-base arm no case.
-- That is a board difference: a creature whose CDA cannot be determined is a 0/0
-- that CR 704.5f buries, not one with no P/T that survives. setPT stays at layer
-- 7b, where CR 208.2a does not reach and an unevaluable quantity determines
-- nothing.
--
-- CR 604.3a(3): a CDA does not affect any other object, so the Filter.Context is
-- the object's OWN controller -- contrast applyModification's, which is the
-- source's (CR 109.5). The caller always passes the view bounded at
-- Layer.CharacteristicPT, so a CDA's count sees layers 1-6.
--
-- Not implemented: CR 613.8a's clause (c) makes two CDAs able to depend on each
-- other, so a CDA counting a characteristic another CDA defines should see the
-- defined value and sees the printed one instead (#1332).
applyCharacteristicPT :: Count.ViewOf -> GameState -> ObjectId -> ProjectedCharacteristics -> ProjectedCharacteristics
applyCharacteristicPT viewOf gs oid pc = case PC.characteristicPT pc of
  Nothing -> pc
  Just (p, t) ->
    let context = Filter.contextFor (controllerOf oid gs) (Just oid)
     in pc
          { PC.power = Just (Quantity.determine viewOf context gs oid p),
            PC.toughness = Just (Quantity.determine viewOf context gs oid t)
          }

-- projectDeciding with the decision memo dropped, which is what all but one
-- caller wants. `admits` is bound before `oid` so the candidate-only work inside
-- the fold is still shared across a whole-board sweep.
projectWith :: (Layer -> Bool) -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectWith admits cands =
  let forObject = projectDeciding admits cands
   in \oid gs -> fst (forObject oid gs)

-- Project one object against a PRECOMBINED candidate list, applying only the
-- layers the predicate admits. CR 613.1 applies layers in order and Layer's
-- derived Ord IS that order, so `(< bound)` is exactly the layers before this one.
--
-- The bound exists for counting: a Count evaluated while layer L is being applied
-- sees its candidates through `< L`, so one encountered inside that fold is
-- applied at some K < L and sees `< K`. The bound strictly decreases and Layer is
-- finite, so the nesting terminates.
--
-- It is EXACT wherever it is taken, which is the layers holding nothing that
-- writes an aspect anything there counts: CR 613.1 has already settled every
-- earlier layer, and no later one has begun. Where a layer does hold such a pair,
-- CR 613.8a's clause (b) makes the two effects dependent and the layer goes
-- through `resolve`, which reads the running board instead of re-projecting --
-- so no bound is taken and none is needed.
--
-- Returns CR 613.6's decision memo beside the characteristics, since the question
-- "did this effect's affected set include `oid`?" is settled in here and cannot
-- be re-derived from a layer-bounded view without contradicting either CR 613.6
-- or CR 613.7/613.8. abilitiesRemoved is the one caller that wants it, through
-- decisionsUpTo; everything else goes through projectWith just above.
projectDeciding :: (Layer -> Bool) -> [Gathered] -> ObjectId -> GameState -> (ProjectedCharacteristics, Map (ObjectId, Natural) Bool)
-- Candidates-in, then a worker taking the object: everything derived from the
-- candidate list alone is bound before `oid`, so projectAll shares it across the
-- board instead of rebuilding it per object.
projectDeciding admits cands = forObject
  where
    -- Layers 4, 5 and 7a are always visited, even with no gathered effect there:
    -- an object's own CDAs are not gathered candidates. For an object with none,
    -- each extra pass is the identity.
    layers = filter admits (Set.toAscList (Set.insert Layer.Type (Set.insert Layer.Color (Set.insert Layer.CharacteristicPT (Set.fromList (fmap gLayer cands))))))
    -- The layers CR 613.8 could reorder anything in: those holding an effect whose
    -- affected set another can move (staticallyMovable), and those holding an
    -- effect whose MAGNITUDE another can move (countingLayers). Bound before the
    -- object, so a whole-board sweep pays once rather than per object per layer.
    --
    -- Deliberately coarser than the fold's own test, skipping the CR 613.6 memo
    -- (decidedAt) and the filter's own aspects. Both only turn True into False, so
    -- this over-admits -- costing the general path, never a different answer.
    movableLayers = Set.union (Set.fromList (fmap gLayer (filter staticallyMovable cands))) countingLayers
    -- CR 613.8a clause (b)'s "what it does to" limb, screened statically: a layer
    -- holding an effect whose quantities read an aspect that same layer writes. Same-layer is
    -- the whole of it -- a count reading only EARLIER layers is answered exactly by
    -- the bounded view, which is why "holds a count" would be the wrong test.
    --
    -- Over-admits by counting an effect's own writes and by ignoring which
    -- candidate wrote what, both in the harmless direction: they can only route a
    -- layer through resolve, whose answers agree with the tighter fold wherever CR
    -- 613.8 is silent.
    countingLayers = Set.fromList (fmap gLayer (filter countsItsOwnLayer cands))
    writesByLayer = Map.fromListWith Set.union (fmap (\c -> (gLayer c, modificationWrites (gModification c))) cands)
    countsItsOwnLayer c = not (Set.disjoint (modificationReads (gModification c)) (Map.findWithDefault Set.empty (gLayer c) writesByLayer))
    forObject oid gs =
      let applyLayer (partial, decided) lyr =
            let -- What a Count sees when nothing at this layer can move it: the
                -- layers strictly below (CR 613.1). Bound once per layer so the
                -- candidate-only work inside projectUpTo is shared across every
                -- count the layer evaluates.
                bounded = viewUpTo lyr cands gs
                -- CR 613.3: characteristic-defining abilities first, within the
                -- layer they define. Subtype is layer 4 (CR 613.1d), colour is
                -- layer 5 (CR 613.1e), P/T is layer 7a (CR 613.4a). Taken per
                -- OBJECT, since the dependency scan seeds every other object's
                -- snapshot the same way.
                seedFor o p = case lyr of
                  Layer.Type -> applySubtypeDefining p
                  Layer.Color -> applyColorDefining p
                  Layer.CharacteristicPT -> applyCharacteristicPT bounded gs o p
                  _ -> p
                seeded = seedFor oid partial
                -- CR 613.6: the affected set is asked ONCE per effect, at the
                -- lowest layer that effect reaches, and remembered in `decided` for
                -- its other layers. It remembers "no" as faithfully as "yes": an
                -- artifact already a creature is outside March of the Machines' set
                -- when March starts to apply, so it stays outside at 7b (#233).
                --
                -- Only an effect with parts in more than one layer carries a key;
                -- everything else is Nothing and never touches the Map.
                --
                -- Object-parameterised, like applyUnit and applyOne below: the
                -- dependency scan asks all three about every object on the board,
                -- not only the one being projected.
                appliesTo o ds pc c = case gEffect c of
                  Just k | Just answer <- Map.lookup k ds -> answer
                  _ -> affects (gSource c) o (gAffected c) pc gs
                -- Fold every part of ONE effect landing in this layer, in the order
                -- the card lists them (CR 613.6). The parts share a source and an
                -- affected set, so the caller asks applicability once.
                --
                -- The ViewOf comes from the caller and is the SAME one for every
                -- object the effect reaches: CR 613.6 applies an effect's parts to
                -- its whole affected set at once, so a count inside it must not see
                -- the effect's own work on a sibling.
                applyUnit viewOf o pc cs = List.foldl' (\p c -> applyModification viewOf (gSource c) gs o (gModification c) p) pc (NonEmpty.toList cs)
                -- Apply one effect, recording its decision the first time.
                -- Re-inserting an existing key rewrites the value just read, so this
                -- is idempotent rather than a second determination.
                applyOne viewOf o (pc, ds) cs =
                  let c = NonEmpty.head cs
                      answer = appliesTo o ds pc c
                      ds' = case gEffect c of
                        Nothing -> ds
                        Just k -> Map.insert k answer ds
                   in (if answer then applyUnit viewOf o pc cs else pc, ds')
                -- CR 613.6's other half of movableAspects, and the half that is per
                -- object: an effect whose set this object already settled cannot be
                -- moved at it, whatever its filter reads.
                decidedAt ds c = case gEffect c of
                  Just k -> Map.member k ds
                  Nothing -> False
                -- Every OTHER battlefield object's state as this layer begins --
                -- characteristics and CR 613.6 memo alike -- which is the partial
                -- that object's own fold holds here, so all of them derive the same
                -- order. Lazy, and scanned after the projected object, so a
                -- dependency visible right here never builds one.
                --
                -- Terminates for projectUpTo's reason: a snapshot admits strictly
                -- fewer layers, so a movable layer below `lyr` recurses on a
                -- strictly shorter Layer.
                --
                -- Not implemented: a snapshot carries CR 208.3's noncreature P/T
                -- gate, which the projected object's own mid-fold partial does not,
                -- so an affected set filtering on the POWER of a permanent that is
                -- not yet a creature can read the two differently (#1111). Nor are
                -- objects outside the battlefield scanned, so a MatchingAnywhere set
                -- whose dependency shows only in another zone is missed (#1112).
                snapshot o =
                  let (p, d) = projectDeciding (\l -> admits l && l < lyr) cands o gs
                   in (seedFor o p, d)
                -- Keyed rather than an association list, because resolve's ViewOf
                -- looks an object up once per candidate a Count folds over. Strict in
                -- the Map's values only to WHNF, and every value here is a pair
                -- constructor, so the snapshots stay as lazy as the comment above says.
                otherBoards = Map.fromSet snapshot (Set.delete oid (GameState.battlefield gs))
                -- CR 613.8b: an effect that depends on another waits for it, and CR
                -- 613.7 timestamp order picks the next among those waiting on
                -- nothing. Re-deriving `ready` each round IS CR 613.8c, and removing
                -- one effect per pass makes it terminate: `pending` is finite and
                -- strictly shorter each call, and `batch` is never empty.
                --
                -- Applicability is judged HERE, as each effect is applied, rather
                -- than from `seeded` -- CR 613.8's premise describes a state that
                -- exists only once a predecessor has applied.
                --
                -- An empty `ready` means every remaining effect waits on another, so
                -- a dependency loop is in there, and CR 613.8b says to apply the
                -- loop in timestamp order. Only the loop's own members escape; an
                -- effect merely waiting on the loop gets its turn once it unwinds.
                --
                -- `pending` holds EFFECTS, not modifications, that being CR 613.8's
                -- unit.
                resolve (pc, ds) others pending = case pending of
                  [] -> (pc, ds)
                  _ ->
                    let -- One applicability answer per effect per round per object,
                        -- shared by the dependency scan rather than recomputed per
                        -- pair -- this is the hot loop. CR 613.6 gives every part of
                        -- an effect the same affected set, so the head part answers
                        -- for the unit.
                        answersAt o p d = fmap (\(i, cs) -> (i, appliesTo o d p (NonEmpty.head cs))) pending
                        answerFor ans i = Maybe.fromMaybe False (List.lookup i ans)
                        -- The board as it stands: every battlefield object plus the
                        -- projected one, which need not be on the battlefield.
                        running = Map.insert oid (pc, ds) others
                        -- What a Count sees: CR 613 puts no bound on the state an
                        -- effect's magnitude is computed from, so a count reads the
                        -- layers below AND this layer's effects that have already
                        -- applied. Pawl.ProjectionSpec's Ferocious Chorus group is
                        -- what proves it, and proves the reorder beside it.
                        --
                        -- No recursion, so nothing here has to terminate: the
                        -- partials are already built. What made a bound necessary
                        -- was a count RE-PROJECTING its candidates; this reads them.
                        --
                        -- An object with no entry falls back to the bounded view. A
                        -- card in a zone gather does not walk has no continuous
                        -- effect on it to miss (#160). Not implemented: a SPELL is
                        -- projected but not scanned here, so a count reading a
                        -- same-layer effect on one still gets the bound (#1332).
                        --
                        -- noncreaturePT is CR 208.3, applied for the reason
                        -- projectDeciding applies it to its own result: the card types
                        -- it reads are settled at layer 4, and the bounded view this
                        -- stands in for applies it too, so the two agree wherever
                        -- there is nothing at this layer to see.
                        viewOfBoard board o = case Map.lookup o board of
                          Just (p, _) -> Just (viewOfCharacteristics o (noncreaturePT o gs p) (controllerOf o gs) (countersOf o gs) gs)
                          Nothing -> bounded o
                        view = viewOfBoard running
                        -- Every object CR 613.8a's question ranges over, the
                        -- projected one first.
                        boards = (oid, pc, ds, answersAt oid pc ds) : fmap (\(o, (p, d)) -> (o, p, d, answersAt o p d)) (Map.toList others)
                        -- CR 613.8a clause (b)'s "what it applies to", at ONE
                        -- object: applying `b` there changes whether `a` applies.
                        -- The tentative application is thrown away and only the
                        -- answer kept. An effect that does not apply at `o` changes
                        -- nothing there, so it cannot be the `b` of a dependency
                        -- seen at `o`.
                        --
                        -- `b` is applied WHOLE, every part landing in this layer:
                        -- half an effect is not a state CR 613 describes.
                        changesAt (j, bs) (i, as) (o, p, d, ans) =
                          let a = NonEmpty.head as
                           in not (decidedAt d a)
                                && answerFor ans j
                                && appliesTo o d (applyUnit view o p bs) a /= answerFor ans i
                        -- `a` depends on `b` when that holds ANYWHERE: CR 613.8a
                        -- asks about the effect's whole affected SET, so one object
                        -- deciding it for the board is the rule rather than an
                        -- approximation of it, and CR 613.8b's loop -- which shows
                        -- as opposite one-way edges at different objects -- becomes
                        -- visible at all.
                        --
                        -- Reached only for a pair that could interact, `b` writing
                        -- an aspect `a`'s filter reads. Clause (c)'s CDA exclusion
                        -- needs no test: a CDA is never a candidate, both in-place
                        -- folds sitting outside this list. Clause (b)'s "text" half
                        -- has no producer; "existence" is handled by liveGiven.
                        movesSet x@(_, as) y@(_, bs) =
                          case movableAspects (NonEmpty.head as) of
                            Nothing -> False
                            Just aspects ->
                              not (Set.disjoint aspects (unitWrites bs))
                                && any (changesAt y x) boards
                        -- CR 613.8a clause (b)'s LAST limb: applying `b` changes
                        -- what `a` does to the things it applies to, without
                        -- touching what it applies to. Only a magnitude can move --
                        -- the two P/T arms are the only modifications carrying a
                        -- quantity (modificationReads) -- so what is compared is the
                        -- power and toughness `a` writes, from the SAME base under
                        -- two views: the board as it stands, and the board with `b`
                        -- applied wherever it applies.
                        --
                        -- Judged only where `a` itself applies. CR 613.8a asks what
                        -- an effect does "to any of the things it applies to", so an
                        -- object outside `a`'s set has no answer to give.
                        --
                        -- No decidedAt gate, unlike changesAt above: a settled
                        -- affected set says nothing about how much the effect does.
                        changesMagnitude (i, as) (_, bs) =
                          let after = appliedEverywhere bs
                              afterView = viewOfBoard after
                              writtenPT p = (PC.power p, PC.toughness p)
                           in any
                                ( \(o, p, _, ans) ->
                                    answerFor ans i
                                      && writtenPT (applyUnit view o p as) /= writtenPT (applyUnit afterView o p as)
                                )
                                boards
                        -- `b` applied to every object whose set holds it, judged and
                        -- applied against the board as it stands -- CR 613.6's
                        -- simultaneity again, and the same rule applyOne obeys when
                        -- the effect is really chosen.
                        appliedEverywhere bs =
                          Map.mapWithKey
                            (\o (p, d) -> (if appliesTo o d p (NonEmpty.head bs) then applyUnit view o p bs else p, d))
                            running
                        dependsOnOne x@(i, as) y@(j, bs) =
                          j /= i
                            && ( movesSet x y
                                   || (not (Set.disjoint (unitReads as) (unitWrites bs)) && changesMagnitude x y)
                               )
                        ready = filter (\a -> not (any (dependsOnOne a) pending)) pending
                        -- The dependency edges, built only when the whole round is
                        -- blocked -- which is the one case that needs to know the
                        -- SHAPE of the tangle rather than merely that there is one.
                        edges = Map.fromList (fmap (\a@(i, _) -> (i, fmap fst (filter (dependsOnOne a) pending))) pending)
                        -- Everything reachable from `start` by following edges.
                        reach seen queue = case queue of
                          [] -> seen
                          x : xs ->
                            if Set.member x seen
                              then reach seen xs
                              else reach (Set.insert x seen) (Map.findWithDefault [] x edges <> xs)
                        -- On a cycle iff it can reach itself in one step or more.
                        onCycle (i, _) = Set.member i (reach Set.empty (Map.findWithDefault [] i edges))
                        batch = case ready of
                          _ : _ -> ready
                          -- `ready` empty means every remaining effect has an
                          -- outgoing edge, and a finite graph where every node has
                          -- one contains a cycle -- so `cyclic` is never empty and
                          -- the fallback to `pending` is unreachable. Written out
                          -- because `minimumBy` is partial and this keeps it total.
                          [] -> case filter onCycle pending of
                            [] -> pending
                            cyclic -> cyclic
                        -- CR 613.7a gives every part of one ability the source
                        -- permanent's timestamp, so the head part's stamp is the
                        -- unit's.
                        (chosen, next) = List.minimumBy (Ord.comparing (\(_, cs) -> gTimestamp (NonEmpty.head cs))) batch
                     in resolve (applyOne view oid (pc, ds) next) (Map.mapWithKey (\o st -> applyOne view o st next) others) (filter ((/= chosen) . fst) pending)
                -- Is there anything at this layer CR 613.8 could reorder? One Set
                -- lookup, almost always in an empty Set.
                movableHere = Set.member lyr movableLayers
                -- CR 613.6's memo, populated against `seeded` -- sound only on the
                -- branch below where nothing is movable, which is exactly where an
                -- effect's answer cannot change as the layer is applied.
                remember ds c = case gEffect c of
                  Nothing -> ds
                  Just k
                    | gLayer c /= lyr || Map.member k ds -> ds
                    | otherwise -> Map.insert k (affects (gSource c) oid (gAffected c) seeded gs) ds
             in if movableHere
                  then resolve (seeded, decided) otherBoards (zip [0 :: Int ..] (effectUnits (filter (\c -> gLayer c == lyr) cands)))
                  else
                    -- Nothing here can be moved, so no candidate depends on any other:
                    -- no affected set can change as the layer is applied, and no
                    -- magnitude either, since nothing at this layer writes an aspect
                    -- anything here counts (countingLayers). CR 613.8 therefore says
                    -- nothing, CR 613.7 timestamp order stands, and judging against
                    -- `seeded` through the bounded view gives the same answers as
                    -- judging one at a time -- the rule where the rule is silent, not
                    -- a shortcut past it. Also almost every layer of almost every
                    -- projection, which is why it keeps a tighter fold than
                    -- `resolve`'s.
                    let decided' = List.foldl' remember decided cands
                        applies c = case gEffect c of
                          Nothing -> affects (gSource c) oid (gAffected c) seeded gs
                          Just k -> Map.findWithDefault False k decided'
                        ordered = List.sortOn gTimestamp (filter (\c -> gLayer c == lyr && applies c) cands)
                        step pc c = applyModification bounded (gSource c) gs oid (gModification c) pc
                     in (List.foldl' step seeded ordered, decided')
          (folded, decisions) = List.foldl' applyLayer (copiableCharacteristics oid gs, Map.empty) layers
       in (noncreaturePT oid gs folded, decisions)

-- CR 208.3: "A noncreature permanent has no power or toughness, even if it's a
-- card with a power and toughness printed on it (such as a Vehicle)." Applied to
-- the FINISHED fold, which is the only place it can go: the card types it reads
-- are settled at CR 613.1d layer 4, so an uncrewed Consulate Dreadnought reports
-- no power while a crewed one reports 7 -- CR 301.7b's "it immediately has its
-- printed power and toughness", falling out of the type change rather than being
-- a second effect.
--
-- CR 208.3a is the other half and needs no code: an effect that would set or
-- modify a noncreature permanent's P/T "is created even though it doesn't do
-- anything unless that permanent becomes a creature", and pawl's continuous
-- effects are STORED and re-applied per projection, so a Veteran Motorist's
-- +1/+1 on an uncrewed Vehicle sits in GameState.continuousEffects and starts
-- counting the moment layer 4 makes it a creature. That it applies at all is
-- what applying this gate AFTER layer 7 buys -- gating before the fold would
-- leave `addPT` nothing to add to and lose the effect for good.
--
-- Applied at the END of projectDeciding rather than in projectWith's body, and
-- that placement is load-bearing for speed rather than taste: moving it out
-- eta-expands away the candidate-only sharing projectAll rests on, and costs 2x
-- on the aura benchmarks.
--
-- ON THE BATTLEFIELD ONLY, which is rule 208.3's own second sentence: "A
-- noncreature object not on the battlefield has power or toughness only if it
-- has a power and toughness printed on it." So a Vehicle in a hand or a
-- graveyard keeps its printed numbers, and every off-battlefield read of them
-- -- Filter.PowerAtLeast over a graveyard pool, a Count, the codec -- is
-- unaffected.
--
-- Sound on a layer-BOUNDED projection too (projectUpTo), and on the mid-layer
-- partial `resolve` builds its ViewOf from, though either may be reading card
-- types its layer has not finished: a printed creature type arrives in the seed
-- and no bound can remove it, so the only thing a bound can hide is a
-- type-CHANGING effect -- and layer 4 is settled before layer 7, which is the
-- only layer whose modifications carry a quantity to count with.
noncreaturePT :: ObjectId -> GameState -> ProjectedCharacteristics -> ProjectedCharacteristics
noncreaturePT oid gs pc
  | Set.member CardType.Creature (PC.cardTypes pc) = pc
  | not (Set.member oid (GameState.battlefield gs)) = pc
  | otherwise = pc {PC.power = Nothing, PC.toughness = Nothing}

-- CR 208.5: "If a creature somehow has no value for its power, its power is 0.
-- The same is true for toughness." The hole this fills is opened by a card that
-- takes a characteristic-defining ability away -- Blood Moon on an Ashaya, Soul
-- of the Wild who has made herself a nonbasic land, where CR 305.7 strips the
-- CDA that was her only source of a value and CR 305.7's own "doesn't add or
-- remove any card types" leaves her a creature.
--
-- Guarded on CREATURE, which is the whole of the rule's premise, and the line CR
-- 208.3 draws from the other side: a noncreature permanent does not HAVE a power
-- or a toughness at all, which is a different thing from having one with no
-- value, so an uncrewed Consulate Dreadnought keeps the Nothing noncreaturePT
-- just gave it (CR 301.7a). Applied after noncreaturePT for that reason -- 208.3
-- decides whether 208.5's premise is even reached, so it has to run first.
--
-- The card types are read off the FINISHED fold, so a permanent that layer 4
-- made a creature is judged as one (CR 613.1d).
noValuePT :: ProjectedCharacteristics -> ProjectedCharacteristics
noValuePT pc
  | not (Set.member CardType.Creature (PC.cardTypes pc)) = pc
  | otherwise =
      pc
        { PC.power = Just (Maybe.fromMaybe 0 (PC.power pc)),
          PC.toughness = Just (Maybe.fromMaybe 0 (PC.toughness pc))
        }

-- Project one object against a PRECOMPUTED candidate list. gather is
-- oid-independent, so a whole-board sweep gathers once and folds each object
-- (projectAll) instead of re-gathering per object.
--
-- Where CR 208.5 goes, and deliberately NOT inside projectWith beside
-- noncreaturePT: a layer-bounded view (projectUpTo) is a mid-fold intermediate,
-- and "has no value" is not a question that can be asked of one -- a */* creature
-- has no value yet at every bound below layer 7a and a value at every bound above
-- it. Every projection a reader can observe -- project, projectAll, and so every
-- powerOf, toughnessOf and state-based-action sweep -- comes through here.
--
-- Not implemented: CR 208.5's substitution is likewise absent from the view a
-- Count reads mid-layer, so a count over the power of a creature whose only
-- source of one has been stripped reads no value where the rule says 0 (#1332).
projectFrom :: [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectFrom cands oid gs = noValuePT (projectWith (const True) cands oid gs)

-- CR 613.1: a projection bounded to the layers BEFORE `bound` -- what a Count sees
-- while layer `bound` is being applied. See projectWith for the termination
-- argument this serves.
projectUpTo :: Layer -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectUpTo bound = projectWith (< bound)

-- CR 613.6's answers, as the fold itself reached them, for every multi-part
-- effect whose lowest layer is at or below `bound`: keyed by gEffect, True when
-- the effect's affected set held `oid` at the layer it started to apply.
--
-- Bounded INCLUSIVELY, unlike projectUpTo, and that is the whole point: an
-- effect deciding at `bound` decides against what its same-layer predecessors
-- have already produced (CR 613.7 timestamp order, CR 613.8 dependency order),
-- so the layer has to be run to reach the answer. Running it does not spoil the
-- answer, because the answer was recorded as the effect applied rather than read
-- off the finished layer -- which is the trap `< bound` was avoiding and
-- `<= bound` on projectUpTo would fall into.
--
-- Terminates for the same reason projectUpTo does: a Count met while layer
-- `bound` is being applied still sees `< bound`, so the nesting strictly
-- descends a finite Layer.
decisionsUpTo :: Layer -> [Gathered] -> ObjectId -> GameState -> Map (ObjectId, Natural) Bool
decisionsUpTo bound cands oid gs = snd (projectDeciding (<= bound) cands oid gs)

-- Project every battlefield object against ONE gather: O(gather + P*fold) instead
-- of the O(P*(gather+fold)) of calling project per object. The hot path for SBA
-- sweeps and combat, which query many objects against the same state.
projectAll :: GameState -> Map ObjectId ProjectedCharacteristics
projectAll gs =
  let cands = gather gs
      -- Bound separately, NOT inlined: projectWith does its candidate-only work
      -- when applied to `cands`, so sharing this partial application shares that
      -- work across every object on the board.
      forObject = projectFrom cands
   in Map.fromSet (\oid -> forObject oid gs) (GameState.battlefield gs)

-- One object's characteristics out of a PRE-PROJECTED board, falling back to a
-- fresh single-object projection for an id the board does not hold. Every
-- `...Given` reader below is this plus one field read, and every plain `...Of`
-- reader is one of those against Map.empty.
--
-- Not an approximation of `project`, it IS `project`: where the key exists,
-- projectAll folded the same candidate list `project` would rebuild for that
-- object alone, and where it does not, the id is off the battlefield and
-- projecting it here is the work `project` would have done anyway.
--
-- The board is a SNAPSHOT of one GameState, right only while that state is the one
-- being read. Every hoist in the engine sits inside a single pure pass over one
-- `gs`, and the monadic callers around them take a fresh State.get.
--
-- The battlefield test keeps the fallback from costing anything: the board is
-- value-strict, so touching it at all projects every permanent, and an id gather
-- could never have reached is not worth that. Never a different answer -- the
-- board is keyed on this state's battlefield, so an off-battlefield id could not
-- have been a key.
projectGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> ProjectedCharacteristics
projectGiven pcs oid gs =
  let found =
        if Map.null pcs || not (Set.member oid (GameState.battlefield gs))
          then Nothing
          else Map.lookup oid pcs
   in case found of
        Just pc -> pc
        Nothing -> project oid gs

powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf = powerGiven Map.empty

powerGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Maybe Integer
powerGiven pcs oid gs = PC.power (projectGiven pcs oid gs)

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = PC.toughness (project oid gs)

-- CR 702: an object's keyword abilities after the layer fold, counted per
-- keyword (see ProjectedCharacteristics.keywords for why the count is kept).
-- Most readers want hasKeyword or totalToxic rather than the raw counts.
keywordsOf :: ObjectId -> GameState -> Map Keyword Natural
keywordsOf = keywordsGiven Map.empty

keywordsGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Map Keyword Natural
keywordsGiven pcs oid gs = PC.keywords (projectGiven pcs oid gs)

-- CR 105.2 / 613.1e: an object's colours after the layer fold. The sole read
-- point -- the closed half never reads Face.manaCost for colour.
colorsOf :: ObjectId -> GameState -> Set Color.Color
colorsOf = colorsGiven Map.empty

colorsGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Color.Color
colorsGiven pcs oid gs = PC.colors (projectGiven pcs oid gs)

-- CR 602 / 613.1f: an object's activated abilities after the layer system, the
-- same projection posture as keywordsOf. A Humility'd creature has none.
--
-- CR 702.178a's gate is applied HERE, over the finished projection, rather than
-- inside the fold: "as long as your speed is 4, this object has '[Ability]'" is
-- an ability the object has or lacks, and every reader of an object's activated
-- abilities goes through this pair. The layer system is asked first and this
-- second, which is the right order -- a Muraganda Raceway whose rules text CR
-- 305.7 stripped has no max speed ability to gate, whatever its controller's
-- speed, and CR 613.1f's LoseAllAbilities says the same of a creature.
--
-- The condition is re-asked on every read, not sampled: CR 604.1 makes a static
-- ability "simply true", so speed falling would take the ability away with no
-- event in between. Cheap by construction -- the filter is skipped entirely for
-- the overwhelming majority of abilities, which carry no condition.
--
-- The view is the FULL one, unlike Projection.conditionHolds' layer-bounded view:
-- nothing here is inside the fold, so there is no circularity to bound against.
-- The Filter.Context is the object's own controller and the object itself, which
-- is what makes CR 109.5's "your" in "your speed is 4" the ability's controller.
abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesOf = abilitiesGiven Map.empty

abilitiesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesGiven pcs oid gs = abilitiesFromCharacteristics (projectGiven pcs oid gs) oid gs

-- abilitiesGiven with the projection already in hand -- the half
-- viewOfCharacteristics calls, which holds a ProjectedCharacteristics and no way
-- to ask for another one.
--
-- CR 702.29b and CR 702.77b are why handAbilitiesOf is in this list: a cycling or
-- reinforce ability "continues to exist while the object is on the battlefield
-- and in all other zones", so the object HAS it here. What it cannot do here is
-- be activated -- CR 113.6m gives its "Discard this card" cost the hand, and
-- Pawl.Engine.Activate.abilitiesForGiven filters this list through functionsIn
-- before offering anything. Pawl.Engine.Mana's reader is safe for the other
-- reason: neither ability adds mana, so CR 605.1a excludes both.
abilitiesFromCharacteristics :: ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesFromCharacteristics pc oid gs =
  let granted ability = case ActivatedAbility.condition ability of
        Nothing -> True
        Just cond -> Condition.holds (fullView gs) (Filter.contextFor (controllerOf oid gs) (Just oid)) gs oid cond
   in -- Rule 702's own activated abilities are appended here, the shape
      -- intrinsicReplacementsOf takes one function down: the card's printed list
      -- is the projection's, and Pawl.Engine.Keyword mints the rule's from the
      -- POST-LAYER keyword map, so Humility takes crew away with the rest. Every
      -- reader gets one flat list and never learns rule 702 wrote part of it.
      filter
        granted
        ( PC.activatedAbilities pc
            <> Keyword.battlefieldAbilitiesOf (PC.keywords pc)
            <> Keyword.handAbilitiesOf (Map.keysSet (PC.keywords pc))
        )

-- CR 614 / 613 layer 6: an object's replacement effects after the layer system,
-- the same projection posture as abilitiesOf. A Humility'd creature has none --
-- except the shield pair, which no layer can reach; see shieldOf.
replacementsOf :: ObjectId -> GameState -> [ReplacementEffect]
replacementsOf oid gs =
  let pc = project oid gs
   in PC.replacementEffects pc <> intrinsicReplacementsOf pc <> shieldOf oid gs

-- CR 122.1c: the pair of effects one or more shield counters create -- "if this
-- permanent would be destroyed as the result of an effect, instead remove a shield
-- counter from it" and "if damage would be dealt to this permanent, prevent that
-- damage and remove a shield counter from it".
--
-- ONE of each however many counters are on it, which is the rule's own word
-- ("create a single replacement effect and a single prevention effect"): the count
-- is how many times the pair may still be applied, and each application takes one
-- off. A list keyed on presence rather than on the number is what says that -- two
-- counters must not offer CR 616.1's choice two interchangeable candidates.
--
-- Read off Object.counters rather than off the projection, unlike every arm of
-- intrinsicReplacementsOf: counters are not a characteristic (CR 122.1's markers
-- "are not objects and have no characteristics"), and rule 122.1c gives the pair as
-- a rule rather than as an ability. So the layer system has nothing to remove --
-- "if a creature with a shield counter loses its abilities, the shield counter will
-- still protect it as normal" -- which is CR 306.5b's posture reached by a
-- different route.
--
-- Neither effect carries a pattern naming its permanent. The destruction half has
-- no pattern field at all and is self-only by construction, and the prevention
-- half's self-scope is read off its SOURCE in Replacement.applies rather than baked
-- into DamagePattern.whichRecipient, because that field is compared to the event's
-- Pawl.Types.Recipient TAG -- which says how the damage reached the permanent (CR
-- 510.1b), not which permanent it reached, so a shield baked as ToCreature would
-- miss the damage an attacker assigns to the same permanent as a planeswalker.
shieldOf :: ObjectId -> GameState -> [ReplacementEffect]
shieldOf oid gs =
  if shieldCounters oid gs == 0
    then []
    else
      [ ReplacementEffect.DestructionR DestructionRewrite.RemoveShieldCounter,
        ReplacementEffect.DamageR
          DamagePattern.MkDamagePattern
            { DamagePattern.whichKind = Nothing,
              DamagePattern.whatSource = Filter.Type.And [],
              DamagePattern.whichRecipient = Nothing
            }
          DamageRewrite.PreventRemovingShieldCounter
      ]

-- CR 122.1c: how many shield counters this object has, which is how many more
-- events its pair may replace. Also the short-circuit's question in
-- replacementsAffecting below, where a permanent's counters are the one route to a
-- replacement effect that no base FACE shows.
shieldCounters :: ObjectId -> GameState -> Natural
shieldCounters oid gs = case Game.lookupObject oid gs of
  Nothing -> 0
  Just obj -> Map.findWithDefault 0 CounterKind.Shield (Object.counters obj)

-- CR 306.5b / 614.1c: a planeswalker's intrinsic "enters with loyalty counters"
-- replacement effect, CR 310.4b's identically shaped one for a battle's defense,
-- and rule 702.136a's riot and CR 714.3a's Saga lore counter beside them. Those
-- four are the whole list of ENTRY replacements, and each arm below is one of
-- them; the keyword call at the end also mints CR 702.37b's megamorph row, which
-- is a CR 614.1e one (see below).
--
-- CR 310.8a's protector is NOT here, though it is also chosen as a battle enters:
-- rule 310.8a names no ability and cites no rule 614, where CR 310.4b says
-- outright that its ability "creates a replacement effect". It lives in
-- Pawl.Engine.Event.designateProtector instead, which explains what minting it
-- here cost.
--
-- Minted from the finished projection rather than stored on the card, the posture
-- Mana.subtypeMana and Keyword.triggeredAbilitiesOf take. Three consequences, all
-- of them the rules':
--
--   * keyed on the PROJECTED card type, so a permanent that became a planeswalker
--     is judged as one and a paper-only one is not;
--   * reads the PROJECTED loyalty, a copiable value under CR 707.2, so a Clone of
--     a planeswalker enters with the copy's printed loyalty per CR 707.5 -- which
--     falls out of the mint, since the loop re-collects each iteration (CR 616.1f);
--   * minting AFTER the layer fold puts it out of reach of LoseAllAbilities, which
--     is deliberate: CR 306.5b gives the ability as a rule, so the card type is
--     what layer 6 would have to remove.
--
-- Nothing for a planeswalker with no printed loyalty, which the CardSpec lint
-- makes unrepresentable. The battle/defense arm leans on that lint's twin the same
-- way, and all three consequences above read across unchanged -- CR 310.4b is CR
-- 306.5b with one characteristic swapped for another, down to the projected card
-- type deciding and the projected number being read.
--
-- The KEYWORD half is minted by Keyword.mintedReplacementsOf off the SAME
-- finished projection, and the third consequence above reads the other way for
-- it: minting after the layer fold puts riot INSIDE LoseAllAbilities' reach,
-- because the keyword itself is what layer 6 removes. That is the rule --
-- Humility'd, a creature has no riot to offer a choice -- where CR 306.5b's
-- loyalty survives because a card type is not an ability.
--
-- That half is not all CR 614.1c's: CR 702.37b's megamorph mints a CR 614.1e
-- TURNED-FACE-UP row through the same call. It needs no gathering of its own,
-- because the CR 616.1 loop matches every gathered row against the event it is
-- offered -- so a row of one class simply never applies to an event of another.
intrinsicReplacementsOf :: ProjectedCharacteristics -> [ReplacementEffect]
intrinsicReplacementsOf pc =
  [ -- CR 614.1c: the entering object is the ability's own source, so the pattern
  -- is Filter.IsSource.
  ReplacementEffect.EntryR Filter.Type.IsSource (EntryRewrite.WithCounters CounterKind.Loyalty n)
  | Set.member CardType.Planeswalker (PC.cardTypes pc),
    Loyalty.MkLoyalty n <- Maybe.maybeToList (PC.loyalty pc)
  ]
    -- CR 310.4b's intrinsic "this permanent enters with a number of defense
    -- counters on it equal to its printed defense number" -- CR 306.5b's clause
    -- one rule number over, keyed on the projected card type and reading the
    -- projected defense for the same three reasons.
    <> [ ReplacementEffect.EntryR Filter.Type.IsSource (EntryRewrite.WithCounters CounterKind.Defense n)
       | Set.member CardType.Battle (PC.cardTypes pc),
         Defense.MkDefense n <- Maybe.maybeToList (PC.defense pc)
       ]
    <> Keyword.mintedReplacementsOf (PC.keywords pc)
    -- CR 714.3a's intrinsic "this Saga enters with a lore counter on it", minted
    -- off the same finished projection for CR 306.5b's reason: a subtype is not an
    -- ability, so a Saga under Humility keeps it.
    <> Saga.entryReplacementsOf pc

-- CR 614.1: every replacement effect active on the battlefield, PAIRED WITH ITS
-- SOURCE -- a ControllerRelation pattern (CR 109.5's "you") is unanswerable
-- without it. The set is collected live rather than locked in ahead of time.
-- Short-circuits when no permanent's base card has one, so an ordinary zone change
-- does not project the whole board.
--
-- The short-circuit reads BASE cards while the result reads the PROJECTION, sound
-- only because the ways to acquire an unprinted replacement effect are
-- `EntryR AsCopy` -- and a card with that arm is itself a base card with a
-- replacement effect -- CR 122.1c's shield counters, whose own disjunct reads the
-- object rather than a face, or a minting keyword, which the two riot disjuncts cover:
-- one for a face that PRINTS such a keyword and one for a face whose static
-- ability GRANTS it (Spider-Punk's "other Spiders you control have riot"). The
-- granting card is on the battlefield whenever the grant is, which is what makes
-- reading base faces enough.
--
-- Not implemented: a minting keyword that reaches a permanent through a stored
-- continuous effect or a keyword counter is on no base face, so this gate does
-- not see it (#833).
--
-- Past the short-circuit this projects per permanent rather than threading one
-- board, so a board holding any replacement effect pays a fresh gather per
-- permanent (#435).
replacementsAffecting :: GameState -> [(ObjectId, ReplacementEffect)]
replacementsAffecting gs =
  let onBattlefield = Set.toList (GameState.battlefield gs)
      -- CR 122.1c's pair is minted from the permanent's COUNTERS, so it is on no
      -- base face either -- and unlike the three intrinsic disjuncts below it is on
      -- no projection of the face's own text, which is why it is asked before the
      -- face is looked up at all.
      baseHas oid | shieldCounters oid gs > 0 = True
      baseHas oid = case Game.faceOf oid gs of
        Nothing -> False
        -- The planeswalker disjunct keeps CR 306.5b's intrinsic replacement inside
        -- the short-circuit: minted from the projection, it appears in no base
        -- face's list.
        Just face ->
          not (null (Face.replacementEffects face))
            || Set.member CardType.Planeswalker (TypeLine.types (Face.typeLine face))
            -- The Saga disjunct is the planeswalker one's twin: CR 714.3a's
            -- intrinsic replacement is minted from the projection too, so it
            -- appears in no base face's list either.
            || Set.member Subtype.Type.Saga (TypeLine.subtypes (Face.typeLine face))
            -- And the battle disjunct is the third of the same shape, for CR
            -- 310.4b's intrinsic replacement.
            || Set.member CardType.Battle (TypeLine.types (Face.typeLine face))
            || any Keyword.mintsReplacement (Face.keywords face)
            || any (any (grantsKeywordWhere Keyword.mintsReplacement) . StaticAbility.modifications) (Face.staticAbilities face)
      forOne oid = fmap (\re -> (oid, re)) (replacementsOf oid gs)
   in if not (any baseHas onBattlefield)
        then []
        else concatMap forOne onBattlefield

-- Does this modification hand its affected objects a keyword satisfying `p`? Read
-- only by short-circuits over BASE faces, to answer "could anything on this board
-- have the thing rule 702 mints from that keyword" -- `Keyword.mintsReplacement`
-- for the gather above (rule 702.136a's riot), and
-- `Keyword.mintsCombatRestriction` for Pawl.Engine.CombatRestriction's (rule
-- 702.98a's unleash).
--
-- A CLASSIFICATION of a modification's shape, not a case on an effect's identity:
-- what it asks is "does this grant a keyword", and the predicate the caller
-- passes is the rulebook's.
--
-- Exhaustive rather than a catch-all, for the reason `layer` gives above: a
-- modification added later that also hands out abilities would otherwise answer
-- False here and take its grantee out of the gathered set, which is a MISSING
-- row rather than a build failure.
grantsKeywordWhere :: (Keyword -> Bool) -> Modification.Modification -> Bool
grantsKeywordWhere p m = case m of
  Modification.GainKeyword k -> p k
  Modification.LoseAllAbilities -> False
  Modification.SetBasePowerToughness _ _ -> False
  Modification.ModifyPowerToughness _ _ -> False
  Modification.SetLandSubtype _ -> False
  Modification.SetLandSubtypeToChosen -> False
  Modification.AddLandSubtype _ -> False
  Modification.SetCreatureSubtype _ -> False
  Modification.AddCreatureSubtype _ -> False
  Modification.AddEveryCreatureSubtype -> False
  Modification.AddCardType _ -> False
  Modification.AddSupertype _ -> False
  Modification.RemoveSupertype _ -> False
  Modification.ChangeSubtypeWord _ _ -> False
  Modification.SetController _ -> False
  Modification.SetControllerToSource -> False
  Modification.SetColor _ -> False
  Modification.AddColor _ -> False
  Modification.AddChosenColor -> False
  Modification.SwitchPowerToughness -> False

-- CR 603 / 613 layer 6: an object's printed-and-granted triggered abilities after
-- the layer system. A Humility'd creature has none.
--
-- Not the whole list: rule 702.70's poisonous is a triggered ability the RULES give
-- an object for holding a keyword, and Keyword.triggeredAbilitiesOf mints those
-- from PC.keywords instead. A reader wanting every triggered ability must add
-- them, as Event's scan does. Not folded into PC.triggeredAbilities because that
-- field is built DURING the fold while the mint needs the finished keyword counts
-- -- which is also what makes Humility free.
triggeredAbilitiesOf :: ObjectId -> GameState -> [TriggeredAbility Card.Type.Card]
triggeredAbilitiesOf oid gs = PC.triggeredAbilities (project oid gs)

subtypesOf :: ObjectId -> GameState -> Set Subtype.Type.Subtype
subtypesOf = subtypesGiven Map.empty

subtypesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Subtype.Type.Subtype
subtypesGiven pcs oid gs = PC.subtypes (projectGiven pcs oid gs)

-- CR 201.1 / 707.2: the object's projected names -- a Clone's is the name it
-- copied, not "Clone". Plural for the objects rule 709 gives several names, and
-- EMPTY for one CR 708.2a gives none.
namesOf :: ObjectId -> GameState -> Set CardName.CardName
namesOf oid = PC.names . project oid

-- CR 709.4a: "An object has the chosen name if one of its names is the chosen
-- name." The one question the rules ask about a name, and the only reader every
-- name comparison in the engine should go through -- a Room with both doors open
-- has "Roaring Furnace" and "Steaming Sauna" and does NOT have the
-- "Roaring Furnace//Steaming Sauna" its combined Face renders them as.
hasName :: CardName.CardName -> ObjectId -> GameState -> Bool
hasName name oid gs = Set.member name (namesOf oid gs)

-- CR 205.4: the object's projected supertypes, the sibling of subtypesOf.
supertypesOf :: ObjectId -> GameState -> Set Supertype.Supertype
supertypesOf = supertypesGiven Map.empty

-- The same supertypes against a pre-projected board (#200):
-- Mana.productionTagsGiven asks this of every mana source in a sweep that has
-- already gathered one.
supertypesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Supertype.Supertype
supertypesGiven pcs oid gs = PC.supertypes (projectGiven pcs oid gs)

cardTypesOf :: ObjectId -> GameState -> Set CardType.CardType
cardTypesOf = cardTypesGiven Map.empty

cardTypesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set CardType.CardType
cardTypesGiven pcs oid gs = PC.cardTypes (projectGiven pcs oid gs)

-- CR 613.1d: creature-ness is the projected card-type question, the same
-- projection posture as keywordsOf. An Opalescence'd enchantment is a creature.
isCreatureOf :: ObjectId -> GameState -> Bool
isCreatureOf = isCreatureGiven Map.empty

isCreatureGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isCreatureGiven pcs oid gs = Set.member CardType.Creature (cardTypesGiven pcs oid gs)

-- CR 613.1d again, for the card type CR 115.4's "any target" pool and CR 120.3c's
-- loyalty removal both ask about. Projected for isCreatureOf's reason.
isPlaneswalkerOf :: ObjectId -> GameState -> Bool
isPlaneswalkerOf = isPlaneswalkerGiven Map.empty

isPlaneswalkerGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isPlaneswalkerGiven pcs oid gs = Set.member CardType.Planeswalker (cardTypesGiven pcs oid gs)

-- CR 613.1d a third time, for CR 115.4's fourth kind of "any target" and CR
-- 120.3h's defense-counter removal. Projected for isCreatureOf's reason.
--
-- Pawl.Engine.Battle.isBattle asks the same question of an already-finished
-- projection, and is the form rule 310's own module uses -- that module imports no
-- Projection, deliberately. This is the id-taking form its callers on this side of
-- the graph (Pawl.Engine.Target's pool, Pawl.Engine.Damage's classification) want.
isBattleOf :: ObjectId -> GameState -> Bool
isBattleOf = isBattleGiven Map.empty

isBattleGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isBattleGiven pcs oid gs = Set.member CardType.Battle (cardTypesGiven pcs oid gs)

-- The same question against a PRECOMPUTED candidate list rather than a
-- pre-projected board. For a caller asking about a handful of objects out of a
-- whole battlefield, that is cheaper: one gather and one fold per object, where
-- projectAll would fold every permanent. Same answer either way.
isCreatureFrom :: [Gathered] -> ObjectId -> GameState -> Bool
isCreatureFrom cands oid gs = Set.member CardType.Creature (PC.cardTypes (projectFrom cands oid gs))

-- Membership, which DISCARDS the count -- right for every keyword whose multiple
-- instances the rules call redundant (CR 702.3c, CR 702.9c). A keyword that stacks
-- gets its own reader instead, as totalToxic does.
hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword = hasKeywordGiven Map.empty

hasKeywordGiven :: Map ObjectId ProjectedCharacteristics -> Keyword -> ObjectId -> GameState -> Bool
hasKeywordGiven pcs keyword oid gs = Map.member keyword (keywordsGiven pcs oid gs)

-- Rule 702.164b: a creature's total toxic value sums the N of every toxic ability
-- it has. Not hasKeyword's question, toxic being parameterized, but the same
-- projection posture -- summed over post-layer keywords, so a Humility'd creature
-- has none. Each ability contributes its own N with no redundancy clause, which is
-- why the projection counts keywords instead of setting them.
totalToxic :: ObjectId -> GameState -> Natural
totalToxic oid gs = toxicIn (keywordsOf oid gs)

-- totalToxic's fold, over a keyword map the caller already has -- so a reader that
-- took the map through CR 608.2h's last-known fallback sums it the same way.
toxicIn :: Map Keyword Natural -> Natural
toxicIn keywords =
  let value keyword count = case keyword of
        Keyword.Type.Toxic n -> n * count
        _ -> 0
   in sum (Map.elems (Map.mapWithKey value keywords))

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
-- NOT `gather`, and PROJECTION-FREE throughout: affects reads controllerOf to
-- supply CR 109.5's "you" when matching a Filter, so a controlGrants that
-- consulted the layers would be mutually recursive with it. That is why
-- Affected.Matching and MatchingAnywhere are unsupported below (#195), and why the
-- CR 305.7 gate this used to apply is gone -- liveGiven -> affectsBase -> affects's
-- Matching arm -> controllerOf -> back here diverged rather than answering wrong,
-- for a subtype-setting effect whose filter asks about control (Celestial Dawn).
--
-- Hoisted like setLandSubtypeEffects and for its reason: `controls` calls
-- controllerOf once per battlefield object, so recomputing this list inside it
-- would be quadratic in the battlefield, inside a loop the SBA sweep runs at every
-- priority boundary.
--
-- Removing that gate is what CR 613.1 wanted anyway: control is layer 2
-- (CR 613.1b), the CR 305.7 land-subtype strip is layer 4 (CR 613.1d) and
-- LoseAllAbilities layer 6 (CR 613.1f), so both strips happen AFTER this grant has
-- started to apply, and CR 613.6 keeps it applying "even if the ability generating
-- the effect is removed during this process". CR 613.8a scopes dependency within a
-- layer, so no reordering reaches across either. Nothing is added at layer 6
-- either: its vocabulary cannot put a static ability on an object.
--
-- Unobservable in that direction today, and honestly so: no card in the pool is a
-- land whose PRINTED rules text grants control, so nothing can watch a grant
-- survive the strip. What the removal is observable for is the divergence above,
-- via Pawl.ProjectionSpec's two Celestial Dawn cases. Pawl.ProjectionSpec's "a
-- layer-6 strip on the Aura does not undo its layer-2 grant" pins the same
-- direction for layer 6.
controlGrants :: GameState -> [ControlGrant]
controlGrants gs =
  let grantsOf permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.faceOf permId gs of
          Nothing -> []
          Just face ->
            let isControl sa = any isControlOp (StaticAbility.modifications sa)
                isControlOp m = case m of
                  Modification.SetControllerToSource -> True
                  _ -> False
                toGrant sa =
                  MkControlGrant
                    { cgSource = permId,
                      cgAffected = StaticAbility.affected sa,
                      cgTimestamp = Object.timestamp permObj
                    }
             in fmap toGrant (filter isControl (Face.staticAbilities face))
   in concatMap grantsOf (Set.toList (GameState.battlefield gs))

-- CR 108.4 / 613.1b: an object's controller is its owner, overridden by layer-2
-- control effects, last timestamp wins (CR 613.7). Two sources -- stored
-- continuous effects and control-granting static abilities -- both carrying a
-- Timestamp, so they merge into one maximum.
--
-- Still a lean fold rather than the full ProjectedCharacteristics pass: control
-- feeds combat, mana and priority, and is needed before P/T.
controllerOf :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOf oid gs = controllerOfGiven (controlGrants gs) Set.empty oid gs

-- controllerOf with the grant list PRECOMPUTED and a visited set.
--
-- The visited set is a CR 613.8b loop-escape analog, not an implementation of it
-- (#946) -- the shape liveGiven had before appliedSetEffects replaced it with rule
-- 613.8's real ordering: deriving a grant's player asks for its SOURCE's
-- controller, which can re-enter this function. Re-entering an object already
-- under question returns its owner, so a cycle grants nothing. Unreachable today
-- and unobservable if it existed -- a direct query on any object in the cycle
-- returns that object's owner either way.
controllerOfGiven :: [ControlGrant] -> Set ObjectId -> ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOfGiven grants visited oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj ->
    if Set.member oid visited
      then Just (defaultControllerOf obj)
      else
        let visited' = Set.insert oid visited
            -- Does an affected set carried by `source` name `oid`? `controlNames`
            -- below is the enumeration this membership test reads off.
            namesFrom source a = Set.member oid (controlNames gs source a)
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
              [] -> Just (defaultControllerOf obj)
              setters -> Just (snd (List.maximumBy (Ord.comparing fst) setters))

-- Which objects an affected set NAMES, for the CR 613.1b layer-2 control fold.
-- Parameterized by the source because Affected.Attached asks about the SOURCE's
-- state, and the stored and derived paths carry different sources.
--
-- Total with no wildcard, and three of five arms empty: this fold must not project
-- (see controlGrants), ruling out Matching and MatchingAnywhere, and
-- AttachedPlayerControls would re-enter controllerOf. No card produces any of the
-- three (#195).
controlNames :: GameState -> ObjectId -> Affected.Affected -> Set ObjectId
controlNames gs source a = case a of
  Affected.TheseObjects s -> s
  -- CR 303.4m: the source's own attachment. No projection needed, which is what
  -- keeps the fold reading this lean.
  Affected.Attached -> maybe Set.empty Set.singleton (Game.lookupObject source gs >>= Object.attachedTo >>= Recipient.objectOf)
  Affected.Matching _ -> Set.empty
  Affected.MatchingAnywhere _ -> Set.empty
  Affected.AttachedPlayerControls _ -> Set.empty

-- CR 603.3a: every object whose controller a CR 613.1b layer-2 effect OVERRIDES,
-- and who it says controls it -- the sample Event.recordEvent takes so a trigger
-- scanned at the CR 117.5 boundary is still credited to whoever controlled its
-- source at the event. See GameState.controlWhenTriggered for why unnamed objects
-- need no entry.
--
-- Both of controllerOfGiven's setter sources are enumerated in the order it
-- consults them, and the VALUE is its own answer rather than a re-derivation, so
-- the CR 613.7 timestamp contest is settled once.
controlOverrides :: GameState -> Map ObjectId PlayerId.PlayerId
controlOverrides gs =
  let grants = controlGrants gs
      fromStored eff = case ContinuousEffect.modification eff of
        Modification.SetController _ -> controlNames gs (ContinuousEffect.source eff) (ContinuousEffect.affected eff)
        _ -> Set.empty
      fromGrant g = controlNames gs (cgSource g) (cgAffected g)
      named = Set.unions (fmap fromStored (GameState.continuousEffects gs) <> fmap fromGrant grants)
      entry oid = fmap ((,) oid) (controllerOfGiven grants Set.empty oid gs)
   in Map.fromList (Maybe.mapMaybe entry (Set.toList named))

-- CR 110.2 / 108.4a: the controller a CR 613.1b layer-2 effect OVERRIDES. Two
-- rules, one per kind of object: a permanent's default controller is whoever it
-- entered under (CR 110.2), and a card that has no controller at all uses its
-- owner (CR 108.4a).
--
-- Object.enteredUnder is written for the two zones CR 109.4 gives a controller
-- -- CR 110.2a's entry controller on the battlefield, CR 405.4's caster on the
-- stack (see Pawl.Types.Object) -- so it is Nothing everywhere else and this is
-- the owner CR 108.4a asks for.
--
-- On the hot path: controllerOfGiven runs once per battlefield object inside
-- `controls`, which the SBA sweep calls at every priority boundary. The one
-- Maybe match measured inside the benchmark suite's run-to-run stddev.
defaultControllerOf :: Object.Object -> PlayerId.PlayerId
defaultControllerOf obj = Maybe.fromMaybe (Object.owner obj) (Object.enteredUnder obj)

-- The battlefield permanents a player controls (CR 108.4). Computes the grant
-- list ONCE and threads it, rather than letting each controllerOf rebuild it --
-- the difference between linear and quadratic in the battlefield, in a function
-- the state-based-action sweep calls at every priority boundary.
controls :: PlayerId.PlayerId -> GameState -> [ObjectId]
controls pid gs = controlsGiven (controlGrants gs) pid gs

-- controls with the grant list PRECOMPUTED, for a caller that then asks
-- controllerOfGiven about the permanents it hands back. Threading the one list
-- keeps such a loop from rebuilding it per candidate.
controlsGiven :: [ControlGrant] -> PlayerId.PlayerId -> GameState -> [ObjectId]
controlsGiven grants pid gs =
  filter (\oid -> controllerOfGiven grants Set.empty oid gs == Just pid) (Set.toList (GameState.battlefield gs))

-- CR 800.4a: does this stored effect give `pid` control of an object? The
-- classification Departure asks for. SetController's payload IS the player who
-- gains control -- baked at effect creation as the source's controller -- so the
-- payload is what "that player" names.
givesControlTo :: PlayerId.PlayerId -> ContinuousEffect.ContinuousEffect -> Bool
givesControlTo pid eff = case ContinuousEffect.modification eff of
  Modification.SetController who -> who == pid
  -- Names no player, so it cannot be classified from the effect alone: CR 109.5
  -- makes its player the current controller of the source, which needs a
  -- GameState this function does not take. False is right for every reachable
  -- state, the constructor being authored only on a static ability the projection
  -- re-derives and never stores. The residual case is #199.
  Modification.SetControllerToSource -> False
  _ -> False
