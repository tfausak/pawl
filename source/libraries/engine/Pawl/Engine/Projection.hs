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
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LastKnown as LastKnown
import Pawl.Types.Layer (Layer)
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import Pawl.Types.Modification (Modification)
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
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
import qualified Pawl.Types.Supertype as Supertype
import Pawl.Types.Timestamp (Timestamp)
import qualified Pawl.Types.Toughness as Toughness
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TypeLine as TypeLine

-- CR 613.1: the layer a modification applies in. THE ABI classification the
-- rules core would ask -- never the modification's identity. One of the case-on-
-- Modification functions this module is the sole home of.
layer :: Modification -> Layer
layer m = case m of
  Modification.GainKeyword _ -> Layer.Ability
  Modification.LoseAllAbilities -> Layer.Ability
  Modification.SetBasePowerToughness _ _ -> Layer.SetPT
  Modification.ModifyPowerToughness _ _ -> Layer.ModifyPT
  Modification.SetLandSubtype _ -> Layer.Type
  Modification.AddLandSubtype _ -> Layer.Type
  Modification.SetCreatureSubtype _ -> Layer.Type
  Modification.AddCardType _ -> Layer.Type
  Modification.ChangeSubtypeWord _ _ -> Layer.Text
  Modification.SetController _ -> Layer.Control
  Modification.SetControllerToSource -> Layer.Control
  Modification.SetColor _ -> Layer.Color
  Modification.AddColor _ -> Layer.Color
  Modification.SwitchPowerToughness -> Layer.SwitchPT

-- Apply one modification to characteristics-in-progress. THE ONE applier
-- (Resolve : Effect :: Projection : Modification). P/T quantities are evaluated
-- here against the CURRENT state, which is correct for a static ability's
-- continuous effect (CR 604.2 -- Opalescence's mana value is re-read per affected
-- object every projection). A continuous effect created by a spell's RESOLUTION
-- must not be re-read (CR 608.2h / 611.2d); it is frozen to Literals at store
-- time by Resolve, via freezeQuantities -- and is not stored at all when a
-- quantity would not freeze, so the P/T quantities reaching here from a
-- resolution are Literals and this evaluation is the identity on them.
--
-- CR 109.5: "for a static ability, this is the current controller of the object
-- it's on" -- the effect's SOURCE's controller, not the affected object's. `src`
-- (the Gathered candidate's own source) supplies both the
-- perspective and the InSlot binding source for the built Filter.Context. `lyr`
-- is the layer bound the Pawl.Types.Count fold sees (viewUpTo) -- the layers
-- already applied when this modification is folded in. This is the #34 fix: a
-- characteristic-defining ability is the OTHER case (CR 604.3a(3): a CDA does
-- not directly affect the characteristics of any other object), and it is
-- applied by applyCharacteristicPT, which builds its context from the
-- object's OWN controller instead.
--
-- Omnath, Locus of Mana is the first static ability in the pool whose
-- modification carries a player-scoped quantity, and PowerToughnessSpec's "CR
-- 109.5 the count reads Omnath's controller" pins that the count reads the right
-- PLAYER -- an opponent floating green does not move it.
--
-- Not implemented: that is not a falsifier for THIS line. Omnath's static ability
-- is on Omnath and modifies Omnath, so `src` and `oid` are one object and both
-- perspectives resolve to the same controller; swapping the two would not change
-- its power. Discriminating them needs a source whose controller can differ from
-- the affected object's -- an Aura on an opponent's creature (#155).
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
        -- CR 205.1a: "when an effect sets one or more of an object's subtypes,
        -- the new subtype(s) replaces any existing subtypes from the appropriate
        -- set (creature types, land types, artifact types, enchantment types,
        -- planeswalker types, or spell types)". The appropriate set here is the
        -- CREATURE types (CR 205.3m, Pawl.Engine.Subtype.isCreatureType) and only
        -- those -- CR 205.1b's last sentence says the object keeps "all of its
        -- prior card types and subtypes other than creature types". So an
        -- animated permanent's land type survives becoming a Frog, and so would
        -- an artifact's or an enchantment's.
        --
        -- Nothing else moves. Unlike SetLandSubtype this strips no abilities:
        -- CR 305.7's ability clause is a rule about LANDS, not about setting a
        -- subtype, and Turn to Frog's own ability loss is a separate layer-6
        -- modification (CR 613.1f). And CR 205.1a's last sentence -- "Removing
        -- an object's subtype doesn't affect its card types at all" -- is why no
        -- card type is touched: "becomes a blue Frog" leaves the object a
        -- creature because it already was one, and Jade Statue's "becomes a
        -- Golem artifact creature" says the card-type half separately with
        -- AddCardType.
        --
        -- Not checked: CR 205.3d's "an object can't gain a subtype that doesn't
        -- correspond to one of that object's types", which none of the layer-4
        -- subtype arms asks (#530).
        Modification.SetCreatureSubtype s ->
          pc {PC.subtypes = Set.insert s (Set.filter (not . Subtype.isCreatureType) (PC.subtypes pc))}
        Modification.AddCardType t ->
          pc {PC.cardTypes = Set.insert t (PC.cardTypes pc)}
        -- CR 305.7, second sentence: "It loses all abilities generated from its
        -- rules text, ITS OLD LAND TYPES, and any copiable effects affecting that
        -- land, and it gains the appropriate mana ability for each new basic land
        -- type." Three clauses, and the arm does the first two; the mana ability
        -- rides the new subtype and is read at the mana call site (CR 305.6).
        --
        -- The SUBTYPE clause takes the land types and nothing else. CR 205.3i
        -- (Pawl.Engine.Subtype.isLandType) is the list; the fourth sentence -- "Setting
        -- a land's subtype doesn't add or remove any card types (such as
        -- creature) or supertypes" -- is why a creature type on an animated
        -- permanent has to survive.
        --
        -- CR 305.7 says "one or more of the basic land types" and this
        -- modification carries exactly one, which is not a narrowing: no printed
        -- card SETS a land's subtype to more than one basic type (Blood Moon,
        -- Magus of the Moon and Zhao all set Mountain alone; the multi-type
        -- cards -- Urborg, Prismatic Omen -- all ADD).
        --
        -- The ABILITY clause takes every kind of ability a card's rules text can
        -- generate, which is every one this record carries plus the three decided
        -- outside it. Keywords and the four fields below are stripped here; a
        -- permanent's static abilities, its player abilities and its block
        -- requirements are decided before the fold instead, by the CR 305.7 gates
        -- in gather, Pawl.Engine.PlayerEffect.applying and Pawl.Engine.BlockRequirement.
        -- instances -- all three calling liveGiven -- because an ability whose
        -- effect lands on OTHER objects has to be kept out of the candidate list
        -- rather than erased from its own projection. Those gates read BASE
        -- characteristics and this arm reads the projection, so the two halves do
        -- not agree on an object that became a land at layer 4 (#391); what this
        -- arm reaches, it strips completely.
        --
        -- CR 305.7's next sentence -- "Note that this doesn't remove any
        -- abilities that were GRANTED to the land by other effects" -- needs no
        -- guard here: every field this clears is seeded from the card by
        -- copiableCharacteristics, and the only granting modification is
        -- GainKeyword at layer 6, which is applied after this layer-4 arm and so
        -- lands on an already-emptied map.
        --
        -- CR 604.3 makes a characteristic-defining ability a static ability, so
        -- characteristicPT goes with the rest -- the same reason LoseAllAbilities
        -- clears it at layer 6. CR 613.6 rescues neither: a CDA would first apply
        -- at layer 7a, which is after both.
        --
        -- Not stripped, and not an oversight: CR 305.7's third clause, "any
        -- copiable effects affecting that land", is a layer-1 question this
        -- layer-4 arm cannot answer (#406).
        Modification.SetLandSubtype s ->
          pc
            { PC.subtypes = Set.insert s (Set.filter (not . Subtype.isLandType) (PC.subtypes pc)),
              PC.keywords = Map.empty,
              PC.characteristicPT = Nothing,
              PC.activatedAbilities = [],
              PC.replacementEffects = [],
              PC.triggeredAbilities = []
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
        -- CR 105.3's parenthetical: an "in addition" colour is added to the
        -- object's existing colours rather than replacing them.
        Modification.AddColor cs ->
          pc {PC.colors = Set.union cs (PC.colors pc)}
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

-- One layer's worth of a continuous effect: its source (for the Matching
-- ExcludesSource self-exclusion), the set it affects, its timestamp, and the one
-- modification that applies in `gLayer`. Projection-internal; not a domain type.
data Gathered = MkGathered
  { -- WHICH effect this part belongs to, named only when that matters: Just
    -- (source, the ability's index on that source) for a static ability with
    -- parts in more than one layer, Nothing for everything else. CR 613.6's "the
    -- same set of objects in each other applicable layer" is a decision keyed by
    -- exactly this pair, made once and reused (projectWith). Two parts of one
    -- ability share it; two abilities never do, even on the same permanent with
    -- the same filter.
    --
    -- A one-part effect is asked once by construction, so it needs no identity at
    -- all: every counter, every stored effect and every single-line static
    -- ability is Nothing, which is the whole pool bar three cards.
    gEffect :: !(Maybe (ObjectId, Natural)),
    gSource :: ObjectId,
    gAffected :: Affected.Affected,
    gLayer :: Layer,
    -- CR 613.6's DECISION POINT: the lowest layer reached by any part of the
    -- effect THIS part belongs to -- "if an effect starts to apply in one layer and/or
    -- sublayer, it will continue to be applied to the same set of objects in each
    -- other applicable layer and/or sublayer". The layer fold gets this for free
    -- by visiting layers in order and memoizing on gEffect; a caller that must
    -- ask the same question from OUTSIDE the fold (abilitiesRemoved) has no memo
    -- to read, so the answer is carried here instead of re-derived at the wrong
    -- layer (#326).
    --
    -- Equal to gLayer for every one-part effect, which is every counter, every
    -- stored effect and every single-line static ability.
    gLowest :: Layer,
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
  -- A source attached to a PLAYER names no object, so the set is empty here too
  -- -- CR 702.5d's enchant-player Auras reach the battlefield through
  -- AttachedPlayerControls below instead.
  Affected.Attached -> (Game.lookupObject source gs >>= Object.attachedTo >>= Recipient.objectOf) == Just oid
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
        -- is additionally safe today because Pawl.Engine.Resolve constructs every
        -- stored ContinuousEffect with Affected.TheseObjects, never Matching;
        -- that is a fact about Resolve's current call sites, not something
        -- this fold enforces. That is a laziness accident, not a structural
        -- guarantee -- such a card would force `perspective`, which recurses
        -- back into controllerOf -> controlGrants -> this same liveness gate,
        -- and hangs rather than answering wrong (#197).
        perspective = controllerOf source gs
     in Set.member oid (GameState.battlefield gs)
          && Filter.matches (Filter.MkContext perspective (Just source)) (viewOfCharacteristics oid partial (controllerOf oid gs) gs) f
  -- CR 303.4b / 303.4m: the SOURCE's attachment again, but read for the PLAYER it
  -- names -- "creatures enchanted player controls". A source that is unattached,
  -- or attached to an object, names no player and so affects nobody.
  --
  -- The controller comparison is CR 613.1b's layer 2, already applied by the time
  -- this set is asked: a creature the enchanted player has since lost control of
  -- is out of the set, and one they have gained control of is in it. Same
  -- `controllerOf oid gs` the Matching arm above hands to the candidate's view.
  --
  -- Unlike that arm's `perspective`, this call is FORCED on every candidate, so
  -- the #197 hazard is worth stating rather than inheriting: controllerOf ->
  -- controlGrants -> liveGiven -> affectsBase re-enters this function, which would
  -- loop if a SetLandSubtype effect (the only kind liveGiven feeds) ever carried
  -- an AttachedPlayerControls set. None does -- the pool's one static example is
  -- Blood Moon, and Pawl.Engine.Resolve stores every ContinuousEffect with
  -- Affected.TheseObjects -- and controllerOfGiven's own namesFrom answers False
  -- for this arm rather than recursing.
  --
  -- The Filter's perspective is the Matching arm's, not the enchanted player's:
  -- CR 109.5 fixes "you" as the effect's source's controller, and being the set
  -- the enchanted player controls does not move that. Curse of Death's Hold's
  -- filter is a bare card type, so nothing forces it today either.
  --
  -- The candidate's controller is bound ONCE and used twice (the comparison and
  -- the view), because controllerOf is the un-hoisted variant -- it rebuilds
  -- controlGrants, a whole-board walk, on every call. That still costs one such
  -- walk per candidate per layer this set is asked in, which the Matching arm
  -- above avoids only by leaving the same call an unforced thunk. Hoisting it out
  -- of `affects` would mean threading the grant list through every caller.
  Affected.AttachedPlayerControls f -> case Game.lookupObject source gs >>= Object.attachedTo of
    Just (Recipient.ToPlayer pid) ->
      let controller = controllerOf oid gs
       in Set.member oid (GameState.battlefield gs)
            && controller == Just pid
            && Filter.matches (Filter.MkContext (controllerOf source gs) (Just source)) (viewOfCharacteristics oid partial controller gs) f
    _ -> False

-- CR 205.4a: supertypes are read from the printed type line -- no Modification
-- arm changes a supertype (#311). Empty when the object has no underlying card.
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
viewOfObject oid gs = viewOfObjectGiven Map.empty (controlGrants gs) oid gs

-- viewOfObject against a pre-projected board and a precomputed grant list -- the
-- shape a caller filtering a whole pool of candidates wants, so that one
-- projection and one grant walk serve every candidate instead of two per
-- candidate. See projectGiven for what the board is and when it is valid.
viewOfObjectGiven :: Map ObjectId ProjectedCharacteristics -> [ControlGrant] -> ObjectId -> GameState -> Filter.View
viewOfObjectGiven pcs grants oid gs =
  viewOfCharacteristics oid (projectGiven pcs oid gs) (controllerOfGiven grants Set.empty oid gs) gs

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

-- CR 113.7a / 608.2h: `fullView`, except that the one object named by `src` is
-- read from last known information once it no longer exists. The view a resolving
-- spell or ability wants for anything it reads ABOUT ITS OWN SOURCE -- "this
-- creature deals damage equal to its power" when the cost already sacrificed it
-- (Ghitu Fire-Eater).
--
-- Scoped to `src` alone, not applied to every id, and that is the whole point:
-- CR 608.2h's fallback is about "a specific object" an effect asks after, while
-- an off-battlefield candidate a COUNT sweeps is matched on printed
-- characteristics instead (#160, viewUpTo's haddock). Widening this to all ids
-- would silently overturn that rule.
--
-- The trigger is that the id names no object. It is not a proxy for "left the
-- battlefield": CR 400.7 mints a fresh id on every zone change and deletes the
-- old one, so an id that still resolves is an object that has not moved, and one
-- that does not is precisely CR 608.2h's "no longer in the zone it was expected
-- to be in". A source that is still on the battlefield therefore reads LIVE, as
-- CR 608.2h's first clause requires -- last known information is the fallback,
-- never the default.
--
-- Nothing when the source is gone AND nothing was recorded for it -- an object
-- that ceased without a zone change ever running over it, which is what
-- Game.cease does to an ability and what Departure.objectsLeaveWith does to a
-- departing player's objects. Honest Nothing rather than a zero, and it lands on
-- the no-op every caller already gives an unevaluable quantity.
--
-- The controller comes from the same record, not from a Nothing: CR 608.2h says
-- the effect uses the object's LAST KNOWN INFORMATION, and CR 613.1b control is
-- information about the object even though CR 109.3 denies it is a
-- characteristic. Passing Nothing here would say the gone source was controlled
-- by nobody rather than by whoever last controlled it -- which a ControlledBy
-- filter read against that source would answer wrongly.
viewWithLastKnown :: ObjectId -> GameState -> Count.ViewOf
viewWithLastKnown src gs oid =
  if oid == src && not (Map.member oid (GameState.objects gs))
    then
      fmap
        (\lk -> viewOfCharacteristics oid (LastKnown.characteristics lk) (Just (LastKnown.controller lk)) gs)
        (Map.lookup oid (GameState.lastKnown gs))
    else fullView gs oid

-- CR 608.2h: this object's last known information, and ONLY when the id names
-- nothing -- "the effect uses the current information of that object if it's in
-- the public zone it was expected to be in; if it's no longer in that zone ...
-- the effect uses the object's last known information". Nothing while the object
-- is still there, so a caller falls through to its ordinary live reader: last
-- known information is the fallback, never the default.
--
-- The shared liveness test for the two readers below, so a rule that says "use
-- last known information" cannot come to mean one thing for keywords and another
-- for control. viewWithLastKnown above makes the same test inline rather than
-- through this, deliberately: it wants Nothing when the object is gone and
-- nothing was filed, where fullView would hand back a Just over an empty
-- projection, so it cannot share the fall-through shape these two want.
lastKnownOf :: ObjectId -> GameState -> Maybe LastKnown.LastKnown
lastKnownOf oid gs =
  if Map.member oid (GameState.objects gs)
    then Nothing
    else Map.lookup oid (GameState.lastKnown gs)

-- keywordsOf with CR 608.2h's fallback: the post-layer keywords the object has,
-- or the ones it last had once its id names nothing.
--
-- The reader CR 702.2e, CR 702.15c and CR 702.90d ask for. All three carry the
-- same sentence -- "If an object is no longer in the zone it's expected to be in
-- as an effect causes it to deal damage, its last known information is used to
-- determine whether it had [deathtouch / lifelink / infect]" -- and rule 702.164
-- has no such clause, so toxic rides this by uniformity, exactly as it rides the
-- deal-time capture itself (Pawl.Engine.Damage.damageEvent). Falling back to the
-- WHOLE keyword map rather than to one keyword is what keeps the four answers
-- from drifting.
keywordsWithLastKnown :: ObjectId -> GameState -> Map Keyword Natural
keywordsWithLastKnown oid gs = case lastKnownOf oid gs of
  Just lk -> PC.keywords (LastKnown.characteristics lk)
  Nothing -> keywordsOf oid gs

-- controllerOf with the same fallback. CR 702.15b is why a controller is wanted
-- at all -- its answer is "that source's controller, or its owner if it has no
-- controller" -- but the authority for taking it from last known information is
-- CR 608.2h, NOT CR 702.15c: that rule licenses last known information for
-- "whether it had lifelink" and says nothing about who controlled it. CR 608.2h
-- is the general clause that does ("If the effect requires information from a
-- specific object ... the effect uses the object's last known information"), and
-- viewWithLastKnown above already cites it for exactly this reason.
--
-- LastKnown.controller is a PlayerId rather than a Maybe -- CR 613.1b control is
-- recorded for the object as it left, which is why that record keeps it
-- separately from the characteristics CR 109.3 says it is not among -- so this
-- answers Just wherever the live reader would have answered Nothing for a gone
-- source. Nothing survives only for an id nothing was ever filed under.
controllerWithLastKnown :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerWithLastKnown oid gs = case lastKnownOf oid gs of
  Just lk -> Just (LastKnown.controller lk)
  Nothing -> controllerOf oid gs

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
-- subtypes from the type line, colours from printedColorsOf with devoid applied on
-- top, and power/controller are Nothing (a card in a library has neither under the
-- rules that matter here). This is what lets a Filter read an object's colour
-- outside the battlefield without a projection that does not exist there.
viewOfCard :: Card.Type.Card -> Filter.View
viewOfCard card =
  let typeLine = Card.Type.typeLine card
   in Filter.MkView
        { Filter.cardTypes = TypeLine.types typeLine,
          Filter.supertypes = TypeLine.supertypes typeLine,
          -- CR 604.3 / 702.114a: a characteristic-defining ability functions in
          -- ALL zones, and nothing off the battlefield is projected (viewUpTo
          -- falls back here, #160) -- so devoid is applied here rather than
          -- inherited from a fold this object never enters.
          Filter.colors =
            if definesColorless (Card.Type.keywords card)
              then Set.empty
              else printedColorsOf card,
          Filter.subtypes = TypeLine.subtypes typeLine,
          -- CR 702: read straight off the printed card, like the type line above
          -- it and for the same reason -- no projection exists off the
          -- battlefield, so a search that asks about a keyword is answered from
          -- the printing. NOT typecycling, which was the example here and cannot
          -- be one: CR 702.29e's "[type]" is "any card type, subtype, supertype,
          -- or combination thereof", and a keyword is none of those.
          Filter.keywords = Card.Type.keywords card,
          Filter.power = Nothing,
          Filter.controller = Nothing,
          -- A printed card off the battlefield is not an object, so it has no
          -- identity for IsSource to compare -- the same vacuous posture power
          -- and controller already take here.
          Filter.identity = Nothing,
          Filter.playerIdentity = Nothing,
          -- CR 506.3: only a creature can attack, and a card in a library or hand
          -- is not one -- so it has no combat status either.
          Filter.attacking = False,
          -- CR 509.1a: nor can it block, for the same reason.
          Filter.blocking = False,
          -- And it was never on the battlefield to be declared as one, so the
          -- turn's event log holds nothing about it either.
          Filter.attackedThisTurn = False,
          -- CR 303.4b: only a permanent on the battlefield is attached to
          -- anything, and a printed card off it is not one.
          Filter.attachedToCreature = False,
          -- CR 303.4 again: nor is it attached to a permanent, for the same reason.
          Filter.attachedToPermanent = False,
          -- CR 701.3a: only Pawl.Engine.Resolve's AttachTarget arm fills this field, and
          -- its candidates are battlefield permanents, so a card in a library or a
          -- hand is never asked whether an attach could land on it.
          Filter.canHostSubject = False,
          -- CR 111.6: "A token isn't a card." This builder describes a card in a
          -- zone the battlefield is not (a library search, viewUpTo's fallback),
          -- and CR 704.5d already made a token in any such zone cease to exist --
          -- so no token can reach here, and False is not a lost distinction.
          Filter.token = False
        }

-- Shared assembly: fill a View from a projection's characteristics plus the
-- printed supertypes (not projected) and a supplied controller.
-- CR 508.3a: does this event record THIS object being declared as an attacker?
-- Only Combat.declareAttackers appends one, which is what keeps CR 508.4's
-- creature put onto the battlefield attacking -- one that "never attacked" --
-- out of the answer.
declaredIt :: ObjectId -> GameEvent.GameEvent -> Bool
declaredIt oid event = case event of
  GameEvent.AttackerDeclared declared -> declared == oid
  _ -> False

viewOfCharacteristics :: ObjectId -> ProjectedCharacteristics -> Maybe PlayerId.PlayerId -> GameState -> Filter.View
viewOfCharacteristics oid pc controller gs =
  Filter.MkView
    { Filter.cardTypes = PC.cardTypes pc,
      Filter.supertypes = printedSupertypes oid gs,
      Filter.colors = PC.colors pc,
      Filter.subtypes = PC.subtypes pc,
      -- CR 109.3 / 613.1f: abilities are characteristics and layer 6 writes them,
      -- so this comes off the PROJECTION alongside cardTypes and colors -- a
      -- creature that gained flying matches, and one under Humility does not.
      -- Map.keysSet because PC.keywords counts instances (CR 702) and
      -- Filter.HasKeyword asks only membership.
      Filter.keywords = Map.keysSet (PC.keywords pc),
      Filter.power = PC.power pc,
      Filter.controller = controller,
      Filter.identity = Just oid,
      Filter.playerIdentity = Nothing,
      -- CR 508.1k: attacking is a combat STATUS, not a characteristic (CR 109.3),
      -- so it comes straight off the combat record and not from the projection
      -- this function is otherwise assembling.
      Filter.attacking = Map.member oid (Combat.attackers (GameState.combat gs)),
      -- CR 509.1g: likewise a combat status and not a characteristic, read off
      -- the OTHER map. Combat.blockers is keyed by ATTACKER, so being a blocking
      -- creature is membership in some attacker's set rather than a key lookup --
      -- and deliberately not Map.member, which is Pawl.Engine.Combat.isBlocked's
      -- question about the attacker (CR 509.1h).
      Filter.blocking = any (Set.member oid) (Map.elems (Combat.blockers (GameState.combat gs))),
      -- CR 608.2i: a look-back read of the turn's event log rather than of the
      -- combat record, because the record does not survive the phase -- CR 511.3
      -- removes every creature from combat as the end of combat step ends, and
      -- Combat.clearCombat is what does it. The log outlives that and is cleared
      -- at turn handoff, which is the span "this turn" names.
      Filter.attackedThisTurn = any (declaredIt oid) (GameState.events gs),
      -- CR 701.3a: likewise not a characteristic (CR 109.3 names "what an Aura
      -- enchants" among the things that are not one), so the attachment comes
      -- off Object.attachedTo. The HOST's creature-ness, though, is projected --
      -- CR 613 layer 4 can make a land a creature -- so it goes through
      -- isCreatureOf, which is a projection OF ANOTHER OBJECT.
      --
      -- That is why this field must stay lazy: `affects` calls this function
      -- from inside a projection, and forcing a second projection there would
      -- recurse. See Pawl.Engine.Filter.View's own note on the field.
      -- A player host answers False without a projection at all: CR 701.3a's
      -- question is whether the attachment is to a CREATURE, and a player is not
      -- one -- Recipient.objectOf's Nothing is that answer.
      Filter.attachedToCreature = case Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf of
        Nothing -> False
        Just host -> isCreatureOf host gs,
      -- CR 303.4: the same stored attachment, asked a question that stops short of
      -- the host's characteristics -- does the attachment name an object that is
      -- on the battlefield, which CR 110.1 is the definition of a permanent, or a
      -- player? Recipient.objectOf splits the two, and the battlefield membership
      -- is what rules out a stale attachment to a host that has already left:
      -- CR 704.5m buries such an Aura, but only on the next pass, and until then
      -- it is attached to nothing that exists. Unlike the field above there is no
      -- projection OF ANOTHER OBJECT behind this, so it carries no recursion
      -- hazard and needs no laziness argument.
      Filter.attachedToPermanent = case Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf of
        Nothing -> False
        Just host -> Set.member host (GameState.battlefield gs),
      -- CR 701.3a: filled only by Pawl.Engine.Resolve's AttachTarget arm, which is the
      -- one place that knows what is being moved. Every view this function builds
      -- is asked about some candidate in isolation, and "could the subject be
      -- attached here" is not a question about the candidate alone.
      Filter.canHostSubject = False,
      -- CR 111.6: not a characteristic either, and unlike the two fields above it
      -- is not even mutable -- Object.source is fixed for the life of the object
      -- (CR 400.7 mints a new one on every zone change), so this is a constant
      -- input to the projection being assembled and costs it nothing.
      Filter.token = Game.isToken oid gs
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

-- Printed characteristics before any effect: CR 613.1's starting point, "the
-- values of the characteristics printed on that card". NOT CR 613.2/613.4, which
-- order the SUBLAYERS within layers 1 and 7 and say nothing about where the fold
-- begins.
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.cardOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { -- No card behind this object (an ability or trigger on the stack): it has
        -- no printed name and no type line to seed from.
        PC.name = CardName.MkCardName Text.empty,
        PC.supertypes = Set.empty,
        PC.keywords = Map.empty,
        PC.colors = Set.empty,
        PC.power = Nothing,
        PC.toughness = Nothing,
        PC.loyalty = Nothing,
        PC.characteristicPT = Nothing,
        PC.cardTypes = Set.empty,
        PC.subtypes = Set.empty,
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
          { PC.name = Card.Type.name card,
            PC.supertypes = TypeLine.supertypes (Card.Type.typeLine card),
            -- CR 702: a printed keyword appears once in the card's text, so the
            -- seed's count is 1 apiece. Multiplicity is what layer-6 grants add
            -- on top (CR 702.164b).
            PC.keywords = Map.fromSet (const 1) (Card.Type.keywords card),
            PC.colors = printedColorsOf card,
            -- Quantity.evaluate, not Quantity.determine: CR 208.2a's "use 0
            -- instead" belongs to a characteristic-defining ability, and a
            -- printed Star with no CDA behind it has none, so it evaluates to
            -- Nothing here. Primal Plasma (P5) is the pool's one such card --
            -- its star is given a value by an as-enters REPLACEMENT (CR 208.2b),
            -- not by a CDA -- so it projects no power or toughness until that
            -- entry choice applies. Unobservable on the battlefield, where the
            -- entry loop always applies the choice before the Moved event
            -- exists, but a Primal Plasma CARD in a hand, library or graveyard
            -- reports Nothing where CR 208.2b says 0 (#76). A star that DOES
            -- have a CDA behind it is Nothing here too, and stays Nothing only
            -- until layer 7a: seedCharacteristicPT put the substituted pair in
            -- PC.characteristicPT, and applyCharacteristicPT determines it there.
            PC.power = case Card.Type.power card of
              Nothing -> Nothing
              Just (Power.MkPower q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
            PC.toughness = case Card.Type.toughness card of
              Nothing -> Nothing
              Just (Toughness.MkToughness q) -> Quantity.evaluate seedViewOf seedContext gs oid q,
            -- CR 306.5a: a literal number, so it is copied through rather than
            -- evaluated the way the two Quantity-valued fields above are.
            PC.loyalty = Card.Type.loyalty card,
            PC.characteristicPT = seedCharacteristicPT card,
            PC.cardTypes = TypeLine.types (Card.Type.typeLine card),
            PC.subtypes = TypeLine.subtypes (Card.Type.typeLine card),
            PC.activatedAbilities = Card.Type.activatedAbilities card,
            PC.replacementEffects = Card.Type.replacementEffects card,
            PC.triggeredAbilities = Card.Type.triggeredAbilities card
          }

-- CR 202.2 / 204.2: an object's PRINTED colours -- the colours of the coloured
-- mana symbols in its mana cost, together with the colours its colour indicator
-- denotes. CR 202.2b: an object with no coloured mana symbols and no indicator is
-- colourless.
--
-- No devoid here. CR 702.114a makes devoid a CHARACTERISTIC-DEFINING ability, and
-- CR 613.3 puts characteristic-defining abilities at the START of their layer --
-- layer 5 for colour (CR 613.1e) -- not before the fold begins. applyColorDefining
-- is where it lands; see projectWith.
printedColorsOf :: Card.Type.Card -> Set Color.Color
printedColorsOf card =
  Set.union
    (Card.Type.colorIndicator card)
    (manaCostColors (Card.Type.manaCost card))

-- CR 702.114a: "Devoid is a characteristic-defining ability. 'Devoid' means
-- 'This object is colorless.'" THE one place that decides what devoid means, so
-- the fold and the off-battlefield card view cannot drift apart on it.
definesColorless :: Set Keyword -> Bool
definesColorless = Set.member Keyword.Devoid

-- CR 613.3 / 613.1e: the object's own colour-defining ability, applied at the
-- START of layer 5 -- "within layers 2-6, apply effects from characteristic-
-- defining abilities first, then all other effects in timestamp order".
--
-- Folded IN PLACE rather than emitted as a synthetic Gathered, for
-- applyCharacteristicPT's three reasons, which transfer word for word: a CDA
-- affects only the object it is on (CR 604.3a(3)) so it has no affected set to
-- gather over; CR 604.3 makes it function in ALL zones while gather walks the
-- battlefield only; and it has no source object and no timestamp, so it has
-- nothing to sort on under CR 613.7.
--
-- Read from the PARTIAL projection's keywords rather than from the card. At
-- layer 5 that map holds the printed keywords, those a copy effect brought in at
-- layer 1, and those a text-changing effect wrote at layer 3 -- and it cannot yet
-- hold a layer-6 grant, because layer 6 has not been applied. That is exactly CR
-- 604.3a(2)'s list of what makes a static ability characteristic-defining, so the
-- rule holds by construction rather than by a test.
--
-- Humility therefore cannot remove it: LoseAllAbilities is layer 6, after this.
--
-- Not implemented: a devoid GRANTED by a layer-6 effect does nothing to colour.
-- Per CR 604.3a(2) such a grant is not characteristic-defining, so it would be an
-- ordinary layer-5 colour effect timestamped when granted (CR 613.7a), which this
-- does not build (#622).
applyColorDefining :: ProjectedCharacteristics -> ProjectedCharacteristics
applyColorDefining pc =
  if definesColorless (Map.keysSet (PC.keywords pc))
    then pc {PC.colors = Set.empty}
    else pc

-- CR 202.1b: a land has no mana cost at all, so it contributes no colours.
manaCostColors :: Maybe ManaCost.ManaCost -> Set Color.Color
manaCostColors mc = case mc of
  Nothing -> Set.empty
  Just (ManaCost.MkManaCost symbols) -> Set.fromList (concatMap symbolColors symbols)

-- CR 202.2b: only a coloured mana symbol carries a colour ("Objects with no
-- colored mana symbols in their mana costs are colorless"). Generic ({2}), {X},
-- and the colourless symbol ({C}) carry none -- {C} is colourless mana, and
-- CR 105.2c says colourless is not a colour.
--
-- A LIST, not a Maybe, because of CR 107.4e's last sentence: "A hybrid mana
-- symbol is all of its component colors." Burning-Tree Emissary's {R/G}{R/G}
-- makes it both red and green, not one or the other and not multicoloured-as-a-
-- third-thing (CR 202.2d: "An object with one or more hybrid mana symbols
-- and/or Phyrexian mana symbols in its mana cost is all of the colors of those
-- mana symbols"). NOT CR 202.2c, whose premise is "two or more DIFFERENT colored
-- mana symbols" and so does not reach {R/G}{R/G}'s two identical ones; and not
-- CR 105.3, which is about an EFFECT changing a colour rather than about what a
-- mana cost makes an object.
--
-- CONTRIBUTIONS, not a set: this may repeat a colour, and deduplicating is the
-- caller's job because the caller is the one building a set. Colour is a set
-- property under CR 105.2 ("an object can be one or more of the five colors"), so
-- manaCostColors above unions the whole cost with Set.fromList and a repeat
-- inside one symbol is absorbed there along with the repeat ACROSS symbols that
-- {R/G}{R/G} already produces. Deduplicating here would only cover the second
-- case -- and only for a degenerate `Hybrid t t` that no card prints.
symbolColors :: ManaSymbol.ManaSymbol -> [Color.Color]
symbolColors symbol = case symbol of
  ManaSymbol.OfType (ManaType.Colored c) -> [c]
  ManaSymbol.OfType ManaType.Colorless -> []
  ManaSymbol.Hybrid a b -> Maybe.mapMaybe colorOfManaType [a, b]
  -- CR 107.4e's last sentence again. A monocolored hybrid's other component is
  -- generic mana -- CR 107.4b, a numerical symbol, which is not one of CR 107.4a's
  -- five coloured mana symbols and so contributes no colour (CR 202.2a's own
  -- example: "an object with a mana cost of {2} is colorless"). The named half is
  -- therefore the whole contribution: Flame Javelin ({2/R}{2/R}{2/R}) is red and
  -- only red, however it was paid for.
  ManaSymbol.MonocoloredHybrid t -> Maybe.maybeToList (colorOfManaType t)
  -- CR 107.4f's first clause: "Phyrexian mana symbols are COLORED mana symbols:
  -- {W/P} is white, ... and {G/P} is green" -- and CR 202.2d says the object is
  -- that colour, alongside the hybrids: "An object with one or more hybrid mana
  -- symbols and/or Phyrexian mana symbols in its mana cost is all of the colors
  -- of those mana symbols, in addition to any other colors the object might be."
  --
  -- A total `[c]` rather than a mapMaybe, because ManaSymbol.Phyrexian carries a
  -- Color and not a ManaType: there is no colourless Phyrexian symbol. The other
  -- half of the symbol is LIFE, which is no mana at all and so no colour either,
  -- exactly as a monocolored hybrid's generic half is none. Mutagenic Growth is
  -- green even when 2 life paid for it and no green mana was ever made -- proved
  -- by ManaSpec's "Mutagenic Growth is green on the stack even when 2 life paid
  -- for it".
  ManaSymbol.Phyrexian c -> [c]
  -- CR 107.4h's last sentence, which settles this outright: "Snow is neither a
  -- color nor a type of mana." CR 202.2d's colour-granting list names the hybrid
  -- and Phyrexian symbols and not this one, so Icehide Golem is colorless (CR
  -- 202.2b) despite having a mana cost.
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

-- CR 608.2h / 611.2d: evaluate a modification's quantities ONCE and rewrite them
-- to Literals. Called by Resolve when a spell's resolution STORES a continuous
-- effect -- "if an effect requires information from the game ... the answer is
-- determined only once, when the effect is applied."
--
-- `oid` is the SOURCE (the resolving spell), not the affected object: for a
-- spell that object is also the one CR 601.2b's chosen X was stamped on, and
-- `you` is its controller, whose hand a player-scoped count counts.
--
-- Not the announcing object for an ACTIVATED ability, where the two differ
-- (Quantity.evaluateFor); an X-cost activation that stored a continuous effect
-- measured by its X would freeze nothing here. No card in the pool does (#550).
--
-- Deliberately NOT applied to a static ability's effect: CR 611.2 scopes 611.2a-d
-- to "a continuous effect generated by the resolution of a spell or ability", and
-- a static ability's effect (CR 604.2) is regenerated every projection and
-- evaluated per affected object -- Opalescence's mana value must keep moving.
--
-- Nothing when ANY quantity it carries cannot be evaluated at store time. CR
-- 608.2h/611.2d have the answer determined "only once, when the effect is
-- applied" -- so a value that cannot be determined at that one moment cannot be
-- determined later either, and re-reading it live against the affected object on
-- every projection would be a WRONG answer rather than a deferred one. Resolve
-- stores nothing in that case, the same posture CR 611.2b already gives this
-- opcode for a duration that never starts. Untamed Might's "+X/+X" is the pool's
-- producer, and ProjectionSpec's "CR 608.2h/611.2d Untamed Might's X is frozen"
-- is what proves the freeze happens at all.
--
-- Not Literal 0, which would be inventing an answer: CR 208.2a's "if the ability
-- needs to use a number that can't be determined ... use 0 instead of that
-- number" is scoped to a characteristic-defining ability, and this is not one.
--
-- Cases on Modification, so it lives HERE (Projection is the sole home), the same
-- standing rewriteModification has.
freezeQuantities :: GameState -> ObjectId -> Maybe PlayerId.PlayerId -> Modification -> Maybe Modification
freezeQuantities gs oid you m =
  -- CR 608.2h / 611.2d: read the CURRENT state through the real projection --
  -- `oid` is the source, `you` its controller, matching the doc above.
  let viewOf = fullView gs
      context = Filter.MkContext you (Just oid)
      freeze q = fmap Quantity.Type.Literal (Quantity.evaluate viewOf context gs oid q)
   in case m of
        Modification.SetBasePowerToughness p t -> Modification.SetBasePowerToughness <$> freeze p <*> freeze t
        Modification.ModifyPowerToughness p t -> Modification.ModifyPowerToughness <$> freeze p <*> freeze t
        -- Every other modification carries no quantity to freeze, so it stores as
        -- written; named explicitly per Modification's exhaustiveness discipline.
        Modification.GainKeyword _ -> Just m
        Modification.LoseAllAbilities -> Just m
        Modification.SetLandSubtype _ -> Just m
        Modification.AddLandSubtype _ -> Just m
        Modification.SetCreatureSubtype _ -> Just m
        Modification.AddCardType _ -> Just m
        Modification.ChangeSubtypeWord _ _ -> Just m
        Modification.SetController _ -> Just m
        Modification.SetControllerToSource -> Just m
        Modification.SetColor _ -> Just m
        Modification.AddColor _ -> Just m
        Modification.SwitchPowerToughness -> Just m

-- Every Quantity a modification carries, in the order it carries them. Another
-- case on Modification, so it lives HERE for freezeQuantities' reason, even
-- though its caller is elsewhere: Resolve's D4 lints have to see INSIDE a
-- ModifyTarget's modification to find the X in Untamed Might's "+X/+X" and any
-- slot a future card reads there.
--
-- NOTE: when a Modification gains a Quantity field, add it here as well as to
-- freezeQuantities -- the compiler forces the arm to exist, not to be right.
quantitiesOf :: Modification -> [Quantity.Type.Quantity]
quantitiesOf m = case m of
  Modification.SetBasePowerToughness p t -> [p, t]
  Modification.ModifyPowerToughness p t -> [p, t]
  Modification.GainKeyword _ -> []
  Modification.LoseAllAbilities -> []
  Modification.SetLandSubtype _ -> []
  Modification.AddLandSubtype _ -> []
  Modification.SetCreatureSubtype _ -> []
  Modification.AddCardType _ -> []
  Modification.ChangeSubtypeWord _ _ -> []
  Modification.SetController _ -> []
  Modification.SetControllerToSource -> []
  Modification.SetColor _ -> []
  Modification.AddColor _ -> []
  Modification.SwitchPowerToughness -> []

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
        -- The OTHER subtype set, and the arm most at risk of being read as this
        -- one. CR 305.7's ability strip is a rule about a LAND whose subtype is
        -- set; CR 205.1a/205.1b's creature-type set carries no such clause, so a
        -- Turn to Frog must not take its target's rules text. The existing
        -- wildcard already covers it, but the confusion is worth naming.
        Modification.SetCreatureSubtype _ -> False
        _ -> False
      fromStored eff =
        if isSet (ContinuousEffect.modification eff)
          then [(ContinuousEffect.source eff, ContinuousEffect.affected eff)]
          else []
      -- The affected set is read UNREWRITTEN here, where gatherStatic applies CR
      -- 612's word swap to the same ability's (#402). So a text-changed source
      -- would have this gate and the layer fold disagreeing about which
      -- permanents it names. Unreachable: the pool's only SetLandSubtype is
      -- Blood Moon, which selects by card type and supertype and so carries no
      -- land-type word for a swap to reach. Rewriting here is not free either --
      -- textChangesAffecting folds the whole effect list, and this function is
      -- hoisted out of gather's walk precisely to avoid per-permanent cost
      -- (#584).
      fromPerm permId = case Game.cardOf permId gs of
        Nothing -> []
        Just card ->
          fmap (\sa -> (permId, StaticAbility.affected sa)) $
            filter (any isSet . StaticAbility.modifications) (Card.Type.staticAbilities card)
   in concatMap fromStored (GameState.continuousEffects gs)
        <> concatMap fromPerm (Set.toList (GameState.battlefield gs))

-- CR 305.7: a land whose subtype is SET to a basic type loses its rules-text
-- abilities. This is the GATE half of that rule, shared by all three readers whose
-- ability lands on objects other than the bearer -- gather here,
-- Pawl.Engine.PlayerEffect.applying and Pawl.Engine.BlockRequirement.instances -- since such an
-- ability has to be kept out of its reader's candidate list rather than erased
-- from the bearer's own projection afterwards. Every other kind of rules-text
-- ability is stripped inside the fold instead, by applyModification's
-- SetLandSubtype arm. So an object's static abilities are
-- live unless a live SetLandSubtype applies to it. "Live" recurses on the
-- stripper's own source; "applies to" reads BASE characteristics (nonbasic is a
-- printed supertype, and card-type Land is read off the printed type line here),
-- so nothing recurses into the projection and the result is order-INDEPENDENT. A
-- cycle trips the visited set (both treated as live -- the CR 613.8b loop-escape
-- analog, not an implementation of it, #37).
--
-- The base read is a RESTRICTION, not merely a shortcut, and the two halves of
-- CR 305.7 therefore disagree about their affected set: a permanent that becomes
-- a land only through a layer-4 type change is not seen HERE at all, while the
-- fold's arm reads the projection and does reach it. Ashaya, Soul of the Wild is
-- the card that does that. Blood Moon takes her characteristic-defining P/T, her
-- activated, triggered and replacement abilities and her keywords -- all of which
-- the arm reaches -- and leaves her own animating STATIC ability standing, which
-- only this gate could have taken (#391).
--
-- Ashaya is NOT the dependency loop #37 waits for, and the way she misses is
-- worth recording so the next reader does not re-derive it. A loop needs each
-- effect to change the other's existence; here only one direction holds. Blood
-- Moon depends on Ashaya (CR 613.8a clause (b): applying Ashaya's type change
-- puts alice's creatures INTO "nonbasic lands"), and Ashaya does not depend on
-- Blood Moon, because CR 613.8a asks what applying the other would change FROM
-- THE CURRENT STATE -- CR 613.8c re-asks after each application -- and in that
-- state Ashaya is a Legendary Creature -- Elemental that no subtype-setting
-- effect reaches. Pawl.ProjectionSpec's Ashaya + Blood Moon pair proves that
-- one-way half in both timestamp orders.
--
-- So no board in the pool makes the escape's ANSWER observable. The visited
-- branch is taken on every such board -- Blood Moon asking whether Blood Moon
-- strips Blood Moon -- but what it returns is then thrown away by an affectsBase
-- that is False for an enchantment.
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
textChangesAffecting :: ObjectId -> GameState -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)]
textChangesAffecting oid gs =
  let pairOf eff = case ContinuousEffect.modification eff of
        Modification.ChangeSubtypeWord from to ->
          if affects (ContinuousEffect.source eff) oid (ContinuousEffect.affected eff) (baseCharacteristics oid gs) gs
            then Just (from, to)
            else Nothing
        _ -> Nothing
   in Maybe.mapMaybe pairOf (GameState.continuousEffects gs)

-- Apply text-changes to a modification's basic-land-type words (CR 612.1/612.2):
-- SetLandSubtype/AddLandSubtype carry a land-type word. SetCreatureSubtype
-- carries a subtype word too and is deliberately NOT rewritten -- see its arm --
-- and every other modification carries none at all. Projection's charter (it cases
-- on Modification); it is delegated to by Resolve.rewriteEffect for the inner
-- modification of ModifyTarget.
rewriteModification :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modification -> Modification
rewriteModification pairs m =
  let swap from to s = if s == from then to else s
      apply1 acc (from, to) = case acc of
        Modification.SetLandSubtype s -> Modification.SetLandSubtype (swap from to s)
        Modification.AddLandSubtype s -> Modification.AddLandSubtype (swap from to s)
        -- Carries a subtype word, and CR 612.2 does contemplate rewriting one --
        -- it names "a creature type word used as a creature type" beside the
        -- land-type case. What cannot reach it is the PAIR: the pool's only
        -- ChangeSubtypeWord producer is Magical Hack, whose text replaces one
        -- basic land type with another and whose pair is answered by
        -- Prompt.ChooseBasicLandTypes. So `from` is never a creature type and a
        -- swap here would be the identity on every board pawl can reach. The
        -- card that would make the difference visible, Artificial Evolution, has
        -- no producer in the pool (#529).
        Modification.SetCreatureSubtype _ -> acc
        -- A control op carries no subtype word for CR 612 to rewrite: identity.
        Modification.SetController _ -> acc
        _ -> acc
   in List.foldl' apply1 m pairs

-- rewriteModification's sibling for the OTHER half of a static ability. CR 612.1:
-- a text-changing effect "can apply to any words or symbols printed on that
-- object, but generally affects only that object's rules text", and an ability's
-- affected clause is rules text like any other -- so a hacked Kormus Bell, whose
-- "All Swamps" is Affected.Matching (HasSubtype Swamp), animates Islands after
-- the swap and stops animating Swamps.
--
-- Delegates to Filter.rewrite, which #395 added for a Filter carried by an
-- EFFECT; this is the same call one level up, and the only thing #402 was
-- missing.
--
-- EXHAUSTIVE over Affected, not a wildcard: the two arms that carry a Filter are
-- the two that could hide a land-type word, and a new arm carrying one must
-- break this build rather than silently keep the old word.
rewriteAffected :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Affected.Affected -> Affected.Affected
rewriteAffected pairs a = case a of
  Affected.Matching f -> Affected.Matching (Filter.rewrite pairs f)
  Affected.AttachedPlayerControls f -> Affected.AttachedPlayerControls (Filter.rewrite pairs f)
  -- A frozen id set names no word (CR 611.2c locks it at resolution), and an
  -- attachment names none either -- both read the SOURCE's own state.
  Affected.TheseObjects _ -> a
  Affected.Attached -> a

-- Every continuous effect in the game: stored resolution effects, plus the
-- static abilities of every battlefield permanent (CR 613.7a: with the
-- permanent's own timestamp), dropping a permanent whose static abilities are
-- not live (CR 305.7 stripped). NOT filtered by object here -- project filters
-- per layer against the partial.
--
-- CR 613.6: the affected set belongs to the EFFECT, not to each of its parts, so
-- the parts of one static ability all carry that ability's key -- which is what
-- lets projectWith decide their set once. A stored effect and a counter are each
-- a single part and carry none.
--
-- TWO ability losses are asked about here, and only about a PERMANENT'S OWN
-- static abilities -- Pawl.Engine.PlayerEffect.applying asks the same pair, about the
-- same permanents, for the CR 613.10/613.11 half of a card's text.
-- CR 305.7's land-subtype strip (liveGiven) drops the permanent
-- outright; CR 613.1f's layer-6 ability removal (abilitiesRemoved) drops only an
-- ability whose every part lands after layer 6 (gatherStatic). Neither gate
-- touches a stored effect or a counter, because neither IS an ability for layer 6
-- to remove: CR 611.2a gives a resolved spell's continuous effect a duration of
-- its own ("lasts as long as stated by the spell or ability creating it ... If no
-- duration is stated, it lasts until the end of the game"), and CR 122.1a/613.4c
-- make a counter's +1/+1 a rule about the counter rather than an ability of the
-- object it sits on. Humility removes neither.
gather :: GameState -> [Gathered]
gather gs =
  let ungated = gatherGiven (const False) gs
   in -- Almost every board has no ability-removing effect at all, and then the
      -- gathered list IS the ungated one -- no second walk of the battlefield's
      -- static abilities and no projection spent on the question. A board that
      -- does have one pays for the stored effects, emblems and counters twice;
      -- none of those three costs a projection, and only the second walk of the
      -- static abilities ever did.
      if any (removesAbilities . gModification) ungated
        then gatherGiven (abilitiesRemoved ungated gs) gs
        else ungated

-- gather's body, with the CR 613.1f gate left open as a parameter: `stripped`
-- answers "were this permanent's abilities removed by the time layer 6
-- finished?" for each battlefield permanent. Called TWICE by gather -- once with
-- the gate wired shut (`const False`) to build the very list the gate reads, and
-- once with the real answer -- and once by abilityRemoval, which needs only the
-- first of those.
gatherGiven :: (ObjectId -> Bool) -> GameState -> [Gathered]
gatherGiven stripped gs =
  let setEffs = setLandSubtypeEffects gs
      -- A stored effect carries exactly one modification (Pawl.Engine.Resolve stores one
      -- per opcode), so CR 613.6 has nothing to hold together here -- and nothing
      -- to get wrong either, since every stored effect's set is the CR 611.2c
      -- TheseObjects one, locked at resolution.
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
                 in -- One thunk per permanent, shared by every one of its abilities
                    -- and forced by none of them unless that ability is entirely
                    -- above layer 6 -- so the projection it costs is paid for at
                    -- most once per permanent, and on almost every board not at all.
                    concat (zipWith (gatherStatic permId (Object.timestamp permObj) changes (stripped permId)) [0 ..] (Card.Type.staticAbilities card))
              else []
      static = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
      fromEmblem emblemId = case Game.lookupObject emblemId gs of
        Nothing -> []
        Just emblemObj -> case Game.cardOf emblemId gs of
          Nothing -> []
          Just card ->
            -- CR 114.4 / 113.6: an emblem's abilities function in the command
            -- zone. Its static ability's continuous effect shares the emblem's
            -- entry timestamp (CR 613.7a). No liveness/text-change pass, and
            -- never stripped: nothing in scope strips an emblem's abilities or
            -- rewrites land types, and CR 613.1f's removers in the pool reach
            -- creatures, which an emblem (CR 114.5, not a permanent) is not.
            concat (zipWith (gatherStatic emblemId (Object.timestamp emblemObj) [] False) [0 ..] (Card.Type.staticAbilities card))
      emblems = concatMap fromEmblem (Set.toList (GameState.command gs))
      counters = counterGathered gs
   in stored <> static <> emblems <> counters

-- CR 613.1f, hoisted over the whole game: "were THIS object's abilities removed
-- by the time layer 6 finished?", as one predicate. For a caller OUTSIDE the
-- layer machine that must ask it once per battlefield permanent
-- (Pawl.Engine.PlayerEffect.applying, for CR 604.2's "and has the ability"), so that the
-- candidate list is built once per read rather than once per permanent -- the
-- same posture setLandSubtypeEffects has for CR 305.7's liveGiven.
--
-- The list is gathered with the layer-6 gate OFF, which is what the gate itself
-- reads, and why this needs an extra pass rather than a fixpoint. Deciding
-- whether a source's abilities were removed means projecting that source up to
-- CR 613.6's decision point (abilitiesRemoved), which is never above layer 6, and
-- a projection bounded below layer 6 applies no candidate at layer 6 or later --
-- so it cannot see, and so cannot be changed by, the layer-7 parts the gate
-- drops.
--
-- WELL-FOUNDED, and this is the whole argument: nothing reachable from here reads
-- a player effect back. The layer machine's only inputs are static abilities,
-- stored continuous effects and counters; a CR 613.10/613.11 effect is a sibling
-- tier applied AFTER that machine has run and is not among them. Pawl.Engine.Projection
-- accordingly does not import Pawl.Engine.PlayerEffect -- the module graph is what
-- enforces it -- so the call is one-way and cannot re-enter.
abilityRemoval :: GameState -> ObjectId -> Bool
abilityRemoval gs =
  let ungated = gatherGiven (const False) gs
   in -- Almost every board has no ability-removing effect at all, and then no
      -- projection is spent on the question at all.
      if any (removesAbilities . gModification) ungated
        then abilitiesRemoved ungated gs
        else const False

-- CR 613.1f: does this modification REMOVE abilities? The layer-6 classification
-- abilitiesRemoved asks for, in the same standing as setLandSubtypeEffects's
-- isSet -- Projection is the sole home of a case on Modification.
-- Total, like modificationWrites: a new ability-removing Modification must break
-- this build rather than silently answer False and reopen #297.
removesAbilities :: Modification -> Bool
removesAbilities m = case m of
  Modification.LoseAllAbilities -> True
  -- CR 613.1f names ability-ADDING effects in the same layer, and adding is not
  -- removing.
  Modification.GainKeyword _ -> False
  -- CR 305.7 strips a land's rules text, which IS an ability loss -- but it is a
  -- layer-4 type change (see `layer`), not a layer-6 removal, and it is performed
  -- separately and earlier: applyModification's own arm empties the ability
  -- fields, and liveGiven keeps the static abilities out of the candidate list.
  -- Answering True here would double-count it into a layer whose ordering it does
  -- not have.
  Modification.SetLandSubtype _ -> False
  -- CR 205.1a/205.1b's creature-type set has no ability clause at all -- CR
  -- 305.7's strip belongs to the LAND arm above, not to setting a subtype -- so
  -- this removes nothing in any layer.
  Modification.SetCreatureSubtype _ -> False
  Modification.SetBasePowerToughness _ _ -> False
  Modification.ModifyPowerToughness _ _ -> False
  Modification.SwitchPowerToughness -> False
  Modification.AddLandSubtype _ -> False
  Modification.ChangeSubtypeWord _ _ -> False
  Modification.AddCardType _ -> False
  Modification.SetColor _ -> False
  Modification.AddColor _ -> False
  Modification.SetController _ -> False
  Modification.SetControllerToSource -> False

-- CR 613.1f / 613.1g: were `oid`'s abilities removed by the time layer 6
-- finished? Layer 6 is applied before layer 7, so an ability removed there
-- generates no layer-7 effect at all, and CR 613.6's rescue ("it will continue to
-- be applied ... even if the ability generating the effect is removed during this
-- process") cannot reach it -- an ability whose only parts are in layer 7 never
-- STARTED to apply in an earlier layer. gatherStatic is where that distinction is
-- drawn; this only answers the removal question.
--
-- The removers are read off the SAME candidate list, which is where CR 613.6's
-- rescue lands instead: an ability-removing effect is itself a layer-6 part, so
-- an ability that carries one is never gated by this and a Humility'd Humility
-- keeps applying its own layer-7b 1/1 (ProjectionSpec's Humility + Opalescence
-- timestamp pair proves it).
--
-- Each remover's affected set is judged at CR 613.6's decision point -- the
-- LOWEST layer its whole effect reaches (gLowest), not at layer 6 -- because "if
-- an effect starts to apply in one layer and/or sublayer, it will continue to be
-- applied to the same set of objects in each other applicable layer and/or
-- sublayer". That is the same answer projectWith's `decided` memo reaches inside
-- the fold, which is the point: the two must not disagree, or the fold strips an
-- object this gate did not.
--
-- Humility cannot tell the two readings apart -- layer 6 IS its lowest layer, so
-- it decides there either way, which is what still lets an Opalescence-animated
-- Rule of Law be inside its "each creature": the animation is layer 4 and the
-- partial already has it. Titania's Song can tell them apart: its one ability
-- pairs a layer-4 type change with the layer-6 removal, and its "each noncreature
-- artifact" set reads the very card type that layer-4 part writes, so judging at
-- layer 6 would find the artifact already animated and miss it (PlayerEffectSpec's
-- Sapphire Medallion case).
--
-- The bound is never above layer 6: a remover carries a layer-6 modification (CR
-- 613.1f), so its effect's lowest layer is at most that. The whole-game argument
-- above -- that a projection bounded below layer 6 cannot see the layer-7 parts
-- the gate drops -- therefore still holds, and holds more strongly the lower the
-- bound goes.
--
-- Grouped by that layer so one projection serves every remover deciding there,
-- and lazily enough that `any` short-circuits before projecting for a layer it
-- never reaches. Almost every board with a remover at all has exactly one.
--
-- STILL an approximation in one place: projectUpTo excludes the whole of the
-- decision layer, while the fold decides an effect's set against the state its
-- SAME-LAYER predecessors have already produced (CR 613.7 timestamp order, CR
-- 613.8 dependency). The two disagree only when another effect in that same layer
-- moves the remover's set. A layer-6 remover CAN now suffer that in principle --
-- GainKeyword and LoseAllAbilities both write the Keywords aspect, so a remover
-- whose affected set named a keyword would be decided here without the same-layer
-- grants -- but no such remover exists: Humility, the pool's only layer-6 one,
-- selects by card type. Titania's Song would suffer it only beside a second
-- layer-4 effect (#510).
--
-- NOT asked of the remover's own source: whether a stripper was itself stripped
-- is a question about ORDER WITHIN layer 6, which the fold settles by CR 613.7
-- timestamp as it applies that layer, not something this gate can restate (#37's
-- neighbourhood -- see the layer-6 grant/Humility timestamp test).
abilitiesRemoved :: [Gathered] -> GameState -> ObjectId -> Bool
abilitiesRemoved cands gs oid =
  let byLowest = Map.fromListWith (<>) [(gLowest c, [c]) | c <- cands, removesAbilities (gModification c)]
      removesAt (lyr, cs) =
        let partial = projectUpTo lyr cands oid gs
         in any (\c -> affects (gSource c) oid (gAffected c) partial gs) cs
   in any removesAt (Map.toList byLowest)

-- One static ability's parts, ready to fold: CR 613.6's unit. `n` is the
-- ability's index on its source, and (src, n) is what every part of a MULTI-part
-- ability carries as its key, so projectWith can tell that a layer-4 part and a
-- layer-7b part are the same effect and must share one affected set. A one-part
-- ability is asked once by construction and carries no key at all.
--
-- Read-point 2 (CR 612): the text-changes affecting the SOURCE rewrite each
-- part's basic-land-type words before the part is folded onto any other object.
-- Hack Blood Moon's SetLandSubtype Mountain -> SetLandSubtype Island.
--
-- `stripped` is CR 613.1f's answer for the SOURCE (abilitiesRemoved): were its
-- abilities removed by the time layer 6 finished? It costs an ability all of its
-- parts, and only when every one of them applies AFTER layer 6 -- CR 613.1g's
-- layer 7 is the only such layer in the vocabulary. Two clauses, both load-
-- bearing:
--
--   * ALL parts after 6, so nothing is retracted that CR 613.6 protects. An
--     ability with a part in layers 1-5 has already started to apply by the time
--     anything removes it, and "will continue to be applied ... even if the
--     ability generating the effect is removed during this process" -- a March of
--     the Machines that Opalescence animated and Humility then stripped keeps
--     setting its artifacts' P/T at 7b, off its layer-4 part.
--   * AFTER 6, not at-or-after. An ability whose own part is IN layer 6 starts to
--     apply in the very layer that removes it, which is the same rescue: Humility
--     animated by Opalescence strips itself and still sets its 1/1 at 7b.
--
-- The whole ability is dropped rather than only its layer-7 parts, which is the
-- same statement: the branch is only taken when every part is a layer-7 one.
gatherStatic :: ObjectId -> Timestamp -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Bool -> Natural -> StaticAbility.StaticAbility -> [Gathered]
gatherStatic src ts changes stripped n sa =
  let ms = fmap (rewriteModification changes) (StaticAbility.modifications sa)
      key = case ms of
        _ NonEmpty.:| (_ : _) -> Just (src, n)
        _ -> Nothing
      -- CR 613.6's decision point, computed once for the whole ability and
      -- copied onto each of its parts. Total: an ability has at least one
      -- modification, so this minimum is over a NonEmpty.
      lowest = minimum (fmap layer ms)
      -- CR 612.1: rewritten for the same reason the modifications above are --
      -- the affected clause is rules text too (#402).
      --
      -- Hoisted out of `one` and short-circuited, both for the same reason: this
      -- runs inside gather, which the SBA sweep reruns at every priority
      -- boundary. Inside `one` it would rebuild the filter once PER PART (three
      -- times for Kormus Bell), and Filter.rewrite walks and rebuilds the whole
      -- tree even for an empty pair list -- unlike rewriteModification, whose
      -- fold over [] is free. An ordinary board has no text change at all, so
      -- the guard is what keeps this off the hot path entirely.
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
   in -- The cheap structural test first, so `stripped`'s projection is forced only
      -- for an ability the rest of the rule could actually reach. "Every part is
      -- after layer 6" and "the lowest layer is after layer 6" are the same
      -- statement.
      if lowest > Layer.Ability && stripped then [] else parts

-- CR 122.1a / 613.4c: a +1/+1 counter adds +1/+1 and a -1/-1 counter adds -1/-1,
-- in layer 7c. Emit each battlefield object's counters as ONE synthetic 7c
-- ModifyPowerToughness with net delta d = (#PlusOnePlusOne - #MinusOneMinusOne) on
-- each axis, folded by the same path as Giant Growth. Constructed HERE (Projection
-- is the sole home that may name a Modification constructor). Layer 7c is purely
-- additive, so pre-combining the counters and the object's own timestamp are both
-- unobservable (spec section 4). d == 0 emits nothing.
--
-- CR 122.1b / 613.1f: a keyword counter grants its keyword instead, which is
-- LAYER 6 and not 7c. Emitted alongside, one grant per counter rather than one
-- per kind: the layer-6 arm counts instances (see applyModification's GainKeyword
-- comment on CR 702.164's lack of a redundancy clause), so two flying counters
-- must arrive as two grants, exactly as two separate GainKeyword effects would.
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
              -- CR 122.1e: a loyalty counter grants nothing. It "indicates how
              -- much loyalty" a planeswalker has, and no CR 613 layer reads
              -- loyalty at all -- CR 704.5i's state-based action and CR 606.6's
              -- activation gate count Object.counters directly instead.
              CounterKind.Loyalty -> []
         in pt <> concatMap grantOf (Map.toList cs)

-- A characteristic a projection holds, at the coarseness CR 613.8a's dependency
-- question needs: applying one effect can only change what another applies to if
-- it WRITES something that one READS. Projection-internal; not a domain type, and
-- deliberately coarser than ProjectedCharacteristics -- "the subtypes changed" is
-- enough to make two effects worth comparing exactly.
data Aspect
  = Types
  | Subtypes
  | Colors
  | -- CR 109.3 counts abilities among an object's characteristics, and CR 613.1f
    -- is the layer that writes them, so a keyword is an aspect exactly as a
    -- subtype is. Coarse like the rest: "the keywords changed" is enough to make
    -- two effects worth comparing exactly.
    Keywords
  | PowerA
  | Controller
  deriving (Eq, Ord)

-- Which aspects a Filter reads. Exhaustive on purpose: a new Filter arm that
-- reads a projected characteristic must be classified here, or CR 613.8a would
-- silently stop seeing dependencies through it.
--
-- Several arms read nothing a modification can write. CR 205.4a supertypes come
-- off the printed type line (printedSupertypes) and nothing projects them;
-- IsSource and IsPlayer ask who the candidate IS, which no effect changes; and
-- IsToken asks what it is REPRESENTED BY, which no effect changes either (its own
-- arm below).
--
-- IsAttacking reads nothing either, and the reason is worth stating because
-- CR 506.4 makes it look otherwise. That rule removes a permanent from combat
-- when its CONTROLLER changes or when it stops being a CREATURE, so an engine
-- that derived attacking-ness from those characteristics would have to read
-- Controller and Types here. pawl does not derive it: attacking-ness is a stored
-- combat record (CR 109.3 is emphatic that it is not a characteristic), CR 506.4
-- is performed by EDITING that record, and the edit happens between projections
-- (Combat.removeChanged, from Engine.settleForPriority) rather than inside one.
-- So the record is a fixed INPUT to any single projection: applying one
-- modification before another cannot change what this arm answers, which is
-- exactly the question CR 613.8a asks. That holds for the TYPES clause too, and
-- it is the one that looks most like a dependency: a modification really can be
-- what makes a permanent stop being a creature, but the removal it causes lands
-- at the next settle, not inside the projection that noticed it. CR 506.4's "an
-- effect specifically removes it from combat" clause does not disturb it either:
-- Effect.RemoveFromCombat edits the same record from Resolve.applyEffect, which
-- is a RESOLUTION and so between projections just as squarely as a settle is. The
-- clauses that remain unbuilt -- phasing (#154), and the ones about an attacked
-- battle (#302) -- arrive by one of those two doors as well. The clauses about an
-- attacked PLANESWALKER take a third door that disturbs this even less: they are
-- answered where the attack target is read (Combat.stillAttacked), so they never
-- edit the record at all, and this arm's input is untouched by them.
--
-- What that costs is TIMING, not dependency: the rules remove the permanent the
-- instant control or creature-ness changes, and pawl removes it at the next
-- settle. That window is argued where the sampling happens. The effect clause has
-- no such window at all, because a resolving effect edits the record on the spot.
filterReads :: Filter.Type.Filter Keyword.Keyword -> Set Aspect
filterReads f = case f of
  Filter.Type.HasCardType _ -> Set.singleton Types
  Filter.Type.HasSupertype _ -> Set.empty
  Filter.Type.HasColor _ -> Set.singleton Colors
  Filter.Type.HasSubtype _ -> Set.singleton Subtypes
  -- CR 613.1f: layer 6 adds and removes abilities, so what this atom answers
  -- moves under the fold exactly as HasCardType's answer moves under layer 4 --
  -- see modificationWrites' three keyword writers below.
  Filter.Type.HasKeyword _ -> Set.singleton Keywords
  Filter.Type.PowerAtLeast _ -> Set.singleton PowerA
  Filter.Type.ControlledBy _ -> Set.singleton Controller
  Filter.Type.IsSource -> Set.empty
  Filter.Type.IsPlayer _ -> Set.empty
  Filter.Type.IsAttacking -> Set.empty
  -- Reads nothing, for IsAttacking's reason and no weaker version of it. The
  -- argument above is about the RECORD, not about attacking-ness in particular,
  -- and Combat.blockers is the same kind of record: written by the CR 509.1
  -- declaration, edited by CR 506.4's removals through Game.removeFromCombat,
  -- and emptied at CR 511.3 -- every one of those between projections rather
  -- than inside one, so it is a fixed input to any single projection too.
  --
  -- Checked rather than copied, because blocking has an input attacking does
  -- not: CR 509.1b's evasion restrictions gate the DECLARATION on projected
  -- characteristics (flying, CR 702.9b). That changes nothing here. The
  -- declaration is a turn-based action (CR 509.1) that writes the record once
  -- and then is over; no projection runs inside it, and re-ordering two
  -- modifications cannot reach back into a declaration that has already
  -- happened.
  Filter.Type.IsBlocking -> Set.empty
  -- Reads nothing, for IsToken's strong reason rather than IsAttacking's. No
  -- Modification writes GameState.events -- CR 608.2i's record is appended by
  -- the change-and-emit funnels and never edited -- so no CR 613 layer can move
  -- a set this atom selects, and CR 613.8a sees no dependency through it.
  Filter.Type.AttackedThisTurn -> Set.empty
  -- Declared as reading Types even though the types it reads are the HOST's, not
  -- the candidate's. Aspect names an aspect of ONE object's projection, so there
  -- is nothing here that can say "another object's card types"; over-declaring
  -- orders a Types-writing effect before an effect whose affected set asks this,
  -- which is the conservative direction. Nothing in the pool puts this atom in an
  -- affected set, so no ordering observable today turns on the choice (#357).
  Filter.Type.IsAttachedToCreature -> Set.singleton Types
  -- Reads nothing, unlike its sibling above, and the difference is exactly why
  -- the two are separate atoms: this one stops at Object.attachedTo and asks
  -- whether the attachment names an object or a player (CR 303.4). No Modification
  -- writes that field -- CR 701.3's attach is a keyword ACTION performed by a
  -- resolution, between projections rather than inside one -- so no CR 613 layer
  -- can move a set this atom selects.
  Filter.Type.IsAttachedToPermanent -> Set.empty
  -- Over-declared, deliberately. The characteristics behind this atom are the
  -- CANDIDATE's (an Equipment needs a creature, CR 301.5) and the SUBJECT's (its
  -- enchant ability, CR 702.5a, whose own Filter can read anything), and Aspect
  -- names aspects of one object's projection, so there is no honest way to say
  -- "and another object's". Declaring everything orders any characteristic-writing
  -- effect before an affected set that asks this, which is the conservative
  -- direction -- and no card in the pool puts this atom in an affected set at all,
  -- because it is a destination filter, so nothing observable turns on the choice.
  -- The same limitation IsAttachedToCreature's arm above records (#357).
  Filter.Type.CanHostSubject -> Set.fromList [Types, Subtypes, Colors, Keywords, PowerA, Controller]
  -- Reads nothing, and for a stronger reason than the three empty arms above:
  -- CR 111.3 makes a token's effect-defined values "functionally equivalent" to
  -- printed ones rather than a separate kind of characteristic, and no
  -- Modification writes Object.source. So no effect can move a "nontoken" set,
  -- and CR 613.8a sees no dependency through this atom -- which is what makes
  -- Ashaya's affected set depend only on card type and controller.
  Filter.Type.IsToken -> Set.empty
  Filter.Type.And fs -> foldMap filterReads fs
  Filter.Type.Or fs -> foldMap filterReads fs
  Filter.Type.Not g -> filterReads g

-- Which aspects a Modification writes -- the other half of the pair above, and
-- another legitimate case-on-Modification that Projection is the sole home of.
--
-- THREE arms write Keywords, and each of them writes PC.keywords in
-- applyModification above: GainKeyword adds one (CR 613.1f), LoseAllAbilities
-- empties the map (CR 613.1f again -- Humility), and SetLandSubtype empties it
-- too, because CR 305.7's second sentence says a land whose subtype is set "loses
-- all abilities generated from its rules text". Filter.HasKeyword reads that map,
-- so all three can move an affected set and CR 613.8a has to see them. Keyword
-- COUNTERS need no arm of their own: counterGathered mints CounterKind.Keyword as
-- a synthetic GainKeyword candidate, so it arrives here as one.
--
-- An ability change can also matter to CR 613.8 by changing an effect's
-- EXISTENCE, which is a different clause of CR 613.8a and still lives in
-- staticAbilitiesLive (CR 305.7) rather than here.
modificationWrites :: Modification -> Set Aspect
modificationWrites m = case m of
  Modification.GainKeyword _ -> Set.singleton Keywords
  Modification.LoseAllAbilities -> Set.singleton Keywords
  Modification.SetBasePowerToughness _ _ -> Set.singleton PowerA
  Modification.ModifyPowerToughness _ _ -> Set.singleton PowerA
  Modification.SwitchPowerToughness -> Set.singleton PowerA
  Modification.SetLandSubtype _ -> Set.fromList [Subtypes, Keywords]
  Modification.AddLandSubtype _ -> Set.singleton Subtypes
  Modification.SetCreatureSubtype _ -> Set.singleton Subtypes
  Modification.ChangeSubtypeWord _ _ -> Set.singleton Subtypes
  Modification.AddCardType _ -> Set.singleton Types
  Modification.SetColor _ -> Set.singleton Colors
  Modification.AddColor _ -> Set.singleton Colors
  Modification.SetController _ -> Set.singleton Controller
  Modification.SetControllerToSource -> Set.singleton Controller

-- Could another effect move this one's affected set at all? The structural half
-- of projectWith's movableReads: a set is movable when something a modification
-- writes selects it -- a Matching set's predicate over characteristics, or an
-- AttachedPlayerControls set's controller (CR 613.1b). A TheseObjects set names
-- ids (CR 611.2c) and an Attached one reads its source's attachment off the game
-- state (CR 303.4m), and no modification writes either.
staticallyMovable :: Gathered -> Bool
staticallyMovable c = case gAffected c of
  Affected.Matching _ -> True
  Affected.TheseObjects _ -> False
  Affected.Attached -> False
  -- Movable, unlike Attached: the attachment half is immovable for Attached's
  -- reason, but WHO CONTROLS a candidate is a layer-2 effect's business
  -- (CR 613.1b), so a control change moves this set.
  Affected.AttachedPlayerControls _ -> True

-- CR 613.8's unit is an EFFECT, not a modification: "an effect is said to
-- 'depend on' another", and CR 613.6 calls one ability's modifications "the parts
-- of the effect". Two parts that land in the SAME layer are therefore applied
-- together, with nothing else allowed between them -- so the reorder groups a
-- layer's candidates into effects before it orders them.
--
-- gatherStatic emits one ability's parts contiguously and keys them alike
-- (gEffect), and filtering by layer preserves that order, so adjacency is enough
-- to find a unit. A Nothing key is always a unit of one: it marks a
-- single-modification ability, a stored effect or a counter, none of which has a
-- second part to be separated from.
--
-- Ashaya, Soul of the Wild + Blood Moon is the pair that needs it, and the reason
-- nothing before it did. Ashaya's one ability adds the card type Land AND the
-- subtype Forest; Blood Moon's affected set reads card types only, so it depends
-- (CR 613.8a) on the first part and not on the second. Ordered per modification,
-- an older Blood Moon slots in BETWEEN them and its SetLandSubtype is overwritten
-- by the Forest it was meant to replace, leaving a creature that is a Mountain and
-- a Forest at once. March of the Machines' two AddCardType parts are the pool's
-- other same-layer pair, and never noticed: nothing on its boards is ordered
-- against it.
effectUnits :: [Gathered] -> [NonEmpty.NonEmpty Gathered]
effectUnits =
  let sameEffect a b = case (gEffect a, gEffect b) of
        (Just x, Just y) -> x == y
        _ -> False
   in NonEmpty.groupBy sameEffect

-- CR 613: apply continuous effects layer by layer (only the layers with effects,
-- ascending). Within a layer, CR 613.8's dependency ordering, falling back to CR
-- 613.7 timestamp order where no dependency exists. An effect's affected set is
-- evaluated against the partial projection as it stands when that effect applies.
-- CR 613.8's EXISTENCE dependency is the exception: it is handled by
-- source-liveness rather than by the reorder. design.md section 2.5.
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
-- A fourth reason applies to this CDA and not to applyColorDefining's: this one
-- is DYNAMIC. Devoid's value is a constant, so computing it early would still
-- give the right answer; Tarmogoyf's P/T reads the graveyards' card types, which
-- change over time. Computing it before the fold would freeze the computed NUMBER
-- into Binding.copy at entry (Engine.hs's as-enters drain snapshots
-- copiableCharacteristics), so a Clone of a Tarmogoyf would keep whatever P/T the
-- graveyards held the moment it entered. CR 707.2 makes a copy acquire the values
-- derived from the printed TEXT -- the ability itself -- so the copy has to
-- recompute, and folding here is what makes it.
--
-- Quantity.determine rather than Quantity.evaluate, and a BARE ASSIGNMENT rather
-- than setPT, because of CR 208.2a's last sentence: "If the ability needs to use
-- a number that can't be determined, including inside a calculation, use 0
-- instead of that number." A CDA therefore always produces a number, and setPT's
-- keep-the-base arm is left with no case to handle. The difference is a board
-- difference: Monstrous War-Leech cast with an empty graveyard has no greatest
-- mana value to take, so it is a 0/0 that CR 704.5f buries rather than a creature
-- with no P/T at all that survives. Pawl.PowerToughnessSpec casts it and proves
-- that.
--
-- setPT stays where the other caller is, layer 7b
-- (Modification.SetBasePowerToughness, CR 613.4b), because that is a different
-- rule: a stored effect that sets base P/T is not a characteristic-defining
-- ability, so CR 208.2a does not reach it and a quantity it cannot evaluate
-- determines nothing rather than 0.
--
-- NOTE: CR 208.5, CR 208.2a's sibling -- "If a creature somehow has no value for
-- its power, its power is 0. The same is true for toughness" -- is a rule about
-- the READ POINTS and is still not implemented: powerOf and toughnessOf return
-- Nothing for a creature with no value rather than 0 (#65).
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
          { PC.power = Just (Quantity.determine viewOf context gs oid p),
            PC.toughness = Just (Quantity.determine viewOf context gs oid t)
          }

-- Project one object against a PRECOMBINED candidate list, applying only the
-- layers the predicate admits. CR 613.1 applies layers in order and Layer's
-- derived Ord IS that order, so `(< bound)` is exactly "the layers before this
-- one".
--
-- The bound exists for counting: a Pawl.Types.Count evaluated while layer L is
-- being applied sees its candidates through `< L`, so a count encountered inside
-- THAT fold is applied at some K < L and sees `< K`. The bound strictly
-- decreases and Layer is finite, so the nesting terminates.
--
-- That BOUND is a terminating approximation: a count is exact whenever it reads
-- layers strictly earlier than its consumer's, and under-reads a count over its
-- own layer or later (#157). It is unrelated to the CR 613.8 dependency ordering
-- below, which is about effects rather than counts and is implemented.
projectWith :: (Layer -> Bool) -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
-- Written as candidates-in, then a worker taking the object: everything derived
-- from the CANDIDATE LIST alone -- the layer list, and the CR 613.8 movable-layer
-- set -- is bound before `oid`, so projectAll shares it across the whole board
-- instead of rebuilding it per object.
projectWith admits cands = forObject
  where
    -- Layer 5 and layer 7a are ALWAYS visited, even when no gathered effect
    -- lives there: an object's own characteristic-defining abilities are not
    -- gathered candidates (applyColorDefining, applyCharacteristicPT). For an
    -- object with neither, each extra pass is an identity function over an empty
    -- candidate filter.
    layers = filter admits (Set.toAscList (Set.insert Layer.Color (Set.insert Layer.CharacteristicPT (Set.fromList (fmap gLayer cands)))))
    -- The layers CR 613.8 could reorder anything in: those holding an effect with
    -- a Matching set, the only kind another effect can move. Bound HERE, before
    -- the object, so a whole-board sweep pays for it once for the board rather
    -- than once per object per layer -- and for most boards it is empty, so the
    -- per-layer question becomes a lookup in an empty Set.
    --
    -- Deliberately coarser than the movableReads inside the fold: this skips the
    -- CR 613.6 memo test (per object, and it changes as the fold runs) and the
    -- filter's own aspects. Both only ever turn a True into a False, so this
    -- over-admits -- which costs the general path where the tight one would have
    -- done, and never a different answer.
    movableLayers = Set.fromList (fmap gLayer (filter staticallyMovable cands))
    forObject oid gs =
      let applyLayer (partial, decided) lyr =
            let seeded = case lyr of
                  -- CR 613.3: characteristic-defining abilities first, within
                  -- the layer they define. Colour is layer 5 (CR 613.1e), P/T is
                  -- layer 7a (CR 613.4a).
                  Layer.Color -> applyColorDefining partial
                  Layer.CharacteristicPT -> applyCharacteristicPT lyr cands gs oid partial
                  _ -> partial
                -- CR 613.6: "If an effect starts to apply in one layer and/or
                -- sublayer, it will continue to be applied to the same set of objects
                -- in each other applicable layer." The affected set is therefore asked
                -- ONCE per effect, at the lowest layer that effect reaches, and the
                -- answer is remembered in `decided` for its other layers. It remembers
                -- "no" as faithfully as "yes": an artifact that was ALREADY a creature
                -- is outside March of the Machines' set when March starts to apply, so
                -- it stays outside at 7b and keeps its printed P/T (#233).
                --
                -- Only an effect with parts in more than one layer carries a key
                -- (gEffect); everything else -- every counter, every stored effect,
                -- every single-line static ability -- is Nothing and never touches the
                -- Map.
                appliesTo ds pc c = case gEffect c of
                  Just k | Just answer <- Map.lookup k ds -> answer
                  _ -> affects (gSource c) oid (gAffected c) pc gs
                -- Fold every part of ONE effect that lands in this layer, in the
                -- order the card lists them (CR 613.6: "the parts of the effect").
                -- The parts share a source and an affected set, so the caller asks
                -- applicability once and this only writes.
                applyUnit pc cs = List.foldl' (\p c -> applyModification lyr (gSource c) cands gs oid (gModification c) p) pc (NonEmpty.toList cs)
                -- Apply one effect, recording its decision the first time.
                -- Re-inserting an existing key writes the value it just read, so this
                -- is idempotent rather than a second determination.
                applyOne (pc, ds) cs =
                  let c = NonEmpty.head cs
                      answer = appliesTo ds pc c
                      ds' = case gEffect c of
                        Nothing -> ds
                        Just k -> Map.insert k answer ds
                   in (if answer then applyUnit pc cs else pc, ds')
                -- What could move `c`'s affected set, as the aspects its filter reads
                -- -- or Nothing when nothing can move it at all. Three ways to be
                -- immovable, none of them an optimization of CR 613.8a so much as its
                -- own precondition made cheap to test: a TheseObjects set names ids
                -- (CR 611.2c) and an Attached one reads the source's own attachment off
                -- the game state (CR 303.4m), neither of which any modification writes;
                -- an effect CR 613.6 already decided answers from the memo; and a
                -- filter that reads no projected aspect at all (`And []`, IsSource, a
                -- supertype) has nothing in it to change.
                movableReads ds c = case gEffect c of
                  Just k | Map.member k ds -> Nothing
                  _ -> case gAffected c of
                    Affected.TheseObjects _ -> Nothing
                    Affected.Attached -> Nothing
                    Affected.Matching f ->
                      let aspects = filterReads f
                       in if Set.null aspects then Nothing else Just aspects
                    -- Always movable, whatever the filter reads: the set is
                    -- narrowed by WHO CONTROLS each candidate, and Controller is
                    -- an aspect layer 2 writes (CR 613.1b).
                    Affected.AttachedPlayerControls f -> Just (Set.insert Controller (filterReads f))
                -- CR 613.8b: an effect that depends on another waits for it, and among
                -- the effects waiting on nothing, CR 613.7 timestamp order picks the
                -- next. Re-deriving `ready` each time round IS CR 613.8c ("the order of
                -- remaining effects is reevaluated"), and removing one effect per
                -- pass is what makes it terminate: `pending` is finite and strictly
                -- shorter on every call, and `batch` is never empty, so a choice is
                -- always available to remove.
                --
                -- Applicability is judged HERE, as each effect is applied, rather than
                -- from `seeded`. That is CR 613.8's premise: "applying the other would
                -- change ... what it applies to" describes a state that only exists if
                -- an effect is asked after its predecessor has applied.
                --
                -- When `ready` is empty every remaining effect is waiting on
                -- another, so somewhere in there is a dependency loop, and CR
                -- 613.8b's last sentence says to ignore the rule for it: "the
                -- effects in the dependency loop are applied in timestamp order".
                -- Only the loop's OWN members escape -- an effect that merely
                -- waits on the loop keeps waiting, and gets its turn once the loop
                -- has unwound -- so the fallback is restricted to the effects
                -- that sit on a cycle rather than to everything left.
                --
                -- `pending` holds EFFECTS (effectUnits), not modifications, because
                -- that is CR 613.8's unit -- see effectUnits.
                resolve (pc, ds) pending = case pending of
                  [] -> (pc, ds)
                  _ ->
                    let -- One applicability answer per effect per round, shared by
                        -- the dependency scan rather than recomputed per pair. An
                        -- effect that does not apply changes nothing, so it cannot
                        -- be the `b` of a dependency either. CR 613.6 gives every
                        -- part of an effect the same affected set, so the head part
                        -- answers for the unit.
                        answered = fmap (\(i, cs) -> (i, cs, appliesTo ds pc (NonEmpty.head cs))) pending
                        -- CR 613.8a clause (b), the "what it applies to" half: `a`
                        -- depends on `b` when applying `b` would change whether `a`
                        -- applies. The tentative application is thrown away and only
                        -- the answer kept -- and it is only reached for a pair that
                        -- could interact at all, `b` writing an aspect `a` reads.
                        --
                        -- Clause (c)'s characteristic-defining exclusion needs no test:
                        -- a CDA is never a candidate (applyCharacteristicPT folds it at
                        -- 7a, outside this list), so no pair here is CDA-vs-non-CDA.
                        -- Clause (b)'s "text" and "what it does to" halves are not
                        -- implemented and have no producer; "existence" is handled by
                        -- staticAbilitiesLive. The CR decides all of this over an
                        -- effect's whole affected set and this decides it per projected
                        -- object, which agrees for everything the Filter vocabulary can
                        -- express (#236).
                        --
                        -- `b` is applied WHOLE here, every part of it that lands in
                        -- this layer, for the same reason applyOne does: half an
                        -- effect is not a state CR 613 ever describes.
                        dependsOnOne (i, as, answer) (j, bs, bApplies) =
                          let a = NonEmpty.head as
                           in case movableReads ds a of
                                Nothing -> False
                                Just aspects ->
                                  j /= i
                                    && bApplies
                                    && not (Set.disjoint aspects (foldMap (modificationWrites . gModification) bs))
                                    && appliesTo ds (applyUnit pc bs) a /= answer
                        ready = filter (\a -> not (any (dependsOnOne a) answered)) answered
                        -- The dependency edges, built only when the whole round is
                        -- blocked -- which is the one case that needs to know the
                        -- SHAPE of the tangle rather than merely that there is one.
                        edges = Map.fromList (fmap (\a@(i, _, _) -> (i, fmap (\(j, _, _) -> j) (filter (dependsOnOne a) answered))) answered)
                        -- Everything reachable from `start` by following edges.
                        reach seen queue = case queue of
                          [] -> seen
                          x : xs ->
                            if Set.member x seen
                              then reach seen xs
                              else reach (Set.insert x seen) (Map.findWithDefault [] x edges <> xs)
                        -- On a cycle iff it can reach itself in one step or more.
                        onCycle (i, _, _) = Set.member i (reach Set.empty (Map.findWithDefault [] i edges))
                        batch = case ready of
                          _ : _ -> ready
                          -- `ready` empty means every remaining effect has an
                          -- outgoing edge, and a finite graph where every node has
                          -- one contains a cycle -- so `cyclic` is never empty and
                          -- the fallback to `answered` is unreachable. Written out
                          -- because `minimumBy` is partial and this keeps it total.
                          [] -> case filter onCycle answered of
                            [] -> answered
                            cyclic -> cyclic
                        -- CR 613.7a gives every part of one ability the source
                        -- permanent's timestamp, so the head part's stamp is the
                        -- unit's.
                        (chosen, next, _) = List.minimumBy (Ord.comparing (\(_, cs, _) -> gTimestamp (NonEmpty.head cs))) batch
                     in resolve (applyOne (pc, ds) next) (filter ((/= chosen) . fst) pending)
                -- Is there anything at this layer CR 613.8 could reorder? See
                -- movableLayers above: one Set lookup, almost always in an empty Set.
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
                  then resolve (seeded, decided) (zip [0 :: Int ..] (effectUnits (filter (\c -> gLayer c == lyr) cands)))
                  else
                    -- Nothing here can be moved, so no candidate depends on any other
                    -- (CR 613.8a needs one to change what another applies to) and no
                    -- candidate's answer can change as the layer is applied. CR 613.8
                    -- therefore says nothing, CR 613.7 timestamp order stands, and
                    -- judging applicability against `seeded` gives the same answers as
                    -- judging it one at a time -- so this branch is not a shortcut past
                    -- the rule, it is the rule where the rule is silent. It is also
                    -- almost every layer of almost every projection, which is why it
                    -- keeps the older, tighter fold rather than sharing `resolve`'s.
                    let decided' = List.foldl' remember decided cands
                        applies c = case gEffect c of
                          Nothing -> affects (gSource c) oid (gAffected c) seeded gs
                          Just k -> Map.findWithDefault False k decided'
                        ordered = List.sortOn gTimestamp (filter (\c -> gLayer c == lyr && applies c) cands)
                        step pc c = applyModification lyr (gSource c) cands gs oid (gModification c) pc
                     in (List.foldl' step seeded ordered, decided')
       in fst (List.foldl' applyLayer (copiableCharacteristics oid gs, Map.empty) layers)

-- Project one object against a PRECOMPUTED candidate list. gather is
-- oid-independent, so a whole-board sweep gathers once and folds each object
-- (projectAll) instead of re-gathering per object.
--
-- Layers 5 and 7a are ALWAYS in the layer list, even when no gathered effect
-- lives there: an object's own characteristic-defining abilities are not gathered
-- candidates (see applyColorDefining and applyCharacteristicPT). For an object
-- with no CDA each extra pass is an identity function over an empty candidate
-- filter.
projectFrom :: [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectFrom = projectWith (const True)

-- CR 613.1: a projection bounded to the layers BEFORE `bound` -- the fold a
-- Pawl.Types.Count sees while layer `bound` is being applied. See projectWith's
-- comment for the termination argument this exists to serve.
projectUpTo :: Layer -> [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectUpTo bound = projectWith (< bound)

-- Project every battlefield object against ONE gather: O(gather + P*fold) instead
-- of the O(P*(gather+fold)) of calling project per object. The hot path for SBA
-- sweeps and combat, which query many objects against the same state.
projectAll :: GameState -> Map ObjectId ProjectedCharacteristics
projectAll gs =
  let cands = gather gs
      -- Bound separately, and NOT inlined: projectWith does its candidate-only
      -- work (the layer list, the CR 613.8 movable-layer set) when it is applied
      -- to `cands`, so sharing this partial application shares that work across
      -- every object on the board.
      forObject = projectFrom cands
   in Map.fromSet (\oid -> forObject oid gs) (GameState.battlefield gs)

-- One object's characteristics out of a PRE-PROJECTED board -- the Map projectAll
-- returns -- falling back to a fresh single-object projection for an id the board
-- does not hold. Every `...Given` reader below is this plus one field read, and
-- every plain `...Of` reader is one of those against Map.empty.
--
-- This is not an approximation of `project`, it IS `project`. Where the key
-- exists, projectAll folded the SAME gathered candidate list `project` would
-- rebuild for that object alone (see projectAll), so the two agree by
-- construction. Where it does not, the id is not on the battlefield -- gather
-- walks the battlefield only -- and projecting it here is the same work
-- `project` would have done anyway. Sba.stillLegalEnchant's haddock argues the
-- identical point for the SBA sweep's board.
--
-- The board is a SNAPSHOT of one GameState, so it is the right answer only while
-- that state is the one being read. A caller hoists it inside a single pure pass
-- over one `gs` and must re-project after any change to the state; every hoist
-- in the engine sits in exactly such a pass (Sba.performStateBasedActions,
-- Combat.legalAttackers, Mana.manaSources, Action.legalActions,
-- Target.legalRecipients), and the monadic callers around them take a fresh
-- State.get and so a fresh board.
--
-- Map.empty is the honest way to say "no snapshot": every read then projects for
-- itself, which is the right cost for a lone query and the same answer either
-- way.
--
-- The battlefield test is what keeps the fallback from COSTING anything. The
-- board is built value-strict (Map.fromSet), so touching it at all projects every
-- permanent -- and an id gather could never have reached is not worth that: a
-- spell on the stack matched by a Filter (Target.legalRecipients' Pool.Spells
-- arm) would otherwise force a whole-board projection to learn it is absent.
-- Never a different answer, only a cheaper route to the same one: the board is
-- keyed on GameState.battlefield of this same state, so an off-battlefield id
-- could not have been a key. Map.null short-circuits it for the Map.empty
-- callers, who are every plain reader above.
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
powerOf oid gs = PC.power (project oid gs)

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = PC.toughness (project oid gs)

-- CR 702: an object's keyword abilities after the layer fold, counted per
-- keyword (see ProjectedCharacteristics.keywords for why the count is kept).
-- Most readers want hasKeyword or totalToxic rather than the raw counts.
keywordsOf :: ObjectId -> GameState -> Map Keyword Natural
keywordsOf = keywordsGiven Map.empty

keywordsGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Map Keyword Natural
keywordsGiven pcs oid gs = PC.keywords (projectGiven pcs oid gs)

-- CR 105.2 / 613.1e: an object's colours after the layer fold. The SOLE read
-- point -- the closed half never reads Card.manaCost for colour, the same
-- discipline keywordsOf established at M2a.
colorsOf :: ObjectId -> GameState -> Set Color.Color
colorsOf = colorsGiven Map.empty

colorsGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Color.Color
colorsGiven pcs oid gs = PC.colors (projectGiven pcs oid gs)

-- CR 602 / 613.1f: an object's activated abilities after the layer system, the
-- same projection posture as keywordsOf. A Humility'd creature has none.
abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesOf = abilitiesGiven Map.empty

abilitiesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Type.Card]
abilitiesGiven pcs oid gs = PC.activatedAbilities (projectGiven pcs oid gs)

-- CR 614 / 613 layer 6: an object's replacement effects after the layer system,
-- the same projection posture as abilitiesOf. A Humility'd creature has none.
replacementsOf :: ObjectId -> GameState -> [ReplacementEffect]
replacementsOf oid gs =
  let pc = project oid gs
   in PC.replacementEffects pc <> intrinsicReplacementsOf pc

-- CR 306.5b: "A planeswalker has the intrinsic ability 'This permanent enters
-- with a number of loyalty counters on it equal to its printed loyalty number.'
-- This ability creates a replacement effect (see rule 614.1c)."
--
-- MINTED from the finished projection rather than stored on the card, the posture
-- Pawl.Engine.Mana.subtypeMana takes for CR 305.6's intrinsic mana ability and
-- Pawl.Engine.Keyword.triggeredAbilitiesOf takes for CR 702.70's poisonous. Three
-- consequences, all of them the rules':
--
--   * It is keyed on the PROJECTED card type, so a permanent that became a
--     planeswalker is judged as one and a card that is a planeswalker only on
--     paper is not.
--   * It reads the PROJECTED loyalty, which CR 707.2 makes a copiable value -- so
--     a Clone entering as a copy of a planeswalker enters with the COPY's printed
--     loyalty. That is CR 707.5's "if the text that's being copied includes any
--     abilities that replace the enters-the-battlefield event (such as 'enters
--     with' ...), those abilities will take effect", and it falls out of the mint
--     rather than needing machinery, because the loop re-collects each iteration
--     (CR 616.1f) and finds the newly-stamped loyalty.
--   * Minting AFTER the layer fold puts it out of reach of layer 6's
--     LoseAllAbilities, which empties PC.replacementEffects. That is deliberate
--     and matches the two precedents above: CR 306.5b gives the ability to a
--     planeswalker as a rule, and the card type is what layer 6 would have to take
--     away for it to stop.
--
-- Nothing for a planeswalker with no printed loyalty, which is unrepresentable
-- today anyway: the CardSpec lint holds "planeswalker iff loyalty" in both
-- directions.
intrinsicReplacementsOf :: ProjectedCharacteristics -> [ReplacementEffect]
intrinsicReplacementsOf pc =
  [ -- CR 614.1c: "[THIS PERMANENT] enters with . . .", which is Filter.IsSource
  -- -- the entering object is the ability's own source (see
  -- Pawl.Types.ReplacementEffect on why the pattern is a Filter).
  ReplacementEffect.EntryR Filter.Type.IsSource (EntryRewrite.WithCounters CounterKind.Loyalty n)
  | Set.member CardType.Planeswalker (PC.cardTypes pc),
    Loyalty.MkLoyalty n <- Maybe.maybeToList (PC.loyalty pc)
  ]

-- CR 614.1: every replacement effect active on the battlefield, PAIRED WITH ITS
-- SOURCE -- a ControllerRelation pattern (CR 109.5's "you") is unanswerable
-- without it. CR 614.1 is why there is a live set to collect at all: replacement
-- effects "apply continuously as events happen -- they aren't locked in ahead of
-- time". NOT CR 614.6, which this used to cite and which is about what happens
-- once an event IS replaced -- a ControllerRelation pattern (CR 109.5's "you") is unanswerable
-- without it. Short-circuits when no permanent has one in its base card, so an
-- ordinary zone change (a draw, a land entering) does NOT project the whole
-- board.
--
-- The short-circuit reads BASE cards while the result reads the PROJECTION, which
-- is sound only because the one way to acquire a replacement effect you were not
-- printed with is `EntryR AsCopy` -- and a card with that arm is itself a base
-- card with a replacement effect, so it keeps `baseHas` true for its own object.
--
-- Past the short-circuit this projects per permanent rather than threading one
-- board (projectGiven), so a board holding any replacement effect at all pays a
-- fresh gather for every permanent on it (#435).
replacementsAffecting :: GameState -> [(ObjectId, ReplacementEffect)]
replacementsAffecting gs =
  let onBattlefield = Set.toList (GameState.battlefield gs)
      baseHas oid = case Game.cardOf oid gs of
        Nothing -> False
        -- The planeswalker disjunct is what keeps CR 306.5b's INTRINSIC
        -- replacement inside the short-circuit: it is minted from the projection
        -- and so appears in no base card's list, and without this a board whose
        -- only replacement effect is a planeswalker's loyalty would answer [].
        Just card ->
          not (null (Card.Type.replacementEffects card))
            || Set.member CardType.Planeswalker (TypeLine.types (Card.Type.typeLine card))
      forOne oid = fmap (\re -> (oid, re)) (replacementsOf oid gs)
   in if not (any baseHas onBattlefield)
        then []
        else concatMap forOne onBattlefield

-- CR 603 / 613 layer 6: an object's PRINTED-AND-GRANTED triggered abilities after
-- the layer system, the same projection posture as abilitiesOf. A Humility'd
-- creature has none.
--
-- NOT the whole list: rule 702.70's poisonous is a triggered ability the RULES
-- give an object for holding a keyword, and Pawl.Engine.Keyword.triggeredAbilitiesOf
-- mints those from PC.keywords instead. Pawl.Engine.Event's event scan adds them; a
-- reader that wants every triggered ability an object has must do the same.
-- Deliberately not folded into PC.triggeredAbilities: that field is built DURING
-- the layer fold, while the mint has to read the FINISHED keyword counts, which
-- only exist once the fold is over. Deriving after the fold is also what makes
-- Humility free -- LoseAllAbilities empties PC.keywords, so there is nothing left
-- to mint from.
triggeredAbilitiesOf :: ObjectId -> GameState -> [TriggeredAbility Card.Type.Card]
triggeredAbilitiesOf oid gs = PC.triggeredAbilities (project oid gs)

subtypesOf :: ObjectId -> GameState -> Set Subtype.Type.Subtype
subtypesOf = subtypesGiven Map.empty

subtypesGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Set Subtype.Type.Subtype
subtypesGiven pcs oid gs = PC.subtypes (projectGiven pcs oid gs)

-- CR 201.1 / 707.2: the object's projected name -- a Clone's is the name it
-- copied, not "Clone".
nameOf :: ObjectId -> GameState -> CardName.CardName
nameOf oid = PC.name . project oid

-- CR 205.4: the object's projected supertypes, the sibling of subtypesOf.
supertypesOf :: ObjectId -> GameState -> Set Supertype.Supertype
supertypesOf = supertypesGiven Map.empty

-- The same supertypes against a pre-projected board, the subtypesGiven shape and
-- carrying its reason (#200): Mana.productionTagsGiven asks this of every mana
-- source in a sweep that has already gathered one.
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
-- loyalty removal both ask about. Projected for the same reason isCreatureOf is:
-- CR 613.1d puts card types in layer 4, so an effect that adds or removes the
-- type changes the answer, and the printed type line is the wrong place to ask.
isPlaneswalkerOf :: ObjectId -> GameState -> Bool
isPlaneswalkerOf = isPlaneswalkerGiven Map.empty

isPlaneswalkerGiven :: Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isPlaneswalkerGiven pcs oid gs = Set.member CardType.Planeswalker (cardTypesGiven pcs oid gs)

-- The same question against a PRECOMPUTED candidate list rather than a
-- pre-projected board -- projectFrom's posture instead of projectGiven's. For a
-- caller that asks about a HANDFUL of objects out of a whole battlefield
-- (Combat.removeChanged's combatants), that is the cheaper of the two: one
-- gather and one fold per object asked about, where projectAll would fold every
-- permanent on the board. Same answer either way, by projectAll's own argument.
isCreatureFrom :: [Gathered] -> ObjectId -> GameState -> Bool
isCreatureFrom cands oid gs = Set.member CardType.Creature (PC.cardTypes (projectFrom cands oid gs))

-- Membership, which DISCARDS the count -- and is exactly right for every
-- keyword whose multiple instances the rules call redundant (CR 702.3c
-- defender, CR 702.9c flying). A keyword that stacks is asked about with its
-- own reader instead; totalToxic just below is the first.
hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword = hasKeywordGiven Map.empty

hasKeywordGiven :: Map ObjectId ProjectedCharacteristics -> Keyword -> ObjectId -> GameState -> Bool
hasKeywordGiven pcs keyword oid gs = Map.member keyword (keywordsGiven pcs oid gs)

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
totalToxic oid gs = toxicIn (keywordsOf oid gs)

-- totalToxic's fold, over a keyword map the caller already has. Split out so a
-- reader that has taken the map through a different route -- CR 608.2h's
-- last-known fallback, which Pawl.Engine.Damage.damageEvent needs -- sums it the
-- same way instead of restating rule 702.164b's arithmetic.
toxicIn :: Map Keyword Natural -> Natural
toxicIn keywords =
  let value keyword count = case keyword of
        Keyword.Toxic n -> n * count
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
-- Layer 6/Humility is invisible to this fold -- a control-granting static ability
-- stripped by LoseAllAbilities still appears here -- and CR 613.1 says that is the
-- right answer, not a shortcut. Control-changing effects are applied in layer 2
-- (CR 613.1b) and ability-removing effects in layer 6 (CR 613.1f), so the grant
-- has already been made by the time anything strips the ability that made it. No
-- reordering can reach across that: CR 613.8a scopes dependency to effects
-- "applied in the same layer (and, if applicable, sublayer)", and CR 613.6 keeps
-- an effect applying "even if the ability generating the effect is removed during
-- this process". ProjectionSpec's "CR 613.1b before CR 613.1f" tests prove it.
--
-- That covers REMOVAL, which is the only half that exists: the layer-6 vocabulary
-- is LoseAllAbilities and GainKeyword, and neither can put a control-granting
-- STATIC ability on an object, so there is nothing added at layer 6 for this walk
-- to be missing either.
--
-- The same blindness would be a defect one layer further down, where the order
-- flips: an ability removed at layer 6 generates no layer-7 effect, which is a
-- question `gather` DOES ask (abilitiesRemoved). This walk stays blind on purpose
-- because layer 2 is on the other side of layer 6, not because the question is
-- unanswerable.
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
-- every stored ContinuousEffect Pawl.Engine.Resolve constructs carries
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
-- Parameterized by the source because Affected.Attached is a question about the
-- SOURCE's state, and the stored and derived paths carry different sources.
--
-- Deliberately a total case with no wildcard, and two of its four arms are
-- empty: this fold must not project (see controlGrants), which rules out
-- Matching, and AttachedPlayerControls would re-enter controllerOf, which is the
-- fold that reads this. No card produces either (#195).
controlNames :: GameState -> ObjectId -> Affected.Affected -> Set ObjectId
controlNames gs source a = case a of
  Affected.TheseObjects s -> s
  -- CR 303.4m: the source's own attachment. No projection needed, which is what
  -- keeps the fold reading this lean.
  Affected.Attached -> maybe Set.empty Set.singleton (Game.lookupObject source gs >>= Object.attachedTo >>= Recipient.objectOf)
  Affected.Matching _ -> Set.empty
  Affected.AttachedPlayerControls _ -> Set.empty

-- CR 603.3a: every object whose controller a CR 613.1b layer-2 effect currently
-- OVERRIDES, and who it says controls it -- the sample Event.recordEvent takes
-- so that a trigger scanned at the CR 117.5 boundary can still be credited to
-- whoever controlled its source at the event. See GameState.controlWhenTriggered
-- for why the objects NOT named here need no entry.
--
-- Both of controllerOfGiven's two setter sources are enumerated, in the same
-- order it consults them: the stored continuous effects (Effect.GainControl's
-- baked SetController) and the control-granting static abilities (Control
-- Magic's derived SetControllerToSource). The VALUE is controllerOfGiven's own
-- answer rather than a re-derivation, so the CR 613.7 timestamp contest between
-- them is settled once, in the one place that knows how.
--
-- Cheap on the board that has no control effect at all -- the common case --
-- where `named` is empty and the only cost is `controlGrants`' battlefield walk.
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

-- CR 110.2 / 108.4a: the controller a CR 613.1b layer-2 effect OVERRIDES.
--
-- Two rules, one per kind of object this is asked about, exactly as
-- Pawl.Types.Object's own note splits them. For a PERMANENT it is CR 110.2 --
-- "a permanent's controller is, by default, the player under whose control it
-- entered the battlefield" -- and the owner fallback is not 108.4a but the fact
-- that in this pool the player who put it there IS its owner unless a CR 616.1b
-- replacement said otherwise. For anything else -- a card in a library, a
-- graveyard, a hand -- there is no controller at all, and CR 108.4a is what says
-- to use the owner: "if anything asks for the controller of a card that doesn't
-- have one (because it's not a permanent or spell), use its owner instead".
--
-- Object.enteredUnder is written by exactly one thing, Pawl.Engine.Replacement's
-- CR 616.1b rewrite, and is Nothing on every other object -- so for the whole
-- pool but Gather Specimens' victims this is the owner it always was.
--
-- ON THE HOT PATH, which #582 flagged as the cost of moving the base off
-- Object.owner: controllerOfGiven runs once per battlefield object inside
-- `controls`, which the state-based-action sweep calls at every priority
-- boundary. One Maybe match, measured on the tasty-bench suite (main vs. this
-- branch: goldfish / casting / fighting / fighting-aura / fighting-no-aura, 2p):
-- 20.0/159/29.9/588/338 ms -> 20.1/164/30.4/598/350 ms, every move inside the
-- benchmark's own run-to-run stddev (+-0.9 to +-30 ms on those means).
defaultControllerOf :: Object.Object -> PlayerId.PlayerId
defaultControllerOf obj = Maybe.fromMaybe (Object.owner obj) (Object.enteredUnder obj)

-- The battlefield permanents a player controls (CR 108.4). Computes the grant
-- list ONCE and threads it, rather than letting each controllerOf rebuild it --
-- the difference between linear and quadratic in the battlefield, in a function
-- the state-based-action sweep calls at every priority boundary.
controls :: PlayerId.PlayerId -> GameState -> [ObjectId]
controls pid gs = controlsGiven (controlGrants gs) pid gs

-- controls with the grant list PRECOMPUTED, for a caller that then asks
-- controllerOfGiven (or anything else built on the same list) about the
-- permanents it hands back -- Combat.legalAttackers, Mana.manaSources and
-- Action.legalActions all do, and threading the one list keeps the loop from
-- rebuilding it per candidate on top of the rebuild this function already avoids.
controlsGiven :: [ControlGrant] -> PlayerId.PlayerId -> GameState -> [ObjectId]
controlsGiven grants pid gs =
  filter (\oid -> controllerOfGiven grants Set.empty oid gs == Just pid) (Set.toList (GameState.battlefield gs))

-- CR 800.4a: does this stored effect give `pid` control of an object? The
-- control-granting classification Pawl.Engine.Departure asks, so that the case on
-- Modification stays in the one module allowed to make it (see
-- Pawl.Types.Modification). Modification.SetController's payload IS the player who
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
  -- controller -- see the induction in Pawl.Engine.Departure.
  Modification.SetControllerToSource -> False
  _ -> False
