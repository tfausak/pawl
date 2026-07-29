module Pawl.Projection where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import qualified Pawl.Count as Count
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Subtype as Subtype
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Combat as Combat
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Keyword (Keyword)
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.LastKnown as LastKnown
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
import qualified Pawl.Type.Subtype as Subtype.Type
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
        -- CR 305.7, second sentence: "It loses all abilities generated from its
        -- rules text, ITS OLD LAND TYPES, and any copiable effects affecting that
        -- land, and it gains the appropriate mana ability for each new basic land
        -- type." Three clauses, and the arm does the first two; the mana ability
        -- rides the new subtype and is read at the mana call site (CR 305.6).
        --
        -- The SUBTYPE clause takes the land types and nothing else. CR 205.3i
        -- (Pawl.Subtype.isLandType) is the list; the fourth sentence -- "Setting
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
        -- in gather, Pawl.PlayerEffect.applying and Pawl.BlockRequirement.
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
-- Resolve.cease does to an ability and what Departure.objectsLeaveWith does to a
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
          Filter.playerIdentity = Nothing,
          -- CR 506.3: only a creature can attack, and a card in a library or hand
          -- is not one -- so it has no combat status either.
          Filter.attacking = False,
          -- And it was never on the battlefield to be declared as one, so the
          -- turn's event log holds nothing about it either.
          Filter.attackedThisTurn = False,
          -- CR 303.4b: only a permanent on the battlefield is attached to
          -- anything, and a printed card off it is not one.
          Filter.attachedToCreature = False,
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
      Filter.power = PC.power pc,
      Filter.controller = controller,
      Filter.identity = Just oid,
      Filter.playerIdentity = Nothing,
      -- CR 508.1k: attacking is a combat STATUS, not a characteristic (CR 109.3),
      -- so it comes straight off the combat record and not from the projection
      -- this function is otherwise assembling.
      Filter.attacking = Map.member oid (Combat.attackers (GameState.combat gs)),
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
      -- recurse. See Pawl.Filter.View's own note on the field.
      Filter.attachedToCreature = case Game.lookupObject oid gs >>= Object.attachedTo of
        Nothing -> False
        Just host -> isCreatureOf host gs,
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

-- Printed characteristics before any effect (CR 613.2/613.4 starting point).
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.cardOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { -- No card behind this object (an ability or trigger on the stack): it has
        -- no printed name and no type line to seed from.
        PC.name = Text.empty,
        PC.supertypes = Set.empty,
        PC.keywords = Map.empty,
        PC.colors = Set.empty,
        PC.power = Nothing,
        PC.toughness = Nothing,
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
  Just (ManaCost.MkManaCost symbols) -> Set.fromList (concatMap symbolColors symbols)

-- CR 202.2b: only a coloured mana symbol carries a colour ("Objects with no
-- colored mana symbols in their mana costs are colorless"). Generic ({2}), {X},
-- and the colourless symbol ({C}) carry none -- {C} is colourless mana, and
-- CR 105.2c says colourless is not a colour.
--
-- A LIST, not a Maybe, because of CR 107.4e's last sentence: "A hybrid mana
-- symbol is all of its component colors." Burning-Tree Emissary's {R/G}{R/G}
-- makes it both red and green, not one or the other and not multicoloured-as-a-
-- third-thing (CR 105.3: an object with two or more colours IS each of them).
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
            filter (any isSet . StaticAbility.modifications) (Card.Type.staticAbilities card)
   in concatMap fromStored (GameState.continuousEffects gs)
        <> concatMap fromPerm (Set.toList (GameState.battlefield gs))

-- CR 305.7: a land whose subtype is SET to a basic type loses its rules-text
-- abilities. This is the GATE half of that rule, shared by all three readers whose
-- ability lands on objects other than the bearer -- gather here,
-- Pawl.PlayerEffect.applying and Pawl.BlockRequirement.instances -- since such an
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
-- SetLandSubtype/AddLandSubtype carry a land-type word; every other modification
-- has none to rewrite here. Projection's charter (it cases on Modification); it is
-- delegated to by Resolve.rewriteEffect for the inner modification of ModifyTarget.
rewriteModification :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modification -> Modification
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
--
-- CR 613.6: the affected set belongs to the EFFECT, not to each of its parts, so
-- the parts of one static ability all carry that ability's key -- which is what
-- lets projectWith decide their set once. A stored effect and a counter are each
-- a single part and carry none.
--
-- TWO ability losses are asked about here, and only about a PERMANENT'S OWN
-- static abilities -- Pawl.PlayerEffect.applying asks the same pair, about the
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
      -- A stored effect carries exactly one modification (Pawl.Resolve stores one
      -- per opcode), so CR 613.6 has nothing to hold together here -- and nothing
      -- to get wrong either, since every stored effect's set is the CR 611.2c
      -- TheseObjects one, locked at resolution.
      fromStored eff =
        MkGathered
          { gEffect = Nothing,
            gSource = ContinuousEffect.source eff,
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
-- (Pawl.PlayerEffect.applying, for CR 604.2's "and has the ability"), so that the
-- candidate list is built once per read rather than once per permanent -- the
-- same posture setLandSubtypeEffects has for CR 305.7's liveGiven.
--
-- The list is gathered with the layer-6 gate OFF, which is what the gate itself
-- reads, and why this needs an extra pass rather than a fixpoint. Deciding
-- whether a source's abilities were removed means projecting that source through
-- layers 1-5 (abilitiesRemoved), and a projection bounded below layer 6 applies
-- no candidate at layer 6 or later -- so it cannot see, and so cannot be changed
-- by, the layer-7 parts the gate drops.
--
-- WELL-FOUNDED, and this is the whole argument: nothing reachable from here reads
-- a player effect back. The layer machine's only inputs are static abilities,
-- stored continuous effects and counters; a CR 613.10/613.11 effect is a sibling
-- tier applied AFTER that machine has run and is not among them. Pawl.Projection
-- accordingly does not import Pawl.PlayerEffect -- the module graph is what
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
  Modification.SetBasePowerToughness _ _ -> False
  Modification.ModifyPowerToughness _ _ -> False
  Modification.SwitchPowerToughness -> False
  Modification.AddLandSubtype _ -> False
  Modification.ChangeSubtypeWord _ _ -> False
  Modification.AddCardType _ -> False
  Modification.SetColor _ -> False
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
-- The affected set is judged against the projection layers 1-5 leave behind
-- (projectUpTo Layer.Ability), which is exactly the partial the fold itself uses
-- when it applies layer 6: no layer-6 modification WRITES an aspect any Filter
-- reads (modificationWrites), so CR 613.8 reorders nothing within layer 6 and the
-- two readings cannot disagree. This is what lets an Opalescence-animated Bad
-- Moon be inside Humility's "each creature" -- the animation is layer 4.
--
-- Layer 6 is where a remover's set is asked here, rather than CR 613.6's lowest
-- layer of the ability carrying it (#326).
--
-- NOT asked of the remover's own source: whether a stripper was itself stripped
-- is a question about ORDER WITHIN layer 6, which the fold settles by CR 613.7
-- timestamp as it applies that layer, not something this gate can restate (#37's
-- neighbourhood -- see the layer-6 grant/Humility timestamp test).
abilitiesRemoved :: [Gathered] -> GameState -> ObjectId -> Bool
abilitiesRemoved cands gs oid =
  let partial = projectUpTo Layer.Ability cands oid gs
      removes c =
        removesAbilities (gModification c)
          && affects (gSource c) oid (gAffected c) partial gs
   in any removes cands

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
  let ms = StaticAbility.modifications sa
      key = case ms of
        _ NonEmpty.:| (_ : _) -> Just (src, n)
        _ -> Nothing
      one m =
        let m' = rewriteModification changes m
         in MkGathered
              { gEffect = key,
                gSource = src,
                gAffected = StaticAbility.affected sa,
                gLayer = layer m',
                gTimestamp = ts,
                gModification = m'
              }
      parts = fmap one (NonEmpty.toList ms)
   in -- The cheap structural test first, so `stripped`'s projection is forced only
      -- for an ability the rest of the rule could actually reach.
      if all ((> Layer.Ability) . gLayer) parts && stripped then [] else parts

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
-- (Combat.removeControlChanged, from Engine.settleForPriority) rather than inside
-- one. So the record is a fixed INPUT to any single projection: applying one
-- modification before another cannot change what this arm answers, which is
-- exactly the question CR 613.8a asks. The arm stays empty when the remaining
-- CR 506.4 clauses land (#246), because they will be sampled the same way.
--
-- What that costs is TIMING, not dependency: the rules remove the permanent the
-- instant control changes, and pawl removes it at the next settle. That window is
-- argued where the sampling happens.
filterReads :: Filter.Type.Filter -> Set Aspect
filterReads f = case f of
  Filter.Type.HasCardType _ -> Set.singleton Types
  Filter.Type.HasSupertype _ -> Set.empty
  Filter.Type.HasColor _ -> Set.singleton Colors
  Filter.Type.HasSubtype _ -> Set.singleton Subtypes
  Filter.Type.PowerAtLeast _ -> Set.singleton PowerA
  Filter.Type.ControlledBy _ -> Set.singleton Controller
  Filter.Type.IsSource -> Set.empty
  Filter.Type.IsPlayer _ -> Set.empty
  Filter.Type.IsAttacking -> Set.empty
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
-- The two layer-6 arms write nothing here: no Filter reads abilities, so losing
-- or gaining one cannot change what any affected set matches. That is not the
-- same as saying an ability change cannot matter to CR 613.8 at all -- it can
-- change an effect's EXISTENCE, which is a different clause of CR 613.8a and
-- lives in staticAbilitiesLive (CR 305.7).
--
-- SetLandSubtype writes abilities too, and declares only Subtypes for the same
-- reason: the abilities it empties are unreadable by any Filter, so the only part
-- of it CR 613.8a can see is the subtype it sets.
modificationWrites :: Modification -> Set Aspect
modificationWrites m = case m of
  Modification.GainKeyword _ -> Set.empty
  Modification.LoseAllAbilities -> Set.empty
  Modification.SetBasePowerToughness _ _ -> Set.singleton PowerA
  Modification.ModifyPowerToughness _ _ -> Set.singleton PowerA
  Modification.SwitchPowerToughness -> Set.singleton PowerA
  Modification.SetLandSubtype _ -> Set.singleton Subtypes
  Modification.AddLandSubtype _ -> Set.singleton Subtypes
  Modification.ChangeSubtypeWord _ _ -> Set.singleton Subtypes
  Modification.AddCardType _ -> Set.singleton Types
  Modification.SetColor _ -> Set.singleton Colors
  Modification.SetController _ -> Set.singleton Controller
  Modification.SetControllerToSource -> Set.singleton Controller

-- Could another effect move this one's affected set at all? The structural half
-- of projectWith's movableReads: only a Matching set is a predicate over
-- characteristics that something else can change. A TheseObjects set names ids
-- (CR 611.2c) and an Attached one reads its source's attachment off the game
-- state (CR 303.4m).
staticallyMovable :: Gathered -> Bool
staticallyMovable c = case gAffected c of
  Affected.Matching _ -> True
  Affected.TheseObjects _ -> False
  Affected.Attached -> False

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
    layers = filter admits (Set.toAscList (Set.insert Layer.CharacteristicPT (Set.fromList (fmap gLayer cands))))
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
            let seeded =
                  if lyr == Layer.CharacteristicPT
                    then applyCharacteristicPT lyr cands gs oid partial
                    else partial
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
      -- Bound separately, and NOT inlined: projectWith does its candidate-only
      -- work (the layer list, the CR 613.8 movable-layer set) when it is applied
      -- to `cands`, so sharing this partial application shares that work across
      -- every object on the board.
      forObject = projectFrom cands
   in Map.fromSet (\oid -> forObject oid gs) (GameState.battlefield gs)

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

subtypesOf :: ObjectId -> GameState -> Set Subtype.Type.Subtype
subtypesOf oid gs = PC.subtypes (project oid gs)

-- CR 201.1 / 707.2: the object's projected name -- a Clone's is the name it
-- copied, not "Clone".
nameOf :: ObjectId -> GameState -> Text
nameOf oid gs = PC.name (project oid gs)

-- CR 205.4: the object's projected supertypes, the sibling of subtypesOf.
supertypesOf :: ObjectId -> GameState -> Set Supertype.Supertype
supertypesOf oid gs = PC.supertypes (project oid gs)

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
