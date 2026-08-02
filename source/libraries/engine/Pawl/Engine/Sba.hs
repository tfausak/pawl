module Pawl.Engine.Sba where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Departure as Departure.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- CR 704.5a (life <= 0), CR 704.5b (drawing from an empty library), and CR
-- 704.5c (ten or more poison counters). Two-Headed Giant's shared-poison variant
-- (CR 704.6b / 810) is out of scope (design.md §6).
losesNow :: GameState -> PlayerId -> Bool
losesNow gs pid = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player ->
    Player.status player == Status.Playing
      && ( Player.life player <= 0
             || Set.member pid (GameState.drewFromEmpty gs)
             || Map.findWithDefault 0 PlayerCounterKind.Poison (Player.counters player) >= 10
         )

-- CR 704.5h: a creature with toughness > 0 dealt damage by a deathtouch source
-- since the last SBA check is destroyed. "Deathtouch source" is read from the
-- event's deal-time bit (CR 702.2e last-known information), NOT re-derived now --
-- so a source that lost deathtouch (Humility) or left after dealing damage is
-- still judged by what it was. See the M3b spec, section 4. Now read from the
-- WATERMARKED slice of the turn log, not a drained queue.
woundedByDeathtouch :: GameState -> ObjectId -> Bool
woundedByDeathtouch gs oid =
  let hits ev =
        DamageEvent.target ev == Recipient.ToCreature oid
          && DamageEvent.amount ev > 0
          && DamageEvent.dealtByDeathtouch ev
   in any hits (Event.unscannedDamage gs)

-- CR 704.5f: toughness 0 or less -- a put-into-graveyard, NOT a destruction, so
-- ungated by indestructible and NOT saved by regeneration (CR 701.19a).
--
-- The isCreature guard is not redundant. Only creatures have printed toughness
-- today, so toughnessOf already implies it -- but CR 301.7a is why the two are
-- not the same question: "Each Vehicle has a printed power and toughness, but it
-- has these characteristics only if it's also a creature." A noncreature Vehicle
-- has the printed numbers and not the characteristic, and 704.5f/g must not
-- touch it. Ask the classification, never the identity.
-- Takes the object's already-projected characteristics (checkStateBasedActions
-- projects the whole board once, per CR 704.3 simultaneity, rather than
-- re-projecting per object).
zeroToughness :: PC.ProjectedCharacteristics -> Bool
zeroToughness pc =
  Set.member CardType.Creature (PC.cardTypes pc)
    && case PC.toughness pc of
      Nothing -> False
      Just t -> t <= 0

-- CR 704.5i: "If a planeswalker has loyalty 0, it's put into its owner's
-- graveyard." CR 306.9 states the same rule from the card type's side.
--
-- A put-into-graveyard and NOT a destruction, the CR 704.5f shape rather than the
-- CR 704.5g one: it is ungated by indestructible and offers regeneration no
-- opportunity (CR 701.19a), which is why the classification below maps it to the
-- bury batch.
--
-- Takes the GameState as well as the projection, unlike zeroToughness above,
-- because CR 306.5c puts a permanent's loyalty in its COUNTERS -- "the loyalty of
-- a planeswalker on the battlefield is equal to the number of loyalty counters on
-- it" -- and no layer projects it. A planeswalker that never received counters
-- reads 0 here and is buried, which is the rule and not an accident: a
-- planeswalker on the battlefield always has CR 306.5b's counters unless
-- something removed them.
--
-- The card-type guard is load-bearing rather than defensive, and in the opposite
-- direction to zeroToughness's: Object.counters is keyed by kind for EVERY
-- permanent, so absent this guard every creature on the battlefield would read as
-- having loyalty 0 and be buried. CR 122.1e is what confines the reading -- "the
-- number of loyalty counters on a PLANESWALKER on the battlefield indicates how
-- much loyalty it has".
zeroLoyalty :: GameState -> PC.ProjectedCharacteristics -> ObjectId -> Bool
zeroLoyalty gs pc oid =
  Set.member CardType.Planeswalker (PC.cardTypes pc)
    && case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> Map.findWithDefault 0 CounterKind.Loyalty (Object.counters obj) == 0

-- CR 704.5g/h: a creature destroyed by lethal marked damage or by a deathtouch
-- source. A DESTRUCTION -- indestructible-gated (CR 702.12b) and regeneration-
-- interceptable (CR 701.19a via the Pawl.Engine.Event destroy funnel). Excludes 704.5f
-- (that is zeroToughness), so toughness here is > 0.
destroyedBySba :: GameState -> PC.ProjectedCharacteristics -> ObjectId -> Bool
destroyedBySba gs pc oid =
  let isCreature = Set.member CardType.Creature (PC.cardTypes pc)
      indestructible = Map.member Keyword.Indestructible (PC.keywords pc)
   in isCreature && not indestructible && case PC.toughness pc of
        Nothing -> False
        Just toughness ->
          toughness > 0
            && ( ( case Game.lookupObject oid gs of
                     Nothing -> False
                     Just obj -> toInteger (Object.damage obj) >= toughness
                 )
                   || woundedByDeathtouch gs oid
               )

-- CR 704.5n: "If an Equipment or Fortification is attached to an illegal
-- permanent or to a player, it becomes unattached from that permanent or player.
-- It remains on the battlefield."
--
-- The shape difference from CR 704.5m, which is why this is a separate
-- classification rather than another clause of fallsOff: an Aura DIES, an
-- Equipment merely DETACHES and stays. CR 301.5c says the same from the card
-- type's side -- "An Equipment that equips an illegal or nonexistent permanent
-- becomes unattached from that permanent but remains on the battlefield."
--
-- "Illegal" is CR 301.5: "An Equipment can be attached to a creature. It can't
-- legally be attached to anything that isn't a creature." So a host that is gone,
-- or that is not a creature, is illegal -- and `pcs` answers both at once, since
-- an object no longer on the battlefield has no entry in it.
--
-- Reads the shared pre-pass projection for the same reason fallsOff does (see its
-- haddock): a per-object project here would reintroduce the cubic sweep. An
-- Equipment whose creature dies THIS pass therefore detaches on the NEXT one,
-- exactly as an Aura falls off on the next one.
--
-- CR 704.5n's "or to a player" clause is expressible but has no producer: no
-- effect in this pool attaches an Equipment to a player (CR 301.5 lets an
-- Equipment be attached only to a creature, and Pawl.Engine.Resolve.attachmentFor
-- refuses anything else). Written anyway, as the `Nothing` a player host yields from
-- Recipient.objectOf -- there is no host object to be a creature, so the rule
-- detaches it.
--
-- Guarded on the PROJECTED subtype, so a permanent that stops being an Equipment
-- while attached stops matching here; cannotBeAttached below is the CR 704.5p
-- branch that detaches it instead.
becomesUnattached :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> Bool
becomesUnattached pcs gs oid = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Object.attachedTo obj of
    Nothing -> False
    Just host ->
      let isEquipment = case Map.lookup oid pcs of
            Nothing -> False
            Just pc -> Set.member Subtype.Equipment (PC.subtypes pc)
          hostIsCreature = case Recipient.objectOf host >>= (\h -> Map.lookup h pcs) of
            Nothing -> False
            Just pc -> Set.member CardType.Creature (PC.cardTypes pc)
       in isEquipment && not hostIsCreature

-- CR 704.5p: "If a battle or creature is attached to an object or player, it
-- becomes unattached and remains on the battlefield. Similarly, if any
-- nonbattle, noncreature permanent that's neither an Aura, an Equipment, nor a
-- Fortification is attached to an object or player, it becomes unattached and
-- remains on the battlefield."
--
-- The complement of becomesUnattached, and the reason the two are separate
-- classifications rather than clauses of one: CR 704.5n asks whether the HOST is
-- still legal, and this asks whether the attached permanent may be attached to
-- anything at all. An Equipment animated while equipping a perfectly legal
-- creature is the case only this one catches -- CR 301.5c, "An Equipment that's
-- also a creature can't equip a creature unless that Equipment has reconfigure".
-- That exception costs nothing here: CR 702.151b makes a reconfigure Equipment
-- "stop being a creature until it becomes unattached", so it never reaches this
-- branch in the first place (and no card in the pool has reconfigure). They share
-- an ACTION (detach, stay on the battlefield), so performStateBasedActions ORs
-- them into one list.
--
-- Read off the PROJECTED characteristics, which is the whole point: the card
-- types this cases on are exactly what CR 613's layer 4 changes, and a permanent
-- that was neither a creature nor an Aura when it became attached is what this
-- rule exists for. "Aura" and "Equipment" are subtypes (CR 205.3h, CR 301.5), so
-- they are read from PC.subtypes -- unlike CR 704.5m's fallsOff, which asks the
-- printed card for an enchant ability because it needs that ability's spec.
--
-- Two clauses of the rule have no constructor to case on and are therefore
-- unreachable rather than elided: there is no CardType.Battle (both "battle"
-- halves) and no Subtype.Fortification. "Or to a player" needs no clause at all
-- here: this rule asks only whether the attached permanent may be attached to
-- ANYTHING, so the `Just _` below covers an object and a player alike.
--
-- This is also where CR 303.4d's second clause -- "An Aura that's also a creature
-- can't enchant anything. If this occurs somehow, the Aura becomes unattached,
-- then is put into its owner's graveyard" -- is enforced, without naming it: such
-- an Aura is a creature that is attached, so it detaches HERE (first sentence)
-- and CR 704.5m's fallsOff buries it on the next pass. The rule's "then" IS that
-- pass boundary. Proven by Liquimetal Coating plus Skilled Animator in
-- Pawl.AuraSpec, which is the only route to it: every printed enchantment
-- animator excludes Auras, so the Aura has to be made an ARTIFACT first.
--
-- CR 303.4d's clause is a RESTRICTION as well as a state-based action ("can't
-- enchant anything"), and only the state-based half lives here. The restriction
-- half is Pawl.Engine.Resolve.attachmentFor's first Aura conjunct, which refuses to attach
-- an Aura that is also a creature to anything.
cannotBeAttached :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> Bool
cannotBeAttached pcs gs oid = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Object.attachedTo obj of
    Nothing -> False
    Just _ -> case Map.lookup oid pcs of
      Nothing -> False
      Just pc ->
        let isCreature = Set.member CardType.Creature (PC.cardTypes pc)
            isAura = Set.member Subtype.Aura (PC.subtypes pc)
            isEquipment = Set.member Subtype.Equipment (PC.subtypes pc)
         in -- Written as the rule's two sentences rather than as the smaller
            -- equivalent `isCreature || not (isAura || isEquipment)`, so each
            -- half can be checked against the CR on its own. The `not isCreature`
            -- conjunct is the second sentence's own "noncreature" qualifier.
            isCreature || (not isCreature && not isAura && not isEquipment)

-- CR 704.5m: "If an Aura is attached to an illegal object or player, or is not
-- attached to an object or player, that Aura is put into its owner's graveyard."
-- Three clauses: unattached, attached to an id that is no longer a permanent, and
-- attached to one its own enchant ability no longer admits (CR 303.4c's "as
-- defined by its enchant ability and other applicable effects").
--
-- CR 303.4c's own wording splits that last clause differently -- "the object it
-- was attached to no longer exists, or the player it was attached to has left the
-- game" -- and both halves land in the SAME place here, because a pool's
-- candidate list already excludes them: Target.creatureRecipients scans the
-- battlefield of players still in the game, and Target.playerRecipients IS
-- Game.stillPlaying. So an enchant-player Aura (CR 702.5d) whose player has left
-- is illegal by the same test that judges every other Aura, with no player-only
-- branch. Pawl.AuraSpec's "the player it was attached to has left the game" case
-- is the proof.
--
-- The third clause goes through stillLegalEnchant below rather than calling
-- Target.stillAdmitted directly, so that the common enchant spec is answered off
-- `pcs` -- the SAME pre-pass Projection.projectAll performStateBasedActions
-- computed once for every other CR 704.3 classification. (A spec carrying a
-- Filter still reaches stillAdmitted, by that function's own fallthrough; see
-- its haddock.)
-- stillAdmitted reaches Target.admittedRecipients -> basePoolGiven Pool.Creatures
-- -> creatureRecipients -> Projection.isCreatureOf, and THAT is `project oid gs`
-- -- a fresh `gather` PER Aura. Every other classify here shares one `gather`
-- precisely because gather's neighbouring lesson (Projection.hs's `liveGiven`
-- comment) is that recomputing it inside a per-object loop makes project
-- O(permanents^3) per SBA sweep; calling stillLegal here reintroduced that same
-- shape one level down (20 permanents with 2 attached Auras costing ~40 extra
-- `gather`s and ~400 extra `project`s per pass).
--
-- CR 303.4d's first clause -- an Aura can't enchant itself -- is the `oid == self`
-- arm. Unreachable in this pool (a Creatures enchant spec cannot name the Aura
-- spell on the stack), written anyway because it costs one comparison. CR
-- 303.4d's SECOND clause -- an Aura that's also a creature can't enchant anything
-- -- is the "unattached" arm here plus cannotBeAttached above: that rule detaches
-- the animated Aura on one pass, and this buries it on the next, which is the
-- order CR 303.4d states. See cannotBeAttached's haddock.
--
-- A put-into-graveyard, NOT a destruction: CR 704.5m says "put into its owner's
-- graveyard", so this goes through Event.changeZone and consults neither
-- indestructible (CR 702.12b) nor a regeneration shield (CR 701.19a).
fallsOff :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> Bool
fallsOff pcs gs oid = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> case Card.Type.enchant card of
    Nothing -> False
    Just spec -> case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> case Object.attachedTo obj of
        Nothing -> True
        Just recipient ->
          Recipient.objectOf recipient == Just oid
            || not (stillLegalEnchant pcs gs oid spec recipient)

-- CR 303.4c: is `recipient` still one the enchanting Aura `source`'s spec
-- ADMITS? Answered off `pcs` -- the pre-pass projection every other
-- classification in performStateBasedActions shares (CR 704.3 simultaneity) --
-- for the one spec shape that reduces to a lookup, and by the general
-- Target.stillAdmitted for every other.
--
-- Admission, NOT target legality: CR 303.4c asks whether the Aura enchants "an
-- illegal object or player as defined by its enchant ability and other
-- applicable effects", which rule 702's TARGETING restrictions do not speak to.
-- Protection would bury this Aura, but by its own separate clause (CR 702.16c),
-- while shroud (CR 702.18) and hexproof (CR 702.11) restrict targeting and
-- nothing else -- so an Aura stays attached to a host that gains either. See
-- Target.admittedRecipients.
--
-- Pool.Creatures with no Filter is the shape MOST Card.enchant specs in this pool
-- carry (Unholy Strength's "enchant creature"). Target.creatureRecipients
-- (Target.hs) tags every candidate it produces ToCreature, drawn from the
-- battlefield objects owned by a still-playing player (Game.stillPlaying);
-- with no Filter left to narrow that set, "still legal" reduces EXACTLY to
-- "still a creature, on the battlefield, owned by a player still in the game" --
-- which is what the Pool.Creatures arm below reads off `pcs` (a Map.lookup) plus
-- one owner check. This is not an approximation: `pcs Map.! target`, when it
-- exists, IS `Projection.project target gs` (projectAll folds the SAME gathered
-- candidate list stillAdmitted would rebuild from scratch), and a missing key
-- means exactly what Target.creatureRecipients' own battlefield scan would have
-- missed it for -- target is not on the battlefield at all.
--
-- Any OTHER shape falls through to the general, slower Target.stillAdmitted,
-- which reuses the SAME pool and Filter Cast/Resolve already judge -- rather
-- than assuming the
-- Creatures-with-no-Filter shape holds regardless, a shortcut that would go
-- silently wrong the day it stops holding. That day has come: Setessan Training's
-- "Enchant creature you control" is a Creatures spec that DOES carry a Filter, and
-- its ControlledBy You conjunct is unanswerable from `pcs` -- CR 109.5 makes that
-- "you" the AURA's controller (enchant is a static ability, CR 702.5a), so the
-- answer changes when an opponent steals the enchanted creature even though the
-- reduction's three facts all still hold. Pawl.AuraSpec's Control Magic case is
-- the proof. CR 702.5d's enchant-player Auras are the second producer: Curse of
-- Death's Hold's Pool.Players spec falls through, and Target.playerRecipients IS
-- Game.stillPlaying, so CR 303.4c's "the player it was attached to has left the
-- game" is answered by the general path with no clause of its own.
--
-- So the fallthrough pays the per-Aura re-projection the reduction above exists to
-- avoid -- but only for the Auras whose spec the reduction cannot serve, and `pcs`
-- remains the source of truth for every other CR 704.3 classification on the pass,
-- so the simultaneity the pre-pass buys is untouched. Serving a filtered spec off
-- `pcs` would mean answering Filter.matches against the pre-pass projection
-- instead of a fresh one, which is #430.
--
-- That fallback is general in its recipient TAG as well as in its pool and
-- filter, and that is the whole reason Object.attachedTo stores a Recipient: the
-- tag is the one the Aura's own pool produced when it attached, so a Pool.Players
-- spec is judged against ToPlayer candidates and a Pool.Permanents one against
-- ToObject candidates, with nothing here to keep in step. Re-deriving the tag
-- from the Pool would have been the alternative, and it would have had to be
-- taught every pool separately.
--
-- The fast arm is therefore matched on the PAIR, not on the spec alone: it is a
-- reduction of Pool.Creatures' candidate list specifically, so a recipient of any
-- other shape falls through rather than being read as an object id it is not.
stillLegalEnchant :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> TargetSpec.TargetSpec -> Recipient.Recipient -> Bool
stillLegalEnchant pcs gs source spec recipient = case (spec, recipient) of
  (TargetSpec.MkTargetSpec Pool.Creatures Nothing, Recipient.ToCreature target) ->
    case Map.lookup target pcs of
      Nothing -> False
      Just pc ->
        Set.member CardType.Creature (PC.cardTypes pc)
          && case Game.lookupObject target gs of
            Nothing -> False
            Just obj -> List.elem (Object.owner obj) (Game.stillPlaying gs)
  -- The Aura is on the battlefield when this SBA asks, so its controller is
  -- live -- the CR 608.2b case this perspective exists for cannot arise here.
  _ -> Target.stillAdmitted (Projection.controllerOf source gs) source recipient spec gs

-- CR 704.5j: the same-named legendary groups one player controls, as a list of
-- groups, each with two or more members. Both halves are read from the
-- PROJECTION, not the printed card, which is the whole reason a Clone is caught:
-- CR 707.2 copies name and supertype alike, so a Clone of Thalia is legendary and
-- named Thalia even though its own card is neither.
--
-- Grouped by name per controller. Two players each with a Thalia is not the
-- legend rule's business (it says "controlled by the same player"), and one
-- player's Thalia and Urborg are two different names.
legendGroups :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> [(PlayerId, NonEmpty.NonEmpty ObjectId)]
legendGroups pcs gs =
  let legendary oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          | Set.member Supertype.Legendary (PC.supertypes pc) ->
              fmap (\controller -> ((controller, PC.name pc), [oid])) (Projection.controllerOf oid gs)
          | otherwise -> Nothing
      keyed = Maybe.mapMaybe legendary (Set.toList (GameState.battlefield gs))
      byKey = Map.fromListWith (<>) keyed
      -- Only a group of two or more is the legend rule's business, and the
      -- candidate list is sorted so the prompt's order is stable rather than an
      -- accident of how Map.fromListWith accumulated it.
      toGroup ((controller, _), oids) = case List.sort oids of
        first : rest@(_ : _) -> Just (controller, first NonEmpty.:| rest)
        _ -> Nothing
   in Maybe.mapMaybe toGroup (Map.toList byKey)

-- CR 704.5j: ask one same-named group's controller which to keep, and return the
-- rest -- the permanents this pass must put into their OWNERS' graveyards
-- (Event.changeZone is owner-relative, so that falls out).
--
-- Asks but does not move, and that separation is the rule rather than tidiness.
-- CR 704.3 performs every applicable state-based action "simultaneously as a
-- single event", so the choice has to be made against the state as the pass
-- began -- including a group member that another action is ALSO about to bury.
-- Keeping that one is a legal choice, and it puts every other same-named legend
-- into the graveyard beside it; dropping it from the candidates would decide for
-- the player and could leave a copy alive that they chose to lose.
--
-- A plain put-into-graveyard, not a destruction: CR 704.5j says "put into", so
-- the caller consults neither indestructible (CR 702.12b) nor a regeneration
-- shield, exactly as CR 704.5f's zero-toughness bury does.
--
-- FILTERED, NOT TRUSTED: an answer naming a permanent outside the group would
-- otherwise bury the whole group, so it falls back to the head.
chooseLegendVictims :: (PlayerId, NonEmpty.NonEmpty ObjectId) -> Game [ObjectId]
chooseLegendVictims (controller, candidates) = do
  gs <- State.get
  answer <- Trans.lift (Program.prompt (Prompt.ChooseLegend (Decide.deciderFor controller gs) controller candidates))
  let kept = if List.elem answer (NonEmpty.toList candidates) then answer else NonEmpty.head candidates
  pure (filter (/= kept) (NonEmpty.toList candidates))

-- CR 704.5k: "If two or more permanents have the supertype world, all except the
-- one that has had the world supertype for the shortest amount of time are put
-- into their owners' graveyards. In the event of a tie for the shortest amount of
-- time, all are put into their owners' graveyards." The permanents this pass must
-- bury, which is every world permanent but the newest arrival.
--
-- The neighbouring legend rule (CR 704.5j) is a different shape in three ways,
-- and each one is a place this could have been wrongly copied from it:
--
-- 1. It ASKS; this does not. CR 704.5j has the controller choose a survivor, so
--    chooseLegendVictims raises a prompt. CR 704.5k decides by the clock, and
--    that answer is a fact about the board rather than a choice, so prompting
--    here would be the engine inventing a decision the rules never offer. Not an
--    elision -- there is nothing to ask.
-- 2. It is scoped to one controller and one name; this is scoped to neither.
--    "If two or more permanents" is the whole condition, so two players each with
--    a world permanent (a board the legend rule leaves alone) is exactly the case
--    this rule fires on.
-- 3. It has no tie clause; this one does. Game.freshTimestamp hands out a
--    distinct stamp per object, so no two permanents can be equally new and the
--    tie arm below is unreachable today. Written regardless, because it costs
--    one `case` and it is what the rule says: the day two permanents can share
--    an arrival time, "all are put into their owners' graveyards" is the answer,
--    not "the lower id survives".
--
-- The CLOCK is Object.timestamp -- when the permanent entered the battlefield
-- (CR 613.7d) -- and NOT a separate record of when it became world. The two are
-- the same instant for every board pawl can reach TODAY, and that is a fact
-- about what pawl cannot yet express rather than a fact about Magic. All three
-- of the ways they could come apart are missing capabilities, each with an issue:
--
-- 1. A supertype gained or lost on the battlefield. No Modification arm changes
--    a supertype (#311), so nothing projects one -- the projection seeds
--    supertypes from the card and no layer touches them afterwards.
-- 2. A permanent that BECOMES a copy of a world permanent, which needs no
--    supertype-changing effect at all: CR 707.2 lists supertype among the
--    copiable values. pawl copies only as an object ENTERS (Binding.copyOf,
--    written by the CR 614 as-enters replacement), the same moment it is
--    stamped; CR 707.3's on-the-battlefield half is #313, and Crystalline
--    Resonance is the card that would break this reading -- it can be older than
--    the world permanent it copies, so the timestamp says "bury it" where the
--    rule says "keep it".
-- 3. A restamp with no zone change. CR 701.3c's is the only one, and it needs
--    the world permanent to be attachable, which no printing that carries the
--    world supertype is.
--
-- Whichever of #311 or #313 lands first has to give this rule a clock of its own:
-- a per-object "world since", sampled where the supertype set is established.
--
-- Read off the PROJECTION rather than the printed type line, for the reason
-- legendGroups is: CR 707.2 makes a copy of a world permanent world too.
--
-- CR 801.12 narrows this rule to permanents "within its controller's range of
-- influence". CR 801.1 makes limited range of influence an OPTION, and pawl has
-- no representation for a multiplayer option at all (#175), so every world
-- permanent is always in range and the narrowing is inert.
--
-- A put-into-graveyard, NOT a destruction: CR 704.5k says "put into", so the
-- caller consults neither indestructible (CR 702.12b) nor a regeneration shield,
-- exactly as CR 704.5f's zero-toughness bury and CR 704.5j's legend rule do.
worldVictims :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> [ObjectId]
worldVictims pcs gs =
  let stamped oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          | Set.member Supertype.World (PC.supertypes pc) ->
              fmap (\obj -> (Object.timestamp obj, oid)) (Game.lookupObject oid gs)
          | otherwise -> Nothing
      worlds = Maybe.mapMaybe stamped (Set.toList (GameState.battlefield gs))
   in case fmap fst worlds of
        [] -> []
        ts : rest ->
          -- The SHORTEST amount of time is the LARGEST timestamp: the newest
          -- arrival is the one that has been world for the least time.
          let newest = List.foldl' max ts rest
              (survivors, older) = List.partition (\(t, _) -> t == newest) worlds
           in case survivors of
                [_] -> fmap snd older
                _ -> fmap snd worlds

-- CR 704.3: repeat until no state-based action is performed. ONE pass here, with
-- the repeat living in Engine's CR 117.5 settle loop (settleForPriority). A
-- single pass is NOT sufficient IN GENERAL -- CR 704.5m's Aura falls off, and CR
-- 704.5n's Equipment detaches, only on the pass AFTER the creature dies -- so
-- settleForPriority is the entry point a caller wanting a settled board should
-- use. Engine.runStep's own two direct, unlooped calls are the exception, each
-- safe for reasons local to that call site, not repeated here.
checkStateBasedActions :: Game ()
checkStateBasedActions = Monad.void performStateBasedActions

-- One SBA pass, also reporting whether any state-based action was PERFORMED (a
-- creature buried or a player departed). CR 704.3: the caller repeats the check
-- while that flag is True. The flag lets the CR 117.5 settle loop (Engine) decide
-- whether to repeat WITHOUT a deep GameState comparison.
--
-- Monadic since P5: CR 704.5f's put-into-graveyard and CR 704.5g's destruction
-- both go through funnels that can now raise a CR 616 replacement prompt (a
-- creature dying with two applicable death-replacements genuinely must ask its
-- controller which to apply). M3g's decider re-entrancy already permits prompting
-- from inside the settle loop.
performStateBasedActions :: Game Bool
performStateBasedActions = do
  gs <- State.get
  let -- CR 704.5f/g are checked against the state BEFORE any of them apply: SBAs
      -- are simultaneous. Project the whole board once (one gather) and judge each
      -- object against it, rather than re-projecting per object.
      pcs = Projection.projectAll gs
      classify oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          -- CR 704.5f wins when both apply: toughness <= 0 is a put-into-graveyard.
          | zeroToughness pc -> Just False
          -- CR 704.5i, the other put-into-graveyard, checked against the same
          -- pre-pass board for the same CR 704.3 simultaneity reason.
          | zeroLoyalty gs pc oid -> Just False
          | destroyedBySba gs pc oid -> Just True
          | otherwise -> Nothing
      onBattlefield = Set.toList (GameState.battlefield gs)
      toGraveyard = filter (\oid -> classify oid == Just False) onBattlefield
      toDestroy = filter (\oid -> classify oid == Just True) onBattlefield
      -- CR 704.5q / 122.3: a permanent with both a +1/+1 and a -1/-1 counter has N
      -- of each removed (N = min). Computed against the SAME pre-pass state as the
      -- bury/destroy classification, which is what makes the ordering immaterial:
      -- net P/T is preserved, so it can neither cause nor prevent a death.
      annihilateOne oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj ->
          let cs = Object.counters obj
              plus = Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs
              minus = Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs
              n = min plus minus
           in if n > 0 then Just (oid, n) else Nothing
      annihilations = Maybe.mapMaybe annihilateOne onBattlefield
      -- CR 704.5m: an Aura attached to nothing, or to something its enchant
      -- ability no longer admits. Judged against the SAME pre-pass pcs/gs as
      -- every other classification above -- see fallsOff's Haddock for why an
      -- Aura whose creature dies THIS pass survives it and falls off the next.
      unattachedAuras = filter (fallsOff pcs gs) onBattlefield
      -- CR 704.5n and CR 704.5p: computed from the same pre-pass state, for the
      -- same reason. One list because they share an action -- detach, stay on
      -- the battlefield -- and differ only in why the attachment is illegal.
      detaching = filter (\oid -> becomesUnattached pcs gs oid || cannotBeAttached pcs gs oid) onBattlefield
      -- CR 704.5h's window is "since the last SBA check", so the watermark is the
      -- log length AS THIS PASS BEGAN: every 704.5h victim was computed from that
      -- same pre-pass state, and the Moved events this pass itself appends carry
      -- no damage. The record is never removed.
      watermark :: Natural
      watermark = Natural.length (GameState.events gs)
      -- CR 704.5j, computed from the SAME pre-pass state as every classification
      -- above, because CR 704.3 performs all applicable state-based actions
      -- simultaneously. Deliberately NOT filtered against the buries below: see
      -- chooseLegendVictims for why a member that is dying anyway must stay on
      -- the ballot.
      legendsToResolve = legendGroups pcs gs
      -- CR 704.5k, from the SAME pre-pass state as everything above, for the
      -- same CR 704.3 reason. Unlike the legend rule this one asks nobody, so
      -- it is a pure list rather than a prompt.
      worldLosers = worldVictims pcs gs
  -- CR 704.5j: the legend rule is the one state-based action that ASKS, and it
  -- asks BEFORE anything below moves -- so every choice is made against the state
  -- this pass began in, which is what CR 704.3's "simultaneously as a single
  -- event" requires.
  legendVictims <- fmap concat (Monad.mapM chooseLegendVictims legendsToResolve)
  -- Every put-into-graveyard this pass performs, as ONE deduplicated batch:
  -- CR 704.5f (toughness <= 0), CR 704.5i (loyalty 0), CR 704.5j (the legend
  -- rule's losers), CR 704.5k (the world rule's) and CR 704.5m (an Aura attached
  -- to nothing). None of the five is a destruction, so none consults
  -- indestructible or a regeneration shield.
  --
  -- Deduplicated because the sets overlap: a legend at 0 toughness whose
  -- controller kept a DIFFERENT copy is named by 704.5f and 704.5j alike, and
  -- moving it twice would emit a second zone-change event and fire its
  -- dies-triggers again. 704.5f and 704.5k overlap the same way, and reachably:
  -- Opalescence animates a world enchantment, Night of Souls' Betrayal makes it
  -- a 0/0, and the older of two world permanents is then named by both.
  --
  -- Moved as a BATCH on the pre-pass state, not one move at a time on the live
  -- board, for the same CR 704.3 reason every classification above was computed
  -- from `gs`: each member's CR 616.1 loop collects its replacement candidates
  -- from the board the pass began in, so an animated Rest in Peace this pass is
  -- itself burying still exiles the cards the rest of the batch would put into
  -- graveyards. See Pawl.Engine.Replacement's applyReplacementsIn.
  Monad.mapM_ (\oid -> Event.changeZoneInBatch gs oid Zone.Graveyard) (List.nub (toGraveyard <> legendVictims <> worldLosers <> unattachedAuras))
  -- CR 704.5n / 704.5p: the Equipment does NOT follow its creature -- it detaches
  -- and stays. Not a zone change, so unlike the Aura above it does not funnel
  -- through Pawl.Engine.Event: no Moved event, no replacement, no trigger.
  State.modify' (\g -> g {GameState.objects = List.foldl' (\m oid -> Map.adjust (\o -> o {Object.attachedTo = Nothing}) oid m) (GameState.objects g) detaching})
  -- CR 704.5g/h: destruction through the funnel, Regenerable -- and that is not a
  -- default so much as the point, since CR 701.19a's shield exists to replace
  -- exactly this destruction.
  --
  -- A permanent the legend rule or the world rule already buried is excluded
  -- rather than left to no-op on a dead id, and the two halves of this line say
  -- the same thing from opposite ends: CR 704.5j and CR 704.5k are
  -- put-into-graveyards, not destructions, so neither offers the shield an
  -- opportunity nor may consume one here.
  --
  -- ONE batch, not one call per victim, and on the SAME pre-pass board as the
  -- put-into-graveyard batch above, because CR 704.3 makes the two halves one
  -- event: "performs all applicable state-based actions simultaneously as a
  -- single event". Splitting them is an implementation order, not a rules one, so
  -- a replacement effect belonging to a permanent buried above is still in force
  -- for a destruction here -- an animated Rest in Peace this pass buries exiles
  -- the card of the creature this pass destroys.
  --
  -- The funnel's own CR 702.12b gate is judged against that same board too, so it
  -- asks what `destroyedBySba` asked above rather than second-guessing it from a
  -- board the buries have already changed. Only the funnel's existence filter is
  -- live, which is what keeps a permanent the buries already moved -- CR 704.5m's
  -- Aura is the one this line does not exclude by name -- from being offered a
  -- destruction that never happens (CR 614.7). Event.destroyIn sets all three
  -- readers out.
  Event.destroyInBatch gs Regenerability.Regenerable (filter (\oid -> List.notElem oid legendVictims && List.notElem oid worldLosers) toDestroy)
  destroyed <- State.get
  let leaving = filter (losesNow destroyed) (Game.stillPlaying destroyed)
      departed = foldr (Departure.depart Departure.Type.Lost) destroyed leaving
      -- CR 704.5d: a token in any zone other than the battlefield ceases to exist.
      -- Computed from the post-bury state so a token that just died (now in the
      -- graveyard) or was redirected (Rest in Peace -> exile) is removed here; its
      -- move already emitted a zone-change event, so a future dies-trigger still
      -- sees it (CR 111.7's parenthetical). Keyed to "not on the battlefield",
      -- never to a specific zone, so exile is caught too.
      isVanishingToken oid = case Game.lookupObject oid departed of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfToken _ -> Object.zone obj /= Zone.Battlefield
          _ -> False
      vanishing = filter isVanishingToken (Map.keys (GameState.objects departed))
      ceaseToExist g oid = case Game.lookupObject oid g of
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
           in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
      vanished = List.foldl' ceaseToExist departed vanishing
      removeN n c = let c' = c - n in if c' == 0 then Nothing else Just c'
      balance g (oid, n) =
        let strip obj = obj {Object.counters = Map.update (removeN n) CounterKind.MinusOneMinusOne (Map.update (removeN n) CounterKind.PlusOnePlusOne (Object.counters obj))}
         in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
      outcome = Departure.outcomeAfterLeaving leaving departed
      drained = vanished {GameState.damageScannedThrough = watermark}
      balanced = List.foldl' balance drained annihilations
      -- A state-based action was performed iff a permanent was buried (a creature
      -- at 0 toughness, CR 704.5f, or a planeswalker at 0 loyalty, CR 704.5i) or
      -- destroyed (a regenerated creature still counts, which the CR 704.3 settle
      -- loop re-checks and -- because the regen healed the damage -- terminates), a
      -- player left, a token ceased to exist, an Aura fell off (CR 704.5m), a
      -- permanent detached (CR 704.5n / 704.5p), the legend rule buried a
      -- duplicate legend (CR 704.5j), or the world rule buried an older world
      -- permanent (CR 704.5k).
      acted = not (null legendVictims) || not (null worldLosers) || not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations) || not (null unattachedAuras) || not (null detaching)
  -- CR 104.1: a game ends the moment a result is reached, so a later pass may
  -- not replace one. The existing result therefore wins; this pass only settles
  -- an outcome when the game did not already have one. Same ordering as
  -- Departure.leaveGame -- the two doors that write GameState.result agree.
  State.put balanced {GameState.result = GameState.result balanced <|> outcome}
  pure acted
