module Pawl.Engine.Sba where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Engine.Speed as Speed
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
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
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- CR 704.5a (life <= 0), CR 704.5b (drawing from an empty library), CR 704.5c
-- (ten or more poison counters), and CR 704.6c / CR 903.10a (twenty-one combat
-- damage from one commander). Two-Headed Giant's shared-poison variant (CR
-- 704.6b / 810) is out of scope (design.md §6).
--
-- Rule 704.6c's disjunct is delegated to Pawl.Engine.Commander, the way CR
-- 704.5z's is to Pawl.Engine.Speed: this module owns WHEN a state-based action
-- is checked, not what each one means.
losesNow :: GameState -> PlayerId -> Bool
losesNow gs pid = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player ->
    Player.status player == Status.Playing
      && ( Player.life player <= 0
             || Set.member pid (GameState.drewFromEmpty gs)
             || Map.findWithDefault 0 PlayerCounterKind.Poison (Player.counters player) >= 10
             || Commander.lethalDamage pid gs
         )

-- CR 704.5h: a creature with toughness > 0 dealt damage by a deathtouch source
-- since the last SBA check is destroyed. "Deathtouch source" is read from the
-- event's deal-time bit (CR 702.2e last-known information), NOT re-derived now --
-- so a source that lost deathtouch (Humility) or left after dealing damage is
-- still judged by what it was.
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
-- The isCreature guard states CR 301.7a's distinction rather than relying on it.
-- An uncrewed Consulate Dreadnought is a permanent with printed numbers and no
-- toughness CHARACTERISTIC, and 704.5f/g must not touch it -- which
-- Projection.noncreaturePT already guarantees by answering Nothing for it.
--
-- CR 208.5 does not change that. Its substitution (Projection.noValuePT) is
-- itself creature-guarded, so a noncreature still arrives here with Nothing and
-- this conjunct is still the same by construction -- what 208.5 changed is that
-- a CREATURE now always arrives with a Just, making the toughness case below
-- exhaustive in practice rather than the place a no-value creature escaped.
-- Kept because the two remain different questions, and this one states CR
-- 704.5f's own premise: "if a CREATURE has toughness 0 or less".
--
-- Takes the object's already-projected characteristics (checkStateBasedActions
-- projects the whole board once, per CR 704.3 simultaneity).
zeroToughness :: PC.ProjectedCharacteristics -> Bool
zeroToughness pc =
  Set.member CardType.Creature (PC.cardTypes pc)
    && case PC.toughness pc of
      Nothing -> False
      Just t -> t <= 0

-- CR 704.5i: a planeswalker with loyalty 0 is put into its owner's graveyard. CR
-- 306.9 states the same rule from the card type's side.
--
-- A put-into-graveyard and NOT a destruction, the CR 704.5f shape rather than the
-- CR 704.5g one: ungated by indestructible and offering regeneration no
-- opportunity (CR 701.19a), which is why the classification below maps it to the
-- bury batch.
--
-- Takes the GameState as well as the projection, unlike zeroToughness above,
-- because CR 306.5c puts a permanent's loyalty in its COUNTERS and no layer
-- projects it. A planeswalker that never received counters reads 0 here and is
-- buried, which is the rule and not an accident: CR 306.5b gives a planeswalker
-- its counters as it enters, so it has them unless something removed them.
--
-- The card-type guard is load-bearing rather than defensive, and in the opposite
-- direction to zeroToughness's: Object.counters is keyed by kind for EVERY
-- permanent, so absent this guard every creature on the battlefield would read as
-- having loyalty 0 and be buried. CR 122.1e confines the reading to
-- planeswalkers.
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

-- CR 704.5n: an Equipment or Fortification attached to an illegal permanent or to
-- a player becomes unattached and remains on the battlefield.
--
-- The shape difference from CR 704.5m, which is why this is a separate
-- classification rather than another clause of fallsOff: an Aura DIES, an
-- Equipment merely DETACHES and stays (CR 301.5c says the same from the card
-- type's side).
--
-- "Illegal" is CR 301.5: an Equipment can be attached only to a creature. So a
-- host that is gone, or that is not a creature, is illegal -- and `pcs` answers
-- both at once, since an object no longer on the battlefield has no entry in it.
-- Reading the shared pre-pass projection (a per-object project here would
-- reintroduce the cubic sweep) means an Equipment whose creature dies THIS pass
-- detaches on the NEXT one, exactly as an Aura falls off on the next one.
--
-- CR 704.5n's "or to a player" clause is expressible but has no producer. Written
-- anyway, as the `Nothing` a player host yields from Recipient.objectOf -- there
-- is no host object to be a creature, so the rule detaches it.
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

-- CR 704.5p: a battle or creature attached to an object or player becomes
-- unattached and remains on the battlefield, and so does any nonbattle,
-- noncreature permanent that is neither an Aura, an Equipment, nor a
-- Fortification.
--
-- The complement of becomesUnattached, and the reason the two are separate
-- classifications rather than clauses of one: CR 704.5n asks whether the HOST is
-- still legal, and this asks whether the attached permanent may be attached to
-- anything at all. An Equipment animated while equipping a perfectly legal
-- creature is the case only this one catches (CR 301.5c). That rule's reconfigure
-- exception costs nothing here: CR 702.151b makes a reconfigure Equipment stop
-- being a creature until it becomes unattached, so it never reaches this branch
-- (and no card in the pool has reconfigure). The two share an ACTION -- detach,
-- stay on the battlefield -- so performStateBasedActions ORs them into one list.
--
-- Read off the PROJECTED characteristics, which is the whole point: the card
-- types this cases on are exactly what CR 613's layer 4 changes. "Aura" and
-- "Equipment" are subtypes (CR 205.3h, CR 301.5), so they come from PC.subtypes
-- -- unlike CR 704.5m's fallsOff, which asks the printed card for an enchant
-- ability because it needs that ability's spec.
--
-- Subtype.Fortification is the one clause of the rule with no constructor to case
-- on, and is therefore unreachable rather than elided. The BATTLE clause is
-- written out, though nothing reaches it: the pool has a battle, but CR 310.9
-- forbids attaching one and no effect in the pool tries. That is
-- becomesUnattached's posture toward CR
-- 704.5n's "or to a player" -- express the clause, and let the pool decide when it
-- fires. "Or to a player" needs no clause at all here: this rule asks only whether
-- the attached permanent may be attached to ANYTHING, so the `Just _` below covers
-- an object and a player alike.
--
-- This is also where CR 303.4d's second clause is enforced without being named:
-- an Aura that is also a creature is attached, so it detaches HERE and CR 704.5m's
-- fallsOff buries it on the next pass. The rule's "then" IS that pass boundary.
-- Only the state-based half lives here; the RESTRICTION half ("can't enchant
-- anything") is Pawl.Engine.Attach.attachmentFor's first Aura conjunct.
cannotBeAttached :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> Bool
cannotBeAttached pcs gs oid = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Object.attachedTo obj of
    Nothing -> False
    Just _ -> case Map.lookup oid pcs of
      Nothing -> False
      Just pc ->
        let isBattle = Set.member CardType.Battle (PC.cardTypes pc)
            isCreature = Set.member CardType.Creature (PC.cardTypes pc)
            isAura = Set.member Subtype.Aura (PC.subtypes pc)
            isEquipment = Set.member Subtype.Equipment (PC.subtypes pc)
         in -- Written as the rule's two sentences rather than as the smaller
            -- equivalent `isBattle || isCreature || not (isAura || isEquipment)`,
            -- so each half can be checked against the CR on its own. The
            -- `not isBattle` and `not isCreature` conjuncts are the second
            -- sentence's own "nonbattle, noncreature" qualifiers -- and they are
            -- what makes the two sentences differ at all, since a battle that is
            -- also an Aura detaches by the first and not by the second.
            isBattle || isCreature || (not isBattle && not isCreature && not isAura && not isEquipment)

-- CR 704.5m: an Aura attached to an illegal object or player, or attached to
-- nothing, is put into its owner's graveyard. Three clauses: unattached, attached
-- to an id that is no longer a permanent, and attached to one its own enchant
-- ability no longer admits (CR 303.4c).
--
-- ABILITIES, plural, where CR 702.5c applies: Card.enchantSpec is the conjunction
-- of every instance, so the third clause fires when the host stops matching ANY
-- of them. Nothing here has to know how many there were.
--
-- CR 303.4c's own wording splits that last clause differently, and both halves
-- land in the SAME place here because a pool's candidate list already excludes
-- them: Target.creatureRecipients scans the battlefield of players still in the
-- game, and Target.playerRecipients IS Game.stillPlaying. So an enchant-player
-- Aura (CR 702.5d) whose player has left is illegal by the same test that judges
-- every other Aura, with no player-only branch.
--
-- The third clause goes through stillLegalEnchant below rather than calling
-- Target.stillAdmitted directly, so that the common enchant spec is answered off
-- `pcs` -- the SAME pre-pass Projection.projectAll performStateBasedActions
-- computed once for every other CR 704.3 classification. Calling stillAdmitted
-- here instead means a fresh `gather` PER Aura, which is the O(permanents^3)
-- shape Projection.hs's `liveGiven` comment warns about, one level down. (A spec
-- carrying a Filter still reaches stillAdmitted by that function's fallthrough.)
--
-- CR 303.4d's first clause -- an Aura can't enchant itself -- is the `oid == self`
-- arm. Unreachable in this pool, written anyway because it costs one comparison.
-- Its SECOND clause is the "unattached" arm here plus cannotBeAttached above:
-- that rule detaches the animated Aura on one pass, and this buries it on the
-- next, which is the order CR 303.4d states.
--
-- A put-into-graveyard, NOT a destruction, so this goes through Event.changeZone
-- and consults neither indestructible (CR 702.12b) nor a regeneration shield (CR
-- 701.19a).
fallsOff :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> Bool
fallsOff pcs gs oid = case Game.faceOf oid gs of
  Nothing -> False
  Just face -> case Card.enchantSpec face of
    Nothing -> False
    Just spec -> case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> case Object.attachedTo obj of
        Nothing -> True
        Just recipient ->
          Recipient.objectOf recipient == Just oid
            || not (stillLegalEnchant pcs gs oid spec recipient)

-- CR 303.4c: is `recipient` still one the enchanting Aura `source`'s spec ADMITS?
-- Answered off `pcs` -- the pre-pass projection every other classification in
-- performStateBasedActions shares (CR 704.3 simultaneity) -- for the one spec
-- shape that reduces to a lookup, and by the general Target.stillAdmitted for
-- every other.
--
-- Admission, NOT target legality: CR 303.4c asks about an illegal object or
-- player as defined by the enchant ability, which rule 702's TARGETING
-- restrictions do not speak to. Protection would bury this Aura, but by its own
-- separate clause (CR 702.16c), while shroud (CR 702.18) and hexproof (CR 702.11)
-- restrict targeting and nothing else -- so an Aura stays attached to a host that
-- gains either. See Target.admittedRecipients.
--
-- Pool.Creatures with no Filter is the shape MOST folded enchant specs in this
-- pool carry (Unholy Strength) -- Card.enchantSpec leaves a lone unfiltered
-- instance exactly as printed, which is what keeps this arm reachable at all. Target.creatureRecipients tags every candidate
-- ToCreature, drawn from the battlefield objects owned by a still-playing player,
-- so with no Filter to narrow that set "still legal" reduces EXACTLY to "still a
-- creature, on the battlefield, owned by a player still in the game" -- a
-- Map.lookup on `pcs` plus one owner check. Not an approximation: `pcs Map.!
-- target`, when it exists, IS `Projection.project target gs`, and a missing key
-- means what creatureRecipients' own battlefield scan would have missed it for.
--
-- Any OTHER shape falls through to the general, slower Target.stillAdmitted,
-- which reuses the SAME pool and Filter Cast/Resolve already judge, rather than
-- assuming the Creatures-with-no-Filter shape holds regardless. Two producers
-- need it: Setessan Training's "Enchant creature you control" carries a Filter
-- whose ControlledBy You conjunct is unanswerable from `pcs` -- CR 109.5 makes
-- that "you" the AURA's controller (CR 702.5a), so the answer changes when an
-- opponent steals the enchanted creature -- and CR 702.5d's enchant-player Auras
-- carry a Pool.Players spec. A CR 702.5c conjunction of several instances is a
-- third: Filter.And is a Filter, so it lands here too. The fallthrough pays the per-Aura re-projection the
-- reduction exists to avoid, but only for those. Serving a filtered spec off
-- `pcs` would mean answering Filter.matches against the pre-pass projection
-- instead of a fresh one, which is #430.
--
-- That fallback is general in its recipient TAG as well as in its pool and
-- filter, which is the whole reason Object.attachedTo stores a Recipient: the tag
-- is the one the Aura's own pool produced when it attached, with nothing here to
-- keep in step. So the fast arm is matched on the PAIR, not on the spec alone --
-- it reduces Pool.Creatures' candidate list specifically, and a recipient of any
-- other shape falls through rather than being read as an object id it is not.
stillLegalEnchant :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> TargetSpec.TargetSpec -> Recipient.Recipient -> Bool
stillLegalEnchant pcs gs source spec recipient = case (spec, recipient) of
  (TargetSpec.MkTargetSpec Pool.Creatures Nothing count, Recipient.ToCreature target) | count == TargetCount.one ->
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
-- CR 707.2 copies name and supertype alike.
--
-- Grouped by name per controller, since the rule says "controlled by the same
-- player": two players each with a Thalia is not its business.
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
-- rest -- the permanents this pass must put into their OWNERS' graveyards.
--
-- Asks but does not move, and that separation is the rule rather than tidiness.
-- CR 704.3 performs every applicable state-based action simultaneously, so the
-- choice has to be made against the state as the pass began -- including a group
-- member that another action is ALSO about to bury. Keeping that one is a legal
-- choice; dropping it from the candidates would decide for the player and could
-- leave a copy alive that they chose to lose.
--
-- A plain put-into-graveyard, not a destruction, so the caller consults neither
-- indestructible (CR 702.12b) nor a regeneration shield.
--
-- FILTERED, NOT TRUSTED: an answer naming a permanent outside the group would
-- otherwise bury the whole group, so it falls back to the head.
chooseLegendVictims :: (PlayerId, NonEmpty.NonEmpty ObjectId) -> Game [ObjectId]
chooseLegendVictims (controller, candidates) = do
  gs <- State.get
  answer <- Game.choose (Prompt.ChooseLegend (Decide.deciderFor controller gs) controller candidates)
  let kept = if List.elem answer (NonEmpty.toList candidates) then answer else NonEmpty.head candidates
  pure (filter (/= kept) (NonEmpty.toList candidates))

-- CR 704.5k: with two or more world permanents, all but the one that has been
-- world for the shortest time are put into their owners' graveyards, and on a tie
-- for shortest all of them are. So: every world permanent but the last one to
-- have become world.
--
-- The neighbouring legend rule (CR 704.5j) differs in three ways, each a place
-- this could have been wrongly copied from it:
--
-- 1. It ASKS; this does not. CR 704.5k decides by the clock, and that answer is a
--    fact about the board rather than a choice, so prompting here would be the
--    engine inventing a decision the rules never offer. Not an elision.
-- 2. It is scoped to one controller and one name; this is scoped to neither, so
--    two players each with a world permanent -- a board the legend rule leaves
--    alone -- is exactly the case this rule fires on.
-- 3. It has no tie clause; this one does, and the tie is REACHABLE:
--    Engine.sampleWorldSince stamps everything that became world in one settle
--    pass with ONE timestamp, so two permanents made world simultaneously compare
--    equal and both are buried.
--
-- The CLOCK is Object.worldSince -- when the permanent became world -- and NOT
-- Object.timestamp, which is when it entered the battlefield (CR 613.7d). The two
-- part company whenever layer 4 grants the supertype to a permanent already on
-- the battlefield (CR 613.1d, Modification.AddSupertype), and a permanent that
-- entered EARLIER but became world LATER is the survivor. Pawl.DamageSpec's
-- "CR 704.5k the clock is when it became world, not when it entered" is that
-- board.
--
-- An unstamped world permanent is not a candidate. Unreachable rather than
-- tolerated: Engine.sampleWorldSince runs ahead of every pass this module has,
-- both inside Engine.performSettle and at Engine.checkSba, so anything world here
-- has a stamp. Deliberately NOT defaulted to
-- Object.timestamp, which would restore the proxy on exactly the boards the clock
-- exists for.
--
-- Read off the PROJECTION rather than the printed type line, for the reason
-- legendGroups is: CR 707.2 makes a copy of a world permanent world too.
--
-- CR 801.12 narrows this rule to a controller's range of influence. CR 801.1
-- makes limited range of influence an OPTION, and pawl has no representation for
-- a multiplayer option at all (#175), so the narrowing is inert.
--
-- A put-into-graveyard, NOT a destruction, so the caller consults neither
-- indestructible (CR 702.12b) nor a regeneration shield.
worldVictims :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> [ObjectId]
worldVictims pcs gs =
  let stamped oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          | Set.member Supertype.World (PC.supertypes pc) ->
              fmap (\ts -> (ts, oid)) (Game.lookupObject oid gs >>= Object.worldSince)
          | otherwise -> Nothing
      worlds = Maybe.mapMaybe stamped (Set.toList (GameState.battlefield gs))
   in case fmap fst worlds of
        [] -> []
        ts : rest ->
          -- The SHORTEST amount of time is the LARGEST timestamp: the last one to
          -- become world has been world for the least time.
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
-- use.
checkStateBasedActions :: Game ()
checkStateBasedActions = Monad.void performStateBasedActions

-- One SBA pass, also reporting whether any state-based action was PERFORMED (a
-- creature buried or a player departed). CR 704.3: the caller repeats the check
-- while that flag is True. The flag lets the CR 117.5 settle loop (Engine) decide
-- whether to repeat WITHOUT a deep GameState comparison.
--
-- Monadic because CR 704.5f's put-into-graveyard and CR 704.5g's destruction both
-- go through funnels that can raise a CR 616 replacement prompt: a creature dying
-- with two applicable death-replacements must ask its controller which to apply.
--
-- The whole pass is ONE event, which Event.simultaneously stamps on everything it
-- records: CR 704.3 performs all applicable state-based actions "simultaneously
-- as a single event". That is the rule every classification below already rests
-- on -- each is computed from the SAME pre-pass board -- so the bracket only
-- carries into the event log a fact this function has always asserted. What it
-- buys is CR 603.10a's look-back: a permanent this pass buries is on the board a
-- permanent it buries alongside is read against.
--
-- The bracket spans the whole pass and not each victim, which is the rule's own
-- scope: the put-into-graveyard batch, the destruction batch and the departures
-- are one check, and splitting them would make CR 704.3's "simultaneously" a
-- sequence again.
performStateBasedActions :: Game Bool
performStateBasedActions = Event.simultaneously $ do
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
      -- CR 704.5j, from the SAME pre-pass state as every classification above.
      -- Deliberately NOT filtered against the buries below: see
      -- chooseLegendVictims for why a member that is dying anyway must stay on
      -- the ballot.
      legendsToResolve = legendGroups pcs gs
      -- CR 704.5k, from the SAME pre-pass state, for the same CR 704.3 reason.
      -- Unlike the legend rule this one asks nobody.
      worldLosers = worldVictims pcs gs
      -- CR 704.5z, from the SAME pre-pass state again. Lives in
      -- Pawl.Engine.Speed with the rest of rule 702.179 rather than here, the way
      -- Pawl.Engine.Monarch keeps rule 725's settle-loop work: this module owns
      -- WHEN a state-based action is checked, not what each one means.
      revving = Speed.startingEngines pcs gs
      -- CR 704.5s / 714.4, from the SAME pre-pass state as everything above.
      -- Lives in Pawl.Engine.Saga with the rest of rule 714, the way CR 704.5z
      -- lives in Pawl.Engine.Speed.
      --
      -- The unscanned event log is an input because CR 704.5s exempts a Saga whose
      -- chapter ability "has triggered but not yet left the stack", and the window
      -- where such an ability has triggered but is not yet ON the stack is real:
      -- Engine.performSettle runs this pass before placePendingTriggers. See
      -- Saga.awaitingChapter.
      --
      -- CR 101.2's prohibition is subtracted HERE and not left to the funnel's own
      -- gate, and that is a TERMINATION argument rather than a tidiness one: a
      -- finished Saga that can't be sacrificed would keep `acted` below True
      -- forever while moving nothing, and CR 704.3's "repeat until no
      -- state-based action is performed" would never come to rest. Nothing is
      -- PERFORMED for such a Saga, so nothing is reported -- which is also what
      -- the rule says, since CR 101.2 stops the sacrifice from happening at all.
      --
      -- No board in the pool reaches it, so this is argued rather than tested:
      -- the pool's one prohibition (Garland, Royal Kidnapper) names CREATURES,
      -- and no printing here is both a Saga and a creature. The guard stands
      -- because the failure it prevents is a hang rather than a wrong answer.
      told =
        filter
          (\(_, oid) -> not (SacrificeRestriction.prohibited oid gs))
          (Saga.sacrificing (\oid -> Projection.controllerOf oid gs) pcs (Event.unscannedEvents gs) gs)
      -- CR 704.5v / 310.7, from the SAME pre-pass state as everything above, and
      -- living in Pawl.Engine.Battle with the rest of rule 310 the way CR 704.5s
      -- lives in Pawl.Engine.Saga.
      --
      -- The unscanned event log is an input for the reason CR 704.5s's is: the rule
      -- exempts a battle whose ability "has triggered but not yet left the stack",
      -- and this pass runs before placePendingTriggers. See Battle.awaitingAbility.
      routed = Battle.defeated pcs (Event.unscannedEvents gs) gs
      -- CR 704.5w / 704.5x: the battles whose protector designation has become
      -- illegal, paired with the projection and controller the re-choice needs.
      -- What "illegal" means is CR 310.10, and it lives in Pawl.Engine.Battle with
      -- the rest of rule 310, the way CR 704.5s lives in Pawl.Engine.Saga.
      --
      -- Computed from the SAME pre-pass pcs/gs as every classification above, so
      -- a battle whose protector is a player this pass is about to remove is
      -- judged against the board the pass began in (CR 704.3).
      undefendedOne oid = do
        pc <- Map.lookup oid pcs
        Monad.guard (Battle.isBattle pc)
        controller <- Projection.controllerOf oid gs
        Monad.guard (Battle.needsProtector pc controller (Game.stillPlaying gs) (Battle.isBeingAttacked oid gs) (Object.protector =<< Game.lookupObject oid gs))
        pure (oid, pc, controller)
      undefended = Maybe.mapMaybe undefendedOne onBattlefield
  -- CR 704.5j: the legend rule is one of the two state-based actions that ASK --
  -- CR 310.10's protector re-choice below is the other -- and both ask BEFORE
  -- anything below moves, so every choice is made against the state this pass
  -- began in (CR 704.3's simultaneity).
  legendVictims <- fmap concat (Monad.mapM chooseLegendVictims legendsToResolve)
  -- CR 704.5w / 704.5x: the other state-based action that ASKS, in the same window
  -- and for the same reason as the legend rule above. A battle for which no player
  -- can be chosen joins the put-into-graveyard batch below, which is what the
  -- second sentence of each of those rules, and of CR 310.10, says.
  --
  -- A battle this pass is ALSO about to move is still asked, which CR 704.3 is
  -- what settles: the actions are simultaneous, so neither is conditioned on the
  -- other having been skipped. The stamp is then erased by Object.newIncarnation
  -- when the move lands, so the cost is a decision-log entry rather than a wrong
  -- board -- the same posture chooseLegendVictims takes toward a member that CR
  -- 704.5f is burying anyway.
  redesignated <- Monad.mapM (\(oid, pc, controller) -> fmap ((,) oid) (Battle.designateProtector pc controller oid)) undefended
  -- The chosen protectors are stamped BEFORE the batch below moves anything, so a
  -- battle that found one is not also read as one that did not. Not a zone change
  -- and not an event: CR 310.8a's designation is a mark on the object, and neither
  -- rule 704.5w nor 704.5x makes it trigger anything.
  State.modify' (\g -> g {GameState.objects = List.foldl' (\m (oid, picked) -> Map.adjust (\o -> o {Object.protector = picked}) oid m) (GameState.objects g) redesignated})
  -- CR 903.9a: the THIRD state-based action that asks, in the same window and for
  -- the same reason as the two above -- a commander that reached a graveyard or
  -- exile since the last check, offered to its owner for the command zone.
  --
  -- Asked before anything below moves, so the offer is made against the board this
  -- pass began in. A commander that is ALSO about to be moved by the batch below
  -- cannot arise: rule 903.9a only reaches objects already in a graveyard or in
  -- exile, and every batch below moves things OFF the battlefield.
  returningCommanders <-
    fmap Maybe.catMaybes . Monad.forM (Commander.returnable gs) $ \(owner, oid) -> do
      decision <- Game.choose (Prompt.ReturnCommander (Decide.deciderFor owner gs) owner oid)
      pure $ case decision of
        CommandZoneDecision.Returns -> Just oid
        CommandZoneDecision.Leaves -> Nothing
  -- CR 310.10's second sentence: "if no player can be chosen this way, the battle
  -- is put into its owner's graveyard". NOT EXERCISED by any test, and not for want
  -- of trying -- a Siege's candidates are its controller's opponents still in the
  -- game, so reaching this needs a game whose battle's controller has no opponent
  -- left, and CR 104.2a ended that game already. Kept because the alternative is a
  -- battle that sits at no protector forever, re-asked and unanswerable every pass
  -- (#853).
  let undefendable = Maybe.mapMaybe (\(oid, picked) -> if Maybe.isNothing picked then Just oid else Nothing) redesignated
  -- Every put-into-graveyard this pass performs, as ONE deduplicated batch:
  -- CR 704.5f (toughness <= 0), CR 704.5i (loyalty 0), CR 704.5j (the legend
  -- rule's losers), CR 704.5k (the world rule's), CR 704.5m (an Aura attached to
  -- nothing), CR 704.5v (a battle at defense 0) and CR 704.5w/704.5x (a battle no
  -- player can protect). None of the seven is a destruction, so none consults
  -- indestructible or a regeneration shield.
  --
  -- Deduplicated because the sets overlap: a legend at 0 toughness whose
  -- controller kept a DIFFERENT copy is named by 704.5f and 704.5j alike, and
  -- moving it twice would emit a second zone-change event and fire its
  -- dies-triggers again. 704.5f and 704.5k overlap reachably too, via Opalescence
  -- plus Night of Souls' Betrayal on a world enchantment.
  --
  -- Moved as a BATCH on the pre-pass state, not one move at a time on the live
  -- board, for the same CR 704.3 reason every classification above was computed
  -- from `gs`: each member's CR 616.1 loop collects its replacement candidates
  -- from the board the pass began in, so an animated Rest in Peace this pass is
  -- itself burying still exiles the cards the rest of the batch would put into
  -- graveyards. See Pawl.Engine.Replacement's applyReplacementsIn.
  Monad.mapM_ (\oid -> Event.changeZoneInBatch gs oid Zone.Graveyard) (List.nub (toGraveyard <> legendVictims <> worldLosers <> unattachedAuras <> undefendable <> routed))
  -- CR 903.9a's ACTION half: "its owner may put it into the command zone". A real
  -- zone change (CR 400.7 mints a fresh incarnation), so it goes through the same
  -- batch funnel as the buries above rather than editing the zone sets -- a
  -- commander leaving a graveyard is a zone change like any other, and anything
  -- watching for one must see it.
  Monad.mapM_ (\oid -> Event.changeZoneInBatch gs oid Zone.Command) returningCommanders
  -- CR 704.5n / 704.5p: the Equipment does NOT follow its creature -- it detaches
  -- and stays. Not a zone change, so unlike the Aura above it does not funnel
  -- through Pawl.Engine.Event: no Moved event, no replacement, no trigger.
  State.modify' (\g -> g {GameState.objects = List.foldl' (\m oid -> Map.adjust (\o -> o {Object.attachedTo = Nothing}) oid m) (GameState.objects g) detaching})
  -- CR 704.5g/h: destruction through the funnel, Regenerable -- the point rather
  -- than a default, since CR 701.19a's shield exists to replace exactly this
  -- destruction.
  --
  -- A permanent the legend rule or the world rule already buried is excluded
  -- rather than left to no-op on a dead id: CR 704.5j and CR 704.5k are
  -- put-into-graveyards, not destructions, so neither offers the shield an
  -- opportunity nor may consume one here.
  --
  -- ONE batch, not one call per victim, and on the SAME pre-pass board as the
  -- put-into-graveyard batch above, because CR 704.3 makes the two halves one
  -- event. Splitting them is an implementation order, not a rules one, so a
  -- replacement effect belonging to a permanent buried above is still in force
  -- for a destruction here.
  --
  -- The funnel's own CR 702.12b gate is judged against that same board too, so it
  -- asks what `destroyedBySba` asked above rather than second-guessing it from a
  -- board the buries have already changed. Only the funnel's existence filter is
  -- live, which is what keeps a permanent the buries already moved from being
  -- offered a destruction that never happens (CR 614.7). TWO members are left to
  -- that filter rather than excluded by name here: CR 704.5m's Aura, and CR
  -- 310.10's undefendable battle -- an animated Siege with lethal damage whose
  -- protector has just left is in both `toDestroy` and `undefendable`.
  Event.destroyInBatch gs Regenerability.Regenerable (filter (\oid -> List.notElem oid legendVictims && List.notElem oid worldLosers) toDestroy)
  -- CR 704.5s / 714.4: the Saga's controller SACRIFICES it. Neither a
  -- put-into-graveyard nor a destruction, so it joins neither batch above -- CR
  -- 701.21a is its own game action, ungated by indestructible and offering
  -- regeneration nothing, and it goes through Pawl.Engine.Event.sacrifice, the one
  -- funnel for it.
  --
  -- One call per Saga on the LIVE board, unlike the two batches above, because
  -- that funnel takes one object and re-reads the state. The difference is
  -- unobservable here in a way it would not be for the batches: a sacrifice moves
  -- only the Saga named, and the classifier already fixed which Sagas and whose
  -- from the pre-pass board -- so nothing this pass does can add one, and the
  -- funnel's own existence check is what drops one another action already moved.
  --
  -- A board with two finished Sagas is where a batched version would differ, and
  -- only if one of them replaced the other's move; no card in the pool does (#842).
  Monad.mapM_ (uncurry Event.sacrifice) told
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
          -- Object.zone and NOT GameState.battlefield, which is the one place in
          -- the engine where the two must disagree: CR 702.26d says "tokens
          -- continue to exist on the battlefield while phased out", and a
          -- phased-out permanent is absent from that set (CR 702.26b). Reading the
          -- set here would make a phasing token cease to exist the moment it
          -- phased out.
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
      -- CR 704.5z: "that player's speed becomes 1", applied to the players
      -- classified from the pre-pass board above. Applied LATE like every other
      -- action in this pass, so a permanent this same pass destroyed still starts
      -- its controller's engines -- CR 704.3 makes the whole check one event.
      revved = List.foldl' (flip Speed.startEngines) balanced revving
      -- A state-based action was performed iff any of the classifications above
      -- named something. A regenerated creature still counts as destroyed, which
      -- the CR 704.3 settle loop re-checks and -- because the regen healed the
      -- damage -- terminates.
      acted = not (null legendVictims) || not (null worldLosers) || not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations) || not (null unattachedAuras) || not (null detaching) || not (null revving) || not (null told) || not (null undefended) || not (null returningCommanders)
  -- CR 104.1: a game ends the moment a result is reached, so a later pass may
  -- not replace one. The existing result therefore wins; this pass only settles
  -- an outcome when the game did not already have one. Same ordering as
  -- Departure.leaveGame -- the two doors that write GameState.result agree.
  State.put revved {GameState.result = GameState.result revved <|> outcome}
  pure acted
