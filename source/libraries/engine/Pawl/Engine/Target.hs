module Pawl.Engine.Target where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Defender as Defender
import qualified Pawl.Engine.Exile as Exile
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Binding as Binding.Type
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Decider (Decider)
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.ModeIndex (ModeIndex)
import qualified Pawl.Types.ModeIndex as ModeIndex
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Pile as Pile
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.Quantity (Quantity)
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotCount as SlotCount
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.TargetCount as TargetCount
import Pawl.Types.TargetSlot (TargetSlot)
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone

-- CR 115: a target slot's legal recipients -- the set the slot itself admits
-- (admittedRecipients below), less every candidate rule 702 forbids TARGETING
-- (targetable below, where shroud, hexproof and the restrictions after them
-- live), less CR 115.5's one candidate, a spell or ability on the stack being an
-- illegal target for itself.
--
-- CR 115.5 is subtracted HERE and not in admittedGiven because it is a TARGETING
-- rule, exactly as rule 702's restrictions are: what an enchant slot admits (CR
-- 303.4c, Sba.stillLegalEnchant) asks no targeting question at all. Both of CR
-- 115's moments honour it, since both route through this function -- CR 601.2c's
-- choosing and CR 608.2b's re-validation.
--
-- ITS GATE IS THE RULE'S OWN WORDS, "on the stack": `source` is the object on the
-- stack only for a SPELL. A permanent's activated ability passes the source
-- PERMANENT, so this subtracts nothing there -- the right answer rather than a
-- happy accident, since the ability and not the permanent is the object CR 115.5
-- speaks of, so Prodigal Sorcerer may still ping itself.
--
-- An ability targeting ITSELF is excluded EARLIER, and not by declining to ask:
-- Activate.activateAbility and Engine.placeBorne mint the ability onto the stack
-- and then choose its targets against the snapshot they bound BEFORE minting it,
-- so the ability is absent from the state abilityRecipients draws the pool from.
-- CR 602.2a and CR 603.3d put the ability on the stack first, so that snapshot is
-- not the state those rules describe; the two differ by exactly the ability
-- object, which this rule would subtract anyway, so no board tells them apart.
--
-- THOSE TWO FACTS ARE COUPLED, and the coupling is why the snapshot must not be
-- "tidied" on its own. This function's gate reads `source`, which on the ability
-- path is the permanent and never on the stack -- so a caller moved to the
-- post-mint state without also being given the ability's own id would offer the
-- ability itself, which is a real CR 115.5 violation where the snapshot is only a
-- harmless deviation. Correcting it means a second frame here (the object ON THE
-- STACK, distinct from the object targeting is relative to) threaded through
-- legalSets, selectionLegal, chooseTargets, fillableModes and Resolve's CR 608.2b
-- re-check. Pawl.TargetSpec's "CR 602.2a/115.5 Adric's ability is not offered its
-- own object among the abilities it may counter" fences the half-way state.
--
-- The two frames are SEPARATE, and keeping them apart is the whole point:
--
--   * `source` is the object the targeting is relative to -- the spell object at
--     cast, the source permanent for an ability. It frames only CR 601.2c's
--     "another", carried as the Filter's own Not IsSource (#163), so that drops
--     whichever tag the Pool produced and re-validation sees the same rule
--     selection did.
--   * `perspective` is CR 109.5's "you", supplied by the caller. "A creature an
--     opponent controls" is a ControlledBy Opponent filter, and a player
--     candidate is narrowed by the same fold through the IsPlayer atom (#168).
--
-- Deriving the perspective here as `Projection.controllerOf source` instead
-- returns Nothing once the source leaves the battlefield, making ControlledBy
-- vacuously False and the whole set empty -- so CR 608.2b's re-check would fizzle
-- an ability whose source was merely killed in response, when that rule says to
-- use last known information. The controller is knowable when the source is not,
-- because an ability is its own object on the stack: resolution-path callers read
-- it from that object's stamped owner (CR 113.8), and cast/activate-path callers
-- already hold the acting player.
--
-- Maybe, not PlayerId, matching Filter.MkContext's own field: Nothing is a
-- genuinely absent perspective, which leaves a player-referencing filter
-- vacuously False.
--
-- NO SLOT BINDINGS, which is what makes this the wrapper rather than the
-- primitive: a GraveyardScope.InSlot pool asks what another slot holds, and this
-- entry point answers that it holds nothing. Empty is the honest answer for a
-- slot map that was never supplied, and it is the vacuous posture every
-- player-referencing question here takes. legalSets below is where the bindings
-- come from at CR 601.2c, and Pawl.Engine.Resolve's stillLegal calls are where
-- they come from at CR 608.2b; a caller with a slot-scoped pool and neither must
-- use legalRecipientsGiven directly.
legalRecipients :: Maybe PlayerId -> ObjectId -> TargetSlot -> GameState -> Set Recipient
legalRecipients perspective source slot gs =
  let pcs = Projection.projectAll gs
   in legalRecipientsGiven pcs (Projection.controlGrants gs) (poolsGiven pcs gs) perspective False Map.empty source slot gs

-- The same set given a board the CALLER has already walked. `pcs` and `grants`
-- are one whole-board projection and one control-grant walk, and threading them
-- in is what makes the whole ENUMERATION take one of each rather than one per
-- slot per ability per permanent (#716): the wrapper above hoists per CALL, and
-- Action.legalActions' mode-fillability gate calls it once per permanent.
--
-- It changes no answer, for the reason at Projection.projectGiven: the board is
-- a snapshot of one GameState, and both this and its caller are pure functions
-- of the same one.
--
-- Both stay THUNKS here. `pcs` the caller has usually already forced, but
-- `sourceView` below must not become strict -- see its own note.
--
-- `pools` is the third of the same kind and the one the wrapper above could not
-- share at all: every base pool but the graveyard's is a function of `gs` alone,
-- so building one per slot per ability made the enumeration walk the battlefield
-- once per ability even after #716 threaded the projections in (#1073). See
-- Pools.
--
-- `bindings` is the ANNOUNCEMENT's whole binding environment -- what its other
-- slots hold, which a GraveyardScope.InSlot pool is resolved against (see
-- graveyardRecipients), plus the NUMBERS the announcement holds -- CR 603.2's
-- event amount and CR 601.2b's X alike -- which a CR 202.3 computed bound reads
-- (see slotContext). A whole Binding rather than the recipients alone because the
-- second half is not a recipient at all.
--
-- `unannounced` is CR 700.2a's fillability gate saying it is asking ahead of the
-- announcement, and the only thing it changes is that a computed bound reading a
-- number the seed cannot supply states no bound rather than an unmeetable one
-- (Filter.boundUnannounced). True at exactly one call site,
-- fillableModesGiven's, which is where the argument for it lives; False
-- everywhere else, which leaves such a bound vacuously False. That is right at CR
-- 601.2c and CR 608.2b, where the announcement is made and answers it, and it is
-- what makes Pawl.Engine.Activate's pre-X call a KNOWN gap rather than a silent
-- widening (#2672).
legalRecipientsGiven :: Map ObjectId PC.ProjectedCharacteristics -> [Projection.ControlGrant] -> Pools -> Maybe PlayerId -> Bool -> Map SlotName Binding.Type.Binding -> ObjectId -> TargetSlot -> GameState -> Set Recipient
legalRecipientsGiven pcs grants pools perspective unannounced bindings source slot gs =
  -- The SAME thunk both halves read, so the whole-board projection is taken at
  -- most once per slot even when this is reached through the wrapper above, and
  -- once per enumeration when it is not (admittedGiven's own note).
  let -- CR 115.5's gate, hoisted out of the fold: one scan of the stack per
      -- slot rather than one per candidate.
      sourceOnStack = elem source (GameState.stack gs)
      -- CR 702.11d's "[quality] spells ... or abilities ... from [quality]
      -- sources" -- the SOURCE's characteristics, which no other targeting
      -- question here reads. `source` is already the object rule 702.11d names in
      -- both halves: the spell object for a spell, and for an ability the object
      -- CR 113.7 says generated it, which is the permanent this caller passes.
      --
      -- Hoisted like `pcs`, so one slot takes one view rather than one per
      -- candidate; and a THUNK, so a slot with no "hexproof from" candidate on it
      -- -- which is every slot on almost every board -- pays for neither the view
      -- nor the control-grant walk it would force. `grants` arriving as a
      -- parameter does not change that: the wrapper above passes an unforced
      -- Projection.controlGrants, and the enumeration passes one it has already
      -- paid for. That is the same posture opponentOf's controller read takes
      -- below.
      sourceView = Projection.viewOfObjectGiven pcs grants source gs
      keep recipient =
        not (sourceOnStack && Recipient.objectOf recipient == Just source)
          && targetable pcs perspective source sourceView gs recipient
   in Set.filter keep (admittedGiven pcs grants pools perspective unannounced bindings source slot gs)

-- CR 115.1 / CR 303.4c / CR 701.3a: the recipients the SLOT itself admits -- its
-- Pool's base candidate set (CR 115.4's "any target" is creatures, planeswalkers
-- and battles on the battlefield plus players still in the game) narrowed by its
-- Filter. Rule 702's targeting restrictions are NOT applied.
--
-- Separate from legalRecipients because "can't be the target of" and "is an
-- illegal object to be attached to" are different questions, and rule 702 says so
-- itself: protection states both halves separately (CR 702.16b for targeting, CR
-- 702.16c for attachment), while shroud (CR 702.18) and hexproof (CR 702.11)
-- state only the first -- so an Aura already attached to a permanent that has
-- shroud stays attached, and Attach.attachmentFor may move one onto it.
--
-- Hence the two callers here rather than at legalRecipients:
-- Sba.stillLegalEnchant's general path (CR 303.4c) and Attach.attachmentFor (CR
-- 701.3a). Both ask what the enchant SLOT admits; neither is a player choosing a
-- target.
--
-- No slot bindings, and neither caller can want any: both ask what an ENCHANT
-- slot admits, and the slot CR 303.4a declares draws from the battlefield,
-- which no GraveyardScope reaches.
admittedRecipients :: Maybe PlayerId -> ObjectId -> TargetSlot -> GameState -> Set Recipient
admittedRecipients perspective source slot gs =
  let pcs = Projection.projectAll gs
   in admittedGiven pcs (Projection.controlGrants gs) (poolsGiven pcs gs) perspective False Map.empty source slot gs

admittedGiven :: Map ObjectId PC.ProjectedCharacteristics -> [Projection.ControlGrant] -> Pools -> Maybe PlayerId -> Bool -> Map SlotName Binding.Type.Binding -> ObjectId -> TargetSlot -> GameState -> Set Recipient
admittedGiven pcs grants pools perspective unannounced bindings source slot gs =
  let pool = TargetSlot.pool slot
      narrowing = TargetSlot.filter slot
      context = slotContext pcs perspective unannounced bindings source (TargetSlot.amount slot) gs
      -- ONE whole-board projection and ONE control-grant walk for the whole
      -- slot: both the base pool's creature test and the Filter's per-candidate
      -- view are asked of every object on the battlefield, and each was a fresh
      -- Projection.gather (#200). The snapshot argument is at
      -- Projection.projectGiven, and holds here because this is a pure function of
      -- one GameState.
      --
      -- `pcs` and `grants` are both the CALLER's thunks so that
      -- legalRecipientsGiven's restriction pass and this admission pass share one
      -- projection and one grant walk rather than taking two of each -- and, one
      -- level further out, so that a whole enumeration shares one of each (#716).
      -- Thunks, so a slot that asks neither question pays for neither.
      keep recipient = case recipient of
        -- CR 115.1: a player candidate is narrowed too ("target opponent"), by a
        -- Filter that asks about the player rather than about an object -- the
        -- IsPlayer atom (#168). Every object-shaped atom is vacuously False
        -- against a player view, so a slot that says "target creature you
        -- control" cannot accidentally admit a player.
        --
        -- Through Count.playerView rather than Filter.playerView so that the one
        -- atom CR 120.1 lets a player answer -- was this seat dealt damage this
        -- turn (Needle Drop) -- is read off the board here as it is in the
        -- Scope.OverPlayers fold, rather than going vacuously False.
        Recipient.ToPlayer pid -> against (Count.playerView gs pid)
        Recipient.ToCreature oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
        Recipient.ToPlaneswalker oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
        Recipient.ToBattle oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
        Recipient.ToObject oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
        -- Unreachable: no pool holds a pile. CR 406.4's pile is substituted into
        -- the OFFER by piledOffer, after this narrowing has run over the cards
        -- it stands for.
        Recipient.ToPile _ -> False
      against view = case narrowing of
        Nothing -> True
        Just f -> Filter.matches context view f
   in Set.filter keep (basePoolGiven pools context (Binding.targetsOf bindings) pool gs)

-- THE Filter.Context a target SLOT's own filter is matched against. Extracted
-- from admittedGiven so that CR 608.2h's departed host (lastKnownAdmits below)
-- is judged against the same context a live candidate is, rather than against a
-- bare Filter.contextFor that would answer differently for the atoms filled
-- here.
--
-- THE one site that fills Filter.sourcePower and Filter.slotAmount, and one of
-- the two that fill Filter.defendingPlayer, because it is the one site that
-- matches a TARGET SLOT's Filter -- both of CR 115's moments (CR 601.2c's
-- choosing and CR 608.2b's re-check) reach those atoms through here, and CR
-- 702.134a, CR 702.39a and CR 202.3's computed bound are the only clauses that
-- write them in a slot. CR 508.1c's gate is where the other defending-player
-- read is (Pawl.Engine.CombatRestriction.inForce, Armored Galleon), and no
-- target slot can reach it. All are thunks, like the caller's `pcs`: a slot
-- whose filter never names an atom pays for neither the source's projection,
-- the combat lookup, nor the bound's evaluation.
slotContext :: Map ObjectId PC.ProjectedCharacteristics -> Maybe PlayerId -> Bool -> Map SlotName Binding.Type.Binding -> ObjectId -> Maybe Quantity -> GameState -> Filter.Context
slotContext pcs perspective unannounced bindings source amount gs =
  let -- CR 601.2c's chosen recipients out of the announcement's whole binding
      -- environment, which is what the two slot-reading atoms below want; the
      -- amounts beside them go to boundAmounts instead.
      targets = Binding.targetsOf bindings
      base =
        Filter.MkContext
          { Filter.perspective = perspective,
            Filter.source = Just source,
            Filter.sourcePower = Projection.powerWithLastKnownGiven pcs source gs,
            -- Nothing HERE and filled below: CR 202.3's computed bound is the slot's
            -- own Quantity, and evaluating one takes a Filter.Context -- so this
            -- record is the context the evaluation runs in, and the answer is laid
            -- over it. Nothing inside it is what makes that non-circular: no Quantity
            -- arm reads this field.
            Filter.slotAmount = Nothing,
            -- False HERE and laid over below, slotAmount's reason one field up: the
            -- answer turns on whether that evaluation came back with a number.
            Filter.boundUnannounced = False,
            -- CR 508.5, asked of the SOURCE: rule 702.39a's clause is on an
            -- attacking creature, and `source` is the object CR 113.7 says the
            -- ability came from.
            Filter.defendingPlayer = Defender.playerOfAttacker Projection.controllerWithLastKnown source gs,
            -- Nothing: a target slot is judged before the effect names anyone, so
            -- there is no recipient it could have reached yet. CR 119.5's atom
            -- lives in an effect's QUANTITY, which is evaluated later and
            -- elsewhere.
            Filter.recipient = Nothing,
            -- Off `bindings`, exactly as slotNames below is and for CR 601.2c's
            -- other sibling-slot reading: "another target creature" is a slot
            -- forbidding what a SIBLING slot holds (Fall of the Hammer's
            -- Not (IsBound "dealer")), and rule 601.2c makes sharing the default,
            -- so the restriction has to be a Filter the card writes rather than
            -- machinery. What a caller supplies no bindings for stays vacuously
            -- False -- IsBound's own call rather than a posture this record takes
            -- for every field: slotControllers below is read by an atom that
            -- WIDENS on the same absence (CR 110.2's SameControllerAsBound).
            --
            -- WIDENING at CR 601.2c is still the offer: legalSetsGiven's first
            -- pass hands every slot the seed alone, so the union is what a
            -- dependent slot is offered, and selectionLegal is where an
            -- announcement naming one creature twice is rejected.
            Filter.slotObjects = fmap (Set.fromList . Maybe.mapMaybe Recipient.objectOf . Set.toList) targets,
            -- THE one site that fills it, alongside sourcePower and
            -- defendingPlayer above and for the same reason: SameNameAsBound
            -- lives in a target slot's Filter, and this is where one is matched.
            --
            -- Off `bindings`, which is what the announcement already holds --
            -- CR 603.2's own bindings for a triggered ability (Harness the Storm's
            -- cast spell) plus whatever sibling slots the first pass answered.
            -- A slot holding several recipients contributes all of their names,
            -- which is CR 709.4a's membership read once more: the candidate has
            -- "the same name as" the slot if it shares a name with any of them.
            --
            -- Through CR 608.2h's last-known reader rather than a live
            -- projection, because the bound object is NOT the target and the two
            -- rules differ: CR 608.2b blanks a departed TARGET, while "that
            -- spell" is a reference the ability already made and rule 608.2h
            -- keeps answerable. Harness the Storm whose spell was countered in
            -- response still knows the name it named.
            --
            -- A THUNK, like the two above: one projection per bound object, paid
            -- for only by a filter that names the atom.
            Filter.slotNames = fmap (foldMap (foldMap (foldMap Filter.names . Projection.viewWithLastKnownAnywhere gs) . Recipient.objectOf)) targets,
            -- CR 110.2's other read of the same objects, alongside slotNames above
            -- and filled the same way: SameControllerAsBound lives in a target
            -- slot's Filter, this is where one is matched, and the CR 608.2h
            -- reader is what keeps a bound target that has since left the
            -- battlefield answerable rather than silently changing the sibling
            -- slot's legality at CR 608.2b.
            --
            -- A KEY PER BOUND SLOT and no more, which is the distinction that
            -- atom's vacuous direction rests on: `fmap` leaves a slot the
            -- announcement has not answered yet out of the map entirely, where a
            -- slot naming an object with no controller (CR 108.4) gets an empty
            -- set. The first widens and the second refuses.
            --
            -- A THUNK, like its siblings: one projection per bound object, paid
            -- for only by a filter that names the atom.
            Filter.slotControllers = fmap (foldMap (foldMap (foldMap (maybe Set.empty Set.singleton . Filter.controller) . Projection.viewWithLastKnownAnywhere gs) . Recipient.objectOf)) targets,
            -- CR 603.2's NUMBERS out of the same environment: "that much", the
            -- amount the trigger's own event stamped, which the bound below reads
            -- through Quantity.InSlot. Not an atom's input -- no Filter arm reads
            -- this map -- but the channel Pawl.Engine.Quantity needs, CR 603.3d
            -- choosing the target before the ability object holds a binding of its
            -- own. Read at both of CR 115's moments, like every field here.
            Filter.boundAmounts = Map.mapMaybe Binding.Type.amount bindings,
            -- Nothing: CR 303.4b's atom names what the SOURCE enchants, and no
            -- printing puts that in a target slot -- "enchanted creature" is a
            -- reference the card already made rather than a choice CR 601.2c
            -- leaves open. Pawl.CardSpec's position lint is what keeps that true,
            -- and widening it here would be a capability no card asks for.
            Filter.sourceAttachedTo = Nothing,
            -- Empty, sourceAttachedTo's reason one atom over: CR 201.4's name is
            -- chosen while the spell RESOLVES (CR 608.2c), and a target slot is
            -- matched at CR 601.2c, before any of that has happened. So there is
            -- no chosen name here even for a source that will have one, and
            -- HasChosenName is vacuously False. Pawl.CardSpec's position lint is
            -- what keeps a card out of the slot.
            Filter.sourceChosenNames = Set.empty
          }
      evaluated = amount >>= Quantity.evaluate (Projection.fullView gs) base gs source
   in -- CR 202.3 / 601.2c: the slot's own computed mana-value bound, evaluated
      -- against the context above and handed to Filter.ManaValueAtMostAmount.
      -- THIS is the one site that fills it, sourcePower's and slotNames' sibling
      -- in that respect and for the same reason: the atom lives in a target
      -- slot's Filter, and this is where one is matched -- at both of CR 115's
      -- moments, so the bound is read again at CR 608.2b rather than frozen at
      -- CR 601.2c.
      --
      -- Evaluated against the SOURCE (Celestine's "the amount of life YOU gained
      -- this turn" is CR 109.5's you, which `perspective` above carries), never
      -- against the candidate: the bound is one number for the whole slot.
      --
      -- The source is also what a Quantity.InSlot bound would be read off, and
      -- for a triggered ability that is the wrong object -- CR 113.7 makes it the
      -- permanent, while CR 603.2's amount belongs to the announcement. That is
      -- what `boundAmounts` above carries, and Venerable Warsinger's "where X is
      -- the amount of damage this creature dealt to that player" is the printed
      -- shape that needs it.
      --
      -- A THUNK for sourcePower's reason: a slot naming an amount whose filter
      -- never asks pays for no evaluation.
      base
        { Filter.slotAmount = evaluated,
          -- CR 601.2b: the slot NAMES a bound and the announcement that would fix it
          -- has not been made -- Stir the Grave's "mana value X or less" at the
          -- castability gate, which runs before its caster names X. Only then, so a
          -- bound the announcement DID make and that still cannot be read stays
          -- vacuously False, and a slot naming no bound at all is untouched.
          -- Filter.boundUnannounced carries the rest of the argument.
          Filter.boundUnannounced = unannounced && Maybe.isJust amount && Maybe.isNothing evaluated
        }

-- CR 608.2h asked of a target SLOT: would it admit `host` as that object MOST
-- RECENTLY existed, once `host` no longer exists at all? The counterpart of
-- admittedGiven above for a departed object, and NOT a widening of it: an
-- enumeration over a pool can only ever offer what a zone holds now, so an
-- object the game has already forgotten has to be asked about by id.
--
-- The one caller is Pawl.Engine.Attach.attachableWithLastKnown, which is itself
-- called only from CR 701.3a's search-side question (Auratouched Mage's "an Aura
-- card that could enchant it", with the Mage killed in response to its own
-- trigger). It answers a BOOL rather than a Recipient because there is no live
-- recipient to hand back: nothing will be attached to a host that is gone.
--
-- False when nothing was filed, which is the same no-op every other CR 608.2h
-- reader gives an id it has no record of.
lastKnownAdmits :: Maybe PlayerId -> ObjectId -> TargetSlot -> ObjectId -> GameState -> Bool
lastKnownAdmits perspective source slot host gs =
  let context = slotContext (Projection.projectAll gs) perspective False Map.empty source (TargetSlot.amount slot) gs
      admits view =
        poolHeldLastKnown (TargetSlot.pool slot) view
          && Maybe.maybe True (Filter.matches context view) (TargetSlot.filter slot)
   in -- lastKnownOf first, so a host that is still on the board falls to False
      -- here rather than being answered off the live view viewWithLastKnownAnywhere
      -- would hand back: that board is attachmentFor's question, not this one.
      Maybe.isJust (Projection.lastKnownOf host gs)
        && Maybe.maybe False admits (Projection.viewWithLastKnownAnywhere gs host)

-- Which pools could have held an object that no longer exists, judged from its
-- last known card types. The POOL half of lastKnownAdmits above, and it is not
-- optional: an enchant ability keeps its "creature" in the POOL rather than in
-- the Filter (Unholy Strength's slot is Pool.Creatures with no filter at all),
-- so a last-known check that matched only the filter would let a departed LAND
-- be enchanted by "enchant creature".
--
-- A total case for basePoolGiven's reason: a new Pool constructor must break the
-- build here rather than default to an answer.
--
-- Every battlefield pool reads the last known CARD TYPES, since CR 110.1 makes
-- being a permanent a fact about the object's types. The record carries no zone
-- of its own, so what is read is what the object WAS rather than where it was;
-- the caller only ever asks about an ability's source, which was a permanent
-- while it existed.
--
-- A REGRESSION FENCE in one direction only. Answering False for every pool
-- reddens Pawl.AuraSpec's "the Aura the dead Mage could have hosted is in
-- alice's hand", so the creature test is paid for; answering True for every pool
-- reddens nothing, because the one card that asks this question is a CREATURE
-- and Pool.Creatures is what its Aura's slot names. A card searching for an Aura
-- from a noncreature source would be the observer.
--
-- The other pools answer False. CR 400.7 makes an object that changed zones a
-- new object, so the id this is asked about names nothing in a graveyard, in
-- exile or on the stack -- and Pool.Players is not even asked, its candidates
-- being players, which have no last known information and are looked up by
-- PlayerId rather than by ObjectId.
poolHeldLastKnown :: Pool.Pool -> Filter.View -> Bool
poolHeldLastKnown pool view =
  let types = Filter.cardTypes view
      hasType t = Set.member t types
   in case pool of
        Pool.Creatures -> hasType CardType.Creature
        Pool.Permanents -> any Card.isPermanentType (Set.toList types)
        Pool.AnyTarget -> hasType CardType.Creature || hasType CardType.Planeswalker || hasType CardType.Battle
        Pool.SpellsAndPermanents -> any Card.isPermanentType (Set.toList types)
        -- Only the planeswalker half can be asked: the player half is looked up by
        -- PlayerId, as Pool.Players below is. A REGRESSION FENCE rather than a
        -- proven behaviour: the one caller asks about an ENCHANT slot, and CR
        -- 702.5a's "Enchant [object or player]" cannot name this pair, so
        -- answering False here reddens nothing.
        Pool.PlayersAndPlaneswalkers -> hasType CardType.Planeswalker
        Pool.Players -> False
        Pool.Spells -> False
        Pool.Abilities -> False
        Pool.CardsInGraveyard _ -> False
        Pool.CardsInExile -> False
        Pool.CreaturesAndCardsInGraveyard _ -> hasType CardType.Creature

-- CR 702.18a (shroud), CR 702.11b/702.11d (hexproof) and CR 702.16b
-- (protection): THE targeting-restriction gate, the one every restriction rule
-- 702 states lands in. It is asked of a candidate the slot has already admitted,
-- and it answers with CR 101.2's "can't": what it rejects is gone, so no Filter
-- can put it back.
-- Both of CR 115's moments route through legalRecipients, so neither needs a
-- clause of its own here.
--
-- The three restrictions differ in the two things this function reads that
-- Filter.matches does not, which is the whole reason they are separate keywords
-- rather than one keyword with a field:
--
--   * WHO IS AIMING. Shroud names no player, so it stops the permanent's own
--     controller as readily as anyone else, while hexproof's "your opponents
--     control" makes the answer depend on the targeting player. `perspective` is
--     that player -- CR 109.5's "you" -- and CR 702.11b's "your" is the
--     CANDIDATE's controller, which CR 109.5 fixes for a static ability.
--     opponentOf below is that comparison, and both of rule 702.11's permanent
--     clauses carry it. Protection does NOT: rule 702.16b names no player at
--     all, so it stops the candidate's own controller as shroud does.
--   * WHAT IS AIMING. CR 702.11d's variant adds a quality the SOURCE must have
--     -- "[quality] spells your opponents control or abilities your opponents
--     control from [quality] sources" -- which `sourceView` answers and plain
--     hexproof never asks. Rule 702.16b asks that question of the same view, in
--     the same two halves ("spells with the stated quality" and "abilities from a
--     source with the stated quality"), and asks nothing else.
--
-- The quality is matched against the source with the CANDIDATE's controller as
-- the Context's perspective, not the targeting player's: rule 702.11d's ability,
-- and rule 702.16b's, is a static ability of the candidate, so CR 109.5 fixes its
-- "you" as the candidate's controller. No quality in the pool reads a perspective at all --
-- CR 702.16a's list of what a quality may be is "any characteristic value or
-- information", and every printed one is a colour or a card type -- but the frame
-- has to be right before one does.
--
-- MEMBERSHIP, never the projection's per-keyword count, which CR 702.18b, CR
-- 702.11h and CR 702.16m all say outright. Membership OF EACH KEY rather than of
-- one key, for hexproof and protection alike: the quality rides the constructor,
-- so a permanent's hexproof abilities are however many keys of its keyword map
-- happen to be Hexproof, and CR 702.11h's "the same hexproof ability" is per key.
-- Rule 702.11f's card, which prints two, is exactly the one a single-key lookup
-- would get wrong; CR 702.16g says the same of protection.
--
-- The POST-layer keywords, like every other keyword reader, so a hexproof granted
-- at layer 6 restricts and a Humility'd Slippery Bogle does not -- and, by CR
-- 702.11e, so does a "hexproof from black", which needs no clause of its own here
-- because it is not a separate keyword to strip.
--
-- The battlefield conjunct is CR 113.6. Shroud is printed on a creature card, so
-- a Blurred Mongoose SPELL has none and Cancel may target it. That is
-- load-bearing rather than defensive: Pool.Spells tags a stack object ToObject,
-- and its projection still carries the card's printed keywords. (It also
-- short-circuits `pcs` for a slot whose candidates are all off the battlefield.)
--
-- The restrictions after these three widen this function and nothing else.
--
-- CR 702.18a's "or player" half, CR 702.11c's and CR 702.16b's come in through a
-- DIFFERENT reader. A player has no keywords: rule 702's keywords live on objects
-- and are folded by the CR 613.1-613.7 layers, so the player halves ride the CR
-- 613.10/613.11 player axis as PlayerEffect.CantBeTargetedBy (Ivory Mask, Leyline
-- of Sanctity). PlayerEffect.protectedFromTargeting is the typed question; this
-- module never sees the constructor. The two halves are separate readers because
-- they read different things -- post-layer KEYWORDS, which `pcs` holds, versus
-- the CR 613.10/613.11 tier, which the layer machine does not compute at all --
-- and neither could serve the other.
--
-- NOT because the player half is cheap: PlayerEffect.applying forces
-- Projection.abilityRemoval, a whole-board gather, the moment any permanent
-- carries a player ability, and this asks it once per player candidate. That is
-- the same cost class the cast path already pays on such a board (Cast.castable
-- reaches `applying` through Cost.spellAdjustments, once per card in hand per
-- legalActions pass), and the benchmarks were unmoved. Hoisting `applying` per
-- enumeration the way `pcs` is hoisted is #435's question, and #578 would catch
-- it regressing.
--
-- EXHAUSTIVE over Recipient rather than routed through Recipient.objectOf: with
-- the player arm split out, an objectOf-shaped match would leave a Nothing branch
-- no input reaches and would silently swallow a new constructor -- a new one must
-- break this build rather than default to targetable.
targetable :: Map ObjectId PC.ProjectedCharacteristics -> Maybe PlayerId -> ObjectId -> Filter.View -> GameState -> Recipient -> Bool
targetable pcs perspective source sourceView gs recipient =
  let restrictedObject oid =
        let keywords = Projection.keywordsGiven pcs oid gs
            -- The candidate's controller, read at most once and only where a
            -- hexproof or protection ability is present to ask about it -- every
            -- reader of it sits behind one of those lists being non-empty, which
            -- is no candidate at all on almost every board. See opponentOf.
            controller = Projection.controllerOf oid gs
            hexproofs = Maybe.mapMaybe hexproofQuality (Map.keys keywords)
            -- CR 702.11b's Nothing stops every spell an opponent controls; CR
            -- 702.11d's Just stops only the ones whose source has the quality. CR
            -- 702.11f's card has several of these and is stopped by ANY of them,
            -- that rule making it several abilities rather than one compound one.
            stops quality = case quality of
              Nothing -> True
              Just f -> Filter.matches (Filter.contextFor controller (Just source)) sourceView f
            -- CR 702.16b's qualities, read off the same keys for hexproof's
            -- reason: CR 702.16g makes "protection from [A] and from [B]" two
            -- abilities, so a permanent's protection abilities are however many
            -- keys of its keyword map happen to be Protection, and CR 702.16m's
            -- redundancy is per key.
            --
            -- Rule 702.16b is the SHORTER sentence: it names no player at all,
            -- so this disjunct carries no opponentOf -- a black spell its own
            -- controller casts cannot target their own creature with protection
            -- from black, where hexproof from black would have let it through.
            -- What it asks OF THE SOURCE is `stops (Just f)` above, down to the
            -- Context: rule 702.16b's ability is the CANDIDATE's, so CR 109.5
            -- fixes its "you" as the candidate's controller too.
            protects = Filter.matches (Filter.contextFor controller (Just source)) sourceView
            -- The conjuncts are in cost order, and the order is the whole reason
            -- `sourceView` costs nothing on an ordinary board: no hexproof or
            -- protection ability at all reads no controller, a hexproof ability
            -- its own controller is aiming past reads no source view, and only
            -- the last conjunct of each forces it. `any` over an empty list is
            -- False without forcing either.
            restricted =
              Map.member Keyword.Shroud keywords
                || (not (null hexproofs) && opponentOf perspective controller && any stops hexproofs)
                || any protects (Maybe.mapMaybe protectionQuality (Map.keys keywords))
         in not (Set.member oid (GameState.battlefield gs) && restricted)
   in case recipient of
        Recipient.ToPlayer pid -> not (PlayerEffect.protectedFromTargeting perspective pid gs)
        Recipient.ToCreature oid -> restrictedObject oid
        Recipient.ToPlaneswalker oid -> restrictedObject oid
        Recipient.ToBattle oid -> restrictedObject oid
        Recipient.ToObject oid -> restrictedObject oid
        -- Unreachable, for the reason `keep` above gives: no pool holds a pile.
        -- Unrestricted rather than excluded, which is the answer rule 702 gives
        -- every candidate that is not a permanent.
        Recipient.ToPile _ -> True

-- CR 702.11b / CR 702.11d's "your opponents": is `perspective` -- CR 109.5's
-- "you" for the spell or ability being aimed -- someone other than the
-- candidate's controller?
--
-- Every other player is an opponent by construction (CR 806.1). CR 102.3 makes a
-- TEAMMATE not an opponent, the only reading this is wrong for, and pawl has no
-- teams -- the same argument Count.playersFor and Filter.matches carry.
--
-- Takes the controller rather than reading it, because targetable above needs the
-- same answer for CR 702.11d's Context and reading it twice would rebuild the
-- control-grant list twice. That list is Projection.controllerOf's own, not the
-- `grants` legalRecipientsGiven and admittedGiven are handed: it is built only
-- for a candidate that already HAS a hexproof ability, which is no candidate at
-- all on almost every board. Threading `grants` down to here as well is the fix
-- if one ever makes the rebuild matter.
--
-- Nothing either way is False, the vacuous posture every player-referencing
-- question here already takes: a question with no "you" in it names no opponent,
-- and neither does a candidate with no controller -- which CR 110.2 makes
-- unreachable for the battlefield candidates the caller above asks about.
opponentOf :: Maybe PlayerId -> Maybe PlayerId -> Bool
opponentOf perspective controller = case (perspective, controller) of
  (Just you, Just c) -> you /= c
  _ -> False

-- CR 702.11b / CR 702.11d: the quality one keyword is a hexproof ability from --
-- Nothing for a keyword that is not one at all, `Just Nothing` for rule 702.11b's
-- unqualified ability, and `Just (Just q)` for rule 702.11d's variant. The nested
-- Maybe is the honest shape: the outer answers "is this hexproof?" and the inner
-- carries what rule 702.11d parameterizes.
--
-- Not Projection.hasKeywordGiven's lookup, which asks about ONE key: with the
-- quality on the constructor there is no single key to look up, and rule 702.11f
-- puts two of them on one card.
hexproofQuality :: Keyword.Keyword -> Maybe (Maybe (Filter.Type.Filter Keyword.Keyword))
hexproofQuality keyword = case keyword of
  Keyword.Hexproof quality -> Just quality
  _ -> Nothing

-- CR 702.16b: the quality one keyword is a protection ability from, and Nothing
-- for a keyword that is not one at all. hexproofQuality above without the inner
-- Maybe: rule 702.16a states a quality on every protection ability, where rule
-- 702.11b's plain hexproof states none. Rule 702.16j's "protection from
-- everything" is the variant that would want one, and is not modelled (#2229).
protectionQuality :: Keyword.Keyword -> Maybe (Filter.Type.Filter Keyword.Keyword)
protectionQuality keyword = case keyword of
  Keyword.Protection quality -> Just quality
  _ -> Nothing

-- Every base pool that is a function of the GAME STATE alone, taken once and
-- shared by every slot of every ability in one enumeration (#1073). One field
-- per Pool constructor that ignores the Filter.Context, which is every one that
-- names no graveyard -- see basePoolGiven for why a graveyard arm cannot be here.
-- Pool.CreaturesAndCardsInGraveyard is half of each: its battlefield half is the
-- creature field below, and only its graveyard half is built per slot.
--
-- LAZY FIELDS, and that is the whole design: a record with no strictness
-- annotations makes each pool a thunk, so an enumeration whose abilities all
-- target creatures pays for the creature walk and for nothing else. Sharing it
-- costs a caller that reaches one pool exactly what taking it inline did.
--
-- NOT a Map keyed by Pool. A lookup would want a Nothing arm, and a
-- Map.findWithDefault would let a new Pool constructor slip through silently;
-- basePoolGiven's case is what makes one break the build instead.
data Pools = MkPools
  { creaturePool :: Set Recipient,
    playerPool :: Set Recipient,
    anyTargetPool :: Set Recipient,
    permanentPool :: Set Recipient,
    spellPool :: Set Recipient,
    abilityPool :: Set Recipient,
    spellsAndPermanentsPool :: Set Recipient,
    playersAndPlaneswalkersPool :: Set Recipient,
    exilePool :: Set Recipient
  }

-- The pools of one board. `pcs` is the caller's whole-board projection, as
-- everywhere else here.
poolsGiven :: Map ObjectId PC.ProjectedCharacteristics -> GameState -> Pools
poolsGiven pcs gs =
  MkPools
    { creaturePool = creatureRecipientsGiven pcs gs,
      playerPool = playerRecipients gs,
      anyTargetPool =
        Set.union
          (playerRecipients gs)
          ( onePerObject
              [ creatureRecipientsGiven pcs gs,
                planeswalkerRecipientsGiven pcs gs,
                battleRecipientsGiven pcs gs
              ]
          ),
      permanentPool = permanentRecipients gs,
      spellPool = spellRecipients gs,
      abilityPool = abilityRecipients gs,
      spellsAndPermanentsPool = Set.union (spellRecipients gs) (permanentRecipients gs),
      -- No onePerObject, unlike anyTargetPool above: the two halves are a player
      -- and an object, so no permanent can appear under two tags here.
      playersAndPlaneswalkersPool =
        Set.union (playerRecipients gs) (planeswalkerRecipientsGiven pcs gs),
      exilePool = exileRecipients gs
    }

-- The closed part: build the pool's base recipient set over zones, tagging each
-- candidate with how it is referenced (CR 115). Each arm is one field of the
-- caller's Pools and nothing else.
--
-- The Context is the SAME one the Filter is matched against, and the arms that
-- name a GRAVEYARD or EXILE read it -- `bindings` only the graveyard ones: CR
-- 400.1's per-player zones make a pool that names one have to say whose, and a
-- GraveyardScope answers with either the Context's perspective (CR 109.5's
-- would-be controller, the player CR 601.2c has choosing targets) or another
-- slot's own answer. Every battlefield and stack arm ignores both, because those
-- zones are shared by all players (CR 400.1 again) -- which is what lets those
-- arms be hoisted into Pools and leaves a graveyard one built here, per slot,
-- against that slot's own Context. Exile is shared too and stays hoisted; CR
-- 406.4's per-chooser narrowing is `piledOffer`'s, taken at the prompt rather
-- than here -- see its arm.
basePoolGiven :: Pools -> Filter.Context -> Map SlotName (Set Recipient) -> Pool.Pool -> GameState -> Set Recipient
basePoolGiven pools context bindings pool gs = case pool of
  Pool.Creatures -> creaturePool pools
  Pool.Players -> playerPool pools
  Pool.AnyTarget -> anyTargetPool pools
  Pool.Permanents -> permanentPool pools
  Pool.Spells -> spellPool pools
  Pool.Abilities -> abilityPool pools
  Pool.SpellsAndPermanents -> spellsAndPermanentsPool pools
  Pool.PlayersAndPlaneswalkers -> playersAndPlaneswalkersPool pools
  Pool.CardsInGraveyard scope -> graveyardRecipients context bindings scope gs
  -- CR 406.1's whole zone, face-down cards included, and hoisted because CR 400.1
  -- shares it between all players the way no graveyard is shared.
  --
  -- CR 406.4's per-chooser narrowing is NOT taken here, and the rule's two halves
  -- are why: a face-down card a player may not look at is still a LEGAL target
  -- for their spell -- the pile they choose instead can name it -- so dropping it
  -- from this set would make CR 608.2b fizzle the very target CR 406.4 handed
  -- them. What the rule restricts is the ANNOUNCEMENT, so `piledOffer` takes the
  -- narrowing where the prompt is raised and nowhere else.
  Pool.CardsInExile -> exilePool pools
  -- The union of the two arms above, built HERE rather than hoisted into Pools for
  -- the graveyard arm's reason: half of it is per-slot. The battlefield half is the
  -- shared thunk, so a slot naming this pool pays the creature walk once with every
  -- other slot that names one.
  Pool.CreaturesAndCardsInGraveyard scope ->
    Set.union (creaturePool pools) (graveyardRecipients context bindings scope gs)

-- CR 115.2 (only permanents are legal targets, save for the exceptions the
-- graveyard, exile, spell and player arms above are) with CR 109.2 (an
-- unqualified creature description means a permanent on the battlefield):
-- creatures on the battlefield, per playing player's zone, tagged ToCreature.
-- Reads Projection.isCreatureOf, so a permanent made a creature by the layer
-- system counts and one that lost the type does not.
creatureRecipients :: GameState -> Set Recipient
creatureRecipients gs = creatureRecipientsGiven (Projection.projectAll gs) gs

creatureRecipientsGiven :: Map ObjectId PC.ProjectedCharacteristics -> GameState -> Set Recipient
creatureRecipientsGiven pcs gs =
  let isCreatureId oid = Projection.isCreatureGiven pcs oid gs
   in Set.fromList
        . fmap Recipient.ToCreature
        $ concatMap
          (filter isCreatureId . (\pid -> Game.zoneMembers Zone.Battlefield pid gs))
          (Game.stillPlaying gs)

-- CR 115.4 and CR 115.2: planeswalkers on the battlefield, per playing player's
-- zone, tagged ToPlaneswalker -- the planeswalker half of both anyTargetPool and
-- playersAndPlaneswalkersPool. The same walk creatureRecipientsGiven makes and
-- shares its projection with, asking Projection.isPlaneswalkerGiven instead.
--
-- A pool of its own rather than a widened creatureRecipients, because the two
-- describe different candidate sets: CR 306 planeswalkers and CR 302 creatures
-- overlap but neither contains the other, and Pool.Creatures must not start
-- offering planeswalkers. The TAG is not what settles which of CR 120.3's results
-- damage to the candidate has -- Pawl.Engine.Damage.damagedCardTypes settles that
-- off the projection when the damage is applied -- so a permanent with both card
-- types is deduplicated to one candidate by onePerObject below rather than
-- appearing once per tag.
planeswalkerRecipients :: GameState -> Set Recipient
planeswalkerRecipients gs = planeswalkerRecipientsGiven (Projection.projectAll gs) gs

planeswalkerRecipientsGiven :: Map ObjectId PC.ProjectedCharacteristics -> GameState -> Set Recipient
planeswalkerRecipientsGiven pcs gs =
  let isPlaneswalkerId oid = Projection.isPlaneswalkerGiven pcs oid gs
   in Set.fromList
        . fmap Recipient.ToPlaneswalker
        $ concatMap
          (filter isPlaneswalkerId . (\pid -> Game.zoneMembers Zone.Battlefield pid gs))
          (Game.stillPlaying gs)

-- CR 115.4: battles on the battlefield, per playing player's zone, tagged
-- ToBattle. planeswalkerRecipientsGiven's twin one card type over, sharing the same
-- walk and the same projection, and a pool of its own for the same reason: CR 310
-- battles are a candidate set that neither contains nor is contained by the other
-- two.
--
-- The walk is over each still-playing player's slice of the battlefield, which
-- Game.zoneMembers cuts by OWNER -- the same walk the two arms above make, and the
-- reason it is right here is that CR 115.4 draws no line at all: neither the
-- battle's controller nor its protector narrows the candidates. Being a legal
-- target is not being attackable (CR 310.9b): a "deals 3 damage to any target"
-- spell may name a battle whose protector the caster is.
battleRecipients :: GameState -> Set Recipient
battleRecipients gs = battleRecipientsGiven (Projection.projectAll gs) gs

battleRecipientsGiven :: Map ObjectId PC.ProjectedCharacteristics -> GameState -> Set Recipient
battleRecipientsGiven pcs gs =
  let isBattleId oid = Projection.isBattleGiven pcs oid gs
   in Set.fromList
        . fmap Recipient.ToBattle
        $ concatMap
          (filter isBattleId . (\pid -> Game.zoneMembers Zone.Battlefield pid gs))
          (Game.stillPlaying gs)

-- CR 115.4 lists what may be chosen -- "creatures, players, planeswalkers, or
-- battles" -- rather than how many ways there are to choose one, so a PERMANENT
-- with more than one of those card types is one candidate and not one per card
-- type. Liquimetal Coating plus March of the Machines makes Jace Beleren an
-- artifact creature planeswalker (CR 205.1b), and offering him twice would be one
-- target choice too many.
--
-- The survivor keeps the earliest list's tag, and which that is carries no rules
-- weight: Pawl.Engine.Damage.damagedCardTypes reads the recipient's PROJECTED
-- card types as the damage is applied, so every one of CR 120.3's results the
-- permanent is owed applies whichever tag it was chosen under. What the fixed
-- order buys is that the answer is deterministic, which CR 608.2b's
-- re-validation needs: it re-derives this very set and asks whether the recipient
-- already chosen is still in it.
--
-- Players are unioned in outside this, because CR 115.1 makes a player a
-- recipient in its own right rather than an object, so no permanent can collide
-- with one.
onePerObject :: [Set Recipient] -> Set Recipient
onePerObject sets =
  let step (seen, kept) recipient = case Recipient.objectOf recipient of
        Nothing -> (seen, recipient : kept)
        Just oid ->
          if Set.member oid seen
            then (seen, kept)
            else (Set.insert oid seen, recipient : kept)
   in Set.fromList (snd (Foldable.foldl' step (Set.empty, []) (concatMap Set.toList sets)))

-- CR 115: players still in the game, tagged ToPlayer.
playerRecipients :: GameState -> Set Recipient
playerRecipients gs = Set.fromList (fmap Recipient.ToPlayer (Game.stillPlaying gs))

-- CR 110.1: permanents on the battlefield, tagged ToObject.
permanentRecipients :: GameState -> Set Recipient
permanentRecipients gs = Set.fromList (fmap Recipient.ToObject (Set.toList (GameState.battlefield gs)))

-- CR 112.1: only spells (Source.OfCard) on the stack, tagged ToObject; abilities
-- and permanents are excluded by Game.isSpell.
spellRecipients :: GameState -> Set Recipient
spellRecipients gs = Set.fromList (fmap Recipient.ToObject (filter (\oid -> Game.isSpell oid gs) (GameState.stack gs)))

-- CR 113.9: only activated and triggered abilities (Source.OfAbility /
-- OfTrigger / OfInherentTrigger) on the stack, tagged ToObject; spells and
-- permanents are excluded by Game.isAbility. Stifle's "target activated or
-- triggered ability".
--
-- The same walk spellRecipients makes over the same list, and DISJOINT from it by
-- construction, which is CR 113.9's first two sentences.
--
-- Nothing filters out a MANA ability, and nothing needs to: CR 605.3b and CR
-- 605.4a keep one off the stack entirely, so this walk can never see one.
-- Stifle's "(Mana abilities can't be targeted.)" is reminder text for those rules
-- -- see Pawl.Types.Pool.Abilities.
abilityRecipients :: GameState -> Set Recipient
abilityRecipients gs = Set.fromList (fmap Recipient.ToObject (filter (\oid -> Game.isAbility oid gs) (GameState.stack gs)))

-- CR 404.1: the cards in the graveyards the scope names, tagged ToObject -- Raise
-- Dead's "target creature card in your graveyard", and Withered Wretch's "exile
-- target card from a graveyard", which names no player at all. CR 115.2's clause
-- (a), its OTHER-ZONE half, since playerRecipients above is already the "or a
-- player" one.
--
-- ToObject, like permanentRecipients and unlike creatureRecipients, because the
-- candidates are CARDS: CR 109.2's battlefield default is switched off by the
-- card's own word "card", so "creature" is a Filter over an untagged card here.
-- Game.zoneMembers Zone.Graveyard is per-OWNER (CR 400.3), which is what makes
-- the scope answerable at all -- CR 108.4 gives a card in a graveyard no
-- controller to ask about.
--
-- Whose graveyard is the GraveyardScope's answer, read by graveyardScopePlayers
-- below, in two readings:
--
--   * Scoped is PlayerEffect.playersInScope's, rather than a second reading of
--     CR 109.5 written here: that function folds the one membership test, which
--     is where PlayerScope.Opponents' CR 806.1 argument lives. Nothing -> no
--     players is its report of an absent perspective -- the vacuous posture every
--     player-referencing Filter atom takes. PlayerScope.EachPlayer never reaches
--     it: "a graveyard" names the whole table with no perspective to lack.
--
--   * InSlot is the PLAYER recipients another slot of the same announcement
--     holds -- Dwell on the Past's "their graveyard". Deliberately the same
--     lookup at both of CR 115's moments, and the moments differ only in what
--     the caller passes:
--
--       - CR 601.2c chooses every target AT ONCE, so nothing has been announced
--         yet and legalSets passes the named slot's own LEGAL SET. The union
--         over it is every card the announcement could reach -- a superset of
--         any one coherent answer, which selectionLegal then judges whole. No
--         order between the two slots is invented, which is what a
--         one-slot-at-a-time prompt would have to do. The SET stays the union;
--         it is the COUNT that slotCapacities narrows to one coherent answer.
--       - CR 608.2b re-checks a spell whose slots are FILLED, so Resolve passes
--         the chosen targets and the union is over the one player named.
--
--     A slot holding an object rather than a player contributes nothing, the
--     same empty answer ObjectRef gives for a slot holding a player. A slot that
--     is itself InSlot-scoped is answerable but not usefully so: legalSets fills
--     `bindings` from a pass that gave it no bindings of its own, so it holds
--     nothing and this is empty -- which terminates rather than recurring.
graveyardRecipients :: Filter.Context -> Map SlotName (Set Recipient) -> GraveyardScope.GraveyardScope -> GameState -> Set Recipient
graveyardRecipients context bindings scope gs =
  graveyardsOf (graveyardScopePlayers (Filter.perspective context) bindings scope gs) gs

-- The players a GraveyardScope names, and the WHOLE of what either reading of
-- that type means: the two arms are exactly the two paragraphs above, and this is
-- the one place they are read.
--
-- Shared with Pawl.Engine.Resolve, whose ObjectRef.EachCardInGraveyard sweep asks
-- the same question at CR 608.2c; see #1310. The two callers differ only in what
-- they do with the answer -- a target pool wants CR 404.1's cards as recipients,
-- a sweep wants them filtered and in CR 608.2f's APNAP order -- so splitting here
-- rather than at the recipients keeps one reading of the scope for both.
--
-- Unordered: the caller imposes whatever order its own rule asks for.
graveyardScopePlayers :: Maybe PlayerId -> Map SlotName (Set Recipient) -> GraveyardScope.GraveyardScope -> GameState -> [PlayerId]
graveyardScopePlayers perspective bindings scope gs = case scope of
  GraveyardScope.Scoped playerScope -> Maybe.fromMaybe [] (PlayerEffect.playersInScope perspective gs playerScope)
  GraveyardScope.InSlot slot ->
    Maybe.mapMaybe playerOf (Set.toList (Map.findWithDefault Set.empty slot bindings))

-- CR 404.1 over a list of players, deduplicated by the Set the caller gets back.
graveyardsOf :: [PlayerId] -> GameState -> Set Recipient
graveyardsOf pids gs =
  Set.fromList
    . fmap Recipient.ToObject
    $ concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) pids

-- The player a Recipient names, if it names one. Recipient.objectOf's twin on
-- the CR 115.1 player side.
playerOf :: Recipient -> Maybe PlayerId
playerOf recipient = case recipient of
  Recipient.ToPlayer pid -> Just pid
  Recipient.ToCreature _ -> Nothing
  Recipient.ToPlaneswalker _ -> Nothing
  Recipient.ToBattle _ -> Nothing
  Recipient.ToObject _ -> Nothing
  Recipient.ToPile _ -> Nothing

-- CR 406.1: the cards in the exile zone, tagged ToObject -- Riftsweeper's
-- "choose target face-up exiled card". CR 115.2's clause (a) again, the same
-- other-zone half graveyardRecipients above is, and the second pool to leave the
-- battlefield and the stack behind.
--
-- Reads GameState.exile WHOLE, exactly as permanentRecipients reads
-- GameState.battlefield, and takes no scope at all. That is CR 400.1's second
-- sentence: the other zones are shared by all players, so there is no per-player
-- copy of exile to fold over and no "whose" for the Context's perspective to
-- answer -- see Pawl.Types.Pool.CardsInExile for why a PlayerScope here would be
-- an owner filter wearing a zone's type.
--
-- No stillPlaying filter, and none is needed -- unlike graveyardRecipients, which
-- folds a list of players and so must have one. CR 800.4a takes every object a
-- departing player owned out of the game, which
-- Pawl.Engine.Departure.objectsLeaveWith performs by deleting those ids from
-- every zone, exile included. That sweep is gated on
-- Departure.continuesAfterDeparture, so a two-player loser's exiled cards do stay
-- in the set -- unobservably, because CR 104.2a ends that game at once. The
-- rule's LAST clause pushes the other way and is honoured by the same absence:
-- objects still controlled by that player are exiled, and they are owned by
-- somebody still here.
--
-- EVERY exiled card, face up or face down. CR 406.4's per-chooser substitution
-- is `piledOffer`'s, made as a prompt is raised: that rule asks whether a PLAYER
-- is allowed to look at the card, and this function has no player to ask it of
-- -- which is the same reason this pool takes no scope. Hoisting a per-chooser
-- question in here would answer for whoever the first slot of the enumeration
-- happened to belong to.
exileRecipients :: GameState -> Set Recipient
exileRecipients gs = Set.fromList (fmap Recipient.ToObject (Set.toList (GameState.exile gs)))

-- CR 608.2b: a target that left the zone it was chosen in is illegal (its id
-- names an object that no longer exists, per CR 400.7), and legality is
-- otherwise re-judged against the slot in the current state.
--
-- `bindings` is the resolving object's OWN chosen targets, which is what makes
-- CR 608.2b exact where CR 601.2c could only be a superset: a
-- GraveyardScope.InSlot pool re-derived here reads the one player the spell
-- actually named, so a card that has since moved to somebody else's graveyard is
-- no longer a legal target for it.
stillLegal :: Maybe PlayerId -> Map SlotName Binding.Type.Binding -> ObjectId -> Recipient -> TargetSlot -> GameState -> Bool
stillLegal perspective bindings source recipient slot gs =
  let pcs = Projection.projectAll gs
   in Set.member recipient (legalRecipientsGiven pcs (Projection.controlGrants gs) (poolsGiven pcs gs) perspective False bindings source slot gs)

-- CR 303.4c: is `recipient` still one the slot ADMITS -- the same membership
-- question stillLegal asks, minus rule 702's targeting restrictions. See
-- admittedRecipients for why an attached Aura is not asked a targeting question.
stillAdmitted :: Maybe PlayerId -> ObjectId -> Recipient -> TargetSlot -> GameState -> Bool
stillAdmitted perspective source recipient slot gs = Set.member recipient (admittedRecipients perspective source slot gs)

-- One legal set per named slot; casting prompts with exactly this map. `source`
-- is the object the targeting is relative to -- the spell object at cast, the
-- source permanent for an ability. CR 601.2c's "another" needs no separate pass:
-- a slot that excludes its source says so with Not IsSource, and a slot that does
-- not is untouched, so Prodigal Sorcerer may still target itself with AnyTarget
-- (CR 115.4). CR 115.5's self-exclusion is
-- a DIFFERENT rule: unconditional, and firing only where its own words do, for a
-- source that is itself on the stack -- see legalRecipients.
--
-- `seed` is what the announcement ALREADY has bound before any target is chosen
-- -- CR 601.2b's announced X for a cast (Stir the Grave's bound), CR 603.2's
-- trigger bindings for a triggered ability being placed (Harness the Storm's cast
-- spell, Venerable Warsinger's event amount), CR 707.10's copied decisions minus
-- the targets CR 707.10c is re-choosing (Resolve.chooseNewTargetsFor), and empty
-- for an activation, whose X does not reach a slot yet (#2672). It joins the
-- per-slot bindings the two passes below build, so an atom that reads a slot
-- cannot tell the two apart.
legalSets :: Maybe PlayerId -> Map SlotName Binding.Type.Binding -> ObjectId -> Map SlotName TargetSlot -> GameState -> Map SlotName (Set Recipient)
legalSets perspective seed source slots gs =
  let pcs = Projection.projectAll gs
   in legalSetsGiven pcs (Projection.controlGrants gs) (poolsGiven pcs gs) perspective False seed source slots gs

-- The same map on a board the caller already walked -- see legalRecipientsGiven.
--
-- TWO PASSES, because CR 601.2c makes one slot's legal set depend on another's:
-- a GraveyardScope.InSlot pool names a slot, so the first pass answers every
-- slot with no bindings at all and the second re-answers only the slots that
-- name one, against the first pass. The rule chooses every target AT ONCE, so
-- there is no order to consult -- what a dependent slot is offered is the UNION
-- over the answers the slot it names could still take, and selectionLegal is
-- where the announcement is judged as one act.
--
-- The second pass reads the FIRST pass's map and not its own, which is what
-- makes a chain of dependent slots terminate rather than recur: a slot naming
-- another dependent slot reads that slot's first-pass answer, which is empty.
-- No card writes one, and nothing here would loop if one did.
--
-- Ordinary cards pay nothing: `dependent` is empty for every slot map with no
-- slot-scoped pool in it, so the second pass is a Map.filter over a map with at
-- most a handful of keys.
legalSetsGiven :: Map ObjectId PC.ProjectedCharacteristics -> [Projection.ControlGrant] -> Pools -> Maybe PlayerId -> Bool -> Map SlotName Binding.Type.Binding -> ObjectId -> Map SlotName TargetSlot -> GameState -> Map SlotName (Set Recipient)
legalSetsGiven pcs grants pools perspective unannounced seed source slots gs =
  let answer bindings slot = legalRecipientsGiven pcs grants pools perspective unannounced bindings source slot gs
      -- The FIRST pass sees the seed alone, which is the only thing bound before
      -- CR 601.2c chooses anything; the second sees it under the first pass's
      -- answers. Map.union is left-biased, so a target slot's own answer wins over
      -- a seed entry that happened to share its name.
      independent = fmap (answer seed) slots
      dependent = fmap (answer (Map.union (fmap Binding.toRecipients independent) seed)) (Map.filter (dependsOnSlot . TargetSlot.pool) slots)
   in -- Map.union is left-biased, so the second pass wins wherever it answered.
      Map.union dependent independent

-- Must this slot's answer be judged against what its SIBLING slots were answered
-- with (CR 601.2c)? Two ways one slot depends on another, and they are the pool's
-- and the filter's:
--
--   * the POOL names a slot (dependsOnSlot below), which is Dwell on the Past's
--     "their graveyard";
--   * the FILTER reads one, which is CR 601.2c's "another" between two slots of
--     one announcement -- Fall of the Hammer's Not (IsBound "dealer").
--
-- Filter.boundSlots is the read half, the same set Pawl.Engine.Resolve's dataflow
-- lint folds over a mode's slots, so an atom that function does not report is one
-- this gate does not fire for -- which is the pairing that comment insists on.
--
-- `declared` is the ANNOUNCEMENT's own slot names, and a filter naming anything
-- else is deliberately not a sibling read: CR 603.2's trigger bindings reach
-- legalSetsGiven as `seed` and reach selectionLegal not at all, so re-deriving
-- such a slot against `chosen` alone would answer it off an empty binding rather
-- than off the trigger's, and reject an announcement the rule allows. Harness the
-- Storm's twin slot is the shape (its filter names `thatSpell`), and that half is
-- a REGRESSION FENCE rather than a proven behaviour: every card in the pool whose
-- slot filter names a seed is on a triggered ability, and no trigger reaches
-- selectionLegal at all (#2472), so dropping `declared` reddens nothing.
--
-- Only the JOINT CHECK reads this, never legalSetsGiven's second pass. That pass
-- is a WIDENING -- it offers the union over what a named slot could still take --
-- and "another" is a NARROWING, so handing it every sibling candidate at once
-- would exclude every candidate and leave the slot unfillable.
jointlyJudged :: Set SlotName -> TargetSlot -> Bool
jointlyJudged declared slot =
  dependsOnSlot (TargetSlot.pool slot)
    || not (Set.disjoint declared (foldMap Filter.boundSlots (TargetSlot.filter slot)))

-- Does this pool's candidate set depend on what another target slot is answered
-- with (CR 601.2c)? A GraveyardScope's InSlot is the one axis that does, wherever
-- it appears; every other pool draws from a zone no slot names.
dependsOnSlot :: Pool.Pool -> Bool
dependsOnSlot = Maybe.isJust . scopeSlot

-- WHICH slot that is, and the whole of dependsOnSlot above: a pool whose
-- candidates are scoped to another slot's answer names it here, and every other
-- pool answers Nothing. Two readers, and they want the two halves -- the gates
-- want the Bool, slotCapacities below wants the name.
scopeSlot :: Pool.Pool -> Maybe SlotName
scopeSlot pool = case pool of
  Pool.Creatures -> Nothing
  Pool.Players -> Nothing
  Pool.AnyTarget -> Nothing
  Pool.Permanents -> Nothing
  Pool.Spells -> Nothing
  Pool.Abilities -> Nothing
  Pool.SpellsAndPermanents -> Nothing
  Pool.PlayersAndPlaneswalkers -> Nothing
  Pool.CardsInGraveyard scope -> graveyardScopeSlot scope
  Pool.CardsInExile -> Nothing
  -- Its graveyard half carries the same axis, so the answer is that half's.
  Pool.CreaturesAndCardsInGraveyard scope -> graveyardScopeSlot scope

-- The slot a GraveyardScope names, if it names one.
graveyardScopeSlot :: GraveyardScope.GraveyardScope -> Maybe SlotName
graveyardScopeSlot scope = case scope of
  GraveyardScope.Scoped _ -> Nothing
  GraveyardScope.InSlot slot -> Just slot

-- CR 601.2c: the range of numbers this slot may be answered with on this board
-- -- the printed count, narrowed by how many legal recipients there actually
-- are. A caster cannot announce more targets than they can then choose legally,
-- and a slot whose range has collapsed to a single number is not a variable
-- number of targets at all ("in some cases, the number of targets will be
-- defined by the spell's text").
--
-- The board narrowing is the WHOLE ceiling for "any number of target ..."
-- (Soulfire Eruption), which prints no maximum -- TargetCount.ceilingOn is where
-- the two cases meet.
--
-- `x` is the value announced at CR 601.2b, which a slot counting "each of X
-- target creatures" reads instead of a printed range (SlotCount.at). It comes in
-- as a number rather than being read off the board here because CR 601.2b's
-- announcement is not recorded anywhere yet -- the object is put on the stack
-- carrying it only after CR 601.2c is done.
--
-- `capacity` is how many of the slot's legal recipients ONE announcement could
-- name, which is the slot's own candidate count for every slot but a
-- graveyard-scoped one -- see slotCapacities.
announcedRange :: Natural -> TargetSlot -> Natural -> (Natural, Natural)
announcedRange x slot capacity =
  let count = SlotCount.at x (TargetSlot.count slot)
      ceiling_ = TargetCount.ceilingOn capacity count
   in (min (TargetCount.least count) ceiling_, ceiling_)

-- CR 601.2c: how many of each slot's legal recipients a single COHERENT
-- announcement could name -- the number the count is measured against, both when
-- the offer is built (chooseTargets, selectionLegal) and when a mode's
-- fillability is judged (fillableModesGiven).
--
-- For every slot whose pool names no other slot this is just how many candidates
-- it has: the whole legal set is one coherent answer, and nothing narrows it.
--
-- A GraveyardScope.InSlot pool is the exception, and the reason this function
-- exists. legalSetsGiven offers it the UNION over the graveyards of every player
-- the named slot could still take, which is a superset of any one coherent
-- answer -- CR 400.1 gives each player their own graveyard, so a card in one
-- player's is unreachable beside another player. The most such a slot can be
-- answered with is therefore the largest total over the player selections the
-- named slot itself admits: the graveyards are pairwise disjoint, so that is the
-- sum of the `k` largest per-player counts, where `k` is the most targets the
-- named slot may be answered with. (Every printing in the pool names exactly one
-- player, so `k` is 1 and this is a maximum; the general form costs a sort.)
--
-- Anything in the legal set that is in NO candidate player's graveyard is added
-- back whole: Pool.CreaturesAndCardsInGraveyard's battlefield half is scoped by
-- nothing, so a coherent answer may take every creature it offers beside the
-- cards from one graveyard.
--
-- The named slot's own capacity is its plain candidate count rather than another
-- pass through here, which is what makes this terminate: legalSetsGiven's second
-- pass already answers a slot naming a dependent slot with nothing.
slotCapacities :: Natural -> Map SlotName TargetSlot -> Map SlotName (Set Recipient) -> GameState -> Map SlotName Natural
slotCapacities x slots sets gs = Map.mapWithKey capacity slots
  where
    legalOf name = Map.findWithDefault Set.empty name sets
    capacity name slot =
      let legal = legalOf name
       in case scopeSlot (TargetSlot.pool slot) of
            Nothing -> Natural.length legal
            -- The scope names a slot this announcement does not declare -- CR
            -- 603.2's trigger bindings reach a pool as `seed` and never as a
            -- slot. Nothing here can bound such a slot's answers, so the
            -- candidate count stands, which is what it was before this function
            -- existed.
            Just named -> case Map.lookup named slots of
              Nothing -> Natural.length legal
              Just namedSlot ->
                let candidates = legalOf named
                    pids = Maybe.mapMaybe playerOf (Set.toList candidates)
                    k = snd (announcedRange x namedSlot (Natural.length candidates))
                    per = List.sortBy (flip compare) (fmap (\pid -> Natural.length (Set.intersection legal (graveyardsOf [pid] gs))) pids)
                    elsewhere = Natural.length (Set.difference legal (graveyardsOf pids gs))
                 in elsewhere + sum (take (Natural.toIntSaturating k) per)

-- CR 601.2c's two announcements over one slot map, in the rule's own order: how
-- many targets each variable slot gets, then the targets themselves.
--
-- A slot whose count the card or the board already fixes is not offered at the
-- first prompt, there being one legal answer; a count outside the offered range
-- is clamped back into it, which is the same game as answering its nearest end.
-- Neither prompt is raised when it has nothing to ask.
--
-- The answer is NOT validated here -- `selectionLegal` below is that, asked by
-- the callers that reverse an announcement (CR 601.2e, CR 602.2).
chooseTargets :: Decider -> PlayerId -> ObjectId -> Natural -> Map SlotName TargetSlot -> Map SlotName (Set Recipient) -> Game (Map SlotName (Set Recipient))
chooseTargets decider pid oid x slots sets = do
  gs <- State.get
  let offered = fmap (piledOffer (Just pid) gs) sets
      ranges = Map.intersectionWith (announcedRange x) slots (slotCapacities x slots offered gs)
      variable = Map.keysSet (Map.filter (uncurry (/=)) ranges)
      offers = Map.restrictKeys (Map.intersectionWith (\targetSlot legal -> (SlotCount.at x (TargetSlot.count targetSlot), legal)) slots offered) variable
  announced <-
    if Map.null offers
      then pure Map.empty
      else Game.choose (Prompt.AnnounceTargets decider pid oid offers)
  let counts =
        Map.filter (> 0) $
          Map.mapWithKey
            (\slot (lo, hi) -> max lo (min hi (Map.findWithDefault lo slot announced)))
            ranges
      asked = Map.intersectionWith (,) counts offered
  if Map.null asked
    then pure Map.empty
    else do
      answer <- Game.choose (Prompt.ChooseTargets decider pid oid asked)
      Map.traverseWithKey (\slot picked -> drawFromPiles (Just pid) (Map.findWithDefault Set.empty slot sets) picked) answer

-- CR 406.4's second half, taken over one slot's legal set as a prompt is built:
-- a candidate this chooser may not name specifically -- a card exiled face down
-- that they are not allowed to look at -- is replaced by the PILE it sits in,
-- "otherwise, they may choose a pile of face-down exiled cards".
--
-- A SUBSTITUTION rather than an addition, which is the rule's "otherwise": the
-- pile is what the chooser gets INSTEAD of the card. Two cards of one pile
-- collapse to one candidate, so the offer says how many piles there are and
-- never how many cards are in one.
--
-- Taken here rather than in the pool, because the two halves of rule 406.4 are
-- about different moments: legality is the card's (basePoolGiven's exile arm),
-- and only the ANNOUNCEMENT is narrowed. Every caller that raises a prompt over
-- a target set owes this call -- Pawl.Engine.Resolve.chooseNewTargetsFor is the
-- other one -- or it would offer by name what the rule says may not be named.
--
-- Applied to the set the slot's Filter already narrowed, so a pile stands for
-- its filter-passing members alone. Not implemented: the rule's own order, which
-- draws from the whole pile and then judges the card drawn, so a filtered slot
-- that offered a pile would here always draw a card it admits (#2567). No card in
-- `data/cards/` reaches it: Riftsweeper's "face-up exiled card" is the pool's
-- one filtered exile slot and it admits no face-down card, so it offers no pile.
piledOffer :: Maybe PlayerId -> GameState -> Set Recipient -> Set Recipient
piledOffer perspective gs = Set.map replace
  where
    replace recipient = Maybe.fromMaybe recipient $ do
      oid <- Recipient.objectOf recipient
      if Exile.mayChoose perspective oid gs
        then Nothing
        else fmap Recipient.ToPile (Exile.pileOf oid gs)

-- CR 406.4: "and then a card is chosen at random from within that pile" -- every
-- pile an announcement named, replaced by the card the draw picked out of it, so
-- what CR 601.2c records as a target is a card and nothing downstream of this
-- ever meets a pile.
--
-- Prompt.RandomObject is the draw, which is the engine asking rather than
-- rolling, and Game.ask rather than Game.choose: randomness is not CR 104.4b's
-- optional action, so a loop containing a draw out of a pile stays a loop of
-- mandatory actions. `legal` is the slot's own legal set, the one piledOffer
-- substituted over, so the pile's members here are exactly the cards that
-- candidate stood for.
--
-- Elided at one member and skipped at none, the posture the three randomness
-- prompts over a candidate list take (Pawl.Engine.Resolve's RandomObject and
-- RandomOpponent, Pawl.Engine.Engine's RandomFirstPlayer): a one-card pile
-- leaves nothing to draw, and CR 702.143e makes every foretold card such a
-- pile. Filtered rather than trusted, so an answer naming a card outside the
-- pile falls back to the first of them -- from
-- Pawl.Engine.Resolve.chooseNewTargetsFor nothing downstream checks the drawn
-- card at all, and from chooseTargets selectionLegal would admit any other
-- exiled card, CR 406.4 keeping every one of them legal.
--
-- A pile whose members have gone is DROPPED rather than kept: an answer holding
-- a pile no longer offered is short by one target, which selectionLegal then
-- refuses under CR 601.2e. Keeping it would record a pile as a target.
--
-- Not implemented: CR 406.4's last sentence, which delays the drawn card's
-- reveal until a cost is paid. Nothing in pawl reveals a chosen target at all,
-- and no cost in `data/cards/` chooses an exiled card (#2568).
drawFromPiles :: Maybe PlayerId -> Set Recipient -> Set Recipient -> Game (Set Recipient)
drawFromPiles perspective legal picked = do
  drawn <- traverse draw (Set.toList picked)
  pure (Set.fromList (Maybe.catMaybes drawn))
  where
    draw recipient = case recipient of
      Recipient.ToPile pile -> do
        gs <- State.get
        case pileMembers perspective pile legal gs of
          [] -> pure Nothing
          [only] -> pure (Just (Recipient.ToObject only))
          first : second : more -> do
            let offered = first NonEmpty.:| (second : more)
            answer <- Game.ask (Prompt.RandomObject offered)
            pure . Just . Recipient.ToObject $
              if List.elem answer (NonEmpty.toList offered) then answer else first
      _ -> pure (Just recipient)

-- The cards piledOffer folded into one pile: the members of `legal` this chooser
-- may not name and Pawl.Engine.Exile.pileOf sorts into this pile. Ascending by
-- object id, which is Set.toList's order and is what the draw is offered.
pileMembers :: Maybe PlayerId -> Pile.Pile -> Set Recipient -> GameState -> [ObjectId]
pileMembers perspective pile legal gs =
  [ oid
  | recipient <- Set.toList legal,
    oid <- Maybe.maybeToList (Recipient.objectOf recipient),
    not (Exile.mayChoose perspective oid gs),
    Exile.pileOf oid gs == Just pile
  ]

-- CR 601.2c: is this answer a legal filling of these slots? Each slot answered
-- with a number of targets its count allows, nothing named that was not offered,
-- and each recipient still one its own slot admits. A slot may be absent when
-- zero is a number it allows -- that is zero targets chosen, and CR 115.6's last
-- clause is what makes it a legal announcement rather than a missing one.
--
-- Measured against the BOARD-NARROWED range, the one chooseTargets offered: a
-- caster who could not find three legal targets has not announced an illegal
-- number, they were never offered it.
--
-- THE JOINT CHECK is the third conjunct, and it is what makes CR 601.2c's "all
-- at once" a whole answer rather than an order: a slot that names another slot
-- was OFFERED the union over that slot's candidates (legalSetsGiven), so
-- membership in `sets` alone would let a caster take a card from one player's
-- graveyard while targeting another -- or name one creature in both of Fall of
-- the Hammer's slots, which rule 601.2c's "another" forbids. Re-deriving the
-- dependent slots against `chosen` under `seed` -- exactly the derivation CR
-- 608.2b will make at resolution, which reads the object's whole binding
-- environment -- judges the announcement as the single act the rule makes it,
-- with neither slot resolved before the other. jointlyJudged is which slots those
-- are, in both readings.
--
-- An announced COUNT is measured against slotCapacities rather than against the
-- union, so a caster is never offered more targets for a graveyard-scoped slot
-- than one graveyard can supply. What the joint check below still catches is
-- WHICH recipients were named, not how many.
--
-- `seed` is the announcement's OWN bindings, the same map the offer was computed
-- against (legalSets) -- CR 601.2b's X for a cast, and nothing at all for an
-- activation, whose X does not reach a slot yet (#2672). The joint check joins it
-- UNDER the chosen targets, exactly as legalSetsGiven's second pass does, so the
-- re-derivation reads the same environment the offer did: a slot's CR 202.3
-- computed bound reading Binding.variableX (Pawl.TargetSpec's "CR 601.2c the
-- joint check re-derives a jointly judged slot against the announced X") is
-- answerable here rather than vacuously unmeetable, and the offer and this
-- re-check cannot disagree about one announcement.
--
-- The seed reaches the BINDINGS and not jointlyJudged's `declared`: that argument
-- is the announcement's declared slot names, and its own haddock is why a slot
-- filter naming a seed entry must stay out of this check.
selectionLegal :: Maybe PlayerId -> Map SlotName Binding.Type.Binding -> ObjectId -> Natural -> Map SlotName TargetSlot -> Map SlotName (Set Recipient) -> Map SlotName (Set Recipient) -> GameState -> Bool
selectionLegal perspective seed source x slots sets chosen gs =
  Set.isSubsetOf (Map.keysSet chosen) (Map.keysSet sets)
    && and (Map.elems (Map.mapWithKey slotLegal slots))
    && and (Map.elems (Map.mapWithKey coherent (Map.filter (jointlyJudged (Map.keysSet slots)) slots)))
  where
    pcs = Projection.projectAll gs
    caps = slotCapacities x slots sets gs
    slotLegal slot targetSlot =
      let legal = Map.findWithDefault Set.empty slot sets
          picked = Map.findWithDefault Set.empty slot chosen
          (_, hi) = announcedRange x targetSlot (Map.findWithDefault 0 slot caps)
          -- The count the slot DEMANDS, unnarrowed by the board -- unlike the
          -- ceiling beside it, which the board is entitled to lower (a caster
          -- cannot choose more targets than there are). CR 601.2c gives no such
          -- relief on the minimum: an announcement that cannot be filled makes the
          -- casting or the activation illegal, and CR 601.2e returns the game to
          -- before it was proposed.
          --
          -- Where the count is printed, `fillableModes` refused the mode before
          -- the announcement began and this bound is the same number
          -- announcedRange narrowed to. Where it is CR 601.2b's X, that gate ran
          -- at the X=0 floor and could not know the value, so this is the only
          -- place an X announced above what the board can supply is caught --
          -- Pawl.CombatSpec's "CR 601.2c announcing more X than there are
          -- creatures reverses the whole activation" proves it.
          demanded = TargetCount.least (SlotCount.at x (TargetSlot.count targetSlot))
          size = Natural.length picked
       in Set.isSubsetOf picked legal && size >= demanded && size <= hi
    coherent slot targetSlot =
      Set.isSubsetOf
        (Map.findWithDefault Set.empty slot chosen)
        (legalRecipientsGiven pcs (Projection.controlGrants gs) (poolsGiven pcs gs) perspective False (Map.union (fmap Binding.toRecipients chosen) seed) source targetSlot gs)

-- CR 700.2a: the mode indices every one of whose target slots can be filled --
-- that is, has at least as many legal recipients as its count demands (a mode
-- with no slots is trivially fillable). Self-exclusion against the SOURCE
-- ("another target creature" on a creature's own ability) is honored because it
-- lives in the slot's own Filter. Shared by spells (Cast) and abilities
-- (Activate/Engine).
--
-- Each slot is asked against slotCapacities, which is what a single coherent
-- announcement could reach through the slot's own POOL. Not implemented:
-- narrowing a mode's fillability across slots that exclude each other through a
-- FILTER (Fall of the Hammer's, through selectionLegal's joint check), so that
-- mode is judged fillable off a board holding one creature and the cast is then
-- reversed at CR 601.2e (#2803).
--
-- `extra` is the slots EVERY mode carries in addition to its own -- CR 303.4a's
-- enchant slot, declared by the card rather than by a mode, which castability
-- must see or an Aura with no legal creature would be castable and then countered
-- on resolution (CR 601.2c). An ability has no enchant slot and passes Map.empty.
fillableModes :: Maybe PlayerId -> Map SlotName Binding.Type.Binding -> ObjectId -> Map SlotName TargetSlot -> Modal.Modal Card (GrantedAbility.GrantedAbility Card) -> GameState -> Set ModeIndex
fillableModes perspective seed source extra modal gs =
  let pcs = Projection.projectAll gs
   in fillableModesGiven pcs (Projection.controlGrants gs) (poolsGiven pcs gs) perspective seed source extra modal gs

-- The same set on a board the caller already walked -- see legalRecipientsGiven.
-- This is the half Action.legalActions' activation gate wants: it asks this
-- question once per permanent, and the wrapper above takes a whole-board sweep
-- apiece to answer it (#716).
fillableModesGiven :: Map ObjectId PC.ProjectedCharacteristics -> [Projection.ControlGrant] -> Pools -> Maybe PlayerId -> Map SlotName Binding.Type.Binding -> ObjectId -> Map SlotName TargetSlot -> Modal.Modal Card (GrantedAbility.GrantedAbility Card) -> GameState -> Set ModeIndex
fillableModesGiven pcs grants pools perspective seed source extra modal gs =
  let ms = Foldable.toList (Modal.modes modal)
      fillable i m =
        let slots = Map.union extra (Mode.targetSlots m)
            -- CR 601.2b's other FLOOR, the one `short` below cannot spell: a
            -- slot's CR 202.3 computed bound that reads a number the seed cannot
            -- supply states no bound at all rather than an unmeetable one (see
            -- Filter.boundUnannounced). Permissive is the floor's direction for a
            -- bound as X=0 is for a count, and both are the same question -- is
            -- there SOME value of the announcement under which this mode is
            -- fillable -- so Stir the Grave's mode is refused for an empty
            -- graveyard and for nothing else. A bound the seed DOES answer narrows
            -- here as it always did.
            --
            -- FIVE of the six callers ask before that announcement exists: CR
            -- 601.2b names the modes before the X, so Cast's three (castable's
            -- targetable, entwineOffer, castProposed's mode gate) and Activate's
            -- two (activatableGiven, activateAbility's mode gate) all run first.
            -- The sixth, Pawl.Engine.Engine.placeBorne, hands in CR 603.2's
            -- bindings, which ARE the announcement -- so for it this is not a
            -- floor but a widening: a trigger's bound its own event bindings
            -- cannot answer is judged fillable here and then admits nothing at CR
            -- 603.3d.
            --
            -- No card can write one, and the pairing rather than the constructor
            -- is why: Pawl.CardSpec's "every slot a triggered ability reads is
            -- bound for its condition" subtracts Event.eventBindingSlots from the
            -- read side, and Resolve.modeSlots reports a slot's `amount` reads into
            -- it -- the ones a PlayerRef buried in the number names included -- so
            -- a bound naming a slot the condition does not supply fails "no
            -- dangling triggered-ability slot" before it can reach a board.
            -- Venerable Warsinger is the pool's one such bound, and its condition's
            -- arm supplies Binding.eventAmount unconditionally.
            sets = legalSetsGiven pcs grants pools perspective True seed source slots gs
         in -- CR 115.6 / 601.2c: a slot is unfillable when the board cannot supply
            -- the MINIMUM its count demands. An "up to one" slot with no legal
            -- recipient demands none, and is answered with zero targets.
            if or (Map.elems (Map.intersectionWith short slots (slotCapacities 0 slots sets gs)))
              then Nothing
              else Just (ModeIndex.MkModeIndex i)
      -- CR 601.2b's X=0 FLOOR, the value every castability and activation gate
      -- is asked at: a slot counting the announced X demands no target until
      -- that announcement exists, and announcing zero is a legal answer.
      --
      -- Measured against slotCapacities rather than the candidate count, for that
      -- function's reason: a graveyard-scoped slot is offered the union over the
      -- named slot's players, and a minimum no ONE of those graveyards can supply
      -- is a mode with no legal announcement rather than a fillable one.
      short slot capacity = capacity < TargetCount.least (SlotCount.at 0 (TargetSlot.count slot))
   in Set.fromList (Maybe.mapMaybe (uncurry fillable) (zip [0 :: Natural ..] ms))

-- CR 603.2: every target slot with its "that player" atoms baked against this
-- binding environment (Pawl.Engine.Filter.bakeBound). The whole of what makes
-- "target creature that player controls" answerable, and it is applied at both
-- of CR 115's moments -- Pawl.Engine.Engine.placeBorne's CR 603.3d choosing and
-- Pawl.Engine.Resolve.resolveModes' CR 608.2b re-check -- so the rule re-judges
-- what the choice was offered against.
--
-- A REWRITE rather than a Context field, unlike the AMOUNT half of the same
-- environment (slotContext's boundAmounts): a player substituted into an atom
-- leaves the slot answerable by itself, where a bound reading CR 603.2's number
-- has to reach Pawl.Engine.Quantity, which takes a Context and no bindings. The
-- order is not CR 601.2c's
-- simultaneity problem that a slot depending on ANOTHER SLOT is
-- (Pawl.Types.GraveyardScope's InSlot, two passes in legalSetsGiven): a trigger's
-- event bindings are fixed before the ability is put on the stack, so nothing
-- being chosen now can change them.
bakeSlots :: Map SlotName PlayerId -> Map SlotName TargetSlot -> Map SlotName TargetSlot
bakeSlots players = fmap (bakeSlot players)

-- The slot's `amount` is baked beside its Filter, and by the matching function:
-- CR 202.3's computed bound is a Quantity, and a Quantity names CR 603.2's
-- bindings through PlayerRef.InSlot exactly as a Filter names them through
-- ControlledByBound. An unbaked one would be unanswerable where the filter beside
-- it is answerable. A REGRESSION FENCE rather than a proved behaviour: no
-- committed bound holds a PlayerRef.InSlot -- the pool's bounds name either
-- Quantity.LifeGainedThisTurn against a PlayerRef.Relative, which baking leaves
-- alone, or Quantity.InSlot, which carries no PlayerRef at all -- so no board
-- today tells the two readings apart.
bakeSlot :: Map SlotName PlayerId -> TargetSlot -> TargetSlot
bakeSlot players slot =
  slot
    { TargetSlot.filter = fmap (Filter.bakeBound players) (TargetSlot.filter slot),
      TargetSlot.amount = fmap (Quantity.bakeBound players) (TargetSlot.amount slot)
    }

-- bakeSlots over a whole modal payload, for the caller that must bake BEFORE the
-- modes are chosen: CR 700.2b's mode selection asks which modes are fillable
-- (fillableModes), and an unbaked slot admits nothing, which would take a
-- perfectly fillable trigger off the stack under CR 603.3c.
bakeModal :: Map SlotName PlayerId -> Modal.Modal Card (GrantedAbility.GrantedAbility Card) -> Modal.Modal Card (GrantedAbility.GrantedAbility Card)
bakeModal players modal =
  modal {Modal.modes = fmap (\m -> m {Mode.targetSlots = bakeSlots players (Mode.targetSlots m)}) (Modal.modes modal)}
