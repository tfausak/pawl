module Pawl.Engine.Target where

import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.ModeIndex (ModeIndex)
import qualified Pawl.Types.ModeIndex as ModeIndex
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.ProjectedCharacteristics as PC
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.TargetSpec (TargetSpec)
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- CR 115: a target slot's legal recipients -- the set its spec admits
-- (admittedRecipients below), less every candidate rule 702 forbids TARGETING
-- (targetable below, where shroud, hexproof and the restrictions after them
-- live), less CR 115.5's one candidate, a spell or ability on the stack being an
-- illegal target for itself.
--
-- CR 115.5 is subtracted HERE and not in admittedGiven because it is a TARGETING
-- rule, exactly as rule 702's restrictions are: what an enchant spec admits (CR
-- 303.4c, Sba.stillLegalEnchant) asks no targeting question at all. Both of CR
-- 115's moments honour it, since both route through this function -- CR 601.2c's
-- choosing and CR 608.2b's re-validation.
--
-- ITS GATE IS THE RULE'S OWN WORDS, "on the stack": `source` is the object on the
-- stack only for a SPELL. A permanent's activated ability passes the source
-- PERMANENT, so this subtracts nothing there -- the right answer rather than a
-- happy accident, since the ability and not the permanent is the object CR 115.5
-- speaks of, so Prodigal Sorcerer may still ping itself. An ability targeting
-- ITSELF is the case this cannot reach, because the ability object's own id is
-- not in this frame at all (#638).
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
legalRecipients :: Maybe PlayerId -> ObjectId -> TargetSpec -> GameState -> Set Recipient
legalRecipients perspective source spec gs =
  -- The SAME thunk both halves read, so the whole-board projection is still
  -- taken at most once per slot (admittedGiven's own note).
  let pcs = Projection.projectAll gs
      -- CR 115.5's gate, hoisted out of the fold: one scan of the stack per
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
      -- nor the control-grant walk it would force. That is the same posture
      -- opponentOf's controller read takes below.
      sourceView = Projection.viewOfObjectGiven pcs (Projection.controlGrants gs) source gs
      keep recipient =
        not (sourceOnStack && Recipient.objectOf recipient == Just source)
          && targetable pcs perspective source sourceView gs recipient
   in Set.filter keep (admittedGiven pcs perspective source spec gs)

-- CR 115.1 / CR 303.4c / CR 701.3a: the recipients the SPEC itself admits -- its
-- Pool's base candidate set (CR 115.4's "any target" is creatures and
-- planeswalkers on the battlefield plus players still in the game; the battles
-- that rule also names are not admitted, #302) narrowed by its Filter. Rule 702's
-- targeting restrictions are NOT applied.
--
-- Separate from legalRecipients because "can't be the target of" and "is an
-- illegal object to be attached to" are different questions, and rule 702 says so
-- itself: protection states both halves separately (CR 702.16b for targeting, CR
-- 702.16c for attachment), while shroud (CR 702.18) and hexproof (CR 702.11)
-- state only the first -- so an Aura already attached to a permanent that has
-- shroud stays attached, and Resolve.attachmentFor may move one onto it.
--
-- Hence the two callers here rather than at legalRecipients:
-- Sba.stillLegalEnchant's general path (CR 303.4c) and Resolve.attachmentFor (CR
-- 701.3a). Both ask what the enchant SPEC admits; neither is a player choosing a
-- target.
admittedRecipients :: Maybe PlayerId -> ObjectId -> TargetSpec -> GameState -> Set Recipient
admittedRecipients perspective source spec gs = admittedGiven (Projection.projectAll gs) perspective source spec gs

admittedGiven :: Map ObjectId PC.ProjectedCharacteristics -> Maybe PlayerId -> ObjectId -> TargetSpec -> GameState -> Set Recipient
admittedGiven pcs perspective source spec gs =
  let pool = TargetSpec.pool spec
      narrowing = TargetSpec.filter spec
      context = Filter.MkContext perspective (Just source)
      -- ONE whole-board projection and ONE control-grant walk for the whole
      -- slot: both the base pool's creature test and the Filter's per-candidate
      -- view are asked of every object on the battlefield, and each was a fresh
      -- Projection.gather (#200). The snapshot argument is at
      -- Projection.projectGiven, and holds here because this is a pure function of
      -- one GameState.
      --
      -- `pcs` is the CALLER's thunk so that legalRecipients' restriction pass and
      -- this admission pass share one projection rather than taking two. Thunks,
      -- so a slot that asks neither question pays for neither.
      grants = Projection.controlGrants gs
      keep recipient = case recipient of
        -- CR 115.1: a player candidate is narrowed too ("target opponent"), by a
        -- Filter that asks about the player rather than about an object -- the
        -- IsPlayer atom (#168). Every object-shaped atom is vacuously False
        -- against a player view, so a spec that says "target creature you
        -- control" cannot accidentally admit a player.
        Recipient.ToPlayer pid -> against (Filter.playerView pid)
        Recipient.ToCreature oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
        Recipient.ToPlaneswalker oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
        Recipient.ToObject oid -> against (Projection.viewOfObjectGiven pcs grants oid gs)
      against view = case narrowing of
        Nothing -> True
        Just f -> Filter.matches context view f
   in Set.filter keep (basePoolGiven pcs context pool gs)

-- CR 702.18a (shroud) and CR 702.11b/702.11d (hexproof): THE
-- targeting-restriction gate, the one every restriction rule 702 states lands
-- in. It is asked of a candidate the spec has already admitted, and it answers
-- with CR 101.2's "can't": what it rejects is gone, so no Filter can put it back.
-- Both of CR 115's moments route through legalRecipients, so neither needs a
-- clause of its own here.
--
-- The two restrictions differ in the two things this function reads that
-- Filter.matches does not, which is the whole reason they are separate keywords
-- rather than one keyword with a field:
--
--   * WHO IS AIMING. Shroud names no player, so it stops the permanent's own
--     controller as readily as anyone else, while hexproof's "your opponents
--     control" makes the answer depend on the targeting player. `perspective` is
--     that player -- CR 109.5's "you" -- and CR 702.11b's "your" is the
--     CANDIDATE's controller, which CR 109.5 fixes for a static ability.
--     opponentOf below is that comparison, and both of rule 702.11's permanent
--     clauses carry it.
--   * WHAT IS AIMING. CR 702.11d's variant adds a quality the SOURCE must have
--     -- "[quality] spells your opponents control or abilities your opponents
--     control from [quality] sources" -- which `sourceView` answers and plain
--     hexproof never asks. Protection (CR 702.16b) asks the same question of the
--     same view, in the same two halves; it is not built (#555 covers only rule
--     702.11d).
--
-- The quality is matched against the source with the CANDIDATE's controller as
-- the Context's perspective, not the targeting player's: rule 702.11d's ability
-- is a static ability of the candidate, so CR 109.5 fixes its "you" as the
-- candidate's controller. No quality in the pool reads a perspective at all --
-- CR 702.16a's list of what a quality may be is "any characteristic value or
-- information", and every printed one is a colour or a card type -- but the frame
-- has to be right before one does.
--
-- MEMBERSHIP, never the projection's per-keyword count, which both CR 702.18b and
-- CR 702.11h say outright. Membership OF EACH KEY rather than of one key, for
-- hexproof: the quality rides the constructor, so a permanent's hexproof
-- abilities are however many keys of its keyword map happen to be Hexproof, and
-- CR 702.11h's "the same hexproof ability" is per key. Rule 702.11f's card, which
-- prints two, is exactly the one a single-key lookup would get wrong.
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
-- The restrictions after these two widen this function and nothing else.
-- Protection (CR 702.16b) needs the SOURCE's characteristics, which is
-- `sourceView` -- already here, since CR 702.11d needed it first.
--
-- CR 702.18a's "or player" half and CR 702.11c's come in through a DIFFERENT
-- reader. A player has no keywords: rule 702's keywords live on objects and are
-- folded by the CR 613.1-613.7 layers, so the player halves ride the CR
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
-- and Cost.totalMana both call `applying` once per card in hand per legalActions
-- pass), and the benchmarks were unmoved. Hoisting `applying` per enumeration the
-- way `pcs` is hoisted is #435's question, and #578 would catch it regressing.
--
-- EXHAUSTIVE over Recipient rather than routed through Recipient.objectOf: with
-- the player arm split out, an objectOf-shaped match would leave a Nothing branch
-- no input reaches and would silently swallow a new constructor. CR 120.3h's
-- battle (#302) must break this build rather than default to targetable.
targetable :: Map ObjectId PC.ProjectedCharacteristics -> Maybe PlayerId -> ObjectId -> Filter.View -> GameState -> Recipient -> Bool
targetable pcs perspective source sourceView gs recipient =
  let restrictedObject oid =
        let keywords = Projection.keywordsGiven pcs oid gs
            -- The candidate's controller, read at most once and only where a
            -- hexproof ability is present to ask about it -- both readers of it
            -- sit behind the `hexproofs` list being non-empty, which is no
            -- candidate at all on almost every board. See opponentOf.
            controller = Projection.controllerOf oid gs
            hexproofs = Maybe.mapMaybe hexproofQuality (Map.keys keywords)
            -- CR 702.11b's Nothing stops every spell an opponent controls; CR
            -- 702.11d's Just stops only the ones whose source has the quality. CR
            -- 702.11f's card has several of these and is stopped by ANY of them,
            -- that rule making it several abilities rather than one compound one.
            stops quality = case quality of
              Nothing -> True
              Just f -> Filter.matches (Filter.MkContext controller (Just source)) sourceView f
            -- The three conjuncts are in cost order, and the order is the whole
            -- reason `sourceView` costs nothing on an ordinary board: no hexproof
            -- ability at all reads no controller, a hexproof ability its own
            -- controller is aiming past reads no source view, and only the last
            -- conjunct forces it.
            restricted =
              Map.member Keyword.Shroud keywords
                || (not (null hexproofs) && opponentOf perspective controller && any stops hexproofs)
         in not (Set.member oid (GameState.battlefield gs) && restricted)
   in case recipient of
        Recipient.ToPlayer pid -> not (PlayerEffect.protectedFromTargeting perspective pid gs)
        Recipient.ToCreature oid -> restrictedObject oid
        Recipient.ToPlaneswalker oid -> restrictedObject oid
        Recipient.ToObject oid -> restrictedObject oid

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
-- one admittedGiven hoists: it is built only for a candidate that already HAS a
-- hexproof ability, which is no candidate at all on almost every board. Threading
-- the hoisted list through is the fix if one ever makes the rebuild matter.
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

-- The closed part: build the pool's base recipient set over zones, tagging each
-- candidate with how it is referenced (CR 115). Each arm is one of the
-- per-recipient builders below and nothing else.
--
-- The Context is the SAME one the Filter is matched against, and only the
-- graveyard arm reads it: CR 400.1's per-player zones make a pool that names one
-- have to say whose, and the Context's perspective is what answers. CR 109.5's
-- would-be controller is why that is the right frame -- the player CR 601.2c has
-- choosing targets. Every battlefield, stack and EXILE arm ignores it, because
-- those zones are shared by all players (CR 400.1 again).
basePoolGiven :: Map ObjectId PC.ProjectedCharacteristics -> Filter.Context -> Pool.Pool -> GameState -> Set Recipient
basePoolGiven pcs context pool gs = case pool of
  Pool.Creatures -> creatureRecipientsGiven pcs gs
  Pool.Players -> playerRecipients gs
  Pool.AnyTarget ->
    Set.unions
      [ creatureRecipientsGiven pcs gs,
        planeswalkerRecipientsGiven pcs gs,
        playerRecipients gs
      ]
  Pool.Permanents -> permanentRecipients gs
  Pool.Spells -> spellRecipients gs
  Pool.Abilities -> abilityRecipients gs
  Pool.SpellsAndPermanents -> Set.union (spellRecipients gs) (permanentRecipients gs)
  Pool.CardsInGraveyard scope -> graveyardRecipients context scope gs
  Pool.CardsInExile -> exileRecipients gs

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

-- CR 115.4: planeswalkers on the battlefield, per playing player's zone, tagged
-- ToPlaneswalker. The same walk creatureRecipientsGiven makes and shares its
-- projection with, asking Projection.isPlaneswalkerGiven instead.
--
-- The tag is what CR 120.3c needs and CR 120.3e must not get, which is why this
-- is a pool of its own rather than a widened creatureRecipients: the two answers
-- to "what does damage to this do" are picked here, once, and carried on the
-- recipient. A permanent with BOTH card types would therefore appear under both
-- tags, which is one target choice too many (#503).
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
-- Whose graveyard is PlayerEffect.playersInScope's answer rather than a second
-- reading of CR 109.5 written here: that function folds the one membership test,
-- which is where PlayerScope.Opponents' CR 806.1 argument lives. NOT
-- Count.playersFor, which answers for a PlayerRef, whose InSlot arm names a slot
-- CR 601.2c has not filled at the moment this pool is read.
--
-- Nothing -> empty, playersInScope's report of an absent perspective -- the
-- vacuous posture every player-referencing Filter atom takes.
-- PlayerScope.EachPlayer never reaches it: "a graveyard" names the whole table
-- with no perspective to lack.
graveyardRecipients :: Filter.Context -> PlayerScope.PlayerScope -> GameState -> Set Recipient
graveyardRecipients context scope gs =
  case PlayerEffect.playersInScope (Filter.perspective context) gs scope of
    Nothing -> Set.empty
    Just pids ->
      Set.fromList
        . fmap Recipient.ToObject
        $ concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) pids

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
-- CR 406.3's face-up default is what makes the whole set offerable. No card in
-- pawl's pool exiles face down, so there is no face-down pile for CR 406.4's
-- choose-at-random rule to reach (#557).
exileRecipients :: GameState -> Set Recipient
exileRecipients gs = Set.fromList (fmap Recipient.ToObject (Set.toList (GameState.exile gs)))

-- CR 608.2b: a target that left the zone it was chosen in is illegal (its id
-- names an object that no longer exists, per CR 400.7), and legality is
-- otherwise re-judged against the spec in the current state.
stillLegal :: Maybe PlayerId -> ObjectId -> Recipient -> TargetSpec -> GameState -> Bool
stillLegal perspective source recipient spec gs = Set.member recipient (legalRecipients perspective source spec gs)

-- CR 303.4c: is `recipient` still one the spec ADMITS -- the same membership
-- question stillLegal asks, minus rule 702's targeting restrictions. See
-- admittedRecipients for why an attached Aura is not asked a targeting question.
stillAdmitted :: Maybe PlayerId -> ObjectId -> Recipient -> TargetSpec -> GameState -> Bool
stillAdmitted perspective source recipient spec gs = Set.member recipient (admittedRecipients perspective source spec gs)

-- One legal set per named slot; casting prompts with exactly this map. `source`
-- is the object the targeting is relative to -- the spell object at cast, the
-- source permanent for an ability. CR 601.2c's "another" needs no separate pass:
-- a slot that excludes its source says so with Not IsSource, and a slot that does
-- not is untouched, so Prodigal Sorcerer may still target itself with AnyTarget
-- (CR 115.4). CR 115.5's self-exclusion is
-- a DIFFERENT rule: unconditional, and firing only where its own words do, for a
-- source that is itself on the stack -- see legalRecipients.
legalSets :: Maybe PlayerId -> ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSets perspective source specs gs = fmap (\spec -> legalRecipients perspective source spec gs) specs

-- CR 700.2a: the mode indices all of whose target slots have a legal recipient
-- (a mode with no slots is trivially fillable). Self-exclusion ("another") is
-- honored because it lives in the slot's own Filter. Shared by spells (Cast) and
-- abilities (Activate/Engine).
--
-- `extra` is the slots EVERY mode carries in addition to its own -- CR 303.4a's
-- enchant slot, declared by the card rather than by a mode, which castability
-- must see or an Aura with no legal creature would be castable and then countered
-- on resolution (CR 601.2c). An ability has no enchant spec and passes Map.empty.
fillableModes :: Maybe PlayerId -> ObjectId -> Map SlotName TargetSpec -> Modal.Modal Card -> GameState -> Set ModeIndex
fillableModes perspective source extra modal gs =
  let ms = Foldable.toList (Modal.modes modal)
      fillable i m =
        let sets = legalSets perspective source (Map.union extra (Mode.targetSpecs m)) gs
         in if any Set.null (Map.elems sets)
              then Nothing
              else Just (ModeIndex.MkModeIndex i)
   in Set.fromList (Maybe.mapMaybe (uncurry fillable) (zip [0 :: Natural ..] ms))
