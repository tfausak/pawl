-- CR 613.10 / 613.11: the continuous effects that affect PLAYERS and the RULES
-- OF THE GAME rather than the characteristics of objects. A sibling TIER to the
-- CR 613 layer system, not a layer in it: CR 613.1 makes the seven layers a
-- machine for computing object characteristics and nothing else, and
-- 613.10/613.11 both apply AFTER that machine has run.
--
-- The dependency is therefore ONE-WAY, and that is what keeps it well founded:
-- this module reads the layer machine's finished answers, while
-- Pawl.Engine.Projection is untouched by this module and never sees these types.
--
-- This module is the ONLY module that may case on Pawl.Types.PlayerEffect and
-- Pawl.Types.PlayerScope -- the standing Pawl.Engine.Resolve has over Effect,
-- Pawl.Engine.Projection over Modification, Pawl.Engine.Event over
-- TriggerCondition and Pawl.Engine.Expiry over Expiry. Every consumer asks a
-- TYPED QUESTION and never sees a constructor.
module Pawl.Engine.PlayerEffect where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaFilter as ManaFilter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.Card as Card
import Pawl.Types.Filter (Filter)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import Pawl.Types.ManaCost (ManaCost)
import Pawl.Types.ManaUnit (ManaUnit)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerEffect (PlayerEffect)
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PlayerScope (PlayerScope)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

-- CR 109.5: "you" on an object is its controller, and for a static ability the
-- CURRENT controller. `pid` is the player being asked about; `controller` is the
-- player the scope is anchored to. The argument order is (asked-about, anchor)
-- and the two are never interchangeable.
--
-- The anchor is CR 109.5's "you" for every caller but one: protectedFromTargeting
-- below passes the PROTECTED player, because CR 702.11c's "your opponents" are
-- the opponents of whoever has hexproof rather than of the effect's controller.
-- The parameter is named for the common case.
inScope :: PlayerId -> PlayerId -> PlayerScope -> Bool
inScope pid controller scope = case scope of
  PlayerScope.You -> pid == controller
  -- Every other player. Not a two-player shortcut: CR 806.1 has a free-for-all's
  -- players compete as individuals against each other, so every other player is
  -- an opponent by construction, and CR 102.2 says the same for two players.
  --
  -- CR 102.3 is the ONE reading this is wrong for -- in a game between teams a
  -- teammate is not an opponent -- and pawl has no teams to express (#175).
  PlayerScope.Opponents -> pid /= controller
  -- Thalia's ruling: "including your own".
  PlayerScope.EachPlayer -> True

-- The same scope as a SET rather than as a membership test -- CR 400.1's
-- per-player zones asked in the direction a zone fold needs it
-- (Pawl.Engine.Target.graveyardRecipients). Built ON inScope rather than beside
-- it, so there is exactly one reading of what a PlayerScope names.
--
-- CR 102.1: a player who has left keeps their row in GameState.players
-- (Player.status turns Departed, the key stays), so the fold is over
-- Game.stillPlaying rather than the map's keys, and no scope names a departed
-- seat.
--
-- Nothing is an ABSENT perspective, which is CR 109.5's "you" with nobody to be
-- -- the vacuous posture every player-referencing Filter atom takes. EachPlayer
-- is answerable anyway, because it never asks the perspective a question: "target
-- card in a graveyard" names the whole table whoever is reading it.
playersInScope :: Maybe PlayerId -> GameState -> PlayerScope -> Maybe [PlayerId]
playersInScope perspective gs scope =
  let everyone = Game.stillPlaying gs
      relative = fmap (\you -> filter (\pid -> inScope pid you scope) everyone) perspective
   in case scope of
        PlayerScope.You -> relative
        PlayerScope.Opponents -> relative
        PlayerScope.EachPlayer -> Just everyone

-- CR 604.2: every player effect applying to `pid` right now. Gathered LIVE from
-- the battlefield on every read and never captured, the same posture
-- Projection.gather takes for staticAbilities -- which is why Rule of Law
-- leaving the battlefield lifts its restriction with nothing to unwind.
--
-- The scope is resolved DYNAMICALLY (see Pawl.Types.PlayerScope): CR 611.2c
-- lets a rules-modifying effect reach objects that were not affected when it
-- began, so no set is ever frozen on this axis.
--
-- The (controller, scope, effect) triples are local: nothing outside this
-- function ever sees one.
applying :: PlayerId -> GameState -> [PlayerEffect]
applying pid gs =
  let -- Hoisted out of the walk exactly as Projection.gather hoists it: an
      -- inlined call would recompute the whole game's SetLandSubtype list once
      -- per permanent.
      setEffs = Projection.setLandSubtypeEffects gs
      -- Hoisted for the same reason, and a thunk until a permanent that actually
      -- has a player ability forces it -- so the ordinary board pays nothing for
      -- the CR 604.2 question below.
      removed = Projection.abilityRemoval gs
      fromPermanent oid = case Game.cardOf oid gs of
        Nothing -> []
        Just card -> case Card.playerAbilities card of
          -- The overwhelming majority of permanents: no ability, so no
          -- controller projection and no CR 305.7 check is paid for.
          [] -> []
          abilities -> case Projection.controllerOf oid gs of
            Nothing -> []
            Just controller ->
              -- TWO ability losses, exactly the pair Projection.gather asks about
              -- for a permanent's static abilities.
              --
              -- CR 305.7: a land whose subtype has been SET to a basic type
              -- loses its rules-text abilities, this one included (Blood Moon on
              -- Reliquary Tower).
              --
              -- CR 604.2: a static ability's continuous effect is active only
              -- while the permanent remains on the battlefield and has the
              -- ability, so a CR 613.1f layer-6 removal takes this one with it
              -- (Humility on Thalia). CR 613.6's rescue for an effect that has
              -- already STARTED to apply cannot reach it: CR 613.10/613.11 apply
              -- a player effect AFTER the seven layers have run, so it never
              -- started to apply before layer 6 and the cut is unconditional.
              if (null setEffs || Projection.liveGiven setEffs Set.empty oid gs)
                && not (removed oid)
                then fmap (\ability -> (controller, PlayerStaticAbility.scope ability, PlayerStaticAbility.effect ability)) abilities
                else []
      printed = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
      -- CR 611.2c: the stored carrier. Its controller is read off the record and
      -- never re-derived -- see Pawl.Types.ActivePlayerEffect -- while its scope is
      -- resolved live, exactly as the printed carrier's is.
      --
      -- Neither gate above touches it, because it is not an ability for CR 613.1f
      -- to remove: CR 611.2a gives a resolved spell's continuous effect a duration
      -- of its own. Humility cannot take back a Silence that has already resolved.
      storedOne active =
        ( ActivePlayerEffect.controller active,
          ActivePlayerEffect.scope active,
          ActivePlayerEffect.effect active
        )
      stored = fmap storedOne (GameState.playerEffects gs)
      keep (controller, scope, _) = inScope pid controller scope
      effectOf (_, _, effect) = effect
   in fmap effectOf (filter keep (printed <> stored))

-- CR 601.2i: how many spells this player has cast this turn. A fold over the
-- whole event log, which is exactly "this turn" because Engine.handoffTurn clears
-- it at the handoff and no reader ever drains it (scannedThrough is a watermark,
-- not a consumption). Rule of Law's ruling demands precisely this: the whole
-- turn, including spells cast before it was on the battlefield.
castsThisTurn :: PlayerId -> GameState -> Integer
castsThisTurn pid gs =
  let mine caster = caster == pid
   in toInteger (length (filter mine (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events gs)))))

-- CR 601.3: a player can begin to cast a spell only if no rule or effect
-- prohibits it. The prohibit half. Cast.permitsCastWhileSearching is not the
-- general allow half of CR 601.3 -- it is only the Panglacial Wurm timing
-- exception, one specific instance of "allows".
--
-- CR 101.2 is why this folds as a DISJUNCTION: a "can't" effect takes precedence
-- over anything allowing or directing. One applicable prohibition is enough and
-- nothing outvotes it.
--
-- Deliberately does NOT take the spell. Both prohibitions here are quality-free
-- -- "can't cast spells", "can't cast more than one spell" -- so the answer does
-- not depend on WHICH spell, and a parameter nothing reads would assert a
-- generality this engine has not built. It grows an ObjectId when CR 601.3a's
-- quality-bearing prohibitions do (#95).
prohibitsCasting :: PlayerId -> GameState -> Bool
prohibitsCasting pid gs =
  let cast = castsThisTurn pid gs
      prohibits effect = case effect of
        PlayerEffect.CantCastSpells -> True
        PlayerEffect.CantCastMoreThan limit -> cast >= toInteger limit
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        -- CR 702.18a / 702.11c restrict TARGETING, not casting: a player with
        -- shroud may cast anything, and Pawl.Engine.Target.targetable is where
        -- the restriction lands (CR 115.4, CR 601.2c).
        PlayerEffect.CantBeTargetedBy _ -> False
   in any prohibits (applying pid gs)

-- Does this spell match the cost-adjustment Filter? Evaluated against the
-- PROJECTED view (Projection.viewOfObject) -- a card type is CR 613.1d layer 4
-- and a colour is CR 613.1e layer 5 -- never a printed characteristic. The
-- perspective is the spell's own controller (CR 109.5). Runs through the
-- identity-blind Filter.matches: this module never learns which spell produced
-- the Filter.
matchesSpell :: Filter Keyword -> ObjectId -> GameState -> Bool
matchesSpell filter_ oid gs =
  -- No source in scope at this site: `oid` is the AFFECTED object, not a source.
  Filter.matches (Filter.MkContext (Projection.controllerOf oid gs) Nothing) (Projection.viewOfObject oid gs) filter_

-- CR 613.11 / 601.2f: the cost increases and the cost reductions that apply to
-- `pid` casting `oid`, as two lists.
--
-- Kept APART, never summed into one signed delta: CR 601.2f applies every
-- increase before any reduction, and CR 118.7a gives a reduction a restriction
-- an increase does not have. The two halves do not even have the same shape --
-- an increase is an amount of generic mana and a reduction is a ManaCost.
-- Pawl.Engine.Cost.applyAdjustments consumes the pair; this only decides
-- membership.
--
-- matchesSpell is called only from inside an arm that already matched a
-- cost-modifying constructor, so a board with no Thalia and no Medallion runs no
-- projections at all.
costAdjustments :: PlayerId -> ObjectId -> GameState -> ([Natural], [ManaCost])
costAdjustments pid oid gs =
  let matching :: Filter Keyword -> a -> Maybe a
      matching criterion amount = if matchesSpell criterion oid gs then Just amount else Nothing
      increaseOf effect = case effect of
        PlayerEffect.IncreaseSpellCost criterion amount -> matching criterion amount
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
      reductionOf effect = case effect of
        PlayerEffect.ReduceSpellCost criterion amount -> matching criterion amount
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
      effects = applying pid gs
   in (Maybe.mapMaybe increaseOf effects, Maybe.mapMaybe reductionOf effects)

-- CR 702.18a / 702.11c: is `pid` protected from being the target of a spell or
-- ability controlled by `caster`?
--
-- The typed question Pawl.Engine.Target.targetable asks for its
-- Recipient.ToPlayer arm, so that module never sees a PlayerEffect constructor.
-- It answers the PLAYER halves of shroud and hexproof; the permanent halves are
-- keywords and stay where the other keyword reads are.
--
-- The scope is read against `pid`, the PROTECTED player: CR 702.11c's "you" is
-- the player who has hexproof, and "your opponents" are theirs. That is a
-- DIFFERENT anchor from the one PlayerStaticAbility.scope uses (the effect's
-- controller), which is why the argument order here is (caster, protected) and
-- inScope's is (asked-about, controller). See
-- Pawl.Types.PlayerEffect.CantBeTargetedBy for where the two would come apart.
--
-- A Nothing caster is a question with no CR 109.5 "you" in it. Hexproof does not
-- stop it -- nobody's opponent -- while shroud stops it anyway, because
-- EachPlayer never asks the caster a question. That is exactly the carve-out
-- playersInScope above already makes for the same constructor, and it is the
-- rules answer too: CR 702.18a names no player at all.
--
-- MEMBERSHIP, never a tally: CR 702.18b and CR 702.11h make multiple instances
-- redundant for the player half as well as the permanent half.
protectedFromTargeting :: Maybe PlayerId -> PlayerId -> GameState -> Bool
protectedFromTargeting caster pid gs =
  let stops effect = case effect of
        PlayerEffect.CantBeTargetedBy scope -> case caster of
          Just who -> inScope who pid scope
          Nothing -> case scope of
            PlayerScope.EachPlayer -> True
            PlayerScope.Opponents -> False
            PlayerScope.You -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
   in any stops (applying pid gs)

-- CR 402.2: a player's maximum hand size, normally seven cards. NOT CR 103.5's
-- starting hand size, which is a different seven (Mulligan.openingHand) that
-- this constant deliberately does not share -- the rules keep them apart, and
-- Reliquary Tower changes only one of them.
defaultMaximumHandSize :: Natural
defaultMaximumHandSize = 7

-- CR 402.2 / 613.11: this player's maximum hand size. Nothing IS "no maximum
-- hand size" (Reliquary Tower) -- never a sentinel, and never a very large
-- number.
maximumHandSize :: PlayerId -> GameState -> Maybe Natural
maximumHandSize pid gs =
  let removes effect = case effect of
        PlayerEffect.NoMaximumHandSize -> True
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
   in if any removes (applying pid gs)
        then Nothing
        else Just defaultMaximumHandSize

-- CR 500.5 / 106.4 / 613.11: which of the unspent mana in this player's pool do
-- they keep as a step or phase ends (Upwelling, Omnath Locus of Mana)? The typed
-- question Pawl.Engine.Mana.emptyManaPools asks, so the turn-based action of CR
-- 703.4q never learns which effect answered it.
--
-- A PER-UNIT predicate rather than a Bool about the whole pool, because CR 106.4
-- loses "this mana" and a card may name only some of it: Omnath, Locus of Mana
-- keeps green and drops the rest of the same pool. The pid and the state are
-- taken FIRST so a caller sweeping a pool resolves the applicable effects once
-- and asks the units afterwards.
--
-- A DISJUNCTION over the applicable effects: two retention effects that name
-- different mana are not in conflict, so a unit either effect keeps is kept, and
-- an empty list keeps nothing. CR 613.11's timestamp order has nothing to order
-- here -- unlike a cost adjustment, no answer depends on which ran first.
--
-- Read LIVE through `applying`, like every other question here, so an Upwelling
-- destroyed earlier in the step is simply not found and the pools empty with
-- nothing to unwind (CR 604.2).
--
-- Not the finest granularity the pool asks for. A card that keeps only the mana
-- it just added (Shizuko, Karn) needs the retention to leave this player-axis
-- carrier altogether (#352).
keepsUnspentMana :: PlayerId -> GameState -> ManaUnit -> Bool
keepsUnspentMana pid gs =
  let keeps effect = case effect of
        PlayerEffect.DontLoseUnspentMana f -> Just f
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
      filters = Maybe.mapMaybe keeps (applying pid gs)
   in \unit -> any (\f -> ManaFilter.matches f unit) filters
