-- CR 613.10 / 613.11: the continuous effects that affect PLAYERS and the RULES
-- OF THE GAME rather than the characteristics of objects. A sibling TIER to the
-- CR 613 layer system, not a layer in it: CR 613.1 opens "the values of an
-- object's characteristics are determined by starting with the actual object",
-- so the seven layers are a machine for computing object characteristics and
-- nothing else, and 613.10/613.11 both apply AFTER that machine has run.
--
-- The dependency is therefore ONE-WAY, and that is what keeps it well founded:
-- this module reads the layer machine's finished answers (a projected view, a
-- controller, CR 604.2's "has the ability"), while Pawl.Engine.Projection is untouched
-- by this module and never sees these types.
--
-- This module is the ONLY module that may case on Pawl.Types.PlayerEffect and
-- Pawl.Types.PlayerScope -- the standing Pawl.Engine.Resolve
-- has over Effect, Pawl.Engine.Projection over Modification, Pawl.Engine.Event over
-- TriggerCondition and Pawl.Engine.Expiry over Expiry. Every consumer asks a TYPED
-- QUESTION and never sees a constructor.
module Pawl.Engine.PlayerEffect where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.Card as Card
import Pawl.Types.Filter (Filter)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import Pawl.Types.ManaCost (ManaCost)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerEffect (PlayerEffect)
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PlayerScope (PlayerScope)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

-- CR 109.5: "the words 'you' and 'your' on an object refer to the object's
-- controller ... for a static ability, this is the current controller of the
-- object it's on". `pid` is the player being asked about; `controller` is the
-- effect's controller. The argument order is (asked-about, effect's) and the two
-- are never interchangeable.
inScope :: PlayerId -> PlayerId -> PlayerScope -> Bool
inScope pid controller scope = case scope of
  PlayerScope.You -> pid == controller
  -- Every other player. Not a two-player shortcut: CR 806.1 has a free-for-all's
  -- players compete as individuals against each other, so every other player is
  -- an opponent by construction and `pid /= controller` is exactly that. CR 102.2
  -- says the same thing for two players; the predicate serves both.
  --
  -- CR 102.3 is the ONE reading this is wrong for -- in a game between teams a
  -- teammate is not an opponent -- and pawl has no teams to express (#175).
  --
  -- Pinned at three seats by PlayerEffectSpec's "CR 806.1 at three seats
  -- Silence stops BOTH opponents, and still spares the caster".
  PlayerScope.Opponents -> pid /= controller
  -- Thalia's ruling: "including your own".
  PlayerScope.EachPlayer -> True

-- CR 604.2: every player effect applying to `pid` right now. Gathered LIVE from
-- the battlefield on every read and never captured, the same posture
-- Projection.gather takes for staticAbilities -- which is why Rule of Law
-- leaving the battlefield lifts its restriction with nothing to unwind.
--
-- The scope is resolved DYNAMICALLY (see Pawl.Types.PlayerScope): CR 611.2c
-- classifies a rules-modifying effect as one that "can affect objects that
-- weren't affected when that continuous effect began", so no set is ever frozen
-- on this axis.
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
              -- while the permanent "remains on the battlefield and has the
              -- ability", so a CR 613.1f layer-6 removal takes this one with it
              -- (Humility on Thalia). CR 613.6's rescue -- an effect that has
              -- STARTED to apply continues "even if the ability generating the
              -- effect is removed during this process" -- cannot reach it: CR
              -- 613.10/613.11 apply a player effect AFTER the seven layers have
              -- run, so it never started to apply before layer 6 and the cut is
              -- unconditional. That is the same shape as a static ability all of
              -- whose parts land after layer 6 (Projection.gatherStatic).
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
      -- of its own ("lasts as long as stated by the spell or ability creating it
      -- ... If no duration is stated, it lasts until the end of the game").
      -- Humility cannot take back a Silence that has already resolved.
      storedOne active =
        ( ActivePlayerEffect.controller active,
          ActivePlayerEffect.scope active,
          ActivePlayerEffect.effect active
        )
      stored = fmap storedOne (GameState.playerEffects gs)
      keep (controller, scope, _) = inScope pid controller scope
      effectOf (_, _, effect) = effect
   in fmap effectOf (filter keep (printed <> stored))

-- CR 601.2i: how many spells this player has cast this turn. A fold over P4's
-- whole log, which is exactly "this turn" because Engine.handoffTurn clears it at
-- the handoff and no reader ever drains it (scannedThrough is a watermark, not a
-- consumption). Rule of Law's ruling demands precisely this: "looks at the entire
-- turn ... even if Rule of Law wasn't on the battlefield when that spell was
-- cast."
castsThisTurn :: PlayerId -> GameState -> Integer
castsThisTurn pid gs =
  let mine caster = caster == pid
   in toInteger (length (filter mine (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events gs)))))

-- CR 601.3: "a player can begin to cast a spell only if ... no rule or effect
-- prohibits that player from casting it". The prohibit half. Cast
-- .permitsCastWhileSearching is not the general allow half of CR 601.3 (the
-- ordinary casting permission) -- it is only the Panglacial Wurm timing
-- exception, one specific instance of "allows".
--
-- CR 101.2 is why this folds as a DISJUNCTION: "When a rule or effect allows or
-- directs something to happen, and another effect states that it can't happen,
-- the 'can't' effect takes precedence." One applicable prohibition is enough and
-- nothing outvotes it.
--
-- Deliberately does NOT take the spell. Both of P7's prohibitions are
-- quality-free -- "can't cast spells", "can't cast more than one spell" -- so the
-- answer does not depend on WHICH spell, and a parameter nothing reads would
-- assert a generality this phase has not built. It grows an ObjectId when CR
-- 601.3a's quality-bearing prohibitions do (#95).
prohibitsCasting :: PlayerId -> GameState -> Bool
prohibitsCasting pid gs =
  let cast = castsThisTurn pid gs
      prohibits effect = case effect of
        PlayerEffect.CantCastSpells -> True
        PlayerEffect.CantCastMoreThan limit -> cast >= toInteger limit
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana -> False
   in any prohibits (applying pid gs)

-- Does this spell match the cost-adjustment Filter? Evaluated against the
-- PROJECTED view (Projection.viewOfObject) -- a card type is CR 613.1d layer 4
-- and a colour is CR 613.1e layer 5 -- never a printed characteristic, per the
-- standing house rule. The perspective is the spell's own controller (CR 109.5),
-- harmless to today's card-type/colour filters and well-defined for a future
-- ControlledBy filter. Runs through the identity-blind Filter.matches: this
-- module never learns which spell produced the Filter.
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
-- an increase is an amount of generic mana (a Natural) and a reduction is an
-- amount of mana that may name a type (a ManaCost). Pawl.Engine.Cost.applyAdjustments is
-- what consumes the pair; this function only decides membership.
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
        PlayerEffect.DontLoseUnspentMana -> Nothing
      reductionOf effect = case effect of
        PlayerEffect.ReduceSpellCost criterion amount -> matching criterion amount
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana -> Nothing
      effects = applying pid gs
   in (Maybe.mapMaybe increaseOf effects, Maybe.mapMaybe reductionOf effects)

-- CR 402.2: "each player has a maximum hand size, which is normally seven
-- cards." This is NOT CR 103.5's starting hand size, which is a different seven
-- (Mulligan.openingHand) that this constant deliberately does not share -- the
-- rules keep them apart, and Reliquary Tower changes only one of them.
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
        PlayerEffect.DontLoseUnspentMana -> False
   in if any removes (applying pid gs)
        then Nothing
        else Just defaultMaximumHandSize

-- CR 500.5 / 106.4 / 613.11: does this player keep the unspent mana in their
-- mana pool as a step or phase ends (Upwelling)? The typed question
-- Pawl.Engine.Mana.emptyManaPools asks, so the turn-based action of CR 703.4q never
-- learns which effect answered it.
--
-- A Bool about the WHOLE pool, which is exactly as much as today's carrier can
-- say. A card that keeps only mana of a stated type (Omnath, Locus of Mana)
-- needs this to become a per-unit predicate (#351), and one that keeps only the
-- mana it just added (Shizuko, Karn) needs the retention to leave this carrier
-- altogether (#352).
--
-- Read LIVE through `applying`, like every other question here, so an Upwelling
-- destroyed earlier in the step is simply not found and the pools empty with
-- nothing to unwind (CR 604.2). Pinned by ManaSpec's "CR 604.2 destroying
-- Upwelling restores the emptying in the same step".
keepsUnspentMana :: PlayerId -> GameState -> Bool
keepsUnspentMana pid gs =
  let keeps effect = case effect of
        PlayerEffect.DontLoseUnspentMana -> True
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.NoMaximumHandSize -> False
   in any keeps (applying pid gs)
