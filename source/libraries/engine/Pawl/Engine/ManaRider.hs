-- | The interpreter for CR 106.6's additional effects
-- (Pawl.Types.ManaRiderEffect), kept in a module of its own so that the sites
-- that ASK a rider's question never learn its arms --
-- Pawl.Engine.PlayerEffect.cantBeCountered's arrangement, and for its reason.
--
-- Below Pawl.Engine.Event deliberately: Pawl.Engine.Mana, the module that
-- STAMPS a rider onto a unit, imports Event, so the reader cannot live there.
module Pawl.Engine.ManaRider where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaRider as ManaRider
import qualified Pawl.Types.ManaRiderEffect as ManaRiderEffect
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 106.6 through CR 101.2: does a rider on the mana that PAID for this
-- object stop it from being countered (Boseiju, Who Shelters All, Delighted
-- Halfling)?
--
-- Read off CR 400.7d's record -- Pawl.Types.Object.manaSpent, "what mana was
-- spent to pay those costs" -- rather than off a stored continuous effect. CR
-- 106.6a says such an effect "is created once for each mana produced", so the
-- eager reading would mint one per unit at payment time; the lazy one answers
-- the same at every moment anything can ask, because CR 701.6a's question is
-- only ever asked at Pawl.Engine.Event.counterOne and CR 601.2h's payment
-- writes the record before CR 601.2i makes the spell cast. That equivalence
-- rests on this arm having no other observer, which is what tells it from
-- `granted` below: a payload with an independently observable existence takes
-- the eager road instead.
--
-- ANY unit suffices, which is CR 106.6a again: each mana carries the clause in
-- its own right, so one Boseiju colourless among five unrestricted mana is
-- still "that mana ... spent on" the spell.
uncounterable :: ObjectId.ObjectId -> GameState.GameState -> Bool
uncounterable oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just object ->
    let stops rider = case ManaRider.effect rider of
          ManaRiderEffect.CantBeCountered -> matchesCondition oid gs rider
          ManaRiderEffect.GainsHasteUntilEndOfTurn -> False
     in any (maybe False stops . ManaUnit.rider) (Mana.unwrap (Object.manaSpent object))

-- | CR 106.6a's eager road, and the one CR 611.2 effect in the engine that no
-- resolution mints: "if the spell or ability creates a continuous effect ... if
-- the mana is spent, a separate effect is created once for each mana produced".
-- Called by Pawl.Engine.Cost's recordSpent, which is the one place that knows
-- which units went, on the same state CR 400.7d's record is written to.
--
-- ONE effect per matching UNIT rather than one over the payment, which is that
-- rule's own last clause; each takes its own timestamp, and CR 613.7 orders
-- them. Generator Servant's two colourless are the printing.
--
-- The effect names the paid-for object as CR 611.2c's fixed set of one and
-- rides CR 400.7a onto the permanent that spell becomes
-- (Pawl.Engine.Event.carryOver). That crossing is the whole of what makes the
-- grant observable: haste on a spell is nothing, and Pawl.ManaSpec's Generator
-- Servant case reads the attack rather than the stored row.
--
-- `source` is the object being paid for and not the mana's own source, which
-- Pawl.Types.ManaUnit deliberately does not carry -- mana outlives its source
-- and CR 400.7 mints a fresh id on every zone change, so there is no id left to
-- name. Nothing here reads it: an AtCleanup expiry consults no source
-- (Pawl.Engine.Expiry's sweep), and an Affected.TheseObjects set names its
-- objects without one (Pawl.Engine.Projection.affectsGiven).
--
-- CantBeCountered is the arm that does NOT come this way; `uncounterable` above
-- says why.
granted :: ObjectId.ObjectId -> Mana.Mana -> GameState.GameState -> GameState.GameState
granted oid spent gs = List.foldl' mint gs (Maybe.mapMaybe keywordOf (Mana.unwrap spent))
  where
    keywordOf unit = do
      rider <- ManaUnit.rider unit
      keyword <- case ManaRider.effect rider of
        ManaRiderEffect.CantBeCountered -> Nothing
        ManaRiderEffect.GainsHasteUntilEndOfTurn -> Just Keyword.Haste
      Monad.guard (matchesCondition oid gs rider)
      pure keyword
    mint g keyword = case Projection.controllerOf oid g of
      -- No controller is no CR 109.5 "you" to arm a duration against.
      -- Unreachable on this road -- the paid-for object is on the stack, and
      -- controllerOf falls back to its owner -- and written out because arm is
      -- total over Duration.
      Nothing -> g
      Just controller -> case Expiry.arm Map.empty controller oid Duration.UntilEndOfTurn g of
        -- UntilEndOfTurn always arms; this branch is written out for the same
        -- reason, and the payment binds no slot for a duration to have named.
        Nothing -> g
        Just expiry ->
          let (ts, g1) = Game.freshTimestamp g
              effect =
                ContinuousEffect.MkContinuousEffect
                  { ContinuousEffect.source = oid,
                    ContinuousEffect.timestamp = ts,
                    ContinuousEffect.expiry = expiry,
                    ContinuousEffect.modification = Modification.GainKeyword keyword,
                    ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
                  }
           in g1 {GameState.continuousEffects = effect : GameState.continuousEffects g1}

-- Both roads' shared half: does this rider's printed "if that mana is spent on
-- ..." clause hold of the object the mana paid for?
--
-- Matched against the object's own view under its CONTROLLER's perspective --
-- the context Pawl.Engine.Mana.admitsUnder builds for a restriction's filter,
-- and honest here for the same reason: the clause names no source and binds no
-- slot.
matchesCondition :: ObjectId.ObjectId -> GameState.GameState -> ManaRider.ManaRider -> Bool
matchesCondition oid gs rider =
  Filter.matches
    (Filter.contextFor (Game.teams gs) (Projection.controllerOf oid gs) Nothing)
    (Projection.viewOfObject oid gs)
    (ManaRider.condition rider)
