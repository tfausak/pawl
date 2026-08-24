-- | The interpreter for CR 106.6's additional effects
-- (Pawl.Types.ManaRiderEffect), kept in a module of its own so that the sites
-- that ASK a rider's question never learn its arms --
-- Pawl.Engine.PlayerEffect.cantBeCountered's arrangement, and for its reason.
--
-- Below Pawl.Engine.Event deliberately: Pawl.Engine.Mana, the module that
-- STAMPS a rider onto a unit, imports Event, so the reader cannot live there.
module Pawl.Engine.ManaRider where

import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaRider as ManaRider
import qualified Pawl.Types.ManaRiderEffect as ManaRiderEffect
import qualified Pawl.Types.ManaUnit as ManaUnit
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
-- the same at every moment anything can ask, because CR 113.6g's question is
-- only ever asked at Pawl.Engine.Event.counterOne and the record is written
-- before CR 601.2i makes the spell cast. That equivalence rests on pawl having
-- no per-object continuous-effect carrier (#2246) and on this type's one arm
-- having no other observer; a payload with an independently observable
-- existence -- a haste grant, a +1\/+1 counter -- would need the eager route.
--
-- ANY unit suffices, which is CR 106.6a again: each mana carries the clause in
-- its own right, so one Boseiju colourless among five unrestricted mana is
-- still "that mana ... spent on" the spell.
--
-- The condition is matched against the object's own view under its
-- CONTROLLER's perspective -- the context Pawl.Engine.Mana.spendableAmong
-- builds for a restriction's filter, and honest here for the same reason: the
-- clause names no source and binds no slot.
uncounterable :: ObjectId.ObjectId -> GameState.GameState -> Bool
uncounterable oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just object ->
    let context = Filter.contextFor (Projection.controllerOf oid gs) Nothing
        view = Projection.viewOfObject oid gs
        stops rider = case ManaRider.effect rider of
          ManaRiderEffect.CantBeCountered -> Filter.matches context view (ManaRider.condition rider)
     in any (maybe False stops . ManaUnit.rider) (Mana.unwrap (Object.manaSpent object))
