{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Combat where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Codec.AttackTarget as AttackTarget
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Combat as Combat

-- | Four maps keyed by an ObjectId, which is a Natural newtype, so they take
-- 'Common.naturalMap': a JSON object keyed by the decimal id rather than an array
-- of entries, which is what keeps a written-out combat diffable.
--
-- `struckFirst` is 'Fields.required' over 'Common.maybe' rather than
-- 'Fields.defaulted', so the absent state is written as an explicit null. That
-- is load-bearing beyond taste: CR 510.4's Nothing means the first combat
-- damage step has not happened, while an empty set means it has and nobody had
-- first or double strike -- so a second combat damage step is due in one case
-- and already run in the other. An encoder folding the two together would lose
-- that, which is why it has its own case in the spec.
--
-- `defenders` is a LIST and not a set: CR 802.4 and CR 802.5 both read it in
-- APNAP order, so the order is part of the value.
codec :: Codec.Codec Combat.Combat
codec = Fields.object $ do
  attackers <- Fields.defaulted "attackers" Map.empty (Common.naturalMap ObjectId.codec AttackTarget.codec) Combat.attackers
  blockers <- Fields.defaulted "blockers" Map.empty (Common.naturalMap ObjectId.codec (Common.set ObjectId.codec)) Combat.blockers
  struckFirst <- Fields.required "struckFirst" (Common.maybe (Common.set ObjectId.codec)) Combat.struckFirst
  joinedUnder <- Fields.defaulted "joinedUnder" Map.empty (Common.naturalMap ObjectId.codec PlayerId.codec) Combat.joinedUnder
  attackedUnder <- Fields.defaulted "attackedUnder" Map.empty (Common.naturalMap ObjectId.codec PlayerId.codec) Combat.attackedUnder
  attackedControlledBy <- Fields.defaulted "attackedControlledBy" Map.empty (Common.naturalMap ObjectId.codec PlayerId.codec) Combat.attackedControlledBy
  attacked <- Fields.defaulted "attacked" Set.empty (Common.set AttackTarget.codec) Combat.attacked
  declaredAttacked <- Fields.defaulted "declaredAttacked" Set.empty (Common.set AttackTarget.codec) Combat.declaredAttacked
  declaredAttackedThisStep <- Fields.defaulted "declaredAttackedThisStep" Set.empty (Common.set AttackTarget.codec) Combat.declaredAttackedThisStep
  declaredAttackers <- Fields.defaulted "declaredAttackers" Set.empty (Common.set ObjectId.codec) Combat.declaredAttackers
  declaredBlockers <- Fields.defaulted "declaredBlockers" Set.empty (Common.set ObjectId.codec) Combat.declaredBlockers
  blockersDeclared <- Fields.defaulted "blockersDeclared" False Common.boolean Combat.blockersDeclared
  attackingNothing <- Fields.defaulted "attackingNothing" Set.empty (Common.set ObjectId.codec) Combat.attackingNothing
  defenders <- Fields.defaulted "defenders" [] (Common.list PlayerId.codec) Combat.defenders
  pure
    Combat.MkCombat
      { Combat.attackers = attackers,
        Combat.blockers = blockers,
        Combat.struckFirst = struckFirst,
        Combat.joinedUnder = joinedUnder,
        Combat.attackedUnder = attackedUnder,
        Combat.attackedControlledBy = attackedControlledBy,
        Combat.attacked = attacked,
        Combat.declaredAttacked = declaredAttacked,
        Combat.declaredAttackedThisStep = declaredAttackedThisStep,
        Combat.declaredAttackers = declaredAttackers,
        Combat.declaredBlockers = declaredBlockers,
        Combat.blockersDeclared = blockersDeclared,
        Combat.attackingNothing = attackingNothing,
        Combat.defenders = defenders
      }
