{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Combat where

import qualified Pawl.Codec.AttackTarget as AttackTarget
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Combat as Combat

-- | Three maps keyed by an ObjectId, which is a Natural newtype, so they take
-- 'Common.naturalMap': a JSON object keyed by the decimal id rather than an array
-- of entries, which is what keeps a written-out combat diffable.
--
-- `struckFirst` and `defender` are 'Fields.required' over 'Common.maybe' rather
-- than 'Fields.defaulted', so the absent state is written as an explicit null.
-- For `struckFirst` that is load-bearing beyond taste: CR 510.4's Nothing means
-- the first combat damage step has not happened, while an empty set means it has
-- and nobody had first or double strike -- so a second combat damage step is due
-- in one case and already run in the other. An encoder folding the two together
-- would lose that, which is why each has its own case in the spec.
codec :: Codec.Codec Combat.Combat
codec = Fields.object $ do
  attackers <- Fields.required "attackers" (Common.naturalMap ObjectId.codec AttackTarget.codec) Combat.attackers
  blockers <- Fields.required "blockers" (Common.naturalMap ObjectId.codec (Common.set ObjectId.codec)) Combat.blockers
  struckFirst <- Fields.required "struckFirst" (Common.maybe (Common.set ObjectId.codec)) Combat.struckFirst
  joinedUnder <- Fields.required "joinedUnder" (Common.naturalMap ObjectId.codec PlayerId.codec) Combat.joinedUnder
  attacked <- Fields.required "attacked" (Common.set AttackTarget.codec) Combat.attacked
  declaredAttacked <- Fields.required "declaredAttacked" (Common.set AttackTarget.codec) Combat.declaredAttacked
  declaredAttackedThisStep <- Fields.required "declaredAttackedThisStep" (Common.set AttackTarget.codec) Combat.declaredAttackedThisStep
  declaredAttackers <- Fields.required "declaredAttackers" (Common.set ObjectId.codec) Combat.declaredAttackers
  declaredBlockers <- Fields.required "declaredBlockers" (Common.set ObjectId.codec) Combat.declaredBlockers
  blockersDeclared <- Fields.required "blockersDeclared" Common.boolean Combat.blockersDeclared
  defender <- Fields.required "defender" (Common.maybe PlayerId.codec) Combat.defender
  pure
    Combat.MkCombat
      { Combat.attackers = attackers,
        Combat.blockers = blockers,
        Combat.struckFirst = struckFirst,
        Combat.joinedUnder = joinedUnder,
        Combat.attacked = attacked,
        Combat.declaredAttacked = declaredAttacked,
        Combat.declaredAttackedThisStep = declaredAttackedThisStep,
        Combat.declaredAttackers = declaredAttackers,
        Combat.declaredBlockers = declaredBlockers,
        Combat.blockersDeclared = blockersDeclared,
        Combat.defender = defender
      }
