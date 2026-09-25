{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LastKnown where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.AttackTarget as AttackTarget
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.ControlClock as ControlClock
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Source as Source
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LastKnown as LastKnown

-- | All sixteen axes, none derivable from another: the type's own haddock says why
-- CR 608.2h needs each of them beside the projection.
codec :: Codec.Codec LastKnown.LastKnown
codec = Fields.object $ do
  characteristics <- Fields.required "characteristics" ProjectedCharacteristics.codec LastKnown.characteristics
  controller <- Fields.required "controller" PlayerId.codec LastKnown.controller
  owner <- Fields.required "owner" PlayerId.codec LastKnown.owner
  source <- Fields.required "source" Source.codec LastKnown.source
  counters <- Fields.required "counters" (Common.multiset (CounterKind.codec Keyword.codec)) LastKnown.counters
  copiable <- Fields.required "copiable" ProjectedCharacteristics.codec LastKnown.copiable
  attached <- Fields.required "attached" (Common.set ObjectId.codec) LastKnown.attached
  host <- Fields.defaulted "host" Nothing (Common.maybe ObjectId.codec) LastKnown.host
  chosenNames <- Fields.required "chosenNames" (Common.set CardName.codec) LastKnown.chosenNames
  attacking <- Fields.required "attacking" Common.boolean LastKnown.attacking
  attackTarget <- Fields.required "attackTarget" (Common.maybe AttackTarget.codec) LastKnown.attackTarget
  blocking <- Fields.required "blocking" Common.boolean LastKnown.blocking
  protector <- Fields.required "protector" (Common.maybe PlayerId.codec) LastKnown.protector
  paidCosts <- Fields.defaulted "paidCosts" Map.empty (Common.multiset Keyword.codec) LastKnown.paidCosts
  controlClock <- Fields.defaulted "controlClock" Map.empty (Common.keyedList ControlClock.entry) LastKnown.controlClock
  zone <- Fields.required "zone" Zone.codec LastKnown.zone
  pure
    LastKnown.MkLastKnown
      { LastKnown.characteristics = characteristics,
        LastKnown.controller = controller,
        LastKnown.owner = owner,
        LastKnown.source = source,
        LastKnown.counters = counters,
        LastKnown.copiable = copiable,
        LastKnown.attached = attached,
        LastKnown.host = host,
        LastKnown.chosenNames = chosenNames,
        LastKnown.attacking = attacking,
        LastKnown.attackTarget = attackTarget,
        LastKnown.blocking = blocking,
        LastKnown.protector = protector,
        LastKnown.paidCosts = paidCosts,
        LastKnown.controlClock = controlClock,
        LastKnown.zone = zone
      }
