{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Placement where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.FaceDownReason as FaceDownReason
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.Readiness as Readiness
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Placement as Placement
import qualified Pawl.Types.Readiness as Readiness.Type
import qualified Pawl.Types.TapState as TapState.Type

codec :: Codec.Codec Placement.Placement
codec = Fields.object $ do
  card <- Fields.required "card" CardName.codec Placement.card
  label <- Fields.defaulted "label" Nothing (Common.maybe Label.codec) Placement.label
  tapped <- Fields.defaulted "tapped" TapState.Type.Untapped TapState.flag Placement.tapped
  readiness <- Fields.defaulted "ready" Readiness.Type.Sick Readiness.codec Placement.readiness
  damage <- Fields.defaulted "damage" 0 Common.natural Placement.damage
  counters <- Fields.defaulted "counters" Map.empty (Common.multiset (CounterKind.codec Keyword.codec)) Placement.counters
  token <- Fields.defaulted "token" False Common.boolean Placement.token
  controller <- Fields.defaulted "controller" Nothing (Common.maybe Label.codec) Placement.controller
  attached <- Fields.defaulted "attached" Nothing (Common.maybe Label.codec) Placement.attached
  commander <- Fields.defaulted "commander" False Common.boolean Placement.commander
  face <- Fields.defaulted "face" Nothing (Common.maybe CardName.codec) Placement.face
  faceDown <- Fields.defaulted "faceDown" Nothing (Common.maybe FaceDownReason.codec) Placement.faceDown
  protector <- Fields.defaulted "protector" Nothing (Common.maybe Label.codec) Placement.protector
  pure
    Placement.MkPlacement
      { Placement.card = card,
        Placement.label = label,
        Placement.tapped = tapped,
        Placement.readiness = readiness,
        Placement.damage = damage,
        Placement.counters = counters,
        Placement.token = token,
        Placement.controller = controller,
        Placement.attached = attached,
        Placement.commander = commander,
        Placement.face = face,
        Placement.faceDown = faceDown,
        Placement.protector = protector
      }
