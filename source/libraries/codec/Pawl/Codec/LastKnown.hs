{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LastKnown where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.Source as Source
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LastKnown as LastKnown

-- | All nine axes, none derivable from another: the type's own haddock says why
-- CR 608.2h needs each of them beside the projection.
codec :: Codec.Codec LastKnown.LastKnown
codec = Fields.object $ do
  characteristics <- Fields.required "characteristics" ProjectedCharacteristics.codec LastKnown.characteristics
  controller <- Fields.required "controller" PlayerId.codec LastKnown.controller
  owner <- Fields.required "owner" PlayerId.codec LastKnown.owner
  source <- Fields.required "source" Source.codec LastKnown.source
  counters <- Fields.required "counters" (Common.multiset (CounterKind.codec Keyword.codec)) LastKnown.counters
  copiable <- Fields.required "copiable" ProjectedCharacteristics.codec LastKnown.copiable
  attachedTo <- Fields.required "attachedTo" (Common.maybe Recipient.codec) LastKnown.attachedTo
  chosenNames <- Fields.required "chosenNames" (Common.set CardName.codec) LastKnown.chosenNames
  blocking <- Fields.required "blocking" Common.boolean LastKnown.blocking
  pure
    LastKnown.MkLastKnown
      { LastKnown.characteristics = characteristics,
        LastKnown.controller = controller,
        LastKnown.owner = owner,
        LastKnown.source = source,
        LastKnown.counters = counters,
        LastKnown.copiable = copiable,
        LastKnown.attachedTo = attachedTo,
        LastKnown.chosenNames = chosenNames,
        LastKnown.blocking = blocking
      }
