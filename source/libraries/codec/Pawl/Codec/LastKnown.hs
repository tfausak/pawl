{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LastKnown where

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

-- | One counter kind and how many of it were on the object (CR 122.1). A pair
-- per kind rather than 'Common.multiset', which spells a count by repeating the
-- element: the counts here are read as numbers, and Pawl.Codec.EntryRiders
-- writes the same shape for the same reason.
-- | All six axes, none derivable from another: the type's own haddock says why
-- CR 608.2h needs each of them beside the projection.
codec :: Codec.Codec LastKnown.LastKnown
codec = Fields.object $ do
  characteristics <- Fields.required "characteristics" ProjectedCharacteristics.codec LastKnown.characteristics
  controller <- Fields.required "controller" PlayerId.codec LastKnown.controller
  source <- Fields.required "source" Source.codec LastKnown.source
  counters <- Fields.required "counters" (Common.multiset (CounterKind.codec Keyword.codec)) LastKnown.counters
  copiable <- Fields.required "copiable" ProjectedCharacteristics.codec LastKnown.copiable
  attachedTo <- Fields.required "attachedTo" (Common.maybe Recipient.codec) LastKnown.attachedTo
  pure
    LastKnown.MkLastKnown
      { LastKnown.characteristics = characteristics,
        LastKnown.controller = controller,
        LastKnown.source = source,
        LastKnown.counters = counters,
        LastKnown.copiable = copiable,
        LastKnown.attachedTo = attachedTo
      }
