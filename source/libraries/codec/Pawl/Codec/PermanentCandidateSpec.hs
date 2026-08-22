module Pawl.Codec.PermanentCandidateSpec where

import qualified Pawl.Codec.PermanentCandidate as PermanentCandidate
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InstanceOrdinal as InstanceOrdinal
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PermanentCandidate as PermanentCandidate
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- Rest in Peace's rewrite, borrowed from Pawl.Codec.ReplacementEffectSpec, since
-- what this module is about is the RECORD around the effect rather than the
-- effect itself.
effect :: ReplacementEffect.ReplacementEffect a
effect =
  ReplacementEffect.ZoneChangeR
    ( ZoneChangeR.MkZoneChangeR
        ZoneChangePattern.MkZoneChangePattern
          { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
            ZoneChangePattern.whatObject = Filter.And [],
            ZoneChangePattern.whoseObject = ControllerRelation.Anyones
          }
        Zone.Exile
    )

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentCandidate" $ do
  Spec.it s "the first instance of an ability" $
    Common.assertCodec
      s
      PermanentCandidate.codec
      PermanentCandidate.MkPermanentCandidate
        { PermanentCandidate.source = ObjectId.MkObjectId 4,
          PermanentCandidate.effect = effect,
          PermanentCandidate.ordinal = InstanceOrdinal.MkInstanceOrdinal 0
        }
      " {\"source\":4,\"effect\":{\"type\":\"ZoneChangeR\",\"value\":{\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"}},\"destination\":{\"type\":\"Exile\"}}},\"ordinal\":0} "
  -- CR 702.136b: the SECOND instance of an equal ability on one source is a
  -- different identity, and the ordinal is the only field that says so.
  Spec.it s "the second instance of the same ability" $
    Common.assertCodec
      s
      PermanentCandidate.codec
      PermanentCandidate.MkPermanentCandidate
        { PermanentCandidate.source = ObjectId.MkObjectId 4,
          PermanentCandidate.effect = effect,
          PermanentCandidate.ordinal = InstanceOrdinal.MkInstanceOrdinal 1
        }
      " {\"source\":4,\"effect\":{\"type\":\"ZoneChangeR\",\"value\":{\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"}},\"destination\":{\"type\":\"Exile\"}}},\"ordinal\":1} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PermanentCandidate.codec
