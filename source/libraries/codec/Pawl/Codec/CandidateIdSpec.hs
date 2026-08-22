module Pawl.Codec.CandidateIdSpec where

import qualified Pawl.Codec.CandidateId as CandidateId
import qualified Pawl.Codec.PermanentCandidateSpec as PermanentCandidateSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate
import qualified Pawl.Types.InstanceOrdinal as InstanceOrdinal
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PermanentCandidate as PermanentCandidate
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CandidateId" $ do
  Spec.it s "OfPermanent" $
    Common.assertCodec
      s
      CandidateId.codec
      ( CandidateId.OfPermanent
          PermanentCandidate.MkPermanentCandidate
            { PermanentCandidate.source = ObjectId.MkObjectId 4,
              PermanentCandidate.effect = PermanentCandidateSpec.effect,
              PermanentCandidate.ordinal = InstanceOrdinal.MkInstanceOrdinal 0
            }
      )
      " {\"type\":\"OfPermanent\",\"value\":{\"source\":4,\"effect\":{\"type\":\"ZoneChangeR\",\"value\":{\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"}},\"destination\":{\"type\":\"Exile\"}}},\"ordinal\":0}} "
  Spec.it s "OfFloating" $
    Common.assertCodec
      s
      CandidateId.codec
      ( CandidateId.OfFloating
          FloatingCandidate.MkFloatingCandidate
            { FloatingCandidate.source = ObjectId.MkObjectId 3,
              FloatingCandidate.timestamp = Timestamp.MkTimestamp 8
            }
      )
      " {\"type\":\"OfFloating\",\"value\":{\"source\":3,\"timestamp\":8}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CandidateId.codec
