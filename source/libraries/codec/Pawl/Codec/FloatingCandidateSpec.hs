module Pawl.Codec.FloatingCandidateSpec where

import qualified Pawl.Codec.FloatingCandidate as FloatingCandidate
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FloatingCandidate" $ do
  -- The two fields are DISTINCT here, so an encoder that swapped them could not
  -- pass: both are naturals, and equal ones would round trip either way.
  Spec.it s "a floating identity" $
    Common.assertCodec
      s
      FloatingCandidate.codec
      FloatingCandidate.MkFloatingCandidate
        { FloatingCandidate.source = ObjectId.MkObjectId 3,
          FloatingCandidate.timestamp = Timestamp.MkTimestamp 8
        }
      " {\"source\":3,\"timestamp\":8} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s FloatingCandidate.codec
