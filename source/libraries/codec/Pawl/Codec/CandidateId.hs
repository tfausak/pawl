module Pawl.Codec.CandidateId where

import qualified Pawl.Codec.FloatingCandidate as FloatingCandidate
import qualified Pawl.Codec.PermanentCandidate as PermanentCandidate
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CandidateId as CandidateId

codec :: Codec.Codec CandidateId.CandidateId
codec =
  Arm.tagged
    [ Arm.payload "OfPermanent" PermanentCandidate.codec CandidateId.OfPermanent (\x -> case x of CandidateId.OfPermanent y -> Just y; _ -> Nothing),
      Arm.payload "OfFloating" FloatingCandidate.codec CandidateId.OfFloating (\x -> case x of CandidateId.OfFloating y -> Just y; _ -> Nothing)
    ]
