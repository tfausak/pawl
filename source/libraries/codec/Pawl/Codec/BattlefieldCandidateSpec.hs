module Pawl.Codec.BattlefieldCandidateSpec where

import qualified Pawl.Codec.BattlefieldCandidate as BattlefieldCandidate
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BattlefieldCandidate as BattlefieldCandidate
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BattlefieldCandidate" $ do
  -- Instantiated at a natural rather than at ProjectedCharacteristics: what this
  -- module has to prove is that the two fields are written under their own names
  -- and in their own order, which the inner codec being a parameter makes
  -- independent of what rides there.
  Spec.it s "a controller and what rides beside it" $
    Common.assertCodec
      s
      (BattlefieldCandidate.codec Common.natural)
      BattlefieldCandidate.MkBattlefieldCandidate
        { BattlefieldCandidate.controller = PlayerId.MkPlayerId 1,
          BattlefieldCandidate.characteristics = 7
        }
      " {\"controller\":1,\"characteristics\":7} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s (BattlefieldCandidate.codec Common.natural)
