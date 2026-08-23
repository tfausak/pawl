module Pawl.Codec.ExtraTurnSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.ExtraTurn as ExtraTurn
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExtraTurn" $ do
  -- CR 500.7 with nothing skipped -- Time Warp.
  Spec.it s "a turn that skips nothing" $
    Common.assertCodec
      s
      ExtraTurn.codec
      ExtraTurn.MkExtraTurn
        { ExtraTurn.taker = PlayerId.MkPlayerId 1,
          ExtraTurn.source = ObjectId.MkObjectId 4,
          ExtraTurn.skipped = Set.empty
        }
      " {\"taker\":1,\"source\":4,\"skipped\":[]} "
  -- CR 500.11's skip, travelling WITH the turn rather than referencing it --
  -- Savor the Moment's "skip the untap step of that turn".
  Spec.it s "a turn whose untap step is skipped" $
    Common.assertCodec
      s
      ExtraTurn.codec
      ExtraTurn.MkExtraTurn
        { ExtraTurn.taker = PlayerId.MkPlayerId 2,
          ExtraTurn.source = ObjectId.MkObjectId 5,
          ExtraTurn.skipped = Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap))
        }
      " {\"taker\":2,\"source\":5,\"skipped\":[{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Untap\"}}}]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ExtraTurn.codec
