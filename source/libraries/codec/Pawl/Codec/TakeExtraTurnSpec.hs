{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TakeExtraTurnSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TakeExtraTurn" $ do
  -- CR 500.7. The skipped phases are a SET, written ascending, and Common.set's
  -- decoder rejects a repeat so the schema's uniqueItems stays honest.
  Spec.it s "MkTakeExtraTurn, no skipped phases" $
    Common.assertCodec
      s
      TakeExtraTurn.codec
      ( TakeExtraTurn.MkTakeExtraTurn
          { TakeExtraTurn.player = PlayerRef.Relative PlayerRelation.You,
            TakeExtraTurn.skips = Set.empty
          }
      )
      """ {"player":{"type":"Relative","value":{"type":"You"}},"skips":[]} """
  Spec.it s "MkTakeExtraTurn, skipped phases are written in order" $
    Common.assertCodec
      s
      TakeExtraTurn.codec
      ( TakeExtraTurn.MkTakeExtraTurn
          { TakeExtraTurn.player = PlayerRef.Relative PlayerRelation.You,
            TakeExtraTurn.skips = Set.fromList [PhaseSelector.CombatPhase, PhaseSelector.BeginningPhase]
          }
      )
      """ {"player":{"type":"Relative","value":{"type":"You"}},"skips":[{"type":"BeginningPhase"},{"type":"CombatPhase"}]} """
  Spec.it s "has a schema" $ Common.assertHasSchema s TakeExtraTurn.codec
