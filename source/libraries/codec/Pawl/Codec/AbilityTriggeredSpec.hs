{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AbilityTriggeredSpec where

import qualified Pawl.Codec.AbilityTriggered as AbilityTriggered
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TriggerCondition as TriggerCondition

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AbilityTriggered" $ do
  -- CR 603.2.
  Spec.it s "MkAbilityTriggered, every key" $
    Common.assertCodec
      s
      AbilityTriggered.codec
      ( AbilityTriggered.MkAbilityTriggered
          { AbilityTriggered.source = ObjectId.MkObjectId 1,
            AbilityTriggered.controller = PlayerId.MkPlayerId 0,
            AbilityTriggered.condition = TriggerCondition.SelfEnters
          }
      )
      """ {"source":1,"controller":0,"condition":{"type":"SelfEnters"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityTriggered.codec
