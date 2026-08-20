{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.HandAction where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.HandAction as HandAction

-- | Pawl.Codec.Clause's shape, minus the two riders a hand action has no room
-- for: CR 103.6 makes the whole window a "may", so there is no Optionality to
-- write, and no rule 103 action states a cost.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (HandAction.HandAction card)
codec cardCodec = Fields.object $ do
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) HandAction.condition
  effects <- Fields.defaulted "effects" [] (Common.list (Effect.codec cardCodec)) HandAction.effects
  pure
    HandAction.MkHandAction
      { HandAction.condition = condition,
        HandAction.effects = effects
      }
