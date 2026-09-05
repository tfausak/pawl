{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpecialAction where

import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AbilityName as AbilityName.Type
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.SpecialAction as SpecialAction

codec :: Codec.Codec SpecialAction.SpecialAction
codec =
  Arm.tagged
    [ Arm.nullary "DiscardThisAnyTime" SpecialAction.DiscardThisAnyTime,
      Arm.payload "IgnoreThisUntilEndOfTurn" ignoreCodec (uncurry SpecialAction.IgnoreThisUntilEndOfTurn) (\x -> case x of SpecialAction.IgnoreThisUntilEndOfTurn n y -> Just (n, y); _ -> Nothing)
    ]

-- CR 116.2d's two payloads, keyed rather than positional: which ability the
-- payment ignores and what it costs.
ignoreCodec :: Codec.Codec (AbilityName.Type.AbilityName, Cost.Type.Cost Keyword.Type.Keyword)
ignoreCodec = Fields.object $ do
  ability <- Fields.required "ability" AbilityName.codec fst
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) snd
  pure (ability, cost)
