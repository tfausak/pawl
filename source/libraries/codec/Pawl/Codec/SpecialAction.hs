module Pawl.Codec.SpecialAction where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.SpecialAction as SpecialAction

codec :: Codec.Codec SpecialAction.SpecialAction
codec =
  Arm.tagged
    [ Arm.nullary "DiscardThisAnyTime" SpecialAction.DiscardThisAnyTime,
      Arm.payload "IgnoreThisUntilEndOfTurn" costCodec SpecialAction.IgnoreThisUntilEndOfTurn (\x -> case x of SpecialAction.IgnoreThisUntilEndOfTurn y -> Just y; _ -> Nothing)
    ]
  where
    costCodec = Cost.codec Keyword.codec
