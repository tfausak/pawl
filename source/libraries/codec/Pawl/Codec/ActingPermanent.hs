module Pawl.Codec.ActingPermanent where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ActingPermanent as ActingPermanent

codec :: Codec.Codec ActingPermanent.ActingPermanent
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Self" ActingPermanent.Self,
      Arm.payload "Matching" (Filter.codec Keyword.codec) ActingPermanent.Matching (\x -> case x of ActingPermanent.Matching y -> Just y; _ -> Nothing)
    ]

tagOf :: ActingPermanent.ActingPermanent -> String
tagOf x = case x of
  ActingPermanent.Self {} -> "Self"
  ActingPermanent.Matching {} -> "Matching"
