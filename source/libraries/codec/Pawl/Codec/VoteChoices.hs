module Pawl.Codec.VoteChoices where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.VoteObjects as VoteObjects
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.VoteChoices as VoteChoices

codec :: Codec.Codec VoteChoices.VoteChoices
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Objects" VoteObjects.codec VoteChoices.Objects (\x -> case x of VoteChoices.Objects y -> Just y; _ -> Nothing),
      Arm.payload "Words" (Common.nonEmpty SlotName.codec) VoteChoices.Words (\x -> case x of VoteChoices.Words y -> Just y; _ -> Nothing)
    ]

tagOf :: VoteChoices.VoteChoices -> String
tagOf x = case x of
  VoteChoices.Objects {} -> "Objects"
  VoteChoices.Words {} -> "Words"
