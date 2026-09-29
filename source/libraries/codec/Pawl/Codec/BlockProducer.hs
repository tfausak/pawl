module Pawl.Codec.BlockProducer where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.BlockProducer as BlockProducer

codec :: Codec.Codec BlockProducer.BlockProducer
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Declared" BlockProducer.Declared,
      Arm.nullary "PutOntoBattlefield" BlockProducer.PutOntoBattlefield,
      Arm.payload "ByEffect" (Common.set ObjectId.codec) BlockProducer.ByEffect (\x -> case x of BlockProducer.ByEffect y -> Just y; _ -> Nothing)
    ]

tagOf :: BlockProducer.BlockProducer -> String
tagOf x = case x of
  BlockProducer.Declared -> "Declared"
  BlockProducer.PutOntoBattlefield -> "PutOntoBattlefield"
  BlockProducer.ByEffect _ -> "ByEffect"
