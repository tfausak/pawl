{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.RecipientSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Recipient" $ do
  Spec.it s "ToCreature" $
    Common.assertJsonCodec
      s
      Recipient.toJson
      Recipient.fromJson
      (Recipient.ToCreature (ObjectId.MkObjectId 1))
      """ {"type":"ToCreature","value":1} """
  -- CR 120.3c's recipient tag is a different arm of Recipient from ToObject
  -- (see Pawl.Types.Recipient's comment on it), so it gets its own case.
  Spec.it s "ToPlaneswalker" $
    Common.assertJsonCodec
      s
      Recipient.toJson
      Recipient.fromJson
      (Recipient.ToPlaneswalker (ObjectId.MkObjectId 2))
      """ {"type":"ToPlaneswalker","value":2} """
  -- CR 120.3h's is a third, for CR 115.4's fourth kind of "any target".
  Spec.it s "ToBattle" $
    Common.assertJsonCodec
      s
      Recipient.toJson
      Recipient.fromJson
      (Recipient.ToBattle (ObjectId.MkObjectId 5))
      """ {"type":"ToBattle","value":5} """
  Spec.it s "ToPlayer" $
    Common.assertJsonCodec
      s
      Recipient.toJson
      Recipient.fromJson
      (Recipient.ToPlayer (PlayerId.MkPlayerId 3))
      """ {"type":"ToPlayer","value":3} """
  Spec.it s "ToObject" $
    Common.assertJsonCodec
      s
      Recipient.toJson
      Recipient.fromJson
      (Recipient.ToObject (ObjectId.MkObjectId 4))
      """ {"type":"ToObject","value":4} """
