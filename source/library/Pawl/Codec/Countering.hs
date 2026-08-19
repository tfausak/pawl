{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Countering where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Countering as Countering

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema. 'Countering.spell' and 'Countering.source' are both an ObjectId
-- and are not interchangeable, which is why they are named.
codec :: Codec.Codec Countering.Countering
codec = Fields.object $ do
  spell <- Fields.required "spell" ObjectId.codec Countering.spell
  source <- Fields.required "source" ObjectId.codec Countering.source
  controller <- Fields.required "controller" PlayerId.codec Countering.controller
  pure
    Countering.MkCountering
      { Countering.spell = spell,
        Countering.source = source,
        Countering.controller = controller
      }
