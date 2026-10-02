{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Taking where

import qualified Pawl.Codec.Choices as Choices
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Taking as Taking

codec :: Codec.Codec Taking.Taking
codec = Fields.object $ do
  action <- Fields.required "action" Common.text Taking.action
  choices <- Fields.contramap Taking.choices Choices.fields
  pure
    Taking.MkTaking
      { Taking.action = action,
        Taking.choices = choices
      }
