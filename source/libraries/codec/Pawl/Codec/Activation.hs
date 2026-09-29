{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Activation where

import qualified Pawl.Codec.Choices as Choices
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Activation as Activation

codec :: Codec.Codec Activation.Activation
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec Activation.object
  ability <- Fields.defaulted "ability" Nothing (Common.maybe Common.natural) Activation.ability
  choices <- Fields.contramap Activation.choices Choices.fields
  pure
    Activation.MkActivation
      { Activation.object = object,
        Activation.ability = ability,
        Activation.choices = choices
      }
