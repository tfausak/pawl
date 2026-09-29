{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Casting where

import qualified Pawl.Codec.Choices as Choices
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Casting as Casting

codec :: Codec.Codec Casting.Casting
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec Casting.object
  choices <- Fields.contramap Casting.choices Choices.fields
  pure
    Casting.MkCasting
      { Casting.object = object,
        Casting.choices = choices
      }
