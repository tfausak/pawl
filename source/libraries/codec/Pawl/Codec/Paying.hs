{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Paying where

import qualified Pawl.Codec.Choices as Choices
import qualified Pawl.Codec.PaymentDecision as PaymentDecision
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Paying as Paying

codec :: Codec.Codec Paying.Paying
codec = Fields.object $ do
  decision <- Fields.required "decision" PaymentDecision.codec Paying.decision
  choices <- Fields.contramap Paying.choices Choices.fields
  pure Paying.MkPaying {Paying.decision = decision, Paying.choices = choices}
