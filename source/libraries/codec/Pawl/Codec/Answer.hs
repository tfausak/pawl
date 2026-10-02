{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Answer where

import qualified Pawl.Codec.Reply as Reply
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Answer as Answer

codec :: Codec.Codec Answer.Answer
codec = Fields.object $ do
  prompt <- Fields.required "prompt" Common.text Answer.prompt
  with <- Fields.required "with" Reply.codec Answer.with
  pure Answer.MkAnswer {Answer.prompt = prompt, Answer.with = with}
