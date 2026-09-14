{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Convoking where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Convoking as Convoking

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec Convoking.Convoking
codec = Fields.object $ do
  spell <- Fields.required "spell" ObjectId.codec Convoking.spell
  convokedBy <- Fields.required "convokedBy" (Common.set ObjectId.codec) Convoking.convokedBy
  pure
    Convoking.MkConvoking
      { Convoking.spell = spell,
        Convoking.convokedBy = convokedBy
      }
