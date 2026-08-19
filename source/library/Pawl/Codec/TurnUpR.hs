{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TurnUpR where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TurnUpR as TurnUpR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec TurnUpR.TurnUpR
codec = Fields.object $ do
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) TurnUpR.matching
  rewrite <- Fields.required "rewrite" TurnUpRewrite.codec TurnUpR.rewrite
  pure
    TurnUpR.MkTurnUpR
      { TurnUpR.matching = matching,
        TurnUpR.rewrite = rewrite
      }
