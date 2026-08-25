{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TurnUpR where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Codec.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TurnUpR as TurnUpR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
--
-- `requiring` is engine-baked -- Pawl.Types.TurnUpR says why -- so it defaults to
-- Nothing and no card file writes it, the posture Pawl.Codec.PhasePattern's
-- whosePhase takes.
codec :: Codec.Codec TurnUpR.TurnUpR
codec = Fields.object $ do
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) TurnUpR.matching
  requiring <- Fields.defaulted "requiring" Nothing (Common.maybe TurnUpProcedure.codec) TurnUpR.requiring
  rewrite <- Fields.required "rewrite" TurnUpRewrite.codec TurnUpR.rewrite
  pure
    TurnUpR.MkTurnUpR
      { TurnUpR.matching = matching,
        TurnUpR.requiring = requiring,
        TurnUpR.rewrite = rewrite
      }
