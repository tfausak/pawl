{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AlternativeActivationCost where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.KeywordDesignator as KeywordDesignator
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AlternativeActivationCost as AlternativeActivationCost

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.PlayerEffect's AlternativeActivationCost arm.
-- `onlyFirst` is defaulted to Nothing, every matching activation.
codec :: Codec.Codec AlternativeActivationCost.AlternativeActivationCost
codec = Fields.object $ do
  grantedBy <- Fields.required "grantedBy" (KeywordDesignator.codec Keyword.codec) AlternativeActivationCost.grantedBy
  onlyFirst <- Fields.defaulted "onlyFirst" Nothing (Common.maybe TurnScope.codec) AlternativeActivationCost.onlyFirst
  cost <- Fields.required "cost" ManaCost.codec AlternativeActivationCost.cost
  pure
    AlternativeActivationCost.MkAlternativeActivationCost
      { AlternativeActivationCost.grantedBy = grantedBy,
        AlternativeActivationCost.onlyFirst = onlyFirst,
        AlternativeActivationCost.cost = cost
      }
