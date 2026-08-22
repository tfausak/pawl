{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ReduceActivationCost where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.KeywordFamily as KeywordFamily
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.PlayerEffect's ReduceActivationCost arm.
--
-- `grantedBy` is DEFAULTED to Nothing, which is "every activated ability of a
-- matching source" -- the shape Heartstone and Blossoming Tortoise print, so
-- neither card file says it.
codec :: Codec.Codec ReduceActivationCost.ReduceActivationCost
codec = Fields.object $ do
  whichAbilities <- Fields.required "whichAbilities" (Filter.codec Keyword.codec) ReduceActivationCost.whichAbilities
  grantedBy <- Fields.defaulted "grantedBy" Nothing (Common.maybe KeywordFamily.codec) ReduceActivationCost.grantedBy
  reduction <- Fields.required "reduction" ManaCost.codec ReduceActivationCost.reduction
  floor_ <- Fields.required "floor" Common.natural ReduceActivationCost.floor
  pure
    ReduceActivationCost.MkReduceActivationCost
      { ReduceActivationCost.whichAbilities = whichAbilities,
        ReduceActivationCost.grantedBy = grantedBy,
        ReduceActivationCost.reduction = reduction,
        ReduceActivationCost.floor = floor_
      }
