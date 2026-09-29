{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SacrificeToEnter where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SacrificeToEnter as SacrificeToEnter

codec :: Codec.Codec SacrificeToEnter.SacrificeToEnter
codec = Fields.object $ do
  count <- Fields.required "count" Common.natural SacrificeToEnter.count
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) SacrificeToEnter.filter
  pure
    SacrificeToEnter.MkSacrificeToEnter
      { SacrificeToEnter.count = count,
        SacrificeToEnter.filter = filter_
      }
