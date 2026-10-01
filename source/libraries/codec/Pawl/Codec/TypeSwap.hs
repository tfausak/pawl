{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TypeSwap where

import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TypeSwap as TypeSwap

codec :: Codec.Codec TypeSwap.TypeSwap
codec = Fields.object $ do
  from <- Fields.required "from" Subtype.codec TypeSwap.from
  to <- Fields.required "to" Subtype.codec TypeSwap.to
  pure TypeSwap.MkTypeSwap {TypeSwap.from = from, TypeSwap.to = to}
