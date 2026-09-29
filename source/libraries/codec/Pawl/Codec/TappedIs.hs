{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TappedIs where

import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TappedIs as TappedIs

codec :: Codec.Codec TappedIs.TappedIs
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec TappedIs.object
  tapped <- Fields.required "tapped" TapState.flag TappedIs.tapped
  pure TappedIs.MkTappedIs {TappedIs.object = object, TappedIs.tapped = tapped}
