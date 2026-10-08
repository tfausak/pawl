{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PileDraw where

import qualified Pawl.Codec.Pile as Pile
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PileDraw as PileDraw

codec :: Codec.Codec PileDraw.PileDraw
codec = Fields.object $ do
  pile <- Fields.required "pile" Pile.codec PileDraw.pile
  ordinal <- Fields.required "ordinal" Common.natural PileDraw.ordinal
  pure
    PileDraw.MkPileDraw
      { PileDraw.pile = pile,
        PileDraw.ordinal = ordinal
      }
