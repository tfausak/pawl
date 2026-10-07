{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SourceChoices where

import qualified Data.Set as Set
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SourceChoices as SourceChoices

codec :: Codec.Codec SourceChoices.SourceChoices
codec = Fields.object $ do
  names <- Fields.defaulted "names" Set.empty (Common.set CardName.codec) SourceChoices.names
  colors <- Fields.defaulted "colors" Set.empty (Common.set Color.codec) SourceChoices.colors
  subtype <- Fields.defaulted "subtype" Nothing (Common.maybe Subtype.codec) SourceChoices.subtype
  pure
    SourceChoices.MkSourceChoices
      { SourceChoices.names = names,
        SourceChoices.colors = colors,
        SourceChoices.subtype = subtype
      }
