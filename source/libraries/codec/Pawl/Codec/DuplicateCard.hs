{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DuplicateCard where

import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DuplicateCard as DuplicateCard

codec :: Codec.Codec DuplicateCard.DuplicateCard
codec = Fields.object $ do
  printing <- Fields.required "printing" PrintingId.codec DuplicateCard.printing
  values <- Fields.required "values" ProjectedCharacteristics.codec DuplicateCard.values
  pure
    DuplicateCard.MkDuplicateCard
      { DuplicateCard.printing = printing,
        DuplicateCard.values = values
      }
