{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SubtypesAre where

import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SubtypesAre as SubtypesAre

-- | Subtypes as bare names, as Pawl.Codec.TypesAre spells card types.
codec :: Codec.Codec SubtypesAre.SubtypesAre
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec SubtypesAre.object
  subtypes <- Fields.required "subtypes" (Common.set Arm.keyedEnum) SubtypesAre.subtypes
  pure SubtypesAre.MkSubtypesAre {SubtypesAre.object = object, SubtypesAre.subtypes = subtypes}
