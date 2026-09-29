{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TypesAre where

import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TypesAre as TypesAre

-- | Card types as bare names: @["Battle", "Creature"]@.
codec :: Codec.Codec TypesAre.TypesAre
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec TypesAre.object
  types <- Fields.required "types" (Common.set Arm.keyedEnum) TypesAre.types
  pure TypesAre.MkTypesAre {TypesAre.object = object, TypesAre.types = types}
