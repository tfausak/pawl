module Pawl.Codec.SpellWasCastSpec where

import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Codec.SpellWasCast as SpellWasCast
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SpellWasCast as SpellWasCast

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SpellWasCast" $ do
  -- CR 601.2i, with the stack object's characteristics as of the cast.
  Spec.it s "MkSpellWasCast, every key" $
    Common.assertCodec
      s
      SpellWasCast.codec
      ( SpellWasCast.MkSpellWasCast
          { SpellWasCast.player = PlayerId.MkPlayerId 0,
            SpellWasCast.spell = ObjectId.MkObjectId 7,
            SpellWasCast.characteristics = ProjectedCharacteristicsSpec.testCharacteristics
          }
      )
      ( "{\"player\":0,\"spell\":7,\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "}"
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s SpellWasCast.codec
