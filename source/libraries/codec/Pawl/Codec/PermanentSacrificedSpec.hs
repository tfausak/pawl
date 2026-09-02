module Pawl.Codec.PermanentSacrificedSpec where

import qualified Pawl.Codec.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentSacrificed" $ do
  -- Vengeful Tracker's payload: whose sacrifice fires it, and the quality the
  -- sacrificed permanent has to have.
  Spec.it s "MkPermanentSacrificed, both keys" $
    Common.assertCodec
      s
      PermanentSacrificed.codec
      ( PermanentSacrificed.MkPermanentSacrificed
          { PermanentSacrificed.player = PlayerRelation.Opponent,
            PermanentSacrificed.filter = Filter.HasCardType CardType.Artifact
          }
      )
      " {\"player\":{\"type\":\"Opponent\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentSacrificed.codec
