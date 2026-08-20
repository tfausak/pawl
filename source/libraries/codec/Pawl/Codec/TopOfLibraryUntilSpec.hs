module Pawl.Codec.TopOfLibraryUntilSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TopOfLibraryUntil" $ do
  -- Treasure Hunt's "until you reveal a nonland card".
  Spec.it s "MkTopOfLibraryUntil, both keys" $
    Common.assertCodec
      s
      TopOfLibraryUntil.codec
      ( TopOfLibraryUntil.MkTopOfLibraryUntil
          { TopOfLibraryUntil.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibraryUntil.filter = Filter.Not (Filter.HasCardType CardType.Land)
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  -- The filter is what ENDS the walk, so a ref without one names a walk that
  -- never stops: a decode failure rather than a library taken whole.
  Spec.it s "a walk with no filter is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} ") >>= Codec.decode TopOfLibraryUntil.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s TopOfLibraryUntil.codec
