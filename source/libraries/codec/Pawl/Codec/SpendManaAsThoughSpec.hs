module Pawl.Codec.SpendManaAsThoughSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.SpendManaAsThough as SpendManaAsThough
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.SpendManaAsThough as SpendManaAsThough

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SpendManaAsThough" $ do
  -- CR 609.4b, as Celestial Dawn's first clause widens white to the five
  -- colours of CR 106.1a.
  Spec.it s "a permission" $
    Common.assertCodec
      s
      SpendManaAsThough.codec
      ( SpendManaAsThough.MkSpendManaAsThough
          { SpendManaAsThough.which = ManaFilter.OfType (ManaType.Colored Color.White),
            SpendManaAsThough.asThough = Set.fromList (fmap ManaType.Colored [Color.White, Color.Blue]),
            SpendManaAsThough.only = False
          }
      )
      " {\"which\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}},\"asThough\":[{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}},{\"type\":\"Colored\",\"value\":{\"type\":\"Blue\"}}],\"only\":false} "
  -- Celestial Dawn's second clause, which carries the word "only".
  Spec.it s "a restriction" $
    Common.assertCodec
      s
      SpendManaAsThough.codec
      ( SpendManaAsThough.MkSpendManaAsThough
          { SpendManaAsThough.which = ManaFilter.NotOfType (ManaType.Colored Color.White),
            SpendManaAsThough.asThough = Set.singleton ManaType.Colorless,
            SpendManaAsThough.only = True
          }
      )
      " {\"which\":{\"type\":\"NotOfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}},\"asThough\":[{\"type\":\"Colorless\"}],\"only\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SpendManaAsThough.codec
