module Pawl.Codec.PermanentBecomesDesignatedSpec where

import qualified Pawl.Codec.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentBecomesDesignated" $ do
  Spec.it s "MkPermanentBecomesDesignated, Renowned over a controlled set" $
    Common.assertCodec
      s
      PermanentBecomesDesignated.codec
      ( PermanentBecomesDesignated.MkPermanentBecomesDesignated
          { PermanentBecomesDesignated.designation = Designation.Renowned,
            PermanentBecomesDesignated.filter = Filter.ControlledBy PlayerRelation.You
          }
      )
      " {\"designation\":{\"type\":\"Renowned\"},\"filter\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}} "
  -- The other designation, over the source itself.
  Spec.it s "MkPermanentBecomesDesignated, Monstrous over the source" $
    Common.assertCodec
      s
      PermanentBecomesDesignated.codec
      ( PermanentBecomesDesignated.MkPermanentBecomesDesignated
          { PermanentBecomesDesignated.designation = Designation.Monstrous,
            PermanentBecomesDesignated.filter = Filter.IsSource
          }
      )
      " {\"designation\":{\"type\":\"Monstrous\"},\"filter\":{\"type\":\"IsSource\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentBecomesDesignated.codec
