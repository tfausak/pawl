module Pawl.Codec.ForetellCost where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ForetellCost as ForetellCost

-- | The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ForetellCost.ForetellCost keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "Stated" (Cost.codec keywordCodec) ForetellCost.Stated (\x -> case x of ForetellCost.Stated y -> Just y; _ -> Nothing),
      Arm.payload "ManaCostReducedBy" ManaCost.codec ForetellCost.ManaCostReducedBy (\x -> case x of ForetellCost.ManaCostReducedBy y -> Just y; _ -> Nothing)
    ]

tagOf :: ForetellCost.ForetellCost keyword -> String
tagOf x = case x of
  ForetellCost.Stated {} -> "Stated"
  ForetellCost.ManaCostReducedBy {} -> "ManaCostReducedBy"
