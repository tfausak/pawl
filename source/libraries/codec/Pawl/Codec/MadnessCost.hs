module Pawl.Codec.MadnessCost where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.MadnessCost as MadnessCost

-- | The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (MadnessCost.MadnessCost keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "Stated" (Cost.codec keywordCodec) MadnessCost.Stated (\x -> case x of MadnessCost.Stated y -> Just y; _ -> Nothing),
      Arm.nullary "OwnManaCost" MadnessCost.OwnManaCost
    ]

tagOf :: MadnessCost.MadnessCost keyword -> String
tagOf x = case x of
  MadnessCost.Stated {} -> "Stated"
  MadnessCost.OwnManaCost {} -> "OwnManaCost"
