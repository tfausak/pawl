module Pawl.Codec.WhichCounters where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.WhichCounters as WhichCounters

-- | The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (WhichCounters.WhichCounters keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "OfKind" (CounterKind.codec keywordCodec) WhichCounters.OfKind (\x -> case x of WhichCounters.OfKind y -> Just y; _ -> Nothing),
      Arm.nullary "OfAnyKind" WhichCounters.OfAnyKind
    ]

tagOf :: WhichCounters.WhichCounters keyword -> String
tagOf x = case x of
  WhichCounters.OfKind {} -> "OfKind"
  WhichCounters.OfAnyKind -> "OfAnyKind"
