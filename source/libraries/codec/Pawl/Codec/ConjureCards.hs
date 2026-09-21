module Pawl.Codec.ConjureCards where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ConjureCards as ConjureCards

-- | The card codec is a PARAMETER, 'Pawl.Codec.Conjure''s posture and for its
-- reason: the written arm is where card data nests inside card data, and
-- 'Pawl.Codec.Card' is what ties the knot.
--
-- Tagged like every other sum rather than told apart by JSON type -- an array
-- being the written list and an object the ref -- which is
-- 'Pawl.Codec.ObjectRef''s own reason: no schema can state that claim as one the
-- decoder guarantees.
codec :: (Typeable.Typeable card) => Codec.Codec card -> Codec.Codec (ConjureCards.ConjureCards card)
codec cardCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "Written" (Common.nonEmpty cardCodec) ConjureCards.Written (\x -> case x of ConjureCards.Written y -> Just y; _ -> Nothing),
      Arm.payload "Duplicate" ObjectRef.codec ConjureCards.Duplicate (\x -> case x of ConjureCards.Duplicate y -> Just y; _ -> Nothing)
    ]

tagOf :: ConjureCards.ConjureCards card -> String
tagOf x = case x of
  ConjureCards.Written {} -> "Written"
  ConjureCards.Duplicate {} -> "Duplicate"
