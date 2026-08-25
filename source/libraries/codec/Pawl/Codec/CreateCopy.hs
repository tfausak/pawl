{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CreateCopy where

import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CreateCopy as CreateCopy

-- | The count is ELIDED when it is one, so every card that mints a single copy
-- writes the ref alone. Previously that was two whole payload SHAPES -- a bare
-- ObjectRef or a [quantity, ref] array -- told apart by JSON type; as a
-- defaulted key there is one shape with an optional field (#1305).
--
-- 'Fields.defaulted' takes the default once, where the old pair held it twice:
-- the encoder's @q == Quantity.Literal 1@ guard and the decoder's fallback had
-- to agree, or a round trip stopped being the identity.
--
-- The riders are elided the same way and for Create's reason: CR 110.5b's
-- default is no riders at all, which is every copy token in the pool but
-- Littjara Mirrorlake's.
codec :: Codec.Codec CreateCopy.CreateCopy
codec = Fields.object $ do
  quantity <- Fields.defaulted "quantity" CreateCopy.defaultQuantity Quantity.codec CreateCopy.quantity
  ref <- Fields.required "ref" ObjectRef.codec CreateCopy.ref
  riders <- Fields.defaulted "riders" EntryRiders.defaultValue EntryRiders.codec CreateCopy.riders
  pure
    CreateCopy.MkCreateCopy
      { CreateCopy.quantity = quantity,
        CreateCopy.ref = ref,
        CreateCopy.riders = riders
      }
