module Pawl.Codec.Destroy where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Regenerability as Regenerability
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Destroy as Destroy

-- | The bound slot is ELIDED when absent, so a card that says nothing about
-- counting its sweep writes only the two keys it always did. That elision was
-- previously a third array element recovered by JSON TYPE; as a named key it is
-- just an absent key, and nothing has to be told apart (#1305).
codec :: Codec.Codec Destroy.Destroy
codec =
  Fields.object $
    Destroy.MkDestroy
      <$> Fields.required "ref" ObjectRef.codec Destroy.ref
      <*> Fields.required "regenerability" Regenerability.codec Destroy.regenerability
      <*> Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Destroy.slot
