{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DoesNotUntapNext where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DoesNotUntapNext as DoesNotUntapNext

-- | A bare object keyed by the record's field names, `steps` defaulting to the
-- one step most cards print. The tag that picks it is written by
-- Pawl.Codec.Effect's DoesNotUntapNext arm.
codec :: Codec.Codec DoesNotUntapNext.DoesNotUntapNext
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec DoesNotUntapNext.ref
  steps <- Fields.defaulted "steps" 1 Common.natural DoesNotUntapNext.steps
  pure
    DoesNotUntapNext.MkDoesNotUntapNext
      { DoesNotUntapNext.ref = ref,
        DoesNotUntapNext.steps = steps
      }
