{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentActed where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.PermanentAction as PermanentAction
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentActed as PermanentActed

-- | A bare object keyed by the record's field names, the permanent written with
-- whichever codec its parameter takes.
codec :: (Typeable.Typeable permanent) => Codec.Codec permanent -> Codec.Codec (PermanentActed.PermanentActed permanent)
codec permanent = Fields.object $ do
  action <- Fields.required "action" PermanentAction.codec PermanentActed.action
  actor <- Fields.required "permanent" permanent PermanentActed.permanent
  pure PermanentActed.MkPermanentActed {PermanentActed.action = action, PermanentActed.permanent = actor}
