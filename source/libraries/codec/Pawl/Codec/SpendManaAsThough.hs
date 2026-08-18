{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpendManaAsThough where

import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SpendManaAsThough as SpendManaAsThough

-- | A bare object keyed by the record's field names, as every other
-- Pawl.Types.PlayerEffect payload is (#1464).
codec :: Codec.Codec SpendManaAsThough.SpendManaAsThough
codec = Fields.object $ do
  which <- Fields.required "which" ManaFilter.codec SpendManaAsThough.which
  asThough <- Fields.required "asThough" (Common.set ManaType.codec) SpendManaAsThough.asThough
  only <- Fields.required "only" Common.boolean SpendManaAsThough.only
  pure
    SpendManaAsThough.MkSpendManaAsThough
      { SpendManaAsThough.which = which,
        SpendManaAsThough.asThough = asThough,
        SpendManaAsThough.only = only
      }
