{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TappedForMana where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TappedForMana as TappedForMana

-- | A bare object keyed by the record's field names. Both keys required: this
-- is a log entry rather than card data, written by one producer that always
-- knows both halves (Pawl.Engine.Cost.applyManaTriggers), so an absent `mana`
-- would only ever be a truncated document.
codec :: Codec.Codec TappedForMana.TappedForMana
codec = Fields.object $ do
  permanent <- Fields.required "permanent" ObjectId.codec TappedForMana.permanent
  mana <- Fields.required "mana" (Common.set ManaType.codec) TappedForMana.mana
  pure TappedForMana.MkTappedForMana {TappedForMana.permanent = permanent, TappedForMana.mana = mana}
