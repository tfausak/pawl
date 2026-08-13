{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Mode where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Clause as Clause
import qualified Pawl.Codec.TargetSlot as TargetSlot
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Mode as Mode

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (Mode.Mode card)
codec cardCodec = Fields.object $ do
  clauses <- Fields.defaulted "clauses" Seq.empty (Common.seq (Clause.codec cardCodec)) Mode.clauses
  targetSlots <- Fields.defaulted "targetSlots" Map.empty TargetSlot.codecMap Mode.targetSlots
  pure Mode.MkMode {Mode.clauses = clauses, Mode.targetSlots = targetSlots}
