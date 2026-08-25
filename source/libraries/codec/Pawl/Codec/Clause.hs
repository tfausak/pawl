{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Clause where

import qualified Data.Sequence as Seq
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ClauseIndex as ClauseIndex
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.Codec.PayGate as PayGate
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Optionality as Optionality

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema. Every rider is the marked case and so is elided when absent: CR
-- 608.2c's "If you do", CR 701.46a's "if", CR 608.2d's "or", CR 603.5's "may",
-- and CR 118.12's resolution cost.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (Clause.Clause card)
codec cardCodec = Fields.object $ do
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) Clause.condition
  effects <- Fields.defaulted "effects" Seq.empty (Common.seq (Effect.codec cardCodec)) Clause.effects
  ifTaken <- Fields.defaulted "ifTaken" Nothing (Common.maybe ClauseIndex.codec) Clause.ifTaken
  optionality <- Fields.defaulted "optionality" Optionality.Mandatory Optionality.codec Clause.optionality
  orElse <- Fields.defaulted "orElse" Nothing (Common.maybe ClauseIndex.codec) Clause.orElse
  payGate <- Fields.defaulted "payGate" Nothing (Common.maybe PayGate.codec) Clause.payGate
  pure
    Clause.MkClause
      { Clause.ifTaken = ifTaken,
        Clause.condition = condition,
        Clause.orElse = orElse,
        Clause.optionality = optionality,
        Clause.payGate = payGate,
        Clause.effects = effects
      }
