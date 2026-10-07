{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaUnit where

import qualified Pawl.Codec.ManaRestriction as ManaRestriction
import qualified Pawl.Codec.ManaRetention as ManaRetention
import qualified Pawl.Codec.ManaRider as ManaRider
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.ProductionTag as ProductionTag
import qualified Pawl.Codec.SourceChoices as SourceChoices
import qualified Pawl.Codec.SpendTrigger as SpendTrigger
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaUnit as ManaUnit

-- | All eight axes, CR 106.6's three included: a restriction is a predicate over
-- the object being paid for and a rider is an effect on it, so both ride the
-- unit rather than being recoverable from the source that made it.
--
-- Every key REQUIRED, unlike Pawl.Codec.ManaAddition's. This is game state
-- rather than card data, written and read back by the replay transcript, so a
-- defaulted key would let a dropped field round trip silently. The two
-- exceptions are `sourceLastExiled` and `spendTrigger`, Nothing on all but Ice
-- Cauldron's and Pyromancer's Goggles' mana and so written only when set, which
-- keeps the pools every scenario renders unchanged; their round trip is still
-- exact.
codec :: Codec.Codec ManaUnit.ManaUnit
codec = Fields.object $ do
  manaType <- Fields.required "manaType" ManaType.codec ManaUnit.manaType
  tags <- Fields.required "tags" (Common.set ProductionTag.codec) ManaUnit.tags
  retention <- Fields.required "retention" ManaRetention.codec ManaUnit.retention
  restriction <- Fields.required "restriction" (Common.maybe ManaRestriction.codec) ManaUnit.restriction
  rider <- Fields.required "rider" (Common.maybe ManaRider.codec) ManaUnit.rider
  sourceChoices <- Fields.required "sourceChoices" (Common.maybe SourceChoices.codec) ManaUnit.sourceChoices
  spendTrigger <- Fields.defaulted "spendTrigger" Nothing (Common.maybe SpendTrigger.codec) ManaUnit.spendTrigger
  sourceLastExiled <- Fields.defaulted "sourceLastExiled" Nothing (Common.maybe ObjectId.codec) ManaUnit.sourceLastExiled
  pure
    ManaUnit.MkManaUnit
      { ManaUnit.manaType = manaType,
        ManaUnit.tags = tags,
        ManaUnit.retention = retention,
        ManaUnit.restriction = restriction,
        ManaUnit.rider = rider,
        ManaUnit.spendTrigger = spendTrigger,
        ManaUnit.sourceChoices = sourceChoices,
        ManaUnit.sourceLastExiled = sourceLastExiled
      }
