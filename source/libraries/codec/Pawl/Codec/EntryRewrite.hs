module Pawl.Codec.EntryRewrite where

import qualified Data.Maybe as Maybe
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EntryRewrite as EntryRewrite

-- | The wire format is unchanged by the conversion to a bundle.
--
-- @AsCopy@ takes 'Arm.optionalPayload': CR 707.9's exceptions are omitted when
-- there are none, so a plain Clone's rewrite stays the nullary tag it has always
-- been, and one constructor accepts both shapes. That is the case
-- 'Arm.optionalPayload' exists for.
codec :: Codec.Codec EntryRewrite.EntryRewrite
codec =
  Arm.tagged
    encode
    [ Arm.optionalPayload "AsCopy" (Common.list CopyException.codec) (EntryRewrite.AsCopy . Maybe.fromMaybe []),
      Arm.payload "ChoiceOf" (Common.list EntryOption.codec) EntryRewrite.ChoiceOf,
      Arm.payload "WithCounters" WithCounters.codec EntryRewrite.WithCounters,
      Arm.nullary "ChooseColor" EntryRewrite.ChooseColor,
      Arm.nullary "ChooseBasicLandType" EntryRewrite.ChooseBasicLandType,
      Arm.payload "ChooseCardNames" (Filter.codec Keyword.codec) EntryRewrite.ChooseCardNames,
      Arm.nullary "UnderSourceControl" EntryRewrite.UnderSourceControl,
      Arm.nullary "Riot" EntryRewrite.Riot,
      Arm.nullary "Unleash" EntryRewrite.Unleash,
      Arm.nullary "Tapped" EntryRewrite.Tapped,
      Arm.payload "PayLifeOrTapped" Common.natural EntryRewrite.PayLifeOrTapped,
      Arm.payload "SacrificeAnyNumber" SacrificeAnyNumber.codec EntryRewrite.SacrificeAnyNumber
    ]
  where
    encode r = case r of
      EntryRewrite.AsCopy [] -> Common.nullary "AsCopy"
      EntryRewrite.AsCopy exceptions -> Common.tagged "AsCopy" . Just $ Common.encodeList (Codec.encode CopyException.codec) exceptions
      EntryRewrite.ChoiceOf options -> Common.tagged "ChoiceOf" . Just $ Common.encodeList (Codec.encode EntryOption.codec) options
      EntryRewrite.WithCounters x -> Common.tagged "WithCounters" . Just $ Codec.encode WithCounters.codec x
      EntryRewrite.ChooseColor -> Common.nullary "ChooseColor"
      EntryRewrite.ChooseBasicLandType -> Common.nullary "ChooseBasicLandType"
      EntryRewrite.ChooseCardNames f -> Common.tagged "ChooseCardNames" . Just $ Codec.encode (Filter.codec Keyword.codec) f
      EntryRewrite.UnderSourceControl -> Common.nullary "UnderSourceControl"
      EntryRewrite.Riot -> Common.nullary "Riot"
      EntryRewrite.Unleash -> Common.nullary "Unleash"
      EntryRewrite.Tapped -> Common.nullary "Tapped"
      EntryRewrite.PayLifeOrTapped n -> Common.tagged "PayLifeOrTapped" . Just $ Common.encodeNatural n
      EntryRewrite.SacrificeAnyNumber x -> Common.tagged "SacrificeAnyNumber" . Just $ Codec.encode SacrificeAnyNumber.codec x
