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
    [ -- CR 707.9's exceptions are ELIDED when empty, so the matcher answers
      -- @Just Nothing@ for a plain Clone and @Just (Just xs)@ otherwise -- which
      -- is what keeps the bare tag this arm has always written.
      Arm.optionalPayload
        "AsCopy"
        (Common.list CopyException.codec)
        (EntryRewrite.AsCopy . Maybe.fromMaybe [])
        ( \x -> case x of
            EntryRewrite.AsCopy [] -> Just Nothing
            EntryRewrite.AsCopy exceptions -> Just (Just exceptions)
            _ -> Nothing
        ),
      Arm.payload "ChoiceOf" (Common.list EntryOption.codec) EntryRewrite.ChoiceOf (\x -> case x of EntryRewrite.ChoiceOf y -> Just y; _ -> Nothing),
      Arm.payload "WithCounters" WithCounters.codec EntryRewrite.WithCounters (\x -> case x of EntryRewrite.WithCounters y -> Just y; _ -> Nothing),
      Arm.nullary "ChooseColor" EntryRewrite.ChooseColor,
      Arm.nullary "ChooseBasicLandType" EntryRewrite.ChooseBasicLandType,
      Arm.payload "ChooseCardNames" (Filter.codec Keyword.codec) EntryRewrite.ChooseCardNames (\x -> case x of EntryRewrite.ChooseCardNames y -> Just y; _ -> Nothing),
      Arm.nullary "UnderSourceControl" EntryRewrite.UnderSourceControl,
      Arm.nullary "Riot" EntryRewrite.Riot,
      Arm.nullary "Unleash" EntryRewrite.Unleash,
      Arm.payload "Bloodthirst" Common.natural EntryRewrite.Bloodthirst (\x -> case x of EntryRewrite.Bloodthirst y -> Just y; _ -> Nothing),
      Arm.nullary "Tapped" EntryRewrite.Tapped,
      Arm.nullary "EntersTransformed" EntryRewrite.EntersTransformed,
      Arm.payload "PayLifeOrTapped" Common.natural EntryRewrite.PayLifeOrTapped (\x -> case x of EntryRewrite.PayLifeOrTapped y -> Just y; _ -> Nothing),
      Arm.payload "RevealOrTapped" (Filter.codec Keyword.codec) EntryRewrite.RevealOrTapped (\x -> case x of EntryRewrite.RevealOrTapped y -> Just y; _ -> Nothing),
      Arm.payload "SacrificeAnyNumber" SacrificeAnyNumber.codec EntryRewrite.SacrificeAnyNumber (\x -> case x of EntryRewrite.SacrificeAnyNumber y -> Just y; _ -> Nothing)
    ]
