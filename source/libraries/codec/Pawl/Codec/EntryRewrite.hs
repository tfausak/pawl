module Pawl.Codec.EntryRewrite where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.AsCopy as AsCopy
import qualified Pawl.Codec.EntryFlip as EntryFlip
import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EntryRewrite as EntryRewrite

-- | @AsCopy@ takes a required object payload: the eligible filter has no default
-- (#1512), so the bare tag a plain Clone used to write is no longer a legal
-- rewrite. CR 707.9's exceptions stay optional INSIDE that object.
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Codec.DamageR gives: @RunEffects@ carries a card's effects and neither
-- module may name the other.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (EntryRewrite.EntryRewrite effect)
codec effectCodec =
  Arm.tagged
    [ Arm.payload "AsCopy" AsCopy.codec EntryRewrite.AsCopy (\x -> case x of EntryRewrite.AsCopy y -> Just y; _ -> Nothing),
      Arm.payload "ChoiceOf" (Common.list EntryOption.codec) EntryRewrite.ChoiceOf (\x -> case x of EntryRewrite.ChoiceOf y -> Just y; _ -> Nothing),
      Arm.payload "ChoiceByCoinFlip" EntryFlip.codec EntryRewrite.ChoiceByCoinFlip (\x -> case x of EntryRewrite.ChoiceByCoinFlip y -> Just y; _ -> Nothing),
      Arm.payload "WithCounters" WithCounters.codec EntryRewrite.WithCounters (\x -> case x of EntryRewrite.WithCounters y -> Just y; _ -> Nothing),
      Arm.nullary "ChooseColor" EntryRewrite.ChooseColor,
      Arm.nullary "ChooseBasicLandType" EntryRewrite.ChooseBasicLandType,
      Arm.nullary "ChoosePlayer" EntryRewrite.ChoosePlayer,
      Arm.payload "ChooseCardNames" (Filter.codec Keyword.codec) EntryRewrite.ChooseCardNames (\x -> case x of EntryRewrite.ChooseCardNames y -> Just y; _ -> Nothing),
      Arm.nullary "UnderSourceControl" EntryRewrite.UnderSourceControl,
      Arm.nullary "ReadAhead" EntryRewrite.ReadAhead,
      Arm.nullary "Riot" EntryRewrite.Riot,
      Arm.nullary "Unleash" EntryRewrite.Unleash,
      Arm.payload "Bloodthirst" Common.natural EntryRewrite.Bloodthirst (\x -> case x of EntryRewrite.Bloodthirst y -> Just y; _ -> Nothing),
      Arm.nullary "Tapped" EntryRewrite.Tapped,
      Arm.nullary "EntersTransformed" EntryRewrite.EntersTransformed,
      Arm.payload "PayLifeOrTapped" Common.natural EntryRewrite.PayLifeOrTapped (\x -> case x of EntryRewrite.PayLifeOrTapped y -> Just y; _ -> Nothing),
      Arm.payload "RevealOrTapped" (Filter.codec Keyword.codec) EntryRewrite.RevealOrTapped (\x -> case x of EntryRewrite.RevealOrTapped y -> Just y; _ -> Nothing),
      Arm.payload "SacrificeAnyNumber" SacrificeAnyNumber.codec EntryRewrite.SacrificeAnyNumber (\x -> case x of EntryRewrite.SacrificeAnyNumber y -> Just y; _ -> Nothing),
      Arm.payload "RunEffects" (Common.seq effectCodec) EntryRewrite.RunEffects (\x -> case x of EntryRewrite.RunEffects y -> Just y; _ -> Nothing)
    ]
