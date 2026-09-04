{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamageRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Recipient as Recipient.Type

-- | The wire format is unchanged by the conversion to a bundle.
codec :: Codec.Codec DamageRewrite.DamageRewrite
codec =
  Arm.tagged
    [ Arm.nullary "PreventAll" DamageRewrite.PreventAll,
      Arm.nullary "PreventRemovingShieldCounter" DamageRewrite.PreventRemovingShieldCounter,
      Arm.payload "PreventNext" Common.natural DamageRewrite.PreventNext (\x -> case x of DamageRewrite.PreventNext y -> Just y; _ -> Nothing),
      Arm.payload "PreventAllBut" Common.natural DamageRewrite.PreventAllBut (\x -> case x of DamageRewrite.PreventAllBut y -> Just y; _ -> Nothing),
      Arm.payload "SetAmount" Common.natural DamageRewrite.SetAmount (\x -> case x of DamageRewrite.SetAmount y -> Just y; _ -> Nothing),
      Arm.payload "Scale" Scaling.codec DamageRewrite.Scale (\x -> case x of DamageRewrite.Scale y -> Just y; _ -> Nothing),
      Arm.payload "Redirect" Recipient.codec DamageRewrite.Redirect (\x -> case x of DamageRewrite.Redirect y -> Just y; _ -> Nothing),
      Arm.payload "RedirectNext" redirectNext (uncurry DamageRewrite.RedirectNext) (\x -> case x of DamageRewrite.RedirectNext n y -> Just (n, y); _ -> Nothing),
      Arm.payload "RedirectMatching" (Filter.codec Keyword.codec) DamageRewrite.RedirectMatching (\x -> case x of DamageRewrite.RedirectMatching y -> Just y; _ -> Nothing)
    ]

-- | The counted redirection's two halves keyed by name rather than positional,
-- the convention every payload follows (#1464): the remaining amount and the
-- destination it moves damage to.
redirectNext :: Codec.Codec (Natural.Natural, Recipient.Type.Recipient)
redirectNext = Fields.object $ do
  remaining <- Fields.required "remaining" Common.natural fst
  to <- Fields.required "to" Recipient.codec snd
  pure (remaining, to)
