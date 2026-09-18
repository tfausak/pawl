module Pawl.Codec.DrawRewrite where

import qualified Pawl.Codec.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DrawRewrite as DrawRewrite

codec :: Codec.Codec DrawRewrite.DrawRewrite
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "GainLife" Common.natural DrawRewrite.GainLife (\x -> case x of DrawRewrite.GainLife y -> Just y; _ -> Nothing),
      Arm.payload "FromOutsideTheGame" FromOutsideTheGame.codec DrawRewrite.FromOutsideTheGame (\x -> case x of DrawRewrite.FromOutsideTheGame y -> Just y; _ -> Nothing),
      Arm.payload "Dredge" Common.natural DrawRewrite.Dredge (\x -> case x of DrawRewrite.Dredge y -> Just y; _ -> Nothing)
    ]

tagOf :: DrawRewrite.DrawRewrite -> String
tagOf x = case x of
  DrawRewrite.GainLife {} -> "GainLife"
  DrawRewrite.FromOutsideTheGame {} -> "FromOutsideTheGame"
  DrawRewrite.Dredge {} -> "Dredge"
