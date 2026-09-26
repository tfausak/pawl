module Pawl.Codec.RevealCause where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RevealCause as RevealCause

codec :: Codec.Codec RevealCause.RevealCause
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Ordinary" RevealCause.Ordinary,
      Arm.payload "ForMiracle" (Cost.codec Keyword.codec) RevealCause.ForMiracle (\x -> case x of RevealCause.ForMiracle y -> Just y; _ -> Nothing)
    ]

tagOf :: RevealCause.RevealCause -> String
tagOf x = case x of
  RevealCause.Ordinary {} -> "Ordinary"
  RevealCause.ForMiracle {} -> "ForMiracle"
