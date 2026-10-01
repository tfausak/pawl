module Pawl.Codec.TokenPlus where

import qualified Data.Typeable as Typeable
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TokenPlus as TokenPlus

-- | The card codec is a PARAMETER for Pawl.Codec.TokenR's reason.
codec :: (Typeable.Typeable card) => Codec.Codec card -> Codec.Codec (TokenPlus.TokenPlus card)
codec cardCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "One" cardCodec TokenPlus.One (\x -> case x of TokenPlus.One y -> Just y; _ -> Nothing),
      Arm.payload "ThatMany" cardCodec TokenPlus.ThatMany (\x -> case x of TokenPlus.ThatMany y -> Just y; _ -> Nothing)
    ]

tagOf :: TokenPlus.TokenPlus card -> String
tagOf x = case x of
  TokenPlus.One {} -> "One"
  TokenPlus.ThatMany {} -> "ThatMany"
