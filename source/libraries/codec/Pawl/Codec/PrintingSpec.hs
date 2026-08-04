{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PrintingSpec where

import qualified Pawl.Codec.CardSpec as CardSpec
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Printing as Printing
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Printing as Printing

-- | MkPrinting just delegates to Card's own codec, so 'CardSpec.mountainCard' is
-- reused rather than a second synthetic Card being built here. The
-- registry-backed round-trip over every real Printing stays in
-- Pawl.CodecIntegrationSpec, which this sublibrary sits above.
spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.Printing" . Spec.it s "MkPrinting delegates to Card's own codec" $
    Common.assertJsonCodec
      s
      Printing.toJson
      Printing.fromJson
      (Printing.MkPrinting CardSpec.mountainCard)
      """ {"faces":[{"name":"Mountain","typeLine":{"supertypes":[{"type":"Basic"}],"types":[{"type":"Land"}],"subtypes":[{"type":"Mountain"}]}}]} """
