module Pawl.Codec.ManaCost where

import qualified Pawl.Codec.ManaSymbol as ManaSymbol
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaCost as ManaCost

-- | NOT 'Common.scalar': a ManaCost's wire shape is exactly the array
-- 'Common.list' already produces for @[ManaSymbol.ManaSymbol]@, with no fixed
-- schema of its own to file under "ManaCost" -- filing one would give the same
-- array a second, redundant $defs entry. A plain 'Codec.MkCodec' around
-- 'Common.list' reuses that schema as-is.
codec :: Codec.Codec ManaCost.ManaCost
codec =
  Codec.MkCodec
    { Codec.encode = Codec.encode listCodec . ManaCost.unwrap,
      Codec.decode = fmap ManaCost.MkManaCost . Codec.decode listCodec,
      Codec.schema = Codec.schema listCodec
    }
  where
    listCodec = Common.list ManaSymbol.codec
