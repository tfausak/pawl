module Pawl.Types.DamageR where

import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite

-- | The payload of Pawl.Types.ReplacementEffect's DamageR arm (#1305): which
-- damage events are intercepted, and how each is rewritten.
data DamageR = MkDamageR
  { matching :: DamagePattern.DamagePattern,
    rewrite :: DamageRewrite.DamageRewrite
  }
  deriving (Eq, Ord, Show)
