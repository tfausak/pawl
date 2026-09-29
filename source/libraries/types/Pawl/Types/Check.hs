module Pawl.Types.Check where

import qualified Pawl.Types.CountIs as CountIs
import qualified Pawl.Types.DamageIs as DamageIs
import qualified Pawl.Types.LifeIs as LifeIs
import qualified Pawl.Types.TappedIs as TappedIs

-- | One fact a scenario asserts about the game, in the nouns a client would
-- display.
data Check
  = Life LifeIs.LifeIs
  | Count CountIs.CountIs
  | Damage DamageIs.DamageIs
  | Tapped TappedIs.TappedIs
  deriving (Eq, Ord, Show)
