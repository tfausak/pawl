module Pawl.Types.LifeLoss where

import qualified Pawl.Types.LifeLossCause as LifeLossCause
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's LoseLife arm: "these players, this many",
-- plus CR 119.3's classification of where the loss came from.
--
-- Pawl.Types.PlayerQuantity's two fields with a third, SPUN OUT rather than
-- bolted onto that record, which is the rule that record's own haddock states: a
-- life loss needs a field its fellow sharers do not. Pawl.Types.Mill left the
-- same way.
--
-- The cause is a CLASSIFICATION rather than an effect's identity, which is why
-- the rules core may read it: Pawl.Engine.Replacement.applies narrows a row by
-- exactly this grain. No printing in data\/cards\/ states it -- they all lose
-- life as ordinary effects and the codec elides the default -- and the one caller
-- that sets it is Pawl.Engine.Rad, which builds rule 728.1's ability out of
-- ordinary opcodes and must still say that its loss is CR 728.1a's.
data LifeLoss = MkLifeLoss
  { player :: PlayerRef.PlayerRef,
    quantity :: Quantity.Quantity,
    -- | CR 119.3 for anything a card prints; CR 728.1a for rule 728.1's ability.
    cause :: LifeLossCause.LifeLossCause
  }
  deriving (Eq, Ord, Show)
