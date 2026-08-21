{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BlockCost where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.PerCreature as PerCreature
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BlockCost as BlockCost

-- | "subject" is Pawl.Codec.AttackCost's key and names the same axis: the
-- creatures the effect is ABOUT. "perBlocker" is one blocker's share, not the
-- card's whole cost -- a "for each" would repeat it per taxed blocker before CR
-- 509.1d totals the declaration.
--
-- No "scope" key, which is the one place this codec is not its twin's mirror:
-- Pawl.Types.BlockCost's header says why CR 509 gives the cost no second object
-- to be judged against.
codec :: Codec.Codec BlockCost.BlockCost
codec = Fields.object $ do
  subject <- Fields.required "subject" Affected.codec BlockCost.subject
  perBlocker <- Fields.required "perBlocker" PerCreature.codec BlockCost.perBlocker
  pure
    BlockCost.MkBlockCost
      { BlockCost.subject = subject,
        BlockCost.perBlocker = perBlocker
      }
