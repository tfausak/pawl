module Pawl.Types.VillainousChoiceRewrite where

-- | CR 701.55c / 614.1a: how a replacement rewrites one instruction to face a
-- villainous choice.
--
-- A type rather than a payload-free Pawl.Types.ReplacementEffect arm, for
-- Pawl.Types.ProliferateRewrite's reason.
data VillainousChoiceRewrite
  = -- | CR 701.55c: The Valeyard's "they face that choice an additional time".
    AdditionalTime
  deriving (Bounded, Enum, Eq, Ord, Show)
