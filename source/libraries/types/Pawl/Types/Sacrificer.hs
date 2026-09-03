module Pawl.Types.Sacrificer where

-- | CR 701.21a: which player a sacrifice instruction is addressed to. That rule
-- lets only a permanent's controller sacrifice it, so the two arms are two
-- printed templates rather than two policies.
data Sacrificer
  = -- | "Sacrifice it", addressed to the resolving object's controller (CR
    -- 109.5). Rule 701.21a's second sentence is then a real gate: a permanent
    -- that changed hands since the instruction was written is not sacrificed at
    -- all, which is what Ray of Command stealing a Thatcher Revolt token before
    -- its delayed "sacrifice those tokens" turns on.
    EffectController
  | -- | "[That permanent]'s controller sacrifices it", addressed to whoever
    -- controls the named permanent when the instruction runs -- CR 701.54c's
    -- three-temptation tier is the pooled producer, and rule 701.21a's second
    -- sentence can never refuse this one.
    PermanentController
  deriving (Bounded, Enum, Eq, Ord, Show)
