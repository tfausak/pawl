module Pawl.Types.TokenPlus where

-- | What Pawl.Types.TokenR's `plus` appends to a token creation (CR 614.1a): a
-- lot of this card in the SAME creation event, sized as printed.
--
-- Parametric in @card@ for Pawl.Types.TokenR's reason.
data TokenPlus card
  = -- | "those tokens plus a 1/1 white Soldier creature token" (Queen Allenal of
    -- Ruadach): one more token.
    One card
  | -- | "those tokens plus that many 1/1 green Squirrel creature tokens"
    -- (Chatterfang, Squirrel General): one more per token the event creates.
    ThatMany card
  deriving (Eq, Ord, Show)

instance Functor TokenPlus where
  fmap f plus = case plus of
    One card -> One (f card)
    ThatMany card -> ThatMany (f card)

instance Foldable TokenPlus where
  foldr f z plus = case plus of
    One card -> f card z
    ThatMany card -> f card z
