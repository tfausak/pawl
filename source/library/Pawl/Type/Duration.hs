module Pawl.Type.Duration where

-- How long a stored continuous effect lasts (CR 611.2). Only UntilEndOfTurn
-- (CR 514.2 drops it during cleanup) exists at M3b; static-ability effects carry
-- no Duration -- they last while their source and ability do, which is "while
-- re-derived from the battlefield". Grows WhileSourceOnBattlefield,
-- UntilYourNextTurn, etc. as cards need them.
data Duration
  = UntilEndOfTurn -- CR 514.2 (M3b)
  | Indefinite -- CR 611: "lasts indefinitely" (Magical Hack); cleanup never drops it
  deriving (Eq, Ord, Show)
