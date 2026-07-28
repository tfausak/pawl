module Pawl.Type.Supertype where

-- CR 205.4a lists five supertypes. The three missing ones are Snow (#305),
-- World (#306), and Ongoing, which is scheme-only (CR 205.4h) and folded into
-- #131 with the rest of Archenemy.
data Supertype
  = -- CR 205.4c: any land with this supertype is a basic land, and any land
    -- without it is nonbasic even if it has a basic land type.
    Basic
  | -- CR 205.4d: subject to CR 704.5j's legend rule, which Pawl.Sba implements.
    -- CR 205.4e's OTHER rule -- a legendary instant or sorcery can't be cast
    -- unless you control a legendary creature or planeswalker -- is not
    -- checked (#307).
    Legendary
  deriving (Eq, Ord, Show)
