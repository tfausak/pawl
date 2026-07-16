module Pawl.Quantity where

import Pawl.Type.GameState (GameState)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Quantity (Quantity)
import qualified Pawl.Type.Quantity as Quantity

-- Nothing when the value cannot be determined.
--
-- Only Literal exists in M1a, so this is always Just today. The Maybe is here
-- from the start because Star resolves in layer 7a, and the layer system does
-- not exist until M3 — a Star can be shaped now but not evaluated now. The
-- GameState and ObjectId are what Star will read; they are unused until then,
-- and callers already pass them, so adding Star touches no call site.
evaluate :: GameState -> ObjectId -> Quantity -> Maybe Integer
evaluate _ _ quantity = case quantity of
  Quantity.Literal n -> Just n
