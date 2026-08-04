-- | CR 108.2: a card. Its printed characteristics live on its FACES; this type
-- is the container and the layout that says how they combine.
--
-- The knot is tied here rather than in Pawl.Types.Face so that a token-defining
-- effect keeps naming a whole card (CR 712.9), and so no `Modal Card` in the
-- codebase has to change.
module Pawl.Types.Card where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Layout as Layout

data Card = MkCard
  { -- | CR 709-722: how this card's faces combine. Normal for every card with
    -- exactly one face.
    layout :: Layout.Layout,
    -- | CR 709.2 / 715.2c: one CARD, however many faces it prints. NonEmpty
    -- rather than the Seq that Pawl.Types.Modal uses for its own non-empty
    -- invariant, because Pawl.Engine.Card.combined must be TOTAL -- a Maybe
    -- there would infect every characteristic read in the engine, where
    -- Modal's by-index lookup is honestly partial.
    --
    -- Printed order. Several layouts have positional rules -- CR 709.5's left
    -- and right halves, CR 710.1a's top and bottom, CR 712.8a's front face --
    -- so the ORDER lives here even though a face is REFERRED to by name.
    faces :: NonEmpty.NonEmpty (Face.Face Card)
  }
  deriving (Eq, Ord, Show)
