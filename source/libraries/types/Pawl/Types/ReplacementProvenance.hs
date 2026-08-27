module Pawl.Types.ReplacementProvenance where

-- | Where a permanent's replacement effect came from: its card's own text, or a
-- rule that mints one onto the permanent.
--
-- PROVENANCE, not shape, for Pawl.Types.ReplacementOrigin's reason and with a
-- sharper edge: CR 702.16e's protection prevention is a plain PreventAll naming
-- the permanent itself, the identical value Phyrexian Vindicator and Stormwild
-- Capridor print, so no reading of the EFFECT can tell the two apart. The mark is
-- therefore made where the row is built (Pawl.Engine.Projection.replacementsOf)
-- and carried on Pawl.Types.PermanentCandidate.
--
-- Read only by Pawl.Engine.Replacement.printedBy, which is CR 615.13's "prevented
-- THIS WAY": a trigger phrased that way is about the prevention its own card's
-- text set up, and a rule minting one onto the permanent is not that text --
-- whether the rule read a counter (CR 122.1c) or a keyword the card does print
-- (CR 702.16e), since "this way" points at the preceding sentence rather than at
-- the object.
data ReplacementProvenance
  = -- | The card's own rules text -- printed on a face, copied onto it (CR
    -- 707.2), or granted to it by another object's ability.
    Printed
  | -- | Minted onto the permanent by a rule: CR 122.1c's shield pair, CR 122.1d's
    -- and CR 122.1h's counter rows, CR 306.5b's and CR 310.4b's intrinsic entry
    -- replacements, CR 714.3a's lore counter, and every row rule 702 gives a
    -- keyword (Pawl.Engine.Keyword.mintedReplacementsOf).
    Minted
  deriving (Bounded, Enum, Eq, Ord, Show)
