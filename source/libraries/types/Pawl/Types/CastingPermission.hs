module Pawl.Types.CastingPermission where

-- | CR 113.6 / 601.3: a static permission to cast a card from a zone or under a
-- condition it normally could not. CastFromLibraryWhileSearching is Panglacial
-- Wurm; a general "cast from the top of your library" (Garruk's Horde) is a
-- future permission. Only Pawl.Engine.Cast reads it, as a membership test per
-- arm. Two producers: Card.castingPermissions (printed) and
-- Pawl.Engine.Keyword.castingPermissionsOf (what rule 702 gives for a keyword).
--
-- OBJECT-scoped throughout: every arm is a permission a CARD grants about
-- ITSELF. The player-scoped sibling -- a continuous effect that lets its player
-- cast any card from their graveyard (Yawgmoth's Will) -- is a different carrier
-- and still has none (#96).
data CastingPermission
  = CastFromLibraryWhileSearching
  | -- | CR 702.34a's first static ability, the half functioning while the card is
    -- in a graveyard. Produced by Pawl.Engine.Keyword.castingPermissionsOf from
    -- the Flashback keyword, not printed in card JSON -- but a constructor here
    -- rather than a keyword read at the gate, so Pawl.Engine.Cast keeps asking
    -- "may this be cast from the graveyard?" and never "does this have
    -- flashback?".
    CastFromGraveyard
  deriving (Eq, Ord, Show)
