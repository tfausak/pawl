module Pawl.Type.CastingPermission where

-- CR 113.6 / 601.3: a static permission to cast a card from a zone or under a
-- condition it normally could not. Classified by the permission pattern (the M3f
-- TriggerCondition shape). CastFromLibraryWhileSearching = Panglacial Wurm: "while
-- you're searching your library, you may cast this from your library." A general
-- "cast from the top of your library" (Garruk's Horde) is a future permission.
-- Only Pawl.Cast reads it, as a membership test per arm
-- (permitsCastWhileSearching, permitsCastFromGraveyard). Two producers:
-- Card.castingPermissions (printed) and Pawl.Keyword.castingPermissionsOf (the
-- ones rule 702 gives a card for a keyword it holds).
--
-- OBJECT-scoped throughout: every arm is a permission a CARD grants about
-- ITSELF. The player-scoped sibling -- a continuous effect that lets its player
-- cast any card from their graveyard (Yawgmoth's Will) -- is a different carrier
-- and still has none (#96).
data CastingPermission
  = CastFromLibraryWhileSearching
  | -- CR 702.34a's first static ability, the half that "functions while the card
    -- is in a player's graveyard": "You may cast this card from your graveyard
    -- ... by paying [cost] rather than paying its mana cost." Produced by
    -- Pawl.Keyword.castingPermissionsOf from the Flashback keyword, not printed
    -- in card JSON -- but a constructor here rather than a keyword read at the
    -- gate, so Pawl.Cast keeps asking "may this be cast from the graveyard?"
    -- and never "does this card have flashback?".
    CastFromGraveyard
  deriving (Eq, Ord, Show)
