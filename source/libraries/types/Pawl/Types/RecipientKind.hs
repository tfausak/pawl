module Pawl.Types.RecipientKind where

-- | WHICH KIND of thing a Pawl.Types.Recipient names -- that type's arm set with
-- the id and the player taken out, so that a filter can ask what a candidate
-- targets without holding the board. Closed-half vocabulary, like
-- Pawl.Types.Pool, which is where each tag is minted.
--
-- A CLASSIFICATION rather than an identity: the atom that reads it
-- (Filter.TargetsOnlyOne) asks what sort of thing the word "target" named, never
-- which object it was.
data RecipientKind
  = -- | CR 115.1a: a creature on the battlefield (Recipient.ToCreature).
    Creature
  | -- | CR 115.4 / 306.6: a planeswalker (Recipient.ToPlaneswalker).
    Planeswalker
  | -- | CR 115.4 / 310.1: a battle (Recipient.ToBattle).
    Battle
  | -- | CR 115.1: a player (Recipient.ToPlayer).
    Player
  | -- | CR 110.1 / 112.1 / 113.9 / 404.1: an object named generically -- a
    -- permanent, a spell, an ability, or a card in a graveyard or in exile
    -- (Recipient.ToObject).
    --
    -- Not implemented: telling those apart, which needs the board rather than the
    -- tag -- Radiate's "targets only a single permanent or player" cannot be
    -- written with this arm (#3140).
    Object
  | -- | CR 406.4: a pile of face-down exiled cards (Recipient.ToPile). Never
    -- survives CR 601.2c, so no filter can meet one; the arm is here because the
    -- classification is total.
    Pile
  deriving (Bounded, Enum, Eq, Ord, Show)
