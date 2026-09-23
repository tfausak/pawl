module Pawl.Types.PermissionVerb where

-- | CR 601.3 / 305.1: what a CR 601.3 permission lets a player do with a card it
-- covers. Garruk's Horde's "you may CAST creature spells" is Cast; Serra
-- Paragon's "you may PLAY a land ... or CAST a permanent spell", which is the
-- glossary's "play" (a land played or a spell cast), is Play.
--
-- A field of the permission rather than a second arm beside
-- Pawl.Types.PlayerEffect's PlayLandsFrom, because a Play permission is ONE
-- permission: its once-each-turn budget (Pawl.Types.PermissionLimit) is spent
-- by a land play or a cast, whichever comes first, and two arms would keep two.
data PermissionVerb
  = -- | CR 601.3: the covered card may be cast, and never played as a land.
    Cast
  | -- | CR 601.3 / 305.1: the covered card may be cast, or played if it is a land.
    Play
  deriving (Bounded, Enum, Eq, Ord, Show)
