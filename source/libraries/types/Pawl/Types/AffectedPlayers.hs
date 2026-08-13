module Pawl.Types.AffectedPlayers where

import qualified Pawl.Types.PlayerScope as PlayerScope

-- | WHICH players a Pawl.Types.PlayerEffect installed by a resolution applies to
-- (CR 611.1 / 613.11) -- the payload of Effect.AffectPlayers and of the
-- Pawl.Types.ActivePlayerEffect it stores.
--
-- Two readings, which is exactly Pawl.Types.GraveyardScope's split one axis over:
-- a card either names a CLASS of players relative to the effect's controller
-- ("your opponents"), or names the seat it TARGETED ("target player"). The
-- printed carrier (Pawl.Types.PlayerStaticAbility) keeps the bare PlayerScope,
-- because a static ability has no slots for the second reading to be resolved
-- against; the stored carrier has them, since a resolution binds slots.
--
-- PARAMETRIC over what the second arm names, and instantiated twice rather than
-- carrying both readings as separate arms: a CARD writes
-- @AffectedPlayers SlotName@, and the store holds @AffectedPlayers PlayerId@,
-- with Pawl.Engine.Resolve baking the one into the other as the effect begins.
-- So neither instantiation has an arm it cannot answer, and no lint is owed to
-- keep a card from writing a PlayerId -- the shape Pawl.Types.PlayerRef needs a
-- Pawl.CardSpec sweep for, since its Specific arm rides in a type the codec
-- round-trips whole.
--
-- BAKED rather than dynamic, and CR 611.2c is not the reason: that rule's freeze
-- is about the set of OBJECTS a continuous effect modifies, and its carve-out
-- keeps a rules-modifying effect's objects dynamic (which is why the Scoped arm
-- is still resolved on every read -- see Pawl.Types.PlayerScope). The seat is
-- fixed by CR 601.2c instead, which chose it as the spell was cast, and CR 608.2b
-- says what happens when that choice has gone bad: "if the spell or ability
-- creates any continuous effects that affect game rules (see rule 613.11), those
-- effects don't apply to illegal targets", so an illegal or unfilled slot stores
-- nothing at all.
data AffectedPlayers player
  = -- | CR 109.5, resolved against the effect's controller on every read --
    -- Silence's "your opponents".
    Scoped PlayerScope.PlayerScope
  | -- | The players named from outside the effect: a target slot on the card
    -- (Cease-Fire's "target player"), and the seat that slot was answered with
    -- once Resolve has baked it.
    Named player
  deriving (Eq, Ord, Show)
