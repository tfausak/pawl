module Pawl.Types.Create where

import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 111.1's token creation: WHO creates them, how many, of what card,
-- entering how, and under what name the rest of the instruction list may read
-- them.
--
-- Parametric in @card@ for 'Pawl.Types.Effect.Effect'\'s reason: the token's
-- card is card DATA nested inside card data, and the parameter is what keeps
-- 'Pawl.Types.Effect' from naming a concrete card type.
data Create card = MkCreate
  { quantity :: Quantity.Quantity,
    card :: card,
    -- | CR 110.5b's default is no riders at all, which is most tokens, so the
    -- key is elided rather than written.
    riders :: EntryRiders.EntryRiders Quantity.Quantity,
    -- | The slot the created tokens are bound to, when a later effect in the
    -- same list reads them. Absent when nothing does.
    slot :: Maybe SlotName.SlotName,
    -- | CR 111.2: "the player who creates a token is its owner. The token enters
    -- the battlefield under that player's control." CR 109.5's "you" is the
    -- default and covers almost every printing, so the key is elided; a card
    -- says otherwise when the creator is somebody the sentence identifies --
    -- Rampage of the Clans' "for each permanent destroyed this way, ITS
    -- CONTROLLER creates a 3/3 green Centaur creature token", where the
    -- reference is a PlayerRef.ControllerOfBound over the loop's own member.
    --
    -- A PlayerRef and not a PlayerId: only a resolution knows a seat. A
    -- reference naming SEVERAL players creates the tokens for each of them, in
    -- APNAP order (CR 608.2f); one naming nobody creates none (CR 101.3).
    creator :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
