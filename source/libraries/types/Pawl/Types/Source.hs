module Pawl.Types.Source where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | What is behind an object. The three card-shaped constructors name their
-- printing by id rather than carrying it (#1592), so the rules distinction
-- between them is the only distinction left: a card, a token and an emblem are
-- three different things under CR 108, CR 111 and CR 114, and the engine cases
-- on which of the three it has.
data Source
  = -- | CR 108: a card, named by its entry in GameState.printings.
    OfCard PrintingId.PrintingId
  | -- | CR 111.3/111.6: a token -- a permanent not represented by a card. Its
    -- characteristics ARE a Card (CR 111.3: effect-defined values are functionally
    -- equivalent to printed ones), interned like any other printing, and carrying
    -- no print-level data because a token is not a card (CR 111.6).
    OfToken PrintingId.PrintingId
  | -- | CR 602: an activated ability on the stack -- the source permanent's id plus
    -- the ability. The ability travels with the object so it resolves even if the
    -- source leaves (CR 113.7a).
    --
    -- The id alone is enough once the source is gone: CR 400.7 mints a fresh id
    -- on every zone change, so this one then names nothing, which is exactly the
    -- trigger for CR 608.2h's last known information, filed under the same id in
    -- GameState.lastKnown. Nothing about the source is copied in here; a snapshot
    -- would have to be kept in step with a source that can still change while the
    -- ability waits on the stack.
    OfAbility ObjectId.ObjectId (ActivatedAbility.ActivatedAbility Card.Card)
  | -- | CR 603.3: a triggered ability on the stack -- the source permanent's id
    -- plus the ability. Travels with the object so it resolves even if the source
    -- leaves (CR 603.3d).
    OfTrigger ObjectId.ObjectId (TriggeredAbility.TriggeredAbility Card.Card)
  | -- | CR 114: an emblem -- an object in the command zone whose only
    -- characteristics are its abilities (CR 114.3). Its characteristics ARE a
    -- Card and are interned like a token's; unlike a token it is never a permanent
    -- (CR 114.5) and never on the battlefield. Owned and controlled by the player
    -- who created it (CR 114.2 / 109.4c).
    OfEmblem PrintingId.PrintingId
  | -- | CR 725.2 / CR 702.179d: a triggered ability with no object source,
    -- controlled by a specific player baked in at trigger time (like
    -- DelayedTrigger's controller). Its customers are the abilities the rulebook
    -- states without a card to bear them -- the monarch's pair, and the speed
    -- increase a player with 1 or more speed has.
    OfInherentTrigger PlayerId.PlayerId (TriggeredAbility.TriggeredAbility Card.Card)
  deriving (Eq, Ord, Show)
