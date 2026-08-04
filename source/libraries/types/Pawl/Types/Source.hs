module Pawl.Types.Source where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

data Source
  = OfCard Printing.Printing
  | -- | CR 111.3/111.6: a token -- a permanent not represented by a card. Its
    -- characteristics ARE a Card (CR 111.3: effect-defined values are functionally
    -- equivalent to printed ones) with no Printing (a token isn't a card, CR
    -- 111.6). Game.cardOf returns this Card, so the whole projection/mana/combat/
    -- SBA pipeline reads a token with no special case. A future
    -- OfToken Card (Maybe Printing) carries a physical token's metadata when
    -- Printing grows any (spec section 8).
    OfToken Card.Card
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
    -- Card (like OfToken), so Game.cardOf reads it with no special case; unlike a
    -- token it is never a permanent (CR 114.5) and never on the battlefield.
    -- Owned and controlled by the player who created it (CR 114.2 / 109.4c).
    OfEmblem Card.Card
  | -- | CR 725.2: a triggered ability with no object source, controlled by a
    -- specific player baked in at trigger time (like DelayedTrigger's controller).
    -- The monarch's two inherent abilities are the only customers.
    OfInherentTrigger PlayerId.PlayerId (TriggeredAbility.TriggeredAbility Card.Card)
  deriving (Eq, Ord, Show)
