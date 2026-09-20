module Pawl.Types.GrantPlayFromExile where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Effect's GrantPlayFromExile arm: which objects the
-- permission covers, whose it is, how long it lasts, and how its holder may pay
-- for what they cast under it.
--
-- SPUN OUT of Pawl.Types.DurationRef, which that type's own haddock asks for as
-- soon as one sharer needs a field the others do not (#1305): GainControl has
-- nothing to say about mana, and a Maybe bolted onto the shared record would
-- have made the field's absence into the tag telling the arms apart.
-- Pawl.Types.PreventAllDamage was spun out for the same reason afterwards.
--
-- `spending` is CR 118.14's "and mana of any type can be spent to cast that
-- spell", printed on Dire Fleet Daredevil beside the permission itself. It rides
-- the grant rather than the card being exiled, which is rule 118.14's own
-- scoping: the permission is the granting effect's, so the same card cast under
-- some other permission pays its printed colours.
--
-- `withoutPayingManaCost` is CR 118.9's "you may cast [this object] without
-- paying its mana cost", printed in the same sentence as the permission on
-- Extract Power. It rides the grant for `spending`'s reason: the waiver is the
-- granting effect's, so the same card played under some other permission pays.
-- Pawl.Types.ExilePlayPermission's field of the same name is where it lands.
--
-- `player` is WHO may play, CR 601.3's "that player" -- Elkin Lair's "THE
-- PLAYER may play that card this turn", the upkeep player the trigger bound,
-- not the Lair's controller. Pawl.Types.OfferCast's `caster` is the same field
-- one opcode over, and takes the same default: CR 109.5's "you", the resolving
-- controller. Suspend Aggression's "its owner" is the third spelling, and the
-- one that needs no "you" at all -- CR 108.4a answers the controller of a card
-- already in exile with its owner, so PlayerRef.ControllerOfBound says it.
--
-- ONE seat, since Pawl.Types.ExilePlayPermission holds one (CR 715.3d: "that
-- card ... that player"). So Pawl.Engine.Resolve.Effect's arm grants nothing for
-- a reference naming nobody, and nothing for one naming several --
-- PlayerRef.InSlot's own collapse.
data GrantPlayFromExile = MkGrantPlayFromExile
  { duration :: Duration.Duration,
    player :: PlayerRef.PlayerRef,
    ref :: ObjectRef.ObjectRef,
    spending :: ManaSpending.ManaSpending,
    withoutPayingManaCost :: Bool
  }
  deriving (Eq, Ord, Show)
