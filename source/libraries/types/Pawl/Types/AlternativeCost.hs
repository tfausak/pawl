module Pawl.Types.AlternativeCost where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

-- | CR 118.9: one alternative cost a face PRINTS, which its controller may pay
-- rather than the spell's mana cost -- and, when the card states one, the
-- condition rule 604.2 gates that permission on.
--
-- A Cost plus a Condition rather than a bare Cost, because the two printings part
-- exactly there: Fireblast's alternative is offered on every board, while
-- Asmoranomardicadaistinaculdacar's is offered only "as long as you've discarded
-- a card this turn". Nothing means UNCONDITIONED, and that is not the same claim
-- as a condition that happens to hold -- Pawl.Engine.Cost.costsFor offers an
-- unconditioned alternative without asking the board anything at all.
--
-- Not a field on Pawl.Types.Cost, which is also an activated ability's activation
-- cost (CR 602.1a): a gate on WHEN a cost may be paid is CR 118.9's business and
-- an activation restriction is CR 602.5's, carried by
-- Pawl.Types.ActivationRestriction. Nor is the condition an
-- ActivatedAbility.condition -- rule 118.9 is about SPELLS, so this rides a face.
--
-- MONOMORPHIC in the keyword where Cost is parametric: a Condition reaches a
-- Pawl.Types.Filter through a Count, and that Filter is already fixed at
-- Pawl.Types.Keyword, so there is no parameter left to thread.
--
-- The condition is a printed static ability's "as long as" clause (CR 604.2), so
-- CR 604.7 forbids answering it from an object's last known information; the
-- evaluation at Pawl.Engine.Cost.costsFor is of a card sitting in a zone, which
-- has one.
data AlternativeCost = MkAlternativeCost
  { condition :: Maybe Condition.Condition,
    cost :: Cost.Cost Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
