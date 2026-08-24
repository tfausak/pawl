module Pawl.Types.PaymentSubject where

import qualified Pawl.Types.ObjectId as ObjectId

-- | WHAT a cost is being paid for, at the one grain CR 106.6's restrictions ask
-- about: a spell being cast, an ability being activated, or neither.
--
-- CR 601.2h's payment is a cast's and CR 602.2b's is an activation's, and a
-- restricted mana may admit one, the other or both
-- (Pawl.Types.ManaRestriction). A special action's cost (CR 116.2), CR 508.1j \/
-- 509.1f's combat toll and CR 118.12's resolution-time payment are neither, so
-- they get an arm of their own rather than an absent one.
--
-- Carried into Pawl.Engine.Cost.pay from the caller rather than derived there,
-- on Pawl.Types.PaymentMoment's precedent and for its reason: the cost itself
-- does not say which door it came in by, and a parameter is what makes a new
-- caller state its subject instead of inheriting a default.
--
-- Deliberately no codec. A payment in flight is never serialised -- this is a
-- runtime argument, as PaymentMoment is, and not a field of any type
-- Pawl.Codec covers.
data PaymentSubject
  = -- | Neither a cast nor an activation: a special action's cost, a combat
    -- toll, CR 118.12's payment as a spell or ability resolves.
    ForNeither
  | -- | CR 601.2h: the object being CAST.
    Casting ObjectId.ObjectId
  | -- | CR 602.2b: the SOURCE of the ability being activated, which is the
    -- object "activate abilities of artifacts" is about rather than the ability.
    Activating ObjectId.ObjectId
  deriving (Eq, Ord, Show)

-- | The spell being cast, and Nothing for the other two arms.
--
-- CR 400.7d's record of which mana paid for a spell is the one question that
-- wants this narrowing rather than the whole subject: Pawl.Engine.Cost's
-- @recordSpent@ writes only for a cast, since every printing that reads the
-- record asks about one.
castOf :: PaymentSubject -> Maybe ObjectId.ObjectId
castOf subject = case subject of
  ForNeither -> Nothing
  Casting oid -> Just oid
  Activating _ -> Nothing
