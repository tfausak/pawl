module Pawl.Types.PaymentSubject where

import qualified Pawl.Types.ObjectId as ObjectId

-- | WHAT a cost is being paid for, at the one grain CR 106.6's restrictions ask
-- about: a spell being cast, an ability being activated, a permanent being
-- unlocked or turned face up, or none of those.
--
-- CR 601.2h's payment is a cast's, CR 602.2b's is an activation's, and CR 116.2m
-- \/ 709.5e's and CR 116.2b's are two special actions' -- a restricted mana may
-- admit any of them, under a predicate each (Pawl.Types.ManaRestriction).
--
-- FOUR named payments and not one per special action: CR 116.2 lists eleven, and
-- what earns an arm is a PRINTED rider naming that payment. Nothing printed names
-- foretelling or plotting, so those share ForNeither with CR 508.1j \/ 509.1f's
-- combat toll and CR 118.12's resolution-time payment, which are no special
-- action at all -- an arm of their own rather than an absent one.
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
  = -- | None of the four arms below: a special action no printed rider names
    -- (CR 116.2h's foretell), a combat toll, or CR 118.12's payment as a spell
    -- or ability resolves.
    ForNeither
  | -- | CR 601.2h: the object being CAST.
    Casting ObjectId.ObjectId
  | -- | CR 602.2b: the SOURCE of the ability being activated, which is the
    -- object "activate abilities of artifacts" is about rather than the ability.
    --
    -- Which is why CR 400.7d's record of the mana spent does NOT read this: the
    -- ability object is what the record belongs on, and Pawl.Engine.Cost.pay takes
    -- it as its own argument.
    Activating ObjectId.ObjectId
  | -- | CR 116.2m \/ 709.5e: the PERMANENT whose locked half's unlock cost is
    -- being paid.
    Unlocking ObjectId.ObjectId
  | -- | CR 116.2b: the PERMANENT being turned face up.
    TurningFaceUp ObjectId.ObjectId
  deriving (Eq, Ord, Show)
