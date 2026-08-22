module Pawl.Types.FaceDownReason where

-- | CR 708.6: what a player must know about each of their face-down objects
-- includes "what ability or rules caused the permanents to be face down". This
-- type is that fact, and CR 708.7 is why the engine needs it rather than merely
-- the players -- "the ability or rules that allow a permanent to be face down
-- may also allow the permanent's controller to turn it face up", so the allower
-- decides which turn-face-up procedures exist.
--
-- Rides Facing.FaceDown and not Object, which is CR 701.40a's "that permanent is
-- a manifested permanent FOR AS LONG AS IT REMAINS FACE DOWN" written as a
-- place: turning face up and CR 400.7's new incarnation both erase the status,
-- so both erase the reason with it and no second write is needed.
--
-- NOT a characteristic, so emphatically not a field of
-- FaceDownCharacteristics: CR 708.2 lists what the object IS, and this says what
-- turned it over. The two answer different questions and one of them is copiable.
--
-- A closed-half type. Rules 701.40 and 702.37 are the rulebook, so naming them
-- here is the same act as naming a Phase -- never the identity of an effect.
data FaceDownReason
  = -- | CR 702.37c: cast face down using a morph ability, "turn it face down and
    -- announce that you're using a morph ability".
    Morphed
  | -- | CR 702.168b: cast face down using a disguise ability, "turn the card face
    -- down and announce that you are using a disguise ability".
    --
    -- Its own arm and not Morphed reused, because the two allowers LIST different
    -- characteristics -- CR 702.168b's list carries ward {2} and CR 702.37c's does
    -- not -- and CR 708.2 makes the list the allower's. The listing rides
    -- Facing.FaceDown beside this, so nothing reads the reason to find it; what
    -- the distinction buys is CR 708.6's own question, which a player must be able
    -- to answer about each of their face-down objects.
    Disguised
  | -- | CR 701.40a: manifested, i.e. put onto the battlefield face down by the
    -- keyword action. The one reason that unlocks CR 701.40b's procedure.
    Manifested
  | -- | CR 708.2a's other producer: a face-up permanent turned face down by a
    -- spell or ability, Backslide's "turn target creature with a morph ability
    -- face down". CR 708.7's permission does NOT follow from this one -- nothing
    -- about being turned face down allows turning back up -- which is why it is
    -- its own arm rather than folded in with Morphed. A permanent turned face
    -- down this way is still turnable by CR 702.37e if the card would have a
    -- morph cost, and that rule asks about the CARD rather than about this.
    TurnedFaceDown
  deriving (Eq, Ord, Show)
