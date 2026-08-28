module Pawl.Types.AttachBound where

import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.3: attach the object bound in `subject` to the object targeted in
-- `destination`. The third arrangement of Pawl.Types.Effect's attach family, and
-- the two fields are not the same kind of thing: `subject` is a BINDING the
-- engine stamped (Binding.became, the permanent a CR 400.7e entry trigger names
-- as "it"), while `destination` is a TARGET the card's own "target creature"
-- declared.
--
-- That asymmetry is the whole point. Effect.Attach's mover is the ability's
-- source and Effect.AttachTarget's destination is picked as the effect resolves
-- (CR 115.10a: not a target), so neither can say "attach IT to TARGET creature".
-- Here CR 115.1d puts the destination's choice at CR 603.3d, which is what makes
-- hexproof (CR 702.11b) refuse it, CR 115.7 able to move it, and CR 608.2b able
-- to fizzle it.
data AttachBound = MkAttachBound
  { subject :: SlotName.SlotName,
    destination :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
