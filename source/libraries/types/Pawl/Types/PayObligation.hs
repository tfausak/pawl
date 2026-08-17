module Pawl.Types.PayObligation where

-- | CR 118.12's two limbs: whether the cost a clause states is one its payer may
-- decline, or one they must pay if able. The rule names both -- the "if [a
-- player] does" clause "checks whether the player chose to pay an OPTIONAL cost
-- or STARTED TO PAY a mandatory cost".
--
-- @Optional@ is the unmarked case and covers CR 118.12a's whole rewriting: "'[Do
-- something] unless [a player does something else]'" becomes "[A player MAY do
-- something else]", so Mana Leak, ward (CR 702.21a) and fabricate (CR 702.123a)
-- are all optional however their text reads. Merfolk Seer prints the "may"
-- itself. @Mandatory@ is Standstill's "sacrifice this enchantment. If you do":
-- no "may" anywhere, and its controller sacrifices it if able.
--
-- NOT Pawl.Types.Optionality, which is CR 603.5's printed "may" over the
-- clause's own instructions. The two questions sit on one clause and mean
-- different things, and Pawl.Types.PayBranch's note that "a card setting both
-- would ask twice" is the hazard one shared type would invite. A card writes
-- exactly one of them.
--
-- Not a Bool, for Pawl.Types.Optionality's reason: @Mandatory@ says which limb
-- of the rule is in play where @True@ would say nothing.
data PayObligation
  = -- | The payer is asked (Prompt.ChooseToPay) and may decline.
    Optional
  | -- | The payer is not asked. CR 118.3 still governs: a payer who cannot pay
    -- fully has not paid, and the "can't" branch runs.
    Mandatory
  deriving (Bounded, Enum, Eq, Ord, Show)
