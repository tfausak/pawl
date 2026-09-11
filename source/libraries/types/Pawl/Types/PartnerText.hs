module Pawl.Types.PartnerText where

-- | CR 702.124i: which "partner—[text]" ability a card has. The rule lists
-- exactly these four, and two cards pair only when they share one.
data PartnerText
  = -- | CR 702.124i: "partner—Character select".
    CharacterSelect
  | -- | CR 702.124i: "partner—Father & son".
    FatherAndSon
  | -- | CR 702.124i: "partner—Friends forever".
    FriendsForever
  | -- | CR 702.124i: "partner—Survivors".
    Survivors
  deriving (Bounded, Enum, Eq, Ord, Show)
