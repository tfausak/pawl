module Pawl.Types.StatedFlip where

import qualified Pawl.Types.CoinFace as CoinFace

-- | The payload of Pawl.Types.PlayerEffect's StateCoinFlip arm (CR 705.3): what
-- an effect states about a coin flip this player flips, in the rule's own two
-- halves.
--
-- Both halves in one payload rather than two arms, because rule 705.3 words them
-- as "and\/or" and Edgar, King of Figaro prints both at once ("those coins come
-- up heads and you win those flips"). A card stating only one leaves the other
-- field inert.
data StatedFlip = MkStatedFlip
  { -- | CR 705.3's "a coin flip has a certain result": the face to use instead of
    -- the one the coin came up. Nothing states no face, and the actual flip
    -- stands.
    face :: Maybe CoinFace.CoinFace,
    -- | CR 705.3's "a certain player wins a coin flip". Reaches even CR 705.2's
    -- first-sentence flip, which no player could otherwise win -- Edgar's own
    -- ruling says so, and rule 705.3's last sentence is what allows it.
    wins :: Bool,
    -- | Edgar's "the FIRST time you flip one or more coins each turn". False
    -- states every flip. Plain card text narrowing the statement, with no
    -- comprehensive rule behind it -- Pawl.Types.TriggerFrequency says the same
    -- of the trigger side of that phrase.
    --
    -- Spent on the first INSTRUCTION and not on the first FLIP, which is what
    -- Edgar's ruling on a multi-coin instruction says ("Edgar's last ability
    -- modifies that set of flips"). Pawl.Engine.Coin.statementsFor is where the
    -- statements are read once for the whole instruction, and Pawl.CoinSpec's
    -- "CR 705.3 a statement spent on an instruction reaches every coin of it"
    -- proves the two apart over Flock of Rabid Sheep.
    firstEachTurn :: Bool
  }
  deriving (Eq, Ord, Show)
