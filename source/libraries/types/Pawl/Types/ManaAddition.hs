module Pawl.Types.ManaAddition where

import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 106.3 / 106.4: "this player adds one mana, decided this way" -- the whole
-- payload of Pawl.Types.Effect's AddMana arm.
--
-- The PlayerRef is the half CR 106.4 leaves open: the rule says the mana "goes
-- into a player's mana pool" without saying whose, and CR 106.3's "instructs a
-- player to add that mana" is the sentence a card fills in. Almost every printing
-- leaves it unwritten and means CR 109.5's "you" -- Llanowar Elves' "Add {G}" --
-- which is why the codec DEFAULTS this field to @Relative You@ rather than
-- requiring it; Shizuko, Caller of Autumn's "that player adds {G}{G}{G}" is the
-- printing that writes it, as @InSlot@ over the slot CR 603.2b's step event
-- bound.
--
-- A PlayerRef and not a PlayerScope, for the reason Pawl.Types.PlayerCounters'
-- field gives: only PlayerRef can name a binding slot, and naming one is the
-- whole point here.
--
-- Still ONE unit of mana, as the AddMana arm's own haddock says -- this record
-- adds a recipient and a duration to that instruction and changes nothing about
-- its size.
--
-- The RETENTION rides this instruction rather than a second opcode, because
-- Shizuko's "they don't lose THIS mana" names the units the preceding
-- instruction added and there is no slot machinery for mana units for a second
-- opcode to name. It is not a player-axis effect either
-- (Pawl.Types.PlayerEffect.DontLoseUnspentMana): that carrier says one thing
-- about a player's whole pool, and this sentence says different things about two
-- manas of one pool -- the criterion Pawl.Types.SpendManaAsThough's haddock
-- states for the same split.
data ManaAddition = MkManaAddition
  { player :: PlayerRef.PlayerRef,
    production :: ManaProduction.ManaProduction,
    retention :: ManaRetention.ManaRetention
  }
  deriving (Eq, Ord, Show)
