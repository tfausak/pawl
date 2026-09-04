module Pawl.Types.ManaAddition where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaRider as ManaRider
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 106.3 / 106.4: "this player adds this much mana, decided this way" -- the
-- whole payload of Pawl.Types.Effect's AddMana arm.
--
-- The PlayerRef is the half CR 106.4 leaves open: the rule says the mana "goes
-- into a player's mana pool" without saying whose, and CR 106.3's "instructs a
-- player to add that mana" is the sentence a card fills in. Almost every printing
-- leaves it unwritten and means CR 109.5's "you" -- Llanowar Elves' "Add {G}" --
-- which is why the codec DEFAULTS this field to @Relative You@ rather than
-- requiring it. Shizuko, Caller of Autumn's "that player adds {G}{G}{G}" writes
-- it as @InSlot@ over the slot CR 603.2b's step event bound, and Stadium Vendors'
-- "that player adds two mana" over the slot its own ChoosePlayer bound.
--
-- A PlayerRef and not a PlayerScope, for the reason Pawl.Types.PlayerCounters'
-- field gives: only PlayerRef can name a binding slot, and naming one is the
-- whole point here.
--
-- The COUNT is how many units this one instruction adds, and it is a field
-- rather than that many AddMana effects because CR 105.4's choice is made ONCE
-- per instruction: Stadium Vendors' "two mana of any one color they choose" adds
-- two, both of the colour the recipient names, where two AddMana effects would
-- ask twice and let the answers differ. A card whose type is already settled may
-- be written either way -- Shizuko, Caller of Autumn spells {G}{G}{G} as three
-- effects -- since with one option there is nothing to keep in step.
--
-- The RESTRICTION rides this instruction for the same reason, and CR 106.6a is
-- the rule that says so in as many words: a restriction is "created by the spell
-- or ability" and "will apply to all mana produced" by it, so it belongs to the
-- instruction and is copied onto every unit that instruction adds rather than
-- being authored one unit at a time. Geosurge writes its seven AddMana effects
-- with the same restriction on each. The RIDER (Pawl.Types.ManaRider) rides here
-- for the same reason and by the same sentence, which names "any restrictions or
-- additional effects" together.
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
    -- | CR 106.3: how many mana this ONE instruction adds, all of the type its
    -- production decides. The codec defaults it to 1, which is what every
    -- printing saying "add one mana" means.
    count :: Natural.Natural,
    retention :: ManaRetention.ManaRetention,
    -- | CR 106.6: what the mana this instruction adds may be spent on, stamped
    -- onto every unit it produces (Pawl.Types.ManaUnit.restriction). Nothing is
    -- the unrestricted default every printing but a CR 106.6 one means.
    restriction :: Maybe ManaRestriction.ManaRestriction,
    -- | CR 106.6's other shape, riding this instruction for the same reason and
    -- stamped onto every unit alongside the restriction
    -- (Pawl.Types.ManaUnit.rider): what the mana DOES to the spell it is spent
    -- on, as opposed to what it may be spent on at all. Boseiju, Who Shelters
    -- All prints this and no restriction, Mishra's Workshop a restriction and no
    -- rider, and Delighted Halfling both.
    rider :: Maybe ManaRider.ManaRider
  }
  deriving (Eq, Ord, Show)
