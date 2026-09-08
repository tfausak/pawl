module Pawl.Types.RuleAbilities where

import qualified Pawl.Types.ActivationProhibition as ActivationProhibition
import qualified Pawl.Types.AttachRestriction as AttachRestriction
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BlockCost as BlockCost
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.CounterRestriction as CounterRestriction
import qualified Pawl.Types.CrewRestriction as CrewRestriction
import qualified Pawl.Types.EntryRestriction as EntryRestriction
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Types.UntapRestriction as UntapRestriction

-- | CR 613.11: the ability families whose continuous effects "affect game rules
-- rather than objects" -- the thirteen lists Pawl.Types.Face carries beside its
-- keywords and its static abilities, and which no layer of CR 613 touches.
--
-- ONE bundle rather than thirteen fields on Pawl.Types.ProjectedCharacteristics,
-- because the thirteen share a single posture down to the line: each is seeded
-- from the printed face, rewritten by no layer, and read by exactly one engine
-- module outside the layer fold. Grouping them makes CR 702.140e's union one
-- `<>` and CR 707.2a's copy one field, so a fourteenth family cannot be added
-- to the record and left out of either; see #3373.
--
-- Face keeps the thirteen apart because a card's JSON names each one; nothing
-- here needs that, since every reader asks for one field by name.
data RuleAbilities = MkRuleAbilities
  { -- | CR 602.2: "its activated abilities can't be activated" (Arrest).
    activationProhibitions :: [ActivationProhibition.ActivationProhibition],
    -- | CR 303.4 / 301.5: "this creature can't be equipped" (Goblin Brawler).
    attachRestrictions :: [AttachRestriction.AttachRestriction],
    -- | CR 508.1h: Ghostly Prison's {2} per attacking creature.
    attackCosts :: [AttackCost.AttackCost],
    -- | CR 508.1d: "creatures ... attack each combat if able" (Curse of the
    -- Nightly Hunt).
    attackRequirements :: [AttackRequirement.AttackRequirement],
    -- | CR 509.1d: Oppressive Rays' {3} per blocking creature.
    blockCosts :: [BlockCost.BlockCost],
    -- | CR 509.1a: "this creature can block an additional creature each combat"
    -- (Foriysian Brigade).
    blockPermissions :: [BlockPermission.BlockPermission],
    -- | CR 509.1c: "all creatures able to block enchanted creature do so" (Lure).
    blockRequirements :: [BlockRequirement.BlockRequirement],
    -- | CR 508.1c / 509.1b: "enchanted creature can't attack or block" (Pacifism).
    combatRestrictions :: [CombatRestriction.CombatRestriction],
    -- | CR 122.6: "counters can't be put on ..." (Solemnity).
    counterRestrictions :: [CounterRestriction.CounterRestriction],
    -- | CR 702.122d: "enchanted creature can't ... crew Vehicles" (Revoke
    -- Privileges).
    crewRestrictions :: [CrewRestriction.CrewRestriction],
    -- | CR 400.4a: "creature cards in graveyards and libraries can't enter the
    -- battlefield" (Grafdigger's Cage).
    entryRestrictions :: [EntryRestriction.EntryRestriction],
    -- | CR 701.21a: "creatures you control but don't own ... can't be sacrificed"
    -- (Garland, Royal Kidnapper).
    sacrificeRestrictions :: [SacrificeRestriction.SacrificeRestriction],
    -- | CR 502.3: "each land with an activated ability that isn't a mana ability
    -- doesn't untap during its controller's untap step" (Tsabo's Web).
    untapRestrictions :: [UntapRestriction.UntapRestriction]
  }
  deriving (Eq, Ord, Show)

-- | CR 702.140e: a merged permanent has all abilities of each card representing
-- it, which for these thirteen families is concatenation. Field by field rather
-- than a derived instance, so a fourteenth field cannot be added without
-- -Werror naming this site.
instance Semigroup RuleAbilities where
  left <> right =
    MkRuleAbilities
      { activationProhibitions = activationProhibitions left <> activationProhibitions right,
        attachRestrictions = attachRestrictions left <> attachRestrictions right,
        attackCosts = attackCosts left <> attackCosts right,
        attackRequirements = attackRequirements left <> attackRequirements right,
        blockCosts = blockCosts left <> blockCosts right,
        blockPermissions = blockPermissions left <> blockPermissions right,
        blockRequirements = blockRequirements left <> blockRequirements right,
        combatRestrictions = combatRestrictions left <> combatRestrictions right,
        counterRestrictions = counterRestrictions left <> counterRestrictions right,
        crewRestrictions = crewRestrictions left <> crewRestrictions right,
        entryRestrictions = entryRestrictions left <> entryRestrictions right,
        sacrificeRestrictions = sacrificeRestrictions left <> sacrificeRestrictions right,
        untapRestrictions = untapRestrictions left <> untapRestrictions right
      }

-- | An object with no card behind it (an ability on the stack) has none of
-- these, which is also what CR 613.1f's strip would leave if any layer reached
-- them.
instance Monoid RuleAbilities where
  mempty =
    MkRuleAbilities
      { activationProhibitions = [],
        attachRestrictions = [],
        attackCosts = [],
        attackRequirements = [],
        blockCosts = [],
        blockPermissions = [],
        blockRequirements = [],
        combatRestrictions = [],
        counterRestrictions = [],
        crewRestrictions = [],
        entryRestrictions = [],
        sacrificeRestrictions = [],
        untapRestrictions = []
      }
