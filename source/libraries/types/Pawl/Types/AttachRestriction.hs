module Pawl.Types.AttachRestriction where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 303.4's last sentence -- "other effects can limit what a permanent can be
-- enchanted by" -- and CR 301.5's equivalent for an Equipment, as one printed
-- prohibition: a permanent saying what may not become attached TO IT. Consecrate
-- Land's "enchanted land ... can't be enchanted by other Auras" and Goblin
-- Brawler's "this creature can't be equipped" are the pool's printings.
--
-- The DESTINATION's half of CR 701.3a. The other half is the moving permanent's
-- own text -- an Aura's enchant ability (CR 702.5a), an Equipment's "attached to
-- a creature" (CR 301.5) -- and Pawl.Engine.Attach.attachmentFor is where the two
-- meet.
--
-- The TENTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement, Pawl.Types.AttackRequirement,
-- Pawl.Types.CombatRestriction, Pawl.Types.AttackCost, Pawl.Types.BlockCost,
-- Pawl.Types.BlockPermission and Pawl.Types.SacrificeRestriction. NOT a
-- Pawl.Types.Modification, and for the reason that type's sibling
-- Pawl.Types.SacrificeRestriction states at length: every arm of Modification is
-- a CHARACTERISTIC change computed inside CR 613's layers, and CR 613.11 puts a
-- prohibition in the class of continuous effects that "affect game rules rather
-- than objects", which CR 101.2a says is not an ability being added or removed.
-- Pawl.Engine.Projection sees none of these.
--
-- CR 101.2 is what gives it force over CR 701.3a's permission, and CR 702.16c and
-- CR 702.16d are the rulebook's own instances of the same shape -- protection
-- states the Aura half and the Equipment half separately, in these two fields'
-- words.
--
-- NOT a targeting restriction. CR 702.5a gives the ENCHANT ability both jobs, and
-- this is neither of them: an Aura spell may still target a permanent that
-- refuses it, and it still resolves and enters attached (CR 608.3c) before CR
-- 704.5m buries it. Protection is the only quality that also forbids the
-- targeting, and it says so in a clause of its own (CR 702.16b). Consecrate
-- Land's Gatherer ruling states the Aura half of that and Goblin Brawler's the
-- Equipment half ("you can activate an equip ability that targets Goblin Brawler,
-- but the Equipment will fail to move onto it").
--
-- ONE shape rather than an Aura arm and an Equipment arm: the two rules say the
-- same thing about the same moment (CR 701.3a's attach), and which permanents are
-- barred is exactly what 'attachers' spells. Consecrate Land writes the Aura
-- subtype into that filter; Goblin Brawler writes the Equipment subtype.
--
-- NO "unless" gate, the narrowing Pawl.Types.SacrificeRestriction takes and for
-- its reason: CR 508.1c writes a gate into the rule Pawl.Types.CombatRestriction
-- serves, and CR 701.3a has no counterpart.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture every sibling above takes -- so CR 613.11 lets the prohibition reach
-- permanents that were not on the battlefield when it began, and a Consecrate
-- Land leaving lifts it with nothing to unwind.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.AttachRestriction is the only module that may read it, and it
-- answers a Bool about a pair.
data AttachRestriction = MkAttachRestriction
  { -- | Which permanents may not be attached to. An Affected, not a bare
    -- ObjectId, so the set is re-derived on every read -- Consecrate Land's
    -- Affected.Attached follows the Aura as an effect moves it, and Goblin
    -- Brawler's Matching IsSource names the printing itself. The field name
    -- Pawl.Types.SacrificeRestriction and every Pawl.Types.CombatRestriction arm
    -- spell, and for its reason: it names the RESTRICTED permanents, never
    -- something they act on.
    affected :: Affected.Affected,
    -- | Which permanents may not become attached to them. Matched with the
    -- RESTRICTING permanent as the filter's source and its controller as CR
    -- 109.5's "you", since this is that card's own text: Consecrate Land's
    -- "other Auras" is @Not IsSource@ conjoined with the Aura subtype, which is
    -- what keeps Consecrate Land itself attachable to the land it protects.
    --
    -- Asked of the moving permanent's PROJECTION, so a Consecrate Land under a
    -- CR 613 layer-4 effect that removes the Aura subtype stops barring
    -- anything.
    attachers :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
