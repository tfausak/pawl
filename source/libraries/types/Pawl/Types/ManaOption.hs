module Pawl.Types.ManaOption where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana

-- | CR 106.12: ONE way to tap a permanent for mana -- the cost CR 602.2b makes
-- that activation pay, paired with the whole yield it adds.
--
-- FIVE parts, strictly. The RIDER the ability prints about when it may be
-- activated (CR 602.5) rides along with the cost, because CR 605.3b gives a mana
-- ability no stack window for Pawl.Engine.Activate to gate it in and
-- Pawl.Engine.Cost.manaActivations is where CR 605.3a's two windows both ask
-- instead. A cost cannot carry it: Pawl.Types.Cost is CR 118.1's payment and
-- says nothing about timing, and CR 305.6's intrinsic ability has a cost with no
-- ability to have printed a rider at all.
--
-- The ABILITY the route came from rides along for CR 602.5b's counted rider,
-- which is a fact about the ability rather than about the route and so can be
-- read off none of the other four parts:
-- Pawl.Engine.ActivationRestriction.restrictionMet needs it to ask which ability
-- of this object has already been spent, and Pawl.Engine.Cost.tapForManaWith
-- needs it to record the one it just spent. Nothing for CR 305.6's intrinsic
-- route, which is printed on no card and so is no ability's. Loot, the
-- Pathfinder's "{G}, {T}: Add three mana of any one color" is the printing, and
-- Pawl.ManaSpec's Loot group is what proves it.
--
-- The cost and the yield both, because either alone loses a distinction the player is entitled
-- to make. The yield alone cannot separate an Urborg'd Mana Confluence's free
-- {B} from the {B} it charges 1 life for (#1117); the cost alone cannot separate
-- Birds of Paradise's five colours (CR 105.4).
--
-- VALUES throughout -- the cost, and the ability beside it -- rather than an
-- ordinal into the source's mana abilities, for Pawl.Types.ReplacementEntry's
-- reason: two EQUAL abilities have to stay interchangeable, which value equality
-- gives and an ordinal does not, and there is no one list to index into, CR
-- 305.6's intrinsic ability being printed on no card and a member of nothing.
-- Two abilities alike in cost and yield but not in themselves are two options
-- and not one, which is the same rule read the other way: CR 602.5b's memory is
-- keyed to the ability, so spending one leaves the other.
data ManaOption = MkManaOption
  { cost :: Cost.Cost Keyword.Keyword,
    restrictions :: [ActivationRestriction.ActivationRestriction],
    -- | CR 602.5b: the ability this route is one selection of, or Nothing for CR
    -- 305.6's intrinsic one. See the note above the record.
    ability :: Maybe (ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)),
    yield :: Mana.Mana,
    -- | CR 405.6c: what else this activation does -- everything the chosen mode
    -- says that is not a mana production. Ancient Tomb's "This land deals 2
    -- damage to you", which Pawl.Engine.Cost.tapForManaWith runs through its
    -- Pawl.Types.ManaAbilityPerformer once the mana is added.
    --
    -- Part of the OPTION rather than looked up from the source afterwards, for
    -- the same reason the cost is: the option is what the player was offered,
    -- and two routes alike in cost and yield but not in what else they do are
    -- two options rather than one (List.nub in
    -- Pawl.Engine.Mana.manaOptionsOfGiven compares this too).
    effects :: [Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)]
  }
  deriving (Eq, Ord, Show)
