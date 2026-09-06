module Pawl.Types.ActivationProhibition where

import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Affected as Affected

-- | CR 602.2 / CR 101.2: one printed ACTIVATION PROHIBITION -- an effect saying
-- an object's "activated abilities can't be activated". Arrest and Volrath's
-- Curse state it of every one of them; Realmbreaker's Grasp states it of every
-- one but a mana ability.
--
-- A printed static carrier alongside Pawl.Types.StaticAbility,
-- Pawl.Types.PlayerStaticAbility, Pawl.Types.CombatRestriction,
-- Pawl.Types.SacrificeRestriction and its siblings.
-- Pawl.Types.BlockRequirement's header argues why neither of the first two can
-- hold one, and every step of that argument holds here unchanged: CR 613.1's
-- layers compute an OBJECT's characteristics and "can't be activated" is not
-- one of them, and a PlayerScope names PLAYERS where this names a permanent.
--
-- NOT a Pawl.Types.Modification, for the reason
-- Pawl.Types.SacrificeRestriction states at length: CR 613.11 puts a continuous
-- effect that "affects game rules rather than objects" outside the layer
-- system, CR 101.2a says such an effect is not an ability being added or
-- removed, and Pawl.Engine.Projection sees none of them. Losing the ability and
-- being unable to activate it are observably different -- an ability nothing
-- removed still triggers Pawl.Types.Filter's keyword reads and still answers
-- Tsabo's Web -- so a layer-6 removal would be the wrong reading rather than a
-- cheaper one.
--
-- The PLAYER-scoped sentence is not this type. Sen Triplets' "your opponents
-- can't ... activate abilities" names players and reaches
-- Pawl.Types.PlayerEffect's CantActivateAbilities; CR 701.35a's detain names one
-- permanent a resolution already chose, and rides that permanent as
-- Object.detainedUntil. This is the printed sentence AIMED AT AN OBJECT, which
-- neither of those can say.
--
-- The STORED counterpart -- a prohibition a resolution puts on a permanent for a
-- duration, Deadlock Trap's "this turn" -- is Pawl.Types.ForbidActivation and
-- the Pawl.Types.ActiveActivationProhibition rows it leaves; both roads meet at
-- Pawl.Engine.ActivationProhibition.cantActivate.
--
-- Gathered LIVE from the battlefield on every activation window and never
-- captured, the posture every sibling carrier takes: an Arrest that left the
-- battlefield lifts its prohibition with nothing to unwind.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.ActivationProhibition is the only module that may read this type,
-- and it answers a Bool about one object and one CR 605.1a kind.
data ActivationProhibition = MkActivationProhibition
  { -- | Whose activated abilities can't be activated. An Affected, not a bare
    -- ObjectId, so the set is re-derived at every window -- the field name every
    -- sibling restriction spells, and for its reason: it names the RESTRICTED
    -- objects, never something they act on.
    affected :: Affected.Affected,
    -- | Which CR 605.1a KIND of activated ability the prohibition refuses, or
    -- Nothing for every kind. Arrest and Volrath's Curse name none ("its
    -- activated abilities can't be activated"); Realmbreaker's Grasp names one
    -- by exclusion ("unless they're mana abilities", which leaves
    -- NonManaAbility).
    --
    -- Maybe rather than a set, Pawl.Types.CounterRestriction's field one carrier
    -- over: the rulebook's division has two sides, so an empty set would be a
    -- second spelling of "no prohibition at all" and the full one a second
    -- spelling of Nothing. Just ManaAbility is expressible and no printing
    -- writes it; the field is a classification either way, so admitting the
    -- other side costs nothing and asserts nothing.
    kind :: Maybe AbilityKind.AbilityKind,
    -- | CR 116.2d: the name this face gives the ability stating the
    -- prohibition, so that face's own SpecialAction.IgnoreThisUntilEndOfTurn
    -- can say WHICH effect a payment ignores. Pawl.Types.AffectedUnless.name on
    -- the combat carrier and Pawl.Types.PlayerStaticAbility.name on the player
    -- axis, with the same meaning and the same dataflow lint.
    --
    -- Nothing for Arrest and Realmbreaker's Grasp, which grant no such
    -- permission. Volrath's Curse names this row and its two combat
    -- restrictions alike, one sentence declaring all three.
    name :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
