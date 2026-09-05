module Pawl.Types.SpecialAction where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

-- | CR 116.2: a special action a face's printed text grants -- something a
-- player may do with priority that does not use the stack. WHICH player is the
-- constructor's own question: CR 116.2e's is the card's holder, CR 116.2d's is
-- whoever the ignored ability is affecting.
--
-- Named for the OPERATION and never for the card. CR 116.2e is one card by name,
-- but the engine must not learn that name: Pawl.Engine.Action reads this
-- permission off the card data and Pawl.Types.Action carries the rulebook's own
-- vocabulary, so neither half cases on an identity.
--
-- The carrier for the rows of CR 116.2 a card grants IN PROSE. The rules grant
-- CR 116.2a's land play to every player, so it needs no printed permission; CR
-- 116.2f, CR 116.2h and CR 116.2k are granted by KEYWORDS (suspend, foretell,
-- plot), so they belong to Pawl.Types.Keyword rather than here whenever those
-- land; and CR 116.2c's permission is created by a RESOLUTION rather than
-- printed, so it rides the stored effect it ends (Pawl.Types.Expiry's WhenPaid)
-- rather than a face's text (#875).
data SpecialAction
  = -- | CR 116.2e: "You may discard this card any time you could cast an
    -- instant." Circling Vultures is the card the rule names.
    --
    -- The timing is NOT carried, because CR 116.2e's own last sentence overrides
    -- the printed wording: "a player can take such an action any time they have
    -- priority". So the permission is unconditional and nothing reads casting
    -- timing to offer it.
    DiscardThisAnyTime
  | -- | CR 116.2d: "some effects from static abilities allow a player to take an
    -- action to ignore the effect from that ability for a duration". Leonin
    -- Arbiter's "any player may pay {2} for that player to ignore this effect
    -- until end of turn" is one; Damping Engine, Volrath's Curse and Lost in
    -- Thought are the only others printed.
    --
    -- WHICH effect is named by an AbilityName, matched against
    -- PlayerStaticAbility.name on the same face -- CR 116.2d's own subject is
    -- "the effect from that ability", singular, and a permanent granting the
    -- permission on one of two unrelated abilities must ignore only the one
    -- named. A name and not an index, for Pawl.Types.AbilityName's reason, and it
    -- reaches SEVERAL rows rather than one: Damping Engine's single sentence
    -- declares two player abilities that both carry its name, so one payment
    -- still covers both. Pawl.AbilitySlotLintSpec joins the two sides.
    --
    -- The DURATION is not carried: all four print "until end of turn", and CR
    -- 116.2d states only "for a duration" rather than fixing one, so a card that
    -- printed another would want a Pawl.Types.Duration here.
    --
    -- WHO may take it is not carried either, and needs no payload: the rule's own
    -- answer is the players the ability is AFFECTING, which the carrier's
    -- PlayerScope already states. Leonin Arbiter's "any player" is its EachPlayer
    -- scope and Damping Engine's "that player" is its narrower one, so
    -- Pawl.Engine.Ignore.canIgnore derives both from
    -- Pawl.Engine.PlayerEffect.affectedBy. The two Auras narrow by a scope on an
    -- OBJECT-axis effect instead, which that derivation does not reach (#1268).
    --
    -- The COST is carried, since it is what the printed sentence varies most
    -- freely -- {2}, sacrificing a permanent, exiling three cards from a
    -- graveyard. Paid through Pawl.Engine.Cost like every other, and never
    -- totalled through CR 601.2f: a special action is neither a spell being cast
    -- nor an ability being activated.
    IgnoreThisUntilEndOfTurn AbilityName.AbilityName (Cost.Cost Keyword.Keyword)
  deriving (Eq, Ord, Show)
