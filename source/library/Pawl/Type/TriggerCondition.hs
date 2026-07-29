module Pawl.Type.TriggerCondition where

import Pawl.Type.Condition (Condition)
import Pawl.Type.Filter (Filter)
import Pawl.Type.Phase (Phase)
import Pawl.Type.TurnScope (TurnScope)

-- CR 603.2: the pattern that fires a triggered ability. Only Pawl.Event may case
-- on it for RULES purposes; Pawl.Codec also cases on every constructor, but only
-- as the JSON data boundary (encode/decode), not to decide game behaviour.
data TriggerCondition
  = -- CR 603.6a: "when this ... enters [the battlefield]" -- fires when the object
    -- BEARING the ability enters. Self-scoped: the scan checks every permanent
    -- (CR 603.6a), so the bearer's identity is part of the match, not an accident
    -- of which object the scan happened to visit. PermanentEnters below is the
    -- other half of that same rule's sentence, and is kept SEPARATE rather than
    -- rewritten as `PermanentEnters IsSource`: this arm is a bare comparison of
    -- ids, while that one has to READ the entrant's characteristics, and reading
    -- them can come up empty for an entrant that ceased without a zone change
    -- ever filing last known information (see Pawl.Event.matchesTrigger).
    SelfEnters
  | -- CR 603.6a's SECOND written form, in the same breath as the first:
    -- "Whenever a [type] enters, . . ." -- fires when ANY permanent the Filter
    -- admits enters the battlefield, whoever bears the ability. The "[type]" is
    -- a Filter, matched against the entering permanent with the bearer as the
    -- Filter.Context's source and the bearer's controller as its perspective.
    --
    -- Named for the rule ("a permanent enters the battlefield"), NOT
    -- "OtherEnters": the bearer is not excluded by this constructor. CR 603.6a
    -- says "all permanents on the battlefield (INCLUDING THE NEWCOMERS) are
    -- checked", so a permanent must be able to see its own entry through this
    -- condition whenever the Filter admits it. Soul Warden's "whenever ANOTHER
    -- creature enters" writes that exclusion into the Filter as
    -- `Not IsSource` -- the one spelling Filter.IsSource's own haddock already
    -- fixes for "another" (#163) -- rather than into a parallel exclusion flag
    -- here.
    PermanentEnters Filter
  | -- CR 603.2b: "at the beginning of [each|your] <step>". Matched against a
    -- GameEvent.StepBegan; the TurnScope decides whose turn qualifies.
    StepBegins Phase TurnScope
  | -- CR 603.8: a STATE trigger -- it fires whenever its condition is true, not
    -- when an event occurs. "It doesn't trigger again until the ability has
    -- resolved, has been countered, or has otherwise left the stack", which is why
    -- Pawl.Event derives armedness from the stack rather than storing it.
    StateIs Condition
  | -- CR 603.2 / 509-510: the bearer dealt combat damage to a player. Rides P4's
    -- event history -- combat damage already records a DamageDealt event.
    SelfDealsCombatDamageToPlayer
  | -- CR 725.2: a creature dealt combat damage to the monarch. NOT bearer-scoped
    -- (any creature); matched only via Pawl.Monarch.inherentMatch, never through a
    -- card's bearer. Rides P4's DamageDealt history.
    CreatureDealtCombatDamageToMonarch
  | -- CR 702.29c: "'When you cycle this card' means 'When you discard this card
    -- to pay an activation cost of a cycling ability.'" Self-scoped like
    -- SelfEnters: the scan visits every candidate source, so the bearer being the
    -- cycled card is part of the match.
    --
    -- The bearer is the card in the zone it landed in, because that same rule
    -- continues "these abilities trigger from whatever zone the card winds up in
    -- after it's cycled" -- the graveyard, for every printing today.
    SelfCycled
  | -- CR 603.6 (a zone-change trigger): "when this card is put into your
    -- graveyard from your library" -- Narcomoeba's. Self-scoped like SelfEnters
    -- and SelfCycled: the scan visits every candidate source, so the bearer
    -- being the card that arrived is part of the match.
    --
    -- The bearer is the card AS IT NOW IS IN THE GRAVEYARD -- CR 400.7's new
    -- incarnation, which CR 400.7e says such an ability may find ("can find the
    -- new object that it became in the zone it moved to when the ability
    -- triggered, if that zone is a public zone"; a graveyard is public, CR
    -- 400.2) -- because CR 113.6k puts the ability there: "A trigger
    -- condition that can't trigger from the battlefield functions in all zones
    -- it can trigger from." A card cannot be put into a graveyard from a library
    -- while it is on the battlefield, so this condition can never trigger from
    -- the battlefield, and the one zone it can trigger from is the graveyard it
    -- lands in.
    --
    -- No zone pair is carried, so this is not a general "moved from A to B".
    -- Narcomoeba is the only card in the pool with the shape, and the two zones
    -- are exactly what makes CR 113.6k apply to it; a second card wanting a
    -- different pair is the one that must generalise this.
    --
    -- The printed "your ... your" needs no controller check. A card is always
    -- put into its OWNER's graveyard from its OWNER's library, and a card in a
    -- graveyard has no controller (CR 108.4), so CR 113.8 makes the ability's
    -- controller that same owner: the two "your"s can only ever name the player
    -- the scan already picks.
    SelfPutIntoGraveyardFromLibrary
  deriving (Eq, Ord, Show)
