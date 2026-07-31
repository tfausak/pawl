module Pawl.Types.TriggerCondition where

import Pawl.Types.Condition (Condition)
import Pawl.Types.Filter (Filter)
import Pawl.Types.Phase (Phase)
import Pawl.Types.PlayerRelation (PlayerRelation)
import Pawl.Types.TriggerFrequency (TriggerFrequency)
import Pawl.Types.TurnScope (TurnScope)

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
  | -- CR 701.9a: "whenever [a player] discards a card" -- Megrim's "whenever an
    -- OPPONENT discards a card". Matched against GameEvent.Discarded, whose
    -- PlayerId is the discarding player; the PlayerRelation reads that player
    -- against CR 109.5's "you", the ability's controller (CR 603.3a).
    --
    -- NOT self-scoped, unlike SelfCycled just above: the bearer is a bystander
    -- watching someone else's hand, so nothing about the bearer is part of the
    -- match. Bartered Cow's "when you discard this card" is the self-scoped
    -- sibling, and is a different condition that does not exist yet (#319).
    --
    -- The DiscardCause is deliberately not part of this condition. CR 702.29a
    -- makes cycling a discard, so a cycled card must fire this; CR 702.29d
    -- bounds it to once, which the single Discarded event supplies by
    -- construction (see Pawl.Types.GameEvent).
    PlayerDiscards PlayerRelation
  | -- CR 508.3a: "An ability that reads 'Whenever [a creature] attacks, . . .'
    -- triggers if that creature is declared as an attacker." Hanweir Garrison's.
    -- Self-scoped like SelfEnters and SelfCycled: the scan visits every
    -- permanent, so the bearer being the declared attacker is part of the match.
    --
    -- DECLARED is the whole content of the condition, not a synonym for
    -- "attacking": the same rule's last sentence is "such abilities won't trigger
    -- if a creature is put onto the battlefield attacking", and CR 508.4 says such
    -- a creature is attacking but "for the purposes of trigger events and effects
    -- ... never attacked". So this matches GameEvent.AttackerDeclared, which only
    -- the declaration appends, and never the combat record.
    --
    -- No attack TARGET is carried. CR 508.3a's second sentence ("Whenever [a
    -- creature] attacks [a player, planeswalker, or battle]") is a different
    -- condition, and no card in the pool has it.
    --
    -- The TriggerFrequency is Aurelia, the Warleader's "for the first time each
    -- turn" -- a payload rather than a sibling condition, because it narrows this
    -- same trigger event rather than naming a different one. Hanweir Garrison is
    -- EveryTime. See Pawl.Types.TriggerFrequency for why no rule number is cited
    -- for the phrase.
    SelfAttacks TriggerFrequency
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
  | -- CR 603.6c: a LEAVES-THE-BATTLEFIELD ability, narrowed to the one written
    -- form Doomed Traveler prints. That rule's second written form is "Whenever
    -- [something] is put into a graveyard from the battlefield", and CR 700.4
    -- abbreviates it: "The term dies means 'is put into a graveyard from the
    -- battlefield.'" So the match is a zone PAIR -- battlefield to graveyard --
    -- and NOT the whole of CR 603.6c, whose first clause is "a permanent moves
    -- from the battlefield to another zone", any zone at all. A card that says
    -- "leaves the battlefield" must not be conflated with one that says "dies",
    -- so the wider condition is a separate one that does not exist yet (#384).
    --
    -- Self-scoped like SelfEnters and SelfCycled: the scan visits every
    -- candidate source, so the bearer being the permanent that died is part of
    -- the match, not an accident of which object the scan visited.
    --
    -- The bearer is the incarnation that was ON THE BATTLEFIELD -- the id
    -- ZoneChange.departed carries and GameState.lastKnown files under -- and
    -- NOT the CR 400.7 incarnation that arrived in the graveyard. That is CR
    -- 603.10a ("some zone-change triggers look back in time. These are
    -- leaves-the-battlefield abilities ...") read through CR 603.10's own
    -- definition of looking back: "using the existence of those abilities and
    -- the appearance of objects immediately prior to the event". Both halves
    -- live in that one last-known record -- the ability exists because the
    -- characteristics taken from the pre-move projection say so, and CR 603.3a's
    -- controller is the player who controlled the permanent as it left.
    --
    -- The arriving incarnation is not lost, though: CR 400.7e lets the ability
    -- "find the new object that it became in the zone it moved to", and
    -- Pawl.Event.eventBindings binds that id under Pawl.Binding.became. So the
    -- bearer and the object the payload acts on are deliberately two different
    -- ids, and the one printed word "it" means whichever of them the sentence
    -- is about.
    --
    -- The contrast with SelfPutIntoGraveyardFromLibrary above is exact, and the
    -- two must not be conflated: that one is library to graveyard, functions IN
    -- the graveyard (CR 113.6k, since it can never trigger from the
    -- battlefield), and reads the ARRIVING incarnation; this one is battlefield
    -- to graveyard, functions on the battlefield, and reads the DEPARTING one.
    SelfDies
  deriving (Eq, Ord, Show)
