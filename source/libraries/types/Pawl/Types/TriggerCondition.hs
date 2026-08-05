module Pawl.Types.TriggerCondition where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 603.2: the pattern that fires a triggered ability. Only Pawl.Engine.Event may case
-- on it for RULES purposes; Pawl.Codec also cases on every constructor, but only
-- as the JSON data boundary, not to decide game behaviour.
data TriggerCondition
  = -- | CR 603.6a: "when this ... enters" -- fires when the object BEARING the
    -- ability enters. Self-scoped: the scan checks every permanent, so the
    -- bearer's identity is part of the match. PermanentEnters below is the other
    -- half of that same sentence, kept SEPARATE rather than written as
    -- `PermanentEnters IsSource`: this arm is a bare comparison of ids, while that
    -- one has to READ the entrant's characteristics, and reading them can come up
    -- empty for an entrant that ceased without a zone change ever filing last
    -- known information (see Pawl.Engine.Event.matchesTrigger).
    SelfEnters
  | -- | CR 603.6a's SECOND written form -- "whenever a [type] enters" -- fires when
    -- ANY permanent the Filter admits enters, whoever bears the ability. The
    -- bearer is the Filter.Context's source and its controller the perspective.
    --
    -- Named for the rule, NOT "OtherEnters": the bearer is not excluded here,
    -- since CR 603.6a checks the newcomers too. Soul Warden's "whenever ANOTHER
    -- creature enters" writes that exclusion into the Filter as `Not IsSource`
    -- (#163) rather than into a parallel exclusion flag.
    PermanentEnters (Filter.Filter Keyword.Keyword)
  | -- | CR 603.2b: "at the beginning of [each|your] <step>". Matched against a
    -- GameEvent.StepBegan; the TurnScope decides whose turn qualifies.
    StepBegins Phase.Phase TurnScope.TurnScope
  | -- | CR 603.8: a STATE trigger -- it fires whenever its condition is true, not
    -- when an event occurs, and not again until the ability has left the stack.
    -- That is why Pawl.Engine.Event derives armedness from the stack rather than
    -- storing it.
    StateIs Condition.Condition
  | -- | CR 603.2 / 509-510: the bearer dealt combat damage to a player, read off
    -- the DamageDealt event history.
    SelfDealsCombatDamageToPlayer
  | -- | CR 725.2: a creature dealt combat damage to the monarch. NOT bearer-scoped
    -- (any creature); matched only via Pawl.Engine.Monarch.inherentMatch, never through a
    -- card's bearer.
    CreatureDealtCombatDamageToMonarch
  | -- | CR 702.179d: "whenever one or more opponents lose life during your turn".
    -- The inherent speed-increase ability's event, and CreatureDealtCombatDamageToMonarch's
    -- sibling in every respect -- borne by no card, matched only via
    -- Pawl.Engine.Speed.inherentPending, never through a card's bearer, which is
    -- why Pawl.Engine.Event.matchesTrigger answers False for it.
    --
    -- Neither of the rule's other two clauses is here. "If your speed is less than
    -- 4" is CR 603.4's intervening "if", which rides
    -- Pawl.Types.TriggeredAbility.intervening; "this ability triggers only once
    -- each turn" is a limit on the ABILITY rather than a description of the event,
    -- which GameState.speedIncreasedThisTurn carries. Folding either in would make
    -- this constructor mean one ability instead of one event.
    OpponentLostLifeDuringYourTurn
  | -- | CR 702.29c: "when you cycle this card". Self-scoped like SelfEnters. The
    -- bearer is the card in the zone it landed in, which is that rule's second
    -- sentence -- the graveyard, for every printing today.
    SelfCycled
  | -- | CR 701.9a: "whenever [a player] discards a card" -- Megrim's. Matched
    -- against GameEvent.Discarded, whose PlayerId is the discarding player; the
    -- PlayerRelation reads that player against CR 109.5's "you", the ability's
    -- controller (CR 603.3a). NOT self-scoped, unlike SelfCycled above: the bearer
    -- is a bystander watching someone else's hand. Bartered Cow's "when you
    -- discard this card" is the self-scoped sibling, and does not exist yet
    -- (#319).
    --
    -- The DiscardCause is deliberately not part of this condition. CR 702.29a
    -- makes cycling a discard, so a cycled card must fire this; CR 702.29d bounds
    -- it to once, which the single Discarded event supplies by construction.
    PlayerDiscards PlayerRelation.PlayerRelation
  | -- | CR 508.3a: "whenever [a creature] attacks" -- Hanweir Garrison's.
    -- Self-scoped like SelfEnters.
    --
    -- DECLARED is the whole content of the condition, not a synonym for
    -- "attacking": CR 508.3a exempts a creature put onto the battlefield
    -- attacking, and CR 508.4 says such a creature never attacked. So this matches
    -- GameEvent.AttackerDeclared, which only the declaration appends, and never
    -- the combat record. No attack TARGET is carried; CR 508.3a's second sentence
    -- is a different condition no card in the pool has.
    --
    -- The TriggerFrequency is Aurelia, the Warleader's "for the first time each
    -- turn" -- a payload rather than a sibling condition, because it narrows this
    -- same trigger event rather than naming a different one.
    SelfAttacks TriggerFrequency.TriggerFrequency
  | -- | CR 603.6 (a zone-change trigger): "when this card is put into your
    -- graveyard from your library" -- Narcomoeba's. Self-scoped like SelfEnters.
    --
    -- The bearer is the card AS IT NOW IS IN THE GRAVEYARD -- CR 400.7's new
    -- incarnation, which CR 400.7e lets the ability find since a graveyard is
    -- public (CR 400.2) -- because CR 113.6k puts the ability wherever it can
    -- trigger from, and a card cannot be put into a graveyard from a library while
    -- it is on the battlefield.
    --
    -- No zone pair is carried, so this is not a general "moved from A to B": the
    -- two zones are exactly what makes CR 113.6k apply, and a second card wanting
    -- a different pair is the one that must generalise this.
    -- SelfPutIntoGraveyardFromAnywhere below is a card wanting NO origin zone,
    -- and is a sibling rather than that generalisation: Narcomoeba's printed
    -- sentence names a library and must stay silent for a discard.
    --
    -- The printed "your ... your" needs no controller check. A card is always put
    -- into its OWNER's graveyard from its OWNER's library, and a card in a
    -- graveyard has no controller (CR 108.4), so CR 113.8 makes the ability's
    -- controller that same owner.
    SelfPutIntoGraveyardFromLibrary
  | -- | CR 603.6: "when this card is put into a graveyard from anywhere" -- Serra
    -- Avatar's. Self-scoped like SelfEnters, and the WIDEST of the three
    -- put-into-a-graveyard conditions: any zone at all is a legal origin, so a
    -- discard, a mill, a countered spell and a death all fire it.
    --
    -- NOT a leaves-the-battlefield ability, which is CR 603.6c's own last
    -- sentence saying so in as many words, and the reason this is a sibling of
    -- SelfDies rather than its generalisation. Two consequences follow, and both
    -- are what make the two constructors behave differently rather than one being
    -- a wider Filter over the other:
    --
    --   * no CR 603.10a look-back. That rule's list of exceptions covers
    --     leaves-the-battlefield abilities and this is not one, so CR 603.10's
    --     normal reading applies: the bearer is the object as it exists
    --     immediately AFTER the event, which is the CR 400.7 incarnation in the
    --     graveyard. SelfDies reads the departing incarnation instead.
    --   * CR 113.6k puts the ability in the graveyard. Since the trigger never
    --     fires with the bearer on the battlefield, the condition cannot trigger
    --     from there, so it functions in every zone it can trigger from -- which
    --     is what lets a Serra Avatar discarded out of a hand trigger at all.
    --
    -- SUPERSET of SelfPutIntoGraveyardFromLibrary above in what it matches, and
    -- still a separate constructor: Narcomoeba's printed sentence names one
    -- origin zone and must stay silent for a discard, so collapsing the two would
    -- be a rules divergence rather than a refactor.
    SelfPutIntoGraveyardFromAnywhere
  | -- | CR 603.6c, narrowed to the one written form Doomed Traveler prints -- CR
    -- 700.4's "dies", the battlefield-to-graveyard pair. NOT the whole of CR
    -- 603.6c, whose first clause reaches any zone at all; that is
    -- SelfLeavesTheBattlefield below, since a card saying "leaves the
    -- battlefield" must not be conflated with one saying "dies". Self-scoped like
    -- SelfEnters.
    --
    -- The bearer is the incarnation that was ON THE BATTLEFIELD -- the id
    -- ZoneChange.departed carries and GameState.lastKnown files under -- and NOT
    -- the CR 400.7 incarnation that arrived. That is CR 603.10a read through CR
    -- 603.10's own definition of looking back, and both halves live in that one
    -- last-known record: the ability exists because the pre-move projection says
    -- so, and CR 603.3a's controller is whoever controlled the permanent as it
    -- left. The arriving incarnation is not lost -- CR 400.7e lets the ability
    -- find it, and Pawl.Engine.Event.eventBindings binds that id under
    -- Pawl.Engine.Binding.became -- so the bearer and the object the payload acts
    -- on are deliberately two different ids.
    --
    -- The contrast with SelfPutIntoGraveyardFromLibrary above is exact: that one
    -- is library to graveyard, functions IN the graveyard (CR 113.6k), and reads
    -- the ARRIVING incarnation; this one is battlefield to graveyard, functions on
    -- the battlefield, and reads the DEPARTING one.
    SelfDies
  | -- | The SAME written form as SelfDies above, read by a BYSTANDER rather than by
    -- the permanent that died. Meren of Clan Nel Toth's "whenever another creature
    -- you control dies".
    --
    -- Two constructors for exactly PermanentEnters' and SelfEnters' reason: that
    -- one is a bare comparison of ids, while this one has to READ the dead
    -- permanent's characteristics, and reading them can come up empty for a
    -- permanent that ceased without last known information ever being filed.
    -- Nothing about the bearer is part of the match; the bearer is the
    -- Filter.Context's source, and its controller is CR 109.5's "you". Named for
    -- the rule, NOT "AnotherPermanentDies": "another" is
    -- Filter.Not Filter.IsSource inside the Filter (#163), and a card watching
    -- EVERY creature die is the same constructor with a wider Filter.
    --
    -- The candidate is ZoneChange.departed and NOT ZoneChange.object -- the
    -- permanent as it was on the battlefield, read from CR 608.2h last known
    -- information (CR 603.10a again). That is what makes "you control" answerable
    -- CORRECTLY: by the CR 117.5 boundary the candidate is a card in a graveyard,
    -- CR 108.4 gives it no controller, and CR 108.4a would hand back its OWNER --
    -- a different player for anything its controller had stolen.
    --
    -- Nothing about the DEAD permanent is bound for the payload to name, so a card
    -- saying "return that creature card to your hand" is not expressible through
    -- this condition (#616).
    PermanentDies (Filter.Filter Keyword.Keyword)
  | -- | CR 603.6c's FIRST written form -- "when [this object] leaves the
    -- battlefield" -- taken whole: ANY other zone, so an exile, a bounce and a
    -- shuffle into a library all fire it. Thragtusk's.
    --
    -- SIBLING of SelfDies above, never its superset in code even though it is one
    -- in the rules. The two written forms are different printed sentences, and CR
    -- 700.4 narrows the second to a graveyard, so a Doomed Traveler must stay
    -- silent for exactly the bounce that fires a Thragtusk. Self-scoped and a
    -- LOOK-BACK for SelfDies' reasons (CR 603.10a): the bearer is
    -- ZoneChange.departed, and CR 603.3a's controller is whoever controlled it
    -- then.
    --
    -- Where it DIVERGES from SelfDies is CR 400.7e, whose rescue of the arriving
    -- object holds only for a public destination -- satisfied by construction for
    -- a graveyard (CR 400.2), but a real test here, since a hand or a library is
    -- hidden. Pawl.Engine.Event.eventBindings binds Pawl.Engine.Binding.became only
    -- for a public destination, and nothing at all for a hidden one.
    --
    -- CR 603.6c's SECOND trigger event, a phased-in permanent leaving the game
    -- with its owner, is not matched (#385).
    SelfLeavesTheBattlefield
  | -- | CR 701.6a: "whenever a spell or ability you control counters a spell" --
    -- Baral, Chief of Compliance's. Matched against GameEvent.SpellCountered,
    -- whose Countering carries the controller of whatever DID the countering; the
    -- PlayerRelation reads that player against CR 109.5's "you", the ability's
    -- controller (CR 603.3a). NOT self-scoped -- PlayerDiscards' shape, not a
    -- Self- condition's -- since Baral is a creature watching an instant resolve.
    --
    -- The relation is on the COUNTERING side, never on the countered spell's: the
    -- printed sentence has one "you" and it modifies "a spell or ability", so
    -- Baral triggers on countering its own controller's spell just as readily.
    --
    -- Only a countered SPELL fires this, which is the printed word rather than an
    -- omission. Stifle counters an ABILITY and Baral stays silent -- CR 113.9 says
    -- an ability on the stack is not a spell, so Pawl.Engine.Event.counter ceases
    -- it (CR 608.2n) and records no event to match. The countered-ability sibling
    -- of that record does not exist, and no card in the pool wants one (#541).
    SpellOrAbilityCounters PlayerRelation.PlayerRelation
  | -- | CR 615.13: "whenever damage that would be dealt to you is prevented" --
    -- Selfless Squire's. Matched against GameEvent.DamagePrevented, whose
    -- Recipient is whom the prevented damage was addressed to; the PlayerRelation
    -- reads that player against CR 109.5's "you", the ability's controller (CR
    -- 603.3a). PlayerDiscards' shape, not a Self- condition's: the bearer is a
    -- creature watching damage addressed to its controller.
    --
    -- Scoped to a PLAYER recipient rather than any recipient, which is the
    -- printed sentence rather than an omission -- CR 615.13 itself says nothing
    -- about who the damage was addressed to, so a card watching damage to a
    -- CREATURE being prevented would be a different condition, and no card in the
    -- pool is one. It fires for damage prevented to a player and stays silent for
    -- damage prevented to a permanent.
    --
    -- NOT linked to the prevention effect that fired it. Selfless Squire's own
    -- ruling is explicit that any prevention at all triggers it, including one
    -- its own first ability had nothing to do with; a card printing "prevented
    -- this way" is what must add the link (#687).
    DamageToPlayerPrevented PlayerRelation.PlayerRelation
  deriving (Eq, Ord, Show)
