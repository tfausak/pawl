{-# LANGUAGE GADTs #-}

module Pawl.Types.Prompt where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Types.Action (Action)
import Pawl.Types.Color (Color)
import Pawl.Types.Concession (Concession)
import Pawl.Types.Cost (Cost)
import Pawl.Types.Decider (Decider)
import Pawl.Types.EntryOption (EntryOption)
import Pawl.Types.EntwineDecision (EntwineDecision)
import Pawl.Types.Keyword (Keyword)
import Pawl.Types.Mana (Mana)
import Pawl.Types.ModeIndex (ModeIndex)
import Pawl.Types.MulliganDecision (MulliganDecision)
import Pawl.Types.MulliganOffer (MulliganOffer)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.OptionalDecision (OptionalDecision)
import Pawl.Types.PhyrexianPayment (PhyrexianPayment)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.Recipient (Recipient)
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.Subtype (Subtype)
import Pawl.Types.TriggerSource (TriggerSource)

data Prompt r where
  ChooseAction :: Decider -> PlayerId -> [Action] -> Prompt Action
  -- CR 104.3a: "A player can concede the game at any time. A player who concedes
  -- leaves the game immediately." Asked before ChooseAction wherever a player
  -- would receive priority.
  --
  -- This is the only CHOICE prompt that carries no Decider, and that asymmetry
  -- IS the CR 723.6 mechanism: "The controller of another player can't make that
  -- player concede. A player may concede the game at any time, even if they are
  -- controlled by another player." So the ask must reach the true player, and
  -- there must be nowhere to put a controller. Folding concede into ChooseAction
  -- as an Action constructor -- the obvious way to save a prompt -- would hand
  -- the controller exactly the power CR 723.6 forbids and leave the controlled
  -- player with no channel at all. (Shuffle and RandomFirstPlayer carry no
  -- Decider either, for the unrelated reason that randomness is not a choice.)
  --
  -- "At any time" is narrowed to "at each priority grant", and while a CR 729
  -- subgame is running this asks about the SUBGAME rather than the main game
  -- (#144, #397). Pawl.GameSpec's "conceding before the settle window and after
  -- it name different winners" pins what the first narrowing costs.
  Concede :: PlayerId -> Prompt Concession
  Shuffle :: [ObjectId] -> Prompt [ObjectId]
  -- CR 729.2: "Randomly determine which player goes first." The NonEmpty is the
  -- turn order; the answer is the starting player, and the order is then rotated
  -- to begin with them (CR 103.1: "the game's default turn order begins with the
  -- starting player and proceeds clockwise").
  --
  -- Carries no Decider, and neither does Shuffle: this is randomness, not a
  -- choice, so there is no player whose decision could be usurped and nowhere for
  -- a controller (CR 723) to sit. Concede is the only other constructor without
  -- one, for the opposite reason -- it IS a choice, and CR 723.6 bars the
  -- controller specifically from making it. Asked only where the rules call for
  -- randomness -- a subgame's start. A MAIN game's starting player is settled
  -- before the game begins, by any mutually agreeable method (CR 103.1), which is
  -- the caller-supplied turn order Setup.emptyGame takes.
  --
  -- NonEmpty, not [], for the same reason Setup.emptyGame takes one: the answer
  -- has to come from somewhere, and a fallback must be total.
  RandomFirstPlayer :: NonEmpty PlayerId -> Prompt PlayerId
  -- CR 514.2. The [ObjectId] is the hand; the Natural is how many to discard.
  ChooseDiscard :: Decider -> PlayerId -> [ObjectId] -> Natural -> Prompt [ObjectId]
  -- CR 507.1 / 703.4h: immediately after the beginning of combat step begins, the
  -- active player chooses one of their opponents, and that player becomes the
  -- defending player. The NonEmpty is the candidates (Combat.attackableOpponents);
  -- the answer is the one chosen.
  --
  -- NonEmpty, not [], for the same reason Setup.emptyGame and RandomFirstPlayer
  -- take one: the answer has to come from somewhere and a fallback must be total.
  -- No opponents at all cannot arise while the game is running -- the last one
  -- leaving ends it (CR 104.2a) -- and is handled by not performing the action,
  -- which leaves Combat.defender at Nothing and therefore no attack possible --
  -- the same wording Combat.declareAttackers' own comment uses, and deliberately
  -- not a claim about legality: Combat.canAttack never reads this field, so the
  -- creatures remain legal attackers (CR 508.1a) and what does not happen is the
  -- CR 508.1 declaration.
  --
  -- Not asked when there is exactly one candidate (#169). CR 507.1 makes the
  -- choice exist only in a multiplayer game; a two-player game gets its defending
  -- player from CR 506.2's second sentence with nothing to ask.
  ChooseDefender :: Decider -> PlayerId -> NonEmpty PlayerId -> Prompt PlayerId
  -- CR 601.2g (and CR 602.2b for an ability): which mana source to activate next
  -- while paying a cost.
  --
  -- Asked once per source TAPPED, against a shrinking candidate list, which is
  -- not once per mana the cost demands: a Sol Ring pays {2} in one activation and
  -- so raises this once.
  --
  -- Elided only when there is exactly ONE candidate, where no choice exists.
  -- Same-card candidates are NOT treated as interchangeable: two Llanowar Elves
  -- can differ by an Equipment, an Aura, counters or borrowed control, and none of
  -- that is visible in the printed card (#12, #217).
  ChooseManaSource :: Decider -> PlayerId -> NonEmpty ObjectId -> Prompt ObjectId
  -- CR 605.3b: which mana the source (the ObjectId) produces, asked as the mana
  -- ability resolves -- immediately, since a mana ability never uses the stack.
  -- The answer is a YIELD, the whole mana one activation adds, so Sol Ring's
  -- "{T}: Add {C}{C}" is one candidate of two units rather than two candidates of
  -- one. Three separate things reach this one prompt, because they are
  -- observationally the same question:
  --
  --   * one AddMana effect offering a choice -- Birds of Paradise's "add one mana
  --     of any color", whose five options are CR 105.4's five colours;
  --   * a permanent with SEVERAL single-type mana abilities -- an Urborg'd
  --     Mountain (CR 305.6/305.7) is both a Mountain and a Swamp, so its
  --     controller picks which of its two intrinsic abilities to activate; and
  --   * a mana ability with several MODES (CR 700.2), where the mode picks the
  --     yield. No card in the pool has one.
  --
  -- Collapsing them is sound because a source taps once and adds one yield, so
  -- "which ability", "which mode" and "which colours" have the same answer set
  -- and the same consequences. It would stop being sound if two abilities of one
  -- permanent differed in cost or in a rider (City of Brass' damage) -- but
  -- Mana.tapForMana reads neither today, so nothing observable is being lost
  -- here that is not already gone (#238).
  --
  -- The candidates are deduplicated by the WHOLE yield, which is the one elision
  -- needing no judgement: two ways to produce black mana add the same mana either
  -- way.
  ChooseManaYield :: Decider -> PlayerId -> ObjectId -> NonEmpty Mana -> Prompt Mana
  -- CR 701.34a: which permanents and players a proliferating player gives another
  -- counter to. The [ObjectId] is every permanent holding at least one counter and
  -- the [PlayerId] every player holding at least one; the answer is the subset of
  -- each that gets one more of every kind it already has.
  --
  -- CHOOSE, not target. Proliferate declares no target spec, so a candidate is
  -- offered no matter whose it is -- an opponent's -1/-1'd creature sits on the
  -- same list as your own +1/+1'd one, and picking between them is the whole
  -- decision. Nothing here is re-checked for legality at resolution (CR 608.2b),
  -- because nothing was ever targeted.
  --
  -- Asked whenever EITHER list is non-empty, and elided only when both are empty.
  -- Deliberately NOT elided for a single candidate, unlike ChooseManaSource: "any
  -- number" (CR 701.34a) includes none, so even one candidate is a real yes or no
  -- -- proliferating a -1/-1 counter onto your own creature is a choice a player
  -- may well decline.
  ChooseProliferate :: Decider -> PlayerId -> [ObjectId] -> [PlayerId] -> Prompt (Set ObjectId, Set PlayerId)
  -- CR 704.5j: which of two or more same-named legendary permanents its
  -- controller keeps. The NonEmpty is the whole same-named group; the answer is
  -- the ONE that survives, and every other member is put into its owner's
  -- graveyard.
  --
  -- One prompt per name, not one per player: a player controlling two Thalias and
  -- two Urborgs faces two separate applications of the legend rule, and each is
  -- its own decision.
  --
  -- The answer is what is KEPT rather than what is buried, because CR 704.5j is
  -- worded that way ("that player chooses one of them, and the rest are put
  -- into..."), and because it stays a single choice however large the group gets.
  --
  -- Never elided: the prompt is only raised for a group of two or more, which is
  -- always a real choice. The permanents may differ in counters, Auras, damage or
  -- summoning sickness, none of which the shared name can see -- the same reason
  -- ChooseManaSource refuses to treat same-card candidates as interchangeable.
  ChooseLegend :: Decider -> PlayerId -> NonEmpty ObjectId -> Prompt ObjectId
  -- CR 508.1. The [ObjectId] is the legal attackers; the answer is which of them
  -- attack. WHOM they attack is not asked here: the defending player was already
  -- chosen at the beginning of combat step (Prompt.ChooseDefender), and CR 508.1b
  -- calls for a per-creature announcement only if that player controls a
  -- planeswalker, protects a battle, or the game lets the active player attack
  -- multiple other players. A defending player can control a planeswalker now
  -- (Jace Beleren), but AttackTarget has no arm to name one (#493, #59).
  DeclareAttackers :: Decider -> PlayerId -> [ObjectId] -> Prompt [ObjectId]
  -- CR 509.1. The legal blockers, then the attackers they may block. The answer
  -- maps each blocking creature to the attacker it blocks.
  DeclareBlockers :: Decider -> PlayerId -> [ObjectId] -> [ObjectId] -> Prompt (Map ObjectId ObjectId)
  -- CR 510.1 / 702.19b: the attacker divides its power among the legal recipients.
  -- The Map is recipient -> lethal threshold (blockers -> lethal, the defender ->
  -- 0); trample-ness is entirely in whether the defender is a key and what the
  -- thresholds are. Not asked when the division is forced (single blocker, no
  -- excess). Validation is Damage.legalAssignment. See the M2c spec, section 4.
  AssignCombatDamage :: Decider -> PlayerId -> ObjectId -> Map Recipient Natural -> Natural -> Prompt (Map Recipient Natural)
  -- CR 601.2c. One legal-recipient set per named slot of the spell being cast
  -- (the ObjectId); the answer fills every slot. Slots agree by NAME, never by
  -- position. Not asked when the spell has no slots: zero slots is no choice
  -- at all, and where the rules leave nothing to ask, don't prompt.
  ChooseTargets :: Decider -> PlayerId -> ObjectId -> Map SlotName (Set Recipient) -> Prompt (Map SlotName Recipient)
  -- CR 612 / the D4 binding: choose the two basic land types for a text-changing
  -- spell's slot (Magical Hack: "one basic land type" -> "another"). Bound at cast
  -- alongside ChooseTargets; the legal set is always the five basics, so unlike a
  -- target it never gates castability, which is what makes cast-vs-resolution
  -- timing indistinguishable here (#60).
  ChooseBasicLandTypes :: Decider -> PlayerId -> ObjectId -> SlotName -> Prompt (Subtype, Subtype)
  -- CR 701.23 / 701.23b: the [ObjectId] is the library cards MATCHING the
  -- criterion (the engine pre-filters to legal choices); Nothing is "fail to
  -- find," always permitted for a search of one's own library for a quality.
  SearchLibrary :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- CR 608.2g: the re-entrant cast opportunity during a library search (Panglacial
  -- Wurm) -- an effect that "specifically instructs or allows a player to cast a
  -- spell during resolution", following CR 601.2a-i except that no player receives
  -- priority after it is cast. The [ObjectId] is the searcher's library cards
  -- castable-while-searching (the engine pre-filters to permitted, affordable,
  -- fillable). Nothing = decline / done. Offered in a loop before the search finds
  -- (per the ruling), so multiple copies may be cast. CR 605.3a permits mana
  -- activation to pay.
  CastWhileSearching :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- CR 601.2b: choose the value of X while casting (the ObjectId is the spell).
  -- The Natural is the greatest value this player could actually PAY for right
  -- now: the largest X at which the cost being cast, totalled at CR 601.2f, is
  -- still payable (Cast.affordableX, which climbs the very predicate
  -- Cast.payableCost gated this cast on at CR 601.2b's X=0 floor).
  --
  -- ADVISORY, not a limit, and emphatically not a clamp. The answer is filtered
  -- against it nowhere: CR 601.2b lets the player announce the value of the
  -- variable freely, and an announcement the total cost cannot pay is answered by
  -- CR 601.2 reversing the whole casting -- "the game returns to the moment
  -- before the casting of that spell was proposed" -- which is pawl's no-op,
  -- minus the prompts (#56). What the bound adds is the INFORMATION a player at a
  -- table has and an answerer, which sees only this payload and never the
  -- GameState, did not (#417) -- the shape #176 gave DeclareMulligan.
  --
  -- COUNTS LIFE, not only mana: Cost.canPay measures CR 601.2b's nonhybrid
  -- resolutions, so a Phyrexian symbol's 2 life (CR 107.4f) is one of the routes
  -- the climb can find, and the bound is the greatest X payable by ANY route the
  -- cast itself would accept. It inherits that predicate's caveats with its
  -- reading -- notably #365, where the mana part and a PayLife component are
  -- measured separately. It is measured BEFORE this cost's Phyrexian symbols are
  -- announced, which is CR 601.2b's own order (X precedes the Phyrexian
  -- announcement), so the routes it counts are exactly the ones still open to the
  -- player being asked.
  --
  -- A bare Natural rather than a Maybe: this prompt is issued only for a
  -- candidate cost that already passed the X=0 floor, so a greatest payable X
  -- always exists, and 0 is a real answer (cast the spell for its X-free
  -- remainder) rather than an absent one. There is no "unbounded" case to
  -- represent -- a player's mana is finite, and every {X} spends it.
  --
  -- Prompted before targets (CR 601.2b precedes 601.2c), and only when the cost
  -- contains a Variable symbol -- a spell with no {X} is not asked (where the
  -- rules leave nothing to choose, don't prompt).
  ChooseX :: Decider -> PlayerId -> ObjectId -> Natural -> Prompt Natural
  -- CR 702.42a: whether this player uses the entwine ability of the modal spell
  -- they are casting -- "You may choose all modes of this spell instead of just
  -- the number specified. If you do, you pay an additional [cost]." The ObjectId
  -- is the spell; the Cost is the ADDITIONAL cost entwining would add on top of
  -- whichever candidate cost is then announced (CR 601.2f).
  --
  -- ONE question rather than two, because rule 702.42a is one sentence: the
  -- widened mode choice (CR 601.2b's first clause) and the intention to pay an
  -- additional cost (CR 601.2b's third) are the same decision, so answering
  -- Entwines settles both. It is therefore asked BEFORE ChooseModes, which for
  -- an entwined cast then has nothing left to ask.
  --
  -- Asked ONLY when entwining is actually available: the spell has the keyword,
  -- every one of its modes is legal (CR 700.2a -- "choose all modes" is not open
  -- when one of them can't be chosen), and some candidate cost plus this one is
  -- payable. Where the rules leave nothing to ask, don't prompt; where they do,
  -- the engine never decides to pay on the player's behalf.
  ChooseEntwine :: Decider -> PlayerId -> ObjectId -> Cost Keyword -> Prompt EntwineDecision
  -- CR 601.2b / 700.2a: choose the mode(s) while casting (the ObjectId is the
  -- spell). The Set ModeIndex is the LEGAL modes -- the engine pre-filters to modes
  -- whose targets are all fillable (CR 700.2a). The Natural is how many to choose.
  -- The answer is the chosen subset. Prompted before X and targets, and ONLY when
  -- #legal > count; a forced selection is not asked -- which is every entwined
  -- cast, where CR 702.42a's "all modes" leaves no subset to pick.
  ChooseModes :: Decider -> PlayerId -> ObjectId -> Set ModeIndex -> Natural -> Prompt (Set ModeIndex)
  -- CR 707.5 / 614.1c / 614.12a: as an object enters AS A COPY (Clone), its
  -- controller chooses which permanent to copy. The ObjectId is the entering
  -- object; the [ObjectId] is the legal copy targets (battlefield creatures other
  -- than itself; the engine pre-filters). Nothing is the "may" decline (Clone's
  -- own wording, not a rule; it then enters as itself, a 0/0). Answered inside the zone change that puts
  -- the object onto the battlefield (CR 614.12a), before the enters event is
  -- recorded -- the choice really is made as the object enters. The legal set
  -- excludes anything entering in the same batch (CR 614.12a: a sibling
  -- entering at the same time is not yet "on the battlefield" when the choice
  -- is made; see Pawl.Engine.Replacement's applyReplacementsIn).
  ChooseCopyTarget :: Decider -> PlayerId -> ObjectId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- CR 208.2b / 614.1c: as an object enters, its controller chooses among the
  -- shapes an "as this creature enters, it becomes your choice of ..." ability
  -- offers (Primal Plasma). The ObjectId is the entering object; the answer is an
  -- index into the offered list.
  --
  -- The chosen shape is written into the object's COPIABLE snapshot (CR 707.2), so
  -- a later Clone copies the choice without any further machinery -- and then, if
  -- it copied the ABILITY too, makes its own choice on top (CR 616.2).
  --
  -- Asked only when two or more options are offered; one option is not a choice.
  ChooseEntryOption :: Decider -> PlayerId -> ObjectId -> [EntryOption] -> Prompt Natural
  -- CR 603.3b: "each player, in APNAP order, puts each triggered ability they
  -- control ... on the stack in any order they choose." The [TriggerSource]
  -- is that player's pending triggers, each entry naming what the ability hangs
  -- on, in the engine's canonical order; the answer is a permutation of the entry
  -- INDICES, giving the order they are PUT ON THE STACK (so the last named
  -- resolves first).
  --
  -- TriggerSource rather than ObjectId because CR 725.2's inherent monarch
  -- abilities "have no source" and are ordinary triggered abilities in every
  -- other respect, so they are in this batch with everything else and have no id
  -- to put on the wire. An interpreter reading TriggerSource.Sourceless knows it
  -- is looking at one of those two abilities rather than at a missing object.
  --
  -- Positional by necessity, unlike a target slot: each entry carries only its
  -- SOURCE, no ability discriminator. That is CONTINGENT, not a rules property:
  -- it holds only while no single source can have two DISTINCT abilities
  -- triggered in the same batch (two triggers from the SAME ability on one
  -- source really are indistinguishable, and any permutation among those is
  -- equivalent). It holds today only as an accident of the settle schedule --
  -- Sarcomancy already carries two triggered abilities (an ETB and an upkeep
  -- trigger), but they cannot co-trigger because the step event that would fire
  -- the upkeep trigger is always scanned before any spell can resolve to place
  -- Sarcomancy and fire its ETB in the same batch. A source with two distinct
  -- abilities triggered together makes two different abilities identical entries
  -- on the wire while their order genuinely matters, and the payload would need
  -- an ability discriminator alongside the source (#61). Sourceless is that same
  -- gap in a second place: CR 725.2's two abilities share one controller and one
  -- (absent) source, so two Sourceless entries would be indistinguishable too.
  -- Unreachable, because one triggers on a step beginning and the other on combat
  -- damage and a settle always separates the two events; the discriminator #61
  -- asks for would cover both.
  --
  -- Asked ONLY when the player controls two or more -- with one there is nothing
  -- to choose, and where the rules leave nothing to ask, don't prompt.
  --
  -- CR 603.3b's TWO-PART process (first the
  -- triggers whose condition is not another ability triggering, then the rest)
  -- is vacuous while no condition triggers on another ability triggering; this
  -- carries the note, not the machinery.
  OrderTriggers :: Decider -> PlayerId -> [TriggerSource] -> Prompt [Natural]
  -- CR 616.1: with two or more applicable replacement or prevention effects in
  -- the highest non-empty bucket, the affected object's controller (or its owner,
  -- or the affected player) chooses which to apply NEXT -- and then the process
  -- repeats over what is applicable now (616.1f), so this is asked once per
  -- iteration, not once per event. The [ObjectId] is each candidate's SOURCE, in
  -- the engine's canonical order (battlefield ascending, then the floating
  -- store); the answer is an index into it.
  --
  -- Positional, and carrying exactly the caveat #61 records for OrderTriggers: a
  -- source with two DISTINCT applicable replacement abilities would put two
  -- different effects on the wire as identical entries. That is reachable in a way
  -- it is not for triggers -- Doubling Season has two replacement abilities -- but
  -- they are in different EVENT CLASSES and so are never candidates for the same
  -- event. A single source with two same-class applicable replacements needs a
  -- discriminator alongside the source (#74).
  --
  -- Asked ONLY when the bucket holds two or more candidates that are not all equal
  -- as values: with one there is nothing to choose, and among equal values every
  -- order yields the same board (each still gets its own CR 614.5 opportunity).
  ChooseReplacement :: Decider -> PlayerId -> [ObjectId] -> Prompt Natural
  -- CR 603.7c: which of several minted tokens a Create's slot binds -- the "it"
  -- that a delayed triggered ability armed in the same resolution will name
  -- ("Sacrifice it at the beginning of the next end step"). The ObjectId is the
  -- resolving source holding the binding; the NonEmpty is the tokens that were
  -- actually minted, in creation order; the answer is the one bound.
  --
  -- Reachable only through a replacement. The Pawl.CardSpec lint rejects a card
  -- whose Create binds a slot while its PRINTED quantity is anything but exactly
  -- one -- greater, variable, or zero alike (#53), but CR
  -- 614.16 ("if an effect would create one or more tokens") scales the count at
  -- RUNTIME, long after that lint has passed: Doubling Season on Tidal Wave mints
  -- two Walls where CR 603.7c's "it" names one.
  --
  -- CR 707.10e is the codified analogue, and it is what settles this as a CHOICE
  -- rather than a rule the engine may apply itself: where a replacement effect
  -- causes a copy to target more than one object, "the copy's controller chooses
  -- one of them to be the new target." Its Frontline Heroism / Anointed
  -- Procession example is this exact shape -- two tokens are created, and "the
  -- copy targets one of those tokens of your choice. The copy doesn't target both
  -- the tokens." Binding the first minted token would be the engine choosing.
  --
  -- No SlotName in the payload: the candidates are distinct minted objects, so
  -- two slot-binding Creates in one resolution already ask distinguishable
  -- questions, and the slot name is card-data vocabulary rather than anything a
  -- player sees.
  --
  -- Asked ONLY when two or more tokens were minted. One token is the whole
  -- candidate set, and where the rules leave nothing to ask, don't prompt.
  ChooseBoundToken :: Decider -> PlayerId -> ObjectId -> NonEmpty ObjectId -> Prompt ObjectId
  -- CR 701.21a: which permanents to sacrifice to pay a cost. The ObjectId is the
  -- spell being cast or the permanent whose ability is being activated; the
  -- [ObjectId] is the payer's permanents matching the component's criterion (the
  -- engine pre-filters, in ascending order); the Natural is how many. The answer
  -- is a Set because one permanent cannot be sacrificed twice for one payment.
  --
  -- Deliberately NOT Prompt.ChooseTargets or the TargetSpec machinery: CR 115.1
  -- makes a target only what the word "target" names, and conflating the two
  -- would let shroud, hexproof and "becomes the target" triggers observe a
  -- sacrifice choice. Its shape mirrors ChooseDiscard (candidates plus a count).
  --
  -- Asked ONLY when there is a choice -- more candidates than the count. Exactly
  -- as many as the count is forced, and where the rules leave nothing to ask,
  -- don't prompt.
  --
  -- Issued once per component and carries no record of what an earlier component
  -- already consumed, so two Sacrifice components of one cost each see the full
  -- candidate list (#112).
  ChooseSacrifices :: Decider -> PlayerId -> ObjectId -> [ObjectId] -> Natural -> Prompt (Set ObjectId)
  -- CR 701.3a: where an effect that moves an already-attached permanent puts it.
  -- The first ObjectId is the permanent being moved (Crown of the Ages' targeted
  -- Aura); the NonEmpty is the destinations its card text admits.
  --
  -- CHOOSE, not target. Crown of the Ages' ruling is explicit -- "This only
  -- targets the Aura and not either creature" -- so a destination is offered no
  -- matter whose it is and no matter whether it can be targeted, and nothing here
  -- is re-checked under CR 608.2b. The offer is the card's TEXT and nothing more:
  -- Crown of the Ages says "another creature" and gets every creature, including
  -- ones CR 303.4j will then refuse to move the Aura onto, because narrowing past
  -- what the card says would answer that rule's question on the player's behalf.
  -- Aura Graft says "another permanent IT CAN ENCHANT" and gets only the legal
  -- ones, because that is what ITS text says (Filter.CanHostSubject).
  --
  -- Elided at exactly one candidate, the ChooseManaSource posture: the effect is
  -- mandatory ("Attach ... to another creature", no "may"), so a single
  -- destination leaves nothing to decide. The current host is never among the
  -- candidates (CR 701.3b's second sentence), so it is not an option being
  -- withheld.
  --
  -- NonEmpty, not []: the caller does not raise this at all when no destination
  -- exists, and a fallback must be total.
  ChooseAttachment :: Decider -> PlayerId -> ObjectId -> NonEmpty ObjectId -> Prompt ObjectId
  -- CR 601.2b: "If the spell has alternative or additional costs that will be
  -- paid as it's being cast ... the player announces their intentions to pay any
  -- or all of those costs." Issued after the modes and before X and targets, at
  -- 601.2b's own position. The ObjectId is the spell; the [Cost] is the PAYABLE
  -- candidates (the engine pre-filters: each candidate from Pawl.Engine.Cost.costsFor,
  -- run through total, then tested with canPay at the CR 601.2b X=0 floor).
  --
  -- CR 118.9b makes an alternative cost optional, so a player who can afford both
  -- is genuinely choosing. Asked ONLY when two or more candidates are payable;
  -- one is forced, and where the rules leave nothing to ask, don't prompt.
  ChooseCost :: Decider -> PlayerId -> ObjectId -> [Cost Keyword] -> Prompt (Cost Keyword)
  -- CR 103.5: whether this player takes a mulligan. The MulliganOffer carries
  -- both halves of what a player at a table can see -- how many mulligans they
  -- have already taken, and how many cards taking another would bottom. Those
  -- diverge under CR 103.5c's free first mulligan, which is why the payload is
  -- not the raw count it used to be (#176).
  --
  -- Asked in turn order, once per round, only while the player's current hand is
  -- > 0 cards (CR 103.5 final sentence: no mulligans past a zero-card hand). A
  -- player who has kept is never asked again (CR 103.5: keeping is terminal).
  -- Carries a Decider like every other player-facing prompt; at game setup
  -- activeControl is Nothing, so it is the player themselves (CR 723 satisfied
  -- for free).
  DeclareMulligan :: Decider -> PlayerId -> MulliganOffer -> Prompt MulliganDecision
  -- CR 103.5: after redrawing, put `count` cards from `hand` on the bottom of the
  -- library, in the player's chosen order. The [ObjectId] is the redrawn hand;
  -- the answer is an ordered list of exactly `count` of those ids (first-listed
  -- ends up higher in the library, drawn sooner). Bottom order IS future draw
  -- order, so it is a real choice even when the subset is forced (count == hand
  -- size), which is why the answer is an ordered list, never a Set. Asked only
  -- when the hand has >= 2 cards; with 0 or 1 there is one possible outcome, and
  -- where the rules leave nothing to ask, don't prompt.
  Bottom :: Decider -> PlayerId -> [ObjectId] -> Natural -> Prompt [ObjectId]
  -- CR 103.5b: "If an effect allows a player to perform an action 'any time
  -- [that player] could mulligan,' the player may perform that action at a time
  -- they would declare whether they will take a mulligan." The [ObjectId] is the
  -- cards in this player's hand that grant such an action; the answer is which
  -- one to use, or Nothing to decline.
  --
  -- Offered immediately BEFORE each DeclareMulligan, and again after each action
  -- taken -- CR 103.5b's last sentence ("If the player performs the action, they
  -- then declare whether they will take a mulligan") makes the declaration
  -- follow, and nothing in the rule limits a player to one action. Offered in
  -- EVERY round, not just the first ("This need not be in the first round of
  -- mulligans"), and only to a player who has not yet kept -- a player who kept
  -- never declares again, so there is no time at which they could act.
  --
  -- Performing the action is NOT taking a mulligan: nothing is shuffled or
  -- bottomed and the CR 103.5 mulligan count is untouched, so it feeds neither
  -- the bottom count nor CR 103.5c's free allowance.
  --
  -- Not asked when the list is empty; where the rules leave nothing to ask,
  -- don't prompt. Carries a Decider like every other player-facing prompt.
  MulliganAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- CR 103.6: an action a card in this player's opening hand lets them take once
  -- the mulligan process is complete -- "begin the game with it on the
  -- battlefield" (CR 103.6a). The [ObjectId] is the cards in hand offering one;
  -- the answer is which to take, or Nothing to decline.
  --
  -- Offered in turn order, starting player first (CR 103.6), and repeatedly to
  -- the same player until they decline: CR 103.6 lets a player take "any such
  -- actions in any order", so both which and how many are theirs to choose.
  --
  -- A SEPARATE channel from MulliganAction, not a reuse of it: that window sits
  -- AT a mulligan declaration and this one opens once the whole process is
  -- complete, so an interpreter that could not tell them apart could not answer
  -- either well. Not asked when the list is empty; where the rules leave nothing
  -- to ask, don't prompt.
  OpeningHandAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
  -- CR 603.5 / 608.2d: whether the controller of a resolving spell or ability
  -- exercises a printed "may". The ObjectId is the object RESOLVING (the spell,
  -- or the ability object on the stack -- not its source, since two triggers off
  -- one source resolve as two distinct stack objects); the ModeIndex is which of
  -- its chosen modes is asking, so a modal payload with two optional modes puts
  -- two DISTINGUISHABLE questions on the wire -- the discriminator OrderTriggers
  -- (#61) and ChooseReplacement (#74) had to do without.
  --
  -- CR 603.5 is what makes this a resolution-time prompt rather than a
  -- cast-time one: an optional ability "goes on the stack when it triggers,
  -- regardless of whether their controller intends to exercise the ability's
  -- option or not. The choice is made when the ability resolves." CR 608.2d then
  -- places it exactly -- a choice not already made as part of putting the spell
  -- or ability on the stack is announced "while applying the effect".
  --
  -- NEVER elided. "You may gain 2 life" is a genuine choice even when it looks
  -- strictly good: a life total is read by other cards, so both answers are
  -- distinguishable game states, and the engine does not make a player's choice.
  -- The one case where it is not asked is where nothing is being decided: an
  -- ability whose targets are ALL illegal never reaches this prompt, because CR
  -- 608.2b removes it from the stack first (Pawl.Engine.Resolve's fizzle). That covers
  -- every optional card in the pool, all of which are single-mode; a MODAL
  -- payload mixing a live mode with a dead optional one would reach this prompt
  -- with nothing to decide (#336).
  ChooseOptional :: Decider -> PlayerId -> ObjectId -> ModeIndex -> Prompt OptionalDecision
  -- CR 601.2b: "If a cost that will be paid as the spell is being cast includes
  -- Phyrexian mana symbols, the player announces whether they intend to pay 2
  -- life or a corresponding colored mana cost for each of those symbols." CR
  -- 118.13a is the rule that puts the choice HERE rather than at payment: it "is
  -- made as its controller proposes that spell or ability". The ObjectId is the
  -- spell being cast or the permanent whose ability is being activated (CR
  -- 602.2b sends an activation through the same rule); the Color is the symbol's
  -- own, so a {W/P} and a {G/P} in one cost put DISTINGUISHABLE questions on the
  -- wire.
  --
  -- ONE PROMPT PER SYMBOL, in printed order, and the NonEmpty is the routes that
  -- are actually payable given the announcements already made -- so a player can
  -- never announce a route CR 118.3 will not let them complete. Payable is
  -- measured at CR 601.2f's TOTAL and not at the printed cost, which is what keeps
  -- that promise in the presence of a cost increase or reduction (ManaSpec's
  -- "CR 601.2f a reduction opens the coloured-mana route, so the announcement is
  -- asked"). Two symbols of the SAME colour ask two identical questions, which is
  -- sound where it would not be for OrderTriggers (#61): the answers are
  -- interchangeable, since both symbols demand the same mana and the same 2 life,
  -- so the pair of answers is all that is observable and which prompt got which is
  -- not. Dismember ({1}{B/P}{B/P}) is the card that asks them.
  --
  -- Elided when only one route is payable, where no choice exists -- no source of
  -- the symbol's colour at all, or a life total below CR 119.4's floor.
  AnnouncePhyrexianPayment :: Decider -> PlayerId -> ObjectId -> Color -> NonEmpty PhyrexianPayment -> Prompt PhyrexianPayment
