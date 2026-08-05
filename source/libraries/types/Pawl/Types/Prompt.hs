{-# LANGUAGE GADTs #-}

module Pawl.Types.Prompt where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.HybridPayment as HybridPayment
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggerEntry as TriggerEntry

data Prompt r where
  ChooseAction :: Decider.Decider -> PlayerId.PlayerId -> [Action.Action] -> Prompt Action.Action
  -- | CR 104.3a: a player may concede at any time. Asked before ChooseAction
  -- wherever a player would receive priority.
  --
  -- The only CHOICE prompt carrying no Decider, and that asymmetry IS CR 723.6's
  -- mechanism: the ask must reach the true player, and there must be nowhere to
  -- put a controller. Folding concede into ChooseAction would hand the controller
  -- exactly the power that rule forbids. (Shuffle and RandomFirstPlayer carry no
  -- Decider either, for the unrelated reason that randomness is not a choice.)
  --
  -- "At any time" is narrowed to "at each priority grant", and while a CR 729
  -- subgame runs this asks about the SUBGAME rather than the main game (#144,
  -- #397).
  Concede :: PlayerId.PlayerId -> Prompt Concession.Concession
  Shuffle :: [ObjectId.ObjectId] -> Prompt [ObjectId.ObjectId]
  -- | CR 729.2: randomly determine which player goes first. The NonEmpty is the
  -- turn order; the answer is the starting player, and the order is then rotated
  -- to begin with them (CR 103.1).
  --
  -- Carries no Decider, and neither does Shuffle: randomness is not a choice, so
  -- there is no decision to usurp and nowhere for a CR 723 controller to sit.
  -- Asked only where the rules call for randomness -- a subgame's start. A main
  -- game's starting player is settled before the game begins (CR 103.1), which is
  -- the caller-supplied turn order Setup.emptyGame takes.
  --
  -- NonEmpty rather than []: the answer has to come from somewhere and a fallback
  -- must be total.
  RandomFirstPlayer :: NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 514.2. The [ObjectId] is the hand; the Natural is how many to discard.
  ChooseDiscard :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 507.1 / 703.4h: the active player chooses one of their opponents to be
  -- the defending player. The NonEmpty is the candidates; the answer is the one
  -- chosen.
  --
  -- NonEmpty rather than []: a fallback must be total. No opponents at all cannot
  -- arise while the game runs (CR 104.2a), and is handled by not performing the
  -- action, leaving Combat.defender at Nothing -- not a claim about legality,
  -- since Combat.canAttack never reads that field.
  --
  -- Not asked when there is exactly one candidate (#169): CR 507.1 makes the
  -- choice exist only in a multiplayer game, and a two-player game gets its
  -- defending player from CR 506.2.
  ChooseDefender :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 601.2g (and CR 602.2b for an ability): which mana source to activate next
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
  ChooseManaSource :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 605.3b: which mana the source produces, asked as the mana ability
  -- resolves -- immediately, a mana ability never using the stack. The answer is a
  -- YIELD, the whole mana one activation adds, so "{T}: Add {C}{C}" is one
  -- candidate of two units. Three things reach this one prompt, being
  -- observationally the same question:
  --
  --   * one AddMana effect offering a choice, whose options are CR 105.4's colours;
  --   * a permanent with several single-type mana abilities, an Urborg'd Mountain
  --     being both a Mountain and a Swamp (CR 305.6/305.7); and
  --   * a mana ability with several MODES (CR 700.2). No card in the pool has one.
  --
  -- Collapsing them is sound because a source taps once and adds one yield, so all
  -- three have the same answer set and consequences. It would stop being sound if
  -- two abilities of one permanent differed in cost or in a rider, but
  -- Mana.tapForMana reads neither today (#238).
  --
  -- Candidates are deduplicated by the WHOLE yield, the one elision needing no
  -- judgement: two ways to produce black mana add the same mana.
  ChooseManaYield :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty Mana.Mana -> Prompt Mana.Mana
  -- | CR 701.34a: which permanents and players a proliferating player gives another
  -- counter to. The lists are every permanent and player holding at least one
  -- counter; the answer is the subset of each that gets one more of every kind it
  -- already has.
  --
  -- CHOOSE, not target: proliferate declares no target spec, so a candidate is
  -- offered whoever's it is, and nothing is re-checked at resolution (CR 608.2b).
  --
  -- Asked whenever either list is non-empty, elided only when both are empty.
  -- Deliberately not elided for a single candidate, unlike ChooseManaSource: "any
  -- number" includes none, so even one candidate is a real yes or no.
  ChooseProliferate :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [PlayerId.PlayerId] -> Prompt (Set.Set ObjectId.ObjectId, Set.Set PlayerId.PlayerId)
  -- | CR 701.54a: which creature a tempted player controls becomes their
  -- Ring-bearer. The NonEmpty is the creatures they control; the answer is the ONE
  -- that takes the designation.
  --
  -- CHOOSE, not target: rule 701.54a says "choose a creature you control", so no
  -- target spec is declared and nothing is re-checked at resolution (CR 608.2b).
  --
  -- Raised only for TWO OR MORE candidates, ChooseLegend's shape. One creature is
  -- not a choice: the action is mandatory and has exactly one legal answer, so
  -- performing it is not the engine deciding anything. It is emphatically not an
  -- elision of ChooseProliferate's kind, whose "any number" makes even a lone
  -- candidate a real yes or no.
  --
  -- NOT raised at all for zero candidates, and that is CR 701.54d rather than an
  -- omission: the player is tempted anyway, so Pawl.Engine.Ring.tempt still counts
  -- the temptation and still gives them the emblem.
  ChooseRingBearer :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 704.5j: which of two or more same-named legendary permanents its
  -- controller keeps. The NonEmpty is the whole same-named group; the answer is the
  -- ONE that survives.
  --
  -- One prompt per name, not per player: a player controlling two pairs faces two
  -- separate applications of the legend rule.
  --
  -- The answer is what is KEPT rather than what is buried, which is how CR 704.5j
  -- is worded and stays a single choice however large the group gets.
  --
  -- Never elided: the prompt is only raised for a group of two or more, and the
  -- permanents may differ in counters, Auras, damage or summoning sickness, none of
  -- which the shared name can see.
  ChooseLegend :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 508.1a. The [ObjectId] is the legal attackers; the answer is which of them
  -- attack. WHAT each one attacks is a SEPARATE question, asked once per chosen
  -- creature by ChooseAttackTarget below, because CR 508.1b is a separate step of
  -- the declaration and asks per creature rather than per declaration.
  DeclareAttackers :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt [ObjectId.ObjectId]
  -- | CR 508.1b: the active player announces which player, planeswalker or battle
  -- each chosen creature is attacking. The ObjectId is the creature being
  -- announced; the NonEmpty is what it may attack, the defending player first.
  --
  -- One prompt PER CREATURE, which is what the rule asks for rather than a
  -- convenience -- a per-declaration answer would collapse a choice the rules keep
  -- apart.
  --
  -- CR 508.4 reaches this prompt too, for a creature put onto the battlefield
  -- attacking: same question, same candidates, same chooser (CR 506.3b). One prompt
  -- and not two, since an interpreter that could tell them apart would answer them
  -- identically.
  --
  -- Elided at exactly one candidate, which is CR 508.1b's own condition read
  -- backwards: with no planeswalker, no battle and one defending player the rule
  -- calls for no announcement.
  ChooseAttackTarget :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty AttackTarget.AttackTarget -> Prompt AttackTarget.AttackTarget
  -- | CR 509.1. The legal blockers, then the attackers they may block. The answer
  -- maps each blocking creature to the attacker it blocks.
  DeclareBlockers :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [ObjectId.ObjectId] -> Prompt (Map.Map ObjectId.ObjectId ObjectId.ObjectId)
  -- | CR 510.1 / 702.19b: the attacker divides its power among the legal recipients.
  -- The Map is recipient -> lethal threshold (blockers -> lethal, the defender ->
  -- 0); trample-ness is entirely in whether the defender is a key and what the
  -- thresholds are. Not asked when the division is forced (single blocker, no
  -- excess). Validation is Damage.legalAssignment. See the M2c spec, section 4.
  AssignCombatDamage :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map Recipient.Recipient Natural.Natural -> Natural.Natural -> Prompt (Map.Map Recipient.Recipient Natural.Natural)
  -- | CR 601.2c. One legal-recipient set per named slot of the spell being cast
  -- (the ObjectId); the answer fills every slot. Slots agree by NAME, never by
  -- position. Not asked when the spell has no slots: zero slots is no choice
  -- at all, and where the rules leave nothing to ask, don't prompt.
  ChooseTargets :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map SlotName.SlotName (Set.Set Recipient.Recipient) -> Prompt (Map.Map SlotName.SlotName Recipient.Recipient)
  -- | CR 612: choose the two basic land types for a text-changing spell's slot.
  --
  -- Asked AS THE EFFECT IS APPLIED, not as the spell is cast: CR 608.2d puts a
  -- choice that is not one of CR 601.2b-d's cast-time announcements at the
  -- application, and a word swap is none of those. So Resolve raises this and Cast
  -- does not. The ObjectId is the resolving spell.
  --
  -- The two moments are observably different even though the legal set is always
  -- the five basics and so never gates castability: a countered spell is never
  -- asked at all, and a player asked at cast would choose without information the
  -- responses gave them.
  --
  -- Named for the CR 612 word REPLACEMENT rather than for counting its payload --
  -- the pair is not what makes this prompt what it is, the swap is.
  --
  -- The Set is the words the NEW type may not be, stated by the text-changer's own
  -- card text. Empty for a card that restricts nothing, carried anyway so the two
  -- swap prompts present restrictions the same way.
  ChooseLandTypeSwap :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> SlotName.SlotName -> Set.Set Subtype.Subtype -> Prompt (Subtype.Subtype, Subtype.Subtype)
  -- | CR 612: ChooseLandTypeSwap's sibling for the other family CR 612.2 names, a
  -- creature type word used as a creature type.
  --
  -- Asked at the same moment and for the same CR 608.2d reason. Its own constructor
  -- rather than a family field on that one, because the two offer different words
  -- and a responder that knows which prompt it is answering knows which list.
  --
  -- The Set is the words the NEW type may not be, read off Effect.ChangeText, which
  -- reads it off the card -- such a restriction is printed card text, so it lives
  -- in the card's data rather than hard-coded here.
  --
  -- No candidate list, unlike ChooseLandTypeSwap's implied five: CR 205.3m's
  -- creature types run to hundreds, so the family is named rather than enumerated.
  -- Asked whenever the effect applies, with no one-option case to elide.
  ChooseCreatureTypeSwap :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> SlotName.SlotName -> Set.Set Subtype.Subtype -> Prompt (Subtype.Subtype, Subtype.Subtype)
  -- | CR 614.1c: as an object enters, its controller chooses ONE basic land
  -- type ("As this Aura enters, choose a basic land type" -- Convincing
  -- Mirage). The ObjectId is the entering object.
  --
  -- Singular, and deliberately not ChooseLandTypeSwap above. That prompt
  -- answers with a PAIR because CR 612's word swap needs two words (Magical
  -- Hack's "one basic land type" and the "another" replacing it); this is a
  -- single choice, made at a different moment (entry, not resolution) and by a
  -- different subsystem (Pawl.Engine.Replacement, not Pawl.Engine.Resolve).
  -- Answering it with a pair and dropping half would be the engine deciding
  -- something no player was asked.
  --
  -- No candidate list: CR 305.6 fixes the five basic land types ("The basic
  -- land types are Plains, Island, Swamp, Mountain, and Forest") the way CR
  -- 105.1 fixes the five colours for ChooseColor, and no card in the pool
  -- narrows them. Asked whenever the entering object has a controller to ask --
  -- five types are five distinguishable options, so there is no one-option case
  -- to elide. Pawl.Engine.Replacement's arm has an unreachable no-controller
  -- fallback beside it; see there.
  --
  -- No SlotName, unlike ChooseLandTypeSwap: that prompt names the spell's
  -- text-change slot, and this choice is bound into no slot at all -- it is
  -- written to Object.chosenSubtype on the entering permanent.
  ChooseBasicLandType :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Subtype.Subtype
  -- | CR 701.23 / 701.23b: the [ObjectId] is the library cards MATCHING the
  -- criterion (the engine pre-filters to legal choices); Nothing is "fail to
  -- find," always permitted for a search of one's own library for a quality.
  SearchLibrary :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 608.2g: the re-entrant cast opportunity during a library search (Panglacial
  -- Wurm) -- an effect that "specifically instructs or allows a player to cast a
  -- spell during resolution", following CR 601.2a-i except that no player receives
  -- priority after it is cast. The [ObjectId] is the searcher's library cards
  -- castable-while-searching (the engine pre-filters to permitted, affordable,
  -- fillable). Nothing = decline / done. Offered in a loop before the search finds
  -- (per the ruling), so multiple copies may be cast. CR 605.3a permits mana
  -- activation to pay.
  CastWhileSearching :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 601.2b: choose the value of X while casting a spell -- or, through CR
  -- 602.2b, while activating an ability, which is the same rule reached by "the
  -- remainder of the process for activating an ability is identical to the
  -- process for casting a spell listed in rules 601.2b-i". The ObjectId is
  -- whichever object is on the stack: the spell, or the ability object
  -- (Activate.activateAbility, #544).
  --
  -- The Natural is the greatest value this player could actually PAY for right now,
  -- climbed by Cast.affordableX / Activate.affordableX from the same predicate
  -- their own gates asked at CR 601.2b's X=0 floor. The two measure different costs
  -- (#90), so the bound is the greatest payable X of the cost that will really be
  -- paid.
  --
  -- ADVISORY, not a limit and not a clamp: the answer is filtered against it
  -- nowhere. CR 601.2b lets the player announce the value freely, and an
  -- announcement the cost cannot pay is answered by CR 601.2 / 602.2's reversal,
  -- which is pawl's no-op minus the prompts (#741). Both callers take that reversal
  -- at THIS step rather than carrying an already lost spell to CR 601.2h. What the
  -- bound adds is the INFORMATION a player at a table has and an answerer, seeing
  -- only this payload, did not (#417).
  --
  -- Counts LIFE, not only mana: Cost.canPay measures CR 601.2b's nonhybrid
  -- resolutions, so a Phyrexian symbol's 2 life (CR 107.4f) is one route the climb
  -- can find. It inherits that predicate's caveats, notably #365. Measured before
  -- this cost's Phyrexian symbols are announced, which is CR 601.2b's own order.
  --
  -- A bare Natural rather than a Maybe: the prompt is issued only for a cost that
  -- already passed the X=0 floor, so a greatest payable X always exists and 0 is a
  -- real answer. There is no unbounded case -- a player's mana is finite.
  --
  -- Prompted before targets (CR 601.2b precedes 601.2c), and only when the cost
  -- contains a Variable symbol.
  ChooseX :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt Natural.Natural
  -- | Rule 702.42a: whether this player uses the entwine ability of the modal spell
  -- they are casting. The ObjectId is the spell; the Cost is the additional cost
  -- entwining adds on top of whichever candidate cost is then announced (CR 601.2f).
  --
  -- One question rather than two, that rule being one sentence: the widened mode
  -- choice and the intention to pay an additional cost are the same decision, so
  -- this is asked BEFORE ChooseModes, which then has nothing left to ask.
  --
  -- Asked only when entwining is available -- the spell has the keyword, every mode
  -- is legal (CR 700.2a), and some candidate cost plus this one is payable.
  ChooseEntwine :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Cost.Cost Keyword.Keyword -> Prompt EntwineDecision.EntwineDecision
  -- | CR 601.2b / 700.2a: choose the mode(s) while casting (the ObjectId is the
  -- spell). The Set ModeIndex is the LEGAL modes -- the engine pre-filters to modes
  -- whose targets are all fillable (CR 700.2a). The Natural is how many to choose.
  -- The answer is the chosen subset. Prompted before X and targets, and ONLY when
  -- #legal > count; a forced selection is not asked -- which is every entwined
  -- cast, where CR 702.42a's "all modes" leaves no subset to pick.
  ChooseModes :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Set.Set ModeIndex.ModeIndex -> Natural.Natural -> Prompt (Set.Set ModeIndex.ModeIndex)
  -- | CR 707.5 / 614.1c / 614.12a: as an object enters AS A COPY, its controller
  -- chooses which permanent to copy. The ObjectId is the entering object; the
  -- [ObjectId] is the legal copy targets, pre-filtered by the engine; Nothing is
  -- the card's own "may" decline, after which it enters as itself. Answered inside the zone change (CR 614.12a) before
  -- the enters event is recorded, so the choice really is made as the object
  -- enters. The legal set excludes anything entering in the same batch, a sibling
  -- not yet being "on the battlefield" when the choice is made.
  ChooseCopyTarget :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 208.2b / 614.1c: as an object enters, its controller chooses among the
  -- shapes an "as this creature enters, it becomes your choice of ..." ability
  -- offers (Primal Plasma). The ObjectId is the entering object; the answer is an
  -- index into the offered list.
  --
  -- The chosen shape is written into the object's COPIABLE snapshot (CR 707.2), so
  -- a later Clone copies the choice without any further machinery -- and then, if
  -- it copied the ABILITY too, makes its own choice on top (CR 616.2).
  --
  -- Asked only when two or more options are offered; one option is not a choice.
  ChooseEntryOption :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [EntryOption.EntryOption] -> Prompt Natural.Natural
  -- | CR 614.1c: as an object enters, its controller chooses a colour
  -- ("As this creature enters, choose a color" -- Painter's Servant). The
  -- ObjectId is the entering object.
  --
  -- No candidate list: CR 105.1 fixes the five colours and no card in the pool
  -- narrows them. Asked whenever the entering object has a controller to ask --
  -- five colours are five distinguishable options, so there is no one-option
  -- case to elide. Replacement's arm has an unreachable no-controller fallback
  -- beside it; see there.
  ChooseColor :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Color.Color
  -- | CR 201.4 / 614.1c: as an object enters, a player chooses a card name ("you
  -- and an opponent each choose a card name other than a basic land card name"
  -- -- Null Chamber). The PlayerId is the CHOOSER, who is the entering object's
  -- controller for one of Null Chamber's two asks and an opponent for the other;
  -- the ObjectId is the entering object.
  --
  -- The one as-enters choice prompt whose chooser is not always the entering
  -- object's controller, which is why the two are not assumed equal anywhere
  -- this is raised. Pawl.Engine.Replacement asks in CR 101.4's APNAP order,
  -- since both choices are made at the same time.
  --
  -- No candidate list, for ChooseCreatureTypeSwap's reason carried further: CR
  -- 205.3m's creature types run to hundreds, and CR 201.4's "the name of a card
  -- in the Oracle card reference" is not a set the engine holds at all. Nothing
  -- in the layering forbids that -- Pawl.Registry sits ahead of the engine in
  -- the table, so the edge would be legal -- but it is not wired, and wiring it
  -- would hand the closed half a card-by-name lookup, which is the capability
  -- design.md §1's invariant is enforced by withholding (see
  -- Pawl.Engine.Card's header). #663 is that whole question.
  --
  -- The Filter is CR 201.4a's restriction, read off the card that asks: "the
  -- player must choose the name of a card whose Oracle text matches those
  -- characteristics", which for Null Chamber is "other than a basic land card
  -- name". ADVISORY, like ChooseX's bound -- the answer is filtered against it
  -- nowhere (#663), and for the same reason there is no candidate list.
  ChooseCardName :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Filter.Filter Keyword.Keyword -> Prompt CardName.CardName
  -- | Which opponent an as-enters choice names -- Null Chamber's "you and AN
  -- OPPONENT each choose a card name". The PlayerId is the chooser, the ObjectId
  -- is the entering object, and the NonEmpty is that player's opponents. CR
  -- 614.12a is what puts the ask before the permanent enters: "If a replacement
  -- effect that modifies how a permanent enters the battlefield requires a
  -- choice, that choice is made before the permanent enters the battlefield."
  --
  -- Its own prompt rather than a reuse of ChooseDefender, which also answers with
  -- an opponent: that prompt asks CR 507.1's question, whom to attack, and a
  -- responder that knows which prompt it is answering knows which question it was
  -- asked.
  --
  -- WHO is asked is pawl's reading rather than a rule's: the card leaves "an
  -- opponent" open and no rule assigns the pick, so it goes to the ability's
  -- controller -- CR 109.5's "you", the player the card's other half already
  -- names. Asked only when there are two or more: CR 102.2's two-player game
  -- leaves exactly one opponent and nothing to ask.
  ChooseOpponent :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 603.3b: each player, in APNAP order, puts the triggered abilities they
  -- control on the stack in any order they choose. The [TriggerEntry] is that
  -- player's pending triggers in the engine's canonical order; the answer is a
  -- permutation of the entry INDICES giving the order they are put on the stack, so
  -- the last named resolves first.
  --
  -- A TriggerSource rather than an ObjectId inside the entry because CR 725.2's
  -- inherent monarch abilities have no source and are ordinary triggered abilities
  -- otherwise, so they are in this batch with no id to put on the wire.
  --
  -- Positional, but no longer positional BY NECESSITY: the entry carries the
  -- ability alongside the source (#61), so two entries are equal exactly when they
  -- are interchangeable. A source with two DISTINCT abilities triggered by one
  -- event is real and in the pool, and the two entries differ; two triggers of the
  -- SAME ability stay equal, which is the other half of the requirement.
  --
  -- Asked only when the player controls two or more. Still asked when those two or
  -- more are all EQUAL, which ChooseReplacement elides (#590): the equality holds
  -- up to their bindings, and nothing here has read those to check.
  --
  -- CR 603.3b's two-part process is vacuous while no condition triggers on another
  -- ability triggering; this carries the note, not the machinery.
  OrderTriggers :: Decider.Decider -> PlayerId.PlayerId -> [TriggerEntry.TriggerEntry] -> Prompt [Natural.Natural]
  -- | CR 615.7: with damage from two or more applicable sources at once, the
  -- shielded player or the permanent's controller chooses which damage the shield
  -- prevents. The [DamageEvent] is those simultaneous events in the engine's
  -- canonical order; the answer is a permutation of their INDICES, giving the order
  -- the shields are offered them.
  --
  -- An ORDER rather than a pick, which is the rule read closely: within one event a
  -- shield covers as much as it can and no more, then moves on with whatever is
  -- left, so the only freedom the rule grants is WHICH event is covered first.
  --
  -- The whole DamageEvent is on the wire rather than just each source: the amounts
  -- are what the answer turns on, and the riders decide what surviving damage does.
  --
  -- Asked only when the order is observable -- two or more events one shield
  -- admits, and a remaining amount neither 0 nor enough to cover all of them.
  OrderDamage :: Decider.Decider -> PlayerId.PlayerId -> [DamageEvent.DamageEvent] -> Prompt [Natural.Natural]
  -- | CR 616.1: with two or more applicable replacement or prevention effects in
  -- the highest non-empty bucket, the affected object's controller (or its owner,
  -- or the affected player) chooses which to apply NEXT -- and then the process
  -- repeats over what is applicable now (616.1f), so this is asked once per
  -- iteration, not once per event. The [ObjectId] is each candidate's SOURCE, in
  -- the engine's canonical order (battlefield ascending, then the floating
  -- store); the answer is an index into it.
  --
  -- Positional, and carrying the hole OrderTriggers no longer has: a source with
  -- two DISTINCT applicable replacement abilities would put two different effects
  -- on the wire as identical entries. No card in the pool has two in the same event
  -- class, so it is unreachable; the fix is the shape the trigger side already took
  -- -- an entry carrying the applicable effect alongside its source (#74).
  --
  -- Asked only when the bucket holds two or more candidates that are not all
  -- indistinguishable: among indistinguishable ones every order yields the same
  -- board, each still getting its own CR 614.5 opportunity. Indistinguishable is
  -- equal in the EFFECT, plus -- where application reads the applying candidate,
  -- which is Replacement.readsApplier's question -- equal in CR 109.5's "you".
  ChooseReplacement :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt Natural.Natural
  -- | CR 603.7c: which of several minted tokens a Create's slot binds -- the "it" a
  -- delayed triggered ability armed in the same resolution will name. The ObjectId
  -- is the resolving source holding the binding; the NonEmpty is the tokens
  -- actually minted, in creation order.
  --
  -- Reachable only through a replacement: the CardSpec lint rejects a card whose
  -- Create binds a slot while its printed quantity is anything but exactly one
  -- (#53), but CR 614.16 scales the count at RUNTIME, long after that lint passed.
  --
  -- CR 707.10e is the codified analogue and what settles this as a CHOICE rather
  -- than a rule the engine may apply itself: where a replacement causes a copy to
  -- target more than one object, its controller chooses one. Binding the first
  -- minted token would be the engine choosing.
  --
  -- No SlotName in the payload: the candidates are distinct minted objects, so
  -- two slot-binding Creates in one resolution already ask distinguishable
  -- questions, and the slot name is card-data vocabulary rather than anything a
  -- player sees.
  --
  -- Asked ONLY when two or more tokens were minted. One token is the whole
  -- candidate set, and where the rules leave nothing to ask, don't prompt.
  ChooseBoundToken :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.21a: which permanents to sacrifice to pay a cost. The ObjectId is the
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
  ChooseSacrifices :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 701.3a: where an effect that moves an already-attached permanent puts it.
  -- The first ObjectId is the permanent being moved (Crown of the Ages' targeted
  -- Aura); the NonEmpty is the destinations its card text admits.
  --
  -- CHOOSE, not target: such an effect targets the Aura and not either creature, so
  -- a destination is offered whoever's it is and whether or not it can be targeted,
  -- and nothing is re-checked under CR 608.2b. The offer is the card's TEXT and
  -- nothing more -- "another creature" gets every creature, including ones CR
  -- 303.4j will refuse to move the Aura onto, since narrowing past what the card
  -- says would answer that rule on the player's behalf; "another permanent it can
  -- enchant" gets only the legal ones (Filter.CanHostSubject).
  --
  -- Elided at exactly one candidate, the ChooseManaSource posture: the effect is
  -- mandatory, so a single destination leaves nothing to decide. The current host
  -- is never among the candidates (CR 701.3b), so it is not being withheld.
  --
  -- NonEmpty rather than []: the caller does not raise this when no destination
  -- exists, and a fallback must be total.
  ChooseAttachment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 601.2b: the player announces their intention to pay any alternative or
  -- additional costs. Issued after the modes and before X and targets, at that
  -- rule's own position. The [Cost] is the PAYABLE candidates, pre-filtered through
  -- Cost.costsFor, total and canPay at the CR 601.2b X=0 floor.
  --
  -- CR 118.9b makes an alternative cost optional, so a player who can afford both
  -- is genuinely choosing. Asked only when two or more candidates are payable.
  ChooseCost :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [Cost.Cost Keyword.Keyword] -> Prompt (Cost.Cost Keyword.Keyword)
  -- | CR 103.5: whether this player takes a mulligan. The MulliganOffer carries
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
  DeclareMulligan :: Decider.Decider -> PlayerId.PlayerId -> MulliganOffer.MulliganOffer -> Prompt MulliganDecision.MulliganDecision
  -- | CR 103.5: after redrawing, put `count` cards from `hand` on the bottom of the
  -- library, in the player's chosen order. The [ObjectId] is the redrawn hand;
  -- the answer is an ordered list of exactly `count` of those ids (first-listed
  -- ends up higher in the library, drawn sooner). Bottom order IS future draw
  -- order, so it is a real choice even when the subset is forced (count == hand
  -- size), which is why the answer is an ordered list, never a Set. Asked only
  -- when the hand has >= 2 cards; with 0 or 1 there is one possible outcome, and
  -- where the rules leave nothing to ask, don't prompt.
  Bottom :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 103.5b: an action a card lets a player take "any time they could
  -- mulligan". The [ObjectId] is the cards in hand granting one; the answer is
  -- which to use, or Nothing to decline.
  --
  -- Offered immediately BEFORE each DeclareMulligan and again after each action
  -- taken -- that rule makes the declaration follow, and nothing in it limits a
  -- player to one action. Offered in every round, not just the first, and only to a
  -- player who has not yet kept.
  --
  -- Performing the action is NOT taking a mulligan: nothing is shuffled or
  -- bottomed, so it feeds neither the bottom count nor CR 103.5c's free allowance.
  --
  -- Not asked when the list is empty.
  MulliganAction :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 103.6 / 103.6a: an action a card in this player's opening hand lets them
  -- take once the mulligan process is complete. The [ObjectId] is the cards in hand
  -- offering one; the answer is which to take, or Nothing to decline.
  --
  -- Offered in turn order, starting player first, and repeatedly to the same player
  -- until they decline: CR 103.6 lets them take any such actions in any order.
  --
  -- A SEPARATE channel from MulliganAction rather than a reuse: that window sits AT
  -- a mulligan declaration and this one opens once the process is complete, so an
  -- interpreter that could not tell them apart could not answer either well.
  OpeningHandAction :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 603.5 / 608.2d: whether the controller of a resolving spell or ability
  -- exercises a printed "may". The ObjectId is the object RESOLVING (the spell,
  -- or the ability object on the stack -- not its source, since two triggers off
  -- one source resolve as two distinct stack objects); the ModeIndex is which of
  -- its chosen modes is asking, so a modal payload with two optional modes puts
  -- two DISTINGUISHABLE questions on the wire -- the discriminator
  -- ChooseReplacement (#74) still does without, and the one OrderTriggers gained
  -- with its TriggerEntry (#61).
  --
  -- CR 603.5 makes this a resolution-time prompt rather than a cast-time one: an
  -- optional ability goes on the stack regardless, and the choice is made when it
  -- resolves. CR 608.2d then places it exactly.
  --
  -- NEVER elided. "You may gain 2 life" is a genuine choice even when it looks
  -- strictly good: a life total is read by other cards, so both answers are
  -- distinguishable game states. The one case where it is not asked is where
  -- nothing is being decided -- an ability whose targets are all illegal is removed
  -- from the stack by CR 608.2b first. That covers every optional card in the pool,
  -- all single-mode; a modal payload mixing a live mode with a dead optional one
  -- would reach this prompt with nothing to decide (#336).
  ChooseOptional :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> Prompt OptionalDecision.OptionalDecision
  -- | CR 118.12 / 118.12a: whether this player pays a cost a RESOLVING spell or
  -- ability offers them -- Mana Leak's "unless its controller pays {3}", which CR
  -- 118.12a rewrites as "its controller may pay {3}. If they don't, counter it."
  -- The ObjectId is the object resolving and the ModeIndex is which of its chosen
  -- modes is asking, both for Prompt.ChooseOptional's reasons; the Cost is what
  -- is being offered, which is the information the answer turns on.
  --
  -- The PlayerId is emphatically NOT the resolving controller, which is what
  -- separates this from every other resolution-time prompt: CR 118.12's clause
  -- names a player, and for this card family that player is the one the effects
  -- would be aimed AT. Routed through Decide.deciderFor like every other
  -- player-facing prompt, so a player controlled under CR 723.1 has their
  -- controller answer.
  --
  -- Asked at CR 118.12's own moment, when the spell or ability RESOLVES, so a
  -- countered Mana Leak never asks -- observably different from an announcement
  -- at CR 601.2f-h, where the cost of a spell being cast is paid.
  --
  -- NEVER elided for a payable cost: CR 118.12a's rewriting makes the cost a
  -- "may", and both answers reach different boards. The one case not asked is
  -- where the rules leave nothing to ask -- CR 118.3's "a player can't pay a cost
  -- without having the necessary resources to pay it fully", which leaves
  -- declining as the only possible answer. CR 118.12's clause covers that case in
  -- as many words ("does, doesn't, or CAN'T"). Its Standstill example is NOT this
  -- one: that cost is mandatory ("sacrifice this enchantment. If you do"), so its
  -- "can't" is 118.12's "started to pay a mandatory cost" limb, which is the
  -- positive shape pawl cannot represent at all (#701).
  ChooseToPay :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> Cost.Cost Keyword.Keyword -> Prompt PaymentDecision.PaymentDecision
  -- | CR 601.2b: the player announces whether they intend to pay 2 life or a
  -- coloured mana cost for each Phyrexian symbol. CR 118.13a puts the choice HERE
  -- rather than at payment. The ObjectId is the spell or the permanent whose
  -- ability is being activated (CR 602.2b); the Color is the symbol's own, so two
  -- symbols of different colours put DISTINGUISHABLE questions on the wire.
  --
  -- One prompt per symbol, in printed order, and the NonEmpty is the routes
  -- actually payable given the announcements already made, so a player can never
  -- announce a route CR 118.3 will not let them complete. Payable is measured at CR
  -- 601.2f's TOTAL rather than the printed cost, which keeps that promise under a
  -- cost increase or reduction. Two symbols of the SAME colour ask two identical
  -- questions, which is sound here because the answers are interchangeable: both
  -- demand the same mana and the same 2 life, so which prompt got which is not
  -- observable -- a claim OrderTriggers could not make until its entry carried an
  -- ability (#61) and ChooseReplacement still cannot (#74).
  --
  -- Elided when only one route is payable -- no source of the symbol's colour, or a
  -- life total below CR 119.4's floor.
  AnnouncePhyrexianPayment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Color.Color -> NonEmpty.NonEmpty PhyrexianPayment.PhyrexianPayment -> Prompt PhyrexianPayment.PhyrexianPayment
  -- | CR 601.2b: "If a cost that will be paid as the spell is being cast includes
  -- hybrid mana symbols, the player announces the nonhybrid equivalent cost they
  -- intend to pay." CR 118.13a puts that choice HERE rather than at payment. The
  -- ObjectId is the spell or the permanent whose ability is being activated (CR
  -- 602.2b); the ManaType is the symbol's own stated type, so a {2/R} and a {2/G}
  -- in one cost put DISTINGUISHABLE questions on the wire.
  --
  -- CR 107.4e's MONOCOLORED half only ({2/R}), which is the half whose two ways
  -- spend a different NUMBER of mana. A colour/colour hybrid ({W/U}) is still not
  -- announced and could not be answered with a HybridPayment anyway (#729).
  --
  -- One prompt per symbol, in printed order, and the NonEmpty is the routes
  -- actually payable given the announcements already made -- the
  -- AnnouncePhyrexianPayment contract, for the same CR 601.2b last sentence, and
  -- payable is measured at CR 601.2f's TOTAL for the same reason. Two {2/R}s in
  -- one cost ask two identical questions, which is sound here because the answers
  -- are interchangeable: both demand the same one mana or the same {2}, so which
  -- prompt got which is not observable.
  --
  -- Elided when only one route is payable -- no source of the symbol's type, or
  -- too few mana on the board for the {2}.
  AnnounceHybridPayment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaType.ManaType -> NonEmpty.NonEmpty HybridPayment.HybridPayment -> Prompt HybridPayment.HybridPayment
