module Pawl.Types.Filter where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | A first-order predicate over one candidate -- an object, or (CR 115.1) a
-- player, since a target may be either -- expressed as data and evaluated by one
-- generic matcher (Pawl.Engine.Filter.matches) that never learns which effect
-- produced it. Its atoms case on CHARACTERISTICS, and on a handful of things CR
-- 109.3 says are NOT characteristics but that the closed half owns just as
-- squarely: combat status, attachment, what a permanent is represented by, and
-- what happened earlier this turn. Casing on a characteristic classification is
-- legitimate; the invariant forbids only casing on an EFFECT's identity.
--
-- Flat, not layered: the atoms and the And/Or/Not combinators are sibling arms of
-- one type, mirroring Pawl.Types.Quantity. A Simple/combinator split would buy an
-- enforceable normal form only if it also restricted the recursion (CNF/DNF).
-- `And []` is the trivial predicate, so a bare "target creature" needs no
-- separate "always" arm.
--
-- PARAMETRIC in the keyword, and only to break a module cycle: HasKeyword below
-- names Pawl.Types.Keyword, and that module already names this one (CR 702.29e
-- typecycling and CR 702.14c landwalk each carry their "[type]" as a Filter).
-- Pawl.Types.Keyword ties the knot by instantiating `Filter Keyword`, the single
-- application every module but this one, Pawl.Types.Cost and
-- Pawl.Types.CostComponent writes.
data Filter keyword
  = HasCardType CardType.CardType -- CR 205 / 300: the object's card types include this one.
  | HasSupertype Supertype.Supertype -- CR 205.4: the object's supertypes include this one.
  | HasColor Color.Color -- CR 105.2: the object's colours include this one.
  | HasSubtype Subtype.Subtype -- CR 205.3: the object's subtypes include this one.
  | -- | CR 201.2: the object's names include this LITERAL one --
    -- Asmoranomardicadaistinaculdacar's "search your library for a card named The
    -- Underworld Cookbook". A name is a characteristic (CR 109.3 lists it first),
    -- so this sits with the four atoms above rather than needing their defence.
    --
    -- MEMBERSHIP, exactly as CR 709.4a states the test: "an object has the chosen
    -- name if one of its names is the chosen name". A split card off the stack
    -- and a Room with both doors unlocked each show two names at once, and the
    -- joined string they render as is not among them (see #650), so an atom that
    -- compared to a single name would miss the halves it is spelling.
    --
    -- The PRINTED name, and not Pawl.Types.EntryRewrite.ChooseCardNames' chosen
    -- one: that machinery answers "what did a player name?", read by
    -- PlayerEffect.CantCastChosenName, where this is a name another card's own
    -- text prints. The two are different questions and neither can express the
    -- other -- a card cannot choose on the player's behalf, and a player cannot
    -- be asked to name what a card already says.
    HasName CardName.CardName
  | -- | The object HAS this keyword ability (CR 702.1) -- Plummet's "target
    -- creature with flying" (CR 702.9). Needs no defence like the atoms below,
    -- since CR 109.3 lists abilities among an object's characteristics.
    --
    -- MEMBERSHIP, not equality of an ability list: the projection stores keywords
    -- as a count because CR 702 instances stack, and this asks only whether the
    -- key is present. For a parameterized keyword that makes `HasKeyword (Toxic
    -- 2)` ask about toxic 2 SPECIFICALLY, which is the narrow question and the
    -- right one: CR 702.14a's "[type]walk" is a written ability in its own right,
    -- so Quagmire's "creatures with swampwalk" must not reach islandwalk. Ask the
    -- family with HasKeywordFamily below; Pawl.FilterSpec pins the pair apart.
    --
    -- Read through the PROJECTION, so a creature that gains flying at CR 613.1f
    -- layer 6 matches and a Humility'd one stops matching -- in every zone, since
    -- CR 613.1 names none: a library search, a cost criterion, an explore's land
    -- test and a mill tally all read a card's projected keywords rather than its
    -- printed ones (see #1911).
    HasKeyword keyword
  | -- | The object has SOME keyword ability of this family (CR 702.1), whatever
    -- its payload -- Flensing Raptor's "another target creature you control with
    -- toxic", which a Phyrexian Mite with toxic 1 and a creature with toxic 3
    -- satisfy alike (CR 702.164a).
    --
    -- A SIBLING of HasKeyword above rather than a widening of it, because the two
    -- questions are both real: this one is what card text asks when it names the
    -- ability, and that one is what it asks when it names the written instance.
    -- Widening HasKeyword in place would have made `HasKeyword (Toxic 2)` and
    -- `HasKeyword (Toxic 3)` observably the same value.
    --
    -- CONCRETE where HasKeyword is parametric, which is why the parameter above
    -- survives: Pawl.Types.KeywordFamily is payload-free and imports nothing, so
    -- naming it here opens no cycle. Nullary keywords have no family constructor,
    -- so the two atoms partition rather than overlap -- there is exactly one way
    -- to write "a creature with flying" and exactly one to write "a creature with
    -- toxic". Answered off the PROJECTION for HasKeyword's reason.
    HasKeywordFamily KeywordFamily.KeywordFamily
  | PowerAtLeast Integer -- CR 208.1: the object's power is >= this literal.
  | -- | CR 208.1 in the other direction: the object's power is <= this literal --
    -- Ezuri, Claw of Progress' "a creature you control with power 2 or less".
    --
    -- A SIBLING of PowerAtLeast above and NOT its negation: power is a
    -- characteristic an object may simply not have, so `Not (PowerAtLeast 3)`
    -- admits every land, instant and player on the board, while this admits none
    -- of them. Both arms answer False for an absent power, so neither is
    -- reachable from the other, and Pawl.FilterSpec pins that pair apart.
    --
    -- Answered off the PROJECTION, in every zone: Imperial Recruiter's "creature
    -- card with power 2 or less" reads a library card's full CR 613 projection,
    -- Tarmogoyf's CR 208.2a power included, and so do a graveyard-exile cost
    -- criterion and a mill tally (see #1911). CR 208.2a's characteristic-defining
    -- power arrives at layer 7a of that same fold, which CR 604.3 runs off the
    -- battlefield as well as on it.
    PowerAtMost Integer
  | -- | CR 208.1 compared against the SOURCE rather than a literal: the object's
    -- power is less than the power of the object the evaluation comes from. CR
    -- 702.134a's "target attacking creature with power less than this creature's
    -- power" is the one clause that asks, and Pawl.Engine.Keyword.mentor is the
    -- only site that writes it -- Pawl.CardSpec's lint keeps it out of card data,
    -- where it would read a source power no other Filter position supplies.
    --
    -- Context-relative like IsSource, and for the same reason the two power atoms
    -- above are not: there is no literal to carry, since the bound is whatever the
    -- source's power is when the match is made. Pawl.Engine.Filter.Context's
    -- sourcePower is where that arrives, filled by Pawl.Engine.Target.admittedGiven
    -- -- the one site that evaluates a target slot's Filter, at both of CR 115's
    -- moments -- and Nothing wherever neither this atom nor
    -- PowerGreaterThanSource below can appear.
    --
    -- STRICTLY less, and vacuously False when either power is absent: an object
    -- with no power is not "a creature with lesser power", the posture PowerAtMost
    -- takes for the same reason.
    PowerLessThanSource
  | -- | CR 208.1 compared against the SOURCE the other way: the object's power is
    -- GREATER than the power of the object the evaluation comes from. CR 702.149a's
    -- "another creature with power greater than this creature's power" is the one
    -- clause that asks, and Pawl.Engine.Keyword.training is the only site that
    -- writes it -- Pawl.CardSpec's lint keeps it out of card data alongside its
    -- sibling.
    --
    -- A SIBLING of PowerLessThanSource rather than `Not PowerLessThanSource`: the
    -- negation admits equal power, and admits a candidate with no power at all,
    -- since that arm is vacuously False. Rule 702.149a's "greater" is strict and
    -- wants neither.
    --
    -- Context-relative for that atom's reason, and reading the same
    -- Pawl.Engine.Filter.Context sourcePower -- which this atom is the first to
    -- want anywhere but a target slot: at a TRIGGER match
    -- (Pawl.Engine.Event.matchesTrigger) and at CR 509.1b's blocking gate
    -- (Pawl.Engine.CombatRestriction.cantBeBlockedBy), where the source is the
    -- attacker being blocked.
    PowerGreaterThanSource
  | -- | CR 202.3: the object's mana value is <= this literal -- Ojutai's
    -- Command's "creature card with mana value 2 or less".
    --
    -- Only AT MOST, where power above has both directions, because that is the
    -- direction the cards ask in: a mana value bounds what a cheap-thing effect
    -- may reach. Nothing in the pool asks for a mana value floor, and an unused
    -- arm is the speculative construction the project forbids.
    --
    -- Answerable OFF the battlefield, as the two power atoms above are and for
    -- the same reason: rule 202.3 reads the printed mana cost, which exists in
    -- every zone, and the graveyard is where the card asking is looking (CR 115.2's
    -- other zone half, via Pool.CardsInGraveyard). No Modification writes a mana
    -- cost, so there is nothing projected to read instead.
    ManaValueAtMost Integer
  | -- | CR 202.3 read for its PARITY rather than against a bound -- Void
    -- Winnower's "spells with even mana values", whose own reminder text settles
    -- the boundary case: "(Zero is even.)"
    --
    -- Payload-free, where the atom above carries a literal, because the printed
    -- sentence has no number in it: the parity is the whole of the test, and
    -- "odd" is Not of this rather than a second arm (nothing in the pool asks
    -- for odd).
    --
    -- Answerable off the battlefield for ManaValueAtMost's reason, and CR 202.3e
    -- is what makes it INTERESTING off the stack: a variable in the cost counts
    -- as zero anywhere but the stack, so an {X}{R}{R} card has an even mana
    -- value in a hand and either parity once X is chosen. That is the one axis a
    -- proposal choice moves, and it is why CR 601.3a's lookahead exists --
    -- Pawl.Engine.PlayerEffect.prohibitsCasting is where the rule is read.
    ManaValueIsEven
  | ControlledBy PlayerRelation.PlayerRelation -- CR 109.5 / 102.2: controller relates thus to the perspective.
  | -- | CR 508.5: the candidate's controller is the DEFENDING PLAYER for the
    -- object the evaluation comes from -- CR 702.39a's "target creature defending
    -- player controls", which Pawl.Engine.Keyword.provoke is the only site to
    -- write. Kept out of card data by Pawl.CardSpec's lint, as PowerLessThanSource
    -- is and for the same reason.
    --
    -- NOT ControlledBy Opponent, and the difference is a wrong answer rather than
    -- a nicety: CR 506.2a and CR 508.5a make exactly one opponent the defending
    -- player, so on a
    -- board with three seats that filter admits a creature controlled by an
    -- opponent who is not being attacked at all.
    --
    -- Context-relative like PowerLessThanSource, and the same machinery: the
    -- answer depends on the combat record rather than on the candidate, so
    -- Pawl.Engine.Filter.Context's defendingPlayer is where it arrives, filled by
    -- Pawl.Engine.Target.admittedGiven for a target slot and by
    -- Pawl.Engine.CombatRestriction.inForce for a CR 508.1c gate (Armored
    -- Galleon), and Nothing everywhere else. Vacuously
    -- False when either player is absent -- a source that is not attacking has no
    -- defending player.
    ControlledByDefendingPlayer
  | -- | CR 603.2: the candidate's controller is the PLAYER BOUND at this slot --
    -- Trygon Predator's "target artifact or enchantment THAT PLAYER controls",
    -- where "that player" is the one the trigger's own event named
    -- (Pawl.Engine.Binding.triggerPlayer). The atom card JSON writes; the arm
    -- below is what it becomes.
    --
    -- NOT ControlledBy Opponent, and the difference is a wrong answer rather than
    -- a nicety, exactly as it is for ControlledByDefendingPlayer above: CR 806.1's
    -- free-for-all has several opponents, and only one of them is the player the
    -- event named. The two coincide on a two-player board alone.
    --
    -- Answered by REWRITING rather than by a Context field
    -- (Pawl.Engine.Filter.bakeBound): the two moments that judge a target slot --
    -- CR 603.3d's choosing (Pawl.Engine.Engine.placeBorne) and CR 608.2b's
    -- re-check (Pawl.Engine.Resolve.resolveModes) -- each hold the bindings and
    -- hand Pawl.Engine.Target a slot with this atom already replaced. Vacuously
    -- False if it survives to a match, which is a slot that named no one player.
    ControlledByBound SlotName.SlotName
  | -- | The atom above with its player resolved: the candidate's controller IS this
    -- player. RUNTIME-ONLY, in Modification.SetController's sense and enforced the
    -- same way -- the codec round-trips the PlayerId, so Pawl.CardSpec lints the
    -- pool against a card authoring one, a baked player in printed text being
    -- meaningless (#199).
    --
    -- Perspective-free, unlike every other player-relating atom here: the player is
    -- named outright, so CR 109.5's "you" does not enter into it.
    ControlledByPlayer PlayerId.PlayerId
  | -- | The candidate's controller is the player the surrounding effect is
    -- CURRENTLY BEING APPLIED TO -- Biorhythm's "each player's life total becomes
    -- the number of creatures THEY control", where "they" is each recipient in
    -- turn rather than any one player.
    --
    -- NOT ControlledBy You, and the difference is a wrong answer rather than a
    -- nicety: CR 109.5's "you" is the spell's controller, so that filter would hand
    -- every seat the controller's own count. Nor ControlledBy Opponent, which names
    -- a set rather than the one seat being looked at.
    --
    -- Context-relative like ControlledByDefendingPlayer above, and by the same
    -- machinery: the answer depends on which recipient the effect has reached
    -- rather than on the candidate, so Pawl.Engine.Filter.Context's `recipient` is
    -- where it arrives, filled by Pawl.Engine.Resolve per recipient and Nothing
    -- everywhere else. Vacuously False there, which is every position but a
    -- per-recipient effect's quantity.
    --
    -- Deliberately NOT PlayerRef.Candidate, which is the word one type over for the
    -- player a Scope.OverPlayers fold is looking at. A fold's candidate SHADOWS
    -- inside the fold and this does not: Arbiter of Knollridge's "the highest life
    -- total among all players" is a fold nested inside a per-recipient set, and one
    -- word for both would make its inner reading ambiguous.
    ControlledByRecipient
  | -- | CR 108.3 / 110.2: the candidate's OWNER relates thus to the perspective --
    -- Garland, Royal Kidnapper's "creatures you control but don't own", which is
    -- `And [ControlledBy You, Not (OwnedBy You)]`.
    --
    -- A SIBLING of ControlledBy above and never derivable from it: CR 110.2 makes
    -- ownership and control independent, and the whole point of the atom is the
    -- board where they disagree. Context-relative in ControlledBy's way -- the atom
    -- carries no PlayerId, and who "you" is comes from the Context (CR 109.5) --
    -- and reading the same PlayerRelation, since CR 108.3's owner is a player like
    -- any other.
    --
    -- ANSWERABLE IN EVERY ZONE, where ControlledBy is not, and that difference is
    -- CR 108.3's rather than an inconsistency: an owner is fixed when the game
    -- starts and no rule changes it, while CR 108.4 gives a card outside the
    -- battlefield and the stack no controller at all. That is manaValue's posture
    -- rather than power's, and Pawl.Engine.Filter.View's `owner` field says so.
    -- Nothing off an OBJECT, though -- a printed card being matched by a search is
    -- not an object and has no owner, so the atom is vacuously False there.
    --
    -- Uncharacteristic, for IsAttacking's reason: CR 109.3's characteristic list
    -- has no owner in it, and no CR 613 layer writes one -- CR 613.1b's layer 2
    -- changes CONTROL and rule 108.3 has no counterpart. So
    -- Pawl.Engine.Projection.filterReads declares it as reading nothing, alongside
    -- IsToken.
    OwnedBy PlayerRelation.PlayerRelation
  | -- | The candidate IS the evaluation's source object. Context-relative like
    -- ControlledBy: the Filter carries no object id, and the answer comes from the
    -- Context. `Not IsSource` is how CR 601.2c's "another" and a continuous
    -- effect's own "each other" card text (Opalescence) are both written -- one
    -- relation, one spelling, rather than a parallel Exclusion field on each
    -- (#163).
    IsSource
  | -- | The candidate IS the object the resolution bound in this slot -- Into the
    -- Wilds' "if it's a land card", where the clause before it looked at the top
    -- card of the library and bound it (Effect.LookAt).
    --
    -- IsSource's sibling: that atom tests the candidate against the evaluation's
    -- source and this one against an object the RESOLUTION named, and neither
    -- carries an id -- both read Pawl.Engine.Filter.Context, this one through
    -- `slotObjects`. So the atom is what lets a Count NARROW a zone to one card
    -- the resolution already named, which is what makes Into the Wilds' count
    -- over a hidden zone (CR 400.2) a question about the card its controller was
    -- shown rather than about the library it sits in.
    --
    -- NOT ControlledByBound, which asks after the bound object's CONTROLLER: CR
    -- 108.4 gives a card in a library none at all, so that atom is vacuously
    -- False for the very candidates this one exists to match.
    --
    -- MEMBERSHIP where the slot names SEVERAL objects: CR 115.10a's group binding
    -- -- the batch a mill (CR 701.17c), a look (CR 701.20e) or a move named --
    -- admits every one of its members, which is what "from among them" asks
    -- (Midnight Tilling). A slot naming one object is the singleton case of the
    -- same read.
    --
    -- Vacuously False where the slot names no object: the map is empty for most
    -- readers, and an illegal target (CR 608.2b) and a multi-TARGET slot drop out
    -- of it -- the one plural shape this does not admit, no card in data/cards/
    -- writing the atom over a slot whose count is above one. That is the posture
    -- every context-relative atom here takes.
    --
    -- READABLE PAST THE RESOLUTION for the one carrier that snapshots the map:
    -- a floating replacement row (Pawl.Types.ActiveReplacement) keeps the
    -- installing resolution's bindings, so a pattern written with this atom names
    -- the object the resolution named rather than a class of them, and CR 400.7h
    -- is what moves that name onto the spell a card cast under the row's source's
    -- permission became.
    IsBound SlotName.SlotName
  | -- | CR 201.2 / 709.4a asked of TWO objects: the candidate shares a name with
    -- the object this slot holds -- Harness the Storm's "target card with the
    -- same name as that spell", where the slot is CR 603.2's own binding of the
    -- cast spell, made as the trigger was gathered and read at CR 601.2c.
    --
    -- HasName's context-relative sibling, standing to it as IsBound stands to
    -- IsSource: that atom carries the name and this one reads it off
    -- Pawl.Engine.Filter.Context, which is where the board-holding caller puts
    -- the bound object's names.
    --
    -- SHARES A NAME, not "has the same set of names": CR 709.4a's test is
    -- membership at both ends, so an object showing two names has the same name
    -- as one showing either. Set INTERSECTION is that said once.
    --
    -- Vacuously False where the slot names no object, or where the bound object
    -- has no name at all (CR 708.2a's face-down object) -- the posture IsBound
    -- above takes, and the answer CR 709.4a itself gives for a nameless object.
    SameNameAsBound SlotName.SlotName
  | -- | CR 115.1: the candidate is a PLAYER who relates thus to the perspective --
    -- "target opponent". Context-relative like ControlledBy, but separate from it
    -- rather than a reuse, because ControlledBy asks who controls an OBJECT
    -- candidate and this asks who the candidate IS. CR 109.1's list of what an
    -- object is has no "player" in it, and CR 108.4 gives a controller only to a
    -- card representing a permanent or spell, so the two are never both answerable
    -- for one candidate.
    IsPlayer PlayerRelation.PlayerRelation
  | -- | The candidate PLAYER is the controller of the object a slot names --
    -- Spikeshell Harrier's "each OTHER player", which is `Not` of this atom over
    -- every player, the other player being the opponent whose permanent the
    -- ability targeted.
    --
    -- IsPlayer's sibling in what it asks of the candidate (who they ARE, not what
    -- they control) and Pawl.Types.PlayerRef.ControllerOfBound's twin one type
    -- over: that reference NAMES the player, this one tests a candidate against
    -- them, and a card excluding them from a fold needs the second. NOT
    -- ControlledByBound, which asks after an OBJECT candidate's controller: a
    -- Scope.OverPlayers candidate is a player, and CR 108.4 gives a player no
    -- controller, so that atom is vacuously False for every one of them.
    --
    -- Answered by REWRITING at Pawl.Engine.Count.bakePerspective, the shape
    -- ControlsMoreThanYou above takes and for its reason: Pawl.Engine.Filter holds
    -- no board and cannot project a controller, while the fold that supplies the
    -- player candidates holds both the board and the view. CR 608.2h reaches it
    -- through that view -- see ControllerOfBound, which carries the argument.
    -- Vacuously False if it survives to Pawl.Engine.Filter.matches, which is every
    -- position but a Scope.OverPlayers count's filter.
    IsControllerOfBound SlotName.SlotName
  | -- | CR 110.2: the candidate PLAYER controls strictly more permanents matching
    -- this filter than the perspective player does (CR 109.5) -- Oreskos
    -- Explorer's "the number of players who control more lands than you", whose
    -- nested filter is `HasCardType Land`.
    --
    -- The one atom that RE-FRAMES the perspective. Every other player-relating
    -- atom here asks how the candidate STANDS to "you" -- a relation, answered off
    -- the candidate alone -- while this asks a question about the candidate's own
    -- board and compares the answer against yours. That is what CR 109.1 makes
    -- awkward: a player is not an object, so Pawl.Engine.Filter.playerView has no
    -- field to read this off, and a board is not a characteristic in any case.
    --
    -- Answered by REWRITING rather than by a View field or a Context one, the
    -- shape ControlledByBound above has: Pawl.Engine.Count.bakePerspective holds
    -- the game state, counts both sides for one candidate, and replaces the atom
    -- with a trivially true or trivially false predicate before the match.
    -- Vacuously False if it survives to Pawl.Engine.Filter.matches, which is every
    -- position but a Pawl.Types.Scope.OverPlayers count's filter -- the only place
    -- the candidate is a player and the only place anything bakes it.
    --
    -- STRICT, which is what lets Oreskos ask it of EVERY player: "more lands than
    -- you" excludes you by arithmetic rather than by a relation, so the card's
    -- scope is EachPlayer rather than an opponent relation, and a seat merely LEVEL
    -- with you is not one that controls more. Surveyor's Scope's "at
    -- least two more lands than you" wants a margin beside the filter; that card
    -- needs a search destination pawl does not have either, so neither half is
    -- built (#1381).
    --
    -- CARRIES A FILTER rather than naming lands: the question is CR 110.2's
    -- control of some described permanent, and the description is a Filter like
    -- any other -- matched against each battlefield permanent through the same
    -- CR 613 projection every other count reads.
    ControlsMoreThanYou (Filter keyword)
  | -- | CR 400.1 / 404.1: the candidate PLAYER's own graveyard holds at least
    -- this many cards -- The Master of Lake-town's "each graveyard with seven or
    -- more cards in it", folded through Scope.OverPlayers EachPlayer.
    --
    -- A PLAYER atom, like the two above and unlike every characteristic atom:
    -- rule 400.1 gives each player their own graveyard, and CR 109.3 counts no
    -- zone among an object's characteristics, so there is nothing to ask of an
    -- object candidate. Vacuously False if it survives to
    -- Pawl.Engine.Filter.matches, the posture ControlsMoreThanYou takes and for
    -- its reason: the answer is a question about the board, and that module holds
    -- none. Pawl.Engine.Count.bakePerspective answers it.
    --
    -- Carries a BARE Natural rather than a Zone, a Comparison or a Count. Not a
    -- Zone, because four of CR 400.1's seven are shared rather than per-player
    -- and no card asks a player fold about the other two, and ManaValueAtMost's
    -- note forbids the unused arms. Not a Count, though that would generalize
    -- zone, comparison and filter at once, because Pawl.Types.Count already
    -- imports this module -- naming it here closes a module cycle, and the
    -- parametricity escape hatch is spent on `keyword`. Natural rather than
    -- Integer because a zone's size cannot be negative:
    -- Pawl.JsonCodec.Common.natural rejects a negative literal at decode, where
    -- Common.integer would accept -1 and yield a vacuously true filter.
    CardsInGraveyardAtLeast Natural.Natural
  | -- | CR 508.1k: the candidate is an ATTACKING creature -- declared as an
    -- attacker this combat phase and not since removed from combat (CR 506.4).
    -- Kill Shot's "target attacking creature".
    --
    -- The first atom reading something CR 109.3 says is NOT a characteristic,
    -- which is no breach of the invariant above: combat status is a RULES concept
    -- the closed half already owns (CR 506-511, Pawl.Types.Combat), so reading it
    -- is the same kind of act as reading a card type.
    IsAttacking
  | -- | CR 509.1g: the candidate is a BLOCKING creature -- declared as a blocker
    -- this combat phase and not since removed under CR 506.4. Labyrinth of
    -- Skophos' "target attacking or blocking creature" is spelled
    -- `Or [IsAttacking, IsBlocking]` rather than given a third atom meaning "in
    -- combat", the two roles being separate rules concepts separate cards ask
    -- about separately. Uncharacteristic for IsAttacking's reason.
    --
    -- NOT the same question as "is something blocking it": Pawl.Engine.Combat.isBlocked
    -- asks whether an ATTACKER has an entry in Combat.blockers, and this asks
    -- whether the candidate is a MEMBER of some attacker's set. CR 509.1h keeps
    -- them apart -- a creature remains blocked once every blocker leaves combat --
    -- so an attacker can be blocked when nothing answers True here.
    IsBlocking
  | -- | CR 509.1h: the candidate is a BLOCKED creature -- an attacking creature
    -- one or more creatures were declared blocking, or that an effect said
    -- becomes blocked. Curtain of Light's "target unblocked attacking creature"
    -- is `And [IsAttacking, Not IsBlocked]`, the one-relation-one-spelling
    -- posture IsToken and IsTapped take. Uncharacteristic for IsAttacking's
    -- reason.
    --
    -- The OTHER side of IsBlocking above, and not derivable from it in either
    -- direction: this is Pawl.Engine.Combat.isBlocked, membership of the KEY of
    -- Combat.blockers, where IsBlocking asks about the sets. CR 509.1h is what
    -- pulls them apart -- a creature stays blocked once every creature blocking
    -- it has left combat, and an effect can confer the status with no blocker
    -- ever assigned.
    IsBlocked
  | -- | CR 608.2i: the candidate was DECLARED as an attacker earlier this turn --
    -- Relentless Assault's "all creatures that attacked this turn". A look-back
    -- read of the turn-scoped GameEvent log, which CR 608.2i sanctions; never a
    -- stamp on the object. Uncharacteristic for IsAttacking's reason.
    --
    -- NOT a synonym for IsAttacking, and not expressible in terms of it:
    -- Combat.attackers is wiped by Combat.clearCombat as the end of combat step
    -- ends (CR 511.3), so a postcombat main phase resolving this spell sees no
    -- attackers in the live record. The event log is the right footing because it
    -- is cleared at turn handoff, exactly the span "this turn" names.
    --
    -- DECLARED, like TriggerCondition.SelfAttacks and for that arm's reason: CR
    -- 508.4 says a creature put onto the battlefield attacking never attacked, and
    -- only Combat.declareAttackers appends GameEvent.AttackerDeclared.
    AttackedThisTurn
  | -- | CR 701.17a: the candidate is a card that was MILLED earlier this turn --
    -- The Master, Transcendent's "target creature card in a graveyard that was
    -- milled this turn". AttackedThisTurn's look-back read of the turn-scoped
    -- GameEvent log, one event arm over (GameEvent.Milled), and uncharacteristic
    -- for IsAttacking's reason: how a card reached the zone it is in is a rules
    -- record the closed half owns, not a characteristic of the card (CR 109.3).
    --
    -- NOT expressible as "moved from a library to a graveyard this turn", which
    -- is the reading the Moved entries would give: The Master's own ruling says a
    -- card put into a graveyard from a library without the word "mill" -- Rowan's
    -- Grim Search -- is not a legal target, and surveil (CR 701.25a) and explore
    -- (CR 701.44a) each bin a card off the top of a library without milling it.
    --
    -- Matches the incarnation the mill LEFT the card as (CR 400.7, CR 701.17c),
    -- so a milled card that has since moved again is no longer one this admits --
    -- the card in the graveyard is a different object from the one that came back
    -- to it.
    MilledThisTurn
  | -- | CR 120.1 / 608.2i: the candidate was DEALT DAMAGE earlier this turn --
    -- Fatal Blow's "destroy target creature that was dealt damage this turn".
    -- AttackedThisTurn's look-back read of the turn-scoped GameEvent log, one
    -- event arm over (GameEvent.DamageDealt), and object-scoped where
    -- Quantity.PlayersDealtDamageThisTurn is the player-scoped half of the same
    -- question.
    --
    -- The CR defines no term for the phrase: rule 702.54a (bloodthirst) is the
    -- only place it appears, where it is ordinary English over rule 120.1's
    -- "objects can deal damage to ... creatures" read back through rule 608.2i.
    --
    -- NOT expressible as Object.damage being positive, which is the reading CR
    -- 120.3e's marks would give: CR 120.6 removes all marked damage when a
    -- permanent regenerates, and CR 120.3d has a wither or infect source mark
    -- nothing at all -- and a creature in either state was still dealt damage
    -- this turn. Marked damage is a strict subset of the question.
    DealtDamageThisTurn
  | -- | CR 303.4b / 701.3a: the candidate is ATTACHED to something the nested
    -- Filter admits -- Crown of the Ages' "target Aura attached to a creature"
    -- (`AttachedTo (HasCardType Creature)`), Aura Graft's "target Aura that's
    -- attached to a permanent" (`AttachedTo (And [])`), and Miracle Worker's
    -- "target Aura attached to a creature you control". Bride's Gown's "as long
    -- as an Equipment named Groom's Finery is attached to a creature you
    -- control" is the same atom in a CR 604.2 condition rather than a target
    -- slot, which is the position the CR 613 layer fold evaluates.
    --
    -- Uncharacteristic for IsAttacking's reason -- attachment is a rules concept
    -- the closed half owns (CR 301.5, 303.4, 701.3, Object.attachedTo) -- while
    -- the nest asks about the HOST's characteristics, evaluated against the
    -- host's own Pawl.Engine.Filter.View.
    --
    -- The nest reads the host and the OUTER context: CR 109.5's "you" is the
    -- ability's controller wherever it appears, so `AttachedTo (ControlledBy
    -- You)` is "attached to something you control" and never "attached to
    -- something its own controller controls". A consequence worth stating:
    -- `AttachedTo IsSource` then asks nearly what IsAttachedToSource below asks,
    -- and differs on exactly one board -- a source that has left the battlefield,
    -- which this atom's own narrowing excludes and that one's does not.
    --
    -- False for a candidate attached to a PLAYER (CR 303.4's other destination)
    -- and for one whose host has left the battlefield (CR 110.1) -- the host
    -- view is filled only for an object that is a permanent, which is the window
    -- CR 704.5m closes on the next state-based-action pass.
    AttachedTo (Filter keyword)
  | -- | CR 303.4b's "enchanted" and CR 301.5a's "equipped", asked of the HOST:
    -- something the nested Filter admits is attached TO the candidate -- A Tale
    -- for the Ages' "enchanted creatures you control", whose affected set is `And
    -- [HasCardType Creature, ControlledBy You, HasAttached (HasSubtype Aura)]`.
    -- The subtype nest is the RULE's word and not a narrowing of ours: rule
    -- 303.4b makes only an Aura's host enchanted, so an equipped creature is not
    -- an enchanted one, and CR 301.5a's "equipped" is this same atom nested on
    -- `HasSubtype Equipment` (Stone Haven Outfitter).
    --
    -- The MIRROR of AttachedTo above, the two roles swapped: that atom asks what
    -- the candidate is attached to, this asks what is attached to the candidate.
    -- Neither expresses the other, because attachment is DIRECTED and a Filter is
    -- evaluated against one candidate -- there is nowhere inside the nest to turn
    -- the arrow around.
    --
    -- ANY, not all, and CR 303.4b is what settles that: a creature is enchanted
    -- once an Aura is attached to it, whatever else is attached alongside. So the
    -- trivial nest `HasAttached (And [])` reads "has something attached to it",
    -- and the atom is False for a candidate carrying nothing.
    --
    -- Uncharacteristic for AttachedTo's reason, and the nest reads the ATTACHER's
    -- characteristics against the OUTER context: CR 109.5's "you" stays the
    -- ability's controller, so `HasAttached (ControlledBy You)` is Archon of the
    -- Wild Rose's "enchanted by Auras you control" rather than a question about
    -- the attacher's own controller.
    --
    -- Narrowed to attachers ON THE BATTLEFIELD, exactly as AttachedTo's host is
    -- and for CR 110.1's reason.
    --
    -- Not implemented: a PLAYER candidate, whom CR 303.4b does let an Aura
    -- enchant -- the atom is False for one, since Pawl.Engine.Filter.playerView
    -- holds no board to find the attachers on (#2030).
    HasAttached (Filter keyword)
  | -- | CR 701.3a / 301.5a: the candidate is attached to the evaluation's SOURCE -- Kemba's
    -- Legion's "for each Equipment attached to this creature", where the Equipment
    -- is the candidate and the creature is the source. "Equipment attached to it"
    -- is `And [HasSubtype Equipment, IsAttachedToSource]`; the subtype conjunct is
    -- the card's word and is not implied here.
    --
    -- Context-relative like IsSource, and the same comparison in the other
    -- direction: IsSource asks whether the candidate IS the source, this whether
    -- its host is, and IsHostOfSource below whether the SOURCE's host is the
    -- candidate. Vacuously False where the candidate is attached to nothing or
    -- to a player (CR 303.4's other destination), and where no source frames the
    -- match.
    --
    -- Nullary rather than an arm of AttachedTo above, and not a synonym for it
    -- either: host IDENTITY is not a host QUALITY, so this reads an ObjectId off
    -- the candidate's own view rather than the host's, and answers without any
    -- projection of the host at all.
    IsAttachedToSource
  | -- | CR 303.4b's "enchanted": the candidate is what the evaluation's SOURCE is
    -- attached to -- Ray of Frost's "enchanted creature", where the Aura is the
    -- source and the creature it enchants is the candidate.
    --
    -- The THIRD attachment direction, and the one the two atoms above cannot
    -- say between them: IsSource asks whether the candidate is the source,
    -- IsAttachedToSource whether the candidate's host is, and this whether the
    -- SOURCE's host is the candidate. Vacuously False where the source is
    -- attached to nothing or to a player (CR 303.4's other destination), and
    -- where no source frames the match.
    --
    -- Nullary for IsAttachedToSource's reason, and reading the mirror-image
    -- field: the host id comes off Pawl.Engine.Filter.Context rather than off
    -- the candidate's view, because it is ONE reading of the source, the same
    -- for every candidate. So this too answers with no projection of anything.
    --
    -- Answerable only where that field is filled, which is the CR 604.2 clause
    -- of a static ability, the CR 603.4 intervening clause of a triggered one,
    -- and an effect's Pawl.Types.ObjectRef; Pawl.CardSpec's position lint is
    -- what keeps a card out of every other position.
    IsHostOfSource
  | -- | CR 701.3a's last sentence: the candidate is one the SUBJECT of the
    -- surrounding attach -- the permanent being moved -- could legally be attached
    -- to. Aura Graft's "another permanent IT CAN ENCHANT".
    --
    -- Context-relative like IsSource, except that the subject arrives through
    -- Pawl.Engine.Filter.View rather than through Context, the answer being
    -- per-candidate and needing the game state. Vacuously False wherever no attach
    -- frames the match.
    --
    -- Asks about the SUBJECT and not about the candidate, which no combination of
    -- the atoms above can express -- a destination filter narrowed by HasCardType
    -- would still admit a creature the Aura's enchant ability rejects. NOT a
    -- restatement of CR 303.4j, which is the backstop for a card that does NOT say
    -- "it can enchant" (Crown of the Ages), where the destination is offered and
    -- the move then fails.
    --
    -- Writing it into any other Filter position is a FAILING TEST rather than a
    -- quiet False: Pawl.CardSpec walks every Filter position a card has and
    -- rejects the atom in all but an attach's destination. The mirror question --
    -- a candidate that could be attached to a fixed host -- is CanAttachToSubject
    -- below rather than a widening of this atom. Answered by
    -- Pawl.Engine.Attach.attachmentFor, so it reads CR 303.4's other limits on
    -- what a permanent can be enchanted by along with the subject's own enchant
    -- ability -- Pawl.AuraSpec's "Aura Graft will not move an Aura onto a land
    -- Consecrate Land protects" is what proves the two arrive together.
    CanHostSubject
  | -- | CR 701.3a from the other side: could THIS CANDIDATE legally be attached
    -- to the object the surrounding instruction fixes? Auratouched Mage's "an
    -- Aura card that could enchant it".
    --
    -- The mirror of CanHostSubject above, with the two roles swapped: there the
    -- fixed object is the permanent being moved and the candidate is a would-be
    -- host; here the fixed object is the HOST and the candidate is what would be
    -- attached to it. "Subject" means the same thing in both -- the object the
    -- instruction fixes, one reading per evaluation -- which is why both arrive
    -- through Pawl.Engine.Filter.View rather than through Context: the answer is
    -- per-candidate and needs the game state.
    --
    -- Not expressible with the atoms above, for CanHostSubject's reason one
    -- direction over: a search narrowed by HasSubtype would still find an Aura
    -- whose enchant ability rejects the host.
    --
    -- Answerable in a SEARCH filter and nowhere else, since
    -- Pawl.Engine.Resolve's Effect.Search arm is the only site that fills the
    -- field -- there the fixed object is the searching ability's own source.
    -- Writing it into any other Filter position is a FAILING TEST rather than a
    -- quiet False, exactly as CanHostSubject's misuse is: Pawl.CardSpec walks
    -- every Filter position a card has and rejects the atom in all but a
    -- search's. Answered by Pawl.Engine.Attach.attachmentFor, the same function
    -- that performs the move, so CR 303.4's other limits on what a permanent can
    -- be enchanted by arrive with the candidate's own enchant ability.
    --
    -- Not implemented: the same question with the host fixed by anything but the
    -- searching ability's source -- Sovereigns of Lost Alara's bound creature,
    -- Bruna's, and Takklemaggot's choose-position reading of CanHostSubject
    -- (#2028).
    CanAttachToSubject
  | -- | CR 111.6: the candidate is a token. Ashaya, Soul of the Wild's "nontoken
    -- creatures you control" is spelled `Not IsToken` -- one relation, one
    -- spelling, the way "another" is spelled `Not IsSource` (#163).
    -- Uncharacteristic for IsAttacking's reason (CR 111, Pawl.Types.Source).
    --
    -- Unlike the other such atoms it is not merely uncharacteristic but IMMUTABLE:
    -- CR 111.3 makes a token's effect-defined values equivalent to printed ones,
    -- so nothing in CR 613 can turn a card into a token or back. That is what lets
    -- Pawl.Engine.Projection.filterReads declare it as reading nothing.
    IsToken
  | -- | CR 110.5: the candidate is tapped. Wood Elemental's "untapped Forests"
    -- is spelled `Not IsTapped`, the one-relation-one-spelling posture IsToken's
    -- comment states (#163).
    --
    -- Uncharacteristic, like IsAttacking and IsToken: CR 110.5 makes tap state a
    -- STATUS rather than a characteristic, so nothing in CR 613 projects it and
    -- Pawl.Engine.Projection.filterReads declares it as reading nothing. Unlike
    -- IsToken it is not immutable -- a permanent taps and untaps constantly --
    -- but mutability is not what filterReads asks about.
    IsTapped
  | -- | CR 701.27g: the candidate is a "transformed permanent" -- a double-faced
    -- permanent on the battlefield with its back face up. Mutagen Connoisseur's
    -- "for each transformed permanent you control", whose "you control" is a
    -- ControlledBy You conjunct beside this atom, the split IsRingBearer's
    -- comment describes.
    --
    -- BOTH of the rule's exclusions are in the answer rather than in the caller.
    -- The first is that a permanent showing its front face is never transformed
    -- "even if it had its back face up previously", so the answer reads the face
    -- the object shows NOW (Pawl.Engine.Game.isFrontFaceUp, off Object.face) and
    -- never Object.turnedOverAt beside it, which records when it last turned and
    -- is what a history reading would consult. The second is that an object
    -- represented by more than one card is never transformed either; it holds
    -- today because no object is, meld being unimplemented -- see #369, whose
    -- lander owes this atom the conjunct.
    --
    -- The battlefield conjunct is the atom's own rather than the surrounding
    -- set's, unlike IsRingBearer's: CR 701.27g states it, and a double-faced
    -- SPELL on the stack cast with its back face up (CR 712.11a) would otherwise
    -- answer True to a Count scoped over some other zone.
    --
    -- Uncharacteristic, like IsToken and IsTapped: CR 712.8d/e make which face is
    -- up the thing a permanent's characteristics are read OFF, so it is not one
    -- of them (CR 109.3), nothing in CR 613 writes it, and
    -- Pawl.Engine.Projection.filterReads declares it as reading nothing.
    Transformed
  | -- | CR 701.54e: the candidate "is your Ring-bearer" -- the permanent carrying
    -- the Ring-bearer designation made for the perspective player. The Ring's own
    -- "YOUR Ring-bearer is legendary" (CR 701.54c), which is rulebook text rather
    -- than a card's, so Pawl.Engine.Ring mints it.
    --
    -- Context-relative like IsSource and ControlledBy: the atom carries no
    -- PlayerId, and whose Ring-bearer is asked comes from the Context's
    -- perspective (CR 109.5). It needs no PlayerRelation payload either, unlike
    -- ControlledBy -- CR 701.54e is only ever asked as "YOUR Ring-bearer", and an
    -- opponent-relative spelling would be an arm no rule and no card asks for.
    --
    -- ONE of CR 701.54e's three conjuncts, not all three: this asks only about the
    -- designation, and the rule's "on the battlefield under your control" is
    -- spelled by the surrounding set -- Affected.Matching's own battlefield gate
    -- and a ControlledBy You conjunct beside this atom. Pawl.Engine.Ring's
    -- isRingBearerOf is the same three conjuncts assembled for a caller with no
    -- Filter in hand.
    --
    -- Uncharacteristic, for IsAttacking's reason: CR 701.54b makes Ring-bearer a
    -- DESIGNATION a permanent has, which CR 109.3's characteristic list has no
    -- room for and which the same rule says is not a copiable value. Like IsToken
    -- it is uncharacteristic AND unwritable by any CR 613 layer, which is what
    -- lets Pawl.Engine.Projection.filterReads declare it as reading nothing.
    IsRingBearer
  | -- | Does the CANDIDATE have this designation? Aragorn, Hornburg Hero's
    -- "whenever a renowned creature you control deals combat damage to a player"
    -- and Rune-Brand Juggler's "sacrifice a suspected creature".
    --
    -- IsRingBearer's shape, and for the same rule-shaped reason: the rules behind
    -- Pawl.Types.Designation make each mark "a designation that has no rules
    -- meaning other than to act as a marker that ... other spells and abilities can
    -- identify", which is what a Filter atom is for. Unlike that one it asks
    -- nothing of the perspective -- none of those designations belongs to a player,
    -- so "you control" is a ControlledBy conjunct beside this atom rather than
    -- something inside it.
    --
    -- NOT Pawl.Types.Quantity.HasDesignation, which asks the same designation of
    -- the object an evaluation is AIMED at (Power's position) for rule 702.112a's
    -- intervening "if". Two readings of one designation, kept apart the way CR
    -- 701.54b's is: a candidate side and a self side.
    --
    -- NOT what CR 701.60c hangs off `Suspected` either: menace and "this creature
    -- can't block" are read off the designation by Pawl.Engine.Projection and
    -- Pawl.Engine.CombatRestriction, and a filter asking for either of those would
    -- match a permanent that got it elsewhere.
    --
    -- Uncharacteristic, for IsRingBearer's reason: each of those rules says the
    -- designation is "neither an ability nor part of the permanent's copiable
    -- values", so no CR 613 layer writes it and
    -- Pawl.Engine.Projection.filterReads declares this atom as reading nothing.
    HasDesignation Designation.Designation
  | -- | CR 122.1: does the CANDIDATE have one or more counters of this kind on it?
    -- Renegade Krasis' "each other creature you control with a +1/+1 counter on
    -- it".
    --
    -- "One or more" and not a count: every printing that asks reads presence, and
    -- a threshold would have to say which comparison it meant. The KIND is a
    -- payload because CR 122.1 makes each kind its own marker.
    --
    -- Uncharacteristic, for HasDesignation's reason: CR 109.3's list has no counters
    -- in it, so no CR 613 layer writes them and
    -- Pawl.Engine.Projection.filterReads declares this as reading nothing. The P/T
    -- a +1/+1 counter grants is CR 613.4c's, which the projection applies over the
    -- top of what this atom reads.
    HasCounters (CounterKind.CounterKind keyword)
  | -- | CR 602.1 / 605.1a: does the CANDIDATE have one or more activated
    -- abilities that aren't mana abilities? Tsabo's Web's "each land with an
    -- activated ability that isn't a mana ability", and Ravager Wurm's second
    -- mode says the same words.
    --
    -- ONE atom for the whole clause rather than an ability atom and a mana
    -- qualifier beside it: CR 605.1a's mana-ability test is about an ability and
    -- not about the object, so there is nothing for a conjunct to be asked of.
    -- Every printing that asks this question asks it with the exclusion already
    -- attached, and the two cards that word it differently
    -- (Magewright's Stone's "with {T} in its cost") ask about the ability's COST,
    -- which is a third question again.
    --
    -- Characteristic, unlike the atoms above it: CR 109.3 counts abilities among
    -- an object's characteristics and CR 613.1f writes them, so
    -- Pawl.Engine.Projection.filterReads declares this atom as reading Keywords
    -- -- Humility takes the abilities away and the atom stops matching.
    --
    -- The abilities it reads are the ones the object HAS, which is not the same
    -- list as the ones it can activate here: CR 702.29b and CR 702.77b keep a
    -- cycling or reinforce ability in existence in every zone while letting it be
    -- activated only from a hand, and this atom is the reader those two rules
    -- were written for.
    HasNonManaActivatedAbility
  | And [Filter keyword]
  | Or [Filter keyword]
  | Not (Filter keyword)
  deriving (Eq, Ord, Show)
