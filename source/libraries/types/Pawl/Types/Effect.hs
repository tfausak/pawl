module Pawl.Types.Effect where

import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown

-- | The ISA (design.md section 1): first-order, non-recursive in CONTROL FLOW
-- -- no branches and no recursive calls -- and with no functions in any field.
-- The ONLY module that may case on a constructor is Pawl.Engine.Resolve; the
-- rules core asks classifications, never identities.
--
-- The one iteration in it is ForEach's, and it is not a loop in that sense: CR
-- 608.2f's per-object processing runs a body over a set SWEPT ONCE before the
-- first pass, so the count is data an analysis can read off the instruction.
--
-- The `card` parameter lets an opcode embed a card's characteristics WITHOUT a
-- module cycle: Card embeds [Effect Card], and ties the knot by instantiating
-- `Effect Card`. The resulting data nesting is structural, not a recursive CALL.
--
-- Two field conventions recur. An opcode takes an ObjectRef rather than a bare
-- SlotName once a card names a SET (Murder beside Day of Judgment); only
-- ObjectRef.InSlot is ever a target (CR 115.10a), and the ones that STORE a
-- continuous effect additionally owe CR 611.2c a frozen sweep, where the
-- one-shots under CR 608.2c/608.2f store nothing. An opcode takes a PlayerRef
-- rather than a SlotName once a rule reaches players the card did not target
-- (Draw's `Relative You` beside Ancestral Recall's `InSlot`).
data Effect card
  = -- | CR 120.1: deal this much damage to what the ObjectRef names. CR 120.1a /
    -- 115.4 let damage reach a PLAYER, which no ObjectRef can name, so the
    -- InSlot arm reads a Recipient rather than an ObjectId. WHO deals it is the
    -- payload's `dealer` (CR 120.2b), absent where CR 113.7's resolving source is
    -- it and present for Rabid Bite.
    DealDamage DealDamage.DealDamage
  | -- | CR 701.14: the two slots' creatures fight -- "each of those creatures
    -- deals damage equal to its power to the other creature" (CR 701.14a), and
    -- that damage is not combat damage (CR 701.14d).
    --
    -- A separate opcode rather than a pair of DealDamage instructions because CR
    -- 701.14b is a condition on the PAIR: if either creature is gone or is no
    -- longer a creature, NEITHER deals damage. Two DealDamage instructions would
    -- each answer for themselves and let the survivor hit alone.
    --
    -- No Quantity field: CR 701.14a fixes the amount at each creature's own
    -- power, so nothing about it is the card's to say.
    Fight Fight.Fight
  | -- | CR 611: create a continuous effect on the objects the ObjectRef names,
    -- for a duration. Resolve stores the Modification without ever casing on it
    -- -- or, when a quantity inside it has no answer at resolution (CR 608.2h),
    -- stores nothing rather than a value to re-read later.
    --
    -- CR 611.2c fixes the affected set when the effect begins, so Resolve sweeps
    -- ONCE and freezes the result as Affected.TheseObjects -- never the Filter,
    -- which re-evaluated per projection would pump a creature that became
    -- attacking later.
    ModifyTarget ModifyTarget.ModifyTarget
  | -- | CR 612: rewrite subtype words in the target spell or permanent. The
    -- SubtypeFamily is which words the card's own text names, and so which words
    -- the player is asked for; the Set is what the NEW word may not be. Resolve
    -- announces the pair as the effect applies (CR 608.2d) and bakes it into a
    -- stored ChangeSubtypeWord.
    --
    -- The family is not stored alongside the pair: CR 612.2's gate reads the
    -- family of the word being REPLACED, which Pawl.Engine.Subtype answers from
    -- the word itself.
    ChangeText ChangeText.ChangeText
  | -- | CR 605: one player adds ONE unit of mana, of the type the payload's
    -- ManaProduction names -- one fixed type, or one colour its controller
    -- chooses (CR 105.4). A mode adding more holds the opcode more than once,
    -- and Mana.manaRoutesOfGiven reads a mode's whole list as one activation's
    -- yield. WHICH player is written rather than assumed because CR 106.4 puts
    -- the mana in "a player's mana pool" without saying whose.
    --
    -- Two routes reach a pool. A MANA ABILITY's is Cost.tapForMana at payment
    -- (CR 605.3b), which reads the ManaProduction alone and ignores the rest of
    -- the payload -- the recipient (#1673), the retention (#1808) and CR 106.6's
    -- spending restriction (#1976); everything else resolves through
    -- Resolve.applyEffect, which reads the whole record.
    AddMana ManaAddition.ManaAddition
  | -- | CR 701.23: the players Search.searcher names each search the library of
    -- each player Search.owner names, for Search.quantity cards matching
    -- Search.filter, put them where Search.destination says, then that library's
    -- owner shuffles. The Filter is evaluated over each card's own CR 613
    -- projection (Projection.viewOfObject).
    --
    -- TWO refs, because CR 701.23a's looking and CR 400.1's ownership are
    -- independent (Extract's `You`/`InSlot`). The SEARCHER looks, answers CR
    -- 101.2's prohibition, is offered CR 601.3's cast over their own library and
    -- performs the CR 701.23e-silent placement; the OWNER's library is read and
    -- shuffled.
    --
    -- Whether the Quantity is a CEILING or a QUOTA is read off the Filter rather
    -- than stored, CR 701.23b letting a search STATING A QUALITY find fewer where
    -- CR 701.23d makes a bare quantity find that many. Filter.statesAQuality
    -- classifies; Search.upTo is the one case it cannot reach.
    --
    -- Not implemented: a search of any zone but a library (#1318).
    Search Search.Search
  | -- | CR 701.13 / Rest in Peace: exile every card in every graveyard.
    -- Targetless and bulk; a general exile-from-zone is future.
    ExileAllGraveyards
  | -- | CR 727.1/727.1a: restart the game. The starting player of the new game
    -- is the resolving controller, so no slot is needed.
    --
    -- The ObjectRef is CR 727.5's exemption and names the cards that skip the
    -- rebuild, staying in exile; optional, since a card saying nothing about it
    -- exempts nothing. A ref rather than a Filter because Karn Liberated picks by
    -- CR 607.2a linkage as well as by characteristics.
    RestartGame (Maybe ObjectRef.ObjectRef)
  | -- | CR 723.1: you control target player during that player's next turn.
    -- Installs pending control keyed to the slot's chosen player, with the
    -- ability's controller as the decider. Mindslaver's shape.
    ControlPlayerNextTurn SlotName.SlotName
  | -- | CR 701.8 / 702.12b: destroy the permanents the ObjectRef names -- move
    -- each to its owner's graveyard via the changeZone funnel UNLESS it is
    -- indestructible. NOT MoveToZone Graveyard: the destruction is itself
    -- interceptable, Pawl.Engine.Event.destroy offering a WouldBeDestroyed event
    -- to the CR 616.1 loop first, which is how a regeneration shield (CR 701.19a)
    -- catches it. The Regenerability is CR 701.19c's rider, carried by the
    -- destruction rather than the victim -- Terror has it and CR 704.5g's
    -- state-based action does not, for the same creature.
    --
    -- Three optional slots BIND what this destruction leaves a later effect of
    -- the same resolution to look back at: how many permanents it ACTUALLY
    -- destroyed (Bane of Progress, read as Quantity.InSlot), the cards it put
    -- into a graveyard (Come Back Wrong) and the PERMANENTS it destroyed
    -- (Rampage of the Clans' "for each permanent destroyed this way, its
    -- controller ...", walked by ForEach below). None of them is "matched by the
    -- ObjectRef": all three come out of Event.destroyReturning. Definitions,
    -- never targets. Pawl.Types.Destroy is where each is spelled out.
    Destroy Destroy.Destroy
  | -- | CR 701.21/701.21a: the slot's target permanent is sacrificed -- its
    -- CONTROLLER moves it to its OWNER's graveyard. NOT a destruction, which CR
    -- 701.21a says outright, so this consults neither indestructible nor a
    -- regeneration shield. "This creature" needs no separate opcode:
    -- Engine.placeOne binds the trigger's SOURCE into the reserved
    -- Pawl.Engine.Binding.triggerSource slot.
    Sacrifice SlotName.SlotName
  | -- | CR 701.3 / 702.6a: attach THIS permanent (the effect's source) to the
    -- slot's target, so the Equipment is the source and the only slot is what it
    -- attaches TO. CR 701.3a moves an already-attached source and CR 701.3c
    -- restamps it; CR 701.3b leaves it put if it cannot legally be attached.
    Attach SlotName.SlotName
  | -- | CR 701.3 / 303.4j: attach the SLOT'S TARGET -- an Aura, Equipment or
    -- Fortification already on the battlefield -- to an object chosen as this
    -- resolves (Crown of the Ages). Attach's mirror, differing in WHAT MOVES:
    -- this targets the thing that moves and not the destination, so the
    -- destination is a resolution-time choice outside CR 608.2b and a bare
    -- Filter.
    --
    -- The Filter is the destination's card text; Aura Graft's "another permanent
    -- IT CAN ENCHANT" is `Filter.CanHostSubject`. The "another" is NOT in it, CR
    -- 701.3b making attachment to the current host a no-op whatever the card
    -- says. An illegal destination leaves the subject where it was, unrestamped.
    AttachTarget AttachTarget.AttachTarget
  | -- | CR 303.4d / CR 301.5c: attach the SLOT'S TARGET to EACH permanent the
    -- Filter matches, rather than to one of them. Both rules end the same
    -- sentence -- an Aura can't enchant more than one object or player, an
    -- Equipment can't equip more than one creature, and in each case the
    -- ATTACHMENT's controller chooses which one it ends up on -- so this opcode
    -- names the whole set and Pawl.Engine.Attach.arbitrate reduces it.
    --
    -- AttachTarget's sibling, and the payload is the same record read a second
    -- way: there the Filter's matches are the menu the resolving controller
    -- picks from (CR 608.2d), here they are all named at once and the choice
    -- moves to the subject's controller. That reassignment is the only thing
    -- observable about this opcode, and it needs two seats to see.
    AttachTargetToEach AttachTarget.AttachTarget
  | -- | CR 400.7: move the objects the ObjectRef names to a zone through the
    -- changeZone funnel. The destination is data, so this is one opcode for every
    -- zone move; Hand is owner-relative, changeZone carrying Object.owner.
    -- Distinct from Destroy, which checks indestructible.
    --
    -- The EntryRiders are what the effect says about the object AS IT ENTERS the
    -- battlefield, beyond its own text (Meandering Towershell's "tapped and
    -- attacking"), shared with Create -- CR 109.3 makes neither a
    -- characteristic. Inert for any other destination.
    --
    -- The Maybe SlotName BINDS the incarnations CR 400.7 mints at the
    -- DESTINATION (CR 400.7j), so the rest of this effect and any delayed ability
    -- it arms can name the object, the old id being gone. A definition, never a
    -- target.
    --
    -- The trailing Maybe Zone is the zone the effect's own words say the object
    -- is moved OUT of (Reassembling Skeleton's "from your graveyard"). It exists
    -- for CR 113.6m, a CLASSIFICATION (Pawl.Engine.EffectZone) that can only
    -- report a zone the data states; the resolver ignores it, and EffectZone
    -- answers Nothing for EachMatching, that rule asking about one object.
    --
    -- The LibraryPlacement is the END a LIBRARY destination arrives at (CR
    -- 401.2), either stated (Griptide) or left to each moved object's OWNER
    -- (Aetherspouts); it also says whether CR 401.4's arrangement of two or more
    -- simultaneous arrivals is the owner's or a random one (Endurance). Inert
    -- elsewhere; a CardSpec lint forbids OwnerChooses there, that one asking a
    -- question with no board behind it.
    MoveToZone MoveToZone.MoveToZone
  | -- | CR 121.1: the players the PlayerRef names each draw this many cards, one
    -- at a time (CR 121.2). Empty-library draw is a loss (CR 104.3c), unlike
    -- Mill -- the asymmetry that keeps the two separate.
    Draw PlayerQuantity.PlayerQuantity
  | -- | CR 701.17: the players the PlayerRef names each mill this many. A short
    -- or empty library mills fewer, no penalty (CR 701.17b) -- unlike Draw, which
    -- loses.
    --
    -- The MillTally is "and remember how many of them counted", for a later
    -- effect to read as Quantity.InSlot (CR 728.1's "for each nonland card milled
    -- this way"). Nothing for a mill nothing looks back at.
    Mill Mill.Mill
  | -- | CR 701.20a: the cards the ObjectRef names are REVEALED -- shown to ALL
    -- players -- and nothing else happens. The public counterpart of LookAt, and
    -- the pair is CR 701.20e's own distinction. Nothing moves either way (CR
    -- 701.20b).
    --
    -- AN OPTIONAL SLOT, where LookAt's is required: the public
    -- GameEvent.Revealed this appends is already the whole record, where LookAt's
    -- binding is all it leaves.
    --
    -- Not implemented: rule 701.20a keeps a revealed card revealed "for as long
    -- as necessary", which pawl has nowhere to store (#1408).
    Reveal Reveal.Reveal
  | -- | CR 701.20e: the cards the ObjectRef names are LOOKED AT -- shown to one
    -- player rather than all -- and bound into the slot, so a later clause of the
    -- same resolution can act on what was seen (Into the Wilds).
    --
    -- NOTHING MOVES and nothing is recorded: rule 701.20e reaches CR 701.20b
    -- through "the same rules as revealing a card", and GameEvent.Revealed would
    -- be the wrong event, that one being public.
    --
    -- The ObjectRef is what makes this reusable where Scry's look is not:
    -- ObjectRef.TopOfLibrary names a card in a library by POSITION, the one thing
    -- no Filter can say about a hidden zone (CR 400.2).
    --
    -- A look naming SEVERAL cards binds them as a GROUP, which CR 701.20e's "from
    -- among them" reads back through Filter.IsBound.
    --
    -- Not implemented: no seat is shown anything, there being no per-player view
    -- of the state (#1412), so the binding is the whole of the look and WHO looks
    -- is not carried.
    LookAt LookAt.LookAt
  | -- | CR 701.22a: the players the PlayerRef names each scry this many -- look
    -- at the top N of their own library, then put any number on the bottom in any
    -- order and the rest back on top in any order.
    --
    -- The ORDERED PARTITION is the player's: Prompt.ChooseScry asks for both ends
    -- and both orders. Not asked for CR 701.22b's scry 0, an empty library, or
    -- the one card that IS the whole library.
    --
    -- No zone change and so no CR 400.7 incarnation: rule 701.22a reorders WITHIN
    -- one library (CR 701.20b via CR 701.20e), so Resolve rewrites
    -- GameState.library directly.
    --
    -- The LOOK is the prompt, so no GameEvent is recorded. Not the LookAt arm
    -- either: scry's look and its ordered partition are one instruction, so the
    -- cards looked at are the candidate list of the prompt that decides where
    -- they go. CR 701.22d's trigger reads GameEvent.Scried, which
    -- Pawl.Engine.Resolve.scryOne records outside the prompt guard.
    Scry PlayerQuantity.PlayerQuantity
  | -- | CR 701.25a: the players the PlayerRef names each surveil this many, the
    -- unwanted cards going to their GRAVEYARD rather than the bottom of the
    -- library.
    --
    -- CROSSES A ZONE BOUNDARY, where Scry does not: the graveyard half is
    -- funnelled through Event.changeZone, so each card put there mints a CR 400.7
    -- incarnation, and only the kept half is a library rewrite. The elision is
    -- Scry's minus one case -- a lone card that is the whole library IS a real
    -- question here. CR 701.25c's surveil 0 and an empty library ask nothing.
    --
    -- Not implemented: CR 701.25b's "additional cards" rider (#1343).
    Surveil PlayerQuantity.PlayerQuantity
  | -- | CR 701.29a: the players the PlayerRef names each fateseal this many --
    -- Scry's library rewrite over AN OPPONENT'S library. The asymmetry is the
    -- whole rule: the fatesealer is shown the cards and answers
    -- Prompt.ChooseFateseal, while the library's owner is shown nothing. The
    -- PlayerRef names the FATESEALER; WHICH opponent is a second choice, asked as
    -- Prompt.ChooseOpponent as the effect applies.
    --
    -- Rule 701.29 is one sentence, so there is no zero case, no "even if
    -- impossible" clause and no additional-cards rider, unlike CR 701.22 and CR
    -- 701.25.
    Fateseal PlayerQuantity.PlayerQuantity
  | -- | CR 701.44a: the permanents the ObjectRef names each explore. Each one's
    -- controller reveals the top card of their library; a land card goes to their
    -- hand, and otherwise a +1/+1 counter goes on the exploring permanent and
    -- they MAY put the revealed card into their graveyard.
    --
    -- ONE opcode rather than Reveal plus a condition and a branch. Rule 701.44 is
    -- part of the rulebook, so reading "is a land card" here is the same kind of
    -- act as Pawl.Engine.Keyword casing on CR 702's keywords; LookAt's branch
    -- does live in the card, rule 701.20e's look not being a keyword action.
    --
    -- The REVEAL is public (CR 701.20a); the CHOICE is Prompt.ChooseExplore's. CR
    -- 701.44b makes the permanent explore even where the actions were impossible,
    -- which is why an empty library still reaches the counter, and CR 701.44c
    -- reads the controller from last known information.
    --
    -- Not implemented: CR 701.44d's choice of WHICH of several simultaneous
    -- explores goes first (#1345).
    Explore ObjectRef.ObjectRef
  | -- | CR 701.9: the slot's target player discards this many. The DISCARDING
    -- player chooses which (CR 701.9b) via Prompt.ChooseDiscard. A hand smaller
    -- than the count discards all of it (CR 609.3), forced, so it is not
    -- prompted.
    Discard Discard.Discard
  | -- | CR 119.3: the players the PlayerRef names each lose this much life. CR
    -- 704.5a's state-based action may follow, from Pawl.Engine.Sba.
    --
    -- NOT a DealDamage aimed at a player. CR 119.2 runs one way only, so the
    -- damage funnel would wrongly subject life loss to CR 614/615's replacement
    -- and prevention, to infect's CR 120.3b diversion and to toxic's CR 120.3g
    -- rider, and would append a GameEvent.DamageDealt. GainLife is a separate
    -- opcode rather than a signed amount, the two being distinct trigger events.
    LoseLife PlayerQuantity.PlayerQuantity
  | -- | CR 119.3: the players the PlayerRef names each gain this much life.
    -- LoseLife's mirror but for the sign, and separate from it for that arm's
    -- reason. No state-based action follows a gain (CR 704.5a is about a total of
    -- 0 or less).
    GainLife PlayerQuantity.PlayerQuantity
  | -- | CR 701.12c: the two players the ExchangeSides names exchange life totals
    -- -- Mirror Universe's controller and its target (WithController), or Soul
    -- Conduit's "two target players" (BetweenTargets). Each gains or loses
    -- whatever it takes to reach the other's PREVIOUS total, which is why a card
    -- cannot spell this with a GainLife and a LoseLife: the second would read a
    -- total the first had already overwritten. Not a PlayerRef, an exchange
    -- having exactly two sides.
    ExchangeLifeTotals ExchangeSides.ExchangeSides
  | -- | CR 119.5: the players the PlayerRef names each end up with this life
    -- total -- Magister Sphinx' `InSlot` over a Literal, Arbiter of Knollridge's
    -- `EachPlayer` over a fold.
    --
    -- Not expressible as a GainLife or a LoseLife: the amount is the DIFFERENCE
    -- between a total nobody wrote down and the new one, and its sign varies per
    -- player. Not a raw write either -- CR 119.5 says the player "gains or loses
    -- the necessary amount of life", so this resolves into the same CR 608.2i
    -- events a life-gain trigger reads.
    SetLifeTotal PlayerQuantity.PlayerQuantity
  | -- | Reverse the Sands' "redistribute any number of players' life totals": the
    -- resolving controller chooses any number of the players in the game and
    -- hands each of them one of THOSE players' previous totals, using each total
    -- exactly once. CR 119.7 and CR 119.8 name the action; every seat's new total
    -- is a gain or a loss of the necessary amount, as SetLifeTotal's CR 119.5
    -- makes it.
    --
    -- CHOOSE, not target, and so no SlotName: the whole assignment is picked on
    -- RESOLUTION via Prompt.ChooseRedistribution. Nullary, the printed words
    -- leaving nothing for an author to vary.
    --
    -- NOT a composition of SetLifeTotals: each would read a total an earlier one
    -- had overwritten, and no card data could name the permutation a player picks
    -- while the spell resolves.
    RedistributeLifeTotals
  | -- | CR 702.179c: the players the PlayerRef names each have their speed
    -- increased by this much -- "if a player has no speed and they are instructed
    -- to increase their speed by a certain value, their speed becomes that
    -- value", which is why the rule needs an opcode rather than a plain addition
    -- against a stored zero. NOT a "set speed to" opcode: CR 702.179b's set is
    -- the rules core's own (Pawl.Engine.Speed's startEngines, CR 704.5aa).
    IncreaseSpeed PlayerQuantity.PlayerQuantity
  | -- | The other direction: the named players each have their speed reduced by
    -- this much, never below the floor beside it (Spikeshell Harrier).
    --
    -- NOT IncreaseSpeed with a negated Quantity: CR 702.179c makes an increase
    -- CREATE a speed for a player who has none, and no rule says the same of a
    -- decrease. The floor is the card's own sentence, and IncreaseSpeed has
    -- nowhere to put it; rule 702.179 bounds speed in neither direction, so no
    -- other bound applies. Whether an effect may push speed past 4 is unsettled
    -- (#809) and is IncreaseSpeed's question.
    DecreaseSpeed SpeedDecrease.SpeedDecrease
  | -- | CR 111: create this many tokens with the given effect-defined
    -- characteristics (CR 111.3). The `card` is the token's text, embedded
    -- literally; Create (Literal 2) mints two distinct objects. Targetless and
    -- unprompted. NOT a copy-token (CR 707) and NOT a predefined token (CR
    -- 111.10): given, not derived.
    --
    -- Create.riders is what the effect says about the tokens beyond their text
    -- (Hanweir Garrison's "tapped and attacking"), outside the embedded card
    -- because CR 109.3 makes it no characteristic.
    --
    -- Create.slot BINDS what this Create minted into the resolving object's LIVE
    -- bindings, so a delayed ability armed by this resolution can name it. A
    -- definition, never a target. WHAT it binds is decided by the PRINTED
    -- Create.quantity, the only thing that can tell CR 603.7c's singular "it"
    -- from a card's plural "those tokens": Literal 1 binds the one token, and if
    -- CR 614.16 multiplied the count asks which of them "it" names, while any
    -- other quantity binds every token. See Resolve.namesEveryToken.
    Create (Create.Create card)
  | -- | CR 707.1 / 111.3: create this many tokens that are copies of each object
    -- the ObjectRef names (Cackling Counterpart, Rite of Replication). Create's
    -- sibling and not a case of it: the token's text is DERIVED from a permanent's
    -- copiable values (CR 707.2) rather than given as a literal card.
    --
    -- The Quantity counts tokens PER named object, and they enter SIMULTANEOUSLY
    -- (Event.createTokens takes the count), which is what CR 614.12's entry loop
    -- and CR 616.1g's containment are asked about.
    --
    -- Not implemented: EntryRiders and a bound slot, each with a real printing
    -- behind it -- Kiki-Jiki's haste-and-sacrifice, Ochre Jelly's counters
    -- (#1255).
    CreateCopy CreateCopy.CreateCopy
  | -- | CR 707.4 / 613.1a: make a permanent ALREADY ON THE BATTLEFIELD a copy of
    -- another object (Unstable Shapeshifter). CreateCopy mints a new object off
    -- an existing one's copiable values; this rewrites an existing object's.
    --
    -- Writes the copiable values THEMSELVES (CR 707.2), stamping the subject's
    -- copy snapshot -- the same place the CR 707.5 entry replacement and
    -- CreateCopy write, so all three converge on
    -- Projection.copiableCharacteristics and CR 707.3 holds for free. CR 707.4's
    -- two riders fall out of that: nothing moves zones, and layers 2-7 re-apply
    -- over the new layer-1 base.
    --
    -- Not implemented: the CR 707.9a exception every printed producer carries
    -- ("except it has this ability"), so pawl's Shapeshifter loses its own trigger
    -- as it copies and cannot copy again (#1292); and a stated duration (#1753).
    BecomeCopy BecomeCopy.BecomeCopy
  | -- | CR 614.3 / 615.3: install a floating replacement effect for a duration,
    -- with a use count, an origin and an optional condition. Fog and Drudge
    -- Skeletons' regeneration are both this opcode, differing only in the
    -- payload's event class. Targetless -- a floating replacement watches a class
    -- of events, and where the printed sentence is about ONE object it says so
    -- with a Filter.IsBound over a slot an earlier effect of the same resolution
    -- defined (CR 400.7j / 400.7h; Dire Fleet Daredevil's "that spell"), which
    -- ActiveReplacement.slots is what answers. Resolve stores the row into
    -- GameState.replacements with this effect's SOURCE (CR 113.7).
    --
    -- The ReplacementOrigin is CR 614.15's self-replacement bit. The Condition
    -- rides the installed row and is asked as the event would happen (CR 614.1),
    -- never latched on resolution -- see Pawl.Types.Replace.
    --
    -- A field rather than a general conditional Effect arm: this rides ONE
    -- opcode's ONE object, so the effect list stays a straight-line sequence,
    -- where an `If` arm would put a BRANCH between two effect lists. NOT
    -- Pawl.Types.Clause.condition, which gates whether a clause runs at all.
    Replace (Replace.Replace (Effect card))
  | -- | CR 614.10a: each player the PlayerRef names skips their NEXT occurrence
    -- of this step or phase. Fatigue names a step; Stonehorn Dignitary names a
    -- whole phase (CR 500.1).
    --
    -- NOT a Replace carrying a PhaseR, though CR 614.1b makes this a replacement
    -- effect: the pattern would have to name a player known only at resolution,
    -- which a ReplacementEffect written on a card cannot. No Duration and no
    -- Uses, CR 614.10a's "next" being the use count, so Resolve installs one
    -- floating replacement per named player with Uses.Once and Expiry.Never.
    SkipNextPhase SkipNextPhase.SkipNextPhase
  | -- | CR 615.7: install a prevention SHIELD over the recipients an ObjectRef
    -- names, for a duration (Mending Hands). The quantity is the shield's printed
    -- size, which then counts DAMAGE down (Pawl.Types.DamageRewrite.PreventNext).
    -- DealDamage's ObjectRef, because CR 115.4's "any target" reaches a PLAYER.
    -- One shield per recipient, CR 615.11's shape for free.
    --
    -- NOT a Replace carrying a DamageR: the pattern must name the shielded
    -- permanent OR PLAYER, known only at resolution, so Resolve bakes the
    -- Recipient into DamagePattern.whichRecipient. A Filter.IsBound over a slot
    -- would reach the permanent half (that is how Replace's own ZoneChangeR names
    -- one object), and no Filter names a player, so the two halves would need two
    -- spellings where the baked Recipient is one. A Duration, CR 615.3 giving a
    -- prevention effect two terminators; no Uses field, the shield being spent in
    -- damage rather than applications.
    --
    -- PreventNextDamage.riders is CR 615.5's ADDITIONAL EFFECT (Test of Faith).
    -- It rides the shield rather than being a sibling effect because it fires
    -- when the shield does, possibly turns later, and reads what that application
    -- prevented (Pawl.Engine.Binding.eventAmount).
    --
    -- Not implemented: a shield naming more than one recipient (gap #1108).
    PreventNextDamage (PreventNextDamage.PreventNextDamage (Effect card))
  | -- | CR 615.1 / 615.3: install an UNBOUNDED prevention shield over the
    -- recipients an ObjectRef names, for a duration (Selfless Squire).
    --
    -- PreventNextDamage with the Quantity removed, and the missing field is the
    -- whole difference: this has no amount to spend and ends only when its
    -- duration does, hence a DamageRewrite.PreventAll rather than a PreventNext
    -- of some large number. With no running count, CR 615.5's "the damage
    -- prevented this way" is per APPLICATION here (Brace for Impact).
    PreventAllDamage (PreventAllDamage.PreventAllDamage (Effect card))
  | -- | CR 614.9: install a floating REDIRECTION effect (Turn the Tables).
    -- RedirectDamage.from is the damage's original recipient,
    -- RedirectDamage.to where it goes instead. NOT a Replace carrying a DamageR,
    -- for PreventNextDamage's reason doubled: BOTH sides are known only at
    -- resolution.
    --
    -- The Maybe DamageKind is PRINTED, not assumed: Turn the Tables says "all
    -- COMBAT damage", and an opcode without the field would redirect its
    -- controller's noncombat damage away too, weaker than printed. Nothing means
    -- any kind.
    RedirectDamage RedirectDamage.RedirectDamage
  | -- | CR 701.6/701.6a: counter the objects the ObjectRef names via the
    -- Event.counter funnel. ONE opcode for both of that rule's subjects: which
    -- ending the countering has (the owner's graveyard for a spell, CR 608.2n's
    -- cease for an ability) is the funnel's own classification of what it is
    -- handed.
    --
    -- Distinct from MoveToZone Graveyard the way Destroy is: it carries the
    -- funnel's can't-be-countered gates (CR 113.6g, CR 613.11) and records for a
    -- SPELL a distinct "was countered" event. The optional slot is how many the
    -- funnel actually countered, for Swift Silence's "for each spell countered
    -- this way" to read as Quantity.InSlot.
    Counter Counter.Counter
  | -- | CR 122.6: put this many counters of this kind on the permanents the
    -- ObjectRef names. A counter is persistent object state, NOT a zone change --
    -- Resolve.applyEffect edits Object.counters in place. The counter's P/T
    -- effect is the projection's (CR 122.1a / 613.4c), not this opcode's. Each
    -- named permanent gets its OWN call to Event.putCounters, because CR 614.16
    -- replaces one placement at a time.
    PutCounters PutCounters.PutCounters
  | -- | CR 122: remove this many counters of this kind from the slot's target
    -- permanent. PutCounters' mirror, and separate rather than one signed amount:
    -- CR 122.7's "when the Nth counter is put on" reads only the putting
    -- direction.
    --
    -- Asking for more than are present removes the ones that are there and no
    -- more; CR 122 states no rule making the instruction fail. Passes through no
    -- CR 614.16 gate -- that rule replaces a PLACEMENT.
    RemoveCounters RemoveCounters.RemoveCounters
  | -- | CR 122 / 107.14: the players the PlayerRef names each get N counters of a
    -- player-counter kind. Subsumes any self-scoped player counter (energy,
    -- experience, rad) as `Relative You`.
    --
    -- PlayerRef and not PlayerScope, since only PlayerRef can name a binding
    -- slot: CR 702.70a's poison counters go to `InSlot Binding.triggerPlayer`.
    -- Targetless in itself, though a slot this reads may have been filled by
    -- TARGETING (CR 601.2c).
    GainPlayerCounters PlayerCounters.PlayerCounters
  | -- | CR 122: the players the PlayerRef names each LOSE N counters of a
    -- player-counter kind -- CR 728.1's "removes one rad counter from
    -- themselves". GainPlayerCounters' mirror, separate for LoseLife's reason.
    --
    -- Removing more than the player has removes what they have and no more, the
    -- count being a Natural and CR 122 knowing no negative counter, rather than
    -- being an error or a no-op.
    RemovePlayerCounters PlayerCounters.PlayerCounters
  | -- | CR 107.14: "you may pay any amount of {E}" -- the resolving controller
    -- names an amount as the spell or ability resolves, removes that many energy
    -- counters from themselves, and the amount is bound to this SlotName for a
    -- later effect of the same resolution to read as Quantity.InSlot. Harnessed
    -- Lightning's "then you may pay any amount of {E}. Harnessed Lightning deals
    -- that much damage to that creature" is the printing.
    --
    -- NOT a Pawl.Types.CostComponent, and that is the whole reason it is an
    -- effect: CR 118.1 makes a cost "an action or payment necessary to take
    -- another action", and nothing on this card is gated on the payment -- pay
    -- nothing and the damage clause still runs, dealing 0. CR 118.12's gate is
    -- the other shape and prints an "If you do" (Aether Refinery), which
    -- Pawl.Types.PayGate already carries. So this opcode neither reaches
    -- Pawl.Engine.Cost.canPay nor takes CR 118.12's branch.
    --
    -- NOT RemovePlayerCounters above with some "chosen" Quantity: a Quantity is
    -- EVALUATED against the game, and this amount is ASKED FOR (CR 107.14 read
    -- through CR 118.3, which caps it at what the payer has). Folding a prompt
    -- into Quantity would put a choice inside the vocabulary every quantity read
    -- shares.
    --
    -- The payer is the resolving controller (CR 109.5's "you") rather than a
    -- PlayerRef, and no printing separates the two: Scryfall
    -- o:"pay any amount of {E}" and o:"pay one or more {E}", 2026-08-19, name
    -- "you" on every hit (Die Young, Harnessed Lightning, Galvanic Discharge,
    -- Aether Spike, Wheel of Potential, Wrath of the Skies, Suppression Ray,
    -- Vault 112: Sadistic Simulation, Aether Refinery, Localized Destruction,
    -- Pia Nalaar, Chief Mechanic). A card offering the payment to somebody else
    -- would be a PlayerRef field here, and Aether Spike is the card to check
    -- first, its OTHER payment ("unless its controller pays {1} for each {E}
    -- paid this way") being a CR 118.12 gate on a different player.
    --
    -- The "MAY" is subsumed rather than carried as a second decision: the
    -- printed amount is "ANY amount", zero included, so declining and paying
    -- nothing are the same answer. Not implemented: "pay ONE OR MORE {E}", whose
    -- floor is 1 and whose "If you do" is CR 118.12's branch (#1919).
    --
    -- The slot is not optional. Every printing reads the amount back ("that
    -- much", "for each {E} paid this way", "the amount of {E} paid this way"),
    -- which is what makes the payment worth stating at all.
    PayAnyEnergy SlotName.SlotName
  | -- | CR 701.26a: tap the permanents the ObjectRef names. A permanent that is
    -- ALREADY tapped is left alone, which is that rule's second sentence and
    -- falls out of the resolution being an assignment to TapState.Tapped.
    Tap ObjectRef.ObjectRef
  | -- | CR 701.26b: untap the permanents the ObjectRef names.
    Untap ObjectRef.ObjectRef
  | -- | CR 701.35a: detain the permanents the ObjectRef names.
    --
    -- NO DURATION beside it, rule 701.35a fixing it, so a Duration here would let
    -- a card file contradict the rulebook. ONE opcode for a sentence with three
    -- limbs (can't attack, can't block, activated abilities can't be activated),
    -- which Pawl.Engine.Detain reads apart.
    Detain ObjectRef.ObjectRef
  | -- | CR 701.15a: goad the permanents the ObjectRef names, until the next turn
    -- of this resolution's controller.
    --
    -- NO DURATION and no goader beside it, Detain's reasons unchanged: rule
    -- 701.15a fixes the duration, and the goader is CR 109.5's "you". ONE opcode
    -- for CR 701.15b's two requirements (attacks each combat if able, attacks a
    -- player other than the goader if able), which
    -- Pawl.Engine.AttackRequirement reads apart -- so no card file states either
    -- half and no card file can state one without the other.
    Goad ObjectRef.ObjectRef
  | -- | CR 502.3 / 611.2: the permanents the ObjectRef names don't untap during
    -- their controller's NEXT untap step (Elvish Hunter). CR 701.43a's exert is
    -- NOT this opcode: it names the exerting player's own next untap step rather
    -- than the victim's controller's, so it rides Object.exertedBy.
    --
    -- NOT Tap and not a rider on it. The two clauses come apart on both sides:
    -- Elvish Hunter prohibits without tapping, and Wall of Frost prohibits a
    -- creature that tapped itself by attacking. Stores NO duration -- it writes
    -- Object.doesNotUntapNext, which CR 701.43b ends at the untap step it applies
    -- in. The PRINTED static twin is Pawl.Types.UntapRestriction.
    DoesNotUntapNext ObjectRef.ObjectRef
  | -- | CR 701.27a: turn the permanents the ObjectRef names over, so each shows
    -- its other face.
    --
    -- A one-shot under CR 608.2c: it writes Object.face, which every
    -- characteristic read goes through (Pawl.Engine.Game.faceOf). The gates on
    -- whether anything happens (CR 701.27c, CR 701.27d) are read off the card's
    -- LAYOUT by Pawl.Engine.Card.turnedOver, never off which card it is.
    --
    -- The transform WORDING only. CR 701.28's convert turns a permanent over by
    -- the same subrules (CR 701.28a) and needs its own opcode (#698).
    Transform ObjectRef.ObjectRef
  | -- | CR 702.26b: the permanents the ObjectRef names phase out.
    --
    -- NOT a zone change, which is CR 702.26d in as many words, so this does not
    -- go through the zone-change funnel and no zone-change ability triggers. It
    -- writes GameState.phasedOut via Pawl.Engine.Phasing.phaseOutSet, shared with
    -- CR 502.1's turn-based action, so the two ways of phasing out cannot
    -- disagree about CR 702.26g's closure or CR 702.26h's tie-break.
    --
    -- Stores NO duration, CR 702.26a fixing when it ends. No PhaseIn twin, that
    -- same rule being the only thing that phases anything in.
    PhaseOut ObjectRef.ObjectRef
  | -- | CR 708.2: turn the named permanent face down with the characteristics the
    -- effect LISTS for it. Backslide lists none, so CR 708.2a's 2/2 supplies
    -- them; Cyber Conversion lists a set and carries it.
    --
    -- NOT the same act as Transform, which CR 701.27b keeps separate in as many
    -- words. The listed values are "the COPIABLE values of that object's
    -- characteristics" (CR 708.2), which is why the whole of it is one status
    -- field that Pawl.Engine.Game.faceOf substitutes for.
    --
    -- Not implemented: turning a SET face down, which Ixidron's "turn all other
    -- nontoken creatures face down" would want.
    TurnFaceDown TurnFaceDown.TurnFaceDown
  | -- | CR 708: turn the slot's target permanent face up. The mirror of
    -- TurnFaceDown and NOT of CR 116.2b's special action: no procedure is taken
    -- and no cost is paid, so Showstopping Surprise's "turn it face up if it's
    -- face down" needs neither a morph ability nor a mana cost on the card
    -- underneath.
    --
    -- A bare SlotName because the turning-over takes no arguments: CR 708.8 has
    -- the permanent simply regain its own copiable values, so there is nothing
    -- for the effect to list the way TurnFaceDown lists CR 708.2's.
    --
    -- The "if it's face down" of the card's own text is not a condition to
    -- author: turning a face-up permanent face up is a no-op, so the opcode's own
    -- guard is the clause.
    TurnFaceUp SlotName.SlotName
  | -- | CR 506.4: an effect that specifically removes a permanent from combat --
    -- the rule's one clause a card ASKS for rather than a condition the engine
    -- has to notice, which is why it is an opcode and not a sampler like
    -- Combat.removeChanged (Labyrinth of Skophos).
    --
    -- Removal ONLY: CR 506.4's second sentence is the whole effect, so there is
    -- no inverse opcode and no duration. CR 506.4a and CR 506.4b bound what
    -- removal is NOT and neither reaches this opcode.
    --
    -- Not implemented: removing a swept SET from combat (#1397).
    RemoveFromCombat SlotName.SlotName
  | -- | CR 509.1h's escape clause: an effect SAYS an attacking creature becomes
    -- blocked (Curtain of Light). CR 508.4d names the same clause for a creature
    -- that entered the battlefield attacking after the declaration.
    --
    -- Blocked BY NOTHING, which is the rule: the status and the set of creatures
    -- blocking are separate (CR 509.1h against CR 510.1c), so a creature this
    -- blocks assigns no combat damage and takes none.
    --
    -- The BLOCKING side is not this opcode and never was: a creature put onto the
    -- battlefield already blocking a named attacker is EntryRiders.blocking (CR
    -- 509.4), and a creature already on the battlefield being made to block is CR
    -- 509.1c's requirements -- Pawl.Types.BlockRequirement and
    -- Pawl.Types.ActiveBlockRequirement, with Lure and provoke in the pool.
    --
    -- Not implemented: CR 509.1h's other direction, an effect saying a creature
    -- becomes UNBLOCKED (Scryfall `oracle:"becomes unblocked"`, 2026-08-14, no
    -- hit).
    BecomesBlocked SlotName.SlotName
  | -- | CR 500.8: add phases to a turn, directly after the specified phase, in
    -- written order -- Aggravated Assault is `[ExtraCombat, ExtraMain]`. A
    -- payload rather than a sibling opcode per shape, because CR 500.8 does not
    -- fix which phases are added. Targetless. Executed via Turn.splicePhases,
    -- where the CR 505.1a/506.1 detail of WHAT is inserted and the CR 511.3
    -- question of WHERE both live.
    AddPhases [ExtraPhase.ExtraPhase]
  | -- | CR 724.1: end the turn (Time Stop). Nullary -- rule 724.1's six steps
    -- fix the whole procedure, and no printing parameterises any of them.
    --
    -- Not a schedule rewrite that a caller composes: CR 724.1a-f differ from CR
    -- 608's resolution process, so the opcode owns the pending-trigger watermark
    -- (724.1a), the exile of the whole stack including the resolving object
    -- (724.1b), the state-based check that grants no priority (724.1c), and the
    -- jump to the cleanup step (724.1d). CR 724.1f's "no player gets priority
    -- during this process" is what GameState.endTurnSignal carries out to
    -- Engine.priorityLoop; it cannot be expressed in `remaining` alone.
    --
    -- Not implemented: CR 724.2's sibling, ending the COMBAT PHASE, whose one
    -- printing (Mandate of Peace) says "cast this spell only during combat" and
    -- so waits on #527 (#873).
    EndTurn
  | -- | CR 613.1b / 611.2c: install a layer-2 control effect on the objects the
    -- ObjectRef names, for a duration. The new controller is this effect's
    -- source's controller, baked into a stored SetController effect -- derived,
    -- never chosen -- and each object whose controller changed is re-Sicked (CR
    -- 302.6). NOT a reuse of ModifyTarget, whose Modification is static card data
    -- and cannot carry a resolution-time PlayerId. Permanent control (CR 613),
    -- distinct from Mindslaver's (CR 723); the swept set is FROZEN (CR 611.2c).
    GainControl DurationRef.DurationRef
  | -- | CR 603.7: create the delayed triggered ability this card declares under
    -- this name (Face.delayedAbilities). First-order: the payload is card data
    -- joined by a name, so this opcode carries no nested ability. The resolving
    -- object's binding environment is captured as the ability is armed, which is
    -- how CR 603.7c's "it" survives the end of this resolution.
    --
    -- The Duration is CR 603.7b's stated duration; Nothing is that rule's
    -- default, once only at the next trigger event. The Onset is the envelope's
    -- other end -- when the ability becomes armed. See Pawl.Types.Onset for why a
    -- total field rather than a second Maybe, and why the gate cannot live in the
    -- ability's own trigger condition.
    --
    -- A resolution is not the only arming -- CR 603.7a's third clause, "a static
    -- ability that allows a player to take an action". Chancellor of the Forge
    -- arms this from a CR 103.6 opening-hand action, before the game's first
    -- turn, where Pawl.Engine.Resolve.performHandAction passes the acting CARD as
    -- the resolving object; CR 603.7g fixes that card as the source.
    ArmDelayedTrigger ArmDelayedTrigger.ArmDelayedTrigger
  | -- | CR 611.1 / 613.11: install a stored PLAYER or RULES-modifying continuous
    -- effect on some players for a duration (Silence, Cease-Fire).
    --
    -- Targets only through its AffectedPlayers: a Scoped effect watches a CLASS
    -- and prompts for nothing, while the Named arm names a slot the ability
    -- targeted and Resolve bakes it to a seat. Its controller is BAKED IN (CR
    -- 109.5) -- the source may be in a graveyard by the time anyone asks.
    AffectPlayers AffectPlayers.AffectPlayers
  | -- | CR 509.1c / 613.11: install a stored BLOCKING REQUIREMENT for a duration.
    -- Provoke (CR 702.39a) is `RequireBlock UntilEndOfCombat (InSlot
    -- provokeTarget) (EachMatching IsSource)`.
    --
    -- Rule 509.1c's two axes are both OBJECTS -- which creature must block, and
    -- what it must block -- so this takes ObjectRefs rather than AffectPlayers'
    -- scope, one requirement instance per (blocker, attacker) pair.
    RequireBlock RequireBlock.RequireBlock
  | -- | CR 701.19c / 611.1: install a stored REGENERATION PROHIBITION over the
    -- permanents the ref names, for a duration. Hurr Jackal's is
    -- `CantBeRegenerated UntilEndOfTurn (InSlot target)`.
    --
    -- RequireBlock's shape one axis narrower, and stored for the same reason: the
    -- prohibition outlives the resolution that made it, where
    -- Pawl.Types.Regenerability is a property of one destruction and is set by
    -- the effect doing the destroying (Terror's "It can't be regenerated").
    CantBeRegenerated CantBeRegenerated.CantBeRegenerated
  | -- | CR 508.1d / 613.11: install a stored ATTACKING REQUIREMENT for a
    -- duration. Alluring Siren's is `RequireAttack UntilEndOfTurn (InSlot
    -- target) (Relative You)`.
    --
    -- RequireBlock's twin, and not its mirror image: rule 508.1d's two axes are
    -- an OBJECT and a PLAYER -- which creature must attack, and whom it must
    -- attack (CR 508.1b) -- so this pairs an ObjectRef with a PlayerRef, one
    -- requirement instance per (attacker, defender) pair.
    RequireAttack RequireAttack.RequireAttack
  | -- | CR 114.2: the resolving controller gets an emblem with the given
    -- abilities, put into the command zone. Targetless; the abilities ride a Card
    -- so the emblem reuses the whole ability pipeline.
    CreateEmblem card
  | -- | CR 725: a player becomes the monarch. The beneficiary is named by the
    -- MonarchTarget: the resolving controller, the controller of the ability's
    -- bound source, or a target slot, which is the one arm that makes this opcode
    -- target. Emits GameEvent.BecameMonarch.
    BecomeMonarch MonarchTarget.MonarchTarget
  | -- | The permanent in the slot GAINS THIS DESIGNATION -- CR 702.112a's
    -- renown, CR 701.37a's monstrous, CR 701.60a's suspect and CR 719.3a's
    -- solved.
    --
    -- ONE opcode over Pawl.Types.Designation and not one per mark, because every
    -- rule that mints one words the write identically. Casing on the designation
    -- is not casing on an effect's identity: it is one payload of one opcode, the
    -- way ItBecomes carries a Daytime.
    --
    -- A SlotName and not an ObjectRef, so that it names the same permanent the
    -- PutCounters beside it in the clause does. The slot need not be a reserved
    -- binding: Person of Interest's names Binding.triggerSource, while
    -- Rune-Brand Juggler's is a CR 115.6 target slot.
    --
    -- Writes Object.designations, which holds DESIGNATIONS rather than
    -- characteristics, so nothing in CR 613 could carry it. What CR 701.60c hangs
    -- off `Suspected` is read off the designation wherever it is asked, not
    -- written here. Idempotent by construction, which CR 702.112c and CR 701.60d
    -- lean on; emits GameEvent.BecameDesignated only on a change.
    Designate Designate.Designate
  | -- | CR 716.2a's first half: "[Cost]: This Class's level becomes N." The
    -- slot's permanent gets that level.
    --
    -- Designate's shape above, and for its reason: CR 716.2b makes a level a
    -- designation, so this writes Object.classLevel rather than creating a CR 613
    -- modification, and nothing in CR 613 could carry it. BECOMES rather than
    -- increments -- rule 716.2a states an absolute -- and the "only if this Class
    -- is level N-1" half of the same sentence rides
    -- Pawl.Types.ActivatedAbility.condition on the level bar's own ability rather
    -- than being re-checked here, which is CR 113.7a: once activated, an ability
    -- exists on the stack independently of its source.
    SetClassLevel SetClassLevel.SetClassLevel
  | -- | CR 701.60a's other ending: the named permanents are NO LONGER SUSPECTED.
    -- Rule 701.60a's "until it leaves the battlefield" needs no opcode,
    -- Object.newIncarnation already dropping the designation.
    --
    -- Not a designation-parameterised inverse of Designate: CR 701.60a's ending
    -- belongs to `Suspected` alone, no rule taking renowned or monstrous away.
    Unsuspect ObjectRef.ObjectRef
  | -- | CR 702.100a and CR 702.100b together: put a +1/+1 counter on the slot's
    -- permanent, and if one or more actually land, that permanent EVOLVES.
    --
    -- ONE opcode and not a PutCounters beside a marker, unlike renown's pair:
    -- rule 702.100b makes the marker CONDITIONAL on counters having been put, and
    -- two effects in a clause cannot state that dependency. The counter's kind
    -- and count are the rule's, so neither is a payload.
    Evolve SlotName.SlotName
  | -- | CR 702.134a and CR 702.134c together: put a +1/+1 counter on the slot's
    -- creature, and record that the source MENTORED it.
    --
    -- Evolve's shape one rule over. The slot names rule 702.134a's chosen TARGET,
    -- the mentor being the resolving ability's own source.
    Mentor SlotName.SlotName
  | -- | CR 702.149a and CR 702.149c together: put a +1/+1 counter on the slot's
    -- creature, and record that it TRAINED. Evolve's shape and Evolve's reason
    -- for being one opcode -- rule 702.149c makes the marker conditional on
    -- counters having been put. The slot is Binding.triggerSource, unlike
    -- Mentor's chosen target.
    Train SlotName.SlotName
  | -- | CR 731.1: "it becomes day" / "it becomes night" -- the GAME gains that
    -- designation. Targetless and player-free, unlike BecomeMonarch: rule 731.1
    -- puts the designation on the game itself.
    --
    -- NOT just a write: CR 702.145c and CR 702.145f make daybound and nightbound
    -- permanents transform as the designation arrives, so Pawl.Engine.Resolve
    -- hands this to Pawl.Engine.Daytime rather than assigning GameState.daytime.
    ItBecomes Daytime.Daytime
  | -- | CR 725 (Palace Jailer): exile the slot's target UNTIL an opponent of the
    -- effect's controller becomes the monarch. The DURATION is the novelty -- the
    -- exiled incarnation is registered in GameState.exiledUntilMonarch and
    -- returned by Pawl.Engine.Monarch's settle-loop sweep. NOT MoveToZone, which
    -- has no duration and schedules no return.
    ExileUntilMonarch SlotName.SlotName
  | -- | CR 702.55a: exile the object the ObjectRef names, HAUNTING the creature
    -- the SlotName's target names. The LINK is the novelty: the exiled
    -- incarnation is filed in GameState.haunting against the object targeted,
    -- which CR 702.55b's "creature it haunts" reads and
    -- TriggerCondition.HauntedCreatureDies matches on.
    --
    -- THREE incarnations are in play (CR 400.7): the ObjectRef is
    -- Pawl.Engine.Binding.became, the graveyard card the death minted, since rule
    -- 702.55a's "it" is the card and the ability's source is the permanent that
    -- died; the link is keyed on the id the exile move mints. Only the second
    -- slot is a target (CR 115.10a).
    ExileHaunting ExileHaunting.ExileHaunting
  | -- | CR 729.1/729.1b: play a Magic subgame, then bind its outcome -- the
    -- WINNER -- into this slot for a later effect to read. Nothing is bound when
    -- the subgame is a draw, which is what lets Shahrazad's
    -- PlayerRef.EachPlayerExcept read "each player who doesn't win" as the whole
    -- table. A definition, not a cast-time target -- the winner is known only when
    -- the subgame ends.
    PlaySubgame SlotName.SlotName
  | -- | CR 608.2d: the resolving controller chooses one of their opponents, and
    -- the player chosen is bound into this slot for a LATER EFFECT of the same
    -- resolution to name (Skullwinder, Infernal Offering), read through
    -- Pawl.Types.Chooser's BoundInSlot.
    --
    -- CHOOSE, not target (CR 115.10a), so the pick happens while applying the
    -- effect (CR 608.2d) rather than at CR 601.2c and CR 608.2b has nothing to
    -- re-validate. The slot is a definition, which Pawl.CardSpec's dataflow lint
    -- sees through Pawl.Engine.Resolve.definedSlots. Elided at one candidate.
    --
    -- Not implemented: "choose a player", which would offer the controller too
    -- and so needs a scope beside the slot (#1444).
    ChooseOpponent SlotName.SlotName
  | -- | ChooseOpponent's twin with the decision replaced by randomness: one of
    -- the resolving controller's opponents is picked AT RANDOM and bound into
    -- this slot for a later effect of the same resolution to name (Ruhan of the
    -- Fomori's "choose an opponent at random. Ruhan attacks that player").
    --
    -- Its own arm rather than a flag on ChooseOpponent, for the reason
    -- Pawl.Types.ObjectRef's RandomCardInHand gives beside ChosenCardInHand: the
    -- two differ in who answers -- a seat weighing options against nobody
    -- weighing anything -- so Pawl.Types.Prompt's RandomOpponent carries no
    -- Pawl.Types.Decider where Prompt.ChooseOpponent does. CR 701.9b is the
    -- rulebook's own acknowledgment that "at random" and "the player chooses"
    -- are different instructions over the same domain.
    --
    -- CHOOSE, not target (CR 115.10a). The slot is a definition, which
    -- Pawl.CardSpec's dataflow lint sees through
    -- Pawl.Engine.Resolve.definedSlots. Elided at one candidate, CR 102.2
    -- leaving a two-player game exactly one opponent.
    ChooseOpponentAtRandom SlotName.SlotName
  | -- | CR 706.1: roll a die of the stated kind, and bind the result as an
    -- AMOUNT at the payload's slot for a later effect of the same resolution to
    -- read through Pawl.Types.Quantity's InSlot (Ancient Copper Dragon's "roll a
    -- d20. You create a number of Treasure tokens equal to the result").
    --
    -- CR 706.4's half of CR 706 needs nothing more: no results table, so the
    -- number itself is the whole outcome and the card's own later text says what
    -- to do with it. CR 706.3's table needs nothing more EITHER, and gets no arm
    -- here: a striation is one Pawl.Types.Clause of the same mode, gated by its
    -- `condition` on a Condition.Compares over this slot, which CR 706.3b's "all
    -- part of one ability" is exactly. Djinni Windseer in Pawl.DiceSpec is what
    -- proves it. Not implemented: CR 706.3c's "Roll again" (#2124).
    --
    -- ChooseOpponentAtRandom's posture, one type over: Pawl.Types.Prompt's
    -- RollDie carries no Pawl.Types.Decider and no PlayerId, because a die
    -- result is nobody's choice -- there is no seat weighing options, so there
    -- is nothing for CR 723 to usurp. The engine still only OFFERS and FILTERS:
    -- it names the die and admits the interpreter's answer only inside CR
    -- 706.1a's 1..N, never computing the outcome itself.
    --
    -- The slot is a definition, which Pawl.CardSpec's dataflow lint sees through
    -- Pawl.Engine.Resolve.definedSlots.
    RollDie RollDie.RollDie
  | -- | CR 103.5b (Serum Powder): exile every card in the resolving controller's
    -- hand, then draw that many. Targetless and controller-scoped.
    --
    -- ONE opcode rather than an exile composed with a Draw: "that many" is the
    -- hand size BEFORE the exile. The card granting the action is itself exiled
    -- with the rest, CR 103.5b's action not being a cost.
    ExileHandThenDraw
  | -- | CR 701.34a: choose any number of permanents and/or players that have a
    -- counter, then give each one additional counter of each kind it already has.
    --
    -- CHOOSE, not target (the rule's own word), so no SlotName: the set is picked
    -- on RESOLUTION via Prompt.ChooseProliferate. Nullary, rule 701.34a fixing
    -- the count at one per kind. Object counters ride Event.putCounters and
    -- player counters Event.putPlayerCounters, so CR 614's counter replacements
    -- get their opportunity against either recipient.
    Proliferate
  | -- | CR 701.39a: "bolster N" -- choose a creature the resolving controller
    -- controls with the least toughness, or tied for least, then put that many
    -- +1\/+1 counters on it. CHOOSE, not target, so no SlotName;
    -- Prompt.ChooseBolster asks on resolution.
    --
    -- ONE payload, rule 701.39a fixing everything else. N is a Quantity rather
    -- than a Natural because the pool prints it as an expression (Dragonscale
    -- General's "where X is the number of tapped creatures you control"). Not a
    -- PutCounters over a cleverer ObjectRef: a ref DESCRIBES a set and the whole
    -- set is counted (CR 115.10a), where this rule has a player pick ONE out of
    -- it.
    Bolster Quantity.Quantity
  | -- | CR 701.47a: "amass [subtype] N" -- if the resolving controller controls no
    -- Army creature, create a 0\/0 black [subtype] Army creature token; then
    -- choose an Army creature they control, put N +1\/+1 counters on it, and give
    -- it the subtype in addition to its other types (CR 205.1b). CHOOSE, not
    -- target; Prompt.ChooseAmass asks on resolution.
    --
    -- ONE opcode rather than four composed effects: rule 701.47a fixes the ORDER,
    -- the second instruction reads state the first writes, and the third and
    -- fourth act on the object the second chose, which no card-data slot names.
    -- Amass.subtype carries the card's word, the Army type being the rulebook's.
    -- Performed by Pawl.Engine.Amass.amass, one procedure that cannot stop early
    -- (CR 701.47b).
    Amass Amass.Amass
  | -- | CR 701.68a: "blight N" -- the players the PlayerRef names each put N
    -- -1\/-1 counters on a creature THEY control. CHOOSE, not target;
    -- Prompt.ChooseBlight asks on resolution.
    --
    -- The candidate set is "a creature you control" UNCONSTRAINED -- Bolster's
    -- pool narrowed by least toughness is the contrast. The "you" is whoever the
    -- instruction ADDRESSES, which need not be the resolving controller (High
    -- Perfect Morcant's "each opponent blights 1"), and each named player picks
    -- from THEIR OWN creatures and is asked separately.
    --
    -- The Quantity has no NON-LITERAL producer in `data/cards/`, and both
    -- printings that would be one state their amount as a COST rather than as an
    -- instruction -- Soul Immolation's additional cost and Blighted Nightmare's
    -- activation cost, which are Pawl.Types.CostComponent.BlightX. Scryfall
    -- `o:/[Bb]light X/`, 2026-08-20, returns those two and nothing else; a
    -- printing reading "blight X" after a colon or a period would refute it.
    Blight PlayerQuantity.PlayerQuantity
  | -- | CR 701.54a: the Ring tempts the resolving controller -- they get an emblem
    -- named The Ring if they have none (CR 701.54c), then choose a creature they
    -- control to become their Ring-bearer. CHOOSE, not target;
    -- Prompt.ChooseRingBearer asks on resolution.
    --
    -- Nullary, rule 701.54a fixing the chooser, the count and the qualification.
    -- ONE opcode rather than an emblem-maker composed with a choice, because CR
    -- 701.54c fixes their ORDER and makes the first conditional on state the
    -- second writes. Performed by Pawl.Engine.Ring.tempt, one procedure that
    -- cannot stop early (CR 701.54d).
    TemptWithTheRing
  | -- | CR 701.49: the resolving controller ventures into the dungeon -- they
    -- enter the dungeon they own if they are in none (CR 701.49a), and otherwise
    -- move their venture marker along one arrow (CR 701.49b). CHOOSE, not target;
    -- Prompt.ChooseRoom asks on resolution. Performed by
    -- Pawl.Engine.Dungeon.venture. Nullary: the venturer is "you", and which
    -- dungeon is answered by CR 701.49a from the cards that player owns.
    --
    -- Not implemented: CR 701.49d's "venture into [quality]", the variant naming
    -- a particular dungeon, which would be the one payload (#1334).
    Venture
  | -- | CR 701.21a: the PLAYERS the slot names each sacrifice this many
    -- permanents matching the Filter, each chosen by that player (Diabolic
    -- Edict names one; Rishadan Cutpurse's gate binds several).
    --
    -- Distinct from Sacrifice, which names a PERMANENT: there the effect picks
    -- the victim, here the sacrificing player does, which is why this one
    -- prompts.
    --
    -- CR 101.4: with several, every seat's pick is made first -- in APNAP order,
    -- each seat knowing the ones before it (CR 101.4b) -- and only then does
    -- anything leave the battlefield.
    --
    -- CR 609.3: a player with fewer matching permanents sacrifices all of them
    -- and one with none sacrifices nothing -- forced, so neither is prompted.
    PlayerSacrifices PlayerSacrifices.PlayerSacrifices
  | -- | CR 500.7: the players the PlayerRef names each get one extra turn, added
    -- directly after the turn this resolves in.
    --
    -- No count and no "which turn": CR 500.7's clause about multiple extra turns
    -- is about several such effects rather than one adding several. WHERE they go
    -- is Engine.handoffTurn's question, reading GameState.extraTurns as the stack
    -- CR 500.7 describes.
    --
    -- CR 500.11 / 614.1b: the PhaseSelectors are the steps and phases the created
    -- turn SKIPS (Savor the Moment's `Step (Beginning Untap)`). They ride this
    -- opcode because "that turn" has to name the turn this same resolution just
    -- created, and CR 500.7's most-recently-created-first ordering means
    -- SkipNextPhase's "next" names a DIFFERENT turn as soon as another
    -- extra-turn effect resolves afterwards.
    TakeExtraTurn TakeExtraTurn.TakeExtraTurn
  | -- | CR 701.24: the referenced objects are shuffled into their OWNERS'
    -- libraries (Riftsweeper). The move goes through the changeZone funnel,
    -- landing in the OWNER's library by CR 400.3, and that library is then
    -- shuffled (CR 701.24a).
    --
    -- NOT MoveToZone Library: CR 701.24c shuffles the library even if the named
    -- objects are not where they were expected, so a CR 616.1 replacement
    -- cancelling the move must not cancel the shuffle. And a shuffle is its own
    -- observable event, CR 701.24e and CR 701.24f triggering on it.
    --
    -- ShuffleIntoLibrary.library NAMES the library, which is what CR 701.24c's
    -- first half needs: an owner read off the objects disappears with them, so a
    -- card whose shuffle-in resolves with every named object gone (Dwell on the
    -- Past) would have no library left to shuffle. Absent when the card's own
    -- words derive it instead (Riftsweeper); the libraries shuffled are the UNION
    -- of the two readings, and one named twice is still shuffled once.
    ShuffleIntoLibrary ShuffleIntoLibrary.ShuffleIntoLibrary
  | -- | CR 608.2g: offer a player the cast of the object the slot names. CR
    -- 310.12b's "then you may cast it transformed without paying its mana cost"
    -- is the producer, and the CastOffer is that sentence's two riders --
    -- "instructs or allows" is the payload's Optionality and WHICH player its
    -- PlayerRef.
    --
    -- The slot is a READ, not a definition, and may be filled either way: CR
    -- 310.12b binds it with a MoveToZone earlier in the same instruction list,
    -- while Harness the Storm fills it at CR 601.2c. Resolve reads it off the
    -- resolving object's LIVE bindings either way.
    --
    -- CR 601.3's permission comes from the offer ITSELF, which is what lets the
    -- second of those reach a graveyard with no standing permission in sight:
    -- Cast.castableWhenOffered asks the prohibitions and the cost, never the
    -- zone. An OFFER and not a cast, even at Mandatory: CR 601.2b's announcements
    -- still belong to the caster, and one they cannot complete is reversed by CR
    -- 601.2.
    OfferCast OfferCast.OfferCast
  | -- | CR 601.3: grant the permission to play the objects the ObjectRef names,
    -- for a duration (Victor Mancha, Runaway).
    --
    -- The OPPOSITE of OfferCast: an offer is one cast taken during this
    -- resolution, while this is a standing permission the player exercises later,
    -- at their own timing.
    --
    -- PLAY and not cast, after CR 601.1a: Pawl.Engine.Cast reads the permission
    -- for the spell, and Pawl.Engine.Action.playableLands for CR 305.1's special
    -- action. CR 611.2b: if the stated duration never starts,
    -- Pawl.Engine.Expiry.arm answers Nothing and Resolve stores nothing.
    --
    -- CR 118.14's "and mana of any type can be spent to cast that spell" is the
    -- payload's `spending` rider, riding the GRANT because rule 118.14's last
    -- sentence scopes it to the permission.
    --
    -- Not implemented: a beneficiary other than CR 109.5's "you". The owner-side
    -- grants (Release to the Wind, Soul Partition) each carry a second clause
    -- pawl cannot yet spell.
    GrantPlayFromExile GrantPlayFromExile.GrantPlayFromExile
  | -- | CR 702.170c: "in addition to the plot special action, some spells and
    -- abilities cause a card in exile to become plotted" -- so the objects the
    -- ObjectRef names each become plotted (Kellan Joins Up).
    --
    -- NOT named Plot: CR 702.170e reserves that verb for CR 116.2k's special
    -- action, which this route is not -- it pays no plot cost, wants no plot
    -- keyword, and takes no special action. The rule's own words are "become
    -- plotted".
    --
    -- NO DURATION and no beneficiary beside it: CR 702.170d fixes both, naming
    -- the card's OWNER and putting no end on the permission.
    --
    -- The SIBLING of GrantPlayFromExile and never a use of it. That opcode
    -- writes Object.playableFromExile, CR 715.3d's permission for a named
    -- PLAYER with no cost stated; rule 702.170d names the owner, makes the cast
    -- free, and fixes the timing to a later turn's main phase. Object.plotted's
    -- own comment argues one field cannot answer both, and one opcode cannot
    -- either.
    MakePlotted ObjectRef.ObjectRef
  | -- | CR 608.2f: an action taken on several objects and/or players that cannot
    -- be processed simultaneously "is instead processed considering each affected
    -- player or object individually" -- so take the swept set one member at a
    -- time and run the BODY for each, with that member bound under the payload's
    -- slot for that iteration.
    --
    -- The ONE arm that runs a sequence per member. Two things need it: a
    -- per-object step acting on what that same step produced (Soulfire Eruption,
    -- rule 608.2f's own second example), and a body whose payload is keyed to
    -- the MEMBER rather than applied to it -- Rampage of the Clans' "its
    -- controller creates a 3/3 green Centaur creature token", which no opcode
    -- can spread across a set because the token is not aimed at the member at
    -- all. Every other set-naming opcode applies ITSELF across the swept set and
    -- needs no binding -- reach for those first.
    --
    -- NOT the control flow design.md section 1 keeps out of the ISA: the bound is
    -- the SWEPT SET, read once before the first iteration and fixed (CR 608.2c),
    -- so there is no condition to evaluate and no branch to take. The slot is a
    -- definition and never a target (CR 115.10a), though the REF may name a slot
    -- CR 601.2c filled by targeting.
    --
    -- Scoped to the iteration, both halves: the member binding is passed down
    -- rather than written onto the resolving object, and a name the BODY defines
    -- is reset to its pre-loop value before each pass.
    --
    -- ORDER: APNAP (CR 608.2f's primary determination) and then, within one
    -- controller, that rule's secondary sentence -- the RESOLVING controller's
    -- choice, asked as Prompt.OrderForEach.
    ForEach (ForEach.ForEach (Effect card))
  deriving (Eq, Ord, Show)
