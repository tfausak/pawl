module Pawl.Types.ObjectRef where

import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary

-- | WHICH OBJECTS an object-affecting effect names -- the object-side counterpart
-- of Pawl.Types.PlayerRef.
--
-- InSlot stands apart from the other arms: its objects were named BEFORE the
-- effect runs -- a slot, filled at cast or as the ability was placed -- where
-- every other arm's are found AS it runs. That is the distinction CR 115.10a
-- draws: only InSlot can name a target; no other arm ever does.
data ObjectRef
  = -- | The objects bound in a slot (CR 601.2c filled it by targeting, or the
    -- engine reserved it -- Binding.triggerSource). Usually ONE, the slot's
    -- single Recipient, subject to CR 608.2b's illegal-target check when the slot
    -- was a target.
    --
    -- But a slot bound to a whole GROUP names every one of them -- Salt Road
    -- Skirmish's "they gain haste until end of turn" over the two tokens the
    -- sentence before it made, and Act on Impulse's "those cards" over the three
    -- its own move exiled. A group is a definition and never a
    -- target (CR 115.10a), so it owes CR 608.2b nothing; the arm still reads at
    -- most one object per slot for every OTHER kind of binding, which is why
    -- "target creature" cannot become two.
    InSlot SlotName.SlotName
  | -- | Every PERMANENT ON THE BATTLEFIELD matching the Filter -- Day of
    -- Judgment's "all creatures". The battlefield is where CR 109.2 puts it; a
    -- set drawn from another zone is one of the arms below -- a graveyard's is
    -- EachCardInGraveyard, a hand's EachCardInYourHand, exile's
    -- EachCardExiledWithSource -- and a FILTERED sweep of a zone is written on
    -- the arm for that zone: the battlefield's is here, and a graveyard's, the
    -- stack's and the linked exile set's are on their own arms. A hand and a
    -- library still have none (#1309).
    --
    -- Not a target and never one (CR 115.10a), so CR 608.2b has nothing to
    -- fizzle. The set is swept when the effect executes (CR 608.2c) and is then
    -- fixed for that instruction; judging which swept objects are affected
    -- before any of them is (CR 608.2f) belongs to the opcode's funnel rather
    -- than to this type.
    --
    -- A CONTINUOUS effect over a set must additionally freeze the swept set into
    -- the effect itself (CR 611.2c), storing Affected.TheseObjects; the one-shots
    -- that take this type store nothing.
    EachMatching (Filter.Filter Keyword.Keyword)
  | -- | Every CARD IN A GRAVEYARD matching the Filter, in the graveyards the
    -- PlayerScope names -- Rise of the Dark Realms' "all creature cards from all
    -- graveyards". EachMatching's sibling with CR 109.2's battlefield default
    -- switched off by the card's own words, which is CR 109.2a: a description
    -- carrying "card" and the name of a zone "means a card matching that
    -- description in the stated zone".
    --
    -- The zone is BAKED IN rather than carried as a Pawl.Types.Zone, the shape
    -- Pawl.Types.Pool.CardsInGraveyard takes one question over: each zone whose
    -- filtered sweep a card in the pool asks for gets its own arm --
    -- EachCardExiledWithSource takes the exile one -- and the hidden zones (CR
    -- 400.2) would owe a visibility question a graveyard does not (#1309).
    --
    -- The PlayerScope is WHOSE, which CR 400.1 forces this arm to say and
    -- EachMatching's shared battlefield never has to. Not the
    -- Pawl.Types.GraveyardScope a target pool carries: that type's other arm
    -- reads the players another TARGET SLOT names, a reading no mass effect in
    -- the pool asks for (#1310). Enumerated by
    -- Pawl.Engine.PlayerEffect.playersInScope, the same fold over the one
    -- membership test that pool uses, so the two cannot drift.
    --
    -- Not a target and never one (CR 115.10a), and swept when the effect executes
    -- (CR 608.2c) -- the two properties EachMatching above has, for its reasons.
    EachCardInGraveyard EachCardInGraveyard.EachCardInGraveyard
  | -- | Every card in the RESOLVING CONTROLLER's hand -- Ignorant Bliss' "exile
    -- all cards from your hand". EachMatching's sibling with CR 109.2's
    -- battlefield default switched off the same way EachCardInGraveyard switches
    -- it off, CR 109.2a.
    --
    -- Nullary, where the graveyard arms carry a scope and a filter, and each
    -- omission is a rule rather than an economy. NO PLAYER: CR 400.2 makes a hand
    -- a hidden zone, so an arm reaching anyone else's would owe a visibility
    -- question this one never asks -- CR 109.5's "you" is the resolving
    -- controller, who may already look at their own hand. NO FILTER: a filtered
    -- sweep of a hidden zone is the same question, since matching would reveal
    -- which cards matched, and nothing needs to be told apart when EVERY card
    -- goes (#1309).
    --
    -- Not a target and never one (CR 115.10a) -- a hidden zone has no target pool
    -- at all (#559) -- and swept when the effect executes (CR 608.2c), the two
    -- properties EachMatching above has.
    --
    -- ONE card chosen out of a hand rather than all of them is ChosenCardInHand
    -- below, which does reach another player's hand -- it answers the visibility
    -- question this arm avoids rather than dodging it, by asking that hand's own
    -- owner (CR 402.3).
    EachCardInYourHand
  | -- | CR 607.2a's linked set: every card in exile that an instruction in an
    -- ability of THIS EFFECT'S SOURCE put there -- Hoarding Dragon's "the exiled
    -- card". EachMatching's sibling with CR 109.2's battlefield default switched
    -- off, the way EachCardInGraveyard and EachCardInYourHand switch it off, and
    -- the arm is what makes the rest of rule 607.2a sayable: "the second ability
    -- refers only to cards in the exile zone that were put there as a result of
    -- an instruction to exile them in the first ability."
    --
    -- NO PLAYER: exile is a public zone (CR 400.2) and the set is defined by
    -- which object exiled the card, not by whose it is.
    --
    -- The Filter is OPTIONAL because a linked reference usually names the whole
    -- set it is linked to -- Hoarding Dragon, Promise of Tomorrow and Savior of
    -- Ollenbock each take all of it, and write no filter. Karn Liberated is the
    -- printing that narrows the set with its own words, "all non-Aura PERMANENT
    -- CARDS exiled with Karn", so the subset has to be sayable; a stated filter
    -- is read exactly as EachMatching's is, against each linked card's
    -- projection.
    --
    -- Singular and plural are ONE arm, which is CR 607.3: an ability referring to
    -- "the exiled card" whose linked ability exiled several "performs that action
    -- on each exiled card". So a sweep is the faithful reading of both wordings,
    -- and Hoarding Dragon's singular text needs no separate spelling.
    --
    -- Read against GameState.exiledWith, which is where the link lives; see that
    -- field for what the key is and for what rule 607.2a's per-ABILITY scope is
    -- approximated by.
    --
    -- Not a target and never one (CR 115.10a) -- the reference is a definition,
    -- not a choice -- and swept when the effect executes (CR 608.2c), the two
    -- properties EachMatching above has.
    EachCardExiledWithSource (Maybe (Filter.Filter Keyword.Keyword))
  | -- | Every SPELL ON THE STACK matching the Filter -- Swift Silence's "all
    -- other spells". EachMatching's sibling with CR 109.2's battlefield default
    -- switched off by the word "spell", which is CR 109.2b: a description
    -- carrying that word "means a spell matching that description on the stack".
    --
    -- SPELLS ONLY, never the abilities that share the zone. That is not a
    -- narrowing this arm chose: rule 109.2b's word is "spell", and CR 112.1
    -- makes a spell a CARD on the stack, so an activated or triggered ability is
    -- not one. The test is Pawl.Engine.Game.isSpell, a classification of the
    -- object's kind and never of the card's identity. An arm for a swept set of
    -- ABILITIES waits for a printing that names one; "counter all abilities"
    -- is not implemented (#1397).
    --
    -- "Other" is written `Not IsSource`, the one spelling CR 601.2c's "another"
    -- and Opalescence's "each other" already share -- the resolving spell is
    -- still on the stack while its own instructions run (CR 608.2), so an arm
    -- that dropped the source unasked would make "counter all spells" unsayable.
    --
    -- The zone is BAKED IN rather than carried as a Pawl.Types.Zone, which is
    -- EachCardInGraveyard's reason and the shape #1309 fixed for every zone: a
    -- card wanting a filtered sweep of one more zone gets one more arm.
    --
    -- Not a target and never one (CR 115.10a), so CR 608.2b has nothing to
    -- fizzle -- which is the whole difference between this and Cancel's targeted
    -- Pool.Spells slot. Swept when the effect executes (CR 608.2c), so a spell
    -- that has already left the stack is not in the set and one put there since
    -- the countering spell was cast is.
    EachSpell (Filter.Filter Keyword.Keyword)
  | -- | Every PLAYER in the game -- Molten Disaster's "and each player". The one
    -- arm that names no object at all, and it is here rather than on
    -- Pawl.Types.PlayerRef because the opcode that needs it takes an ObjectRef:
    -- CR 120.3a makes a player a damage recipient, and Effect.DealDamage already
    -- reaches one through the InSlot arm. Every OTHER ObjectRef-taking opcode
    -- reads objects only, and this arm drops out of the sweep there rather than
    -- being rejected -- the posture Pawl.Engine.Resolve.objectRefObjects already
    -- takes for a player bound in a slot.
    --
    -- Payload-free rather than carrying a Pawl.Types.PlayerRef: "each player" is
    -- what the card says, and a PlayerRef would make InSlot sayable twice over.
    --
    -- Not a target (CR 115.10a) and swept when the effect executes, the two
    -- properties EachMatching above has.
    --
    -- Not implemented, recorded here because the card's JSON cannot carry a
    -- comment: a sentence naming creatures AND players ("each creature without
    -- flying and each player") has to be written as two DealDamage
    -- instructions, so its one CR 608.2f batch becomes two (#1285).
    EachPlayer
  | -- | The cards on top of a library, deepest named last -- Count on Luck's "the
    -- top card of your library" and Act on Impulse's "the top three cards of your
    -- library". A library is a per-player zone (CR 400.1) kept as an ordered pile
    -- (CR 401.2), so "the top card" is a position rather than a property, and that
    -- is what no Filter can say: EachMatching sweeps the battlefield (CR 109.2) and
    -- a Filter matches characteristics, neither of which can pick the head of a
    -- hidden pile (CR 400.2).
    --
    -- The PlayerRef is WHOSE library, so "the top card of target player's library"
    -- is the same arm through its InSlot. The Natural is HOW MANY off the top of
    -- EACH library it names, so a depth of three over "each player" is three per
    -- seat rather than three in total -- which is what "exile the top three cards
    -- of each player's library" would say. A library holding fewer cards than the
    -- depth gives up what it has (CR 609.3), and an empty one gives nothing.
    --
    -- A Natural rather than a Pawl.Types.Quantity, the choice
    -- Pawl.Types.DamageRewrite made for the same reason: every printed depth in
    -- the pool is a literal number. Not implemented: a card whose depth is X
    -- (Monastery Raid's "exile the top X cards of your library instead") has no
    -- spelling here (#1375).
    --
    -- Not a target and never one (CR 115.10a) -- the player may be targeted, the
    -- cards are not -- so CR 608.2b has nothing to fizzle. Read when the effect
    -- executes (CR 608.2c), which is what makes an empty library a no-op rather
    -- than an error: there is no top card, so the arm names nothing.
    TopOfLibrary TopOfLibrary.TopOfLibrary
  | -- | A card in a graveyard, matching the Filter, CHOSEN as the effect runs
    -- rather than swept -- Port of Karfell's "return a creature card from your
    -- graveyard to the battlefield tapped".
    --
    -- NOT A TARGET, and the distinction this arm exists for. A graveyard is a
    -- public zone (CR 400.2), so nothing about the zone would stop the card from
    -- saying "target"; CR 115.1 is what settles it, since a spell or ability is
    -- targeted only where its own text says "target [something]". This sentence
    -- does not, so the choice is made while applying the effect (CR 608.2d)
    -- rather than announced on the stack (CR 601.2c), and CR 608.2b has nothing
    -- to re-validate. That is also why the arm is here rather than in
    -- Pawl.Types.GraveyardScope, which is a TARGET pool: the two questions look
    -- alike on the board and differ in every rule that reads them -- shroud,
    -- hexproof, "becomes the target" triggers and the fizzle.
    --
    -- WHO CHOOSES is the Pawl.Types.Chooser, which also fixes how many cards
    -- the arm names: TheController is CR 608.2c's default and one card across
    -- the whole scope (Port of Karfell), EachInScope is one card per player in
    -- scope, each chosen by that player out of their own graveyard (Exhume's
    -- "each player puts a creature card from their graveyard onto the
    -- battlefield"), and BoundInSlot is one card chosen by the one player a slot
    -- names -- Skullwinder's "choose an opponent. That player returns a card
    -- from their graveyard to their hand", where Effect.ChooseOpponent filled the
    -- slot a sentence earlier.
    --
    -- The PlayerScope is WHOSE GRAVEYARDS the candidates are drawn from, which
    -- CR 400.1 forces this arm to say for EachCardInGraveyard's reason. Under
    -- TheController it is independent of the chooser -- `You` is Port of
    -- Karfell's "your graveyard", and the wider scopes are Extract from
    -- Darkness' "a graveyard", still chosen from by the effect's controller.
    -- Under the other two the chooser fixes whose graveyard and the scope is the
    -- outer bound on which graveyards the instruction reaches, which is what the
    -- sentences themselves do.
    --
    -- ONE card per chooser, with no count beside the Filter, and CR 609.3
    -- covers the shortfall: a graveyard holding no matching card yields
    -- nothing, and that share of the instruction is ignored (CR 101.3) rather
    -- than failing. Not implemented: a count above one -- Fall of the Thran's
    -- "each player returns TWO land cards from their graveyard to the
    -- battlefield" -- and with it the exclusion "another" states, which Blood
    -- for Bones gets for free because its first choice has already left the
    -- graveyard by the time the second is offered (#1437).
    --
    -- Read when the effect executes (CR 608.2c), the property every arm above
    -- but InSlot has. Unlike them the read is a QUESTION, so only
    -- Pawl.Engine.Resolve's MoveToZone arm -- which already gathers its objects
    -- in the Game monad -- can carry it out; Resolve's pure sweep answers
    -- nothing for it. A card that writes this arm under any other opcode
    -- therefore names no object and does nothing, which is an INERT card-data
    -- error of the same kind as a stated origin zone on somebody else's
    -- permanent (Pawl.Engine.EffectZone's note), and gets no lint for the same
    -- reason: nothing reaches the wire and no rule is misread.
    ChosenCardInGraveyard ChosenCardInGraveyard.ChosenCardInGraveyard
  | -- | A card in a HAND, chosen as the effect runs -- Karn Liberated's "+4:
    -- target player exiles a card from their hand". ChosenCardInGraveyard's
    -- sibling over the hidden zone CR 400.2 makes a hand, and the hidden half is
    -- the whole difference between them.
    --
    -- ONE PlayerRef, where the graveyard arm needs a Pawl.Types.Chooser beside a
    -- Pawl.Types.PlayerScope, because for a hand those two questions have one
    -- answer: CR 402.3 lets a player look at their own hand and at nobody else's,
    -- so the player who chooses IS the player whose hand is looked in. The ref
    -- therefore names the choosers and the hands at once, and the pair the
    -- graveyard arm can legitimately split -- "target opponent chooses a card in
    -- YOUR graveyard" -- has no legal spelling here to keep apart. EachPlayer is
    -- one choice each, in APNAP order (CR 608.2e, CR 101.4); an InSlot is Karn's
    -- one targeted seat; `Relative You` is the resolving controller choosing in
    -- their own hand.
    --
    -- UNFILTERED, where the graveyard arm carries a Filter, and NOT for
    -- EachCardInYourHand's visibility reason: a filter narrowing a hand only its
    -- own owner is shown reveals nothing to anybody else. "A card from their
    -- hand" simply needs no filter to say, and a narrowed hand choice waits for a
    -- printing that asks for one. Not implemented (#1635).
    --
    -- NOT A TARGET, which the zone settles here rather than CR 115.1's "target"
    -- test settling it as it does for the graveyard arm: pawl has no target pool
    -- over a hidden zone, since announcing such a target would reveal the card
    -- (#559). The PLAYER is the target Karn's text names, and the card is chosen
    -- while the effect is applied (CR 608.2d).
    --
    -- ONE card per chooser, with CR 609.3 covering the shortfall exactly as it
    -- does for the graveyard arm: an empty hand yields nothing and that share of
    -- the instruction is ignored (CR 101.3).
    --
    -- Read when the effect executes (CR 608.2c), and a QUESTION rather than a
    -- read, so only Pawl.Engine.Resolve's MoveToZone arm can carry it out --
    -- ChosenCardInGraveyard's note above describes what a card writing it under
    -- any other opcode gets, and why that inert answer earns no lint.
    ChosenCardInHand PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)
