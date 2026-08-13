module Pawl.Types.ObjectRef where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

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
    -- set drawn from a graveyard is EachCardInGraveyard below, and from any
    -- other zone has no card in the pool (#1309).
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
    -- Pawl.Types.Pool.CardsInGraveyard takes one question over: no card in the
    -- pool sweeps a filtered set out of any other zone, and the hidden ones (CR
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
    EachCardInGraveyard PlayerScope.PlayerScope (Filter.Filter Keyword.Keyword)
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
    TopOfLibrary PlayerRef.PlayerRef Natural.Natural
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
    -- battlefield"). Not implemented: a chooser named by a SLOT -- Obscura
    -- Confluence's "target player returns a creature card from their graveyard
    -- to their hand" -- and one an effect chooses at resolution, which is what
    -- Infernal Offering's "choose an opponent" would need first (#1436).
    --
    -- The PlayerScope is WHOSE GRAVEYARDS the candidates are drawn from, which
    -- CR 400.1 forces this arm to say for EachCardInGraveyard's reason. Under
    -- TheController it is independent of the chooser -- `You` is Port of
    -- Karfell's "your graveyard", and the wider scopes are Extract from
    -- Darkness' "a graveyard", still chosen from by the effect's controller.
    -- Under EachInScope the one phrase names both, which is what the sentence
    -- itself does.
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
    ChosenCardInGraveyard Chooser.Chooser PlayerScope.PlayerScope (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
