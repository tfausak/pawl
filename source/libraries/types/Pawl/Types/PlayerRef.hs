module Pawl.Types.PlayerRef where

import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 400.1: whose zone a scope folds over, per-player zones being a player's
-- own. And whose MANA POOL, since CR 106.4 attaches a pool to a player exactly as
-- CR 400.1 attaches a library, so Pawl.Types.ManaCount uses this type too and
-- Pawl.Engine.Count.playersFor resolves both.
--
-- Deliberately NOT Pawl.Types.PlayerScope, which looks like the same type.
-- PlayerScope is resolved against a perspective and nothing else; this can also
-- name a binding slot, which PlayerEffect's evaluator has no way to resolve. A
-- target pool needs BOTH readings and neither type whole, which is what
-- Pawl.Types.GraveyardScope is: this type's Relative arm cannot say CR 806.1's
-- "each opponent" as one value, and PlayerScope has no slot.
data PlayerRef
  = -- | Every player's copy of the zone. For a SHARED zone (CR 400.1: battlefield,
    -- stack, exile, command) this is the only meaningful value; the pairing is
    -- rejected by Pawl.Codec.InZone as card data is decoded and swept for again
    -- by Pawl.CardSpec, not by this type, see #161.
    EachPlayer
  | -- | EachPlayer minus the player a slot names -- Shahrazad's "each player who
    -- doesn't win the subgame", where the slot holds the subgame's winner.
    --
    -- NOT Relative Opponent: the excluded seat is the one a slot names, which
    -- need not be the perspective, and the set keeps the perspective in it when
    -- the slot names somebody else. NOT InSlot's complement either -- that arm
    -- names ONE player, and this is the rest of the table.
    --
    -- A SLOT NAMING NOBODY EXCLUDES NOBODY, so the set is every player. That is
    -- the reading the producer needs rather than a fallback: a drawn subgame has
    -- no winner, and "each player who doesn't win" is then the whole table. It is
    -- also why this is a slot-EXCLUSION rather than a slot-read -- an unfilled
    -- slot makes InSlot's readers no-op, and here it must widen instead.
    EachPlayerExcept SlotName.SlotName
  | -- | CR 109.5 / 102.2, resolved against the evaluation context's perspective.
    Relative PlayerRelation.PlayerRelation
  | -- | The player bound in a slot -- Sudden Impact's "that player's hand", where
    -- the slot was filled by targeting (CR 601.2c).
    InSlot SlotName.SlotName
  | -- | InSlot's PLURAL twin: EVERY player a slot names, rather than the one it
    -- names alone. Jungle Wayfinder's "each player may search THEIR library" is
    -- one producer -- the searchers are the seats that took CR 603.5's "may",
    -- bound under Binding.mayPlayers, and at three seats there are two of them.
    -- Bellowing Mauler's "each player loses 4 life unless they sacrifice a
    -- nontoken creature of their choice" is the other, over the seats a CR
    -- 118.12 gate's answer put in Binding.gatePlayers.
    --
    -- Not a widening of InSlot, deliberately. That arm's contract is the printed
    -- "that player" (Sudden Impact) and it reads through Binding.onlyOne, so a
    -- slot naming several names nobody; a TARGET slot legitimately holds several
    -- recipients, and letting InSlot read them all would silently change what
    -- every card writing it means. The two arms are the two printed words.
    --
    -- A slot naming NOBODY names nobody here too -- InSlot's collapse rather than
    -- EachPlayerExcept's widening -- since the seats are the ones an answer put
    -- in, and none went in when nobody accepted.
    --
    -- Slots holding non-player recipients are dropped rather than refused: this
    -- is the same read as InSlot's, which declines a slot holding an object.
    EachInSlot SlotName.SlotName
  | -- | InSlot's BAKED half, and runtime-only: one particular player, named
    -- outright. Pawl.Engine.Condition.bakeBound substitutes it for an InSlot as a
    -- CR 611.2b duration begins, so a "for as long as" condition naming the
    -- player a trigger's event bound (Garland, Royal Kidnapper's "for as long as
    -- they're the monarch") still answers once the resolution that stored it is
    -- over and its bindings are unreachable. Filter.ControlledByPlayer is the same
    -- move one type over, and Modification.SetController's baked controller is the
    -- older precedent.
    --
    -- NO CARD MAY WRITE IT -- only a resolution knows a PlayerId -- which the
    -- codec cannot enforce (it is total both ways, since an Expiry serialises
    -- through Pawl.Codec.Condition) and Pawl.CardSpec's pool sweep does.
    Specific PlayerId.PlayerId
  | -- | The player a Pawl.Types.Scope.OverPlayers fold is currently looking at --
    -- Malignus' "the highest life total among your opponents", where the life
    -- read is each candidate's rather than any one player's.
    --
    -- Pawl.Types.Quantity.Power's position, one type over: the object-reading
    -- arms name no object and read whichever one the evaluation is aimed at,
    -- and this names no player and reads whichever one it is aimed at. So it is
    -- resolved where the candidate is known -- Pawl.Engine.Quantity, off
    -- Pawl.Engine.Filter.playerView's identity -- rather than at
    -- Pawl.Engine.Count.playersFor, which holds no candidate and answers Nothing
    -- for it.
    --
    -- The OTHER candidate a card may mean is the player a per-player instruction
    -- is currently applying to -- Shahrazad's "each player who doesn't win loses
    -- half THEIR life", Nature's Resurgence's "each creature card in THEIR
    -- graveyard". Same word, because it is the same question with no view to hand,
    -- and Pawl.Engine.Quantity.forCandidate answers it by substitution before the
    -- fold runs: at the quantity's own references and at a nested count's SCOPE,
    -- never at that count's per-member quantity, which the fold below still
    -- supplies its own candidate for.
    --
    -- Pawl.Types.Search.owner is that second reading one type over, and the one
    -- position outside a quantity that answers it: Jungle Wayfinder's "each
    -- player may search THEIR library" is one instruction per player, so the
    -- library read is the searcher this pass reached, and
    -- Pawl.Engine.Resolve's Effect.Search arm substitutes it there.
    --
    -- Undeterminable ANYWHERE ELSE, deliberately: an evaluation aimed at an object
    -- or at nothing has no candidate player, so a card writing this outside a
    -- position some fold supplies a candidate for reads Nothing and its effect
    -- does nothing. That is the honest answer rather than a silent substitution
    -- of "you".
    Candidate
  | -- | CR 108.4 / 110.2: the CONTROLLER of the object a slot names --
    -- Spikeshell Harrier's "return target creature or Vehicle an opponent
    -- controls to its owner's hand. If THAT OPPONENT's speed ...", where the
    -- player is identified by the permanent the ability targeted.
    --
    -- InSlot's twin one indirection out. That arm reads a slot whose recipient IS
    -- a player (CR 115.1); this one reads a slot whose recipient is an OBJECT and
    -- asks who controls it, which is the only spelling a card has for a player it
    -- never targeted. Neither is derivable from the other: a slot holds one kind
    -- of recipient or the other.
    --
    -- CR 608.2h is load-bearing rather than incidental. The sentence that names
    -- the player generally MOVES the object first -- Spikeshell Harrier's does,
    -- from the battlefield to a hand -- and CR 108.4 gives a card in a hand no
    -- controller at all, so the live reading is empty by the time the next clause
    -- asks. That rule's "or if the effect has moved it from a public zone to a
    -- hidden zone, the effect uses the object's last known information" is the
    -- clause this arm rests on, and every reader answers it through a last-known
    -- aware view: Pawl.Engine.Resolve.playerRefPlayers through
    -- Pawl.Engine.Projection.controllerWithLastKnown, Pawl.Engine.Quantity
    -- through the view its caller supplies.
    --
    -- Undeterminable where no view can answer, which is Candidate's posture and
    -- for a neighbouring reason: Pawl.Engine.Count.playersFor holds no projection,
    -- so a Scope or a ManaCount naming this reference reads Nothing and its count
    -- is unanswered (#1441).
    ControllerOfBound SlotName.SlotName
  | -- | CR 508.6: the players attacking the player a slot names, narrowed by
    -- relation -- Curse of Vitality's "each opponent attacking that player".
    --
    -- A SET, like EachPlayer and Relative Opponent and unlike the two slot-reading
    -- arms above, which name one seat each. The card text is a plural and the rule
    -- it quotes is a predicate over the table, so the arm folds the roster rather
    -- than indirecting through one player.
    --
    -- Not derivable from Relative plus a filter: pawl has no predicate over
    -- players outside this type, and the fact being asked -- who controls a
    -- creature attacking a given player -- is about the combat record rather than
    -- about a characteristic.
    --
    -- Unanswerable where no projection is in hand: Pawl.Engine.Count.playersFor
    -- reads no controller, so a Scope or a ManaCount naming this reference is
    -- unanswered, which is ControllerOfBound's posture and for its reason (#1441).
    Attacking AttackingPlayers.AttackingPlayers
  deriving (Eq, Ord, Show)
