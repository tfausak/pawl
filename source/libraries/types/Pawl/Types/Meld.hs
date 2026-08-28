module Pawl.Types.Meld where

import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 701.42a's keyword action, and only it: "to meld the two cards in a meld
-- pair, put them onto the battlefield with their back faces up and combined. The
-- resulting permanent is a single object represented by two cards."
--
-- The EXILE is not part of this. Hanweir Battlements prints "exile them, then
-- meld them", and the exile is the card's own earlier instruction -- an ordinary
-- 'Pawl.Types.Effect.MoveToZone' binding both cards to a slot this opcode then
-- names. Keeping the two apart is what makes CR 701.42c come out right without a
-- rollback: "if an effect instructs a player to meld objects that can't be
-- melded, they stay in their current zone", which is rule 701.42c's own Graf Rats
-- example -- when the gate below refuses, this opcode does nothing at all and the
-- cards are left wherever the earlier instruction put them.
--
-- The gate is CR 701.42b's: "only two cards belonging to the same meld pair can
-- be melded. Tokens, cards that aren't meld cards, or meld cards that don't form
-- a meld pair can't be melded." Pawl.Engine.Resolve reads it off each named
-- object -- a CARD (CR 108.2, so not a token and not a copy) whose layout is
-- 'Pawl.Types.Layout.Meld', all sharing one owner.
data Meld card = MkMeld
  { -- | The cards to meld, which the melding ability has already named -- a slot
    -- an earlier 'Pawl.Types.Effect.MoveToZone' bound (CR 400.7j), for the pool's
    -- only pair.
    objects :: ObjectRef.ObjectRef,
    -- | The combined back face, INLINE, and interned at resolution.
    --
    -- Carried here for 'Pawl.Types.Effect.Create'\'s reason: no Pawl.Engine
    -- module imports Pawl.Registry and 'Pawl.Types.GameState.GameState' holds no
    -- name-keyed map, so an opcode naming its result by name would have nothing
    -- to resolve the name against. Every card an effect brings into being in pawl
    -- is either already an object in the game or is carried inline.
    --
    -- CR 712.4b leaves a meld card's own half of the oversized face meaningless
    -- on its own, so neither component prints this face and nothing is lost by it
    -- never being a card in a deck. CR 712.8g is what then reads it: the melded
    -- permanent "has only the characteristics of the combined back face", which
    -- is 'Pawl.Engine.Game.cardOfSource'\'s answer for the
    -- 'Pawl.Types.Source.OfMeld' this opcode builds.
    --
    -- Parametric in @card@ for 'Pawl.Types.Effect.Effect'\'s reason: the combined
    -- face is card DATA nested inside card data, and the parameter is what keeps
    -- 'Pawl.Types.Effect' from naming a concrete card type.
    result :: card
  }
  deriving (Eq, Ord, Show)
