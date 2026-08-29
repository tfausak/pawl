module Pawl.Types.Conjure where

import qualified Pawl.Types.ConjureDestination as ConjureDestination

-- | Alchemy\'s conjure keyword action: create a card that was in nobody\'s deck
-- and put it into a zone.
--
-- Digital-only, so there is no rule to cite: @docs\/rules.txt@ contains no
-- "conjure", and the authority is Arena\'s own text -- "conjure a card named
-- Ornithopter into your hand". What the CR does settle is what the result is
-- NOT: it is no token (CR 111.1's are created by an effect and are not cards),
-- so the conjured object is an ordinary card, castable and shufflable, and
-- 'Pawl.Types.Source.OfCard' is what backs it.
--
-- The card is carried INLINE, 'Pawl.Types.Meld.Meld''s reason spelled out
-- there: no @Pawl.Engine@ module imports @Pawl.Registry@ and
-- 'Pawl.Types.GameState.GameState' holds no name-keyed map, so an opcode naming
-- its card by name would have nothing to resolve the name against. The printed
-- sentence names the card ("a card named Ornithopter"); pawl\'s card file
-- writes that card out.
--
-- Parametric in @card@ for 'Pawl.Types.Effect.Effect''s reason: the conjured
-- card is card DATA nested inside card data, and the parameter is what keeps
-- 'Pawl.Types.Effect' from naming a concrete card type.
--
-- Not implemented: a count other than one (Kami of Bamboo Groves\' "conjure two
-- cards named Forest"), a conjurer other than the resolving controller
-- (Juggernaut Peddler\'s "conjures a card named Juggernaut into their hand"),
-- and a slot binding the conjured card for a later clause to name (Mothlight
-- Processionist\'s "discard that card") (#2638).
data Conjure card = MkConjure
  { -- | The card conjured, written out in full.
    card :: card,
    -- | The zone it arrives in.
    destination :: ConjureDestination.ConjureDestination
  }
  deriving (Eq, Ord, Show)
