module Pawl.Types.TurnUpR where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

-- | The payload of Pawl.Types.ReplacementEffect's TurnUpR arm (#1305): which
-- permanents being turned face up are intercepted (CR 614.1e), and how the
-- turning-over is rewritten.
data TurnUpR = MkTurnUpR
  { matching :: Filter.Filter Keyword.Keyword,
    -- | WHICH of CR 708.7's procedures the row is conditional on, or Nothing for
    -- every road up. A RULE's condition and never a card's: CR 702.37b's second
    -- clause is "put a +1/+1 counter on it IF ITS MEGAMORPH COST WAS PAID to turn
    -- it face up", and CR 702.37e's procedure is the only one that pays it, so
    -- Pawl.Engine.Keyword.mintedReplacementsFor's megamorph arm mints Just Morph
    -- here. CR 701.40c's second road up a manifested megamorph card pays that
    -- card's MANA cost, and an Effect.TurnFaceUp names no procedure at all;
    -- neither pays a megamorph cost, and both are refused.
    --
    -- ON THE ROW rather than on the rewrite class, because CR 614.1e applies down
    -- every road and a card's own "as this creature is turned face up" clause
    -- carries no such condition -- Bubble Smuggler's four +1/+1 counters land off
    -- CR 702.168d's disguise procedure. Gating the WithCounters constructor
    -- instead would refuse them; see #987.
    --
    -- ENGINE-BAKED, like Pawl.Types.PhasePattern's whosePhase: the codec accepts
    -- it so a minted row round-trips, and Pawl.CardSpec holds that no printing
    -- authors one.
    requiring :: Maybe TurnUpProcedure.TurnUpProcedure,
    rewrite :: TurnUpRewrite.TurnUpRewrite
  }
  deriving (Eq, Ord, Show)
