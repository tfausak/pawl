module Pawl.Types.ManaSymbol where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.ManaType as ManaType

-- | CR 107.4. Grows: hybrid Phyrexian (#364).
data ManaSymbol
  = Generic Natural.Natural
  | OfType ManaType.ManaType
  | -- | CR 107.4e: a symbol payable with either of two mana types ({W/U}).
    --
    -- ONE MANA PER SYMBOL, whichever half. CR 107.4e's other half -- the
    -- monocolored hybrid {2/B} -- is MonocoloredHybrid below instead, because it
    -- is the one symbol two mana can pay. Phyrexian (CR 107.4f) is separate for a
    -- different reason again: it can be paid with life rather than mana at all.
    --
    -- A pair rather than a Set, so the printed order survives a round trip. The
    -- two are ALTERNATIVES, so order carries no meaning beyond presentation, and
    -- `Hybrid t t` is degenerate rather than illegal -- it simply means `OfType
    -- t`, and no card prints one.
    Hybrid Hybrid.Hybrid
  | -- | CR 107.4e: {2/B}, payable with either one mana of the stated type or two
    -- mana of any type.
    --
    -- The {2/X} shape ONLY, and the generic half is always two -- CR 107.4 prints
    -- exactly {2/W}, {2/U}, {2/B}, {2/R} and {2/G}, so the number is fixed here
    -- rather than carried, and a {3/B} that no card prints stays unsayable.
    --
    -- NARROWER THAN THE RULE'S NAME: CR 107.4's monocolored hybrid list also
    -- holds {C/W}..{C/G}, which one mana pays either way and which no card in the
    -- pool uses. This constructor is not where those go.
    --
    -- The one symbol two mana can pay, so it breaks the one-supply-per-demand
    -- shape. Pawl.Engine.Mana.resolutions absorbs it by enumerating CR 601.2b's
    -- nonhybrid equivalent costs above the search, so each symbol is still paid
    -- one way at a time.
    MonocoloredHybrid ManaType.ManaType
  | -- | CR 107.4f: payable either with one mana of its colour or by paying 2 life
    -- (Mutagenic Growth).
    --
    -- A Color and not a ManaType, which makes CR 107.4f's first clause true by
    -- construction: the five Phyrexian symbols are all COLORED, so `Phyrexian
    -- Colorless` stays unsayable rather than a case every reader has to rule out.
    -- Pawl.Engine.Projection's symbolColors cashes it: CR 202.2d makes the OBJECT
    -- that colour, so Mutagenic Growth is green even when 2 life paid for it.
    --
    -- The two ways are not two mana. Unlike the hybrids above, one of them spends
    -- no mana at all, so Pawl.Engine.Mana's cost resolution carries an amount of
    -- LIFE alongside its demands and CR 119.4's floor decides whether that way is
    -- open. WHICH way is taken is the player's, announced as they propose the
    -- spell (CR 118.13a) or immediately before a resolution-time cost is paid
    -- (CR 118.13b), so this symbol is gone before the cost is paid.
    --
    -- CR 107.4f's OTHER half -- the ten hybrid Phyrexian symbols ({G/U/P}) -- is
    -- not this constructor and has none of its own (#364).
    Phyrexian Color.Color
  | -- | CR 107.4h: {S} in a cost, payable with one mana of any type produced by a
    -- snow source (CR 106.3). Icehide Golem, whose whole cost this is.
    --
    -- CARRIES NOTHING, and the two things it might have carried are both wrong.
    -- Not a ManaType, because CR 107.4h makes snow neither a colour nor a type of
    -- mana -- ANY type pays it. Not a Generic 1 either, because CR 107.4h
    -- separates the two: generic-mana reductions do not affect {S}. So this is
    -- the first symbol whose payability turns on how a mana was PRODUCED rather
    -- than on what it is -- Pawl.Types.ProductionTag.Snow is that fact.
    --
    -- ONE neighbouring rule is not implemented: CR 107.4h's third sentence, {S}
    -- referring to snow-produced mana SPENT to pay a cost. Real cards do state
    -- this, none of them in the pool (#515).
    --
    -- The other two neighbours are done, and both are elsewhere. CR 106.11 --
    -- adding mana represented by {S} -- is Pawl.Types.ManaProduction.SnowSymbol,
    -- rewritten to colorless mana by Pawl.Engine.Mana.producedTypes. CR 118.7g --
    -- a reduction BY {S}, which the rule turns back into a plain generic
    -- reduction -- is why Pawl.Engine.Cost.applyAdjustments asks costGenericOf on
    -- the cost side and reducingGenericOf on the reduction side.
    Snow
  | -- | CR 107.3 / 601.2b: the {X} symbol in a cost. Contributes the chosen value
    -- of X once chosen (0 before, for the CR 601.2b castability floor and for a
    -- mana value off the stack, CR 202.3e).
    Variable
  deriving (Eq, Ord, Show)
