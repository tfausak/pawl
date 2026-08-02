module Pawl.Types.ManaSymbol where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaType as ManaType

-- | CR 107.4. Grows: hybrid Phyrexian (#364).
data ManaSymbol
  = Generic Natural.Natural
  | OfType ManaType.ManaType
  | -- | CR 107.4e: "Each one represents a cost that can be paid in one of two ways,
    -- as represented by the two halves of the symbol. A hybrid symbol such as
    -- {W/U} can be paid with either white or blue mana."
    --
    -- ONE MANA PER SYMBOL, whichever half. CR 107.4e's other half -- the
    -- monocolored hybrid {2/B} -- is MonocoloredHybrid below instead, because it
    -- is the one symbol two mana can pay. Phyrexian (CR 107.4f) is separate for a
    -- different reason again -- it can be paid with life rather than with mana at
    -- all -- and is Phyrexian below.
    --
    -- A pair rather than a Set, so the printed order survives a round trip. The
    -- two are ALTERNATIVES, so order carries no meaning beyond presentation, and
    -- `Hybrid t t` is degenerate rather than illegal -- it simply means `OfType
    -- t`, and no card prints one.
    Hybrid ManaType.ManaType ManaType.ManaType
  | -- | CR 107.4e: "a monocolored hybrid symbol such as {2/B} can be paid with
    -- either one black mana or two mana of any type."
    --
    -- The {2/X} shape ONLY, and the generic half is always two -- CR 107.4 prints
    -- exactly {2/W}, {2/U}, {2/B}, {2/R} and {2/G}, so the number is fixed here
    -- rather than carried, and a {3/B} that no card prints stays unsayable. The
    -- ManaType is the other half, the one a single mana satisfies.
    --
    -- NARROWER THAN THE RULE'S NAME. CR 107.4's "monocolored hybrid symbols" list
    -- also holds {C/W}..{C/G}, which one mana pays either way and which no card
    -- in the pool uses; nothing here represents those, and this constructor is
    -- not where they go.
    --
    -- This is the symbol that two mana can pay, and so the one that broke the
    -- one-supply-per-demand shape Pawl.Engine.Mana's payment used to rest on.
    -- Pawl.Engine.Mana.resolutions is what absorbs it: CR 601.2b's "nonhybrid equivalent
    -- cost" is enumerated above the search, so each symbol is still paid one way
    -- at a time and nothing below ever sees a demand that eats two units.
    MonocoloredHybrid ManaType.ManaType
  | -- | CR 107.4f: "A Phyrexian mana symbol represents a cost that can be paid
    -- either with one mana of its color or by paying 2 life" (Mutagenic Growth).
    --
    -- A Color and not a ManaType, which makes CR 107.4f's FIRST clause true by
    -- construction: "Phyrexian mana symbols are COLORED mana symbols: {W/P} is
    -- white, {U/P} is blue, {B/P} is black, {R/P} is red, and {G/P} is green."
    -- Those five are the whole list -- there is no colourless Phyrexian symbol to
    -- represent -- so `Phyrexian Colorless` stays unsayable rather than being a
    -- case every reader of the colour has to rule out. Pawl.Engine.Projection's
    -- symbolColors is the one that cashes it: CR 202.2d makes the OBJECT that
    -- colour, so Mutagenic Growth is green even when 2 life paid for it and no
    -- green mana was ever made.
    --
    -- The two ways are not two mana. Unlike the hybrids above, one of them spends
    -- no mana at all, so Pawl.Engine.Mana's cost resolution carries an amount of LIFE
    -- alongside its demands, and CR 119.4's floor ("only if their life total is
    -- greater than or equal to the amount of the payment") is what decides
    -- whether that way is open. WHICH way is taken is the player's, announced as
    -- they propose the spell (CR 118.13a) by Pawl.Engine.Mana's announcePhyrexian, so
    -- this symbol is gone before the cost is paid at all.
    --
    -- CR 107.4f's OTHER half -- the ten hybrid Phyrexian symbols, "{G/U/P}", paid
    -- with either of two colours or with 2 life -- is not this constructor and
    -- has none of its own (#364).
    Phyrexian Color.Color
  | -- | CR 107.4h: "When used in a cost, the snow mana symbol {S} represents a cost
    -- that can be paid with one mana of any type produced by a snow source (see
    -- rule 106.3)." Icehide Golem, whose whole cost this is.
    --
    -- CARRIES NOTHING, and the two things it might have carried are both wrong.
    -- Not a ManaType, because CR 107.4h's last sentence forbids it: "Snow is
    -- neither a color nor a type of mana" -- ANY type of mana pays it. Not a
    -- Generic 1 either, because CR 107.4h's second sentence separates the two
    -- outright: "Effects that reduce the amount of generic mana you pay don't
    -- affect {S} costs." So this is the first symbol whose payability turns on
    -- how a mana was PRODUCED rather than on what it is --
    -- Pawl.Types.ProductionTag.Snow is that fact, and Pawl.Engine.Mana.waysOf
    -- turns this symbol into a demand for one mana of any type carrying it.
    --
    -- The OTHER direction is not this constructor. CR 106.11 -- "if an effect
    -- would add mana represented by one or more snow mana symbols ... that much
    -- colorless mana is added" -- is about PRODUCING mana, which is
    -- Pawl.Types.ManaProduction's business, and nothing there can say {S}
    -- (#514). That rule appears to be INERT rather than merely unimplemented:
    -- no printed card adds mana represented by {S}. Checked against Scryfall
    -- 2026-08-02 -- 44 cards carry {S} in their oracle text and every one of
    -- them is a COST, with `oracle:"add {S}"` returning none.
    --
    -- CR 118.7g's reduction BY {S} is a third thing again, and the rule turns it
    -- back into a plain generic one: "the cost is reduced by that much generic
    -- mana." The SYMBOL is not generic and the REDUCTION it names is, which is
    -- why that does not contradict CR 107.4h's second sentence -- and why
    -- Pawl.Engine.Cost.applyAdjustments, which answers one question for both
    -- sides, gets the reduction side wrong (#516). That rule looks inert too:
    -- no printed card states a reduction in {S} (same Scryfall check, `costs
    -- {S} less` and `{S} less to cast` both return none).
    --
    -- CR 107.4h's own third sentence is not implemented either: "The {S} symbol
    -- can also be used to refer to mana of any type produced by a snow source
    -- SPENT to pay a cost" (#515).
    Snow
  | -- | CR 107.3 / 601.2b: the {X} symbol in a cost. Contributes the chosen value
    -- of X once chosen (0 before, for the CR 601.2b castability floor and for a
    -- mana value off the stack, CR 202.3e).
    Variable
  deriving (Eq, Ord, Show)
