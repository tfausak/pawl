module Pawl.Type.ManaSymbol where

import Numeric.Natural (Natural)
import Pawl.Type.ManaType (ManaType)

-- CR 107.4. Grows: Phyrexian, snow.
data ManaSymbol
  = Generic Natural
  | OfType ManaType
  | -- CR 107.4e: "Each one represents a cost that can be paid in one of two ways,
    -- as represented by the two halves of the symbol. A hybrid symbol such as
    -- {W/U} can be paid with either white or blue mana."
    --
    -- ONE MANA PER SYMBOL, whichever half. CR 107.4e's other half -- the
    -- monocolored hybrid {2/B} -- is MonocoloredHybrid below instead, because it
    -- is the one symbol two mana can pay. Phyrexian (CR 107.4f) is absent for a
    -- different reason again: it can be paid with life rather than mana at all
    -- (#263).
    --
    -- A pair rather than a Set, so the printed order survives a round trip. The
    -- two are ALTERNATIVES, so order carries no meaning beyond presentation, and
    -- `Hybrid t t` is degenerate rather than illegal -- it simply means `OfType
    -- t`, and no card prints one.
    Hybrid ManaType ManaType
  | -- CR 107.4e: "a monocolored hybrid symbol such as {2/B} can be paid with
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
    -- one-supply-per-demand shape Pawl.Mana's payment used to rest on.
    -- Pawl.Mana.resolutions is what absorbs it: CR 601.2b's "nonhybrid equivalent
    -- cost" is enumerated above the search, so each symbol is still paid one way
    -- at a time and nothing below ever sees a demand that eats two units.
    MonocoloredHybrid ManaType
  | -- CR 107.3 / 601.2b: the {X} symbol in a cost. Contributes the chosen value
    -- of X once chosen (0 before, for the CR 601.2b castability floor and for a
    -- mana value off the stack, CR 202.3b).
    Variable
  deriving (Eq, Ord, Show)
