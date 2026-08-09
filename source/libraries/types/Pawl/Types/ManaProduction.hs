module Pawl.Types.ManaProduction where

import qualified Pawl.Types.ManaType as ManaType

-- | CR 106.3: how an AddMana effect decides WHICH mana it puts into the pool.
-- One fixed type -- Llanowar Elves' "Add {G}"; one mana of a colour the producing
-- player chooses (Birds of Paradise), which CR 105.4 restricts to the five
-- colours, never colourless; the colour an earlier linked ability chose
-- (Coldsteel Heart); or the snow mana symbol CR 106.11 rewrites.
--
-- Data hanging off the one AddMana opcode rather than a second opcode: "add one
-- mana" is a single instruction, and what varies is a payload saying how its type
-- is determined. Nothing in the rules core cases on this -- it asks
-- Mana.producedTypes for the options and prompts among them, which is the only
-- obligation a future constructor ("of any type") would carry.
data ManaProduction
  = OfType ManaType.ManaType
  | AnyColor
  | -- | CR 607.2d: an ability referring to "the chosen color" is linked to the
    -- ability that caused the choice, and adds mana of THAT colour. Coldsteel
    -- Heart's "{T}: Add one mana of the chosen color", linked to its "As this
    -- artifact enters, choose a color".
    --
    -- Read off Object.chosenColor, which is where CR 614.1c's entry rewrite
    -- writes -- the same field Modification.AddChosenColor reads for Painter's
    -- Servant, and the same link CR 607.2d describes. NOT A CHOICE: the colour is
    -- already settled, so producedTypes offers one option and nothing prompts,
    -- which is what separates this from AnyColor.
    --
    -- No colour chosen yields NO option rather than a fallback colour. A
    -- permanent whose entry rewrite ran is never in that state (CR 614.1c makes
    -- the choice AS it enters), and inventing a colour for one placed onto the
    -- battlefield by a fixture would be the engine making a player's choice.
    Chosen
  | -- | CR 106.11: an effect that would add mana represented by a snow mana
    -- symbol adds that much COLORLESS mana instead. One symbol, one mana, so
    -- "add {S}{S}" is two of these exactly as "add {C}{C}" is two OfType
    -- Colorless -- Pawl.Engine.Mana.producedTypes is where the rewrite happens.
    --
    -- NOT spelled `OfType Colorless` in card data, even though the mana that
    -- reaches the pool is identical. The two differ in what they transcribe: a
    -- card printed "add {S}" says {S}, and pre-applying CR 106.11 by hand while
    -- writing the JSON would move a rule out of the closed half and into whoever
    -- typed the file, where nothing checks it. The engine owns its own rewrite.
    --
    -- Carries NO snow-ness. CR 107.4h asks whether the SOURCE is snow (CR
    -- 106.3), never which symbol the effect was written with, so the
    -- Pawl.Types.ProductionTag.Snow tag is decided exactly as it is for every
    -- other production -- and a nonsnow permanent that adds {S} adds mana that
    -- cannot pay {S}.
    SnowSymbol
  deriving (Eq, Ord, Show)
