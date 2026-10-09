module Pawl.Types.StickerSheet where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AbilitySticker as AbilitySticker
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker

-- | CR 123.2: one sticker sheet. Not a card; no characteristics.
data StickerSheet = MkStickerSheet
  { -- | Its collector number in Scryfall's set sunf.
    number :: Natural.Natural,
    name :: Text.Text,
    -- | CR 123.6: the name stickers' words, one entry per sticker.
    names :: Seq.Seq Text.Text,
    -- | CR 123.9: how many art stickers; they carry no data.
    art :: Natural.Natural,
    abilities :: Seq.Seq AbilitySticker.AbilitySticker,
    powerToughness :: Seq.Seq PowerToughnessSticker.PowerToughnessSticker,
    -- | MTGJSON's text, which Pawl.StickerSpec's lint ties the fields above to.
    oracleText :: Maybe Text.Text
  }
  deriving (Eq, Ord, Show)
