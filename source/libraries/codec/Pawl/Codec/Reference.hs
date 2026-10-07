module Pawl.Codec.Reference where

import Control.Monad ((>=>))
import qualified Data.Char as Char
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Reference as Reference
import qualified Text.Read as Read

-- | One string: @"$bear"@ for a label, @"Grizzly Bears"@ for the first object
-- with that name, @"Grizzly Bears#2"@ for the second, @"trigger of $bear"@ and
-- @"ability of $bear"@ for the bear's newest ability on the stack, @"spell of

-- $bolt"@ for the spell the bolt became when it was cast. A string rather than a
-- tagged object because a scenario is mostly references, and because a
-- reference is also a map key ('Pawl.Codec.Move'\'s blocks and assignments).

codec :: Codec.Codec Reference.Reference
codec =
  Common.text
    { Codec.encode = Codec.encode Common.text . toText,
      Codec.decode = Codec.decode Common.text >=> fromText
    }

toText :: Reference.Reference -> Text.Text
toText reference = case reference of
  Reference.Labelled label -> Text.cons '$' (Label.unwrap label)
  Reference.TriggerOf source -> Text.pack "trigger of " <> toText source
  Reference.AbilityOf source -> Text.pack "ability of " <> toText source
  Reference.SpellOf source -> Text.pack "spell of " <> toText source
  Reference.Printed name occurrence -> case occurrence of
    1 -> CardName.unwrap name
    _ -> CardName.unwrap name <> Text.pack ("#" <> show occurrence)

fromText :: Text.Text -> Either Text.Text Reference.Reference
fromText text
  | Just source <- Text.stripPrefix (Text.pack "trigger of ") text = fmap Reference.TriggerOf (fromText source)
  | Just source <- Text.stripPrefix (Text.pack "ability of ") text = fmap Reference.AbilityOf (fromText source)
  | Just source <- Text.stripPrefix (Text.pack "spell of ") text = fmap Reference.SpellOf (fromText source)
  | otherwise = case Text.uncons text of
      Nothing -> Left (Text.pack "expected a reference but got an empty string")
      Just ('$', rest)
        | Text.null rest -> Left (Text.pack "expected a label after $")
        | otherwise -> Right (Reference.Labelled (Label.MkLabel rest))
      Just _ -> case Text.breakOnEnd (Text.pack "#") text of
        (before, after)
          | Text.null before -> Right (Reference.Printed (CardName.MkCardName text) 1)
          | Text.null after || not (Text.all Char.isDigit after) -> Left (Text.pack "expected a number after # but got " <> after)
          | otherwise -> case Read.readMaybe (Text.unpack after) :: Maybe Natural of
              Nothing -> Left (Text.pack "expected a number after # but got " <> after)
              Just occurrence -> Right (Reference.Printed (CardName.MkCardName (Text.dropEnd 1 before)) occurrence)
