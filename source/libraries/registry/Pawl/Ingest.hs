-- Building a card from one record of script/ingest/candidates.jq's reduction
-- of MTGJSON (#9), for the cards 'Pawl.Oracle.render' answers for: a single
-- face whose text is keywords alone. Everything here reads a field MTGJSON
-- carries as structured data (CR 202.1, CR 205) or a keyword name from
-- 'Pawl.Oracle.standalone'; a word it does not know disqualifies the card
-- rather than being guessed at.
module Pawl.Ingest where

import qualified Control.Applicative as Applicative
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Oracle as Oracle
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Text.Read as Read

-- | The card a candidate record means, its Oracle text verbatim (empty where
-- MTGJSON has none), or why it is not one this builds.
candidate :: Value.Value -> Either Text.Text Card.Card
candidate value = do
  fields <- Common.asObject value
  let nullable key = case Common.optionalField key fields of
        Nothing -> Right Nothing
        Just (Value.Null _) -> Right Nothing
        Just v -> fmap Just (Common.asText v)
      words' key = Common.field key fields >>= Common.asArray >>= traverse Common.asText
  name <- Common.field "name" fields >>= Common.asText
  cost <- nullable "manaCost" >>= traverse manaCost
  indicator <- case Common.optionalField "colorIndicator" fields of
    Nothing -> Right []
    Just (Value.Null _) -> Right []
    Just v -> Common.asArray v >>= traverse Common.asText >>= traverse colour
  supertypes <- words' "supertypes" >>= traverse (enumWord "supertype")
  types <- words' "types" >>= traverse (enumWord "type")
  subtypes <- words' "subtypes" >>= traverse (enumWord "subtype")
  power <- nullable "power" >>= traverse (fmap Power.MkPower . literal)
  toughness <- nullable "toughness" >>= traverse (fmap Toughness.MkToughness . literal)
  text <- fmap (Maybe.fromMaybe Text.empty) (nullable "text")
  keywords <- traverse keyword (Oracle.normalise text)
  let typeLine =
        TypeLine.MkTypeLine
          { TypeLine.supertypes = Set.fromList (supertypes :: [Supertype.Supertype]),
            TypeLine.types = Set.fromList (types :: [CardType.CardType]),
            TypeLine.subtypes = Set.fromList (subtypes :: [Subtype.Subtype])
          }
      face = Oracle.bare (CardName.MkCardName name) (Just text) cost (Set.fromList indicator) typeLine power toughness (Map.fromListWith (+) (fmap (\k -> (k, 1)) keywords))
  pure Card.MkCard {Card.layout = Layout.Normal, Card.faces = face NonEmpty.:| []}

-- | Every face's Oracle text from the reduction's @texts@, keyed by card name
-- (faces joined by " // ", as MTGJSON names a card) and face name; and, for a
-- face name only one face prints, by face name alone, for a card the pool
-- joins differently (Replenish is also a face of Eiganjo Dynastorian).
texts :: Value.Value -> Either Text.Text (Map.Map (Text.Text, Text.Text) Text.Text, Map.Map Text.Text Text.Text)
texts value = do
  entries <- Common.asArray value >>= traverse entry
  let byFace = Map.fromListWith Set.union [(name, Set.singleton text) | (_, name, text) <- entries]
  pure
    ( Map.fromList [((card, name), text) | (card, name, text) <- entries],
      Map.mapMaybe (\set -> case Set.toList set of [one] -> Just one; _ -> Nothing) byFace
    )
  where
    entry v = do
      fields <- Common.asObject v
      card <- Common.field "card" fields >>= Common.asText
      name <- Common.field "name" fields >>= Common.asText
      text <- Common.field "text" fields >>= Common.asText
      pure (card, name, text)

-- | The card with each face's Oracle text set from 'texts', where it has one.
stamp :: (Map.Map (Text.Text, Text.Text) Text.Text, Map.Map Text.Text Text.Text) -> Card.Card -> Card.Card
stamp (exact, byFace) card =
  card {Card.faces = fmap (\face -> maybe face (\text -> face {Face.oracleText = Just text}) (lookupFace (CardName.unwrap (Face.name face)))) (Card.faces card)}
  where
    cardName = Text.intercalate (Text.pack " // ") (fmap (CardName.unwrap . Face.name) (NonEmpty.toList (Card.faces card)))
    lookupFace name = Map.lookup (cardName, name) exact Applicative.<|> Map.lookup name byFace

-- | A line of normalised Oracle text as the keyword it names.
keyword :: Text.Text -> Either Text.Text Keyword.Keyword
keyword line = maybe (Left (Text.pack "text: " <> line)) Right (Map.lookup line byPrinted)

byPrinted :: Map.Map Text.Text Keyword.Keyword
byPrinted = Map.fromList [(Text.toLower p, k) | k <- Oracle.standalone, Just p <- [Oracle.printed k]]

-- | A type-line word as the enumerated constructor it spells: words joined and
-- everything but letters dropped, so "Assembly-Worker" is AssemblyWorker and
-- "Urza's" is Urzas.
enumWord :: (Bounded a, Enum a, Show a) => String -> Text.Text -> Either Text.Text a
enumWord kind word =
  case filter ((== spelled) . show) [minBound .. maxBound] of
    [x] -> Right x
    _ -> Left (Text.pack (kind <> ": ") <> word)
  where
    spelled = concatMap (filter Char.isAlpha . capitalise . Text.unpack) (Text.split (`elem` " -") word)
    capitalise s = case s of
      c : rest -> Char.toUpper c : rest
      [] -> []

-- | A color indicator's letter (CR 204, CR 105.1).
colour :: Text.Text -> Either Text.Text Color.Color
colour t = case Text.unpack t of
  [c] | Just (ManaType.Colored x) <- manaType c -> Right x
  _ -> Left (Text.pack "color indicator: " <> t)

-- | A printed power or toughness that is a plain number. Anything else
-- disqualifies, a star (CR 208.2) above all.
literal :: Text.Text -> Either Text.Text Quantity.Quantity
literal t =
  if not (Text.null t) && Text.all Char.isDigit t
    then maybe (Left (Text.pack "power/toughness: " <> t)) (Right . Quantity.Literal) (Read.readMaybe (Text.unpack t))
    else Left (Text.pack "power/toughness: " <> t)

-- | A mana cost in MTGJSON's spelling, "{2}{W}{W}" (CR 202.1, CR 107.4). The
-- pool writes {0} as a cost of no symbols, which is still a cost, unlike a
-- land's absent one.
manaCost :: Text.Text -> Either Text.Text ManaCost.ManaCost
manaCost t = case Text.uncons t of
  Nothing -> Right (ManaCost.MkManaCost [])
  Just _ -> fmap (ManaCost.MkManaCost . filter (/= ManaSymbol.Generic 0)) (traverse symbol (symbols t))
  where
    symbols = filter (not . Text.null) . fmap (Text.dropWhile (== '{')) . Text.splitOn (Text.pack "}")
    bad s = Left (Text.pack "mana symbol: {" <> s <> Text.pack "}")
    symbol s = case Text.unpack s of
      "X" -> Right ManaSymbol.Variable
      "S" -> Right ManaSymbol.Snow
      [c] | Just m <- manaType c -> Right (ManaSymbol.OfType m)
      [c, '/', 'P'] | Just (ManaType.Colored x) <- manaType c -> Right (ManaSymbol.Phyrexian x)
      ['2', '/', c] | Just m <- manaType c -> Right (ManaSymbol.MonocoloredHybrid m)
      [a, '/', b] | Just l <- manaType a, Just r <- manaType b -> Right (ManaSymbol.Hybrid Hybrid.MkHybrid {Hybrid.left = l, Hybrid.right = r})
      digits | all Char.isDigit digits, Just n <- Read.readMaybe digits -> Right (ManaSymbol.Generic n)
      _ -> bad s

-- | A mana symbol's letter as the type of mana it stands for (CR 107.4).
manaType :: Char -> Maybe ManaType.ManaType
manaType c = case c of
  'W' -> Just (ManaType.Colored Color.White)
  'U' -> Just (ManaType.Colored Color.Blue)
  'B' -> Just (ManaType.Colored Color.Black)
  'R' -> Just (ManaType.Colored Color.Red)
  'G' -> Just (ManaType.Colored Color.Green)
  'C' -> Just ManaType.Colorless
  _ -> Nothing
