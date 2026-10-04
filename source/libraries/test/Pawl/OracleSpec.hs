-- Covers Pawl.Oracle and Pawl.Ingest, and every keyword-only card file against
-- its Oracle text (#9).
module Pawl.OracleSpec where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Keyword as Codec.Keyword
import qualified Pawl.Ingest as Ingest
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.Oracle as Oracle
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TypeLine as TypeLine

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Pawl.Oracle" $ do
  Spec.it s "each keyword-only card says what its Oracle text says" $ do
    loaded <- Registry.defaultRoot >>= Registry.loadRoot
    let keywordOnly = [(card, rendered) | entry@(_, Right card) <- loaded, Maybe.isJust (Registry.referenceCard entry), Just rendered <- [Oracle.render card]]
    Spec.assertBool s (not (null keywordOnly)) "no keyword-only cards"
    Monad.forM_ keywordOnly $ \(card, rendered) -> do
      let name = Text.unpack (Slug.unwrap (Registry.filedAs card))
      case Face.oracleText (NonEmpty.head (Card.faces card)) of
        Nothing -> Spec.assertFailure s (name <> ": no Oracle text")
        Just text -> Spec.assertEqWith s name (Oracle.normalise text) (Oracle.normalise rendered)

  -- `pawl ingest` stamps the text on every card MTGJSON has; a dungeon (CR
  -- 309) is not among them, since MTGJSON files dungeons with the tokens.
  Spec.it s "every face of the reference carries its Oracle text" $ do
    loaded <- Registry.defaultRoot >>= Registry.loadRoot
    let missing =
          [ Text.unpack (CardName.unwrap (Face.name face))
          | entry@(_, Right card) <- loaded,
            Maybe.isJust (Registry.referenceCard entry),
            face <- NonEmpty.toList (Card.faces card),
            Maybe.isNothing (Face.oracleText face),
            CardType.Dungeon `Set.notMember` TypeLine.types (Face.typeLine face)
          ]
    Spec.assertEq s [] missing

  Spec.it s "normalise drops reminder text and splits a keyword line" $ do
    Spec.assertEq s (fmap Text.pack ["flying", "trample", "vigilance"]) (Oracle.normalise (Text.pack "Flying, vigilance\nTrample (This creature can deal excess combat damage.)"))
    Spec.assertEq s [] (Oracle.normalise (Text.pack "({T}: Add {G}.)"))

  Spec.it s "render answers only for a single face of characteristics and keywords" $ do
    registry <- Registry.defaultRoot >>= Registry.fileRegistry
    elves <- Registry.named registry "Llanowar Elves"
    Spec.assertEq s (Just Nothing) (fmap Oracle.render elves)
    angel <- Registry.named registry "Serra Angel"
    Spec.assertEq s (Just (Just (Text.pack "Flying\nVigilance"))) (fmap Oracle.render angel)

  Spec.it s "each standalone keyword prints as its codec tag spells it" $
    Monad.forM_
      Oracle.standalone
      ( \k -> case Oracle.printed k of
          Nothing -> Spec.assertFailure s (show k <> ": no printed form")
          Just p -> Spec.assertEqWith s (show k) (Codec.encode Codec.Keyword.codec k) (Common.nullary (tagOf p))
      )

  -- The witness that 'Oracle.standalone' is complete: every arm the keyword
  -- codec accepts with no value is listed, or is one of the three whose bare
  -- form prints a parameter or is never printed at all.
  Spec.it s "standalone lists every keyword the codec writes as a bare tag" $ do
    let unprinted = [Keyword.Bloodthirst Nothing, Keyword.Modular Nothing, Keyword.Suspend Nothing]
    case bareTags (Define.run (Codec.schema Codec.Keyword.codec)) of
      Left reason -> Spec.assertFailure s (Text.unpack reason)
      Right tags -> do
        Spec.assertEq s (length tags) (length Oracle.standalone + length unprinted)
        Monad.forM_ tags $ \tag -> case Codec.decode Codec.Keyword.codec (Common.nullary tag) of
          Left reason -> Spec.assertFailure s (tag <> ": " <> Text.unpack reason)
          Right k -> Spec.assertBool s (k `elem` Oracle.standalone || k `elem` unprinted) (tag <> ": not in Pawl.Oracle.standalone")

  -- Each record as script/ingest/candidates.jq writes it, against a card
  -- written by hand and checked against Scryfall, so the two are independent.
  -- Rograkh is red only by its color indicator (CR 204), and both cost {0},
  -- which the pool writes as a cost of no symbols.
  Spec.it s "candidate builds the card the pool holds" $ do
    registry <- Registry.defaultRoot >>= Registry.fileRegistry
    Monad.forM_
      [ ("Ornithopter", "{\"name\":\"Ornithopter\",\"manaCost\":\"{0}\",\"colorIndicator\":null,\"supertypes\":[],\"types\":[\"Artifact\",\"Creature\"],\"subtypes\":[\"Thopter\"],\"power\":\"0\",\"toughness\":\"2\",\"text\":\"Flying\"}"),
        ("Rograkh, Son of Rohgahh", "{\"name\":\"Rograkh, Son of Rohgahh\",\"manaCost\":\"{0}\",\"colorIndicator\":[\"R\"],\"supertypes\":[\"Legendary\"],\"types\":[\"Creature\"],\"subtypes\":[\"Kobold\",\"Warrior\"],\"power\":\"0\",\"toughness\":\"1\",\"text\":\"First strike, menace, trample\\nPartner (You can have two commanders if both have partner.)\"}")
      ]
      $ \(name, json) -> do
        card <- Registry.named registry name
        case (Common.parse (Text.pack json), card) of
          (Right value, Just expected) -> Spec.assertEqWith s name (Right expected) (Ingest.candidate value)
          _ -> Spec.assertFailure s (name <> ": no record or no card")

  -- Replenish is reprinted as the spell face of Eiganjo Dynastorian, whose
  -- entry drops the reminder text; the card's own entry wins.
  Spec.it s "stamp prefers the entry naming the card itself" $ do
    registry <- Registry.defaultRoot >>= Registry.fileRegistry
    replenish <- Registry.named registry "Replenish"
    let own = "Return all enchantment cards from your graveyard to the battlefield. (Auras with nothing to enchant remain in your graveyard.)"
        json = "[{\"card\":\"Eiganjo Dynastorian // Replenish\",\"name\":\"Replenish\",\"text\":\"Return all enchantment cards from your graveyard to the battlefield.\"},{\"card\":\"Replenish\",\"name\":\"Replenish\",\"text\":\"" <> own <> "\"}]"
        unstamped c = c {Card.faces = fmap (\f -> f {Face.oracleText = Nothing}) (Card.faces c)}
    case (Common.parse (Text.pack json) >>= Ingest.texts, replenish) of
      (Right known, Just card) -> Spec.assertEq s (Just (Text.pack own)) (Face.oracleText (NonEmpty.head (Card.faces (Ingest.stamp known (unstamped card)))))
      _ -> Spec.assertFailure s "Replenish: no texts or no card"

  Spec.it s "candidate leaves out a card it would have to guess at" $ do
    let reason json = Common.parse (Text.pack json) >>= Monad.void . Ingest.candidate
    Spec.assertEq s (Left (Text.pack "text: ward {2}")) (reason "{\"name\":\"X\",\"manaCost\":\"{1}\",\"supertypes\":[],\"types\":[\"Creature\"],\"subtypes\":[],\"power\":\"1\",\"toughness\":\"1\",\"text\":\"Ward {2}\"}")
    Spec.assertEq s (Left (Text.pack "power/toughness: *")) (reason "{\"name\":\"X\",\"manaCost\":\"{1}\",\"supertypes\":[],\"types\":[\"Creature\"],\"subtypes\":[],\"power\":\"*\",\"toughness\":\"1\",\"text\":null}")
    Spec.assertEq s (Left (Text.pack "subtype: Nonesuch")) (reason "{\"name\":\"X\",\"manaCost\":\"{1}\",\"supertypes\":[],\"types\":[\"Creature\"],\"subtypes\":[\"Nonesuch\"],\"power\":\"1\",\"toughness\":\"1\",\"text\":null}")

-- A printed keyword as its constructor's name: each word capitalised, then
-- everything but letters dropped ("Jump-start" is JumpStart).
tagOf :: Text.Text -> String
tagOf = concatMap (filter Char.isAlpha . capitalise . Text.unpack) . Text.split (`elem` " -")
  where
    capitalise w = case w of
      c : rest -> Char.toUpper c : rest
      [] -> []

-- Every tag of the Keyword union whose arm requires nothing but the tag.
bareTags :: Value.Value -> Either Text.Text [String]
bareTags schema = do
  defs <- Common.asObject schema >>= Common.field "$defs" >>= Common.asObject
  arms <- Common.field "Keyword" defs >>= Common.asObject >>= Common.field "oneOf" >>= Common.asArray
  fmap Maybe.catMaybes . Monad.forM arms $ \arm -> do
    fields <- Common.asObject arm
    required <- Common.field "required" fields >>= Common.asArray >>= traverse Common.asText
    tag <- Common.field "properties" fields >>= Common.asObject >>= Common.field "type" >>= Common.asObject >>= Common.field "const" >>= Common.asText
    pure (if required == [Text.pack "type"] then Just (Text.unpack tag) else Nothing)
