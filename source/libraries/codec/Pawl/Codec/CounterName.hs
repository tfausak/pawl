module Pawl.Codec.CounterName where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterKindFamily as CounterKindFamily
import qualified Pawl.Types.CounterName as CounterName

-- | A bare string on the wire, filed in @$defs@ under its own name, exactly as
-- 'Pawl.Codec.AbilityName' is.
--
-- The rejection lives in the INNER codec rather than in the wrap, because
-- 'Common.wrapper' takes a total injection and so cannot fail.
codec :: Codec.Codec CounterName.CounterName
codec = Common.wrapper unreserved CounterName.UnsafeMkCounterName CounterName.unwrap

-- | 'Common.text' with its decoder tightened to CR 122.1's interchangeability
-- sentence -- a record update, so the encoder and the schema stay shared. A card
-- naming a spelling 'Pawl.Types.CounterKind' already has a constructor for fails
-- to DECODE, so the corpus cannot hold two kinds for one rules object. It goes
-- through 'make' rather than restating the test, so there is one door and one
-- invariant.
--
-- The schema still says only "string": 'reserved' is a blocklist rather than a
-- pattern, so the decoder is STRICTER than the schema claims -- the reverse of
-- the governing principle in 'Pawl.JsonCodec.Common', and deliberate. Stating it
-- in the schema means a negative regex with one alternative per reserved
-- spelling, regenerated whenever a kind is added, which is a second copy of the
-- same list.
unreserved :: Codec.Codec Text.Text
unreserved =
  Common.text
    { Codec.decode = \value -> CounterName.unwrap <$> (Common.asText value >>= make)
    }

-- | The only door into 'Pawl.Types.CounterName.CounterName'. Rejects a
-- 'reserved' spelling, and rejects the empty name, which no card prints and
-- which would read as "the counter with no name".
--
-- It lives in the codec rather than beside the type because 'reserved' does:
-- card data is the only thing that names a counter, and this is where card data
-- comes in.
make :: Text.Text -> Either Text.Text CounterName.CounterName
make text
  | Text.null text = Left (Text.pack "CounterName: a counter's name must not be empty")
  | Set.member text reserved =
      Left
        ( Text.pack "CounterName: CR 122.1 makes counters with the same name interchangeable, and Pawl.Types.CounterKind already names "
            <> text
        )
  | otherwise = Right (CounterName.UnsafeMkCounterName text)

-- | The spellings a card may NOT name, because 'Pawl.Types.CounterKind' already
-- has a constructor for each: a card writing @Named "flying"@ while another
-- writes @Keyword Flying@ would make two Map keys on @Object.counters@ for one
-- rules object, and every count read would split between them. CR 122.1 says
-- those two counters are interchangeable, so representing them apart is a
-- rules-observable divergence rather than an inconvenience.
--
-- DERIVED, not hand-kept. 'CounterKindFamily.CounterKindFamily' derives Bounded
-- and Enum, so the fold below is over every constructor the compiler knows of;
-- 'spellings' is exhaustive over that type and 'familyOf' is exhaustive over
-- 'CounterKind.CounterKind', so a new kind cannot reach a card without someone
-- answering what a card would print for it.
--
-- A blocklist fences only what it can name, and CR 122.1b's list of keyword
-- counters ends in "as well as any variants of those keywords" -- an open clause,
-- so no enumeration written here can be complete. 'spellings' reserves the
-- fifteen the rule spells out, and a variant spelling -- "hexproof from black" --
-- goes through. That is a standing property of a blocklist against an open rule
-- rather than a slot left unbuilt: a card naming a variant is caught where the
-- card is reviewed.
reserved :: Set.Set Text.Text
reserved = foldMap spellings [minBound .. maxBound]

-- | What a card prints for each kind, and the reason 'reserved' cannot go stale:
-- the case is exhaustive and takes no wildcard, so a new family has to be
-- spelled here before this module compiles.
--
-- 'CounterKindFamily.Named' answers the empty set rather than a spelling, and it
-- is the only arm that may: those are the names 'make' checks, so reserving one
-- would reserve every name at once.
spellings :: CounterKindFamily.CounterKindFamily -> Set.Set Text.Text
spellings family = case family of
  CounterKindFamily.PlusOnePlusOne -> one "+1/+1"
  CounterKindFamily.MinusOneMinusOne -> one "-1/-1"
  -- CR 122.1b's fifteen, in the rule's own order. The payload is dropped by
  -- 'familyOf', so one arm answers for every keyword counter -- which is why
  -- this arm is a list where the others are a single spelling.
  CounterKindFamily.Keyword ->
    Set.fromList $
      fmap
        Text.pack
        [ "flying",
          "first strike",
          "double strike",
          "deathtouch",
          "decayed",
          "exalted",
          "haste",
          "hexproof",
          "indestructible",
          "lifelink",
          "menace",
          "reach",
          "shadow",
          "trample",
          "vigilance"
        ]
  CounterKindFamily.Loyalty -> one "loyalty"
  CounterKindFamily.Lore -> one "lore"
  CounterKindFamily.Defense -> one "defense"
  CounterKindFamily.Time -> one "time"
  CounterKindFamily.Fade -> one "fade"
  CounterKindFamily.Shield -> one "shield"
  CounterKindFamily.Finality -> one "finality"
  CounterKindFamily.Stun -> one "stun"
  CounterKindFamily.Level -> one "level"
  CounterKindFamily.Hone -> one "hone"
  CounterKindFamily.Named -> Set.empty
  where
    one = Set.singleton . Text.pack

-- | Pawl.Engine.Keyword.familyOf's twin, one layer down because 'reserved' is
-- needed here. Exhaustive and wildcard-free: this is the link that makes a new
-- 'CounterKind.CounterKind' constructor a compile error rather than a silently
-- unreserved spelling.
--
-- PARAMETRIC in the keyword, since the answer never reads the payload -- CR
-- 122.1b's arm reserves all fifteen spellings at once, and CR 122.1's named arm
-- reserves none.
--
-- No production caller; the spec is its only user, and that is deliberate.
-- Deleting it as unused removes the only thing that asks a new kind for its
-- spelling, silently reopening #2062. Nothing else will catch that: this
-- module has no export list (project-wide -Wno-missing-export-lists), so no
-- unused-binding warning ever fires here.
familyOf :: CounterKind.CounterKind keyword -> CounterKindFamily.CounterKindFamily
familyOf kind = case kind of
  CounterKind.PlusOnePlusOne -> CounterKindFamily.PlusOnePlusOne
  CounterKind.MinusOneMinusOne -> CounterKindFamily.MinusOneMinusOne
  CounterKind.Keyword _ -> CounterKindFamily.Keyword
  CounterKind.Loyalty -> CounterKindFamily.Loyalty
  CounterKind.Lore -> CounterKindFamily.Lore
  CounterKind.Defense -> CounterKindFamily.Defense
  CounterKind.Time -> CounterKindFamily.Time
  CounterKind.Fade -> CounterKindFamily.Fade
  CounterKind.Shield -> CounterKindFamily.Shield
  CounterKind.Finality -> CounterKindFamily.Finality
  CounterKind.Stun -> CounterKindFamily.Stun
  CounterKind.Level -> CounterKindFamily.Level
  CounterKind.Hone -> CounterKindFamily.Hone
  CounterKind.Named _ -> CounterKindFamily.Named
