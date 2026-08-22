module Pawl.Codec.CardSpec where

import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Card as Card
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TypeLine as TypeLine

-- Fixtures --------------------------------------------------------------------
--
-- Only the CONTAINER is exercised here -- how faces and the layout are written
-- and read back. Every case about what one face carries lives in
-- Pawl.Codec.FaceSpec.

-- Every field at the value Pawl.Codec.Face's decoder defaults it to, so a case
-- only writes the fields it is about.
bareFace :: CardName.CardName -> Face.Face Card.Card
bareFace n =
  Face.MkFace
    { Face.name = n,
      Face.manaCost = Nothing,
      Face.typeLine = TypeLine.MkTypeLine Set.empty Set.empty Set.empty,
      Face.power = Nothing,
      Face.toughness = Nothing,
      Face.loyalty = Nothing,
      Face.defense = Nothing,
      Face.keywords = Set.empty,
      Face.colorIndicator = Set.empty,
      Face.characteristicPT = Nothing,
      Face.staticAbilities = [],
      Face.spell = Face.defaultSpell,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.delayedAbilities = Map.empty,
      Face.rooms = Seq.empty,
      Face.castingPermissions = [],
      Face.castingRestrictions = [],
      Face.enchant = [],
      Face.counterability = Counterability.Counterable,
      Face.additionalCosts = [],
      Face.maximumX = Nothing,
      Face.alternativeCosts = [],
      Face.costReductions = [],
      Face.playerAbilities = [],
      Face.blockRequirements = [],
      Face.blockPermissions = [],
      Face.attackRequirements = [],
      Face.combatRestrictions = [],
      Face.sacrificeRestrictions = [],
      Face.untapRestrictions = [],
      Face.attachRestrictions = [],
      Face.entryRestrictions = [],
      Face.attackCosts = [],
      Face.blockCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = [],
      Face.specialActions = []
    }

-- | A basic land, which is what every file in the corpus but @wax-wane.json@
-- looks like: one face, and no "layout" key.
mountainCard :: Card.Card
mountainCard =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          (bareFace (CardName.MkCardName (Text.pack "Mountain")))
            { Face.typeLine =
                TypeLine.MkTypeLine
                  (Set.singleton Supertype.Basic)
                  (Set.singleton CardType.Land)
                  (Set.singleton Subtype.Mountain)
            }
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Card" $ do
  -- "faces" is the only required key: CR 709-722's Normal is the absence of a
  -- card saying otherwise, so every single-face file says nothing about layout.
  Spec.it s "a single-face card carries only faces, and no layout key" $
    Common.assertCodec
      s
      Card.codec
      mountainCard
      " {\"faces\":[{\"name\":\"Mountain\",\"typeLine\":{\"supertypes\":[{\"type\":\"Basic\"}],\"types\":[{\"type\":\"Land\"}],\"subtypes\":[{\"type\":\"Mountain\"}]}}]} "
  Spec.it s "a card with two faces round-trips, and layout is omitted when Normal" $ do
    let face n =
          (bareFace (CardName.MkCardName (Text.pack n)))
            { Face.typeLine = TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Instant) Set.empty
            }
        card =
          Card.MkCard
            { Card.layout = Layout.Normal,
              Card.faces = face "Wax" NonEmpty.:| [face "Wane"]
            }
    -- assertJsonCodec rather than a bare encode-then-decode round trip: only
    -- pinning the literal proves the "layout" key is ABSENT for Normal, which a
    -- round trip through the defaulting decoder could not see.
    Common.assertCodec
      s
      Card.codec
      card
      " {\"faces\":[{\"name\":\"Wax\",\"typeLine\":{\"types\":[{\"type\":\"Instant\"}]}},{\"name\":\"Wane\",\"typeLine\":{\"types\":[{\"type\":\"Instant\"}]}}]} "
  -- Omission is permitted on input, never required: a file that spells the
  -- default out must still load.
  Spec.it s "a card that spells its layout out still decodes" $
    Common.assertFromJson
      s
      (Codec.decode Card.codec)
      " {\"faces\":[{\"name\":\"Mountain\",\"typeLine\":{\"supertypes\":[{\"type\":\"Basic\"}],\"types\":[{\"type\":\"Land\"}],\"subtypes\":[{\"type\":\"Mountain\"}]}}],\"layout\":{\"type\":\"Normal\"}} "
      mountainCard
  -- Where the at-least-one-face invariant is enforced: Card.faces is a NonEmpty
  -- so that Pawl.Engine.Card.combined can be total, and Common.nonEmpty is
  -- the only gate a file passes through to get there.
  Spec.it s "a card with no faces is rejected rather than decoded" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"faces\":[]} ") >>= Codec.decode Card.codec))
      "expected an empty faces array to fail to decode"
