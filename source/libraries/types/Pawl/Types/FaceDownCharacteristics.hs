module Pawl.Types.FaceDownCharacteristics where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TypeLine as TypeLine

-- | CR 708.2: "face-down spells and face-down permanents have no
-- characteristics other than those LISTED by the ability or rules that allowed
-- the spell or permanent to be face down". This is that list -- the whole of
-- what a face-down object is, carried on Facing's FaceDown arm so
-- Pawl.Engine.Game.faceOf has something to read it from.
--
-- COPIABLE, not a layer. CR 708.2's second sentence -- "any listed
-- characteristics are the copiable values of that object's characteristics" --
-- is why this replaces the printed face at Pawl.Engine.Card.faceDownFace rather
-- than being applied as a CR 613 layer over it. CR 708.10 is the same fact from
-- the other side: a face-down permanent that becomes a copy of something still
-- has these characteristics.
--
-- ONLY the fields a printing lists. The pool lists a type line and a power and
-- toughness (Cyber Conversion's and Missy's "a 2\/2 Cyberman artifact creature",
-- Magar of the Magic Strings' "a 3\/3 creature with ...", Yedora, Grave
-- Gardener's "a Forest land") and, from a keyword rather than a card,
-- disguise's and cloak's ward {2}. Everything CR 708.2a leaves off the list --
-- name, mana cost, colour, supertypes, text -- is empty for every listing there
-- is, so it is Card.faceDownFace's constant rather than a field here.
--
-- Two listings pawl cannot yet carry: the ABILITIES Magar lists (#1667), and
-- the KEYWORD disguise and cloak list (#922). Both are more fields on this
-- record when a card asks.
data FaceDownCharacteristics = MkFaceDownCharacteristics
  { typeLine :: TypeLine.TypeLine,
    -- | Absent for a listing that names no creature -- Yedora's "Forest land"
    -- has no power to list, and CR 208.1 gives a noncreature permanent none.
    power :: Maybe Power.Power,
    toughness :: Maybe Toughness.Toughness
  }
  deriving (Eq, Ord, Show)

-- | CR 708.2a, which is the list for an ability or effect that names none: "it
-- becomes a 2\/2 face-down creature with no text, no name, no subtypes, and no
-- mana cost". Morph (CR 702.37c), the EntryRiders.faceDown rider and
-- Effect.TurnFaceDown's Backslide all list nothing and get this.
defaultValue :: FaceDownCharacteristics
defaultValue =
  MkFaceDownCharacteristics
    { typeLine =
        TypeLine.MkTypeLine
          { TypeLine.supertypes = Set.empty,
            TypeLine.types = Set.singleton CardType.Creature,
            TypeLine.subtypes = Set.empty
          },
      power = Just (Power.MkPower (Quantity.Literal 2)),
      toughness = Just (Toughness.MkToughness (Quantity.Literal 2))
    }
