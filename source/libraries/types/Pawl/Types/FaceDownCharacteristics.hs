module Pawl.Types.FaceDownCharacteristics where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Ward as Ward

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
-- Gardener's "a Forest land"), Magar's two quoted abilities and, from a keyword
-- rather than a card, disguise's and cloak's ward {2}. Everything else CR 708.2a
-- leaves off the list -- name, mana cost, colour, supertypes -- is empty for
-- every listing there is, so it is Card.faceDownFace's constant rather than a
-- field here.
--
-- Parametric in the quoted-ability type for Pawl.Types.GrantedAbility's reason:
-- the ability types import Pawl.Types.Effect, which reaches this module through
-- EntryRiders, so Pawl.Types.Facing instantiates it where the cycle is closed.
data FaceDownCharacteristics ability = MkFaceDownCharacteristics
  { typeLine :: TypeLine.TypeLine,
    -- | Absent for a listing that names no creature -- Yedora's "Forest land"
    -- has no power to list, and CR 208.3 gives a noncreature permanent none.
    power :: Maybe Power.Power,
    toughness :: Maybe Toughness.Toughness,
    -- | CR 702.168b / 701.58a: the ward {2} disguise and cloak list. A Set where
    -- Pawl.Types.Face.keywords counts: a listing says WHICH keywords the object
    -- has, and no rule lists one twice. Pawl.Engine.Card.faceDownFace is where
    -- the two meet, at one instance each.
    keywords :: Set.Set Keyword.Keyword,
    -- | CR 708.2: whole quoted abilities the listing names, Magar of the Magic
    -- Strings' two.
    abilities :: [ability]
  }
  deriving (Eq, Ord, Show)

-- | CR 708.2a, which is the list for an ability or effect that names none: "it
-- becomes a 2\/2 face-down creature with no text, no name, no subtypes, and no
-- mana cost". Morph (CR 702.37c), manifest (CR 701.40a) and
-- Effect.TurnFaceDown's Backslide all list nothing and get this. An entry rider
-- that lists its own -- Yedora, Grave Gardener's "It's a Forest land" -- carries
-- a value of its own instead.
defaultValue :: FaceDownCharacteristics ability
defaultValue =
  MkFaceDownCharacteristics
    { typeLine =
        TypeLine.MkTypeLine
          { TypeLine.supertypes = Set.empty,
            TypeLine.types = Set.singleton CardType.Creature,
            TypeLine.subtypes = Set.empty
          },
      power = Just (Power.MkPower (Quantity.Literal 2)),
      toughness = Just (Toughness.MkToughness (Quantity.Literal 2)),
      keywords = Set.empty,
      abilities = []
    }

-- | CR 702.168b and CR 701.58a, which list the same thing in the same words: "a
-- 2\/2 face-down creature card with ward {2}, no name, no subtypes, and no mana
-- cost". CR 708.2a's list plus the one keyword, which is the whole of what
-- disguise adds to morph.
--
-- ONE value for two rules, because the rules are one sentence apart: rule
-- 701.58a is rule 702.168b's listing put onto the battlefield instead of onto
-- the stack, and CR 701.58d has a single permanent under both at once. What
-- separates them is the FaceDownReason beside this on Pawl.Types.Facing and the
-- procedure that reads it, never the list.
--
-- Ward {2} and not a family designator: CR 702.168b lists a WRITTEN instance,
-- which is what Pawl.Types.Keyword carries and what
-- Pawl.Engine.Keyword.abilitiesFor needs to mint CR 702.21a's trigger with a
-- cost on it.
disguisedValue :: FaceDownCharacteristics ability
disguisedValue =
  defaultValue
    { keywords =
        Set.singleton
          ( Keyword.Ward
              Ward.MkWard
                { Ward.cost =
                    Cost.MkCost
                      { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
                        Cost.components = []
                      },
                  Ward.perEach = Nothing
                }
          )
    }
