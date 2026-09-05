module Pawl.Codec.TokenRSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.TokenR as TokenR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TypeLine as TypeLine

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TokenR" $ do
  -- CR 614.16: Doubling Season doubles the tokens you create.
  Spec.it s "MkTokenR" $
    Common.assertCodec
      s
      codec
      ( TokenR.MkTokenR
          { TokenR.matching = TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours, TokenPattern.whatToken = Filter.And []},
            TokenR.scaling = Just (Scaling.Multiply 2),
            TokenR.plus = Nothing
          }
      )
      " {\"matching\":{\"whose\":{\"type\":\"Yours\"}},\"scaling\":{\"type\":\"Multiply\",\"value\":2}} "
  -- CR 614.1a: Queen Allenal of Ruadach appends a token and scales nothing.
  Spec.it s "an append with no scaling" $
    Common.assertCodec
      s
      codec
      ( TokenR.MkTokenR
          { TokenR.matching = TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours, TokenPattern.whatToken = Filter.HasCardType CardType.Creature},
            TokenR.scaling = Nothing,
            TokenR.plus = Just soldier
          }
      )
      " {\"matching\":{\"whose\":{\"type\":\"Yours\"},\"whatToken\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"plus\":{\"faces\":[{\"name\":\"Soldier Token\",\"typeLine\":{\"types\":[{\"type\":\"Creature\"}]}}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
  where
    codec = TokenR.codec Card.codec

-- A one-face creature token with every other field at the value
-- Pawl.Codec.Face's decoder defaults it to; Pawl.Codec.CardSpec's bareFace.
soldier :: Card.Card
soldier =
  Card.MkCard
    { Card.layout = Layout.Normal,
      Card.faces =
        NonEmpty.singleton
          Face.MkFace
            { Face.name = CardName.MkCardName (Text.pack "Soldier Token"),
              Face.manaCost = Nothing,
              Face.typeLine = TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) Set.empty,
              Face.power = Nothing,
              Face.toughness = Nothing,
              Face.loyalty = Nothing,
              Face.defense = Nothing,
              Face.vanguard = Nothing,
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
              Face.dungeonEntryQuality = Nothing,
              Face.castingPermissions = [],
              Face.castingRestrictions = [],
              Face.enchant = [],
              Face.counterability = Counterability.Counterable,
              Face.additionalCosts = [],
              Face.maximumX = [],
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
              Face.counterRestrictions = [],
              Face.activationProhibitions = [],
              Face.entryRestrictions = [],
              Face.attackCosts = [],
              Face.blockCosts = [],
              Face.mulliganActions = [],
              Face.openingHandActions = [],
              Face.specialActions = []
            }
    }
