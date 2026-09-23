module Pawl.Codec.PlayerAttacksWithSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerAttacksWith" $ do
  -- Hermes, Overseer of Elpis' payload: whose declaration fires it, the quality
  -- the declared creatures have to have, and how many of them the declaration
  -- has to name.
  Spec.it s "MkPlayerAttacksWith, all three keys" $
    Common.assertCodec
      s
      PlayerAttacksWith.codec
      ( PlayerAttacksWith.MkPlayerAttacksWith
          { PlayerAttacksWith.player = PlayerRelation.You,
            PlayerAttacksWith.filter = Filter.HasSubtype Subtype.Bird,
            PlayerAttacksWith.attackers = 1
          }
      )
      " {\"player\":{\"type\":\"You\"},\"filter\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Bird\"}},\"attackers\":1} "
  -- Military Intelligence's floor of two, which is what parts this key from a
  -- defaulted one: a payload that omitted it would read as Hermes' one.
  Spec.it s "MkPlayerAttacksWith, a floor above one" $
    Common.assertCodec
      s
      PlayerAttacksWith.codec
      ( PlayerAttacksWith.MkPlayerAttacksWith
          { PlayerAttacksWith.player = PlayerRelation.You,
            PlayerAttacksWith.filter = Filter.HasCardType CardType.Creature,
            PlayerAttacksWith.attackers = 2
          }
      )
      " {\"player\":{\"type\":\"You\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"attackers\":2} "
  -- A zero floor fires on any declaration whatever the filter, since
  -- AttackersDeclared only fires non-empty, and no printing means that.
  Spec.it s "rejects a zero floor" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode PlayerAttacksWith.codec =<< Common.parse (Text.pack "{\"player\":{\"type\":\"You\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"attackers\":0}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerAttacksWith.codec
