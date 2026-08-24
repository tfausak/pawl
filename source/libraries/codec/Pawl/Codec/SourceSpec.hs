module Pawl.Codec.SourceSpec where

import qualified Pawl.Codec.ActivatedAbilitySourceSpec as ActivatedAbilitySourceSpec
import qualified Pawl.Codec.Source as Source
import qualified Pawl.Codec.TriggeredAbilitySourceSpec as TriggeredAbilitySourceSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource

-- | Every constructor, and each printing id DISTINCT: the card-shaped
-- arms carry the same payload type, so equal ids would round trip whichever tag
-- the encoder wrote.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Source" $ do
  -- CR 108.
  Spec.it s "OfCard" $
    Common.assertCodec
      s
      Source.codec
      (Source.OfCard (PrintingId.MkPrintingId 2))
      " {\"type\":\"OfCard\",\"value\":2} "
  -- CR 111.3/111.6: a token's characteristics are interned like a card's, so
  -- the payload is the same id and only the tag says it is not a card.
  Spec.it s "OfToken" $
    Common.assertCodec
      s
      Source.codec
      (Source.OfToken (PrintingId.MkPrintingId 3))
      " {\"type\":\"OfToken\",\"value\":3} "
  -- CR 602.
  Spec.it s "OfAbility" $
    Common.assertCodec
      s
      Source.codec
      ( Source.OfAbility
          ActivatedAbilitySource.MkActivatedAbilitySource
            { ActivatedAbilitySource.source = ObjectId.MkObjectId 5,
              ActivatedAbilitySource.ability = ActivatedAbilitySourceSpec.ability
            }
      )
      " {\"type\":\"OfAbility\",\"value\":{\"source\":5,\"ability\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"modal\":{\"modes\":[{}]}}}} "
  -- CR 603.3.
  Spec.it s "OfTrigger" $
    Common.assertCodec
      s
      Source.codec
      ( Source.OfTrigger
          TriggeredAbilitySource.MkTriggeredAbilitySource
            { TriggeredAbilitySource.source = ObjectId.MkObjectId 6,
              TriggeredAbilitySource.ability = TriggeredAbilitySourceSpec.ability,
              TriggeredAbilitySource.createdAt = Nothing
            }
      )
      " {\"type\":\"OfTrigger\",\"value\":{\"source\":6,\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}}}} "
  -- CR 114.
  Spec.it s "OfEmblem" $
    Common.assertCodec
      s
      Source.codec
      (Source.OfEmblem (PrintingId.MkPrintingId 4))
      " {\"type\":\"OfEmblem\",\"value\":4} "
  -- CR 707.10 / 112.1a: the copied spell's printing, which is again the same
  -- payload as OfCard's and again told apart only by the tag.
  Spec.it s "OfSpellCopy" $
    Common.assertCodec
      s
      Source.codec
      (Source.OfSpellCopy (PrintingId.MkPrintingId 7))
      " {\"type\":\"OfSpellCopy\",\"value\":7} "
  -- CR 725.2 / CR 702.179d: the same ability as OfTrigger above, under a
  -- controller instead of a source id. The two arms differ only there, so an
  -- arm writing the other's tag would still have to write the other's key.
  Spec.it s "OfInherentTrigger" $
    Common.assertCodec
      s
      Source.codec
      ( Source.OfInherentTrigger
          InherentTriggerSource.MkInherentTriggerSource
            { InherentTriggerSource.controller = PlayerId.MkPlayerId 1,
              InherentTriggerSource.ability = TriggeredAbilitySourceSpec.ability
            }
      )
      " {\"type\":\"OfInherentTrigger\",\"value\":{\"controller\":1,\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Source.codec
