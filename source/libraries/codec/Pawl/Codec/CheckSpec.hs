module Pawl.Codec.CheckSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Check as Check
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.Check as Check.Type
import qualified Pawl.Types.CountIs as CountIs.Type
import qualified Pawl.Types.DamageIs as DamageIs.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.LifeIs as LifeIs.Type
import qualified Pawl.Types.Reference as Reference.Type
import qualified Pawl.Types.TapState as TapState.Type
import qualified Pawl.Types.TappedIs as TappedIs.Type
import qualified Pawl.Types.Zone as Zone.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Check" $ do
  Spec.it s "Life" $
    Common.assertCodec s Check.codec (Check.Type.Life (LifeIs.Type.MkLifeIs (Label.Type.MkLabel (Text.pack "bob")) 18)) " {\"Life\":{\"player\":\"bob\",\"life\":18}} "
  Spec.it s "Count" $
    Common.assertCodec s Check.codec (Check.Type.Count (CountIs.Type.MkCountIs (Label.Type.MkLabel (Text.pack "bob")) Zone.Type.Battlefield (CardName.Type.MkCardName (Text.pack "Goblin Piker")) 0)) " {\"Count\":{\"player\":\"bob\",\"zone\":\"Battlefield\",\"card\":\"Goblin Piker\",\"count\":0}} "
  Spec.it s "Damage" $
    Common.assertCodec s Check.codec (Check.Type.Damage (DamageIs.Type.MkDamageIs (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "wall"))) 2)) " {\"Damage\":{\"object\":\"@wall\",\"damage\":2}} "
  Spec.it s "Tapped" $
    Common.assertCodec s Check.codec (Check.Type.Tapped (TappedIs.Type.MkTappedIs (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "bear"))) TapState.Type.Untapped)) " {\"Tapped\":{\"object\":\"@bear\",\"tapped\":false}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Check.codec
