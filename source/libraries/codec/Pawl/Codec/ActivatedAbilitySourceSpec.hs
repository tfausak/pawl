module Pawl.Codec.ActivatedAbilitySourceSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Codec.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.ObjectId as ObjectId

-- | What the ability itself encodes to is Pawl.Codec.ActivatedAbility's own
-- case; one mana cost and no modes is enough to show the two keys this record
-- adds around it.
ability :: ActivatedAbility.ActivatedAbility Card.Card
ability =
  ActivatedAbility.MkActivatedAbility
    (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])
    (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1))
    []
    Nothing
    Nothing

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivatedAbilitySource" $ do
  -- CR 602: the source permanent's id travels beside the ability, so the two
  -- are separate keys rather than the ability alone.
  Spec.it s "an activated ability and the id of what it came from" $
    Common.assertCodec
      s
      ActivatedAbilitySource.codec
      ActivatedAbilitySource.MkActivatedAbilitySource
        { ActivatedAbilitySource.source = ObjectId.MkObjectId 5,
          ActivatedAbilitySource.ability = ability
        }
      " {\"source\":5,\"ability\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"modal\":{\"modes\":[{}]}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActivatedAbilitySource.codec
