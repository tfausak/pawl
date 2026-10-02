module Pawl.Codec.GrantedAbilitySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostDirection as CostDirection
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RuleAbilities as RuleAbilities
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | The `card` parameter is instantiated at 'Text.Text', reached only through the
-- Modal codec, the posture 'Pawl.Codec.ActivatedAbilitySpec' takes.
codec :: Codec.Codec (GrantedAbility.GrantedAbility Text.Text)
codec = GrantedAbility.codec Common.text

emptyModal :: Modal.Modal Text.Text (GrantedAbility.GrantedAbility Text.Text)
emptyModal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)

-- Every arm, since the whole point of the type is that CR 613.1f's grant reaches
-- several of CR 113.3's ability kinds and the wire has to say which.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GrantedAbility" $ do
  -- Presence of Gond's "{T}: ..." reduced to its cost.
  Spec.it s "Activated" $
    Common.assertCodec
      s
      codec
      ( GrantedAbility.Activated
          ( ActivatedAbility.MkActivatedAbility
              (Cost.MkCost Nothing [CostComponent.TapThis])
              []
              0
              emptyModal
              []
              Activator.Controller
              Nothing
              Nothing
              Nothing
          )
      )
      " {\"type\":\"Activated\",\"value\":{\"cost\":{\"mana\":null,\"components\":[{\"type\":\"TapThis\"}]},\"modal\":{\"modes\":[{}]}}} "
  -- Sixth Sense's "Whenever this creature deals combat damage to a player, ..."
  -- reduced to its condition.
  Spec.it s "Triggered" $
    Common.assertCodec
      s
      codec
      ( GrantedAbility.Triggered
          ( TriggeredAbility.MkTriggeredAbility
              (TriggerCondition.SelfDealsCombatDamageToPlayer PlayerRelation.AnyPlayer)
              emptyModal
              Nothing
              TriggerLimit.Unlimited
          )
      )
      " {\"type\":\"Triggered\",\"value\":{\"condition\":{\"type\":\"SelfDealsCombatDamageToPlayer\",\"value\":{\"type\":\"AnyPlayer\"}},\"modal\":{\"modes\":[{}]}}} "
  -- Streetwise Negotiator's "This creature assigns combat damage equal to its
  -- toughness rather than its power", as backup hands it over.
  Spec.it s "Static" $
    Common.assertCodec
      s
      codec
      ( GrantedAbility.Static
          ( StaticAbility.MkStaticAbility
              (Affected.Matching Filter.IsSource)
              Nothing
              Set.empty
              Nothing
              (NonEmpty.singleton Modification.AssignCombatDamageWithToughness)
          )
      )
      " {\"type\":\"Static\",\"value\":{\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"IsSource\"}},\"modifications\":[{\"type\":\"AssignCombatDamageWithToughness\"}]}} "
  -- Chomping Kavu's "This creature can't be blocked by creatures with power 2
  -- or less", as backup hands it over.
  Spec.it s "Rules" $
    Common.assertCodec
      s
      codec
      ( GrantedAbility.Rules
          mempty
            { RuleAbilities.combatRestrictions =
                [CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy (Affected.Matching Filter.IsSource) (Filter.PowerAtMost 2) Nothing Nothing)]
            }
      )
      " {\"type\":\"Rules\",\"value\":{\"combatRestrictions\":[{\"type\":\"CantBeBlockedBy\",\"value\":{\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"IsSource\"}},\"blockers\":{\"type\":\"PowerAtMost\",\"value\":2}}}]}} "
  -- CR 613.10: Nerd Rage's "You have no maximum hand size".
  Spec.it s "Player" $
    Common.assertCodec
      s
      codec
      (GrantedAbility.Player (PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You Nothing Nothing PlayerEffect.NoMaximumHandSize))
      " {\"type\":\"Player\",\"value\":{\"scope\":{\"type\":\"You\"},\"effect\":{\"type\":\"NoMaximumHandSize\"}}} "
  -- CR 601.2f: Richlau, Headmaster's "This spell costs {1} less to cast".
  Spec.it s "SelfCostReduction" $
    Common.assertCodec
      s
      codec
      (GrantedAbility.SelfCostReduction (CostReduction.MkCostReduction (ManaCost.MkManaCost [ManaSymbol.Generic 1]) (Quantity.Literal 1) Nothing Nothing CostDirection.Less))
      " {\"type\":\"SelfCostReduction\",\"value\":{\"amount\":[{\"type\":\"Generic\",\"value\":1}],\"perEach\":{\"type\":\"Literal\",\"value\":1}}} "
  -- CR 118.9: Mine Security's "You may pay {0} rather than pay this spell's
  -- mana cost".
  Spec.it s "SelfAlternativeCost" $
    Common.assertCodec
      s
      codec
      (GrantedAbility.SelfAlternativeCost (AlternativeCost.MkAlternativeCost Nothing (Cost.MkCost (Just (ManaCost.MkManaCost [])) [])))
      " {\"type\":\"SelfAlternativeCost\",\"value\":{\"cost\":{\"mana\":[]}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
