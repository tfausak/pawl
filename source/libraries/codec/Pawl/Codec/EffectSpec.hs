module Pawl.Codec.EffectSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- | Every case below instantiates the `card` parameter at 'Text.Text', a
-- stand-in that is never a real card -- 'Effect.toJson'/'Effect.fromJson' reach
-- it only through the supplied codec (below), so any type proves the shape.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Effect.Effect Text.Text -> Value.Value
toJson = Effect.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Effect.Effect Text.Text)
fromJson = Effect.fromJson cardFromJson

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Effect" $ do
  Spec.it s "DealDamage" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 3))
      "{\"type\":\"DealDamage\",\"value\":[\"target\",{\"type\":\"Literal\",\"value\":3}]}"
  -- ModifyTarget's ObjectRef is untagged, so both arms have to survive: Giant
  -- Growth's chosen slot and Trumpet Blast's swept "attacking creatures".
  Spec.it s "ModifyTarget round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))))
      "{\"type\":\"ModifyTarget\",\"value\":[{\"type\":\"UntilEndOfTurn\"},{\"type\":\"GainKeyword\",\"value\":{\"type\":\"Trample\"}},\"t\"]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Trample) (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      "{\"type\":\"ModifyTarget\",\"value\":[{\"type\":\"UntilEndOfTurn\"},{\"type\":\"GainKeyword\",\"value\":{\"type\":\"Trample\"}},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}]}"
  Spec.it s "ChangeText" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ChangeText (SlotName.MkSlotName (Text.pack "target")))
      "{\"type\":\"ChangeText\",\"value\":\"target\"}"
  Spec.it s "AddMana, a fixed type and any color" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana (ManaProduction.OfType (ManaType.Colored Color.Green)))
      "{\"type\":\"AddMana\",\"value\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddMana ManaProduction.AnyColor)
      "{\"type\":\"AddMana\",\"value\":{\"type\":\"AnyColor\"}}"
  Spec.it s "Search" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Search (Filter.HasCardType CardType.Land) SearchDestination.BattlefieldTapped)
      "{\"type\":\"Search\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},{\"type\":\"BattlefieldTapped\"}]}"
  Spec.it s "ExileAllGraveyards" $
    Common.assertJsonCodec s toJson fromJson Effect.ExileAllGraveyards "{\"type\":\"ExileAllGraveyards\"}"
  Spec.it s "RestartGame" $
    Common.assertJsonCodec s toJson fromJson Effect.RestartGame "{\"type\":\"RestartGame\"}"
  Spec.it s "ControlPlayerNextTurn" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ControlPlayerNextTurn (SlotName.MkSlotName (Text.pack "target")))
      "{\"type\":\"ControlPlayerNextTurn\",\"value\":\"target\"}"
  -- Both ObjectRef arms Destroy's own comment gives (Murder's slot, Day of
  -- Judgment's swept set), plus the two shapes CR 701.19c's regeneration rider
  -- takes. The two-element literal here is what pins the elided (Nothing) arm of
  -- the bound-count slot below.
  Spec.it s "Destroy carries its CR 701.19c rider both ways" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.Regenerable Nothing)
      "{\"type\":\"Destroy\",\"value\":[\"t\",{\"type\":\"Regenerable\"}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "t"))) Regenerability.CantBeRegenerated Nothing)
      "{\"type\":\"Destroy\",\"value\":[\"t\",{\"type\":\"CantBeRegenerated\"}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing)
      "{\"type\":\"Destroy\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"Regenerable\"}]}"
  -- Bane of Progress' "destroyed this way": the third element is the slot the
  -- sweep binds its count into, and it is ELIDED when absent -- so every Destroy
  -- already on disk keeps its two-element payload (pinned by the Nothing case
  -- above). Moved from Pawl.CodecSpec (formerly built on `payloadLength`),
  -- rewritten against the literal the encoder actually produces.
  Spec.it s "Destroy's bound-count slot round-trips and is written only when present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Destroy (ObjectRef.EachMatching (Filter.HasCardType CardType.Artifact)) Regenerability.Regenerable (Just (SlotName.MkSlotName (Text.pack "destroyed"))))
      "{\"type\":\"Destroy\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}},{\"type\":\"Regenerable\"},\"destroyed\"]}"
  Spec.it s "Sacrifice" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self")))
      "{\"type\":\"Sacrifice\",\"value\":\"self\"}"
  Spec.it s "Attach" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Attach (SlotName.MkSlotName (Text.pack "target")))
      "{\"type\":\"Attach\",\"value\":\"target\"}"
  -- CR 701.3: the destination Filter travels in the payload, distinguishing this
  -- arm's wire format from Attach's bare slot above.
  Spec.it s "AttachTarget" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AttachTarget (SlotName.MkSlotName (Text.pack "target")) (Filter.HasCardType CardType.Creature))
      "{\"type\":\"AttachTarget\",\"value\":[\"target\",{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}]}"
  -- MoveToZone's payload takes Create's shape below: four emitted forms, told
  -- apart by LENGTH first and then, at three elements, by JSON TYPE (an Object is
  -- the EntryRiders, anything else is the bound slot). Moved from
  -- Pawl.CodecSpec, rewritten against the literals the encoder actually
  -- produces.
  Spec.it s "MoveToZone round-trips all four shapes, and elides the defaults" $ do
    let slot = SlotName.MkSlotName (Text.pack "target")
        bound = SlotName.MkSlotName (Text.pack "exiled")
        attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True}
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone slot Zone.Hand EntryRiders.defaultValue Nothing)
      "{\"type\":\"MoveToZone\",\"value\":[\"target\",{\"type\":\"Hand\"}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone slot Zone.Exile EntryRiders.defaultValue (Just bound))
      "{\"type\":\"MoveToZone\",\"value\":[\"target\",{\"type\":\"Exile\"},\"exiled\"]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone bound Zone.Battlefield attacking Nothing)
      "{\"type\":\"MoveToZone\",\"value\":[\"exiled\",{\"type\":\"Battlefield\"},{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.MoveToZone bound Zone.Battlefield attacking (Just bound))
      "{\"type\":\"MoveToZone\",\"value\":[\"exiled\",{\"type\":\"Battlefield\"},{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"exiled\"]}"
  -- Both of Draw's proven PlayerRef shapes: Divination's controller draw and
  -- Ancestral Recall's targeted one.
  Spec.it s "Draw" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 2))
      "{\"type\":\"Draw\",\"value\":[{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},{\"type\":\"Literal\",\"value\":2}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Draw (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 3))
      "{\"type\":\"Draw\",\"value\":[{\"type\":\"InSlot\",\"value\":\"target\"},{\"type\":\"Literal\",\"value\":3}]}"
  Spec.it s "Mill" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Mill (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 2))
      "{\"type\":\"Mill\",\"value\":[\"target\",{\"type\":\"Literal\",\"value\":2}]}"
  Spec.it s "Discard" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Discard (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 1))
      "{\"type\":\"Discard\",\"value\":[\"target\",{\"type\":\"Literal\",\"value\":1}]}"
  Spec.it s "LoseLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.LoseLife (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Quantity.Literal 2))
      "{\"type\":\"LoseLife\",\"value\":[{\"type\":\"InSlot\",\"value\":\"target\"},{\"type\":\"Literal\",\"value\":2}]}"
  Spec.it s "GainLife" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainLife (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))
      "{\"type\":\"GainLife\",\"value\":[{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},{\"type\":\"Literal\",\"value\":1}]}"
  -- Create's EntryRiders and bound slot are each ELIDED when they are the
  -- default, exactly like MoveToZone above: four emitted forms, the middle two
  -- told apart at decode by JSON TYPE. Moved from Pawl.CodecSpec, rewritten
  -- against the literals the encoder actually produces.
  Spec.it s "Create round-trips all four shapes, and elides the defaults" $ do
    let attacking = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True}
        plain = EntryRiders.defaultValue
        slot = SlotName.MkSlotName (Text.pack "token")
        card = Text.pack "Goblin Piker"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 2) card plain Nothing)
      "{\"type\":\"Create\",\"value\":[{\"type\":\"Literal\",\"value\":2},\"Goblin Piker\"]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 1) card plain (Just slot))
      "{\"type\":\"Create\",\"value\":[{\"type\":\"Literal\",\"value\":1},\"Goblin Piker\",\"token\"]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 2) card attacking Nothing)
      "{\"type\":\"Create\",\"value\":[{\"type\":\"Literal\",\"value\":2},\"Goblin Piker\",{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Create (Quantity.Literal 1) card attacking (Just slot))
      "{\"type\":\"Create\",\"value\":[{\"type\":\"Literal\",\"value\":1},\"Goblin Piker\",{\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true},\"token\"]}"
  Spec.it s "Replace" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Replace Duration.UntilEndOfTurn Uses.Once (ReplacementEffect.DestructionR DestructionRewrite.Regenerate))
      "{\"type\":\"Replace\",\"value\":[{\"type\":\"UntilEndOfTurn\"},{\"type\":\"Once\"},{\"type\":\"DestructionR\",\"value\":{\"type\":\"Regenerate\"}}]}"
  -- CR 614.10a: Fatigue's slot read, plus Stonehorn Dignitary's whole-phase
  -- selector -- the arm a Phase alone cannot spell (CR 500.1, "a turn consists
  -- of five phases").
  Spec.it s "SkipNextPhase" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep)))
      "{\"type\":\"SkipNextPhase\",\"value\":[{\"type\":\"InSlot\",\"value\":\"target\"},{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"DrawStep\"}}}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.SkipNextPhase (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) PhaseSelector.CombatPhase)
      "{\"type\":\"SkipNextPhase\",\"value\":[{\"type\":\"InSlot\",\"value\":\"target\"},{\"type\":\"CombatPhase\"}]}"
  Spec.it s "Counter" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Counter (SlotName.MkSlotName (Text.pack "spell")))
      "{\"type\":\"Counter\",\"value\":\"spell\"}"
  Spec.it s "PutCounters" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "creature")))
      "{\"type\":\"PutCounters\",\"value\":[{\"type\":\"PlusOnePlusOne\"},{\"type\":\"Literal\",\"value\":1},\"creature\"]}"
  -- Every PlayerRef shape the opcode accepts: the self-scoped one every card in
  -- the pool uses, and the slot read CR 702.70a's "that player" needs.
  Spec.it s "GainPlayerCounters" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerRef.Relative PlayerRelation.You) PlayerCounterKind.Energy (Quantity.Literal 2))
      "{\"type\":\"GainPlayerCounters\",\"value\":[{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},{\"type\":\"Energy\"},{\"type\":\"Literal\",\"value\":2}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainPlayerCounters (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) PlayerCounterKind.Poison (Quantity.Literal 3))
      "{\"type\":\"GainPlayerCounters\",\"value\":[{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},{\"type\":\"Poison\"},{\"type\":\"Literal\",\"value\":3}]}"
  -- CR 701.26a's Tap is Untap's mirror and shares its wire shape, so the two
  -- must not collapse into one tag.
  Spec.it s "Tap round-trips, and is not Untap" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      "{\"type\":\"Tap\",\"value\":\"target\"}"
    Spec.assertBool
      s
      ( toJson (Effect.Tap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          /= toJson (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      )
      "Tap and Untap of the same slot encode differently"
  Spec.it s "Untap round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Untap (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      "{\"type\":\"Untap\",\"value\":\"target\"}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.Untap (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature)))
      "{\"type\":\"Untap\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}"
  Spec.it s "RemoveFromCombat" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.RemoveFromCombat (SlotName.MkSlotName (Text.pack "target")))
      "{\"type\":\"RemoveFromCombat\",\"value\":\"target\"}"
  -- Both shapes in the pool: Aggravated Assault's pair and Full Throttle's two
  -- combat phases with no main phase between them.
  Spec.it s "AddPhases round-trips the pair and a repeated phase" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain])
      "{\"type\":\"AddPhases\",\"value\":[{\"type\":\"ExtraCombat\"},{\"type\":\"ExtraMain\"}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AddPhases [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat])
      "{\"type\":\"AddPhases\",\"value\":[{\"type\":\"ExtraCombat\"},{\"type\":\"ExtraCombat\"}]}"
  -- GainControl's own two arms: Act of Treason's slot and Aura Thief's "all
  -- enchantments".
  Spec.it s "GainControl round-trips both ObjectRef arms" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainControl Duration.UntilEndOfTurn (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      "{\"type\":\"GainControl\",\"value\":[{\"type\":\"UntilEndOfTurn\"},\"target\"]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.GainControl Duration.Indefinite (ObjectRef.EachMatching (Filter.HasCardType CardType.Enchantment)))
      "{\"type\":\"GainControl\",\"value\":[{\"type\":\"Indefinite\"},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Enchantment\"}}]}"
  -- The three shapes the encoder can emit, told apart by LENGTH: a bare ability
  -- name (CR 603.7a/b's defaults), a two-element form (a stated duration, onset
  -- still the default), and a three-element form (a stated onset, whose last
  -- element is the duration or null). Moved from Pawl.CodecSpec, rewritten
  -- against the literals the encoder actually produces.
  Spec.it s "ArmDelayedTrigger round-trips all three shapes, and elides the default onset" $ do
    let sacrificeIt = AbilityName.MkAbilityName (Text.pack "sacrifice it")
        eachCombat = AbilityName.MkAbilityName (Text.pack "each combat")
        returnIt = AbilityName.MkAbilityName (Text.pack "return it")
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger sacrificeIt Onset.Immediately Nothing)
      "{\"type\":\"ArmDelayedTrigger\",\"value\":\"sacrifice it\"}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger eachCombat Onset.Immediately (Just Duration.UntilEndOfTurn))
      "{\"type\":\"ArmDelayedTrigger\",\"value\":[\"each combat\",{\"type\":\"UntilEndOfTurn\"}]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger returnIt Onset.FromYourNextTurn Nothing)
      "{\"type\":\"ArmDelayedTrigger\",\"value\":[\"return it\",{\"type\":\"FromYourNextTurn\"},null]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ArmDelayedTrigger returnIt Onset.FromYourNextTurn (Just Duration.UntilEndOfTurn))
      "{\"type\":\"ArmDelayedTrigger\",\"value\":[\"return it\",{\"type\":\"FromYourNextTurn\"},{\"type\":\"UntilEndOfTurn\"}]}"
  Spec.it s "AffectPlayers" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.AffectPlayers Duration.UntilEndOfTurn PlayerScope.Opponents PlayerEffect.CantCastSpells)
      "{\"type\":\"AffectPlayers\",\"value\":[{\"type\":\"UntilEndOfTurn\"},{\"type\":\"Opponents\"},{\"type\":\"CantCastSpells\"}]}"
  -- The `card` payload comes only from the caller-supplied codec, exactly like
  -- Create's above -- proven at two different `card` values through the SAME
  -- constant codec, so a leak straight to the constructor (bypassing the codec
  -- argument) would fail this rather than merely coincide. Moved from
  -- Pawl.CodecSpec's "parametricity" group.
  Spec.it s "CreateEmblem reaches its card only through the supplied codec" $
    Spec.assertEqWith
      s
      "the emblem payload comes from the argument, not the card"
      (Effect.toJson (const sentinel) (Effect.CreateEmblem (Text.pack "a wholly different card type")))
      (Effect.toJson (const sentinel) (Effect.CreateEmblem (0 :: Int)))
  Spec.it s "BecomeMonarch" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.BecomeMonarch MonarchTarget.TheController)
      "{\"type\":\"BecomeMonarch\",\"value\":{\"type\":\"TheController\"}}"
  Spec.it s "ExileUntilMonarch" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.ExileUntilMonarch (SlotName.MkSlotName (Text.pack "target")))
      "{\"type\":\"ExileUntilMonarch\",\"value\":\"target\"}"
  Spec.it s "PlaySubgame" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser")))
      "{\"type\":\"PlaySubgame\",\"value\":\"loser\"}"
  Spec.it s "ExileHandThenDraw" $
    Common.assertJsonCodec s toJson fromJson Effect.ExileHandThenDraw "{\"type\":\"ExileHandThenDraw\"}"
  Spec.it s "Proliferate" $
    Common.assertJsonCodec s toJson fromJson Effect.Proliferate "{\"type\":\"Proliferate\"}"
  Spec.it s "PlayerSacrifices" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.PlayerSacrifices (SlotName.MkSlotName (Text.pack "t")) (Filter.HasCardType CardType.Creature) (Quantity.Literal 1))
      "{\"type\":\"PlayerSacrifices\",\"value\":[\"t\",{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"Literal\",\"value\":1}]}"
  -- CR 500.7: Time Warp's slot read, whose skip set is empty, plus Savor the
  -- Moment's self-scoped arm carrying CR 500.11's skip of one step of the turn
  -- it creates, and a two-member set (a Set, so the wire format has to survive
  -- more than one).
  Spec.it s "TakeExtraTurn" $ do
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Set.empty)
      "{\"type\":\"TakeExtraTurn\",\"value\":[{\"type\":\"InSlot\",\"value\":\"target\"},[]]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn (PlayerRef.Relative PlayerRelation.You) (Set.singleton (PhaseSelector.Step (Phase.Beginning BeginningStep.Untap))))
      "{\"type\":\"TakeExtraTurn\",\"value\":[{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},[{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Untap\"}}}]]}"
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Effect.TakeExtraTurn PlayerRef.EachPlayer (Set.fromList [PhaseSelector.Step (Phase.Beginning BeginningStep.Untap), PhaseSelector.CombatPhase]))
      "{\"type\":\"TakeExtraTurn\",\"value\":[{\"type\":\"EachPlayer\"},[{\"type\":\"Step\",\"value\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Untap\"}}},{\"type\":\"CombatPhase\"}]]}"

-- The stand-in a parametricity test hands over in place of a real card codec:
-- any Value at all, so long as both instantiations are given the same one.
sentinel :: Value.Value
sentinel = Common.text (Text.pack "SENTINEL")
