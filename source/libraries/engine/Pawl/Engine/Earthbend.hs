-- | CR 701.66, "earthbend N": the whole of the keyword action, as the three
-- instructions rule 701.66a spells it out as.
--
-- Pawl.Engine.Blight's and Pawl.Engine.Amass's sibling, standing on the same
-- ground: rule 701 is a keyword-action rule exactly as rule 702 is a keyword
-- rule, so the procedure lives in the engine rather than in card data. The
-- closed\/open invariant forbids the rules core casing on an EFFECT's identity,
-- and nothing here does -- Pawl.Engine.Resolve.Effect's Effect.Earthbend arm
-- calls in without saying which effect it is.
--
-- What is DIFFERENT from those two is that rule 701.66a's three instructions are
-- already in the effect vocabulary: an animation (CR 611, layers 4, 6 and 7b), a
-- counter placement (CR 122.6) and a CR 603.7 delayed ability. So this module
-- writes them as Effects and lets the same executor run them, rather than
-- reaching past it into the board. The one thing it cannot write as card data is
-- the delayed ability itself, which is why that lives here and is filed on
-- Pawl.Engine.Keyword's minted roster.
--
-- CR 701.66b's "whenever a player earthbends" has no arm anywhere, and no
-- GameEvent to hang one on. Scryfall `o:"earthbends"`, 2026-09-06, returns no
-- card; a printing worded that way is what would need one.
module Pawl.Engine.Earthbend where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Earthbend as Earthbend
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.TapState as TapState
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- | CR 701.66a's first two sentences, in written order (CR 608.2c): the
-- animation, then the counters. Handed to Pawl.Engine.Resolve.Effect's executor
-- one at a time, so each runs exactly as the same instruction printed on a card
-- would.
--
-- THREE Modifications and so three continuous effects, where the rule prints one
-- sentence. CR 613 is what makes that indistinguishable: the three sit in layers
-- 4, 7b and 6, and a timestamp orders only effects competing within one layer,
-- so no third effect can be interleaved between them. Pawl.Types.Modification is
-- one modification per effect and a sum over them would be a new type for this
-- one sentence.
--
-- NO Land card type is added. Rule 701.66a says "0\/0 land creature", and the
-- object is already a land -- rule 701.66a's own "target land you control" is
-- what guarantees it, and CR 608.2b re-checks that at resolution. An
-- AddCardType Land would be a layer-4 no-op on every board that reaches here.
--
-- Duration.Indefinite and not UntilEndOfTurn: rule 701.66a states no duration,
-- so CR 611.2a leaves the effect running until the game ends -- which in
-- practice means until the land leaves the battlefield and CR 400.7 makes it a
-- new object the frozen CR 611.2c set no longer names.
instructions :: Earthbend.Earthbend -> [Effect Card (GrantedAbility.GrantedAbility Card)]
instructions earthbend =
  let ref = Earthbend.ref earthbend
      animate modification = Effect.ModifyTarget (ModifyTarget.MkModifyTarget Duration.Indefinite modification ref)
   in [ animate (Modification.AddCardType CardType.Creature),
        animate (Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness (Quantity.Literal 0) (Quantity.Literal 0))),
        animate (Modification.GainKeyword Keyword.Haste),
        Effect.PutCounters
          PutCounters.MkPutCounters
            { PutCounters.kind = CounterKind.PlusOnePlusOne,
              PutCounters.quantity = Earthbend.quantity earthbend,
              PutCounters.ref = ref
            }
      ]

-- | CR 701.66a's third sentence, as the opcode that creates it. Onset.Immediately
-- with no stated duration -- CR 603.7a's floor and CR 603.7b's default -- because
-- rule 701.66a gates neither end of the envelope: the ability watches from the
-- moment it is created and fires once.
arm :: Effect Card (GrantedAbility.GrantedAbility Card)
arm =
  Effect.ArmDelayedTrigger
    ArmDelayedTrigger.MkArmDelayedTrigger
      { ArmDelayedTrigger.name = returnName,
        ArmDelayedTrigger.onset = Onset.Immediately,
        ArmDelayedTrigger.duration = Nothing
      }

-- | The name rule 701.66a's delayed ability is filed under on
-- Pawl.Engine.Keyword.mintedDelayedAbilities. A card may not declare one under
-- this name (Pawl.CardSpec), which is what makes the fallback order in
-- Pawl.Engine.Resolve.Effect immaterial.
returnName :: AbilityName
returnName = AbilityName.MkAbilityName (Text.pack "earthbend")

-- | CR 701.66a: "when that land dies or is put into exile, return it to the
-- battlefield tapped under your control".
--
-- The condition names Pawl.Engine.Binding.earthbentLand, which the arming
-- resolution stamped onto the resolving object: CR 603.7c captures that whole
-- environment, so the entry still remembers which land it was about long after
-- the spell has gone.
--
-- The payload acts on Binding.became and NOT on the earthbent land, which is the
-- CR 400.7 incarnation split undying and persist meet one rule over: the id the
-- condition matched is the battlefield permanent, which no longer exists, while
-- `became` is the card in the graveyard or in exile that the return has to move.
--
-- EntryRiders.underOwner is False, which is CR 110.2a read literally: rule
-- 701.66a states otherwise -- "under your control" -- and "you" is the delayed
-- ability's controller (CR 603.7d), the player who earthbent.
returnAbility :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
returnAbility =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = returnCondition,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton effect))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }
  where
    effect =
      Effect.MoveToZone
        MoveToZone.MkMoveToZone
          { MoveToZone.ref = ObjectRef.InSlot Binding.became,
            MoveToZone.zone = Zone.Battlefield,
            MoveToZone.riders =
              EntryRiders.MkEntryRiders
                { EntryRiders.tapped = TapState.Tapped,
                  EntryRiders.attacking = False,
                  EntryRiders.blocking = Nothing,
                  EntryRiders.transformed = False,
                  EntryRiders.counters = Map.empty,
                  EntryRiders.underOwner = False,
                  EntryRiders.exiledFaceDown = False,
                  EntryRiders.faceDown = Nothing
                },
            MoveToZone.slot = Nothing,
            MoveToZone.origin = Nothing,
            MoveToZone.placement = LibraryPlacement.defaultValue,
            MoveToZone.duration = Nothing
          }

-- | The condition alone, so Pawl.Engine.Event.Binding's arms and the specs can
-- name it without building the whole ability.
returnCondition :: TriggerCondition
returnCondition = TriggerCondition.BoundDiesOrIsExiled Binding.earthbentLand
