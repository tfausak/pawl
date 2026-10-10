{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryRiders where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.EntryAttack as EntryAttack
import qualified Pawl.Codec.EntryBlock as EntryBlock
import qualified Pawl.Codec.FaceDownState as FaceDownState
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterKind as CounterKind.Type
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Quantity as Quantity.Type

-- | One counter kind and the count an object enters with, which is a Quantity
-- rather than a number (CR 122.6, CR 107.3c): Printlifter Ooze's "X +1/+1
-- counters on it, where X is the number of other creatures you control".
--
-- A PAIR PER KIND, where this used to be 'Common.multiset' -- a plain array with
-- repeats, which can spell a literal count and nothing else.
counter :: Codec.Codec (CounterKind.Type.CounterKind Keyword.Type.Keyword, Quantity.Type.Quantity)
counter = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) fst
  count <- Fields.required "count" Quantity.codec snd
  pure (kind, count)

-- | Every field is defaulted, so riders equal to 'defaultValue' write the empty
-- object -- and their own key is then elided by whichever effect carries them.
codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (EntryRiders.EntryRiders Quantity.Type.Quantity ability)
codec abilityCodec = Fields.object $ do
  tapped <- Fields.defaulted "tapped" (EntryRiders.tapped EntryRiders.defaultValue) TapState.codec EntryRiders.tapped
  attacking <- Fields.defaulted "attacking" (EntryRiders.attacking EntryRiders.defaultValue) (Common.maybe EntryAttack.codec) EntryRiders.attacking
  blocking <- Fields.defaulted "blocking" (EntryRiders.blocking EntryRiders.defaultValue) (Common.maybe EntryBlock.codec) EntryRiders.blocking
  transformed <- Fields.defaulted "transformed" (EntryRiders.transformed EntryRiders.defaultValue) Common.boolean EntryRiders.transformed
  counters <- Fields.defaulted "counters" (EntryRiders.counters EntryRiders.defaultValue) (Common.keyedList counter) EntryRiders.counters
  underOwner <- Fields.defaulted "underOwner" (EntryRiders.underOwner EntryRiders.defaultValue) Common.boolean EntryRiders.underOwner
  exiledFaceDown <- Fields.defaulted "exiledFaceDown" (EntryRiders.exiledFaceDown EntryRiders.defaultValue) Common.boolean EntryRiders.exiledFaceDown
  attachedTo <- Fields.defaulted "attachedTo" (EntryRiders.attachedTo EntryRiders.defaultValue) (Common.maybe SlotName.codec) EntryRiders.attachedTo
  faceDown <- Fields.defaulted "faceDown" (EntryRiders.faceDown EntryRiders.defaultValue) (Common.maybe (FaceDownState.codec abilityCodec)) EntryRiders.faceDown
  noted <- Fields.defaulted "noted" (EntryRiders.noted EntryRiders.defaultValue) Common.boolean EntryRiders.noted
  characteristics <- Fields.defaulted "characteristics" (EntryRiders.characteristics EntryRiders.defaultValue) (Common.seq (Modification.codec abilityCodec)) EntryRiders.characteristics
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = tapped,
        EntryRiders.attacking = attacking,
        EntryRiders.blocking = blocking,
        EntryRiders.transformed = transformed,
        EntryRiders.counters = counters,
        EntryRiders.underOwner = underOwner,
        EntryRiders.exiledFaceDown = exiledFaceDown,
        EntryRiders.attachedTo = attachedTo,
        EntryRiders.faceDown = faceDown,
        EntryRiders.noted = noted,
        EntryRiders.characteristics = characteristics
      }
