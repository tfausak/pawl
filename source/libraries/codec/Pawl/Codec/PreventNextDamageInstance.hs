{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PreventNextDamageInstance where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PreventNextDamageInstance as PreventNextDamageInstance

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PreventAllDamage writes the unbounded shield's.
--
-- Takes no effect codec, where both sibling shields do: this record carries no
-- CR 615.5 rider, so it is not parametric in the effect (see
-- Pawl.Types.PreventNextDamageInstance).
--
-- @chosenSource@ is 'Fields.defaulted' to the trivial predicate, which is what
-- "a source of your choice" says on every printing of this rule; @ref@ and
-- @duration@ are required, both being fields the type does not make optional.
codec :: Codec.Codec PreventNextDamageInstance.PreventNextDamageInstance
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec PreventNextDamageInstance.duration
  ref <- Fields.required "ref" ObjectRef.codec PreventNextDamageInstance.ref
  chosenSource <- Fields.defaulted "chosenSource" (Filter.And []) (Filter.codec Keyword.codec) PreventNextDamageInstance.chosenSource
  pure
    PreventNextDamageInstance.MkPreventNextDamageInstance
      { PreventNextDamageInstance.duration = duration,
        PreventNextDamageInstance.ref = ref,
        PreventNextDamageInstance.chosenSource = chosenSource
      }
