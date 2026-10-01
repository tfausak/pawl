{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PreventNextDamageInstance where

import qualified Data.Sequence as Seq
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PreventNextDamageInstance as PreventNextDamageInstance

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PreventAllDamage writes the unbounded shield's.
--
-- The effect codec is a PARAMETER rather than an import, for
-- Pawl.Codec.PreventNextDamage's reason.
--
-- @chosenSource@ is 'Fields.defaulted' to the trivial predicate, which is what
-- "a source of your choice" says on every printing of this rule; @ref@ and
-- @duration@ are required, both being fields the type does not make optional.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (PreventNextDamageInstance.PreventNextDamageInstance effect)
codec effectCodec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec PreventNextDamageInstance.duration
  ref <- Fields.required "ref" ObjectRef.codec PreventNextDamageInstance.ref
  chosenSource <- Fields.defaulted "chosenSource" (Filter.And []) (Filter.codec Keyword.codec) PreventNextDamageInstance.chosenSource
  riders <- Fields.defaulted "riders" Seq.empty (Common.seq effectCodec) PreventNextDamageInstance.riders
  pure
    PreventNextDamageInstance.MkPreventNextDamageInstance
      { PreventNextDamageInstance.duration = duration,
        PreventNextDamageInstance.ref = ref,
        PreventNextDamageInstance.chosenSource = chosenSource,
        PreventNextDamageInstance.riders = riders
      }
