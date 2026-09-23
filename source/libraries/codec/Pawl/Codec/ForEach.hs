{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForEach where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.LoopMembers as LoopMembers
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.LoopMembers as LoopMembers

-- | A bare object keyed by the record's field names.
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.ForEach gives: the record is parametric in the effect so that
-- neither module has to name the other.
--
-- The three structural fields are REQUIRED, unlike Pawl.Codec.PreventNextDamage's
-- rider: a loop with no body and a loop with no name for its member are both an
-- author's mistake rather than a shorter way of saying something. `individually`
-- is not one of them: CR 608.2f says simultaneous processing is what happens "in
-- most cases", so its False is the rule's own default and a card states only the
-- exception. `members` defaults to LoopMembers.Every for the same reason: "for
-- each" is every member unless the card says "you may".
codec ::
  (Typeable.Typeable effect) =>
  Codec.Codec effect ->
  Codec.Codec (ForEach.ForEach effect)
codec effectCodec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec ForEach.ref
  members <- Fields.defaulted "members" LoopMembers.Every LoopMembers.codec ForEach.members
  slot <- Fields.required "slot" SlotName.codec ForEach.slot
  body <- Fields.required "body" (Common.seq effectCodec) ForEach.body
  individually <- Fields.defaulted "individually" False Common.boolean ForEach.individually
  pure
    ForEach.MkForEach
      { ForEach.ref = ref,
        ForEach.members = members,
        ForEach.slot = slot,
        ForEach.body = body,
        ForEach.individually = individually
      }
