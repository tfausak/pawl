{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.OfferCast where

import qualified Pawl.Codec.CastObligation as CastObligation
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.CastRepetition as CastRepetition
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PermissionVerb as PermissionVerb
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CastObligation as CastObligation.Type
import qualified Pawl.Types.CastRepetition as CastRepetition.Type
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.PermissionVerb as PermissionVerb.Type
import qualified Pawl.Types.PlayerRef as PlayerRef.Type
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, with the riders elided when
-- they are the default -- MoveToZone's posture with its EntryRiders.
--
-- Every key but `ref` defaults, and every default is the narrow reading: the
-- resolving controller casts, they may decline, one card is cast, and the card
-- itself is what goes on the stack. A defaulted Mandatory would silently turn
-- every existing offer into an instruction, a defaulted AnyNumber would turn
-- every one of them into a repeated offer, and a defaulted True on `copied`
-- would make CR 707.12's copy of every one of them. Likewise `verb` defaults to
-- Cast, and `controlWhileResolving` to no control.
codec :: Codec.Codec OfferCast.OfferCast
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec OfferCast.ref
  caster <- Fields.defaulted "caster" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec OfferCast.caster
  optionality <- Fields.defaulted "optionality" CastObligation.Type.Optional CastObligation.codec OfferCast.optionality
  verb <- Fields.defaulted "verb" PermissionVerb.Type.Cast PermissionVerb.codec OfferCast.verb
  offer <- Fields.defaulted "offer" CastOffer.defaultValue CastOffer.codec OfferCast.offer
  repetition <- Fields.defaulted "repetition" CastRepetition.Type.Once CastRepetition.codec OfferCast.repetition
  copied <- Fields.defaulted "copied" False Common.boolean OfferCast.copied
  controlWhileResolving <- Fields.defaulted "controlWhileResolving" False Common.boolean OfferCast.controlWhileResolving
  pure
    OfferCast.MkOfferCast
      { OfferCast.ref = ref,
        OfferCast.caster = caster,
        OfferCast.optionality = optionality,
        OfferCast.verb = verb,
        OfferCast.offer = offer,
        OfferCast.repetition = repetition,
        OfferCast.copied = copied,
        OfferCast.controlWhileResolving = controlWhileResolving
      }
