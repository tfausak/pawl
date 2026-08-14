module Pawl.Codec.Filter where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.KeywordFamily as KeywordFamily
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Filter as Filter

-- | Recursive, mirroring Quantity's toJson/fromJson: And/Or carry their
-- operands as a JSON Array, Not as a single nested object, and each atom
-- delegates to the leaf-enum codec for the characteristic it cases on.
--
-- The keyword codec is a PARAMETER: Pawl.Codec.Keyword imports this module for
-- CR 702.29e's typecycling filter, so a direct reference to Pawl.Codec.Keyword
-- here would close a module cycle. Every caller at 'Filter Keyword.Keyword'
-- passes 'Pawl.Codec.Keyword.codec' -- which is itself defined partly as
-- 'codec Keyword.codec', tying the knot at the value level; 'Codec.schema'
-- breaks it at the schema level (Pawl.JsonSchema.Define.define registers the
-- name before running the body). And/Or/Not recurse on 'codec keywordCodec'
-- itself for the same reason. Pawl.Codec.KeywordFamily is called DIRECTLY
-- below and needs no parameter, mirroring the types it encodes: a family
-- carries no filter, so that module imports neither this one nor
-- Pawl.Codec.Keyword.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Filter.Filter keyword)
codec keywordCodec =
  Arm.tagged
    [ Arm.payload "HasCardType" CardType.codec Filter.HasCardType (\x -> case x of Filter.HasCardType y -> Just y; _ -> Nothing),
      Arm.payload "HasSupertype" Supertype.codec Filter.HasSupertype (\x -> case x of Filter.HasSupertype y -> Just y; _ -> Nothing),
      Arm.payload "HasColor" Color.codec Filter.HasColor (\x -> case x of Filter.HasColor y -> Just y; _ -> Nothing),
      Arm.payload "HasSubtype" Subtype.codec Filter.HasSubtype (\x -> case x of Filter.HasSubtype y -> Just y; _ -> Nothing),
      Arm.payload "HasKeyword" keywordCodec Filter.HasKeyword (\x -> case x of Filter.HasKeyword y -> Just y; _ -> Nothing),
      Arm.payload "HasKeywordFamily" KeywordFamily.codec Filter.HasKeywordFamily (\x -> case x of Filter.HasKeywordFamily y -> Just y; _ -> Nothing),
      Arm.payload "PowerAtLeast" Common.integer Filter.PowerAtLeast (\x -> case x of Filter.PowerAtLeast y -> Just y; _ -> Nothing),
      Arm.payload "PowerAtMost" Common.integer Filter.PowerAtMost (\x -> case x of Filter.PowerAtMost y -> Just y; _ -> Nothing),
      Arm.nullary "PowerLessThanSource" Filter.PowerLessThanSource,
      Arm.nullary "PowerGreaterThanSource" Filter.PowerGreaterThanSource,
      Arm.nullary "ControlledByDefendingPlayer" Filter.ControlledByDefendingPlayer,
      Arm.payload "ControlledByBound" SlotName.codec Filter.ControlledByBound (\x -> case x of Filter.ControlledByBound y -> Just y; _ -> Nothing),
      -- Runtime-only, and accepted here anyway: the codec must stay total, so a
      -- corpus lint keeps the pool honest instead (#199) -- the treatment
      -- Modification.SetController's baked PlayerId gets.
      Arm.payload "ControlledByPlayer" PlayerId.codec Filter.ControlledByPlayer (\x -> case x of Filter.ControlledByPlayer y -> Just y; _ -> Nothing),
      Arm.nullary "ControlledByRecipient" Filter.ControlledByRecipient,
      Arm.payload "ManaValueAtMost" Common.integer Filter.ManaValueAtMost (\x -> case x of Filter.ManaValueAtMost y -> Just y; _ -> Nothing),
      Arm.nullary "ManaValueIsEven" Filter.ManaValueIsEven,
      Arm.payload "ControlledBy" PlayerRelation.codec Filter.ControlledBy (\x -> case x of Filter.ControlledBy y -> Just y; _ -> Nothing),
      Arm.payload "OwnedBy" PlayerRelation.codec Filter.OwnedBy (\x -> case x of Filter.OwnedBy y -> Just y; _ -> Nothing),
      Arm.payload "IsPlayer" PlayerRelation.codec Filter.IsPlayer (\x -> case x of Filter.IsPlayer y -> Just y; _ -> Nothing),
      Arm.payload "IsControllerOfBound" SlotName.codec Filter.IsControllerOfBound (\x -> case x of Filter.IsControllerOfBound y -> Just y; _ -> Nothing),
      -- Recursive like Not below, and for the atom's own reason rather than the
      -- combinator's: the payload describes the permanents being counted.
      Arm.payload "ControlsMoreThanYou" (codec keywordCodec) Filter.ControlsMoreThanYou (\x -> case x of Filter.ControlsMoreThanYou y -> Just y; _ -> Nothing),
      Arm.nullary "IsSource" Filter.IsSource,
      Arm.payload "IsBound" SlotName.codec Filter.IsBound (\x -> case x of Filter.IsBound y -> Just y; _ -> Nothing),
      Arm.nullary "IsAttacking" Filter.IsAttacking,
      Arm.nullary "IsBlocking" Filter.IsBlocking,
      Arm.nullary "AttackedThisTurn" Filter.AttackedThisTurn,
      Arm.nullary "IsAttachedToCreature" Filter.IsAttachedToCreature,
      Arm.nullary "IsAttachedToPermanent" Filter.IsAttachedToPermanent,
      Arm.nullary "IsAttachedToSource" Filter.IsAttachedToSource,
      Arm.nullary "CanHostSubject" Filter.CanHostSubject,
      Arm.nullary "IsToken" Filter.IsToken,
      Arm.nullary "IsTapped" Filter.IsTapped,
      Arm.nullary "IsRingBearer" Filter.IsRingBearer,
      Arm.payload "HasDesignation" Designation.codec Filter.HasDesignation (\x -> case x of Filter.HasDesignation y -> Just y; _ -> Nothing),
      Arm.payload "HasCounters" (CounterKind.codec keywordCodec) Filter.HasCounters (\x -> case x of Filter.HasCounters y -> Just y; _ -> Nothing),
      Arm.nullary "HasNonManaActivatedAbility" Filter.HasNonManaActivatedAbility,
      Arm.payload "And" (Common.list (codec keywordCodec)) Filter.And (\x -> case x of Filter.And y -> Just y; _ -> Nothing),
      Arm.payload "Or" (Common.list (codec keywordCodec)) Filter.Or (\x -> case x of Filter.Or y -> Just y; _ -> Nothing),
      Arm.payload "Not" (codec keywordCodec) Filter.Not (\x -> case x of Filter.Not y -> Just y; _ -> Nothing)
    ]
