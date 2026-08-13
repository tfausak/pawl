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
import qualified Pawl.Json.Value as Value
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
codec :: (Typeable.Typeable keyword) => Codec.Codec keyword -> Codec.Codec (Filter.Filter keyword)
codec keywordCodec =
  Arm.tagged
    encode
    [ Arm.payload "HasCardType" CardType.codec Filter.HasCardType,
      Arm.payload "HasSupertype" Supertype.codec Filter.HasSupertype,
      Arm.payload "HasColor" Color.codec Filter.HasColor,
      Arm.payload "HasSubtype" Subtype.codec Filter.HasSubtype,
      Arm.payload "HasKeyword" keywordCodec Filter.HasKeyword,
      Arm.payload "HasKeywordFamily" KeywordFamily.codec Filter.HasKeywordFamily,
      Arm.payload "PowerAtLeast" Common.integer Filter.PowerAtLeast,
      Arm.payload "PowerAtMost" Common.integer Filter.PowerAtMost,
      Arm.nullary "PowerLessThanSource" Filter.PowerLessThanSource,
      Arm.nullary "PowerGreaterThanSource" Filter.PowerGreaterThanSource,
      Arm.nullary "ControlledByDefendingPlayer" Filter.ControlledByDefendingPlayer,
      Arm.payload "ControlledByBound" SlotName.codec Filter.ControlledByBound,
      -- Runtime-only, and accepted here anyway: the codec must stay total, so a
      -- corpus lint keeps the pool honest instead (#199) -- the treatment
      -- Modification.SetController's baked PlayerId gets.
      Arm.payload "ControlledByPlayer" PlayerId.codec Filter.ControlledByPlayer,
      Arm.nullary "ControlledByRecipient" Filter.ControlledByRecipient,
      Arm.payload "ManaValueAtMost" Common.integer Filter.ManaValueAtMost,
      Arm.nullary "ManaValueIsEven" Filter.ManaValueIsEven,
      Arm.payload "ControlledBy" PlayerRelation.codec Filter.ControlledBy,
      Arm.payload "OwnedBy" PlayerRelation.codec Filter.OwnedBy,
      Arm.payload "IsPlayer" PlayerRelation.codec Filter.IsPlayer,
      -- Recursive like Not below, and for the atom's own reason rather than the
      -- combinator's: the payload describes the permanents being counted.
      Arm.payload "ControlsMoreThanYou" (codec keywordCodec) Filter.ControlsMoreThanYou,
      Arm.nullary "IsSource" Filter.IsSource,
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
      Arm.payload "HasDesignation" Designation.codec Filter.HasDesignation,
      Arm.payload "HasCounters" (CounterKind.codec keywordCodec) Filter.HasCounters,
      Arm.nullary "HasNonManaActivatedAbility" Filter.HasNonManaActivatedAbility,
      Arm.payload "And" (Common.list (codec keywordCodec)) Filter.And,
      Arm.payload "Or" (Common.list (codec keywordCodec)) Filter.Or,
      Arm.payload "Not" (codec keywordCodec) Filter.Not
    ]
  where
    encode filter_ = case filter_ of
      Filter.HasCardType t -> Common.tagged "HasCardType" . Just $ Codec.encode CardType.codec t
      Filter.HasSupertype sup -> Common.tagged "HasSupertype" . Just $ Codec.encode Supertype.codec sup
      Filter.HasColor c -> Common.tagged "HasColor" . Just $ Codec.encode Color.codec c
      Filter.HasSubtype sub -> Common.tagged "HasSubtype" . Just $ Codec.encode Subtype.codec sub
      Filter.HasKeyword k -> Common.tagged "HasKeyword" . Just $ Codec.encode keywordCodec k
      Filter.HasKeywordFamily f -> Common.tagged "HasKeywordFamily" . Just $ Codec.encode KeywordFamily.codec f
      Filter.PowerAtLeast n -> Common.tagged "PowerAtLeast" . Just $ Value.integer n
      Filter.PowerAtMost n -> Common.tagged "PowerAtMost" . Just $ Value.integer n
      Filter.PowerLessThanSource -> Common.nullary "PowerLessThanSource"
      Filter.PowerGreaterThanSource -> Common.nullary "PowerGreaterThanSource"
      Filter.ControlledByDefendingPlayer -> Common.nullary "ControlledByDefendingPlayer"
      Filter.ControlledByBound slot -> Common.tagged "ControlledByBound" . Just $ Codec.encode SlotName.codec slot
      Filter.ControlledByPlayer pid -> Common.tagged "ControlledByPlayer" . Just $ Codec.encode PlayerId.codec pid
      Filter.ControlledByRecipient -> Common.nullary "ControlledByRecipient"
      Filter.ManaValueAtMost n -> Common.tagged "ManaValueAtMost" . Just $ Value.integer n
      Filter.ManaValueIsEven -> Common.nullary "ManaValueIsEven"
      Filter.ControlledBy r -> Common.tagged "ControlledBy" . Just $ Codec.encode PlayerRelation.codec r
      Filter.OwnedBy r -> Common.tagged "OwnedBy" . Just $ Codec.encode PlayerRelation.codec r
      Filter.IsPlayer r -> Common.tagged "IsPlayer" . Just $ Codec.encode PlayerRelation.codec r
      Filter.ControlsMoreThanYou f -> Common.tagged "ControlsMoreThanYou" . Just $ Codec.encode (codec keywordCodec) f
      Filter.IsSource -> Common.nullary "IsSource"
      Filter.IsAttacking -> Common.nullary "IsAttacking"
      Filter.IsBlocking -> Common.nullary "IsBlocking"
      Filter.AttackedThisTurn -> Common.nullary "AttackedThisTurn"
      Filter.IsAttachedToCreature -> Common.nullary "IsAttachedToCreature"
      Filter.IsAttachedToPermanent -> Common.nullary "IsAttachedToPermanent"
      Filter.IsAttachedToSource -> Common.nullary "IsAttachedToSource"
      Filter.CanHostSubject -> Common.nullary "CanHostSubject"
      Filter.IsToken -> Common.nullary "IsToken"
      Filter.IsTapped -> Common.nullary "IsTapped"
      Filter.IsRingBearer -> Common.nullary "IsRingBearer"
      Filter.HasDesignation d -> Common.tagged "HasDesignation" . Just $ Codec.encode Designation.codec d
      Filter.HasCounters k -> Common.tagged "HasCounters" . Just $ Codec.encode (CounterKind.codec keywordCodec) k
      Filter.HasNonManaActivatedAbility -> Common.nullary "HasNonManaActivatedAbility"
      Filter.And fs -> Common.tagged "And" . Just $ Codec.encode (Common.list (codec keywordCodec)) fs
      Filter.Or fs -> Common.tagged "Or" . Just $ Codec.encode (Common.list (codec keywordCodec)) fs
      Filter.Not f -> Common.tagged "Not" . Just $ Codec.encode (codec keywordCodec) f
