module Pawl.Codec.Filter where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Expansion as Expansion
import qualified Pawl.Codec.KeywordFamily as KeywordFamily
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.ProductionTag as ProductionTag
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Codec.Zone as Zone
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
    tagOf
    [ Arm.payload "HasCardType" CardType.codec Filter.HasCardType (\x -> case x of Filter.HasCardType y -> Just y; _ -> Nothing),
      Arm.payload "HasSupertype" Supertype.codec Filter.HasSupertype (\x -> case x of Filter.HasSupertype y -> Just y; _ -> Nothing),
      Arm.payload "HasColor" Color.codec Filter.HasColor (\x -> case x of Filter.HasColor y -> Just y; _ -> Nothing),
      Arm.payload "HasSubtype" Subtype.codec Filter.HasSubtype (\x -> case x of Filter.HasSubtype y -> Just y; _ -> Nothing),
      Arm.payload "HasName" CardName.codec Filter.HasName (\x -> case x of Filter.HasName y -> Just y; _ -> Nothing),
      Arm.payload "HasNameOriginallyPrintedIn" Expansion.codec Filter.HasNameOriginallyPrintedIn (\x -> case x of Filter.HasNameOriginallyPrintedIn y -> Just y; _ -> Nothing),
      Arm.payload "HasKeyword" keywordCodec Filter.HasKeyword (\x -> case x of Filter.HasKeyword y -> Just y; _ -> Nothing),
      Arm.payload "HasKeywordFamily" KeywordFamily.codec Filter.HasKeywordFamily (\x -> case x of Filter.HasKeywordFamily y -> Just y; _ -> Nothing),
      Arm.payload "PowerAtLeast" Common.integer Filter.PowerAtLeast (\x -> case x of Filter.PowerAtLeast y -> Just y; _ -> Nothing),
      Arm.payload "PowerAtMost" Common.integer Filter.PowerAtMost (\x -> case x of Filter.PowerAtMost y -> Just y; _ -> Nothing),
      Arm.nullary "ToughnessGreaterThanPower" Filter.ToughnessGreaterThanPower,
      Arm.nullary "PowerLessThanSource" Filter.PowerLessThanSource,
      Arm.nullary "PowerGreaterThanSource" Filter.PowerGreaterThanSource,
      Arm.payload "PowerIsAmountInSlot" SlotName.codec Filter.PowerIsAmountInSlot (\x -> case x of Filter.PowerIsAmountInSlot y -> Just y; _ -> Nothing),
      Arm.payload "PowerAtLeastAmountInSlot" SlotName.codec Filter.PowerAtLeastAmountInSlot (\x -> case x of Filter.PowerAtLeastAmountInSlot y -> Just y; _ -> Nothing),
      Arm.nullary "ControlledByDefendingPlayer" Filter.ControlledByDefendingPlayer,
      Arm.payload "ControlledByBound" SlotName.codec Filter.ControlledByBound (\x -> case x of Filter.ControlledByBound y -> Just y; _ -> Nothing),
      -- Runtime-only, and accepted here anyway: the codec must stay total, so a
      -- corpus lint keeps the pool honest instead (#199) -- the treatment
      -- Modification.SetController's baked PlayerId gets.
      Arm.payload "ControlledByPlayer" PlayerId.codec Filter.ControlledByPlayer (\x -> case x of Filter.ControlledByPlayer y -> Just y; _ -> Nothing),
      Arm.nullary "ControlledByRecipient" Filter.ControlledByRecipient,
      Arm.payload "ManaValueAtMost" Common.integer Filter.ManaValueAtMost (\x -> case x of Filter.ManaValueAtMost y -> Just y; _ -> Nothing),
      Arm.nullary "ManaValueLessThanSource" Filter.ManaValueLessThanSource,
      Arm.nullary "ManaValueEqualToSource" Filter.ManaValueEqualToSource,
      Arm.nullary "SharesColorWithSource" Filter.SharesColorWithSource,
      Arm.nullary "ManaValueIsEven" Filter.ManaValueIsEven,
      Arm.nullary "ManaValueAtMostAmount" Filter.ManaValueAtMostAmount,
      Arm.nullary "ManaValueEqualToAmount" Filter.ManaValueEqualToAmount,
      Arm.payload "ControlledBy" PlayerRelation.codec Filter.ControlledBy (\x -> case x of Filter.ControlledBy y -> Just y; _ -> Nothing),
      Arm.payload "OwnedBy" PlayerRelation.codec Filter.OwnedBy (\x -> case x of Filter.OwnedBy y -> Just y; _ -> Nothing),
      Arm.payload "IsPlayer" PlayerRelation.codec Filter.IsPlayer (\x -> case x of Filter.IsPlayer y -> Just y; _ -> Nothing),
      Arm.payload "IsControllerOfBound" SlotName.codec Filter.IsControllerOfBound (\x -> case x of Filter.IsControllerOfBound y -> Just y; _ -> Nothing),
      -- Recursive like Not below, and for the atom's own reason rather than the
      -- combinator's: the payload describes the permanents being counted.
      Arm.payload "ControlsMoreThanYou" (codec keywordCodec) Filter.ControlsMoreThanYou (\x -> case x of Filter.ControlsMoreThanYou y -> Just y; _ -> Nothing),
      -- Natural rather than Common.integer above, so a negative literal is
      -- rejected at decode instead of decoding into a vacuously true filter: a
      -- zone holds no negative number of cards.
      Arm.payload "CardsInGraveyardAtLeast" Common.natural Filter.CardsInGraveyardAtLeast (\x -> case x of Filter.CardsInGraveyardAtLeast y -> Just y; _ -> Nothing),
      Arm.nullary "IsSource" Filter.IsSource,
      Arm.payload "IsObject" ObjectId.codec Filter.IsObject (\x -> case x of Filter.IsObject y -> Just y; _ -> Nothing),
      Arm.nullary "TargetsSource" Filter.TargetsSource,
      Arm.nullary "TargetsOnlySource" Filter.TargetsOnlySource,
      -- Recursive for ControlsMoreThanYou's reason: the payload describes the one
      -- TARGET, and a card author writes it exactly as they write any other filter.
      Arm.payload "TargetsOnlyOne" (codec keywordCodec) Filter.TargetsOnlyOne (\x -> case x of Filter.TargetsOnlyOne y -> Just y; _ -> Nothing),
      Arm.payload "TargetsMatching" (codec keywordCodec) Filter.TargetsMatching (\x -> case x of Filter.TargetsMatching y -> Just y; _ -> Nothing),
      Arm.payload "TargetsPlayer" PlayerRelation.codec Filter.TargetsPlayer (\x -> case x of Filter.TargetsPlayer y -> Just y; _ -> Nothing),
      Arm.payload "IsBound" SlotName.codec Filter.IsBound (\x -> case x of Filter.IsBound y -> Just y; _ -> Nothing),
      Arm.payload "SameNameAsBound" SlotName.codec Filter.SameNameAsBound (\x -> case x of Filter.SameNameAsBound y -> Just y; _ -> Nothing),
      Arm.nullary "SameNameAsSource" Filter.SameNameAsSource,
      Arm.nullary "SameOwnerAsSource" Filter.SameOwnerAsSource,
      Arm.payload "SameControllerAsBound" SlotName.codec Filter.SameControllerAsBound (\x -> case x of Filter.SameControllerAsBound y -> Just y; _ -> Nothing),
      Arm.payload "SameControllerAsHostOfBound" SlotName.codec Filter.SameControllerAsHostOfBound (\x -> case x of Filter.SameControllerAsHostOfBound y -> Just y; _ -> Nothing),
      Arm.payload "SharesCreatureTypeWithBound" SlotName.codec Filter.SharesCreatureTypeWithBound (\x -> case x of Filter.SharesCreatureTypeWithBound y -> Just y; _ -> Nothing),
      Arm.payload "ToughnessLessThanBound" SlotName.codec Filter.ToughnessLessThanBound (\x -> case x of Filter.ToughnessLessThanBound y -> Just y; _ -> Nothing),
      Arm.nullary "HasChosenName" Filter.HasChosenName,
      Arm.nullary "HasChosenColor" Filter.HasChosenColor,
      Arm.nullary "HasChosenSubtype" Filter.HasChosenSubtype,
      Arm.nullary "OfChosenPlayer" Filter.OfChosenPlayer,
      Arm.nullary "IsAttacking" Filter.IsAttacking,
      Arm.payload "IsAttackingPlayer" PlayerRelation.codec Filter.IsAttackingPlayer (\x -> case x of Filter.IsAttackingPlayer y -> Just y; _ -> Nothing),
      Arm.payload "IsAttackingPlaneswalker" PlayerRelation.codec Filter.IsAttackingPlaneswalker (\x -> case x of Filter.IsAttackingPlaneswalker y -> Just y; _ -> Nothing),
      Arm.payload "IsAttackingBattle" PlayerRelation.codec Filter.IsAttackingBattle (\x -> case x of Filter.IsAttackingBattle y -> Just y; _ -> Nothing),
      Arm.nullary "DeclaredAttackedThisCombat" Filter.DeclaredAttackedThisCombat,
      Arm.nullary "IsBlocking" Filter.IsBlocking,
      Arm.nullary "IsBlocked" Filter.IsBlocked,
      Arm.nullary "AttackedThisTurn" Filter.AttackedThisTurn,
      Arm.nullary "DeclaredAttackerThisCombat" Filter.DeclaredAttackerThisCombat,
      Arm.nullary "DeclaredBlockerThisCombat" Filter.DeclaredBlockerThisCombat,
      Arm.nullary "MilledThisTurn" Filter.MilledThisTurn,
      Arm.nullary "CrewedSourceThisTurn" Filter.CrewedSourceThisTurn,
      Arm.nullary "ConvokedSourceThisTurn" Filter.ConvokedSourceThisTurn,
      Arm.nullary "SaddledSourceThisTurn" Filter.SaddledSourceThisTurn,
      Arm.nullary "CantCrewVehicles" Filter.CantCrewVehicles,
      Arm.nullary "DealtDamageThisTurn" Filter.DealtDamageThisTurn,
      Arm.nullary "EnteredThisTurn" Filter.EnteredThisTurn,
      Arm.nullary "ControlledSinceTurnBegan" Filter.ControlledSinceTurnBegan,
      -- Recursive for ControlsMoreThanYou's reason: the payload describes the
      -- HOST, and a card author writes it exactly as they write any other filter.
      Arm.payload "AttachedTo" (codec keywordCodec) Filter.AttachedTo (\x -> case x of Filter.AttachedTo y -> Just y; _ -> Nothing),
      -- Recursive for the arm above's reason, the payload describing the ATTACHER
      -- rather than the host.
      Arm.payload "HasAttached" (codec keywordCodec) Filter.HasAttached (\x -> case x of Filter.HasAttached y -> Just y; _ -> Nothing),
      Arm.nullary "IsAttachedToSource" Filter.IsAttachedToSource,
      Arm.nullary "IsHostOfSource" Filter.IsHostOfSource,
      Arm.nullary "EnteredWithSource" Filter.EnteredWithSource,
      Arm.nullary "CanHostSubject" Filter.CanHostSubject,
      Arm.nullary "CanAttachToSubject" Filter.CanAttachToSubject,
      Arm.payload "HostOfSubjectHasCardType" CardType.codec Filter.HostOfSubjectHasCardType (\x -> case x of Filter.HostOfSubjectHasCardType y -> Just y; _ -> Nothing),
      Arm.nullary "IsCommander" Filter.IsCommander,
      Arm.nullary "IsToken" Filter.IsToken,
      Arm.nullary "IsActivatedAbility" Filter.IsActivatedAbility,
      Arm.nullary "IsAbility" Filter.IsAbility,
      Arm.nullary "IsEmblem" Filter.IsEmblem,
      -- Recursive for AttachedTo's reason, the payload describing the ability's
      -- SOURCE rather than the ability.
      Arm.payload "FromSource" (codec keywordCodec) Filter.FromSource (\x -> case x of Filter.FromSource y -> Just y; _ -> Nothing),
      Arm.nullary "IsTapped" Filter.IsTapped,
      Arm.nullary "IsFaceDown" Filter.IsFaceDown,
      -- Recursive for AttachedTo's reason, the payload describing the CARD
      -- representing the candidate rather than the candidate.
      Arm.payload "RepresentedByCard" (codec keywordCodec) Filter.RepresentedByCard (\x -> case x of Filter.RepresentedByCard y -> Just y; _ -> Nothing),
      Arm.nullary "IsExiledFaceDown" Filter.IsExiledFaceDown,
      Arm.nullary "Transformed" Filter.Transformed,
      Arm.nullary "IsRingBearer" Filter.IsRingBearer,
      Arm.nullary "IsPaired" Filter.IsPaired,
      Arm.nullary "IsPairedWithSource" Filter.IsPairedWithSource,
      Arm.nullary "IsBlockedBySource" Filter.IsBlockedBySource,
      Arm.payload "HasDesignation" Designation.codec Filter.HasDesignation (\x -> case x of Filter.HasDesignation y -> Just y; _ -> Nothing),
      Arm.payload "HasCounters" (CounterKind.codec keywordCodec) Filter.HasCounters (\x -> case x of Filter.HasCounters y -> Just y; _ -> Nothing),
      Arm.nullary "HasCountersOfAnyKind" Filter.HasCountersOfAnyKind,
      Arm.nullary "HasNonManaActivatedAbility" Filter.HasNonManaActivatedAbility,
      Arm.nullary "HasActivatedAbility" Filter.HasActivatedAbility,
      Arm.payload "IsInZone" Zone.codec Filter.IsInZone (\x -> case x of Filter.IsInZone y -> Just y; _ -> Nothing),
      Arm.payload "WasCastFrom" Zone.codec Filter.WasCastFrom (\x -> case x of Filter.WasCastFrom y -> Just y; _ -> Nothing),
      Arm.payload "TagWasSpent" ProductionTag.codec Filter.TagWasSpent (\x -> case x of Filter.TagWasSpent y -> Just y; _ -> Nothing),
      Arm.payload "And" (Common.list (codec keywordCodec)) Filter.And (\x -> case x of Filter.And y -> Just y; _ -> Nothing),
      Arm.payload "Or" (Common.list (codec keywordCodec)) Filter.Or (\x -> case x of Filter.Or y -> Just y; _ -> Nothing),
      Arm.payload "Not" (codec keywordCodec) Filter.Not (\x -> case x of Filter.Not y -> Just y; _ -> Nothing)
    ]

tagOf :: Filter.Filter keyword -> String
tagOf x = case x of
  Filter.HasCardType {} -> "HasCardType"
  Filter.HasSupertype {} -> "HasSupertype"
  Filter.HasColor {} -> "HasColor"
  Filter.HasSubtype {} -> "HasSubtype"
  Filter.HasName {} -> "HasName"
  Filter.HasNameOriginallyPrintedIn {} -> "HasNameOriginallyPrintedIn"
  Filter.HasKeyword {} -> "HasKeyword"
  Filter.HasKeywordFamily {} -> "HasKeywordFamily"
  Filter.PowerAtLeast {} -> "PowerAtLeast"
  Filter.PowerAtMost {} -> "PowerAtMost"
  Filter.ToughnessGreaterThanPower {} -> "ToughnessGreaterThanPower"
  Filter.PowerLessThanSource {} -> "PowerLessThanSource"
  Filter.PowerGreaterThanSource {} -> "PowerGreaterThanSource"
  Filter.PowerIsAmountInSlot {} -> "PowerIsAmountInSlot"
  Filter.PowerAtLeastAmountInSlot {} -> "PowerAtLeastAmountInSlot"
  Filter.ControlledByDefendingPlayer {} -> "ControlledByDefendingPlayer"
  Filter.ControlledByBound {} -> "ControlledByBound"
  Filter.ControlledByPlayer {} -> "ControlledByPlayer"
  Filter.ControlledByRecipient {} -> "ControlledByRecipient"
  Filter.ManaValueAtMost {} -> "ManaValueAtMost"
  Filter.ManaValueLessThanSource {} -> "ManaValueLessThanSource"
  Filter.ManaValueEqualToSource {} -> "ManaValueEqualToSource"
  Filter.SharesColorWithSource {} -> "SharesColorWithSource"
  Filter.ManaValueIsEven {} -> "ManaValueIsEven"
  Filter.ManaValueAtMostAmount {} -> "ManaValueAtMostAmount"
  Filter.ManaValueEqualToAmount {} -> "ManaValueEqualToAmount"
  Filter.ControlledBy {} -> "ControlledBy"
  Filter.OwnedBy {} -> "OwnedBy"
  Filter.IsPlayer {} -> "IsPlayer"
  Filter.IsControllerOfBound {} -> "IsControllerOfBound"
  Filter.ControlsMoreThanYou {} -> "ControlsMoreThanYou"
  Filter.CardsInGraveyardAtLeast {} -> "CardsInGraveyardAtLeast"
  Filter.IsSource {} -> "IsSource"
  Filter.IsObject {} -> "IsObject"
  Filter.TargetsSource {} -> "TargetsSource"
  Filter.TargetsOnlySource {} -> "TargetsOnlySource"
  Filter.TargetsOnlyOne {} -> "TargetsOnlyOne"
  Filter.TargetsMatching {} -> "TargetsMatching"
  Filter.TargetsPlayer {} -> "TargetsPlayer"
  Filter.IsBound {} -> "IsBound"
  Filter.SameNameAsBound {} -> "SameNameAsBound"
  Filter.SameNameAsSource {} -> "SameNameAsSource"
  Filter.SameOwnerAsSource {} -> "SameOwnerAsSource"
  Filter.SameControllerAsBound {} -> "SameControllerAsBound"
  Filter.SameControllerAsHostOfBound {} -> "SameControllerAsHostOfBound"
  Filter.SharesCreatureTypeWithBound {} -> "SharesCreatureTypeWithBound"
  Filter.ToughnessLessThanBound {} -> "ToughnessLessThanBound"
  Filter.HasChosenName {} -> "HasChosenName"
  Filter.HasChosenColor {} -> "HasChosenColor"
  Filter.HasChosenSubtype {} -> "HasChosenSubtype"
  Filter.OfChosenPlayer {} -> "OfChosenPlayer"
  Filter.IsAttacking {} -> "IsAttacking"
  Filter.IsAttackingPlayer {} -> "IsAttackingPlayer"
  Filter.IsAttackingPlaneswalker {} -> "IsAttackingPlaneswalker"
  Filter.IsAttackingBattle {} -> "IsAttackingBattle"
  Filter.DeclaredAttackedThisCombat {} -> "DeclaredAttackedThisCombat"
  Filter.IsBlocking {} -> "IsBlocking"
  Filter.IsBlocked {} -> "IsBlocked"
  Filter.AttackedThisTurn {} -> "AttackedThisTurn"
  Filter.DeclaredAttackerThisCombat {} -> "DeclaredAttackerThisCombat"
  Filter.DeclaredBlockerThisCombat {} -> "DeclaredBlockerThisCombat"
  Filter.MilledThisTurn {} -> "MilledThisTurn"
  Filter.CrewedSourceThisTurn {} -> "CrewedSourceThisTurn"
  Filter.ConvokedSourceThisTurn {} -> "ConvokedSourceThisTurn"
  Filter.SaddledSourceThisTurn {} -> "SaddledSourceThisTurn"
  Filter.CantCrewVehicles {} -> "CantCrewVehicles"
  Filter.DealtDamageThisTurn {} -> "DealtDamageThisTurn"
  Filter.EnteredThisTurn {} -> "EnteredThisTurn"
  Filter.ControlledSinceTurnBegan {} -> "ControlledSinceTurnBegan"
  Filter.AttachedTo {} -> "AttachedTo"
  Filter.HasAttached {} -> "HasAttached"
  Filter.IsAttachedToSource {} -> "IsAttachedToSource"
  Filter.IsHostOfSource {} -> "IsHostOfSource"
  Filter.EnteredWithSource {} -> "EnteredWithSource"
  Filter.CanHostSubject {} -> "CanHostSubject"
  Filter.CanAttachToSubject {} -> "CanAttachToSubject"
  Filter.HostOfSubjectHasCardType {} -> "HostOfSubjectHasCardType"
  Filter.IsCommander {} -> "IsCommander"
  Filter.IsToken {} -> "IsToken"
  Filter.IsActivatedAbility {} -> "IsActivatedAbility"
  Filter.IsAbility {} -> "IsAbility"
  Filter.IsEmblem {} -> "IsEmblem"
  Filter.FromSource {} -> "FromSource"
  Filter.IsTapped {} -> "IsTapped"
  Filter.IsFaceDown {} -> "IsFaceDown"
  Filter.RepresentedByCard {} -> "RepresentedByCard"
  Filter.IsExiledFaceDown {} -> "IsExiledFaceDown"
  Filter.Transformed {} -> "Transformed"
  Filter.IsRingBearer {} -> "IsRingBearer"
  Filter.IsPaired {} -> "IsPaired"
  Filter.IsPairedWithSource {} -> "IsPairedWithSource"
  Filter.IsBlockedBySource {} -> "IsBlockedBySource"
  Filter.HasDesignation {} -> "HasDesignation"
  Filter.HasCounters {} -> "HasCounters"
  Filter.HasCountersOfAnyKind {} -> "HasCountersOfAnyKind"
  Filter.HasNonManaActivatedAbility {} -> "HasNonManaActivatedAbility"
  Filter.HasActivatedAbility {} -> "HasActivatedAbility"
  Filter.IsInZone {} -> "IsInZone"
  Filter.WasCastFrom {} -> "WasCastFrom"
  Filter.TagWasSpent {} -> "TagWasSpent"
  Filter.And {} -> "And"
  Filter.Or {} -> "Or"
  Filter.Not {} -> "Not"
