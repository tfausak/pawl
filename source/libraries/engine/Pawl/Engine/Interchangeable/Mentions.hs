-- | Whether a stored tree -- an effect, an ability, a replacement, a cost, a
-- duration -- might name one object: the question
-- Pawl.Engine.Interchangeable asks of every stored row. Split out of it for
-- size.
--
-- One function per type, each matching every constructor POSITIONALLY, so a
-- new constructor is a missing-pattern error here and a new field an arity
-- error, rather than an ObjectId this silently stops reading. A field whose
-- type can hold no ObjectId, slot name or Filter is a named discard, and that
-- is the one site nothing forces: a type that later gains one owes every
-- discard of it here a function.
--
-- A tree names an object in three ways: an ObjectId it holds outright, a slot
-- it reads (slotNames), and a Filter atom (filterNames). A Card it holds is
-- printed data and names nothing -- Pawl.FilterPositionLintSpec's "CR
-- 702.119c no card writes IsObject" keeps the one runtime atom out of it. A
-- type parameter is answered by the function the caller passes for it.
module Pawl.Engine.Interchangeable.Mentions where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Binding as Binding.Engine
import qualified Pawl.Types.AbilityAddsMana as AbilityAddsMana
import qualified Pawl.Types.ActingPermanent as ActingPermanent
import qualified Pawl.Types.ActivateManaAbilities as ActivateManaAbilities
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationCriteria as ActivationCriteria
import qualified Pawl.Types.ActivationProhibition as ActivationProhibition
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.AfterObjectTurn as AfterObjectTurn
import qualified Pawl.Types.AgainstLastCardExiledWith as AgainstLastCardExiledWith
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AimedAt as AimedAt
import qualified Pawl.Types.AimedPlayers as AimedPlayers
import qualified Pawl.Types.AlternativeActivationCost as AlternativeActivationCost
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.Ante as Ante
import qualified Pawl.Types.AnyNumberDiscard as AnyNumberDiscard
import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Types.Arithmetic as Arithmetic
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.AttachAll as AttachAll
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.AttachRestriction as AttachRestriction
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.AttachedToBound as AttachedToBound
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackLimitUnless as AttackLimitUnless
import qualified Pawl.Types.AttackPermission as AttackPermission
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.AttackTargetRef as AttackTargetRef
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.Backup as Backup
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.Behold as Behold
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Blight as Blight
import qualified Pawl.Types.BlockCost as BlockCost
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.BoundMeasure as BoundMeasure
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.CantAttackPlayer as CantAttackPlayer
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.CantBlockCreatures as CantBlockCreatures
import qualified Pawl.Types.CardLeavesZone as CardLeavesZone
import qualified Pawl.Types.CardPutIntoGraveyard as CardPutIntoGraveyard
import qualified Pawl.Types.CardsPutIntoZone as CardsPutIntoZone
import qualified Pawl.Types.CastFrom as CastFrom
import qualified Pawl.Types.CastFromZone as CastFromZone
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.CastRepetition as CastRepetition
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT
import qualified Pawl.Types.ChooseCardName as ChooseCardName
import qualified Pawl.Types.ChooseNumber as ChooseNumber
import qualified Pawl.Types.ChoosePermanents as ChoosePermanents
import qualified Pawl.Types.ChoosePlayer as ChoosePlayer
import qualified Pawl.Types.ChoosePlayerAtRandom as ChoosePlayerAtRandom
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.ChosenPermanent as ChosenPermanent
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CoinFlipR as CoinFlipR
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.CompletedDungeon as CompletedDungeon
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.ConjureCards as ConjureCards
import qualified Pawl.Types.Connive as Connive
import qualified Pawl.Types.ControlPlayer as ControlPlayer
import qualified Pawl.Types.ControlSides as ControlSides
import qualified Pawl.Types.ControlSlots as ControlSlots
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CopyOriginal as CopyOriginal
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostAddition as CostAddition
import qualified Pawl.Types.CostBasis as CostBasis
import qualified Pawl.Types.CostChange as CostChange
import qualified Pawl.Types.CostChoice as CostChoice
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostModifier as CostModifier
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.CostSubject as CostSubject
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterDestination as CounterDestination
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.CounterRestriction as CounterRestriction
import qualified Pawl.Types.CounterSubject as CounterSubject
import qualified Pawl.Types.CountersFromPermanents as CountersFromPermanents
import qualified Pawl.Types.CountersFromThis as CountersFromThis
import qualified Pawl.Types.Craft as Craft
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.CreatureExploits as CreatureExploits
import qualified Pawl.Types.CrewRestriction as CrewRestriction
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.DestructionR as DestructionR
import qualified Pawl.Types.Devotion as Devotion
import qualified Pawl.Types.Devour as Devour
import qualified Pawl.Types.DieResult as DieResult
import qualified Pawl.Types.DieRollR as DieRollR
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DoesNotUntapNext as DoesNotUntapNext
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.DrawCountR as DrawCountR
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Earthbend as Earthbend
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Emerge as Emerge
import qualified Pawl.Types.EntersWith as EntersWith
import qualified Pawl.Types.EntryAttack as EntryAttack
import qualified Pawl.Types.EntryBlock as EntryBlock
import qualified Pawl.Types.EntryFlip as EntryFlip
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryPrice as EntryPrice
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRestriction as EntryRestriction
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Equip as Equip
import qualified Pawl.Types.ExchangeBlocks as ExchangeBlocks
import qualified Pawl.Types.ExchangeOwnership as ExchangeOwnership
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExchangeValues as ExchangeValues
import qualified Pawl.Types.ExchangeWithTopOfLibrary as ExchangeWithTopOfLibrary
import qualified Pawl.Types.ExchangeZones as ExchangeZones
import qualified Pawl.Types.ExchangedValue as ExchangedValue
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.ExileMaterials as ExileMaterials
import qualified Pawl.Types.ExilePermanents as ExilePermanents
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForEachNumber as ForEachNumber
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBeingBlocked as ForbidBeingBlocked
import qualified Pawl.Types.ForetellCost as ForetellCost
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.FromReference as FromReference
import qualified Pawl.Types.FullText as FullText
import qualified Pawl.Types.GainControl as GainControl
import qualified Pawl.Types.GrantLookAtExiled as GrantLookAtExiled
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.HandAction as HandAction
import qualified Pawl.Types.Impending as Impending
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordCount as KeywordCount
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.KeywordTally as KeywordTally
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LifeGainR as LifeGainR
import qualified Pawl.Types.LifeLoss as LifeLoss
import qualified Pawl.Types.LifeLossPattern as LifeLossPattern
import qualified Pawl.Types.LifeLossR as LifeLossR
import qualified Pawl.Types.LimitUnless as LimitUnless
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.MadnessCost as MadnessCost
import qualified Pawl.Types.MakeForetold as MakeForetold
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaCount as ManaCount
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRider as ManaRider
import qualified Pawl.Types.Measures as Measures
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillCountR as MillCountR
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifiedRoll as ModifiedRoll
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveMana as MoveMana
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Operand as Operand
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.OrElse as OrElse
import qualified Pawl.Types.PaidExpiry as PaidExpiry
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PendingDamageEffect as PendingDamageEffect
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import qualified Pawl.Types.PerCreature as PerCreature
import qualified Pawl.Types.PermanentActed as PermanentActed
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentCandidate as PermanentCandidate
import qualified Pawl.Types.PermanentDealsCombatDamageToPlayer as PermanentDealsCombatDamageToPlayer
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PermanentTappedForMana as PermanentTappedForMana
import qualified Pawl.Types.PermanentsBecomeTargeted as PermanentsBecomeTargeted
import qualified Pawl.Types.PermanentsDealCombatDamageToPlayer as PermanentsDealCombatDamageToPlayer
import qualified Pawl.Types.PlacesSticker as PlacesSticker
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerDesignationTally as PlayerDesignationTally
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.PlaysLand as PlaysLand
import qualified Pawl.Types.PlotFromZone as PlotFromZone
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PreventNextDamageInstance as PreventNextDamageInstance
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Prohibit as Prohibit
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.ProliferateR as ProliferateR
import qualified Pawl.Types.Protection as Protection
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.PutSticker as PutSticker
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RandomCardInGraveyard as RandomCardInGraveyard
import qualified Pawl.Types.RandomCardInHand as RandomCardInHand
import qualified Pawl.Types.RandomCardInLibrary as RandomCardInLibrary
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.RemovalCount as RemovalCount
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.RemoveCountersAmong as RemoveCountersAmong
import qualified Pawl.Types.RemovePlayerCounters as RemovePlayerCounters
import qualified Pawl.Types.Repeat as Repeat
import qualified Pawl.Types.RepeatIf as RepeatIf
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.RuleAbilities as RuleAbilities
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Types.SacrificeToEnter as SacrificeToEnter
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.ScryR as ScryR
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SelfCountersRemoved as SelfCountersRemoved
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.SetOwner as SetOwner
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotCount as SlotCount
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SlotPerPlayer as SlotPerPlayer
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.Splice as Splice
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Suspend as Suspend
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents
import qualified Pawl.Types.TargetChooser as TargetChooser
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TheseDiscard as TheseDiscard
import qualified Pawl.Types.Times as Times
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenPlus as TokenPlus
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.UntapR as UntapR
import qualified Pawl.Types.UntapRestriction as UntapRestriction
import qualified Pawl.Types.UntapRewrite as UntapRewrite
import qualified Pawl.Types.VillainousChoiceR as VillainousChoiceR
import qualified Pawl.Types.Vote as Vote
import qualified Pawl.Types.VoteChoices as VoteChoices
import qualified Pawl.Types.VoteObjects as VoteObjects
import qualified Pawl.Types.Ward as Ward
import qualified Pawl.Types.WhenSpent as WhenSpent
import qualified Pawl.Types.WhichCounters as WhichCounters
import qualified Pawl.Types.While as While
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR
import qualified Pawl.Types.ZoneScope as ZoneScope

-- | Whether the caller has searched the environment a tree's slots read.
data Slots
  = -- | The row stores its environment and the caller searched it.
    Searched
  | -- | Nothing fixes the environment, so a slot read might name anything.
    Unread

-- | The object a traversal looks for, and whether the slots it reads were
-- searched.
data Asking = MkAsking
  { object :: ObjectId,
    slots :: Slots
  }

-- Whether reading this slot might name the object.
slotNames :: Asking -> SlotName.SlotName -> Bool
slotNames asking _slot = contextNames asking

-- Whether an atom reading the resolution context -- an enclosing target slot's
-- amount, an attach's subject, a baked crew or attachment set -- might name the
-- object. The same answer as a slot read, since a stored row's context is its
-- environment.
contextNames :: Asking -> Bool
contextNames asking = case slots asking of
  Searched -> False
  Unread -> True

-- Whether a Filter might single this object out from one alike in every other
-- respect.
--
-- Two objects equal whole bar the timestamp, projecting alike, named by no
-- other object, relation, stored row or combat assignment, present every atom
-- the same Pawl.Engine.Filter.View in all but three respects, so an atom reading
-- none of them answers the same of both, now and after either is chosen:
--
--   * IDENTITY: IsObject names an id outright.
--   * The turn's EVENT LOG: the look-back atoms (AttackedThisTurn,
--     MilledThisTurn, DealtDamageThisTurn, EnteredThisTurn, the
--     crewed/convoked/saddled-the-source atoms) and EnteredWithSource
--     (GameState.enteredWith) read a record kept per object id.
--   * A RESOLUTION CONTEXT: the slot atoms, the slot and amount operands, the
--     attach and search subject atoms, CantCrewVehicles and
--     AttachedNoLaterThanSource's baked set, answered by slotNames and
--     contextNames.
--
-- No atom reads a timestamp. The source comparisons read the row's source,
-- which Pawl.Engine.Interchangeable checks is neither object; the combat atoms
-- read the combat it searches; the attachment atoms read the candidate's own
-- attachedTo, which whole-object Eq compares, or another object's, which
-- namedByAnother searches. A nested Filter is asked of some OTHER object -- a
-- host, a target, a source, a represented card, an attacher, a permanent a
-- player counts -- which may be one of the two, so it is recursed into.
filterNames :: Asking -> Filter.Filter keyword -> Bool
filterNames asking criterion = case criterion of
  Filter.HasCardType _cardType -> False
  Filter.HasSupertype _supertype -> False
  Filter.HasColor _color -> False
  Filter.IsMonocolored -> False
  Filter.SharesColorWithSource -> False
  Filter.HasSubtype _subtype -> False
  Filter.HasName _name -> False
  Filter.NameWordsAtLeast _n -> False
  Filter.HasNameOriginallyPrintedIn _expansion -> False
  Filter.HasKeyword _keyword -> False
  Filter.HasKeywordFamily _family -> False
  Filter.Measures measures -> measuresNames asking measures
  Filter.ManaValueIsEven -> False
  Filter.ControlledBy _relation -> False
  Filter.ControlledByDefendingPlayer -> False
  Filter.ControlledByBound slot -> slotNames asking slot
  Filter.ControlledByPlayer _player -> False
  Filter.ControlledByRecipient -> False
  Filter.OwnedBy _relation -> False
  Filter.OwnedByRecipient -> False
  Filter.IsSource -> False
  Filter.IsObject named -> named == object asking
  Filter.TargetsSource -> False
  Filter.TargetsOnlySource -> False
  Filter.TargetsOnlyOne nested -> filterNames asking nested
  Filter.HasSingleTarget -> False
  Filter.TargetsMatching nested -> filterNames asking nested
  Filter.TargetsPlayer _relation -> False
  Filter.IsBound slot -> slotNames asking slot
  Filter.IsTarget -> slotNames asking Binding.Engine.announcedTargets
  Filter.SameNameAsBound slot -> slotNames asking slot
  Filter.SameNameAsSource -> False
  Filter.SameOwnerAsSource -> False
  Filter.SameControllerAsBound slot -> slotNames asking slot
  Filter.SameControllerAsHostOfBound slot -> slotNames asking slot
  Filter.SharesCreatureTypeWithBound slot -> slotNames asking slot
  Filter.HasChosenName -> False
  Filter.HasChosenColor -> False
  Filter.HasChosenSubtype -> False
  Filter.IsLastExiledWithSource -> False
  Filter.OfChosenPlayer -> False
  Filter.OfRelatedPlayer _ -> False
  Filter.IsPlayer _relation -> False
  Filter.IsControllerOfBound slot -> slotNames asking slot
  Filter.ControlsMoreThanYou _margin nested -> filterNames asking nested
  Filter.CardsInGraveyardAtLeast _n -> False
  Filter.IsAttacking -> False
  Filter.IsAttackingPlayer _relation -> False
  Filter.IsAttackingPlaneswalker _relation -> False
  Filter.IsAttackingBattle _relation -> False
  Filter.DeclaredAttackedThisCombat -> False
  Filter.IsBlocking -> False
  Filter.IsBlocked -> False
  Filter.DeclaredAttackerThisCombat -> False
  Filter.DeclaredBlockerThisCombat -> False
  Filter.AttackedThisTurn -> True
  Filter.MilledThisTurn -> True
  Filter.CantCrewVehicles -> contextNames asking
  Filter.CrewedSourceThisTurn -> True
  Filter.ConvokedSourceThisTurn -> True
  Filter.SaddledSourceThisTurn -> True
  Filter.DealtDamageThisTurn -> True
  Filter.EnteredThisTurn -> True
  Filter.ControlledSinceTurnBegan -> False
  Filter.AttachedTo nested -> filterNames asking nested
  Filter.HasAttached nested -> filterNames asking nested
  Filter.IsAttachedToSource -> False
  Filter.IsAttachedToEvaluated -> False
  Filter.IsHostOfSource -> False
  Filter.EnteredWithSource -> True
  Filter.AttachedNoLaterThanSource -> contextNames asking
  Filter.CanHostSubject -> contextNames asking
  Filter.CanAttachToSubject -> contextNames asking
  Filter.HostOfSubjectHasCardType _cardType -> contextNames asking
  Filter.IsToken -> False
  Filter.IsCommander -> False
  Filter.IsActivatedAbility -> False
  Filter.IsAbility -> False
  Filter.IsEmblem -> False
  Filter.FromSource nested -> filterNames asking nested
  Filter.IsTapped -> False
  Filter.IsFaceDown -> False
  Filter.RepresentedByCard nested -> filterNames asking nested
  Filter.IsExiledFaceDown -> False
  Filter.Transformed -> False
  Filter.IsRingBearer -> False
  Filter.IsPaired -> False
  Filter.IsPairedWithSource -> False
  Filter.IsBlockedBySource -> False
  Filter.HasDesignation _designation -> False
  Filter.HasCounters _kind -> False
  Filter.HasCountersOfAnyKind -> False
  Filter.HasSticker _kind -> False
  Filter.Stickered -> False
  Filter.HasNonManaActivatedAbility -> False
  Filter.HasActivatedAbility -> False
  Filter.IsInZone _zone -> False
  Filter.WasCastFrom _zone -> False
  Filter.TagWasSpent _tag -> False
  Filter.Paid _keywordDesignator -> False
  Filter.And nested -> any (filterNames asking) nested
  Filter.Or nested -> any (filterNames asking) nested
  Filter.Not nested -> filterNames asking nested

abilityAddsManaNames :: Asking -> AbilityAddsMana.AbilityAddsMana -> Bool
abilityAddsManaNames asking x = case x of
  AbilityAddsMana.MkAbilityAddsMana _player source _mana -> filterNames asking source

activateManaAbilitiesNames :: Asking -> ActivateManaAbilities.ActivateManaAbilities -> Bool
activateManaAbilitiesNames asking x = case x of
  ActivateManaAbilities.MkActivateManaAbilities player filter_ -> playerRefNames asking player || filterNames asking filter_

activatedAbilityNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> ActivatedAbility.ActivatedAbility card ability -> Bool
activatedAbilityNames asking onCard onAbility x = case x of
  ActivatedAbility.MkActivatedAbility cost maximumX _minimumX modal restrictions _activator condition _name keyword -> costNames asking (keywordNames asking) cost || any (quantityNames asking) maximumX || modalNames asking onCard onAbility modal || any (activationRestrictionNames asking) restrictions || any (conditionNames asking) condition || any (keywordNames asking) keyword

activationProhibitionNames :: Asking -> ActivationProhibition.ActivationProhibition -> Bool
activationProhibitionNames asking x = case x of
  ActivationProhibition.MkActivationProhibition affected _kind _name -> affectedNames asking affected

activationRestrictionNames :: Asking -> ActivationRestriction.ActivationRestriction -> Bool
activationRestrictionNames asking x = case x of
  ActivationRestriction.SorcerySpeed -> False
  ActivationRestriction.DuringPhase _duringPhase -> False
  ActivationRestriction.DuringTurn _turnScope -> False
  ActivationRestriction.AttackedThisStep -> False
  ActivationRestriction.AfterBlockersDeclared -> False
  ActivationRestriction.BeforeCombatDamage -> False
  ActivationRestriction.BeforeEndStep -> False
  ActivationRestriction.OnlyIf condition -> conditionNames asking condition
  ActivationRestriction.OnlyOnce -> False
  ActivationRestriction.OnlyOnceEachTurn -> False
  ActivationRestriction.DuringDieRoll -> False
  ActivationRestriction.InstantSpeed -> False
  ActivationRestriction.SpendOnly _ -> False

activeReplacementNames :: Asking -> ActiveReplacement.ActiveReplacement -> Bool
activeReplacementNames asking x = case x of
  ActiveReplacement.MkActiveReplacement effect source _controller _timestamp expiry _uses _origin condition rider slots_ -> replacementEffectNames asking (const False) (grantedAbilityNames asking (const False)) (effectNames asking (const False) (grantedAbilityNames asking (const False))) effect || source == object asking || expiryNames asking expiry || any (conditionNames asking) condition || any (preventionRiderNames asking) rider || any (slotNames asking) (Map.keys slots_) || any (elem (object asking)) slots_

affectPlayersNames :: Asking -> AffectPlayers.AffectPlayers -> Bool
affectPlayersNames asking x = case x of
  AffectPlayers.MkAffectPlayers duration players effect -> durationNames asking duration || affectedPlayersNames asking (slotNames asking) players || playerEffectNames asking effect

affectedNames :: Asking -> Affected.Affected -> Bool
affectedNames asking x = case x of
  Affected.TheseObjects objectId -> elem (object asking) objectId
  Affected.Matching filter_ -> filterNames asking filter_
  Affected.MatchingAnywhere filter_ -> filterNames asking filter_
  Affected.MatchingOffBattlefield filter_ -> filterNames asking filter_
  Affected.Attached -> False
  Affected.AttachedPlayerControls filter_ -> filterNames asking filter_
  Affected.PlayedThisWay duration -> durationNames asking duration

affectedPlayersNames :: Asking -> (player -> Bool) -> AffectedPlayers.AffectedPlayers player -> Bool
affectedPlayersNames _asking onPlayer x = case x of
  AffectedPlayers.Scoped _playerScope -> False
  AffectedPlayers.Named player -> onPlayer player

affectedUnlessNames :: Asking -> AffectedUnless.AffectedUnless -> Bool
affectedUnlessNames asking x = case x of
  AffectedUnless.MkAffectedUnless affected unless _name -> affectedNames asking affected || any (conditionNames asking) unless

againstLastCardExiledWithNames :: Asking -> (quantity -> Bool) -> AgainstLastCardExiledWith.AgainstLastCardExiledWith quantity -> Bool
againstLastCardExiledWithNames asking onQuantity x = case x of
  AgainstLastCardExiledWith.MkAgainstLastCardExiledWith filter_ quantity -> filterNames asking filter_ || onQuantity quantity

againstSlotNames :: Asking -> (quantity -> Bool) -> AgainstSlot.AgainstSlot quantity -> Bool
againstSlotNames asking onQuantity x = case x of
  AgainstSlot.MkAgainstSlot slot quantity -> slotNames asking slot || onQuantity quantity

aggregationNames :: Asking -> (quantity -> Bool) -> Aggregation.Aggregation quantity -> Bool
aggregationNames _asking onQuantity x = case x of
  Aggregation.Members -> False
  Aggregation.DistinctCardTypes -> False
  Aggregation.DistinctColors -> False
  Aggregation.MostSharingACreatureType -> False
  Aggregation.MostSharingACardType -> False
  Aggregation.DistinctNames -> False
  Aggregation.Greatest quantity -> onQuantity quantity
  Aggregation.Total quantity -> onQuantity quantity

aimedAtNames :: Asking -> AimedAt.AimedAt -> Bool
aimedAtNames asking x = case x of
  AimedAt.MkAimedAt defenders _kinds -> aimedPlayersNames asking defenders

aimedPlayersNames :: Asking -> AimedPlayers.AimedPlayers -> Bool
aimedPlayersNames asking x = case x of
  AimedPlayers.Scoped _playerScope -> False
  AimedPlayers.EachInSlot slotName -> slotNames asking slotName
  AimedPlayers.BoundPlayer _playerId -> False

alternativeActivationCostNames :: Asking -> AlternativeActivationCost.AlternativeActivationCost -> Bool
alternativeActivationCostNames asking x = case x of
  AlternativeActivationCost.MkAlternativeActivationCost grantedBy _onlyFirst _cost -> keywordDesignatorNames asking grantedBy

alternativeCostNames :: Asking -> AlternativeCost.AlternativeCost -> Bool
alternativeCostNames asking x = case x of
  AlternativeCost.MkAlternativeCost condition cost -> any (conditionNames asking) condition || costNames asking (keywordNames asking) cost

amassNames :: Asking -> Amass.Amass -> Bool
amassNames asking x = case x of
  Amass.MkAmass quantity _subtype slot -> quantityNames asking quantity || any (slotNames asking) slot

anyNumberDiscardNames :: Asking -> AnyNumberDiscard.AnyNumberDiscard -> Bool
anyNumberDiscardNames asking x = case x of
  AnyNumberDiscard.MkAnyNumberDiscard player cards discarded -> playerRefNames asking player || anyNumberMatchingNames asking cards || any (slotNames asking) discarded

anyNumberMatchingNames :: Asking -> AnyNumberMatching.AnyNumberMatching -> Bool
anyNumberMatchingNames asking x = case x of
  AnyNumberMatching.MkAnyNumberMatching filter_ atMost -> filterNames asking filter_ || any (quantityNames asking) atMost

arithmeticNames :: Asking -> (quantity -> Bool) -> Arithmetic.Arithmetic quantity -> Bool
arithmeticNames asking onQuantity x = case x of
  Arithmetic.Plus plus -> plusNames asking onQuantity plus
  Arithmetic.Halved halved -> halvedNames asking onQuantity halved
  Arithmetic.Times times -> timesNames asking onQuantity times
  Arithmetic.Negate quantity -> onQuantity quantity

armDelayedTriggerNames :: Asking -> (ability -> Bool) -> ArmDelayedTrigger.ArmDelayedTrigger ability -> Bool
armDelayedTriggerNames asking onAbility x = case x of
  ArmDelayedTrigger.MkArmDelayedTrigger _name _onset duration ability -> any (durationNames asking) duration || any onAbility ability

asCopyNames :: Asking -> (ability -> Bool) -> (effect -> Bool) -> AsCopy.AsCopy ability effect -> Bool
asCopyNames asking onAbility onEffect x = case x of
  AsCopy.MkAsCopy eligible exceptions _tapped counters whenYouDo _zone -> filterNames asking eligible || any (copyExceptionNames asking onAbility) exceptions || any (withCountersNames asking) counters || any onEffect whenYouDo

attachAllNames :: Asking -> AttachAll.AttachAll -> Bool
attachAllNames asking x = case x of
  AttachAll.MkAttachAll subjects destination -> objectRefNames asking subjects || filterNames asking destination

attachBoundNames :: Asking -> AttachBound.AttachBound -> Bool
attachBoundNames asking x = case x of
  AttachBound.MkAttachBound subject destination -> slotNames asking subject || slotNames asking destination

attachRestrictionNames :: Asking -> AttachRestriction.AttachRestriction -> Bool
attachRestrictionNames asking x = case x of
  AttachRestriction.MkAttachRestriction affected attachers -> affectedNames asking affected || filterNames asking attachers

attachTargetNames :: Asking -> AttachTarget.AttachTarget -> Bool
attachTargetNames asking x = case x of
  AttachTarget.MkAttachTarget slot filter_ -> slotNames asking slot || filterNames asking filter_

attachedToBoundNames :: Asking -> AttachedToBound.AttachedToBound -> Bool
attachedToBoundNames asking x = case x of
  AttachedToBound.MkAttachedToBound slot filter_ -> slotNames asking slot || filterNames asking filter_

attackCostNames :: Asking -> AttackCost.AttackCost -> Bool
attackCostNames asking x = case x of
  AttackCost.MkAttackCost subject perAttacker _scope -> affectedNames asking subject || perCreatureNames asking perAttacker

attackLimitUnlessNames :: Asking -> AttackLimitUnless.AttackLimitUnless -> Bool
attackLimitUnlessNames asking x = case x of
  AttackLimitUnless.MkAttackLimitUnless _limit _defenders unless -> any (conditionNames asking) unless

attackPermissionNames :: Asking -> AttackPermission.AttackPermission -> Bool
attackPermissionNames asking x = case x of
  AttackPermission.MkAttackPermission affected -> affectedNames asking affected

attackRequirementNames :: Asking -> AttackRequirement.AttackRequirement -> Bool
attackRequirementNames asking x = case x of
  AttackRequirement.MkAttackRequirement subject _object_ while _arity -> affectedNames asking subject || any (conditionNames asking) while

attackTargetRefNames :: Asking -> AttackTargetRef.AttackTargetRef -> Bool
attackTargetRefNames asking x = case x of
  AttackTargetRef.Players playerRef -> playerRefNames asking playerRef
  AttackTargetRef.Permanents objectRef -> objectRefNames asking objectRef

attackingPlayersNames :: Asking -> AttackingPlayers.AttackingPlayers -> Bool
attackingPlayersNames asking x = case x of
  AttackingPlayers.MkAttackingPlayers _relation attacked -> slotNames asking attacked

backupNames :: Asking -> (keyword -> Bool) -> Backup.Backup keyword -> Bool
backupNames _asking onKeyword x = case x of
  Backup.MkBackup _count printedAbove -> any onKeyword printedAbove

becomeCopyNames :: Asking -> (ability -> Bool) -> BecomeCopy.BecomeCopy ability -> Bool
becomeCopyNames asking onAbility x = case x of
  BecomeCopy.MkBecomeCopy original subject duration exceptions _newTargets -> copyOriginalNames asking original || objectRefNames asking subject || any (durationNames asking) duration || any (copyExceptionNames asking onAbility) exceptions

beholdNames :: Asking -> Behold.Behold keyword -> Bool
beholdNames asking x = case x of
  Behold.MkBehold _count whichObjects -> filterNames asking whichObjects

bindingNames :: Asking -> Binding.Binding -> Bool
bindingNames asking x = case x of
  Binding.MkBinding targets _amount _modes copy objects _sticker -> any (any (recipientNames asking)) targets || any (projectedCharacteristicsNames asking) copy || any (elem (object asking)) objects

blightNames :: Asking -> Blight.Blight -> Bool
blightNames asking x = case x of
  Blight.MkBlight player quantity slot -> playerRefNames asking player || quantityNames asking quantity || any (slotNames asking) slot

blockCostNames :: Asking -> BlockCost.BlockCost -> Bool
blockCostNames asking x = case x of
  BlockCost.MkBlockCost subject perBlocker attackers -> affectedNames asking subject || perCreatureNames asking perBlocker || any (filterNames asking) attackers

blockPermissionNames :: Asking -> BlockPermission.BlockPermission -> Bool
blockPermissionNames asking x = case x of
  BlockPermission.MkBlockPermission affected additional while -> affectedNames asking affected || any (quantityNames asking) additional || any (conditionNames asking) while

blockRequirementNames :: Asking -> BlockRequirement.BlockRequirement -> Bool
blockRequirementNames asking x = case x of
  BlockRequirement.MkBlockRequirement subject attacker while _arity -> any (affectedNames asking) subject || any (affectedNames asking) attacker || any (conditionNames asking) while

boundMeasureNames :: Asking -> BoundMeasure.BoundMeasure -> Bool
boundMeasureNames asking x = case x of
  BoundMeasure.MkBoundMeasure slot _measure -> slotNames asking slot

candidateIdNames :: Asking -> CandidateId.CandidateId -> Bool
candidateIdNames asking x = case x of
  CandidateId.OfPermanent permanentCandidate -> permanentCandidateNames asking permanentCandidate
  CandidateId.OfFloating floatingCandidate -> floatingCandidateNames asking floatingCandidate

cantAttackPlayerNames :: Asking -> CantAttackPlayer.CantAttackPlayer -> Bool
cantAttackPlayerNames asking x = case x of
  CantAttackPlayer.MkCantAttackPlayer affected _defenders _kinds unless _name -> affectedNames asking affected || any (conditionNames asking) unless

cantBeBlockedByNames :: Asking -> CantBeBlockedBy.CantBeBlockedBy -> Bool
cantBeBlockedByNames asking x = case x of
  CantBeBlockedBy.MkCantBeBlockedBy affected blockers unless _name -> affectedNames asking affected || filterNames asking blockers || any (conditionNames asking) unless

cantBlockCreaturesNames :: Asking -> CantBlockCreatures.CantBlockCreatures -> Bool
cantBlockCreaturesNames asking x = case x of
  CantBlockCreatures.MkCantBlockCreatures affected attackers unless _name -> affectedNames asking affected || filterNames asking attackers || any (conditionNames asking) unless

cardLeavesZoneNames :: Asking -> CardLeavesZone.CardLeavesZone -> Bool
cardLeavesZoneNames asking x = case x of
  CardLeavesZone.MkCardLeavesZone filter_ _scope _from _to _whose -> filterNames asking filter_

cardPutIntoGraveyardNames :: Asking -> CardPutIntoGraveyard.CardPutIntoGraveyard -> Bool
cardPutIntoGraveyardNames asking x = case x of
  CardPutIntoGraveyard.MkCardPutIntoGraveyard filter_ _from -> filterNames asking filter_

cardsPutIntoZoneNames :: Asking -> CardsPutIntoZone.CardsPutIntoZone -> Bool
cardsPutIntoZoneNames asking x = case x of
  CardsPutIntoZone.MkCardsPutIntoZone filter_ _from _to -> filterNames asking filter_

castFromNames :: Asking -> CastFrom.CastFrom -> Bool
castFromNames asking x = case x of
  CastFrom.MkCastFrom caster from -> playerRefNames asking caster || inZoneNames asking from

castFromZoneNames :: Asking -> CastFromZone.CastFromZone -> Bool
castFromZoneNames asking x = case x of
  CastFromZone.MkCastFromZone from matching _limit _verb _pool additionalCosts _reduction -> inZoneNames asking from || filterNames asking matching || any (costComponentNames asking (keywordNames asking)) additionalCosts

castOfferNames :: Asking -> CastOffer.CastOffer -> Bool
castOfferNames asking x = case x of
  CastOffer.MkCastOffer _transformed _withoutPayingManaCost payingInstead _spending restriction offeredBy -> any (costNames asking (keywordNames asking)) payingInstead || any (filterNames asking) restriction || any (keywordNames asking) offeredBy

castRepetitionNames :: Asking -> CastRepetition.CastRepetition -> Bool
castRepetitionNames asking x = case x of
  CastRepetition.Once -> False
  CastRepetition.AnyNumber -> False
  CastRepetition.WithinTotalManaValue quantity -> quantityNames asking quantity

changeTextNames :: Asking -> ChangeText.ChangeText -> Bool
changeTextNames asking x = case x of
  ChangeText.MkChangeText _family _forbidden slot -> slotNames asking slot

characteristicPTNames :: Asking -> CharacteristicPT.CharacteristicPT -> Bool
characteristicPTNames asking x = case x of
  CharacteristicPT.MkCharacteristicPT power toughness -> quantityNames asking power || quantityNames asking toughness

chooseCardNameNames :: Asking -> ChooseCardName.ChooseCardName -> Bool
chooseCardNameNames asking x = case x of
  ChooseCardName.MkChooseCardName player restriction -> playerRefNames asking player || filterNames asking restriction

chooseNumberNames :: Asking -> ChooseNumber.ChooseNumber -> Bool
chooseNumberNames asking x = case x of
  ChooseNumber.MkChooseNumber slot _upTo -> slotNames asking slot

choosePermanentsNames :: Asking -> ChoosePermanents.ChoosePermanents -> Bool
choosePermanentsNames asking x = case x of
  ChoosePermanents.MkChoosePermanents chooser permanents slot -> playerRefNames asking chooser || anyNumberMatchingNames asking permanents || slotNames asking slot

choosePlayerNames :: Asking -> ChoosePlayer.ChoosePlayer -> Bool
choosePlayerNames asking x = case x of
  ChoosePlayer.MkChoosePlayer _scope slot -> slotNames asking slot

choosePlayerAtRandomNames :: Asking -> ChoosePlayerAtRandom.ChoosePlayerAtRandom -> Bool
choosePlayerAtRandomNames asking x = case x of
  ChoosePlayerAtRandom.MkChoosePlayerAtRandom _scope slot -> slotNames asking slot

chooserNames :: Asking -> Chooser.Chooser -> Bool
chooserNames asking x = case x of
  Chooser.TheController -> False
  Chooser.EachInScope -> False
  Chooser.BoundInSlot slotName -> slotNames asking slotName

chosenCardFromAmongNames :: Asking -> ChosenCardFromAmong.ChosenCardFromAmong -> Bool
chosenCardFromAmongNames asking x = case x of
  ChosenCardFromAmong.MkChosenCardFromAmong slot filter_ count chooser _upTo -> slotNames asking slot || filterNames asking filter_ || quantityNames asking count || playerRefNames asking chooser

chosenCardInGraveyardNames :: Asking -> ChosenCardInGraveyard.ChosenCardInGraveyard -> Bool
chosenCardInGraveyardNames asking x = case x of
  ChosenCardInGraveyard.MkChosenCardInGraveyard chooser players filter_ count -> chooserNames asking chooser || zoneScopeNames asking players || filterNames asking filter_ || quantityNames asking count

chosenCardInHandNames :: Asking -> ChosenCardInHand.ChosenCardInHand -> Bool
chosenCardInHandNames asking x = case x of
  ChosenCardInHand.MkChosenCardInHand player filter_ -> playerRefNames asking player || filterNames asking filter_

chosenPermanentNames :: Asking -> ChosenPermanent.ChosenPermanent -> Bool
chosenPermanentNames asking x = case x of
  ChosenPermanent.MkChosenPermanent filter_ chooser -> filterNames asking filter_ || playerRefNames asking chooser

clauseNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> Clause.Clause card ability -> Bool
clauseNames asking onCard onAbility x = case x of
  Clause.MkClause _ifTaken condition orElse optionality payGate effects -> any (conditionNames asking) condition || any (orElseNames asking) orElse || optionalityNames asking optionality || any (payGateNames asking) payGate || any (effectNames asking onCard onAbility) effects

coinFlipRNames :: Asking -> CoinFlipR.CoinFlipR -> Bool
coinFlipRNames asking x = case x of
  CoinFlipR.MkCoinFlipR whose _rewrite -> controllerRelationNames asking whose

combatRestrictionNames :: Asking -> CombatRestriction.CombatRestriction -> Bool
combatRestrictionNames asking x = case x of
  CombatRestriction.CantAttack affectedUnless -> affectedUnlessNames asking affectedUnless
  CombatRestriction.CantBlock affectedUnless -> affectedUnlessNames asking affectedUnless
  CombatRestriction.CantBeBlockedBy cantBeBlockedBy -> cantBeBlockedByNames asking cantBeBlockedBy
  CombatRestriction.CantBlockCreatures cantBlockCreatures -> cantBlockCreaturesNames asking cantBlockCreatures
  CombatRestriction.CantAttackPlayer cantAttackPlayer -> cantAttackPlayerNames asking cantAttackPlayer
  CombatRestriction.CantAttackAlone affectedUnless -> affectedUnlessNames asking affectedUnless
  CombatRestriction.CantAttackMoreThan attackLimitUnless -> attackLimitUnlessNames asking attackLimitUnless
  CombatRestriction.CantBlockMoreThan limitUnless -> limitUnlessNames asking limitUnless

comparesNames :: Asking -> Compares.Compares -> Bool
comparesNames asking x = case x of
  Compares.MkCompares measured _comparison threshold -> quantityNames asking measured || quantityNames asking threshold

completedDungeonNames :: Asking -> CompletedDungeon.CompletedDungeon -> Bool
completedDungeonNames asking x = case x of
  CompletedDungeon.MkCompletedDungeon player _dungeon -> playerRefNames asking player

conditionNames :: Asking -> Condition.Condition -> Bool
conditionNames asking x = case x of
  Condition.Compares compares -> comparesNames asking compares
  Condition.Any items -> any (conditionNames asking) items
  Condition.All items -> any (conditionNames asking) items
  Condition.During _duringPhase -> False

conjureNames :: Asking -> (card -> Bool) -> Conjure.Conjure card -> Bool
conjureNames asking onCard x = case x of
  Conjure.MkConjure quantity cards _selection _destination slot -> quantityNames asking quantity || conjureCardsNames asking onCard cards || any (slotNames asking) slot

conjureCardsNames :: Asking -> (card -> Bool) -> ConjureCards.ConjureCards card -> Bool
conjureCardsNames asking onCard x = case x of
  ConjureCards.Written nonEmpty -> any onCard nonEmpty
  ConjureCards.Duplicate objectRef -> objectRefNames asking objectRef
  ConjureCards.Reference fromReference -> fromReferenceNames asking fromReference

conniveNames :: Asking -> Connive.Connive -> Bool
conniveNames asking x = case x of
  Connive.MkConnive quantity ref -> quantityNames asking quantity || objectRefNames asking ref

controlPlayerNames :: Asking -> ControlPlayer.ControlPlayer -> Bool
controlPlayerNames asking x = case x of
  ControlPlayer.MkControlPlayer slot _manaFromLandsOnly -> slotNames asking slot

controlSidesNames :: Asking -> ControlSides.ControlSides -> Bool
controlSidesNames asking x = case x of
  ControlSides.BetweenTargets slotName -> slotNames asking slotName
  ControlSides.WithSource slotName -> slotNames asking slotName
  ControlSides.BetweenSlots (ControlSlots.MkControlSlots one two) -> slotNames asking one || slotNames asking two

controllerRelationNames :: Asking -> ControllerRelation.ControllerRelation -> Bool
controllerRelationNames asking x = case x of
  ControllerRelation.Related _ -> False
  ControllerRelation.EnchantedPlayers -> False
  ControllerRelation.InSlot slotName -> slotNames asking slotName
  ControllerRelation.Among _playerId -> False

copyExceptionNames :: Asking -> (ability -> Bool) -> CopyException.CopyException ability -> Bool
copyExceptionNames asking onAbility x = case x of
  CopyException.SetPowerToughness _setPowerToughness -> False
  CopyException.GainKeywords keyword -> any (keywordNames asking) keyword
  CopyException.GainThisAbility -> False
  CopyException.AddCardTypes _cardType -> False
  CopyException.AddSubtypes _subtype -> False
  CopyException.AddSupertypes _supertype -> False
  CopyException.RemoveSupertypes _supertype -> False
  CopyException.SetName _cardName -> False
  CopyException.SetColors _color -> False
  CopyException.NoManaCost -> False
  CopyException.GainAbility ability -> onAbility ability
  CopyException.DontCopyColors -> False

copyOriginalNames :: Asking -> CopyOriginal.CopyOriginal -> Bool
copyOriginalNames asking x = case x of
  CopyOriginal.OfObject objectRef -> objectRefNames asking objectRef
  CopyOriginal.Named _cardName -> False

copyStackObjectNames :: Asking -> (ability -> Bool) -> CopyStackObject.CopyStackObject ability -> Bool
copyStackObjectNames asking onAbility x = case x of
  CopyStackObject.MkCopyStackObject ref targets quantity copier exceptions -> objectRefNames asking ref || copyTargetsNames asking targets || quantityNames asking quantity || playerRefNames asking copier || any (copyExceptionNames asking onAbility) exceptions

copyTargetsNames :: Asking -> CopyTargets.CopyTargets -> Bool
copyTargetsNames asking x = case x of
  CopyTargets.Copied -> False
  CopyTargets.ChosenByController -> False
  CopyTargets.ForEach objectRef -> any (objectRefNames asking) objectRef
  CopyTargets.Stated objectRef -> objectRefNames asking objectRef

costNames :: Asking -> (keyword -> Bool) -> Cost.Cost keyword -> Bool
costNames asking onKeyword x = case x of
  Cost.MkCost _mana components -> any (costComponentNames asking onKeyword) components

costBasisNames :: Asking -> CostBasis.CostBasis -> Bool
costBasisNames asking x = case x of
  CostBasis.MkCostBasis slot _reducedBy -> slotNames asking slot

costChoiceNames :: Asking -> CostChoice.CostChoice -> Bool
costChoiceNames asking x = case x of
  CostChoice.MkCostChoice unwrap -> any (costNames asking (keywordNames asking)) unwrap

costComponentNames :: Asking -> (keyword -> Bool) -> CostComponent.CostComponent keyword -> Bool
costComponentNames asking onKeyword x = case x of
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.ReturnThis -> False
  CostComponent.PayLife _amount -> False
  CostComponent.PayHalfLife _rounding -> False
  CostComponent.Sacrifice sacrifice -> sacrificeNames asking sacrifice
  CostComponent.TapForTotalPower tapForTotalPower -> tapForTotalPowerNames asking tapForTotalPower
  CostComponent.TapPermanents tapPermanents -> tapPermanentsNames asking tapPermanents
  CostComponent.ReturnPermanents returnPermanents -> returnPermanentsNames asking returnPermanents
  CostComponent.ExilePermanents exilePermanents -> exilePermanentsNames asking exilePermanents
  CostComponent.DiscardCards discardCards -> discardCardsNames asking discardCards
  CostComponent.DiscardThis _discardCause -> False
  CostComponent.PutCardFromHandOntoBattlefield filter_ -> filterNames asking filter_
  CostComponent.PayEnergy _amount -> False
  CostComponent.AddLoyaltyToThis _natural -> False
  CostComponent.RemoveLoyaltyFromThis _amount -> False
  CostComponent.RemoveCountersFromThis countersFromThis -> countersFromThisNames asking onKeyword countersFromThis
  CostComponent.RemoveCounters countersFromPermanents -> countersFromPermanentsNames asking onKeyword countersFromPermanents
  CostComponent.PutPlusOneCountersOnThis _natural -> False
  CostComponent.Blight _amount -> False
  CostComponent.Forage -> False
  CostComponent.FlipCoin -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileThis -> False
  CostComponent.ExileCardsFromGraveyard exileCardsFromGraveyard -> exileCardsFromGraveyardNames asking exileCardsFromGraveyard
  CostComponent.ExileMaterials exileMaterials -> exileMaterialsNames asking exileMaterials
  CostComponent.ExileTopFromGraveyard filter_ -> filterNames asking filter_
  CostComponent.CollectEvidence _natural -> False
  CostComponent.CollectEvidenceOfTargets -> False
  CostComponent.ExileCardFromHand filter_ -> filterNames asking filter_
  CostComponent.RevealCardFromHand filter_ -> filterNames asking filter_
  CostComponent.Behold behold -> beholdNames asking behold
  CostComponent.BeholdAndExile filter_ -> filterNames asking filter_
  CostComponent.MillCards _natural -> False
  CostComponent.RevealTopOfLibrary _natural -> False
  CostComponent.ChooseOpponent -> False
  CostComponent.Waterbend _amount -> False
  CostComponent.WaterbendInstead _natural -> False

costReductionNames :: Asking -> CostReduction.CostReduction -> Bool
costReductionNames asking x = case x of
  CostReduction.MkCostReduction _amount perEach condition whichTargets _direction -> quantityNames asking perEach || any (conditionNames asking) condition || any (filterNames asking) whichTargets

countNames :: Asking -> (quantity -> Bool) -> Count.Count quantity -> Bool
countNames asking onQuantity x = case x of
  Count.MkCount scope filter_ aggregation -> scopeNames asking scope || filterNames asking filter_ || aggregationNames asking onQuantity aggregation

countedDiscardNames :: Asking -> CountedDiscard.CountedDiscard -> Bool
countedDiscardNames asking x = case x of
  CountedDiscard.MkCountedDiscard slot quantity discarded -> slotNames asking slot || quantityNames asking quantity || any (slotNames asking) discarded

counterNames :: Asking -> Counter.Counter -> Bool
counterNames asking x = case x of
  Counter.MkCounter ref slot sources instead -> objectRefNames asking ref || any (slotNames asking) slot || any (slotNames asking) sources || any (counterDestinationNames asking) instead

counterDestinationNames :: Asking -> CounterDestination.CounterDestination -> Bool
counterDestinationNames asking x = case x of
  CounterDestination.MkCounterDestination _zone _position only slot -> any (filterNames asking) only || any (slotNames asking) slot

counterKindNames :: Asking -> (keyword -> Bool) -> CounterKind.CounterKind keyword -> Bool
counterKindNames _asking onKeyword x = case x of
  CounterKind.PlusOnePlusOne -> False
  CounterKind.MinusOneMinusOne -> False
  CounterKind.Keyword keyword -> onKeyword keyword
  CounterKind.Loyalty -> False
  CounterKind.Lore -> False
  CounterKind.Defense -> False
  CounterKind.Time -> False
  CounterKind.Fade -> False
  CounterKind.Age -> False
  CounterKind.Shield -> False
  CounterKind.Finality -> False
  CounterKind.Stun -> False
  CounterKind.Level -> False
  CounterKind.Hone -> False
  CounterKind.Named _counterName -> False

counterPatternNames :: Asking -> CounterPattern.CounterPattern -> Bool
counterPatternNames asking x = case x of
  CounterPattern.MkCounterPattern whichKind subject whose onWhat onWho -> any (counterKindNames asking (keywordNames asking)) whichKind || counterSubjectNames asking subject || controllerRelationNames asking whose || filterNames asking onWhat || any (controllerRelationNames asking) onWho

counterPlacementNames :: Asking -> CounterPlacement.CounterPlacement -> Bool
counterPlacementNames asking x = case x of
  CounterPlacement.MkCounterPlacement kind permanents -> counterKindNames asking (keywordNames asking) kind || filterNames asking permanents

counterRNames :: Asking -> CounterR.CounterR -> Bool
counterRNames asking x = case x of
  CounterR.MkCounterR matching _scaling -> counterPatternNames asking matching

counterRestrictionNames :: Asking -> CounterRestriction.CounterRestriction -> Bool
counterRestrictionNames asking x = case x of
  CounterRestriction.MkCounterRestriction affected kind -> affectedNames asking affected || any (counterKindNames asking (keywordNames asking)) kind

counterSubjectNames :: Asking -> CounterSubject.CounterSubject -> Bool
counterSubjectNames asking x = case x of
  CounterSubject.ByEffect -> False
  CounterSubject.ByPlayer controllerRelation -> controllerRelationNames asking controllerRelation
  CounterSubject.ByAnything -> False

countersFromPermanentsNames :: Asking -> (keyword -> Bool) -> CountersFromPermanents.CountersFromPermanents keyword -> Bool
countersFromPermanentsNames asking onKeyword x = case x of
  CountersFromPermanents.MkCountersFromPermanents _count kind whichPermanent _spread -> whichCountersNames asking onKeyword kind || filterNames asking whichPermanent

countersFromThisNames :: Asking -> (keyword -> Bool) -> CountersFromThis.CountersFromThis keyword -> Bool
countersFromThisNames asking onKeyword x = case x of
  CountersFromThis.MkCountersFromThis kind _count -> counterKindNames asking onKeyword kind

craftNames :: Asking -> (keyword -> Bool) -> Craft.Craft keyword -> Bool
craftNames asking onKeyword x = case x of
  Craft.MkCraft cost materials -> costNames asking onKeyword cost || exileMaterialsNames asking materials

createNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> Create.Create card ability -> Bool
createNames asking onCard onAbility x = case x of
  Create.MkCreate quantity card riders slot creator -> quantityNames asking quantity || onCard card || entryRidersNames asking (quantityNames asking) onAbility riders || any (slotNames asking) slot || playerRefNames asking creator

createCopyNames :: Asking -> (ability -> Bool) -> CreateCopy.CreateCopy ability -> Bool
createCopyNames asking onAbility x = case x of
  CreateCopy.MkCreateCopy quantity ref riders slot exceptions -> quantityNames asking quantity || objectRefNames asking ref || entryRidersNames asking (quantityNames asking) onAbility riders || any (slotNames asking) slot || any (copyExceptionNames asking onAbility) exceptions

creatureExploitsNames :: Asking -> CreatureExploits.CreatureExploits -> Bool
creatureExploitsNames asking x = case x of
  CreatureExploits.MkCreatureExploits exploiter exploited -> filterNames asking exploiter || filterNames asking exploited

crewRestrictionNames :: Asking -> CrewRestriction.CrewRestriction -> Bool
crewRestrictionNames asking x = case x of
  CrewRestriction.MkCrewRestriction affected -> affectedNames asking affected

cyclingNames :: Asking -> (keyword -> Bool) -> Cycling.Cycling keyword -> Bool
cyclingNames asking onKeyword x = case x of
  Cycling.MkCycling cost searchFor -> costNames asking onKeyword cost || any (filterNames asking) searchFor

damagePartNames :: Asking -> DamagePart.DamagePart -> Bool
damagePartNames asking x = case x of
  DamagePart.MkDamagePart ref quantity -> objectRefNames asking ref || quantityNames asking quantity

damagePatternNames :: Asking -> DamagePattern.DamagePattern -> Bool
damagePatternNames asking x = case x of
  DamagePattern.MkDamagePattern _whichKind whatSource whatRecipient _whoRecipient whichRecipient whichSource boundRecipient -> filterNames asking whatSource || any (filterNames asking) whatRecipient || any (recipientNames asking) whichRecipient || elem (object asking) whichSource || any (slotNames asking) boundRecipient

damageRNames :: Asking -> (effect -> Bool) -> DamageR.DamageR effect -> Bool
damageRNames asking onEffect x = case x of
  DamageR.MkDamageR matching rewrite riders -> damagePatternNames asking matching || damageRewriteNames asking onEffect rewrite || any onEffect riders

damageRewriteNames :: Asking -> (effect -> Bool) -> DamageRewrite.DamageRewrite effect -> Bool
damageRewriteNames asking onEffect x = case x of
  DamageRewrite.PreventAll -> False
  DamageRewrite.PreventRemovingShieldCounter -> False
  DamageRewrite.PreventNext _natural -> False
  DamageRewrite.PreventAllBut _natural -> False
  DamageRewrite.PreventUpTo _natural -> False
  DamageRewrite.SetAmount _natural -> False
  DamageRewrite.Scale _scaling -> False
  DamageRewrite.Redirect recipient -> recipientNames asking recipient
  DamageRewrite.RedirectNext _natural recipient -> recipientNames asking recipient
  DamageRewrite.RedirectMatching filter_ -> filterNames asking filter_
  DamageRewrite.RunEffects seq_ -> any onEffect seq_

dealDamageNames :: Asking -> DealDamage.DealDamage -> Bool
dealDamageNames asking x = case x of
  DealDamage.MkDealDamage parts dealer _excess -> any (damagePartNames asking) parts || any (slotNames asking) dealer

delayedTriggerNames :: Asking -> DelayedTrigger.DelayedTrigger -> Bool
delayedTriggerNames asking x = case x of
  DelayedTrigger.MkDelayedTrigger ability source _controller bindings _window expiry _createdAt -> triggeredAbilityNames asking (const False) (grantedAbilityNames asking (const False)) ability || source == object asking || any (slotNames asking) (Map.keys bindings) || any (bindingNames asking) bindings || any (expiryNames asking) expiry

designateNames :: Asking -> Designate.Designate -> Bool
designateNames asking x = case x of
  Designate.MkDesignate _designation slot value -> slotNames asking slot || any (quantityNames asking) value

destroyNames :: Asking -> Destroy.Destroy -> Bool
destroyNames asking x = case x of
  Destroy.MkDestroy ref _regenerability slot buried permanents -> objectRefNames asking ref || any (slotNames asking) slot || any (slotNames asking) buried || any (slotNames asking) permanents

destructionRNames :: Asking -> DestructionR.DestructionR -> Bool
destructionRNames asking x = case x of
  DestructionR.MkDestructionR matching _rewrite -> any (filterNames asking) matching

devotionNames :: Asking -> Devotion.Devotion -> Bool
devotionNames asking x = case x of
  Devotion.MkDevotion player _colors -> playerRefNames asking player

devourNames :: Asking -> Devour.Devour keyword -> Bool
devourNames asking x = case x of
  Devour.MkDevour quality _count -> any (filterNames asking) quality

permanentActedNames :: Asking -> (permanent -> Bool) -> PermanentActed.PermanentActed permanent -> Bool
permanentActedNames _asking onPermanent x = case x of
  PermanentActed.MkPermanentActed _action permanent -> onPermanent permanent

actingPermanentNames :: Asking -> ActingPermanent.ActingPermanent -> Bool
actingPermanentNames asking x = case x of
  ActingPermanent.Self -> False
  ActingPermanent.Matching filter_ -> filterNames asking filter_

dieResultNames :: Asking -> (player -> Bool) -> DieResult.DieResult player -> Bool
dieResultNames _asking onPlayer x = case x of
  DieResult.MkDieResult roller _result -> onPlayer roller

dieRollRNames :: Asking -> DieRollR.DieRollR -> Bool
dieRollRNames asking x = case x of
  DieRollR.MkDieRollR whose _rewrite -> controllerRelationNames asking whose

discardNames :: Asking -> Discard.Discard -> Bool
discardNames asking x = case x of
  Discard.Counted countedDiscard -> countedDiscardNames asking countedDiscard
  Discard.These theseDiscard -> theseDiscardNames asking theseDiscard
  Discard.AnyNumber anyNumberDiscard -> anyNumberDiscardNames asking anyNumberDiscard

discardCardsNames :: Asking -> DiscardCards.DiscardCards keyword -> Bool
discardCardsNames asking x = case x of
  DiscardCards.MkDiscardCards _count whichCards -> filterNames asking whichCards

doesNotUntapNextNames :: Asking -> DoesNotUntapNext.DoesNotUntapNext -> Bool
doesNotUntapNextNames asking x = case x of
  DoesNotUntapNext.MkDoesNotUntapNext ref _steps -> objectRefNames asking ref

drawNames :: Asking -> Draw.Draw -> Bool
drawNames asking x = case x of
  Draw.MkDraw player quantity slot -> playerRefNames asking player || quantityNames asking quantity || any (slotNames asking) slot

drawCountRNames :: Asking -> DrawCountR.DrawCountR -> Bool
drawCountRNames asking x = case x of
  DrawCountR.MkDrawCountR whose _atLeast _rewrite -> controllerRelationNames asking whose

drawRNames :: Asking -> DrawR.DrawR -> Bool
drawRNames asking x = case x of
  DrawR.MkDrawR whose rewrite -> controllerRelationNames asking whose || drawRewriteNames asking rewrite

drawRewriteNames :: Asking -> DrawRewrite.DrawRewrite -> Bool
drawRewriteNames asking x = case x of
  DrawRewrite.GainLife _natural -> False
  DrawRewrite.FromOutsideTheGame fromOutsideTheGame -> fromOutsideTheGameNames asking fromOutsideTheGame
  DrawRewrite.Dredge _natural -> False
  DrawRewrite.YouDraw -> False

dungeonRoomNames :: Asking -> (card -> Bool) -> DungeonRoom.DungeonRoom card -> Bool
dungeonRoomNames asking onCard x = case x of
  DungeonRoom.MkDungeonRoom _name ability _exits -> modalNames asking onCard (grantedAbilityNames asking onCard) ability

durationNames :: Asking -> Duration.Duration -> Bool
durationNames asking x = case x of
  Duration.UntilEndOfTurn -> False
  Duration.Indefinite -> False
  Duration.Perpetual -> False
  Duration.UntilYourNextTurn -> False
  Duration.UntilYourNextUpkeep -> False
  Duration.UntilEndOfYourNextTurn -> False
  Duration.UntilEndOfNextTurnOf playerRef -> playerRefNames asking playerRef
  Duration.DuringNextTurnOf playerRef -> playerRefNames asking playerRef
  Duration.DuringYourNextTurn -> False
  Duration.DuringThatExtraTurn -> False
  Duration.ForAsLongAs condition -> conditionNames asking condition
  Duration.UntilEndOfCombat -> False
  Duration.UntilEndOfCombatOnYourNextTurn -> False
  Duration.UntilPaid cost -> costNames asking (keywordNames asking) cost
  Duration.UntilUsed -> False

eachCardFromAmongNames :: Asking -> EachCardFromAmong.EachCardFromAmong -> Bool
eachCardFromAmongNames asking x = case x of
  EachCardFromAmong.MkEachCardFromAmong slot filter_ -> slotNames asking slot || filterNames asking filter_

eachCardInGraveyardNames :: Asking -> EachCardInGraveyard.EachCardInGraveyard -> Bool
eachCardInGraveyardNames asking x = case x of
  EachCardInGraveyard.MkEachCardInGraveyard graveyards filter_ -> zoneScopeNames asking graveyards || filterNames asking filter_

eachCardInHandNames :: Asking -> EachCardInHand.EachCardInHand -> Bool
eachCardInHandNames asking x = case x of
  EachCardInHand.MkEachCardInHand hands filter_ -> zoneScopeNames asking hands || any (filterNames asking) filter_

earthbendNames :: Asking -> Earthbend.Earthbend -> Bool
earthbendNames asking x = case x of
  Earthbend.MkEarthbend quantity ref -> quantityNames asking quantity || objectRefNames asking ref

effectNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> Effect.Effect card ability -> Bool
effectNames asking onCard onAbility x = case x of
  Effect.DealDamage dealDamage -> dealDamageNames asking dealDamage
  Effect.Fight fight -> fightNames asking fight
  Effect.ModifyTarget modifyTarget -> modifyTargetNames asking onAbility modifyTarget
  Effect.ChangeText changeText -> changeTextNames asking changeText
  Effect.AddMana manaAddition -> manaAdditionNames asking manaAddition
  Effect.ActivateManaAbilities activateManaAbilities -> activateManaAbilitiesNames asking activateManaAbilities
  Effect.MoveMana moveMana -> moveManaNames asking moveMana
  Effect.Search search -> searchNames asking search
  Effect.ExileAllGraveyards -> False
  Effect.RestartGame objectRef -> any (objectRefNames asking) objectRef
  Effect.ControlPlayerNextTurn slotName -> slotNames asking slotName
  Effect.ControlPlayerThisResolution controlPlayer -> controlPlayerNames asking controlPlayer
  Effect.Destroy destroy -> destroyNames asking destroy
  Effect.Sacrifice sacrificeEffect -> sacrificeEffectNames asking sacrificeEffect
  Effect.Attach slotName -> slotNames asking slotName
  Effect.AttachAsThoughCreature slotName -> slotNames asking slotName
  Effect.AttachTarget attachTarget -> attachTargetNames asking attachTarget
  Effect.AttachTargetToEach attachTarget -> attachTargetNames asking attachTarget
  Effect.AttachBound attachBound -> attachBoundNames asking attachBound
  Effect.AttachAll attachAll -> attachAllNames asking attachAll
  Effect.Unattach objectRef -> objectRefNames asking objectRef
  Effect.MoveToZone moveToZone -> moveToZoneNames asking onAbility moveToZone
  Effect.Draw draw -> drawNames asking draw
  Effect.Mill mill -> millNames asking mill
  Effect.Reveal reveal -> revealNames asking reveal
  Effect.FromOutsideTheGame fromOutsideTheGame -> fromOutsideTheGameNames asking fromOutsideTheGame
  Effect.ExileThisSpell -> False
  Effect.LookAt lookAt -> lookAtNames asking lookAt
  Effect.ArrangeInLibrary objectRef -> objectRefNames asking objectRef
  Effect.Scry playerQuantity -> playerQuantityNames asking playerQuantity
  Effect.Surveil playerQuantity -> playerQuantityNames asking playerQuantity
  Effect.Fateseal playerQuantity -> playerQuantityNames asking playerQuantity
  Effect.Clash slotName -> slotNames asking slotName
  Effect.Explore objectRef -> objectRefNames asking objectRef
  Effect.Connive connive -> conniveNames asking connive
  Effect.Discard discard -> discardNames asking discard
  Effect.LoseLife lifeLoss -> lifeLossNames asking lifeLoss
  Effect.GainLife playerQuantity -> playerQuantityNames asking playerQuantity
  Effect.ExchangeLifeTotals exchangeSides -> exchangeSidesNames asking exchangeSides
  Effect.ExchangeValues exchangeValues -> exchangeValuesNames asking exchangeValues
  Effect.ExchangeZones exchangeZones -> exchangeZonesNames asking exchangeZones
  Effect.ExchangeWithCardInHand chosenCardInHand -> chosenCardInHandNames asking chosenCardInHand
  Effect.SetLifeTotal playerQuantity -> playerQuantityNames asking playerQuantity
  Effect.LoseGame playerRef -> playerRefNames asking playerRef
  Effect.WinGame playerRef -> playerRefNames asking playerRef
  Effect.DrawGame -> False
  Effect.RedistributeLifeTotals -> False
  Effect.IncreaseSpeed playerQuantity -> playerQuantityNames asking playerQuantity
  Effect.DecreaseSpeed speedDecrease -> speedDecreaseNames asking speedDecrease
  Effect.Create create -> createNames asking onCard onAbility create
  Effect.Conjure conjure -> conjureNames asking onCard conjure
  Effect.CreateCopy createCopy -> createCopyNames asking onAbility createCopy
  Effect.BecomeCopy becomeCopy -> becomeCopyNames asking onAbility becomeCopy
  Effect.CopyStackObject copyStackObject -> copyStackObjectNames asking onAbility copyStackObject
  Effect.Replace replace -> replaceNames asking onCard onAbility (effectNames asking onCard onAbility) replace
  Effect.SkipNextPhase skipNextPhase -> skipNextPhaseNames asking skipNextPhase
  Effect.PreventNextDamage preventNextDamage -> preventNextDamageNames asking (effectNames asking onCard onAbility) preventNextDamage
  Effect.PreventAllDamage preventAllDamage -> preventAllDamageNames asking (effectNames asking onCard onAbility) preventAllDamage
  Effect.PreventNextDamageInstance preventNextDamageInstance -> preventNextDamageInstanceNames asking (effectNames asking onCard onAbility) preventNextDamageInstance
  Effect.RedirectDamage redirectDamage -> redirectDamageNames asking redirectDamage
  Effect.Counter counter -> counterNames asking counter
  Effect.PutCounters putCounters -> putCountersNames asking putCounters
  Effect.DistributeCounters putCounters -> putCountersNames asking putCounters
  Effect.RemoveCounters removeCounters -> removeCountersNames asking removeCounters
  Effect.RemoveCountersAmong removeCountersAmong -> removeCountersAmongNames asking removeCountersAmong
  Effect.MoveCounters moveCounters -> moveCountersNames asking moveCounters
  Effect.PutCountersFrom putCountersFrom -> putCountersFromNames asking putCountersFrom
  Effect.GainPlayerCounters playerCounters -> playerCountersNames asking playerCounters
  Effect.RemovePlayerCounters removal -> removePlayerCountersNames asking removal
  Effect.PayAnyEnergy slotName -> slotNames asking slotName
  Effect.ChooseNumber chooseNumber -> chooseNumberNames asking chooseNumber
  Effect.Tap objectRef -> objectRefNames asking objectRef
  Effect.Untap objectRef -> objectRefNames asking objectRef
  Effect.Detain objectRef -> objectRefNames asking objectRef
  Effect.Goad objectRef -> objectRefNames asking objectRef
  Effect.Pair objectRef -> objectRefNames asking objectRef
  Effect.DoesNotUntapNext doesNotUntapNext -> doesNotUntapNextNames asking doesNotUntapNext
  Effect.Transform objectRef -> objectRefNames asking objectRef
  Effect.Convert objectRef -> objectRefNames asking objectRef
  Effect.Flip objectRef -> objectRefNames asking objectRef
  Effect.Meld meld -> meldNames asking onCard meld
  Effect.PhaseOut objectRef -> objectRefNames asking objectRef
  Effect.TurnFaceDown turnFaceDown -> turnFaceDownNames asking onAbility turnFaceDown
  Effect.TurnFaceUp slotName -> slotNames asking slotName
  Effect.RemoveFromCombat objectRef -> objectRefNames asking objectRef
  Effect.BecomesBlocked slotName -> slotNames asking slotName
  Effect.SwitchBlockers slotName -> slotNames asking slotName
  Effect.ExchangeBlocks exchangeBlocks -> exchangeBlocksNames asking exchangeBlocks
  Effect.AddPhases _items -> False
  Effect.EndTurn -> False
  Effect.EndCombatPhase -> False
  Effect.GainControl gainControl -> gainControlNames asking gainControl
  Effect.ExchangeControl controlSides -> controlSidesNames asking controlSides
  Effect.ArmDelayedTrigger armDelayedTrigger -> armDelayedTriggerNames asking onAbility armDelayedTrigger
  Effect.AffectPlayers affectPlayers -> affectPlayersNames asking affectPlayers
  Effect.RequireBlock requireBlock -> requireBlockNames asking requireBlock
  Effect.RequireAttack requireAttack -> requireAttackNames asking requireAttack
  Effect.Prohibit prohibit -> prohibitNames asking prohibit
  Effect.ForbidAttack forbidAttack -> forbidAttackNames asking forbidAttack
  Effect.ForbidBeingBlocked forbidBeingBlocked -> forbidBeingBlockedNames asking forbidBeingBlocked
  Effect.CreateEmblem card -> onCard card
  Effect.BecomeMonarch monarchTarget -> monarchTargetNames asking monarchTarget
  Effect.TakeTheInitiative _initiativeTarget -> False
  Effect.Designate designate -> designateNames asking designate
  Effect.SetClassLevel setClassLevel -> setClassLevelNames asking setClassLevel
  Effect.Unsuspect objectRef -> objectRefNames asking objectRef
  Effect.SetHalfLocked setHalfLocked -> setHalfLockedNames asking setHalfLocked
  Effect.CounterAndMark counterAndMark -> permanentActedNames asking (slotNames asking) counterAndMark
  Effect.BecomeProtector slotName -> slotNames asking slotName
  Effect.Mentor slotName -> slotNames asking slotName
  Effect.Firebend manaAddition -> manaAdditionNames asking manaAddition
  Effect.Exploit -> False
  Effect.GiveGift -> False
  Effect.ItBecomes _daytime -> False
  Effect.ExileHaunting exileHaunting -> exileHauntingNames asking exileHaunting
  Effect.PlaySubgame slotName -> slotNames asking slotName
  Effect.ChoosePlayer choosePlayer -> choosePlayerNames asking choosePlayer
  Effect.ChoosePlayerAtRandom choosePlayerAtRandom -> choosePlayerAtRandomNames asking choosePlayerAtRandom
  Effect.ChoosePermanents choosePermanents -> choosePermanentsNames asking choosePermanents
  Effect.RollDie rollDie -> rollDieNames asking rollDie
  Effect.Reroll -> False
  Effect.RerollStoredResults slotName -> slotNames asking slotName
  Effect.FlipCoin flipCoin -> flipCoinNames asking flipCoin
  Effect.ExileHandThenDraw -> False
  Effect.NoteManaSpent -> False
  Effect.Proliferate -> False
  Effect.ChooseCardName chooseCardName -> chooseCardNameNames asking chooseCardName
  Effect.Bolster quantity -> quantityNames asking quantity
  Effect.Amass amass -> amassNames asking amass
  Effect.Blight blight -> blightNames asking blight
  Effect.Earthbend earthbend -> earthbendNames asking earthbend
  Effect.Airbend objectRef -> objectRefNames asking objectRef
  Effect.TemptWithTheRing -> False
  Effect.OpenAttraction -> False
  Effect.Planeswalk -> False
  Effect.Abandon -> False
  Effect.ClaimPrize -> False
  Effect.Forage -> False
  Effect.Populate -> False
  Effect.Learn -> False
  Effect.TimeTravel -> False
  Effect.Recruit -> False
  Effect.Cloak playerRef -> playerRefNames asking playerRef
  Effect.ManifestDread playerRef -> playerRefNames asking playerRef
  Effect.Venture _subtype -> False
  Effect.PlayerSacrifices playerSacrifices -> playerSacrificesNames asking playerSacrifices
  Effect.Vote vote -> voteNames asking vote
  Effect.TakeExtraTurn takeExtraTurn -> takeExtraTurnNames asking takeExtraTurn
  Effect.ShuffleIntoLibrary shuffleIntoLibrary -> shuffleIntoLibraryNames asking shuffleIntoLibrary
  Effect.Ante ante -> anteNames asking ante
  Effect.PutSticker putSticker -> putStickerNames asking putSticker
  Effect.SetOwner (SetOwner.MkSetOwner player ref) -> playerRefNames asking player || objectRefNames asking ref
  Effect.ExchangeOwnership (ExchangeOwnership.MkExchangeOwnership one other) -> objectRefNames asking one || objectRefNames asking other
  Effect.ExchangeWithTopOfLibrary (ExchangeWithTopOfLibrary.MkExchangeWithTopOfLibrary ref player) -> objectRefNames asking ref || playerRefNames asking player
  Effect.Shuffle playerRef -> playerRefNames asking playerRef
  Effect.OfferCast offerCast -> offerCastNames asking offerCast
  Effect.OfferNamedCopy _cardName -> False
  Effect.OfferNotedCopy castOffer -> castOfferNames asking castOffer
  Effect.GrantPlayFromExile grantPlayFromExile -> grantPlayFromExileNames asking grantPlayFromExile
  Effect.GrantLookAtExiled grantLookAtExiled -> grantLookAtExiledNames asking grantLookAtExiled
  Effect.MakePlotted objectRef -> objectRefNames asking objectRef
  Effect.MakeForetold makeForetold -> makeForetoldNames asking makeForetold
  Effect.MakeWarped objectRef -> objectRefNames asking objectRef
  Effect.ForEach forEach -> forEachNames asking (effectNames asking onCard onAbility) forEach
  Effect.ForEachNumber forEachNumber -> forEachNumberNames asking (effectNames asking onCard onAbility) forEachNumber
  Effect.Repeat repeat_ -> repeatNames asking (effectNames asking onCard onAbility) repeat_
  Effect.RepeatIf repeatIf -> repeatIfNames asking (effectNames asking onCard onAbility) repeatIf
  Effect.Heal objectRef -> objectRefNames asking objectRef
  Effect.ChooseNewTargets objectRef -> objectRefNames asking objectRef
  Effect.ChangeTargets objectRef -> objectRefNames asking objectRef

emergeNames :: Asking -> (keyword -> Bool) -> Emerge.Emerge keyword -> Bool
emergeNames asking onKeyword x = case x of
  Emerge.MkEmerge cost quality -> costNames asking onKeyword cost || any (filterNames asking) quality

entersWithNames :: Asking -> (ability -> Bool) -> EntersWith.EntersWith ability -> Bool
entersWithNames asking onAbility x = case x of
  EntersWith.MkEntersWith counters keywords abilities -> any (withCountersNames asking) counters || any (keywordNames asking) keywords || any onAbility abilities

entryAttackNames :: Asking -> EntryAttack.EntryAttack -> Bool
entryAttackNames asking x = case x of
  EntryAttack.Chosen -> False
  EntryAttack.SameAs slotName -> slotNames asking slotName
  EntryAttack.UnderPlayer slotName -> slotNames asking slotName

entryBlockNames :: Asking -> EntryBlock.EntryBlock -> Bool
entryBlockNames asking x = case x of
  EntryBlock.Chosen -> False
  EntryBlock.Specified slotName -> slotNames asking slotName

entryFlipNames :: Asking -> EntryFlip.EntryFlip -> Bool
entryFlipNames asking x = case x of
  EntryFlip.MkEntryFlip heads tails -> entryOptionNames asking heads || entryOptionNames asking tails

entryOptionNames :: Asking -> EntryOption.EntryOption -> Bool
entryOptionNames asking x = case x of
  EntryOption.MkEntryOption _power _toughness keywords -> any (keywordNames asking) keywords

entryPriceNames :: Asking -> EntryPrice.EntryPrice -> Bool
entryPriceNames asking x = case x of
  EntryPrice.PayLife _natural -> False
  EntryPrice.Reveal filter_ -> filterNames asking filter_

entryRNames :: Asking -> (ability -> Bool) -> (effect -> Bool) -> EntryR.EntryR ability effect -> Bool
entryRNames asking onAbility onEffect x = case x of
  EntryR.MkEntryR matching rewrite -> filterNames asking matching || entryRewriteNames asking onAbility onEffect rewrite

entryRestrictionNames :: Asking -> EntryRestriction.EntryRestriction -> Bool
entryRestrictionNames asking x = case x of
  EntryRestriction.MkEntryRestriction affected _origins -> affectedNames asking affected

entryRewriteNames :: Asking -> (ability -> Bool) -> (effect -> Bool) -> EntryRewrite.EntryRewrite ability effect -> Bool
entryRewriteNames asking onAbility onEffect x = case x of
  EntryRewrite.AsCopy asCopy -> asCopyNames asking onAbility onEffect asCopy
  EntryRewrite.ChoiceOf items -> any (entryOptionNames asking) items
  EntryRewrite.ChoiceByCoinFlip entryFlip -> entryFlipNames asking entryFlip
  EntryRewrite.ChooseColors _natural -> False
  EntryRewrite.ChooseBasicLandType -> False
  EntryRewrite.ChooseCreatureType -> False
  EntryRewrite.ChoosePlayer -> False
  EntryRewrite.ChooseCardNames filter_ -> filterNames asking filter_
  EntryRewrite.ChooseCardName filter_ -> filterNames asking filter_
  EntryRewrite.WithCounters withCounters -> withCountersNames asking withCounters
  EntryRewrite.EntersWith entersWith -> entersWithNames asking onAbility entersWith
  EntryRewrite.UnderSourceControl -> False
  EntryRewrite.SacrificeAnyNumber sacrificeAnyNumber -> sacrificeAnyNumberNames asking sacrificeAnyNumber
  EntryRewrite.SacrificeToEnter sacrificeToEnter -> sacrificeToEnterNames asking sacrificeToEnter
  EntryRewrite.ExileFromGraveyard filter_ -> filterNames asking filter_
  EntryRewrite.EntersAttachedTo filter_ -> filterNames asking filter_
  EntryRewrite.Riot -> False
  EntryRewrite.ReadAhead -> False
  EntryRewrite.Unleash -> False
  EntryRewrite.Sunburst -> False
  EntryRewrite.ModularSunburst -> False
  EntryRewrite.Bloodthirst _natural -> False
  EntryRewrite.Amplify _natural -> False
  EntryRewrite.Tribute _natural -> False
  EntryRewrite.Compleated _natural -> False
  EntryRewrite.Tapped -> False
  EntryRewrite.OrTapped entryPrice -> entryPriceNames asking entryPrice
  EntryRewrite.EntersTransformed -> False
  EntryRewrite.RunEffects seq_ -> any onEffect seq_

entryRidersNames :: Asking -> (count -> Bool) -> (ability -> Bool) -> EntryRiders.EntryRiders count ability -> Bool
entryRidersNames asking onCount onAbility x = case x of
  EntryRiders.MkEntryRiders _tapped attacking blocking _transformed counters _underOwner _exiledFaceDown attachedTo faceDown _noted characteristics -> any (entryAttackNames asking) attacking || any (entryBlockNames asking) blocking || any (counterKindNames asking (keywordNames asking)) (Map.keys counters) || any onCount counters || any (slotNames asking) attachedTo || any (faceDownStateNames asking onAbility) faceDown || any (modificationNames asking onAbility) characteristics

equipNames :: Asking -> (keyword -> Bool) -> Equip.Equip keyword -> Bool
equipNames asking onKeyword x = case x of
  Equip.MkEquip cost quality -> costNames asking onKeyword cost || any (filterNames asking) quality

exchangeBlocksNames :: Asking -> ExchangeBlocks.ExchangeBlocks -> Bool
exchangeBlocksNames asking x = case x of
  ExchangeBlocks.MkExchangeBlocks first second -> slotNames asking first || slotNames asking second

exchangeSidesNames :: Asking -> ExchangeSides.ExchangeSides -> Bool
exchangeSidesNames asking x = case x of
  ExchangeSides.WithController slotName -> slotNames asking slotName
  ExchangeSides.BetweenTargets slotName -> slotNames asking slotName

exchangeValuesNames :: Asking -> ExchangeValues.ExchangeValues -> Bool
exchangeValuesNames asking x = case x of
  ExchangeValues.MkExchangeValues one other duration -> exchangedValueNames asking one || exchangedValueNames asking other || durationNames asking duration

exchangeZonesNames :: Asking -> ExchangeZones.ExchangeZones -> Bool
exchangeZonesNames asking x = case x of
  ExchangeZones.MkExchangeZones player _zones -> playerRefNames asking player

exchangedValueNames :: Asking -> ExchangedValue.ExchangedValue -> Bool
exchangedValueNames asking x = case x of
  ExchangedValue.LifeTotal playerRef -> playerRefNames asking playerRef
  ExchangedValue.Power objectRef -> objectRefNames asking objectRef
  ExchangedValue.Toughness objectRef -> objectRefNames asking objectRef

exileCardsFromGraveyardNames :: Asking -> ExileCardsFromGraveyard.ExileCardsFromGraveyard keyword -> Bool
exileCardsFromGraveyardNames asking x = case x of
  ExileCardsFromGraveyard.MkExileCardsFromGraveyard _count whichCards -> filterNames asking whichCards

exileHauntingNames :: Asking -> ExileHaunting.ExileHaunting -> Bool
exileHauntingNames asking x = case x of
  ExileHaunting.MkExileHaunting card host -> slotNames asking card || slotNames asking host

exileMaterialsNames :: Asking -> ExileMaterials.ExileMaterials keyword -> Bool
exileMaterialsNames asking x = case x of
  ExileMaterials.MkExileMaterials _count _orMore whichObjects -> filterNames asking whichObjects

expiryNames :: Asking -> Expiry.Expiry -> Bool
expiryNames asking x = case x of
  Expiry.AtCleanup -> False
  Expiry.Never -> False
  Expiry.Perpetual -> False
  Expiry.While while -> whileNames asking while
  Expiry.AtTurnOf _playerId -> False
  Expiry.AtUpkeepOf _playerId -> False
  Expiry.AtEndOfTurnOf _afterTurn -> False
  Expiry.DuringTurnOf _afterTurn -> False
  Expiry.DuringTurnOfControllerOf afterObjectTurn -> AfterObjectTurn.object afterObjectTurn == object asking
  Expiry.DuringExtraTurn _timestamp -> False
  Expiry.AtEndOf _phaseSelector -> False
  Expiry.AtEndOfCombatOn _afterTurn -> False
  Expiry.WhenPaid paidExpiry -> paidExpiryNames asking paidExpiry
  Expiry.WhenUsed -> False

faceNames :: Asking -> (card -> Bool) -> Face.Face card -> Bool
faceNames asking onCard x = case x of
  Face.MkFace _name _oracleText _manaCost _typeLine power toughness _loyalty _defense _startingIntensity _vanguard _canBeYourCommander _claimsStartingPlayer _anteOnly keywords _colorIndicator characteristicPT staticAbilities spell activatedAbilities replacementEffects triggeredAbilities delayedAbilities rooms _dungeonEntryQuality _castingPermissions _castingRestrictions enchant _counterability additionalCosts additionalCostChoices modeCosts maximumX _minimumX alternativeCosts costReductions playerAbilities blockRequirements blockPermissions attackRequirements combatRestrictions sacrificeRestrictions untapRestrictions crewRestrictions attackPermissions attachRestrictions entryRestrictions counterRestrictions activationProhibitions attackCosts blockCosts mulliganActions openingHandActions specialActions -> any (powerNames asking) power || any (toughnessNames asking) toughness || any (keywordNames asking) (Map.keys keywords) || any (characteristicPTNames asking) characteristicPT || any (staticAbilityNames asking (grantedAbilityNames asking onCard)) staticAbilities || modalNames asking onCard (grantedAbilityNames asking onCard) spell || any (activatedAbilityNames asking onCard (grantedAbilityNames asking onCard)) activatedAbilities || any (printedReplacementNames asking onCard (grantedAbilityNames asking onCard) (effectNames asking onCard (grantedAbilityNames asking onCard))) replacementEffects || any (triggeredAbilityNames asking onCard (grantedAbilityNames asking onCard)) triggeredAbilities || any (triggeredAbilityNames asking onCard (grantedAbilityNames asking onCard)) delayedAbilities || any (dungeonRoomNames asking onCard) rooms || any (targetSlotNames asking) enchant || any (costComponentNames asking (keywordNames asking)) additionalCosts || any (costChoiceNames asking) additionalCostChoices || any (costNames asking (keywordNames asking)) modeCosts || any (quantityNames asking) maximumX || any (alternativeCostNames asking) alternativeCosts || any (costReductionNames asking) costReductions || any (playerStaticAbilityNames asking) playerAbilities || any (blockRequirementNames asking) blockRequirements || any (blockPermissionNames asking) blockPermissions || any (attackRequirementNames asking) attackRequirements || any (combatRestrictionNames asking) combatRestrictions || any (sacrificeRestrictionNames asking) sacrificeRestrictions || any (untapRestrictionNames asking) untapRestrictions || any (crewRestrictionNames asking) crewRestrictions || any (attackPermissionNames asking) attackPermissions || any (attachRestrictionNames asking) attachRestrictions || any (entryRestrictionNames asking) entryRestrictions || any (counterRestrictionNames asking) counterRestrictions || any (activationProhibitionNames asking) activationProhibitions || any (attackCostNames asking) attackCosts || any (blockCostNames asking) blockCosts || any (handActionNames asking onCard) mulliganActions || any (handActionNames asking onCard) openingHandActions || any (specialActionNames asking) specialActions

faceDownCharacteristicsNames :: Asking -> (ability -> Bool) -> FaceDownCharacteristics.FaceDownCharacteristics ability -> Bool
faceDownCharacteristicsNames asking onAbility x = case x of
  FaceDownCharacteristics.MkFaceDownCharacteristics _typeLine power toughness keywords abilities -> any (powerNames asking) power || any (toughnessNames asking) toughness || any (keywordNames asking) keywords || any onAbility abilities

faceDownStateNames :: Asking -> (ability -> Bool) -> FaceDownState.FaceDownState ability -> Bool
faceDownStateNames asking onAbility x = case x of
  FaceDownState.MkFaceDownState _reason listed -> faceDownCharacteristicsNames asking onAbility listed

fightNames :: Asking -> Fight.Fight -> Bool
fightNames asking x = case x of
  Fight.MkFight first second -> slotNames asking first || slotNames asking second

flipCoinNames :: Asking -> FlipCoin.FlipCoin -> Bool
flipCoinNames asking x = case x of
  FlipCoin.MkFlipCoin count _reading slot misses -> quantityNames asking count || slotNames asking slot || any (slotNames asking) misses

floatingCandidateNames :: Asking -> FloatingCandidate.FloatingCandidate -> Bool
floatingCandidateNames asking x = case x of
  FloatingCandidate.MkFloatingCandidate source _timestamp -> source == object asking

forEachNames :: Asking -> (effect -> Bool) -> ForEach.ForEach effect -> Bool
forEachNames asking onEffect x = case x of
  ForEach.MkForEach ref _members slot body _individually payGate -> objectRefNames asking ref || slotNames asking slot || any onEffect body || any (payGateNames asking) payGate

forEachNumberNames :: Asking -> (effect -> Bool) -> ForEachNumber.ForEachNumber effect -> Bool
forEachNumberNames asking onEffect x = case x of
  ForEachNumber.MkForEachNumber upTo slot body -> quantityNames asking upTo || slotNames asking slot || any onEffect body

forbidAttackNames :: Asking -> ForbidAttack.ForbidAttack -> Bool
forbidAttackNames asking x = case x of
  ForbidAttack.MkForbidAttack duration affected aimedAt -> durationNames asking duration || restrictedCreaturesNames asking (objectRefNames asking) affected || any (aimedAtNames asking) aimedAt

forbidBeingBlockedNames :: Asking -> ForbidBeingBlocked.ForbidBeingBlocked -> Bool
forbidBeingBlockedNames asking x = case x of
  ForbidBeingBlocked.MkForbidBeingBlocked duration affected -> durationNames asking duration || filterNames asking affected

foretellCostNames :: Asking -> (keyword -> Bool) -> ForetellCost.ForetellCost keyword -> Bool
foretellCostNames asking onKeyword x = case x of
  ForetellCost.Stated cost -> costNames asking onKeyword cost
  ForetellCost.ManaCostReducedBy _manaCost -> False

fromOutsideTheGameNames :: Asking -> FromOutsideTheGame.FromOutsideTheGame -> Bool
fromOutsideTheGameNames asking x = case x of
  FromOutsideTheGame.MkFromOutsideTheGame _count _upTo _destination filter_ _reveal -> filterNames asking filter_

fromReferenceNames :: Asking -> FromReference.FromReference -> Bool
fromReferenceNames asking x = case x of
  FromReference.MkFromReference filter_ amount -> filterNames asking filter_ || any (quantityNames asking) amount

fullTextNames :: Asking -> (ability -> Bool) -> FullText.FullText ability -> Bool
fullTextNames asking onAbility x = case x of
  FullText.MkFullText graveyard alsoHas -> playerRefNames asking graveyard || any onAbility alsoHas

gainControlNames :: Asking -> GainControl.GainControl -> Bool
gainControlNames asking x = case x of
  GainControl.MkGainControl duration ref to -> durationNames asking duration || objectRefNames asking ref || playerRefNames asking to

grantLookAtExiledNames :: Asking -> GrantLookAtExiled.GrantLookAtExiled -> Bool
grantLookAtExiledNames asking x = case x of
  GrantLookAtExiled.MkGrantLookAtExiled cards _followsExiler -> objectRefNames asking cards

grantPlayFromExileNames :: Asking -> GrantPlayFromExile.GrantPlayFromExile -> Bool
grantPlayFromExileNames asking x = case x of
  GrantPlayFromExile.MkGrantPlayFromExile duration player ref _spending _alternativeCost condition _verb _increase _landEnters -> durationNames asking duration || playerRefNames asking player || objectRefNames asking ref || any (conditionNames asking) condition

-- A granted ability's slots are its OWN: each read resolves against the
-- bindings of the stack object it becomes or of the object it is granted to,
-- which Pawl.Engine.Interchangeable.namedByAnother searches and whole-object
-- Eq compares. So inside one, every slot read is Searched, whatever the row
-- granting it stores.
grantedAbilityNames :: Asking -> (card -> Bool) -> GrantedAbility.GrantedAbility card -> Bool
grantedAbilityNames asking onCard x =
  let own = MkAsking (object asking) Searched
   in case x of
        GrantedAbility.Activated activatedAbility -> activatedAbilityNames own onCard (grantedAbilityNames own onCard) activatedAbility
        GrantedAbility.Triggered triggeredAbility -> triggeredAbilityNames own onCard (grantedAbilityNames own onCard) triggeredAbility
        GrantedAbility.Static staticAbility -> staticAbilityNames own (grantedAbilityNames own onCard) staticAbility
        GrantedAbility.Rules ruleAbilities -> ruleAbilitiesNames own ruleAbilities
        GrantedAbility.Replacement printedReplacement -> printedReplacementNames own onCard (grantedAbilityNames own onCard) (effectNames own onCard (grantedAbilityNames own onCard)) printedReplacement
        GrantedAbility.Player playerStaticAbility -> playerStaticAbilityNames own playerStaticAbility
        GrantedAbility.SelfCostReduction costReduction -> costReductionNames own costReduction
        GrantedAbility.SelfAlternativeCost alternativeCost -> alternativeCostNames own alternativeCost
        GrantedAbility.SelfSpendManaAsThough _spendManaAsThough -> False

halvedNames :: Asking -> (quantity -> Bool) -> Halved.Halved quantity -> Bool
halvedNames _asking onQuantity x = case x of
  Halved.MkHalved _rounding quantity -> onQuantity quantity

handActionNames :: Asking -> (card -> Bool) -> HandAction.HandAction card -> Bool
handActionNames asking onCard x = case x of
  HandAction.MkHandAction condition effects -> any (conditionNames asking) condition || any (effectNames asking onCard (grantedAbilityNames asking onCard)) effects

impendingNames :: Asking -> (keyword -> Bool) -> Impending.Impending keyword -> Bool
impendingNames asking onKeyword x = case x of
  Impending.MkImpending _counters cost -> costNames asking onKeyword cost

inZoneNames :: Asking -> InZone.InZone -> Bool
inZoneNames asking x = case x of
  InZone.MkInZone _zone player -> playerRefNames asking player

keywordNames :: Asking -> Keyword.Keyword -> Bool
keywordNames asking x = case x of
  Keyword.Deathtouch -> False
  Keyword.Defender -> False
  Keyword.DoubleStrike -> False
  Keyword.Equip equip -> equipNames asking (keywordNames asking) equip
  Keyword.EquipPlaneswalker cost -> costNames asking (keywordNames asking) cost
  Keyword.FirstStrike -> False
  Keyword.Flash -> False
  Keyword.Flying -> False
  Keyword.Haste -> False
  Keyword.Hexproof filter_ -> any (filterNames asking) filter_
  Keyword.Indestructible -> False
  Keyword.Intimidate -> False
  Keyword.Landwalk filter_ -> filterNames asking filter_
  Keyword.Lifelink -> False
  Keyword.LivingMetal -> False
  Keyword.MoreThanMeetsTheEye cost -> costNames asking (keywordNames asking) cost
  Keyword.Protection protection -> protectionNames asking protection
  Keyword.Reach -> False
  Keyword.Shroud -> False
  Keyword.Trample -> False
  Keyword.TrampleOverPlaneswalkers -> False
  Keyword.Vigilance -> False
  Keyword.Ward ward -> wardNames asking (keywordNames asking) ward
  Keyword.Banding -> False
  Keyword.Rampage _natural -> False
  Keyword.CumulativeUpkeep cost -> costNames asking (keywordNames asking) cost
  Keyword.Flanking -> False
  Keyword.Phasing -> False
  Keyword.Buyback cost -> costNames asking (keywordNames asking) cost
  Keyword.Shadow -> False
  Keyword.Cycling cycling -> cyclingNames asking (keywordNames asking) cycling
  Keyword.Echo cost -> costNames asking (keywordNames asking) cost
  Keyword.Horsemanship -> False
  Keyword.Fading _natural -> False
  Keyword.Kicker cost -> costNames asking (keywordNames asking) cost
  Keyword.Multikicker cost -> costNames asking (keywordNames asking) cost
  Keyword.StickerKicker cost -> costNames asking (keywordNames asking) cost
  Keyword.Flashback cost -> costNames asking (keywordNames asking) cost
  Keyword.Fear -> False
  Keyword.Morph morph -> morphNames asking (keywordNames asking) morph
  Keyword.Amplify _natural -> False
  Keyword.Provoke -> False
  Keyword.Storm -> False
  Keyword.Affinity filter_ -> filterNames asking filter_
  Keyword.Entwine cost -> costNames asking (keywordNames asking) cost
  Keyword.Modular _natural -> False
  Keyword.Sunburst -> False
  Keyword.Bushido _natural -> False
  Keyword.Soulshift _natural -> False
  Keyword.Splice splice -> spliceNames asking (keywordNames asking) splice
  Keyword.Offering filter_ -> filterNames asking filter_
  Keyword.Ninjutsu cost -> costNames asking (keywordNames asking) cost
  Keyword.Epic -> False
  Keyword.Paradigm -> False
  Keyword.Convoke -> False
  Keyword.Dredge _natural -> False
  Keyword.Bloodthirst _natural -> False
  Keyword.Haunt -> False
  Keyword.Replicate cost -> costNames asking (keywordNames asking) cost
  Keyword.Graft _natural -> False
  Keyword.Recover cost -> costNames asking (keywordNames asking) cost
  Keyword.Ripple _natural -> False
  Keyword.SplitSecond -> False
  Keyword.Suspend suspend -> any (suspendNames asking (keywordNames asking)) suspend
  Keyword.Vanishing _natural -> False
  Keyword.Absorb _natural -> False
  Keyword.AuraSwap cost -> costNames asking (keywordNames asking) cost
  Keyword.Delve -> False
  Keyword.Fortify cost -> costNames asking (keywordNames asking) cost
  Keyword.Frenzy _natural -> False
  Keyword.Gravestorm -> False
  Keyword.Poisonous _natural -> False
  Keyword.Champion filter_ -> filterNames asking filter_
  Keyword.Changeling -> False
  Keyword.Evoke cost -> costNames asking (keywordNames asking) cost
  Keyword.Hideaway _natural -> False
  Keyword.Prowl cost -> costNames asking (keywordNames asking) cost
  Keyword.Reinforce reinforce -> reinforceNames asking (keywordNames asking) reinforce
  Keyword.Conspire -> False
  Keyword.Persist -> False
  Keyword.Wither -> False
  Keyword.Devour devour -> devourNames asking devour
  Keyword.Exalted -> False
  Keyword.Unearth cost -> costNames asking (keywordNames asking) cost
  Keyword.Cascade -> False
  Keyword.Annihilator _natural -> False
  Keyword.LevelUp cost -> costNames asking (keywordNames asking) cost
  Keyword.Infect -> False
  Keyword.BattleCry -> False
  Keyword.LivingWeapon -> False
  Keyword.Undying -> False
  Keyword.Miracle cost -> costNames asking (keywordNames asking) cost
  Keyword.Soulbond -> False
  Keyword.Overload cost -> costNames asking (keywordNames asking) cost
  Keyword.Unleash -> False
  Keyword.Cipher -> False
  Keyword.Evolve -> False
  Keyword.Extort -> False
  Keyword.Fuse -> False
  Keyword.Tribute _natural -> False
  Keyword.Dethrone -> False
  Keyword.Outlast cost -> costNames asking (keywordNames asking) cost
  Keyword.Prowess -> False
  Keyword.Dash cost -> costNames asking (keywordNames asking) cost
  Keyword.Exploit -> False
  Keyword.Menace -> False
  Keyword.Renown _natural -> False
  Keyword.Awaken cost -> costNames asking (keywordNames asking) cost
  Keyword.Devoid -> False
  Keyword.Ingest -> False
  Keyword.Myriad -> False
  Keyword.Surge cost -> costNames asking (keywordNames asking) cost
  Keyword.Skulk -> False
  Keyword.Emerge emerge -> emergeNames asking (keywordNames asking) emerge
  Keyword.Escalate cost -> costNames asking (keywordNames asking) cost
  Keyword.Melee -> False
  Keyword.Crew _natural -> False
  Keyword.Fabricate _natural -> False
  Keyword.Partner -> False
  Keyword.PartnerText _partnerText -> False
  Keyword.PartnerWith _cardName -> False
  Keyword.ChooseABackground -> False
  Keyword.DoctorsCompanion -> False
  Keyword.Undaunted -> False
  Keyword.Improvise -> False
  Keyword.Aftermath -> False
  Keyword.Embalm cost -> costNames asking (keywordNames asking) cost
  Keyword.Eternalize cost -> costNames asking (keywordNames asking) cost
  Keyword.Afflict _natural -> False
  Keyword.Ascend -> False
  Keyword.Assist -> False
  Keyword.JumpStart -> False
  Keyword.Mentor -> False
  Keyword.Afterlife _natural -> False
  Keyword.Riot -> False
  Keyword.Spectacle cost -> costNames asking (keywordNames asking) cost
  Keyword.Escape cost -> costNames asking (keywordNames asking) cost
  Keyword.Companion filter_ -> filterNames asking filter_
  Keyword.Foretell foretellCost -> foretellCostNames asking (keywordNames asking) foretellCost
  Keyword.Demonstrate -> False
  Keyword.Daybound -> False
  Keyword.Nightbound -> False
  Keyword.Decayed -> False
  Keyword.Cleave cost -> costNames asking (keywordNames asking) cost
  Keyword.Training -> False
  Keyword.Compleated -> False
  Keyword.Reconfigure cost -> costNames asking (keywordNames asking) cost
  Keyword.Blitz cost -> costNames asking (keywordNames asking) cost
  Keyword.Casualty _natural -> False
  Keyword.ReadAhead -> False
  Keyword.Ravenous -> False
  Keyword.Squad cost -> costNames asking (keywordNames asking) cost
  Keyword.Prototype _prototype -> False
  Keyword.ForMirrodin -> False
  Keyword.Toxic _natural -> False
  Keyword.Backup backup -> backupNames asking (keywordNames asking) backup
  Keyword.Bargain -> False
  Keyword.Craft craft -> craftNames asking (keywordNames asking) craft
  Keyword.Disguise cost -> costNames asking (keywordNames asking) cost
  Keyword.Plot cost -> costNames asking (keywordNames asking) cost
  Keyword.Saddle _natural -> False
  Keyword.Spree -> False
  Keyword.Offspring cost -> costNames asking (keywordNames asking) cost
  Keyword.Gift _gift -> False
  Keyword.Freerunning cost -> costNames asking (keywordNames asking) cost
  Keyword.Impending impending -> impendingNames asking (keywordNames asking) impending
  Keyword.Exhaust -> False
  Keyword.Boast -> False
  Keyword.PowerUp -> False
  Keyword.ClassLevel _classLevel -> False
  Keyword.Forecast -> False
  Keyword.StartYourEngines -> False
  Keyword.JobSelect -> False
  Keyword.Tiered -> False
  Keyword.Exert -> False
  Keyword.Enlist -> False
  Keyword.Bestow cost -> costNames asking (keywordNames asking) cost
  Keyword.Station -> False
  Keyword.Mutate cost -> costNames asking (keywordNames asking) cost
  Keyword.UmbraArmor -> False
  Keyword.Retrace -> False
  Keyword.Mayhem cost -> any (costNames asking (keywordNames asking)) cost
  Keyword.Madness madnessCost -> madnessCostNames asking (keywordNames asking) madnessCost
  Keyword.Rebound -> False
  Keyword.Scavenge cost -> costNames asking (keywordNames asking) cost
  Keyword.Encore cost -> costNames asking (keywordNames asking) cost
  Keyword.Teamwork _natural -> False
  Keyword.WebSlinging cost -> costNames asking (keywordNames asking) cost
  Keyword.Sneak cost -> costNames asking (keywordNames asking) cost
  Keyword.Increment -> False
  Keyword.Storied -> False
  Keyword.Mobilize keywordCount -> keywordCountNames asking keywordCount
  Keyword.Firebending keywordCount -> keywordCountNames asking keywordCount
  Keyword.Transmute cost -> costNames asking (keywordNames asking) cost
  Keyword.Transfigure cost -> costNames asking (keywordNames asking) cost
  Keyword.Warp cost -> costNames asking (keywordNames asking) cost
  Keyword.Disturb cost -> costNames asking (keywordNames asking) cost
  Keyword.Harmonize cost -> costNames asking (keywordNames asking) cost

keywordCountNames :: Asking -> KeywordCount.KeywordCount keyword -> Bool
keywordCountNames asking x = case x of
  KeywordCount.Fixed _natural -> False
  KeywordCount.Power -> False
  KeywordCount.Tally keywordTally -> keywordTallyNames asking keywordTally
  KeywordCount.PlayerCounters playerCounterTally -> playerCounterTallyNames asking playerCounterTally

keywordDesignatorNames :: Asking -> KeywordDesignator.KeywordDesignator Keyword.Keyword -> Bool
keywordDesignatorNames asking x = case x of
  KeywordDesignator.OfFamily _keywordFamily -> False
  KeywordDesignator.OfKeyword keyword -> keywordNames asking keyword
  KeywordDesignator.PrintedKicker -> False

keywordTallyNames :: Asking -> KeywordTally.KeywordTally keyword -> Bool
keywordTallyNames asking x = case x of
  KeywordTally.MkKeywordTally scope filter_ -> scopeNames asking scope || filterNames asking filter_

libraryPlacementNames :: Asking -> LibraryPlacement.LibraryPlacement -> Bool
libraryPlacementNames asking x = case x of
  LibraryPlacement.Stated _libraryPosition -> False
  LibraryPlacement.OwnerChooses -> False
  LibraryPlacement.RandomOrder _libraryPosition -> False
  LibraryPlacement.Beneath quantity -> quantityNames asking quantity
  LibraryPlacement.BeneathOrBottom quantity -> quantityNames asking quantity

lifeGainRNames :: Asking -> LifeGainR.LifeGainR -> Bool
lifeGainRNames asking x = case x of
  LifeGainR.MkLifeGainR whose _rewrite -> controllerRelationNames asking whose

lifeLossNames :: Asking -> LifeLoss.LifeLoss -> Bool
lifeLossNames asking x = case x of
  LifeLoss.MkLifeLoss player quantity _cause tally -> playerRefNames asking player || quantityNames asking quantity || any (slotNames asking) tally

lifeLossPatternNames :: Asking -> LifeLossPattern.LifeLossPattern -> Bool
lifeLossPatternNames asking x = case x of
  LifeLossPattern.MkLifeLossPattern whose _whichCause -> controllerRelationNames asking whose

lifeLossRNames :: Asking -> LifeLossR.LifeLossR -> Bool
lifeLossRNames asking x = case x of
  LifeLossR.MkLifeLossR matching _rewrite -> lifeLossPatternNames asking matching

limitUnlessNames :: Asking -> LimitUnless.LimitUnless -> Bool
limitUnlessNames asking x = case x of
  LimitUnless.MkLimitUnless _limit unless -> any (conditionNames asking) unless

lookAtNames :: Asking -> LookAt.LookAt -> Bool
lookAtNames asking x = case x of
  LookAt.MkLookAt ref slot -> objectRefNames asking ref || slotNames asking slot

madnessCostNames :: Asking -> (keyword -> Bool) -> MadnessCost.MadnessCost keyword -> Bool
madnessCostNames asking onKeyword x = case x of
  MadnessCost.Stated cost -> costNames asking onKeyword cost
  MadnessCost.OwnManaCost -> False

makeForetoldNames :: Asking -> MakeForetold.MakeForetold -> Bool
makeForetoldNames asking x = case x of
  MakeForetold.MkMakeForetold cards _manaCostReducedBy -> objectRefNames asking cards

manaAdditionNames :: Asking -> ManaAddition.ManaAddition -> Bool
manaAdditionNames asking x = case x of
  ManaAddition.MkManaAddition player _production count _retention restriction rider whenSpent -> playerRefNames asking player || quantityNames asking count || any (manaRestrictionNames asking) restriction || any (manaRiderNames asking) rider || any (whenSpentNames asking) whenSpent

manaCountNames :: Asking -> ManaCount.ManaCount -> Bool
manaCountNames asking x = case x of
  ManaCount.MkManaCount player _filter_ -> playerRefNames asking player

manaRestrictionNames :: Asking -> ManaRestriction.ManaRestriction -> Bool
manaRestrictionNames asking x = case x of
  ManaRestriction.MkManaRestriction casts activations keywordActivations unlocks turnsFaceUp _prohibits -> any (filterNames asking) casts || any (filterNames asking) activations || any (keywordDesignatorNames asking) keywordActivations || any (filterNames asking) unlocks || any (filterNames asking) turnsFaceUp

manaRiderNames :: Asking -> ManaRider.ManaRider -> Bool
manaRiderNames asking x = case x of
  ManaRider.MkManaRider condition _effect -> filterNames asking condition

measuresNames :: Asking -> Measures.Measures -> Bool
measuresNames asking x = case x of
  Measures.MkMeasures _measure _comparison operand -> operandNames asking operand

meldNames :: Asking -> (card -> Bool) -> Meld.Meld card -> Bool
meldNames asking onCard x = case x of
  Meld.MkMeld objects result -> objectRefNames asking objects || onCard result

millNames :: Asking -> Mill.Mill -> Bool
millNames asking x = case x of
  Mill.MkMill player quantity tally slot -> playerRefNames asking player || quantityNames asking quantity || any (millTallyNames asking) tally || any (slotNames asking) slot

millCountRNames :: Asking -> MillCountR.MillCountR -> Bool
millCountRNames asking x = case x of
  MillCountR.MkMillCountR whose _rewrite -> controllerRelationNames asking whose

millTallyNames :: Asking -> MillTally.MillTally -> Bool
millTallyNames asking x = case x of
  MillTally.MkMillTally slot filter_ -> slotNames asking slot || filterNames asking filter_

modalNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> Modal.Modal card ability -> Bool
modalNames asking onCard onAbility x = case x of
  Modal.MkModal modes _selection -> any (modeNames asking onCard onAbility) modes

modeNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> Mode.Mode card ability -> Bool
modeNames asking onCard onAbility x = case x of
  Mode.MkMode clauses targetSlots -> any (clauseNames asking onCard onAbility) clauses || any (slotNames asking) (Map.keys targetSlots) || any (targetSlotNames asking) targetSlots

modificationNames :: Asking -> (ability -> Bool) -> Modification.Modification ability -> Bool
modificationNames asking onAbility x = case x of
  Modification.GainKeyword keyword -> keywordNames asking keyword
  Modification.GainKeywordAtManaCost _costKeyword -> False
  Modification.GainEnchant targetSlot -> targetSlotNames asking targetSlot
  Modification.LoseEnchant targetSlot -> targetSlotNames asking targetSlot
  Modification.GainCastingPermission _castingPermission -> False
  Modification.GainAbility ability -> onAbility ability
  Modification.GainAbilitiesOfSource keyword -> any (keywordNames asking) keyword
  Modification.GainCraftMaterialAbilities items -> any (activationRestrictionNames asking) items
  Modification.GainAbilitiesOfStickers -> False
  Modification.LoseAllAbilities -> False
  Modification.LoseNamedAbility _abilityName -> False
  Modification.LoseKeyword keyword -> keywordNames asking keyword
  Modification.LoseKeywordFamily _keywordFamily -> False
  Modification.SetBasePowerToughness setBasePowerToughness -> setBasePowerToughnessNames asking setBasePowerToughness
  Modification.ModifyPowerToughness modifyPowerToughness -> modifyPowerToughnessNames asking modifyPowerToughness
  Modification.SetLandSubtype _subtype -> False
  Modification.SetLandSubtypeToChosen -> False
  Modification.AddLandSubtype _subtype -> False
  Modification.SetCreatureSubtype _subtype -> False
  Modification.AddCreatureSubtype _subtype -> False
  Modification.AddEveryCreatureSubtype -> False
  Modification.LoseEveryCreatureSubtype -> False
  Modification.SetCreatureSubtypesOfLastCardExiledWith filter_ -> filterNames asking filter_
  Modification.AddSubtype _subtype -> False
  Modification.AddCardType _cardType -> False
  Modification.SetCardType _cardType -> False
  Modification.LoseCardType _cardType -> False
  Modification.AddSupertype _supertype -> False
  Modification.RemoveSupertype _supertype -> False
  Modification.ChangeSubtypeWord _changeSubtypeWord -> False
  Modification.ExchangeTextBoxes -> False
  Modification.AddNamesMatching filter_ -> filterNames asking filter_
  Modification.SetName _cardName -> False
  Modification.InsertNameWords _nameInsertion -> False
  Modification.HasFullText fullText -> fullTextNames asking onAbility fullText
  Modification.SetController _playerId -> False
  Modification.SetControllerToSource -> False
  Modification.SetColor _color -> False
  Modification.AddColor _color -> False
  Modification.AddChosenColor -> False
  Modification.SwitchPowerToughness -> False
  Modification.AssignCombatDamageWithToughness -> False
  Modification.GrantsStationToughness -> False
  Modification.Intensify quantity -> quantityNames asking quantity

modifiedRollNames :: Asking -> ModifiedRoll.ModifiedRoll -> Bool
modifiedRollNames asking x = case x of
  ModifiedRoll.MkModifiedRoll _sides _natural _modifier cost _limit -> any (costNames asking (keywordNames asking)) cost

modifyPowerToughnessNames :: Asking -> ModifyPowerToughness.ModifyPowerToughness -> Bool
modifyPowerToughnessNames asking x = case x of
  ModifyPowerToughness.MkModifyPowerToughness power toughness -> quantityNames asking power || quantityNames asking toughness

modifyTargetNames :: Asking -> (ability -> Bool) -> ModifyTarget.ModifyTarget ability -> Bool
modifyTargetNames asking onAbility x = case x of
  ModifyTarget.MkModifyTarget duration modification ref each -> durationNames asking duration || modificationNames asking onAbility modification || objectRefNames asking ref || any (slotNames asking) each

monarchTargetNames :: Asking -> MonarchTarget.MonarchTarget -> Bool
monarchTargetNames asking x = case x of
  MonarchTarget.TheController -> False
  MonarchTarget.ControllerOfSource -> False
  MonarchTarget.InSlot slotName -> slotNames asking slotName

morphNames :: Asking -> (keyword -> Bool) -> Morph.Morph keyword -> Bool
morphNames asking onKeyword x = case x of
  Morph.MkMorph cost _variant -> costNames asking onKeyword cost

moveCountersNames :: Asking -> MoveCounters.MoveCounters -> Bool
moveCountersNames asking x = case x of
  MoveCounters.MkMoveCounters from kinds slot to -> objectRefNames asking from || movedKindsNames asking kinds || any (slotNames asking) slot || objectRefNames asking to

moveManaNames :: Asking -> MoveMana.MoveMana -> Bool
moveManaNames asking x = case x of
  MoveMana.MkMoveMana from to -> playerRefNames asking from || playerRefNames asking to

moveToZoneNames :: Asking -> (ability -> Bool) -> MoveToZone.MoveToZone ability -> Bool
moveToZoneNames asking onAbility x = case x of
  MoveToZone.MkMoveToZone ref _zone riders slot _origin placement _duration -> objectRefNames asking ref || entryRidersNames asking (quantityNames asking) onAbility riders || any (slotNames asking) slot || libraryPlacementNames asking placement

movedKindsNames :: Asking -> MovedKinds.MovedKinds -> Bool
movedKindsNames asking x = case x of
  MovedKinds.Every -> False
  MovedKinds.Named counterKind quantity -> counterKindNames asking (keywordNames asking) counterKind || quantityNames asking quantity
  MovedKinds.EveryOfKind counterKind -> counterKindNames asking (keywordNames asking) counterKind
  MovedKinds.Chosen quantity -> quantityNames asking quantity
  MovedKinds.AnyNumber -> False
  MovedKinds.AtLeastOne -> False
  MovedKinds.AnyNumberOfKind counterKind -> counterKindNames asking (keywordNames asking) counterKind
  MovedKinds.EachAbsentKind -> False
  MovedKinds.UpToOneChosen -> False

objectRefNames :: Asking -> ObjectRef.ObjectRef -> Bool
objectRefNames asking x = case x of
  ObjectRef.InSlot slotName -> slotNames asking slotName
  ObjectRef.EachMatching filter_ -> filterNames asking filter_
  ObjectRef.EachCardInGraveyard eachCardInGraveyard -> eachCardInGraveyardNames asking eachCardInGraveyard
  ObjectRef.EachCardInYourHand -> False
  ObjectRef.EachCardInHand eachCardInHand -> eachCardInHandNames asking eachCardInHand
  ObjectRef.EachCardInYourLibrary filter_ -> any (filterNames asking) filter_
  ObjectRef.EachCardYouOwn filter_ -> filterNames asking filter_
  ObjectRef.EachCardExiledWithSource filter_ -> any (filterNames asking) filter_
  ObjectRef.EachCardExiledWithAbility _abilityName -> False
  ObjectRef.EachCardEncodedOnSource filter_ -> any (filterNames asking) filter_
  ObjectRef.EachSpell filter_ -> filterNames asking filter_
  ObjectRef.EachAbility filter_ -> filterNames asking filter_
  ObjectRef.EachOnStack filter_ -> filterNames asking filter_
  ObjectRef.EachPlayer -> False
  ObjectRef.EachOpponent -> False
  ObjectRef.ChosenPlayer -> False
  ObjectRef.Players playerRef -> playerRefNames asking playerRef
  ObjectRef.TopOfLibrary topOfLibrary -> topOfLibraryNames asking topOfLibrary
  ObjectRef.TopOfLibraryUntil topOfLibraryUntil -> topOfLibraryUntilNames asking topOfLibraryUntil
  ObjectRef.TopOfGraveyard playerRef -> playerRefNames asking playerRef
  ObjectRef.ChosenCardInGraveyard chosenCardInGraveyard -> chosenCardInGraveyardNames asking chosenCardInGraveyard
  ObjectRef.ChosenCardInHand chosenCardInHand -> chosenCardInHandNames asking chosenCardInHand
  ObjectRef.ChosenCardFromAmong chosenCardFromAmong -> chosenCardFromAmongNames asking chosenCardFromAmong
  ObjectRef.EachCardFromAmong eachCardFromAmong -> eachCardFromAmongNames asking eachCardFromAmong
  ObjectRef.RandomCardInHand randomCardInHand -> randomCardInHandNames asking randomCardInHand
  ObjectRef.RandomCardInGraveyard randomCardInGraveyard -> randomCardInGraveyardNames asking randomCardInGraveyard
  ObjectRef.RandomCardInLibrary randomCardInLibrary -> randomCardInLibraryNames asking randomCardInLibrary
  ObjectRef.AnyNumberMatching anyNumberMatching -> anyNumberMatchingNames asking anyNumberMatching
  ObjectRef.ChosenPermanent chosenPermanent -> chosenPermanentNames asking chosenPermanent
  ObjectRef.SourceAndChosenPermanent filter_ -> filterNames asking filter_
  ObjectRef.AttachedToBound attachedToBound -> attachedToBoundNames asking attachedToBound
  ObjectRef.FromAnywhere slotName -> slotNames asking slotName

offerCastNames :: Asking -> OfferCast.OfferCast -> Bool
offerCastNames asking x = case x of
  OfferCast.MkOfferCast ref caster _optionality _verb offer repetition _copied _controlWhileResolving slot -> objectRefNames asking ref || playerRefNames asking caster || castOfferNames asking offer || castRepetitionNames asking repetition || any (slotNames asking) slot

operandNames :: Asking -> Operand.Operand -> Bool
operandNames asking x = case x of
  Operand.Literal _integer -> False
  Operand.OfSource _measure -> False
  Operand.Own _measure -> False
  Operand.OfBound bound -> boundMeasureNames asking bound
  Operand.AmountInSlot slot -> slotNames asking slot
  Operand.EnclosingAmount -> contextNames asking

optionalityNames :: Asking -> Optionality.Optionality -> Bool
optionalityNames asking x = case x of
  Optionality.Mandatory -> False
  Optionality.Optional playerRef -> playerRefNames asking playerRef

orElseNames :: Asking -> OrElse.OrElse -> Bool
orElseNames asking x = case x of
  OrElse.MkOrElse _sibling chooser _villainous -> playerRefNames asking chooser

paidExpiryNames :: Asking -> PaidExpiry.PaidExpiry -> Bool
paidExpiryNames asking x = case x of
  PaidExpiry.MkPaidExpiry _player cost -> costNames asking (keywordNames asking) cost

payGateNames :: Asking -> PayGate.PayGate -> Bool
payGateNames asking x = case x of
  PayGate.MkPayGate payer cost basis _branch _obligation perEach _offeredAt -> playerRefNames asking payer || costChoiceNames asking cost || any (costBasisNames asking) basis || any (quantityNames asking) perEach

pendingDamageEffectNames :: Asking -> PendingDamageEffect.PendingDamageEffect -> Bool
pendingDamageEffectNames asking x = case x of
  PendingDamageEffect.MkPendingDamageEffect effects targets _controller source -> any (effectNames asking (const False) (grantedAbilityNames asking (const False))) effects || any (slotNames asking) (Map.keys targets) || any (any (recipientNames asking)) targets || source == object asking

pendingEntryEffectNames :: Asking -> PendingEntryEffect.PendingEntryEffect -> Bool
pendingEntryEffectNames asking x = case x of
  PendingEntryEffect.MkPendingEntryEffect object_ _controller effects -> object_ == object asking || any (effectNames asking (const False) (grantedAbilityNames asking (const False))) effects

perCreatureNames :: Asking -> PerCreature.PerCreature -> Bool
perCreatureNames asking x = case x of
  PerCreature.Fixed cost -> costNames asking (keywordNames asking) cost
  PerCreature.Counted quantity -> quantityNames asking quantity

permanentBecomesDesignatedNames :: Asking -> PermanentBecomesDesignated.PermanentBecomesDesignated -> Bool
permanentBecomesDesignatedNames asking x = case x of
  PermanentBecomesDesignated.MkPermanentBecomesDesignated _designation filter_ -> filterNames asking filter_

permanentCandidateNames :: Asking -> PermanentCandidate.PermanentCandidate -> Bool
permanentCandidateNames asking x = case x of
  PermanentCandidate.MkPermanentCandidate source _provenance effect _ordinal -> source == object asking || replacementEffectNames asking (const False) (grantedAbilityNames asking (const False)) (effectNames asking (const False) (grantedAbilityNames asking (const False))) effect

permanentDealsCombatDamageToPlayerNames :: Asking -> PermanentDealsCombatDamageToPlayer.PermanentDealsCombatDamageToPlayer -> Bool
permanentDealsCombatDamageToPlayerNames asking x = case x of
  PermanentDealsCombatDamageToPlayer.MkPermanentDealsCombatDamageToPlayer filter_ _recipient -> filterNames asking filter_

permanentSacrificedNames :: Asking -> PermanentSacrificed.PermanentSacrificed -> Bool
permanentSacrificedNames asking x = case x of
  PermanentSacrificed.MkPermanentSacrificed _player filter_ -> filterNames asking filter_

permanentTappedForManaNames :: Asking -> PermanentTappedForMana.PermanentTappedForMana -> Bool
permanentTappedForManaNames asking x = case x of
  PermanentTappedForMana.MkPermanentTappedForMana _player filter_ _mana -> filterNames asking filter_

permanentsBecomeTargetedNames :: Asking -> PermanentsBecomeTargeted.PermanentsBecomeTargeted -> Bool
permanentsBecomeTargetedNames asking x = case x of
  PermanentsBecomeTargeted.MkPermanentsBecomeTargeted filter_ _kind -> filterNames asking filter_

permanentsDealCombatDamageToPlayerNames :: Asking -> PermanentsDealCombatDamageToPlayer.PermanentsDealCombatDamageToPlayer -> Bool
permanentsDealCombatDamageToPlayerNames asking x = case x of
  PermanentsDealCombatDamageToPlayer.MkPermanentsDealCombatDamageToPlayer filter_ _recipient _oneOrMorePlayers -> filterNames asking filter_

playerAttacksWithNames :: Asking -> PlayerAttacksWith.PlayerAttacksWith -> Bool
playerAttacksWithNames asking x = case x of
  PlayerAttacksWith.MkPlayerAttacksWith _player filter_ _attackers -> filterNames asking filter_

playerCounterTallyNames :: Asking -> PlayerCounterTally.PlayerCounterTally -> Bool
playerCounterTallyNames asking x = case x of
  PlayerCounterTally.MkPlayerCounterTally player _kind -> playerRefNames asking player

playerCountersNames :: Asking -> PlayerCounters.PlayerCounters -> Bool
playerCountersNames asking x = case x of
  PlayerCounters.MkPlayerCounters player _kind quantity -> playerRefNames asking player || quantityNames asking quantity

playerDesignationTallyNames :: Asking -> PlayerDesignationTally.PlayerDesignationTally -> Bool
playerDesignationTallyNames asking x = case x of
  PlayerDesignationTally.MkPlayerDesignationTally player _designation -> playerRefNames asking player

activationCriteriaNames :: Asking -> ActivationCriteria.ActivationCriteria -> Bool
activationCriteriaNames asking x = case x of
  ActivationCriteria.MkActivationCriteria grantedBy _whichKind _whichLoyalty -> any (keywordDesignatorNames asking) grantedBy

costAdditionNames :: Asking -> CostAddition.CostAddition -> Bool
costAdditionNames asking x = case x of
  CostAddition.MkCostAddition components _scale -> any (costComponentNames asking (keywordNames asking)) components

costChangeNames :: Asking -> CostChange.CostChange -> Bool
costChangeNames asking x = case x of
  CostChange.Increase _natural -> False
  CostChange.Reduce _appliedReduction -> False
  CostChange.Add costAddition -> costAdditionNames asking costAddition

costModifierNames :: Asking -> CostModifier.CostModifier -> Bool
costModifierNames asking x = case x of
  CostModifier.MkCostModifier subject matching whichTargets perTarget _onlyFirst change -> costSubjectNames asking subject || filterNames asking matching || any (filterNames asking) whichTargets || any (filterNames asking) perTarget || costChangeNames asking change

costSubjectNames :: Asking -> CostSubject.CostSubject -> Bool
costSubjectNames asking x = case x of
  CostSubject.Spells -> False
  CostSubject.Activations activationCriteria -> activationCriteriaNames asking activationCriteria

playerEffectNames :: Asking -> PlayerEffect.PlayerEffect -> Bool
playerEffectNames asking x = case x of
  PlayerEffect.CantCastSpells -> False
  PlayerEffect.CantActivateAbilities keywordDesignator -> any (keywordDesignatorNames asking) keywordDesignator
  PlayerEffect.CantCastMoreThan _natural -> False
  PlayerEffect.ModifyCost costModifier -> costModifierNames asking costModifier
  PlayerEffect.AlternativeActivationCost alternativeActivationCost -> alternativeActivationCostNames asking alternativeActivationCost
  PlayerEffect.PlayAdditionalLands _natural -> False
  PlayerEffect.NoMaximumHandSize -> False
  PlayerEffect.SetMaximumHandSize _natural -> False
  PlayerEffect.IncreaseMaximumHandSize _natural -> False
  PlayerEffect.ReduceMaximumHandSize _natural -> False
  PlayerEffect.DontLoseUnspentMana _manaFilter -> False
  PlayerEffect.LoseLifeForUnspentMana -> False
  PlayerEffect.SpendManaAsThough _spendManaAsThough -> False
  PlayerEffect.CantBeTargetedBy _playerScope -> False
  PlayerEffect.HasProtectionFrom filter_ -> filterNames asking filter_
  PlayerEffect.CastAsThoughItHadFlash filter_ -> filterNames asking filter_
  PlayerEffect.MayPlayAsThoughItHadFlash filter_ -> filterNames asking filter_
  PlayerEffect.ActivateKeywordAtInstantSpeed keywordDesignator -> keywordDesignatorNames asking keywordDesignator
  PlayerEffect.ActivateLoyaltyAtInstantSpeed filter_ -> filterNames asking filter_
  PlayerEffect.CantBeCountered filter_ -> filterNames asking filter_
  PlayerEffect.DamageCantBePrevented damagePattern -> damagePatternNames asking damagePattern
  PlayerEffect.DamageCantBeRedirected damagePattern -> damagePatternNames asking damagePattern
  PlayerEffect.CantSearchLibraries _cantSearchLibraries -> False
  PlayerEffect.CantBecomeMonarch -> False
  PlayerEffect.CantSetSchemesInMotion -> False
  PlayerEffect.CantAttackWithCreatures -> False
  PlayerEffect.CantCastMatching filter_ -> filterNames asking filter_
  PlayerEffect.CastOnlyAtSorcerySpeed -> False
  PlayerEffect.CantPlayLands filter_ -> filterNames asking filter_
  PlayerEffect.CastFrom castFromZone -> castFromZoneNames asking castFromZone
  PlayerEffect.PlayLandsFrom inZone -> inZoneNames asking inZone
  PlayerEffect.PlotFrom plotFromZone -> plotFromZoneNames asking plotFromZone
  PlayerEffect.CastFromHandWithoutPayingManaCost filter_ -> filterNames asking filter_
  PlayerEffect.CantGetCounters _playerCounterKind -> False
  PlayerEffect.StateCoinFlip _statedFlip -> False
  PlayerEffect.ModifyDieRoll modifiedRoll -> modifiedRollNames asking modifiedRoll
  PlayerEffect.AdditionalVotes _natural -> False
  PlayerEffect.AdditionalSurveilCards _natural -> False
  PlayerEffect.CantGainLife -> False
  PlayerEffect.CantLoseLife -> False

playerQuantityNames :: Asking -> PlayerQuantity.PlayerQuantity -> Bool
playerQuantityNames asking x = case x of
  PlayerQuantity.MkPlayerQuantity player quantity -> playerRefNames asking player || quantityNames asking quantity

playerRefNames :: Asking -> PlayerRef.PlayerRef -> Bool
playerRefNames asking x = case x of
  PlayerRef.EachPlayerExcept slotName -> slotNames asking slotName
  PlayerRef.EachOpponentExcept slotName -> slotNames asking slotName
  PlayerRef.Relative _playerRelation -> False
  PlayerRef.InSlot slotName -> slotNames asking slotName
  PlayerRef.EachInSlot slotName -> slotNames asking slotName
  PlayerRef.Specific _playerId -> False
  PlayerRef.Candidate -> False
  PlayerRef.ControllerOfBound slotName -> slotNames asking slotName
  PlayerRef.OwnerOfBound slotName -> slotNames asking slotName
  PlayerRef.ControllerOfObject objectId -> objectId == object asking
  PlayerRef.OwnerOfObject objectId -> objectId == object asking
  PlayerRef.ChosenPlayerOfBound slotName -> slotNames asking slotName
  PlayerRef.Attacking attackingPlayers -> attackingPlayersNames asking attackingPlayers

playerSacrificesNames :: Asking -> PlayerSacrifices.PlayerSacrifices -> Bool
playerSacrificesNames asking x = case x of
  PlayerSacrifices.MkPlayerSacrifices players filter_ quantity -> playerRefNames asking players || filterNames asking filter_ || quantityNames asking quantity

playerStaticAbilityNames :: Asking -> PlayerStaticAbility.PlayerStaticAbility -> Bool
playerStaticAbilityNames asking x = case x of
  PlayerStaticAbility.MkPlayerStaticAbility _scope condition _name effect -> any (conditionNames asking) condition || playerEffectNames asking effect

playsLandNames :: Asking -> PlaysLand.PlaysLand -> Bool
playsLandNames asking x = case x of
  PlaysLand.MkPlaysLand _player _from filter_ -> filterNames asking filter_

plotFromZoneNames :: Asking -> PlotFromZone.PlotFromZone -> Bool
plotFromZoneNames asking x = case x of
  PlotFromZone.MkPlotFromZone from matching -> inZoneNames asking from || filterNames asking matching

plusNames :: Asking -> (quantity -> Bool) -> Plus.Plus quantity -> Bool
plusNames _asking onQuantity x = case x of
  Plus.MkPlus left right -> onQuantity left || onQuantity right

poolNames :: Asking -> Pool.Pool -> Bool
poolNames asking x = case x of
  Pool.Creatures -> False
  Pool.Players -> False
  Pool.AnyTarget -> False
  Pool.Permanents -> False
  Pool.Spells -> False
  Pool.Abilities -> False
  Pool.SpellsAndPermanents -> False
  Pool.PlayersAndPlaneswalkers -> False
  Pool.CardsInGraveyard zoneScope -> zoneScopeNames asking zoneScope
  Pool.CardsInExile -> False
  Pool.CardsInAnte -> False
  Pool.CreaturesAndCardsInGraveyard zoneScope -> zoneScopeNames asking zoneScope

powerNames :: Asking -> Power.Power -> Bool
powerNames asking x = case x of
  Power.MkPower unwrap -> quantityNames asking unwrap

preventAllDamageNames :: Asking -> (effect -> Bool) -> PreventAllDamage.PreventAllDamage effect -> Bool
preventAllDamageNames asking onEffect x = case x of
  PreventAllDamage.MkPreventAllDamage duration _kind ref whatRecipient _whoRecipient _direction chosenSource whatSource riders -> durationNames asking duration || any (objectRefNames asking) ref || any (filterNames asking) whatRecipient || any (filterNames asking) chosenSource || filterNames asking whatSource || any onEffect riders

preventNextDamageNames :: Asking -> (effect -> Bool) -> PreventNextDamage.PreventNextDamage effect -> Bool
preventNextDamageNames asking onEffect x = case x of
  PreventNextDamage.MkPreventNextDamage duration _kind ref whatRecipient _whoRecipient chosenSource quantity riders -> durationNames asking duration || any (objectRefNames asking) ref || any (filterNames asking) whatRecipient || any (filterNames asking) chosenSource || quantityNames asking quantity || any onEffect riders

preventNextDamageInstanceNames :: Asking -> (effect -> Bool) -> PreventNextDamageInstance.PreventNextDamageInstance effect -> Bool
preventNextDamageInstanceNames asking onEffect x = case x of
  PreventNextDamageInstance.MkPreventNextDamageInstance duration ref chosenSource riders -> durationNames asking duration || objectRefNames asking ref || filterNames asking chosenSource || any onEffect riders

preventionNames :: Asking -> Prevention.Prevention -> Bool
preventionNames asking x = case x of
  Prevention.MkPrevention by amounts rider -> candidateIdNames asking by || elem (object asking) (Map.keys amounts) || any (any (recipientNames asking) . Map.keys) amounts || any (preventionRiderNames asking) rider

preventionRiderNames :: Asking -> PreventionRider.PreventionRider -> Bool
preventionRiderNames asking x = case x of
  PreventionRider.MkPreventionRider effects targets _controller source -> any (effectNames asking (const False) (grantedAbilityNames asking (const False))) effects || any (slotNames asking) (Map.keys targets) || any (any (recipientNames asking)) targets || source == object asking

printedReplacementNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> (effect -> Bool) -> PrintedReplacement.PrintedReplacement card ability effect -> Bool
printedReplacementNames asking onCard onAbility onEffect x = case x of
  PrintedReplacement.MkPrintedReplacement condition effect _functionsFrom _name -> any (conditionNames asking) condition || replacementEffectNames asking onCard onAbility onEffect effect

projectedCharacteristicsNames :: Asking -> ProjectedCharacteristics.ProjectedCharacteristics -> Bool
projectedCharacteristicsNames asking x = case x of
  ProjectedCharacteristics.MkProjectedCharacteristics _names _supertypes keywords _colors _manaCost _manaValue _power _toughness _loyalty _defense _intensity characteristicPT _cardTypes _subtypes staticAbilities playerAbilities grantedPlayerAbilities grantedStaticAbilities grantedRuleAbilities specialActions activatedAbilities replacementEffects triggeredAbilities delayedAbilities enchant _castingPermissions ruleAbilities _lostAllAbilities _hasFullText _subtypeWordChanges textChangedKeywords _assignsCombatDamageWithToughness _grantsStationToughness additionalCosts additionalCostChoices alternativeCosts costReductions grantedCostReductions grantedAlternativeCosts _grantedSpendManaAsThough _halves exceptions mergedDonors prepare alternativeSpell spell flipped -> any (keywordNames asking) (Map.keys keywords) || any (characteristicPTNames asking) characteristicPT || any (staticAbilityNames asking (grantedAbilityNames asking (const False))) staticAbilities || any (playerStaticAbilityNames asking) playerAbilities || any (playerStaticAbilityNames asking . snd) grantedPlayerAbilities || any (staticAbilityNames asking (grantedAbilityNames asking (const False)) . snd) grantedStaticAbilities || ruleAbilitiesNames asking grantedRuleAbilities || any (specialActionNames asking) specialActions || any (activatedAbilityNames asking (const False) (grantedAbilityNames asking (const False))) activatedAbilities || any (printedReplacementNames asking (const False) (grantedAbilityNames asking (const False)) (effectNames asking (const False) (grantedAbilityNames asking (const False)))) replacementEffects || any (triggeredAbilityNames asking (const False) (grantedAbilityNames asking (const False))) triggeredAbilities || any (triggeredAbilityNames asking (const False) (grantedAbilityNames asking (const False))) delayedAbilities || any (targetSlotNames asking) enchant || ruleAbilitiesNames asking ruleAbilities || any (keywordNames asking) (Map.keys textChangedKeywords) || any (costComponentNames asking (keywordNames asking)) additionalCosts || any (costChoiceNames asking) additionalCostChoices || any (alternativeCostNames asking) alternativeCosts || any (costReductionNames asking) costReductions || any (costReductionNames asking) grantedCostReductions || any (alternativeCostNames asking) grantedAlternativeCosts || any (copyExceptionNames asking (grantedAbilityNames asking (const False))) exceptions || any (projectedCharacteristicsNames asking) mergedDonors || any (faceNames asking (const False)) prepare || any (faceNames asking (const False)) alternativeSpell || modalNames asking (const False) (grantedAbilityNames asking (const False)) spell || any (projectedCharacteristicsNames asking) flipped

proliferateRNames :: Asking -> ProliferateR.ProliferateR -> Bool
proliferateRNames asking x = case x of
  ProliferateR.MkProliferateR whose _rewrite -> controllerRelationNames asking whose

-- What is prohibited names no object.
prohibitNames :: Asking -> Prohibit.Prohibit -> Bool
prohibitNames asking x = case x of
  Prohibit.MkProhibit _what duration ref -> durationNames asking duration || objectRefNames asking ref

protectionNames :: Asking -> Protection.Protection keyword -> Bool
protectionNames asking x = case x of
  Protection.MkProtection quality spares -> filterNames asking quality || any (filterNames asking) spares

putCountersNames :: Asking -> PutCounters.PutCounters -> Bool
putCountersNames asking x = case x of
  PutCounters.MkPutCounters kind quantity ref -> counterKindNames asking (keywordNames asking) kind || quantityNames asking quantity || objectRefNames asking ref

putCountersFromNames :: Asking -> PutCountersFrom.PutCountersFrom -> Bool
putCountersFromNames asking x = case x of
  PutCountersFrom.MkPutCountersFrom from kind ref -> slotNames asking from || any (counterKindNames asking (keywordNames asking)) kind || objectRefNames asking ref

quantityNames :: Asking -> Quantity.Quantity -> Bool
quantityNames asking x = case x of
  Quantity.Literal _integer -> False
  Quantity.ManaValue -> False
  Quantity.Power -> False
  Quantity.Toughness -> False
  Quantity.Intensity -> False
  Quantity.InSlot slotName -> slotNames asking slotName
  Quantity.WasBound slotName -> slotNames asking slotName
  Quantity.BoundCount slotName -> slotNames asking slotName
  Quantity.UniqueVowelsOnSticker slotName -> slotNames asking slotName
  Quantity.Star -> False
  Quantity.Arithmetic arithmetic -> arithmeticNames asking (quantityNames asking) arithmetic
  Quantity.Count count -> countNames asking (quantityNames asking) count
  Quantity.ManaCount manaCount -> manaCountNames asking manaCount
  Quantity.LifeTotal playerRef -> playerRefNames asking playerRef
  Quantity.StartingLifeTotal playerRef -> playerRefNames asking playerRef
  Quantity.Speed playerRef -> playerRefNames asking playerRef
  Quantity.IsMonarch playerRef -> playerRefNames asking playerRef
  Quantity.HasPlayerDesignation playerDesignationTally -> playerDesignationTallyNames asking playerDesignationTally
  Quantity.IsStartingPlayer playerRef -> playerRefNames asking playerRef
  Quantity.IsActivePlayer playerRef -> playerRefNames asking playerRef
  Quantity.Devotion devotion -> devotionNames asking devotion
  Quantity.PartySize playerRef -> playerRefNames asking playerRef
  Quantity.PlayerCounters playerCounterTally -> playerCounterTallyNames asking playerCounterTally
  Quantity.ObjectCounters counterKind -> counterKindNames asking (keywordNames asking) counterKind
  Quantity.ObjectCountersOfAnyKind -> False
  Quantity.LettersOnNameStickers _ -> False
  Quantity.NameStickers -> False
  Quantity.PowerOfStickers -> False
  Quantity.ToughnessOfStickers -> False
  Quantity.HasDesignation _designation -> False
  Quantity.DesignationValue _designation -> False
  Quantity.StoredResultsOfSameValue -> False
  Quantity.ClassLevel -> False
  Quantity.WasForetold -> False
  Quantity.TributeWasPaid -> False
  Quantity.TimesPaid designator -> keywordDesignatorNames asking designator
  Quantity.CastUsing _keywordFamily -> False
  Quantity.TagWasSpent _productionTag -> False
  Quantity.TagWasSpentOfOwnColor _productionTag -> False
  Quantity.ChosenColorsItIs -> False
  Quantity.ManaSpent -> False
  Quantity.WasToken -> False
  Quantity.WasAttacking -> False
  Quantity.WasBlocking -> False
  Quantity.WasBlockedThisTurn -> False
  Quantity.ControlGainedSinceLastUpkeep playerRef -> playerRefNames asking playerRef
  Quantity.PlayedBy playerRef -> playerRefNames asking playerRef
  Quantity.OpponentsAttacked playerRef -> playerRefNames asking playerRef
  Quantity.AttackersDeclaredThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.AttackersDeclaredThisCombat -> False
  Quantity.AttackedInLastTurnOf playerRef -> playerRefNames asking playerRef
  Quantity.AttackersInTheirLastTurn playerRef -> playerRefNames asking playerRef
  Quantity.CardsDiscardedThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.CardsDrawnThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.BendingsThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.LifeGainedThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.PlayersDealtDamageThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.DamageDealtToPlayersThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.DamageDealtToThisTurn -> False
  Quantity.SpellsCastLastTurn playerRef -> playerRefNames asking playerRef
  Quantity.SpellsCastThisTurn playerRef -> playerRefNames asking playerRef
  Quantity.TimesResolvedThisTurn -> False
  Quantity.SpellsCastBefore -> False
  Quantity.PermanentsDiedThisTurn -> False
  Quantity.SpellsCastUsingThisTurn _keywordFamily -> False
  Quantity.SubgamesThisMatch -> False
  Quantity.DungeonsCompleted playerRef -> playerRefNames asking playerRef
  Quantity.CompletedDungeon completedDungeon -> completedDungeonNames asking completedDungeon
  Quantity.EnteredThisTurn -> False
  Quantity.EnteredFrom inZone -> inZoneNames asking inZone
  Quantity.WasCastFrom castFrom -> castFromNames asking castFrom
  Quantity.BlockersBeyondFirst -> False
  Quantity.AgainstSlot againstSlot -> againstSlotNames asking (quantityNames asking) againstSlot
  Quantity.AgainstCardsExiledWith quantity -> quantityNames asking quantity
  Quantity.AgainstLastCardExiledWith againstLastCardExiledWith -> againstLastCardExiledWithNames asking (quantityNames asking) againstLastCardExiledWith
  Quantity.AgainstCraftMaterials quantity -> quantityNames asking quantity
  Quantity.StationMeasure -> False

randomCardInGraveyardNames :: Asking -> RandomCardInGraveyard.RandomCardInGraveyard -> Bool
randomCardInGraveyardNames asking x = case x of
  RandomCardInGraveyard.MkRandomCardInGraveyard players filter_ count -> zoneScopeNames asking players || filterNames asking filter_ || quantityNames asking count

randomCardInHandNames :: Asking -> RandomCardInHand.RandomCardInHand -> Bool
randomCardInHandNames asking x = case x of
  RandomCardInHand.MkRandomCardInHand player filter_ count -> playerRefNames asking player || filterNames asking filter_ || quantityNames asking count

randomCardInLibraryNames :: Asking -> RandomCardInLibrary.RandomCardInLibrary -> Bool
randomCardInLibraryNames asking x = case x of
  RandomCardInLibrary.MkRandomCardInLibrary player filter_ count -> playerRefNames asking player || filterNames asking filter_ || quantityNames asking count

recipientNames :: Asking -> Recipient.Recipient -> Bool
recipientNames asking x = case x of
  Recipient.ToCreature objectId -> objectId == object asking
  Recipient.ToPlaneswalker objectId -> objectId == object asking
  Recipient.ToBattle objectId -> objectId == object asking
  Recipient.ToPlayer _playerId -> False
  Recipient.ToObject objectId -> objectId == object asking
  Recipient.ToPile _pile -> False

redirectDamageNames :: Asking -> RedirectDamage.RedirectDamage -> Bool
redirectDamageNames asking x = case x of
  RedirectDamage.MkRedirectDamage duration _kind amount from whatRecipient _whoRecipient to chosenSource -> durationNames asking duration || any (quantityNames asking) amount || any (objectRefNames asking) from || any (filterNames asking) whatRecipient || objectRefNames asking to || any (filterNames asking) chosenSource

reinforceNames :: Asking -> (keyword -> Bool) -> Reinforce.Reinforce keyword -> Bool
reinforceNames asking onKeyword x = case x of
  Reinforce.MkReinforce _amount cost -> costNames asking onKeyword cost

removalCountNames :: Asking -> RemovalCount.RemovalCount -> Bool
removalCountNames asking x = case x of
  RemovalCount.Exactly quantity -> quantityNames asking quantity
  RemovalCount.UpTo quantity -> quantityNames asking quantity
  RemovalCount.AnyNumber -> False

removeCountersNames :: Asking -> RemoveCounters.RemoveCounters -> Bool
removeCountersNames asking x = case x of
  RemoveCounters.MkRemoveCounters kind quantity slot tally -> counterKindNames asking (keywordNames asking) kind || quantityNames asking quantity || slotNames asking slot || any (slotNames asking) tally

removePlayerCountersNames :: Asking -> RemovePlayerCounters.RemovePlayerCounters -> Bool
removePlayerCountersNames asking x = case x of
  RemovePlayerCounters.MkRemovePlayerCounters player _kind quantity tally -> playerRefNames asking player || quantityNames asking quantity || any (slotNames asking) tally

removeCountersAmongNames :: Asking -> RemoveCountersAmong.RemoveCountersAmong -> Bool
removeCountersAmongNames asking x = case x of
  RemoveCountersAmong.MkRemoveCountersAmong count from kind tally -> removalCountNames asking count || objectRefNames asking from || whichCountersNames asking (keywordNames asking) kind || any (slotNames asking) tally

repeatNames :: Asking -> (effect -> Bool) -> Repeat.Repeat effect -> Bool
repeatNames asking onEffect x = case x of
  Repeat.MkRepeat chooser body gate -> playerRefNames asking chooser || any onEffect body || any (conditionNames asking) gate

repeatIfNames :: Asking -> (effect -> Bool) -> RepeatIf.RepeatIf effect -> Bool
repeatIfNames asking onEffect x = case x of
  RepeatIf.MkRepeatIf process condition ifHolds -> any onEffect process || conditionNames asking condition || any onEffect ifHolds

replaceNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> (effect -> Bool) -> Replace.Replace card ability effect -> Bool
replaceNames asking onCard onAbility onEffect x = case x of
  Replace.MkReplace duration _uses _origin condition effect -> durationNames asking duration || any (conditionNames asking) condition || replacementEffectNames asking onCard onAbility onEffect effect

replacementEffectNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> (effect -> Bool) -> ReplacementEffect.ReplacementEffect card ability effect -> Bool
replacementEffectNames asking onCard onAbility onEffect x = case x of
  ReplacementEffect.ZoneChangeR zoneChangeR -> zoneChangeRNames asking zoneChangeR
  ReplacementEffect.EntryR entryR -> entryRNames asking onAbility onEffect entryR
  ReplacementEffect.DamageR damageR -> damageRNames asking onEffect damageR
  ReplacementEffect.DestructionR destructionR -> destructionRNames asking destructionR
  ReplacementEffect.CounterR counterR -> counterRNames asking counterR
  ReplacementEffect.TokenR tokenR -> tokenRNames asking onCard tokenR
  ReplacementEffect.TurnUpR turnUpR -> turnUpRNames asking turnUpR
  ReplacementEffect.UntapR untapR -> untapRNames asking untapR
  ReplacementEffect.LifeLossR lifeLossR -> lifeLossRNames asking lifeLossR
  ReplacementEffect.LifeGainR lifeGainR -> lifeGainRNames asking lifeGainR
  ReplacementEffect.DrawR drawR -> drawRNames asking drawR
  ReplacementEffect.DrawCountR drawCountR -> drawCountRNames asking drawCountR
  ReplacementEffect.MillCountR millCountR -> millCountRNames asking millCountR
  ReplacementEffect.CoinFlipR coinFlipR -> coinFlipRNames asking coinFlipR
  ReplacementEffect.DieRollR dieRollR -> dieRollRNames asking dieRollR
  ReplacementEffect.ProliferateR proliferateR -> proliferateRNames asking proliferateR
  ReplacementEffect.ScryR scryR -> scryRNames asking scryR
  ReplacementEffect.VillainousChoiceR villainousChoiceR -> villainousChoiceRNames asking villainousChoiceR
  ReplacementEffect.PhaseR _phasePattern -> False

requireAttackNames :: Asking -> RequireAttack.RequireAttack -> Bool
requireAttackNames asking x = case x of
  RequireAttack.MkRequireAttack duration attacker defender -> durationNames asking duration || restrictedCreaturesNames asking (objectRefNames asking) attacker || attackTargetRefNames asking defender

requireBlockNames :: Asking -> RequireBlock.RequireBlock -> Bool
requireBlockNames asking x = case x of
  RequireBlock.MkRequireBlock duration blocker attacker -> durationNames asking duration || objectRefNames asking blocker || objectRefNames asking attacker

restrictedCreaturesNames :: Asking -> (named -> Bool) -> RestrictedCreatures.RestrictedCreatures named -> Bool
restrictedCreaturesNames asking onNamed x = case x of
  RestrictedCreatures.Named named -> onNamed named
  RestrictedCreatures.Matching filter_ -> filterNames asking filter_

exilePermanentsNames :: Asking -> ExilePermanents.ExilePermanents keyword -> Bool
exilePermanentsNames asking x = case x of
  ExilePermanents.MkExilePermanents _count whichPermanents -> filterNames asking whichPermanents

returnPermanentsNames :: Asking -> ReturnPermanents.ReturnPermanents keyword -> Bool
returnPermanentsNames asking x = case x of
  ReturnPermanents.MkReturnPermanents _count whichPermanents -> filterNames asking whichPermanents

revealNames :: Asking -> Reveal.Reveal -> Bool
revealNames asking x = case x of
  Reveal.MkReveal ref slot -> objectRefNames asking ref || any (slotNames asking) slot

rollDieNames :: Asking -> RollDie.RollDie -> Bool
rollDieNames asking x = case x of
  RollDie.MkRollDie _sides count modifier _reading slot other _roller highest store -> quantityNames asking count || any (quantityNames asking) modifier || slotNames asking slot || any (slotNames asking) other || any (slotNames asking) highest || any (slotNames asking) store

ruleAbilitiesNames :: Asking -> RuleAbilities.RuleAbilities -> Bool
ruleAbilitiesNames asking x = case x of
  RuleAbilities.MkRuleAbilities activationProhibitions attachRestrictions attackCosts attackPermissions attackRequirements blockCosts blockPermissions blockRequirements combatRestrictions counterRestrictions crewRestrictions entryRestrictions sacrificeRestrictions untapRestrictions -> any (activationProhibitionNames asking) activationProhibitions || any (attachRestrictionNames asking) attachRestrictions || any (attackCostNames asking) attackCosts || any (attackPermissionNames asking) attackPermissions || any (attackRequirementNames asking) attackRequirements || any (blockCostNames asking) blockCosts || any (blockPermissionNames asking) blockPermissions || any (blockRequirementNames asking) blockRequirements || any (combatRestrictionNames asking) combatRestrictions || any (counterRestrictionNames asking) counterRestrictions || any (crewRestrictionNames asking) crewRestrictions || any (entryRestrictionNames asking) entryRestrictions || any (sacrificeRestrictionNames asking) sacrificeRestrictions || any (untapRestrictionNames asking) untapRestrictions

sacrificeNames :: Asking -> Sacrifice.Sacrifice keyword -> Bool
sacrificeNames asking x = case x of
  Sacrifice.MkSacrifice _count whichPermanents -> filterNames asking whichPermanents

sacrificeAnyNumberNames :: Asking -> SacrificeAnyNumber.SacrificeAnyNumber -> Bool
sacrificeAnyNumberNames asking x = case x of
  SacrificeAnyNumber.MkSacrificeAnyNumber filter_ kind each -> filterNames asking filter_ || any (counterKindNames asking (keywordNames asking)) kind || quantityNames asking each

sacrificeEffectNames :: Asking -> SacrificeEffect.SacrificeEffect -> Bool
sacrificeEffectNames asking x = case x of
  SacrificeEffect.MkSacrificeEffect ref _sacrificer sacrificed -> objectRefNames asking ref || any (slotNames asking) sacrificed

sacrificeRestrictionNames :: Asking -> SacrificeRestriction.SacrificeRestriction -> Bool
sacrificeRestrictionNames asking x = case x of
  SacrificeRestriction.MkSacrificeRestriction affected -> affectedNames asking affected

sacrificeToEnterNames :: Asking -> SacrificeToEnter.SacrificeToEnter -> Bool
sacrificeToEnterNames asking x = case x of
  SacrificeToEnter.MkSacrificeToEnter _count filter_ -> filterNames asking filter_

scopeNames :: Asking -> Scope.Scope -> Bool
scopeNames asking x = case x of
  Scope.InZone inZone -> inZoneNames asking inZone
  Scope.InHistory _eventShape -> False
  Scope.OverPlayers playerRef -> playerRefNames asking playerRef
  Scope.OverBound slotName -> slotNames asking slotName
  Scope.TopOfGraveyard playerRef -> playerRefNames asking playerRef

scryRNames :: Asking -> ScryR.ScryR -> Bool
scryRNames asking x = case x of
  ScryR.MkScryR whose _rewrite -> controllerRelationNames asking whose

searchNames :: Asking -> Search.Search -> Bool
searchNames asking x = case x of
  Search.MkSearch searcher owner _zones _outsideTheGame quantity filter_ _upTo _destination subject slot _differentIn _exactly -> playerRefNames asking searcher || playerRefNames asking owner || any (quantityNames asking) quantity || filterNames asking filter_ || any (slotNames asking) subject || any (slotNames asking) slot

selfCountersReachedNames :: Asking -> SelfCountersReached.SelfCountersReached -> Bool
selfCountersReachedNames asking x = case x of
  SelfCountersReached.MkSelfCountersReached kind _amount -> counterKindNames asking (keywordNames asking) kind

selfCountersRemovedNames :: Asking -> SelfCountersRemoved.SelfCountersRemoved -> Bool
selfCountersRemovedNames asking x = case x of
  SelfCountersRemoved.MkSelfCountersRemoved kind _zone -> counterKindNames asking (keywordNames asking) kind

setBasePowerToughnessNames :: Asking -> SetBasePowerToughness.SetBasePowerToughness -> Bool
setBasePowerToughnessNames asking x = case x of
  SetBasePowerToughness.MkSetBasePowerToughness power toughness -> any (quantityNames asking) power || any (quantityNames asking) toughness

setClassLevelNames :: Asking -> SetClassLevel.SetClassLevel -> Bool
setClassLevelNames asking x = case x of
  SetClassLevel.MkSetClassLevel _level slot -> slotNames asking slot

setHalfLockedNames :: Asking -> SetHalfLocked.SetHalfLocked -> Bool
setHalfLockedNames asking x = case x of
  SetHalfLocked.MkSetHalfLocked _every _locked slot -> slotNames asking slot

anteNames :: Asking -> Ante.Ante -> Bool
anteNames asking x = case x of
  Ante.MkAnte player ref slot -> playerRefNames asking player || objectRefNames asking ref || any (slotNames asking) slot

putStickerNames :: Asking -> PutSticker.PutSticker -> Bool
putStickerNames asking x = case x of
  PutSticker.MkPutSticker player ref _kinds cap _free bound -> playerRefNames asking player || objectRefNames asking ref || any (quantityNames asking) cap || any (slotNames asking) bound

shuffleIntoLibraryNames :: Asking -> ShuffleIntoLibrary.ShuffleIntoLibrary -> Bool
shuffleIntoLibraryNames asking x = case x of
  ShuffleIntoLibrary.MkShuffleIntoLibrary library refs -> any (playerRefNames asking) library || any (objectRefNames asking) refs

skipNextPhaseNames :: Asking -> SkipNextPhase.SkipNextPhase -> Bool
skipNextPhaseNames asking x = case x of
  SkipNextPhase.MkSkipNextPhase player _selector -> playerRefNames asking player

slotCountNames :: Asking -> SlotCount.SlotCount -> Bool
slotCountNames asking x = case x of
  SlotCount.Printed _targetCount -> False
  SlotCount.AnnouncedX -> False
  SlotCount.UpToAnnouncedX -> False
  SlotCount.UpToComputed quantity -> quantityNames asking quantity

slotPerPlayerNames :: Asking -> SlotPerPlayer.SlotPerPlayer -> Bool
slotPerPlayerNames asking x = case x of
  SlotPerPlayer.MkSlotPerPlayer _players slot -> slotNames asking slot

specialActionNames :: Asking -> SpecialAction.SpecialAction -> Bool
specialActionNames asking x = case x of
  SpecialAction.DiscardThisAnyTime -> False
  SpecialAction.IgnoreThisUntilEndOfTurn _abilityName cost -> costNames asking (keywordNames asking) cost

speedDecreaseNames :: Asking -> SpeedDecrease.SpeedDecrease -> Bool
speedDecreaseNames asking x = case x of
  SpeedDecrease.MkSpeedDecrease player quantity _floor_ -> playerRefNames asking player || quantityNames asking quantity

spellCastNames :: Asking -> SpellCast.SpellCast -> Bool
spellCastNames asking x = case x of
  SpellCast.MkSpellCast filter_ _scope _zone _ordinal _phase _copies -> filterNames asking filter_

spliceNames :: Asking -> (keyword -> Bool) -> Splice.Splice keyword -> Bool
spliceNames asking onKeyword x = case x of
  Splice.MkSplice onto cost -> filterNames asking onto || costNames asking onKeyword cost

staticAbilityNames :: Asking -> (ability -> Bool) -> StaticAbility.StaticAbility ability -> Bool
staticAbilityNames asking onAbility x = case x of
  StaticAbility.MkStaticAbility affected condition _functionsFrom lingers _name modifications -> affectedNames asking affected || any (conditionNames asking) condition || any (durationNames asking) lingers || any (modificationNames asking onAbility) modifications

suspendNames :: Asking -> (keyword -> Bool) -> Suspend.Suspend keyword -> Bool
suspendNames asking onKeyword x = case x of
  Suspend.MkSuspend _counters cost -> costNames asking onKeyword cost

takeExtraTurnNames :: Asking -> TakeExtraTurn.TakeExtraTurn -> Bool
takeExtraTurnNames asking x = case x of
  TakeExtraTurn.MkTakeExtraTurn player _skips count -> playerRefNames asking player || quantityNames asking count

tapForTotalPowerNames :: Asking -> TapForTotalPower.TapForTotalPower keyword -> Bool
tapForTotalPowerNames asking x = case x of
  TapForTotalPower.MkTapForTotalPower _totalPower whichPermanents -> filterNames asking whichPermanents

tapPermanentsNames :: Asking -> TapPermanents.TapPermanents keyword -> Bool
tapPermanentsNames asking x = case x of
  TapPermanents.MkTapPermanents _count whichPermanents _sharingACreatureType -> filterNames asking whichPermanents

targetChooserNames :: Asking -> TargetChooser.TargetChooser -> Bool
targetChooserNames asking x = case x of
  TargetChooser.Relative _playerRelation -> False
  TargetChooser.InSlot slotName -> slotNames asking slotName

targetSlotNames :: Asking -> TargetSlot.TargetSlot -> Bool
targetSlotNames asking x = case x of
  TargetSlot.MkTargetSlot pool filter_ count amount chooser perPlayer -> poolNames asking pool || any (filterNames asking) filter_ || slotCountNames asking count || any (quantityNames asking) amount || any (targetChooserNames asking) chooser || any (slotPerPlayerNames asking) perPlayer

theseDiscardNames :: Asking -> TheseDiscard.TheseDiscard -> Bool
theseDiscardNames asking x = case x of
  TheseDiscard.MkTheseDiscard cards discarded -> objectRefNames asking cards || any (slotNames asking) discarded

timesNames :: Asking -> (quantity -> Bool) -> Times.Times quantity -> Bool
timesNames _asking onQuantity x = case x of
  Times.MkTimes _factor quantity -> onQuantity quantity

tokenPatternNames :: Asking -> TokenPattern.TokenPattern -> Bool
tokenPatternNames asking x = case x of
  TokenPattern.MkTokenPattern whose whatToken -> controllerRelationNames asking whose || filterNames asking whatToken

tokenPlusNames :: Asking -> (card -> Bool) -> TokenPlus.TokenPlus card -> Bool
tokenPlusNames _asking onCard x = case x of
  TokenPlus.One card -> onCard card
  TokenPlus.ThatMany card -> onCard card

tokenRNames :: Asking -> (card -> Bool) -> TokenR.TokenR card -> Bool
tokenRNames asking onCard x = case x of
  TokenR.MkTokenR matching _scaling plus -> tokenPatternNames asking matching || any (tokenPlusNames asking onCard) plus

topOfLibraryNames :: Asking -> TopOfLibrary.TopOfLibrary -> Bool
topOfLibraryNames asking x = case x of
  TopOfLibrary.MkTopOfLibrary player count -> playerRefNames asking player || quantityNames asking count

topOfLibraryUntilNames :: Asking -> TopOfLibraryUntil.TopOfLibraryUntil -> Bool
topOfLibraryUntilNames asking x = case x of
  TopOfLibraryUntil.MkTopOfLibraryUntil player filter_ count -> playerRefNames asking player || filterNames asking filter_ || quantityNames asking count

toughnessNames :: Asking -> Toughness.Toughness -> Bool
toughnessNames asking x = case x of
  Toughness.MkToughness unwrap -> quantityNames asking unwrap

triggerConditionNames :: Asking -> TriggerCondition.TriggerCondition -> Bool
triggerConditionNames asking x = case x of
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters filter_ -> filterNames asking filter_
  TriggerCondition.PermanentsEnter filter_ -> filterNames asking filter_
  TriggerCondition.StepBegins _stepBegins -> False
  TriggerCondition.StateIs condition -> conditionNames asking condition
  TriggerCondition.SelfDealsCombatDamageToPlayer _playerRelation -> False
  TriggerCondition.SelfDealsCombatDamageToPlayerOrBattle -> False
  TriggerCondition.SelfDealsCombatDamage -> False
  TriggerCondition.SelfDealsDamageToPlayer _playerRelation -> False
  TriggerCondition.SelfDealsDamageToCreature -> False
  TriggerCondition.SelfDealsDamage -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer permanentDealsCombatDamageToPlayer -> permanentDealsCombatDamageToPlayerNames asking permanentDealsCombatDamageToPlayer
  TriggerCondition.PermanentsDealCombatDamageToPlayer permanentsDealCombatDamageToPlayer -> permanentsDealCombatDamageToPlayerNames asking permanentsDealCombatDamageToPlayer
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> False
  TriggerCondition.PlayerTookInitiative -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.SelfExiledForMadness -> False
  TriggerCondition.PlayerDiscards _playerRelation -> False
  TriggerCondition.PlayerDiscardsCards _playerRelation -> False
  TriggerCondition.PlayerCycles _playerRelation -> False
  TriggerCondition.PlayerDrawsNthCard _playerDrawsNthCard -> False
  TriggerCondition.SelfAttacks _triggerFrequency -> False
  TriggerCondition.SelfAttacksWithAnother filter_ -> filterNames asking filter_
  TriggerCondition.SelfAttacksPermanent filter_ -> filterNames asking filter_
  TriggerCondition.CreatureAttacksAlone filter_ -> filterNames asking filter_
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.CreatureAttacks filter_ -> filterNames asking filter_
  TriggerCondition.AttachedPlayerIsAttacked -> False
  TriggerCondition.SelfIsAttacked -> False
  TriggerCondition.PlayerAttacks _playerRelation -> False
  TriggerCondition.PlayerAttacksWith playerAttacksWith -> playerAttacksWithNames asking playerAttacksWith
  TriggerCondition.PlayerAttacksPlayer _playerAttacksPlayer -> False
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfAttacksWhileSaddled -> False
  TriggerCondition.SelfAttacksWhile condition -> conditionNames asking condition
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.CreatureBlocks filter_ -> filterNames asking filter_
  TriggerCondition.SelfBlocksCreature filter_ -> filterNames asking filter_
  TriggerCondition.SelfBlocksAtLeast _natural -> False
  TriggerCondition.SelfBlocksOneOrMore filter_ -> filterNames asking filter_
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy filter_ -> filterNames asking filter_
  TriggerCondition.SelfBecomesBlockedByOneOrMore filter_ -> filterNames asking filter_
  TriggerCondition.CreatureBecomesBlockedByAtLeast _creatureBecomesBlockedByAtLeast -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  TriggerCondition.SelfPutIntoGraveyardDuringResolution -> False
  TriggerCondition.CardPutIntoGraveyard cardPutIntoGraveyard -> cardPutIntoGraveyardNames asking cardPutIntoGraveyard
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies filter_ -> filterNames asking filter_
  TriggerCondition.PermanentsDie filter_ -> filterNames asking filter_
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.SelfPutFromBattlefieldInto _ownedZone -> False
  TriggerCondition.PermanentLeavesTheBattlefield filter_ -> filterNames asking filter_
  TriggerCondition.PermanentReturnedToHand filter_ -> filterNames asking filter_
  TriggerCondition.PermanentsReturnedToHand filter_ -> filterNames asking filter_
  TriggerCondition.CardLeavesZone cardLeavesZone -> cardLeavesZoneNames asking cardLeavesZone
  TriggerCondition.CardsLeaveZone cardLeavesZone -> cardLeavesZoneNames asking cardLeavesZone
  TriggerCondition.CardsPutIntoZone cardsPutIntoZone -> cardsPutIntoZoneNames asking cardsPutIntoZone
  TriggerCondition.SelfLeavesGraveyard -> False
  TriggerCondition.AttachedCreatureDies -> False
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  TriggerCondition.PermanentsBecomeTapped filter_ -> filterNames asking filter_
  TriggerCondition.SelfBecomesUntapped -> False
  TriggerCondition.AttachedPermanentTappedForMana -> False
  TriggerCondition.PermanentTappedForMana permanentTappedForMana -> permanentTappedForManaNames asking permanentTappedForMana
  TriggerCondition.AbilityAddsMana abilityAddsMana -> abilityAddsManaNames asking abilityAddsMana
  TriggerCondition.SelfManaAbilityResolves -> False
  TriggerCondition.HauntedCreatureDies -> False
  TriggerCondition.SpellOrAbilityCounters _playerRelation -> False
  TriggerCondition.AbilityIsCountered -> False
  TriggerCondition.DamageToPlayerPrevented _playerRelation -> False
  TriggerCondition.SelfPreventsDamage filter_ -> filterNames asking filter_
  TriggerCondition.PlayerGainsLife _playerRelation -> False
  TriggerCondition.PlayersGainLife _playerRelation -> False
  TriggerCondition.PlayerLosesLife _playerRelation -> False
  TriggerCondition.SelfCountersReached selfCountersReached -> selfCountersReachedNames asking selfCountersReached
  TriggerCondition.SelfBecomesClassLevel _classLevel -> False
  TriggerCondition.SelfLastCounterRemoved selfCountersRemoved -> selfCountersRemovedNames asking selfCountersRemoved
  TriggerCondition.SelfCountersRemoved selfCountersRemoved -> selfCountersRemovedNames asking selfCountersRemoved
  TriggerCondition.SelfCounterRemoved selfCountersRemoved -> selfCountersRemovedNames asking selfCountersRemoved
  TriggerCondition.PermanentsGetCounters counterPlacement -> counterPlacementNames asking counterPlacement
  TriggerCondition.PermanentGetsCounters counterPlacement -> counterPlacementNames asking counterPlacement
  TriggerCondition.SpellCast spellCast -> spellCastNames asking spellCast
  TriggerCondition.SelfCast -> False
  TriggerCondition.SelfBecomesTargeted _playerRelation -> False
  TriggerCondition.ControllerBecomesTarget _controllerBecomesTarget -> False
  TriggerCondition.PermanentsBecomeTargeted permanentsBecomeTargeted -> permanentsBecomeTargetedNames asking permanentsBecomeTargeted
  TriggerCondition.PermanentBecomesTargeted permanentsBecomeTargeted -> permanentsBecomeTargetedNames asking permanentsBecomeTargeted
  TriggerCondition.SelfHalfUnlocked _cardName -> False
  TriggerCondition.RoomFullyUnlocked _playerRelation -> False
  TriggerCondition.AnyOf items -> any (triggerConditionNames asking) items
  TriggerCondition.SelfTurnedFaceUp -> False
  TriggerCondition.SelfTransformedInto _cardName -> False
  TriggerCondition.PermanentTransforms filter_ -> filterNames asking filter_
  TriggerCondition.PermanentTurnedFaceUp filter_ -> filterNames asking filter_
  TriggerCondition.FaceDownPermanentLeavesRevealed -> False
  TriggerCondition.PermanentBecomesDesignated permanentBecomesDesignated -> permanentBecomesDesignatedNames asking permanentBecomesDesignated
  TriggerCondition.PermanentActs permanentActs -> permanentActedNames asking (actingPermanentNames asking) permanentActs
  TriggerCondition.AttachedCreatureMentors -> False
  TriggerCondition.SelfExploits -> False
  TriggerCondition.CreatureExploits creatureExploits -> creatureExploitsNames asking creatureExploits
  TriggerCondition.SelfBecomesCrewed _triggerFrequency -> False
  TriggerCondition.SelfCrewsVehicle -> False
  TriggerCondition.PermanentSacrificed permanentSacrificed -> permanentSacrificedNames asking permanentSacrificed
  TriggerCondition.SagaFinalChapterTriggers _playerRelation -> False
  TriggerCondition.PlayerBecomesMonarch _playerRelation -> False
  TriggerCondition.LoseControlOfBound slotName -> slotNames asking slotName
  TriggerCondition.BoundDiesOrIsExiled slotName -> slotNames asking slotName
  TriggerCondition.BoundDies slotName -> slotNames asking slotName
  TriggerCondition.RoomEntered _roomIndex -> False
  TriggerCondition.PlayerActs _playerActs -> False
  TriggerCondition.PlayerLosesGame _playerRelation -> False
  TriggerCondition.PlayerPlaysLand playsLand -> playsLandNames asking playsLand
  TriggerCondition.PlayerManifestsDread _playerRelation -> False
  TriggerCondition.ChaosEnsues -> False
  TriggerCondition.PlayerRollsPlaneswalker _playerRelation -> False
  TriggerCondition.SetInMotion -> False
  TriggerCondition.PlayerRollsResult dieResult -> dieResultNames asking (const False) dieResult
  TriggerCondition.Visit -> False
  TriggerCondition.PlayerWinsCoinFlip _playerRelation -> False
  TriggerCondition.PlayerLosesCoinFlip _playerRelation -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.SelfBecomesAttachedBy filter_ -> filterNames asking filter_
  TriggerCondition.SelfBecomesAttachedTo filter_ -> filterNames asking filter_
  TriggerCondition.SelfBecomesUnattachedFrom filter_ -> filterNames asking filter_
  TriggerCondition.Reflexive -> False
  TriggerCondition.PermanentBecomesBlockedBy filter_ -> filterNames asking filter_
  TriggerCondition.PlacesSticker placesSticker -> filterNames asking (PlacesSticker.object placesSticker)

triggeredAbilityNames :: Asking -> (card -> Bool) -> (ability -> Bool) -> TriggeredAbility.TriggeredAbility card ability -> Bool
triggeredAbilityNames asking onCard onAbility x = case x of
  TriggeredAbility.MkTriggeredAbility condition modal intervening _name _limit -> triggerConditionNames asking condition || modalNames asking onCard onAbility modal || any (conditionNames asking) intervening

turnFaceDownNames :: Asking -> (ability -> Bool) -> TurnFaceDown.TurnFaceDown ability -> Bool
turnFaceDownNames asking onAbility x = case x of
  TurnFaceDown.MkTurnFaceDown ref characteristics -> objectRefNames asking ref || faceDownCharacteristicsNames asking onAbility characteristics

turnUpRNames :: Asking -> TurnUpR.TurnUpR -> Bool
turnUpRNames asking x = case x of
  TurnUpR.MkTurnUpR matching _requiring paying rewrite -> filterNames asking matching || any (costNames asking (keywordNames asking)) paying || turnUpRewriteNames asking rewrite

turnUpRewriteNames :: Asking -> TurnUpRewrite.TurnUpRewrite -> Bool
turnUpRewriteNames asking x = case x of
  TurnUpRewrite.WithCounters withCounters -> withCountersNames asking withCounters
  TurnUpRewrite.MayAttachTo filter_ -> filterNames asking filter_

untapRNames :: Asking -> UntapR.UntapR -> Bool
untapRNames asking x = case x of
  UntapR.MkUntapR _during rewrite -> untapRewriteNames asking rewrite

untapRestrictionNames :: Asking -> UntapRestriction.UntapRestriction -> Bool
untapRestrictionNames asking x = case x of
  UntapRestriction.MkUntapRestriction affected -> affectedNames asking affected

untapRewriteNames :: Asking -> UntapRewrite.UntapRewrite -> Bool
untapRewriteNames asking x = case x of
  UntapRewrite.RemoveStunCounter -> False
  UntapRewrite.RemoveCounterToUntap counterKind -> counterKindNames asking (keywordNames asking) counterKind

villainousChoiceRNames :: Asking -> VillainousChoiceR.VillainousChoiceR -> Bool
villainousChoiceRNames asking x = case x of
  VillainousChoiceR.MkVillainousChoiceR whose _rewrite -> controllerRelationNames asking whose

voteNames :: Asking -> Vote.Vote -> Bool
voteNames asking x = case x of
  Vote.MkVote starter choices -> playerRefNames asking starter || voteChoicesNames asking choices

voteChoicesNames :: Asking -> VoteChoices.VoteChoices -> Bool
voteChoicesNames asking x = case x of
  VoteChoices.Objects voteObjects -> voteObjectsNames asking voteObjects
  VoteChoices.Words slotName -> any (slotNames asking) slotName

voteObjectsNames :: Asking -> VoteObjects.VoteObjects -> Bool
voteObjectsNames asking x = case x of
  VoteObjects.MkVoteObjects filter_ slot -> filterNames asking filter_ || slotNames asking slot

wardNames :: Asking -> (keyword -> Bool) -> Ward.Ward keyword -> Bool
wardNames asking onKeyword x = case x of
  Ward.MkWard cost perEach -> costNames asking onKeyword cost || any (playerCounterTallyNames asking) perEach

whenSpentNames :: Asking -> WhenSpent.WhenSpent -> Bool
whenSpentNames asking x = case x of
  WhenSpent.MkWhenSpent casts _ability -> filterNames asking casts

whichCountersNames :: Asking -> (keyword -> Bool) -> WhichCounters.WhichCounters keyword -> Bool
whichCountersNames asking onKeyword x = case x of
  WhichCounters.OfKind counterKind -> counterKindNames asking onKeyword counterKind
  WhichCounters.OfAnyKind -> False

whileNames :: Asking -> While.While -> Bool
whileNames asking x = case x of
  While.MkWhile _player condition -> conditionNames asking condition

withCountersNames :: Asking -> WithCounters.WithCounters -> Bool
withCountersNames asking x = case x of
  WithCounters.MkWithCounters counters -> any (counterKindNames asking (keywordNames asking)) (Map.keys counters) || any (quantityNames asking) counters

zoneChangePatternNames :: Asking -> ZoneChangePattern.ZoneChangePattern -> Bool
zoneChangePatternNames asking x = case x of
  ZoneChangePattern.MkZoneChangePattern _whenDestination whoseObject whatObject _whenDiscarded _duringResolution -> controllerRelationNames asking whoseObject || filterNames asking whatObject

zoneChangeRNames :: Asking -> ZoneChangeR.ZoneChangeR -> Bool
zoneChangeRNames asking x = case x of
  ZoneChangeR.MkZoneChangeR matching _destination _revealing _shuffling _position _optional -> zoneChangePatternNames asking matching

zoneScopeNames :: Asking -> ZoneScope.ZoneScope -> Bool
zoneScopeNames asking x = case x of
  ZoneScope.Scoped _playerScope -> False
  ZoneScope.InSlot slotName -> slotNames asking slotName
  ZoneScope.ControllerOfBound slotName -> slotNames asking slotName
  ZoneScope.BoundPlayer _playerId -> False
