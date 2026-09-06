-- Pawl.Engine.Card's lints over the shape of a printed effect and face: power
-- and toughness boxes, Aura pools, mana no payment could spend, zone-bound
-- opcodes, baked engine-minted shapes, riders, and what each card kind's face
-- must carry (CR 205.1, CR 208.1, CR 309, CR 709.5a, CR 712.4b). Split out of
-- Pawl.CardSpec, which keeps the machinery.
module Pawl.EffectLintSpec where

-- Aliased Condition.Type, matching Pawl.Types.Count below and the project-wide
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- Dotted, because Pawl.Types.Keyword already holds the short alias here (the
-- The json sublibrary's own modules, for the CR 701.3a completeness cross-check
-- The logic module, alongside Pawl.Types.Modal below: unambiguous under one
-- alias because the two modules export disjoint names (TriggerSpec's
-- alone: it counts the atom in a card's ENCODED form, which is a traversal of the
-- convention (FilterSpec/CardSpec's Filter.Type note): Pawl.Engine.Condition may
-- hand-maintained one below.
-- later be imported and must not collide.
-- precedent), and Modal.allEffects is how this lint reaches an activated or
-- reverse of TriggerSpec's split).
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.
-- triggered ability's effects (Card.allEffects only reaches the spell).
-- whole card written by somebody else and so an independent witness to the
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Pawl.CardSpec (Framing (SourceHostFramed), MintedKind (MintedEmblem), anyFace, cardAuthoredEffects, cardFilters, cardReplacementEffects, cardResolutionEffects, conditionQuantities, copyTargetsRefs, durationConditions, effectFilters, effectMintedFaces, effectWithNested, faceModals, frame, framedSlotsReadSingly, grantedActivatedAbilities, grantedModifications, grantedTriggeredAbilities, instantLine, mintedFaces, mintedFacesTagged, objectRefFilters, oneFaced, overFaces, replacementEffectRiders, restrictionFilters, spellLine, triggerConditionFilters, triggerConditionSlots, vanillaFace)
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.QuantitySlot as QuantitySlot
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Resolve.Slots as Resolve
import qualified Pawl.Engine.Subtype as Subtype.Engine
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Earthbend as Earthbend
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForbidActivation as ForbidActivation
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.OrElse as OrElse
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.RandomCardInHand as RandomCardInHand
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.UntapRewrite as UntapRewrite
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR
import qualified Pawl.Types.ZoneScope as ZoneScope

-- Every Quantity a Duration holds, off the same enumeration. The Count walk's
-- twin, for the readers that need the Quantity itself rather than the Counts
-- under it.
durationQuantities :: Duration.Duration -> [Quantity.Type.Quantity]
durationQuantities = concatMap conditionQuantities . durationConditions

-- Every Quantity one effect carries: the ones nested in its ObjectRefs, and the
-- ones its own fields hold. ownCounts above is the Count twin, and neither
-- derives from the other -- quantityCounts collapses a Quantity to the Counts
-- beneath it, which loses exactly what a slot reader needs, Quantity.AgainstSlot
-- naming a slot and holding no Count of its own.
--
-- No recursion into a nested effect list (CR 615.5's rider, CR 608.2f's body, an
-- installed replacement's own effects): effectWithNested has already flattened
-- those into the list this is folded over, so an arm that descended would report
-- the same quantities twice. And no descent into a MINTED object's text either,
-- the boundary effectNestedEffects draws in so many words: a token's number is
-- evaluated in a resolution of its own.
--
-- Hand-maintained, ownCounts' caveat: a new OPCODE the compiler forces, since
-- this case is exhaustive; a new Quantity FIELD on an existing one it does not.
effectQuantities :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Quantity.Type.Quantity]
effectQuantities effect = concatMap Resolve.objectRefQuantities (Resolve.effectObjectRefs effect) <> ownQuantities effect

-- effectQuantities' half that is not an ObjectRef's. In ownCounts' arm order, so
-- the two are checkable side by side; the arms that differ from it are the entry
-- riders, whose per-kind count is a Quantity Resolve.slotsOf reads and ownCounts
-- leaves to the Filter sweep.
ownQuantities :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [Quantity.Type.Quantity]
ownQuantities effect = case effect of
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> fmap DamagePart.quantity (Foldable.toList parts)
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification _) -> durationQuantities duration <> Projection.quantitiesOf modification
  Effect.ChangeText {} -> []
  Effect.AddMana _ -> []
  Effect.Search (Search.MkSearch _ _ _ quantity _ _ _ _) -> Maybe.maybeToList quantity
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName _ -> []
  Effect.FromOutsideTheGame _ -> []
  Effect.ExileThisSpell -> []
  Effect.Bolster quantity -> [quantity]
  Effect.Amass (Amass.MkAmass quantity _) -> [quantity]
  Effect.Blight (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.Earthbend (Earthbend.MkEarthbend quantity _) -> [quantity]
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices _ _ quantity) -> [quantity]
  Effect.RestartGame _ -> []
  Effect.ControlPlayerNextTurn _ -> []
  Effect.Destroy {} -> []
  Effect.Sacrifice _ -> []
  -- The entry riders' counts, which Resolve.slotsOf reads and ownCounts does
  -- not: CR 122.6's per-kind number is a Quantity like any other.
  Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> Resolve.riderQuantities riders
  Effect.Draw (Draw.MkDraw _ quantity _) -> [quantity]
  Effect.Mill (Mill.MkMill _ quantity _ _) -> [quantity]
  Effect.Reveal {} -> []
  Effect.LookAt {} -> []
  Effect.Scry (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.Surveil (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.Fateseal (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.Explore {} -> []
  Effect.Discard subject -> case subject of
    Discard.Counted (CountedDiscard.MkCountedDiscard _ quantity _) -> [quantity]
    Discard.These {} -> []
  Effect.LoseLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.GainLife (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.ExchangeLifeTotals _ -> []
  Effect.SetLifeTotal (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed (PlayerQuantity.MkPlayerQuantity _ quantity) -> [quantity]
  Effect.DecreaseSpeed d -> [SpeedDecrease.quantity d]
  Effect.Create (Create.MkCreate quantity _ riders _ _) -> quantity : Resolve.riderQuantities riders
  Effect.Conjure (Conjure.MkConjure quantity _ _) -> [quantity]
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity _ riders) -> quantity : Resolve.riderQuantities riders
  Effect.BecomeCopy {} -> []
  Effect.CopyStackObject {} -> []
  Effect.Replace (Replace.MkReplace duration _ _ condition _) -> durationQuantities duration <> foldMap conditionQuantities condition
  Effect.SkipNextPhase (SkipNextPhase.MkSkipNextPhase _ _) -> []
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration _ _ _ _ _ quantity _) -> quantity : durationQuantities duration
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration _ _ _ _ _ _ _) -> durationQuantities duration
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration _ amount _ _ _ _ _) -> Maybe.maybeToList amount <> durationQuantities duration
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown _ listed) ->
    fmap (\(Power.MkPower quantity) -> quantity) (Maybe.maybeToList (FaceDownCharacteristics.power listed))
      <> fmap (\(Toughness.MkToughness quantity) -> quantity) (Maybe.maybeToList (FaceDownCharacteristics.toughness listed))
  Effect.TurnFaceUp _ -> []
  Effect.Fight _ -> []
  Effect.RemoveFromCombat _ -> []
  Effect.BecomesBlocked _ -> []
  Effect.Counter {} -> []
  Effect.PutCounters (PutCounters.MkPutCounters _ quantity _) -> [quantity]
  Effect.PutCountersFrom {} -> []
  Effect.MoveCounters (MoveCounters.MkMoveCounters _ kinds _ _) -> Maybe.maybeToList (MovedKinds.quantityOf kinds)
  Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ quantity _) -> [quantity]
  Effect.GainPlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> [quantity]
  Effect.RemovePlayerCounters (PlayerCounters.MkPlayerCounters _ _ quantity) -> [quantity]
  Effect.PayAnyEnergy _ -> []
  Effect.Tap _ -> []
  Effect.Untap _ -> []
  Effect.Detain _ -> []
  Effect.Goad _ -> []
  Effect.MakePlotted _ -> []
  Effect.DoesNotUntapNext _ -> []
  Effect.Transform _ -> []
  Effect.Convert _ -> []
  Effect.Meld (Meld.MkMeld _ _) -> []
  Effect.PhaseOut _ -> []
  Effect.AddPhases _ -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef duration _) -> durationQuantities duration
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ _) -> durationQuantities duration
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration _ _) -> durationQuantities duration
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration _) -> durationQuantities duration
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock duration _) -> durationQuantities duration
  Effect.ForbidActivation (ForbidActivation.MkForbidActivation duration _) -> durationQuantities duration
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration _ _) -> durationQuantities duration
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration _ _) -> durationQuantities duration
  Effect.CreateEmblem _ -> []
  Effect.BecomeMonarch _ -> []
  Effect.TakeTheInitiative _ -> []
  Effect.Designate (Designate.MkDesignate _ _) -> []
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> []
  Effect.Unsuspect _ -> []
  Effect.SetHalfLocked {} -> []
  Effect.Evolve _ -> []
  Effect.Mentor _ -> []
  Effect.Train _ -> []
  Effect.ItBecomes _ -> []
  Effect.ExileUntilMonarch _ -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach _ -> []
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.PlaySubgame _ -> []
  Effect.ChoosePlayer _ -> []
  Effect.ChooseOpponentAtRandom _ -> []
  Effect.RollDie rollDie -> RollDie.count rollDie : Maybe.maybeToList (RollDie.modifier rollDie)
  Effect.FlipCoin flipCoin -> [FlipCoin.count flipCoin]
  Effect.TakeExtraTurn takeExtraTurn -> [TakeExtraTurn.count takeExtraTurn]
  Effect.ShuffleIntoLibrary {} -> []
  Effect.Shuffle {} -> []
  Effect.OfferCast {} -> []
  Effect.GrantPlayFromExile grant -> durationQuantities (GrantPlayFromExile.duration grant)
  Effect.ForEach {} -> []

-- The shapes CR 208.1 and CR 208.2 allow in a printed power or toughness box:
-- "two numbers separated by a slash", or a value that "includes a star (*)" --
-- so a Literal, a bare Star, and the Plus that composes them into Tarmogoyf's
-- 1+*. Exhaustive rather than a fallthrough: a new Quantity constructor must
-- say whether a card can print it, and -Werror is what asks.
printedBoxQuantity :: Quantity.Type.Quantity -> Bool
printedBoxQuantity quantity = case quantity of
  Quantity.Type.Literal _ -> True
  Quantity.Type.Star -> True
  -- CR 208.2's composite box. Recursive on both sides, so 1+* is accepted and
  -- 1 + "the number of creatures you control" is not.
  Quantity.Type.Plus (Plus.MkPlus left right) -> printedBoxQuantity left && printedBoxQuantity right
  Quantity.Type.ManaValue -> False
  Quantity.Type.Power -> False
  Quantity.Type.Toughness -> False
  Quantity.Type.InSlot _ -> False
  Quantity.Type.Halved {} -> False
  Quantity.Type.Negate {} -> False
  Quantity.Type.Count {} -> False
  Quantity.Type.ManaCount {} -> False
  Quantity.Type.LifeTotal {} -> False
  Quantity.Type.Speed {} -> False
  Quantity.Type.IsMonarch {} -> False
  Quantity.Type.IsStartingPlayer {} -> False
  Quantity.Type.IsActivePlayer {} -> False
  Quantity.Type.PlayerCounters {} -> False
  Quantity.Type.ObjectCounters {} -> False
  Quantity.Type.ObjectCountersOfAnyKind -> False
  Quantity.Type.HasDesignation {} -> False
  Quantity.Type.ClassLevel -> False
  Quantity.Type.WasKicked -> False
  Quantity.Type.TimesKickedWith {} -> False
  Quantity.Type.TagWasSpent {} -> False
  Quantity.Type.WasToken -> False
  Quantity.Type.WasBlocking -> False
  Quantity.Type.DamageDealtToThisTurn -> False
  Quantity.Type.OpponentsAttacked {} -> False
  Quantity.Type.CardsDiscardedThisTurn {} -> False
  Quantity.Type.LifeGainedThisTurn {} -> False
  Quantity.Type.PlayersDealtDamageThisTurn {} -> False
  Quantity.Type.DamageDealtToPlayersThisTurn {} -> False
  Quantity.Type.SpellsCastLastTurn {} -> False
  Quantity.Type.DungeonsCompleted {} -> False
  Quantity.Type.CompletedDungeon {} -> False
  Quantity.Type.EnteredThisTurn -> False
  Quantity.Type.EnteredFrom {} -> False
  Quantity.Type.WasCastFrom {} -> False
  Quantity.Type.BlockersBeyondFirst -> False
  -- ENGINE-ONLY (Pawl.Types.Quantity's own header): no printed box may
  -- contain it, which this lint is what enforces.
  Quantity.Type.StationMeasure -> False
  Quantity.Type.AgainstSlot {} -> False
  -- AgainstSlot's answer: CR 607.2a's linked pile is read off the board, which
  -- a printed box (CR 208.2) cannot name.
  Quantity.Type.AgainstCardsExiledWith {} -> False

-- Does this face print a power or toughness box CR 208.1/208.2 could not print?
--
-- Projection.baseCharacteristics is why it matters: it evaluates the printed box
-- at the projection's SEED, before any layer has run. A Count there reads a board
-- nobody has described yet -- over Scope.InZone every candidate's view is
-- Nothing, so nothing is kept and Aggregation.Members aggregates the empty list
-- to 0; over Scope.InHistory and Scope.OverPlayers the seed view is bypassed
-- altogether (Pawl.Engine.Count.evaluate reads snapshots and players directly),
-- so the box reads LIVE state and changes under the object. Either answer is a
-- number the card never printed; see #156.
--
-- Scoped to a card's own faces through `anyFace`, and that scope is load-bearing:
-- CR 111.3 lets the creating effect define a token's power and toughness by a
-- computed value -- Rootha, Mastering the Moment's "create an X/X ... token,
-- where X is the greatest mana value among instant and sorcery spells you've cast
-- this turn" -- which Resolve.bakeTokenCharacteristics settles into a Literal as
-- the token is created. So the wire format has to keep permitting the shape, and
-- this is a claim about what a CARD prints rather than one the codec could make.
printedBoxOffends :: Face.Face Card.Type.Card -> Bool
printedBoxOffends card =
  not
    ( all
        printedBoxQuantity
        ( fmap Power.unwrap (Maybe.maybeToList (Face.power card))
            <> fmap Toughness.unwrap (Maybe.maybeToList (Face.toughness card))
        )
    )

-- CR 614.1a: a token replacement that neither scales nor appends replaces the
-- event with itself, which no printing says. Both fields are elided on the
-- wire, so the codec cannot refuse the pair; card data is held to it here.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum.
idleTokenRowOffends :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
idleTokenRowOffends replacement = case replacement of
  ReplacementEffect.TokenR (TokenR.MkTokenR _ scaling plus) -> Maybe.isNothing scaling && Maybe.isNothing plus
  ReplacementEffect.DamageR {} -> False
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.ZoneChangeR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.TurnUpR {} -> False
  ReplacementEffect.UntapR _ -> False
  ReplacementEffect.LifeLossR {} -> False
  ReplacementEffect.LifeGainR {} -> False
  ReplacementEffect.DrawR {} -> False
  ReplacementEffect.DrawCountR {} -> False
  ReplacementEffect.PhaseR _ -> False

-- The non-vacuity half of idleTokenRowOffends' lint, isPhaseR's shape.
isTokenR :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
isTokenR replacement = case replacement of
  ReplacementEffect.TokenR _ -> True
  _ -> False

-- #437: does this replacement carry a PhasePattern with a BAKED player in it?
--
-- PhasePattern.whosePhase is meant to be runtime-only. Nothing is the value card
-- data writes -- Eon Hub's "players skip their upkeep steps" is symmetric and
-- names nobody -- and Just is baked by the engine out of a player a resolution
-- named (Resolve.applyEffect's SkipNextPhase arm, Fatigue's target) or out of a
-- pending extra turn (Replacement.installTurnSkips). Card data cannot name a
-- player at all.
--
-- Nothing enforced that split. Codec.PhasePattern is structural over the record,
-- so it accepts a Just from card JSON -- and a card file could write
-- `"whosePhase": 1`, which is meaningless. Player 1 is a seat in some game, not
-- a fact about a printed card, and the skip would land on whoever happened to
-- hold that id.
--
-- A LINT rather than a type-level split, which is the call #199 already records
-- for the sibling case (Modification.SetController's baked PlayerId, likewise
-- accepted by its codec and likewise kept out of card data by a lint here).
--
-- What makes the split expensive is NOT the codec -- Pawl.Codec.ActiveReplacement
-- round-trips a baked pattern through the same ReplacementEffect codec a card
-- file uses (#126). It is that ReplacementEffect is the carrier for both halves:
-- Face.replacementEffects, which a card authors, and ActiveReplacement.effect,
-- which the engine bakes. A card-side / runtime-side split the way Duration and
-- Expiry are split would therefore have to split or parameterize that whole sum,
-- not just PhasePattern. Modification is shared exactly the same way, between
-- StaticAbility.modifications and a stored ContinuousEffect, which is why the
-- two cases want ONE answer rather than two -- what the issue asks be decided
-- once.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: a second
-- pattern-carrying replacement must break this build rather than silently pass.
phasePatternOffends :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
phasePatternOffends replacement = case replacement of
  ReplacementEffect.PhaseR phasePattern -> Maybe.isJust (PhasePattern.whosePhase phasePattern)
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.ZoneChangeR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.DamageR {} -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.TokenR {} -> False
  ReplacementEffect.TurnUpR {} -> False
  ReplacementEffect.UntapR _ -> False
  ReplacementEffect.LifeLossR {} -> False
  ReplacementEffect.LifeGainR {} -> False
  ReplacementEffect.DrawR {} -> False
  ReplacementEffect.DrawCountR {} -> False

-- Every replacement shape the codec accepts and no card may author, for
-- phasePatternOffends' reason and one more. A card cannot name an ObjectId or a
-- PlayerId, so the recipient a shield covers -- CR 615.7's, and CR 615.3's
-- unbounded one -- is for Resolve's prevention arms to write, CR 609.7a's chosen
-- SOURCE beside it likewise, and CR 615.7's remaining amount rides the same
-- carrier. A card says "a source of your choice" through the chosenSource of
-- Effect.PreventNextDamage, Effect.PreventAllDamage or Effect.RedirectDamage,
-- which is a Filter and names nothing. CR 122.1c's pair is engine-only for a
-- different reason: a RULE creates it off a permanent's counters, so a card
-- printing either half would be claiming an ability no rule gives it.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum.
engineOnlyOffends :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
engineOnlyOffends replacement = case replacement of
  -- `whatRecipient` and `whoRecipient` beside it are the PRINTED halves and are
  -- not swept: a card may describe the recipient it shields (Stormwild Capridor)
  -- and name CR 109.5's relation to the row's controller (Divine Deflection's
  -- "to you"), it just may not name one by id. The REWRITE's destination is the
  -- same split one field over -- DamageRewrite.RedirectMatching describes and is
  -- printed (Pariah), DamageRewrite.Redirect names and is swept.
  ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern rewrite _) ->
    Maybe.isJust (DamagePattern.whichRecipient damagePattern) || Maybe.isJust (DamagePattern.whichSource damagePattern) || engineMintedDamage rewrite
  -- CR 122.1c's destruction half is engine-minted for the same reason its damage
  -- half is, so the sweep reaches it through this arm rather than through a lint
  -- of its own.
  ReplacementEffect.DestructionR rewrite -> engineMintedDestruction rewrite
  -- CR 122.1d's row is engine-minted for CR 122.1c's reason, and the WHOLE arm
  -- rather than one rewrite of it: printed regeneration shares DestructionR, so
  -- that arm needs a per-rewrite test, where nothing a card may print replaces an
  -- untap at all.
  ReplacementEffect.UntapR _ -> True
  ReplacementEffect.PhaseR _ -> False
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.ZoneChangeR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.TokenR {} -> False
  -- Every printing writes this arm whole -- CR 109.5's relation, the cause, and a
  -- rewrite that is a printed floor, a printed scaling or a printed instead-clause
  -- name nothing an engine has to bake -- so nothing here is engine-only.
  ReplacementEffect.LifeLossR {} -> False
  -- Boon Reflection writes this arm whole -- CR 109.5's relation and a printed
  -- scaling name nothing an engine has to bake. LifeLossR's answer, and for its
  -- reason.
  ReplacementEffect.LifeGainR {} -> False
  -- Words of Worship writes this arm whole -- CR 109.5's relation and a printed
  -- amount of life name nothing an engine has to bake -- so nothing here is
  -- engine-only. LifeLossR's answer, and for its reason.
  ReplacementEffect.DrawR {} -> False
  ReplacementEffect.DrawCountR {} -> False
  ReplacementEffect.TurnUpR {} -> False

-- Is this damage rewrite one the ENGINE mints and no card may print? Three of
-- them, for three rules:
--
--   * CR 615.7 versus CR 615.10 -- a counted shield is generated "by the resolution
--     of a spell or ability", never by the static ability a card prints.
--   * CR 122.1c -- the prevention shield counters create is created by the RULE, off
--     a permanent's counters (Pawl.Engine.Projection.shieldOf), so a card printing
--     it would be claiming a static ability the rule does not give it.
--   * CR 614.9 -- a redirection whose destination is a Recipient names an
--     ObjectId or a PlayerId, which is card data's own prohibition; Resolve's
--     RedirectDamage arm is the one producer (#2378).
--
-- A printed one either way would be a rule that does not exist.
engineMintedDamage :: DamageRewrite.DamageRewrite -> Bool
engineMintedDamage rewrite = case rewrite of
  DamageRewrite.PreventNext _ -> True
  DamageRewrite.PreventRemovingShieldCounter -> True
  DamageRewrite.PreventAll -> False
  -- CR 615.10's counterpart to that first rule: a shield with an amount that a
  -- STATIC ability prints (Temple Altisaur), which names nothing an engine has to
  -- bake.
  DamageRewrite.PreventAllBut _ -> False
  DamageRewrite.SetAmount _ -> False
  DamageRewrite.Scale _ -> False
  DamageRewrite.Redirect _ -> True
  -- Redirect with CR 615.7's countdown: the same baked Recipient, and the
  -- remaining amount beside it is engine-written for PreventNext's reason.
  DamageRewrite.RedirectNext _ _ -> True
  -- CR 614.9's redirection with the destination DESCRIBED rather than named,
  -- which is exactly the shape a card may print: Pariah's "dealt to enchanted
  -- creature instead" is a Filter and names no id.
  DamageRewrite.RedirectMatching _ -> False

-- The destruction half of the same question. CR 701.19a's regeneration IS printed
-- (Drudge Skeletons), where CR 122.1c's removal is minted.
engineMintedDestruction :: DestructionRewrite.DestructionRewrite -> Bool
engineMintedDestruction rewrite = case rewrite of
  DestructionRewrite.RemoveShieldCounter -> True
  DestructionRewrite.Regenerate -> False

-- The non-vacuity half of the same lint: is this the replacement that carries a
-- PhasePattern at all? A wildcard is right here, where it is not above -- this
-- asks "did the sweep have anything to look at", not "is it well-formed".
isPhaseR :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
isPhaseR replacement = case replacement of
  ReplacementEffect.PhaseR _ -> True
  _ -> False

-- phasePatternOffends one replacement class over, for the other field the codec
-- accepts and only the engine writes: TurnUpR.requiring, CR 702.37b's "if its
-- megamorph cost was paid to turn it face up". That is a RULE's condition, minted
-- by Pawl.Engine.Keyword.mintedReplacementsFor; a card's own CR 614.1e clause
-- states no procedure and applies down every road (Bubble Smuggler); see #987.
--
-- Exhaustive rather than a wildcard, phasePatternOffends' discipline: a second
-- engine-baked field on this class must break this build rather than pass.
turnUpRequiringOffends :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
turnUpRequiringOffends replacement = case replacement of
  ReplacementEffect.TurnUpR turnUpR -> Maybe.isJust (TurnUpR.requiring turnUpR)
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.ZoneChangeR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.DamageR {} -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.TokenR {} -> False
  ReplacementEffect.UntapR _ -> False
  ReplacementEffect.LifeLossR {} -> False
  ReplacementEffect.LifeGainR {} -> False
  ReplacementEffect.DrawR {} -> False
  ReplacementEffect.DrawCountR {} -> False
  ReplacementEffect.PhaseR _ -> False

-- isPhaseR's twin: did the sweep above have anything to look at? A wildcard for
-- the same reason.
isTurnUpR :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
isTurnUpR replacement = case replacement of
  ReplacementEffect.TurnUpR _ -> True
  _ -> False

-- CR 615.5: "SOME PREVENTION EFFECTS also include an additional effect." So a
-- rider beside a rewrite that prevents nothing is a shape the type admits and
-- the rule does not -- Furnace of Rath's doubling with counters hung off it is
-- not a card. Stormwild Capridor is the pool's one card printing such a rider;
-- other cards print a PreventAll without one (Phyrexian Vindicator).
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: an arm
-- that gains a riders field of its own must be classified here.
riderWithoutPreventionOffends :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
riderWithoutPreventionOffends replacement = case replacement of
  ReplacementEffect.DamageR (DamageR.MkDamageR _ rewrite riders) -> not (null riders) && not (preventsDamage rewrite)
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.ZoneChangeR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.TokenR {} -> False
  ReplacementEffect.TurnUpR {} -> False
  ReplacementEffect.UntapR _ -> False
  ReplacementEffect.LifeLossR {} -> False
  ReplacementEffect.LifeGainR {} -> False
  ReplacementEffect.DrawR {} -> False
  ReplacementEffect.DrawCountR {} -> False
  ReplacementEffect.PhaseR _ -> False

-- CR 615.1a: does this rewrite use the word "prevent"? engineMintedDamage's
-- shape, and the same classification Pawl.Engine.Replacement.prevents makes --
-- restated here rather than imported so the lint holds even if that function is
-- what a change gets wrong.
preventsDamage :: DamageRewrite.DamageRewrite -> Bool
preventsDamage rewrite = case rewrite of
  DamageRewrite.PreventAll -> True
  DamageRewrite.PreventNext _ -> True
  DamageRewrite.PreventAllBut _ -> True
  DamageRewrite.PreventRemovingShieldCounter -> True
  DamageRewrite.SetAmount _ -> False
  DamageRewrite.Scale _ -> False
  DamageRewrite.Redirect _ -> False
  DamageRewrite.RedirectNext _ _ -> False
  DamageRewrite.RedirectMatching _ -> False

-- The non-vacuity half of riderWithoutPreventionOffends' lint, isPhaseR's shape.
hasRider :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
hasRider = not . null . replacementEffectRiders

-- CR 701.24a shuffles a LIBRARY, so a redirect saying to shuffle has to be
-- sending the card into one. The type cannot say that -- the destination and the
-- rider are two independent fields -- so card data is held to it here, as CR
-- 615.5's rider is one lint up.
shufflingOutsideLibraryOffends :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
shufflingOutsideLibraryOffends replacement = case replacement of
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR _ destination _ shuffling) -> shuffling && destination /= Zone.Library
  ReplacementEffect.DamageR {} -> False
  ReplacementEffect.CounterR {} -> False
  ReplacementEffect.EntryR {} -> False
  ReplacementEffect.DestructionR _ -> False
  ReplacementEffect.TokenR {} -> False
  ReplacementEffect.TurnUpR {} -> False
  ReplacementEffect.UntapR _ -> False
  ReplacementEffect.LifeLossR {} -> False
  ReplacementEffect.LifeGainR {} -> False
  ReplacementEffect.DrawR {} -> False
  ReplacementEffect.DrawCountR {} -> False
  ReplacementEffect.PhaseR _ -> False

-- The non-vacuity half of shufflingOutsideLibraryOffends' lint, isPhaseR's shape.
hasZoneChangeRider :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
hasZoneChangeRider replacement = case replacement of
  ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR _ _ revealing shuffling) -> revealing || shuffling
  _ -> False

-- The non-vacuity half of engineOnlyOffends' lint, isPhaseR's shape.
isDamageR :: ReplacementEffect.ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Bool
isDamageR replacement = case replacement of
  ReplacementEffect.DamageR {} -> True
  _ -> False

-- CR 615.1: a prevention effect acts like a "shield" around whatever it's
-- affecting, so one affecting nothing is not a prevention effect at all, and CR
-- 614.9's redirection covers a side the same way. Each field that says what a
-- shield covers is independently optional -- a card NAMES its recipients
-- (Mending Hands' "any target", Carom's "target creature"), DESCRIBES them
-- (Divine Deflection's and Harm's Way's "you and/or permanents you control"),
-- or names a SOURCE instead (Dovin, Hand of Control's "dealt by target
-- permanent") -- so no schema can say that at least one has to be there, and
-- Pawl.Engine.Resolve's three arms build one row per named object and one row
-- for a description, leaving a shield or redirection that names nothing with no
-- row to install and the card silently doing less than it printed.
--
-- The UNBOUNDED shield has a fourth spelling, which is why its arm below reads
-- one more field than the other two: a card may name only the SOURCE and leave
-- the recipient side empty (Pay No Heed's "prevent all damage a source of your
-- choice would deal this turn"), and that shield covers every recipient rather
-- than none. It still has to name that source, because CR 609.7a's chosen id is
-- the only thing the opcode bakes that card data cannot write itself: a shield
-- naming neither a recipient nor a chosen source is Fog's and Luminesce's shape,
-- authored directly as an Effect.Replace carrying a DamageR.
shieldNamingNothingOffends :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
shieldNamingNothingOffends effect = case effect of
  -- CR 615.7's counted shield has no source-only spelling to admit here: no
  -- printing counts an amount down while naming only a source. The recipient-less
  -- printings that name one -- Pilgrim of Justice, Penance -- are CR 615.8's
  -- "the next time ... would deal damage", a rewrite
  -- pawl does not have (gap #3206), and this arm is what keeps one out of the
  -- corpus meanwhile.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ ref whatRecipient whoRecipient _ _ _) ->
    Maybe.isNothing ref && Maybe.isNothing whatRecipient && Maybe.isNothing whoRecipient
  -- `direction` IS read here, because it decides whether `whatRecipient` can
  -- stand in for `ref`. Beside DealtTo the two are alternative spellings of the
  -- covered side, and Pawl.Engine.Resolve builds one row per named recipient or
  -- one row for a description, so either alone installs something. Beside DealtBy
  -- `ref` names CR 120.1's SOURCE and the description is the far END of the
  -- event: that branch folds over the ids the ref named and conjoins the
  -- description onto each row, never making a row of its own, so a by-direction
  -- shield with no ref installs nothing however it describes its recipients.
  -- `whatSource` is not a substitute on either side -- that one narrows a row
  -- rather than creating one, and `And []` is its ordinary value. `chosenSource`
  -- IS one, but only beside DealtTo: that branch installs the source-only
  -- shield's lone row, where the by-direction branch folds over the ids the ref
  -- named and has nothing to fold.
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ ref whatRecipient direction chosenSource _ _) ->
    Maybe.isNothing ref && case direction of
      DamageDirection.DealtBy -> True
      DamageDirection.DealtTo -> Maybe.isNothing whatRecipient && Maybe.isNothing chosenSource
  -- CR 614.9's redirection covers a side the same two ways -- Carom names it,
  -- Harm's Way describes it -- and one saying neither installs nothing.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage _ _ _ from whatRecipient whoRecipient _ _) ->
    Maybe.isNothing from && Maybe.isNothing whatRecipient && Maybe.isNothing whoRecipient
  _ -> False

-- The non-vacuity half of that lint, isPhaseR's shape.
isPreventionShield :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
isPreventionShield effect = case effect of
  Effect.PreventNextDamage {} -> True
  Effect.PreventAllDamage {} -> True
  Effect.RedirectDamage {} -> True
  _ -> False

-- A shield pinned to the SOURCE side of the damage event, one field narrower
-- than isPreventionShield.
isByDirectionShield :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Bool
isByDirectionShield effect = case effect of
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ _ _ direction _ _ _) -> direction == DamageDirection.DealtBy
  _ -> False

-- Every PlayerRef a CLAUSE of this face holds: CR 118.12a's payer and CR 603.5's
-- asker. Both sit on Pawl.Types.Clause rather than inside any effect, so neither
-- effect traversal reaches them; Pawl.Engine.Resolve.modeSlots reads exactly this
-- pair, through the same playerRefSlots classification.
--
-- Over the GRANTED carriers as well as the printed ones, cardResolutionEffects'
-- scope: CR 613.1f's quoted ability is text this face printed, so a clause of one
-- is this face's clause.
faceClausePlayerRefs :: Face.Face Card.Type.Card -> [PlayerRef.PlayerRef]
faceClausePlayerRefs card =
  concatMap clausePlayerRefs
    . concatMap (Foldable.toList . Mode.clauses)
    . concatMap (Foldable.toList . Modal.modes)
    $ faceModals card
      <> fmap ActivatedAbility.modal (grantedActivatedAbilities card)
      <> fmap TriggeredAbility.modal (grantedTriggeredAbilities card)

-- One clause's three: the player CR 118.12's cost is offered to, the player CR
-- 603.5's "may" is asked of, and the player CR 608.2d's either-or is announced
-- by. A clause with no gate offers nobody, a mandatory one asks nobody, and one
-- printing no either-or has no branch to announce.
clausePlayerRefs :: Clause.Clause card ability -> [PlayerRef.PlayerRef]
clausePlayerRefs clause =
  fmap PayGate.payer (Maybe.maybeToList (Clause.payGate clause))
    <> fmap OrElse.chooser (Maybe.maybeToList (Clause.orElse clause))
    <> case Clause.optionality clause of
      Optionality.Mandatory -> []
      Optionality.Optional ref -> [ref]

-- Does this carrier pair CR 615.12's "damage can't be prevented" with a
-- scope narrower than the whole table?
--
-- Pawl.Engine.PlayerEffect.unpreventable asks no player, because CR 615.12's
-- sentence is about a damage EVENT and names no player to ask about. It gathers
-- from the whole board instead -- "which seats have such an effect applying?" --
-- which admits the same events as "it applies" exactly when the scope is
-- PlayerScope.EachPlayer, and reads a narrower one as board-wide. This lint is
-- what makes that exactness a property of the pool rather than a hope: no card
-- may author the scope the fold cannot see.
--
-- The rule, not just the engine, is what backs the ban. Every printed narrowing
-- of CR 615.12 narrows by a quality of the damage EVENT and not by a player:
-- Excruciator's source, Frenzied Baloth's kind, Questing Beast's source
-- relation, Whippoorwill's recipient. That axis is the DamagePattern the
-- constructor now carries, never a carrier scope -- a scope names which players
-- an effect applies TO, and a damage event between two creatures applies to no
-- player at all.
--
-- Asked of CR 614.9's redirection twin too, and for the identical reason: its
-- subject is a damage event as well, so its narrowings ride in the same pattern.
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum: a second
-- player effect whose reading depends on its scope must break this build.
unpreventableScopeOffends :: AffectedPlayers.AffectedPlayers SlotName.SlotName -> PlayerEffect.PlayerEffect -> Bool
unpreventableScopeOffends scope playerEffect = case playerEffect of
  -- A NAMED seat is a narrowing like any other, and the strictest one there is:
  -- "target player" reaches one player where CR 615.12 reaches the whole table.
  PlayerEffect.DamageCantBePrevented _ -> scope /= AffectedPlayers.Scoped PlayerScope.EachPlayer
  -- CR 614.9's twin, banned on the same axis for the same reason: its subject is
  -- a damage event too, so Pawl.Engine.PlayerEffect.unredirectable's board-wide
  -- fold is exact only while EachPlayer is the one scope a card may write.
  PlayerEffect.DamageCantBeRedirected _ -> scope /= AffectedPlayers.Scoped PlayerScope.EachPlayer
  PlayerEffect.CantSearchLibraries _ -> False
  PlayerEffect.HasProtectionFromChosenName -> False
  PlayerEffect.CantBecomeMonarch -> False
  -- Every other arm IS asked about a player, so its scope is read exactly as
  -- written and any of the three is legitimate: Rule of Law and Thalia say
  -- EachPlayer, Silence's stored prohibition says Opponents, and Prowling
  -- Serpopard says You.
  PlayerEffect.IncreaseSpellCost {} -> False
  PlayerEffect.IncreaseActivationCost {} -> False
  PlayerEffect.ReduceSpellCost {} -> False
  PlayerEffect.ReduceActivationCost {} -> False
  PlayerEffect.AddActivationCost {} -> False
  PlayerEffect.AddSpellCost {} -> False
  PlayerEffect.CantCastSpells -> False
  PlayerEffect.CantActivateAbilities -> False
  PlayerEffect.CantCastMoreThan _ -> False
  PlayerEffect.CantCastChosenName -> False
  PlayerEffect.CantPlayLandChosenName -> False
  PlayerEffect.PlayAdditionalLands _ -> False
  PlayerEffect.NoMaximumHandSize -> False
  PlayerEffect.SetMaximumHandSize _ -> False
  PlayerEffect.IncreaseMaximumHandSize _ -> False
  PlayerEffect.ReduceMaximumHandSize _ -> False
  PlayerEffect.DontLoseUnspentMana _ -> False
  PlayerEffect.SpendManaAsThough _ -> False
  PlayerEffect.CantBeTargetedBy _ -> False
  PlayerEffect.CastAsThoughItHadFlash _ -> False
  PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
  PlayerEffect.CantBeCountered _ -> False
  PlayerEffect.CantCastMatching _ -> False
  PlayerEffect.CastOnlyAtSorcerySpeed -> False
  PlayerEffect.CantPlayLands -> False
  PlayerEffect.CastFrom _ -> False
  PlayerEffect.PlayLandsFrom _ -> False
  PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
  PlayerEffect.CantGetCounters _ -> False
  PlayerEffect.StateCoinFlip _ -> False

-- The OTHER half of the same carrier -- and of CR 614.9's twin beside it, whose
-- narrowing rides in the same type -- now that the narrowing is a DamagePattern:
-- does this card author a field of that pattern the engine bakes?
--
-- `whichRecipient` and `whichSource` are the two, and for engineOnlyOffends'
-- reason -- a card cannot name an ObjectId or a PlayerId. Whippoorwill's "damage
-- that would be dealt to THAT CREATURE" does name a recipient, but the creature
-- is the one its resolution chose, so the pattern is the engine's to bake and
-- never the card file's to write, and CR 609.7a's chosen source is the same kind
-- of answer. `whichKind`, `whatSource`, `whatRecipient` and `whoRecipient` are
-- all authorable here; the first two are exactly what Frenzied Baloth and
-- Excruciator print, and the last two describe a recipient rather than naming
-- one -- Lava Burst's "if Lava Burst would deal damage to a creature" writes
-- `whatRecipient`, and no printing in the pool writes `whoRecipient` on THIS
-- carrier.
--
-- Not implemented: no resolution bakes a recipient into THIS pattern, the way
-- Resolve's prevention arms bake one into a shield's, so the field has no
-- producer on either side yet (#845).
--
-- Exhaustive rather than a wildcard, this file's discipline for a sum.
unpreventablePatternOffends :: PlayerEffect.PlayerEffect -> Bool
unpreventablePatternOffends playerEffect = case playerEffect of
  PlayerEffect.DamageCantBePrevented pattern_ -> Maybe.isJust (DamagePattern.whichRecipient pattern_) || Maybe.isJust (DamagePattern.whichSource pattern_)
  PlayerEffect.DamageCantBeRedirected pattern_ -> Maybe.isJust (DamagePattern.whichRecipient pattern_) || Maybe.isJust (DamagePattern.whichSource pattern_)
  PlayerEffect.CantSearchLibraries _ -> False
  PlayerEffect.HasProtectionFromChosenName -> False
  PlayerEffect.CantBecomeMonarch -> False
  PlayerEffect.IncreaseSpellCost {} -> False
  PlayerEffect.IncreaseActivationCost {} -> False
  PlayerEffect.ReduceSpellCost {} -> False
  PlayerEffect.ReduceActivationCost {} -> False
  PlayerEffect.AddActivationCost {} -> False
  PlayerEffect.AddSpellCost {} -> False
  PlayerEffect.CantCastSpells -> False
  PlayerEffect.CantActivateAbilities -> False
  PlayerEffect.CantCastMoreThan _ -> False
  PlayerEffect.CantCastChosenName -> False
  PlayerEffect.CantPlayLandChosenName -> False
  PlayerEffect.PlayAdditionalLands _ -> False
  PlayerEffect.NoMaximumHandSize -> False
  PlayerEffect.SetMaximumHandSize _ -> False
  PlayerEffect.IncreaseMaximumHandSize _ -> False
  PlayerEffect.ReduceMaximumHandSize _ -> False
  PlayerEffect.DontLoseUnspentMana _ -> False
  PlayerEffect.SpendManaAsThough _ -> False
  PlayerEffect.CantBeTargetedBy _ -> False
  PlayerEffect.CastAsThoughItHadFlash _ -> False
  PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
  PlayerEffect.CantBeCountered _ -> False
  PlayerEffect.CantCastMatching _ -> False
  PlayerEffect.CastOnlyAtSorcerySpeed -> False
  PlayerEffect.CantPlayLands -> False
  PlayerEffect.CastFrom _ -> False
  PlayerEffect.PlayLandsFrom _ -> False
  PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
  PlayerEffect.CantGetCounters _ -> False
  PlayerEffect.StateCoinFlip _ -> False

-- The non-vacuity half of both lints above: is this a damage-event prohibition
-- at all -- CR 615.12's or CR 614.9's? A wildcard is right here, where it is not
-- above -- this asks "did the sweep have anything to look at", not "is it
-- well-formed". isPhaseR's shape.
isUnpreventable :: PlayerEffect.PlayerEffect -> Bool
isUnpreventable playerEffect = case playerEffect of
  PlayerEffect.DamageCantBePrevented _ -> True
  PlayerEffect.DamageCantBeRedirected _ -> True
  _ -> False

-- The pattern that narrows nothing: Spider-Punk's, and what a fixture below
-- restates a card's effect to when the pattern is not the axis under test.
anyDamage :: DamagePattern.DamagePattern
anyDamage =
  DamagePattern.MkDamagePattern
    { DamagePattern.whichKind = Nothing,
      DamagePattern.whatSource = Filter.Type.And [],
      DamagePattern.whatRecipient = Nothing,
      DamagePattern.whoRecipient = Nothing,
      DamagePattern.whichRecipient = Nothing,
      DamagePattern.whichSource = Nothing
    }

-- Every (scope, player effect) pair a card authors, on BOTH of the carriers
-- Pawl.Engine.PlayerEffect.applying folds together: the printed static ability
-- (CR 604.2, Spider-Punk) and the stored one a resolution installs (CR 611.2c,
-- Silence -- and Skullcrack's "damage can't be prevented this turn", whenever
-- the pool gains it).
cardPlayerScopes :: Face.Face Card.Type.Card -> [(AffectedPlayers.AffectedPlayers SlotName.SlotName, PlayerEffect.PlayerEffect)]
cardPlayerScopes card =
  fmap printedPlayerScope (Face.playerAbilities card)
    <> Maybe.mapMaybe storedPlayerScope (cardResolutionEffects card)

-- The printed carrier's pair: the record's two fields, in the order the lint
-- above reads them. Wrapped as Scoped so the two carriers read as one list --
-- a static ability has no slot, so it can never be the other arm.
printedPlayerScope :: PlayerStaticAbility.PlayerStaticAbility -> (AffectedPlayers.AffectedPlayers SlotName.SlotName, PlayerEffect.PlayerEffect)
printedPlayerScope ability = (AffectedPlayers.Scoped (PlayerStaticAbility.scope ability), PlayerStaticAbility.effect ability)

-- The stored carrier's pair, or Nothing for the overwhelming majority of
-- effects, which install no continuous effect on the player axis at all. A
-- wildcard here rather than one arm per effect, matching the control lint's own
-- sweep over this sum: Pawl.Types.Effect is the open half's alphabet, and a new
-- resolution effect is not a new player carrier.
storedPlayerScope :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe (AffectedPlayers.AffectedPlayers SlotName.SlotName, PlayerEffect.PlayerEffect)
storedPlayerScope effect = case effect of
  Effect.AffectPlayers (AffectPlayers.MkAffectPlayers _ scope playerEffect) -> Just (scope, playerEffect)
  _ -> Nothing

-- Which chooser-shaped ObjectRef arms Pawl.Engine.Resolve can ASK for at the
-- position this tags. A CR 608.2d choice is announced while the effect is
-- applied, so an opcode that never reaches the Game monad for its objects cannot
-- make one -- and today exactly three arms of Resolve do, over different subsets.
-- Named for those ARMS rather than for the opcodes: this is a property of
-- what Pawl.Engine.Resolve implements, not of the card's alphabet.
data Asks
  = -- | Read through Pawl.Engine.Resolve.Slots.objectRefObjects, which is pure and so
    -- raises no prompt: every ObjectRef position but the three below.
    AsksNothing
  | -- | Pawl.Engine.Resolve's Effect.MoveToZone gather, which runs in the Game
    -- monad. It asks the graveyard, hand, from-among, one-permanent and
    -- any-number arms; the random arm answers @pure []@ there (#1733).
    AsksMoveGather
  | -- | Pawl.Engine.Resolve's Effect.Reveal arm. It asks the from-among arm
    -- through chooseCardFromAmong and the random arm through
    -- Prompt.RandomObject, and falls through to the pure sweep for the two
    -- zone-keyed chosen arms.
    AsksRevealArm
  | -- | Pawl.Engine.Resolve's turnPermanentsOver gather, shared by Effect.Transform
    -- and Effect.Convert. It asks the any-number arm and nothing else: the four
    -- card-shaped chosen arms name cards in a graveyard, a hand or a group, and CR
    -- 701.27a turns over PERMANENTS.
    AsksTransformGather
  deriving (Eq, Show)

-- Whether an ObjectRef arm is a resolution-time QUESTION rather than a read --
-- the five arms Pawl.Types.ObjectRef documents as such, and exactly the five
-- Pawl.Engine.Resolve.Slots.objectRefObjects answers [] for.
--
-- ObjectRef.ChosenPlayer is NOT one, despite the name: the seat was chosen on
-- entry and is read out of stored state here. ObjectRef.EachCardFromAmong is not
-- one either -- "all" states no count and hands out no choice, so CR 608.2d has
-- nobody to ask.
--
-- Exhaustive with no `_`, so a new arm has to be classified rather than
-- defaulting to "a read".
chooserRef :: ObjectRef.ObjectRef -> Bool
chooserRef ref = case ref of
  ObjectRef.InSlot {} -> False
  ObjectRef.EachMatching {} -> False
  ObjectRef.EachCardInGraveyard {} -> False
  ObjectRef.EachCardInYourHand -> False
  ObjectRef.EachCardInHand {} -> False
  ObjectRef.EachCardInYourLibrary {} -> False
  ObjectRef.EachCardExiledWithSource {} -> False
  ObjectRef.EachSpell {} -> False
  ObjectRef.EachOnStack {} -> False
  ObjectRef.EachPlayer -> False
  ObjectRef.EachOpponent -> False
  ObjectRef.ChosenPlayer -> False
  ObjectRef.TopOfLibrary {} -> False
  ObjectRef.TopOfLibraryUntil {} -> False
  ObjectRef.TopOfGraveyard {} -> False
  ObjectRef.EachCardFromAmong {} -> False
  ObjectRef.ChosenCardInGraveyard {} -> True
  ObjectRef.ChosenCardInHand {} -> True
  ObjectRef.ChosenCardFromAmong {} -> True
  ObjectRef.RandomCardInHand {} -> True
  ObjectRef.AnyNumberMatching {} -> True
  ObjectRef.ChosenPermanent {} -> True
  ObjectRef.SourceAndChosenPermanent {} -> True

-- The asking matrix itself: whether the site an Asks names asks THIS arm. A
-- per-(site, arm) pair and not a per-site or per-arm predicate, because both
-- coarser readings admit a ref that names nothing -- MoveToZone's gather does not
-- ask the random arm (#1733), and Reveal's arm does not ask either zone-keyed
-- chosen one.
asksFor :: Asks -> ObjectRef.ObjectRef -> Bool
asksFor asks ref = case asks of
  AsksNothing -> False
  AsksMoveGather -> case ref of
    ObjectRef.ChosenCardInGraveyard {} -> True
    ObjectRef.ChosenCardInHand {} -> True
    ObjectRef.ChosenCardFromAmong {} -> True
    ObjectRef.ChosenPermanent {} -> True
    ObjectRef.SourceAndChosenPermanent {} -> True
    ObjectRef.AnyNumberMatching {} -> True
    _ -> False
  AsksRevealArm -> case ref of
    ObjectRef.ChosenCardFromAmong {} -> True
    ObjectRef.RandomCardInHand {} -> True
    _ -> False
  -- One arm and one only, which is what keeps the widening from weakening the
  -- guarantee: this site asks for the battlefield subset and for nothing else,
  -- so every (site, arm) pair the four arms above classify is unmoved.
  AsksTransformGather -> case ref of
    ObjectRef.AnyNumberMatching {} -> True
    _ -> False

-- Every ObjectRef position one effect holds, each tagged with the asking site
-- that reads it. effectFilters' sibling one field shallower: that traversal takes
-- each ref's Filters, this one takes the ref.
--
-- NOT recursive into a nested effect list, unlike effectFilters: every caller
-- comes through cardAuthoredEffects, which has already closed over the four
-- nesting arms with effectWithNested, and recursing here would double-count.
--
-- Exhaustive and hand-maintained with effectFilters' caveat, and the missing `_`
-- is the whole point: a NEW opcode taking an ObjectRef is AsksNothing only by
-- being written so here, and the build breaks until somebody decides. The one
-- thing -Werror cannot catch is an arm written `[]` that does hold a ref, which
-- is what the cross-check against effectFilters below is for.
-- The same positions Resolve.effectObjectRefs names, tagged: a position added
-- to one belongs in the other, and the two lists are read side by side.
effectObjectRefs :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [(Asks, ObjectRef.ObjectRef)]
effectObjectRefs effect = case effect of
  Effect.AttachTarget {} -> []
  Effect.AttachTargetToEach {} -> []
  Effect.AttachBound {} -> []
  Effect.DealDamage (DealDamage.MkDealDamage parts _ _) -> read_ (fmap DamagePart.ref (Foldable.toList parts))
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ _ ref) -> read_ [ref]
  Effect.ChangeText {} -> []
  Effect.AddMana {} -> []
  Effect.Search {} -> []
  Effect.ExileAllGraveyards -> []
  Effect.Proliferate -> []
  Effect.ChooseCardName {} -> []
  Effect.FromOutsideTheGame {} -> []
  Effect.ExileThisSpell -> []
  Effect.Bolster {} -> []
  Effect.Amass {} -> []
  Effect.Blight {} -> []
  -- CR 701.66a's "target land you control" is an ordinary read, Detain's arm
  -- below: the animation and the counters act on it and nothing gathers.
  Effect.Earthbend (Earthbend.MkEarthbend _ ref) -> read_ [ref]
  Effect.TemptWithTheRing -> []
  Effect.Venture {} -> []
  Effect.ExileHandThenDraw -> []
  Effect.PlayerSacrifices {} -> []
  -- CR 727.5's exemption, optional: a card saying nothing about it exempts
  -- nothing.
  Effect.RestartGame mRef -> read_ (Maybe.maybeToList mRef)
  Effect.ControlPlayerNextTurn {} -> []
  Effect.Destroy (Destroy.MkDestroy ref _ _ _ _) -> read_ [ref]
  Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect ref _) -> read_ [ref]
  -- THE gather that asks, and the one that elides the random arm (#1733).
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ _ _ _) -> [(AsksMoveGather, ref)]
  Effect.Draw {} -> []
  Effect.Mill {} -> []
  -- CR 701.20a's reveal, the other asking arm.
  Effect.Reveal (Reveal.MkReveal ref _) -> [(AsksRevealArm, ref)]
  Effect.LookAt (LookAt.MkLookAt ref _) -> read_ [ref]
  Effect.Scry {} -> []
  Effect.Surveil {} -> []
  Effect.Fateseal {} -> []
  Effect.Explore ref -> read_ [ref]
  Effect.Discard subject -> case subject of
    Discard.Counted {} -> []
    Discard.These ref -> read_ [ref]
  Effect.LoseLife {} -> []
  Effect.GainLife {} -> []
  Effect.ExchangeLifeTotals {} -> []
  Effect.SetLifeTotal {} -> []
  Effect.RedistributeLifeTotals -> []
  Effect.IncreaseSpeed {} -> []
  Effect.DecreaseSpeed {} -> []
  -- CR 111.1's token holds a whole card, but the refs printed ON it are another
  -- object's; mintedFaces is that axis, and every caller here sweeps one face at
  -- a time. CreateEmblem answers the same way for CR 114.2's emblem.
  Effect.Create {} -> []
  Effect.Conjure {} -> []
  Effect.CreateCopy (CreateCopy.MkCreateCopy _ ref _) -> read_ [ref]
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject) -> read_ [original, subject]
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets) -> read_ (ref : copyTargetsRefs targets)
  Effect.Replace {} -> []
  Effect.SkipNextPhase {} -> []
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage _ _ ref _ _ _ _ _) -> read_ (Maybe.maybeToList ref)
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage _ _ ref _ _ _ _ _) -> read_ (Maybe.maybeToList ref)
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage _ _ _ srcRef _ _ destRef _) -> read_ (Maybe.maybeToList srcRef <> [destRef])
  -- A READ and not an ask: CR 708.2's turning-over takes no choice of its own, and
  -- this arm never reaches the Game monad, so an AnyNumberMatching ref written
  -- here would name nothing. inertChoosers is what says so at load time.
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown ref _) -> read_ [ref]
  Effect.TurnFaceUp {} -> []
  Effect.Fight {} -> []
  Effect.RemoveFromCombat ref -> read_ [ref]
  Effect.BecomesBlocked {} -> []
  Effect.Counter (Counter.MkCounter ref _ _) -> read_ [ref]
  Effect.PutCounters (PutCounters.MkPutCounters _ _ ref) -> read_ [ref]
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom _ _ ref) -> read_ [ref]
  -- BOTH sides, each a READ -- CR 122.5 takes no choice of WHICH objects the
  -- counters leave or land on, so this arm goes through the pure objectRefObjects
  -- and a chooser-shaped ref written on either side would name nothing. WHICH
  -- counters go to which recipient IS a choice, and it is asked of the answer
  -- rather than of the ref (Prompt.ChooseDistributedMovedCounters).
  Effect.MoveCounters (MoveCounters.MkMoveCounters from _ _ to) -> read_ [from, to]
  Effect.RemoveCounters {} -> []
  Effect.GainPlayerCounters {} -> []
  Effect.RemovePlayerCounters {} -> []
  Effect.PayAnyEnergy {} -> []
  Effect.Tap ref -> read_ [ref]
  Effect.Untap ref -> read_ [ref]
  Effect.Detain ref -> read_ [ref]
  Effect.Goad ref -> read_ [ref]
  Effect.MakePlotted ref -> read_ [ref]
  Effect.DoesNotUntapNext ref -> read_ [ref]
  Effect.Transform ref -> [(AsksTransformGather, ref)]
  -- The SAME gather, CR 701.28a routing a convert through CR 701.27a-f and
  -- Pawl.Engine.Resolve applying both opcodes through one turnPermanentsOver.
  Effect.Convert ref -> [(AsksTransformGather, ref)]
  -- A plain READ: Pawl.Engine.Resolve's Meld arm sweeps the ref and asks nothing.
  Effect.Meld (Meld.MkMeld ref _) -> read_ [ref]
  Effect.PhaseOut ref -> read_ [ref]
  Effect.AddPhases {} -> []
  Effect.EndTurn -> []
  Effect.EndCombatPhase -> []
  Effect.GainControl (DurationRef.MkDurationRef _ ref) -> read_ [ref]
  Effect.ArmDelayedTrigger {} -> []
  Effect.AffectPlayers {} -> []
  Effect.RequireBlock (RequireBlock.MkRequireBlock _ blocker attacker) -> read_ [blocker, attacker]
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated _ ref) -> read_ [ref]
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock _ ref) -> read_ [ref]
  Effect.ForbidActivation (ForbidActivation.MkForbidActivation _ ref) -> read_ [ref]
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack _ affected _) -> case affected of
    RestrictedCreatures.Named ref -> read_ [ref]
    RestrictedCreatures.Matching _ -> []
  Effect.RequireAttack (RequireAttack.MkRequireAttack _ attacker _) -> read_ [attacker]
  Effect.CreateEmblem {} -> []
  Effect.BecomeMonarch {} -> []
  Effect.TakeTheInitiative {} -> []
  Effect.Designate {} -> []
  Effect.SetClassLevel {} -> []
  Effect.Unsuspect ref -> read_ [ref]
  Effect.SetHalfLocked {} -> []
  Effect.Evolve {} -> []
  Effect.Mentor {} -> []
  Effect.Train {} -> []
  Effect.ItBecomes {} -> []
  Effect.ExileUntilMonarch {} -> []
  Effect.ExileHaunting {} -> []
  Effect.Attach {} -> []
  Effect.PlaySubgame {} -> []
  Effect.ChoosePlayer {} -> []
  Effect.ChooseOpponentAtRandom {} -> []
  Effect.RollDie {} -> []
  Effect.FlipCoin {} -> []
  Effect.TakeExtraTurn {} -> []
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary _ ref) -> read_ [ref]
  -- No ObjectRef at all: the opcode names a library.
  Effect.Shuffle {} -> []
  Effect.OfferCast offer -> read_ [OfferCast.ref offer]
  Effect.GrantPlayFromExile grant -> read_ [GrantPlayFromExile.ref grant]
  Effect.ForEach (ForEach.MkForEach ref _ _) -> read_ [ref]
  where
    read_ :: [ObjectRef.ObjectRef] -> [(Asks, ObjectRef.ObjectRef)]
    read_ = fmap ((,) AsksNothing)

-- The chooser-shaped refs one effect writes where nothing can ask for them: the
-- lint's offenders. Each is a CR 608.2d choice nobody makes, so the ref names no
-- object, that share of the instruction is silently skipped, and the card is
-- weaker than printed with nothing on the wire to show it.
inertChoosers :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> [ObjectRef.ObjectRef]
inertChoosers effect =
  [ref | (asks, ref) <- effectObjectRefs effect, chooserRef ref, not (asksFor asks ref)]

-- CR 709.4a: a card's faces are referred to BY NAME (Card.faceNamed), so two
-- faces sharing a name make that reference ambiguous -- faceNamed would return
-- the FIRST of them and silently hide the second. Over the whole card rather
-- than through anyFace: this is a claim about the SET of names a card prints,
-- which no per-face predicate can state.
distinctFaceNamesOffends :: Card.Type.Card -> Bool
distinctFaceNamesOffends card =
  let names = fmap Face.name (NonEmpty.toList (Card.Type.faces card))
   in length (List.nub names) /= length names

-- CR 709.5a: "Each half of a split card with a shared type line shares the types
-- and subtypes listed on that card's shared type line." pawl stores that
-- literally -- both faces of a Room carry the whole line -- and
-- Pawl.Engine.Card.roomFace deliberately does not subtract it, citing the rule.
-- Nothing else enforces the duplication: a Room whose faces disagreed would load
-- without complaint, and Pawl.Engine.Card.unionTypeLines (set union) would merge
-- the disagreement into a line NEITHER face prints.
--
-- Which cards the claim is about is Card.hasSharedTypeLine's answer rather than
-- a `== Layout.Room` here, so that the lint and the engine's own subtraction
-- range over exactly the same cards -- and so that a new layout has to state
-- whether its faces share a line in the one place -Werror already asks.
--
-- Full type-line equality, not just the two sets CR 709.5a names. The types and
-- subtypes are 709.5a's; the supertypes come from CR 709.5's premise instead --
-- "permanent cards with a single shared type line" is one printed line, and a
-- supertype on it is on it for both halves. No printed Room has a supertype, so
-- the stricter reading costs the corpus nothing and is the one that keeps two
-- stored copies of one line honest.
--
-- Over the whole card rather than through anyFace, for distinctFaceNamesOffends'
-- reason: this is a claim about the faces as a set.
sharedTypeLineOffends :: Card.Type.Card -> Bool
sharedTypeLineOffends card =
  let lines_ = fmap Face.typeLine (Card.Type.faces card)
   in Card.hasSharedTypeLine card && any (/= NonEmpty.head lines_) lines_

-- CR 712.4b: the back face of a meld card "fails to determine its
-- characteristics" anywhere but on a melded permanent on the battlefield, so
-- pawl stores each half of a meld pair as its front face alone
-- (Pawl.Types.Layout's Meld arm). A file that gave one a second face would be
-- storing characteristics no rule can read: every Pawl.Engine.Card arm for this
-- layout answers off NonEmpty.head, so the extra face would be silently dropped
-- rather than rejected. This is where that is made loud.
--
-- A `== Layout.Meld` rather than an engine classifier, unlike
-- sharedTypeLineOffends above: nothing in Pawl.Engine.Card asks "is this a meld
-- card?" -- CR 701.42b's and CR 712.4c's readers case on the layout directly --
-- so there is no shared answer for the lint to range over.
--
-- Over the whole card rather than through anyFace, for distinctFaceNamesOffends'
-- reason: this is a claim about the faces as a set.
meldFaceCountOffends :: Card.Type.Card -> Bool
meldFaceCountOffends card =
  Card.Type.layout card == Layout.Meld && length (Card.Type.faces card) /= 1

-- A MINTED face's own SourceHostFramed filters, for the effect that mints it.
-- They come through effectFilters -- Effect.Create's arm ends
-- `overFaces cardFilters card` -- and inside a CARD that tag means the TOKEN's
-- host, which is not the comparison below's vocabulary: Ashiok, Wicked
-- Manipulator's Nightmare carries a CR 603.4 intervening "if" and no ObjectRef at
-- all, and effectObjectRefs' Create arm rightly answers []. A minted face's own
-- positions are cardFilters' business, and the mintedFaces lints are where they
-- are read.
--
-- Over effectWithNested rather than the effect alone, because effectFilters
-- recurses into a nested effect list of its own: a Create inside a rider
-- contributes its token's filters to the enclosing effect's tally too. That
-- closure is a superset of effectFilters' own recursion, which can only subtract
-- within the effect whose tally it is being subtracted from.
mintedOwn :: Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Set.Set (Filter.Type.Filter Keyword.Keyword)
mintedOwn effect = Set.fromList [f | e <- effectWithNested effect, (_, face) <- effectMintedFaces e, (SourceHostFramed, f) <- cardFilters face]

-- The SourceHostFramed filters a face's resolution effects reach through the
-- Filter traversal, each effect's minted faces subtracted from that effect's own
-- tally, leaving the comparison stated in the one vocabulary it means.
--
-- PER EFFECT, not card-wide: both sides are sets of filter VALUES, so subtracting
-- a card-wide minted set cancels a filter contributed by any other effect that
-- happens to carry an equal one. The case below is the counter-example.
viaFilters :: Face.Face Card.Type.Card -> Set.Set (Filter.Type.Filter Keyword.Keyword)
viaFilters card = Set.unions [Set.difference (Set.fromList [f | (SourceHostFramed, f) <- effectFilters effect]) (mintedOwn effect) | effect <- cardResolutionEffects card]

effectLintSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
effectLintSpec s registry = Spec.describe s "Lint" $ do
  -- CR 208.1 / 208.2: a printed power or toughness box holds a number, or a value
  -- including a star. The type permits any Quantity there, and a computed one
  -- would be evaluated at Projection.baseCharacteristics' seed against a board
  -- that has not been described yet -- see printedBoxOffends.
  Spec.it s "CR 208.1 / 208.2 every printed power and toughness box is a number or a star" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace printedBoxOffends . Printing.card) ps
    Spec.assertEqWith s "no printed box holds a computed quantity" (fmap (S.nameOf . Printing.card) offenders) []
    -- The sweep is vacuous on a predicate that accepts everything, so both
    -- directions are asserted on real card data. Rootha, Mastering the Moment
    -- prints a computed box (Miming Slime's Ooze and Phyrexian Rebirth's Horror
    -- are two more; grep data/cards/ for the shape), and it sits on a
    -- MINTED face (CR 111.3), which is exactly the shape the sweep above must not
    -- reach and the predicate must still reject.
    rootha <- S.printingOf s registry "Rootha, Mastering the Moment"
    let elemental = mintedFaces (S.combinedFace rootha)
    Spec.assertBool
      s
      (any printedBoxOffends elemental)
      "CR 111.3 Rootha's minted Elemental token prints a computed box the predicate rejects"
    Spec.assertBool
      s
      (not (printedBoxOffends (S.combinedFace rootha)))
      "and Rootha's own 3/4 box is accepted"
    -- CR 208.2's star, in both the bare and the composite spelling, accepted.
    goyf <- S.printingOf s registry "Tarmogoyf"
    Spec.assertBool
      s
      (not (printedBoxOffends (S.combinedFace goyf)))
      "CR 208.2 Tarmogoyf's */1+* is accepted"
  Spec.it s "a card with no enchant ability declares no enchant slot" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let card = S.combinedFace piker
    Spec.assertEqWith s "no enchant slot" (Face.enchant card) []
    Spec.assertBool s (not (Card.isAura card)) "not an Aura"
    Spec.assertEqWith s "no enchant slot" (Card.enchantSlotMap card) Map.empty
  -- CR 303.4 / 702.5a: the biconditional. An Aura without enchant has no legal
  -- target and could never be cast; a non-Aura with enchant declares a restriction
  -- nothing reads. The D4 lint cannot see either, because it walks
  -- Mode.targetSlots and the enchant slot is not there -- the same shape the two
  -- pregame windows had until the hand-action sweep above.
  --
  -- "AT LEAST one", since CR 702.5c lets an Aura have several -- the count is not
  -- what makes a card an Aura, only the presence.
  Spec.it s "a card is an Aura iff it declares an enchant ability" $ do
    ps <- S.allPrintings s
    let offends c = Card.isAura c /= not (null (Face.enchant c))
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "Aura iff enchant" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 702.5c makes every instance of enchant apply at once, and its last
  -- sentence -- "The Aura can enchant only objects or players that match all of
  -- its enchant abilities" -- conjoins the POOLS exactly as it does the Filters.
  -- Pawl.Engine.Card.enchantTargetSlot folds the instances into ONE target slot
  -- by Anding their Filters and keeping the FIRST instance's Pool. This lint is
  -- what makes that fold exact, and the rule it enforces is that CR 115's Pool
  -- enum is not closed under intersection. Three shapes, one expressible: a
  -- NESTED pair has a Pool naming the intersection (Creatures against Permanents
  -- is Creatures); a DISJOINT pair intersects to nothing (Creatures against
  -- Players, which is CR 702.5d keeping an enchant-player Aura off permanents),
  -- and no Pool names the empty set; and an OVERLAPPING pair can name a set the
  -- enum simply lacks (AnyTarget against Permanents is
  -- creatures-and-planeswalkers). Taking the first instance is order-dependent
  -- even in the expressible case, so a card whose enchant abilities disagreed
  -- would be silently judged by whichever pool was written down first.
  --
  -- The disjoint case is INCOHERENT rather than merely unrepresentable, and the
  -- CR says what becomes of such an Aura without needing a pool for it: CR 303.4a
  -- makes its spell require a target and CR 601.2c has no appropriate object or
  -- player to announce for it, so it cannot be cast; an effect putting it onto the
  -- battlefield leaves it where it is, or bins it if that zone is the stack (CR
  -- 303.4g); and one that arrived anyway is put into its owner's graveyard on the
  -- next state-based check (CR 704.5m). A card in that shape is dead text.
  --
  -- Unprinted rather than impossible, which is why this lives here rather than
  -- being ruled out: nothing in CR 702.5 requires the instances to agree, and
  -- Animate Dead prints both pools on one card ("enchant creature card in a
  -- graveyard", then "enchant creature put onto the battlefield with this Aura")
  -- -- only its lose-as-it-gains clause keeps the two from applying at once
  -- (#797).
  Spec.it s "every enchant ability on a card draws from the same pool" $ do
    ps <- S.allPrintings s
    let offends c = length (List.nub (fmap TargetSlot.pool (Face.enchant c))) > 1
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "no card mixes enchant pools, since CR 702.5c intersects them and Pool is not closed under intersection" (fmap (S.nameOf . Printing.card) offenders) []
  -- Pawl.Engine.Card.allTargetSlots binds the enchant slot under this name (Task 6), so a
  -- mode declaring it would be silently shadowed.
  -- #199: no card authors a layer-2 control modification into an effect that
  -- RESOLVES. SetControllerToSource is the payload-free constructor and is
  -- INERT when stored: Projection.controllerOfGiven's storedSetter matches only
  -- Modification.SetController, Projection.controlGrants reads control-granting
  -- static abilities off Face.staticAbilities and never off stored effects, and
  -- Projection.applyModification's SetControllerToSource arm is the identity.
  -- A card authoring one would resolve, store the effect, and grant control to
  -- no one -- there is nothing for CR 800.4a to end (see Pawl.Engine.Departure's
  -- proofs).
  --
  -- BOTH control constructors, not just the payload-free one: baking a
  -- PlayerId into static card text is equally unreal, since a card cannot
  -- know who is playing. Control on a card belongs on a STATIC ability
  -- (Control Magic), which the projection re-derives and never stores.
  --
  -- Asked as an EQUALITY on Layer through Projection.layer -- the sanctioned
  -- classification -- rather than by casing on Modification, which only
  -- Pawl.Engine.Projection may do. Layer.Control is exactly the two control
  -- constructors, so this covers a third one automatically.
  --
  -- A codec-level rejection would be the wrong shape: (Codec.decode Modification.codec) is
  -- shared with staticAbilities, which Control Magic legitimately uses.
  Spec.it s "no card authors a control modification into a resolving effect (#199)" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ modification _) -> Projection.layer modification == Layer.Control
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    Spec.assertEqWith s "control belongs on a static ability, never in a stored effect" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 106.6 restricts how mana "can be spent"; it never forbids spending it
  -- outright. A Pawl.Types.ManaRestriction with neither half set is mana no
  -- payment may ever use, which is mana the card might as well not have added --
  -- so it is card data that means nothing rather than a rule pawl implements.
  -- Nothing in the codec can refuse it: both fields default to Nothing, which is
  -- what lets Mishra's Workshop write one key and Omen Hawker the other.
  Spec.it s "no card adds mana no payment could spend (CR 106.6)" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.AddMana addition -> case ManaAddition.restriction addition of
            Just restriction -> null (restrictionFilters restriction)
            Nothing -> False
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    Spec.assertEqWith s "a restriction admitting neither casts nor activations is unspendable mana" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 612.2's family gate lives on a modification's CONSTRUCTOR:
  -- Pawl.Engine.Projection.rewriteModificationWith swaps a land-type word only
  -- inside the two land arms and a creature-type word only inside the two creature
  -- arms. AddSubtype carries no family -- it exists for CR 205.3g's artifact types
  -- and CR 205.3h's enchantment types, which no printed text changer reaches -- and
  -- is deliberately left unrewritten there. That is sound only while no AddSubtype
  -- in the pool holds a word a text changer could swap. This is the fence, and the
  -- reason the two family arms were not generalised into AddSubtype.
  Spec.it s "CR 612.2 no AddSubtype carries a land type or a creature type" $ do
    ps <- S.allPrintings s
    let addedSubtypes modification = case modification of
          Modification.AddSubtype subtype -> [subtype]
          _ -> []
        stored effect = case effect of
          Effect.ModifyTarget (ModifyTarget.MkModifyTarget _ modification _) -> addedSubtypes modification
          _ -> []
        added card = concatMap addedSubtypes (grantedModifications card) <> concatMap stored (cardResolutionEffects card)
        misfiled subtype = Subtype.Engine.isLandType subtype || Subtype.Engine.isCreatureType subtype
        offenders = filter (anyFace (any misfiled . added) . Printing.card) ps
    -- Guards against a vacuous sweep: with no AddSubtype in the pool this would
    -- pass whatever the arm carried. Ygra, Eater of All prints the pool's one.
    Spec.assertBool s (any (anyFace (not . null . added) . Printing.card) ps) "the pool has a card adding a subtype outside both families"
    Spec.assertEqWith s "a land type or a creature type belongs on its own family's arm" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 712.14a's rider is about a double-faced CARD, and CR 111.1 makes a token
  -- no card at all -- so an Effect.Create that carried it would be asking for a
  -- face-turn no rule performs. Pawl.Engine.Resolve's Create arm accordingly does
  -- not read the field, and this is what holds the corpus to that reading. A lint
  -- rather than a per-opcode rider type: CR 110.5b's tap state and CR 508.4's
  -- attacking genuinely are common to Create and MoveToZone, so splitting the
  -- record to keep one field off one opcode would duplicate the other two.
  Spec.it s "no Create carries CR 712.14a's transformed entry rider" $ do
    ps <- S.allPrintings s
    let creates effect = case effect of
          Effect.Create (Create.MkCreate _ _ riders _ _) -> EntryRiders.transformed riders
          _ -> False
        moves effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> EntryRiders.transformed riders
          _ -> False
        offenders = filter (anyFace (any creates . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no transformed rider in the pool at
    -- all this would pass whatever Create did. Befriending the Moths is the card
    -- that prints one.
    Spec.assertBool s (any (anyFace (any moves . cardResolutionEffects) . Printing.card) ps) "the pool has a card returning itself transformed"
    Spec.assertEqWith s "a token is not a card, so no token is created transformed" (fmap (S.nameOf . Printing.card) offenders) []
  -- MoveToZone's Maybe SlotName binds what CR 400.7 minted at the destination, in
  -- one of two shapes: ONE incarnation for a move of one card (Befriending the
  -- Moths' "it"), and a GROUP for a move of several (Act on Impulse's "those
  -- cards"). Pawl.Engine.Resolve picks by how many actually arrived, and only the
  -- singular shape is visible to a SINGULAR READER -- Resolve.slotOne reads
  -- Binding.targets, which a group never fills. Filter.IsBound is NOT one: it
  -- goes to Filter.Context's slotObjects, where a group is every one of its
  -- members.
  --
  -- So the shape a card must not author is a singular read of a slot a move that
  -- may take SEVERAL cards bound: it would silently name nothing rather than
  -- fail. WHICH reads are singular is `readSingly` below and not this paragraph
  -- -- an opcode is one when Resolve hands its slot to legalOne or slotOne with
  -- no slotGroup fallback beside it, which many of them do. A MoveToZone whose
  -- own ref is an InSlot is NOT one, despite reading the slot by hand rather
  -- than through Resolve.objectRefObjects: its branch asks slotGroup FIRST and
  -- moves every member, which is Feral Lightning's "exile them" and Ignorant
  -- Bliss' "return those cards to your hand".
  Spec.it s "no card reads a slot a plural move bound with a singular reader" $ do
    ps <- S.allPrintings s
    let -- The refs that move at most ONE object, and so bind the singular shape
        -- whatever the board holds. A TopOfLibrary is `depth` cards PER LIBRARY,
        -- so it qualifies only at a LITERAL depth of one over a PlayerRef naming a
        -- single library -- "each player's" and, in a game of three, "each
        -- opponent's" both move several, and so does any depth above one. A
        -- COMPUTED depth (Commune with Lava's X) is not statically one, so it is
        -- plural here whatever the board would make it: this lint asks what a card
        -- may be written as, and a card whose depth is a number it computes may
        -- always compute more than one.
        movesAtMostOne ref = case ref of
          ObjectRef.InSlot _ -> True
          ObjectRef.EachMatching _ -> False
          ObjectRef.EachCardInGraveyard {} -> False
          ObjectRef.EachCardInYourHand -> False
          ObjectRef.EachCardInHand {} -> False
          ObjectRef.EachCardInYourLibrary {} -> False
          -- CR 607.3 is what makes this one plural even where the card's own
          -- words are singular: an ability referring to "the exiled card" whose
          -- linked ability exiled several performs its action on each of them.
          ObjectRef.EachCardExiledWithSource {} -> False
          ObjectRef.EachSpell _ -> False
          ObjectRef.EachOnStack _ -> False
          ObjectRef.EachPlayer -> False
          ObjectRef.EachOpponent -> False
          -- Names a player and so moves no object at all, the arm above's answer.
          ObjectRef.ChosenPlayer -> False
          ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary player depth) -> case depth of
            Quantity.Type.Literal n -> n <= 1 && namesOneSeat player
            _ -> False
          -- FALSE however narrow the seat and however small the count: a walk
          -- ends where the count of matches is met, and nothing about the ref
          -- bounds how many cards it passes first. The same reading the computed
          -- depth above gets -- this lint asks what a card may be written as.
          ObjectRef.TopOfLibraryUntil {} -> False
          -- TRUE at one seat: CR 404.1's top card is exactly one card per
          -- graveyard, so the only thing that can make it plural is a PlayerRef
          -- naming several -- TopOfLibrary's namesOneSeat with no depth to fail.
          ObjectRef.TopOfGraveyard player -> namesOneSeat player
          -- One card per CHOOSER: the resolving controller chooses once however
          -- many graveyards the scope draws candidates from, where Exhume's
          -- "each player" is one choice each and so several cards on any board
          -- with more than one stocked graveyard.
          ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard chooser _ _) -> case chooser of
            Chooser.TheController -> True
            Chooser.EachInScope -> False
            -- One seat, so one graveyard and one card -- TheController's answer
            -- with the chooser named by a slot instead of by CR 608.2c.
            Chooser.BoundInSlot _ -> True
          -- One card per CHOOSER again, and here the PlayerRef names the
          -- choosers: Karn Liberated's targeted seat exiles one card, and "each
          -- player" would be one each. The same per-seat count TopOfLibrary
          -- takes of its own PlayerRef, which is why they share namesOneSeat.
          ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand player _) -> namesOneSeat player
          -- One card per ONE seat, so the ref's own count is the whole of it: a
          -- chooser names a single seat or nobody, where ChosenCardInHand's
          -- PlayerRef above may name several hands. A COMPUTED count is plural
          -- here whatever the board would make it, TopOfLibrary's reading above
          -- and for its reason.
          ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong _ _ count _) -> case count of
            Quantity.Type.Literal n -> n <= 1
            _ -> False
          -- FALSE where the arm above is True, which is the whole difference
          -- between them: "all" takes every member that matches, and nothing
          -- about the ref bounds how many a group holds.
          ObjectRef.EachCardFromAmong {} -> False
          -- One card per SEAT, the arm above's answer with randomness in the
          -- chooser's place: Merfolk Spy's slot names one seat and so one card,
          -- and Fall's printed two makes the same seat plural. A COMPUTED count
          -- is plural whatever the board would make it, ChosenCardFromAmong's
          -- reading above and for its reason.
          ObjectRef.RandomCardInHand (RandomCardInHand.MkRandomCardInHand player _ count) -> case count of
            Quantity.Type.Literal n -> n <= 1 && namesOneSeat player
            _ -> False
          -- FALSE, EachCardFromAmong's answer over the battlefield: "any number"
          -- states no bound, so nothing about the ref caps how many permanents
          -- the chooser may name.
          ObjectRef.AnyNumberMatching _ -> False
          -- TRUE where the arm above is False, which is the whole difference
          -- between them: the ref names exactly one permanent however many match.
          ObjectRef.ChosenPermanent _ -> True
          -- FALSE where the arm above is True: the ref names the source
          -- ALONGSIDE the one permanent it picks, so a per-player count over it
          -- moves two.
          ObjectRef.SourceAndChosenPermanent _ -> False
        -- Does this PlayerRef name at most ONE seat? A per-player count over it
        -- -- a library's top card, a card chosen out of a hand -- moves at most
        -- one object exactly when it does.
        namesOneSeat player = case player of
          PlayerRef.Relative PlayerRelation.You -> True
          PlayerRef.Relative PlayerRelation.Opponent -> False
          -- The whole table -- EachPlayer's answer, which this relation is.
          PlayerRef.Relative PlayerRelation.AnyPlayer -> False
          PlayerRef.InSlot _ -> True
          -- A SET -- the arm above's plural, and the whole of what parts them.
          PlayerRef.EachInSlot _ -> False
          PlayerRef.EachPlayer -> False
          -- The whole table but one seat -- EachPlayer's answer, and for its
          -- reason.
          PlayerRef.EachPlayerExcept _ -> False
          -- One seat -- InSlot's answer. Unreachable from card data, which the
          -- sweep below is what enforces.
          PlayerRef.Specific _ -> True
          -- NO seat at all: an ObjectRef is read by a resolution, where no fold
          -- supplies a candidate, so this names nobody and moves nothing --
          -- which is at most one.
          PlayerRef.Candidate -> True
          -- One seat -- InSlot's answer, one indirection out.
          PlayerRef.ControllerOfBound _ -> True
          -- A SET -- Relative Opponent's answer, and for its reason: CR 508.6 is
          -- a predicate over the table rather than a name for one seat.
          PlayerRef.Attacking _ -> False
        -- WHICH opcodes bind a batch is this enumeration and not this paragraph
        -- -- readSingly's posture below. An opcode belongs here when
        -- Pawl.Engine.Resolve hands its slot to `bindObjectsSlot`, and the guard
        -- on the arm is the CONDITION under which that call is reached: none at
        -- all where the arm binds a group however few objects it found, and the
        -- opcode's own plurality test where the arm dispatches on how many
        -- arrived and takes the single binding for one. Grepping that one
        -- function in that one file is what makes the list checkable.
        --
        -- Nothing reports a `bindObjectsSlot` call added later: this case and
        -- readSingly both end in `_ -> []`, so a new binder compiles,
        -- round-trips and sweeps the corpus with no arm on either side. Adding
        -- one to Resolve means coming back here by hand.
        boundPlurally effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ mSlot _ _ _) | not (movesAtMostOne ref) -> Maybe.maybeToList mSlot
          Effect.LookAt (LookAt.MkLookAt ref slot) | not (movesAtMostOne ref) -> [slot]
          Effect.Reveal (Reveal.MkReveal ref mSlot) | not (movesAtMostOne ref) -> Maybe.maybeToList mSlot
          -- CR 701.17c's slot, whose plurality is the mill's DEPTH rather than an
          -- ObjectRef's: a mill of one card binds the singular shape and any
          -- deeper mill may bind a group, so only a literal 1 is singular here.
          -- The depth is per miller, so a ref naming several seats is plural at
          -- any depth -- movesAtMostOne's own reading of a TopOfLibrary.
          Effect.Mill (Mill.MkMill player quantity _ mSlot)
            | not (takesAtMostOne player quantity) ->
                Maybe.maybeToList mSlot
          -- CR 121.1's slot, whose plurality is read exactly as the mill's is:
          -- Pawl.Engine.Resolve's Draw arm binds the singular shape only when one
          -- card was drawn across every drawer.
          Effect.Draw (Draw.MkDraw player quantity mSlot)
            | not (takesAtMostOne player quantity) ->
                Maybe.maybeToList mSlot
          -- No ref test: Pawl.Engine.Resolve's Destroy arm writes both through
          -- bindObjectsSlot however few permanents it destroyed, so neither is
          -- ever the singular shape -- not even for "destroy target creature".
          Effect.Destroy (Destroy.MkDestroy _ _ _ mBuried mPermanents) ->
            Maybe.maybeToList mBuried <> Maybe.maybeToList mPermanents
          -- CR 701.9's look-back slot, the destruction's shape and for its
          -- reason: the counted discard's arm binds the group however few cards
          -- moved, so no ref test and no count test. Psychic Miasma's "if a land
          -- card is discarded this way" is one that writes it. Discard.These
          -- binds nothing at all and so is not here.
          Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard _ _ mDiscarded)) -> Maybe.maybeToList mDiscarded
          -- CR 111.1's minted tokens, whose plurality is Resolve.namesEveryToken
          -- -- any count that is not a literal one, so a computed count is
          -- plural here for the reason a computed depth is. No seat test, unlike
          -- the mill's and the draw's: at a literal one the arm never binds the
          -- group however many creators the reference named, taking the single
          -- binding for one token and ASKING where CR 614.16 multiplied the
          -- count.
          Effect.Create (Create.MkCreate quantity _ _ mSlot _)
            | Resolve.namesEveryToken quantity ->
                Maybe.maybeToList mSlot
          _ -> []
        takesAtMostOne player quantity = case quantity of
          Quantity.Type.Literal n -> n <= 1 && namesOneSeat player
          _ -> False
        -- Every slot Pawl.Engine.Resolve hands to a SINGULAR read with no group
        -- fallback beside it: `legalOne` over the legal-target map, or `slotOne`
        -- over the resolving object's own target bindings. Both are
        -- Binding.onlyOne, so a slot naming several objects answers Nothing and
        -- the instruction is skipped in silence. In Resolve's own arm order, so
        -- the enumeration is checkable by reading down that file.
        --
        -- One shape of singular reader is deliberately absent:
        -- Effect.ControlPlayerNextTurn,
        -- MonarchTarget's InSlot and ExchangeSides' WithController match
        -- Recipient.ToPlayer, and no binder in boundPlurally mints a player slot.
        --
        -- A PlayerRef an effect carries is NOT one of these arms: it sits inside
        -- a payload rather than being one, so readSinglyInPlayerRefs below is its
        -- leg.
        --
        -- A slot read from inside a NUMBER is not one of these arms either:
        -- Quantity.AgainstSlot reaches Filter.slotOneObject, which declines a
        -- group exactly as Binding.onlyOne does, and readSinglyInQuantities
        -- below is its leg. Filter.IsControllerOfBound, the third reader through
        -- that funnel, is filterSlotsReadSingly's; Filter.IsBound reads the whole
        -- set, so it is no risk and no fence for the others either.
        --
        -- A PlayerRef NESTED IN a quantity -- Quantity's own ControllerOfBound
        -- arm -- is covered by the same leg: QuantitySlot.slots answers empty
        -- for it, but quantitySlots folds nestedRefs, so it arrives at
        -- Resolve.playerRefSlots' own arity like any other reference.
        --
        -- The condition of a DELAYED ability is a singular reader too, and it is
        -- fenced on both axes: triggerConditionSlots and filterSlotsReadSingly
        -- both join this list inside clashesIn below, which is why the sweep is
        -- over the FACE rather than over an effect list.
        readSingly effect = case effect of
          -- CR 701.14b's pair, which is why both slots count.
          Effect.Fight (Fight.MkFight one two) -> [one, two]
          Effect.ChangeText (ChangeText.MkChangeText _ _ slot) -> [slot]
          Effect.TurnFaceUp slot -> [slot]
          Effect.BecomesBlocked slot -> [slot]
          Effect.Designate (Designate.MkDesignate _ slot) -> [slot]
          Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ slot) -> [slot]
          Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked _ _ slot) -> [slot]
          Effect.Evolve slot -> [slot]
          Effect.Mentor slot -> [slot]
          Effect.Train slot -> [slot]
          Effect.Attach slot -> [slot]
          Effect.AttachTarget (AttachTarget.MkAttachTarget slot _) -> [slot]
          Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot _) -> [slot]
          -- The DESTINATION alone: AttachBound's `subject` goes through
          -- objectRefObjects, which reads slotGroup and attaches every member.
          Effect.AttachBound (AttachBound.MkAttachBound _ destination) -> [destination]
          Effect.ExileUntilMonarch slot -> [slot]
          -- Both slots: the host through legalOne, the haunting card through
          -- slotOne.
          Effect.ExileHaunting (ExileHaunting.MkExileHaunting card host) -> [card, host]
          -- CR 122.8's read.
          Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom from _ _) -> [from]
          Effect.RemoveCounters (RemoveCounters.MkRemoveCounters _ _ slot) -> [slot]
          -- CR 122.5 names NO slot read singly: both its sides are ObjectRefs and
          -- go through objectRefObjects, which reads slotGroup and moves counters
          -- off every member of one and onto every member of the other. Its third
          -- slot is not one either, being a count this opcode WRITES rather than
          -- an object it reads.
          _ -> []
        -- The slots a Filter these effects carry reads singly, MINUS the ones a
        -- card they MINT carries. effectFilters is the traversal rather than a
        -- new one, and it recurses into a nested effect list of its own, so a
        -- rider's Filter is reached whether or not cardResolutionEffects
        -- flattened it -- harmless either way, this being a Set.
        --
        -- But it also splices a token's, a conjured card's, an emblem's and a
        -- meld result's WHOLE card text into its Create, Conjure, CreateEmblem
        -- and Meld arms, and those Filters are read in a resolution of the
        -- minted object's own. That is the boundary effectNestedEffects draws in
        -- so many words, and the one the other two reading sides honour by
        -- running over cardResolutionEffects; without the subtraction this leg
        -- would attribute a token's read to the card that created it (#2735).
        -- effectMintedFaces is this file's traversal for that axis and reaches
        -- exactly the faces overFaces does at all four of those arms, so the
        -- subtraction removes the spliced text and nothing else. A MULTISET
        -- difference, so a slot the CREATING card reads in its own right
        -- survives the token reading the same one.
        readSinglyInFilters effects =
          concatMap framedSlotsReadSingly (concatMap effectFilters effects)
            List.\\ concatMap framedSlotsReadSingly (concatMap (cardFilters . snd) (concatMap effectMintedFaces effects))
        -- The reading side's fourth carrier: a PlayerRef, whose slot reads are
        -- Resolve.playerRefSlots' own classification rather than a second one
        -- kept here -- SlotArity.One is exactly the arm that hands its slot to
        -- legalOne, and CR 608.2h's ControllerOfBound is the one such arm naming
        -- an OBJECT slot, which is the only kind boundPlurally mints. The
        -- traversals are Resolve's too, so an opcode gaining a PlayerRef field
        -- is fenced without an arm here.
        --
        -- No subtraction of a minted object's text, which readSinglyInFilters
        -- needs: neither traversal descends into the card a Create or a Conjure
        -- carries, so nothing a token prints is attributed to its creator.
        --
        -- CR 118.12a's payer and CR 603.5's asker are the same arm one carrier
        -- over -- a CLAUSE rather than an effect -- so faceClausePlayerRefs is
        -- what collects them and they arrive here through the same
        -- slotsReadSinglyIn.
        readSinglyInPlayerRefs effects =
          slotsReadSinglyIn
            (concatMap (\effect -> Resolve.effectPlayerRefs effect <> concatMap Resolve.objectRefPlayerRefs (Resolve.effectObjectRefs effect)) effects)
        -- The arity classification itself, shared by the effect leg above and the
        -- clause leg clashesIn folds in below.
        slotsReadSinglyIn refs =
          [ slot
          | ref <- refs,
            (slot, SlotArity.One) <- Map.toList (Resolve.playerRefSlots ref)
          ]
        -- The reading side's fifth carrier: a NUMBER. Resolve.quantitySlots'
        -- SlotArity.One entries are exactly the Quantity.AgainstSlot arms, which
        -- aim an inner number at the object a slot names and so reach
        -- Pawl.Engine.Filter.slotOneObject. A Quantity.InSlot reads the slot's
        -- AMOUNT instead and is reported at SlotArity.Amount, a different
        -- namespace rather than a singular read of this one; see #2772.
        --
        -- No subtraction of a minted object's text, which readSinglyInFilters
        -- needs: effectQuantities does not descend into the card a Create or a
        -- Conjure carries.
        readSinglyInQuantities effects =
          [ slot
          | effect <- effects,
            quantity <- effectQuantities effect,
            (slot, SlotArity.One) <- Map.toList (Resolve.quantitySlots quantity)
          ]
        -- The three READING sides at once: this resolution's own effects, the
        -- conditions of the delayed abilities it arms, and the PlayerRefs its
        -- CLAUSES hold. CR 603.7c is what puts the second there -- the entry
        -- captures the arming resolution's whole environment, so a slot an
        -- earlier clause bound is exactly what the condition names -- and
        -- DELAYED abilities alone, since a printed trigger has no captured
        -- environment for a card-authored slot to be in. The effects side is
        -- folded over four walks -- the slots named outright, the slots a Filter
        -- reads singly, the slots a PlayerRef does and the slots a Quantity does
        -- -- the conditions side over the two it has, and the clause side over
        -- the one arity classification the PlayerRef walks share.
        clashesIn effects conditions refs =
          not
            . Set.null
            $ Set.intersection
              (Set.fromList (concatMap boundPlurally effects))
              ( Set.unions
                  [ Set.fromList (concatMap readSingly effects),
                    Set.fromList (readSinglyInFilters effects),
                    Set.fromList (readSinglyInPlayerRefs effects),
                    Set.fromList (readSinglyInQuantities effects),
                    Set.fromList (concatMap triggerConditionSlots conditions),
                    Set.fromList (concatMap (concatMap framedSlotsReadSingly . triggerConditionFilters) conditions),
                    Set.fromList (slotsReadSinglyIn refs)
                  ]
              )
        clashes effects = clashesIn effects [] []
        faceClashes card = clashesIn (cardResolutionEffects card) (fmap TriggeredAbility.condition (Map.elems (Face.delayedAbilities card))) (faceClausePlayerRefs card)
        offenders = filter (anyFace faceClashes . Printing.card) ps
        binds effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ mSlot _ _ _) -> Maybe.isJust mSlot && not (movesAtMostOne ref)
          _ -> False
        bindsDiscarded effect = case effect of
          Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard _ _ mDiscarded)) -> Maybe.isJust mDiscarded
          _ -> False
        bindsTokens effect = case effect of
          Effect.Create (Create.MkCreate quantity _ _ mSlot _) -> Maybe.isJust mSlot && Resolve.namesEveryToken quantity
          _ -> False
        exiledSlot = SlotName.MkSlotName (Text.pack "exiled")
        destroyedSlot = SlotName.MkSlotName (Text.pack "destroyed")
        elsewhereSlot = SlotName.MkSlotName (Text.pack "elsewhere")
        removal slot = Effect.RemoveCounters (RemoveCounters.MkRemoveCounters CounterKind.PlusOnePlusOne (Quantity.Type.Literal 1) slot)
        destruction = Effect.Destroy (Destroy.MkDestroy (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Regenerability.Regenerable Nothing Nothing (Just destroyedSlot))
        -- An opcode whose ObjectRef carries a Filter, so effectFilters reports it
        -- (its Tap arm is `frame SourceHostFramed (objectRefFilters ref)`).
        -- Nothing about the opcode matters here: what is on trial is the atom
        -- inside the predicate.
        tapping f = Effect.Tap (ObjectRef.EachMatching f)
        -- An opcode holding a PlayerRef in a field of its own, Marchesa's Decree
        -- shape. Nothing about the opcode matters here either: what is on trial
        -- is the ARM of the reference.
        gaining ref = Effect.GainLife (PlayerQuantity.MkPlayerQuantity ref (Quantity.Type.Literal 1))
        -- The same reference NESTED IN AN OBJECTREF, CR 400.1's per-seat walk.
        revealing ref = Effect.Reveal (Reveal.MkReveal (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary ref (Quantity.Type.Literal 1))) Nothing)
        -- CR 608.2h's arm anywhere a PlayerRef sits in an effect, which is the
        -- corpus half of this leg. A wildcard because it is a presence probe:
        -- every other arm is not this one, however many there come to be.
        namesControllerOfBound ref = case ref of PlayerRef.ControllerOfBound _ -> True; _ -> False
        readsController effect =
          any
            namesControllerOfBound
            (Resolve.effectPlayerRefs effect <> concatMap Resolve.objectRefPlayerRefs (Resolve.effectObjectRefs effect))
        -- An opcode holding a QUANTITY in a field of its own, and the arm of that
        -- quantity which aims an inner number at the one object a slot names.
        -- Nothing about the opcode matters: what is on trial is the atom inside
        -- the number.
        counting quantity = Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) quantity Nothing)
        against slot = Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot slot Quantity.Type.Power)
        -- The corpus half of the number leg: does any card read ANY slot inside a
        -- number? A presence probe over the traversal rather than over the arm,
        -- since an effectQuantities answering [] everywhere would leave the leg
        -- silently inert.
        readsSlotInNumber effect = not (all (Set.null . QuantitySlot.slots) (effectQuantities effect))
        -- A face whose first clause binds the group and whose second carries the
        -- clause-level PlayerRef on trial. Built as a FACE rather than as an
        -- effect list because the walk from a face to its clauses is the half
        -- being proven; the second clause carries no effects at all, so nothing
        -- but the payer or the asker can name the slot.
        clauseFace gate optionality =
          (vanillaFace "Planted" instantLine)
            { Face.spell =
                Modal.MkModal
                  ( Seq.singleton
                      ( Mode.MkMode
                          ( Seq.fromList
                              [ Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton destruction),
                                Clause.MkClause Nothing Nothing Nothing optionality gate Seq.empty
                              ]
                          )
                          Map.empty
                      )
                  )
                  (ModeSelection.ChooseExactly 1)
            }
        payerFace ref = clauseFace (Just (PayGate.MkPayGate ref (Cost.Type.MkCost Nothing []) PayBranch.IfNotPaid PayObligation.Optional Nothing Nothing)) Optionality.Mandatory
        askerFace ref = clauseFace Nothing (Optionality.Optional ref)
        -- CR 111.1's token, whose OWN printed text carries that same predicate:
        -- Face.enchant is a Filter position cardFilters walks, so effectFilters
        -- splices it in beside the creating card's. Binds no slot itself --
        -- Nothing, at a literal one -- so it adds nothing to the binding side.
        tokenReading f =
          Effect.Create
            ( Create.MkCreate
                (Quantity.Type.Literal 1)
                (oneFaced (vanillaFace "Soldier" (spellLine CardType.Creature Set.empty Set.empty)) {Face.enchant = [TargetSlot.required Pool.Creatures (Just f)]})
                EntryRiders.defaultValue
                Nothing
                (PlayerRef.Relative PlayerRelation.You)
            )
        victimSlot = SlotName.MkSlotName (Text.pack "victim")
        discarding = Effect.Discard (Discard.Counted (CountedDiscard.MkCountedDiscard victimSlot (Quantity.Type.Literal 1) (Just destroyedSlot)))
        minting n = Effect.Create (Create.MkCreate (Quantity.Type.Literal n) (oneFaced (vanillaFace "Soldier" (spellLine CardType.Creature Set.empty Set.empty))) EntryRiders.defaultValue (Just destroyedSlot) (PlayerRef.Relative PlayerRelation.You))
    -- Half the rejected shape is in the pool: Act on Impulse binds a group. No
    -- card in data/cards/ pairs one with a singular read of the same slot, so the
    -- REJECTING direction is proven against a hand-built pair rather than by a
    -- corpus sweep -- the posture the phase-skip lint below takes against Eon Hub
    -- -- and the sweep is a fence against a future card authoring the shape.
    Spec.assertBool s (any (anyFace (any binds . cardResolutionEffects) . Printing.card) ps) "the pool has a card binding what a plural move minted"
    -- The same guard for the two arms added to the binding side: an arm no card
    -- reaches is a fence the corpus sweep can never exercise. Psychic Miasma is
    -- one that writes the first and Thatcher Revolt one that writes the second.
    Spec.assertBool s (any (anyFace (any bindsDiscarded . cardResolutionEffects) . Printing.card) ps) "the pool has a card binding what a counted discard moved"
    Spec.assertBool s (any (anyFace (any bindsTokens . cardResolutionEffects) . Printing.card) ps) "the pool has a card binding a batch of minted tokens"
    Spec.assertBool
      s
      ( clashes
          [ Effect.MoveToZone (MoveToZone.MkMoveToZone (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature)) Zone.Exile EntryRiders.defaultValue (Just exiledSlot) Nothing LibraryPlacement.defaultValue Nothing),
            Effect.TurnFaceUp exiledSlot
          ]
      )
      "a singular read of a plurally bound slot is caught"
    -- Neither TurnFaceUp nor MoveToZone is the only half. The enumeration above IS
    -- the lint, so a fence only TurnFaceUp's slot could trip would leave every
    -- other one-object instruction open. Paired with the board below, which
    -- differs in the slot name alone, so what catches is the intersection rather
    -- than `clashes` answering True for any two effects.
    Spec.assertBool s (clashes [destruction, removal destroyedSlot]) "a singular read outside TurnFaceUp is caught"
    Spec.assertBool s (not (clashes [destruction, removal elsewhereSlot])) "a singular read of another slot is left alone"
    -- The other funnel on the same board: ExileHaunting's haunting card is read
    -- through slotOne rather than through legalOne. Paired with AttachBound,
    -- which takes the same two slot names and is NOT an offender -- its first
    -- slot goes through objectRefObjects, which attaches every member of a group
    -- (CR 712.21c).
    Spec.assertBool s (clashes [destruction, Effect.ExileHaunting (ExileHaunting.MkExileHaunting destroyedSlot elsewhereSlot)]) "a slotOne read of a plurally bound slot is caught"
    Spec.assertBool s (not (clashes [destruction, Effect.AttachBound (AttachBound.MkAttachBound destroyedSlot elsewhereSlot)])) "a group-tolerant read of a plurally bound slot is left alone"
    -- The BINDING side's own two, each against the same singular read and each
    -- paired with the board that must stay legal. CR 701.9's counted discard
    -- binds unconditionally, so the pair differs in the slot name; CR 111.1's
    -- Create binds only where the card says "those tokens", so its pair differs
    -- in the COUNT alone -- which is what proves the guard rather than the arm.
    Spec.assertBool s (clashes [discarding, removal destroyedSlot]) "a singular read of a counted discard's slot is caught"
    Spec.assertBool s (not (clashes [discarding, removal elsewhereSlot])) "a singular read of another slot is left alone beside a discard"
    Spec.assertBool s (clashes [minting 2, removal destroyedSlot]) "a singular read of a batch of minted tokens is caught"
    Spec.assertBool s (not (clashes [minting 1, removal destroyedSlot])) "a singular read of ONE minted token is left alone"
    -- The reading side's other carrier: CR 603.7c hands the arming resolution's
    -- environment to a delayed ability, whose condition names a slot and reads
    -- it through Binding.objectSlots. Paired with the same condition over a slot
    -- the destruction never bound.
    Spec.assertBool s (clashesIn [destruction] [TriggerCondition.LoseControlOfBound destroyedSlot] []) "a delayed condition naming a plurally bound slot is caught"
    Spec.assertBool s (not (clashesIn [destruction] [TriggerCondition.LoseControlOfBound elsewhereSlot] [])) "a delayed condition naming another slot is left alone"
    -- The reading side's third carrier: a Filter an effect carries, where CR
    -- 608.2h's Filter.IsControllerOfBound reads the slot through
    -- Pawl.Engine.Filter.slotOneObject rather than through Resolve. Three boards
    -- differing in one thing each -- the ATOM against the group-tolerant
    -- Filter.IsBound, which is what proves the walk discriminates rather than
    -- reporting every slot a Filter names, and the SLOT against a name the
    -- destruction never bound.
    Spec.assertBool s (clashes [destruction, tapping (Filter.Type.IsControllerOfBound destroyedSlot)]) "a singular read inside a filter is caught"
    Spec.assertBool s (not (clashes [destruction, tapping (Filter.Type.IsBound destroyedSlot)])) "a group-tolerant read inside a filter is left alone"
    Spec.assertBool s (not (clashes [destruction, tapping (Filter.Type.IsControllerOfBound elsewhereSlot)])) "a singular read inside a filter of another slot is left alone"
    -- The same atom under a NEST, which is the half of that walk a top-level
    -- board cannot prove: an arm answering [] for Not would pass every assertion
    -- above.
    Spec.assertBool s (clashes [destruction, tapping (Filter.Type.Not (Filter.Type.IsControllerOfBound destroyedSlot))]) "a singular read nested inside a filter is caught"
    -- And the same atom in a DELAYED ability's condition, the other carrier
    -- clashesIn folds the walk over. Paired with the slot the destruction never
    -- bound, so the pair differs in the slot name alone.
    Spec.assertBool s (clashesIn [destruction] [TriggerCondition.PermanentDies (Filter.Type.IsControllerOfBound destroyedSlot)] []) "a singular read inside a delayed condition's filter is caught"
    Spec.assertBool s (not (clashesIn [destruction] [TriggerCondition.PermanentDies (Filter.Type.IsControllerOfBound elsewhereSlot)] [])) "a singular read inside a delayed condition's filter of another slot is left alone"
    -- The PlayerRef carrier, where CR 608.2h's ControllerOfBound names an OBJECT
    -- slot and reads it through Resolve.playerRefPlayers' legalOne. Three boards
    -- differing in one thing
    -- each -- the ARM against the group-tolerant PlayerRef.EachInSlot, which is
    -- what proves the walk reads Resolve.playerRefSlots' arity rather than
    -- reporting every slot a reference names, and the SLOT against a name the
    -- destruction never bound.
    Spec.assertBool s (clashes [destruction, gaining (PlayerRef.ControllerOfBound destroyedSlot)]) "a singular read inside a player reference is caught"
    Spec.assertBool s (not (clashes [destruction, gaining (PlayerRef.EachInSlot destroyedSlot)])) "a group-tolerant read inside a player reference is left alone"
    Spec.assertBool s (not (clashes [destruction, gaining (PlayerRef.ControllerOfBound elsewhereSlot)])) "a singular read inside a player reference of another slot is left alone"
    -- The same arm NESTED IN AN OBJECTREF, which is the half a top-level field
    -- cannot prove: an objectRefPlayerRefs answering [] for the library walk
    -- would pass all three assertions above.
    Spec.assertBool s (clashes [destruction, revealing (PlayerRef.ControllerOfBound destroyedSlot)]) "a singular read inside an object reference's own player reference is caught"
    -- And the guard that keeps the leg from fencing a shape no card writes:
    -- Rampage of the Clans' creator, Belltower Sphinx's miller, Marchesa's
    -- Decree's loser and Spikeshell Harrier's slowed player all print one.
    Spec.assertBool s (any (anyFace (any readsController . cardResolutionEffects) . Printing.card) ps) "the pool has a card naming a player by the controller of a bound object"
    -- The boundary the leg above must not cross: a MINTED object's text is read
    -- in a resolution of its own, so the token's slot names are not this card's
    -- (#2735). The pair differs in nothing but whose text the atom sits in --
    -- the accepted board here and the rejected one four assertions up carry the
    -- same filter over the same slot.
    Spec.assertBool s (not (clashes [destruction, tokenReading (Filter.Type.IsControllerOfBound destroyedSlot)])) "a minted token's own filter is not the creating card's read"
    -- And the subtraction that buys it is a MULTISET one: the creating card's
    -- own read of the same slot survives the token reading it too. Without this
    -- leg a set difference would pass the assertion above and silently mask a
    -- real offender.
    Spec.assertBool s (clashes [destruction, tapping (Filter.Type.IsControllerOfBound destroyedSlot), tokenReading (Filter.Type.IsControllerOfBound destroyedSlot)]) "the creating card's own read survives a token reading the same slot"
    -- The reading side's fifth carrier: a NUMBER. CR 115.10a's group is what an
    -- earlier effect of the same resolution bound, and Quantity.AgainstSlot aims
    -- an inner number at the ONE object a slot names, through
    -- Pawl.Engine.Filter.slotOneObject. Paired with the same number over a slot
    -- the destruction never bound.
    Spec.assertBool s (clashes [destruction, counting (against destroyedSlot)]) "a singular read inside a number is caught"
    Spec.assertBool s (not (clashes [destruction, counting (against elsewhereSlot)])) "a singular read inside a number of another slot is left alone"
    -- The same atom under a NEST, which is the half a top-level number cannot
    -- prove: a QuantitySlot.slots answering the empty set for Negate would pass
    -- both assertions above.
    Spec.assertBool s (clashes [destruction, counting (Quantity.Type.Negate (against destroyedSlot))]) "a singular read nested inside a number is caught"
    -- The OTHER slot-naming arm of a number, on the same board: Quantity.InSlot
    -- reads Binding.amount through Pawl.Engine.Binding.amountOf and reaches
    -- slotOneObject nowhere, so the group a plural binder wrote at that same
    -- name is not what it asks for and the card is legal. The pair
    -- differs from the caught board in the ARM alone -- same slot, same opcode --
    -- so it proves the walk discriminates rather than reporting every slot a
    -- number names.
    Spec.assertBool s (not (clashes [destruction, counting (Quantity.Type.InSlot destroyedSlot)])) "an amount read inside a number is left alone"
    -- And the two arms in ONE number, which is the half neither board above can
    -- prove: a walk that dropped the whole quantity on seeing an amount read
    -- would pass both of them and silently stop catching the object read beside
    -- it.
    Spec.assertBool s (clashes [destruction, counting (Quantity.Type.Plus (Plus.MkPlus (Quantity.Type.InSlot destroyedSlot) (against destroyedSlot)))]) "an object read beside an amount read of the same slot is still caught"
    -- And the guard that keeps the leg from being silently inert: the pool reads
    -- a slot inside a number -- Rabid Bite's damage is its dealer's power, aimed
    -- at the dealer's own slot.
    Spec.assertBool s (any (anyFace (any readsSlotInNumber . cardResolutionEffects) . Printing.card) ps) "the pool has a card reading a slot inside a number"
    -- The PlayerRef carrier one step out: CR 118.12a's payer and CR 603.5's asker
    -- sit on a CLAUSE, so this pair goes through faceClashes rather than through
    -- `clashes` -- the walk from a face to its clauses is what is on trial. Three
    -- boards differing in one thing each, the PlayerRef leg's own posture: the
    -- ARM against the group-tolerant PlayerRef.EachInSlot, and the SLOT against a
    -- name the destruction never bound.
    Spec.assertBool s (faceClashes (payerFace (PlayerRef.ControllerOfBound destroyedSlot))) "a resolution cost's payer naming a plurally bound slot is caught"
    Spec.assertBool s (not (faceClashes (payerFace (PlayerRef.EachInSlot destroyedSlot)))) "a group-tolerant payer is left alone"
    Spec.assertBool s (not (faceClashes (payerFace (PlayerRef.ControllerOfBound elsewhereSlot)))) "a payer naming another slot is left alone"
    -- The same arm at the OTHER clause position, which the payer's boards cannot
    -- prove: a clausePlayerRefs answering [] for Optionality.Optional would pass
    -- all three above.
    Spec.assertBool s (faceClashes (askerFace (PlayerRef.ControllerOfBound destroyedSlot))) "a may's asker naming a plurally bound slot is caught"
    Spec.assertBool s (not (faceClashes (askerFace (PlayerRef.ControllerOfBound elsewhereSlot)))) "an asker naming another slot is left alone"
    -- And the guard that keeps this leg from fencing a shape no card writes: Mana
    -- Leak, Clash of Wills, Mystic Confluence and Don't Make a Sound all offer
    -- their cost to the targeted spell's controller, and Amulet of Safekeeping to
    -- the controller of the object that did the targeting.
    Spec.assertBool s (any (anyFace (any namesControllerOfBound . faceClausePlayerRefs) . Printing.card) ps) "the pool has a card naming a clause's player by the controller of a bound object"
    Spec.assertEqWith s "a group binding is invisible to a singular reader" (fmap (S.nameOf . Printing.card) offenders) []
  -- OwnerChooses asks a player which END of a library a card arrives at (CR
  -- 401.2), and only a library HAS ends -- so on any other destination it would
  -- put a question on the wire with no board behind it. A stated position on a
  -- non-library move is merely inert card data; this one is not, which is why it
  -- gets a lint of its own.
  Spec.it s "no MoveToZone leaves the end to an owner off a library" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone _ _ _ LibraryPlacement.OwnerChooses _) -> zone /= Zone.Library
          _ -> False
        asks effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ _ _ _ LibraryPlacement.OwnerChooses _) -> True
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no owner-chosen end in the pool this
    -- would pass whatever a card said. Aetherspouts is the card that prints one.
    Spec.assertBool s (any (anyFace (any asks . cardResolutionEffects) . Printing.card) ps) "the pool has a card leaving the end to each owner"
    Spec.assertEqWith s "only a library has ends" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 608.2d: a choice an effect offers is announced while the effect is
  -- applied, so an opcode that gathers its objects through the pure
  -- Pawl.Engine.Resolve.Slots.objectRefObjects cannot make one. Three arms of Resolve
  -- reach the Game monad and ask instead, over DIFFERENT subsets -- see Asks --
  -- and a chooser-shaped ref written anywhere else names no object, so that share
  -- of the instruction is skipped (CR 101.3, CR 609.3) with nothing on the wire
  -- to show it -- so the card is silently weaker than printed, which is what this
  -- rejects at load time.
  Spec.it s "no effect asks for a chosen card where nothing can ask" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (not . all (null . inertChoosers) . cardAuthoredEffects) . Printing.card) ps
        asks = any (anyFace (any (any (chooserRef . snd) . effectObjectRefs) . cardAuthoredEffects) . Printing.card) ps
    -- Guards against a vacuous sweep in the ONE direction it can guard: with no
    -- chooser-shaped ref in the pool at all this would pass whatever a card said.
    -- Exhume, Elvish Piper, Commune with the Gods and Merfolk Spy print them.
    Spec.assertBool s asks "the pool has a card asking a player to pick a card"
    Spec.assertEqWith s "a chosen card under an opcode that cannot ask (CR 608.2d)" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep above still passes VACUOUSLY on the half that matters: every
  -- committed card writes its chooser under one of the five asking pairs, so the
  -- sweep proves nothing about the lint. Both directions are proven here instead,
  -- against hand-built effects (never a card file -- a misauthored card must not
  -- be loadable).
  --
  -- Every asking arm gets an accept case AND a reject case, and the four
  -- degenerate classifications this rules out are why. "Every pair asks" is ruled
  -- out by Transform's four rejects and by MoveToZone's random arm; "MoveToZone
  -- asks everything" by that same random arm (#1733); "the three chosen arms
  -- always ask, the random one never does" by Reveal accepting the random arm and
  -- rejecting both zone-keyed chosen ones; "the battlefield subset asks
  -- everywhere" by the last assertion, which rejects it at two sites and
  -- accepts it at two.
  Spec.it s "the lint itself catches a chosen card under an opcode that cannot ask" $ do
    exhume <- S.printingOf s registry "Exhume"
    let anyCard = Filter.Type.HasCardType CardType.Creature
        group = SlotName.MkSlotName (Text.pack "revealed")
        inGraveyard = ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard Chooser.TheController (ZoneScope.Scoped PlayerScope.You) anyCard)
        inHand = ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand (PlayerRef.Relative PlayerRelation.You) anyCard)
        fromAmong = ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong group anyCard (Quantity.Type.Literal 1) (PlayerRef.Relative PlayerRelation.You))
        atRandom = ObjectRef.RandomCardInHand (RandomCardInHand.MkRandomCardInHand (PlayerRef.Relative PlayerRelation.You) anyCard (Quantity.Type.Literal 1))
        anyNumber = ObjectRef.AnyNumberMatching anyCard
        onePermanent = ObjectRef.ChosenPermanent anyCard
        sourceAndOne = ObjectRef.SourceAndChosenPermanent anyCard
        moves ref = Effect.MoveToZone (MoveToZone.MkMoveToZone ref Zone.Battlefield EntryRiders.defaultValue Nothing Nothing LibraryPlacement.defaultValue Nothing)
        reveals ref = Effect.Reveal (Reveal.MkReveal ref Nothing)
        inert :: [Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)] -> [Bool]
        inert = fmap (not . null . inertChoosers)
    Spec.assertEqWith
      s
      "MoveToZone's gather asks the three chosen arms and not the random one (#1733)"
      (inert (fmap moves [inGraveyard, inHand, fromAmong, atRandom]))
      [False, False, False, True]
    Spec.assertEqWith
      s
      "CR 701.20a's reveal asks from-among and at-random, and neither zone-keyed chosen arm"
      (inert (fmap reveals [inGraveyard, inHand, fromAmong, atRandom]))
      [True, True, False, False]
    -- Tovolar, Dire Overlord's opcode. CR 701.27a turns over PERMANENTS, so its
    -- gather asks for the battlefield subset and for NONE of the four
    -- card-shaped arms: a chosen card written here would still be inert.
    Spec.assertEqWith
      s
      "the transform gather asks none of the four card-shaped chosen arms"
      (inert (fmap Effect.Transform [inGraveyard, inHand, fromAmong, atRandom]))
      [True, True, True, True]
    -- The fourth degenerate classification, and the one widening the matrix for
    -- #774 introduced: "the new arm asks everywhere". Accepted under the two
    -- gathers that ask it -- Transform's and MoveToZone's -- and rejected under
    -- Reveal's arm, which asks other arms but not this one, and under Tap, an
    -- AsksNothing opcode. Without this leg an asksFor row answering True for the
    -- new arm at every site would pass the three assertions above unchanged.
    Spec.assertEqWith
      s
      "the battlefield subset is asked by the transform and move gathers and by neither other site"
      (inert [Effect.Transform anyNumber, moves anyNumber, reveals anyNumber, Effect.Tap anyNumber])
      [False, False, True, True]
    -- The singular of the arm above, asked by the MoveToZone gather alone --
    -- Hanweir Battlements' "exile them, then meld them", the printing that wanted
    -- it. Rejected under Transform and Reveal, the two other sites that ask any
    -- chooser-shaped arm, and under Tap, an AsksNothing opcode. This also asserts
    -- the arm reaches chooserRef at all -- an arm missing from THAT traversal
    -- would answer False here at every site and no -Werror would name it.
    Spec.assertEqWith
      s
      "one chosen permanent is asked by the move gather alone"
      (inert [Effect.Transform onePermanent, moves onePermanent, reveals onePermanent, Effect.Tap onePermanent])
      [True, False, True, True]
    -- The arm above with the source named alongside the choice, and the same row:
    -- Hanweir Battlements' "exile them" is a move, so the MoveToZone gather is
    -- again the only site that asks it. Asserted separately rather than folded
    -- into the row above because the two arms are distinct constructors, and one
    -- missing from chooserRef or from asksFor would answer False everywhere with
    -- no -Werror to name it.
    Spec.assertEqWith
      s
      "the source with one chosen permanent is asked by the move gather alone"
      (inert [Effect.Transform sourceAndOne, moves sourceAndOne, reveals sourceAndOne, Effect.Tap sourceAndOne])
      [True, False, True, True]
    -- CR 701.28a's convert, classified with Transform because it IS Transform's
    -- gather (Pawl.Engine.Resolve.Effect.turnPermanentsOver): the same four card-shaped
    -- arms are inert under it and the same battlefield subset is asked.
    Spec.assertEqWith
      s
      "the convert gather answers exactly as the transform gather does"
      (inert (fmap Effect.Convert [inGraveyard, inHand, fromAmong, atRandom, anyNumber]))
      [True, True, True, True, False]
    -- The other direction on the same opcode: a ref that is a READ rather than a
    -- CR 608.2d question is fine anywhere, so the lint is about the arm and not
    -- about Transform.
    Spec.assertBool
      s
      (null (inertChoosers (Effect.Transform (ObjectRef.InSlot group))))
      "a ref that reads rather than asks is accepted under the same opcode"
    Spec.assertBool
      s
      (all (null . inertChoosers) (cardAuthoredEffects (S.combinedFace exhume)))
      "the real card's chosen graveyard card is accepted"
  -- effectObjectRefs and effectFilters are twins -- one takes each ObjectRef
  -- position, the other takes that ref's Filters -- and nothing but this compares
  -- them, so an opcode added to effectFilters and forgotten here would leave the
  -- lint above silently blind to it.
  --
  -- SETS, not lists: effectFilters recurses into a nested effect list of its own
  -- while effectObjectRefs leaves that to cardResolutionEffects' closure, so the
  -- two differ in how often they reach a nested ref and never in WHICH. Sound
  -- because effectNestedEffects reaches a superset of effectFilters' own
  -- recursion (replacementPrintedEffects contains replacementEffectRiders).
  --
  -- SourceHostFramed and not `hostFramed`: inside an Effect that tag means "an
  -- ObjectRef's filter" to this comparison, which is why a stored Effect.Replace's
  -- own row carries ReplacementRowFramed instead.
  --
  -- `frame SourceHostFramed` on the asking side, exactly as effectFilters applies
  -- it, so the two sides classify a ref's Filters by the same rule. That excludes
  -- the CR 122.1b counter kind a library depth may name from BOTH sides rather
  -- than from one: keywordFilters tags it KeywordFramed and `frame` fills in only
  -- the Unframed, so it is no more an ObjectRef position here than a keyword
  -- written anywhere else is (#2740).
  Spec.it s "every ObjectRef position the Filter traversal reaches is one the asking traversal reaches" $ do
    ps <- S.allPrintings s
    let viaRefs card = Set.fromList [f | (_, ref) <- concatMap effectObjectRefs (cardResolutionEffects card), (SourceHostFramed, f) <- frame SourceHostFramed (objectRefFilters ref)]
        offenders = filter (anyFace (\card -> viaFilters card /= viaRefs card) . Printing.card) ps
    Spec.assertEqWith s "the two ObjectRef traversals agree" (fmap (S.nameOf . Printing.card) offenders) []
    -- The subtraction is PER EFFECT, and this is what says so. Both sides are
    -- sets of filter VALUES, so a card-wide subtraction cancels a filter wherever
    -- ANY minted face carries an equal one -- and `And []` is the corpus's
    -- commonest filter, which is how the Nightmare's own filter hid the +1's
    -- ChosenCardFromAmong before #3279 tagged it. Here the creating effect's token
    -- and a SECOND effect's ObjectRef carry the same predicate: the token's copy
    -- is cancelled and the ObjectRef's survives, where a card-wide subtraction
    -- takes both and leaves the lint unable to see a miss on the second effect.
    --
    -- Proven here rather than by the sweep above, which is green under either
    -- subtraction: no printing in the pool pairs a minted face with a second
    -- effect reading an equal filter, so the sweep cannot tell them apart.
    let victim = Filter.Type.HasCardType CardType.Creature
        reading = Effect.Transform (ObjectRef.EachMatching victim)
        oneEffectSpell effects =
          Modal.MkModal
            (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.fromList effects))) Map.empty))
            (ModeSelection.ChooseExactly 1)
        token =
          oneFaced
            (vanillaFace "Nightmare" (spellLine CardType.Creature Set.empty Set.empty))
              { Face.spell = oneEffectSpell [reading]
              }
        creating = Effect.Create (Create.MkCreate (Quantity.Type.Literal 1) token EntryRiders.defaultValue Nothing (PlayerRef.Relative PlayerRelation.You))
        collided = (vanillaFace "Collided" instantLine) {Face.spell = oneEffectSpell [creating, reading]}
    Spec.assertBool s (Set.member victim (mintedOwn creating)) "the minted face does carry the predicate"
    Spec.assertBool s (Set.member victim (viaFilters collided)) "a minted face cancels its own effect's position and not another effect's"
  -- CR 406.3's rider is a rule about the EXILE ZONE, so on any other destination
  -- it is inert card data, and on a Create it is inert outright -- a token is
  -- created onto the battlefield, and CR 111.7 makes one anywhere else cease to
  -- exist, so no Create ever reaches exile. Event.changeZoneAttaching
  -- gates on the destination, so this lints an authoring mistake rather than
  -- guarding the engine.
  Spec.it s "no effect exiles face down anywhere but exile" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone riders _ _ _ _) -> EntryRiders.exiledFaceDown riders && zone /= Zone.Exile
          Effect.Create (Create.MkCreate _ _ riders _ _) -> EntryRiders.exiledFaceDown riders
          _ -> False
        hides effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> EntryRiders.exiledFaceDown riders
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no face-down exile in the pool at all
    -- this would pass whatever a card said. Ignorant Bliss is the card that
    -- prints one.
    Spec.assertBool s (any (anyFace (any hides . cardResolutionEffects) . Printing.card) ps) "the pool has a card exiling face down"
    Spec.assertEqWith s "only exile keeps a card face down (CR 406.3)" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 509.4's rider is a rule about entering the BATTLEFIELD, so on any other
  -- destination it is inert card data. Both opcodes read it and both apply it --
  -- Pawl.Engine.Resolve's Create arm hands its tokens to
  -- Combat.putOntoBattlefieldBlocking and its MoveToZone arm hands the card it
  -- moved to the same function -- so this lints an authoring mistake rather than
  -- guarding the engine.
  --
  -- Only the MoveToZone can BE mis-zoned, which is why the Create is not an
  -- offender here where it is one in the exile lint above: a Create names no
  -- destination, minting onto the battlefield, and CR 111.7 would make a token
  -- anywhere else cease to exist anyway.
  Spec.it s "no effect enters blocking anywhere but the battlefield" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone riders _ _ _ _) -> Maybe.isJust (EntryRiders.blocking riders) && zone /= Zone.Battlefield
          _ -> False
        creates effect = case effect of
          Effect.Create (Create.MkCreate _ _ riders _ _) -> Maybe.isJust (EntryRiders.blocking riders)
          _ -> False
        moves effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> Maybe.isJust (EntryRiders.blocking riders)
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep, one per road, since a lint asserting an
    -- empty offenders list proves nothing on its own: Flash Foliage creates a
    -- token that's blocking, and Aetherplasm moves a CARD out of a hand onto the
    -- battlefield blocking.
    Spec.assertBool s (any (anyFace (any creates . cardResolutionEffects) . Printing.card) ps) "the pool has a card creating a token that's blocking"
    Spec.assertBool s (any (anyFace (any moves . cardResolutionEffects) . Printing.card) ps) "the pool has a card moving a card onto the battlefield blocking"
    Spec.assertEqWith s "only the battlefield can be entered blocking (CR 509.4)" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sibling lint for the OTHER face-down rider, one field over and pointed
  -- at the opposite zone: CR 708.3 is a rule about entering the BATTLEFIELD, so
  -- on any other destination it is inert card data. Inert on a Create outright,
  -- for the reason CR 712.14a's transformed rider is -- a token is not a card,
  -- and no rule puts one onto the battlefield face down.
  -- Event.changeZoneEntering gates on the destination, so this lints an
  -- authoring mistake rather than guarding the engine.
  Spec.it s "no effect enters face down anywhere but the battlefield" $ do
    ps <- S.allPrintings s
    let offends effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ zone riders _ _ _ _) -> Maybe.isJust (EntryRiders.faceDown riders) && zone /= Zone.Battlefield
          Effect.Create (Create.MkCreate _ _ riders _ _) -> Maybe.isJust (EntryRiders.faceDown riders)
          _ -> False
        entersFaceDown effect = case effect of
          Effect.MoveToZone (MoveToZone.MkMoveToZone _ _ riders _ _ _ _) -> Maybe.isJust (EntryRiders.faceDown riders)
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no face-down entry in the pool at all
    -- this would pass whatever a card said. Soul Summons and Cloudform manifest
    -- (CR 701.40a) and Yedora, Grave Gardener lists its own characteristics (CR
    -- 708.2a), so the guard holds under either shape of the rider.
    Spec.assertBool s (any (anyFace (any entersFaceDown . cardResolutionEffects) . Printing.card) ps) "the pool has a card putting a permanent onto the battlefield face down"
    Spec.assertEqWith s "only the battlefield takes a face-down entry (CR 708.3)" (fmap (S.nameOf . Printing.card) offenders) []
  -- Effect.CreateCopy carries the SAME EntryRiders record Create and MoveToZone
  -- do, but Pawl.Engine.Resolve's arm reads only CR 122.6's `counters` -- what
  -- Littjara Mirrorlake's "except it enters with an additional +1/+1 counter on
  -- it" says. This is the fence for every other field, and ONE lint rather than a
  -- CreateCopy arm added to each above: the lints above each fence one field
  -- across the opcodes that carry it, where here every field but one is unread,
  -- so a card setting any of them would say something nothing performs.
  --
  -- Why each is unread rather than merely unwired. `underOwner` is inert by CR
  -- 111.2, which makes the creating player a token's owner anyway. `transformed`
  -- and `faceDown` are inert for the reason the two lints above give: a token is
  -- not a card (CR 111.1), and CR 707.8a decides a copy token's face by copy rules.
  -- `exiledFaceDown` is inert because a token is created onto the battlefield and
  -- CR 111.7 would end one anywhere else. `tapped`, `attacking` and `blocking`
  -- are the three a printing could really write, and none is implemented (gap
  -- #2302).
  Spec.it s "no CreateCopy carries an entry rider but CR 122.6's counters" $ do
    ps <- S.allPrintings s
    let bare riders = riders == EntryRiders.defaultValue {EntryRiders.counters = EntryRiders.counters riders}
        offends effect = case effect of
          Effect.CreateCopy (CreateCopy.MkCreateCopy _ _ riders) -> not (bare riders)
          _ -> False
        counters effect = case effect of
          Effect.CreateCopy (CreateCopy.MkCreateCopy _ _ riders) -> not (Map.null (EntryRiders.counters riders))
          _ -> False
        offenders = filter (anyFace (any offends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no copy token carrying a rider at all
    -- this would pass whatever the arm read. Littjara Mirrorlake is the card that
    -- prints one.
    Spec.assertBool s (any (anyFace (any counters . cardResolutionEffects) . Printing.card) ps) "the pool has a card creating a copy token with counters on it"
    Spec.assertEqWith s "a copy token reads only CR 122.6's counters" (fmap (S.nameOf . Printing.card) offenders) []
  -- The lint this used to be held that no printing authored a WithCounters
  -- turn-up rewrite, which is what Pawl.Engine.Replacement.applies rested on when
  -- it gated that whole rewrite CLASS on CR 702.37b's "if its megamorph cost was
  -- paid". Bubble Smuggler authors one, so the condition moved onto the ROW it
  -- belongs to (TurnUpR.requiring) and this moved with it; see #987. What must
  -- stay unauthored is the CONDITION, not the rewrite.
  --
  -- A card's own CR 614.1e clause states no procedure -- it applies down every
  -- road the rule reaches -- so a printing writing one would be claiming a rule it
  -- has not got. Pawl.Types.PhasePattern's whosePhase is the same shape of field
  -- and the lint below it the same shape of sweep.
  --
  -- Guarded against a vacuous sweep by the rows themselves: Gift of Doom and
  -- Bubble Smuggler print the two TurnUpRewrites between them, so an empty
  -- offender list is a fact about what those rows say rather than about the pool
  -- reaching TurnUpR at all.
  Spec.it s "no printing authors a turn-up procedure condition (CR 702.37b)" $ do
    ps <- S.allPrintings s
    let writes p = anyFace (any p . cardReplacementEffects) . Printing.card
        offenders = filter (writes turnUpRequiringOffends) ps
        rows = filter (writes isTurnUpR) ps
    Spec.assertBool s (not (null rows)) "the pool prints a turn-up replacement at all (Gift of Doom, Bubble Smuggler)"
    Spec.assertEqWith s "requiring is baked by the engine, never authored" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep passes because the pool is authored correctly, so the REJECTING
  -- direction is proven here against Bubble Smuggler with a procedure baked into
  -- it -- never a card file, since a card that offends a lint must not be
  -- loadable. The whosePhase pair below is the model.
  Spec.it s "the lint itself catches a baked turn-up procedure" $ do
    smuggler <- S.printingOf s registry "Bubble Smuggler"
    let card = S.combinedFace smuggler
        bake replacement = case replacement of
          ReplacementEffect.TurnUpR turnUpR ->
            ReplacementEffect.TurnUpR turnUpR {TurnUpR.requiring = Just TurnUpProcedure.Morph}
          other -> other
        baked = card {Face.replacementEffects = fmap (\pr -> pr {PrintedReplacement.effect = bake (PrintedReplacement.effect pr)}) (Face.replacementEffects card)}
    Spec.assertBool s (not (any turnUpRequiringOffends (cardReplacementEffects card))) "the real Bubble Smuggler names no procedure and is accepted"
    Spec.assertBool s (any turnUpRequiringOffends (cardReplacementEffects baked)) "and the same card naming CR 702.37e's is rejected"
  -- The sibling of the lint above, for the OTHER PlayerId the engine bakes and
  -- the codec accepts. See phasePatternOffends for why a card cannot name a
  -- player, and for why this is a lint rather than a type split (#437).
  Spec.it s "no card authors a player-scoped phase skip (#437)" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any phasePatternOffends . cardReplacementEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no PhaseR in the pool at all this
    -- would pass whatever the classification said. Eon Hub is the card that
    -- prints one.
    Spec.assertBool s (any (anyFace (any isPhaseR . cardReplacementEffects) . Printing.card) ps) "the pool has a card printing a phase skip"
    Spec.assertEqWith s "whosePhase is baked by the engine, never authored" (fmap (S.nameOf . Printing.card) offenders) []
  -- The sweep passes because the pool is authored correctly, so the REJECTING
  -- direction is proven here against Eon Hub with a seat baked into it -- never
  -- a card file, since a card that offends a lint must not be loadable.
  Spec.it s "the lint itself catches a baked whosePhase" $ do
    eonHub <- S.printingOf s registry "Eon Hub"
    let card = S.combinedFace eonHub
        bake replacement = case replacement of
          ReplacementEffect.PhaseR phasePattern ->
            ReplacementEffect.PhaseR phasePattern {PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)}
          other -> other
        baked = card {Face.replacementEffects = fmap (\pr -> pr {PrintedReplacement.effect = bake (PrintedReplacement.effect pr)}) (Face.replacementEffects card)}
    Spec.assertBool s (not (any phasePatternOffends (cardReplacementEffects card))) "the real Eon Hub is symmetric and accepted"
    Spec.assertBool s (any phasePatternOffends (cardReplacementEffects baked)) "and the same card naming a seat is rejected"
  -- The same lint one event class over, for the OTHER fields the codec accepts and
  -- only the engine writes: CR 615.7's shielded recipient and its remaining amount,
  -- plus CR 122.1c's minted pair. See engineOnlyOffends.
  Spec.it s "no card authors a recipient-scoped damage pattern or an engine-minted shield" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any engineOnlyOffends . cardReplacementEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: Fog is the card that prints a DamageR.
    Spec.assertBool s (any (anyFace (any isDamageR . cardReplacementEffects) . Printing.card) ps) "the pool has a card printing a damage replacement"
    Spec.assertEqWith s "a shield is baked by the engine, never authored" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Fog rather than a card file, exactly
  -- as the Eon Hub case above is.
  Spec.it s "the lint itself catches a baked shield" $ do
    fog <- S.printingOf s registry "Fog"
    -- Fog's DamageR is installed by a resolution effect rather than printed as a
    -- static replacement ability, so the baking here is on what
    -- cardReplacementEffects reports rather than on Face.replacementEffects --
    -- which is the sweep's own input either way.
    let printed = cardReplacementEffects (S.combinedFace fog)
        bakeRecipient replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern rewrite riders) ->
            ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern {DamagePattern.whichRecipient = Just (Recipient.ToPlayer (PlayerId.MkPlayerId 1))} rewrite riders)
          other -> other
        bakeShield replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ riders) -> ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern (DamageRewrite.PreventNext 4) riders)
          other -> other
        bakeCounterShield replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ riders) -> ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern DamageRewrite.PreventRemovingShieldCounter riders)
          other -> other
        bakeRedirect replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ riders) -> ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern (DamageRewrite.Redirect (Recipient.ToCreature (ObjectId.MkObjectId 7))) riders)
          other -> other
        printRedirect replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ riders) -> ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern (DamageRewrite.RedirectMatching Filter.Type.IsHostOfSource) riders)
          other -> other
    Spec.assertBool s (any isDamageR printed) "setup: Fog prints a damage replacement to bake"
    Spec.assertBool s (not (any engineOnlyOffends printed)) "the real Fog names no recipient and counts nothing"
    Spec.assertBool s (any (engineOnlyOffends . bakeRecipient) printed) "the same effect naming a shielded player is rejected"
    Spec.assertBool s (any (engineOnlyOffends . bakeShield) printed) "and so is one counting a shield down"
    -- CR 122.1c's prevention, which only Projection.shieldOf may mint.
    Spec.assertBool s (any (engineOnlyOffends . bakeCounterShield) printed) "and so is one removing a shield counter"
    -- CR 614.9's destination, the pair this lint was blind to (#2378): a
    -- Recipient names an id and only Resolve's RedirectDamage arm may write one,
    -- where the printed twin describes and is accepted. The two assertions
    -- together are the lint, since either alone passes on a predicate that
    -- answered one way for every redirect.
    Spec.assertBool s (any (engineOnlyOffends . bakeRedirect) printed) "and so is a redirection naming its destination by id"
    Spec.assertBool s (not (any (engineOnlyOffends . printRedirect) printed)) "while one describing that destination is accepted"
    Spec.assertBool s (engineOnlyOffends (ReplacementEffect.DestructionR DestructionRewrite.RemoveShieldCounter)) "and so is CR 122.1c's destruction half"
    Spec.assertBool s (not (engineOnlyOffends (ReplacementEffect.DestructionR DestructionRewrite.Regenerate))) "while CR 701.19a's printed regeneration is accepted"
    -- CR 122.1d's row, which only Projection.stunOf may mint -- the whole arm,
    -- with no printed rewrite beside it to accept.
    Spec.assertBool s (engineOnlyOffends (ReplacementEffect.UntapR UntapRewrite.RemoveStunCounter)) "and so is CR 122.1d's untap replacement"
  -- CR 615.5's rider is a PREVENTION effect's, which the type cannot say. See
  -- riderWithoutPreventionOffends.
  Spec.it s "no card hangs CR 615.5's additional effect off a rewrite that prevents nothing" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any riderWithoutPreventionOffends . cardReplacementEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: Stormwild Capridor is the card that prints
    -- a rider at all.
    Spec.assertBool s (any (anyFace (any hasRider . cardReplacementEffects) . Printing.card) ps) "the pool has a card printing a CR 615.5 rider"
    Spec.assertEqWith s "and every one of them rides a prevention" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Stormwild Capridor rather than a
  -- card file, exactly as the two cases above are.
  Spec.it s "the lint itself catches a rider on a rewrite that prevents nothing" $ do
    capridor <- S.printingOf s registry "Stormwild Capridor"
    let printed = cardReplacementEffects (S.combinedFace capridor)
        unprevent replacement = case replacement of
          ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern _ riders) -> ReplacementEffect.DamageR (DamageR.MkDamageR damagePattern (DamageRewrite.Scale (Scaling.Multiply 2)) riders)
          other -> other
    Spec.assertBool s (any hasRider printed) "setup: the Capridor prints a rider to move"
    Spec.assertBool s (not (any riderWithoutPreventionOffends printed)) "the real Capridor hangs it off a prevention"
    Spec.assertBool s (any (riderWithoutPreventionOffends . unprevent) printed) "and the same rider on a doubling is rejected"
  -- CR 614.1a: a token row has to DO something. See idleTokenRowOffends.
  Spec.it s "no card prints a token replacement that neither scales nor appends" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any idleTokenRowOffends . cardReplacementEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: Doubling Season, Queen Allenal of
    -- Ruadach and Quina, Qu Gourmet are the cards that print a token row.
    Spec.assertBool s (any (anyFace (any isTokenR . cardReplacementEffects) . Printing.card) ps) "the pool has a card printing a token replacement"
    Spec.assertEqWith s "and every one of them scales or appends" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Queen Allenal rather than a card
  -- file, exactly as the rider lint above is.
  Spec.it s "the lint itself catches a token row that does nothing" $ do
    queen <- S.printingOf s registry "Queen Allenal of Ruadach"
    let printed = cardReplacementEffects (S.combinedFace queen)
        idle replacement = case replacement of
          ReplacementEffect.TokenR (TokenR.MkTokenR tokenPattern _ _) -> ReplacementEffect.TokenR (TokenR.MkTokenR tokenPattern Nothing Nothing)
          other -> other
    Spec.assertBool s (any isTokenR printed) "setup: the Queen prints a token row"
    Spec.assertBool s (not (any idleTokenRowOffends printed)) "the real Queen appends a Soldier"
    Spec.assertBool s (any (idleTokenRowOffends . idle) printed) "and the same row with the Soldier dropped is rejected"
  -- CR 701.24a: the shuffle rider randomizes a library, so the redirect carrying
  -- it has to name one. See shufflingOutsideLibraryOffends.
  Spec.it s "no card shuffles a redirect that does not send the card into a library" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any shufflingOutsideLibraryOffends . cardReplacementEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: Nexus of Fate is the card that prints a
    -- zone-change rider at all.
    Spec.assertBool s (any (anyFace (any hasZoneChangeRider . cardReplacementEffects) . Printing.card) ps) "the pool has a card printing a CR 701.20 or CR 701.24 rider on a redirect"
    Spec.assertEqWith s "and every shuffle among them lands in a library" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Nexus of Fate rather than a card
  -- file, exactly as the pair above is.
  Spec.it s "the lint itself catches a shuffle rider on a redirect into another zone" $ do
    nexus <- S.printingOf s registry "Nexus of Fate"
    let printed = cardReplacementEffects (S.combinedFace nexus)
        toExile replacement = case replacement of
          ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR zoneChangePattern _ revealing shuffling) -> ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR zoneChangePattern Zone.Exile revealing shuffling)
          other -> other
    Spec.assertBool s (any hasZoneChangeRider printed) "setup: the Nexus prints both riders to move"
    Spec.assertBool s (not (any shufflingOutsideLibraryOffends printed)) "the real Nexus shuffles into a library"
    Spec.assertBool s (any (shufflingOutsideLibraryOffends . toExile) printed) "and the same rider on a redirect to exile is rejected"
  -- CR 615.1: a prevention shield surrounds "whatever it's affecting", and every
  -- field saying what that is is separately optional, so the schema cannot ask for
  -- one of them and only this rejects a shield saying nothing at all. See
  -- shieldNamingNothingOffends.
  Spec.it s "no card authors a prevention shield that names nothing to shield" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any shieldNamingNothingOffends . cardResolutionEffects) . Printing.card) ps
    -- Guards against a vacuous sweep: with no prevention shield in the pool at
    -- all this would pass whatever a card said. Mending Hands prints the counted
    -- shield and Inkshield the unbounded one.
    Spec.assertBool s (any (anyFace (any isPreventionShield . cardResolutionEffects) . Printing.card) ps) "the pool has a card printing a prevention shield"
    Spec.assertEqWith s "every shield says what it covers (CR 615.1)" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against real cards rather than a card file,
  -- exactly as the cases above are -- one per spelling of the covered side, since
  -- a lint reading only the ref would accept a stripped Divine Deflection and one
  -- reading only the description would accept a stripped Mending Hands.
  Spec.it s "the lint itself catches a prevention shield with its covered side stripped" $ do
    hands <- S.printingOf s registry "Mending Hands"
    deflection <- S.printingOf s registry "Divine Deflection"
    packLeader <- S.printingOf s registry "Pack Leader"
    dovin <- S.printingOf s registry "Dovin, Hand of Control"
    let strip effect = case effect of
          Effect.PreventNextDamage shield ->
            Effect.PreventNextDamage shield {PreventNextDamage.ref = Nothing, PreventNextDamage.whatRecipient = Nothing, PreventNextDamage.whoRecipient = Nothing}
          Effect.PreventAllDamage shield ->
            Effect.PreventAllDamage shield {PreventAllDamage.ref = Nothing, PreventAllDamage.whatRecipient = Nothing}
          Effect.RedirectDamage redirect ->
            Effect.RedirectDamage redirect {RedirectDamage.from = Nothing, RedirectDamage.whatRecipient = Nothing, RedirectDamage.whoRecipient = Nothing}
          other -> other
        shieldsOf = filter isPreventionShield . cardResolutionEffects . S.combinedFace
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf hands))) "the real Mending Hands names its recipient"
    Spec.assertBool s (any (shieldNamingNothingOffends . strip) (shieldsOf hands)) "and the same shield with that name removed is rejected"
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf deflection))) "the real Divine Deflection describes its recipients instead"
    Spec.assertBool s (any (shieldNamingNothingOffends . strip) (shieldsOf deflection)) "and the same shield with that description removed is rejected"
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf packLeader))) "the real Pack Leader describes the recipients of an unbounded shield"
    Spec.assertBool s (any (shieldNamingNothingOffends . strip) (shieldsOf packLeader)) "and the same shield with that description removed is rejected"
    -- CR 614.9's redirection, both spellings of its covered side.
    carom <- S.printingOf s registry "Carom"
    harmsWay <- S.printingOf s registry "Harm's Way"
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf carom))) "the real Carom names the creature it redirects damage from"
    Spec.assertBool s (any (shieldNamingNothingOffends . strip) (shieldsOf carom)) "and the same redirection with that name removed is rejected"
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf harmsWay))) "the real Harm's Way describes the recipients it covers"
    Spec.assertBool s (any (shieldNamingNothingOffends . strip) (shieldsOf harmsWay)) "and the same redirection with that description removed is rejected"
    -- The by-direction shield names a SOURCE rather than a recipient, which the
    -- lint accepts: a shield covering everyone still says what it covers.
    Spec.assertBool s (any isByDirectionShield (shieldsOf dovin)) "setup: Dovin prints a by-direction shield"
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf dovin))) "and naming only the source it watches is accepted"
    Spec.assertEqWith s "while stripping that name rejects both of Dovin's shields" (length (filter (shieldNamingNothingOffends . strip) (shieldsOf dovin))) (2 :: Int)
    -- CR 609.7a's chosen source is the fourth spelling: Pay No Heed names no
    -- recipient at all and is accepted, where the same shield with that choice
    -- removed says nothing about either end of the damage event and is rejected.
    payNoHeed <- S.printingOf s registry "Pay No Heed"
    let unchoose effect = case effect of
          Effect.PreventAllDamage shield -> Effect.PreventAllDamage shield {PreventAllDamage.chosenSource = Nothing}
          other -> other
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf payNoHeed))) "the real Pay No Heed names only the source of your choice"
    Spec.assertBool s (any (shieldNamingNothingOffends . unchoose) (shieldsOf payNoHeed)) "and the same shield with that choice removed is rejected"
    -- The cell the lint reads `direction` for: beside DealtBy the description is
    -- the OTHER end of the damage event rather than a second spelling of the
    -- covered side, and Pawl.Engine.Resolve's by-direction branch folds over the
    -- ids the ref named, so describing recipients does not save a shield naming
    -- no source. Dovin's other shield is DealtTo, where the same rewrite IS a
    -- covered side, so `any` is what distinguishes the two.
    let describeOnly effect = case effect of
          Effect.PreventAllDamage shield ->
            Effect.PreventAllDamage shield {PreventAllDamage.ref = Nothing, PreventAllDamage.whatRecipient = Just (Filter.Type.And [])}
          other -> other
    Spec.assertBool s (any (shieldNamingNothingOffends . describeOnly) (shieldsOf dovin)) "and a by-direction shield describing recipients but naming no source is rejected"
    -- The legal neighbour that same arm must still accept: a by-direction shield
    -- writing BOTH ends, which is the shape Resolve installs one narrowed row per
    -- named source for.
    muzzle <- S.printingOf s registry "Synthetic Selective Muzzle"
    Spec.assertBool s (any isByDirectionShield (shieldsOf muzzle)) "setup: the Muzzle prints a by-direction shield describing its recipients"
    Spec.assertBool s (not (any shieldNamingNothingOffends (shieldsOf muzzle))) "and naming a source beside that description is accepted"
  -- The same shape one axis over, and the thing that makes
  -- Pawl.Engine.PlayerEffect.unpreventable's board fold EXACT rather than
  -- approximate. See unpreventableScopeOffends.
  Spec.it s "no card narrows CR 615.12's \"damage can't be prevented\" by player" $ do
    ps <- S.allPrintings s
    let offenders = filter (anyFace (any (uncurry unpreventableScopeOffends) . cardPlayerScopes) . Printing.card) ps
    -- Guards against a vacuous sweep: with no such effect in the pool at all
    -- this would pass whatever the classification said. Spider-Punk is the card
    -- that prints one.
    Spec.assertBool s (any (anyFace (any (isUnpreventable . snd) . cardPlayerScopes) . Printing.card) ps) "the pool has a card printing unpreventable damage"
    Spec.assertEqWith s "CR 615.12 names no player, so its carrier is scoped to every player" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Spider-Punk rescoped rather than
  -- against a card file, exactly as the two cases above are.
  Spec.it s "the lint itself catches a narrowed unpreventable-damage carrier" $ do
    spiderPunk <- S.printingOf s registry "Spider-Punk"
    let card = S.combinedFace spiderPunk
        narrow ability = ability {PlayerStaticAbility.scope = PlayerScope.You}
        narrowed = card {Face.playerAbilities = fmap narrow (Face.playerAbilities card)}
        offends = any (uncurry unpreventableScopeOffends) . cardPlayerScopes
    Spec.assertBool s (not (offends card)) "the real Spider-Punk names nobody and is accepted"
    Spec.assertBool s (offends narrowed) "and the same card scoped to its controller is rejected"
    -- The ban is CR 615.12's alone: rescoping does not condemn a card whose
    -- effects are all asked about a player. Prowling Serpopard is the printing
    -- that legitimately says PlayerScope.You.
    serpopard <- S.printingOf s registry "Prowling Serpopard"
    Spec.assertBool s (not (offends (S.combinedFace serpopard))) "a You-scoped countering prohibition is accepted"
    -- And the STORED carrier, which Lava Burst really does pair with CR 615.12:
    -- restated onto Silence's own Effect.AffectPlayers here, so the rejecting
    -- direction is proven on a card whose scope is NOT EachPlayer to begin with.
    silence <- S.printingOf s registry "Silence"
    let unpreventable effect = case effect of
          Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration scope _) -> Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration scope (PlayerEffect.DamageCantBePrevented anyDamage))
          other -> other
        widen effect = case effect of
          Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration _ playerEffect) -> Effect.AffectPlayers (AffectPlayers.MkAffectPlayers duration (AffectedPlayers.Scoped PlayerScope.EachPlayer) playerEffect)
          other -> other
        overSpell f face =
          face
            { Face.spell =
                (Face.spell face)
                  { Modal.modes = fmap (\mode -> mode {Mode.clauses = fmap (\c -> c {Clause.effects = fmap f (Clause.effects c)}) (Mode.clauses mode)}) (Modal.modes (Face.spell face))
                  }
            }
        silenced = S.combinedFace silence
    Spec.assertBool s (not (offends silenced)) "the real Silence, whose stored effect is scoped to its opponents, is accepted"
    Spec.assertBool s (offends (overSpell unpreventable silenced)) "a stored CR 615.12 effect scoped to opponents is rejected"
    Spec.assertBool s (not (offends (overSpell (widen . unpreventable) silenced))) "and the same stored effect scoped to every player is accepted"
  -- The pattern axis of the same carrier, and engineOnlyOffends' twin: a card
  -- may narrow CR 615.12 by kind or by source, and may not name the RECIPIENT,
  -- which only Resolve can bake. See unpreventablePatternOffends.
  Spec.it s "no card authors a recipient into CR 615.12's damage pattern" $ do
    ps <- S.allPrintings s
    let patterns = concatMap (overFaces (fmap snd . cardPlayerScopes) . Printing.card) ps
        offenders = filter (anyFace (any (unpreventablePatternOffends . snd) . cardPlayerScopes) . Printing.card) ps
    -- The non-vacuity guard, and the guard that the AUTHORABLE axes really are
    -- authored: Spider-Punk narrows nothing and Excruciator writes
    -- Filter.IsSource, so the sweep has both a permissive and a narrowed pattern
    -- to look at.
    Spec.assertBool s (any isUnpreventable patterns) "the pool has a card printing unpreventable damage"
    Spec.assertBool s (elem (PlayerEffect.DamageCantBePrevented anyDamage) patterns) "Spider-Punk's pattern narrows nothing"
    Spec.assertBool s (elem (PlayerEffect.DamageCantBePrevented anyDamage {DamagePattern.whatSource = Filter.Type.IsSource}) patterns) "Excruciator's names its own source"
    Spec.assertEqWith s "CR 615.7's recipient is baked, never printed" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against Excruciator restated rather than
  -- against a card file, exactly as the cases above are.
  Spec.it s "the lint itself catches a printed recipient on CR 615.12" $ do
    excruciator <- S.printingOf s registry "Excruciator"
    let card = S.combinedFace excruciator
        bake playerEffect = case playerEffect of
          PlayerEffect.DamageCantBePrevented pattern_ ->
            PlayerEffect.DamageCantBePrevented pattern_ {DamagePattern.whichRecipient = Just (Recipient.ToPlayer S.alice)}
          other -> other
        restate f ability = ability {PlayerStaticAbility.effect = f (PlayerStaticAbility.effect ability)}
        over f = card {Face.playerAbilities = fmap (restate f) (Face.playerAbilities card)}
        offends = any (unpreventablePatternOffends . snd) . cardPlayerScopes
        kind playerEffect = case playerEffect of
          PlayerEffect.DamageCantBePrevented pattern_ ->
            PlayerEffect.DamageCantBePrevented pattern_ {DamagePattern.whichKind = Just DamageKind.Combat}
          other -> other
    Spec.assertBool s (not (offends card)) "the real Excruciator names a source and no recipient, and is accepted"
    Spec.assertBool s (offends (over bake)) "and the same clause naming a shielded player is rejected"
    -- Frenzied Baloth's axis, which is authorable and must stay so: narrowing by
    -- KIND is not what this lint bans.
    Spec.assertBool s (not (offends (over kind))) "narrowing the same clause to combat damage is accepted"
  -- CR 205.1 and CR 114.3, which is one biconditional read across the two kinds
  -- of face a corpus file holds. A card's type line "contains the card's card
  -- type(s)", which Pawl.Codec.TypeLine reads as at least one; CR 205.2c says
  -- "tokens have card types even though they aren't cards", so a minted token's
  -- face is held to the same bar. An EMBLEM is the one face with none: CR 114.3
  -- gives it "no characteristics other than the abilities defined by the effect
  -- that created it" and CR 114.5 adds that "Emblem isn't a card type".
  --
  -- A lint rather than a codec rule, because the wire cannot tell the three
  -- apart: Pawl.Codec.Face decodes an absent `typeLine` as the empty one for the
  -- emblem's sake, and this is what stops a card or a token quietly doing the
  -- same. Pawl.Codec.TypeLine still rejects a type line that is PRESENT and empty.
  Spec.it s "CR 205.1 / 114.3 only an emblem's face has no card type" $ do
    ps <- S.allPrintings s
    let typeless c = Set.null (TypeLine.types (Face.typeLine c))
        offends card =
          let printed = NonEmpty.toList (Card.Type.faces card)
              minted = concatMap mintedFacesTagged printed
           in any typeless printed
                || any (\(kind, face) -> typeless face /= (kind == MintedEmblem)) minted
        offenders = filter (offends . Printing.card) ps
    Spec.assertEqWith s "only an emblem is typeless" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 306.5 / 306.5a: the other card-type biconditional, the Aura/enchant
  -- lint's shape. "Loyalty is a characteristic only planeswalkers have", so a
  -- planeswalker without one has nothing for CR 306.5b's intrinsic replacement
  -- to place and would be buried by CR 704.5i the instant it entered; a
  -- non-planeswalker with a printed loyalty carries a number no rule reads.
  --
  -- Projection.intrinsicReplacementsOf's own comment leans on this in both
  -- directions, which is why it is a lint and not a per-card assertion.
  Spec.it s "a card is a planeswalker iff it has a printed loyalty" $ do
    ps <- S.allPrintings s
    let isPlaneswalker c = Set.member CardType.Planeswalker (TypeLine.types (Face.typeLine c))
        offends c = isPlaneswalker c /= Maybe.isJust (Face.loyalty c)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "planeswalker iff loyalty" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 313.6 / 313.7: the same biconditional again, and the tightest of the
  -- family, since rule 313.6 and rule 313.7 each say "each vanguard card has" one
  -- of these. A vanguard without them has no number for CR 902.4 and CR 902.5 to
  -- read; a non-vanguard with them carries two numbers no rule reads, since only
  -- rule 902 consults them and only of a card in the command zone (CR 313.2).
  Spec.it s "a card is a vanguard iff it has printed modifiers" $ do
    ps <- S.allPrintings s
    let isVanguard c = Set.member CardType.Vanguard (TypeLine.types (Face.typeLine c))
        offends c = isVanguard c /= Maybe.isJust (Face.vanguard c)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "vanguard iff modifiers" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 310.4 / 210.1: the same biconditional one rule number over. "Defense is a
  -- characteristic that battles have", so a battle without one has nothing for CR
  -- 310.4b's intrinsic replacement to place and would enter with no defense
  -- counters at all; a non-battle with a printed defense carries a number no rule
  -- reads.
  --
  -- Projection.intrinsicReplacementsOf's own comment leans on this in both
  -- directions too, which is why it is a lint and not a per-card assertion.
  Spec.it s "a card is a battle iff it has a printed defense" $ do
    ps <- S.allPrintings s
    let isBattle c = Set.member CardType.Battle (TypeLine.types (Face.typeLine c))
        offends c = isBattle c /= Maybe.isJust (Face.defense c)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "battle iff defense" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 309.4 / 309.1: the third biconditional, on the same terms. A dungeon
  -- without rooms has nowhere for CR 309.4a's venture marker to go; a non-dungeon
  -- with rooms carries a graph no rule reads, since CR 309.4's rooms are printed
  -- on dungeon cards and nowhere else.
  Spec.it s "a card is a dungeon iff it has rooms" $ do
    ps <- S.allPrintings s
    let isDungeon c = Set.member CardType.Dungeon (TypeLine.types (Face.typeLine c))
        offends c = isDungeon c /= not (Seq.null (Face.rooms c))
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "dungeon iff rooms" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 309.4 / 309.5a: every arrow points at a room this card actually has. An
  -- out-of-range exit would be offered by Prompt.ChooseRoom and then move the
  -- marker onto a room that does not exist, which no rule describes.
  Spec.it s "CR 309.4 every room's arrows point at rooms of the same card" $ do
    ps <- S.allPrintings s
    let offends c =
          let rooms = Face.rooms c
              inRange e = RoomIndex.unwrap e < Natural.length rooms
           in not (all (all inRange . DungeonRoom.exits) rooms)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "arrows in range" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 309.6: the BOTTOMMOST room is where the dungeon ends, and
  -- Pawl.Engine.Dungeon.isBottommost reads that off the position. This lint is
  -- what makes position and the absence of arrows agree: the last room has no
  -- arrows out of it, and no earlier room is a dead end the marker could never
  -- leave.
  Spec.it s "CR 309.6 a dungeon's last room is its only one with no arrows" $ do
    ps <- S.allPrintings s
    let offends c = case Seq.viewr (Face.rooms c) of
          Seq.EmptyR -> False
          earlier Seq.:> lastRoom ->
            not (Set.null (DungeonRoom.exits lastRoom))
              || any (Set.null . DungeonRoom.exits) earlier
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "one dead end, and it is last" (fmap (S.nameOf . Printing.card) offenders) []
  -- There is deliberately NO fourth biconditional here pairing the creature card
  -- type with a printed power, and the omission is a decision rather than a gap.
  -- CR 208.3 names the counterexample outright -- "even if it's a card with a
  -- power and toughness printed on it (such as a Vehicle)" -- and CR 301.7a says
  -- the same from the subtype's side, so Consulate Dreadnought is a noncreature
  -- card with a printed 7/11 and is not an offender. What holds instead is CR
  -- 208.1's weaker pairing below: the box in the lower right corner holds two
  -- numbers or none.
  --
  -- The three lints above are safe from that exception because loyalty and
  -- defense have no analogue of rule 208.3 -- no rule prints either number on a
  -- card of another type.
  Spec.it s "CR 208.1 a card has a printed power iff it has a printed toughness" $ do
    ps <- S.allPrintings s
    let offends c = Maybe.isJust (Face.power c) /= Maybe.isJust (Face.toughness c)
        offenders = filter (anyFace offends . Printing.card) ps
    Spec.assertEqWith s "power iff toughness" (fmap (S.nameOf . Printing.card) offenders) []
  -- What makes Pawl.Engine.Card.faceNamed's answer unique, and so what makes
  -- referring to a face BY NAME well-defined (CR 709.4a). Held over the whole
  -- pool rather than by construction, because a card file is data.
  --
  Spec.it s "a card's face names are pairwise distinct" $ do
    ps <- S.allPrintings s
    let offenders = filter (distinctFaceNamesOffends . Printing.card) ps
    -- The guard the sibling lints carry: over a pool of one-face cards this
    -- sweep passes on every card without comparing two names, and so proves
    -- nothing. Wax // Wane is what makes it non-vacuous.
    Spec.assertBool s (any ((> 1) . length . Card.Type.faces . Printing.card) ps) "the pool has a multi-face card to lint"
    Spec.assertEqWith s "no card repeats a face name" (fmap (S.nameOf . Printing.card) offenders) []
  -- The rejecting direction, proven against a hand-built offender rather than a
  -- card file: a card that repeats a face name must not be loadable.
  Spec.it s "the lint itself catches a card that repeats a face name" $ do
    let wax = vanillaFace "Wax" instantLine
        offender = Card.Type.MkCard {Card.Type.layout = Layout.Split, Card.Type.faces = wax NonEmpty.:| [wax]}
    Spec.assertBool s (distinctFaceNamesOffends offender) "two faces sharing one name are rejected"
  -- CR 709.5h names "a PARTICULAR half", and CR 709.5j says which halves there
  -- are: "Some cards refer to a 'door' of a Room permanent. A door is a half of
  -- that permanent." So the door a "when you unlock this door" trigger names has
  -- to be one of its own card's faces -- Pawl.Engine.Event.matchesTrigger
  -- compares the two names, and a condition naming anything else can never fire.
  --
  -- Held over the pool rather than by construction, for the reason the
  -- pairwise-distinct lint above gives: a card file is data. Non-vacuity is
  -- asserted the same way, since a pool with no such condition would pass this
  -- sweep without comparing anything.
  Spec.it s "an unlock trigger names one of its own card's faces" $ do
    ps <- S.allPrintings s
    let doors c = [n | TriggerCondition.SelfHalfUnlocked n <- fmap TriggeredAbility.condition (Face.triggeredAbilities c)]
        offends card = any (any (`notElem` fmap Face.name (NonEmpty.toList (Card.Type.faces card))) . doors) (Card.Type.faces card)
        offenders = filter (offends . Printing.card) ps
    Spec.assertBool
      s
      (not (all (all (null . doors) . Card.Type.faces . Printing.card) ps))
      "the pool has a card with an unlock trigger to lint"
    Spec.assertEqWith s "every door named is a face of the card naming it" (fmap (S.nameOf . Printing.card) offenders) []
  -- CR 709.5a, swept over the pool. See sharedTypeLineOffends for what the rule
  -- asks and why the check is full type-line equality.
  Spec.it s "CR 709.5a a Room's faces agree on their shared type line" $ do
    ps <- S.allPrintings s
    let offenders = filter (sharedTypeLineOffends . Printing.card) ps
        rooms = filter (Card.hasSharedTypeLine . Printing.card) ps
    -- The guard the sibling lints carry, and it bites harder here than most: over
    -- a pool with no Room at all this sweep compares nothing, and over a Room with
    -- one face it compares a line against itself.
    Spec.assertBool s (any ((> 1) . length . Card.Type.faces . Printing.card) rooms) "the pool has a multi-face Room to lint"
    Spec.assertEqWith s "no Room's halves disagree" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against the real card restated rather than a card
  -- file, as the repeated-face-name lint above does it: a Room whose halves
  -- disagree must not be loadable. Restating a printed Room rather than building
  -- one is what makes this a claim about the corpus's own shape.
  Spec.it s "the lint itself catches a Room whose halves disagree" $ do
    furnace <- S.printingOf s registry "Roaring Furnace"
    let card = Printing.card furnace
        -- Every half but the left one restated, so each mutation below is a
        -- disagreement rather than a card-wide edit the lint would accept.
        retype f = case Card.Type.faces card of
          x NonEmpty.:| xs -> card {Card.Type.faces = x NonEmpty.:| fmap f xs}
        addType face =
          face
            { Face.typeLine =
                (Face.typeLine face)
                  { TypeLine.types = Set.insert CardType.Artifact (TypeLine.types (Face.typeLine face))
                  }
            }
        dropSubtype face =
          face {Face.typeLine = (Face.typeLine face) {TypeLine.subtypes = Set.empty}}
        addSupertype face =
          face
            { Face.typeLine =
                (Face.typeLine face)
                  { TypeLine.supertypes = Set.singleton Supertype.Legendary
                  }
            }
    Spec.assertBool s (not (sharedTypeLineOffends card)) "the real Roaring Furnace // Steaming Sauna is accepted"
    Spec.assertBool s (sharedTypeLineOffends (retype addType)) "one half gaining a card type is rejected"
    Spec.assertBool s (sharedTypeLineOffends (retype dropSubtype)) "one half losing the Room subtype is rejected"
    -- The set CR 709.5a does not name, and which sharedTypeLineOffends checks
    -- anyway on CR 709.5's premise that the line is one printed line.
    Spec.assertBool s (sharedTypeLineOffends (retype addSupertype)) "one half gaining a supertype is rejected"
    -- NOT an offence on a layout whose halves print their own lines: Onward //
    -- Victory is Instant against Sorcery, and CR 709.4c is what makes that legal
    -- authoring rather than a defect.
    victory <- S.printingOf s registry "Onward"
    Spec.assertBool s (not (sharedTypeLineOffends (Printing.card victory))) "a Split card's halves may differ"
  -- CR 712.4b, swept over the pool. See meldFaceCountOffends for what the rule
  -- asks and why the check is a face count.
  Spec.it s "CR 712.4b a meld card carries its front face alone" $ do
    ps <- S.allPrintings s
    let melds = filter ((== Layout.Meld) . Card.Type.layout . Printing.card) ps
        offenders = filter (meldFaceCountOffends . Printing.card) ps
    -- The guard the sibling lints carry: over a pool with no meld card this
    -- sweep counts nothing.
    Spec.assertBool s (not (null melds)) "the pool has a meld card to lint"
    Spec.assertEqWith s "no meld card prints a second face" (fmap (S.nameOf . Printing.card) offenders) []
  -- The REJECTING direction, against the real pair restated rather than a card
  -- file, as the Room lint above does it: the two halves of a meld pair stitched
  -- into one card must not be loadable.
  Spec.it s "the lint itself catches a meld card carrying a second face" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    ranger <- S.printingOf s registry "Daybreak Ranger"
    let card = Printing.card battlements
        stitched = card {Card.Type.faces = NonEmpty.head (Card.Type.faces card) NonEmpty.:| [NonEmpty.head (Card.Type.faces (Printing.card garrison))]}
    Spec.assertBool s (not (meldFaceCountOffends card)) "the real Hanweir Battlements is accepted"
    Spec.assertBool s (meldFaceCountOffends stitched) "a meld card carrying a second face is rejected"
    -- NOT an offence on the layouts CR 712.1 lists alongside this one: a
    -- nonmodal double-faced card prints two Magic card faces and stores both.
    Spec.assertBool s (not (meldFaceCountOffends (Printing.card ranger))) "a Transforming card may print two faces"

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Card" $ do
  effectLintSpec s registry
