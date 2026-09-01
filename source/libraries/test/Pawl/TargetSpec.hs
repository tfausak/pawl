{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Target: CR 115 target legality, and the rule-702 TARGETING
-- RESTRICTIONS that narrow it. Shroud (CR 702.18) is the pool's first, printed
-- by Blurred Mongoose, and hexproof (CR 702.11) is the second, printed by
-- Slippery Bogle. Their cases sit together because the pair is only interesting
-- together: the two rules differ in exactly one thing, whether the restriction
-- reads WHO is targeting. One case covers the other side of the
-- restriction/admission split -- Pawl.Engine.Sba's CR 303.4c re-check, which
-- asks what an enchant slot ADMITS and must not ask a targeting question --
-- a group of them covers rule 702.11's "hexproof from [quality]" variant (CR
-- 702.11d and 702.11e), the only restriction here that reads what the SOURCE is
-- rather than who controls it, ending on Knight of Grace's printing -- and the
-- rest cover CR 115.2's two escape hatches from "only permanents are legal
-- targets": its clause (b) as Cancel and Stifle, its clause (a) as Raise Dead,
-- Withered Wretch, Riftsweeper and Dwell on the Past. (Those letters are prose
-- inside rule 115.2, not subrule numbers; there is no CR 115.2a.)
--
-- Raise Dead and Withered Wretch are the two halves of CR 400.1's per-player
-- axis -- "in your graveyard" against "from a graveyard" -- and the case that
-- reads their pools reads BOTH off one board, so neither is left to pass against
-- a pool that had stopped asking whose graveyard it was reading.
--
-- Riftsweeper is the other side of that same rule: exile is one of the zones CR
-- 400.1 says are "shared by all players", so its pool has no per-player axis at
-- all, and the case that reads it reads Withered Wretch's off the same board so
-- neither off-battlefield pool can swallow the other.
--
-- Dwell on the Past is the third reading of that axis, and the only one no
-- perspective answers: "their graveyard" is whoever the spell's OTHER slot
-- targets, so its case is about CR 601.2c's one announcement over two slots
-- rather than about the pool alone.
--
-- Three synthetics sit with it: Synthetic Exhume the Archive fixes its card count
-- at two, so a board splitting two cards between two graveyards has no coherent
-- announcement at all; Synthetic Recurring Reclamation puts the mode in CR
-- 700.2d's repeat, where the scope has to name its own occurrence's player slot
-- rather than the first occurrence's; and Synthetic Reclamation Engine is that
-- same repeat one object type over, on an ACTIVATED ability, which re-checks CR
-- 608.2b down a path of its own.
--
-- Fall of the Hammer is beside it because it is the other way one slot can
-- depend on another: not the POOL a slot draws from but the FILTER it is
-- narrowed by, which is CR 601.2c's "another" between two slots of one
-- announcement. Its case reads the same union-offer plus joint-check pair
-- Dwell's does, and turns on an announcement the joint check has to reject.
-- Synthetic Hammer Refrain is that card under CR 700.2d's repeat, where the
-- filter's slot name has to follow the occurrence the way the pool's does.
--
-- Cancel and Stifle's case has a third beside it, on the same pool one rule
-- over: CR 115.5's self-exclusion for an ABILITY, which Adric, Mathematical
-- Genius is the first card in the pool to make reachable. It is a fence rather
-- than a proof -- Pawl.Engine.Target.legalRecipients' own note says why.
--
-- The last case is hexproof's other axis: not who is targeting but WHETHER THE
-- KEYWORD IS THERE AT ALL. Dawnglade Regent grants it through a CR 604.2 "as
-- long as you're the monarch" clause, so the same Doom Blade answers both ways
-- across CR 725.5's no-monarch window.
--
-- The last group is the other side of the split again, and the only one about a
-- slot's own FILTER rather than about a restriction or a pool: Razorfin
-- Abolisher's "target creature with a counter on it" is CR 122.1 asked
-- kind-agnostically, and it is here because what it narrows is admission.
--
-- Gameplay-level: every target slot under test is read out of a committed card rather
-- than hand-built, and the cases that turn on an effect cast and resolve through
-- the stack.
module Pawl.TargetSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural.Type
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
-- The logic module, alongside Pawl.Types.Modal below: unambiguous under one alias
-- because the two export disjoint names (CardSpec's precedent).
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The one target slot a single-slot card or ability declares, read out of the
-- committed printing (S.spellTargetSlot's rationale) but keyed by COUNT rather
-- than by slot name: Cancel calls its slot "spell", not "target".
soleTargetSlot :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe TargetSlot.TargetSlot
soleTargetSlot modal = case Map.elems (Modal.allTargetSlots modal) of
  [only] -> Just only
  _ -> Nothing

-- soleTargetSlot, off the one TRIGGERED ability a printing declares rather than
-- off its spell. Same rationale: the slot is read out of the committed card, so
-- the case exercises the codec's parse and never a hand-built TargetSlot.
triggerTargetSlot :: Printing.Printing -> Maybe TargetSlot.TargetSlot
triggerTargetSlot printing = case Face.triggeredAbilities (S.combinedFace printing) of
  [ability] -> soleTargetSlot (TriggeredAbility.modal ability)
  _ -> Nothing

-- The one ACTIVATED ability of a printing that declares exactly one. Nothing for
-- any other printing, so a card that grew a second ability fails the case that
-- names it rather than silently picking whichever came first -- soleTargetSlot
-- above is the same shape for the same reason.
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
soleActivatedAbility p = case Face.activatedAbilities (S.combinedFace p) of
  [only] -> Just only
  _ -> Nothing

-- `pid` controls the restricted creature -- a Blurred Mongoose or a Slippery
-- Bogle -- and a Goblin Piker, and nothing else. The Piker is the CONTROL in
-- every rule-702 case below: it is a legal target of everything the restricted
-- creature is not, so "the restricted creature is excluded" cannot pass because
-- the whole legal set is empty.
restrictionBoard :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
restrictionBoard restricted piker pid =
  let gs0 = Setup.emptyGame S.bothPlayers
      (restrictedId, gs1) = S.addCreature restricted pid gs0
      (pikerId, gs2) = S.addCreature piker pid gs1
   in (restrictedId, pikerId, gs2)

-- Aims every target slot at one chosen card, tagged the way
-- Pool.CardsInGraveyard tags its candidates (Recipient.ToObject -- the
-- candidates are CARDS, not creatures). S.identityAnswer would answer with
-- Set.lookupMin instead, which is whichever graveyard card happens to have the
-- lowest id -- no way to say "bob's".
aimAtCard :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCard oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- aimAtCard, plus a Prompt.Shuffle that REVERSES the library rather than
-- returning it unchanged. CR 701.24a defines a shuffle as randomising an order,
-- and S.identityAnswer's legal-but-inert answer makes a shuffled library
-- indistinguishable from an unshuffled one -- so a reversal is what lets a test
-- say "this library was shuffled" at all (MulliganSpec's own reversing
-- interpreter, for the same reason). Game.honourShuffle accepts it: a reversal
-- is a permutation, so the contents are unchanged.
aimAtCardShuffling :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCardShuffling oid p = case p of
  Prompt.Shuffle ids -> reverse ids
  _ -> aimAtCard oid p

-- CR 601.2c's whole announcement for Dwell on the Past: bob in the player slot
-- and `oids` in the card slot, with as many cards announced as there are.
--
-- PINNED rather than searched, and pinned to a set the offer may not contain:
-- the case's point is a card that WAS offered (the union over both graveyards)
-- and is still an illegal answer beside bob, so an answerer that filtered
-- against `legal` would hand back an empty slot and the announcement would fail
-- on its COUNT instead -- passing for a reason the case is not about.
aimingDwell :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimingDwell oids p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const (Natural.length oids)) offers
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot _ ->
          if slot == SlotName.MkSlotName (Text.pack "player")
            then Set.singleton (Recipient.ToPlayer S.bob)
            else Set.fromList (fmap Recipient.ToObject oids)
      )
      asked
  _ -> S.identityAnswer p

-- aimingDwell with aimAtCardShuffling's reversing Prompt.Shuffle, for the same
-- reason: CR 701.24a leaves a shuffle observable only through the order it
-- produces.
aimingDwellShuffling :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimingDwellShuffling oids p = case p of
  Prompt.Shuffle ids -> reverse ids
  _ -> aimingDwell oids p

-- aimingDwell for the case that turns on the OFFERED COUNT: bob in the player
-- slot again, `oids` many cards announced, and then as many of `oids` as the
-- engine actually asked for, taken off the front.
--
-- Reading the count out of Prompt.ChooseTargets is the whole difference. The
-- other answerer names its whole list whatever it is asked, so an announcement
-- the engine narrowed and one it did not both end in a reversal -- of the count
-- check in the one case and of the joint check in the other -- and the case
-- could not tell them apart.
aimingDwellObeying :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimingDwellObeying oids p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const (Natural.length oids)) offers
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (n, _) ->
          if slot == SlotName.MkSlotName (Text.pack "player")
            then Set.singleton (Recipient.ToPlayer S.bob)
            else Set.fromList (fmap Recipient.ToObject (take (Natural.toIntSaturating n) oids))
      )
      asked
  _ -> S.identityAnswer p

-- CR 700.2d's whole announcement for Synthetic Recurring Reclamation with its
-- first mode chosen twice: occurrence 0 names bob and `his`, occurrence 1 names
-- `other` and `hisOther`. The two runs differ only in `other`.
--
-- Synthetic Reclamation Engine's case shares it, that card printing the same two
-- modes on an activated ability. What `hisOther` is differs with the fixture: a
-- card in BOB's graveyard for the spell case whichever player `other` is, and a
-- card in CAROL's for the ability case.
--
-- Pinned per slot NAME rather than searched, and the suffixed names are
-- Modal.instanceSlot's. CR 601.2c offers occurrence 1 the union over every
-- player its own slot could still take, so bob's second card is offered for
-- carol's occurrence either way -- what the rename decides is whether the JOINT
-- CHECK still admits it.
aimingReclamation :: PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingReclamation other his hisOther p = case p of
  Prompt.ChooseModes {} -> Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 0]
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (n, _) ->
          let named name = slot == SlotName.MkSlotName (Text.pack name)
              wanted
                | named "player" = [Recipient.ToPlayer S.bob]
                | named "player#1" = [Recipient.ToPlayer other]
                | named "cards" = [Recipient.ToObject his]
                | otherwise = [Recipient.ToObject hisOther]
           in Set.fromList (take (Natural.toIntSaturating n) wanted)
      )
      asked
  _ -> S.identityAnswer p

-- The printed names of the cards in one player's copy of a zone (CR 400.1),
-- sorted so the assertion does not depend on object-id order. Names rather than
-- ids because a card that changes zones is a NEW object (CR 400.7).
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesIn zone pid gs =
  List.sort (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))

-- CR 601.2c's whole announcement for Fall of the Hammer: `dealerId` in the
-- dealer slot and `victimId` in the victim slot. Both counts are fixed at one,
-- so there is no Prompt.AnnounceTargets to answer.
--
-- FILTERED out of the offered set rather than built, which is the posture
-- Pawl.CopySpec's answerers take: Pool.Creatures offers Recipient.ToCreature,
-- and a hand-built recipient of another shape would be dropped at CR 608.2b's
-- re-read with no error. The case that names the same creature in both slots
-- therefore also depends on that creature being OFFERED for the victim slot,
-- which the union assertion in the same case pins.
aimingHammer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingHammer dealerId victimId p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == SlotName.MkSlotName (Text.pack "dealer") then dealerId else victimId
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

-- CR 700.2d's whole announcement for Synthetic Hammer Refrain with its damage
-- mode chosen twice: occurrence 0 names `dealer`/`victim`, occurrence 1 names
-- `dealerTwo`/`victimTwo` under Modal.instanceSlot's suffixed names. Pinned per
-- slot name and FILTERED out of the offered set, aimingHammer's shape and for its
-- reason.
aimingRefrain :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingRefrain dealer victim dealerTwo victimTwo p = case p of
  Prompt.ChooseModes {} -> Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 0]
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let named name = slot == SlotName.MkSlotName (Text.pack name)
              wanted
                | named "dealer" = dealer
                | named "victim" = victim
                | named "dealer#1" = dealerTwo
                | otherwise = victimTwo
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

-- CR 601.2c's whole announcement for Bioshift: `giverId` in the `from` slot and
-- `takerId` in the `to` slot, aimingHammer's shape and FILTERED for its reason.
--
-- Its second prompt is CR 122.5's "any number": the whole offered tally crosses,
-- so a case that moves nothing moved nothing because the announcement or the move
-- refused rather than because the answerer declined.
aimingBioshift :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingBioshift giverId takerId p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == SlotName.MkSlotName (Text.pack "from") then giverId else takerId
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  Prompt.ChooseMovedCounters _ _ _ _ offered -> offered
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Target" $ do
  -- CR 702.18a: "Shroud is a static ability. 'Shroud' means 'This permanent or
  -- player can't be the target of spells or abilities.'" Doom Blade is "target
  -- nonblack creature" and the Mongoose is green, so its Filter admits the
  -- Mongoose and only the restriction can remove it.
  Spec.it s "CR 702.18a an opponent's Doom Blade cannot target Blurred Mongoose" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (mongooseId, pikerId, gs) = restrictionBoard mongoose piker S.bob
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "the Piker beside it is a legal target"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature mongooseId) legal)) "the Mongoose is not"

  -- THE DISCRIMINATOR between shroud and hexproof. CR 702.11b's hexproof says
  -- "spells or abilities YOUR OPPONENTS control"; CR 702.18a's shroud says no
  -- such thing, so the same board with the same Doom Blade answers the same way
  -- when the Mongoose's own controller is the one aiming it. An implementation
  -- that compared controllers would pass the case above and fail this one.
  Spec.it s "CR 702.18a shroud is not hexproof: the Mongoose's own controller cannot target it either" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (mongooseId, pikerId, gs) = restrictionBoard mongoose piker S.alice
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "alice may target her own Piker"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature mongooseId) legal)) "but not her own Mongoose"

  -- CR 702.18a's OTHER half: "This permanent OR PLAYER can't be the target of
  -- spells or abilities." Ivory Mask ("You have shroud") is the producer, and a
  -- player has no keywords to carry it -- rule 702's keywords live on objects
  -- and go through the CR 613.1-613.7 layers, which compute an object's
  -- characteristics and nothing else. It rides the CR 613.10/613.11 player axis
  -- instead, as PlayerEffect.CantBeTargetedBy.
  --
  -- Lightning Bolt's "any target" is CR 115.4, which is what puts a player in a
  -- target slot's candidate set at all.
  Spec.it s "CR 702.18a Ivory Mask's controller cannot be targeted, by anyone" $ do
    ivoryMask <- S.printingOf s registry "Ivory Mask"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let bare = Setup.emptyGame S.bothPlayers
        (_, masked) = S.addCreature ivoryMask S.bob bare
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSlot
        -- The control twin first: without the Mask on the board, bob is an
        -- ordinary CR 115.4 candidate, so the Mask is what removes him.
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice bare)) "without the Mask, alice may bolt bob"
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice masked))) "with it, she may not"
        -- Shroud, not hexproof: CR 702.18a names no player, so it stops bob too.
        -- This is the assertion that an Opponents-scoped implementation fails.
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.bob masked))) "and bob cannot bolt himself either"
        -- Scoped to its controller: the Mask says "YOU have shroud", so alice is
        -- untouched. A whole-table implementation fails here.
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob masked)) "alice, with no Mask, is still a legal target"

  -- THE DISCRIMINATOR for the player halves, the twin of the Mongoose pair
  -- above. CR 702.11c: "'Hexproof' on a player means 'You can't be the target of
  -- spells or abilities your opponents control.'" Leyline of Sanctity is the
  -- producer, and the ONLY thing that differs from Ivory Mask is the scope --
  -- which is why the two share one constructor carrying a PlayerScope rather
  -- than getting one apiece. An implementation that ignored the payload would
  -- pass one of these two tests and fail the other, whichever way it defaulted.
  Spec.it s "CR 702.11c Leyline of Sanctity stops an opponent, but not its own controller" $ do
    leyline <- S.printingOf s registry "Leyline of Sanctity"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, warded) = S.addCreature leyline S.bob (Setup.emptyGame S.bothPlayers)
    case S.spellTargetSlot bolt of
      Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
      Just theSlot -> do
        let legalFor who = Target.legalRecipients (Just who) S.noSource theSlot warded
        Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.bob) (legalFor S.alice))) "alice, his opponent, may not bolt bob"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) (legalFor S.bob)) "but bob may bolt himself -- hexproof names only opponents"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob)) "and alice is targetable as ever"

  -- The GAMEPLAY-level proof for both cards, which the legality reads above are
  -- not: a Lightning Bolt actually cast, with an answerer that aims at bob
  -- whenever the engine offers him. Under the Mask he is never offered, so the
  -- answer falls to alice and SHE takes the 3 -- the second invariant holding,
  -- since the engine is not choosing a target so much as never presenting an
  -- illegal one. The Leyline half is the same cast with the same answerer and the
  -- opposite outcome, because alice IS bob's opponent.
  Spec.it s "CR 702.18a/702.11c a Bolt aimed at a protected player lands elsewhere" $ do
    ivoryMask <- S.printingOf s registry "Ivory Mask"
    leyline <- S.printingOf s registry "Leyline of Sanctity"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let -- Prefers bob for every slot he is offered in, and falls back to the
        -- lowest candidate otherwise -- so "bob was not offered" is the only way
        -- the damage can land anywhere else.
        prefersBob p = case p of
          Prompt.ChooseTargets _ _ _ sets ->
            S.preferring (== Recipient.ToPlayer S.bob) sets
          _ -> S.identityAnswer p
        castAt guard =
          let base = S.landsInPlay mountain 1
              withGuard = maybe base (\g -> snd (S.addCreature g S.bob base)) guard
              (ready, boltId) = S.handOne bolt withGuard
              board = ready {GameState.priority = Just S.alice}
           in S.runPure prefersBob board (S.cast S.alice boltId >> Stack.resolveTop)
        unguarded = castAt Nothing
        masked = castAt (Just ivoryMask)
        warded = castAt (Just leyline)
    -- The control: with nothing protecting him, the answerer gets what it asked
    -- for, so the fixture really does aim at bob.
    Spec.assertEqWith s "unguarded, bob takes the Bolt" (S.lifeOf S.bob unguarded) (Just 17)
    Spec.assertEqWith s "and alice is untouched" (S.lifeOf S.alice unguarded) (Just 20)
    -- CR 702.18a: bob was never offered, so the Bolt landed on alice instead.
    Spec.assertEqWith s "under Ivory Mask, bob takes nothing" (S.lifeOf S.bob masked) (Just 20)
    Spec.assertEqWith s "and alice, the only candidate left, takes it" (S.lifeOf S.alice masked) (Just 17)
    -- CR 702.11c: same cast, same answerer, and alice is his opponent.
    Spec.assertEqWith s "under Leyline of Sanctity, bob takes nothing from his opponent" (S.lifeOf S.bob warded) (Just 20)
    Spec.assertEqWith s "and alice takes it instead" (S.lifeOf S.alice warded) (Just 17)

  -- CR 601.2c / 700.2a: with BOTH players masked and no creature on the board,
  -- Lightning Bolt's only slot has no candidate left, so the spell is
  -- UNCASTABLE rather than castable-and-fizzling. Cast.instantSpeed's own
  -- comment used to call this unobservable for Bolt, on the grounds that
  -- AnyTarget always holds a living player; Ivory Mask is what stopped that
  -- being true, so the claim is pinned here rather than left as prose.
  Spec.it s "CR 601.2c a Bolt with every player masked and no creature is uncastable" $ do
    ivoryMask <- S.printingOf s registry "Ivory Mask"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let base = S.landsInPlay mountain 1
        (_, oneMask) = S.addCreature ivoryMask S.bob base
        (_, bothMasked) = S.addCreature ivoryMask S.alice oneMask
        castableIn board =
          let (ready, boltId) = S.handOne bolt board
           in S.castable S.alice boltId (ready {GameState.priority = Just S.alice})
    -- The control twin, one step at a time: with only bob masked, alice is still
    -- a candidate and the Bolt is castable, so it is the SECOND Mask that empties
    -- the slot rather than anything else about the board.
    Spec.assertBool s (castableIn base) "unmasked, the Bolt is castable"
    Spec.assertBool s (castableIn oneMask) "with bob masked, alice is still a candidate"
    Spec.assertBool s (not (castableIn bothMasked)) "with both masked, there is nothing to target and it is uncastable"

  -- CR 702.18a says "spells OR ABILITIES", so the gate cannot live on the cast
  -- path. Prodigal Sorcerer's "{T}: This creature deals 1 damage to any target"
  -- is an activated ability with a Pool.AnyTarget slot (CR 115.4), and the same
  -- legality call serves it.
  Spec.it s "CR 702.18a shroud stops an ability's target too, not only a spell's" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (mongooseId, pikerId, gs) = restrictionBoard mongoose piker S.alice
    case Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSlot] -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "the Piker is a legal any-target"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.bob) legal) "so is a player (CR 115.4)"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature mongooseId) legal)) "the Mongoose is not"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should print one ability with one target slot"

  -- The restriction is read off the PROJECTION, not off the printed card, so it
  -- lives in the CR 613 layer system like every other keyword. Humility is
  -- "All creatures lose all abilities and have base power and toughness 1/1"
  -- (CR 613.1f, layer 6), and CR 702.18a's shroud is one of the abilities it
  -- takes: a Humility'd Mongoose is an ordinary green creature and Doom Blade
  -- may target it.
  Spec.it s "CR 613.1f Humility takes shroud away, and the Mongoose becomes targetable" $ do
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (mongooseId, _, board) = restrictionBoard mongoose piker S.bob
        humbled = S.withHumility humility board
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature mongooseId) (Target.legalRecipients (Just S.alice) S.noSource theSlot board)))
          "before Humility the Mongoose is untargetable"
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature mongooseId) (Target.legalRecipients (Just S.alice) S.noSource theSlot humbled))
          "under Humility it is a legal target"

  -- CR 115.10a: "Just because an object or player is being affected by a spell
  -- or ability doesn't make that object or player a target of that spell or
  -- ability." Day of Judgment names no target at all, so shroud has nothing to
  -- say about it -- the classic way to get this rule wrong is to make the
  -- restriction a property of being AFFECTED.
  Spec.it s "CR 115.10a Day of Judgment still destroys Blurred Mongoose: shroud restricts targeting, not effects" $ do
    plains <- S.printingOf s registry "Plains"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let base = S.landsInPlay plains 4 -- {2}{W}{W}
        (_, b1) = S.addCreature mongoose S.bob base
        (_, b2) = S.addCreature piker S.bob b1
        (gs, dojId) = S.handOne dayOfJudgment b2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dojId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- By NAME, not by id: CR 400.7 makes the graveyard incarnation a new
        -- object, and Event.changeZone mints it a fresh ObjectId.
        buried = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.bob after)
    Spec.assertEqWith s "both of bob's creatures are gone" (S.creaturesInPlay S.bob after) 0
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Blurred Mongoose") buried) "the Mongoose itself is in bob's graveyard"
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Goblin Piker") buried) "and so is the Piker beside it"

  -- CR 113.6: "Abilities of an instant or sorcery spell usually function only
  -- while that object is on the stack. Abilities of all other objects usually
  -- function only while that object is on the battlefield." Shroud is printed on
  -- a CREATURE card, so a Blurred Mongoose SPELL has none and Cancel targets it
  -- legally. It is CR 113.6g -- "an object's ability that states it can't be
  -- countered ... functions on the stack" -- that then saves it, which is the
  -- card's other half and a different rule.
  Spec.it s "CR 113.6 Cancel legally targets the Blurred Mongoose spell, and CR 113.6g stops it countering" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    let base = S.landsInPlay island 3
        (spellId, onStack) = S.spellOnStack mongoose S.bob base
        (gs, cancelId) = S.handOne cancel onStack
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice cancelId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    case soleTargetSlot (Face.spell (S.combinedFace cancel)) of
      Nothing -> Spec.assertFailure s "Cancel should declare one target slot"
      Just theSlot ->
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject spellId) (Target.legalRecipients (Just S.alice) S.noSource theSlot gs))
          "the Mongoose spell is a legal target on the stack"
    Spec.assertBool s (elem spellId (GameState.stack resolved)) "and is still on the stack, uncountered"
    Spec.assertEqWith s "Cancel resolved into alice's graveyard regardless" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1

  -- CR 115.5: "A spell or ability on the stack is an illegal target for itself."
  -- Cancel's "counter target spell" draws from Pool.Spells with no Filter at
  -- all, so the rule is the ONLY thing that can exclude the Cancel itself --
  -- there is no "another" for a Not IsSource clause to carry.
  --
  -- The second spell is the control: the exclusion has to be the ONE object the
  -- rule names, not an emptied pool.
  Spec.it s "CR 115.5 a Cancel on the stack is not a legal target for itself, though another spell there is" $ do
    island <- S.printingOf s registry "Island"
    cancel <- S.printingOf s registry "Cancel"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    let base = S.landsInPlay island 3
        (victimId, withVictim) = S.spellOnStack mongoose S.bob base
        (cancelId, gs) = S.spellOnStack cancel S.alice withVictim
    case soleTargetSlot (Face.spell (S.combinedFace cancel)) of
      Nothing -> Spec.assertFailure s "Cancel should declare one target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) cancelId theSlot gs
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToObject cancelId) legal))
          "CR 115.5: the Cancel is an illegal target for itself"
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject victimId) legal)
          "and the OTHER spell on the stack is still legal"

  -- The counterweight, and the reason CR 115.5's gate is "is the source on the
  -- STACK" rather than "is the candidate the source": rule 115.5 speaks of the
  -- object ON THE STACK, which for an activated ability is the ability, not the
  -- permanent it was activated from. Prodigal Sorcerer's "{T}: This creature
  -- deals 1 damage to any target" may therefore still name the Sorcerer, which
  -- is the reading Target.legalSets' own note takes for CR 601.2c's "another"
  -- (a slot that excludes its source says so with Not IsSource).
  Spec.it s "CR 115.5 does not stop Prodigal Sorcerer's ability targeting its own source" $ do
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (sorcererId, gs) = S.addCreature sorcerer S.alice (Setup.emptyGame S.bothPlayers)
    case Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSlot] ->
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature sorcererId) (Target.legalRecipients (Just S.alice) sorcererId theSlot gs))
          "the Sorcerer is a legal target of its own ability"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should print one ability with one target slot"

  -- CR 608.2b: "If the spell or ability specifies targets, it checks whether the
  -- targets are still legal. ... If all its targets, for every instance of the
  -- word 'target,' are now illegal, the spell or ability doesn't resolve."
  -- Shroud has to be asked at BOTH of CR 115's moments, and this is the second
  -- one -- the target was legal when CR 601.2c chose it.
  --
  -- No card in this pool GRANTS shroud, so the grant is a stored layer-6
  -- continuous effect (S.withEffect), the shape ColorSpec and ProjectionSpec use
  -- for the same reason. Both halves run off one board and one cast, so the
  -- fizzle cannot be a Doom Blade that never worked.
  Spec.it s "CR 608.2b Doom Blade fizzles when its target gains shroud in response" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let base = S.landsInPlay swamp 2
        (pikerId, board) = S.addCreature piker S.bob base
        (gs, dbId) = S.handOne doomBlade board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId))
        resolve g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
        killed = resolve cast
        fizzled = resolve (S.withEffect pikerId (Modification.GainKeyword Keyword.Shroud) cast)
    Spec.assertEqWith s "untouched, the Piker dies" (S.creaturesInPlay S.bob killed) 0
    Spec.assertEqWith s "shrouded in response, it survives" (S.creaturesInPlay S.bob fizzled) 1
    Spec.assertEqWith s "and Doom Blade is in alice's graveyard either way" (length (Game.zoneMembers Zone.Graveyard S.alice fizzled)) 1

  -- CR 303.4c asks whether an Aura enchants "an illegal object or player as
  -- defined by its enchant ability and other applicable effects" -- which is NOT
  -- a targeting question. Rule 702 says so itself: protection states both halves
  -- separately (CR 702.16b targeting, CR 702.16c "can't be enchanted by Auras
  -- ... put into their owners' graveyards as a state-based action"), while
  -- shroud (CR 702.18) and hexproof (CR 702.11) state only the targeting one.
  --
  -- Setessan Training is the Aura that proves it: "Enchant creature you control"
  -- carries a Filter, so Pawl.Engine.Sba.stillLegalEnchant cannot answer it from
  -- the pre-pass projection and falls through to the general path. Fusing the
  -- restriction into that path buries this Aura.
  Spec.it s "CR 303.4c an Aura stays attached to a creature that gains shroud" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    setessanTraining <- S.printingOf s registry "Setessan Training"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, g1) = S.addCreature piker S.alice gs0
        (auraId, g2) = S.addCreature setessanTraining S.alice g1
        attached = S.attach auraId pikerId g2
        shrouded = S.withEffect pikerId (Modification.GainKeyword Keyword.Shroud) attached
    Spec.assertBool s (Set.member auraId (GameState.battlefield (S.settleSba attached))) "the Aura stays put with no shroud around"
    Spec.assertBool s (Set.member auraId (GameState.battlefield (S.settleSba shrouded))) "and stays put once its host has shroud"

  -- CR 702.11b: "'Hexproof' on a permanent means 'This permanent can't be the
  -- target of spells or abilities your opponents control.'" Slippery Bogle's
  -- entire printed rules text is the keyword, so a case that uses this printing
  -- is asking about 702.11b and nothing else.
  --
  -- BOTH HALVES ON ONE BOARD, with one target slot and one Doom Blade, because the
  -- controller axis IS the rule: an implementation that reused shroud's gate
  -- passes the opponent half and fails the controller half, and one that
  -- inverted the comparison does the reverse. Neither can pass this case.
  --
  -- Doom Blade is "target nonblack creature" and the Bogle is green and blue
  -- (CR 202.2: "an object is the color or colors of the mana symbols in its mana
  -- cost"; CR 107.4e: "a hybrid mana symbol is all of its component colors"), so
  -- the Filter admits it and only the restriction can remove it.
  Spec.it s "CR 702.11b an opponent's Doom Blade cannot target Slippery Bogle, but its own controller's can" $ do
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (bogleId, pikerId, gs) = restrictionBoard bogle piker S.alice
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let mine = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
            theirs = Target.legalRecipients (Just S.bob) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature bogleId) mine) "alice may target her own Bogle"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature bogleId) theirs)) "bob may not, and that is the whole of CR 702.11b"
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) mine) "the Piker beside it is a legal target for alice"
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) theirs) "and for bob"

  -- CR 702.11b says "spells or ABILITIES your opponents control", so the axis is
  -- read for an ability too, and off the ability's own controller: CR 113.8 fixes
  -- that as "the player who activated it", which is the perspective passed here.
  -- Prodigal Sorcerer's "{T}: This creature deals 1 damage to any target" is the
  -- ability, and the same legality call serves it.
  Spec.it s "CR 702.11b the controller axis holds for an ability, not only a spell" $ do
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (bogleId, pikerId, gs) = restrictionBoard bogle piker S.alice
    case Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace sorcerer)) of
      [theSlot] -> do
        let mine = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
            theirs = Target.legalRecipients (Just S.bob) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature bogleId) mine) "alice's own ability may point at her Bogle"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature bogleId) theirs)) "bob's may not"
        Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) theirs) "though bob's may point at the Piker"
        Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) theirs) "and at a player (CR 115.4)"
      _ -> Spec.assertFailure s "Prodigal Sorcerer should print one ability with one target slot"

  -- The whole card, cast and resolved through the stack rather than asked as a
  -- set membership: alice's own Doom Blade destroys her own Slippery Bogle. The
  -- Bogle is the only creature on the battlefield, so CR 601.2c's choice is
  -- forced -- were 702.11b read as 702.18a's shroud, the spell would have no
  -- legal target at all and the Bogle would live.
  Spec.it s "CR 702.11b whole card: alice's Doom Blade destroys her own Slippery Bogle" $ do
    swamp <- S.printingOf s registry "Swamp"
    bogle <- S.printingOf s registry "Slippery Bogle"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let base = S.landsInPlay swamp 2 -- {1}{B}
        (_, board) = S.addCreature bogle S.alice base
        (gs, dbId) = S.handOne doomBlade board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- By NAME, not by id: CR 400.7 makes the graveyard incarnation a new
        -- object, and Event.changeZone mints it a fresh ObjectId.
        buried = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.alice after)
    Spec.assertEqWith s "Doom Blade went on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "alice's own creature is gone" (S.creaturesInPlay S.alice after) 0
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Slippery Bogle") buried) "the Bogle itself is in alice's graveyard"

  -- CR 115.10a: "Just because an object or player is being affected by a spell
  -- or ability doesn't make that object or player a target of that spell or
  -- ability." Day of Judgment names no target at all, so hexproof has nothing to
  -- say about it -- even though alice, who casts it, is exactly the opponent
  -- rule 702.11b names. The classic way to get this rule wrong is to make the
  -- restriction a property of being AFFECTED.
  Spec.it s "CR 115.10a Day of Judgment still destroys Slippery Bogle: hexproof restricts targeting, not effects" $ do
    plains <- S.printingOf s registry "Plains"
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    dayOfJudgment <- S.printingOf s registry "Day of Judgment"
    let base = S.landsInPlay plains 4 -- {2}{W}{W}
        (_, b1) = S.addCreature bogle S.bob base
        (_, b2) = S.addCreature piker S.bob b1
        (gs, dojId) = S.handOne dayOfJudgment b2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dojId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        buried = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.bob after)
    Spec.assertEqWith s "both of bob's creatures are gone" (S.creaturesInPlay S.bob after) 0
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Slippery Bogle") buried) "the Bogle itself is in bob's graveyard"
    Spec.assertBool s (elem (CardName.MkCardName $ Text.pack "Goblin Piker") buried) "and so is the Piker beside it"

  -- Hexproof is read off the PROJECTION and not off the printed card, so it
  -- lives in the CR 613 layer system like every other keyword. Humility is "All
  -- creatures lose all abilities and have base power and toughness 1/1" (CR
  -- 613.1f, layer 6), and rule 702.11a's static ability is one of the abilities
  -- it takes: a Humility'd Bogle is an ordinary 1/1 and an opponent's Doom Blade
  -- may target it.
  Spec.it s "CR 613.1f Humility takes hexproof away, and the Bogle becomes targetable by its opponent" $ do
    bogle <- S.printingOf s registry "Slippery Bogle"
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (bogleId, _, board) = restrictionBoard bogle piker S.bob
        humbled = S.withHumility humility board
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature bogleId) (Target.legalRecipients (Just S.alice) S.noSource theSlot board)))
          "before Humility alice cannot target bob's Bogle"
        Spec.assertBool
          s
          (Set.member (Recipient.ToCreature bogleId) (Target.legalRecipients (Just S.alice) S.noSource theSlot humbled))
          "under Humility it is a legal target"

  -- CR 702.11d: "'Hexproof from [quality]' on a permanent means 'This permanent
  -- can't be the target of [quality] spells your opponents control or abilities
  -- your opponents control from [quality] sources.'" The variant narrows CR
  -- 702.11b by the SOURCE's characteristics, which no other targeting question in
  -- pawl asks -- Target.legalRecipients holds the source for CR 601.2c's
  -- "another" and never looked at what it is.
  --
  -- SIX ANSWERS OFF ONE BOARD, and the shape is what makes them discriminating:
  -- the same Goblin Piker and the same two committed spells throughout, with only
  -- the QUALITY mutated between the rows. Doom Blade is black and Angelic Edict
  -- is white, so each spell is stopped in exactly one row and legal in the other
  -- two. An implementation that never admitted the Piker at all fails the legal
  -- cells; one that ignored the quality and read the variant as plain hexproof
  -- fails the legal cells of the two Just rows; one that read the CANDIDATE's
  -- colour rather than the source's fails every stopped cell, the Piker being red
  -- (CR 202.2, {2}{R}).
  --
  -- Each spell is a REAL object on the stack, which is what makes the source
  -- readable at all: S.noSource names no object, so its view carries no colour
  -- and every quality would be vacuously unmatched -- the whole case would pass
  -- for the wrong reason.
  --
  -- SYNTHETIC GRANT BY CHOICE, not for want of a printing: Knight of Grace is
  -- committed and gets its own case below. What no printed card can do is what
  -- this case needs -- MUTATE the quality across six rows off one board, the
  -- Piker's colour and both spells held fixed while only the ability moves -- so
  -- the ability arrives as a stored layer-6 continuous effect, exactly as the CR
  -- 608.2b case below grants plain hexproof. The granted shape is real Magic:
  -- Skrelv, Defector Mite and Sungold Sentinel both grant one, and both of them
  -- choose a colour first, which pawl cannot prompt for.
  Spec.it s "CR 702.11d hexproof from black stops an opponent's black spell and admits their white one" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let (pikerId, board) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        withHexproof quality = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof quality)) board
    case (S.spellTargetSlot doomBlade, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        -- Doom Blade's pool is Creatures and Angelic Edict's is Permanents, so
        -- the same Piker is tagged differently in the two sets (CR 115).
        let reaches printing theSlot tag gs =
              let (spellId, onStack) = S.spellOnStack printing S.alice gs
               in Set.member (tag pikerId) (Target.legalRecipients (Just S.alice) spellId theSlot onStack)
            blackReaches = reaches doomBlade blackSlot Recipient.ToCreature
            whiteReaches = reaches angelicEdict whiteSlot Recipient.ToObject
            fromBlack = withHexproof (Just (Filter.Type.HasColor Color.Black))
            fromWhite = withHexproof (Just (Filter.Type.HasColor Color.White))
        Spec.assertBool s (blackReaches board) "with no hexproof at all, alice's Doom Blade reaches bob's Piker"
        Spec.assertBool s (whiteReaches board) "and so does her Angelic Edict"
        Spec.assertBool s (not (blackReaches fromBlack)) "hexproof from black stops the black spell"
        Spec.assertBool s (whiteReaches fromBlack) "and leaves the white one alone -- the half a plain-hexproof reading loses"
        Spec.assertBool s (not (whiteReaches fromWhite)) "mutate the quality to white and the white spell is stopped instead"
        Spec.assertBool s (blackReaches fromWhite) "while the black one reaches again"
        -- CR 702.11b is the same constructor with no quality, and stops both.
        Spec.assertBool s (not (blackReaches (withHexproof Nothing))) "unqualified hexproof stops the black spell"
        Spec.assertBool s (not (whiteReaches (withHexproof Nothing))) "and the white one too"
      _ -> Spec.assertFailure s "Doom Blade and Angelic Edict should each declare a target slot"

  -- CR 702.11d's "your opponents control", which the quality does not replace:
  -- the variant narrows WHICH spells are stopped and leaves CR 702.11b's
  -- controller axis exactly where it was. Bob's own black Doom Blade may still
  -- destroy his own creature with hexproof from black.
  --
  -- ONE BOARD, ONE SPELL, TWO PERSPECTIVES, the way the CR 702.11b case above
  -- reads Slippery Bogle: an implementation that dropped the controller test once
  -- the quality matched would make the Piker untargetable by everybody, and one
  -- that dropped the quality test would read the variant as CR 702.11b's plain
  -- hexproof, which is far too strong. Neither passes both assertions.
  --
  -- The grant stays synthetic so this reads off the SAME Piker board as the case
  -- above, with the controller axis as the only thing that moved between them.
  -- Knight of Grace makes the same "your opponents control" assertion off a real
  -- printing below.
  Spec.it s "CR 702.11d hexproof from black does not stop its own controller's black spell" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, board) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        guarded = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof (Just (Filter.Type.HasColor Color.Black)))) board
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let (spellId, onStack) = S.spellOnStack doomBlade S.bob guarded
            reaches caster = Set.member (Recipient.ToCreature pikerId) (Target.legalRecipients (Just caster) spellId theSlot onStack)
        Spec.assertBool s (reaches S.bob) "bob may aim his own Doom Blade at his own Piker (CR 702.11d, 'your opponents control')"
        Spec.assertBool s (not (reaches S.alice)) "alice may not aim the same spell at it"

  -- CR 702.11e: "Any effect that causes an object to lose hexproof will cause an
  -- object to lose all 'hexproof from [quality]' abilities."
  --
  -- Humility ("All creatures lose all abilities and have base power and toughness
  -- 1/1", CR 613.1f layer 6) is pawl's only hexproof-remover -- Modification has
  -- GainKeyword and LoseAllAbilities and nothing narrower -- so it is the witness
  -- this rule gets. The rule is really held BY CONSTRUCTION rather than by this
  -- case: the quality rides the Hexproof constructor, so there is no second
  -- keyword for an ability-removing effect to miss, and the CR 613.1f case above
  -- and this one are the same code path with a different payload.
  --
  -- The grant is stamped BEFORE Humility enters, so CR 613.7's timestamp order
  -- puts the removal last within layer 6. Stamped the other way round the grant
  -- would win, which is CR 613.7 working and not this rule failing.
  Spec.it s "CR 702.11e Humility takes hexproof from black away with the rest of the abilities" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, board) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        guarded = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof (Just (Filter.Type.HasColor Color.Black)))) board
        humbled = S.withHumility humility guarded
    case S.spellTargetSlot doomBlade of
      Nothing -> Spec.assertFailure s "Doom Blade should declare a target slot"
      Just theSlot -> do
        let reaches gs =
              let (spellId, onStack) = S.spellOnStack doomBlade S.alice gs
               in Set.member (Recipient.ToCreature pikerId) (Target.legalRecipients (Just S.alice) spellId theSlot onStack)
        Spec.assertBool s (not (reaches guarded)) "before Humility alice's Doom Blade cannot reach the Piker"
        Spec.assertBool s (reaches humbled) "under Humility it can, the variant having gone with the rest"

  -- The whole card, through the CAST path rather than as a set membership: CR
  -- 601.2c makes a spell with no legal target for a slot uncastable at all, which
  -- is what Pawl.Engine.Cast.castable asks through Target.fillableModes.
  --
  -- The point this case makes that the set-membership ones cannot: the source
  -- Cast passes is the card IN HAND, not a spell object on the stack, and rule
  -- 702.11d's quality has to be answerable of it. A black card in a hand is black
  -- (CR 202.2), so the two frames agree -- but nothing else here would notice if
  -- they stopped.
  --
  -- Both rows again, so neither answer is a Doom Blade that never worked: the
  -- board differs only in the quality, and the white row goes all the way through
  -- resolution to a dead Piker.
  Spec.it s "CR 601.2c whole card: hexproof from black leaves alice's Doom Blade no legal target, hexproof from white leaves it one" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (pikerId, board) = S.addCreature piker S.bob (S.landsInPlay swamp 2) -- {1}{B}
        (base, dbId) = S.handOne doomBlade board
        guarded quality = S.withEffect pikerId (Modification.GainKeyword (Keyword.Hexproof (Just (Filter.Type.HasColor quality)))) base
        resolve gs = snd (Engine.runGamePure S.identityAnswer (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId))) Stack.resolveTop)
    Spec.assertBool s (S.castable S.alice dbId base) "with no hexproof at all the spell has a legal target"
    Spec.assertBool s (not (S.castable S.alice dbId (guarded Color.Black))) "hexproof from black leaves the black Doom Blade none"
    Spec.assertBool s (S.castable S.alice dbId (guarded Color.White)) "hexproof from white leaves it the same one it had"
    Spec.assertEqWith s "and bob's Piker dies to it" (S.creaturesInPlay S.bob (resolve (guarded Color.White))) 0
    Spec.assertEqWith s "while the black row leaves it alive" (S.creaturesInPlay S.bob (guarded Color.Black)) 1

  -- THE PRINTED CARD, which is what the cases above stand in for: Knight of Grace
  -- ({1}{W} Creature -- Human Knight 2/2, "First strike / Hexproof from black /
  -- This creature gets +1/+0 as long as any player controls a black permanent"),
  -- oracle text verified against Scryfall. Its variant arrives off the card's own
  -- `keywords`, through the codec, with no test-side grant anywhere -- so this is
  -- the case that would notice the wire format and `Keyword.Hexproof`'s payload
  -- disagreeing, which no synthetic grant can.
  --
  -- THREE ANSWERS OFF ONE BOARD, and each is load-bearing:
  --
  --   * alice's black Doom Blade does NOT reach it (CR 702.11d).
  --   * alice's WHITE Angelic Edict does -- without this leg an implementation
  --     that read the variant as CR 702.11b's plain hexproof passes.
  --   * BOB's own black Doom Blade does, bob being the Knight's controller. CR
  --     702.11d stops only what "your opponents control", and reading this off
  --     alice would let "that player" and "an opponent" collapse into each other.
  --
  -- The Knight is itself WHITE (CR 202.2, {1}{W}), which is what makes the first
  -- leg discriminating: an implementation that matched the quality against the
  -- CANDIDATE's colours rather than the source's finds no black on it and admits
  -- the Doom Blade.
  --
  -- Every spell is a REAL object on the stack for the reason the CR 702.11d case
  -- above gives: S.noSource names no object, its view carries no colour, and the
  -- whole group would pass vacuously.
  Spec.it s "CR 702.11d Knight of Grace stops an opponent's black spell, admits their white one, and admits its own controller's black one" $ do
    knight <- S.printingOf s registry "Knight of Grace"
    doomBlade <- S.printingOf s registry "Doom Blade"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    case (S.spellTargetSlot doomBlade, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        let (knightId, board) = S.addCreature knight S.bob (Setup.emptyGame S.bothPlayers)
            -- Doom Blade's pool is Creatures and Angelic Edict's is Permanents,
            -- so the same Knight is tagged differently in the two sets (CR 115).
            reaches printing theSlot tag caster =
              let (spellId, onStack) = S.spellOnStack printing caster board
               in Set.member (tag knightId) (Target.legalRecipients (Just caster) spellId theSlot onStack)
        Spec.assertBool
          s
          (not (reaches doomBlade blackSlot Recipient.ToCreature S.alice))
          "alice's black Doom Blade cannot target bob's Knight of Grace (CR 702.11d)"
        Spec.assertBool
          s
          (reaches angelicEdict whiteSlot Recipient.ToObject S.alice)
          "her white Angelic Edict can -- the half a plain-hexproof reading loses"
        Spec.assertBool
          s
          (reaches doomBlade blackSlot Recipient.ToCreature S.bob)
          "and bob's own black Doom Blade can, CR 702.11d stopping only what his opponents control"
      _ -> Spec.assertFailure s "Doom Blade and Angelic Edict should each declare a target slot"

  -- CR 702.16b: "A permanent or player with protection can't be targeted by
  -- spells with the stated quality and can't be targeted by abilities from a
  -- source with the stated quality." Apostle of Purifying Light ({1}{W} Creature
  -- -- Human Cleric 2/1, M20, "Protection from black" plus "{2}: Exile target
  -- card from a graveyard"), oracle text verified against Scryfall. The quality
  -- arrives off the card's own `keywords` through the codec, with no test-side
  -- grant anywhere.
  --
  -- THREE ANSWERS OFF ONE BOARD, and the second is the one that tells this rule
  -- from CR 702.11d's:
  --
  --   * alice's black Doom Blade does NOT reach bob's Apostle.
  --   * BOB's own black Doom Blade does not reach it EITHER. Rule 702.16b names
  --     no player where rule 702.11d stops only what "your opponents control", so
  --     this is the leg a hexproof-shaped implementation fails -- and it is the
  --     exact leg the Knight of Grace case above answers the other way, off the
  --     same two spells.
  --   * Her WHITE Angelic Edict does reach it -- without this leg an
  --     implementation that read protection as CR 702.18a's shroud passes.
  --
  -- The Apostle is itself WHITE (CR 202.2, {1}{W}), which is what makes the first
  -- leg discriminating: an implementation matching the quality against the
  -- CANDIDATE's colours rather than the source's finds no black on it and admits
  -- the Doom Blade.
  --
  -- Every spell is a REAL object on the stack for the reason the CR 702.11d cases
  -- above give: S.noSource names no object, its view carries no colour, and the
  -- whole case would pass vacuously.
  Spec.it s "CR 702.16b Apostle of Purifying Light stops a black spell whoever casts it, and admits a white one" $ do
    apostle <- S.printingOf s registry "Apostle of Purifying Light"
    doomBlade <- S.printingOf s registry "Doom Blade"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    case (S.spellTargetSlot doomBlade, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        let (apostleId, board) = S.addCreature apostle S.bob (Setup.emptyGame S.bothPlayers)
            -- Doom Blade's pool is Creatures and Angelic Edict's is Permanents,
            -- so the same Apostle is tagged differently in the two sets (CR 115).
            reaches printing theSlot tag caster =
              let (spellId, onStack) = S.spellOnStack printing caster board
               in Set.member (tag apostleId) (Target.legalRecipients (Just caster) spellId theSlot onStack)
        Spec.assertBool
          s
          (not (reaches doomBlade blackSlot Recipient.ToCreature S.alice))
          "alice's black Doom Blade cannot target bob's Apostle (CR 702.16b)"
        Spec.assertBool
          s
          (not (reaches doomBlade blackSlot Recipient.ToCreature S.bob))
          "and neither can bob's own -- CR 702.16b names no player, unlike CR 702.11d"
        Spec.assertBool
          s
          (reaches angelicEdict whiteSlot Recipient.ToObject S.alice)
          "her white Angelic Edict can -- the half a shroud-shaped reading loses"
      _ -> Spec.assertFailure s "Doom Blade and Angelic Edict should each declare a target slot"

  -- The same card through the CAST path and on to a resolution, which is what the
  -- set-membership case above stands in for: CR 601.2c makes a spell with no legal
  -- target for a slot uncastable at all, and the source Cast passes is the card IN
  -- HAND rather than a spell object on the stack.
  --
  -- ONE PAIR OF BOARDS DIFFERING IN ONE THING, and Humility is what moves: "All
  -- creatures lose all abilities and have base power and toughness 1/1" is CR
  -- 613.1f at layer 6, and CR 702.16 states no clause holding protection clear of
  -- it. So the humbled row is the same Apostle with the same Doom Blade in the
  -- same hand over the same two Swamps, minus the keyword -- which also makes this
  -- the case that proves the read is POST-layer rather than off the printed card.
  Spec.it s "CR 601.2c whole card: protection leaves alice's Doom Blade no legal target until Humility takes it away" $ do
    swamp <- S.printingOf s registry "Swamp"
    apostle <- S.printingOf s registry "Apostle of Purifying Light"
    doomBlade <- S.printingOf s registry "Doom Blade"
    humility <- S.printingOf s registry "Humility"
    let (_, board) = S.addCreature apostle S.bob (S.landsInPlay swamp 2) -- {1}{B}
        (base, dbId) = S.handOne doomBlade board
        humbled = S.withHumility humility base
        resolve gs = snd (Engine.runGamePure S.identityAnswer (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId))) Stack.resolveTop)
    Spec.assertBool s (not (S.castable S.alice dbId base)) "CR 702.16b protection from black leaves the black Doom Blade no legal target at all"
    Spec.assertBool s (S.castable S.alice dbId humbled) "CR 613.1f under Humility it has one again"
    Spec.assertEqWith s "and the humbled Apostle dies to it" (S.creaturesInPlay S.bob (resolve humbled)) 0
    Spec.assertEqWith s "while the protected one is still standing, never having been aimed at" (S.creaturesInPlay S.bob base) 1

  -- CR 702.16j: "A permanent or player with protection from everything has
  -- protection from each object regardless of that object's characteristic
  -- values." Progenitus ({W}{W}{U}{U}{B}{B}{R}{R}{G}{G} Legendary Creature --
  -- Hydra Avatar 10/10), oracle text verified against Scryfall. The variant needs
  -- no second keyword: the quality is Filter.And [], which every object
  -- satisfies, so all four of rule 702.16's prohibitions read it as they read a
  -- colour.
  --
  -- FOUR ANSWERS OFF ONE BOARD, in two pairs differing only in which of bob's two
  -- creatures the spell is aimed at. A Hill Giant (3/3, no abilities) stands
  -- beside Progenitus so neither negative can pass for a spell that reaches
  -- nothing at all, and the two spells differ in COLOUR -- alice's black Murder
  -- and her white Angelic Edict -- which is what tells this quality from any
  -- single-colour one: an implementation reading Progenitus as protection from
  -- one colour admits the other spell.
  --
  -- MURDER and not the Doom Blade the CR 702.16b case above uses: Progenitus is
  -- itself black (CR 202.2), so "target nonblack creature" could never aim at it
  -- and that leg would pass with no protection anywhere. Murder's pool is
  -- Creatures unfiltered.
  Spec.it s "CR 702.16j Progenitus's protection from everything stops a black spell and a white one alike" $ do
    progenitus <- S.printingOf s registry "Progenitus"
    giant <- S.printingOf s registry "Hill Giant"
    murder <- S.printingOf s registry "Murder"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    case (S.spellTargetSlot murder, S.spellTargetSlot angelicEdict) of
      (Just blackSlot, Just whiteSlot) -> do
        let (progenitusId, board0) = S.addCreature progenitus S.bob (Setup.emptyGame S.bothPlayers)
            (giantId, board) = S.addCreature giant S.bob board0
            -- Murder's pool is Creatures and Angelic Edict's is Permanents, so
            -- the same creature is tagged differently in the two sets (CR 115).
            reaches printing theSlot tag victim =
              let (spellId, onStack) = S.spellOnStack printing S.alice board
               in Set.member (tag victim) (Target.legalRecipients (Just S.alice) spellId theSlot onStack)
        Spec.assertBool
          s
          (not (reaches murder blackSlot Recipient.ToCreature progenitusId))
          "CR 702.16j alice's black Murder cannot target bob's Progenitus"
        Spec.assertBool
          s
          (not (reaches angelicEdict whiteSlot Recipient.ToObject progenitusId))
          "and neither can her white Angelic Edict -- the leg a single-colour quality loses"
        Spec.assertBool
          s
          (reaches murder blackSlot Recipient.ToCreature giantId)
          "while the Hill Giant beside it admits the Murder"
        Spec.assertBool
          s
          (reaches angelicEdict whiteSlot Recipient.ToObject giantId)
          "and the Angelic Edict too, so neither refusal above is a spell that reaches nothing"
      _ -> Spec.assertFailure s "Murder and Angelic Edict should each declare a target slot"

  -- CR 608.2b: "If the spell or ability specifies targets, it checks whether the
  -- targets are still legal. ... If all its targets, for every instance of the
  -- word 'target,' are now illegal, the spell or ability doesn't resolve." The
  -- second of CR 115's two moments, and the controller axis has to be read at
  -- both: a Goblin Piker that gains hexproof in response is out of an OPPONENT's
  -- Doom Blade and still squarely in its own controller's.
  --
  -- Four resolutions off two boards that differ only in who controls the Piker,
  -- so neither answer can be a Doom Blade that never worked. No card in this pool
  -- GRANTS hexproof, so the grant is a stored layer-6 continuous effect
  -- (S.withEffect), as the shroud case above does it.
  Spec.it s "CR 608.2b gaining hexproof in response fizzles an opponent's Doom Blade but not its controller's" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let castAt controller =
          let (pikerId, board) = S.addCreature piker controller (S.landsInPlay swamp 2)
              (gs, dbId) = S.handOne doomBlade board
           in (pikerId, snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice dbId)))
        resolve g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
        (theirPiker, atTheirs) = castAt S.bob
        (myPiker, atMine) = castAt S.alice
        hexproofed oid = S.withEffect oid (Modification.GainKeyword (Keyword.Hexproof Nothing))
    Spec.assertEqWith s "untouched, bob's Piker dies" (S.creaturesInPlay S.bob (resolve atTheirs)) 0
    Spec.assertEqWith s "hexproofed in response, it survives alice's Doom Blade" (S.creaturesInPlay S.bob (resolve (hexproofed theirPiker atTheirs))) 1
    Spec.assertEqWith s "untouched, alice's own Piker dies" (S.creaturesInPlay S.alice (resolve atMine)) 0
    Spec.assertEqWith s "and hexproof does not save it from its own controller (CR 702.11b)" (S.creaturesInPlay S.alice (resolve (hexproofed myPiker atMine))) 0

  -- CR 113.9, the whole rule, as two DISJOINT pools: "activated and triggered
  -- abilities on the stack aren't spells, and therefore can't be countered by
  -- anything that counters only spells. Activated and triggered abilities on
  -- the stack can be countered by effects that specifically counter abilities."
  --
  -- One board with one of each on the stack -- alice's Goblin Piker spell, and
  -- her Prodigal Sorcerer's activated ability above it -- so neither exclusion
  -- can pass by the pool simply being empty. Cancel ("counter target spell",
  -- Pool.Spells) and Stifle ("counter target activated or triggered ability",
  -- Pool.Abilities) are read off their committed printings, so this is the
  -- printed text and not a hand-built target slot.
  --
  -- CR 115.2 is what admits either at all: "only permanents are legal targets
  -- ... unless a spell or ability ... (b) targets an object that can't exist on
  -- the battlefield, such as a spell or ability."
  Spec.it s "CR 113.9 Stifle's pool holds only the ability and Cancel's only the spell" $ do
    cancel <- S.printingOf s registry "Cancel"
    stifle <- S.printingOf s registry "Stifle"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    piker <- S.printingOf s registry "Goblin Piker"
    case Face.activatedAbilities (S.combinedFace sorcerer) of
      [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      ability : _ -> do
        let (srcId, withSorcerer) = S.addCreature sorcerer S.alice (Setup.emptyGame S.bothPlayers)
            -- CR 302.6: the Sorcerer has to have settled before its {T} is legal.
            settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
            (spellId, onStack) = S.spellOnStack piker S.alice settled
            gs = S.runPure S.identityAnswer (onStack {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice srcId ability)
            abilIds = filter (/= spellId) (GameState.stack gs)
            -- soleTargetSlot, not S.spellTargetSlot: neither card calls its slot
            -- "target" -- Cancel's is "spell" and Stifle's is "ability".
            legalFor printing = fmap (\theSlot -> Target.legalRecipients (Just S.bob) S.noSource theSlot gs) (soleTargetSlot (Face.spell (S.combinedFace printing)))
        case (abilIds, legalFor cancel, legalFor stifle) of
          ([abilId], Just cancelLegal, Just stifleLegal) -> do
            Spec.assertEqWith s "Cancel sees the spell and only the spell" cancelLegal (Set.singleton (Recipient.ToObject spellId))
            Spec.assertEqWith s "Stifle sees the ability and only the ability" stifleLegal (Set.singleton (Recipient.ToObject abilId))
          _ -> Spec.assertFailure s "the fixture should put one ability and one spell on the stack, and both cards should declare a target slot"

  -- CR 113.3b against CR 113.3c, INSIDE the pool the case above holds against
  -- Pool.Spells: Stifle's "activated or triggered ability" reaches both kinds and
  -- Squelch's "target activated ability" reaches one, which is
  -- Filter.IsActivatedAbility narrowing the same Pool.Abilities rather than a
  -- second pool. Both offers are read off the committed printings.
  --
  -- One board carrying one of each kind, so neither offer can pass by the pool
  -- being empty: alice's Prodigal Sorcerer's {T} (CR 113.3b) and the Aether Flash
  -- trigger her Goblin Piker's entry raised (CR 113.3c).
  --
  -- Pawl.CounterspellSpec's Squelch group is the gameplay-level twin.
  Spec.it s "CR 113.3b Squelch's pool holds the activated ability alone where Stifle's holds both" $ do
    mountain <- S.printingOf s registry "Mountain"
    aetherFlash <- S.printingOf s registry "Aether Flash"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    stifle <- S.printingOf s registry "Stifle"
    squelch <- S.printingOf s registry "Squelch"
    case soleActivatedAbility sorcerer of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just ping -> do
        let (srcId, withSorcerer) = S.addCreature sorcerer S.alice (Setup.emptyGame S.bothPlayers)
            -- CR 302.6: the Sorcerer has to have settled before its {T} is legal.
            settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
            (_, withFlash) = S.addCreature aetherFlash S.alice settled
            withLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) withFlash [1 .. (2 :: Int)]
            (pikerId, withPiker) = S.addHandCard piker S.alice withLands
            cast = S.runPure S.identityAnswer withPiker (S.cast S.alice pikerId)
            -- CR 603.3 puts Aether Flash's trigger on the stack once the Piker
            -- has entered.
            entered = S.runPure S.identityAnswer cast Stack.resolveTop
            placed = S.runPure S.identityAnswer entered Engine.settleForPriority
            atAlice :: Prompt.Prompt r -> r
            atAlice p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
              _ -> S.identityAnswer p
            gs = S.runPure atAlice (placed {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice srcId ping)
            legalFor printing = fmap (\theSlot -> Target.legalRecipients (Just S.bob) S.noSource theSlot gs) (soleTargetSlot (Face.spell (S.combinedFace printing)))
        case (GameState.stack placed, GameState.stack gs, legalFor stifle, legalFor squelch) of
          ([triggerId], [abilId, _], Just stifleLegal, Just squelchLegal) -> do
            Spec.assertEqWith s "Squelch sees the activated ability and only it" squelchLegal (Set.singleton (Recipient.ToObject abilId))
            Spec.assertEqWith s "where Stifle sees both kinds" stifleLegal (Set.fromList [Recipient.ToObject abilId, Recipient.ToObject triggerId])
          _ -> Spec.assertFailure s "the fixture should put one trigger and then one activated ability on the stack, and both cards should declare a target slot"

  -- CR 602.2a creates an activated ability on the stack BEFORE CR 602.2b routes
  -- the rest of the activation through CR 601.2b-i, so CR 601.2c's targets are
  -- chosen in a state that already holds the ability. Activate.activateAbility
  -- chooses them against the PRE-MINT snapshot instead. The two states differ by
  -- exactly the ability object, and CR 115.5 -- "a spell or ability on the stack
  -- is an illegal target for itself" -- subtracts that object anyway, so no board
  -- tells them apart and this case cannot prove which one pawl reads.
  --
  -- What it does prove is the HALF-correction wrong, and that is why it is here:
  -- legalRecipientsGiven's CR 115.5 gate is `source` being on the stack, and on
  -- this path `source` is the source PERMANENT (see its own note). Moving this
  -- caller to the post-mint state without also handing that function the
  -- ability's own id offers the ability itself, and the answerer below takes it.
  --
  -- Adric, Mathematical Genius' "Ultimate Sacrifice -- {1}{U}, Sacrifice Adric:
  -- Counter target activated or triggered ability" is Stifle's undifferentiated
  -- Pool.Abilities on an ACTIVATED ability, which is what makes the path
  -- reachable at all. Not implemented, so the card file omits it: its other
  -- ability, "{2}{U}, {T}: Copy target activated or triggered ability you
  -- control. You may choose new targets for the copy" -- copying a SPELL on the
  -- stack landed with Twincast, but copying an ABILITY did not (#2208) -- which
  -- leaves pawl's Adric stricter than printed.
  -- "Ultimate Sacrifice" is an ability word (CR 207.2c) and Doctor's companion is
  -- deck construction (CR 903); neither has a rules meaning in play.
  --
  -- The answerer takes the LARGEST recipient, and Recipient's derived Ord orders
  -- ToObject by ObjectId while Game.freshObjectId hands out increasing ones -- so
  -- it names the newest object on the stack the moment it is ever offered one.
  -- bob's Prodigal Sorcerer ability is aimed at ALICE, so whether it was
  -- countered is readable as her life total rather than as a stack length, which
  -- both readings leave empty.
  Spec.it s "CR 602.2a/115.5 Adric's ability is not offered its own object among the abilities it may counter" $ do
    island <- S.printingOf s registry "Island"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    adric <- S.printingOf s registry "Adric, Mathematical Genius"
    case (soleActivatedAbility sorcerer, soleActivatedAbility adric) of
      (Just ping, Just ultimateSacrifice) -> do
        let (srcId, withSorcerer) = S.addCreature sorcerer S.bob (Setup.emptyGame S.bothPlayers)
            -- CR 302.6: the Sorcerer must have settled before its {T} is legal.
            -- Adric needs no such thing -- its cost carries no {T}.
            settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.bob)
            (adricId, withAdric) = S.addCreature adric S.alice settled
            (_, withIsland) = S.addCreature island S.alice withAdric
            (_, withIslands) = S.addCreature island S.alice withIsland
            atAlice :: Prompt.Prompt r -> r
            atAlice p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
              _ -> S.identityAnswer p
            pinging = S.runPure atAlice (withIslands {GameState.priority = Just S.bob}) (Activate.activateAbility S.bob srcId ping)
            takeNewest :: Prompt.Prompt r -> r
            takeNewest p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> maybe Set.empty Set.singleton (Set.lookupMax legal)) sets
              _ -> S.identityAnswer p
            countering = S.runPure takeNewest (pinging {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice adricId ultimateSacrifice)
            after = S.runPure S.identityAnswer (S.runPure S.identityAnswer countering Stack.resolveTop) Stack.resolveTop
        case GameState.stack pinging of
          [pingId] -> do
            -- The discriminating pair, stated first: countering the ping is the
            -- only way alice's life stays 20. An ability that had been offered
            -- itself would have taken itself instead, and the ping would have
            -- resolved.
            Spec.assertEqWith s "alice's life is untouched: Adric's ability countered the ping (CR 701.6a)" (S.lifeOf S.alice after) (Just 20)
            Spec.assertEqWith s "and no damage was ever dealt" (fmap DamageEvent.amount (Maybe.mapMaybe Event.damageOf (S.eventsOf after))) []
            -- Supporting, not discriminating: an ability leaves the stack
            -- whether it resolved (CR 608.2n) or was countered (CR 701.6a), so
            -- this and the two below hold under both readings. They are here to
            -- stop a green that came from the activation never happening.
            Spec.assertEqWith s "the ping is no longer an object at all" (Game.lookupObject pingId after) Nothing
            Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
            Spec.assertEqWith s "Adric is in alice's graveyard: the sacrifice was paid" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
            -- Legibility, and last so it can absorb no mutation the assertions
            -- above should catch: the pool the announcement was made against held
            -- the ping alone.
            case soleTargetSlot (ActivatedAbility.modal ultimateSacrifice) of
              Nothing -> Spec.assertFailure s "Ultimate Sacrifice should declare one target slot"
              Just theSlot -> Spec.assertEqWith s "and the pool held the ping and nothing else" (Target.legalRecipients (Just S.alice) adricId theSlot pinging) (Set.singleton (Recipient.ToObject pingId))
          _ -> Spec.assertFailure s "the fixture should put exactly one ability on the stack"
      _ -> Spec.assertFailure s "Prodigal Sorcerer and Adric should each declare exactly one activated ability"

  -- CR 115.2's OTHER escape hatch, the one Pool.Spells and Pool.Abilities are
  -- not: "only permanents are legal targets for spells and abilities, unless a
  -- spell or ability (a) SPECIFIES THAT IT CAN TARGET AN OBJECT IN ANOTHER ZONE
  -- or a player". Raise Dead's "target creature card in your graveyard" is that
  -- clause, and its pool is Pool.CardsInGraveyard.
  --
  -- Three ways it could go wrong, on ONE board, because each is a different
  -- mistake and any of them alone would pass against a pool that stayed empty:
  --
  --   * CR 400.1's per-player zone -- "each player has their own library, hand,
  --     and graveyard" -- is why the pool carries a ZoneScope at all, and
  --     bob's copy of the very same card is what proves the axis is real rather
  --     than decorative. It cannot be a Filter: CR 108.4 says "a card doesn't
  --     have a controller unless that card represents a permanent or spell", so
  --     ControlledBy is vacuously False for every card in every graveyard.
  --   * the Filter still narrows, so alice's Lightning Bolt is out.
  --   * the pool is DISJOINT from Pool.Creatures, the way Pool.Abilities is from
  --     Pool.Spells: the Piker on the battlefield is offered under neither tag.
  --     CR 109.2's battlefield default does not reach this card, because its text
  --     says the word "card" outright.
  Spec.it s "CR 115.2 clause (a) Raise Dead reaches the creature card in your graveyard and nothing else" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (inPlayId, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (myBoltId, g3) = S.addGraveyardCard bolt S.alice g2
        (theirsId, gs) = S.addGraveyardCard piker S.bob g3
    case S.spellTargetSlot raiseDead of
      Nothing -> Spec.assertFailure s "Raise Dead should declare a target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToObject mineId) legal) "the creature card in alice's own graveyard is legal"
        Spec.assertBool s (not (Set.member (Recipient.ToObject theirsId) legal)) "the identical card in bob's graveyard is not (CR 400.1)"
        Spec.assertBool s (not (Set.member (Recipient.ToObject myBoltId) legal)) "nor is the instant card beside it (the Filter narrows)"
        Spec.assertBool s (not (Set.member (Recipient.ToObject inPlayId) legal)) "nor the Piker on the battlefield, under ToObject"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature inPlayId) legal)) "nor under ToCreature (disjoint from Pool.Creatures)"
        Spec.assertEqWith s "and nothing else at all" legal (Set.singleton (Recipient.ToObject mineId))

  -- The move itself, cast and resolved through the stack: CR 400.7 mints a NEW
  -- object in the hand ("an object that moves from one zone to another becomes a
  -- new object"), so the card is asserted by name in the hand and by the absence
  -- of the id it was targeted under from the graveyard. Raise Dead's own trip to
  -- the graveyard is CR 404.1's "as is any instant or sorcery spell that's
  -- finished resolving".
  Spec.it s "CR 115.2 clause (a) whole card: Raise Dead returns the targeted creature card to alice's hand" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (mineId, board) = S.addGraveyardCard piker S.alice (S.landsInPlay swamp 1)
        (gs, rdId) = S.handOne raiseDead board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice rdId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "Raise Dead went on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "the Piker card is in alice's hand" (S.countByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice resolved) 1
    Spec.assertBool
      s
      (notElem mineId (Game.zoneMembers Zone.Graveyard S.alice resolved))
      "and the object it was targeted under is gone from the graveyard (CR 400.7)"
    Spec.assertEqWith s "that graveyard holds one card, the spent Raise Dead (CR 404.1)" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
    Spec.assertEqWith s "and alice's hand holds one card, the Piker and not the spell" (S.handSize S.alice resolved) 1

  -- CR 608.2b: "A target that's no longer in the zone it was in when it was
  -- targeted is illegal. ... If all its targets ... are now illegal, the spell or
  -- ability doesn't resolve. It's removed from the stack and, if it's a spell,
  -- put into its owner's graveyard."
  --
  -- The response is Event.changeZone rather than a card, because no card in this
  -- pool can be cast in response to a sorcery AND move a card out of a graveyard:
  -- Rest in Peace is the only one that empties a graveyard, and it does it from
  -- an enchantment's enters trigger -- CR 303.1 lets an enchantment be cast only
  -- "during a main phase of their turn when the stack is empty", which is exactly
  -- when Raise Dead is not on it. Both halves run off one board and one cast, so
  -- the fizzle cannot be a Raise Dead that never worked.
  Spec.it s "CR 608.2b Raise Dead fizzles when its target leaves the graveyard in response" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (mineId, board) = S.addGraveyardCard piker S.alice (S.landsInPlay swamp 1)
        (gs, rdId) = S.handOne raiseDead board
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice rdId))
        resolve g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
        returned = resolve cast
        fizzled = resolve (S.runPure S.identityAnswer cast (Event.changeZone mineId Zone.Exile))
    Spec.assertEqWith s "untouched, the Piker card comes back" (S.countByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice returned) 1
    Spec.assertEqWith s "exiled in response, nothing comes back" (S.countByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.alice fizzled) 0
    Spec.assertEqWith s "and Raise Dead is in alice's graveyard either way" (length (Game.zoneMembers Zone.Graveyard S.alice fizzled)) 1

  -- CR 400.1's OTHER half. Raise Dead above says "in your graveyard"; Withered
  -- Wretch's "{1}: Exile target card from a graveyard" names no player at all, so
  -- every player's copy of the zone is in the pool at once -- a SET of players
  -- rather than a relation to one.
  --
  -- Raise Dead is read on the SAME graveyards and asserted here, because widening
  -- that axis and DELETING it look identical from the Wretch's side alone: a pool
  -- that had stopped asking whose graveyard it was reading would satisfy every
  -- assertion about the Wretch below and quietly hand Raise Dead bob's graveyard
  -- too.
  --
  -- The ABSENT Filter is the second claim. "Target card" carries no card type, so
  -- the Lightning Bolt card is as legal as the Piker card beside it; Raise Dead's
  -- HasCardType Creature is what makes that contrast a real one rather than a
  -- vacuous one. CR 109.2's battlefield default is switched off for both by the
  -- printed word "card", so neither slot reaches the Piker on the battlefield.
  Spec.it s "CR 115.2 clause (a) Withered Wretch reaches every graveyard, and Raise Dead still reaches only alice's" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    wretch <- S.printingOf s registry "Withered Wretch"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (inPlayId, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (myBoltId, g3) = S.addGraveyardCard bolt S.alice g2
        (theirsId, gs) = S.addGraveyardCard piker S.bob g3
        wretchSlots = Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace wretch))
    case (wretchSlots, S.spellTargetSlot raiseDead) of
      ([wretchSlot], Just raiseDeadSlot) -> do
        let legal theSlot = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertEqWith
          s
          "the Wretch offers every card in every graveyard, of every card type, and nothing else"
          (legal wretchSlot)
          (Set.fromList (fmap Recipient.ToObject [mineId, myBoltId, theirsId]))
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature inPlayId) (legal wretchSlot)))
          "and not the Piker on the battlefield under ToCreature either (disjoint from Pool.Creatures)"
        Spec.assertEqWith
          s
          "while Raise Dead, on those same graveyards, still reaches only alice's creature card"
          (legal raiseDeadSlot)
          (Set.singleton (Recipient.ToObject mineId))
      _ -> Spec.assertFailure s "Withered Wretch should print one ability with one target slot, and Raise Dead one slot"

  -- The move itself, activated and resolved through the stack, in BOTH
  -- directions off one board: "a graveyard" is a claim about two candidate sets,
  -- and exiling from your own proves only the half Raise Dead already proved.
  --
  -- CR 406.2: "To exile an object is to put it into the exile zone from whatever
  -- zone it's currently in." Exile is SHARED (CR 400.1: "the other zones are
  -- shared by all players"), so Game.zoneMembers reads it per owner rather than
  -- per player's copy -- which is how each assertion below names whose card
  -- moved.
  Spec.it s "CR 406.2 whole card: Withered Wretch exiles a card from alice's graveyard, and from bob's" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wretch <- S.printingOf s registry "Withered Wretch"
    let (wretchId, g1) = S.addCreature wretch S.alice (S.landsInPlay swamp 1)
        (mineId, g2) = S.addGraveyardCard piker S.alice g1
        (theirsId, g3) = S.addGraveyardCard piker S.bob g2
        board = g3 {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace wretch) of
      [ability] -> do
        let exiling oid = S.runPure (aimAtCard oid) (S.runPure (aimAtCard oid) board (Activate.activateAbility S.alice wretchId ability)) Stack.resolveTop
            mine = exiling mineId
            theirs = exiling theirsId
        Spec.assertEqWith s "aimed at her own, alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice mine)) 0
        Spec.assertEqWith s "and the exiled card is hers" (length (Game.zoneMembers Zone.Exile S.alice mine)) 1
        Spec.assertEqWith s "with bob's graveyard untouched" (length (Game.zoneMembers Zone.Graveyard S.bob mine)) 1
        Spec.assertEqWith s "aimed at bob's, HIS graveyard is the empty one" (length (Game.zoneMembers Zone.Graveyard S.bob theirs)) 0
        Spec.assertEqWith s "and the exiled card is his" (length (Game.zoneMembers Zone.Exile S.bob theirs)) 1
        Spec.assertEqWith s "with alice's graveyard untouched" (length (Game.zoneMembers Zone.Graveyard S.alice theirs)) 1
      abilities -> Spec.assertFailure s ("expected one activated ability on Withered Wretch, got " <> show (length abilities))

  -- CR 608.2b for an ABILITY rather than a spell, and against the opponent's
  -- graveyard: "A target that's no longer in the zone it was in when it was
  -- targeted is illegal. ... If all its targets ... are now illegal, the spell or
  -- ability doesn't resolve."
  --
  -- The response moves the card to bob's HAND rather than exiling it, so the two
  -- outcomes are told apart by the exile zone: a fizzle leaves it empty, and an
  -- ability that resolved anyway would put something in it. Both halves run off
  -- one activation, so the fizzle cannot be an activation that never worked.
  Spec.it s "CR 608.2b Withered Wretch's activation fizzles when the card leaves the graveyard in response" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wretch <- S.printingOf s registry "Withered Wretch"
    let (wretchId, g1) = S.addCreature wretch S.alice (S.landsInPlay swamp 1)
        (theirsId, g2) = S.addGraveyardCard piker S.bob g1
        board = g2 {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace wretch) of
      [ability] -> do
        let activated = S.runPure (aimAtCard theirsId) board (Activate.activateAbility S.alice wretchId ability)
            resolve g = S.runPure (aimAtCard theirsId) g Stack.resolveTop
            exiled = resolve activated
            fizzled = resolve (S.runPure S.identityAnswer activated (Event.changeZone theirsId Zone.Hand))
        Spec.assertEqWith s "the activation put one ability on the stack" (length (GameState.stack activated)) 1
        Spec.assertEqWith s "untouched, bob's card is exiled" (length (Game.zoneMembers Zone.Exile S.bob exiled)) 1
        Spec.assertEqWith s "taken to his hand in response, nothing is exiled at all" (length (Game.zoneMembers Zone.Exile S.bob fizzled)) 0
        Spec.assertEqWith s "and the card is still in his hand" (length (Game.zoneMembers Zone.Hand S.bob fizzled)) 1
        Spec.assertEqWith s "with the ability off the stack either way" (length (GameState.stack fizzled)) 0
      abilities -> Spec.assertFailure s ("expected one activated ability on Withered Wretch, got " <> show (length abilities))

  -- CR 115.2 clause (a)'s SECOND zone. Riftsweeper's "choose target face-up
  -- exiled card" names exile, which CR 400.2 lists among the public zones
  -- ("graveyard, battlefield, stack, exile, ante, and command are public
  -- zones"), so every candidate is visible to the chooser exactly as a
  -- graveyard's is.
  --
  -- Four claims off one board. The set equality is the load-bearing one and
  -- subsumes the second; the others name mistakes it would not, on its own, tell
  -- apart from each other:
  --
  --   * EITHER PLAYER's exiled card is legal. CR 400.1: "the other zones are
  --     shared by all players", so exile has no per-player copy and the pool
  --     carries no scope to select among them. bob's card is what proves that a
  --     player axis has not crept in: a pool that had quietly become
  --     "your exile" would still offer alice's.
  --   * The battlefield Piker is NOT in it, under either tag -- the pool is
  --     disjoint from Pool.Creatures and Pool.Permanents, the relation
  --     Pool.Abilities has to Pool.Spells. CR 109.2's battlefield default is
  --     switched off by the card's own word "card".
  --   * The GRAVEYARD Piker is not in it either. Two off-battlefield pools now
  --     exist and neither may swallow the other.
  --   * Withered Wretch, read on the SAME board, still reaches exactly the
  --     graveyard card and neither exiled one -- the other direction of that
  --     same disjointness, which the Riftsweeper assertions alone cannot see.
  Spec.it s "CR 115.2 clause (a) Riftsweeper reaches every exiled card and nothing else, and Withered Wretch still reaches only the graveyard" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    wretch <- S.printingOf s registry "Withered Wretch"
    let (inPlayId, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        (buriedId, g2) = S.addGraveyardCard piker S.alice g1
        (hersId, g3) = S.addExiledCard piker S.alice g2
        (hisId, gs) = S.addExiledCard bolt S.bob g3
        riftSlots = Maybe.mapMaybe (soleTargetSlot . TriggeredAbility.modal) (Face.triggeredAbilities (S.combinedFace riftsweeper))
        wretchSlots = Maybe.mapMaybe (soleTargetSlot . ActivatedAbility.modal) (Face.activatedAbilities (S.combinedFace wretch))
    case (riftSlots, wretchSlots) of
      ([riftSlot], [wretchSlot]) -> do
        let legal theSlot = Target.legalRecipients (Just S.alice) S.noSource theSlot gs
        Spec.assertEqWith
          s
          "both exiled cards, hers and his, of both card types, and nothing else"
          (legal riftSlot)
          (Set.fromList (fmap Recipient.ToObject [hersId, hisId]))
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToCreature inPlayId) (legal riftSlot)))
          "not the Piker on the battlefield under ToCreature either (disjoint from Pool.Creatures)"
        Spec.assertEqWith
          s
          "while Withered Wretch, on that same board, still reaches only the graveyard card"
          (legal wretchSlot)
          (Set.singleton (Recipient.ToObject buriedId))
      _ -> Spec.assertFailure s "Riftsweeper should print one triggered ability with one target slot, and Withered Wretch one activated ability with one"

  -- The whole card, through the trigger pipeline and the stack, in BOTH
  -- directions off one board -- because "its owner shuffles it into THEIR
  -- library" is a claim about a player alice does not pick, and aiming only at
  -- her own card would prove nothing about it.
  --
  -- CR 701.24 is the second half of the effect and is asserted by ORDER, under
  -- an interpreter that REVERSES every Prompt.Shuffle. CR 701.24a ("randomize
  -- the cards within it so that no player knows their order") makes a shuffle
  -- unobservable by any other means -- an identity shuffle and no shuffle at all
  -- look the same -- so the reversal is what turns "was it shuffled?" into a
  -- question the test can ask. bob's run is then read out a SECOND time under
  -- the identity interpreter as the control: that pins the arrival at the BOTTOM
  -- (Effect.ShuffleIntoLibrary states no library position, so the move takes
  -- LibraryPosition.defaultValue and Game.insertIntoZone appends), so the
  -- reversed run's leading position cannot be merely where the move put the
  -- card.
  --
  -- alice controls the Riftsweeper in both runs. When the exiled card is bob's,
  -- it is BOB's library that grows and BOB's that is shuffled: Game.insertIntoZone
  -- files a library arrival under Object.owner, and the shuffle is asked of that
  -- same owner -- which is what "its owner shuffles it into THEIR library" means
  -- and what a controller-relative reading would get wrong.
  Spec.it s "CR 701.24 whole card: Riftsweeper shuffles an exiled card into its OWNER's library, hers or his" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    -- S.addLibraryCard puts each card ON TOP, so the SECOND of each pair is the
    -- one at the head of the library and the first is under it.
    let (_, g1) = S.entersWithTrigger riftsweeper S.alice (Setup.emptyGame S.bothPlayers)
        (herDeeperId, g2) = S.addLibraryCard piker S.alice g1
        (herTopId, g3) = S.addLibraryCard piker S.alice g2
        (hisDeeperId, g4) = S.addLibraryCard bolt S.bob g3
        (hisTopId, g5) = S.addLibraryCard bolt S.bob g4
        (hersId, g6) = S.addExiledCard piker S.alice g5
        (hisId, board) = S.addExiledCard bolt S.bob g6
        runShuffling oid =
          let placed = S.runPure (aimAtCardShuffling oid) board Engine.placePendingTriggers
           in S.runPure (aimAtCardShuffling oid) placed Stack.resolveTop
        runPlain oid =
          let placed = S.runPure (aimAtCard oid) board Engine.placePendingTriggers
           in S.runPure (aimAtCard oid) placed Stack.resolveTop
        shuffledHers = runShuffling hersId
        shuffledHis = runShuffling hisId
        unshuffledHis = runPlain hisId
        -- CR 400.7 mints a fresh id at the destination, so the arrival is
        -- whichever library member was not seeded here.
        arrivalIn pid seeded gs = filter (\oid -> notElem oid seeded) (Game.zoneMembers Zone.Library pid gs)
    Spec.assertEqWith s "aimed at her own, exile holds only bob's card" (Set.size (GameState.exile shuffledHers)) 1
    Spec.assertEqWith s "and ALICE's library grew to three" (length (Game.zoneMembers Zone.Library S.alice shuffledHers)) 3
    Spec.assertEqWith s "with bob's library untouched" (Game.zoneMembers Zone.Library S.bob shuffledHers) [hisTopId, hisDeeperId]
    case arrivalIn S.alice [herTopId, herDeeperId] shuffledHers of
      [arrived] ->
        Spec.assertEqWith
          s
          "CR 701.24a: her library was shuffled, so the reversal shows through"
          (Game.zoneMembers Zone.Library S.alice shuffledHers)
          [arrived, herDeeperId, herTopId]
      _ -> Spec.assertFailure s "exactly one card should have arrived in alice's library"
    Spec.assertEqWith s "aimed at bob's, exile holds only alice's card" (Set.size (GameState.exile shuffledHis)) 1
    Spec.assertEqWith s "and it is BOB's library that grew, not the controller's" (length (Game.zoneMembers Zone.Library S.bob shuffledHis)) 3
    Spec.assertEqWith s "with alice's library untouched" (Game.zoneMembers Zone.Library S.alice shuffledHis) [herTopId, herDeeperId]
    case (arrivalIn S.bob [hisTopId, hisDeeperId] shuffledHis, arrivalIn S.bob [hisTopId, hisDeeperId] unshuffledHis) of
      ([shuffledArrival], [plainArrival]) -> do
        Spec.assertEqWith
          s
          "CR 701.24a: his library was shuffled too"
          (Game.zoneMembers Zone.Library S.bob shuffledHis)
          [shuffledArrival, hisDeeperId, hisTopId]
        Spec.assertEqWith
          s
          "the control: unshuffled, the same card sits at the BOTTOM where the move put it"
          (Game.zoneMembers Zone.Library S.bob unshuffledHis)
          [hisTopId, hisDeeperId, plainArrival]
      _ -> Spec.assertFailure s "exactly one card should have arrived in bob's library in each run"

  -- CR 608.2b for a TRIGGERED ability, over exile: "a target that's no longer in
  -- the zone it was in when it was targeted is illegal. ... If all its targets,
  -- for every instance of the word 'target,' are now illegal, the spell or
  -- ability doesn't resolve."
  --
  -- The response takes the card out of exile to bob's hand, which is a zone
  -- change like any other, so the trigger's one target is gone. Two things must
  -- then be true: nothing arrives in his library, AND his library is not
  -- shuffled either. The second is the one CR 701.24c could be misread into
  -- breaking -- "that library is shuffled even if none of those objects are in
  -- the zone they're expected to be in" is about an effect that IS resolving,
  -- and this ability never resolves at all, so the clause never comes up. The
  -- reversing interpreter is what makes that assertion say anything: the library
  -- is asserted in its ORIGINAL order, which an unconditional shuffle would have
  -- reversed.
  Spec.it s "CR 608.2b Riftsweeper's trigger fizzles when the card leaves exile in response, shuffling nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    let (_, g1) = S.entersWithTrigger riftsweeper S.alice (Setup.emptyGame S.bothPlayers)
        (hisDeeperId, g2) = S.addLibraryCard bolt S.bob g1
        (hisTopId, g3) = S.addLibraryCard bolt S.bob g2
        (hisId, board) = S.addExiledCard piker S.bob g3
        placed = S.runPure (aimAtCardShuffling hisId) board Engine.placePendingTriggers
        resolve g = S.runPure (aimAtCardShuffling hisId) g Stack.resolveTop
        shuffledIn = resolve placed
        fizzled = resolve (S.runPure S.identityAnswer placed (Event.changeZone hisId Zone.Hand))
    Spec.assertEqWith s "the enters trigger went on the stack" (length (GameState.stack placed)) 1
    Spec.assertEqWith s "untouched, bob's library grew to three" (length (Game.zoneMembers Zone.Library S.bob shuffledIn)) 3
    Spec.assertEqWith s "taken to his hand in response, his library is the two it started with, in their original order -- not even shuffled" (Game.zoneMembers Zone.Library S.bob fizzled) [hisTopId, hisDeeperId]
    Spec.assertEqWith s "with the card itself in his hand" (length (Game.zoneMembers Zone.Hand S.bob fizzled)) 1
    Spec.assertEqWith s "with exile empty when the trigger resolved (the card was shuffled in)" (Set.size (GameState.exile shuffledIn)) 0
    Spec.assertEqWith s "and empty when it fizzled too (the card was taken to hand)" (Set.size (GameState.exile fizzled)) 0
    Spec.assertEqWith s "with the ability off the stack" (length (GameState.stack fizzled)) 0

  -- CR 601.2c: "the player announces their choice of an appropriate object or
  -- player for each target the spell requires." ONE announcement over every
  -- slot, not one slot at a time -- and Dwell on the Past is the first card in
  -- the pool whose slots are not independent. "Target player shuffles up to four
  -- target cards from THEIR graveyard into their library" scopes the card slot's
  -- pool, CR 400.1's per-player graveyard, to whoever the player slot names.
  --
  -- The engine offers the UNION over the player slot's own candidates, which is
  -- what the first assertion pins, and judges the announcement WHOLE
  -- (Target.selectionLegal). That is the rule's "all at once" without inventing
  -- an order between the slots -- and the union is why the second run's card has
  -- to be rejected by the joint check rather than by never having been offered.
  --
  -- THREE SEATS, because two collapse the reading under test: with alice and bob
  -- alone, "bob's graveyard" and "not the caster's graveyard" pick out the same
  -- cards, so a pool that had scoped itself to PlayerScope.Opponents would pass.
  -- carol's card is the one only the slot scoping can exclude.
  --
  -- The three runs differ in EXACTLY ONE thing apiece: which graveyard the
  -- chosen cards sit in, and how many of them. All three name bob in the player
  -- slot and pay the same {G} off the same Forest, off one board.
  --
  -- The two-card run is CR 601.2c's "up to four" reaching the opcode: the slot
  -- is read as an ObjectRef (SlotArity.Many), so a reader that took one would
  -- leave the second card in the graveyard, and CR 701.24's plural is what makes
  -- one shuffle serve both.
  Spec.it s "CR 601.2c Dwell on the Past's card slot is scoped to the player its other slot targets" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    dwell <- S.printingOf s registry "Dwell on the Past"
    let (_, g1) = S.addCreature forest S.alice S.threePlayerGame
        (hisId, g2) = S.addGraveyardCard piker S.bob g1
        (hisOtherId, g3) = S.addGraveyardCard bolt S.bob g2
        (hersId, g4) = S.addGraveyardCard bolt S.carol g3
        (board, dwellId) = S.handOne dwell g4
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace dwell))
        offered = Target.legalSets (Just S.alice) Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
        run oids =
          let cast = S.runPure (aimingDwell oids) board (S.cast S.alice dwellId)
           in (cast, S.runPure (aimingDwell oids) cast Stack.resolveTop)
        (castAtBob, atBob) = run [hisId]
        (_, atBoth) = run [hisId, hisOtherId]
        (castAtCarol, _) = run [hersId]
    Spec.assertEqWith
      s
      "the card slot is offered the UNION over the player slot: every graveyard"
      (slotNamed "cards")
      (Set.fromList (fmap Recipient.ToObject [hisId, hisOtherId, hersId]))
    Spec.assertEqWith s "and the player slot every player" (slotNamed "player") (Set.fromList (fmap Recipient.ToPlayer [S.alice, S.bob, S.carol]))
    Spec.assertEqWith s "naming bob and a card in BOB's graveyard, the spell is cast" (length (GameState.stack castAtBob)) 1
    Spec.assertBool s (notElem hisId (Game.zoneMembers Zone.Graveyard S.bob atBob)) "and on resolution his card leaves his graveyard"
    Spec.assertEqWith s "arriving in HIS library (CR 400.3), which was empty" (length (Game.zoneMembers Zone.Library S.bob atBob)) 1
    Spec.assertEqWith s "leaving his other card behind, unnamed" (Game.zoneMembers Zone.Graveyard S.bob atBob) [hisOtherId]
    Spec.assertEqWith s "with carol's graveyard untouched" (Game.zoneMembers Zone.Graveyard S.carol atBob) [hersId]
    Spec.assertEqWith s "naming TWO of his cards, both leave the graveyard" (Game.zoneMembers Zone.Graveyard S.bob atBoth) []
    Spec.assertEqWith s "and both arrive in his library" (length (Game.zoneMembers Zone.Library S.bob atBoth)) 2
    Spec.assertEqWith s "naming bob and a card in CAROL's graveyard, the cast is reversed (CR 601.2)" (length (GameState.stack castAtCarol)) 0
    Spec.assertBool s (elem dwellId (Game.zoneMembers Zone.Hand S.alice castAtCarol)) "and the spell is back in alice's hand"

  -- CR 601.2c: "If the spell has a variable number of targets, the player
  -- announces how many targets they will choose before they announce those
  -- targets." How many they WILL choose -- so a number they could not then choose
  -- legally is not one to offer. Dwell on the Past's card slot is offered the
  -- union over the player slot's candidates (the case above pins that offer), and
  -- CR 400.1 gives each player their own graveyard, so no ONE answer can reach
  -- two graveyards at once.
  --
  -- Target.slotCapacities is the narrowing: the ceiling for a graveyard-scoped
  -- slot is the largest per-player total, not the union's size. Without it a
  -- caster announces two, names the only two cards there are, and CR 601.2e
  -- returns the game to before the spell was proposed.
  --
  -- THREE SEATS, and the two cards are split ONE APIECE between bob and carol,
  -- which is the whole discriminator: the union holds two and every graveyard
  -- holds one.
  --
  -- The answerer OBEYS the count the engine came back with rather than naming
  -- both cards outright. An answerer that named both would fail selectionLegal's
  -- count check under the fix and its joint check without it -- a reversal either
  -- way, and the case would pass whatever the engine offered.
  Spec.it s "CR 601.2c Dwell on the Past cannot be aimed at more cards than one graveyard holds" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    dwell <- S.printingOf s registry "Dwell on the Past"
    let (hisId, g1) = S.addGraveyardCard piker S.bob (S.landsFor forest S.alice 2 S.threePlayerGame)
        (hersId, g2) = S.addGraveyardCard bolt S.carol g1
        (board, dwellId) = S.handOne dwell g2
        cast = S.runPure (aimingDwellObeying [hisId, hersId]) board (S.cast S.alice dwellId)
        resolved = S.runPure (aimingDwellObeying [hisId, hersId]) cast Stack.resolveTop
    Spec.assertEqWith s "asking for two, alice gets the one bob's graveyard holds: the Piker is in his library" (namesIn Zone.Library S.bob resolved) [S.nameOf (Printing.card piker)]
    Spec.assertEqWith s "and out of his graveyard" (namesIn Zone.Graveyard S.bob resolved) []
    Spec.assertEqWith s "carol's Bolt is untouched, never reachable beside bob" (namesIn Zone.Graveyard S.carol resolved) [S.nameOf (Printing.card bolt)]
    Spec.assertEqWith s "and her library is empty" (namesIn Zone.Library S.carol resolved) []
    Spec.assertEqWith s "the spell was cast rather than reversed (CR 601.2e)" (length (GameState.stack cast)) 1
    Spec.assertBool s (notElem dwellId (Game.zoneMembers Zone.Hand S.alice cast)) "so it is not back in alice's hand"

  -- The same narrowing on the FLOOR rather than the ceiling, which is CR 601.2c's
  -- other half: "In some cases, the number of targets will be defined by the
  -- spell's text." A slot whose count is fixed at two has no announcement to
  -- narrow -- the whole spell is illegal to cast when no coherent answer exists,
  -- and CR 601.2e's reversal is a worse prompt than never offering the cast.
  --
  -- Synthetic Exhume the Archive {1}{G} Sorcery (data/cards/synthetic-exhume-the-archive.json):
  -- "Target player shuffles two target cards from their graveyard into their
  -- library." SYNTHETIC because every printing whose target slot draws from
  -- another slot's graveyard prints a minimum of zero -- Scryfall
  -- o:"cards from their graveyard" -o:"up to" -o:"any number", 2026-08-31, no
  -- hit with a targeted card slot; Dwell on the Past, Gaea's Blessing, Krosan
  -- Reclamation, Memory's Journey, Quandrix Command, Rite of Renewal, Stream of
  -- Consciousness and Witness the Future all say "up to", and Loaming Shaman says
  -- "any number". Nothing in the CR forbids the card: rule 601.2c's own "the
  -- number of targets will be defined by the spell's text" is exactly this shape.
  --
  -- TWO BOARDS differing in exactly one thing -- which graveyard the Bolt sits in
  -- -- with the same two Forests paying the same {1}{G} and the Piker in bob's
  -- graveyard both times.
  Spec.it s "CR 601.2c a spell demanding two cards from one graveyard is uncastable when no one graveyard holds two" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    exhume <- S.printingOf s registry "Synthetic Exhume the Archive"
    let (_, g1) = S.addGraveyardCard piker S.bob (S.landsFor forest S.alice 2 S.threePlayerGame)
        boardWith pid = S.handOne exhume (snd (S.addGraveyardCard bolt pid g1))
        (together, togetherId) = boardWith S.bob
        (split, splitId) = boardWith S.carol
    Spec.assertBool s (S.castable S.alice togetherId together) "with both cards in bob's graveyard the spell has a coherent announcement"
    Spec.assertBool s (not (S.castable S.alice splitId split)) "split one apiece it has none, though the union still holds two"

  -- CR 700.2d: "If a particular mode is chosen multiple times, the spell is
  -- treated as if that mode appeared that many times in sequence. If that mode
  -- requires a target, the same player or object may be chosen as the target for
  -- each of those modes, or different targets may be chosen." Different targets
  -- is what Pawl.Engine.Modal.instanceSlot's per-occurrence rename buys, and a
  -- slot's POOL may name a sibling slot by its printed name -- so occurrence 1's
  -- graveyard scope has to follow the rename or it reads OCCURRENCE 0's player.
  --
  -- Synthetic Recurring Reclamation {2}{G} Sorcery (data/cards/synthetic-recurring-reclamation.json):
  -- "Choose two. You may choose the same mode more than once. -- Target player
  -- shuffles up to two target cards from their graveyard into their library. --
  -- Draw a card." SYNTHETIC because the two printed sets do not intersect:
  -- Scryfall o:"choose the same mode more than once", 2026-08-31, returns 22
  -- cards, and no mode of any of them targets a card in another slot's graveyard
  -- (the graveyard modes of the Confluence and Season cycles say "your
  -- graveyard", and Unite the Coalition's "exile target player's graveyard" is a
  -- resolution-time sweep, which reads printed names off Modal.instanceView and
  -- never this path). CR 700.2d states the shape outright.
  --
  -- THE WRONG ANSWER IS WEAKER THAN PRINTED, not stricter, which is what makes
  -- the case worth a board: an unrenamed scope judges occurrence 1's card against
  -- whatever OCCURRENCE 0 named, so naming bob for occurrence 0 and carol for
  -- occurrence 1 admits a card out of BOB's graveyard for her occurrence.
  --
  -- The DESTINATION cannot show it. CR 400.3 sends an object that would go to a
  -- library other than its owner's to its owner's instead, so the misjudged card
  -- lands in bob's library whichever player occurrence 1 named -- what
  -- discriminates is that it MOVES AT ALL, the whole announcement being reversed
  -- when the rename holds.
  --
  -- TWO RUNS off one board, differing in exactly one thing -- which player
  -- occurrence 1 names -- with the same three Forests paying the same {2}{G},
  -- the same two cards in bob's graveyard and the same modes chosen. The bob run
  -- is what keeps the carol run from passing off a spell that never worked.
  --
  -- THREE SEATS: with alice and bob alone the two occurrences would name one
  -- player and the rename could not be observed.
  Spec.it s "CR 700.2d a repeated mode's graveyard scope follows its own occurrence, not the first" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    reclamation <- S.printingOf s registry "Synthetic Recurring Reclamation"
    let (hisId, g1) = S.addGraveyardCard piker S.bob (S.landsFor forest S.alice 3 S.threePlayerGame)
        (hisOtherId, g2) = S.addGraveyardCard bolt S.bob g1
        (board, spellId) = S.handOne reclamation g2
        run other =
          let cast = S.runPure (aimingReclamation other hisId hisOtherId) board (S.cast S.alice spellId)
           in (cast, S.runPure (aimingReclamation other hisId hisOtherId) cast Stack.resolveTop)
        (castAtCarol, atCarol) = run S.carol
        (_, atBob) = run S.bob
    Spec.assertEqWith s "occurrence 1 naming carol, both of bob's cards stay in his graveyard: the announcement is reversed (CR 601.2e)" (namesIn Zone.Graveyard S.bob atCarol) [S.nameOf (Printing.card piker), S.nameOf (Printing.card bolt)]
    Spec.assertEqWith s "so nothing reaches his library either" (namesIn Zone.Library S.bob atCarol) []
    Spec.assertEqWith s "with nothing on the stack" (length (GameState.stack castAtCarol)) 0
    Spec.assertBool s (elem spellId (Game.zoneMembers Zone.Hand S.alice castAtCarol)) "and the spell back in alice's hand"
    Spec.assertEqWith s "occurrence 1 naming bob instead, both cards reach his library" (namesIn Zone.Library S.bob atBob) [S.nameOf (Printing.card piker), S.nameOf (Printing.card bolt)]
    Spec.assertEqWith s "leaving his graveyard empty" (namesIn Zone.Graveyard S.bob atBob) []

  -- CR 700.2d one object type over: the case above is a SPELL, and an activated
  -- ability re-checks CR 608.2b's targets down a second path of its own
  -- (Pawl.Engine.Resolve.resolveModesWith), which built that map without the
  -- per-occurrence rename the announcement had made; see #2806. The announcement is
  -- not what is under test -- Pawl.Engine.Activate goes through
  -- Modal.modesTargetSlots and always renamed -- so the ability reaches the stack
  -- either way, and what CR 608.2b decides is whether occurrence 1's card is
  -- still legal. Judged against OCCURRENCE 0's player it is a card in the wrong
  -- graveyard, dropped with no error.
  --
  -- Synthetic Reclamation Engine {3} Artifact
  -- (data/cards/synthetic-reclamation-engine.json): "{T}: Choose two. You may
  -- choose the same mode more than once. -- Target player shuffles up to two
  -- target cards from their graveyard into their library. -- Draw a card."
  -- SYNTHETIC because no printing puts that instruction on an ability at all:
  -- Scryfall o:"choose the same mode more than once", 2026-08-31, returns 22
  -- cards and every one of them is an instant or a sorcery. CR 700.2a states the
  -- shape outright -- the controller of a modal activated ability chooses the
  -- modes as part of activating it -- and CR 700.2d's own sentence is about "a
  -- modal spell or ability", so nothing in the rules forbids the printing.
  --
  -- THE WRONG ANSWER IS STRICTER THAN PRINTED here, where the spell case's was
  -- weaker: the misjudged card is dropped rather than admitted, so what
  -- discriminates is that carol's card MOVES.
  --
  -- THREE SEATS, the case above's reason: with alice and bob alone the two
  -- occurrences would name one player and the rename could not be observed.
  Spec.it s "CR 608.2b a repeated mode on an ABILITY re-checks each occurrence against its own binding" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    engine <- S.printingOf s registry "Synthetic Reclamation Engine"
    case soleActivatedAbility engine of
      Nothing -> Spec.assertFailure s "Synthetic Reclamation Engine should declare exactly one activated ability"
      Just ability -> do
        let (engineId, g1) = S.addCreature engine S.alice S.threePlayerGame
            (hisId, g2) = S.addGraveyardCard piker S.bob g1
            (hersId, board) = S.addGraveyardCard bolt S.carol g2
            activated = S.runPure (aimingReclamation S.carol hisId hersId) board (Activate.activateAbility S.alice engineId ability)
            resolved = S.runPure (aimingReclamation S.carol hisId hersId) activated Stack.resolveTop
        Spec.assertEqWith s "occurrence 1's card reaches CAROL's library, the player her own occurrence named" (namesIn Zone.Library S.carol resolved) [S.nameOf (Printing.card bolt)]
        Spec.assertEqWith s "leaving her graveyard empty" (namesIn Zone.Graveyard S.carol resolved) []
        Spec.assertEqWith s "and occurrence 0's card reaches bob's" (namesIn Zone.Library S.bob resolved) [S.nameOf (Printing.card piker)]
        -- The proxies last: the announcement is not what this case is about, and
        -- an ability that never reached the stack would leave every graveyard
        -- alone too.
        Spec.assertEqWith s "the activation was accepted, so the ability really did resolve" (length (GameState.stack activated)) 1
        Spec.assertEqWith s "and nothing is left on the stack after it" (length (GameState.stack resolved)) 0

  -- CR 601.2c's other sibling-slot reading, and the one the rule states in its
  -- own words: "The same target can't be chosen multiple times for any one
  -- instance of the word 'target' on the spell. However, if the spell uses the
  -- word 'target' in multiple places, the same object or player can be chosen
  -- once for each instance of the word 'target'." Sharing between two slots is
  -- the rule's DEFAULT, so a card that forbids it says "another", and the
  -- restriction lives in that slot's own Filter rather than in the machinery.
  --
  -- Fall of the Hammer {1}{R} Instant (data/cards/fall-of-the-hammer.json):
  -- "Target creature you control deals damage equal to its power to another
  -- target creature." The victim slot is Filter.Not (Filter.IsBound "dealer"),
  -- which reads what the dealer slot holds -- the first card in the pool whose
  -- slots depend on each other through a FILTER rather than through a pool
  -- (Dwell on the Past above is the pool reading).
  --
  -- Rabid Bite is the same card one word apart: same two Pool.Creatures slots,
  -- same DealDamage off AgainstSlot/Power, and "target creature you don't
  -- control" where this prints "another target creature". So the Wall is the
  -- discriminator that matters: it is ALICE's, and naming it is legal here,
  -- which "another" as a controller test would reject.
  --
  -- The three runs differ in exactly one thing apiece -- which creature fills
  -- the victim slot -- off one board, with the same {1}{R} paid off the same two
  -- Mountains, and the Piker is the dealer in all three.
  --
  -- Damage is read BEFORE state-based actions, so the 2/1 Piker naming itself
  -- would still be readable had the announcement gone through; a self-damage
  -- reading is not hidden behind CR 704.5g.
  Spec.it s "CR 601.2c Fall of the Hammer's victim slot cannot be the creature its dealer slot names" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    wall <- S.printingOf s registry "Wall of Stone"
    rats <- S.printingOf s registry "Typhoid Rats"
    hammer <- S.printingOf s registry "Fall of the Hammer"
    let (dealerId, g1) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
        (wallId, g2) = S.addCreature wall S.alice g1
        (ratsId, g3) = S.addCreature rats S.bob g2
        (board, hammerId) = S.handOne hammer g3
        run victimId =
          let cast = S.runPure (aimingHammer dealerId victimId) board (S.cast S.alice hammerId)
           in (cast, S.runPure (aimingHammer dealerId victimId) cast Stack.resolveTop)
        (castAtSelf, atSelf) = run dealerId
        (_, atRats) = run ratsId
        (_, atWall) = run wallId
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace hammer))
        offered = Target.legalSets (Just S.alice) Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    -- The behaviour first: naming the Piker in both slots is not an announcement
    -- the rule allows, so CR 601.2e returns the game to before the proposal.
    Spec.assertEqWith s "naming the Piker in BOTH slots, the cast is reversed (CR 601.2e)" (length (GameState.stack castAtSelf)) 0
    Spec.assertEqWith s "so the Piker was never dealt its own two damage" (S.damageOf dealerId atSelf) (Just 0)
    Spec.assertBool s (elem hammerId (Game.zoneMembers Zone.Hand S.alice castAtSelf)) "and the spell is back in alice's hand"
    Spec.assertEqWith s "naming bob's Rats instead, the Piker's two damage is marked on them" (S.damageOf ratsId atRats) (Just 2)
    Spec.assertEqWith s "with the Piker itself unharmed" (S.damageOf dealerId atRats) (Just 0)
    -- CR 601.2c's "another" is about the OBJECT, not its controller: alice's own
    -- Wall is a legal victim, which is the whole difference from Rabid Bite.
    Spec.assertEqWith s "naming alice's own Wall, the same two damage is marked on it" (S.damageOf wallId atWall) (Just 2)
    -- The union posture, last: the victim slot is still OFFERED the Piker at CR
    -- 601.2c, which is what makes the first assertion a joint-check rejection
    -- rather than a slot the fix emptied.
    Spec.assertEqWith
      s
      "the victim slot is offered every creature, the dealer's own candidate included"
      (slotNamed "victim")
      (Set.fromList (fmap Recipient.ToCreature [dealerId, wallId, ratsId]))
    Spec.assertEqWith s "and the dealer slot only alice's two" (slotNamed "dealer") (Set.fromList (fmap Recipient.ToCreature [dealerId, wallId]))

  -- CR 601.2c through CR 700.2a: the case above's card, asked one step earlier.
  -- A mode is fillable when SOME announcement fills every one of its slots, not
  -- when each slot can be filled on its own -- so Fall of the Hammer off a board
  -- holding exactly one creature is a spell with no legal announcement and is
  -- never offered, rather than a cast proposed and reversed at CR 601.2e.
  --
  -- Reversal and unofferability are indistinguishable on the board afterwards --
  -- both leave the spell in hand and the stack empty -- so the observable is
  -- Action.legalActions, which is where CR 601.2e's cost lands.
  --
  -- TWO BOARDS differing in exactly ONE thing: whether bob has a creature. The
  -- same two Mountains are untapped on both, so the refusal is not the mana, and
  -- the same Piker is alice's only creature on both, so the dealer slot is
  -- identical and the victim slot is the whole difference.
  Spec.it s "CR 700.2a Fall of the Hammer is unfillable on a board holding one creature" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    rats <- S.printingOf s registry "Typhoid Rats"
    hammer <- S.printingOf s registry "Fall of the Hammer"
    let (dealerId, g1) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
        (alone, hammerId) = S.handOne hammer g1
        (_, together) = S.addCreature rats S.bob alone
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace hammer))
        offered = Target.legalSets (Just S.alice) Map.empty S.noSource slots alone
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    Spec.assertBool s (not (any (S.isCastOf hammerId) (Action.legalActions S.alice alone))) "with only the Piker on the board, the cast is not offered at all"
    Spec.assertBool s (any (S.isCastOf hammerId) (Action.legalActions S.alice together)) "and with bob's Rats beside it, the same spell off the same mana is offered"
    -- The union posture, last and for the case above's reason: each slot still
    -- has a candidate ON ITS OWN, so the refusal is the cross-slot search and not
    -- a slot the change emptied.
    Spec.assertEqWith s "the victim slot is offered the Piker by itself" (slotNamed "victim") (Set.singleton (Recipient.ToCreature dealerId))
    Spec.assertEqWith s "and so is the dealer slot" (slotNamed "dealer") (Set.singleton (Recipient.ToCreature dealerId))

  -- The same question on the ACTIVATION road, which CR 602.2b routes through CR
  -- 601.2b-i and CR 700.2a gates the same way it gates a spell's. Fall of the
  -- Hammer above reaches `fillableModesGiven` through Pawl.Engine.Cast; this
  -- reaches it through Pawl.Engine.Activate.activatableGiven, so the cross-slot
  -- search is proved on both.
  --
  -- Resourceful Defense {2}{W} Enchantment (data/cards/resourceful-defense.json):
  -- "{4}{W}: Move any number of counters from target permanent you control onto
  -- a second target permanent you control." Its `to` slot is
  -- And [ControlledBy You, Not (IsBound "from")], so a controller whose only
  -- permanent is the Defense itself fills each slot alone and no announcement at
  -- all, and CR 700.2a is what keeps the ability off the offer.
  --
  -- FIVE WHITE MANA FLOATING rather than five Plains, and that is the whole
  -- reason this board is built by hand: a land is a permanent its controller
  -- controls, so the mana to activate would fill the second slot by itself and
  -- there would be no unfillable board to build. Both boards carry the same
  -- floating five, so neither answer is about affordability.
  --
  -- TWO BOARDS differing in exactly ONE thing: whether alice also controls a
  -- Piker.
  Spec.it s "CR 700.2a Resourceful Defense's ability is unfillable when it is its controller's only permanent" $ do
    defense <- S.printingOf s registry "Resourceful Defense"
    piker <- S.printingOf s registry "Goblin Piker"
    let white =
          ManaUnit.MkManaUnit
            { ManaUnit.manaType = ManaType.Colored Color.White,
              ManaUnit.tags = Set.empty,
              ManaUnit.retention = ManaRetention.Ordinary,
              ManaUnit.restriction = Nothing,
              ManaUnit.rider = Nothing
            }
        funded = (Setup.emptyGame S.bothPlayers) {GameState.manaPool = Map.singleton S.alice (Mana.Type.MkMana (replicate 5 white)), GameState.priority = Just S.alice}
        (defenseId, alone) = S.addCreature defense S.alice funded
        (_, together) = S.addCreature piker S.alice alone
        activates gs = any (\action -> case action of A.Activate oid _ -> oid == defenseId; _ -> False) (Action.legalActions S.alice gs)
    Spec.assertBool s (not (activates alone)) "with the Defense alone on the battlefield, its ability is not offered at all"
    Spec.assertBool s (activates together) "and with a Piker beside it, the same ability off the same floating five is offered"

  -- The three cases above put CR 601.2c's "another" on a card chosen once. CR
  -- 700.2d puts it on a card whose mode may be chosen twice, and then the
  -- filter's slot NAME has to follow the occurrence exactly as the key and the
  -- pool do -- read under its printed name from occurrence 1 it names occurrence
  -- 0's dealer, and the creature occurrence 1 itself named becomes a legal victim
  -- of its own damage. Weaker than printed, in the caster's favour.
  --
  -- Synthetic Hammer Refrain {1}{R} Instant
  -- (data/cards/synthetic-hammer-refrain.json): "Choose two. You may choose the
  -- same mode more than once. -- Target creature you control deals damage equal
  -- to its power to another target creature. -- Draw a card." SYNTHETIC because
  -- the two printed sets do not intersect: Scryfall o:"choose the same mode more
  -- than once", 2026-08-31, returns 22 cards, and no mode of any of them prints
  -- two targets with one restricting the other. Its damage mode is Fall of the
  -- Hammer's above, one instruction added.
  --
  -- TWO RUNS off one board, differing in exactly one thing -- which creature
  -- fills occurrence 1's victim slot -- with the same {1}{R} paid off the same
  -- two Mountains and the same modes chosen. The legal run is what keeps the
  -- rejected one from passing off a spell that never worked.
  --
  -- TWO PIKERS rather than two printings: occurrence 0's dealer and occurrence
  -- 1's are then indistinguishable except by slot, so a filter that admits the
  -- second because it compared against the first is the only reading that
  -- separates the runs.
  Spec.it s "CR 700.2d a repeated mode's filter reads its own occurrence's sibling slot, not the first's" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    wall <- S.printingOf s registry "Wall of Stone"
    refrain <- S.printingOf s registry "Synthetic Hammer Refrain"
    let (firstDealerId, g1) = S.addCreature piker S.alice (S.landsInPlay mountain 2)
        (secondDealerId, g2) = S.addCreature piker S.alice g1
        (wallId, g3) = S.addCreature wall S.bob g2
        (board, refrainId) = S.handOne refrain g3
        run victimTwo =
          let cast = S.runPure (aimingRefrain firstDealerId wallId secondDealerId victimTwo) board (S.cast S.alice refrainId)
           in (cast, S.runPure (aimingRefrain firstDealerId wallId secondDealerId victimTwo) cast Stack.resolveTop)
        (castAtSelf, atSelf) = run secondDealerId
        (_, atWall) = run wallId
        slots = Modal.modesTargetSlots (Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 0]) (Face.spell (S.combinedFace refrain))
        offered = Target.legalSets (Just S.alice) Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    -- The behaviour first: occurrence 1 naming its own dealer is not an
    -- announcement the rule allows, so CR 601.2e returns the game to before the
    -- proposal and occurrence 0's damage never happens either.
    Spec.assertEqWith s "occurrence 1 naming the creature its OWN dealer slot holds, bob's Wall is unharmed: the cast is reversed (CR 601.2e)" (S.damageOf wallId atSelf) (Just 0)
    Spec.assertEqWith s "and the creature named twice took none of its own two damage" (S.damageOf secondDealerId atSelf) (Just 0)
    Spec.assertEqWith s "naming the Wall for occurrence 1 instead, both Pikers' two damage reaches it" (S.damageOf wallId atWall) (Just 4)
    -- The proxies after: a cast that never happened would leave the Wall
    -- unharmed too.
    Spec.assertEqWith s "the reversed cast left nothing on the stack" (length (GameState.stack castAtSelf)) 0
    Spec.assertBool s (elem refrainId (Game.zoneMembers Zone.Hand S.alice castAtSelf)) "and the spell is back in alice's hand"
    -- The union posture, last and for the case above's reason: occurrence 1's
    -- victim slot is still OFFERED its own dealer at CR 601.2c, so the first
    -- assertion is a joint-check rejection rather than a slot the rename emptied.
    Spec.assertEqWith
      s
      "occurrence 1's victim slot is offered every creature, its own dealer's candidate included"
      (slotNamed "victim#1")
      (Set.fromList (fmap Recipient.ToCreature [firstDealerId, secondDealerId, wallId]))
    Spec.assertEqWith s "and its dealer slot only alice's two" (slotNamed "dealer#1") (Set.fromList (fmap Recipient.ToCreature [firstDealerId, secondDealerId]))

  -- CR 601.2c's sibling-slot reading in its POSITIVE form, where the case above is
  -- the negative one: "another" excludes what a sibling slot holds, and "with the
  -- same controller" demands something of it -- CR 110.2's controller, which every
  -- permanent has.
  --
  -- Bioshift {G/U} Instant (Gatecrash; name, cost, type line and oracle text
  -- checked against Scryfall 2026-08-31), data/cards/bioshift.json:
  --
  --   Move any number of +1/+1 counters from target creature onto another target
  --   creature with the same controller.
  --
  -- Its `to` slot is And [Not (IsBound "from"), SameControllerAsBound "from"], and
  -- the second atom is the one this case exists for: written without it the card
  -- would be WEAKER than printed in the caster's favour, letting counters cross
  -- between two players' creatures.
  --
  -- THREE Walls of Stone, two alice's and one bob's, so the two boards below
  -- differ in exactly one thing -- which creature fills the `to` slot -- with the
  -- same one hybrid mana paid off the same two lands. One printing three times
  -- over, so nothing but the CONTROLLER can separate the candidates: a filter
  -- reading any characteristic would admit or refuse all three alike.
  Spec.it s "CR 601.2c Bioshift's second slot cannot be a creature its first slot's controller does not control" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    wall <- S.printingOf s registry "Wall of Stone"
    bioshift <- S.printingOf s registry "Bioshift"
    let lands = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers))
        (giverId, g1) = S.addCreature wall S.alice lands
        (mineId, g2) = S.addCreature wall S.alice g1
        (theirsId, g3) = S.addCreature wall S.bob g2
        (board, spellId) = S.handOne bioshift (S.addCounter CounterKind.PlusOnePlusOne 3 giverId g3)
        run takerId =
          let cast = S.runPure (aimingBioshift giverId takerId) board (S.cast S.alice spellId)
           in (cast, S.runPure (aimingBioshift giverId takerId) cast Stack.resolveTop)
        (_, ontoMine) = run mineId
        (castAtTheirs, ontoTheirs) = run theirsId
        counters = S.counterOf CounterKind.PlusOnePlusOne
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace bioshift))
        offered = Target.legalSets (Just S.alice) Map.empty S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
    Spec.assertEqWith s "alice's first Wall bears the three counters and nothing else does" (fmap (`counters` board) [giverId, mineId, theirsId]) [3, 0, 0]
    -- THE GAMEPLAY-LEVEL ASSERTIONS, ahead of the reversal's: the counters cross
    -- between alice's two Walls and do not cross to bob's.
    Spec.assertEqWith s "naming alice's other Wall, all three counters cross to it" (fmap (`counters` ontoMine) [giverId, mineId]) [0, 3]
    Spec.assertEqWith s "naming bob's Wall, it receives none" (counters theirsId ontoTheirs) 0
    Spec.assertEqWith s "and alice's first Wall still bears all three" (counters giverId ontoTheirs) 3
    -- CR 601.2e, behind the behaviour: the announcement was not one the rule
    -- allows, so the game returned to before the spell was proposed.
    Spec.assertEqWith s "the cast is reversed" (length (GameState.stack castAtTheirs)) 0
    Spec.assertBool s (elem spellId (Game.zoneMembers Zone.Hand S.alice castAtTheirs)) "and the spell is back in alice's hand"
    -- The union posture, last, and the trap this atom had to avoid: the offer is
    -- made before either target is chosen, so the `to` slot is still offered every
    -- creature -- bob's included. A narrowing offer would empty the slot and make
    -- the spell uncastable rather than restricted.
    Spec.assertEqWith
      s
      "the second slot is offered every creature, bob's own candidate included"
      (slotNamed "to")
      (Set.fromList (fmap Recipient.ToCreature [giverId, mineId, theirsId]))

  -- CR 702.164a: "Toxic is a static ability. It is written 'toxic N,' where N is
  -- a number." Flensing Raptor's enters trigger reads "another target creature
  -- you control with toxic", which names the ABILITY rather than one written
  -- instance -- Filter.HasKeywordFamily, where every other keyword narrowing in
  -- the pool is Filter.HasKeyword. So a single filter has to reach both the
  -- toxic 1 Raptor beside it and the toxic 2 Branchblight Stalker (#522).
  --
  -- TWO Ns is the whole point of the board: a HasKeyword-shaped implementation
  -- could match the entering Raptor's own toxic 1 and would still fail on the
  -- Stalker, so one toxic creature would not discriminate.
  --
  -- The Piker is the control, as in the rule-702 cases above -- without it,
  -- "the creature without toxic is excluded" could pass on an empty legal set.
  -- Bob's Stalker separates the family question from the CR 109.5 controller one,
  -- and the entering Raptor itself pins CR 601.2c's "another" (Not IsSource).
  Spec.it s "CR 702.164a Flensing Raptor's trigger reaches toxic 1 and toxic 2 alike, and nothing else" $ do
    raptor <- S.printingOf s registry "Flensing Raptor"
    stalker <- S.printingOf s registry "Branchblight Stalker"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (otherRaptorId, gs1) = S.addCreature raptor S.alice gs0
        (stalkerId, gs2) = S.addCreature stalker S.alice gs1
        (pikerId, gs3) = S.addCreature piker S.alice gs2
        (hisStalkerId, gs4) = S.addCreature stalker S.bob gs3
        (enteringId, board) = S.entersWithTrigger raptor S.alice gs4
    case triggerTargetSlot raptor of
      Nothing -> Spec.assertFailure s "Flensing Raptor's trigger should declare one target slot"
      Just theSlot -> do
        let legal = Target.legalRecipients (Just S.alice) enteringId theSlot board
        Spec.assertBool s (Set.member (Recipient.ToCreature otherRaptorId) legal) "the Raptor beside it has toxic 1, and is legal"
        Spec.assertBool s (Set.member (Recipient.ToCreature stalkerId) legal) "Branchblight Stalker has toxic 2, and is legal too"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature pikerId) legal)) "Goblin Piker has no toxic at all"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature enteringId) legal)) "CR 601.2c: not the entering Raptor itself"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature hisStalkerId) legal)) "CR 109.5: not bob's toxic creature"

  -- CR 701.24c's FIRST half: "that library is shuffled even if none of those
  -- objects are in the zone they're expected to be in". Dwell on the Past is the
  -- pool's first card that can reach it, because CR 608.2b only fizzles a spell
  -- whose targets are ALL illegal -- its player slot stays legal when both
  -- targeted cards are exiled in response, so the spell resolves with nothing
  -- left to shuffle in and bob's library must be shuffled anyway (#558).
  --
  -- A PAIR OF RUNS off one board differing in exactly one thing: whether the two
  -- cards were exiled between the cast and the resolution. The control run is
  -- what says the shuffle in the other one is not merely the arrival's doing --
  -- it shows the graveyard losing exactly those two cards and the library
  -- gaining exactly two.
  --
  -- ORDER is how a shuffle is observed at all (CR 701.24a makes it unobservable
  -- otherwise), under the interpreter that REVERSES every Prompt.Shuffle. What
  -- ORDER the shuffle leaves is deliberately not asserted anywhere: a real
  -- shuffle has no assertable one. The seeded libraries are two cards each, so a
  -- reversal is visible.
  --
  -- THREE SEATS, and three seeded libraries. alice casts, bob is targeted, carol
  -- is neither -- so "the library the spell NAMES" is told apart both from the
  -- controller's (alice's, untouched) and from everyone's (carol's, untouched).
  -- carol also holds a graveyard card of the same printing as one of bob's,
  -- which is what makes "their graveyard" observable rather than "any".
  Spec.it s "CR 701.24c Dwell on the Past shuffles the targeted player's library even when both targeted cards have left the graveyard" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    dwell <- S.printingOf s registry "Dwell on the Past"
    -- S.addLibraryCard puts each card ON TOP, so the second of each pair heads
    -- the library and the first sits under it.
    let (_, g1) = S.addCreature forest S.alice S.threePlayerGame
        (hisId, g2) = S.addGraveyardCard piker S.bob g1
        (hisOtherId, g3) = S.addGraveyardCard bolt S.bob g2
        (hersId, g4) = S.addGraveyardCard piker S.carol g3
        (herDeeperId, g5) = S.addLibraryCard bolt S.alice g4
        (herTopId, g6) = S.addLibraryCard piker S.alice g5
        (hisDeeperId, g7) = S.addLibraryCard bolt S.bob g6
        (hisTopId, g8) = S.addLibraryCard piker S.bob g7
        (carolDeeperId, g9) = S.addLibraryCard piker S.carol g8
        (carolTopId, g10) = S.addLibraryCard bolt S.carol g9
        (board, dwellId) = S.handOne dwell g10
        cast = S.runPure (aimingDwellShuffling [hisId, hisOtherId]) board (S.cast S.alice dwellId)
        exiled =
          S.runPure S.identityAnswer (S.runPure S.identityAnswer cast (Event.changeZone hisId Zone.Exile)) $
            Event.changeZone hisOtherId Zone.Exile
        resolvedWithCards = S.runPure (aimingDwellShuffling [hisId, hisOtherId]) cast Stack.resolveTop
        resolvedWithout = S.runPure (aimingDwellShuffling [hisId, hisOtherId]) exiled Stack.resolveTop
        arrivalsIn pid seeded gs = filter (`notElem` seeded) (Game.zoneMembers Zone.Library pid gs)
    Spec.assertEqWith s "the spell is on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "the control: his graveyard loses exactly the two named cards" (Game.zoneMembers Zone.Graveyard S.bob resolvedWithCards) []
    Spec.assertEqWith s "and his library gains exactly two" (length (arrivalsIn S.bob [hisTopId, hisDeeperId] resolvedWithCards)) 2
    Spec.assertEqWith s "with carol's graveyard card untouched, so it was HIS graveyard the cards came from" (Game.zoneMembers Zone.Graveyard S.carol resolvedWithCards) [hersId]
    Spec.assertEqWith s "exiled in response, both cards are gone from his graveyard before the spell resolves" (Game.zoneMembers Zone.Graveyard S.bob exiled) []
    Spec.assertEqWith s "so nothing arrives in his library" (length (arrivalsIn S.bob [hisTopId, hisDeeperId] resolvedWithout)) 0
    Spec.assertEqWith
      s
      "CR 701.24c: his library is shuffled all the same -- the reversal shows through"
      (Game.zoneMembers Zone.Library S.bob resolvedWithout)
      [hisDeeperId, hisTopId]
    Spec.assertEqWith s "alice's library is not shuffled, so the library is the one the spell NAMED and not the controller's" (Game.zoneMembers Zone.Library S.alice resolvedWithout) [herTopId, herDeeperId]
    Spec.assertEqWith s "and carol's is not either, so it is one library and not every library" (Game.zoneMembers Zone.Library S.carol resolvedWithout) [carolTopId, carolDeeperId]
    Spec.assertEqWith s "with the spell off the stack" (length (GameState.stack resolvedWithout)) 0

  -- CR 725.5: "If the result of a continuous effect generated by a static
  -- ability is determined based on who is currently the monarch, but there is no
  -- monarch in the game as that effect begins to apply, that effect does nothing
  -- until a player becomes the monarch."
  --
  -- Dawnglade Regent ({5}{G}{G} Creature -- Elk 8/8) is the pool's first card
  -- that can reach that path without any exotic board state, because its two
  -- abilities are of different kinds and start at different moments: "When this
  -- creature enters, you become the monarch" is a TRIGGER that has to resolve,
  -- while "As long as you're the monarch, permanents you control have hexproof"
  -- is a CR 604.2 static that is live the instant the Regent is on the
  -- battlefield. Between the two sits a real window in which the static is
  -- evaluated with no monarch at all -- and CR 725.5 says it does nothing there.
  --
  -- Palace Jailer, the pool's other monarch card, cannot reach it: it makes a
  -- monarch before anything of its own asks who the monarch is.
  --
  -- THE WINDOW IS THE CASE, not the steady state. An implementation that read
  -- "is there a monarch at all", or one that latched the static's answer when the
  -- permanent entered, or one that simply never gated the ability, all pass a
  -- test that only looks after the trigger resolved.
  --
  -- bob's Doom Blade ("Destroy target nonblack creature" -- an 8/8 green Elk is
  -- nonblack, so its Filter admits the Regent and only the restriction can remove
  -- it) is the probe, and the board answers every way it could be wrong at once:
  -- alice's Piker separates "permanents you control" from an IsSource-shaped
  -- affected set, bob's Piker keeps a negative from passing on an empty legal set,
  -- and alice's own perspective is CR 702.11b's opponent scope, which is what
  -- makes the negative below about hexproof rather than about targeting being
  -- broken for some payment or timing reason.
  Spec.it s "CR 725.5 Dawnglade Regent's hexproof does nothing until its own trigger crowns alice" $ do
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    regent <- S.printingOf s registry "Dawnglade Regent"
    piker <- S.printingOf s registry "Goblin Piker"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let -- {5}{G}{G} for the Regent, and {1}{B} twice for bob.
        (gs1, regentCardId) = S.handOne regent (S.landsInPlay forest 7)
        (alicePikerId, gs2) = S.addCreature piker S.alice gs1
        (bobPikerId, gs3) = S.addCreature piker S.bob gs2
        (_, gs4) = S.addCreature swamp S.bob gs3
        (_, gs5) = S.addCreature swamp S.bob gs4
        (bobsBlade, gs6) = S.addHandCard doomBlade S.bob gs5
        (_, gs7) = S.addHandCard doomBlade S.bob gs6
        -- CR 104.3c: neither player may deck during the three runs below.
        (_, gs8) = S.addLibraryCard piker S.alice gs7
        (_, board) = S.addLibraryCard piker S.bob gs8
        -- The Regent's BATTLEFIELD id: CR 400.7 makes the resolved permanent a
        -- new object, so the hand id above cannot name it.
        regentOn gs =
          Maybe.listToMaybe
            [ oid
            | oid <- Set.toList (GameState.battlefield gs),
              fmap Face.name (Game.faceOf oid gs) == Just (S.printingName regent)
            ]
        castRegent = S.runPure S.identityAnswer board (S.cast S.alice regentCardId)
        entered = S.runPure S.identityAnswer castRegent Stack.resolveTop
        -- THE WINDOW: the Regent is on the battlefield and its enters trigger is
        -- on the stack, unresolved. Nobody is the monarch.
        window = S.runPure S.identityAnswer entered Engine.settleForPriority
        -- The same board one resolution later, with alice crowned.
        crowned = S.runPure S.identityAnswer window Stack.resolveTop
    case (S.spellTargetSlot doomBlade, regentOn window) of
      (Nothing, _) -> Spec.assertFailure s "Doom Blade should declare a target slot"
      (_, Nothing) -> Spec.assertFailure s "the Regent should be on the battlefield in the window"
      (Just theSlot, Just regentId) -> do
        let legalFor pid = Target.legalRecipients (Just pid) S.noSource theSlot
            aimAtRegent :: Prompt.Prompt r -> r
            aimAtRegent p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature regentId))) sets
              _ -> S.identityAnswer p
            bobCast = S.runPure aimAtRegent window (S.cast S.bob bobsBlade)
            killed = S.runPure aimAtRegent bobCast Stack.resolveTop
        -- (1) The window itself. CR 725.1: there is no monarch in a game until an
        -- effect instructs a player to become one, and the effect that would is
        -- still on the stack.
        Spec.assertEqWith s "nobody is the monarch yet" (GameState.monarch window) Nothing
        Spec.assertEqWith s "because the enters trigger has not resolved" (length (GameState.stack window)) 1
        Spec.assertBool s (Set.member (Recipient.ToCreature regentId) (legalFor S.bob window)) "so bob may target the Regent: CR 725.5's effect does nothing"
        Spec.assertBool s (Set.member (Recipient.ToCreature alicePikerId) (legalFor S.bob window)) "and alice's Piker beside it"
        -- The whole card through the stack, not merely a set membership: the
        -- removal bob is allowed to cast in that window actually resolves.
        Spec.assertEqWith s "bob's Doom Blade went on the stack above the trigger" (length (GameState.stack bobCast)) 2
        Spec.assertEqWith s "and the Regent is destroyed" (regentOn killed) Nothing
        Spec.assertBool s (elem (S.printingName regent) (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid killed)) (Game.zoneMembers Zone.Graveyard S.alice killed))) "into alice's graveyard"
        -- (2) The steady state, one resolution later. Now CR 604.2's clause is
        -- true, so layer 6 (CR 613.1f) hands out CR 702.11a's hexproof.
        Spec.assertEqWith s "the trigger resolved and alice is the monarch" (GameState.monarch crowned) (Just S.alice)
        Spec.assertBool s (not (Set.member (Recipient.ToCreature regentId) (legalFor S.bob crowned))) "now bob may not target the Regent"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature alicePikerId) (legalFor S.bob crowned))) "nor alice's Piker: the set is PERMANENTS YOU CONTROL, not the source"
        Spec.assertBool s (Set.member (Recipient.ToCreature bobPikerId) (legalFor S.bob crowned)) "though bob's own Piker is still a legal target"
        -- (3) THE DISCRIMINATOR for (2). CR 702.11b's hexproof stops only "spells
        -- or abilities your opponents control", so the Regent's own controller
        -- reaches it. Were bob's spell illegal for a payment or timing reason,
        -- alice's would be illegal too and (2) would prove nothing.
        Spec.assertBool s (Set.member (Recipient.ToCreature regentId) (legalFor S.alice crowned)) "alice may still target her own Regent"
        -- (4) CR 109.5: the clause reads "YOU'RE the monarch", i.e. the static's
        -- own controller. Crowning bob instead answers the question an
        -- implementation that asked "is there a monarch at all" would get wrong.
        let bobCrowned = S.withMonarch S.bob crowned
        Spec.assertBool s (Set.member (Recipient.ToCreature regentId) (legalFor S.bob bobCrowned)) "with bob wearing the crown, alice's Regent has no hexproof"

  -- CR 122.1: "A counter is a marker placed on an object or player ...". Razorfin
  -- Abolisher's slot asks whether the candidate has one AT ALL, with no kind to
  -- look up -- Filter.HasCountersOfAnyKind, the kind-agnostic sibling of the
  -- HasCounters atom Renegade Krasis writes.
  --
  -- Razorfin Abolisher {2}{U} Creature -- Merfolk Wizard (EVE), "{1}{U}, {T}:
  -- Return target creature with a counter on it to its owner's hand." (name,
  -- cost, type line, P/T and Oracle text checked against api.scryfall.com,
  -- 2026-08-27). Nothing is omitted, so pawl's card is neither stricter nor
  -- weaker than printed.
  --
  -- TWO candidates on bob's side, because one board cannot discriminate the atom
  -- alone:
  --
  --   * a Hill Giant carrying a STUN counter -- the only legal target. The kind is
  --     deliberately not +1/+1: a HasCounters PlusOnePlusOne written by mistake
  --     admits nothing here, so the case would fail rather than pass.
  --   * a Goblin Piker carrying none -- rejected by the atom alone. Without it the
  --     legal set is a singleton whatever the filter says, and "the Giant was
  --     returned" would prove nothing about counters.
  --
  -- Different names and different P/T, so which creature moved is read off the
  -- printed name in bob's hand. The Abolisher settles first: CR 302.6 makes its
  -- {T} illegal otherwise, and it is itself a third counterless creature the
  -- filter must keep out.
  razorfinSpec s registry
  -- CR 202.3's bound read off the BOARD rather than off the card, which no Filter
  -- could state before Pawl.Types.TargetSlot grew its `amount`; see #2538.
  celestineSpec s registry
  -- The same bound read off the ANNOUNCEMENT instead: the amount CR 603.2's own
  -- event stamped, which no board can answer.
  warsingerSpec s registry
  -- And the SPELL's announcement: CR 601.2b's X, named one step before CR 601.2c
  -- chooses against it.
  stirTheGraveSpec s registry
  -- And the same announcement read TWICE, by the offer and by CR 601.2c's joint
  -- check: a slot whose bound reads that X and whose pool reads a sibling slot.
  borrowedExhumationSpec s registry

razorfinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
razorfinSpec s registry = Spec.describe s "HasCountersOfAnyKind (CR 122.1)" $ do
  Spec.it s "CR 122.1 Razorfin Abolisher returns the creature with a counter on it" $ do
    built <- razorfinBoard s registry
    giant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    case built of
      Nothing -> Spec.assertFailure s "Razorfin Abolisher should print one activated ability"
      Just (board, ability, giantId, pikerId) -> do
        -- The fixture's own preconditions: the counter really is there, it is
        -- really not a +1/+1 one, and the other creature really has none -- so
        -- nothing below can pass because S.addCounter missed.
        Spec.assertEqWith s "CR 122.1 the Giant carries one stun counter" (S.counterOf CounterKind.Stun giantId board) 1
        Spec.assertEqWith s "and no +1/+1 counter, which is not the kind under test" (S.counterOf CounterKind.PlusOnePlusOne giantId board) 0
        Spec.assertEqWith s "with the Piker carrying none of either" (S.counterOf CounterKind.Stun pikerId board + S.counterOf CounterKind.PlusOnePlusOne pikerId board) 0
        let after = activateAt giantId board (abolisherOf board) ability
        -- THE GAMEPLAY ASSERTION.
        Spec.assertBool s (not (S.onBattlefield giantId after)) "CR 122.1 the creature with a counter on it left the battlefield"
        Spec.assertBool s (elem (S.printingName giant) (namesInHand S.bob after)) "and CR 400.7's new object is in its OWNER's hand"
        Spec.assertBool s (S.onBattlefield pikerId after) "with the counterless creature untouched"
        Spec.assertBool s (notElem (S.printingName piker) (namesInHand S.bob after)) "and nowhere near bob's hand"

  -- WHAT THE ATOM BUYS, asked of the engine's own candidate set: the counterless
  -- Piker is a creature bob controls and Pool.Creatures gathers it, so only CR
  -- 122.1 keeps it out. The answerer asks for it and S.preferring falls back to
  -- the smallest legal recipient when the offer does not hold it -- so a filter
  -- that had admitted the Piker would have returned the Piker instead.
  Spec.it s "CR 122.1 a creature with no counters on it is not a legal target" $ do
    built <- razorfinBoard s registry
    case built of
      Nothing -> Spec.assertFailure s "Razorfin Abolisher should print one activated ability"
      Just (board, ability, giantId, pikerId) -> do
        let after = activateAt pikerId board (abolisherOf board) ability
        -- THE GAMEPLAY ASSERTION, and the one the Piker's admission would change.
        Spec.assertBool s (S.onBattlefield pikerId after) "CR 122.1 the counterless creature was never offered, so it stayed"
        Spec.assertBool s (not (S.onBattlefield giantId after)) "and the countered one was returned in its place"

  -- The admitted SET by identity, and AFTER the two cases above rather than
  -- before them: a membership read is a proxy for what the ability does, and the
  -- board is what this unit exists to move. It is here so that "the Piker stayed"
  -- cannot be read as an activation that never happened.
  Spec.it s "CR 601.2c the slot admits exactly the creature with a counter on it" $ do
    built <- razorfinBoard s registry
    case built of
      Nothing -> Spec.assertFailure s "Razorfin Abolisher should print one activated ability"
      Just (board, ability, giantId, pikerId) -> case soleTargetSlot (ActivatedAbility.modal ability) of
        Nothing -> Spec.assertFailure s "Razorfin Abolisher's ability should declare one target slot"
        Just theSlot -> do
          let legal = Target.legalRecipients (Just S.alice) S.noSource theSlot board
          Spec.assertEqWith s "CR 122.1 exactly the countered creature" legal (Set.singleton (Recipient.ToCreature giantId))
          Spec.assertBool s (not (Set.member (Recipient.ToCreature pikerId) legal)) "the counterless one is not in it"
          Spec.assertBool s (not (Set.member (Recipient.ToCreature (abolisherOf board)) legal)) "nor alice's own counterless Abolisher"

-- Razorfin Abolisher's board: alice's settled Abolisher and two Islands for the
-- {1}{U}, bob's Hill Giant carrying a stun counter and his counterless Goblin
-- Piker. Nothing if the printing stopped declaring exactly one ability.
razorfinBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Maybe (GameState.GameState, ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card), ObjectId.ObjectId, ObjectId.ObjectId))
razorfinBoard s registry = do
  abolisher <- S.printingOf s registry "Razorfin Abolisher"
  island <- S.printingOf s registry "Island"
  giant <- S.printingOf s registry "Hill Giant"
  piker <- S.printingOf s registry "Goblin Piker"
  pure $ case soleActivatedAbility abolisher of
    Nothing -> Nothing
    Just ability ->
      let (_, g1) = S.addCreature abolisher S.alice (Setup.emptyGame S.bothPlayers)
          -- CR 302.6: the Abolisher's cost carries {T}, so it must have settled.
          settled = S.runPure S.identityAnswer g1 (Engine.settleAll S.alice)
          (_, g2) = S.addCreature island S.alice settled
          (_, g3) = S.addCreature island S.alice g2
          (giantId, g4) = S.addCreature giant S.bob g3
          (pikerId, g5) = S.addCreature piker S.bob g4
          board = (S.addCounter CounterKind.Stun 1 giantId g5) {GameState.priority = Just S.alice}
       in Just (board, ability, giantId, pikerId)

-- Activate `ability` off `srcId` aimed at `oid`, and resolve it. One answerer
-- serves both halves -- CR 602.2b's announcement and the resolution -- which is
-- the shape the Withered Wretch cases above already have.
activateAt :: ObjectId.ObjectId -> GameState.GameState -> ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> GameState.GameState
activateAt oid board srcId ability =
  S.runPure (aimAtCreature oid) (S.runPure (aimAtCreature oid) board (Activate.activateAbility S.alice srcId ability)) Stack.resolveTop

-- The Abolisher on the battlefield, by printed name. Read off the board rather
-- than returned by the fixture, which keeps its tuple to the two candidates the
-- cases discriminate between. S.noSource when it is not there, which no case
-- reaches -- the fixture put it on the battlefield.
abolisherOf :: GameState.GameState -> ObjectId.ObjectId
abolisherOf gs =
  Maybe.fromMaybe
    S.noSource
    ( Maybe.listToMaybe
        [ oid
        | oid <- Set.toList (GameState.battlefield gs),
          fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Razorfin Abolisher"))
        ]
    )

-- The printed names of the cards in `pid`'s hand. CR 400.7 makes the returned
-- permanent a new object, so an assertion about what moved reads the NAME.
namesInHand :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesInHand pid gs = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Hand pid gs)

-- Aim every target slot at one creature, falling back to the smallest legal
-- recipient when the offer does not hold it -- which is what makes "the engine
-- never offered it" observable as a different permanent moving.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just oid) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- alice's graveyard holds a Goblin Piker (mana value 2), a Russet Wolves (mana
-- value 4) and a Lightning Bolt (mana value 1), with `gained` life gained this
-- turn planted in the CR 608.2i log Game.lifeGainedThisTurn folds.
--
-- Three DISTINCT mana values, and the Bolt is the one UNDER every bound the cases
-- use: it is kept out by the And's other conjunct alone, so a filter that had
-- stopped narrowing by card type would show up here rather than pass.
celestineGraveyard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Natural.Type.Natural -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
celestineGraveyard piker wolves bolt gained =
  let (pikerId, g1) = S.addGraveyardCard piker S.alice (Setup.emptyGame S.bothPlayers)
      (wolvesId, g2) = S.addGraveyardCard wolves S.alice g1
      (boltId, g3) = S.addGraveyardCard bolt S.alice g2
   in (pikerId, wolvesId, boltId, S.withEvents [GameEvent.LifeGained (LifeChange.MkLifeChange S.alice gained)] g3)

-- Celestine, the Living Saint ({4}{W} Legendary Creature -- Human Warrior 3/4,
-- Oracle text verified against Scryfall): "Flying, lifelink / Healing Tears -- At
-- the beginning of your end step, return target creature card with mana value X
-- or less from your graveyard to the battlefield, where X is the amount of life
-- you gained this turn."
--
-- THE card the computed mana-value bound was waiting for. CR 601.2c chooses the
-- target and CR 608.2b re-reads it, both through
-- Pawl.Engine.Target.admittedGiven, which is the one site that fills
-- Filter.Context.slotAmount -- off the slot's own Quantity.LifeGainedThisTurn.
--
-- The bound is NOT a printed literal, and the two cases below are what say so: a
-- card out of range on one board is in range on another that differs only in how
-- much life was gained, and a board with no gain at all admits nothing.
celestineSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
celestineSpec s registry = Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
  -- THE PROVING CASE, and the MOVING-BOUND control in one: the same graveyard
  -- judged at two life totals. At 2 the Wolves are out of range; at 4 they are in
  -- it, with nothing else about the board changed.
  Spec.it s "CR 202.3 / 601.2c the bound moves with the board, not with the card" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    bolt <- S.printingOf s registry "Lightning Bolt"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    case triggerTargetSlot celestine of
      Nothing -> Spec.assertFailure s "Celestine should declare one triggered ability with one target slot"
      Just theSlot -> do
        let (pikerId, wolvesId, boltId, atTwo) = celestineGraveyard piker wolves bolt 2
            (_, _, _, atFour) = celestineGraveyard piker wolves bolt 4
            legal = Target.legalRecipients (Just S.alice) S.noSource theSlot
        Spec.assertEqWith s "the fixture planted 2 life gained" (Game.lifeGainedThisTurn atTwo S.alice) 2
        Spec.assertEqWith s "and 4 on the other board" (Game.lifeGainedThisTurn atFour S.alice) 4
        Spec.assertBool s (Set.member (Recipient.ToObject pikerId) (legal atTwo)) "mana value 2 is within a bound of 2"
        Spec.assertBool s (not (Set.member (Recipient.ToObject wolvesId) (legal atTwo))) "mana value 4 is not"
        Spec.assertBool s (Set.member (Recipient.ToObject wolvesId) (legal atFour)) "and at 4 gained it is -- the bound moved"
        Spec.assertBool s (not (Set.member (Recipient.ToObject boltId) (legal atFour))) "the instant card under every bound is still out (the And narrows by card type)"
        Spec.assertEqWith s "so the wider board offers both creature cards and nothing else" (legal atFour) (Set.fromList [Recipient.ToObject pikerId, Recipient.ToObject wolvesId])
  -- THE ZERO control, built as the same board differing in exactly one thing: no
  -- life gained, so nothing in a graveyard of mana values 1, 2 and 4 is in range.
  -- Without it a bound the engine simply ignored would pass the case above.
  Spec.it s "CR 202.3 with no life gained this turn the slot admits nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    bolt <- S.printingOf s registry "Lightning Bolt"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    case triggerTargetSlot celestine of
      Nothing -> Spec.assertFailure s "Celestine should declare one triggered ability with one target slot"
      Just theSlot -> do
        let (pikerId, wolvesId, _, atZero) = celestineGraveyard piker wolves bolt 0
        Spec.assertEqWith s "the fixture planted no life gained" (Game.lifeGainedThisTurn atZero S.alice) 0
        Spec.assertEqWith s "and the slot admits nothing at all" (Target.legalRecipients (Just S.alice) S.noSource theSlot atZero) Set.empty
        -- The same two cards ARE offered once the bound reaches them, so the empty
        -- set above is the bound talking and not an empty graveyard.
        let (_, _, _, atFour) = celestineGraveyard piker wolves bolt 4
        Spec.assertEqWith
          s
          "while the same graveyard at 4 gained offers both"
          (Target.legalRecipients (Just S.alice) S.noSource theSlot atFour)
          (Set.fromList [Recipient.ToObject pikerId, Recipient.ToObject wolvesId])
  -- CR 202.3's bound read off the ANNOUNCEMENT rather than off the board: a
  -- Quantity naming a SLOT. CR 601.2c matches the slot as the ability is
  -- announced -- CR 603.3d importing that rule for a trigger -- and slotContext
  -- evaluates the bound against CR 113.7's source, a permanent that carries no
  -- announcement binding, so the answer has to come from the environment the
  -- caller hands over (Filter.Context's boundAmounts).
  --
  -- Three boards differing in exactly one thing, the seed on the SAME slot over
  -- the SAME graveyard: an announcement of 2, one of 4, and one binding nothing.
  -- "thatMuch" is Pawl.Engine.Binding.eventAmount, the name a triggered ability's
  -- own CR 603.2 event stamps -- warsingerSpec below is the printed card, driven
  -- through combat.
  Spec.it s "CR 603.2 a bound naming a slot reads the announcement's amount" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    bolt <- S.printingOf s registry "Lightning Bolt"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    case triggerTargetSlot celestine of
      Nothing -> Spec.assertFailure s "Celestine should declare one triggered ability with one target slot"
      Just theSlot -> do
        -- No life gained, so the printed Quantity this slot replaces would admit
        -- nothing: every offer below is the seeded amount talking.
        let (pikerId, wolvesId, _, noGain) = celestineGraveyard piker wolves bolt 0
            (sourceId, board) = S.addCreature piker S.alice noGain
            name = SlotName.MkSlotName (Text.pack "target")
            slotted = theSlot {TargetSlot.amount = Just (Quantity.Type.InSlot Binding.eventAmount)}
            offer seed = Map.findWithDefault Set.empty name (Target.legalSets (Just S.alice) seed sourceId (Map.singleton name slotted) board)
            announcing n = Map.singleton Binding.eventAmount (Binding.toAmount n)
        Spec.assertEqWith s "an announcement of 2 reaches the mana value 2 card alone" (offer (announcing 2)) (Set.singleton (Recipient.ToObject pikerId))
        Spec.assertEqWith s "and one of 4 reaches the mana value 4 card as well" (offer (announcing 4)) (Set.fromList [Recipient.ToObject pikerId, Recipient.ToObject wolvesId])
        Spec.assertEqWith s "while an announcement binding no amount admits nothing" (offer Map.empty) Set.empty
        Spec.assertBool s (not (Set.member (Recipient.ToObject sourceId) (offer (announcing 4)))) "the source itself is on the battlefield, so the graveyard pool leaves it out"

-- Venerable Warsinger ({1}{R}{W} Creature -- Spirit Cleric 3/3, Oracle text
-- verified against Scryfall): "Vigilance, trample / Whenever this creature deals
-- combat damage to a player, you may return target creature card with mana value
-- X or less from your graveyard to the battlefield, where X is the amount of
-- damage this creature dealt to that player."
--
-- THE card the announcement-read bound was waiting for, and the one celestineSpec
-- cannot reach: Celestine's X is a fact about the BOARD, so its slot is
-- answerable against CR 113.7's source alone. This X is a fact about the
-- ANNOUNCEMENT -- the amount CR 603.2's event stamped under
-- Pawl.Engine.Binding.eventAmount -- and CR 603.3d chooses the target before the
-- ability object on the stack carries any binding at all, so nothing on the board
-- can answer it.
--
-- THREE DISTINCT READINGS of one board, so the offered set names one and rejects
-- two. A -1/-1 counter makes the Warsinger a 2/2 before it connects, so the event
-- carries 2 rather than the printed 3, and alice's graveyard holds a creature card
-- at each of mana value 2, 3 and 4:
--
--   * the event's amount (2) admits the Piker alone -- the printed rule;
--   * the source's printed power (3) would admit the Tyrant too;
--   * a bound that went unanswered admits nothing, which is CR 603.3d's removal.
--
-- The answerer PREFERS every card the rule excludes, so a widened bound is
-- observable as a different permanent arriving rather than as nothing happening --
-- a Lightning Bolt UNDER every bound among them, since it is kept out by the And's
-- other conjunct alone and a filter that had stopped narrowing by card type would
-- otherwise be invisible (the fallback takes the smallest legal recipient, which
-- is the Piker either way).
--
-- THREE SEATS, so "your graveyard" (CR 109.5's you, alice) is a different zone
-- from the damaged player's.
warsingerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
warsingerSpec s registry =
  let board = do
        warsinger <- S.printingOf s registry "Venerable Warsinger"
        piker <- S.printingOf s registry "Goblin Piker"
        tyrant <- S.printingOf s registry "Kalakscion, Hunger Tyrant"
        wolves <- S.printingOf s registry "Russet Wolves"
        bolt <- S.printingOf s registry "Lightning Bolt"
        let (gs0, mine, _, _) = S.threePlayerCombat [warsinger] [] []
            (pikerId, g1) = S.addGraveyardCard piker S.alice gs0
            (tyrantId, g2) = S.addGraveyardCard tyrant S.alice g1
            (wolvesId, g3) = S.addGraveyardCard wolves S.alice g2
            (boltId, g4) = S.addGraveyardCard bolt S.alice g3
            shrunk = List.foldl' (flip (S.addCounter CounterKind.MinusOneMinusOne 1)) g4 mine
            -- The same seam questingBeastSpec uses: the declarations run as
            -- steps, the damage is dealt by hand, and settleForPriority places
            -- the trigger -- so `placed` is the state with the ability on the
            -- stack and its target already chosen.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) (warsingerPlan [tyrantId, wolvesId, boltId]) shrunk
            fought = S.runPure (warsingerPlan [tyrantId, wolvesId, boltId]) atDamage Damage.dealCombatDamage
            placed = S.runPure (warsingerPlan [tyrantId, wolvesId, boltId]) fought Engine.settleForPriority
        pure (mine, pikerId, shrunk, placed, S.runPure (warsingerPlan [tyrantId, wolvesId, boltId]) placed Engine.priorityLoop)
   in Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
        -- THE proving case, at gameplay level: which card came back.
        Spec.it s "CR 603.2 whole card: the bound is the damage the event carried" $ do
          (mine, _, before, _, after) <- board
          case mine of
            [warsingerId] -> do
              Spec.assertEqWith s "CR 202.3: the mana value 2 creature card is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
              Spec.assertEqWith s "and the mana value 3 one the answerer preferred is not" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Kalakscion, Hunger Tyrant")) S.alice after) 0
              Spec.assertEqWith s "nor the mana value 4 one" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Russet Wolves")) S.alice after) 0
              Spec.assertEqWith s "nor the instant under every bound" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Lightning Bolt")) S.alice after) 0
              -- The fixture's own preconditions, asserted rather than assumed.
              Spec.assertEqWith s "the -1/-1 counter makes the Warsinger a 2/2 before it connects" (S.powerToughnessOf warsingerId before) (Just (2, 2))
              Spec.assertEqWith s "CR 510.1b: bob took the Warsinger's 2" (S.lifeOf S.bob after) (Just 18)
            _ -> Spec.assertFailure s "fixture should give alice one Warsinger"
        -- The announcement itself, read off the placed ability rather than
        -- inferred from what happened -- so this says what the event stamped and
        -- what the slot ADMITTED against it.
        Spec.it s "CR 603.3d the slot admits only the card the event's amount reaches" $ do
          (_, pikerId, _, placed, _) <- board
          case GameState.stack placed of
            [abilityId] -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
              Spec.assertEqWith s "the graveyard card the event's 2 reaches is the one target chosen" (Map.lookup (SlotName.MkSlotName (Text.pack "target")) (Binding.targetsOf bindings)) (Just (Set.singleton (Recipient.ToObject pikerId)))
              Spec.assertEqWith s "and the event stamped 2 under CR 603.2's own slot" (Binding.amountOf Binding.eventAmount bindings) (Just 2)
            _ -> Spec.assertFailure s "fixture should place exactly one trigger"

-- Attacks bob, takes the printed "may", and aims every target slot at the
-- graveyard cards the rule EXCLUDES -- falling back to the smallest legal
-- recipient, which is what makes a widened bound observable as a different
-- permanent arriving.
warsingerPlan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
warsingerPlan bait p = case p of
  Prompt.ChooseDefender {} -> S.bob
  Prompt.ChooseTargets _ _ _ asked -> S.preferring (maybe False (`elem` bait) . Recipient.objectOf) asked
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.aggressiveAnswer p

-- Stir the Grave ({X}{B} Sorcery, BOK, paper, Oracle text fetched from Scryfall
-- this session and transcribed whole): "Return target creature card with mana
-- value X or less from your graveyard to the battlefield."
--
-- The SPELL half of the announcement-read bound, where warsingerSpec above is the
-- trigger half. The number is CR 601.2b's announced X rather than CR 603.2's event
-- amount, and the two roads differ in more than which slot the bound names:
--
--   * CR 601.2b names X one step BEFORE CR 601.2c chooses the target, so the
--     value exists when the offer is computed and Pawl.Engine.Cast.castProposed
--     hands it over as the seed. That is the second case here.
--   * CR 700.2a's fillability gate runs EARLIER STILL -- before the mode choice
--     rule 601.2b lists first, and so before any X exists at all. A bound read
--     there states nothing, because rule 601.2b puts no ceiling on the value the
--     caster may name; the gate refuses the spell for what the announcement
--     cannot change and for nothing else. That is the first case, built as a PAIR
--     of graveyards differing in one card.
--
-- The gameplay case is Warsinger's board one rule over: alice's graveyard holds a
-- creature card at each of mana value 2, 3 and 4 plus a Lightning Bolt UNDER every
-- bound among them, and the answerer PREFERS every card the announced X excludes,
-- so a widened bound is observable as a different permanent arriving rather than as
-- nothing happening. The Bolt is what makes a filter that had stopped narrowing by
-- card type visible, since the fallback takes the smallest legal recipient either
-- way.
stirTheGraveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stirTheGraveSpec s registry =
  let boardOf graveyard swamps = do
        stir <- S.printingOf s registry "Stir the Grave"
        swamp <- S.printingOf s registry "Swamp"
        cards <- traverse (S.printingOf s registry) graveyard
        let (gs0, stirId) = S.boltInHand swamp stir swamps Phase.PrecombatMain
            (ids, gs1) = List.foldl' (\(acc, g) c -> let (oid, g') = S.addGraveyardCard c S.alice g in (acc <> [oid], g')) ([], gs0) cards
        pure (stirId, ids, gs1)
   in Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
        -- CR 700.2a asked before CR 601.2b exists, as a pair of boards differing in
        -- their one graveyard card: the same three Swamps either way, a mana value
        -- 4 CREATURE card on one and an instant on the other. A gate that read the
        -- unannounced bound as a bound would refuse BOTH -- mana value 4 is not "4
        -- or less" of an X nobody has named -- and a gate that had stopped narrowing
        -- altogether would offer both. The negative half is carried by the slot's
        -- card-type conjunct rather than by the bound, which is the point: the
        -- permissive floor drops the bound and leaves everything else standing.
        Spec.it s "CR 601.2b the castability gate states no bound the announcement has not made" $ do
          (creatureBoard, _, withCreature) <- boardOf ["Russet Wolves"] 3
          (instantBoard, _, withInstant) <- boardOf ["Lightning Bolt"] 3
          Spec.assertBool s (S.castable S.alice creatureBoard withCreature) "the mana value 4 creature card is reachable by some X, so the cast is offered"
          Spec.assertBool s (not (S.castable S.alice instantBoard withInstant)) "and no X reaches an instant, so it is not"
        -- THE proving case, at gameplay level: which card came back. THREE
        -- DISTINCT READINGS of one graveyard, so the offered set names one and
        -- rejects two -- the announced 2 admits the Piker alone, a bound that had
        -- widened admits the Tyrant and the Wolves as well, and a bound that went
        -- unanswered admits nothing and CR 601.2e reverses the whole cast.
        Spec.it s "CR 601.2c whole card: the bound is the X the caster announced" $ do
          (stirId, ids, gs) <- boardOf ["Goblin Piker", "Kalakscion, Hunger Tyrant", "Russet Wolves", "Lightning Bolt"] 3
          case ids of
            [_, tyrantId, wolvesId, boltId] -> do
              let after = S.runPure (stirPlan 2 [tyrantId, wolvesId, boltId]) gs (S.cast S.alice stirId >> Stack.resolveTop)
              Spec.assertEqWith s "CR 202.3: the mana value 2 creature card is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
              Spec.assertEqWith s "and the mana value 3 one the answerer preferred is not" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Kalakscion, Hunger Tyrant")) S.alice after) 0
              Spec.assertEqWith s "nor the mana value 4 one" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Russet Wolves")) S.alice after) 0
              Spec.assertEqWith s "nor the instant under every bound" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Lightning Bolt")) S.alice after) 0
              -- The fixture's own precondition, asserted rather than assumed: the
              -- cast was not reversed, so {2}{B} was announced and paid.
              Spec.assertEqWith s "CR 601.2h all three Swamps paid {2}{B}" (S.tappedCount S.alice after) 3
            _ -> Spec.assertFailure s "fixture should stock alice's graveyard with four cards"
        -- The other end of the same announcement, on the same board: X is the
        -- caster's to name, so naming 0 leaves a slot no card in that graveyard
        -- satisfies -- and CR 601.2c's unfillable announcement makes the casting
        -- illegal rather than being repaired to some value the board can meet.
        Spec.it s "CR 601.2e an X no graveyard card is under reverses the whole cast" $ do
          (stirId, ids, gs) <- boardOf ["Goblin Piker", "Kalakscion, Hunger Tyrant", "Russet Wolves", "Lightning Bolt"] 3
          case ids of
            [_, tyrantId, wolvesId, boltId] -> do
              let after = S.runPure (stirPlan 0 [tyrantId, wolvesId, boltId]) gs (S.cast S.alice stirId >> Stack.resolveTop)
              Spec.assertEqWith s "no creature card came back" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 0
              Spec.assertEqWith s "CR 601.2: the game returned to before the casting was proposed, so no Swamp is tapped" (S.tappedCount S.alice after) 0
            _ -> Spec.assertFailure s "fixture should stock alice's graveyard with four cards"

-- Announces this X and aims every target slot at the graveyard cards the bound
-- EXCLUDES, falling back to the smallest legal recipient -- warsingerPlan's shape,
-- with CR 601.2b's announcement in place of the combat that supplied the trigger.
stirPlan :: Natural.Type.Natural -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
stirPlan x bait p = case p of
  Prompt.ChooseX {} -> x
  Prompt.ChooseTargets _ _ _ asked -> S.preferring (maybe False (`elem` bait) . Recipient.objectOf) asked
  _ -> S.identityAnswer p

-- Synthetic Borrowed Exhumation ({X}{B} Sorcery,
-- data/cards/synthetic-borrowed-exhumation.json): "Return target creature card
-- with mana value X or less from target player's graveyard to the battlefield."
--
-- SYNTHETIC, and the search that settled it: Scryfall, 2026-08-31, with a
-- User-Agent -- o:"mana value X or less" (95 printings), o:"power X or less"
-- (8), and o:/X or less/ minus those two (7), every one read. Every X-bounded
-- target slot Magic has printed draws from a pool no other slot names and
-- carries a filter naming none either, so no printing pairs the two halves. Stir
-- the Grave above is the bound alone; Dwell on the Past is the sibling-slot pool
-- alone. Nothing in CR 202.3 or 601.2c forbids one card printing both, which is
-- what makes the synthetic legitimate rather than a shape the rules exclude.
--
-- Those two halves on ONE slot are the whole point: the card slot is jointly
-- judged (Target.jointlyJudged, because its pool is CR 400.1's graveyard scoped
-- to whatever the player slot names) AND its CR 202.3 computed bound reads CR
-- 601.2b's announced X. The offer is computed against the seed carrying that X;
-- the joint check re-derives the same slot, and it is handed the same seed. Given
-- the chosen targets alone the bound reads no number, Filter.ManaValueAtMostAmount
-- is vacuously False, the card the caster was OFFERED is not in the re-derived
-- set, and CR 601.2e reverses a casting rule 601.2c allows; see #2676.
--
-- THREE SEATS for Dwell on the Past's reason: with alice and bob alone, "bob's
-- graveyard" and "not the caster's graveyard" pick out the same cards.
--
-- The two runs differ in EXACTLY ONE thing: which graveyard the mana value 2
-- creature card sits in. Both announce X = 2, both name bob in the player slot,
-- both pay the same {2}{B} off the same three Swamps, off one board.
borrowedExhumationSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
borrowedExhumationSpec s registry = Spec.describe s "ManaValueAtMostAmount (CR 202.3)" $ do
  Spec.it s "CR 601.2c the joint check re-derives a jointly judged slot against the announced X" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wolves <- S.printingOf s registry "Russet Wolves"
    evangel <- S.printingOf s registry "Cabal Evangel"
    exhumation <- S.printingOf s registry "Synthetic Borrowed Exhumation"
    let (hisId, g1) = S.addGraveyardCard piker S.bob (S.landsFor swamp S.alice 3 S.threePlayerGame)
        (hisBigId, g2) = S.addGraveyardCard wolves S.bob g1
        (hersId, g3) = S.addGraveyardCard evangel S.carol g2
        (board, spellId) = S.handOne exhumation g3
        slots = Modal.allTargetSlots (Face.spell (S.combinedFace exhumation))
        -- The OFFER, taken through the same door Pawl.Engine.Cast.castProposed
        -- takes it through and against the same seed: CR 601.2b's X and nothing
        -- else. It is what the two runs below are judged against, and it is
        -- insensitive to the joint check -- which is what lets the pair say the
        -- offer and the re-check agree rather than merely that something was
        -- rejected.
        offered = Target.legalSets (Just S.alice) (Binding.fromChoices Map.empty (Just 2) mempty) S.noSource slots board
        slotNamed name = Map.findWithDefault Set.empty (SlotName.MkSlotName (Text.pack name)) offered
        run oid =
          let cast = S.runPure (aimingExhumation 2 oid) board (S.cast S.alice spellId)
           in (cast, S.runPure (aimingExhumation 2 oid) cast Stack.resolveTop)
        (castAtBob, atBob) = run hisId
        (castAtCarol, _) = run hersId
    Spec.assertEqWith s "CR 601.2c the card slot is offered the UNION over the player slot, narrowed by the announced X" (slotNamed "card") (Set.fromList (fmap Recipient.ToObject [hisId, hersId]))
    Spec.assertEqWith s "and the player slot every player" (slotNamed "player") (Set.fromList (fmap Recipient.ToPlayer [S.alice, S.bob, S.carol]))
    -- THE GAMEPLAY ASSERTION, ahead of every proxy: the card the offer named came
    -- back, so the joint check read the same X the offer did.
    Spec.assertEqWith s "CR 202.3 naming bob and the mana value 2 card in HIS graveyard, that card is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.bob atBob) 1
    Spec.assertEqWith s "CR 601.2h so the casting was not reversed: all three Swamps paid {2}{B}" (S.tappedCount S.alice atBob) 3
    Spec.assertEqWith s "with the mana value 4 card in the same graveyard left behind, unoffered" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Russet Wolves")) S.bob atBob) 0
    Spec.assertEqWith s "and his card gone from his graveyard (CR 400.7: a new object arrived)" (Game.zoneMembers Zone.Graveyard S.bob atBob) [hisBigId]
    Spec.assertEqWith s "CR 601.2e naming bob and a card in CAROL's graveyard, the joint check rejects and the whole cast is reversed" (length (GameState.stack castAtCarol)) 0
    Spec.assertEqWith s "so carol's card stayed in her graveyard" (Game.zoneMembers Zone.Graveyard S.carol castAtCarol) [hersId]
    Spec.assertBool s (elem spellId (Game.zoneMembers Zone.Hand S.alice castAtCarol)) "and the spell is back in alice's hand"
    Spec.assertEqWith s "where naming his own card put it on the stack" (length (GameState.stack castAtBob)) 1

-- CR 601.2c's whole announcement for Synthetic Borrowed Exhumation: bob in the
-- player slot and `oid` in the card slot, with CR 601.2b's X announced first.
--
-- PINNED rather than searched, aimingDwell's reason: the run naming carol's card
-- beside bob has to be rejected by the JOINT check, so an answerer that filtered
-- against the offer would hand back an empty slot and the announcement would
-- fail on its count instead -- passing for a reason the case is not about.
aimingExhumation :: Natural.Type.Natural -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingExhumation x oid p = case p of
  Prompt.ChooseX {} -> x
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot _ ->
          if slot == SlotName.MkSlotName (Text.pack "player")
            then Set.singleton (Recipient.ToPlayer S.bob)
            else Set.singleton (Recipient.ToObject oid)
      )
      asked
  _ -> S.identityAnswer p
