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
-- The last case is hexproof's other axis: not who is targeting but WHETHER THE
-- KEYWORD IS THERE AT ALL. Dawnglade Regent grants it through a CR 604.2 "as
-- long as you're the monarch" clause, so the same Doom Blade answers both ways
-- across CR 725.5's no-monarch window.
--
-- Gameplay-level: every target slot under test is read out of a committed card rather
-- than hand-built, and the cases that turn on an effect cast and resolve through
-- the stack.
module Pawl.TargetSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
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
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The one target slot a single-slot card or ability declares, read out of the
-- committed printing (S.spellTargetSlot's rationale) but keyed by COUNT rather
-- than by slot name: Cancel calls its slot "spell", not "target".
soleTargetSlot :: Modal.Modal Card.Type.Card -> Maybe TargetSlot.TargetSlot
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
soleActivatedAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
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
  -- control. You may choose new targets for the copy" -- nothing copies an object
  -- on the stack (#1006) -- which leaves pawl's Adric stricter than printed.
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
            -- whether it resolved (CR 608.2m) or was countered (CR 608.2n), so
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
  --     and graveyard" -- is why the pool carries a GraveyardScope at all, and
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
