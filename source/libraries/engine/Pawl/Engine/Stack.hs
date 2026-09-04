module Pawl.Engine.Stack where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.CarryOver as CarryOver
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.Zone as Zone

-- The runner-aware resolve-the-top-of-stack: CR 729.1a's "the spell or ability
-- that created the subgame" names both kinds of object, so the injected runner
-- goes to the spell branch and to all three ability branches alike; see #137.
-- Engine.priorityLoop supplies playSubgame.
--
-- CR 608.3: a resolving permanent spell becomes a permanent on the battlefield;
-- anything else resolves its effects and then goes to its owner's graveyard
-- (Resolve.resolveSpellWith -- the CR 608.2 executor).
--
-- THE INVARIANT: this dispatches on a CLASSIFICATION -- is-it-a-permanent, read
-- off the type line -- and never on the card's identity. There must never be a
-- `case card of ...` here; that is the fusion of the closed and open halves
-- that sinks the project.
resolveTopWith :: Game Result -> Game ()
resolveTopWith runSubgame = do
  gs <- State.get
  case GameState.stack gs of
    [] -> pure ()
    oid : rest -> case Game.lookupObject oid gs of
      -- A stack id that does not resolve is a bug elsewhere; drop it rather
      -- than wedging the loop.
      Nothing -> State.put gs {GameState.stack = rest}
      Just obj -> case Object.source obj of
        Source.OfCard printingId -> resolveCardBacked runSubgame oid rest printingId
        -- A token is never on the stack (created onto the battlefield, never
        -- cast), and neither is a melded permanent: CR 701.42a puts the two cards
        -- onto the battlefield directly rather than announcing anything.
        Source.OfMeld _ -> State.put gs {GameState.stack = rest}
        Source.OfToken _ -> State.put gs {GameState.stack = rest}
        Source.OfAbility ActivatedAbilitySource.MkActivatedAbilitySource {ActivatedAbilitySource.source = srcId, ActivatedAbilitySource.ability = ability} -> do
          -- CR 601.3's offer is NOT made here. It belongs to the Search effect
          -- itself (Resolve's Effect.Search arm), so it sees the state the player
          -- is actually searching from and reaches searching SPELLS too (#57).
          Resolve.resolveAbilityWith runSubgame oid srcId ability
        Source.OfTrigger TriggeredAbilitySource.MkTriggeredAbilitySource {TriggeredAbilitySource.source = srcId, TriggeredAbilitySource.ability = ability} ->
          -- CR 608.2a: an intervening "if" is checked AGAIN as the ability
          -- resolves. Object.owner is the ability's controller, which is "you".
          --
          -- CR 608.2h supplies the view, for the reason
          -- Event.interveningHolds gives at the gather-time half of this rule:
          -- a leaves-the-battlefield ability's source is gone by construction
          -- (CR 603.10a, CR 400.7), and Projection.fullView would describe it
          -- as an object with no characteristics. The two checks must read
          -- alike, or a trigger that passed the gather would be removed here
          -- for no reason.
          --
          -- The ability's own bindings supply the context's slot objects, as
          -- Event.interveningHolds supplies the pending trigger's: rule 702.100a's
          -- "that creature" is the entrant at Binding.became, not the source. So
          -- CR 608.2h is owed to that entrant too and the view is the UNSCOPED
          -- one -- an entrant killed while the trigger waited is compared at the
          -- power and toughness it last had, and read for the owner CR 400.3 asks
          -- of Breathless Knight's clause, which are the reads this arm alone
          -- observes: at gather time the entrant is still there by construction.
          --
          -- CR 303.4b's host rides in beside the slots, and Event.interveningHolds
          -- supplies it at the gather-time half for the reason the view above must
          -- match: Ray of Frost's "if enchanted creature is red" would otherwise
          -- pass one check and fail the other.
          case TriggeredAbility.intervening ability of
            Just cond
              | not (Condition.holds (Projection.viewWithLastKnownAnywhere gs) ((Filter.contextWithSlots (Game.teams gs) (Just (Object.owner obj)) (Just srcId) (Binding.slotObjects (Object.bindings obj))) {Filter.sourceAttachedTo = Projection.hostOf srcId gs}) gs srcId cond) ->
                  State.modify' (Game.cease oid)
            _ ->
              let chosen = Binding.modesOf (Object.bindings obj)
                  modal = TriggeredAbility.modal ability
               in Resolve.resolveModesWith runSubgame oid srcId (Modal.chosenModes chosen modal)
        -- CR 114.5: an emblem is never on the stack (created into the command
        -- zone, never cast). Drop it, like a token.
        Source.OfEmblem _ -> State.put gs {GameState.stack = rest}
        Source.OfSpellCopy printingId -> resolveCardBacked runSubgame oid rest printingId
        Source.OfInherentTrigger InherentTriggerSource.MkInherentTriggerSource {InherentTriggerSource.ability = ability} ->
          -- An inherent ability has no source object, so the ability object
          -- itself stands in for one and Object.owner is its controller -- the
          -- monarch for CR 725.2's two abilities, the player whose engines are
          -- running for CR 702.179d's.
          --
          -- CR 608.2a: the intervening "if" is checked AGAIN as the ability
          -- resolves, exactly as the OfTrigger arm above does it. Rule 725.2's
          -- abilities have none, so this was once elided; rule 702.179d's "if
          -- your speed is less than 4" is one, and eliding it would let a
          -- trigger that waited on the stack raise a speed already at max.
          --
          -- No board reaches the removal today, and the branch is here because
          -- CR 608.2a says so rather than because anything observes it: rule
          -- 702.179d admits one trigger a turn and nothing else in the pool moves
          -- a player's speed, so the answer cannot change between the CR 603.4
          -- check in Pawl.Engine.Speed and this one. The two are mutually
          -- redundant, not jointly redundant -- Pawl.SpeedSpec's "speed stops at
          -- 4" case fails when BOTH are removed.
          --
          -- The view is the same unscoped one the arm above passes, so the rule
          -- cannot mean one thing for a borne trigger and another for an inherent
          -- one. It describes `oid` as an object with no characteristics either
          -- way -- sound because an inherent ability's condition reads a PLAYER
          -- (CR 702.179d's "your speed"), never the object it hangs on, there
          -- being none to read.
          --
          -- CR 303.4b's host is left unsupplied here for that same reason: `oid` is
          -- the ability object, which CR 303.4 never attaches to anything, so the
          -- field would be Nothing however it was filled.
          case TriggeredAbility.intervening ability of
            Just cond
              | not (Condition.holds (Projection.viewWithLastKnownAnywhere gs) (Filter.contextWithSlots (Game.teams gs) (Just (Object.owner obj)) (Just oid) (Binding.slotObjects (Object.bindings obj))) gs oid cond) ->
                  State.modify' (Game.cease oid)
            _ ->
              let chosen = Binding.modesOf (Object.bindings obj)
                  modal = TriggeredAbility.modal ability
               in Resolve.resolveModesWith runSubgame oid oid (Modal.chosenModes chosen modal)

-- The no-subgame resolve-top (every existing caller and test): a resolving spell
-- or ability with a PlaySubgame effect would draw. Engine's live loop uses
-- resolveTopWith.
resolveTop :: Game ()
resolveTop = resolveTopWith Resolve.noSubgame

-- The object or player an Aura spell's enchant slot names (CR 303.4 / 303.4a).
-- Nothing when the slot is unbound, which CR 303.4a makes unreachable for a cast
-- Aura -- the slot is a required target, and rule 702.5a prints no "up to", so
-- it names exactly one recipient.
--
-- The recipient is handed on UNCHANGED, tag and all, so CR 303.4c's re-check
-- (Sba.stillLegalEnchant) can compare the stored value against the same pool's
-- candidates without re-deriving how it is referenced. Event's CR 303.4f arm gets
-- the same tag a different way, off Attach.attachmentFor, because there is no cast
-- target for it to hand on; routing this one through attachmentFor too would let a
-- legally targeted Aura whose admission answer differs cancel its own resolution.
enchantedBy :: ObjectId -> GameState.GameState -> Maybe Recipient.Recipient
enchantedBy oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> Binding.onlyOne =<< Map.lookup Card.enchantSlot (Binding.targetsOf (Object.bindings obj))

-- CR 608.3 for a resolving spell that has a printing behind it -- CR 108's card
-- (Source.OfCard) and CR 112.1a's copy of one (Source.OfSpellCopy), which CR 608.2
-- resolves "exactly as it resolves the original": effects then CR 608.2n's
-- graveyard for a nonpermanent, where CR 704.5e removes a copy; the battlefield
-- for a permanent, where CR 608.3f makes a copy a token AS IT ARRIVES --
-- Pawl.Engine.Event's zone-change funnel rewrites the arriving incarnation's
-- Source, so the two arms share every line here and neither asks which it holds.
-- ONE classification for both: is-it-a-permanent, read off the type line.
--
-- Unreachable `_`: a PrintingId is minted only by Game.intern, which inserts, and
-- the caller looked the object up. Drops the id rather than wedging the loop,
-- which is what resolveTopWith's arms do for a source with no card behind it.
resolveCardBacked :: Game Result -> ObjectId -> [ObjectId] -> PrintingId.PrintingId -> Game ()
resolveCardBacked runSubgame oid rest printingId = do
  gs <- State.get
  case (Game.lookupObject oid gs, Game.cardOfPrinting printingId gs) of
    (Just obj, Just card) -> do
      -- CR 702.103e: "As a bestowed Aura spell begins resolving, if its
      -- target is illegal, it ceases to be bestowed and the effect making
      -- it an Aura spell ends. It continues resolving as a creature
      -- spell." CR 608.3b is the same sentence from the resolution side,
      -- and routes such a spell to CR 608.3a.
      --
      -- BEFORE the Aura test below rather than inside the CR 608.2b
      -- fizzle arm it replaces, and that ordering is the whole of the
      -- fix: is-it-an-Aura is a projection read, and CR 702.103b's effect
      -- is minted from this very field
      -- (Pawl.Engine.Projection.bestowGathered). Clearing it after the
      -- branch was taken would leave the spell in the Aura arm it no
      -- longer belongs in.
      --
      -- The CR 608.2b question (Resolve.targetsAllIllegal) stands in for
      -- rule 702.103e's "its target": bestow grants exactly one target,
      -- the enchant slot CR 702.103b adds, and Nyxborn Rollicker --
      -- data/cards/'s one bestow card -- prints no other. A bestowed spell
      -- that ALSO printed a still-legal target would keep resolving as an
      -- Aura here, where rule 702.103e reads the enchant slot alone.
      --
      -- One-way, like Pawl.Engine.Sba's CR 702.103f clear: nothing sets
      -- the field back.
      Monad.when (Object.bestowed obj && Resolve.targetsAllIllegal oid gs) $
        State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bestowed = False}) oid (GameState.objects g)})
      -- Re-read, so every classification below sees the projection the
      -- clear above produced. `obj` is NOT re-read: the clear touches one
      -- Bool, and the fields taken off `obj` further down (its face, its
      -- facing, its default controller) are none of them that Bool.
      --
      -- A REGRESSION FENCE at the Aura test alone, said plainly rather
      -- than left to look tested: pointing that one read back at `gs`
      -- leaves the suite green, because an unbestowed spell has no
      -- enchant slot and Resolve.targetsAllIllegal answers False for a
      -- spell with none, so the Aura arm hands the same board back. It is
      -- CR 702.103e's "the effect making it an Aura spell ends" written
      -- out, not a passing test.
      gs1 <- State.get
      -- CR 709.3b: if this spell has a face singled out, its classification
      -- is read off THAT half, not the two combined -- routed through
      -- Game.faceOf rather than Card.combined directly, so this
      -- is-it-a-permanent/is-it-an-Aura check narrows the same way every
      -- OTHER characteristic read of a stack object already does
      -- (Cost.costsFor resolves the same way, through Game.resolveFace).
      -- Action.playableLands asks the same question of a land play through
      -- Card.landFaces, which CR 712.12 gives the same shape: a chosen face
      -- where the layout has one, and the combined view where it does not.
      --
      -- Falls back to the combined view for parity with faceOf's own
      -- fallback; unreachable here since `obj` already resolved via this
      -- same `oid`.
      let face = Maybe.fromMaybe (Card.combined card) (Game.faceOf oid gs1)
          -- CR 712.13 / 709.5d: the half this spell showed on the stack,
          -- carried onto the permanent for the layouts whose rules say so and
          -- dropped for the rest (Card.enteringFace). What the permanent then
          -- does with it is the MOVE's question and not this one -- CR 712.13
          -- leaves a double-faced permanent showing the face, CR 709.5d turns
          -- a Room's into an unlocked designation. Read off `obj` rather than
          -- off `face`, because what the rules carry is the object's own
          -- record of which half is up and not the face a fallback resolved
          -- to.
          entering = Card.enteringFace card (Object.face obj)
          -- CR 110.2b: "the permanent's controller by default is the player
          -- who put that spell onto the stack" -- CR 405.4's caster, which is
          -- what Object.enteredUnder holds for a spell and what
          -- defaultControllerOf reads off it. It is Object.owner for every
          -- spell whose caster owns the card, which is why this argument was
          -- Nothing until one did not (#83).
          --
          -- The DEFAULT and not Resolve.spellController's projected answer,
          -- which is the same rule's other half: an effect that gave someone
          -- control of the permanent SPELL leaves them controlling the
          -- permanent, and CR 110.2b flags that as a different thing from the
          -- default precisely because CR 800.4c tells them apart. Writing a
          -- layer-2 answer into this field would put a control-changing
          -- effect on the wrong side of that line (see Pawl.Types.Object).
          -- Nothing in the pool changes a spell's control, so the two
          -- coincide today.
          --
          -- UNOBSERVED, and said plainly rather than left to look tested: the
          -- pool's one card that casts somebody else's card (Dire Fleet
          -- Daredevil) reaches only instants and sorceries, so no permanent
          -- spell in the suite has a caster its owner disagrees with, and
          -- replacing this with Object.owner leaves the suite green. It is
          -- CR 110.2b written out, not a passing test.
          controller = Projection.defaultControllerOf obj
       in if not (Card.isPermanent face)
            then Resolve.resolveSpellWith runSubgame oid
            else
              -- `entering` is carried on BOTH branches below: CR 712.13 is
              -- about the resolving spell rather than about which kind of
              -- permanent it becomes, and an Aura back face would carry its
              -- face for the same reason a creature one does.
              --
              -- CarryOver.Carried is on both for CR 400.7a and CR 400.7c,
              -- and these two branches are the only places in the engine
              -- that pass it: an effect that changed the permanent SPELL,
              -- and a prevention shield that watched it as a source of
              -- damage, keep applying to the permanent it becomes. The move
              -- itself performs the re-key, before the CR 614.1c entry loop
              -- reads the entering permanent's own rows (Event.carryOver,
              -- CR 614.12).
              if not (Set.member Subtype.Aura (Projection.subtypesOf oid gs1))
                then -- CR 708.4's last sentence: "the permanent the spell becomes
                -- will be a face-down permanent". A STATUS carried across a
                -- zone change, which CR 400.7 otherwise forgets -- so it is
                -- read off the resolving spell and handed to the move,
                -- exactly as `entering` is. A closed-half read: rule 110.5's
                -- status, never the card's identity.
                --
                -- The Aura branch below carries FaceUp instead, and cannot
                -- need this: it is reached only for a spell the projection
                -- calls an Aura, and a face-down spell's face is
                -- Card.faceDownFace -- whose subtypes are the ones the
                -- listing names, and no listing in the pool names Aura.
                  Monad.void (Event.changeZoneAttaching Nothing Set.empty oid Zone.Battlefield LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty (Just controller) entering (Object.facing obj) False CarryOver.Carried)
                else -- CR 303.4a made this spell target, so CR 608.2b applies
                -- to it. THE INVARIANT: is-it-an-Aura is a SUBTYPE read
                -- (CR 205.3h), the same closed-half classification as
                -- is-it-a-permanent above it.
                --
                -- Off the PROJECTION rather than off the printed type line,
                -- because CR 702.103b's bestow makes a spell an Aura on the
                -- stack that prints no such subtype -- the same read
                -- Pawl.Engine.Sba.cannotBeAttached and
                -- Pawl.Engine.Attach.attachmentFor already take one zone
                -- over. Resolve.targetSlotsOf, which the fizzle test below
                -- goes through, reads the enchant ability off that same
                -- projection, so the branch and its target check cannot
                -- disagree about what an Aura is.
                --
                -- A spell that was bestowed and lost its target is not
                -- here: CR 702.103e unbestowed it above, so the projection
                -- no longer calls it an Aura and it took the branch
                -- above.
                  if Resolve.targetsAllIllegal oid gs1
                    then Event.changeZone oid Zone.Graveyard
                    else
                      -- CR 303.4: an Aura ENTERS attached, so the target is
                      -- seeded into the new incarnation rather than written
                      -- after the move (see Event.changeZoneAttaching).
                      Monad.void (Event.changeZoneAttaching Nothing Set.empty oid Zone.Battlefield LibraryPosition.defaultValue (enchantedBy oid gs1) TapState.Untapped Map.empty (Just controller) entering Facing.FaceUp False CarryOver.Carried)
    _ -> State.put gs {GameState.stack = rest}
