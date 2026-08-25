-- Rule 701.3's LEGALITY READING, in one place because callers a library apart need
-- the same answer: Pawl.Engine.Event.attach, which every CR 701.3 move goes
-- through, Pawl.Engine.Resolve's two attach-the-target opcodes, its Effect.Search
-- arm and putFound -- CR 701.3a asked from the CANDIDATE's side, where the host is
-- fixed and the Aura varies (Filter.CanAttachToSubject) -- and two arms of
-- Pawl.Engine.Event -- the CR 303.4k rewrite for an Aura being turned face up, and
-- changeZoneAttaching's CR 303.4f host choice for an Aura entering the battlefield
-- by any means other than resolving as an Aura spell. Event sits BELOW Resolve, so
-- the shared half cannot live in Resolve where it started.
--
-- WHAT MAY BE ATTACHED WHERE, and never the move itself: rule 701.3b's write is
-- Pawl.Engine.Event.attach, beside the other funnels that record an event, since
-- Event imports this module and the edge cannot run both ways. Everything here is
-- a question with no answer written back to the board.
--
-- THE INVARIANT: nothing here asks which CARD is moving, or which card refuses
-- it. It reads the PROJECTION's subtypes (CR 205.3), the enchant ability rule
-- 702.5a gives an Aura, and the destination's own prohibition through
-- Pawl.Engine.AttachRestriction, all of which are classifications the rulebook
-- itself makes -- the same standing Pawl.Engine.Keyword has over rule 702.
module Pawl.Engine.Attach where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.AttachRestriction as AttachRestriction
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype

-- CR 701.3a/701.3b: may `src` legally be attached to `destination` right now?
--
-- CR 701.3a's last sentence is the MOVING permanent's half -- an Aura, Equipment
-- or Fortification can't be attached to something it couldn't enchant, equip or
-- fortify -- so the two guarded branches below dispatch on which `src` is, read
-- through the PROJECTION, so that an Equipment that lost the subtype (CR 301.5c)
-- and a permanent animated into a creature both answer correctly. CR 303.4's
-- last sentence and CR 301.5 are the DESTINATION's half, and the guard above
-- them is where the two meet.
--
-- Equipment is CR 301.5's creature test. Aura is CR 303.4's enchant ability, asked
-- through Target.admittedRecipients rather than a hand-rolled creature test, which
-- honours an enchant slot that narrows further for free. Admission and not target
-- legality: CR 702.5a gives the enchant ability both jobs and this is the second,
-- so rule 702's targeting restrictions do not reach here. CR 109.5's "you" on that
-- enchant slot is the AURA's controller, not the moving effect's. An Aura with no
-- enchant ability answers Nothing here rather than admitting everything: printed
-- Auras cannot be in that shape (the CardSpec lint holds the biconditional), but a
-- permanent that gained the Aura subtype without gaining enchant can be
-- (Modification.AddSubtype without Modification.GainEnchant), and CR 303.4c gives
-- such a permanent nothing it may legally enchant.
--
-- Read off the PROJECTION rather than the printed face, which is what lets an
-- enchant ability be GRANTED (CR 613.1f, Modification.GainEnchant) --
-- exactly as the Aura subtype it is gated on already is. CR 708.2a's substitution
-- still applies, since Projection.baseCharacteristics seeds this field through
-- Game.faceOf: a permanent that is still face down has no enchant ability and no
-- Aura subtype, and this answers Nothing for every destination. That is not an
-- edge case here but the content of CR 303.4k -- see turnUpHosts below.
--
-- The Aura branch's first test is CR 303.4d's "an Aura that's also a creature
-- can't enchant anything", whose state-based half is Sba.cannotBeAttached.
-- Unreachable in this pool, written because it costs one comparison. The Equipment
-- branch has no counterpart: CR 301.5c's matching restriction carries a
-- reconfigure exception nothing here can express (#193).
--
-- The first guard -- the destination naming `src` itself -- is CR 301.5c and CR
-- 303.4d at once. Nothing for a source that is neither, per CR 701.3b; there is no
-- Subtype.Fortification to case on.
--
-- Answers with the RECIPIENT to store rather than a Bool, since CR 303.4
-- attachment is to an object or player and Object.attachedTo records which -- and
-- the tag must be the one the moving permanent's OWN rules reference the
-- destination by, not the one the moving effect targeted it with. Taking the
-- answer from the slot's candidate list is what keeps Sba's CR 303.4c re-check
-- able to compare against that same list later, so `destination` is matched by
-- which object or player it names rather than by how the caller tagged it.
attachmentFor :: ObjectId -> Recipient -> GameState -> Maybe Recipient
attachmentFor src destination gs
  | Recipient.objectOf destination == Just src = Nothing
  -- CR 303.4's last sentence and CR 301.5 with CR 101.2: the DESTINATION's own
  -- limit on what may become attached to it, which CR 101.2 makes beat rule
  -- 701.3a's permission below. Asked before either branch, since Consecrate
  -- Land's clause and Goblin Brawler's are one prohibition
  -- (Pawl.Types.AttachRestriction), and only of an object destination -- CR
  -- 702.5d's enchanted PLAYER is not a permanent, so nothing on the battlefield
  -- can carry a limit about them.
  | Maybe.maybe False (\oid -> AttachRestriction.refuses src oid gs) (Recipient.objectOf destination) = Nothing
  -- CR 301.5, "it can't legally be attached to anything that isn't a creature" --
  -- which is also why a player destination falls to Nothing here rather than
  -- getting a branch of its own.
  | Set.member Subtype.Equipment subtypes = case Recipient.objectOf destination of
      Just oid | Projection.isCreatureOf oid gs -> Just (Recipient.ToCreature oid)
      _ -> Nothing
  | Set.member Subtype.Aura subtypes =
      if Projection.isCreatureOf src gs
        then Nothing
        else case Card.foldEnchant (Projection.enchantOf src gs) of
          Nothing -> Nothing
          Just slot ->
            List.find
              (names destination)
              (Set.toList (Target.admittedRecipients (Projection.controllerOf src gs) src slot gs))
  | otherwise = Nothing
  where
    subtypes = Projection.subtypesOf src gs
    -- Same object, or same player, however either was tagged. Two object tags
    -- (ToCreature / ToObject) name one object; ToPlayer is the only player tag,
    -- so those compare whole.
    names a b = case (Recipient.objectOf a, Recipient.objectOf b) of
      (Just x, Just y) -> x == y
      (Nothing, Nothing) -> a == b
      _ -> False

-- CR 701.3a through CR 608.2h: could `src` be attached to `host` -- as `host` is
-- now, or, once `host` no longer exists, as it MOST RECENTLY existed? Auratouched
-- Mage's "an Aura card that could enchant it" asked of a Mage that was killed
-- while its own trigger was on the stack, which rule 608.2h answers with the last
-- known information of the object the ability names.
--
-- NOT folded into attachmentFor above, and that is the whole shape of this
-- function. Rule 608.2h is scoped to an EFFECT that requires information about a
-- specific object; attachmentFor's other callers are not that -- CR 704.5m's
-- state-based re-check, CR 701.3b's move, CR 303.4k's face-up rewrite and CR
-- 303.4f's host choice all ask what is legal ON THE BOARD, and a dead host is not
-- a place a permanent may be attached under any of them. A fallback inside
-- attachmentFor would make one legal for every one of them.
--
-- The Aura branch alone, where attachmentFor has an Equipment branch beside it:
-- the only reader is CR 701.3a's search-side question, Pawl.CardSpec's position
-- lint keeps Filter.CanAttachToSubject inside a search's filter, and the one
-- search that names it looks for an Aura. An Equipment answers False here, which
-- is the direction that finds LESS than printed rather than more.
--
-- The subtype, the CR 303.4d creature test and the enchant ability are all read
-- off `src`, which is a card in the library and is very much still there; only
-- the HOST is read through rule 608.2h. So this is attachmentFor's Aura branch
-- with its last step -- membership of a live slot's candidate set -- replaced by
-- Target.lastKnownAdmits, which asks the same slot the same question about an
-- object it can no longer enumerate.
attachableWithLastKnown :: ObjectId -> ObjectId -> GameState -> Bool
attachableWithLastKnown src host gs = case Projection.lastKnownOf host gs of
  Nothing -> Maybe.isJust (attachmentFor src (Recipient.ToObject host) gs)
  Just _ ->
    src /= host
      && Set.member Subtype.Aura (Projection.subtypesOf src gs)
      && not (Projection.isCreatureOf src gs)
      && Maybe.maybe
        False
        (\slot -> Target.lastKnownAdmits (Projection.controllerOf src gs) src slot host gs)
        (Card.foldEnchant (Projection.enchantOf src gs))

-- The destinations a Filter admits for a permanent being attached: battlefield
-- permanents matching it, ascending, less the one the subject already holds.
--
-- That exclusion is CR 701.3b's second sentence -- attaching a permanent to what
-- it is already attached to "does nothing" -- and it is also how Crown of the
-- Ages' "ANOTHER creature" is spelled, so a card omitting the word would behave
-- identically.
--
-- One candidate's VIEW carries the one field a projection cannot fill: whether
-- the SUBJECT could legally be attached here (CR 701.3a). Answered by
-- attachmentFor, the same function a move goes through, so an offer and a move
-- cannot disagree. Lazy, so a filter that never names Filter.CanHostSubject pays
-- nothing for it.
--
-- ASCENDING, so both the single-candidate elision at chooseHost and a transcript
-- are deterministic.
--
-- The filter Context is the ASKING ability's -- CR 109.5's "you" is `controller`
-- and IsSource is `source` -- rather than the subject's, because the destination
-- filter is that ability's card text. The two coincide for CR 303.4k, where the
-- Aura's own rider is asking about the Aura.
hostsFor :: PlayerId -> ObjectId -> ObjectId -> Filter.Type.Filter Keyword.Keyword -> GameState -> [ObjectId]
hostsFor controller source subject filter_ gs =
  let host = Game.lookupObject subject gs >>= Object.attachedTo >>= Recipient.objectOf
      context = Filter.contextFor (Just controller) (Just source)
      viewOf oid =
        (Projection.viewOfObject oid gs)
          { Filter.canHostSubject = Maybe.isJust (attachmentFor subject (Recipient.ToObject oid) gs)
          }
   in List.sort
        ( filter
            (\oid -> Just oid /= host && Filter.matches context (viewOf oid) filter_)
            (Set.toList (GameState.battlefield gs))
        )

-- CR 303.4k: the destinations an Aura that is BEING TURNED FACE UP may become
-- attached to, for a rider whose own text is `filter_` ("a creature").
--
-- The whole rule is in the two conjuncts and in when this is called. "The Aura's
-- controller considers the characteristics of that Aura AS IT WOULD EXIST IF IT
-- WERE FACE UP to determine what it may be attached to": there is no
-- counterfactual here because Pawl.Engine.FaceDown.performTurnFaceUp writes the
-- face-up status BEFORE it raises the event this answers, so every read below --
-- the subtypes, the enchant ability, Filter.CanHostSubject -- already sees the
-- face-up object. Raise the event first and CR 708.2a answers instead: a 2/2
-- creature with no text has no enchant ability, so the list would be empty and
-- the Aura would be buried by CR 704.5m. That is Pawl.FaceDownSpec's
-- discriminator for this rule.
--
-- "and they must choose a legal object or player ACCORDING TO THE AURA'S ENCHANT
-- ABILITY and any other applicable effects" is the Filter.CanHostSubject
-- conjunct, added HERE rather than written into the card. Gift of Doom says only
-- "a creature"; rule 303.4k, not the card, supplies the enchant-ability
-- narrowing -- and, through attachmentFor, the destination's own limits (CR
-- 303.4, Pawl.Engine.AttachRestriction) -- which is the difference from
-- Effect.AttachTarget -- there a card
-- that does not say "it can enchant" gets every destination its text admits and
-- CR 303.4j refuses the illegal move afterwards. Rule 303.4k leaves no such
-- backstop open, so an offer that had to be refused would be this engine
-- inventing one.
--
-- CR 303.4f -- changeZoneAttaching's Aura entry -- is that same narrowing with NO
-- card text to intersect, since there the enchant ability IS the whole restriction.
-- So it asks hostsFor with a bare Filter.CanHostSubject rather than through here.
turnUpHosts :: PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Keyword -> GameState -> [ObjectId]
turnUpHosts controller aura filter_ =
  hostsFor controller aura aura (Filter.Type.And [filter_, Filter.Type.CanHostSubject])

-- Which of the offered destinations the player picks, or Nothing when the text
-- admits none (CR 609.3: the effect does as much as it can, and that is nothing).
--
-- `controller` is WHO IS ASKED and is the caller's to decide: the resolving
-- controller for an effect naming one destination (CR 608.2d), and the subject's
-- own controller where CR 303.4d or CR 301.5c reassigns it -- see arbitrate.
--
-- ELIDED AT ONE CANDIDATE, the Prompt.ChooseAttachment posture: with a single
-- destination there is nothing to decide. The current host is never among the
-- candidates (CR 701.3b), so it is not being withheld.
--
-- FILTERED, NOT TRUSTED: an answer naming something that was never offered falls
-- back to the first candidate, since a caller that got this far must pick
-- something.
chooseHost :: PlayerId -> ObjectId -> [ObjectId] -> Game (Maybe ObjectId)
chooseHost controller subject candidates = case candidates of
  [] -> pure Nothing
  first : rest -> case rest of
    [] -> pure (Just first)
    second : more -> do
      gs <- State.get
      let offered = first NonEmpty.:| (second : more)
      answer <- Game.choose (Prompt.ChooseAttachment (Decide.deciderFor controller gs) controller subject offered)
      pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))

-- CR 303.4d and CR 301.5c, whose closing sentences are the same rule twice: "an
-- Aura can't enchant more than one object or player. If a spell or ability would
-- cause an Aura to become attached to more than one object or player, the Aura's
-- controller chooses which object or player it becomes attached to", and the
-- Equipment wording with "equip" for "enchant". An effect that NAMES several
-- destinations hands them here and gets back the one destination the subject
-- keeps.
--
-- The whole content over chooseHost is WHOSE choice it is. CR 608.2d gives every
-- other resolution-time choice to the resolving controller, which is what
-- chooseHost's own callers pass; these two rules take this one away from them and
-- give it to the controller of the thing being attached. So the SUBJECT's
-- controller is read here and nowhere else, and the caller keeps passing its own
-- controller to hostsFor -- CR 109.5's "you" on the destination filter is still
-- the asking ability's card text.
--
-- Subtype-blind on purpose: the two rules agree, so nothing here asks whether the
-- subject is an Aura or an Equipment. A permanent that is neither cannot be
-- attached at all (CR 701.3b), and attachmentFor refuses it downstream.
--
-- A subject with no controller has nobody to make CR 303.4d's choice, so nothing
-- moves. Unreachable: the subject is a battlefield permanent, and
-- Projection.controllerOf answers Nothing only for an object that is not there.
arbitrate :: ObjectId -> [ObjectId] -> Game (Maybe ObjectId)
arbitrate subject candidates = do
  gs <- State.get
  Maybe.maybe (pure Nothing) (\chooser -> chooseHost chooser subject candidates) (Projection.controllerOf subject gs)
