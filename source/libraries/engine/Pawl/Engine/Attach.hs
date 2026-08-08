-- Rule 701.3's attach, in one place because two callers a library apart need the
-- same answer: Pawl.Engine.Resolve's Attach and AttachTarget opcodes, and
-- Pawl.Engine.Event's CR 303.4k arm for an Aura being turned face up. Event sits
-- BELOW Resolve, so the shared half cannot live in Resolve where it started.
--
-- THE INVARIANT: nothing here asks which CARD is moving. It reads the
-- PROJECTION's subtypes (CR 205.3) and the enchant ability rule 702.5a gives an
-- Aura, both of which are classifications the rulebook itself makes -- the same
-- standing Pawl.Engine.Keyword has over rule 702.
module Pawl.Engine.Attach where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
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
-- CR 701.3a's last sentence is the whole rule -- an Aura, Equipment or
-- Fortification can't be attached to something it couldn't enchant, equip or
-- fortify -- so this dispatches on which `src` is, read through the PROJECTION, so
-- that an Equipment that lost the subtype (CR 301.5c) and a permanent animated
-- into a creature both answer correctly.
--
-- Equipment is CR 301.5's creature test. Aura is CR 303.4's enchant ability, asked
-- through Target.admittedRecipients rather than a hand-rolled creature test, which
-- honours an enchant spec that narrows further for free. Admission and not target
-- legality: CR 702.5a gives the enchant ability both jobs and this is the second,
-- so rule 702's targeting restrictions do not reach here. CR 109.5's "you" on that
-- spec is the AURA's controller, not the moving effect's. An Aura with no enchant
-- ability cannot arise -- the CardSpec lint holds the biconditional.
--
-- Read through Game.faceOf, so CR 708.2a's substitution applies: a permanent that
-- is still face down has no enchant ability and no Aura subtype, and this answers
-- Nothing for every destination. That is not an edge case here but the content of
-- CR 303.4k -- see turnUpHosts below.
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
-- answer from the spec's candidate list is what keeps Sba's CR 303.4c re-check
-- able to compare against that same list later, so `destination` is matched by
-- which object or player it names rather than by how the caller tagged it.
attachmentFor :: ObjectId -> Recipient -> GameState -> Maybe Recipient
attachmentFor src destination gs
  | Recipient.objectOf destination == Just src = Nothing
  -- CR 301.5, "it can't legally be attached to anything that isn't a creature" --
  -- which is also why a player destination falls to Nothing here rather than
  -- getting a branch of its own.
  | Set.member Subtype.Equipment subtypes = case Recipient.objectOf destination of
      Just oid | Projection.isCreatureOf oid gs -> Just (Recipient.ToCreature oid)
      _ -> Nothing
  | Set.member Subtype.Aura subtypes =
      if Projection.isCreatureOf src gs
        then Nothing
        else case Game.faceOf src gs >>= Card.enchantSpec of
          Nothing -> Nothing
          Just spec ->
            List.find
              (names destination)
              (Set.toList (Target.admittedRecipients (Projection.controllerOf src gs) src spec gs))
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
      context = Filter.MkContext (Just controller) (Just source)
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
-- counterfactual here because Pawl.Engine.FaceDown.turnFaceUp writes the
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
-- narrowing, which is the difference from Effect.AttachTarget -- there a card
-- that does not say "it can enchant" gets every destination its text admits and
-- CR 303.4j refuses the illegal move afterwards. Rule 303.4k leaves no such
-- backstop open, so an offer that had to be refused would be this engine
-- inventing one.
turnUpHosts :: PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Keyword -> GameState -> [ObjectId]
turnUpHosts controller aura filter_ =
  hostsFor controller aura aura (Filter.Type.And [filter_, Filter.Type.CanHostSubject])

-- Which of the offered destinations the player picks, or Nothing when the text
-- admits none (CR 609.3: the effect does as much as it can, and that is nothing).
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

-- CR 701.3b and CR 701.3c: store the attachment and restamp.
--
-- CR 303.4j for an Aura -- "the Aura doesn't move" -- and CR 701.3b's first
-- sentence for the rest. A FAILURE MODE, not a fizzle: the only thing that does
-- not happen is the move, and in particular the subject stays attached to its old
-- host rather than becoming unattached, so CR 704.5m has nothing to bury.
--
-- CR 701.3c: attaching to a DIFFERENT object gives it a new timestamp, which CR
-- 613.7 orders layer effects by. The caller is responsible for not calling this
-- with the host the subject already has -- CR 701.3b's "does nothing" would
-- otherwise become a restamp.
attach :: ObjectId -> Recipient -> Game ()
attach subject destination = do
  gs <- State.get
  case attachmentFor subject destination gs of
    Nothing -> pure ()
    Just attachment -> do
      let (ts, gs1) = Game.freshTimestamp gs
          move o = o {Object.attachedTo = Just attachment, Object.timestamp = ts}
      State.put gs1 {GameState.objects = Map.adjust move subject (GameState.objects gs1)}
