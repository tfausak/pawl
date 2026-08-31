module Pawl.Types.MovedKinds where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | WHICH counters a CR 122.5 move carries, and how many of each.
--
-- Every arm below is a way the printed text states WHICH counters cross, and
-- rule 122.5 answers each of them differently. Explorer's Cache says "move a
-- +1\/+1 counter", which settles the kind on the card, so nothing is asked --
-- that is 'Named'. Agent's Toolkit says "move a counter", naming none, which
-- leaves WHICH counter moves to the player (Pawl.Types.Prompt's
-- ChooseMovedCounter) -- that is 'Chosen'. Fate Transfer says "move all
-- counters", which names no kind and asks nothing either, because every kind
-- crosses -- that is 'Every'. Resourceful Defense says "move any number of
-- counters", which names neither a kind nor a count and asks for BOTH at once
-- (Pawl.Types.Prompt's ChooseMovedCounters) -- that is 'AnyNumber'. Scrounging
-- Bandar says "move any number of +1\/+1 counters", which names the kind on the
-- card and leaves the count to the player -- that is 'AnyNumberOfKind', 'Named'
-- and 'AnyNumber' each taken half way, and it is why 'AnyNumber' cannot stand
-- in for it: that arm offers every kind the first object bears, so it would let
-- the answerer move a counter the card never named, which is weaker than
-- printed in the answerer's favour. Spike Cannibal says "move all +1\/+1
-- counters", which names the kind on the card and asks nothing either, because
-- the whole tally of that one kind crosses -- that is 'EveryOfKind', 'Named'
-- and 'Every' each taken half way. Goldberry, River-Daughter says "move a
-- counter of each kind not on Goldberry", which names no kind and asks nothing
-- either, because what the DESTINATION already bears is what settles which
-- kinds cross -- that is 'EachAbsentKind', the one arm reading the second
-- object for anything but rule 122.5's own impossibilities. The others are not
-- that one narrowed: rule 122.5's second impossibility ("the first object
-- doesn't have the appropriate kind of counter on it") is the whole of the
-- check under 'Named', is vacuous under 'Chosen', 'AnyNumber' and
-- 'UpToOneChosen' since the kinds ON the object are what is offered, and under
-- 'Every', 'EveryOfKind', 'AnyNumberOfKind' and 'EachAbsentKind' is what
-- empties the batch rather than what forbids it.
--
-- Takesies says "move up to one counter from each permanent", which names no
-- kind and asks the player which one -- 'Chosen''s question -- but lets the
-- answer be NONE, and that is 'UpToOneChosen'. The PER-SOURCE half of that
-- sentence is not what makes it its own arm, though the issue that filed it read
-- it that way (see #2709): Pawl.Types.MoveCounters' `from` is an ObjectRef, so
-- every arm here is already performed once per first object, and 'Chosen' with a
-- count of one would take one counter off each permanent too. What no arm could
-- say is "UP TO": rule 122.5's move happens wherever it is possible, so a card
-- that lets the player leave a given first object alone is asking a question
-- none of the others ask -- #2702's zero-versus-one, read the other way round.
--
-- The count rides on the two arms that HAVE one rather than on a field beside
-- them, because no other arm has one to carry: "all counters" and "all +1/+1
-- counters" are a tally read off the first object and a Quantity is one number,
-- "any number" and "any number of +1\/+1 counters" are the player's answer rather
-- than the card's, and "a counter of each kind" and "up to one counter" fix the
-- count at one on the wording itself, so a field would have to be ignored under
-- every one of them and a card could write a count that means nothing.
--
-- 'Chosen' and 'AnyNumber' are two arms and not one because they ask different
-- questions: 'Chosen' settles the count on the card and asks WHICH KIND, where
-- 'AnyNumber' settles nothing and asks WHICH COUNTERS, so a single answer type
-- would leave one of the two over-specified. 'Chosen' therefore takes its whole
-- count out of the one kind picked, which is exact for a card that PRINTS a
-- count, since no printing that prints one prints anything but "a counter" --
-- Scryfall @oracle:\/(^|[^a-z])move [^.]*counter\/@ with
-- @unique=cards&include_extras=true@, 2026-08-30, classified by hand. That run's
-- kindless moves carrying more than one counter say "all counters" (Fate
-- Transfer, Nexus Mentality, The Ozolith -- 'Every'), "any number of counters"
-- (Resourceful Defense, Slippery Bogbonder, Oozeavite -- 'AnyNumber'), "one or
-- more counters" (Goldberry, River-Daughter's second ability, which is that arm
-- with zero excluded and is not written today, #2702), "a counter of each kind
-- not on Goldberry" (her first -- 'EachAbsentKind') or "up to one counter from
-- each permanent" (Takesies -- 'UpToOneChosen'), and NONE of them prints a count
-- above one -- Takesies' "up to one" is a cap, not a tally. A printing saying
-- "move two counters", where the player could take one +1\/+1 counter and one
-- shield counter, would refute that.
--
-- The same run's KIND-NAMING moves that leave the count to the player all say
-- "any number of +1\/+1 counters" -- Scrounging Bandar, Bioshift, Aetherborn
-- Marauder and Forgotten Ancient, every one of them 'AnyNumberOfKind'. Scrounging
-- Bandar and Bioshift are written today, the second one's "another target
-- creature with the same controller" being Filter.SameControllerAsBound over its
-- first slot; Aetherborn Marauder's "from other permanents you control" is a
-- group on the first side, which ObjectRef.EachMatching says; Forgotten
-- Ancient's "onto other creatures" is a group on the SECOND, which nothing says
-- (#2713).
--
-- @include_extras@ is what makes that run every printing rather than most of
-- one, and it is the parameter every sweep in this neighbourhood needs: without
-- it Scryfall drops the @unk@ set (Unknown Event, set type @funny@), which is
-- where both Oozeavite and Takesies are. @include_funny@ adds nothing beyond it,
-- checked the same day. docs\/design.md section 6 ranks an un-set printing below
-- a regular one and above a synthetic, so neither is outside "every printing".
-- The sweep this one replaced omitted the parameter and so missed exactly the two
-- printings that qualify the claim; see #2706.
--
-- 'EveryOfKind' is not 'Named' with a clever count. Black Panther, Wakandan
-- King's "all +1\/+1 counters" IS written that way -- a Quantity.AgainstSlot
-- reading the counters on the object a slot holds -- and that spelling needs a
-- slot to aim at, which a first side naming a whole GROUP of objects does not
-- have. The tally is per first object there, so the arm has to read it as it
-- performs each pair rather than evaluate one number for the sentence.
data MovedKinds
  = -- | Fate Transfer's "move all counters": every kind on the first object, the
    -- whole tally of each (CR 122.5).
    Every
  | -- | Explorer's Cache's "move a +1\/+1 counter": the kind the card names, that
    -- many of it (CR 122.5).
    Named (CounterKind.CounterKind Keyword.Keyword) Quantity.Quantity
  | -- | Spike Cannibal's "move all +1\/+1 counters": the kind the card names, the
    -- whole tally of it on each first object (CR 122.5).
    EveryOfKind (CounterKind.CounterKind Keyword.Keyword)
  | -- | Agent's Toolkit's "move a counter": one kind the player picks, that many
    -- of it (CR 122.5).
    Chosen Quantity.Quantity
  | -- | Resourceful Defense's "move any number of counters": however many of
    -- however many kinds the player picks (CR 122.5).
    AnyNumber
  | -- | Scrounging Bandar's "move any number of +1\/+1 counters": the kind the
    -- card names, however many of it the player picks (CR 122.5).
    AnyNumberOfKind (CounterKind.CounterKind Keyword.Keyword)
  | -- | Goldberry, River-Daughter's "move a counter of each kind not on
    -- Goldberry": one counter of each kind the DESTINATION does not already bear
    -- (CR 122.5).
    EachAbsentKind
  | -- | Takesies' "move up to one counter from each permanent": one counter of
    -- one kind the player picks, or none at all (CR 122.5).
    UpToOneChosen
  deriving (Eq, Ord, Show)

-- | The count the card asks for, for a caller that reads every Quantity an
-- effect holds. 'Nothing' under 'Every' and 'EveryOfKind', which ask for a tally
-- read off the first object rather than a number, under 'AnyNumber' and
-- 'AnyNumberOfKind', whose count is the player's answer, and under
-- 'EachAbsentKind' and 'UpToOneChosen', whose one the card never writes down.
quantityOf :: MovedKinds -> Maybe Quantity.Quantity
quantityOf x = case x of
  Every -> Nothing
  Named _ quantity -> Just quantity
  EveryOfKind _ -> Nothing
  Chosen quantity -> Just quantity
  AnyNumber -> Nothing
  AnyNumberOfKind _ -> Nothing
  EachAbsentKind -> Nothing
  UpToOneChosen -> Nothing

-- | The kind the CARD names, for a caller that reads every CounterKind an effect
-- holds -- 'quantityOf''s sibling, one field over. 'Nothing' wherever the card
-- writes no kind down: 'Every' reads the first object's whole tally,
-- 'EachAbsentKind' reads the destination's, and 'Chosen', 'AnyNumber' and
-- 'UpToOneChosen' ask the player.
kindOf :: MovedKinds -> Maybe (CounterKind.CounterKind Keyword.Keyword)
kindOf x = case x of
  Every -> Nothing
  Named kind _ -> Just kind
  EveryOfKind kind -> Just kind
  Chosen _ -> Nothing
  AnyNumber -> Nothing
  AnyNumberOfKind kind -> Just kind
  EachAbsentKind -> Nothing
  UpToOneChosen -> Nothing
