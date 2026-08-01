module Pawl.Types.Supertype where

-- CR 205.4a lists five supertypes. The one missing is Ongoing, which is
-- scheme-only (CR 205.4h) and folded into #131 with the rest of Archenemy.
--
-- Ordered as CR 205.4a lists them, which is alphabetical.
data Supertype
  = -- CR 205.4c: any land with this supertype is a basic land, and any land
    -- without it is nonbasic even if it has a basic land type.
    Basic
  | -- CR 205.4d: subject to CR 704.5j's legend rule, which Pawl.Engine.Sba implements.
    -- CR 205.4e's OTHER rule -- a legendary instant or sorcery can't be cast
    -- unless you control a legendary creature or planeswalker -- is
    -- Pawl.Engine.Cast.legendaryRestrictionOk.
    Legendary
  | -- CR 205.4g: "any permanent with the supertype 'snow' is a snow permanent."
    -- That is the whole of the rule -- no state-based action, unlike Legendary
    -- and World, and no casting restriction, unlike Legendary -- so the
    -- constructor plus Filter.HasSupertype answers "is this a snow permanent?",
    -- which is all Skred asks. The sentence CR 205.4g adds is the one that keeps
    -- this honest: a permanent without the supertype is nonsnow "regardless of
    -- its name", so nothing may shortcut through "Snow-Covered ...".
    --
    -- What snow cards ask about the OTHER way round -- whether a MANA was
    -- produced by a snow source (CR 106.3), which CR 107.4h's {S} is paid with
    -- -- reads this same supertype, through Pawl.Engine.Mana.productionTagsGiven.
    -- CR 205.4g's PERMANENT scope is narrower than that question in general:
    -- CR 106.3 lets a spell be a source too. It is wide enough for every source
    -- pawl has, which is a fact about the pool rather than about the rule --
    -- see productionTagsGiven, which is where that reasoning is written out.
    Snow
  | -- CR 205.4f: subject to CR 704.5k's world rule, which Pawl.Engine.Sba implements
    -- (Sba.worldVictims). Unlike the legend rule that one asks nobody: it keeps
    -- the permanent that has had the supertype for the shortest time, which is
    -- a fact rather than a choice.
    World
  deriving (Eq, Ord, Show)
