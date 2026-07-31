module Pawl.Types.Supertype where

-- CR 205.4a lists five supertypes. The one missing is Ongoing, which is
-- scheme-only (CR 205.4h) and folded into #131 with the rest of Archenemy.
--
-- Ordered as CR 205.4a lists them, which is alphabetical.
data Supertype
  = -- CR 205.4c: any land with this supertype is a basic land, and any land
    -- without it is nonbasic even if it has a basic land type.
    Basic
  | -- CR 205.4d: subject to CR 704.5j's legend rule, which Pawl.Sba implements.
    -- CR 205.4e's OTHER rule -- a legendary instant or sorcery can't be cast
    -- unless you control a legendary creature or planeswalker -- is not
    -- checked (#307).
    Legendary
  | -- CR 205.4g: "any permanent with the supertype 'snow' is a snow permanent."
    -- That is the whole of the rule -- no state-based action, unlike Legendary
    -- and World, and no casting restriction, unlike Legendary -- so the
    -- constructor plus Filter.HasSupertype answers "is this a snow permanent?",
    -- which is all Skred asks. The sentence CR 205.4g adds is the one that keeps
    -- this honest: a permanent without the supertype is nonsnow "regardless of
    -- its name", so nothing may shortcut through "Snow-Covered ...".
    --
    -- What snow cards ask about the OTHER way round -- the {S} symbol
    -- (CR 107.4h), and whether a MANA was produced by a snow source (CR 106.3)
    -- -- is not modelled (#467).
    Snow
  | -- CR 205.4f: subject to CR 704.5k's world rule, which Pawl.Sba implements
    -- (Sba.worldVictims). Unlike the legend rule that one asks nobody: it keeps
    -- the permanent that has had the supertype for the shortest time, which is
    -- a fact rather than a choice.
    World
  deriving (Eq, Ord, Show)
