module Pawl.Types.PlayerAction where

-- | An act a player performs that a "whenever [a player] <acts>" trigger (CR
-- 603.2) watches, with no payload the trigger reads. Recorded by
-- Pawl.Types.GameEvent's PlayerActed and matched by
-- Pawl.Types.TriggerCondition's PlayerActs. Each constructor's comment names
-- the rule that places the moment of the act.
data PlayerAction
  = -- | CR 701.22d; CR 701.22b's scry 0 is none.
    Scry
  | -- | CR 701.25d; CR 701.25c's surveil 0 is none.
    Surveil
  | -- | CR 701.34a, once per proliferate, whether or not anything was chosen.
    Proliferate
  | -- | CR 706.1, once per instruction however many dice it rolls; CR 901.9d's
    -- planar die too. A reroll (CR 706.2b) is a roll of its own, under the player
    -- who throws it; Pawl.DiceSpec's Goblin Bookie group proves it. No result
    -- bar (CR 706.7's planar die fires it), and a CR 706.6 ignored die (Pixie
    -- Guide) still leaves its instruction a roll of one or more dice.
    RollDice
  | -- | CR 701.51c.
    OpenAttraction
  | -- | CR 702.159b.
    ClaimPrize
  | -- | CR 701.54d: the Ring tempted this player.
    TemptedByRing
  | -- | CR 701.68d, whatever counters landed; never on rule 701.68b's board.
    Blight
  | -- | CR 701.61a, only once the forage was carried out.
    Forage
  | -- | CR 702.143c: the special action, not CR 702.143d's effect.
    Foretell
  | -- | CR 701.59a.
    CollectEvidence
  | -- | CR 702.174c.
    GiveGift
  | -- | CR 701.66b: as rule 701.66a's delayed triggered ability is created.
    -- data\/scenarios\/event-trigger's "CR 701.66b the Adept fires as rule 701.66a's
    -- delayed ability is created, not when it returns the land" proves it.
    Earthbend
  | -- | CR 701.67c: on paying a waterbend cost, however it was paid.
    -- data\/scenarios\/event-trigger's "CR 701.67c and the same cost paid entirely in
    -- mana fires it just the same" proves the mana half.
    Waterbend
  | -- | CR 701.65b: only when the airbend exiled one or more objects.
    Airbend
  | -- | CR 702.189b: as a firebending ability the player controls resolves.
    Firebend
  | -- | CR 309.7: as the dungeon card is removed from the game.
    CompleteDungeon
  deriving (Bounded, Enum, Eq, Ord, Show)
