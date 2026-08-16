module Pawl.Types.Facing where

import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics

-- | CR 110.5: one of the four status categories a permanent always has a value
-- for -- face up or face down. TapState's sibling, and deliberately a second
-- type rather than more constructors on it: CR 110.5 makes the four categories
-- INDEPENDENT ("each permanent always has one of these values for each of these
-- categories"), so a tapped face-down permanent has to be sayable.
--
-- CR 110.5d scopes the STATUS to permanents, but this field is not so scoped:
-- CR 708.4 has an object turned face down BEFORE it is put onto the stack, so a
-- face-down SPELL carries the same value while it waits there. What CR 110.5d
-- rules out -- a face-down card in a hand, a library or a graveyard -- is ruled
-- out by nothing writing FaceDown there, and CR 400.7's new incarnation
-- (Object.newIncarnation) is what puts every such card back to FaceUp.
--
-- Face-down EXILE is a different thing wearing the same words, and this type is
-- not it: CR 406.3's face-down exiled card has no relation to a permanent's
-- face-down status, which CR 110.5d says in as many words. Object.exiledFaceDown
-- is that other thing, and its own haddock has the rest of the distinction.
data Facing
  = FaceUp
  | -- | CR 708.2: the FaceDown arm carries the characteristics the ability or
    -- rules that allowed the object to be face down LISTED for it, because that
    -- list is the whole of what the object is and nothing else in the game state
    -- records what turned it over. Morph and the rest list nothing and carry
    -- FaceDownCharacteristics.defaultValue.
    FaceDown FaceDownCharacteristics.FaceDownCharacteristics
  deriving (Eq, Ord, Show)

-- | CR 708.2a's face-down status -- what every producer that lists no
-- characteristics writes.
faceDown :: Facing
faceDown = FaceDown FaceDownCharacteristics.defaultValue

-- | Whether an object is face down at all, for a reader that wants the CR 110.5
-- STATUS rather than the list on it.
isFaceDown :: Facing -> Bool
isFaceDown x = case x of
  FaceUp -> False
  FaceDown _ -> True
