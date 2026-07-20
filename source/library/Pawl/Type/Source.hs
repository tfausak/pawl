module Pawl.Type.Source where

import Pawl.Type.ActivatedAbility (ActivatedAbility)
import Pawl.Type.Card (Card)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Printing (Printing)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

data Source
  = OfCard Printing
  | -- CR 111.3/111.6: a token -- a permanent not represented by a card. Its
    -- characteristics ARE a Card (CR 111.3: effect-defined values are functionally
    -- equivalent to printed ones) with no Printing (a token isn't a card, CR
    -- 111.6). Game.cardOf returns this Card, so the whole projection/mana/combat/
    -- SBA pipeline reads a token with no special case. A future
    -- OfToken Card (Maybe Printing) carries a physical token's metadata when
    -- Printing grows any (spec section 8).
    OfToken Card
  | -- CR 602: an activated ability on the stack -- the source permanent's id plus
    -- the ability. The ability travels with the object so it resolves even if the
    -- source leaves (CR 608.2g; LKI is a future refinement).
    OfAbility ObjectId (ActivatedAbility Card)
  | -- CR 603.3: a triggered ability on the stack -- the source permanent's id
    -- plus the ability. Travels with the object so it resolves even if the source
    -- leaves (CR 603.3d).
    OfTrigger ObjectId (TriggeredAbility Card)
  deriving (Eq, Ord, Show)
