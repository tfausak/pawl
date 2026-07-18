module Pawl.Resolve where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Damage as Damage
import qualified Pawl.Game as Game
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Target as Target
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.DamageEvent as DamageEvent
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.Effect as Effect
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.SlotName (SlotName)
import qualified Pawl.Type.Zone as Zone

-- THE ONE LEGITIMATE HOME of `case effect of`: this module is the VM's opcode
-- semantics (design.md section 1). Everything else asks classifications. The
-- executor itself arrives with resolution; slotsOf is the read half of the
-- dataflow lint.
slotsOf :: Effect -> Set SlotName
slotsOf effect = case effect of
  Effect.DealDamage slot _ -> Set.singleton slot

-- CR 608.2b then CR 608.2: re-validate every filled slot against its spec; if
-- the spell has slots and ALL are now illegal, it does not resolve -- it moves
-- to the graveyard with no effect applied (the fizzle). Otherwise the effects
-- run in order (CR 608.2c), each skipping a slot whose target is illegal
-- (illegal targets are unaffected; other parts still happen), and the spell
-- goes to its owner's graveyard as the final part of resolution (CR 608.2n).
resolveSpell :: ObjectId -> GameState -> GameState
resolveSpell oid gs =
  let bury = Game.changeZone oid Zone.Graveyard
   in case Game.lookupObject oid gs of
        Nothing -> gs
        Just obj -> case Game.cardOf oid gs of
          Nothing -> gs
          Just card ->
            let specs = Card.targetSpecs card
                chosen = Object.targets obj
                legalSlot slot recipient = case Map.lookup slot specs of
                  Nothing -> False
                  Just spec -> Target.stillLegal recipient spec gs
                legality = Map.mapWithKey legalSlot chosen
                fizzles = not (Map.null specs) && not (or (Map.elems legality))
             in if fizzles
                  then bury gs
                  else bury (List.foldl' (applyEffect oid legality chosen) gs (Card.effects card))

-- One effect, applied. The case on the constructor is THIS module's charter.
applyEffect :: ObjectId -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> GameState -> Effect -> GameState
applyEffect source legality chosen gs effect = case effect of
  Effect.DealDamage slot quantity ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case Quantity.evaluate gs source quantity of
        -- An unevaluable quantity is a no-op, the powerOf posture.
        Nothing -> gs
        Just n ->
          if n <= 0
            then gs
            -- The applied effect IS the event (the M3a spec, section 4):
            -- constructing this DamageEvent and funneling it is the whole
            -- application. CR 120.3e / 120.3a live in applyDamage.
            else Damage.applyDamage [DamageEvent.MkDamageEvent source recipient (fromInteger n)] gs
      _ -> gs
