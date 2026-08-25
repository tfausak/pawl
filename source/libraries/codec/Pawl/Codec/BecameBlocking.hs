{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecameBlocking where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecameBlocking as BecameBlocking

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec BecameBlocking.BecameBlocking
codec = Fields.object $ do
  blocker <- Fields.required "blocker" ObjectId.codec BecameBlocking.blocker
  attacker <- Fields.required "attacker" ObjectId.codec BecameBlocking.attacker
  -- Defaulted rather than required, as DamageEvent's flags are: CR 509.1's
  -- declaration is the producer of all but a handful of these events, so the key
  -- rides only on CR 509.4's entry.
  putOntoBattlefield <- Fields.defaulted "putOntoBattlefield" False Common.boolean BecameBlocking.putOntoBattlefield
  -- Defaulted for the same reason, and to the same value: CR 509.1's declaration
  -- is the first thing that blocks any attacker, so every event it records
  -- carries this clear too.
  attackerWasBlocked <- Fields.defaulted "attackerWasBlocked" False Common.boolean BecameBlocking.attackerWasBlocked
  pure
    BecameBlocking.MkBecameBlocking
      { BecameBlocking.blocker = blocker,
        BecameBlocking.attacker = attacker,
        BecameBlocking.putOntoBattlefield = putOntoBattlefield,
        BecameBlocking.attackerWasBlocked = attackerWasBlocked
      }
