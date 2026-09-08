module Pawl.Types.DestructionRewrite where

-- | CR 614.8 / 701.19a: how a replacement rewrites a would-be-destroyed event.
-- Under every arm the destruction itself does not happen, so nothing downstream
-- of it (a put-into-graveyard, and therefore Rest in Peace) ever runs.
data DestructionRewrite
  = Regenerate
  | -- | CR 122.1c: "if this permanent would be destroyed as the result of an
    -- effect, instead remove a shield counter from it". Engine-minted from the
    -- counters (Pawl.Engine.Projection.shieldOf), never authored -- the card that
    -- puts the counter on says nothing about the effect it creates, rule 122.1c
    -- does.
    --
    -- A different arm from Regenerate rather than a Regenerate with a rider, for
    -- the reason Pawl.Types.Regenerability's comment already gives: CR 701.19c
    -- names regeneration shields specifically, so a destruction replacement that
    -- is NOT a regeneration must still apply to a destruction that can't be
    -- regenerated. It also does none of regeneration's own work -- no tap, no
    -- damage wipe, no removal from combat (CR 701.19a).
    --
    -- The rule's "as the result of an effect" is not in this constructor but in
    -- Pawl.Engine.Replacement.admits, which reads the DESTRUCTION's own
    -- Pawl.Types.DestructionCause: the restriction is on which events the effect
    -- applies to, exactly as CR 701.19c's is.
    RemoveShieldCounter
  | -- | CR 702.89a: "if enchanted permanent would be destroyed, instead remove
    -- all damage marked on it and destroy this Aura". Engine-minted from
    -- Pawl.Types.Keyword's UmbraArmor onto the AURA (Pawl.Engine.Keyword's
    -- mintedReplacementsFor), never authored.
    --
    -- The first arm that is not self-scoped: the other two replace their own
    -- source's destruction, where this one replaces the destruction of whatever
    -- its source is attached to. Pawl.Engine.Replacement.scopes is where that
    -- difference lives, so the Aura stays the row's source and CR 616.1's choice
    -- between two umbra armors is a choice between two candidates.
    --
    -- Does none of regeneration's other work (CR 701.19a): rule 702.89a removes
    -- the damage and stops, so the permanent keeps its tap state and its place
    -- in combat. It states no restriction on which destructions it reaches
    -- either, so Pawl.Engine.Replacement.admits refuses it neither cause nor an
    -- unregeneratable destruction.
    UmbraArmor
  deriving (Bounded, Enum, Eq, Ord, Show)
