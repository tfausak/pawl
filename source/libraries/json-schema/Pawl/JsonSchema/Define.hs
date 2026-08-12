module Pawl.JsonSchema.Define where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonPointer.Pointer as Pointer
import qualified Pawl.JsonPointer.Token as Token
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Uri.Fragment as Fragment

-- | 'Nothing' marks a definition currently being built. 'Just' is a finished
-- one. The distinction is what 'define' reads to break a cycle.
type SchemaM = State.State (Map.Map Name.Name (Maybe Schema.Schema))

-- | Registers a definition and returns a reference to it. The name goes into
-- the map BEFORE the body is evaluated, so a re-entrant call finds it and
-- returns the reference instead of recurring.
--
-- Memoized on the rendered 'Name.Name' alone: a second 'define' call for a
-- name already present returns the first call's reference without running
-- its own body, even if that body differs.
define :: Name.Name -> SchemaM Schema.Schema -> SchemaM Schema.Schema
define name body = do
  definitions <- State.get
  if Map.member name definitions
    then pure $ reference name
    else do
      State.modify' $ Map.insert name Nothing
      schema <- body
      State.modify' . Map.insert name $ Just schema
      pure $ reference name

reference :: Name.Name -> Schema.Schema
reference name = Schema.fromPairs [Value.pair "$ref" . Value.text $ fragment name]

fragment :: Name.Name -> Text.Text
fragment name =
  Text.pack
    . ('#' :)
    . Builder.toString
    . Fragment.encode
    . Pointer.encode
    . Pointer.MkPointer
    $ [Token.MkToken $ Text.pack "$defs", Token.MkToken $ Name.unwrap name]

run :: SchemaM Schema.Schema -> Value.Value
run m =
  let (root, definitions) = State.runState m Map.empty
   in Value.object $
        [Value.pair "$schema" $ Value.string "https://json-schema.org/draft/2020-12/schema"]
          <> Schema.keywords root
          <> [Value.pair "$defs" . Value.object . Maybe.mapMaybe definition $ Map.toAscList definitions]

definition :: (Name.Name, Maybe Schema.Schema) -> Maybe (Pair.Pair Value.Value)
definition (name, ms) = fmap (Value.pair (Text.unpack $ Name.unwrap name) . Schema.unwrap) ms
