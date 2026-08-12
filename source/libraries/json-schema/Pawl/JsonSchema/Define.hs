-- | Building a schema document, and the @$defs@ table that lets a recursive
-- type have one at all.
module Pawl.JsonSchema.Define where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonPointer.Pointer as Pointer
import qualified Pawl.JsonPointer.Token as Token
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Uri.Fragment as Fragment

-- | 'Nothing' marks a definition currently being built, 'Just' a finished one.
-- The distinction is what 'define' reads to break a cycle, and it holds a
-- 'Schema.Schema' rather than a bare value so a half-built definition cannot be
-- mistaken for a usable one.
type SchemaM = State.State (Map.Map Name.Name (Maybe Schema.Schema))

-- | Registers a definition and returns a reference to it. The name goes into
-- the map BEFORE the body is evaluated, so a re-entrant call finds it and
-- returns the reference instead of recurring -- which is the whole of how a
-- recursive type terminates.
--
-- Memoized on the rendered 'Name.Name' alone: a second 'define' call for a
-- name already present returns the first call's reference without running
-- its own body, even if that body differs. Two codecs naming the same type
-- would silently share whichever one ran first. Nothing enforces that they
-- can't; it holds today by the convention of one codec module per type.
define :: Name.Name -> SchemaM Schema.Schema -> SchemaM Schema.Schema
define name body = do
  definitions <- State.get
  if Map.member name definitions
    then pure (reference name)
    else do
      State.modify' (Map.insert name Nothing)
      schema <- body
      State.modify' (Map.insert name (Just schema))
      pure (reference name)

reference :: Name.Name -> Schema.Schema
reference name = Schema.fromPairs [Value.pair "$ref" (Value.text (fragment name))]

-- | A @$ref@ is a URI-reference holding a JSON Pointer, which is RFC 6901
-- section 6's fragment form: @#@, then the pointer, percent-encoded for a URI
-- fragment. Neither escaping is hand-rolled, and neither undoes the other --
-- @~@ and @\/@ are both legal fragment octets, so 'Pointer.encode'\'s @~0@ and
-- @~1@ survive 'Fragment.encode'. A 'Token.Token' stores unescaped text, so the
-- pointer escaping happens once, there. The @$defs@ KEY takes the same name
-- unescaped: it is a JSON object key, not a pointer.
fragment :: Name.Name -> Text.Text
fragment name =
  Text.pack
    . mappend "#"
    . Builder.toString
    . Fragment.encode
    . Pointer.encode
    . Pointer.MkPointer
    $ [Token.MkToken (Text.pack "$defs"), Token.MkToken (Name.unwrap name)]

run :: SchemaM Schema.Schema -> Value.Value
run m =
  let (root, definitions) = State.runState m Map.empty
   in Value.Object . Object.MkObject $
        [Value.pair "$schema" (Value.text (Text.pack "https://json-schema.org/draft/2020-12/schema"))]
          <> Schema.keywords root
          <> [Value.pair "$defs" (Value.Object (Object.MkObject (Maybe.mapMaybe definition (Map.toAscList definitions))))]

-- | A 'Nothing' entry -- a definition still in progress -- is dropped rather
-- than emitted, which would otherwise write a dangling @$ref@ no key answers.
-- Unreachable in practice: 'define' always overwrites its own 'Nothing' with
-- a 'Just' once its body finishes, so nothing ever calls 'run' mid-build.
definition :: (Name.Name, Maybe Schema.Schema) -> Maybe (Pair.Pair Value.Value)
definition (name, ms) = fmap (Pair.MkPair (String.MkString (Name.unwrap name)) . Schema.unwrap) ms
