module Pawl.Extra.Either where

-- | Converts 'Right' into 'Just' and 'Left' into 'Nothing'.
hush :: Either x a -> Maybe a
hush = either (const Nothing) Just
