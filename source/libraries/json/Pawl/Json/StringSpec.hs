module Pawl.Json.StringSpec where

import qualified Data.Text as Text
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.String as String
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.String" $ do
  Spec.describe s "decode" $ do
    Spec.it s "succeeds with an empty string" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"\"") . Just . String.MkString $ Text.pack ""

    Spec.it s "succeeds with a single character" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"a\"") . Just . String.MkString $ Text.pack "a"

    Spec.it s "succeeds with two characters" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"ab\"") . Just . String.MkString $ Text.pack "ab"

    Spec.it s "succeeds with an escaped quotation mark" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\\" \"") . Just . String.MkString $ Text.pack " \" "

    Spec.it s "succeeds with an escaped reverse solidus" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\\\ \"") . Just . String.MkString $ Text.pack " \\ "

    Spec.it s "succeeds with an escaped solidus" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\/ \"") . Just . String.MkString $ Text.pack " / "

    Spec.it s "succeeds with an escaped backspace" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\b \"") . Just . String.MkString $ Text.pack " \b "

    Spec.it s "succeeds with an escaped form feed" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\f \"") . Just . String.MkString $ Text.pack " \f "

    Spec.it s "succeeds with an escaped line feed" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\n \"") . Just . String.MkString $ Text.pack " \n "

    Spec.it s "succeeds with an escaped carriage return" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\r \"") . Just . String.MkString $ Text.pack " \r "

    Spec.it s "succeeds with an escaped tab" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\t \"") . Just . String.MkString $ Text.pack " \t "

    Spec.it s "fails with an unescaped tab" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \t \"") Nothing

    Spec.it s "succeeds with an escaped control character" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\u001f \"") . Just . String.MkString $ Text.pack " \x1f "

    Spec.it s "succeeds with an unnecessarily escaped character" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\u006F \"") . Just . String.MkString $ Text.pack " o "

    Spec.it s "succeeds with a surrogate pair" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\" \\uD834\\uDD1E \"") . Just . String.MkString $ Text.pack " \x1d11e "

    Spec.it s "fails with an unpaired surrogate" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"\\uD800\"") Nothing

    Spec.it s "fails with an unpaired low surrogate" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"\\uDC00\"") Nothing

    Spec.it s "fails with an invalid low surrogate" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"\\uD800\\u0000\"") Nothing

    Spec.it s "fails with a surrogate followed by an unescaped character" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"\\uD834x\"") Nothing

    Spec.it s "fails with an unknown escape" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"\\q\"") Nothing

    Spec.it s "fails with a lone reverse solidus" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"\\") Nothing

    Spec.it s "fails with an unterminated string" $ do
      Spec.assertEq s (Parsec.parseString String.decode "\"abc") Nothing

  Spec.describe s "encode" $ do
    Spec.it s "works with an empty string" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack "") "\"\""

    Spec.it s "works with one character" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack "a") "\"a\""

    Spec.it s "works with two characters" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack "ab") "\"ab\""

    Spec.it s "escapes a quotation mark" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \" ") "\" \\\" \""

    Spec.it s "escapes a reverse solidus" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \\ ") "\" \\\\ \""

    Spec.it s "does not escape a solidus" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " / ") "\" / \""

    Spec.it s "escapes a backspace" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \b ") "\" \\b \""

    Spec.it s "escapes a form feed" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \f ") "\" \\f \""

    Spec.it s "escapes a line feed" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \n ") "\" \\n \""

    Spec.it s "escapes a carriage return" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \r ") "\" \\r \""

    Spec.it s "escapes a tab" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \t ") "\" \\t \""

    Spec.it s "escapes a control character" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \x1f ") "\" \\u001f \""

    Spec.it s "escapes a null character" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \x0 ") "\" \\u0000 \""

    Spec.it s "does not escape a two-byte character" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \x80 ") "\" \x80 \""

    Spec.it s "does not escape a three-byte character" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \x800 ") "\" \x800 \""

    Spec.it s "does not escape a four-byte character" $ do
      Spec.assertEq s (Builder.toString . String.encode . String.MkString $ Text.pack " \x10000 ") "\" \x10000 \""
