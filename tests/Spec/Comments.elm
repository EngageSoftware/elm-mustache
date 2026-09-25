module Spec.Comments exposing (all)

import Expect
import Mustache
import Test exposing (Test, describe, test)


{-| Comment tags represent content that should never appear in the resulting
output.

The tag's content may contain any substring (including newlines) EXCEPT the
closing delimiter.

Comment tags SHOULD be treated as standalone when appropriate.

-}
all : Test
all =
    describe "Mustache Comments (see https://github.com/mustache/spec/blob/v1.4.3/specs/comments.yml)"
        [ describe "Inline comments"
            [ test "Inline" <|
                \_ ->
                    Mustache.render [] "12345{{! Comment Block! }}67890"
                        |> Expect.equal "1234567890"
            , test "Indented Inline" <|
                \_ ->
                    Mustache.render [] "  12 {{! 34 }}\n"
                        |> Expect.equal "  12 \n"
            , test "Surrounding Whitespace" <|
                \_ ->
                    Mustache.render [] "12345 {{! Comment Block! }} 67890"
                        |> Expect.equal "12345  67890"
            , test "Variable Name Collision" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "! comment" "1", Mustache.Variable "! comment " "2", Mustache.Variable "!comment" "3", Mustache.Variable "comment" "4" ]
                        "comments never show: >{{! comment }}<"
                        |> Expect.equal "comments never show: ><"
            ]
        ]



{-
   notYetSupported : Test
   notYetSupported =
       describe "Multiline comments"
           [ test "Multiline" <|
               \_ ->
                   Mustache.render []
                       """12345{{!
       This is a
       multi-line comment...
   }}67890"""
                       |> Expect.equal "1234567890"
           , test "Standalone" <|
               \_ ->
                   Mustache.render []
                       """Begin.
   {{! Comment Block! }}
   End."""
                       |> Expect.equal
                           """Begin.
   End."""
           , test "Indented Standalone" <|
               \_ ->
                   Mustache.render []
                       """Begin.
       {{! Indented Comment Block! }}
   End."""
                       |> Expect.equal
                           """Begin.
   End."""
           , test "Standalone Line Endings" <|
               \_ ->
                   Mustache.render [] "|\u{000D}\n{{! Standalone Comment }}\u{000D}\n|"
                       |> Expect.equal "|\u{000D}\n|"
           , test "Standalone Without Previous Line" <|
               \_ ->
                   Mustache.render [] "  {{! I'm Still Standalone }}\n!"
                       |> Expect.equal "!"
           , test "Standalone Without Newline" <|
               \_ ->
                   Mustache.render [] "!\n  {{! I'm Still Standalone }}"
                       |> Expect.equal "!\n"
           , test "Multiline Standalone" <|
               \_ ->
                   Mustache.render []
                       """Begin.
   {{!
   Something's going on here...
   }}
   End."""
                       |> Expect.equal
                           """Begin.
   End."""
           , test "Indented Multiline Standalone" <|
               \_ ->
                   Mustache.render []
                       """Begin.
     {{!
       Something's going on here...
     }}
   End."""
                       |> Expect.equal
                           """Begin.
   End."""
           ]
-}
