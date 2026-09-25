module Spec.Inverted exposing (all)

import Expect
import Mustache
import Test exposing (Test, describe, test)


{-| Inverted Section tags and End Section tags are used in combination to wrap a
section of the template.

These tags' content MUST be a non-whitespace character sequence NOT
containing the current closing delimiter; each Inverted Section tag MUST be
followed by an End Section tag with the same content within the same
section.

This tag's content names the data to replace the tag. Name resolution is as
follows:

1.  Split the name on periods; the first part is the name to resolve, any
    remaining parts should be retained.
2.  Walk the context stack from top to bottom, finding the first context
    that is a) a hash containing the name as a key OR b) an object responding
    to a method with the given name.
3.  If the context is a hash, the data is the value associated with the
    name.
4.  If the context is an object and the method with the given name has an
    arity of 1, the method SHOULD be called with a String containing the
    unprocessed contents of the sections; the data is the value returned.
5.  Otherwise, the data is the value returned by calling the method with
    the given name.
6.  If any name parts were retained in step 1, each should be resolved
    against a context stack containing only the result from the former
    resolution. If any part fails resolution, the result should be considered
    falsey, and should interpolate as the empty string.
    If the data is not of a list type, it is coerced into a list as follows: if
    the data is truthy (e.g. `!!data == true`), use a single-element list
    containing the data, otherwise use an empty list.

This section MUST NOT be rendered unless the data list is empty.

Inverted Section and End Section tags SHOULD be treated as standalone when
appropriate.

-}
all : Test
all =
    describe "Mustache Inverted Sections (see https://github.com/mustache/spec/blob/v1.4.3/specs/inverted.yml)"
        [ test "Truthy" <|
            \_ ->
                Mustache.render [ Mustache.Section "boolean" False ] "\"{{^boolean}}This should be rendered.{{/boolean}}\""
                    |> Expect.equal (Just "\"This should be rendered.\"")
        , test "Falsey" <|
            \_ ->
                Mustache.render [ Mustache.Section "boolean" True ] "\"{{^boolean}}This should not be rendered.{{/boolean}}\""
                    |> Expect.equal (Just "\"\"")
        , test "Doubled (without proper whitespace handling)" <|
            \_ ->
                Mustache.render [ Mustache.Section "bool" False, Mustache.Variable "two" "second" ]
                    """{{^bool}}
* first
{{/bool}}
* {{two}}
{{^bool}}
* third
{{/bool}}"""
                    |> Expect.equal
                        (Just
                            """
* first

* second

* third
"""
                        )
        , test "Nested (Falsey)" <|
            \_ ->
                Mustache.render [ Mustache.Section "bool" False ]
                    "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
                    |> Expect.equal (Just "| A B C D E |")
        , test "Nested (Truthy)" <|
            \_ ->
                Mustache.render [ Mustache.Section "bool" True ]
                    "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
                    |> Expect.equal (Just "| A  E |")
        , test "Context Misses" <|
            \_ ->
                Mustache.render []
                    "[{{^missing}}Cannot find key 'missing'!{{/missing}}]"
                    |> Expect.equal (Just "[Cannot find key 'missing'!]")
        , describe "Whitespace Sensitivity"
            [ test "Surrounding Whitespace" <|
                \_ ->
                    Mustache.render [ Mustache.Section "boolean" False ]
                        " | {{^boolean}}\t|\t{{/boolean}} | \n"
                        |> Expect.equal (Just " | \t|\t | \n")
            , test "Internal Whitespace" <|
                \_ ->
                    Mustache.render [ Mustache.Section "boolean" False ]
                        " | {{^boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n"
                        |> Expect.equal (Just " |  \n  | \n")
            , test "Indented Inline Sections" <|
                \_ ->
                    Mustache.render [ Mustache.Section "boolean" False ]
                        " {{^boolean}}NO{{/boolean}}\n {{^boolean}}WAY{{/boolean}}\n"
                        |> Expect.equal (Just " NO\n WAY\n")
            ]
        , describe "Whitespace Insensitivity"
            [ test "Padding" <|
                \_ ->
                    Mustache.render [ Mustache.Section "boolean" False ]
                        "|{{^ boolean }}={{/ boolean }}|"
                        |> Expect.equal (Just "|=|")
            ]
        ]



{-
   notYetSupported : Test
   notYetSupported =
       describe "Sections"
           [ test "Doubled (with proper whitespace handling)" <|
               \_ ->
                   Mustache.render [ Mustache.Section "bool" False, Mustache.Variable "two" "second" ]
                       """
                                                              {{^bool}}
                                                              * first
                                                              {{/bool}}
                                                              * {{two}}
                                                              {{^bool}}
                                                              * third
                                                              {{/bool}}
                                                          """
                       |> Expect.equal
                           """
                                                              * first
                                                              * second
                                                              * third
                                                          """
           , describe "Whitespace Sensitivity"
               [ test "Standalone Lines" <|
                   \_ ->
                       Mustache.render [ Mustache.Section "boolean" False ]
                           """| This Is
   {{^boolean}}
   |
   {{/boolean}}
   | A Line"""
                           |> Expect.equal
                               """| This Is
   |
   | A Line"""
               , test "Standalone Indented Lines" <|
                   \_ ->
                       Mustache.render [ Mustache.Section "boolean" True ]
                           """| This Is
     {{^boolean}}
   |
     {{/boolean}}
   | A Line"""
                           |> Expect.equal
                               """| This Is
   |
   | A Line"""
               , test "Standalone Line Endings" <|
                   \_ ->
                       Mustache.render [ Mustache.Section "boolean" False ]
                           "|\u{000D}\n{{^boolean}}\u{000D}\n{{/boolean}}\u{000D}\n|"
                           |> Expect.equal
                               "|\u{000D}\n|"
               , test "Standalone Without Previous Line" <|
                   \_ ->
                       Mustache.render [ Mustache.Section "boolean" False ]
                           "  {{^boolean}}\n#{{/boolean}}\n/"
                           |> Expect.equal
                               "#\n/"
               , test "Standalone Without Newline" <|
                   \_ ->
                       Mustache.render [ Mustache.Section "boolean" False ]
                           "#{{^boolean}}\n/\n  {{/boolean}}"
                           |> Expect.equal
                               "#\n/\n"
               ]
           ]
-}
{-


    - name: Null is falsey
      desc: Null is falsey.
      data: { "null": null }
      template: '"{{^null}}This should be rendered.{{/null}}"'
      expected: '"This should be rendered."'

    - name: Context
      desc: Objects and hashes should behave like truthy values.
      data: { context: { name: 'Joe' } }
      template: '"{{^context}}Hi {{name}}.{{/context}}"'
      expected: '""'

    - name: List
      desc: Lists should behave like truthy values.
      data: { list: [ { n: 1 }, { n: 2 }, { n: 3 } ] }
      template: '"{{^list}}{{n}}{{/list}}"'
      expected: '""'

    - name: Empty List
      desc: Empty lists should behave like falsey values.
      data: { list: [ ] }
      template: '"{{^list}}Yay lists!{{/list}}"'
      expected: '"Yay lists!"'

   # Dotted Names

   - name: Dotted Names - Truthy
     desc: Dotted names should be valid for Inverted Section tags.
     data: { a: { b: { c: true } } }
     template: '"{{^a.b.c}}Not Here{{/a.b.c}}" == ""'
     expected: '"" == ""'

   - name: Dotted Names - Falsey
     desc: Dotted names should be valid for Inverted Section tags.
     data: { a: { b: { c: false } } }
     template: '"{{^a.b.c}}Not Here{{/a.b.c}}" == "Not Here"'
     expected: '"Not Here" == "Not Here"'

   - name: Dotted Names - Broken Chains
     desc: Dotted names that cannot be resolved should be considered falsey.
     data: { a: { } }
     template: '"{{^a.b.c}}Not Here{{/a.b.c}}" == "Not Here"'
     expected: '"Not Here" == "Not Here"'
-}
