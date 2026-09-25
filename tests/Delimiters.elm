module Delimiters exposing (all)

import Expect
import Mustache
import Test exposing (Test, describe, test)


{-| Interpolation tags are used to integrate dynamic content into the template.

The tag's content MUST be a non-whitespace character sequence NOT containing
the current closing delimiter.

This tag's content names the data to replace the tag. A single period (`.`)
indicates that the item currently sitting atop the context stack should be
used; otherwise, name resolution is as follows:

1.  Split the name on periods; the first part is the name to resolve, any
    remaining parts should be retained.
2.  Walk the context stack from top to bottom, finding the first context
    that is a) a hash containing the name as a key OR b) an object responding
    to a method with the given name.
3.  If the context is a hash, the data is the value associated with the
    name.
4.  If the context is an object, the data is the value returned by the
    method with the given name.
5.  If any name parts were retained in step 1, each should be resolved
    against a context stack containing only the result from the former
    resolution. If any part fails resolution, the result should be considered
    falsey, and should interpolate as the empty string.
    Data should be coerced into a string (and escaped, if appropriate) before
    interpolation.

The Interpolation tags MUST NOT be treated as standalone.

-}
all : Test
all =
    describe "Mustache Interpolation (see https://github.com/mustache/spec/blob/v1.4.3/specs/interpolation.yml)"
        [ test "No Interpolation" <|
            \_ ->
                Mustache.render [] "Hello from {Mustache}!"
                    |> Expect.equal "Hello from {Mustache}!"
        , test "Basic Interpolation" <|
            \_ ->
                Mustache.render [ Mustache.Variable "subject" "world" ] "Hello, {{subject}}!"
                    |> Expect.equal "Hello, world!"
        , test "No Re-interpolation" <|
            \_ ->
                Mustache.render [ Mustache.Variable "template" "{{planet}}", Mustache.Variable "planet" "Earth" ]
                    "{{template}}: {{planet}}"
                    |> Expect.equal "{{planet}}: Earth"
        , test "HTML Escaping" <|
            \_ ->
                Mustache.render [ Mustache.Variable "forbidden" "& \" < >" ]
                    "These characters should be HTML escaped: {{forbidden}}"
                    |> Expect.equal "These characters should be HTML escaped: &amp; &quot; &lt; &gt;"
        , test "Triple Mustache" <|
            \_ ->
                Mustache.render [ Mustache.Variable "forbidden" "& \" < >" ]
                    "These characters should not be HTML escaped: {{{forbidden}}}"
                    |> Expect.equal "These characters should not be HTML escaped: & \" < >"
        , test "Ampersand" <|
            \_ ->
                Mustache.render [ Mustache.Variable "forbidden" "& \" < >" ]
                    "These characters should not be HTML escaped: {{&forbidden}}"
                    |> Expect.equal "These characters should not be HTML escaped: & \" < >"
        , describe "Context Misses"
            [ test "Basic Context Miss Interpolation" <|
                \_ ->
                    Mustache.render [] "I ({{cannot}}) be seen!"
                        |> Expect.equal "I () be seen!"
            , test "Triple Mustache Context Miss Interpolation" <|
                \_ ->
                    Mustache.render [] "I ({{{cannot}}}) be seen!"
                        |> Expect.equal "I () be seen!"
            , test "Ampersand Context Miss Interpolation" <|
                \_ ->
                    Mustache.render [] "I ({{&cannot}}) be seen!"
                        |> Expect.equal "I () be seen!"
            ]
        , describe "Whitespace Sensitivity"
            [ test "Interpolation - Surrounding Whitespace" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "| {{string}} |"
                        |> Expect.equal "| --- |"
            , test "Triple Mustache - Surrounding Whitespace" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "| {{{string}}} |"
                        |> Expect.equal "| --- |"
            , test "Ampersand - Surrounding Whitespace" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "| {{&string}} |"
                        |> Expect.equal "| --- |"
            , test "Interpolation - Standalone" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "  {{string}}\n"
                        |> Expect.equal "  ---\n"
            , test "Triple Mustache - Standalone" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "  {{{string}}}\n"
                        |> Expect.equal "  ---\n"
            , test "Ampersand - Standalone" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "  {{&string}}\n"
                        |> Expect.equal "  ---\n"
            ]
        , describe "Whitespace Insensitivity"
            [ test "Interpolation With Padding" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "|{{ string }}|"
                        |> Expect.equal "|---|"
            , test "Triple Mustache With Padding" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "|{{{ string }}}|"
                        |> Expect.equal "|---|"
            , test "Ampersand With Padding" <|
                \_ ->
                    Mustache.render [ Mustache.Variable "string" "---" ] "|{{& string }}|"
                        |> Expect.equal "|---|"
            ]
        ]



{-
     - name: Basic Integer Interpolation
       desc: Integers should interpolate seamlessly.
       data: { mph: 85 }
       template: '"{{mph}} miles an hour!"'
       expected: '"85 miles an hour!"'

     - name: Triple Mustache Integer Interpolation
       desc: Integers should interpolate seamlessly.
       data: { mph: 85 }
       template: '"{{{mph}}} miles an hour!"'
       expected: '"85 miles an hour!"'

     - name: Ampersand Integer Interpolation
       desc: Integers should interpolate seamlessly.
       data: { mph: 85 }
       template: '"{{&mph}} miles an hour!"'
       expected: '"85 miles an hour!"'

     - name: Basic Decimal Interpolation
       desc: Decimals should interpolate seamlessly with proper significance.
       data: { power: 1.210 }
       template: '"{{power}} jiggawatts!"'
       expected: '"1.21 jiggawatts!"'

     - name: Triple Mustache Decimal Interpolation
       desc: Decimals should interpolate seamlessly with proper significance.
       data: { power: 1.210 }
       template: '"{{{power}}} jiggawatts!"'
       expected: '"1.21 jiggawatts!"'

     - name: Ampersand Decimal Interpolation
       desc: Decimals should interpolate seamlessly with proper significance.
       data: { power: 1.210 }
       template: '"{{&power}} jiggawatts!"'
       expected: '"1.21 jiggawatts!"'

     - name: Basic Null Interpolation
       desc: Nulls should interpolate as the empty string.
       data: { cannot: null }
       template: "I ({{cannot}}) be seen!"
       expected: "I () be seen!"

     - name: Triple Mustache Null Interpolation
       desc: Nulls should interpolate as the empty string.
       data: { cannot: null }
       template: "I ({{{cannot}}}) be seen!"
       expected: "I () be seen!"

     - name: Ampersand Null Interpolation
       desc: Nulls should interpolate as the empty string.
       data: { cannot: null }
       template: "I ({{&cannot}}) be seen!"
       expected: "I () be seen!"


    # Dotted Names

    - name: Dotted Names - Basic Interpolation
      desc: Dotted names should be considered a form of shorthand for sections.
      data: { person: { name: 'Joe' } }
      template: '"{{person.name}}" == "{{#person}}{{name}}{{/person}}"'
      expected: '"Joe" == "Joe"'

    - name: Dotted Names - Triple Mustache Interpolation
      desc: Dotted names should be considered a form of shorthand for sections.
      data: { person: { name: 'Joe' } }
      template: '"{{{person.name}}}" == "{{#person}}{{{name}}}{{/person}}"'
      expected: '"Joe" == "Joe"'

    - name: Dotted Names - Ampersand Interpolation
      desc: Dotted names should be considered a form of shorthand for sections.
      data: { person: { name: 'Joe' } }
      template: '"{{&person.name}}" == "{{#person}}{{&name}}{{/person}}"'
      expected: '"Joe" == "Joe"'

    - name: Dotted Names - Arbitrary Depth
      desc: Dotted names should be functional to any level of nesting.
      data:
        a: { b: { c: { d: { e: { name: 'Phil' } } } } }
      template: '"{{a.b.c.d.e.name}}" == "Phil"'
      expected: '"Phil" == "Phil"'

    - name: Dotted Names - Broken Chains
      desc: Any falsey value prior to the last part of the name should yield ''.
      data:
        a: { }
      template: '"{{a.b.c}}" == ""'
      expected: '"" == ""'

    - name: Dotted Names - Broken Chain Resolution
      desc: Each part of a dotted name should resolve only against its parent.
      data:
        a: { b: { } }
        c: { name: 'Jim' }
      template: '"{{a.b.c.name}}" == ""'
      expected: '"" == ""'

    - name: Dotted Names - Initial Resolution
      desc: The first part of a dotted name should resolve as any other name.
      data:
        a: { b: { c: { d: { e: { name: 'Phil' } } } } }
        b: { c: { d: { e: { name: 'Wrong' } } } }
      template: '"{{#a}}{{b.c.d.e.name}}{{/a}}" == "Phil"'
      expected: '"Phil" == "Phil"'

    - name: Dotted Names - Context Precedence
      desc: Dotted names should be resolved against former resolutions.
      data:
        a: { b: { } }
        b: { c: 'ERROR' }
      template: '{{#a}}{{b.c}}{{/a}}'
      expected: ''

    - name: Dotted Names are never single keys
      desc: Dotted names shall not be parsed as single, atomic keys
      data:
        a.b: c
      template: '{{a.b}}'
      expected: ''

    - name: Dotted Names - No Masking
      desc: Dotted Names in a given context are unvavailable due to dot splitting
      data:
        a.b: c
        a: { b: d }
      template: '{{a.b}}'
      expected: 'd'


   # Implicit Iterators

   - name: Implicit Iterators - Basic Interpolation
     desc: Unadorned tags should interpolate content into the template.
     data: "world"
     template: |
       Hello, {{.}}!
     expected: |
       Hello, world!

   - name: Implicit Iterators - HTML Escaping
     desc: Basic interpolation should be HTML escaped.
     data: '& " < >'
     template: |
       These characters should be HTML escaped: {{.}}
     expected: |
       These characters should be HTML escaped: &amp; &quot; &lt; &gt;

   - name: Implicit Iterators - Triple Mustache
     desc: Triple mustaches should interpolate without HTML escaping.
     data: '& " < >'
     template: |
       These characters should not be HTML escaped: {{{.}}}
     expected: |
       These characters should not be HTML escaped: & " < >

   - name: Implicit Iterators - Ampersand
     desc: Ampersand should interpolate without HTML escaping.
     data: '& " < >'
     template: |
       These characters should not be HTML escaped: {{&.}}
     expected: |
       These characters should not be HTML escaped: & " < >

   - name: Implicit Iterators - Basic Integer Interpolation
     desc: Integers should interpolate seamlessly.
     data: 85
     template: '"{{.}} miles an hour!"'
     expected: '"85 miles an hour!"'
-}
