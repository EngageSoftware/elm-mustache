module Tests exposing (all)

import Expect
import Mustache
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Mustache test suite"
        [ render ]


render : Test
render =
    describe "Rendering"
        [ test "Variable" <|
            \_ ->
                Mustache.render [ Mustache.Variable "name" "John" ] "My name is {{ name }}."
                    |> Expect.equal "My name is John."
        , test "Variable without spaces" <|
            \_ ->
                Mustache.render [ Mustache.Variable "name" "John" ] "My name is {{name}}."
                    |> Expect.equal "My name is John."
        , test "Undefined variable does not render" <|
            \_ ->
                Mustache.render [] "My name is {{name}}."
                    |> Expect.equal "My name is ."
        , test "Section" <|
            \_ ->
                Mustache.render [ Mustache.Section "show" True ] "Hello{{# show }}, world.{{/ show }}"
                    |> Expect.equal "Hello, world."
        , test "Section without spaces" <|
            \_ ->
                Mustache.render [ Mustache.Section "show" True ] "Hello{{#show}}, world.{{/show}}"
                    |> Expect.equal "Hello, world."
        , test "Section with spaces in section name" <|
            \_ ->
                Mustache.render [ Mustache.Section "if show" True ] "Hello{{#if show}}, world.{{/if show}}"
                    |> Expect.equal "Hello, world."
        , test "Hide section" <|
            \_ ->
                Mustache.render [ Mustache.Section "show" False ] "Hello{{# show }}, world.{{/ show }}"
                    |> Expect.equal "Hello"
        , test "Undefined section does not render" <|
            \_ ->
                Mustache.render [] "Hello{{# show }}, world.{{/ show }}"
                    |> Expect.equal "Hello"
        , test "Hide nested section" <|
            \_ ->
                Mustache.render [ Mustache.Section "outer" True, Mustache.Section "inner" False ] "{{# outer }}Hello{{#inner}}, world{{/ inner }}{{/outer}}!"
                    |> Expect.equal "Hello!"
        , test "Show nested section" <|
            \_ ->
                Mustache.render [ Mustache.Section "outer" True, Mustache.Section "inner" True ] "{{# outer }}Hello{{#inner}}, world{{/ inner }}{{/outer}}!"
                    |> Expect.equal "Hello, world!"
        , test "Hide outer section" <|
            \_ ->
                Mustache.render [ Mustache.Section "outer" False, Mustache.Section "inner" True ] "{{# outer }}Hello{{#inner}}, world{{/ inner }}{{/outer}}!"
                    |> Expect.equal "!"
        ]
