# Elm-Mustache

An Elm library for [Mustache templates](https://mustache.github.io/).

Currently, this library support attempts limited support for [version 1.4.3 of the Mustache Spec](https://github.com/mustache/spec/tree/v1.4.3).

## Supported
 - Interpolation of string variables (e.g. `{{ name }}`)
 - Boolean sections and inverted sections (e.g. `{{# show }}` or `{{^ hide }}`)
 - HTML encoding (e.g. `{{ characters }}` renders `Abbot &amp; Costello`)
 - HTML escaping (e.g. `{{{ characters }}}` or `{{& characters }}`)
 - Comments (e.g. `{{! comment }}`)

## Not Supported
 - Partials
 - Lambdas
 - Blocks
 - Set Delimiter (e.g. `{{=<% %>=}}`)
 - Implicit Iterator (e.g. `{{.}}`)
 - Whitespace handling for standalone lines
 - Arbitrary data types


## Usage

```elm
import Mustache


template : String
template =
  "Hi, my name is {{ name }}.{{# show }} Show me!{{/ show }}"

evaluatedTemplate : Maybe String
evaluatedTemplate =
  Mustache.render
    [ Mustache.Variable "name" "John"
    , Mustache.Section "show" True
    ]
    template
    
evaluatedTemplate --> Just "Hi, my name is John. Show me!"
```
