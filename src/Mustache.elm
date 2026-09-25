module Mustache exposing (Node(..), render)

{-| Rendering mustache templates

@docs Node, render

-}

import Parser exposing ((|.), (|=), Parser)


{-| Represent mustache variables
-}
type Node
    = Variable String String
    | Section String Bool


type SyntaxNode
    = TextNode Int String
    | VariableNode String
    | CommentNode
    | OpenSectionNode String
    | CloseSectionNode String


{-| Render a template using a list of variables
-}
render : List Node -> String -> String
render nodes template =
    case Parser.run mustacheParser template of
        Ok syntaxNodes ->
            let
                getValue : String -> String
                getValue variableName =
                    nodes
                        |> List.filterMap
                            (\node ->
                                case node of
                                    Variable name value ->
                                        if name == variableName then
                                            Just value

                                        else
                                            Nothing

                                    Section _ _ ->
                                        Nothing
                            )
                        |> List.head
                        |> Maybe.withDefault ""

                showSection : String -> Bool
                showSection sectionName =
                    nodes
                        |> List.filterMap
                            (\node ->
                                case node of
                                    Section name show ->
                                        if name == sectionName then
                                            Just show

                                        else
                                            Nothing

                                    Variable _ _ ->
                                        Nothing
                            )
                        |> List.head
                        |> Maybe.withDefault False
            in
            (syntaxNodes
                |> List.foldl
                    (\syntaxNode result ->
                        case syntaxNode of
                            TextNode _ nodeText ->
                                if result.skipUntilSectionClosed == Nothing then
                                    { result | renderedTemplate = result.renderedTemplate ++ nodeText }

                                else
                                    result

                            VariableNode variableName ->
                                if result.skipUntilSectionClosed == Nothing then
                                    { result | renderedTemplate = result.renderedTemplate ++ getValue variableName }

                                else
                                    result

                            OpenSectionNode sectionName ->
                                if showSection sectionName then
                                    result

                                else
                                    { result | skipUntilSectionClosed = Just sectionName }

                            CloseSectionNode sectionName ->
                                if result.skipUntilSectionClosed == Just sectionName then
                                    { result | skipUntilSectionClosed = Nothing }

                                else
                                    result

                            CommentNode ->
                                result
                    )
                    { renderedTemplate = "", skipUntilSectionClosed = Nothing }
            ).renderedTemplate

        Err _ ->
            "ERROR parsing template!\n" ++ template


mustacheParser : Parser (List SyntaxNode)
mustacheParser =
    let
        parseNodes : List SyntaxNode -> Parser (Parser.Step (List SyntaxNode) (List SyntaxNode))
        parseNodes nodes =
            Parser.oneOf
                [ Parser.succeed
                    (\offset node ->
                        if (nodes |> List.map getOffset |> List.head) == Just offset then
                            Parser.Done (List.reverse nodes)

                        else
                            Parser.Loop (node :: nodes)
                    )
                    |= Parser.getOffset
                    |= Parser.oneOf [ openSectionParser, closeSectionParser, commentParser, variableParser, textParser ]
                ]

        getOffset : SyntaxNode -> Int
        getOffset node =
            case node of
                TextNode offset _ ->
                    offset

                _ ->
                    0
    in
    Parser.loop [] parseNodes


textParser : Parser SyntaxNode
textParser =
    let
        stringParser : Parser String
        stringParser =
            Parser.chompUntilEndOr "{{"
                |> Parser.getChompedString
    in
    Parser.succeed TextNode
        |= Parser.getOffset
        |= stringParser


variableParser : Parser SyntaxNode
variableParser =
    Parser.succeed VariableNode
        |. Parser.symbol "{{"
        |. Parser.spaces
        |= nameParser
        |. Parser.symbol "}}"


commentParser : Parser SyntaxNode
commentParser =
    Parser.succeed CommentNode
        |. Parser.symbol "{{!"
        |. Parser.spaces
        |. Parser.chompUntil "}}"
        |. Parser.symbol "}}"


openSectionParser : Parser SyntaxNode
openSectionParser =
    Parser.succeed OpenSectionNode
        |. Parser.symbol "{{#"
        |. Parser.spaces
        |= nameParser
        |. Parser.symbol "}}"


closeSectionParser : Parser SyntaxNode
closeSectionParser =
    Parser.succeed CloseSectionNode
        |. Parser.symbol "{{/"
        |. Parser.spaces
        |= nameParser
        |. Parser.symbol "}}"


nameParser : Parser String
nameParser =
    Parser.chompUntil "}}"
        |> Parser.getChompedString
        |> Parser.map String.trimRight
