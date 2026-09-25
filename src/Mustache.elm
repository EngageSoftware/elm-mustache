module Mustache exposing (Node(..), render)

{-| Rendering mustache templates

@docs Node, render

-}

import EscapeHtml
import Parser exposing ((|.), (|=), Parser)


{-| Represent mustache variables
-}
type Node
    = Variable String String
    | Section String Bool


type SyntaxNode
    = TextNode Int String
    | EscapedVariableNode String
    | UnescapedVariableNode String
    | CommentNode
    | OpenSectionNode String
    | OpenInvertedSectionNode String
    | CloseSectionNode


{-| Render a template using a list of variables
-}
render : List Node -> String -> Maybe String
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
            Just
                (syntaxNodes
                    |> List.foldl
                        (\syntaxNode result ->
                            case syntaxNode of
                                TextNode _ nodeText ->
                                    if List.all (\isShown -> isShown) result.openSections then
                                        { result | renderedTemplate = result.renderedTemplate ++ nodeText }

                                    else
                                        result

                                UnescapedVariableNode variableName ->
                                    if List.all (\isShown -> isShown) result.openSections then
                                        { result | renderedTemplate = result.renderedTemplate ++ getValue variableName }

                                    else
                                        result

                                EscapedVariableNode variableName ->
                                    if List.all (\isShown -> isShown) result.openSections then
                                        { result | renderedTemplate = result.renderedTemplate ++ EscapeHtml.escape (getValue variableName) }

                                    else
                                        result

                                OpenSectionNode sectionName ->
                                    { result | openSections = showSection sectionName :: result.openSections }

                                OpenInvertedSectionNode sectionName ->
                                    { result | openSections = (showSection sectionName == False) :: result.openSections }

                                CloseSectionNode ->
                                    { result | openSections = List.tail result.openSections |> Maybe.withDefault [] }

                                CommentNode ->
                                    result
                        )
                        { renderedTemplate = "", openSections = [] }
                ).renderedTemplate

        Err _ ->
            Nothing


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
                    |= Parser.oneOf [ openSectionParser, openInvertedSectionParser, closeSectionParser, commentParser, unescapedVariableParser, ampersandVariableParser, escapedVariableParser, textParser ]
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


escapedVariableParser : Parser SyntaxNode
escapedVariableParser =
    Parser.succeed EscapedVariableNode
        |. Parser.symbol "{{"
        |. Parser.spaces
        |= nameParser
        |. Parser.symbol "}}"


ampersandVariableParser : Parser SyntaxNode
ampersandVariableParser =
    Parser.succeed UnescapedVariableNode
        |. Parser.symbol "{{&"
        |. Parser.spaces
        |= nameParser
        |. Parser.symbol "}}"


unescapedVariableParser : Parser SyntaxNode
unescapedVariableParser =
    Parser.succeed UnescapedVariableNode
        |. Parser.symbol "{{{"
        |. Parser.spaces
        |= nameParser
        |. Parser.symbol "}}}"


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


openInvertedSectionParser : Parser SyntaxNode
openInvertedSectionParser =
    Parser.succeed OpenInvertedSectionNode
        |. Parser.symbol "{{^"
        |. Parser.spaces
        |= nameParser
        |. Parser.symbol "}}"


closeSectionParser : Parser SyntaxNode
closeSectionParser =
    Parser.succeed CloseSectionNode
        |. Parser.symbol "{{/"
        |. Parser.spaces
        |. nameParser
        |. Parser.symbol "}}"


nameParser : Parser String
nameParser =
    Parser.chompUntil "}}"
        |> Parser.getChompedString
        |> Parser.map String.trimRight
