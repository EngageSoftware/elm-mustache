module EscapeHtml exposing (escape)

{-| This library allows to escape html string and unescape named and numeric
character references (e.g. &gt;, &#62;, &x3e;) to the corresponding unicode
characters

#Definition

@docs escape

-}

{- This is based on https://github.com/marcosh/elm-html-to-unicode/tree/1.0.4
   Copyright (c) 2016 Marco Perone
-}

import Dict


{-| Escapes a string converting characters that could be used to inject XSS
vectors (<http://wonko.com/post/html-escaping>). At the moment we escape &, <, >,
", ', \`, !, @, $, %, (, ), =, +, [ and ]. We do _not_ escape space, { or }, since those have special meaning in
Mustache and are used in the official spec tests.

for example

escape "&<>"" == "&amp;&lt;&gt;&quot;"

-}
escape : String -> String
escape =
    convert escapeChars



{- Helper function that applies a converting function to a string as a list of
   characters
-}


convert : (List Char -> List Char) -> String -> String
convert convertChars string =
    string
        |> String.toList
        |> convertChars
        |> List.map String.fromChar
        |> String.concat



{- escapes the characters one by one -}


escapeChars : List Char -> List Char
escapeChars list =
    list
        |> List.map escapeChar
        |> List.concat



{- function that actually performs the escaping of a single character -}


escapeChar : Char -> List Char
escapeChar char =
    Maybe.withDefault [ char ] (Dict.get char escapeDictionary)



{- dictionary that keeps track of the characters that need to be escaped -}


escapeDictionary : Dict.Dict Char (List Char)
escapeDictionary =
    Dict.fromList <|
        List.map (\( char, string ) -> ( char, String.toList string ))
            [ ( '&', "&amp;" )
            , ( '<', "&lt;" )
            , ( '>', "&gt;" )
            , ( '"', "&quot;" )
            , ( '\'', "&#39;" )
            , ( '`', "&#96;" )
            , ( '!', "&#33;" )
            , ( '@', "&#64;" )
            , ( '$', "&#36;" )
            , ( '%', "&#37;" )
            , ( '(', "&#40;" )
            , ( ')', "&#41;" )
            , ( '=', "&#61;" )
            , ( '+', "&#43;" )
            , ( '[', "&#91;" )
            , ( ']', "&#93;" )

            --, ( ' ', "&#32;" )
            --, ( '{', "&#123;" )
            --, ( '}', "&#125;" )
            ]
