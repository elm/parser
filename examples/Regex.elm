module Regex exposing (main)

import Char
import Html exposing (Html, text)
import List
import Parser exposing ((|.), (|=), Parser, Step(..), chompIf, chompWhile, end, getChompedString, loop, oneOf, run, succeed, symbol)



-- MAIN


main : Html msg
main =
    let
        runList parseList =
            case parseList of
                ( parser, inputStr ) :: xs ->
                    (Debug.toString <| run parser inputStr) ++ " " ++ runList xs

                _ ->
                    ""
    in
    text <|
        runList
            [ ( upperOrDigitExactly4, "AB12" )
            , ( imageFile, "hello-17.jpeg" )
            , ( email, "FP_FTW@lang.elm.com" )
            ]
            ++ (Debug.toString <| run betweenFishhooks "<trout> What is between the <tuna> fishhooks <salmon>?")



-- PARSERS


chompXCharsIf : Int -> (Char -> Bool) -> Parser ()
chompXCharsIf x f =
    if x == 0 then
        end

    else
        chompIf f |. chompXCharsIf (x - 1) f


oneOfList : List String -> Parser ()
oneOfList strList =
    let
        symbolParsers =
            List.map symbol strList
    in
    oneOf symbolParsers


{-| Parser for [A-Z0-9]{4}
-}
upperOrDigitExactly4 : Parser String
upperOrDigitExactly4 =
    let
        upperOrDigit =
            \c -> Char.isUpper c || Char.isDigit c
    in
    getChompedString <|
        succeed identity
            |. chompXCharsIf 4 upperOrDigit
            |. end


{-| Taken from <https://github.com/hecrj/html-parser/blob/master/src/Html/Parser.elm>
-}
chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore fn =
    Parser.chompIf fn
        |. Parser.chompWhile fn


{-| <https://github.com/kmarekspartz/char-extra/blob/1.0.0/src/Char/Extra.elm>
-}
isWhitespace : Char -> Bool
isWhitespace char =
    List.member char <| String.toList " \t\u{000D}\n"


{-| Parser for [^\\s]+.(jpg|png|gif|bmp)$
-}
imageFile : Parser String
imageFile =
    let
        nameChars =
            \c -> (not <| isWhitespace c) && (c /= '.')

        imageExtensions =
            [ ".jpg", ".jpeg", ".png", ".gif", ".bmp" ]
    in
    getChompedString <|
        succeed identity
            |. chompOneOrMore nameChars
            |. oneOfList imageExtensions
            |. end


repeat : Parser () -> Parser ()
repeat repeatableParser =
    let
        repeatHelp : () -> Parser (Step () ())
        repeatHelp _ =
            oneOf
                [ succeed (\_ -> Loop ())
                    |= repeatableParser
                , succeed ()
                    |> Parser.map (\_ -> Done ())
                ]
    in
    loop () repeatHelp


{-| Parser for mail address
^[a-zA-Z0-9.!#$%&’\*+/=?^\_\`{|}~-]+@[a-zA-Z0-9-]+(.[a-zA-Z0-9-]+)\*$
-}
email : Parser String
email =
    let
        validCharsBeforeAt =
            [ '.', '!', '#', '$', '%', '&', '’', '*', '+', '/', '=', '?', '^', '_', '`', '{', '}', '~', '-' ]

        isValidCharBeforeAt c =
            Char.isAlphaNum c || List.member c validCharsBeforeAt

        isValidCharAfterAt c =
            Char.isAlphaNum c || c == '-'

        domain =
            chompIf (\c -> c == '.') |. chompOneOrMore isValidCharAfterAt
    in
    getChompedString <|
        succeed identity
            |. chompOneOrMore isValidCharBeforeAt
            |. chompIf (\c -> c == '@')
            |. chompOneOrMore isValidCharAfterAt
            |. repeat domain
            |. end


{-| Extract all text between <fishooks>
<([^>]\*)>
-}
betweenFishhooks : Parser (List String)
betweenFishhooks =
    loop [] fishhooksHelp


fishhooksHelp : List String -> Parser (Step (List String) (List String))
fishhooksHelp revStrs =
    let
        insideFishhooks : Parser String
        insideFishhooks =
            getChompedString <| succeed () |. chompWhile (\c -> c /= '>')
    in
    oneOf
        [ succeed (\str -> Loop (str :: revStrs))
            |. chompWhile (\c -> c /= '<')
            |. chompIf (\c -> c == '<')
            |= insideFishhooks
            |. chompIf (\c -> c == '>')
            |. chompWhile (\c -> c /= '<')
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revStrs))
        ]
