import Browser
import Char
import Html
import Parser exposing (..)



-- MAIN


main =
  let
    input =
      "\"Hell\\u{006f}! What's up?\\r\\nThis >\\t< is a tab, that's what.\\r\\n\\u{1f600}\""

    result =
      run string input

    display text =
      [ Html.dt [] [ Html.text "Rendered:" ]
      , Html.dd [] [ Html.pre [] [ Html.text text ] ]
      ]
  in
  Html.dl [] <|
    [ Html.dt [] [ Html.text "Input:" ]
    , Html.dd [] [ Html.pre [] [ Html.text input ] ]
    , Html.dt [] [ Html.text "Parse result:" ]
    , Html.dd [] [ Html.pre [] [ Html.text <| Debug.toString result ] ]
    ]
      ++ (Result.map display result |> Result.withDefault [])


-- STRINGS


string : Parser String
string =
  succeed identity
    |. token "\""
    |= loop [] stringHelp


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
  oneOf
    [ succeed (\chunk -> Loop (chunk :: revChunks))
        |. token "\\"
        |= oneOf
            [ map (\_ -> "\n") (token "n")
            , map (\_ -> "\t") (token "t")
            , map (\_ -> "\r") (token "r")
            , succeed String.fromChar
                |. token "u{"
                |= unicode
                |. token "}"
            ]
    , token "\""
        |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
    , chompWhile isUninteresting
        |> getChompedString
        |> map (\chunk -> Loop (chunk :: revChunks))
    ]


isUninteresting : Char -> Bool
isUninteresting char =
  char /= '\\' && char /= '"'



-- UNICODE


unicode : Parser Char
unicode =
  getChompedString (chompWhile Char.isHexDigit)
    |> andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
  let
    length = String.length str
    code = String.foldl addHex 0 str
  in
  if length < 4 || length > 6 then
    problem "code point must have between 4 and 6 digits"
  else if 0 <= code && code <= 0x10FFFF then
    succeed (Char.fromCode code)
  else
    problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
  let
    code = Char.toCode char
  in
  if 0x30 <= code && code <= 0x39 then
    16 * total + (code - 0x30)
  else if 0x41 <= code && code <= 0x46 then
    16 * total + (10 + code - 0x41)
  else
    16 * total + (10 + code - 0x61)
