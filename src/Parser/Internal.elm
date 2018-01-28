module Parser.Internal exposing
  ( Parser(..)
  , Step(..)
  , State
  , chomp, chompDigits, chompDotAndExp
  , isBadIntEnd
  , isSubString, isSubChar, findSubString
  )


import Char
import Elm.Kernel.Parser



-- PARSERS


type Parser ctx x a =
  Parser (State ctx -> Step ctx x a)


type Step ctx x a
  = Good a (State ctx)
  | Bad x (State ctx)


type alias State ctx =
  { src : String
  , offset : Int
  , indent : Int
  , context : List ctx
  , row : Int
  , col : Int
  }



-- CHOMPERS


chomp : (Char -> Bool) -> Int -> String -> Int
chomp isGood offset source =
  let
    newOffset =
      isSubChar isGood offset source
  in
    if newOffset < 0 then
      offset

    else
      chomp isGood newOffset source



-- CHOMP DIGITS


chompDigits : (Char -> Bool) -> Int -> String -> Result Int Int
chompDigits isValidDigit offset source =
  let
    newOffset =
      chomp isValidDigit offset source
  in
    -- no digits
    if newOffset == offset then
      Err newOffset

    -- ends with non-digit characters
    else if isSubChar isBadIntEnd newOffset source /= -1 then
      Err newOffset

    -- all valid digits!
    else
      Ok newOffset


isBadIntEnd : Char -> Bool
isBadIntEnd char =
  Char.isDigit char
  || Char.isUpper char
  || Char.isLower char
  || char == '.'



-- CHOMP FLOAT STUFF


chompDotAndExp : Int -> String -> Result Int Int
chompDotAndExp offset source =
  let
    dotOffset =
      isSubChar isDot offset source
  in
    if dotOffset == -1 then
      chompExp offset source

    else
      chompExp (chomp Char.isDigit dotOffset source) source


isDot : Char -> Bool
isDot char =
  char == '.'


chompExp : Int -> String -> Result Int Int
chompExp offset source =
  let
    eOffset =
      isSubChar isE offset source
  in
    if eOffset == -1 then
      Ok offset

    else
      let
        opOffset =
          isSubChar isPlusOrMinus eOffset source

        expOffset =
          if opOffset == -1 then eOffset else opOffset
      in
        if isSubChar isZero expOffset source /= -1 then
          Err expOffset

        else if isSubChar Char.isDigit expOffset source == -1 then
          Err expOffset

        else
          chompDigits Char.isDigit expOffset source


isE : Char -> Bool
isE char =
  char == 'e' || char == 'E'


isZero : Char -> Bool
isZero char =
  char == '0'


isPlusOrMinus : Char -> Bool
isPlusOrMinus char =
  char == '+' || char == '-'



-- LOW-LEVEL HELPERS - see docs in Parser.LowLevel


isSubString : String -> Int -> Int -> Int -> String -> (Int, Int, Int)
isSubString =
  Elm.Kernel.Parser.isSubString


isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar =
  Elm.Kernel.Parser.isSubChar


findSubString : Bool -> String -> Int -> Int -> Int -> String -> (Int, Int, Int)
findSubString =
  Elm.Kernel.Parser.findSubString
