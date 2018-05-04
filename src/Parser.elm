module Parser exposing
  ( Parser
  , run

  , int
  , float
  , number
  , token
  , variable
  , sequence
  , Trailing(..)
  , end

  , spaces
  , lineComment
  , multiComment
  , nestableComment

  , succeed
  , problem
  , map
  , (|=)
  , (|.)
  , map2
  , lazy
  , andThen
  , zeroOrMore

  , oneOf
  , backtrackable
  , commit
  , failure

  , getChompedString
  , chompIf
  , chompWhile
  , chompUntil
  , chompUntilEndOr
  , chompZeroOrMore
  , mapChompedString

  , getIndent
  , withIndent
  , getPosition
  , getRow
  , getCol
  , getOffset
  , getSource
  )


import Char
import Parser.Advanced as A
import Set



-- INFIX OPERATORS - see Parser.Advanced for why 5 and 6 were chosen


infix left 5 (|=) = keeper
infix left 6 (|.) = ignorer



-- PARSERS


type alias Parser a =
  A.Parser Never Problem a



-- RUN


run : Parser a -> String -> Result (List DeadEnd) a
run parser source =
  case A.run parser source of
    Ok a ->
      Ok a

    Err _ ->
      Debug.todo "format errors"



-- PROBLEMS


type alias DeadEnd =
  { row : Int
  , col : Int
  , problem : Problem
  }


type Problem
  = Expecting String
  | ExpectingInt
  | ExpectingHex
  | ExpectingOctal
  | ExpectingBinary
  | ExpectingFloat
  | ExpectingNumber
  | ExpectingVariable
  | ExpectingEnd
  | UnexpectedChar
  | Problem String
  | BadRepeat



-- PRIMITIVES


succeed : a -> Parser a
succeed =
  A.succeed


problem : String -> Parser a
problem str =
  A.problem (Problem str)



-- MAPPING


map : (a -> b) -> Parser a -> Parser b
map =
  A.map


map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 =
  A.map2


keeper : Parser (a -> b) -> Parser a -> Parser b
keeper parseFunc parseArg =
  A.map2 (<|) parseFunc parseArg


ignorer : Parser keep -> Parser ignore -> Parser keep
ignorer keepParser ignoreParser =
  A.map2 always keepParser ignoreParser



-- AND THEN


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
  A.andThen



-- LAZY


lazy : (() -> Parser a) -> Parser a
lazy =
  A.lazy



-- ONE OF


oneOf : List (Parser a) -> Parser a
oneOf =
  A.oneOf



-- ZERO OR MORE


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
  A.zeroOrMore parser BadRepeat



-- BACKTRACKABLE


backtrackable : Parser a -> Parser a
backtrackable =
  A.backtrackable


commit : a -> Parser a
commit =
  A.commit


failure : String -> Parser a
failure str =
  A.failure (Problem str)



-- TOKEN


{-| Parse a token like `let`, `(`, or `+`.

**Note:** A potential pitfall when parsing keywords is getting tricked by
variables that start with a keyword, like `let` in `letters` or `import` in
`important`. This is especially likely if you have a whitespace parser that
can consume zero charcters. So you may want to use `peek` to check on it.

    keyword : Token x -> Parser ()
    keyword (Token _ x as token) =
      backtrackable (chomp token)
        |. peek isNotVarChar x
        |. commit ()
-}
token : String -> Parser ()
token str =
  A.token (toToken str)


toToken : String -> A.Token Problem
toToken str =
  A.Token str (Expecting str)



-- INT


int : Parser Int
int =
  A.int ExpectingInt ExpectingInt



-- FLOAT


float : Parser Float
float =
  A.float ExpectingFloat ExpectingFloat



-- NUMBER


number
  : { int : Maybe (Int -> a)
    , hex : Maybe (Int -> a)
    , octal : Maybe (Int -> a)
    , binary : Maybe (Int -> a)
    , float : Maybe (Float -> a)
    }
  -> Parser a
number i =
  A.number
    { int = Result.fromMaybe ExpectingInt i.int
    , hex = Result.fromMaybe ExpectingHex i.hex
    , octal = Result.fromMaybe ExpectingOctal i.octal
    , binary = Result.fromMaybe ExpectingBinary i.binary
    , float = Result.fromMaybe ExpectingFloat i.float
    , invalid = ExpectingNumber
    , expecting = ExpectingNumber
    }



-- END


end : Parser ()
end =
  A.end ExpectingEnd



-- CHOMPED STRINGS


{-|
    succeed (\start end -> String.slice start end )
      |= getOffset
      |. parser
      |= getOffset
-}
getChompedString : Parser a -> Parser String
getChompedString =
  A.getChompedString


mapChompedString : (String -> a -> b) -> Parser a -> Parser b
mapChompedString =
  A.mapChompedString



-- CHOMP IF


chompIf : (Char -> Bool) -> Parser ()
chompIf isGood =
  A.chompIf isGood UnexpectedChar



-- CHOMP WHILE


chompWhile : (Char -> Bool) -> Parser ()
chompWhile =
  A.chompWhile



-- CHOMP UNTIL


chompUntil : String -> Parser ()
chompUntil str =
  A.chompUntil (toToken str)


chompUntilEndOr : String -> Parser ()
chompUntilEndOr =
  A.chompUntilEndOr



-- CHOMP ZERO OR MORE


chompZeroOrMore : Parser a -> Parser ()
chompZeroOrMore =
  A.chompZeroOrMore



-- INDENTATION


getIndent : Parser Int
getIndent =
  A.getIndent


withIndent : Int -> Parser a -> Parser a
withIndent =
  A.withIndent



-- POSITION


getPosition : Parser (Int, Int)
getPosition =
  A.getPosition


getRow : Parser Int
getRow =
  A.getRow


getCol : Parser Int
getCol =
  A.getCol


getOffset : Parser Int
getOffset =
  A.getOffset


getSource : Parser String
getSource =
  A.getSource



-- VARIABLES


variable :
  { start : Char -> Bool
  , inner : Char -> Bool
  , reserved : Set.Set String
  }
  -> Parser String
variable i =
  A.variable
    { start = i.start
    , inner = i.inner
    , reserved = i.reserved
    , expecting = ExpectingVariable
    }



-- SEQUENCES


sequence
  : { start : String
    , separator : String
    , end : String
    , spaces : Parser ()
    , item : Parser a
    , trailing : Trailing
    }
  -> Parser (List a)
sequence i =
  A.sequence
    { start = toToken i.start
    , separator = toToken i.separator
    , end = toToken i.end
    , spaces = i.spaces
    , item = i.item
    , trailing =
        case i.trailing of
          Forbidden -> A.Forbidden
          Optional -> A.Optional
          Mandatory -> A.Mandatory
    }


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](http://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing = Forbidden | Optional | Mandatory



-- WHITESPACE


spaces : Parser ()
spaces =
  A.spaces


lineComment : String -> Parser ()
lineComment str =
  A.lineComment (toToken str)


multiComment : String -> String -> Parser ()
multiComment open close =
  A.multiComment (toToken open) (toToken close)


nestableComment : String -> String -> Parser ()
nestableComment open close =
  A.nestableComment (toToken open) (toToken close)
