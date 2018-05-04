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
  , Nestable(..)

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
    , trailing = toAdvancedTrailing i.trailing
    }


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](http://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing = Forbidden | Optional | Mandatory


toAdvancedTrailing : Trailing -> A.Trailing
toAdvancedTrailing trailing =
  case trailing of
    Forbidden -> A.Forbidden
    Optional -> A.Optional
    Mandatory -> A.Mandatory



-- WHITESPACE


{-| Parse zero or more `' '`, `'\n'`, and `'\r'` characters.

The implementation is pretty simple:

    spaces : Parser ()
    spaces =
      chompWhile (\c -> c == ' ' || c == '\n' || c == '\r')

So if you need something different (like tabs) just define an alternative with
the necessary tweaks! Check out [`lineComment`](#lineComment) and
[`multiComment`](#multiComment) for more complex situations.
-}
spaces : Parser ()
spaces =
  A.spaces


{-| Parse single-line comments:

    elm : Parser ()
    elm =
      lineComment "--"

    js : Parser ()
    js =
      lineComment "//"

    python : Parser ()
    python =
      lineComment "#"

This parser is defined like this:

    lineComment : String -> Parser ()
    lineComment str =
      symbol str
        |. chompUntilEndOr "\n"

So it will consume the remainder of the line. If the file ends before you see
a newline, that is fine too.
-}
lineComment : String -> Parser ()
lineComment str =
  A.lineComment (toToken str)


{-| Parse multi-line comments. So if you wanted to parse Elm whitespace or
JS whitespace, you could say:

    elm : Parser ()
    elm =
      chompZeroOrMore <|
        oneOf
          [ lineComment "--"
          , multiComment "{-" "-}" Nestable
          , spaces
          ]

    js : Parser ()
    js =
      chompZeroOrMore <|
        oneOf
          [ lineComment "//"
          , multiComment "/*" "*/" NotNestable
          , chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
          ]

**Note:** The fact that `spaces` and `chompWhile` come last is really
important! They can both succeed without chomping any characters, so if they
were the first option, they would always succeed and bypass the others!
-}
multiComment : String -> String -> Nestable -> Parser ()
multiComment open close nestable =
  A.multiComment (toToken open) (toToken close) (toAdvancedNestable nestable)


{-| Not all languages handle multi-line comments the same. Multi-line comments
in C-style syntax are `NotNestable`, meaning they can be implemented like this:

    js : Parser ()
    js =
      symbol "/*"
        |. chompUntil "*/"

In fact, `multiComment "/*" "*/" NotNestable` *is* implemented like that! It is
very simple, but it does not allow you to nest comments like this:

```javascript
/*
line1
/* line2 */
line3
*/
```

It would stop on the first `*/`, eventually throwing a syntax error on the
second `*/`. This can be pretty annoying in long files.

Languages like Elm allow you to nest multi-line comments, but your parser needs
to be a bit fancier to handle this. After you start a comment, you have to
detect if there is another one inside it! And then you have to make sure all
the `{-` and `-}` match up properly! Saying `multiComment "{-" "-}" Nestable`
does all that for you.
-}
type Nestable = NotNestable | Nestable


toAdvancedNestable : Nestable -> A.Nestable
toAdvancedNestable nestable =
  case nestable of
    NotNestable -> A.NotNestable
    Nestable -> A.Nestable
