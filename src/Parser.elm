module Parser exposing
  ( Parser
  , run
  , int, float, symbol, keyword, end
  , succeed, fail, map, zeroOrMore, oneOf, (|=), (|.), map2, lazy, andThen
  , delayedCommit, delayedCommitMap
  , mapWithSource, ignore, ignoreWhile, ignoreUntilBefore, ignoreUntilAfter
  , Error, Problem(..), Context, inContext
  )

{-|

# Parsers
@docs Parser, run

# Numbers and Keywords
@docs int, float, symbol, keyword, end

# Combining Parsers
@docs succeed, fail, map, zeroOrMore, oneOf, (|=), (|.), map2, lazy, andThen

# Delayed Commits
@docs delayedCommit, delayedCommitMap

# Ignoring Stuff
@docs mapWithSource, ignore, ignoreWhile, ignoreUntilBefore, ignoreUntilAfter

# Errors
@docs Error, Problem, Context, inContext
-}

import Char
import Parser.Internal as Internal exposing (Parser(..), Step(..))
import ParserPrimitives as Prim



-- PARSER


{-| A parser! If you have a `Parser Int`, it is a parser that turns
strings into integers.
-}
type alias Parser a =
  Internal.Parser Context Problem a


type alias Step a =
  Internal.Step Context Problem a


type alias State =
  Internal.State Context


{-| Actually run a parser.

    run (keyword "true") "true"  == Ok ()
    run (keyword "true") "True"  == Err ...
    run (keyword "true") "false" == Err ...
-}
run : Parser a -> String -> Result Error a
run (Parser parse) source =
  let
    initialState =
      { source = source
      , offset = 0
      , indent = 1
      , context = []
      , row = 1
      , col = 1
      }
  in
    case parse initialState of
      Good a _ ->
        Ok a

      Bad problem { row, col, context } ->
        Err
          { row = row
          , col = col
          , source = source
          , problem = problem
          , context = context
          }


-- ERRORS


{-| Parse errors as data. You can format it however makes the most
sense for your application. Maybe that is all text, or maybe it is fancy
interactive HTML. Up to you!

You get:

  - The `row` and `col` of the error.
  - The full `source` provided to the [`run`](#run) function.
  - The actual `problem` you ran into.
  - A stack of `context` that describes where the error is *conceptually*.

**Note:** `context` is a stack. That means [`inContext`](#inContext)
adds to the *front* of this list, not the back. So if you want the
[`Context`](#Context) closest to the error, you want the first element
of the `context` stack.
-}
type alias Error =
  { row : Int
  , col : Int
  , source : String
  , problem : Problem
  , context : List Context
  }


{-| The particular problem you ran into.
-}
type Problem
  = BadOneOf (List Problem)
  | BadIgnore
  | BadInt
  | BadFloat
  | ExpectingEnd
  | ExpectingSymbol String
  | ExpectingKeyword String
  | ExpectingVariable
  | Fail String


{-| Most parsers only let you know the row and column where the error
occurred. But what if you could *also* say “the error occured **while
parsing a list**” and let folks know what the *parser* thinks it is
doing?!

The error messages would be a lot nicer! That is what Elm compiler does,
and it is what `Context` helps you do in this library! **See the
[`inContext`](#inContext) docs for a nice example!**

About the actual fields:

  - `description` is set by [`inContext`](#inContext)
  - `row` and `col` are where [`inContext`](#inContext) began

Say you use `inContext` in your list parser. And say get an error trying
to parse `[ 1, 23zm5, 3 ]`. In addition to error information about `23zm5`,
you would have `Context` with the row and column of the starting `[` symbol.
-}
type alias Context =
  { row : Int
  , col : Int
  , description : String
  }



-- PRIMITIVES


{-| A parser that succeeds without consuming any text.

    run (succeed 90210  ) "mississippi" == Ok 90210
    run (succeed 3.141  ) "mississippi" == Ok 3.141
    run (succeed ()     ) "mississippi" == Ok ()
    run (succeed Nothing) "mississippi" == Ok Nothing

Seems weird, but it is often useful in combination with
[`oneOf`](#oneOf) or [`andThen`](#andThen).
-}
succeed : a -> Parser a
succeed a =
  Parser <| \state -> Good a state


{-| A parser always fails.

    run (fail "bad list") "[1,2,3]" == Err ..

Seems weird, but it is often useful in combination with
[`oneOf`](#oneOf) or [`andThen`](#andThen).
-}
fail : String -> Parser a
fail message =
  Parser <| \state -> Bad (Fail message) state



-- MAPPING


{-| Transform the result of a parser. Maybe you have a value that is
an integer or `null`:

    nullOrInt : Parser (Maybe Int)
    nullOrInt =
      oneOf
        [ map Just int
        , map (\_ -> Nothing) (keyword "null")
        ]

    -- run nullOrInt "0"    == Ok (Just 0)
    -- run nullOrInt "13"   == Ok (Just 13)
    -- run nullOrInt "null" == Ok Nothing
    -- run nullOrInt "zero" == Err ...

-}
map : (a -> b) -> Parser a -> Parser b
map func (Parser parse) =
  Parser <| \state1 ->
    case parse state1 of
      Good a state2 ->
        Good (func a) state2

      Bad x state2 ->
        Bad x state2


{-| **This function is not used much in practice.** It is nicer to use
the [parser pipeline][pp] operators [`(|.)`](#|.) and [`(|=)`](#|=)
instead.

[pp]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline

That said, this function can combine two parsers. Maybe you
want to parse some spaces followed by an integer:

    spacesThenInt : Parser Int
    spacesThenInt =
      map2 (\_ n -> n) spaces int

    spaces : Parser ()
    spaces =
      ignoreWhile (\char -> char == ' ')

We can also use `map2` to define `(|.)` and `(|=)` like this:

    (|.) : Parser keep -> Parser ignore -> Parser keep
    (|.) keepParser ignoreParser =
      map2 (\keep _ -> keep) keepParser ignoreParser

    (|=) : Parser (a -> b) -> Parser a -> Parser b
    (|=) funcParser argParser =
      map2 (\func arg -> func arg) funcParser argParser
-}
map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 func (Parser parseA) (Parser parseB) =
  Parser <| \state1 ->
    case parseA state1 of
      Bad x state2 ->
        Bad x state2

      Good a state2 ->
        case parseB state2 of
          Bad x state3 ->
            Bad x state3

          Good b state3 ->
            Good (func a b) state3


{-| **Keep** a value in a parser pipeline.

Read about parser pipelines **[here][]**. They are really nice!

[here]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline
-}
(|=) : Parser (a -> b) -> Parser a -> Parser b
(|=) parseFunc parseArg =
  map2 apply parseFunc parseArg


apply : (a -> b) -> a -> b
apply f a =
  f a


{-| **Ignore** a value in a parser pipeline.

Read about parser pipelines **[here][]**. They are really nice!

[here]: https://github.com/elm-tools/parser/blob/master/README.md#parser-pipeline
-}
(|.) : Parser keep -> Parser ignore -> Parser keep
(|.) keepParser ignoreParser =
  map2 always keepParser ignoreParser


infixl 5 |.
infixl 5 |=



-- AND THEN


{-| Run a parser *and then* run another parser!
-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen callback (Parser parseA) =
  Parser <| \state1 ->
    case parseA state1 of
      Bad x state2 ->
        Bad x state2

      Good a state2 ->
        let
          (Parser parseB) =
            callback a
        in
          parseB state2



-- LAZY


{-| Helper to define recursive parsers. Say we want a parser for simple
boolean expressions:

    true
    false
    (true || false)
    (true || (true || false))

Notice that a boolean expression might contain *other* boolean expressions.
That means we will want to define our parser in terms of itself:

    type Boolean
      = MyTrue
      | MyFalse
      | MyOr Boolean Boolean

    boolean : Parser Boolean
    boolean =
      oneOf
        [ succeed MyTrue
            |. keyword "true"
        , succeed MyFalse
            |. keyword "false"
        , succeed MyOr
            |. symbol "("
            |. spaces
            |= lazy (\_ -> boolean)
            |. spaces
            |. symbol "||"
            |. spaces
            |= lazy (\_ -> boolean)
            |. spaces
            |. symbol ")"
        ]

    spaces : Parser ()
    spaces =
      ignoreWhile (\char -> char == ' ')

**Notice that `boolean` uses `boolean` in its definition!** In Elm, you can
only define a value in terms of itself it is behind a function call. So
`lazy` helps us define these self-referential parsers.

**Note:** In some cases, it may be more natural or efficient to use
`andThen` to hide a self-reference behind a function.
-}
lazy : (() -> Parser a) -> Parser a
lazy thunk =
  Parser <| \state ->
    let
      (Parser parse) =
        thunk ()
    in
      parse state



-- ONE OF


{-| Try a bunch of different parsers. If a parser does not commit, we
move on and try the next one. If a parser *does* commit, we give up on any
remaining parsers.

The idea is: if you make progress and commit to a parser, you want to
get error messages from *that path*. If you bactrack and keep trying stuff
you will get a much less precise error.

So say we are parsing “language terms” that include integers and lists
of integers:

    term : Parser Expr
    term =
      oneOf
        [ listOf int
        , int
        ]

    listOf : Parser a -> Parser (List a)
    listOf parser =
      succeed identity
        |. symbol "["
        |. spaces
        ...

When we get to `oneOf`, we first try the `listOf int` parser. If we see a
`[` we *commit* to that parser. That means if something goes wrong, we do
not backtrack. Instead the parse fails! If we do not see a `[` we move on
to the second option and just try the `int` parser.
-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
  Parser <| \state -> oneOfHelp state [] parsers


oneOfHelp : State -> List Problem -> List (Parser a) -> Step a
oneOfHelp state problems parsers =
  case parsers of
    [] ->
      Bad (BadOneOf (List.reverse problems)) state

    Parser parse :: remainingParsers ->
      case parse state of
        Good _ _ as step ->
          step

        Bad problem { row, col } as step ->
          if state.row == row && state.col == col then
            oneOfHelp state (problem :: problems) remainingParsers

          else
            step



-- ZERO OR MORE


{-| Try to use the parser as many times as possible. Say we want to parse
`NaN` a bunch of times:

    batman : Parser Int
    batman =
      map List.length (zeroOrMore (keyword "NaN"))

    -- run batman "whatever"       == Ok 0
    -- run batman ""               == Ok 0
    -- run batman "NaN"            == Ok 1
    -- run batman "NaNNaN"         == Ok 2
    -- run batman "NaNNaNNaN"      == Ok 3
    -- run batman "NaNNaN batman!" == Ok 2

**Note:** If you are trying to parse things like `[1,2,3]` or `{ x = 3 }`
check out the [`list`](Parser-LanguageKit#list) and
[`record`](Parser-LanguageKit#record) functions in the
[`Parser.LanguageKit`](Parser-LanguageKit) module.
-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
  zeroOrMoreHelp parser []


zeroOrMoreHelp : Parser a -> List a -> Parser (List a)
zeroOrMoreHelp parser revValues =
  oneOf
    [ parser
        |> andThen (\value -> zeroOrMoreHelp parser (value :: revValues))
    , succeed (List.reverse revValues)
    ]



-- DELAYED COMMIT


{-| Only commit if `Parser a` succeeds and `Parser value` makes some progress.

This is very important for generating high quality error messages! Read more
about this [here][1] and [here][2].

[1]: https://github.com/elm-tools/parser/blob/master/README.md#delayed-commits
[2]: https://github.com/elm-tools/parser/blob/master/comparison.md
-}
delayedCommit : Parser a -> Parser value -> Parser value
delayedCommit filler realStuff =
  delayedCommitMap (\_ v -> v) filler realStuff


{-| Like [`delayedCommit`](#delayedCommit), but lets you extract values from
both parsers. Read more about it [here][1] and [here][2].

[1]: https://github.com/elm-tools/parser/blob/master/README.md#delayed-commits
[2]: https://github.com/elm-tools/parser/blob/master/comparison.md
-}
delayedCommitMap : (a -> b -> value) -> Parser a -> Parser b -> Parser value
delayedCommitMap func (Parser parseA) (Parser parseB) =
  Parser <| \state1 ->
    case parseA state1 of
      Bad x _ ->
        Bad x state1

      Good a state2 ->
        case parseB state2 of
          Good b state3 ->
            Good (func a b) state3

          Bad x state3 ->
            if state2.row == state3.row && state2.col == state3.col then
              Bad x state1
            else
              Bad x state3



-- SYMBOLS and KEYWORDS


{-| Parse symbols like `,`, `(`, and `&&`.

    run (symbol "[") "[" == Ok ()
    run (symbol "[") "4" == Err ... (ExpectingSymbol "[") ...
-}
symbol : String -> Parser ()
symbol str =
  token ExpectingSymbol str


{-| Parse keywords like `let`, `case`, and `type`.

    run (keyword "let") "let" == Ok ()
    run (keyword "let") "var" == Err ... (ExpectingKeyword "let") ...
-}
keyword : String -> Parser ()
keyword str =
  token ExpectingKeyword str


token : (String -> Problem) -> String -> Parser ()
token makeProblem str =
  Parser <| \({ source, offset, indent, context, row, col } as state) ->
    let
      (newOffset, newRow, newCol) =
        Prim.isSubString str offset row col source
    in
      if newOffset == -1 then
        Bad (makeProblem str) state

      else
        Good ()
          { source = source
          , offset = newOffset
          , indent = indent
          , context = context
          , row = newRow
          , col = newCol
          }


-- INT


{-| Parse integers. It accepts decimal and hexidecimal formats.

    -- decimal
    run int "1234" == Ok 1234
    run int "1.34" == Err ...
    run int "1e31" == Err ...
    run int "123a" == Err ...
    run int "0123" == Err ...

    -- hexidecimal
    run int "0x001A" == Ok 26
    run int "0x001a" == Ok 26
    run int "0xBEEF" == Ok 48879
    run int "0x12.0" == Err ...
    run int "0x12an" == Err ...

**Note:** If you want a parser for both `Int` and `Float` literals,
check out [`Parser.LanguageKit.number`](Parser-LanguageKit#number).
It does not backtrack, so it should be faster and give better error
messages than using `oneOf` and combining `int` and `float` yourself.

**Note:** If you want to enable octal or binary `Int` literals,
check out [`Parser.LanguageKit.int`](Parser-LanguageKit#int).
-}
int : Parser Int
int =
  Parser <| \{ source, offset, indent, context, row, col } ->
    case intHelp offset (Prim.isSubChar isZero offset source) source of
      Err badOffset ->
        Bad BadInt
          { source = source
          , offset = badOffset
          , indent = indent
          , context = context
          , row = row
          , col = col + (badOffset - offset)
          }

      Ok goodOffset ->
        let
          diff =
            goodOffset - offset
        in
          case String.toInt (String.slice offset diff source) of
            Err _ ->
              Debug.crash badIntMsg

            Ok n ->
              Good n
                { source = source
                , offset = goodOffset
                , indent = indent
                , context = context
                , row = row
                , col = col + diff
                }


intHelp : Int -> Int -> String -> Result Int Int
intHelp offset zeroOffset source =
  if zeroOffset == -1 then
    Internal.chompDigits Char.isDigit offset source

  else if Prim.isSubChar isX zeroOffset source /= -1 then
    Internal.chompDigits Char.isHexDigit (offset + 2) source

--  else if Prim.isSubChar isO zeroOffset source /= -1 then
--    Internal.chompDigits Char.isOctDigit (offset + 2) source

  else if Prim.isSubChar Internal.isBadIntEnd zeroOffset source == -1 then
    Ok zeroOffset

  else
    Err zeroOffset


isZero : Char -> Bool
isZero char =
  char == '0'


isO : Char -> Bool
isO char =
  char == 'o'


isX : Char -> Bool
isX char =
  char == 'x'


badIntMsg : String
badIntMsg =
  """The `Parser.int` parser seems to have a bug.
Please report an SSCCE to <https://github.com/elm-tools/parser/issues>."""



-- FLOAT


{-| Parse floats.

    run float "123"       == Ok 123
    run float "3.1415"    == Ok 3.1415
    run float "0.1234"    == Ok 0.1234
    run float ".1234"     == Ok 0.1234
    run float "1e-42"     == Ok 1e-42
    run float "6.022e23"  == Ok 6.022e23
    run float "6.022E23"  == Ok 6.022e23
    run float "6.022e+23" == Ok 6.022e23
    run float "6.022e"    == Err ..
    run float "6.022n"    == Err ..
    run float "6.022.31"  == Err ..

**Note:** If you want a parser for both `Int` and `Float` literals,
check out [`Parser.LanguageKit.number`](Parser-LanguageKit#number).
It does not backtrack, so it should be faster and give better error
messages than using `oneOf` and combining `int` and `float` yourself.

**Note:** If you want to disable literals like `.123` like Elm,
check out [`Parser.LanguageKit.float`](Parser-LanguageKit#float).
-}
float : Parser Float
float =
  Parser <| \{ source, offset, indent, context, row, col } ->
    case floatHelp offset (Prim.isSubChar isZero offset source) source of
      Err badOffset ->
        Bad BadFloat
          { source = source
          , offset = badOffset
          , indent = indent
          , context = context
          , row = row
          , col = col + (badOffset - offset)
          }

      Ok goodOffset ->
        case String.toFloat (String.slice offset goodOffset source) of
          Err _ ->
            Debug.crash badFloatMsg

          Ok n ->
            Good n
              { source = source
              , offset = goodOffset
              , indent = indent
              , context = context
              , row = row
              , col = col + (goodOffset - offset)
              }


floatHelp : Int -> Int -> String -> Result Int Int
floatHelp offset zeroOffset source =
  if zeroOffset >= 0 then
    Internal.chompDotAndExp zeroOffset source

  else
    let
      dotOffset =
        Internal.chomp Char.isDigit offset source

      result =
        Internal.chompDotAndExp dotOffset source
    in
      case result of
        Err _ ->
          result

        Ok n ->
          if n == offset then Err n else result


badFloatMsg : String
badFloatMsg =
  """The `Parser.float` parser seems to have a bug.
Please report an SSCCE to <https://github.com/elm-tools/parser/issues>."""



-- END


{-| Check if you have reached the end of the string you are parsing.

    justAnInt : Parser Int
    justAnInt =
      succeed identity
        |= int
        |. end

    -- run justAnInt "90210" == Ok 90210
    -- run justAnInt "1 + 2" == Err ...
    -- run int       "1 + 2" == Ok 1

Parsers can succeed without parsing the whole string. Ending your parser
with `end` guarantees that you have successfully parsed the whole string.
-}
end : Parser ()
end =
  Parser <| \state ->
    if String.length state.source == state.offset then
      Good () state

    else
      Bad ExpectingEnd state



-- MAP WITH SOURCE


{-| Run a parser, and when it is done, extract all of the source code
it chomped as well.

This is most useful in combination with the ignore functions, like
[`ignore`](#ignore) and [`ignoreWhile`](#ignoreWhile). The idea is
that you want to allocate as little as possible, so you ignore all
the intermediate stuff and only pull out the final string if you
need it.

For example, say we want a parser for variables that start lower case,
but then have numbers, letters, and underscores:

    variable : Parser String
    variable =
      mapWithSource always <|
        ignore 1 Char.isLower
          |. ignoreWhile isVarChar

    isVarChar : Char -> Bool
    isVarChar char =
      Char.isLower char
      || Char.isUpper char
      || Char.isDigit char
      || char == '_'

**The trick is that we only allocate *one* string here.** Otherwise you
might allocate the first `Char` then the remaining `String` and then put
them together with `String.cons`, allocating a second `String`.
-}
mapWithSource : (String -> a -> b) -> Parser a -> Parser b
mapWithSource func (Parser parse) =
  Parser <| \({source, offset} as state1) ->
    case parse state1 of
      Bad x state2 ->
        Bad x state2

      Good a state2 ->
        let
          subString =
            String.slice offset state2.offset source
        in
          Good (func subString a) state2



-- IGNORE


{-| Ignore exactly `N` characters. If you want to ignore one or more
spaces, you might say:

    oneOrMoreSpaces : Parser ()
    oneOrMoreSpaces =
      ignore 1 isSpace
        |. ignoreWhile isSpace

    isSpace : Char -> Bool
    isSpace char =
      char == ' '

Works well in combination with [`mapWithSource`](#mapWithSource).
-}
ignore : Int -> (Char -> Bool) -> Parser ()
ignore n predicate =
  Parser <| \{ source, offset, indent, context, row, col } ->
    ignoreHelp n predicate source offset indent context row col


ignoreHelp : Int -> (Char -> Bool) -> String -> Int -> Int -> List Context -> Int -> Int -> Step ()
ignoreHelp n predicate source offset indent context row col =
  if n <= 0 then
    Good ()
      { source = source
      , offset = offset
      , indent = indent
      , context = context
      , row = row
      , col = col
      }

  else
    let
      newOffset =
        Prim.isSubChar predicate offset source
    in
      if newOffset == -1 then
        Bad BadIgnore
          { source = source
          , offset = offset
          , indent = indent
          , context = context
          , row = row
          , col = col
          }

      else if newOffset == -2 then
        ignoreHelp (n - 1) predicate source (offset + 1) indent context (row + 1) 1

      else
        ignoreHelp (n - 1) predicate source newOffset indent context row (col + 1)



-- IGNORE WHILE


{-| Ignore zero or more characters. A common use is to ignore spaces:

    spaces : Parser ()
    spaces =
      ignoreWhile isSpace

    isSpace : Char -> Bool
    isSpace char =
      char == ' '

Note that this parses **zero** or more characters, so this parser will
always succeed. So our `spaces` parser will succeed even if `isSpace`
does not match anything.

That said, we only commit to a parser if characters have been consumed.
So if you see no spaces, you only commit if the *next* parser commits.

Works well in combination with [`mapWithSource`](#mapWithSource).
-}
ignoreWhile : (Char -> Bool) -> Parser ()
ignoreWhile predicate =
  Parser <| \{ source, offset, indent, context, row, col } ->
    ignoreWhileHelp predicate source offset indent context row col


ignoreWhileHelp : (Char -> Bool) -> String -> Int -> Int -> List Context -> Int -> Int -> Step ()
ignoreWhileHelp predicate source offset indent context row col =
  let
    newOffset =
      Prim.isSubChar predicate offset source
  in
    -- no match
    if newOffset == -1 then
      Good ()
        { source = source
        , offset = offset
        , indent = indent
        , context = context
        , row = row
        , col = col
        }

    -- matched a newline
    else if newOffset == -2 then
      ignoreWhileHelp predicate source (offset + 1) indent context (row + 1) 1

    -- normal match
    else
      ignoreWhileHelp predicate source newOffset indent context row (col + 1)



-- IGNORE UNTIL


{-| Ignore characters until *after* the given string.
So maybe we want to parse Elm-style single-line comments:

    elmComment : Parser ()
    elmComment =
      symbol "--"
        |. ignoreUntilAfter "\n"

Or maybe you want to parse JS-style multi-line comments:

    jsComment : Parser ()
    jsComment =
      symbol "/*"
        |. ignoreUntilAfter "*/"

**Note:** You must take more care when parsing Elm-style multi-line
comments. Elm can recognize nested comments, but the `jsComment` parser
cannot.
-}
ignoreUntilAfter : String -> Parser ()
ignoreUntilAfter str =
  ignoreUntil False str


{-| Ignore characters until *before* the given string.
-}
ignoreUntilBefore : String -> Parser ()
ignoreUntilBefore str =
  ignoreUntil True str


ignoreUntil : Bool -> String -> Parser ()
ignoreUntil before str =
  Parser <| \({ source, offset, indent, context, row, col } as state) ->
    let
      (newOffset, newRow, newCol) =
        Prim.findSubString before str offset row col source
    in
      if newOffset == -1 then
        Bad BadIgnore state

      else
        Good ()
          { source = source
          , offset = newOffset
          , indent = indent
          , context = context
          , row = newRow
          , col = newCol
          }



-- CONTEXT


{-| Specify what you are parsing right now. So if you have a parser
for lists like `[ 1, 2, 3 ]` you could say:

    list : Parser (List Int)
    list =
      inContext "list" <|
        succeed identity
          |. symbol "["
          |. spaces
          |= commaSep int
          |. spaces
          |. symbol "]"

    -- spaces : Parser ()
    -- commaSep : Parser a -> Parser (List a)

Now you get that extra context information if there is a parse error anywhere
in the list. For example, if you have `[ 1, 23zm5, 3 ]` you could generate an
error message like this:

    I ran into a problem while parsing this list:

        [ 1, 23zm5, 3 ]
             ^
    Looking for a valid integer, like 6 or 90210.

Notice that the error message knows you are parsing a list right now!
-}
inContext : String -> Parser a -> Parser a
inContext ctx (Parser parse) =
  Parser <| \({ context, row, col } as initialState) ->
    let
      state1 =
        changeContext (Context row col ctx :: context) initialState
    in
      case parse state1 of
        Good a state2 ->
          Good a (changeContext context state2)

        Bad _ _ as step ->
          step


changeContext : List Context -> State -> State
changeContext newContext { source, offset, indent, row, col } =
  { source = source
  , offset = offset
  , indent = indent
  , context = newContext
  , row = row
  , col = col
  }
