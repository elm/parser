module Parser exposing
  ( Parser
  , run
  , succeed, fail
  , map, map2, (|=), (|.)
  , andThen, lazy
  , oneOf
  , zeroOrMore

  , int
  , float
  , string

  , ignore
  , ignoreWhile

  , mapWithSource

  , delayedCommit
  , delayedCommitMap

  , Error
  , Problem(..)

  , inContext
  , Context

  , getIndentLevel
  , withIndentLevel

  , getPosition
  , getRow
  , getColumn

  , getOffset
  , getSource
  )

{-|

# Parsers
@docs Parser, run, int, float, string

# Build Your Own Parsers
@docs succed, (|=), (|.), oneOf

# Parser Primitives
@docs succeed, fail, map, map2, andThen, lazy

# Errors
@docs Error, Problem

## Error Context
@docs inContext, Context


# Low-Level Stuff

You are unlikely to need any of this under normal circumstances.

## Indentation
@docs getIndentLevel, withIndentLevel

## Row, Column, and Offset
@docs getPosition, getRow, getColumn, getOffset, getSource

-}


import Parser.Primitives as Prim



-- PARSER


{-| A parser! If you have a `Parser Int`, it is a parser that turns
strings into integers.
-}
type Parser a =
  Parser (State -> Step a)


type Step a
  = Good a State
  | Bad Problem State


type alias State =
  { source : String
  , offset : Int
  , indent : Int
  , context : List Context
  , row : Int
  , col : Int
  }


{-| Actually run a parser.

    run (string "hello") "hello" == Ok ()
    run (string "hello") "hey!!" == Err ...
-}
run : Parser a -> String -> Result Error a
run (Parser parse) source =
  let
    finalStep =
      parse
        { source = source
        , offset = 0
        , indent = 1
        , context = []
        , row = 1
        , col = 1
        }
  in
    case finalStep of
      Good a _ ->
        Ok a

      Bad problem { row, col, context } ->
        Err (Error row col source context problem)



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
  | Expecting String
  | ExpectingString String
  | ExpectingSeparator String
  | ExpectingVariable (Maybe String)
  | Custom String


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


{-| Just succeed without consuming any text.

    run (succeed 90210  ) "mississippi" == Ok 90210
    run (succeed 3.141  ) "mississippi" == Ok 3.141
    run (succeed ()     ) "mississippi" == Ok ()
    run (succeed Nothing) "mississippi" == Ok Nothing

This is often useful in combination with [`oneOf`](#oneOf).
-}
succeed : a -> Parser a
succeed a =
  Parser <| \state -> Good a state


fail : String -> Parser a
fail message =
  Parser <| \state -> Bad (Custom message) state



-- MAPPING


{-| Transform a parser.
-}
map : (a -> b) -> Parser a -> Parser b
map func (Parser parse) =
  Parser <| \state1 ->
    case parse state1 of
      Good a state2 ->
        Good (func a) state2

      Bad x state2 ->
        Bad x state2


{-| Combine two parsers.
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



-- AND THEN


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
This is very important for generating high quality error messages!
-}
delayedCommit : Parser a -> Parser value -> Parser value
delayedCommit filler realStuff =
  delayedCommitMap (\_ v -> v) filler realStuff


{-| Like [`delayedCommit`](#delayedCommit), but lets you extract values from
both parsers. It is unclear when this would be needed, so please open a PR
modifying this documentation if you find a case!
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



-- CONTEXT


{-| Specify what you are parsing right now. So if you have a parser
for lists like `[ 1, 2, 3 ]` you could say:

    list : Parser (List Int)
    list =
      inContext "list" <|
        succeed identity
          |. string "["
          |. spaces
          |= commaSep int
          |. spaces
          |. string "]"

    -- spaces : Parser ()
    -- commaSep : Parser a -> Parser (List a)
    -- int : Parser Int

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



-- STRINGS


string : String -> Parser ()
string str =
  Parser <| \({ source, offset, indent, context, row, col } as state) ->
    let
      (newOffset, newRow, newCol) =
        Prim.isSubString str offset row col source
    in
      if newOffset == -1 then
        Bad (ExpectingString str) state

      else
        Good ()
          { source = source
          , offset = newOffset
          , indent = indent
          , context = context
          , row = newRow
          , col = newCol
          }



-- IGNORE


ignore : (Char -> Bool) -> Parser ()
ignore predicate =
  Parser <| \({ source, offset, indent, context, row, col } as state) ->
    let
      newOffset =
        Prim.isSubChar predicate offset source
    in
      if newOffset == -1 then
        Bad BadIgnore state

      else if newOffset == -2 then
        Good ()
          { source = source
          , offset = offset + 1
          , indent = indent
          , context = context
          , row = row + 1
          , col = 1
          }

      else
        Good ()
          { source = source
          , offset = newOffset
          , indent = indent
          , context = context
          , row = row
          , col = col + 1
          }


ignoreWhile : (Char -> Bool) -> Parser ()
ignoreWhile predicate =
  Parser <| \{ source, offset, indent, context, row, col } ->
    let
      (newOffset, newRow, newCol) =
        chomp predicate offset row col source
    in
      Good ()
        { source = source
        , offset = newOffset
        , indent = indent
        , context = context
        , row = newRow
        , col = newCol
        }


chomp : (Char -> Bool) -> Int -> Int -> Int -> String -> (Int, Int, Int)
chomp otherChars offset row col source =
  let
    newOffset =
      Prim.isSubChar otherChars offset source
  in
    -- no match
    if newOffset == -1 then
      (offset, row, col)

    -- newline match
    else if newOffset == -2 then
      chomp otherChars (offset + 1) (row + 1) 1 source

    -- normal match
    else
      chomp otherChars newOffset row (col + 1) source



-- INDENTATION


getIndentLevel : Parser Int
getIndentLevel =
  Parser <| \state -> Good state.indent state


withIndentLevel : Int -> Parser a -> Parser a
withIndentLevel newIndent (Parser parse) =
  Parser <| \state1 ->
    case parse (changeIndent newIndent state1) of
      Good a state2 ->
        Good a (changeIndent state1.indent state2)

      Bad x state2 ->
        Bad x (changeIndent state1.indent state2)


changeIndent : Int -> State -> State
changeIndent newIndent { source, offset, context, row, col } =
  { source = source
  , offset = offset
  , indent = newIndent
  , context = context
  , row = row
  , col = col
  }



-- POSITION


getPosition : Parser (Int, Int)
getPosition =
  Parser <| \state -> Good (state.row, state.col) state


getRow : Parser Int
getRow =
  Parser <| \state -> Good state.row state


getColumn : Parser Int
getColumn =
  Parser <| \state -> Good state.col state


getOffset : Parser Int
getOffset =
  Parser <| \state -> Good state.offset state


getSource : Parser String
getSource =
  Parser <| \state -> Good state.source state


mapWithSource : (String -> a -> b) -> Parser a -> Parser b
mapWithSource func (Parser parse) =
  Parser <| \({source, offset} as state1) ->
    case parse state1 of
      Bad x state2 ->
        Bad x state2

      Good a state2 ->
        let
          subString =
            String.slice offset (state2.offset - offset) source
        in
          Good (func subString a) state2

