module Parser.LowLevel exposing
  ( getIndentLevel, withIndentLevel
  , getPosition, getRow, getCol, getOffset, getSource
  , isSubString, isSubChar, findSubString
  )

{-| You are unlikely to need any of this under normal circumstances.

# Indentation
@docs getIndentLevel, withIndentLevel

# Row, Column, Offset, and Source
@docs getPosition, getRow, getCol, getOffset, getSource

# Low-Level Helpers
@docs isSubString, isSubChar, findSubString

-}

import Parser exposing (Parser)
import Parser.Internal as I exposing (State)



-- INDENTATION


{-| This parser tracks “indentation level” so you can parse indentation
sensitive languages. Indentation levels correspond to column numbers, so
it starts at 1.
-}
getIndentLevel : Parser Int
getIndentLevel =
  I.Parser <| \state -> I.Good state.indent state


{-| Run a parser with a given indentation level. So you will likely
use `getCol` to get the current column, `andThen` give that to
`withIndentLevel`.
-}
withIndentLevel : Int -> Parser a -> Parser a
withIndentLevel newIndent (I.Parser parse) =
  I.Parser <| \state1 ->
    case parse (changeIndent newIndent state1) of
      I.Good a state2 ->
        I.Good a (changeIndent state1.indent state2)

      I.Bad x state2 ->
        I.Bad x (changeIndent state1.indent state2)


changeIndent : Int -> State ctx -> State ctx
changeIndent newIndent { src, offset, context, row, col } =
  { src = src
  , offset = offset
  , indent = newIndent
  , context = context
  , row = row
  , col = col
  }



-- POSITION


{-| Code editors treat code like a grid. There are rows and columns.
In most editors, rows and colums are 1-indexed. You move to a new row
whenever you see a `\n` character.

The `getPosition` parser succeeds with your current row and column
within the string you are parsing.
-}
getPosition : Parser (Int, Int)
getPosition =
  I.Parser <| \state -> I.Good (state.row, state.col) state


{-| The `getRow` parser succeeds with your current row within
the string you are parsing.
-}
getRow : Parser Int
getRow =
  I.Parser <| \state -> I.Good state.row state


{-| The `getCol` parser succeeds with your current column within
the string you are parsing.
-}
getCol : Parser Int
getCol =
  I.Parser <| \state -> I.Good state.col state


{-| Editors think of code as a grid, but behind the scenes it is just
a flat array of UTF16 characters. `getOffset` tells you your index in
that flat array. So if you have read `"\n\n\n\n"` you are on row 5,
column 1, and offset 4.

**Note:** browsers use UTF16 strings, so characters may be one or two 16-bit
words. This means you can read 4 characters, but your offset will move by 8.
-}
getOffset : Parser Int
getOffset =
  I.Parser <| \state -> I.Good state.offset state


{-| Get the entire string you are parsing right now. Paired with
`getOffset` this can let you use `String.slice` to grab substrings
with very little intermediate allocation.
-}
getSource : Parser String
getSource =
  I.Parser <| \state -> I.Good state.src state



-- LOW-LEVEL HELPERS


{-| When making a fast parser, you want to avoid allocation as much as
possible. That means you never want to mess with the source string, only
keep track of an offset into that string.

You use `isSubString` like this:

    isSubString "let" offset row col "let x = 4 in x"
        --==> ( newOffset, newRow, newCol )

You are looking for `"let"` at a given `offset`. On failure, the
`newOffset` is `-1`. On success, the `newOffset` is the new offset. With
our `"let"` example, it would be `offset + 3`.

You also provide the current `row` and `col` which do not align with
`offset` in a clean way. For example, when you see a `\n` you are at
`row = row + 1` and `col = 1`. Furthermore, some UTF16 characters are
two words wide, so even if there are no newlines, `offset` and `col`
may not be equal.
-}
isSubString : String -> Int -> Int -> Int -> String -> (Int, Int, Int)
isSubString =
  I.isSubString



-- CHARACTERS


{-| Again, when parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubChar isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - `-2` means the predicate succeeded with a `\n`
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.

It is better to use union types in general, but it is worth the
danger *within* parsing libraries to get the benefit *outside*.

So you can write a `chomp` function like this:

    chomp : (Char -> Bool) -> Int -> Int -> Int -> String -> (Int, Int, Int)
    chomp isGood offset row col source =
      let
        newOffset =
          Prim.isSubChar isGood offset source
      in
        -- no match
        if newOffset == -1 then
          (offset, row, col)

        -- newline match
        else if newOffset == -2 then
          chomp isGood (offset + 1) (row + 1) 1 source

        -- normal match
        else
          chomp isGood newOffset row (col + 1) source

Notice that `chomp` can be tail-call optimized, so this turns into a
`while` loop under the hood.
-}
isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar =
  I.isSubChar



-- INDEX


{-| Find a substring after a given offset.

    findSubString before "42" offset row col "Is 42 the answer?"
        --==> (newOffset, newRow, newCol)

If `offset = 0` and `before = True` we would get `(3, 1, 4)`
If `offset = 0` and `before = False` we would get `(5, 1, 6)`

If `offset = 7` we would get `(-1, 1, 18)`
-}
findSubString : Bool -> String -> Int -> Int -> Int -> String -> (Int, Int, Int)
findSubString =
  I.findSubString
