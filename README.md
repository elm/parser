# Parser + Nice Error Messages

Goals:

  - Make writing parsers as simple and fun as possible.
  - Produce excellent error messages.
  - Go pretty fast.

This is achieved with a couple concepts that I have not seen in any other parser libraries: [parser pipelines](#parser-pipelines), [tracking context](#tracking-context), and [delayed commits](#delayed-commits).


## Parser Pipelines

To parse a 2D point like `( 3, 4 )`, you might create a `point` parser like this:

```elm
import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore)


type alias Point =
  { x : Float
  , y : Float
  }


point : Parser Point
point =
  succeed Point
    |. symbol "("
    |. spaces
    |= float
    |. spaces
    |. symbol ","
    |. spaces
    |= float
    |. spaces
    |. symbol ")"


spaces : Parser ()
spaces =
  ignore zeroOrMore (\c -> c == ' ')
```

All the interesting stuff is happening in `point`. It uses two operators:

  - [`(|.)`][ignore] means “parse this, but **ignore** the result”
  - [`(|=)`][keep] means “parse this, and **keep** the result”

So the `Point` function only gets the result of the two `float` parsers.

[ignore]: http://package.elm-lang.org/packages/elm-tools/parser/latest/Parser#|.
[keep]: http://package.elm-lang.org/packages/elm-tools/parser/latest/Parser#|=

The theory is that `|=` introduces more “visual noise” than `|.`, making it pretty easy to pick out which lines in the pipeline are important.

I recommend having one line per operator in your parser pipeline. If you need multiple lines for some reason, use a `let` or make a helper function.


## Tracking Context

Most parsers tell you the row and column of the problem:

    Something went wrong at (4:17)

That may be true, but it is not how humans think. It is how text editors think! It would be better to say:

    I found a problem with this list:

        [ 1, 23zm5, 3 ]
             ^
    I wanted an integer, like 6 or 90219.

Notice that the error messages says `this list`. That is context! That is the language my brain speaks, not rows and columns.

This parser package lets you annotate context with the [`inContext`][inContext] function. You can let the parser know “I am trying to parse a `"list"` right now” so if an error happens anywhere in that context, you get the hand annotation!

[inContext]: http://package.elm-lang.org/packages/elm-tools/parser/latest/Parser#inContext

> **Note:** This technique is used by the parser in the Elm compiler to give more helpful error messages.


## Delayed Commits

To make fast parsers with precise error messages, this package lets you control when a parser **commits** to a certain path.

For example, you are trying to parse the following list:

```elm
[ 1, 23zm5, 3 ]
```

Ideally, you want the error at the `z`, but the libraries I have seen make this difficult to achieve efficiently. You often end up with an error at `[` because “something went wrong”.

**This package introduces [`delayedCommit`][delayedCommit] to resolve this.**

Say we want to create `intList`, a parser for comma separated lists of integers like `[1, 2, 3]`. We would say something like this:

[delayedCommit]: http://package.elm-lang.org/packages/elm-tools/parser/latest/Parser#delayedCommit

```elm
import Parser exposing (..)


{-| We start by ignoring the opening square brace and some spaces.
We only really care about the numbers, so we parse an `int` and
then use `intListHelp` to start chomping other list entries.
-}
intList : Parser (List Int)
intList =
  succeed identity
    |. symbol "["
    |. spaces
    |= andThen (\n -> intListHelp [n]) int
    |. spaces
    |. symbol "]"


{-| `intListHelp` checks if there is a `nextInt`. If so, it
continues trying to find more list items. If not, it gives
back the list of integers we have accumulated so far.
-}
intListHelp : List Int -> Parser (List Int)
intListHelp revInts =
  oneOf
    [ nextInt
        |> andThen (\n -> intListHelp (n :: revInts))
    , succeed (List.reverse revInts)
    ]
```

Now we get to the tricky part! How do we define `nextInt`? Here are two approaches, but only the second one actually works!


```elm
-- BAD
badNextInt : Parser Int
badNextInt =
  succeed identity
    |. spaces
    |. symbol ","
    |. spaces
    |= int

-- GOOD
nextInt : Parser Int
nextInt =
  delayedCommit spaces <|
    succeed identity
      |. symbol ","
      |. spaces
      |= int
```

The `badNextInt` looks pretty normal, but it will not work. It commits as soon as the first `spaces` parser succeeds. It fails in the following situation:

```elm
[ 1, 2, 3 ]
          ^
```

When we get to the closing `]` we have already successfully parsed some spaces. That means we are commited to `badNextInt` and need a comma. That fails, so the whole parse fails!

With `nextInt`, the [`delayedCommit`][delayedCommit] function is saying to parse `spaces` but only commit if progress is made *after* that. So we are only commited to this parser if we see a comma.

<br>

<br>

## [Comparison with Prior Work](https://github.com/elm-tools/parser/blob/master/comparison.md)
