# Semantics

The goal of this document is to explain how different parsers fit together. When will it backtrack? When will it not?

The `keyword` and `map` examples will introduce the concept of "consuming characters" which will pay off later when we get to `oneOf` and `backtrackable` which are the whole point of this library!

<br>

### `keyword : String -> Parser ()`

Say we have `keyword "import"`:

| String        | Result |
|---------------|--------|
| `"import"`    | 6/OK   |
| `"imp"`       | 0/ERR  |
| `"export"`    | 0/ERR  |
| `"important"` | 0/ERR  |

In our `n/OK` notation, we are indicating (1) how many characters we have consumed and (2) if the parser succeeded. Notice that the our parser only succeeds when it gets the string `import` without any extra letters.

<br>


### `map : (a -> b) -> Parser a -> Parser b`

Say we have `map func parser`:

| `parser` | Result  |
|----------|---------|
| n/OK     | n/OK    |
| n/ERR    | n/ERR   |

So the result of `parser` is always the result of `map func parser`.

<br>


### `map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c`

Say we have `map2 func parserA parserB`:

| `parserA` | `parserB` | Result  |
|-----------|-----------|---------|
| n/OK      | m/OK      | n+m/OK  |
| n/OK      | m/ERR     | n+m/ERR |
| n/ERR     |           | n/ERR   |

If `parserA` succeeds, we try `parserB`. We combine the offsets and the status.

If `parserA` fails, that is our result.

<br>


### `either : Parser a -> Parser a -> Parser a`

Say we have `either parserA parserB`:

| `parserA` | `parserB` | Result  |
|-----------|-----------|---------|
| n/OK      |           | n/OK    |
| 0/ERR     | m/OK      | m/OK    |
| 0/ERR     | m/ERR     | m/ERR   |
| n/ERR     |           | n/ERR   |

The 4th case is very important! **If `parserA` consumes one character (or more) you do not even try `parserB`.**

The `either` function does not appear in the public API, but I used it here because it makes the rules a bit easier to read. In the public API, we have `oneOf` instead. You can think of `oneOf` as trying `either` the head of the list, or `oneOf` the parsers in the tail of the list.

The important thing here is that once you start consuming characters, you do not try the subsequent parsers.

<br>


### `andThen : (a -> Parser b) -> Parser a -> Parser b`

Say we have `andThen callback parserA` where `callback a` produces `parserB`:

| `parserA` | `parserB` | Result  |
|-----------|-----------|---------|
| 0/ERR     |           | 0/ERR   |
| n/ERR     |           | n/ERR   |
| n/OK      | m/OK      | n+m/OK  |
| n/OK      | m/ERR     | n+m/ERR |

Even when `parserA` and `parserB` both succeed, you can end up with 0/ERR which `either` can skip over. So it does not matter whether you use `andThen` or `map2` to define a parser.

<br>


### `backtrackable : Parser a -> Parser a`

Say we have `bactrackable parser`:

| `parser` | Result  |
|----------|---------|
| n/OK     | 0/OK    |
| n/ERR    | 0/ERR   |

No matter what happens with `parser`, we are going to pretend that no characters were consumed. This becomes very interesting when paired with `oneOf`. You can have one of the options be `backtrackable`, so even if you do consume some characters, you can still try the next parser if something fails. **It is much more subtle than this though, so definitely read on!**

<br>


## Examples

This parser is intended to give you very precise control over backtracking behavior, and I think that is best explained through examples.

<br>

### `backtrackable`

Say we have `map2 func (backtrackable spaces) (symbol ",")` which can eat a bunch of spaces followed by a comma. Here is how it would work on different strings:

| String  | Result  |
|---------|---------|
| `"  ,"` | 1/OK    |
| `"  :"` | 0/ERR   |
| `"abc"` | 0/ERR   |

Notice that only the offset from `symbol ","` counts towards the total.

This becomes useful when paired with `either`!

<br>


### `backtrackable` + `oneOf` (inefficient)

Say we have the following `parser` definition:

```elm
parser : Parser (Maybe Int)
parser =
  oneOf
    [ succeed Just
        |. backtrackable spaces
        |. symbol ","
        |. spaces
        |= int
    , succeed Nothing
        |. spaces
        |. symbol "]"
    ]
```

Here is how it would work on different strings:

| String    | Result  |
|-----------|---------|
| `"  , 4"` | 3/OK    |
| `"  ,"`   | 1/ERR   |
| `"  , a"` | 2/ERR   |
| `"  ]"`   | 3/OK    |
| `"  a"`   | 2/ERR   |
| `"abc"`   | 0/ERR   |

Some of these cases are tricky, so let's look at them in more depth:

- `"  , a"` &mdash; `backtrackable spaces`, `symbol ","`, and `spaces` all succeed. At that point we have 2/OK. The `int` parser then fails on `a`, so we finish with 2/ERR. That means `oneOf` will NOT try the second possibility.
- `"  ]"` &mdash; `backtrackable spaces` succeeds, but `symbol ","` fails. At that point we have 0/ERR, so `oneOf` tries the second possibility. After backtracking, `spaces` and `symbol "]"` succeed with 3/OK.
- `"  a"` &mdash; `backtrackable spaces` succeeds, but `symbol ","` fails. At that point we have 0/ERR, so `oneOf` tries the second possibility. After backtracking, `spaces` succeeds with 2/OK and `symbol "]"` fails resulting in 2/ERR.

<br>


### `oneOf` (efficient)

Notice that in the previous example, we parsed `spaces` two times in some cases. This is inefficient, especially in large files with lots of whitespace. Backtracking is very inefficient in general though, so **if you are interested in performance, it is worthwhile to try to eliminate as many uses of `backtrackable` as possible.**

So we can rewrite that last example to never backtrack:

```elm
parser : Parser (Maybe Int)
parser =
  succeed identity
  	|. spaces
  	|= oneOf
        [ succeed Just
            |. symbol ","
            |. spaces
            |= int
        , succeed Nothing
            |. symbol "]"
        ]
```

Now we are guaranteed to consume the spaces only one time. After that, we decide if we are looking at a `,` or `]`, so we never backtrack and reparse things.

If you are strategic in shuffling parsers around, you can write parsers that do not need `backtrackable` at all. The resulting parsers are quite fast. They are essentially the same as [LR(k)](https://en.wikipedia.org/wiki/Canonical_LR_parser) parsers, but more pleasant to write. I did this in Elm compiler for parsing Elm code, and it was very significantly faster.