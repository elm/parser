# Semantics

Say we have a `Parser` defined like this:

```elm
number : Parser Int
number =
  succeed identity
  	|. symbol "("
  	|= int
  	|. symbol ")"
```

In this document, the result of running `Parser a` will be represented abstractly as `offset/status`. So with our `number` parser, the results will be like this:

| String | Result |
|--------|--------|
| "(4)"  | 3/OK   |
| "(3"   | 2/ERR  |
| "abc"  | 0/ERR  |

<br>


## Primitives

We will proceed by defining some primitives. Not all of these exist in the actual library. They are meant to be the simplest possible set of parsers that could express everything else in this library, independent of performance.

<br>

### `char : Parser Char`

| String    | Result |
|-----------|--------|
| empty     | 0/ERR  |
| non-empty | 1/OK   |

This could be used to define `symbol`, `keyword`, etc. It is extremely inefficient to do it that way though!

<br>


### `map : (a -> b) -> Parser a -> Parser b`

Say we have `map2 func parser`:

| `parser` | Result  |
|----------|---------|
| n/OK     | n/OK    |
| n/ERR    | n/ERR   |

<br>


### `map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c`

Say we have `map2 func parserA parserB`:

| `parserA` | `parserB` | Result  |
|-----------|-----------|---------|
| n/OK      | m/OK      | n+m/OK  |
| n/OK      | m/ERR     | n+m/ERR |
| n/ERR     |           | n/ERR   |

<br>


### `either : Parser a -> Parser a -> Parser a`

Say we have `either parserA parserB`:

| `parserA` | `parserB` | Result  |
|-----------|-----------|---------|
| n/OK      |           | n/OK    |
| 0/ERR     | m/OK      | m/OK    |
| 0/ERR     | m/ERR     | m/ERR   |
| n/ERR     |           | n/ERR   |

The 4th case is very important! **If you have consumed one character (or more) you commit to `parserA`.**

This could be used to implement `oneOf` which is what appears in the public API of this package.

<br>


### `andThen : (a -> Parser b) -> Parser a -> Parser b`

Say we have `andThen callback parserA` where `callback a` produces `parserB`:

| `parserA` | `parserB` | Result  |
|-----------|-----------|---------|
| 0/ERR     |           | 0/ERR   |
| n/ERR     |           | n/ERR   |
| n/OK      | m/OK      | n+m/OK  |
| n/OK      | m/ERR     | n+m/ERR |

Even when `parserA` and `parserB` both succeed, you can end up with 0/ERR which `either` can skip over. So `either` only commits if you consume characters.

<br>


### `backtrackable : Parser a -> Parser a`

Say we have `bactrackable parser`:

| `parser` | Result  |
|----------|---------|
| n/OK     | 0/OK    |
| n/ERR    | 0/ERR   |

<br>


## Examples

This parser is intended to give you very precise control over backtracking behavior, and I think that is best explained through examples.


### `backtrackable`

Say we have `map2 func (backtrackable spaces) (symbol ",")` which can eat a bunch of spaces followed by a comma. Here is how it would work on different strings:

| String  | Result  |
|---------|---------|
| `"  ,"` | 1/OK    |
| `"  :"` | 0/ERR   |
| `"abc"` | 0/ERR   |

Notice that only the offset from `symbol ","` counts towards the total.

This becomes useful when paired with `either`!


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

Some of these are a bit tricky, so let's look at them in more depth:

- `"  , a"` &mdash; `backtrackable spaces`, `symbol ","`, and `spaces` all succeed. At that point we have 2/OK. The `int` parser then fails on `a`, so we finish with 2/ERR. That means `oneOf` will NOT try the second possibility.
- `"  ]"` &mdash; `backtrackable spaces` succeeds, but `symbol ","` fails. At that point we have 0/ERR, so `oneOf` tries the second possibility. After backtracking, `spaces` and `symbol "]"` succeed with 3/OK.
- `"  a"` &mdash; `backtrackable spaces` succeeds, but `symbol ","` fails. At that point we have 0/ERR, so `oneOf` tries the second possibility. After backtracking, `spaces` succeeds with 2/OK and `symbol "]"` fails resulting in 2/ERR.



### `oneOf` (efficient)

Notice that in the previous example, we parsed `spaces` two times in some cases. **This is inefficient!** In fact, backtracking is very inefficient in general. **If you are interested in performance, it is worthwhile to try to eliminate as many uses of `backtrackable` as possible.**

We can rewrite that last example to never backtrack:

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

If you are strategic in shuffling parsers around, you can write parsers that do not need `backtrackable` at all. The resulting parsers are quite fast. They are essentially the same as [LR(k)](https://en.wikipedia.org/wiki/Canonical_LR_parser) parsers, but more pleasant to write.